
DVlist <- c(newvar)

### Loop generating RSA graphs and polynomial regression model outputs ----
#### Reset .txt of polynomial regressions
sink("zRSAplots/lm.txt")
print("----Polynomial regressions with own and others' values as predictors ----")
sink() # returns output to the console
#### Define DVs and IVs
DVlist <- c( #set DVs
  "LifeSatisfaction", "LifeMeaning", "SelfEsteem", "SelfEsteemName",
  "Self-Efficacy", "NB-Efficacy", "Reg-Efficacy", "UK-Efficacy",
  "AnthroCC",
  "IndEngland", "IndNI", "IndWales", "IndScotland",
  "UK_OveSim", "UK_OveSim2", "UK_ValSim", "UK_ValSim2",
  "Reg_OveSim", "Reg_OveSim2", "Reg_ValSim", "Reg_ValSim2",
  "NB_OveSim", "NB_OveSim2", "NB_ValSim", "NB_ValSim2",
  "Depression", "Anxiety", "Stress", "DAS",
  "Affect_Good", "Affect_Bad",
  "PI_UK", "GI_UK",
  "PI_NB", "GI_NB",
  "PI_Reg", "GI_Reg",
  "PEB_11", "PEB_22"
)
IVlist <- c("_STR", "_SEN", "_OPE", "_CON", "_SVS_1") #define IV to be looked at 2 levels (e.g. self+UK)
IV1list <- c("Self") #define self IV
IV2list <- c("NB", "Reg", "UK") #define comparison IV
#### Descriptives Loop
data_summary <- data %>% 
  select(one_of(DVlist))
sapply(data_summary, mean, na.rm=TRUE)

#### Loop code
  foreach(DV = DVlist, .packages = packages) %dopar% {
  ZDV <- paste("Z", DV, sep = "") # create name for standardised variable
  data[ZDV] <- scale(data[DV]) # create standardised variable
  foreach(xIV = IVlist, .packages = packages) %dopar% {
    foreach(xIV1 = IV1list, .packages = packages) %dopar% {
      IV1 <- paste(xIV1, xIV, sep = "")
      ZIV1 <- paste("Z", IV1, sep = "")
      data[ZIV1] <- scale(data[IV1]) # create standardised variable
      foreach(xIV2 = IV2list, .packages = packages) %dopar% {
        IV2 <- paste(xIV2, xIV, sep = "")
        ZIV2 <- paste("Z", IV2, sep = "")
        data[ZIV2] <- scale(data[IV2]) # create standardised variable for IV2
        dataRSA <- data.frame(data[ZDV], data[ZIV1], data[ZIV2])
        dataRSA <- dataRSA %>% rename(DV = 1, IV1 = 2, IV2 = 3)
        currentRSA <- RSA(DV ~ IV1 * IV2, dataRSA, models = c("SQD", "full", "IA"))
        png(file = paste("zRSAplots/", ZDV, ZIV1, ZIV2, ".png", sep = ""), width = 2000, heigh = 2000, units = "px", res = 300)
        print(plot(currentRSA, model = "full", points = list(show = FALSE, value = "predicted"), xlab = ZIV1, ylab = ZIV2, zlab = ZDV))
        dev.off()
        lm <- lm(data = dataRSA, DV ~ IV1 * IV2 + I(IV1^2) + I(IV2^2))
        sink("zRSAplots/lm.txt", append = TRUE)
        print(paste(DV, "~", IV1, "*", IV2, sep = " "))
        print(summary(lm))
        sink() # returns output to the console
      }
    }
  }
  }

# Confirmation Report Vers
#### Define DVs and IVs
DVlist <- c()
# assumes IVs follow name structure of [level]_[IV]
# e.g. Self_Conservation and Neighbour_Conservation
IVlist <- c() #define IV to be looked at 2 levels (e.g. self+UK)
IV1list <- c() #define "baseline" IV level to be compared against (e.g. own values)
IV2list <- c() #define comparison IV level (e.g., perceived neighbours' values)
#### Loop code
foreach(DV = DVlist, .packages = packages) %dopar% {
  ZDV <- paste("Z", DV, sep = "") # create name for standardised variable
  data[ZDV] <- scale(data[DV]) # z-score DV
  foreach(xIV = IVlist, .packages = packages) %dopar% {
    foreach(xIV1 = IV1list, .packages = packages) %dopar% {
      IV1 <- paste(xIV1, xIV, sep = "") #specify IV1 from list
      ZIV1 <- paste("Z", IV1, sep = "") #z score IV1
      data[ZIV1] <- scale(data[IV1]) # z-score IV1
      foreach(xIV2 = IV2list, .packages = packages) %dopar% {
        IV2 <- paste(xIV2, xIV, sep = "") #specify IV2 from list
        ZIV2 <- paste("Z", IV2, sep = "") #z-score IV2
        data[ZIV2] <- scale(data[IV2]) # create standardised variable for IV2
        dataRSA <- data.frame(data[ZDV], data[ZIV1], data[ZIV2]) #collects variables for analysis
        dataRSA <- dataRSA %>% rename(DV = 1, IV1 = 2, IV2 = 3) 
        #allows for standardised variables to be specified in LM
        currentRSA <- RSA(DV ~ IV1 * IV2, dataRSA, models = c("SQD", "full", "IA"))
        png(file = paste("zRSAplots/", ZDV, ZIV1, ZIV2, ".png", sep = ""), 
            width = 2000, heigh = 2000, units = "px", res = 300)
        print(plot(currentRSA, model = "full", points = list(show = FALSE, value = "predicted"), 
                   xlab = ZIV1, ylab = ZIV2, zlab = ZDV))
        dev.off() #saves plot as high resolution .png
        lm <- lm(data = dataRSA, DV ~ IV1 * IV2 + I(IV1^2) + I(IV2^2))
        sink("zRSAplots/lm.txt", append = TRUE) #prints linear model to a .txt document
        print(paste(DV, "~", IV1, "*", IV2, sep = " ")) #meaningful heading
        print(summary(lm)) #model
        sink() # returns output to the console
      }
    }
  }
}