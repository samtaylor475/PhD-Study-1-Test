DVlist <- c(
  "LifeSatisfaction", "LifeMeaning", "SelfEsteem", "SelfEsteemName",
  "Self-Efficacy", "NB-Efficacy", "Reg-Efficacy", "UK-Efficacy",
  "AnthroCC",
  "IndEngland", "IndNI", "IndWales", "IndScotland",
  "UK_Sim_1", "UK_Sim_2", "UK_Sim_3", "UK_Sim_4",
  "Reg_Sim_1", "Reg_Sim_2", "Reg_Sim_3", "Reg_Sim_4",
  "NB_Sim_1", "NB_Sim_2", "NB_Sim_3", "NB_Sim_4",
  "Depression", "Anxiety", "Stress", "DAS",
  "PI_UK", "GI_UK",
  "PEB_27", "PEB_21"
)
IVlist <- c("_STR", "_SEN", "_OPE", "_CON")
IV1list <- c("Self")
IV2list <- c("NB", "Reg", "UK")


# Reset .txt of polynomial regressions
sink("zRSAplots/lm.txt")
print("----Polynomial regressions with own and others' values as predictors ----")
sink() # returns output to the console


l <- 1
for (i in DVlist) {
  j <- 1
  DV <- DVlist[l] # pull name of current DV
  ZDV <- paste("Z", DV, sep = "") # create name for standardised variable
  data[ZDV] <- scale(data[DV]) # create standardised variable
  for (i in IVlist) {
    k <- 1
    for (i in IV1list) {
      m <- 1
      IV1 <- paste(IV1list[k], IVlist[j], sep = "")
      ZIV1 <- paste("Z", IV1, sep = "")
      data[ZIV1] <- scale(data[IV1]) # create standardised variable
      for (i in IV2list) {
        IV2 <- paste(IV2list[m], IVlist[j], sep = "")
        ZIV2 <- paste("Z", IV2, sep = "")
        data[ZIV2] <- scale(data[IV2]) # create standardised variable for IV2
        dataRSA <- data.frame(data[ZDV], data[ZIV1], data[ZIV2])
        dataRSA <- dataRSA %>% rename(DV = 1, IV1 = 2, IV2 = 3)
        currentRSA <- RSA(DV ~ IV1 * IV2, dataRSA, models = c("SQD", "full", "IA"))
        png(file = paste("zRSAplots/", ZDV, ZIV1, ZIV2, ".png", sep = ""), width = 2000, heigh = 2000, units = "px", res = 300)
        print(plot(currentRSA, model = "full", points = list(show = FALSE, value = "predicted"), xlab = ZIV1, ylab = ZIV2, zlab = ZDV))
        dev.off()
        RSA.ST(currentRSA)
        lm <- lm(data = dataRSA, DV ~ IV1 * IV2 + I(IV1^2) + I(IV2^2))
        sink("zRSAplots/lm.txt", append = TRUE)
        print(paste(DV, "~", IV1, "*", IV2, sep = " "))
        print(summary(lm))
        sink() # returns output to the console
        m <- m + 1
      }
      k <- k + 1
    }
    j <- j + 1
  }
  l <- l + 1
}


# PEB Loop
DVlist <- c(
  "LifeSatisfaction", "LifeMeaning", "SelfEsteem", "SelfEsteemName",
  "Self-Efficacy", "NB-Efficacy", "Reg-Efficacy", "UK-Efficacy",
  "AnthroCC",
  "IndEngland", "IndNI", "IndWales", "IndScotland",
  "UK_Sim_1", "UK_Sim_2", "UK_Sim_3", "UK_Sim_4",
  "Reg_Sim_1", "Reg_Sim_2", "Reg_Sim_3", "Reg_Sim_4",
  "NB_Sim_1", "NB_Sim_2", "NB_Sim_3", "NB_Sim_4"
)
IVlist <- c("_STR", "_SEN", "_OPE", "_CON")
IV1list <- c("Self")
IV2list <- c("NB", "Reg", "UK")
l <- 1
for (i in DVlist) {
  j <- 1
  DV <- DVlist[l] # pull name of current DV
  ZDV <- paste("Z", DV, sep = "") # create name for standardised variable
  data[ZDV] <- scale(data[DV]) # create standardised variable
  for (i in IVlist) {
    k <- 1
    for (i in IV1list) {
      m <- 1
      IV <- paste(IVlist[j], sep = "")
      ZIV1 <- paste("Z", IV, sep = "")
      data[ZIV1] <- scale(data[IV1]) # create standardised variable

      IV2 <- paste(IV2list[m], IVlist[j], sep = "")
      ZIV2 <- paste("Z", IV2, sep = "")
      data[ZIV2] <- scale(data[IV2]) # create standardised variable for IV2
      dataRSA <- data.frame(data[ZDV], data[ZIV1], data[ZIV2])
      dataRSA <- dataRSA %>% rename(DV = 1, IV1 = 2, IV2 = 3)
      currentRSA <- RSA(DV ~ IV1 * IV2, dataRSA, models = c("SQD", "full", "IA"))
      png(file = paste("zRSAplots/", ZDV, ZIV1, ZIV2, ".png", sep = ""), width = 2000, heigh = 2000, units = "px", res = 300)
      print(plot(currentRSA, model = "full", points = list(show = FALSE, value = "predicted"), xlab = ZIV1, ylab = ZIV2, zlab = ZDV))
      dev.off()
      RSA.ST(currentRSA)
      lm <- lm(data = dataRSA, DV ~ IV1 * IV2 + I(IV1^2) + I(IV2^2))
      sink("zRSAplots/lm.txt", append = TRUE)
      print(paste(DV, "~", IV1, "*", IV2, sep = " "))
      print(summary(lm))
      sink() # returns output to the console
      m <- m + 1
      k <- k + 1
    }
    j <- j + 1
  }
  l <- l + 1
}



#### Run for non standardised variables ----
DVlist <- c(
  "LifeSatisfaction", "LifeMeaning", "SelfEsteem", "SelfEsteemName",
  "PE_Efficacy_6", "PE_Efficacy_2", "PE_Efficacy_4", "PE_Efficacy_5",
  "AnthroCC",
  "IndEngland", "IndNI", "IndWales", "IndScotland"
)


IVlist <- c("_STR", "_SEN", "_OPE", "_CON")
IV1list <- c("Self")
IV2list <- c("NB", "Reg", "UK")


l <- 1
for (i in DVlist) {
  j <- 1
  DV <- DVlist[l]
  for (i in IVlist) {
    k <- 1
    for (i in IV1list) {
      IV1 <- paste(IV1list[k], IVlist[j], sep = "")
      IV2 <- paste(IV2list[k], IVlist[j], sep = "")
      dataRSA <- data.frame(data[DVlist[l]], data[IV1], data[IV2])
      dataRSA <- dataRSA %>% rename(DV = 1, IV1 = 2, IV2 = 3)
      currentRSA <- RSA(DV ~ IV1 * IV2, dataRSA, models = c("SQD", "full", "IA"))
      png(file = paste("RSAplots/", DV, IV1, IV2, ".png", sep = ""), width = 2000, heigh = 2000, units = "px", res = 300)
      print(plot(currentRSA, model = "full", points = list(show = FALSE, value = "predicted")))
      dev.off()
      RSA.ST(currentRSA)
      k <- k + 1
    }
    j <- j + 1
  }
  l <- l + 1
}

# next task - renaming issue
currentRSA$DV <- DV
currentRSA$IV1 <- IV1
currentRSA$IV2 <- IV2

ggsave("plot.png", width = 5, height = 5, dpi = 300)
p1 <- plot(currentRSA, bw = TRUE)
trellis.device(color = TRUE, device = "png", filename = "RSA_plot.png")
print(p1)
dev.off()
plot(currentRSA, model = "full", points = list(show = FALSE, value = "predicted"))
