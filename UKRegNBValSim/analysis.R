
# SETUP ----
# search for and install all necessary packages
if (!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
if (!require("ggradar")) devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
packages <- c(
  "tidyverse", "dplyr", "data.table", "tidyr", "psych", "kableExtra", "ggplot2", "ggpubr", "corrplot", "yarrr", "apaTables",
  "knitr", "car", "gridExtra", "pacman", "fmsb", "GGally", "RSA", 
  "qualtRics", # imports qualtrics surveys correctly
  "pollster", # allows for easy weighting
  "weights", # weights
  "survey", # for weighting
  "lme4", # complex linear models
  "doParallel", # allow parallelised computing
  "plotly", #3D plots
  "foreach" # allow more complex/ parallelised for loops
) # list of necessary packages
for (i in packages) {
  if (!is.element(i, .packages(all.available = TRUE))) {
    install.packages(i, depencies = TRUE, repos = "http://cran.us.r-project.org")
  }
  library(i, character.only = TRUE)
}

# Setup parallel computing
n_cores <- detectCores() - 2 #identify number of threads to use - try n/2 if issues
registerDoParallel(n_cores) #sets number of threads to use - try 4 if issues


# Data Setup -----
data_raw <- read_survey("UKRegNBValSim/2022UKRegNBValSimData.csv")
data <- data_raw %>%
  filter(
    Consent_1 == 1,
    Consent_2 == 1,
    Consent_3 == 1,
    DistributionChannel == "anonymous",
    Finished == 1,
    AC1 == 3 | PEB_11 == 100 # AC2
  )
# Generate file of rejected submissions
data_reject <- data_raw %>%
  filter(
    AC1 != 3,
    PEB_11 != 100 # AC2
  ) %>%
  select(PROLIFIC_PID)
# Generate median time
median(data$`Duration (in seconds)`) / 60 # median time in minutes
mean(data$`Duration (in seconds)`) / 60 # mean time in minutes
# Get variable names from codebook
data_variables <- read.csv("UKRegNBValSim/Variable_Codebook.csv")
oldvar <- data_variables$Raw
newvar <- data_variables$Renamed
contentsvar <- data_variables$Item
# Fix Scottish Independence data
data <- subset(data, select = -IndScotland)
data_IndScotland <- read_survey("UKRegNBValSim/2022UKRegNBValSimScotIndData.csv")
data <- merge(data, data_IndScotland[, c("IndScotland", "PROLIFIC_PID")], by = "PROLIFIC_PID", all.x = TRUE)



# Values -----
# SVS_1 Protecting the environment (from destruction or pollution)
# SVS_2 Broadmindedness (being tolerant of different ideas or beliefs)
# SVS_3 Social justice (correcting injustice, protecting societies weak and vulnerable)
# SVS_4 Helpfulness (helping friends and family)
# SVS_5 Honesty (being genuine and sincere)
# SVS_6 Social power (having control over others, being dominant)
# SVS_7 Wealth (having material possessions, money)
# SVS_8 Success (achieving one's goals)
# SVS_9 Ambition (being hard-working, aspiring)
# SVS_10 Curiosity (being interested in everything, novelty, change)
# SVS_11 Freedom (forming own opinions, making own decisions)
# SVS_12 A varied life (filled with challenge, novelty and change)
# SVS_13 An exciting life (having stimulating experiences)
# SVS_14 Pleasure (fulfilling one's desires)
# SVS_15 Enjoying life (enjoying food, sex, leisure etc.)
# SVS_16 Accepting my portion in life (submitting to life's circumstances)
# SVS_17 Respecting tradition (maintaining customs of one's family or culture)
# SVS_18 Honouring of parents or elders (showing respect)
# SVS_19 Obedience (following rules, meeting obligations)
# SVS_20 Social order (having a stable society)
# SVS_21 Family security (living in secure surroundings)

setnames(data, old=c(oldvar), new=c(newvar))
data <- data %>%
  rowwise() %>%
  mutate(
    Self_STR = mean(c(Self_SVS_1, Self_SVS_2, Self_SVS_3, Self_SVS_4, Self_SVS_5), ),
    Self_SEN = mean(c(Self_SVS_6, Self_SVS_7, Self_SVS_8, Self_SVS_9), ),
    Self_OPE = mean(c(Self_SVS_10, Self_SVS_11, Self_SVS_12, Self_SVS_13, Self_SVS_14, Self_SVS_15), ),
    Self_CON = mean(c(Self_SVS_16, Self_SVS_17, Self_SVS_18, Self_SVS_19, Self_SVS_20, Self_SVS_21), ),
    NB_STR = mean(c(NB_SVS_1, NB_SVS_2, NB_SVS_3, NB_SVS_4, NB_SVS_5), ),
    NB_SEN = mean(c(NB_SVS_6, NB_SVS_7, NB_SVS_8, NB_SVS_9), ),
    NB_OPE = mean(c(NB_SVS_10, NB_SVS_11, NB_SVS_12, NB_SVS_13, NB_SVS_14, NB_SVS_15), ),
    NB_CON = mean(c(NB_SVS_16, NB_SVS_17, NB_SVS_18, NB_SVS_19, NB_SVS_20, NB_SVS_21), ),
    Reg_STR = mean(c(Reg_SVS_1, Reg_SVS_2, Reg_SVS_3, Reg_SVS_4, Reg_SVS_5), ),
    Reg_SEN = mean(c(Reg_SVS_6, Reg_SVS_7, Reg_SVS_8, Reg_SVS_9), ),
    Reg_OPE = mean(c(Reg_SVS_10, Reg_SVS_11, Reg_SVS_12, Reg_SVS_13, Reg_SVS_14, Reg_SVS_15), ),
    Reg_CON = mean(c(Reg_SVS_16, Reg_SVS_17, Reg_SVS_18, Reg_SVS_19, Reg_SVS_20, Reg_SVS_21), ),
    UK_STR = mean(c(UK_SVS_1, UK_SVS_2, UK_SVS_3, UK_SVS_4, UK_SVS_5), ),
    UK_SEN = mean(c(UK_SVS_6, UK_SVS_7, UK_SVS_8, UK_SVS_9), ),
    UK_OPE = mean(c(UK_SVS_10, UK_SVS_11, UK_SVS_12, UK_SVS_13, UK_SVS_14, UK_SVS_15), ),
    UK_CON = mean(c(UK_SVS_16, UK_SVS_17, UK_SVS_18, UK_SVS_19, UK_SVS_20, UK_SVS_21), ),
    Depression = mean(c(DepressionAnxiety_1, DepressionAnxiety_2)),
    Anxiety = mean(c(DepressionAnxiety_3, DepressionAnxiety_4)),
    Stress = mean(c(DepressionAnxiety_5, DepressionAnxiety_6)),
    DAS = mean(c(Depression, Anxiety, Stress)),
    PI_NB = mean(c(PI_NB_Like,
                   PI_NB_Id,
                   PI_NB_Emo)),
    PI_Reg = mean(c(PI_Reg_Like,
                    PI_Reg_Id,
                    PI_Reg_Emo)),
    PI_UK = mean(c(PI_UK_Like,
                   PI_UK_Id,
                   PI_UK_Emo)),
    PEB = mean(c_across(starts_with("PEB_")), na.rm=TRUE)
  ) 



### Reliability Estimates and Combined Scales ----
#### Depression
data_depression <- data %>% select(DepressionAnxiety_1, DepressionAnxiety_2)
alpha_depression <- psych::alpha(data_depression)
data_anxiety <- data %>% select(DepressionAnxiety_3, DepressionAnxiety_4)
alpha_anxiety <- psych::alpha(data_anxiety)
data_stress <- data %>% select(DepressionAnxiety_5, DepressionAnxiety_6)
alpha_stress <- psych::alpha(data_stress)

data_PEB <- data %>% select(grep("PEB_", names(data)))
setnames(data_PEB, old=c(newvar), new=c(contentsvar), skip_absent = TRUE)
pca1 <- principal(data_PEB, nfactors = 4, rotate = "varimax")
pca1
pca1_loadings <- as.data.frame(pca1$loadings[, 1:4])
pca1_loadings_clean <- replace(pca1_loadings, pca1_loadings<.3, "N/A")
alpha_PEB <- psych::alpha(data_PEB)

# Descriptives ----
data %>%
  summarise()
## Weighted Mean Tables ----
data$WeightRegion <- 0
regions <- c(1:13)
regionpop <- c(
  2680763, # NE
  7367456, # NW
  5526350, # Yorkshire
  4865583, # E Mid
  5961929, # W Mid
  6269161, # East
  9002488, # LDN
  9217265, # SE
  5659143, # SW
  3169586, # Wales
  5466000, # Scot
  1895510, # NI
  0
)

for (i in regions) {
  data$WeightRegion <- ifelse(data$Region == i,
    (regionpop[i]
    / 67081234)
    / length(which(data$Region == i)),
    data$WeightRegion
  )
}



# Rounded weights (issue - doesn't always = 100)
round(wpct(data$Self_SVS_1, data$WeightRegion) * 100)
round(wpct(data$Self_SVS_2, data$WeightRegion) * 100)
round(wpct(data$Self_SVS_3, data$WeightRegion) * 100)
round(wpct(data$Self_SVS_4, data$WeightRegion) * 100)
round(wpct(data$Self_SVS_5, data$WeightRegion) * 100)

# non-rounded weights
wpct(data$Self_SVS_1, data$WeightRegion) * 100
wpct(data$Self_SVS_2, data$WeightRegion) * 100
wpct(data$Self_SVS_3, data$WeightRegion) * 100
wpct(data$Self_SVS_4, data$WeightRegion) * 100
wpct(data$Self_SVS_5, data$WeightRegion) * 100
wpct(data$Self_SVS_6, data$WeightRegion) * 100
wpct(data$Self_SVS_7, data$WeightRegion) * 100
wpct(data$Self_SVS_8, data$WeightRegion) * 100
wpct(data$Self_SVS_9, data$WeightRegion) * 100
wpct(data$Self_SVS_10, data$WeightRegion) * 100
wpct(data$Self_SVS_11, data$WeightRegion) * 100
wpct(data$Self_SVS_12, data$WeightRegion) * 100
wpct(data$Self_SVS_13, data$WeightRegion) * 100
wpct(data$Self_SVS_14, data$WeightRegion) * 100
wpct(data$Self_SVS_15, data$WeightRegion) * 100
wpct(data$Self_SVS_16, data$WeightRegion) * 100
wpct(data$Self_SVS_17, data$WeightRegion) * 100
wpct(data$Self_SVS_18, data$WeightRegion) * 100
wpct(data$Self_SVS_19, data$WeightRegion) * 100
wpct(data$Self_SVS_20, data$WeightRegion) * 100
wpct(data$Self_SVS_21, data$WeightRegion) * 100

weighted.mean(data$Self_SVS_1, data$WeightRegion)
weighted.mean(data$Self_SVS_2, data$WeightRegion)
weighted.mean(data$Self_SVS_3, data$WeightRegion)
weighted.mean(data$Self_SVS_4, data$WeightRegion)
weighted.mean(data$Self_SVS_5, data$WeightRegion)
weighted.mean(data$Self_SVS_6, data$WeightRegion)
weighted.mean(data$Self_SVS_7, data$WeightRegion)
weighted.mean(data$Self_SVS_8, data$WeightRegion)
weighted.mean(data$Self_SVS_9, data$WeightRegion)
weighted.mean(data$Self_SVS_10, data$WeightRegion)
weighted.mean(data$Self_SVS_11, data$WeightRegion)
weighted.mean(data$Self_SVS_12, data$WeightRegion)
weighted.mean(data$Self_SVS_13, data$WeightRegion)
weighted.mean(data$Self_SVS_14, data$WeightRegion)
weighted.mean(data$Self_SVS_15, data$WeightRegion)
weighted.mean(data$Self_SVS_16, data$WeightRegion)
weighted.mean(data$Self_SVS_17, data$WeightRegion)
weighted.mean(data$Self_SVS_18, data$WeightRegion)
weighted.mean(data$Self_SVS_19, data$WeightRegion)
weighted.mean(data$Self_SVS_20, data$WeightRegion)
weighted.mean(data$Self_SVS_21, data$WeightRegion)

y <- c(
  data$Self_SVS_1,
  data$Self_SVS_2,
  data$Self_SVS_3,
  data$Self_SVS_4,
  data$Self_SVS_5
)
y <- c("data$Self_SVS_1", "data$Self_SVS_2")
# initialise dataframe with column for value name + scores (-1 to 7)

y <- data %>%
  group_by(Region) %>%
  count(Self_SVS_1)

regmeans <- data %>%
  group_by(Region) %>%
  summarise_at(vars(Self_STR, Self_OPE, Self_CON, Self_SEN,
                    PI_UK, PI_Reg),
               list(mean))

regmeans <- data %>%
  group_by(Region) %>%
  summarise_at(vars(Self_STR, Self_OPE, Self_CON, Self_SEN,
                    PI_UK, PI_Reg, PI_NB,
                    GI_UK, GI_Reg, GI_NB),
               list(mean))

# SocioDemographics ----
data %>%
  group_by(SubjectiveClass) %>%
  summarise_at(
    vars(Self_STR, Self_CON, Self_SEN, Self_OPE, ClassDifference),
    list(name = mean)
  )

lm <- lm(LifeSatisfaction ~ SSES_UK + SSES_RE + SSES_NB, data)
summary(lm)
lm <- lm(LifeSatisfaction ~ SSES_UK, data)
summary(lm)
lm <- lm(LifeSatisfaction ~ SSES_RE, data)
summary(lm)
lm <- lm(LifeSatisfaction ~ SSES_NB, data)
summary(lm)

lm <- lm(PEB_5 ~ SSES_UK, data)
summary(lm)

ggplot(data, aes(x = LeftRight, y = Self_STR)) +
  geom_point() +
  geom_smooth()

ggplot(data, aes(x = LeftRight, y = Self_SEN)) +
  geom_point() +
  geom_smooth()

ggplot(data, aes(x = LeftRight, y = Self_OPE)) +
  geom_point() +
  geom_smooth()

ggplot(data, aes(x = LeftRight, y = Self_CON)) +
  geom_point() +
  geom_smooth()

#### PCA ----
lm <- lm(LifeSatisfaction ~ UK_ValSim, data)
summary(lm)
lm <- lm(LifeMeaning ~ UK_ValSim, data)
summary(lm)
lm <- lm(GI_UK ~ UK_ValSim, data)
summary(lm)
lm <- lm(Affect_Good ~ UK_ValSim, data)
summary(lm)
lm <- lm(Affect_Bad ~ UK_ValSim, data)
summary(lm)
lm <- lm(UK_OveSim ~ UK_ValSim, data)
summary(lm)
lm <- lm(DAS ~ UK_ValSim, data)
summary(lm)



