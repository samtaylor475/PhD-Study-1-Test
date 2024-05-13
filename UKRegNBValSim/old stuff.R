# Old way of creating z variables individually ----
l <- 1
for (i in DVlist) {
  DV <- DVlist[l]
  ZDV <- DVnamelist[l]
  data[ZDV] <- scale(data[DV])
  l <- l + 1
}


# Basic RSAS ----

## Test section ----
data$zSelf_STR <- scale(data$Self_STR)
data$zNB_STR <- scale(data$NB_STR)
data$zReg_STR <- scale(data$Reg_STR)
data$zUK_STR <- scale(data$UK_STR)
data$zSelf_SEN <- scale(data$Self_SEN)
data$zLifeSatisfaction <- scale(data$LifeSatisfaction)

ggplot(data, aes(x = Self_STR, y = LifeSatisfaction)) +
  geom_point()
ggplot(data, aes(x = zSelf_STR, y = zLifeSatisfaction)) +
  geom_point(position = "jitter")
ggplot(data, aes(x = zNB_STR, y = zLifeSatisfaction)) +
  geom_point(position = "jitter")

lm <- lm(data = data, LifeSatisfaction ~ Self_STR * NB_STR + I(Self_STR^2) + I(NB_STR^2))
summary(lm)
zlm <- lm(data = data, zLifeSatisfaction ~ zSelf_STR * zNB_STR + I(zSelf_STR^2) + I(zNB_STR^2))
summary(zlm)
zlm <- lm(data = data, zLifeSatisfaction ~ zSelf_STR * zNB_STR)
summary(zlm)

lm <- lmer(data = data, zLifeSatisfaction ~ 1 + (1 | Region))
summary(lm)
lm <- lm(data = data, LifeSatisfaction ~ Self_STR * NB_STR + I(Self_STR^2) + I(NB_STR^2))
summary(lm)
lm <- lmer(data = data, LifeSatisfaction ~ Self_STR * NB_STR + I(Self_STR^2) + I(NB_STR^2) +
  (1 + Self_STR | Region) + (1 + NB_STR | Region) + (1 + I(Self_STR^2) | Region) + (1 + I(NB_STR^2) | Region) + (1 + Self_STR * NB_STR | Region))
summary(lm)

RSA_MIL_ValSim <- RSA(LifeSatisfaction ~ Self_STR * NB_STR, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
RSA_MIL_ValSim <- RSA(zLifeSatisfaction ~ zSelf_STR * zNB_STR, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)

data$zSelf_SEN <- scale(data$Self_SEN)
data$zNB_SEN <- scale(data$NB_SEN)
data$zReg_SEN <- scale(data$Reg_SEN)
data$zUK_SEN <- scale(data$UK_SEN)
data$zLifeSatisfaction <- scale(data$LifeSatisfaction)
##### Quick n dirty plots ----
###### Self-NB -----
## STR
data$zSelf_STR <- scale(data$Self_STR)
data$zNB_STR <- scale(data$NB_STR)
data$zReg_STR <- scale(data$Reg_STR)
data$zUK_STR <- scale(data$UK_STR)
data$zLifeSatisfaction <- scale(data$LifeSatisfaction)

lm <- lm(data = data, LifeSatisfaction ~ Self_STR * NB_STR + I(Self_STR^2) + I(NB_STR^2))
summary(lm)
zlm <- lm(data = data, zLifeSatisfaction ~ zSelf_STR * zNB_STR + I(zSelf_STR^2) + I(zNB_STR^2))
summary(zlm)

lm <- lmer(data = data, zLifeSatisfaction ~ 1 + (1 | Region))
summary(lm)

RSA_MIL_ValSim <- RSA(LifeSatisfaction ~ Self_STR * NB_STR, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
RSA_MIL_ValSim <- RSA(zLifeSatisfaction ~ zSelf_STR * zNB_STR, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
## SEN
lm <- lm(data = data, LifeSatisfaction ~ Self_SEN * NB_SEN + I(Self_SEN^2) + I(NB_SEN^2))
summary(lm)
zlm <- lm(data = data, zLifeSatisfaction ~ zSelf_SEN * zNB_SEN + I(zSelf_SEN^2) + I(zNB_SEN^2))
summary(zlm)

lm <- lmer(data = data, zLifeSatisfaction ~ 1 + (1 | Region))
summary(lm)

RSA_MIL_ValSim <- RSA(LifeSatisfaction ~ Self_SEN * NB_SEN, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
RSA_MIL_ValSim <- RSA(zLifeSatisfaction ~ zSelf_SEN * zNB_SEN, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
# OPE
data$zSelf_OPE <- scale(data$Self_OPE)
data$zNB_OPE <- scale(data$NB_OPE)
data$zReg_OPE <- scale(data$Reg_OPE)
data$zUK_OPE <- scale(data$UK_OPE)
data$zLifeSatisfaction <- scale(data$LifeSatisfaction)

lm <- lm(data = data, LifeSatisfaction ~ Self_OPE * NB_OPE + I(Self_OPE^2) + I(NB_OPE^2))
summary(lm)
zlm <- lm(data = data, zLifeSatisfaction ~ zSelf_OPE * zNB_OPE + I(zSelf_OPE^2) + I(zNB_OPE^2))
summary(zlm)

lm <- lmer(data = data, zLifeSatisfaction ~ 1 + (1 | Region))
summary(lm)

RSA_MIL_ValSim <- RSA(LifeSatisfaction ~ Self_OPE * NB_OPE, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
RSA_MIL_ValSim <- RSA(zLifeSatisfaction ~ zSelf_OPE * zNB_OPE, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
## CON
data$zSelf_CON <- scale(data$Self_CON)
data$zNB_CON <- scale(data$NB_CON)
data$zReg_CON <- scale(data$Reg_CON)
data$zUK_CON <- scale(data$UK_CON)
data$zLifeSatisfaction <- scale(data$LifeSatisfaction)

lm <- lm(data = data, LifeSatisfaction ~ Self_CON * NB_CON + I(Self_CON^2) + I(NB_CON^2))
summary(lm)
zlm <- lm(data = data, zLifeSatisfaction ~ zSelf_CON * zNB_CON + I(zSelf_CON^2) + I(zNB_CON^2))
summary(zlm)

lm <- lmer(data = data, zLifeSatisfaction ~ 1 + (1 | Region))
summary(lm)

RSA_MIL_ValSim <- RSA(LifeSatisfaction ~ Self_CON * NB_CON, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
RSA_MIL_ValSim <- RSA(zLifeSatisfaction ~ zSelf_CON * zNB_CON, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)

###### Self-Reg ----
###### Self-Reg -----
## STR
data$zSelf_STR <- scale(data$Self_STR)
data$zReg_STR <- scale(data$Reg_STR)
data$zReg_STR <- scale(data$Reg_STR)
data$zUK_STR <- scale(data$UK_STR)
data$zLifeSatisfaction <- scale(data$LifeSatisfaction)

lm <- lm(data = data, LifeSatisfaction ~ Self_STR * Reg_STR + I(Self_STR^2) + I(Reg_STR^2))
summary(lm)
zlm <- lm(data = data, zLifeSatisfaction ~ zSelf_STR * zReg_STR + I(zSelf_STR^2) + I(zReg_STR^2))
summary(zlm)

lm <- lmer(data = data, zLifeSatisfaction ~ 1 + (1 | Region))
summary(lm)

RSA_MIL_ValSim <- RSA(LifeSatisfaction ~ Self_STR * Reg_STR, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
RSA_MIL_ValSim <- RSA(zLifeSatisfaction ~ zSelf_STR * zReg_STR, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
## SEN
lm <- lm(data = data, LifeSatisfaction ~ Self_SEN * Reg_SEN + I(Self_SEN^2) + I(Reg_SEN^2))
summary(lm)
zlm <- lm(data = data, zLifeSatisfaction ~ zSelf_SEN * zReg_SEN + I(zSelf_SEN^2) + I(zReg_SEN^2))
summary(zlm)

lm <- lmer(data = data, zLifeSatisfaction ~ 1 + (1 | Region))
summary(lm)

RSA_MIL_ValSim <- RSA(LifeSatisfaction ~ Self_SEN * Reg_SEN, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
RSA_MIL_ValSim <- RSA(zLifeSatisfaction ~ zSelf_SEN * zReg_SEN, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
# OPE
data$zSelf_OPE <- scale(data$Self_OPE)
data$zReg_OPE <- scale(data$Reg_OPE)
data$zReg_OPE <- scale(data$Reg_OPE)
data$zUK_OPE <- scale(data$UK_OPE)
data$zLifeSatisfaction <- scale(data$LifeSatisfaction)

lm <- lm(data = data, LifeSatisfaction ~ Self_OPE * Reg_OPE + I(Self_OPE^2) + I(Reg_OPE^2))
summary(lm)
zlm <- lm(data = data, zLifeSatisfaction ~ zSelf_OPE * zReg_OPE + I(zSelf_OPE^2) + I(zReg_OPE^2))
summary(zlm)

lm <- lmer(data = data, zLifeSatisfaction ~ 1 + (1 | Region))
summary(lm)

RSA_MIL_ValSim <- RSA(LifeSatisfaction ~ Self_OPE * Reg_OPE, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
RSA_MIL_ValSim <- RSA(zLifeSatisfaction ~ zSelf_OPE * zReg_OPE, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
## CON
data$zSelf_CON <- scale(data$Self_CON)
data$zReg_CON <- scale(data$Reg_CON)
data$zReg_CON <- scale(data$Reg_CON)
data$zUK_CON <- scale(data$UK_CON)
data$zLifeSatisfaction <- scale(data$LifeSatisfaction)

lm <- lm(data = data, LifeSatisfaction ~ Self_CON * Reg_CON + I(Self_CON^2) + I(Reg_CON^2))
summary(lm)
zlm <- lm(data = data, zLifeSatisfaction ~ zSelf_CON * zReg_CON + I(zSelf_CON^2) + I(zReg_CON^2))
summary(zlm)

lm <- lmer(data = data, zLifeSatisfaction ~ 1 + (1 | Region))
summary(lm)

##### Self-UK ----
###### Self-UK -----
## STR
data$zSelf_STR <- scale(data$Self_STR)
data$zUK_STR <- scale(data$UK_STR)
data$zReg_STR <- scale(data$Reg_STR)
data$zUK_STR <- scale(data$UK_STR)
data$zLifeSatisfaction <- scale(data$LifeSatisfaction)

lm <- lm(data = data, LifeSatisfaction ~ Self_STR * UK_STR + I(Self_STR^2) + I(UK_STR^2))
summary(lm)
zlm <- lm(data = data, zLifeSatisfaction ~ zSelf_STR * zUK_STR + I(zSelf_STR^2) + I(zUK_STR^2))
summary(zlm)

lm <- lmer(data = data, zLifeSatisfaction ~ 1 + (1 | Region))
summary(lm)

## SEN
lm <- lm(data = data, LifeSatisfaction ~ Self_SEN * UK_SEN + I(Self_SEN^2) + I(UK_SEN^2))
summary(lm)
zlm <- lm(data = data, zLifeSatisfaction ~ zSelf_SEN * zUK_SEN + I(zSelf_SEN^2) + I(zUK_SEN^2))
summary(zlm)

lm <- lmer(data = data, zLifeSatisfaction ~ 1 + (1 | Region))
summary(lm)

# OPE
data$zSelf_OPE <- scale(data$Self_OPE)
data$zUK_OPE <- scale(data$UK_OPE)
data$zReg_OPE <- scale(data$Reg_OPE)
data$zUK_OPE <- scale(data$UK_OPE)
data$zLifeSatisfaction <- scale(data$LifeSatisfaction)

lm <- lm(data = data, LifeSatisfaction ~ Self_OPE * UK_OPE + I(Self_OPE^2) + I(UK_OPE^2))
summary(lm)
zlm <- lm(data = data, zLifeSatisfaction ~ zSelf_OPE * zUK_OPE + I(zSelf_OPE^2) + I(zUK_OPE^2))
summary(zlm)

lm <- lmer(data = data, zLifeSatisfaction ~ 1 + (1 | Region))
summary(lm)

RSA_MIL_ValSim <- RSA(LifeSatisfaction ~ Self_OPE * UK_OPE, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
RSA_MIL_ValSim <- RSA(zLifeSatisfaction ~ zSelf_OPE * zUK_OPE, data, models = c("SQD", "full", "IA"))
plot(RSA_MIL_ValSim, model = "full", points = list(show = FALSE, value = "predicted"))
RSA.ST(RSA_MIL_ValSim)
## CON
data$zSelf_CON <- scale(data$Self_CON)
data$zUK_CON <- scale(data$UK_CON)
data$zReg_CON <- scale(data$Reg_CON)
data$zUK_CON <- scale(data$UK_CON)
data$zLifeSatisfaction <- scale(data$LifeSatisfaction)

lm <- lm(data = data, LifeSatisfaction ~ Self_CON * UK_CON + I(Self_CON^2) + I(UK_CON^2))
summary(lm)
zlm <- lm(data = data, zLifeSatisfaction ~ zSelf_CON * zUK_CON + I(zSelf_CON^2) + I(zUK_CON^2))
summary(zlm)

lm <- lmer(data = data, zLifeSatisfaction ~ 1 + (1 | Region))
summary(lm)

lm <- lm(LifeSatisfaction ~ Self_STR * NB_STR + (NB_STR^2) + (Self_STR^2), data = data)
summary(lm)
