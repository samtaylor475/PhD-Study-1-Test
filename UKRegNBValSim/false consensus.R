cor.test(data$UK_STR, data$Self_STR, method="pearson")
cor.test(data$UK_SEN, data$Self_SEN, method="pearson")
cor.test(data$UK_CON, data$Self_CON, method="pearson")
cor.test(data$UK_OPE, data$Self_OPE, method="pearson")

ggplot(data, aes(Self_STR, UK_STR)) + 
  geom_jitter(width = 0.05, height = 0.05) + 
  geom_smooth(method="lm", se=FALSE) + 
  scale_x_continuous(limits=c(-1,7), 
                     breaks = seq(-1, 7, by = 1),
                     expand = c(0,0))+
  scale_y_continuous(limits=c(-1,7), 
                     breaks = seq(-1, 7, by = 1),
                     expand = c(0,0))+
  theme_bw()+
  theme(aspect.ratio=1/1) +
  geom_abline()
ggplot(data, aes(Self_SEN, UK_SEN))+ 
  geom_jitter(width = 0.05, height = 0.05) + 
  geom_smooth(method="lm", se=FALSE) + 
  scale_x_continuous(limits=c(-1,7), 
                     breaks = seq(-1, 7, by = 1),
                     expand = c(0,0))+
  scale_y_continuous(limits=c(-1,7), 
                     breaks = seq(-1, 7, by = 1),
                     expand = c(0,0))+
  theme_bw()+
  theme(aspect.ratio=1/1) +
  geom_abline()
ggplot(data, aes(Self_CON, UK_CON))+ 
  geom_jitter(width = 0.05, height = 0.05) + 
  geom_smooth(method="lm", se=FALSE) + 
  scale_x_continuous(limits=c(-1,7), 
                     breaks = seq(-1, 7, by = 1),
                     expand = c(0,0))+
  scale_y_continuous(limits=c(-1,7), 
                     breaks = seq(-1, 7, by = 1),
                     expand = c(0,0))+
  theme_bw()+
  theme(aspect.ratio=1/1) +
  geom_abline()
ggplot(data, aes(Self_OPE, UK_OPE))+ 
  geom_jitter(width = 0.05, height = 0.05) + 
  geom_smooth(method="lm", se=FALSE) + 
  scale_x_continuous(limits=c(-1,7), 
                     breaks = seq(-1, 7, by = 1),
                     expand = c(0,0))+
  scale_y_continuous(limits=c(-1,7), 
                     breaks = seq(-1, 7, by = 1),
                     expand = c(0,0))+
  theme_bw()+
  theme(aspect.ratio=1/1) +
  geom_abline()

count(data, Self_STR>UK_STR)
count(data, Self_STR<UK_STR)
count(data, Self_SEN>UK_SEN)
count(data, Self_SEN<UK_SEN)
