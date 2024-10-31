ghg <- read.csv("/cloud/project/activity06/Deemer_GHG_Data.csv")

library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)

ghg$log.ch4 <- log(ghg$ch4+1)

ggplot(data = ghg,
       aes(x=airTemp, y=log.ch4))+
  geom_point()+
  labs(x="Air Temperature", y="Methane Log")

ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)

unique(ghg$Region)

ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)

ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)

mod.full <- lm(log.ch4 ~ airTemp+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg)

summary(mod.full)

res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)

shapiro.test(res.full)

plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)

reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)

chart.Correlation(reg.data, histogram=TRUE, pch=19)

full.step <- ols_step_forward_aic(mod.full)

full.step

full.step$model

plot(full.step)


predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="prediction")

predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="confidence")
