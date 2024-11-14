# CHAPTER 8
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

# CHAPTER 9
ETdata <- read.csv("/cloud/project/activity06/ETdata.csv")

unique(ETdata$crop)

install.packages("lubridate")
install.packages("forecast")

library(lubridate)
library(forecast)

almond <- ETdata %>%
  filter(crop == "Almonds") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

almondtimeseries <- ts(almond$ET.in,
                start = c(2016,1),
                frequency= 12)

almonddecompose <- decompose(almondtimeseries)
plot(almonddecompose)

acf(na.omit(almondtimeseries),
    lag.max = 24)

pacf.plot <- pacf(na.omit(almondtimeseries))

almondy <- na.omit(almondtimeseries)
model1 <- arima(almondy ,
                order = c(1,0,0))
model1

model4 <- arima(almondy ,
                order = c(4,0,0))
model4


AR_fit1 <- almondy - residuals(model1) 
AR_fit4 <- almondy - residuals(model4)

plot(almondy)

points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

newAlmond <- forecast(model4)
newAlmond

newAlmondF <- data.frame(newAlmond)

years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))

ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+ 
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

#HW6

#QUESTION 1
#The authors of the reservoir greenhouse gas study recommend using the following transformation for CO2 data:
# 1/(CO2+1000)
#Use the transformation and design a regression analysis to present to water 
#managers about the impact of reservoir characteristics on carbon dioxide fluxes.
#In designing your regression, you should consider the environmental conditions 
#that impact carbon dioxide fluxes, the availability of data, and the assumptions 
#of ordinary least squares regression. Create a regression table including a 
#R2 and the sample size with paragraph summary of the findings that can be presented 
#to water managers.

ghg$co2Transform <- 1/(ghg$co2 + 1000)

model.flux <- lm(co2Transform ~ airTemp +
                   log.age + mean.depth + surface.area + chlorophyll.a + log.DIP + 
                   log.ch4 + runoff + log.precip, 
                 data=ghg)
summary(model.flux)

res.flux <- rstandard(model.flux)
fit.flux <- fitted.values(model.flux)

qqnorm(res.flux, pch=19, col="grey50")
qqline(res.flux)

shapiro.test(res.flux)

plot(fit.flux,res.flux, pch=19, col="grey50")
abline(h=0)

#QUESTION 2
#Question 2
#Decompose the evapotranspiration time series for almonds, pistachios, 
#fallow/idle fields, corn, and table grapes. Evaluate differences in the observations, 
#trends, and seasonality of the data between the different crops. Write a summary 
#of your evaluation for a water manager that is interested in examining how irrigation 
#can affect evapotranspiration. The manager also wants to understand what crops have the 
#greatest water consumption, the timing of high water consumption, and if there are changes 
#over time. Include plots of your decomposition.

almond <- ETdata %>%
  filter(crop == "Almonds") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm = T))

almondTimeSeries <- ts(almond$ET.in,
                start = c(2016,1),
                frequency= 12)

almondDecompose <- decompose(almondTimeSeries)

plot(almondDecompose)

acf(na.omit(almondTimeSeries),
    lag.max = 24)
almond.acf.plot <- acf(na.omit(almondTimeSeries))


pistachios <- ETdata %>%
  filter(crop == "Pistachios") %>% 
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm = T))

pistachiosTimeSeries <- ts(pistachios$ET.in,
                    start = c(2016,1),
                    frequency= 12)

pistachiosDecompose <- decompose(pistachiosTimeSeries)

plot(pistachiosDecompose)

acf(na.omit(pistachiosTimeSeries),
    lag.max = 24)
pistachios.acf.plot <- acf(na.omit(pistachiosTimeSeries))


fallow <- ETdata %>%
  filter(crop == "Fallow/Idle Cropland") %>%
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm = T))

fallowTimeSeries <- ts(fallow$ET.in, 
                start = c(2016,1),
                frequency= 12)

fallowDecompose <- decompose(fallowTimeSeries)

plot(fallowDecompose)

acf(na.omit(fallowTimeSeries), 
    lag.max = 24)
fallow.acf.plot <- acf(na.omit(fallowTimeSeries))


corn <- ETdata %>%
  filter(crop == "Corn") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm = T))

cornTimeSeries <- ts(corn$ET.in,
              start = c(2016,1),
              frequency= 12)

cornDecompose <- decompose(cornTimeSeries)

plot(cornDecompose)

acf(na.omit(cornTimeSeries),
    lag.max = 24)
corn.acf.plot <- acf(na.omit(cornTimeSeries))


grapes <- ETdata %>% 
  filter(crop == "Grapes (Table/Raisin)") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm = T))

grapesTimeSeries <- ts(grapes$ET.in,
                start = c(2016,1), 
                frequency= 12) 

grapesDecompose <- decompose(grapesTimeSeries)

plot(grapesDecompose)

acf(na.omit(grapesTimeSeries),
    lag.max = 24)
grapes.acf.plot <- acf(na.omit(grapesTimeSeries))

#QUESTION 3
#Design an autoregressive model for pistachios and fallow/idle fields. Forecast future 
#evapotranspiration for each field so that water managers can include estimates in their 
#planning. Make a plot that includes historical and forecasted evapotranspiration for the 
#crops to present to the water manager. Include a brief explanation of your autoregressive 
#models.

pistachiosN <- na.omit(pistachiosTimeSeries)
model4.pist <- arima(pistachiosN ,
                     order = c(4,0,0))
model4.pist

AR_fit4_pist <- pistachiosN - residuals(model4.pist)

plot(pistachiosN)

points(AR_fit4_pist, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)

pistachiosNN <- forecast(model4.pist)
pistachiosNN

pistachiosNNP <- data.frame(pistachiosNN)

years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
pistachiosNNP$dateF <- ymd(paste(years,"/",month,"/",1))

ggplot() +
  geom_line(data = pistachios, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(pistachios$date[1]),pistachiosNNP$dateF[24])+  # Plotting original data
  geom_line(data = pistachiosNNP, aes(x = dateF, y = Point.Forecast),
            col="red") +  
  geom_ribbon(data=pistachiosNNP, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ 
  theme_classic()+
  labs(title="Pistachios", x="Year", y="Evapotranspiration")


fallowN <- na.omit(fallowTimeSeries)
model4.fallow <- arima(fallowN ,
                       order = c(4,0,0)) 
model4.fallow

AR_fit4_fallow <- fallowN - residuals(model4.fallow)

plot(fallowN)

points(AR_fit4_fallow, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)

fallowNN <- forecast(model4.fallow)
fallowNN

fallowNNP <- data.frame(fallowNN)

years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
fallowNNP$dateF <- ymd(paste(years,"/",month,"/",1))

ggplot() +
  geom_line(data = fallow, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(fallow$date[1]),fallowNNP$dateF[24])+
  geom_line(data = fallowNNP, aes(x = dateF, y = Point.Forecast),
            col="red") +
  geom_ribbon(data=fallowNNP, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+
  theme_classic()+
  labs(title="Fallow", x="Year", y="Evapotranspiration")