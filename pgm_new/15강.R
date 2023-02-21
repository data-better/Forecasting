
library(tidyverse)
library(readxl)
GDP = read_excel("./data/데이터.xlsx", sheet="GDP")  

# 윈터스의 승법형 지수평활법에 의한 GDP의 예측
gdp_ts = GDP[,3] %>% ts(start=1960, frequency=4) %>% 
  window(start=2000)
gdp_hw_fit = hw(gdp_ts/1000,seasonal="mult", fan=TRUE, h=12)
summary(gdp_hw_fit)
autoplot(gdp_ts/1000) +  autolayer(gdp_hw_fit) +
  xlab("") +  ylab("GDP(조원)") + theme_bw()

# 신경망에 의한 예측
library(tsibble)
library(fable)

gdp_ts = GDP[,3] %>% ts(start=1960, frequency=4) %>% 
         window(start=1970)  %>% as_tsibble
names(gdp_ts)[2] = "GDP" 
gdp_ts[,2] = gdp_ts[,2]/1000
fit = gdp_ts %>% model(NNETAR(GDP, p=4, P=1, lambda=0))
forc = fit %>% forecast(h = 12) 
forc %>% autoplot(gdp_ts) + 
  labs(x = "NNETAR", y = "GDP", title = "") + theme_bw()

# 프로펫에 의한 GDP의 예측

library(tsibble)
library(fable.prophet)

gdp_ts = GDP[,3] %>% ts(start=1960, frequency=4) %>% 
        window(start=1970)  %>% as_tsibble
names(gdp_ts)[2] = "GDP" 
gdp_ts[,2] = gdp_ts[,2]/1000

gdp_fit = gdp_ts %>% 
  model( mdl = prophet(GDP ~ growth("linear") + 
                         season("year", type = "multiplicative")))
gdp_fc = gdp_fit %>% forecast(h = "3 years")
gdp_fc %>% autoplot(gdp_ts) + xlab("") + theme_bw()

# 예측조합에 의한 예측

library(forecastHybrid)
co2 = read_excel("./data/데이터.xlsx", sheet="CO2")  
co2_ts = co2[,2] %>% ts(start=c(1999,1), frequency=12)
plot(co2_ts, xlab="", ylab="CO2(ppm)", col="steelblue")

co2_train = co2_ts %>% window(end=c(2019,12))
co2_test  = co2_ts %>% window(start=c(2020,1))
h1         = length(co2_test)

Model_1 = hybridModel(co2_train, models = "aes",
                        weights="equal", lambda=0)
fc_1 = forecast::forecast(Model_1, h=h1)
plot(fc_1, main="(a) 동일 가중")
  lines(co2_ts, col=1)
  
  
