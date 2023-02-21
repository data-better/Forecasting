library(tidyverse)
library(readxl)
library(car)
library(forecast)

# <프로그램 6-3> 더빈-왓슨 검정과 자기상관 조정 회귀모형의 작성

gdpf = read_excel("./data/데이터.xlsx", sheet="GDP예측")
 gdpf_ts = gdpf[,2:7] %>% ts(start=2000, frequency=4) 
 gdp_r = gdpf_ts[,4]
 ipi_r = gdpf_ts[,5]
 sbi_r = gdpf_ts[,6]
 gr1 = ts.union(gdp_r, ipi_r, sbi_r)
 
gdp_r_lm = lm(gdp_r~ipi_r+sbi_r, data=gr1)

durbinWatsonTest(gdp_r_lm)
gr2 = ts.union(gdp_r, ipi_r, sbi_r, 
                 gdp_r_1=stats::lag(gdp_r,-1))
gdp_r_lmc = lm(gdp_r~ipi_r+sbi_r+gdp_r_1, data=gr2)
summary(gdp_r_lmc)

# <프로그램6-4> 일반화 시계열 회귀모형을 이용한 예측

fit = auto.arima(gdp_r, xreg=cbind(ipi_r, sbi_r))
summary(fit)
checkresiduals(fit)
fcast = forecast(fit, xreg=cbind( rep(mean(ipi_r[87:90], 4)),  
                                    rep(mean(sbi_r[87:90]), 4) ), h=4)
plot(fcast, main="")


