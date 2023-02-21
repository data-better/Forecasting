# install.packages("forecast") 
# install.packages("ggplot2")
# install.packages("readxl")
library(forecast)
library(ggplot2)
library(readxl)

#### 그림 3-2 백색잡음계열과 시계열도표, 상관도표와 부분상관도표

set.seed(123456)
nn = 52*4
wn = ts(rnorm(nn), start=1970, frequency=4)

plot(wn, main="", xlab="", ylab="백색잡음", col="steelblue")
 abline(h=0, lty=2, col="gray")

acf(wn, main="", col="steelblue", xlab="")
pacf(wn, main="", col="steelblue", xlab="")

Box.test(wn, lag=8, type="Ljung")
wn %>% ggtsdisplay(main="", theme=theme_bw())

#### 그림 3-7 GDP 로그차분계열의 시계열도표, 상관도표와 부분상관도표

gdp = read_excel("./data/데이터.xlsx", sheet="GDP")
gdp_ts = ts(gdp[,3]/1000, start=1960, frequency=4)
dlgdp_ts =  window(diff(log(gdp_ts)), start=2000)
dlgdp_ts %>% ggtsdisplay(main="", theme=theme_bw())

