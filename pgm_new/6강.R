# install.packages("forecast") 
# install.packages("ggplot2")
# install.packages("fGarch")
library(forecast)
library(ggplot2)
library(fGarch)

#### 그림 4-8 ARIMA(1,1,1) 모형에서 생성된 시계열의 특징
set.seed(123456)
nn = 200
arima1_sim = ts(arima.sim(list(order=c(1,1,1), ar=0.6, ma=0.6),
                          n=nn))
plot(arima1_sim, main="", xlab="", ylab="ARIMA(1,1,1)",
     col="steelblue")
acf(arima1_sim, main="", col="steelblue")
pacf(arima1_sim, main="", col="steelblue")
spectrum(arima1_sim, spans=c(3,3), main="")

#### 그림 4-13　ARCH(2) 모형 시계열의 시계열도표, 상관도표와 부분상관도표
set.seed(5)
spec = garchSpec(model = list(alpha = c(0.5, 0.4), beta = 0))
garchsim= garchSim(spec, n = 200)

garchsim %>% ggtsdisplay(main="", theme=theme_bw())
garchsim^2 %>% ggtsdisplay(main="", theme=theme_bw())

