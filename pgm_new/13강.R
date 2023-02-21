library(tseries)
library(vars)

# VAR모형의 추정

m1gdp = read.ts("./data/m1gdp.csv", start=1971, frequency=4,
                 header=TRUE,sep=",")
VAR(m1gdp, p=4, type="const")

#  VAR 모형을 이용한 인과관계검정 
var = VAR(m1gdp, p=4, type="const")
 causality(var, cause="M1")
 causality(var, cause="GDP")
 
# VAR 모형을 이용한 충격반응함수의 도출 
irf.var = irf(var, n.ahead=8, ortho=TRUE, boot=FALSE)
plot(irf.var, col="purple")  
 
# VAR 모형을 이용한 예측오차 분산분해
fevd.var = fevd(var, n.ahead=8, ortho=TRUE, boot=FALSE)
plot(fevd.var)

# VAR 모형을 이용한 예측
predict.var = predict(var, n.ahead=12, ci=0.95)
plot(predict.var)