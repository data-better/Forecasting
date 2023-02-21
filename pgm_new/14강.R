library(tseries)
library(vars)

tb35y=read.ts("./data/rate35y.csv", start=1996, frequency=12, header=TRUE, sep=",")
plot(tb35y)

#  공적분 검정
vecm = ca.jo(tb35y, type="trace", ecdet="const", K=3)
summary(vecm)

#  VAR 모형으로의 전환
var.vecm = vec2var(vecm, r=1)
var.vecm 

# 벡터 오차수정모형을 이용한 예측
predict.vecm = predict(var.vecm, n.ahead = 12, ci=0.95)
plot(predict.vecm)
