####  4-2 AR(1)모형 생성 시계열의 특징
set.seed(123456)
nn = 200
ar1_sim = ts(arima.sim(list(order = c(1,0,0), ar = 0.6), n = nn))

plot(ar1_sim, main="AR(1)", xlab="", ylab="AR(1)=0.6", col="steelblue")

acf(ar1_sim, main="AR(1)", col="steelblue")
acf(ar1_sim, main="AR(1)", col="steelblue", xlab="", type="partial")
#pacf(ar1_sim, main="AR(1)", col="steelblue", xlab="")
spectrum(ar1_sim, spans=c(3,3), main="AR(1)")

####  4-4 MA(1)모형 생성 시계열의 특징

set.seed(123456)
nn = 200
ma1_sim = ts(arima.sim(list(order = c(0,0,1), ma = 0.6), n = nn))

plot(ma1_sim, main="MA(1)", xlab="", ylab="MA(1)=0.6", col="steelblue")
acf(ma1_sim, main="MA(1)", col="steelblue")
acf(ma1_sim, main="MA(1)", col="steelblue", xlab="", type="partial")
#pacf(ma1_sim, main="MA(1)", col="steelblue", xlab="")
spectrum(ma1_sim, spans=c(3,3), main="MA(1)")
