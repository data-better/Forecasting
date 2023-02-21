library(tidyverse)
library(readxl)

# <프로그램 6-1> 동행지수 순환변동치와 선행지수 순환변동치간 교차상관도표 작성
bc = read_excel("./data/데이터.xlsx", sheet="경기종합지수")
bc_ts = bc[,2:3] %>% ts()
ccf(bc_ts[,1], bc_ts[,2], lag=12, main=" ")

# <프로그램 6-2> 경제성장률 모형의 추정
gdpf = read_excel("./data/데이터.xlsx", sheet="GDP예측")
gdpf_ts = gdpf[,2:7] %>% ts(start=2000, frequency=4) 
gdp_r = gdpf_ts[,4]
ipi_r = gdpf_ts[,5]
sbi_r = gdpf_ts[,6]
gr1 = ts.union(gdp_r, ipi_r, sbi_r)
plot(gr1, main="", xlab="", col="steelblue")
gdp_r_lm = lm(gdp_r~ipi_r+sbi_r, data=gr1)
summary(gdp_r_lm)
plot(gdp_r, col="steelblue", ylim=c(-2,10), ylab="", xlab="")
 lines(ts(predict(gdp_r_lm), start=c(2001,1), freq=4), col=2)
 legend("topright", col=c(1, 2), lty=1, c("GDP 성장률", "추정값"),  bty = "n")