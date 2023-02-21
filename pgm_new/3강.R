# 프로그램 2-1
#install.packages("readxl")

library(readxl)

# 프로그램 2-3

gdp = read_excel("./data/데이터.xlsx", sheet="GDP")
 gdp_ts = ts(gdp[,2:3]/1000, start=1960, frequency=4) 
 dlgdp_1 = diff(log(gdp_ts[,2]))
 dlgdp_4 = diff(log(gdp_ts[,2]), 4)
 dlgdp = cbind(dlgdp_1, dlgdp_4)
 spectrum(na.omit(dlgdp), spans=c(3,3), col=c("red", "steelblue"), 
           main="", lty=c(20,1), lwd=1.5)
 legend("topright", col=c("steelblue", "red"), lty=c(1,20), lwd=1.5, 
         c("1차 차분", "4차 차분"), bty = "n")

