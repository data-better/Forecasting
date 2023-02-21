# 프로그램 2-1
#install.packages("readxl"); install.packages("zoo")

library(readxl)
library(zoo)
gdp = read_excel("./data/데이터.xlsx", sheet="GDP")
 date_q = seq(as.Date("1960-01-01"), as.Date("2022-04-01"), "quarter") 
gdp_zoo = zoo(gdp[,2:3], date_q)
head(gdp_zoo)

# 프로그램 2-2
plot(gdp_zoo/1000, screens=1, col=c(2,1), ylab="GDP(조 원)", xlab="")
 legend("topleft", col=c(1, 2), lty=1, 
        c("원계열", "계절조정계열"),bty = "n"
        
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

