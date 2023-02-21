library(mFilter)
library(readxl)

# <프로그램 7-2> GDP의 분해

GDP = read_excel("./data/데이터.xlsx", sheet="GDP")  
GDP_ts = GDP[,2:3] %>% ts(start=1960, frequency=4) %>% window(start=1970)
 gdp_o = GDP_ts[,2]
 gdp_sa = GDP_ts[,1]

# GDP 변동요인 분해 
lgdp.hp = mFilter(log(gdp_sa),filter="HP")   
 gdp_t = exp(lgdp.hp$trend)
gdpsam = exp((log(gdp_sa)+stats::lag(log(gdp_sa),-1)  
                +stats::lag(log(gdp_sa),1))/3)
gdp_s = gdp_o/gdp_sa*100
gdp_i = gdp_sa/gdpsam*100
gdp_c = gdpsam/gdp_t*100

# GDP 변동요인 그래프 작성

par(mfrow=c(2,2))
 plot(gdp_t, main="추세변동", col="steelblue")
 plot(gdp_c, main="순환변동", col="steelblue")
 plot(gdp_s, main="계절변동", col="steelblue")
 plot(gdp_i, main="불규칙변동", col="steelblue")
