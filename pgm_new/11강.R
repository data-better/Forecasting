library(seasonal)
library(readxl)

# <프로그램 7-1> GDP의 계절조정

GDP = read_excel("./data/데이터.xlsx", sheet="GDP")  
gdp = GDP[,3] %>% ts(start=1960, frequency=4) %>% window(start=1970)

# X-13AIMA-SETAS에 의한 계절조정
sa_gdp= seas(gdp/1000, x11.appendfcst = "yes", regression.variables=NULL, regression.aictest = NULL, x11 = list())

# 모형 정리
summary(sa_gdp)

# X-13AIMA-SETAS 프로그램 보여주기
spc(sa_gdp)

# X-13AIMA-SETAS 결과물 출력(HTML)
out(sa_gdp)

# X-13AIMA-SETAS 계절조정계열 저장
gdp_sa_n = final(sa_gdp)

# X-13AIMA-SETAS 계절조정계열 그래프 그리기
plot(sa_gdp)