# install.packages("forecast") 
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("tseries")
library(forecast)
library(ggplot2)
library(readxl)
library(tseries)

#### 그림 5-1　로그변환 및 로그차분변환된 상품 소비자물가지수의 추이

cpi = read_excel("./data/데이터.xlsx", sheet="소비자물가")
cpi_ts = cpi[,3] %>% ts(start=1985, frequency=12) 

cbind("log(상품 CPI)"=log(cpi_ts), "diff(log(상품 CPI))"=diff(log(cpi_ts))) %>%
  autoplot(facets=TRUE) +
  xlab("연도") + ylab("") + ggtitle("") + theme_bw()

#### 단위근 검정 <프로그램 5-1>

cpi_ts %>% log() %>% adf.test()
cpi_ts %>% log() %>% diff() %>% adf.test()

