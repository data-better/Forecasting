library(forecast)
library(tidyverse)
library(readxl)

## <프로그램 5-2> 로그 변환된 월별 종합주가지수에 대한 ARIMA모형 작성

# 데이터 불러오기
kospi = read_excel("./data/데이터.xlsx", sheet="주가지수")
kospi_ts = kospi[,2] %>% ts(start=2004, frequency=12)  %>% window(start=2010)

# ARIMA 모형의 식별
kospi_ts  %>% log %>% ggtsdisplay(main="", theme=theme_bw()) 
kospi_ts  %>% log %>% diff %>% ggtsdisplay(main="", theme=theme_bw())

# ARIMA 모형의 추정
kospi_ts %>% log %>% Arima(order=c(1,1,1))
kospi_ts  %>% log %>% auto.arima(seasonal = FALSE)

# ARIMA 모형의 과대적합 검토
kospi_ts  %>% log %>% Arima(order=c(1,1,1))
kospi_ts  %>% log %>% Arima(order=c(2,1,1))
kospi_ts  %>% log %>% Arima(order=c(1,1,2))

# ARIMA 모형의 진단
kospi_ts %>% log %>% Arima(order=c(1,1,1)) %>%
  checkresiduals(main="", theme=theme_bw())

# ARIMA 모형의 예측
kospi_ts %>% log %>% Arima(order=c(1,1,1)) %>% 
  forecast(h=12)  %>% plot(main="")

## <프로그램 5-3> 일별 종합주가지수 로그수익률에 대한 GARCH모형의 작성

library(quantmod)
library(rugarch)
# 종합주가지수 불러오기 : 2022-08-31까지
getSymbols("^KS11", from=as.Date("2007-01-01"),
             to=as.Date("2022-08-31"))
kospi =KS11$ KS11.Close
r.kospi = kospi %>% log %>% diff  # 로그차분

# GARCH 모형 설정
spec3 = ugarchspec(variance.model=list(model="sGARCH", 
                                         garchOrder=c(1,1)),mean.model=list(armaOrder=c(0,0), 
                                                                            include.mean=TRUE),distribution.model="norm")
# GARCH 모형 추정
fit = ugarchfit(data = na.omit(r.kospi), spec = spec3)
plot(fit) # 메뉴중 3:Conditional SD 선택, 종료 0

# GARCH 모형 예측
forc = ugarchforecast(fit, n.ahead=50)
plot(forc) # 메뉴중 Sigma Prediction (unconditional) 선택
