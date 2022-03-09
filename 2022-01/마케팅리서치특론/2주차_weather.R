# weather.csv 파일 실습하기
## 문제1
library(readr)
weather <- read_csv("weather.csv",
                    col_names = T,
                    locale = locale('ko', encoding = 'euc-kr'))

## 문제2 & 문제3
library(dplyr)
glimpse(weather)
summary(weather)

weekdays(as.Date("2020-03-08"))
weather$요일 <- weekdays(weather$일시)
weather$일시 <- as.factor(weather$일시)

## 문제4
weather$요일 <- as.factor(weather$요일)

## 문제5
var(weather$일강수량, na.rm = T)

## 문제6
library(psych)
descr_result <- describe(weather)
descr_result$cv <- descr_result$sd / descr_result$mean

## 문제7
library(descr)
freq(weather$요일)
str(weather)
weather$요일 <- factor(weather$요일, levels = c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일", "일요일"))

## 문제8
library(ggplot2)
qplot(data = weather, 요일, fill = 요일구분)

##문제10
hist(weather$평균기온, breaks = seq(-25, 50, 1))

##문제11
freq(weather$평균기온 >= 10 & weather$평균기온 <= 20)

##문제12
freq(is.na(weather$일강수량))

##문제13
freq(weather$요일 %in% c("월요일", "화요일"))

##문제14
freq(weather$최고기온 > 30 & weather$평균상대습도 > 80)

##문제15
freq(weather$최저기온 < -10 | weather$합계일조시간 < 1)

#3문제16
library(dplyr)
weather <- weather %>% rename(평균기압 = 평균현지기압)

##문제17
weather$요일구분 <- factor(weather$요일구분, levels = c("휴일", "평일"))

##문제18
weather$일강수량 <- ifelse(weather$일강수량 == 0, NA, weather$일강수량) 

##문제19
library(descr)
freq(is.na(weather$평균기압))

##문제20
mean(weather$평균기압, na.rm = T)

##문제21
weather$평균기압 <- ifelse(is.na(weather$평균기압), 1006.26, weather$평균기압)
