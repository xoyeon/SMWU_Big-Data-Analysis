install.packages("readr")
library(readr)
exam <- read_csv('exam.csv',
                 col_names=T,
                 col_types = cols("c","f","f","i","i","i"),
                 na="na",
                 locale = locale('ko', encoding='euc-kr'))

# 데이터 탐색: glimpse, str, summary
install.packages(dplyr)
install.packages('dplyr')

library(dplyr)
glimpse('exam')
head(exam)
str(exam)
summary(exam)

# 빈도수 구하기
table(exam$address)
install.packages('descr')

library(descr)
freq(exam$address)

install.packages('ggplot2')
library(ggplot2)
qplot(data = exam, address)
qplot(data=exam, address, fill=gender)
qplot(data=exam, class, fill=gender)

# 기술통계량 구하기
summary(exam$math)
table(is.na(exam$math))

mean(exam$math,
        na.rm=T)

install.packages('psych')
library(psych)

describe(exam$math)
describe(exam$english)
describe(exam$history)

# 히스토그램 그리기
hist(exam$english, breaks = seq(0, 100, by=10))
hist(exam$english, breaks = seq(0, 100, by=5))

hist(exam$history, breaks = seq(0, 100, by=10))
hist(exam$history, breaks = seq(0, 100, by=5))

# 논리 연산자와 비교 연산자 #
library(descr)
freq(exam$address == "원효로")
table(exam$address == "원효로")
freq(exam$gender != "Female")
table(exam$gender != "Female")

freq(exam$math == 50)
freq(exam$math !=50)
freq(exam$math >= 50)
freq(exam$math < 50)

freq(exam$english <= 50 & exam$history >= 80)
freq(exam$math >= 90 | exam$history >= 90)
freq(exam$address == "효창동" | exam$address == "청파동" | exam$address == "서계동")
freq(exam$address %in% c("효창동", "청파동", "서계동"))