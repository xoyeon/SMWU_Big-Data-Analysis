# imdb 데이터분석 실습
library(readr)
movie <- read_csv("imdb.csv", col_names = T)

library(dplyr)
glimpse(movie)
movie$Certificate <- as.factor(movie$Certificate)
movie$Genre <- as.factor(movie$Genre)
movie$Released_Year <- as.integer(movie$Released_Year)

install.packages("stringr")
library(stringr)
movie$Runtime <- str_replace_all(string = movie$Runtime,
                                 pattern = " min",
                                 replacement = "")

movie$Runtime <- as.integer(movie$Runtime)

glimpse(movie)

# 데이터 탐색적 분석
summary(movie)

library(descr)
freq(movie$Certificate)
library(ggplot2)
qplot(data = movie,Released_Year, fill = Certificate)

library(psych)
describe(movie$Meta_score)
describe(movie$IMDB_Rating)

# 분산계수 : 표준편차/평균, MS = 12.38/77.97, IMDB = 0.28/7.95
12.38 / 77.97
0.28 / 12.38

hist(movie$Meta_score, breaks = seq(0,100,1))
hist(movie$IMDB_Rating, breaks = seq(0,10,0.1))
table(movie$Released_Year == 2019 & movie$Meta_score > 95)
table(is.na(movie$Gross))

# 변수명 바꾸기
movie <- movie%>% rename(Title = Series_Title,
                 Year = Released_Year)
movie$var1 <- movie$Year
movie$var1 <- NULL

# 변수의 측정값 바꾸기
movie$Running <- movie$Runtime
movie$Running <- ifelse(movie$Running > 200, "Long", "Not Long")
table(movie$Running == "Long")
table(is.na(movie$Gross))
pairs(movie[15:16])
mean(movie$Gross / movie$No_of_Votes, na.rm = T)
movie$Gross <- ifelse(is.na(movie$Gross), 217.1, movie$Gross)

table(movie$Gross == 217.1)
