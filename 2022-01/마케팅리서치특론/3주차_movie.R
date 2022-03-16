# movie 데이터프레임 실습
library(readr)
library(dplyr)
movie <- read_csv("imdb.csv", col_names = T)

names(movie) <- tolower(names(movie)) ## 대문자로 바꾸려면  toupper 함수 사용

# 문제 1
movie1 <- movie %>% filter(year %in% c(2018:2020))
round(mean(movie$runtime), digits = 2)

# 문제 2
library(stringr)
movie2 <- movie %>% filter(certificate == "A") %>% filter(str_detect(genre, "Drama")) %>% arrange(-imdb_rating)

# 문제 3
movie3 <- movie %>% filter(str_detect(genre, "Drama") & str_detect(overview, "crime"))

# 문제 4
movie3 %>% filter(meta_score >= quantile(movie3$meta_score, probs = c(0.9),na.rm = T)) %>% select(title)

# 문제 5
movie5 <- movie %>% select(contains("star") | contains("dir"))

# 문제 6
library(descr)
movie6 <- freq(movie5$star1)

# 문제 7
movie <- movie %>% mutate(score = 10*imdb_rating + meta_score)

# 문제 8
movie <- movie %>% mutate(class = case_when(score <= 120~"D", score <= 130~"C", score <= 160~"B", score <= 180~"A", score > 180~"S"))

# 문제 9
freq(movie$class)
movie$class <- factor(movie$class, levels = c("S", "A", "B", "C", "D"))

# 문제 10
movie %>% group_by(class) %>% summarise(n(), mean(gross, na.rm=T))

# 문제 11
n_distinct(movie$director)
movie %>% group_by(director) %>% summarise(number = n()) %>% arrange(-number) %>% head(10)

n_distinct(movie$imdb_rating)
