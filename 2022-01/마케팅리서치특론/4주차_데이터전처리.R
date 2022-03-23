library(readr)
library(dplyr)

# 변수 배열 순서 정리
exam <- exam %>% relocate(address, gender, class, math, history, english, total, test, average, grade)

# id 변수 만들기
exam <- exam %>% mutate(id = c(1:30)) %>% relocate(id)

# exam_science.csv 불러오기
exam_science <- read_csv("exam_science.csv")

# left_join 함수로 두 개 df 통합
exam <- left_join(exam, exam_science, by = 'id')
exam$id <- NULL
exam$science <- NULL

# 기준변수명이 다른 경우 통합
exam <- exam %>% mutate(ID = c(1:30)) %>% relocate(ID)
exam <- left_join(exam, exam_science, by = c("ID" = "id"))

# exam_add.csv 불러오기
exam_add <- read_csv("exam_add.csv", locale = locale('ko', encoding='euc-kr'))

# 변수명과 척도 일치시키기
exam <- exam %>% rename(id = ID)
glimpse(exam)
glimpse(exam_add)
exam_add$gender <- as.factor(exam_add$gender)
exam_add$class <- as.factor(exam_add$class)
exam_add$test <- as.factor(exam_add$test)
exam <- bind_rows(exam, exam_add)


# exam에서 NA변경하기(average, grade)
exam$average <- ifelse(is.na(exam$average), exam$total/3, exam$average)
exam <- exam %>% mutate(grade = case_when(average < 60~"fail", average < 75~"middle", average < 90~"good", average >= 90~"excellent"))

# 중복된 id 확인 및 동일 사례 제거
n_distinct(exam$id)
exam %>% group_by(id) %>% summarise(count = n()) %>% arrange(-count)
exam <- exam %>% distinct(id, .keep_all = T)

# 이상치 확인 및 NA로 대체
exam <- exam %>% relocate(science, .after = english)
table(exam[5:8] > 100)
table(exam$math > 100)
table(exam$history > 100)
table(exam$english > 100)
table(exam$science > 100)
exam$science <- ifelse(exam$science > 100, NA, exam$science)

# 정해진 범위 안에 들어왔을 때 이상치 확인
library(psych)
descr <- describe(exam[5:8])
## 상한
descr$mean + 2.57583*descr$sd
## 하한
descr$mean - 2.57583*descr$sd
descr <- descr %>% mutate(low = mean - 2.57583*sd, upper = mean + 2.57583*sd)
