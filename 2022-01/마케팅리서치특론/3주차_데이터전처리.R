# fliter 함수 실습
library(readr)
library(dplyr)

exam <- read_csv("exam.csv",
                 col_names = T,
                 col_types = cols("c", "f", "f", "i", "i", "i"),
                 na = "na",
                 locale = locale('ko', encoding = 'euc-kr'))


exam_c1 <- exam %>% filter(class == 1)

exam_male <- exam %>% filter(gender == "Male")
mean(exam_male$english, na.rm = T)

# 문제 1
exam_123 <- exam %>% filter(class %in% c(1,2,3))
mean(exam_123$math)
round(mean(exam_123$math), digits = 2)

# 문제2
exam_n4 <- exam %>% filter(class != 4) %>% filter(math>=90 | history >=95)

# 문제3
quantile(exam$english, probs = c(0.9))
exam %>% filter(english >= quantile(exam$english, probs = c(0.9)))

# select 함수 실습
exam %>% select(class, math, english)
exam %>% select(-address) %>% print(n=Inf)
exam %>% select(contains("add"))

# 문제 4
exam %>% filter(class == 1) %>% select(gender, math)

# arrange 함수 실습
exam %>% arrange(math) ## 오름차순 --> NA는 가장 마지막에 출력 됨
exam %>% arrange(-math) ## 내림차순 --> NA는 가장 마지막에 출력 됨
exam %>% arrange(class, -math) %>% print(n=Inf)

# mutate 함수 실습
exam <- exam %>% mutate(total = math+english+history, average = (math+english+history)/3)

exam <- exam %>% mutate(test = ifelse(total >= 180, "pass", "fail"))
exam$test <- ifelse(is.na(exam$total), NA, exam$test) # NA는 NA로 처리

library(descr)
freq(exam$test)

exam <- exam %>% mutate(grade = ifelse(average < 60, "fail", ifelse(average < 75, "middle", ifelse(average<90, "good", "excellent"))))

exam$test <- NULL
exam$grade <- NULL
exam <- exam %>% mutate(test = case_when(total < 180~"fail", total >= 180~"pass"))
exam <- exam %>% mutate(grade = case_when(average < 60~"fail", average < 75~"middle", average < 90~"good", average >= 90~"excellent"))

# relocate 실습
exam <- exam %>% relocate(total, .before = test)
exam <- exam %>% relocate(average, .after = test)

glimpse(exam)
exam$gender <- as.factor(exam$gender)
exam$class <- as.factor(exam$class)
exam$test <- as.factor(exam$test)
exam$grade <- as.factor(exam$grade)

exam <- exam %>% relocate(where(is.character))
exam <- exam %>% relocate(where(is.factor), .before = where(is.character))

# group_by 함수와 summarise 함수 실습
exam %>% group_by(class) %>% summarise(n(), mean(math, na.rm = T), sd(math, na.rm = T))
exam %>% group_by(class) %>% summarise(count = n(), mean_math = mean(math, na.rm = T), sd_math = sd(math, na.rm = T))

exam_new <- exam %>% group_by(class, gender) %>% summarise(count = n(), mean_history = mean(history))

# 참조: sum 함수
exam_new %>% mutate(perc = count / sum(count))  # 반별로 sum
exam_new %>% mutate(perc = count / sum(exam_new$count))  # 전체 sum : 30명
