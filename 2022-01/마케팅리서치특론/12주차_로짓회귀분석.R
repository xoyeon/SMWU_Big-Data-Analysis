library(readr)
library(dplyr)

# 문제1: 데이터프레임 만들고 변수명 변경하기
HRA <- read_csv("HR attrition.csv")
HRA <- HRA %>% rename(satisfaction = satisfaction_level,
                      evaluation = last_evaluation,
                      projects = number_of_projects,
                      hours = average_monthly_hours,
                      years = years_at_company,
                      accident = work_accident,
                      promotion = promotion_last_5years)

# 문제2: 범주형 척도로 변경하기
str(HRA)
HRA$accident <- as.factor(HRA$accident)
HRA$promotion <- as.factor(HRA$promotion)

table(HRA$department)
HRA$department <- as.factor(HRA$department)
HRA$salary <- as.factor(HRA$salary)

# 문제3: 전체 데이터프레임에서 결측치와 다섯개 계량형 변수에 대해서 이상치 확인하기
summary(HRA)
library(psych)
descr <- describe(HRA[1:5])
descr <- descr %>% mutate(UL = mean + 3*sd, LL = mean - 3*sd)
table(HRA$years > 7.878642)

# 문제4: 여섯개 변수의 상관관계를 확인한 후, 다섯개 연구가설 수립하기
HRA <- HRA %>% relocate(left, .before = satisfaction)
corr.test(HRA[1:6], method = "pearson", alpha = 0.05, use = "pairwise.complete.obs")

## H1: satisfaction -> left(-)
## H2: evaluation -> left(+/-)
## H3: project -> left(+/-)
## H4: hours -> left(+)
## H5: years -> left(+/-)

# 문제5: 로짓회귀식(logit1) 수립하기
HRA$left <- as.factor(HRA$left)
HRA <- HRA %>% mutate(id = c(1:14999))

install.packages("mlogit")
library(mlogit)
logit1 <- glm(left~satisfaction + evaluation + projects + hours + years, data = HRA, family = binomial())

# 문제6: 이상치 제거 및 가설검정
library(car)
outlierTest(logit1)
HRA <- HRA %>% filter(id != 844)
logit1 <- glm(left~satisfaction + evaluation + projects + hours + years, data = HRA, family = binomial())
summary(logit1)
## H3만 기각되고, 나머지(연구/대립) 가설은 채택

# 문제7: 모형적합도 확인하기
install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(logit1$y, logit1$fitted.values)
install.packages("DescTools")
library(DescTools)
PseudoR2(logit1, which = c("CoxSnell", "Nagelkerke"))

# 문제8: 가설 네 개 추가 수립하기
## H5: accident -> left(+)
## H6: promotion -> left(-)
## H7: High salary에 비해서 low -> left(+)
## H8: High salary에 비해서 medium -> left(+)

# 문제9: 변수 추가된 새로운 로짓회귀식(logit2) 수립 및 이상치 제거
logit2 <- glm(left~satisfaction + evaluation + projects + hours + years + accident + promotion + salary, data = HRA, family = binomial())
library(car)
outlierTest(logit2)
HRA <- HRA %>% filter(id != 1001)

logit1 <- glm(left~satisfaction + evaluation + projects + hours + years, data = HRA, family = binomial())
logit2 <- glm(left~satisfaction + evaluation + projects + hours + years + accident + promotion + salary, data = HRA, family = binomial())

# 문제10: 두 개 모형 비교
1-pchisq(logit1$deviance-logit2$deviance, logit1$df.residual-logit2$df.residual)

# 문제11: logit2를 기준으로 가설검정
summary(logit2)

# 문제12: 모형적합도 확인하기
hoslem.test(logit2$y, logit2$fitted.values)
PseudoR2(logit2, which = c("CoxSnell", "Nagelkerke"))

# 문제13: hit ratio 구하기
predictor <- predict(logit2, newdata = HRA) ### logit value(Yi hat)
predictor <- ifelse(predictor < 0, 0, 1)
predictor  ### numeric

predictor <- as.factor(predictor) ### 범주형

install.packages("caret")
library(caret)
confusionMatrix(HRA$left, predictor)

# 문제14: 새로운 사례에 대한 이직여부 예측하기
C15000 <- data.frame(satisfaction = 0.57, evaluation = 0.76, projects = 4, hours = 234, years = 4, accident = 0, promotion = 0, department = "technical", salary = "medium", id = 15000)
str(C15000)

C15000$accident <- as.factor(C15000$accident)
C15000$promotion <- as.factor(C15000$promotion)
C15000$department <- as.factor(C15000$department)
C15000$salary <- as.factor(C15000$salary)

predict(logit2, C15000)
## logit value = -1.056이므로 Pr = 0으로 분류. 즉 이직하지 않는 것으로 예측

# 추가문제: department별로 구분하여 로짓회귀분석 수행하기 (sales 부서)
table(HRA$department)
HRA_sales <- HRA %>% filter(department == "sales")

## 문제 3부터 반복 수행
logit2_sales <- glm(left~satisfaction + evaluation + projects + hours + years + accident + promotion + salary, data = HRA_sales, family = binomial())
summary(logit2_sales)
