ttest <- read.csv("ttest.csv")

library(dplyr)
glimpse(ttest)

n_distinct(ttest$priority)
table(ttest$priority)

# 척도 변경하기
ttest$priority <- as.factor(ttest$priority)
ttest$shipping <- as.factor(ttest$shipping)
ttest$customer <- as.factor(ttest$customer)
ttest$category <- as.factor(ttest$category)
ttest$container <- as.factor(ttest$container)

# 범주형 척도 변수 빈도 확인하기
library(descr)
freq(ttest$priority)
freq(ttest$shipping)
freq(ttest$customer)
freq(ttest$category)
freq(ttest$container)

# 변수 위치 조정하기
ttest <- ttest %>% relocate(where(is.factor))
ttest <- ttest %>% relocate(margin, .before = name)
descr <- descr %>% mutate(LL = mean - 2*sd)
descr <- descr %>% mutate(UL = mean + 2*sd)


# 이상치 검토하기
summary(ttest)
library(psych)
descr <- describe(ttest[c(6:10)])
table(ttest$sales > 8945.98)
table(ttest$price > 670.06)
table(ttest$cost > 47.37)
table(ttest$margin > 0.7837)
 
# 이상치 제외한 데이터 프레임 생성하기
ttest_new <- ttest %>% filter(sales <= 8945.98, price <= 670.06, cost <= 47.37, margin <= 0.7837)


# STEP2: 집단간 데이터 프레임 만들기
ttest_new %>% group_by(customer) %>% summarise(n(), mean(sales))
ttest_HO <- ttest_new %>% filter(customer == "Home Office")
ttest_CS <- ttest_new %>% filter(customer == "Consumer")

# STEP3: 정규성 검토
summary(ttest_HO$sales)
hist(ttest_HO$sales, breaks = seq(0, 9000, 50))
summary(ttest_CS$sales)
hist(ttest_CS$sales, breaks = seq(0, 9000, 50))
shapiro.test(ttest_HO$sales)
shapiro.test(ttest_CS$sales)

# 정규성 조건을 만족하지 않으므로 자연로그 변환하기
ttest_HO <- ttest_HO %>% mutate(lnsales = log(sales))
ttest_CS <- ttest_CS %>% mutate(lnsales = log(sales))
hist(ttest_HO$lnsales, breaks = seq(0, 10, 0.1))
hist(ttest_CS$lnsales, breaks = seq(0, 10, 0.1))
shapiro.test(ttest_HO$lnsales)
shapiro.test(ttest_CS$lnsales)

# STEP4: 등분산성 검토
var.test(ttest_HO$lnsales, ttest_CS$lnsales)
var.test(ttest_HO$sales, ttest_CS$sales)

# STEP5: 등분산 가정 독립표본 t검정/가설검정
t.test(ttest_HO$lnsales, ttest_CS$lnsales, alternative = "two.sided", var.equal = T)
t.test(ttest_HO$sales, ttest_CS$sales, alternative = "two.sided", var.equal = F)

# 대응표본 t검정 
pttest <- read.csv("pttest.csv", local('ko', encoding='euc-kr'))
           
# STEP2: 차이 변수 만들기        
library(dplyr)
pttest <- pttest %>% mutate(d = morning - weekend)

# STEP3: d의 정규서 ㅇ검토
shapiro.test(pttest$d)
summary(pttest$d)
hist(pttest$d, breaks = seq(-15, 8, 1))

## 이상치 확인
describe(pttest$d) ## LL = -9.54 & UL = 5.74
table(pttest$d < -9.54)
table(pttest$d > 5.74)

# STEP4: 대응표본 t검정 통한 가설검정
## 이상치 존치
t.test(pttest$morning, pttest$weekend, alternative = "two.sided", paired = T)

## 대립가설 채택, 한 달 동안 주말배송 주문평균 > 새벽배송 주문평균