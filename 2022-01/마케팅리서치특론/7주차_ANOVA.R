# 실습예제 1

## STEP1: 가설수립
### 사전검토
anova1 <- ttest

library(dplyr)
anova1 %>% group_by(priority) %>% summarise(mean(price, na.rm = T))

### 이상치 검토 및 제거
library(psych)
descr <- describe(anova1$price)
descr <- descr %>% mutate(UL = mean + 2*sd)
descr <- descr %>% mutate(UL = mean - 2*sd)
table(anova1$price > descr$UL)


anova1_new <- anova1 %>% filter(price <= descr$UL)

### Critical과 High를 High로 통합하여 prior 새변수 만들기
install.packages("forcats")
library(forcats)
anova1_new <- anova1_new %>% mutate(prior = fct_collapse(priority, "High" = c("Critical", "High")))
anova1_new %>% group_by(prior) %>% summarise(mean(price, na.rm = T))

## STEP2: 집단간 서브 데이터프레임 만들기
anova1_H <- anova1_new %>% filter(prior == "High")
anova1_M <- anova1_new %>% filter(prior == "Medium")
anova1_L <- anova1_new %>% filter(prior == "Low")
anova1_N <- anova1_new %>% filter(prior == "Not Specified")

## STEP3: 정규성 검토
summary(anova1_H$price)
summary(anova1_M$price)
summary(anova1_L$price)
summary(anova1_N$price)

hist(anova1_H$price, breaks = seq(0, 600, 20))
hist(anova1_M$price, breaks = seq(0, 600, 20))
hist(anova1_L$price, breaks = seq(0, 600, 20))
hist(anova1_N$price, breaks = seq(0, 600, 20))

shapiro.test(anova1_H$price)
shapiro.test(anova1_M$price)
shapiro.test(anova1_L$price)
shapiro.test(anova1_N$price)

## STEP4: 등분산성 조건
install.packages("car")
library(car)
leveneTest(price~prior, data = anova1_new)

## STEP5: ANOVA 검정
anova1_result <- aov(price~prior, data = anova1_new)
summary(anova1_result)


# 실습예제2\

## STEP1: 가설수립
### 사전검토
anova2 <- pttest
anova2 %>% group_by(payment) %>% summarise(mean(expense, na.rm = T))

#### HO: 뮤(simple) = 뮤(account) = 뮤(credit)

### 이상치 검토 및 제거
descr <- describe(anova2$expense)
descr <- descr %>% mutate(LL = mean - 2*sd)
descr <- descr %>% mutate(UL = mean + 2*sd)
table(anova2$expense > descr$UL)
anova2_new <- anova2 %>% filter(expense <= descr$UL)
anova2_new %>% group_by(payment) %>% summarise(mean(expense, na.rm = T))

## STEP2: 집단간 서브 데이터프레임 만들기
anova2_simple <- anova2_new %>% filter(payment == "간편결제")
anova2_account <- anova2_new %>% filter(payment == "계좌이체")
anova2_credit <- anova2_new %>% filter(payment == "시뇽카드")
table(is.na(anova2_new$payment))

## STEP3: 정규성 검토
summary(anova2_simple$expense)
summary(anova2_account$expense)
summary(anova2_credit$expense)

hist(anova2_simple$expense, breaks = seq(0, 2000, 50))
hist(anova2_account$expense, breaks = seq(0, 2000, 50))
hist(anova2_credit$expense, breaks = seq(0, 2000, 50))

shapiro.test(anova2_simple$expense)
shapiro.test(anova2_account$expense)
shapiro.test(anova2_credit$expense)

## STEP4: 등분산성 조건
leveneTest(expense~payment, data = anova2_new)

## STEP5: ANOVA 검정
anova2_result <- aov(expense~payment, data = anova2_new)
summary(anova2_result)

### 유의수준 0.05
oneway.test(expense~payment, data = anova2_new)

## STEP6: 사후분석
install.packages("agricolae")
library(agricolae)
duncan.test(anova2_result, "payment", console = T)
