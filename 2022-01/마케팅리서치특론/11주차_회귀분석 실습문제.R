library(readr)
library(dplyr)

## . 데이터 전처리 관련 문제
# 1. bike.csv를 불러와서 bike 데이터프레임을 만드시오.
bike <- read_csv("bike.csv")
glimpse(bike)

# 2. 문자형척도 변수 네 개를 왼편으로 이동시키시오.
bike <- bike %>% relocate(where(is.character))

# 3. 문자형척도 변수 세 개와 model_year에 대해 빈도수를 확인하시오.
table(bike$model_name)
n_distinct(bike$model_name)
table(bike$owner)
table(bike$location)
n_distinct(bike$location)
table(bike$model_year)

# 4. owner 변수의 척도를 범주형 척도로 변경하시오.
# 5. owner 변수의 범주 중에서 ‘third owner’와 ‘fourth owner or more’를 ‘third owner or more’로 통합하시오. 
bike$owner <- as.factor(bike$owner)
library(forcats)
bike$owner <- fct_collapse(bike$owner, "third owner or more" = c("third owner", "fourth owner or more"))
table(bike$owner)

# 6. 계량형 척도로 측정된 다섯 개 변수(model_year 제외)에 대해 mean ± 3sd를 벗어난 이상치를 제거하시오. 
# 7. bike 데이터프레임에서 NA를 확인하시오.
library(psych)
descr <- describe(bike[5:9])
descr <- descr %>% mutate(UL = mean + 3*sd)
descr <- descr %>% mutate(LL = mean - 3*sd)

bike <- bike %>% filter(kms_driven <= 112639.98027)
bike <- bike %>% filter(mileage <= 89.86444)
bike <- bike %>% filter(power <= 72.30192)
bike <- bike %>% filter(price <= 545572.20872)

table(is.na(bike))

## Ⅱ. 상관관계 확인과 가설수립 및 전제조건 확인하기
# 8. 계량형 척도로 측정된 다섯 개 변수에 대해 상관관계를 확인하시오. 
corr.test(bike[5:9], method = "pearson", alpha = 0.05, use = "pairwise.complete.obs")
pairs.panels(bike[5:9])

# 9. 종속변수로 고려하고 있는 price에 대한 정규성을 검토하시오. price에 대해 자연로그를 취할 경우 정규성이 개선되는지 확인하시오. 
summary(bike$price)
hist(bike$price, breaks = seq(0, 550000, 10000))
shapiro.test(bike$price)

bike <- bike %>% mutate(lnprice = log(price))
summary(bike$lnprice)
bike <- bike %>% filter(lnprice != "-Inf")
summary(bike$lnprice)
hist(bike$lnprice, breaks = seq(7, 14, 0.1))
shapiro.test(bike$lnprice)

# 10. 상관관계를 감안하여 DV(price)와 IV(kms_driven, mileage, power) 간의 인과관계를 확인하기 위한 세 개의 연구가설(H1~H3)을 수립하시오. 
# H1: kms_driven이 증가할수록 price는 낮아진다(음의 인과관계)#
# H2: mileage가 증가할수록 price는 낮아진다(음의 인과관계)#
# H3: POWER가 증가할수록 price는 높아진다(양의 인과관계)#

# 11. 회귀식(lm_1)을 수립한 후, 추정된 회귀식을 토대로 선형성/정규성/등분산성을 검토하시오.
lm_1 <- lm(price~kms_driven+mileage+power, data = bike)
plot(lm_1)

# 12. 오차의 자기상관을 확인하시오.
library(car)
durbinWatsonTest(lm_1)

### DW = 1.09이고(2보다 작고), rho != 0 이므로 positive autocorrelation이 존재함
### 만약 DW >2 이고, rho != 0 이면 negative autocorrelation이 존재하고, rho = 0 이면 autocorrelation 없음

## Ⅲ. 가설검정 및 추가분석
# 13. 가설을 검정하시오. 
### 단측검정: p-value와 알파를 비교
### forward, backward, stepwise(both) 세 가지 방식: f는 IV를 하나씩 추가하는 방식. b는 IV를 다 추가한 상태에서 하나씩 제거하는 방식. S는 두 가지 방식의 혼합
### AIC가 작을수록 GoF가 좋은데, f/b/s는 AIC가 가장 작게 되도록 IV를 선택
step(lm_1, direction = "forward")
step(lm_1, direction = "backward")
step(lm_1, direction = "both")
summary(lm_1)
### Yi(hat) = -0.845 * X1i - 120.825 * X2i + 6175.722 * X3i

# 14. 독립변수의 상대적 중요도를 검토하시오. 
summary(bike)

install.packages("lm.beta")
library(lm.beta)
lm.beta(lm_1)
### 표준화회귀계수 추정치의 절대값이 큰 순서대로 power > kms_driven > mileage

# 15. 다중공선성을 검토하시오.
library(car)
vif(lm_1)
### 세 개의 VIF 값이 모두 5.3보다 작으므로 다중공선성 문제는 우려할 필요가 없다.

# 16. 새로운 독립변수 repair와 DV 간의 인과관계를 확인하기 위한 연구가설(H4)을 수립하시오. 
corr.test(bike[5:9], method = "pearson", alpha = 0.05, use = "pairwise.complete.obs")
### H4: repair가 증가할수록 price는 낮아진다(음의 인과관계)

# 17. repair를 독립변수로 추가하는 것이 타당한지 확인한 후, 만약 그렇다면 새로운 회귀식(lm_2)을 통해 H1~H4에 대한 검정결과를 설명하시오. 
lm_2 <- lm(price~kms_driven + mileage + power + repair, data = bike)
summary(lm_1)
summary(lm_2)
anova(lm_1, lm_2)
### 우선 수정 R-squares가 증가했고, R-squares 중가량(약 0.013)에 따른 F통계량(224.52)의 p-value가 0으로 유의하므로 lm_2가 lm_1보다 설명력이 더 좋다. 즉 repair를 추가함으로써 종속변수 분산(특성)을 더 추가적으로 설명할 수 있다.
summary(lm_2)
### H1, H3, H4는 채택되고, H2는 채택되지 못함.
### 추정된 회귀식 Yi(hat) = 15884.931 -0.755 * X1i + 5909.657 * X3i - 2726.490 * X4i

# 18. first owner를 reference로 하는 두 개의 더미변수를 추가하여 회귀식(lm_3)을 수립한 후 모형설명력이 개선되었는지 확인하시오.
table(bike$owner)
bike$owner <- factor(bike$owner, levels = c("third owner or more", "first owner", "second owner")) ### 만약에 third or more를 reference로 하려면 출력순서를 이와 같이 조정해야 함
bike$owner <- factor(bike$owner, levels = c("first owner", "second owner", "third owner or more")) ### 원래대로로
lm_3 <- lm(price~kms_driven + mileage + power + repair + owner, data = bike) 
summary(lm_2)
summary(lm_3)
anova(lm_2, lm_3)
### 두 개 더미변수(dv1: second owner인지 여부, dv2: third owner or more인지 여부)를 추가한 lm_3가 lm_2보다 모형적합도가 더 좋음. 두개 더미변수를 추가해야 함.

# 19. lm_3의 설명력이 더 좋다면, 이를 기반으로 더미변수의 효과를 설명하시오. 
summary(lm_3)
### 추정된 회귀식 Yi(hat) = 15810 -0.701 * X1i + 5951 * X3i - 2650 * X4i - 13540 * dv1i - 17590 * dv2i
### first owner에 비해서 second owner는 b5만큼 중고차 가격이 하락하고, first owner에 비해서 third owner or more는 b6만큼 중고차 가격이 하락함

# 20. 조절변수 dv3는 중고차가 첫 번째 구입인지 여부를 의미한다(새롭게 만들어야 함). lm_3에 dv3와 repair×dv3를 추가한 lm_4를 수립한 후 모형설명력이 더 좋아졌는지 확인하시오. 만약 좋아졌다면 조절효과(상호작용효과)를 설명하시오.
bike <- bike %>% mutate(dv3 = ifelse(owner == "first owner", 1, 0)) ### 조절변수 dv3i를 생성
lm_4 <- lm(price~kms_driven + mileage + power + repair + owner + dv3 + repair*dv3, data = bike) ### dv3(조절변수)와 상호작용변수를 동시에 추가
summary(lm_3)
summary(lm_4)
anova(lm_3, lm_4)
### lm_3에 비해서 lm_4의 설명력이 더 좋아짐
### 손바뀜이 적을수록(first owner일수록) repair 회수가 가격하락에 미치는 음의 인과관계/영향이 강화된다(더 가격이 떨어진다).

### H5: first owner일수록 repair가 가격하락에 미치는 음의 인과관계가 더 강화된다. (기존 인과관계 강화하는 조절효과)

# 21. lm_4를 stepwise 방식으로 추정한 회귀식을 토대로 아래와 같은 독립변수 측정값이 있을 때 종속변수 추정치(예측치)를 구하시오.
step(lm_4, direction = "both")
### stepwise 방식으로 추정하면 유의한 상수추정치와 회귀계수추정치에 해당되는 변수만 살아남는다
lm_s <- step(lm_4, direction = "both")
### 추정된 회귀식 Yi(hat) = -10960.12 -0.7043 * X1i + 5989.142 * X3i - 1181.238 * X4i + 25791.9 * dv3i - 1747.037 * X4i * dv3i
### kms_driven: 62,000 / power: 11 / repair: 4 / dv3: 1 / repair*dv3: 4

-10960.12 -0.7043 * 62000 + 5989.142 * 11 - 1181.238 * 4 + 25791.9 * 1 - 1747.037 * 4
