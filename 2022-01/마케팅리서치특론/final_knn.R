# 연령 성별 키 몸무게 비만률에 따른 콜레스테롤 예측
library(readr)
library(dplyr)
library(psych)

data <- read.csv("data_5000.csv")
str(data)
summary(data)

data <-  data[, -(3:4)]
data <- data %>% rename(age = age_range)
df <- data
df <- df %>% relocate(c(age, gender, height, weight, bmi, ap_hi, ap_lo, gluc, smoke, alco, active, cardio, cholesterol))
df$cholesterol <- gsub(3, 2, df$cholesterol)

#df <- subset(data, select = c(id, age, gender, height, weight, bmi, cholesterol))
#df$cholesterol <- gsub(3, 2, df$cholesterol)

str(df)
df$cholesterol <- as.factor(df$cholesterol)

df$id <- NULL
df$X <- NULL
str(df)

df_z <- as.data.frame(scale(df[1:12]))
df_z$cholesterol <- df$cholesterol

## STEP2: train 데이터셋과 test 데이터셋 구성
set.seed(123)
ind <- sample(2, nrow(df_z), replace = T, prob = c(0.7, 0.3))
ind
table(ind)

df_train <- df_z[ind == 1, ]
df_test <- df_z[ind == 2, ]

table(df_z$cholesterol == "2") / 5000
table(df_train$cholesterol == "2") / 3513
table(df_test$cholesterol == "2") / 1487

## STEP3: train 데이터 프레임을 이용한 최적의 k값과 모델 도출
install.packages("caret")
library(caret)
grid1 <- expand.grid(k = 3:12)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

set.seed(1234)
knn.train <- train(cholesterol~., data = df_train, method = "knn", trControl = control, tuneGrid = grid1)
knn.train ### K = 12
varImp(knn.train, scale = F)

## STEP4: test 데이터 프레임을 이용한 Knn.train 성능 평가
pred.test1 <- predict(knn.train, newdata = df_test)
pred.test1
confusionMatrix(pred.test1, df_test$cholesterol)

## STEP5: 성능 개선
install.packages("kknn")
library(kknn)
set.seed(12345)
kknn.train <- train.kknn(cholesterol~., data = df_train, kmax = 25, distance = 2, kernel = c("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal"))
kknn.train

pred.test2 <- predict(kknn.train, newdata = df_test)
pred.test2
confusionMatrix(pred.test2, df_test$cholesterol)

#### STEP6: 예측 ####
# df_pred 데이터 정제
df_pred <- read_csv("data_10000.csv")

df_pred <-  df_pred[, -(3:4)]
df_pred <- df_pred %>% rename(age = age_range)
df_pred <- df_pred %>% relocate(c(age, gender, height, weight, bmi, ap_hi, ap_lo, gluc, smoke, alco, active, cardio, cholesterol))
df_pred$cholesterol <- gsub(3, 2, df_pred$cholesterol)

df_pred$cholesterol <- as.factor(df_pred$cholesterol)

df_pred$id <- NULL
df_pred$X <- NULL
df_pred$...1 <- NULL

predict <- df_pred

# cholesterol 버리고 예측
df_pred$cholesterol <- NULL
summary(df_pred)
str(df_pred)

# 정답 2121211122
# knn 2111211111 7개
# kknn 2111211111 7개


df_pred <- as.data.frame(scale(df_pred))
pred.test3 <- predict(knn.train, newdata = df_pred)
pred.test3 #  1 1 1 2 1 1 1 1 1
pred.test4 <- predict(kknn.train, newdata = df_pred)
pred.test4 # 2 1 1 1 2 1 1 1 1 1
