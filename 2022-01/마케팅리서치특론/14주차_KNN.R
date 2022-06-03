# 암 진단 사례

## STEP1: 데이터 프레임 준비 및 전처리
library(dplyr)
library(readr)

cancer <- read_csv("wisc_cancer_data.csv")
str(cancer)
summary(cancer)

cancer$diagnosis <- as.factor(cancer$diagnosis)
cancer$id <- NULL
cancer_z <- as.data.frame(scale(cancer[2:31]))
cancer_z$diagnosis <- cancer$diagnosis

## STEP2: train 데이터셋과 test 데이터셋 구성
set.seed(123)
ind <- sample(2, nrow(cancer_z), replace = T, prob = c(0.7, 0.3))
ind
table(ind)

cancer_train <- cancer_z[ind == 1, ]
cancer_test <- cancer_z[ind == 2, ]

table(cancer_z$diagnosis == "M") / 569
table(cancer_train$diagnosis == "M") / 405
table(cancer_test$diagnosis == "M") / 164

## STEP3: train 데이터 프레임을 이용한 최적의 k값과 모델 도출
library(caret)
grid1 <- expand.grid(k = 3:12)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(1234)
knn.train <- train(diagnosis~., data = cancer_train, method = "knn", trControl = control, tuneGrid = grid1)
knn.train  ### k값이 9인 knn 모델
varImp(knn.train, scale = F)

## STEP4: test 데이터 프레임을 이용한 Knn.train 성능 평가
pred.test1 <- predict(knn.train, newdata = cancer_test)
pred.test1
confusionMatrix(pred.test1, cancer_test$diagnosis)

## STEP5: 성능 개선
install.packages("kknn")
library(kknn)
set.seed(12345)
kknn.train <- train.kknn(diagnosis~., data = cancer_train, kmax = 25, distance = 2, kernel = c("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal"))
kknn.train

pred.test2 <- predict(kknn.train, newdata = cancer_test)
pred.test2
confusionMatrix(pred.test2, cancer_test$diagnosis)

## STEP6: 예측
cancer_pred <- read_csv("wisc_cancer_prediction.csv")
cancer_pred$id <- NULL
cancer_pred <- as.data.frame(scale(cancer_pred))
pred.test3 <- predict(knn.train, newdata = cancer_pred)
pred.test3
pred.test4 <- predict(kknn.train, newdata = cancer_pred)
pred.test4
