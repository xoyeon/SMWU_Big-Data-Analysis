## 암진단 SVM 실습

library(dplyr)
library(readr)

# STEP1: 데이터 프레임 작성 및 전처리
cancer <- read.csv("wisc_cancer_data.csv")
str(cancer)

cancer$diagnosis <- as.factor(cancer$diagnosis)
cancer$id <- NULL
cancer_svm <- as.data.frame(scale(cancer[2:31]))
cancer_svm$diagnosis <- cancer$diagnosis

# STEP2: 두 개의 데이터셋 구성
## train 데이터셋과 test 데이터셋 구성
set.seed(123)
ind <- sample(2, nrow(cancer_svm), replace = T, prob = c(0.7, 0.3))
cancer_svm_train <- cancer_svm[ind == 1, ]
cancer_svm_test <- cancer_svm[ind ==2, ]

## Class 비율 검토
table(cancer_svm$diagnosis == "M") / 569
table(cancer_svm_train$diagnosis == "M") / 405
table(cancer_svm_test$diagnosis == "M") / 164

# STEP3: train 데이터 프레임을 이용한 훈련
## linear kernel trick 활용
install.packages("e1071")
library(e1071)

set.seed(133)
linear.svm <- tune.svm(diagnosis~., data = cancer_svm_train, kernel = "linear", cost = seq(0.05, 5, by = 0.05))
summary(linear.svm)
linear.svm$best.model

# STEP4: test 데이터를 이용한 성능 평가
linear.test <- predict(linear.svm$best.model, newdata = cancer_svm_test)
library(caret)
confusionMatrix(linear.test, cancer_svm_test$diagnosis)

## polynomial kernel trick 활용
### STEP3 & STEP4
set.seed(138)
ploy.svm <- tune.svm(diagnosis~., data = cancer_svm_train, kernel = "polynomial", degree = c(2:5), gamma = seq(0.05, 1, by = 0.2), coef0 = seq(0.05, 1, by = 0.2), cost = c(0.05, 0.1, 0.2, 0.5, 1))
summary(ploy.svm)
ploy.svm$best.model

poly.test <- predict(ploy.svm$best.model, newdata = cancer_svm_test)
confusionMatrix(poly.test, cancer_svm_test$diagnosis)

## rbf kernel trick 활용
### STEP3 & STEP4
set.seed(145)
rbf.svm <- tune.svm(diagnosis~., data = cancer_svm_train, kernel = "radial", gamma = seq(0, 1, by = 0.05), cost = seq(0.05, 2, by = 0.2))
summary(rbf.svm)
rbf.svm$best.model

rbf.test <- predict(rbf.svm$best.model, newdata = cancer_svm_test)
confusionMatrix(rbf.test, cancer_svm_test$diagnosis)

## sigmoid kernel trick 활용
### STEP3 & STEP4
set.seed(333)
sigmoid.svm <- tune.svm(diagnosis~., data = cancer_svm_train, kernel = "sigmoid", gamma = seq(0, 1, by = 0.1), cost = c(0.1, 0.25, 0.5, 1), coef0 = seq(0, 1, by = 0.1))
summary(sigmoid.svm)
sigmoid.svm$best.model

sigmoid.test <- predict(sigmoid.svm$best.model, newdata = cancer_svm_test)
confusionMatrix(sigmoid.test, cancer_svm_test$diagnosis)

### rbf.svm을 최적의 모델로 선정

# STEP5: rbf kernel trick의 성능 개선 (더 안 좋아짐.. 띠옹! --> 원래 모델 재실행)
set.seed(11119)
rbf.svm <- tune.svm(diagnosis~., data = cancer_svm_train, kernel = "radial", gamma = seq(0, 0.5, by = 0.025), cost = seq(0.05, 1, by = 0.1))
summary(rbf.svm)
rbf.svm$best.model

rbf.test <- predict(rbf.svm$best.model, newdata = cancer_svm_test)
confusionMatrix(rbf.test, cancer_svm_test$diagnosis)


# STEP6: 예측
cancer_predict <- read_csv("wisc_cancer_prediction.csv")
cancer_predict$id <- NULL
cancer_predict <- as.data.frame(scale(cancer_predict))
svm.predict <- predict(rbf.svm$best.model, newdata = cancer_predict)
svm.predict
