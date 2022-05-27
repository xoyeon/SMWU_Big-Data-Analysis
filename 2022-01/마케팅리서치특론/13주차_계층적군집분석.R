# STEP1: 결측치 제거하고 필요한 서브 데이터 프레임 생성
library(dplyr)
table(HRA$department)
HRA_hr <- HRA %>% filter(department == "hr")
table(is.na(HRA_hr))

# STEP2: 수치형(0/1)으로 측정된 더미변수 생성
str(HRA_hr)
HRA_hr <- HRA_hr %>% mutate(medium = ifelse(salary == "medium", 1, 0))
HRA_hr <- HRA_hr %>% mutate(low = ifelse(salary == "low", 1, 0))
table(HRA_hr$medium)
table(HRA_hr$low)
HRA_hr$department <- NULL
HRA_hr$salary <- NULL
HRA_hr$id <- NULL

# STEP3: 사례간의 거리 측정
install.packages("cluster")
library(cluster)
distance_hr <- daisy(HRA_hr, metric = "gower")

# STEP4: 계층적 군집분석 실시
HRA_CA_hr <- hclust(distance_hr, method = "ward.D2")
plot(HRA_CA_hr, col = "darkgreen", main = "HRA_CA_hr")

# STEP5: 최적의 군집개수(k*) 구하기
str(HRA_hr)
## 범주형 척도를 계량형 척도로 변경하기
HRA_hr$left <- as.numeric(HRA_hr$left)
HRA_hr$accident <- as.numeric(HRA_hr$accident)
HRA_hr$promotion <- as.numeric(HRA_hr$promotion)

HRA_hr$left <- HRA_hr$left-1
HRA_hr$accident <- HRA_hr$accident-1
HRA_hr$promotion <- HRA_hr$promotion-1
str(HRA_hr)

## (k*) 구하기
install.packages("NbClust")
library(NbClust)
set.seed(123)
HRA_hr_NC1 <- NbClust(HRA_hr, distance = "euclidean", min.nc = 5, max.nc = 15, method = "average")

# STEP6: k*에 기반한 군집분석 결과 해석
HRA_hr_HCA <- cutree(HRA_CA_hr, 9)
table(HRA_hr_HCA)
result_hr <- aggregate(HRA_hr, by = list(cluster = HRA_hr_HCA), mean)
HRA_hr <- HRA_hr %>% mutate(cluster1 = HRA_hr_HCA)



















































































