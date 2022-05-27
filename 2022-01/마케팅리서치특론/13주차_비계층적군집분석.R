# K-평균 군집분석

# STEP1: 필요한 데이터 프레임(HRA_hr_k) 새로 만들기
HRA_hr_k <- HRA_hr[2:6]

# STEP2: 변수 측정값 표준화
HRA_hr_k <- scale(HRA_hr_k)

# STEP3: 최적의 군집개수(k*) 구하기
set.seed(123)
HRA_hr_NC2 <- NbClust(HRA_hr_k, distance = "euclidean", min.nc = 5, max.nc = 15, method = "kmeans")

# STEP4: k-means 시행하기(k* = 5)
set.seed(123)
HRA_hr_KCA <- kmeans(HRA_hr_k, centers = 5, nstart = 25)
HRA_hr_KCA$size
result_hr2 <- aggregate(HRA_hr, by = list(cluster = HRA_hr_KCA$cluster), mean)
HRA_hr <- HRA_hr %>% mutate(cluster2 = HRA_hr_KCA$cluster)
