
library(readxl)
library(tidyr)
library(survival)
library(dplyr)

#Cox PHM with nonfiltered raw data (8:1:1)
train_B <- read.csv("C:/Temp/train_B8_n.csv", header=TRUE)
valid_B <- read.csv("C:/Temp/valid_B8_n.csv", header=TRUE)
test_B <- read.csv("C:/Temp/test_B8_n.csv", header=TRUE)

dim(train_B) #975141     38
dim(valid_B) #121903     38
dim(test_B) #121867     38

#B그룹의 공연_월 = 4일 때에 delta가 모두 0이므로 제거
train_B <- train_B[train_B$공연_월 != 4,]
valid_B <- valid_B[valid_B$공연_월 != 4,]
test_B <- test_B[test_B$공연_월 != 4,]

#B그룹의 예매방식코드 = 4일 때에 delta가 모두 0이므로 제거
train_B <- train_B[train_B$예매방식코드 != 4,]
valid_B <- valid_B[valid_B$예매방식코드 != 4,]
test_B <- test_B[test_B$예매방식코드 != 4,]

#train_B (level 중 한 곳에 delta가 모여있는 경우)
train_B$group <- factor(train_B$group)
train_B$성별 <- factor(train_B$성별)
train_B$단독판매여부 <- factor(train_B$단독판매여부)
train_B$공연시설인기 <- factor(train_B$공연시설인기)
train_B$아동공연.여부 <- factor(train_B$아동공연.여부)
train_B$미세먼지등급 <- factor(train_B$미세먼지등급)
train_B$공연_요일 <- factor(train_B$공연_요일)
train_B$공연_평일주말 <- factor(train_B$공연_평일주말)
train_B$공연_월 <- factor(train_B$공연_월)
train_B$공연_오전오후 <- factor(train_B$공연_오전오후)
train_B$예매_요일 <- factor(train_B$예매_요일)
train_B$예매_평일주말 <- factor(train_B$예매_평일주말)
train_B$예매_월 <- factor(train_B$예매_월)
train_B$결제수단코드 <- factor(train_B$결제수단코드)
train_B$예매방식코드 <- factor(train_B$예매방식코드)
train_B$할인종류코드 <- factor(train_B$할인종류코드)

#valid_B
valid_B$group <- factor(valid_B$group)
valid_B$성별 <- factor(valid_B$성별)
valid_B$단독판매여부 <- factor(valid_B$단독판매여부)
valid_B$공연시설인기 <- factor(valid_B$공연시설인기)
valid_B$아동공연.여부 <- factor(valid_B$아동공연.여부)
valid_B$미세먼지등급 <- factor(valid_B$미세먼지등급)
valid_B$공연_요일 <- factor(valid_B$공연_요일)
valid_B$공연_평일주말 <- factor(valid_B$공연_평일주말)
valid_B$공연_월 <- factor(valid_B$공연_월)
valid_B$공연_오전오후 <- factor(valid_B$공연_오전오후)
valid_B$예매_요일 <- factor(valid_B$예매_요일)
valid_B$예매_평일주말 <- factor(valid_B$예매_평일주말)
valid_B$예매_월 <- factor(valid_B$예매_월)
valid_B$결제수단코드 <- factor(valid_B$결제수단코드)
valid_B$예매방식코드 <- factor(valid_B$예매방식코드)
valid_B$할인종류코드 <- factor(valid_B$할인종류코드)

#test_B
test_B$group <- factor(test_B$group)
test_B$성별 <- factor(test_B$성별)
test_B$단독판매여부 <- factor(test_B$단독판매여부)
test_B$공연시설인기 <- factor(test_B$공연시설인기)
test_B$아동공연.여부 <- factor(test_B$아동공연.여부)
test_B$미세먼지등급 <- factor(test_B$미세먼지등급)
test_B$공연_요일 <- factor(test_B$공연_요일)
test_B$공연_평일주말 <- factor(test_B$공연_평일주말)
test_B$공연_월 <- factor(test_B$공연_월)
test_B$공연_오전오후 <- factor(test_B$공연_오전오후)
test_B$예매_요일 <- factor(test_B$예매_요일)
test_B$예매_평일주말 <- factor(test_B$예매_평일주말)
test_B$예매_월 <- factor(test_B$예매_월)
test_B$결제수단코드 <- factor(test_B$결제수단코드)
test_B$예매방식코드 <- factor(test_B$예매방식코드)
test_B$할인종류코드 <- factor(test_B$할인종류코드)

#train, test data 나누기
X_train_B <- train_B[,-c(1,2)]
y_train_B <- train_B[, c(1,2)]
X_valid_B <- valid_B[,-c(1,2)]
y_valid_B <- valid_B[, c(1,2)]
X_test_B <- test_B[,-c(1,2)]
y_test_B <- test_B[, c(1,2)]

# split numeric, categorical variables
numeric.variable <- c('예매금액', '교통개수', '팔로워합', '수상실적개수', '공연별전사코개수', '강수', '불쾌지수', '소요시간', '매출액', '순예매건수', '공연횟수', '좌석점유율', '할인종류개수', '할인율', '일일확진자', '예매가능공연개수', '공.예', '시설합', '주차시설') #20개의 변수 (ElasticNet을 통해 좌석수 변수 제거)

categorical.variable <- c('group', '성별', '단독판매여부', '공연시설인기', '아동공연.여부', '미세먼지등급', '공연_요일', '공연_월', '공연_오전오후', '예매_요일', '예매_월', '결제수단코드', '예매방식코드', '공연_평일주말', '예매_평일주말', '할인종류코드') #16개의 변수

X_train_num <- X_train_B[, numeric.variable]
X_train_cat <- X_train_B[, categorical.variable]

X_valid_num <- X_valid_B[, numeric.variable]
X_valid_cat <- X_valid_B[, categorical.variable]

X_test_num <- X_test_B[, numeric.variable]
X_test_cat <- X_test_B[, categorical.variable]

#combine
X_train_scaled_n <- cbind(X_train_num, X_train_cat)
X_valid_scaled_n <- cbind(X_valid_num, X_valid_cat)
X_test_scaled_n <- cbind(X_test_num, X_test_cat)

train_n_scaled <- cbind(y_train_B, X_train_scaled_n)
valid_n_scaled <- cbind(y_valid_B, X_valid_scaled_n)
test_n_scaled <- cbind(y_test_B, X_test_scaled_n)

dim(train_n_scaled) #975141     37
dim(valid_n_scaled) #121903     37
dim(test_n_scaled) #121867     37


set.seed(1004)
cox_B_n <- coxph(Surv(y,delta)~., data=train_n_scaled)
summary(cox_B_n) #공연_평일주말, 예매_평일주말의 coef가 NA가 나와 해당 변수를 제거하고 다시 Cox PHM에 적합시킨다.

cox_B_n_filter <- coxph(Surv(y,delta)~.-공연_평일주말-예매_평일주말, data=train_n_scaled)
summary(cox_B_n_filter) #coef에 NA가 더 이상 없으니 해당 모델을 full model로 고정한다.

#Cox PHM with raw data (8:1:1)
train_B <- read.csv("C:/Temp/train_B8_n.csv", header=TRUE)
valid_B <- read.csv("C:/Temp/valid_B8_n.csv", header=TRUE)
test_B <- read.csv("C:/Temp/test_B8_n.csv", header=TRUE)

dim(train_B)
dim(valid_B)
dim(test_B)

#B그룹의 공연_월 = 4일 때에 delta가 모두 0이므로 제거
train_B <- train_B[train_B$공연_월 != 4,]
valid_B <- valid_B[valid_B$공연_월 != 4,]
test_B <- test_B[test_B$공연_월 != 4,]

#B그룹의 예매방식코드 = 4일 때에 delta가 모두 0이므로 제거
train_B <- train_B[train_B$예매방식코드 != 4,]
valid_B <- valid_B[valid_B$예매방식코드 != 4,]
test_B <- test_B[test_B$예매방식코드 != 4,]

#train_B (level 중 한 곳에 delta가 모여있는 경우)
train_B$group <- factor(train_B$group)
train_B$성별 <- factor(train_B$성별)
train_B$단독판매여부 <- factor(train_B$단독판매여부)
train_B$공연시설인기 <- factor(train_B$공연시설인기)
train_B$아동공연.여부 <- factor(train_B$아동공연.여부)
train_B$미세먼지등급 <- factor(train_B$미세먼지등급)
train_B$공연_요일 <- factor(train_B$공연_요일)
train_B$공연_평일주말 <- NULL
train_B$공연_월 <- factor(train_B$공연_월)
train_B$공연_오전오후 <- factor(train_B$공연_오전오후)
train_B$예매_요일 <- factor(train_B$예매_요일)
train_B$예매_평일주말 <- NULL
train_B$예매_월 <- factor(train_B$예매_월)
train_B$결제수단코드 <- factor(train_B$결제수단코드)
train_B$예매방식코드 <- factor(train_B$예매방식코드)
train_B$할인종류코드 <- NULL

#valid_B
valid_B$group <- factor(valid_B$group)
valid_B$성별 <- factor(valid_B$성별)
valid_B$단독판매여부 <- factor(valid_B$단독판매여부)
valid_B$공연시설인기 <- factor(valid_B$공연시설인기)
valid_B$아동공연.여부 <- factor(valid_B$아동공연.여부)
valid_B$미세먼지등급 <- factor(valid_B$미세먼지등급)
valid_B$공연_요일 <- factor(valid_B$공연_요일)
valid_B$공연_평일주말 <- NULL
valid_B$공연_월 <- factor(valid_B$공연_월)
valid_B$공연_오전오후 <- factor(valid_B$공연_오전오후)
valid_B$예매_요일 <- factor(valid_B$예매_요일)
valid_B$예매_평일주말 <- NULL
valid_B$예매_월 <- factor(valid_B$예매_월)
valid_B$결제수단코드 <- factor(valid_B$결제수단코드)
valid_B$예매방식코드 <- factor(valid_B$예매방식코드)
valid_B$할인종류코드 <- NULL

#test_B
test_B$group <- factor(test_B$group)
test_B$성별 <- factor(test_B$성별)
test_B$단독판매여부 <- factor(test_B$단독판매여부)
test_B$공연시설인기 <- factor(test_B$공연시설인기)
test_B$아동공연.여부 <- factor(test_B$아동공연.여부)
test_B$미세먼지등급 <- factor(test_B$미세먼지등급)
test_B$공연_요일 <- factor(test_B$공연_요일)
test_B$공연_평일주말 <- NULL
test_B$공연_월 <- factor(test_B$공연_월)
test_B$공연_오전오후 <- factor(test_B$공연_오전오후)
test_B$예매_요일 <- factor(test_B$예매_요일)
test_B$예매_평일주말 <- NULL
test_B$예매_월 <- factor(test_B$예매_월)
test_B$결제수단코드 <- factor(test_B$결제수단코드)
test_B$예매방식코드 <- factor(test_B$예매방식코드)
test_B$할인종류코드 <- NULL

#train, test data 나누기
X_train_B <- train_B[,-c(1,2)]
y_train_B <- train_B[, c(1,2)]
X_valid_B <- valid_B[,-c(1,2)]
y_valid_B <- valid_B[, c(1,2)]
X_test_B <- test_B[,-c(1,2)]
y_test_B <- test_B[, c(1,2)]

# split numeric, categorical variables
numeric.variable <- c('예매금액', '교통개수', '팔로워합', '수상실적개수', '공연별전사코개수', '강수', '불쾌지수', '소요시간', '좌석수', '매출액', '순예매건수', '공연횟수', '좌석점유율', '할인종류개수', '할인율', '일일확진자', '예매가능공연개수', '공.예', '시설합', '주차시설') #20개의 변수

categorical.variable <- c('group', '성별', '단독판매여부', '공연시설인기', '아동공연.여부', '미세먼지등급', '공연_요일', '공연_월', '공연_오전오후', '예매_요일', '예매_월', '결제수단코드', '예매방식코드') #13개의 변수 (공연_평일주말, 예매_평일주말, 할인종류코드 제외)

X_train_num <- X_train_B[, numeric.variable]
X_train_cat <- X_train_B[, categorical.variable]

X_valid_num <- X_valid_B[, numeric.variable]
X_valid_cat <- X_valid_B[, categorical.variable]

X_test_num <- X_test_B[, numeric.variable]
X_test_cat <- X_test_B[, categorical.variable]

#combine
X_train_scaled_n <- cbind(X_train_num, X_train_cat)
X_valid_scaled_n <- cbind(X_valid_num, X_valid_cat)
X_test_scaled_n <- cbind(X_test_num, X_test_cat)

train_n_scaled <- cbind(y_train_B, X_train_scaled_n)
valid_n_scaled <- cbind(y_valid_B, X_valid_scaled_n)
test_n_scaled <- cbind(y_test_B, X_test_scaled_n)

dim(train_n_scaled) #975141     35
dim(valid_n_scaled) #121903     35
dim(test_n_scaled) #121867     35



set.seed(1004)
cox_B_n <- coxph(Surv(y,delta)~., data=train_n_scaled)
summary(cox_B_n)

#fancy output
cox_summary_to_df <- function(cox_model) {
  # Extract coefficients and p-values
  coefficients <- coef(cox_model)
  p_values <- 2 * (1 - pnorm(abs(coefficients / sqrt(diag(vcov(cox_model))))))
  
  # Create a data frame with coefficients and p-values
  
  cox_summary_df <- data.frame(
    Coefficient = round(coefficients, 3), # Round coefficients to 3 decimal places
    Exp_Coefficient = round(exp(coefficients), 3), # Round exp(coef) to 3 decimal place
    P_Value = round(p_values, 3) # Round p-values to 3 decimal places
    
  )
  return(cox_summary_df)
}

cox_o_sum <- cox_summary_to_df(cox_B_n)
cox_o_sum

cox_o_sum_filter <- cox_summary_to_df(cox_B_n_filter)
cox_o_sum_filter

write.csv(cox_o_sum, file = "cox_B_sum_org.csv")
write.csv(cox_o_sum_filter, file = "cox_o_sum_filter.csv")



## Stratified Cox PHM (w/ Elasticnet penalty)

train_B_dummy <- read.csv("train_B_dummy_org.csv", header=TRUE)
head(train_B_dummy)

valid_B_dummy <- read.csv("val_B_dummy_org.csv", header=TRUE)
head(valid_B_dummy)
dim(valid_B_dummy)

y_valid_B <- valid_B_dummy[,c(1,2)]
X_valid_B <- valid_B_dummy[,-c(1,2)]

y_train_B <- train_B_dummy[,c(1,2)] #975141 obs. of  2 variables
X_train_B <- train_B_dummy[,-c(1,2)] #975141 obs. of  69 variables

X_train_mat <- as.matrix(X_train_B)

#general elasticnet penalty Cox PHM
library(glmnet)
y <- with(y_train_B, Surv(y,delta))
fit <- glmnet(X_train_mat, y, family = "cox")
cvfit <- cv.glmnet(X_train_mat, y, family = "cox", type.measure = "C")

best_alpha <- cvfit$glmnet.fit$alpha
best_lambda <- cvfit$lambda.min
optimal_cox_model1 <- glmnet(X_train_mat, y, family = "cox", lambda = best_lambda)
exp(coef(optimal_cox_model1))


#valid set으로 general ElasticNet penalty Cox PHM

X_valid_mat <- as.matrix(X_valid_B)

#general elasticnet penalty Cox PHM
library(glmnet)
library(survival)
y <- with(y_valid_B, Surv(y,delta))
fit <- glmnet(X_valid_mat, y, family = "cox")
cvfit <- cv.glmnet(X_valid_mat, y, family = "cox", type.measure = "C")

best_alpha <- cvfit$glmnet.fit$alpha
best_lambda <- cvfit$lambda.min
optimal_cox_model1 <- glmnet(X_valid_mat, y, family = "cox", lambda = best_lambda)
exp(coef(optimal_cox_model1))


train_B <- read.csv("C:/Temp/train_B998.csv", header=TRUE)
valid_B <- read.csv("C:/Temp/valid_B998.csv", header=TRUE)
test_B <- read.csv("C:/Temp/test_B998.csv", header=TRUE)

dim(train_B)
dim(valid_B)
dim(test_B)

#train_B
train_B$group <- factor(train_B$group)
train_B$성별 <- factor(train_B$성별)
train_B$단독판매여부 <- factor(train_B$단독판매여부)
train_B$공연시설인기 <- factor(train_B$공연시설인기)
train_B$아동공연.여부 <- factor(train_B$아동공연.여부)
train_B$미세먼지등급 <- factor(train_B$미세먼지등급)
train_B$공연_요일 <- factor(train_B$공연_요일)
train_B$공연_평일주말 <- factor(train_B$공연_평일주말)
train_B$공연_월 <- factor(train_B$공연_월)
train_B$공연_오전오후 <- factor(train_B$공연_오전오후)
train_B$예매_요일 <- factor(train_B$예매_요일)
train_B$예매_평일주말 <- factor(train_B$예매_평일주말)
train_B$예매_월 <- factor(train_B$예매_월)
train_B$결제수단코드 <- factor(train_B$결제수단코드)
train_B$예매방식코드 <- factor(train_B$예매방식코드)
train_B$할인종류코드 <- factor(train_B$할인종류코드)
train_B$공연_월 <- NULL #infinite converge issue

#valid_B
valid_B$group <- factor(valid_B$group)
valid_B$성별 <- factor(valid_B$성별)
valid_B$단독판매여부 <- factor(valid_B$단독판매여부)
valid_B$공연시설인기 <- factor(valid_B$공연시설인기)
valid_B$아동공연.여부 <- factor(valid_B$아동공연.여부)
valid_B$미세먼지등급 <- factor(valid_B$미세먼지등급)
valid_B$공연_요일 <- factor(valid_B$공연_요일)
valid_B$공연_평일주말 <- factor(valid_B$공연_평일주말)
valid_B$공연_월 <- factor(valid_B$공연_월)
valid_B$공연_오전오후 <- factor(valid_B$공연_오전오후)
valid_B$예매_요일 <- factor(valid_B$예매_요일)
valid_B$예매_평일주말 <- factor(valid_B$예매_평일주말)
valid_B$예매_월 <- factor(valid_B$예매_월)
valid_B$결제수단코드 <- factor(valid_B$결제수단코드)
valid_B$예매방식코드 <- factor(valid_B$예매방식코드)
valid_B$할인종류코드 <- factor(valid_B$할인종류코드)
valid_B$공연_월 <- NULL #infinite converge issue

#test_B
test_B$group <- factor(test_B$group)
test_B$성별 <- factor(test_B$성별)
test_B$단독판매여부 <- factor(test_B$단독판매여부)
test_B$공연시설인기 <- factor(test_B$공연시설인기)
test_B$아동공연.여부 <- factor(test_B$아동공연.여부)
test_B$미세먼지등급 <- factor(test_B$미세먼지등급)
test_B$공연_요일 <- factor(test_B$공연_요일)
test_B$공연_평일주말 <- factor(test_B$공연_평일주말)
test_B$공연_월 <- factor(test_B$공연_월)
test_B$공연_오전오후 <- factor(test_B$공연_오전오후)
test_B$예매_요일 <- factor(test_B$예매_요일)
test_B$예매_평일주말 <- factor(test_B$예매_평일주말)
test_B$예매_월 <- factor(test_B$예매_월)
test_B$결제수단코드 <- factor(test_B$결제수단코드)
test_B$예매방식코드 <- factor(test_B$예매방식코드)
test_B$할인종류코드 <- factor(test_B$할인종류코드)
test_B$공연_월 <- NULL #infinite converge issue

#train, test data 나누기
X_train_B <- train_B[,-c(1,2)]
y_train_B <- train_B[, c(1,2)]
X_valid_B <- valid_B[,-c(1,2)]
y_valid_B <- valid_B[, c(1,2)]
X_test_B <- test_B[,-c(1,2)]
y_test_B <- test_B[, c(1,2)]

# split numeric, categorical variables
numeric.variable <- c('예매금액', '교통개수', '팔로워합', '수상실적개수', '공연별전사코개수', '강수', '불쾌지수', '소요시간', '좌석수', '매출액', '순예매건수', '공연횟수', '좌석점유율', '할인종류개수', '할인율', '일일확진자', '예매가능공연개수', '공.예', '시설합', '주차시설') #20개의 변수

categorical.variable <- c('group', '성별', '단독판매여부', '공연시설인기', '아동공연.여부', '미세먼지등급', '공연_요일', '공연_평일주말', '공연_오전오후', '예매_요일', '예매_평일주말', '예매_월', '결제수단코드', '예매방식코드', '할인종류코드') #15개의 변수

X_train_num <- X_train_B[, numeric.variable]
X_train_cat <- X_train_B[, categorical.variable]

X_valid_num <- X_valid_B[, numeric.variable]
X_valid_cat <- X_valid_B[, categorical.variable]

X_test_num <- X_test_B[, numeric.variable]
X_test_cat <- X_test_B[, categorical.variable]


#combine
X_train_scaled <- cbind(X_train_nscaled, X_train_cat)
X_valid_scaled <- cbind(X_valid_nscaled, X_valid_cat)
X_test_scaled <- cbind(X_test_nscaled, X_test_cat)

train_Z_scaled <- cbind(y_train_B, X_train_scaled)
valid_Z_scaled <- cbind(y_valid_B, X_valid_scaled)
test_Z_scaled <- cbind(y_test_B, X_test_scaled)

dim(train_Z_scaled)
dim(valid_Z_scaled)
dim(test_Z_scaled)


## time-dependent ROC analysis

#생존을 예측하는 모델을 만들고 나서 그 예측모델의 성능을 평가하는 방법 중 대표적: Harrell’s c-index, time-dependent ROC analysis
cox.obj<-coxph(Surv(y,delta)~.-공연_평일주말-예매_평일주말, data=valid_n_scaled)

cox.lp<-predict(cox.obj, type="lp")

#CD1
library(survivalROC)
t.ROC<-survivalROC(Stime=valid_n_scaled$y, status=valid_n_scaled$delta, marker=cox.lp, predict.time=30, method="KM")
t.ROC$AUC #0.7520503
#ROC 커브의 area under the curve

#CD5
#Inverse Probability of Censoring 방법으로 데이터에 웨이트를 주어서 sensitivity와 specificity를 계산하는 방법
library(timeROC)
t.ROC2<-timeROC(T=valid_n_scaled$y, delta=as.double(as.character(valid_n_scaled$delta)), marker=cox.lp, times=30, cause=1, iid=TRUE)
t.ROC2$AUC #t=0 NA, t=30 0.7544514

#CD1, CD5 plot
plot(t.ROC$FP, t.ROC$TP, type="l", xlab="1 - Specificity", ylab="Sensitivity") #X축: False positive rate, Y축: True positive rate
plot(t.ROC2, time=30)


#ElasticNet Cox PHM model with non filtering raw data dummy

set.seed(1004)

library(survival)
train_B_dummy <- read.csv("C:/Users/user/Desktop/train_B_dummy_nonfiltering.csv", header=TRUE)
head(train_B_dummy)

valid_B_dummy <- read.csv("C:/Users/user/Desktop/val_B_dummy_nonfiltering.csv", header=TRUE)
head(valid_B_dummy)
dim(valid_B_dummy)

test_B_dummy <- read.csv("C:/Users/user/Desktop/test_B_dummy_nonfiltering.csv", header=TRUE)
head(test_B_dummy)

y_valid_B <- valid_B_dummy[,c(1,2)]
X_valid_B <- valid_B_dummy[,-c(1,2)]

y_train_B <- train_B_dummy[,c(1,2)] #975141 obs. of  2 variables
X_train_B <- train_B_dummy[,-c(1,2)] #975141 obs. of  69 variables

X_train_mat <- as.matrix(X_train_B)
X_valid_mat <- as.matrix(X_valid_B)

#general elasticnet penalty Cox PHM
library(glmnet)
y <- with(y_train_B, Surv(y,delta))
fit <- glmnet(X_train_mat, y, family = "cox", alpha=0.5)
cvfit <- cv.glmnet(X_train_mat, y, family = "cox", type.measure = "C", nfolds=5, alpha=0.5)

plot(fit)
plot(cvfit)

best_alpha <- cvfit$glmnet.fit$alpha
best_lambda <- cvfit$lambda.min #0.0006072664

optimal_cox_model1 <- glmnet(X_train_mat, y, family = "cox", lambda = best_lambda, alpha=0.5)
B_cvfit_org <- round(coef(cvfit, s="lambda.min"),3)
B_optimal_org<- round(coef(optimal_cox_model1, s="lambda.min"),3)
B_cvfit_org <- as.data.frame(as.matrix(B_cvfit_org))
B_optimal_org <- as.data.frame(as.matrix(B_optimal_org))
write.csv(B_cvfit_org, file="C:/Users/user/Desktop/B_cvfit_nonfilter.csv")
write.csv(B_optimal_org, file="C:/Users/user/Desktop/B_optimal_nonfilter.csv")



#ElasticNet Cox PHM model with raw data dummy

library(survival)
train_B_dummy <- read.csv("C:/Users/user/Desktop/train_B_dummy_org.csv", header=TRUE)
head(train_B_dummy)

valid_B_dummy <- read.csv("C:/Users/user/Desktop/val_B_dummy_org.csv", header=TRUE)
head(valid_B_dummy)
dim(valid_B_dummy)

y_valid_B <- valid_B_dummy[,c(1,2)]
X_valid_B <- valid_B_dummy[,-c(1,2)]

y_train_B <- train_B_dummy[,c(1,2)] #975141 obs. of  2 variables
X_train_B <- train_B_dummy[,-c(1,2)] #975141 obs. of  69 variables

X_train_mat <- as.matrix(X_train_B)
X_valid_mat <- as.matrix(X_valid_B)

#general elasticnet penalty Cox PHM
library(glmnet)
y <- with(y_train_B, Surv(y,delta))
fit <- glmnet(X_train_mat, y, family = "cox")
cvfit <- cv.glmnet(X_train_mat, y, family = "cox", type.measure = "C", nfolds=5)

best_alpha <- cvfit$glmnet.fit$alpha #NULL
best_lambda <- cvfit$lambda.min #0.0003332506
optimal_cox_model1 <- glmnet(X_train_mat, y, family = "cox", lambda = best_lambda)
B_cvfit_org <- exp(coef(cvfit, s="lambda.min"))
B_optimal_org<- exp(coef(optimal_cox_model1, s="lambda.min"))
B_cvfit_org <- as.data.frame(as.matrix(B_cvfit_org))
B_optimal_org <- as.data.frame(as.matrix(B_optimal_org))
write.csv(B_cvfit_org, file="C:/Users/user/Desktop/B_cvfit_org.csv")
write.csv(B_optimal_org, file="C:/Users/user/Desktop/B_optimal_org.csv")

plot(fit)
plot(cvfit)



#valid_B import

valid_B <- read.csv("C:/Users/user/Desktop/valid_B8_n.csv", header=TRUE)
valid_B <- valid_B[valid_B$공연_월 != 4,]
valid_B <- valid_B[valid_B$예매방식코드 != 4,]
valid_B$group <- factor(valid_B$group)
valid_B$성별 <- factor(valid_B$성별)
valid_B$단독판매여부 <- factor(valid_B$단독판매여부)
valid_B$공연시설인기 <- factor(valid_B$공연시설인기)
valid_B$아동공연.여부 <- factor(valid_B$아동공연.여부)
valid_B$미세먼지등급 <- factor(valid_B$미세먼지등급)
valid_B$공연_요일 <- factor(valid_B$공연_요일)
valid_B$공연_평일주말 <- factor(valid_B$공연_평일주말)
valid_B$공연_월 <- factor(valid_B$공연_월)
valid_B$공연_오전오후 <- factor(valid_B$공연_오전오후)
valid_B$예매_요일 <- factor(valid_B$예매_요일)
valid_B$예매_평일주말 <- factor(valid_B$예매_평일주말)
valid_B$예매_월 <- factor(valid_B$예매_월)
valid_B$결제수단코드 <- factor(valid_B$결제수단코드)
valid_B$예매방식코드 <- factor(valid_B$예매방식코드)
valid_B$할인종류코드 <- factor(valid_B$할인종류코드)
X_valid_B <- valid_B[,-c(1,2)]
y_valid_B <- valid_B[, c(1,2)]
numeric.variable <- c('예매금액', '교통개수', '팔로워합', '수상실적개수', '공연별전사코개수', '강수', '불쾌지수', '소요시간', '매출액', '순예매건수', '공연횟수', '좌석점유율', '할인종류개수', '할인율', '일일확진자', '예매가능공연개수', '공.예', '시설합', '주차시설') #20개의 변수 (ElasticNet을 통해 좌석수 변수 제거)
categorical.variable <- c('group', '성별', '단독판매여부', '공연시설인기', '아동공연.여부', '미세먼지등급', '공연_요일', '공연_월', '공연_오전오후', '예매_요일', '예매_월', '결제수단코드', '예매방식코드', '공연_평일주말', '예매_평일주말', '할인종류코드') #16개의 변수
X_valid_num <- X_valid_B[, numeric.variable]
X_valid_cat <- X_valid_B[, categorical.variable]
X_valid_scaled_n <- cbind(X_valid_num, X_valid_cat)
valid_n_scaled <- cbind(y_valid_B, X_valid_scaled_n)
dim(valid_n_scaled) #121903     37


#valid set으로 예측 성능 확인하기

set.seed(1004)
train_B <- read.csv("C:/Users/user/Desktop/train_B8_n.csv", header=TRUE)
train_B <- train_B[train_B$공연_월 != 4,]
train_B <- train_B[train_B$예매방식코드 != 4,]
train_B$group <- factor(train_B$group)
train_B$성별 <- factor(train_B$성별)
train_B$단독판매여부 <- factor(train_B$단독판매여부)
train_B$공연시설인기 <- factor(train_B$공연시설인기)
train_B$아동공연.여부 <- factor(train_B$아동공연.여부)
train_B$미세먼지등급 <- factor(train_B$미세먼지등급)
train_B$공연_요일 <- factor(train_B$공연_요일)
train_B$공연_평일주말 <- factor(train_B$공연_평일주말)
train_B$공연_월 <- factor(train_B$공연_월)
train_B$공연_오전오후 <- factor(train_B$공연_오전오후)
train_B$예매_요일 <- factor(train_B$예매_요일)
train_B$예매_평일주말 <- factor(train_B$예매_평일주말)
train_B$예매_월 <- factor(train_B$예매_월)
train_B$결제수단코드 <- factor(train_B$결제수단코드)
train_B$예매방식코드 <- factor(train_B$예매방식코드)
train_B$할인종류코드 <- factor(train_B$할인종류코드)
X_train_B <- train_B[,-c(1,2)]
y_train_B <- train_B[, c(1,2)]
numeric.variable <- c('예매금액', '교통개수', '팔로워합', '수상실적개수', '공연별전사코개수', '강수', '불쾌지수', '소요시간', '매출액', '순예매건수', '공연횟수', '좌석점유율', '할인종류개수', '할인율', '일일확진자', '예매가능공연개수', '공.예', '시설합', '주차시설') #20개의 변수 (ElasticNet을 통해 좌석수 변수 제거)
categorical.variable <- c('group', '성별', '단독판매여부', '공연시설인기', '아동공연.여부', '미세먼지등급', '공연_요일', '공연_월', '공연_오전오후', '예매_요일', '예매_월', '결제수단코드', '예매방식코드', '공연_평일주말', '예매_평일주말', '할인종류코드') #16개의 변수
X_train_num <- X_train_B[, numeric.variable]
X_train_cat <- X_train_B[, categorical.variable]
X_train_scaled_n <- cbind(X_train_num, X_train_cat)
train_n_scaled <- cbind(y_train_B, X_train_scaled_n)
dim(train_n_scaled) #975141     37

cox_B_n <- coxph(Surv(y,delta)~.-공연_평일주말-예매_평일주말, data=train_n_scaled)
summary(cox_B_n)


#fancy output
cox_summary_to_df <- function(cox_model) {
  # Extract coefficients and p-values
  coefficients <- coef(cox_model)
  p_values <- 2 * (1 - pnorm(abs(coefficients / sqrt(diag(vcov(cox_model))))))
  
  # Create a data frame with coefficients and p-values
  
  cox_summary_df <- data.frame(
    Coefficient = round(coefficients, 3), # Round coefficients to 3 decimal places
    Exp_Coefficient = round(exp(coefficients), 3), # Round exp(coef) to 3 decimal place
    P_Value = round(p_values, 3) # Round p-values to 3 decimal places
    
  )
  return(cox_summary_df)
}

cox_o_sum <- cox_summary_to_df(cox_B_n)
cox_o_sum
write.csv(cox_o_sum, file="B_cox.csv")



#예매점유율 중 그룹의 Brier score 구하기

library(survival)
library(survAUC)
train.fit <- coxph(Surv(y,delta)~.-공연_평일주말-예매_평일주말, x=TRUE, y=TRUE, method='breslow', data=train_n_scaled)

lp <- predict(train.fit)
lpnew <- predict(train.fit, newdata=valid_n_scaled)
GHCI(lpnew) #0.6838808

# 예측값 계산
valid_predictions <- predict(train.fit, newdata = valid_n_scaled, type = "expected")

# Brier 점수 계산 함수
calculate_brier_score <- function(predicted_probs, actual_events, time_point) {
  time_indices <- which(valid_n_scaled$y <= time_point) # 해당 시간 이전의 데이터 인덱스 선택
  predicted_probs <- predicted_probs[time_indices] # 해당 시간 이전의 예측값 선택
  actual_events <- actual_events[time_indices]       # 해당 시간 이전의 실제 이벤트 선택
  brier_score <- mean((actual_events - predicted_probs)^2)
  return(brier_score)
}

# 특정 시점 (예: t=7) 에 대한 Brier 점수 계산
brier_score_7 <- calculate_brier_score(valid_predictions, valid_n_scaled$delta, 7)

brier_score_7 #0.3814149

#예매점유율 하 그룹 read

train_dfc_n <- read.csv('C:/Users/user/Desktop/train_C8_n.csv', header=TRUE)
valid_dfc_n <- read.csv('C:/Users/user/Desktop/valid_C8_n.csv', header=TRUE)

## non-scaled
train_dfc_n$group <- factor(train_dfc_n$group)
train_dfc_n$성별 <- factor(train_dfc_n$성별)

train_dfc_n$세부장르명 <- factor(train_dfc_n$세부장르명)
train_dfc_n$미세먼지등급 <- factor(train_dfc_n$미세먼지등급)
train_dfc_n$공연시설인기 <- factor(train_dfc_n$공연시설인기)
train_dfc_n$성별 <- factor(train_dfc_n$성별)

train_dfc_n$공연_요일 <- factor(train_dfc_n$공연_요일)
train_dfc_n$공연_평일주말 <- factor(train_dfc_n$공연_평일주말)
train_dfc_n$공연_월 <- factor(train_dfc_n$공연_월)
train_dfc_n$공연_오전오후 <- factor(train_dfc_n$공연_오전오후)

train_dfc_n$예매_요일 <- factor(train_dfc_n$예매_요일)
train_dfc_n$예매_평일주말 <- factor(train_dfc_n$예매_평일주말)
train_dfc_n$예매_월 <- factor(train_dfc_n$예매_월)

train_dfc_n$단독판매여부 <- factor(train_dfc_n$단독판매여부)
train_dfc_n$아동공연.여부 <- factor(train_dfc_n$아동공연.여부)
train_dfc_n$축제.여부 <- factor(train_dfc_n$축제.여부)
train_dfc_n$내한공연.여부 <- factor(train_dfc_n$내한공연.여부)

train_dfc_n$결제수단코드 <- factor(train_dfc_n$결제수단코드)
train_dfc_n$예매방식코드 <- factor(train_dfc_n$예매방식코드)
train_dfc_n$할인종류코드 <- factor(train_dfc_n$할인종류코드)

valid_dfc_n$group <- factor(valid_dfc_n$group)
valid_dfc_n$성별 <- factor(valid_dfc_n$성별)

valid_dfc_n$세부장르명 <- factor(valid_dfc_n$세부장르명)
valid_dfc_n$미세먼지등급 <- factor(valid_dfc_n$미세먼지등급)
valid_dfc_n$공연시설인기 <- factor(valid_dfc_n$공연시설인기)
valid_dfc_n$성별 <- factor(valid_dfc_n$성별)

valid_dfc_n$공연_요일 <- factor(valid_dfc_n$공연_요일)
valid_dfc_n$공연_평일주말 <- factor(valid_dfc_n$공연_평일주말)
valid_dfc_n$공연_월 <- factor(valid_dfc_n$공연_월)
valid_dfc_n$공연_오전오후 <- factor(valid_dfc_n$공연_오전오후)

valid_dfc_n$예매_요일 <- factor(valid_dfc_n$예매_요일)
valid_dfc_n$예매_평일주말 <- factor(valid_dfc_n$예매_평일주말)
valid_dfc_n$예매_월 <- factor(valid_dfc_n$예매_월)

valid_dfc_n$단독판매여부 <- factor(valid_dfc_n$단독판매여부)
valid_dfc_n$아동공연.여부 <- factor(valid_dfc_n$아동공연.여부)
valid_dfc_n$축제.여부 <- factor(valid_dfc_n$축제.여부)
valid_dfc_n$내한공연.여부 <- factor(valid_dfc_n$내한공연.여부)

valid_dfc_n$결제수단코드 <- factor(valid_dfc_n$결제수단코드)
valid_dfc_n$예매방식코드 <- factor(valid_dfc_n$예매방식코드)
valid_dfc_n$할인종류코드 <- factor(valid_dfc_n$할인종류코드)

## train
X_cox <- subset(train_dfc_n, select = -c(수상실적개수))
train_C_re<-X_cox
train_C_re$공연_평일주말 <- NULL # NA
train_C_re$예매_평일주말 <- NULL # NA
train_C_re$할인종류코드 <- droplevels(train_C_re$할인종류코드, exclude =3)

# Valid 
valid_dfc_n$수상실적개수 <- NULL
valid_dfc_n$공연_평일주말 <- NULL # NA
valid_dfc_n$예매_평일주말 <- NULL # NA
valid_dfc_n <- subset(valid_dfc_n, 할인종류코드 != 3)


calculate_brier_score <- function(predicted_probs, actual_events, time_point) {
  time_indices <- which(valid_dfc_n$y <= time_point) # 해당 시간 이전의 데이터 인덱스 선택
  predicted_probs <- predicted_probs[time_indices] # 해당 시간 이전의 예측값 선택
  actual_events <- actual_events[time_indices]       # 해당 시간 이전의 실제 이벤트 선택
  brier_score <- mean((actual_events - predicted_probs)^2)
  return(brier_score)
}


train.fit_c <- coxph(Surv(y,delta)~., x=TRUE, y=TRUE, method='breslow', data=train_C_re)
valid_predictions_c <- predict(train.fit_c, newdata = valid_dfc_n, type = "expected")
brier_score_7_c <- calculate_brier_score(valid_predictions_c, valid_dfc_n$delta, 7)
brier_score_7_c #0.3438196

