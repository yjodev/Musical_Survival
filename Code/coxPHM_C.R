
install.packages('survminer')
install.packages('glmnet')

library(readxl)
library(tidyr)
library(survival)
library(dplyr)
library(glmnet)
library(survminer)
  

# 데이터 전처리 

dfc <- read.csv("/Users/juna/Downloads/sdf_C_new.csv")


## non-scaled
test_dfc_n <- read.csv('/Users/user/Downloads/test_C8_n.csv')
train_dfc_n <- read.csv('/Users/user/Downloads/train_C8_n.csv')
valid_dfc_n <- read.csv('/Users/user/Downloads/valid_C8_n.csv')

## non-scaled
train_dfc_n$입장권고유번호 <- NULL

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


str(train_dfc_n)


valid_dfc_n$입장권고유번호 <- NULL

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


str(valid_dfc_n)



test_dfc_n$입장권고유번호 <- NULL

test_dfc_n$group <- factor(test_dfc_n$group)
test_dfc_n$성별 <- factor(test_dfc_n$성별)

test_dfc_n$세부장르명 <- factor(test_dfc_n$세부장르명)
test_dfc_n$미세먼지등급 <- factor(test_dfc_n$미세먼지등급)
test_dfc_n$공연시설인기 <- factor(test_dfc_n$공연시설인기)
test_dfc_n$성별 <- factor(test_dfc_n$성별)

test_dfc_n$공연_요일 <- factor(test_dfc_n$공연_요일)
test_dfc_n$공연_평일주말 <- factor(test_dfc_n$공연_평일주말)
test_dfc_n$공연_월 <- factor(test_dfc_n$공연_월)
test_dfc_n$공연_오전오후 <- factor(test_dfc_n$공연_오전오후)

test_dfc_n$예매_요일 <- factor(test_dfc_n$예매_요일)
test_dfc_n$예매_평일주말 <- factor(test_dfc_n$예매_평일주말)
test_dfc_n$예매_월 <- factor(test_dfc_n$예매_월)

test_dfc_n$단독판매여부 <- factor(test_dfc_n$단독판매여부)
test_dfc_n$아동공연.여부 <- factor(test_dfc_n$아동공연.여부)
test_dfc_n$축제.여부 <- factor(test_dfc_n$축제.여부)
test_dfc_n$내한공연.여부 <- factor(test_dfc_n$내한공연.여부)

test_dfc_n$결제수단코드 <- factor(test_dfc_n$결제수단코드)
test_dfc_n$예매방식코드 <- factor(test_dfc_n$예매방식코드)
test_dfc_n$할인종류코드 <- factor(test_dfc_n$할인종류코드)


str(test_dfc_n)


# 1) Elastic을 먼저 돌려보자

train_C_dummy_non <- read.csv("/Users/user/Downloads/train_C_dummy_nonfiltering.csv", header=TRUE)

set.seed(1004)
## non-scaled

y_train_C_non <- train_C_dummy_non[,c(1,2)]
X_train_C_non <- train_C_dummy_non[,-c(1,2)]

y_non <- with(y_train_C_non, Surv(y,delta))

X_train_non_mat <- as.matrix(X_train_C_non)


fit_non <- glmnet(X_train_non_mat, y_non, family = "cox", alpha= 0.5)

cvfit_non <- cv.glmnet(X_train_non_mat, y_non, family = "cox",alpha= 0.5, type.measure = "C", nfolds = 5)

best_alpha_non <- cvfit_non$glmnet.fit$alpha
best_lambda_non <- cvfit_non$lambda.min
optimal_non <- glmnet(X_train_non_mat, y_non, family = "cox", alpha= 0.5, lambda = best_lambda_non)
a_non<- round(coef(optimal_non, s='lambda.min'),3)

dgma_non<-as(a_non, "matrix")
df_dgma_non<-as.data.frame(dgma_non)
write.csv(df_dgma_non, file = "/Users/user/Downloads/C_optimal_coef.csv", row.names = TRUE)



best_lambda_non
plot(fit_non)
plot(cvfit_non)



set.seed(1004)
# 2) 이제 COX를 돌려보자

# 수상 실적을 빼보자
X_cox <- subset(train_dfc_n, select = -c(수상실적개수))

# Cox 비례 위험 모델 적합
cox_n <- coxph(Surv(y,delta) ~ ., data = X_cox)

# 모델 요약 출력
summary(cox_n) # Concordance= 0.66
AIC(cox_n) # 7595781
BIC(cox_n) # 7596597

cox_n_c <- cox_summary_to_df(cox_n)


aggregate(delta~할인종류코드,data=X_cox,FUN=function(x)sum(x==1))


### 이상한 값 전처리
# non-scaled
train_C_re<-X_cox

train_C_re$공연_평일주말 <- NULL # NA
train_C_re$예매_평일주말 <- NULL # NA
train_C_re$할인종류코드 <- droplevels(train_C_re$할인종류코드, exclude =3)


set.seed(1004)
# Cox 비례 위험 모델 적합
cox_all_n_re <- coxph(Surv(y,delta) ~ ., data = train_C_re)

# 모델 요약 출력
summary(cox_all_n_re) # Concordance= 0.66
AIC(cox_all_n_re) # 7593539
BIC(cox_all_n_re) # 7594323



### 표로 보기
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

cox_c <- cox_summary_to_df(cox_all_n_re)

write.csv(cox_c, file = "C_cox_2.csv", row.names = TRUE)




# Valid df로 예측하기
valid_dfc_n$수상실적개수 <- NULL
valid_dfc_n$공연_평일주말 <- NULL # NA
valid_dfc_n$예매_평일주말 <- NULL # NA
valid_dfc_n <- subset(valid_dfc_n, 할인종류코드 != 3)


#C
library(survival)
library(survAUC)
train.fit <- coxph(Surv(y,delta)~., x=TRUE, y=TRUE, method='breslow', data=train_C_re)

lp <- predict(train.fit)
lpnew <- predict(train.fit, newdata=valid_dfc_n)
GHCI(lpnew)


library(SurvMetrics)
BS_cox <- Brier(train.fit, valid_dfc_n, 7)
mat_cox <- predictSurvProb(train.fit, valid_dfc_n, 7)
IBS_cox <- IBS(Surv(valid_dfc_n$y, valid_dfc_n$delta), mat_cox, 7)






## Briers
# 테스트 데이터로 예측
valid_predictions <-predict(cox_all_n_re, newdata = valid_dfc_n, type = "expected")

actual_events <- valid_dfc_n$delta
model_predictions <- valid_predictions

# Brier 점수 계산
brier_score <- mean((actual_events - model_predictions)^2)
brier_score #0.2787388






