
  
library(readxl)
library(tidyr)
library(survival)
library(dplyr)
library(glmnet)
library(survivalROC)
library(timeROC)


# 데이터 읽기 
set.seed(1004)

valid_dfn <- read.csv('valid_A8_n.csv')
test_dfn <- read.csv('test_A8_n.csv')
train_dfn <- read.csv('train_A8_n.csv')

valid_B <- read.csv('valid_B8_n.csv')
valid_dfc_n <- read.csv('valid_C8_n.csv')




# valid_C

## 제거한거
valid_dfc_n$공연_평일주말 <- NULL # NA
valid_dfc_n$예매_평일주말 <- NULL # NA
valid_dfc_n <- subset(valid_dfc_n, 할인종류코드 != 3)
valid_dfc_n$입장권고유번호 <- NULL
valid_dfc_n$수상실적개수 <- NULL

## factor
valid_dfc_n$group <- factor(valid_dfc_n$group)
valid_dfc_n$성별 <- factor(valid_dfc_n$성별)

valid_dfc_n$세부장르명 <- factor(valid_dfc_n$세부장르명)
valid_dfc_n$미세먼지등급 <- factor(valid_dfc_n$미세먼지등급)
valid_dfc_n$공연시설인기 <- factor(valid_dfc_n$공연시설인기)
valid_dfc_n$성별 <- factor(valid_dfc_n$성별)

valid_dfc_n$공연_요일 <- factor(valid_dfc_n$공연_요일)
valid_dfc_n$공연_월 <- factor(valid_dfc_n$공연_월)
valid_dfc_n$공연_오전오후 <- factor(valid_dfc_n$공연_오전오후)

valid_dfc_n$예매_요일 <- factor(valid_dfc_n$예매_요일)
valid_dfc_n$예매_월 <- factor(valid_dfc_n$예매_월)

valid_dfc_n$단독판매여부 <- factor(valid_dfc_n$단독판매여부)
valid_dfc_n$아동공연.여부 <- factor(valid_dfc_n$아동공연.여부)
valid_dfc_n$축제.여부 <- factor(valid_dfc_n$축제.여부)
valid_dfc_n$내한공연.여부 <- factor(valid_dfc_n$내한공연.여부)

valid_dfc_n$결제수단코드 <- factor(valid_dfc_n$결제수단코드)
valid_dfc_n$예매방식코드 <- factor(valid_dfc_n$예매방식코드)
valid_dfc_n$할인종류코드 <- factor(valid_dfc_n$할인종류코드)


#valid_B
valid_B <- valid_B[valid_B$공연_월 != 4,]
valid_B <- valid_B[valid_B$예매방식코드 != 4,]
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
valid_B$좌석수 <- NULL

valid_B$예매_월 <- factor(valid_B$예매_월)
valid_B$결제수단코드 <- factor(valid_B$결제수단코드)
valid_B$예매방식코드 <- factor(valid_B$예매방식코드)
valid_B$할인종류코드 <- factor(valid_B$할인종류코드)
X_valid_B <- valid_B[,-c(1,2)]
y_valid_B <- valid_B[, c(1,2)]

# A
cox_v<- coxph(Surv(y,delta) ~ ., data = valid_dfn)
cox.lp <- predict(cox_v, type = "lp")
t.ROC <- survivalROC(Stime = valid_dfn$y, status = valid_dfn$delta, marker = cox.lp, predict.time = 7, method = "KM")
AUC <- t.ROC$AUC # ROC 커브의 area under the curve , 0.67

# B
set.seed(1004)
cox.obj_B<-coxph(Surv(y,delta)~., data=valid_B)
cox.lp_B<-predict(cox.obj_B, type="lp")

t.ROC_B<-survivalROC(Stime=valid_B$y, status=valid_B$delta, marker=cox.lp_B, predict.time=7, method="KM")
AUC_B<-t.ROC_B$AUC #0.7778462

# C
set.seed(1004)
cox.obj_C<-coxph(Surv(y,delta)~., data=valid_dfc_n)
cox.lp_C<-predict(cox.obj_C, type="lp")

t.ROC_C<-survivalROC(Stime=valid_dfc_n$y, status=valid_dfc_n$delta, marker=cox.lp_C, predict.time=7, method="KM")
AUC_c<-t.ROC_C$AUC #0.6701


# X축: False positive rate, Y축: True positive rate
plot(t.ROC$FP, t.ROC$TP, type="l", xlab="1 - Specificity", ylab="Sensitivity", main=bquote('ROC at time t=7'))
abline(x=y=1)
c_index <- survConcordance(Surv(y, delta) ~ cox.lp, data = valid_dfn)


plot(t.ROC$FP, t.ROC$TP, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity", main = bquote('ROC at time t=7'),col="red", lwd = 2)
lines(t.ROC_B$FP, t.ROC_B$TP, col = "dodgerblue", lwd = 2)
lines(t.ROC_C$FP, t.ROC_C$TP, col = "seagreen", lwd = 2)
lines

abline(h = 0, v = 0, col = "gray")

# 랜덤 포레스트 ROC 커브 그리기 (빨간 점선)
abline(a = 0, b = 1, col = "black", lty = 2, lwd = 2)


legend_text <- c(paste("예매점유율 상, AUC = ", round(AUC, 4)),
                 paste("예매점유율 중, AUC = ", round(AUC_B, 4)),
                 paste("예매점유율 하, AUC = ", round(AUC_c, 4)))

# LEGEND 추가
legend("bottomright", legend = c( legend_text), col = c( "red", "dodgerblue", "seagreen"), lty = c(1, 1, 1), lwd=c(2,2,2),
       title = "Models", cex = 0.8)


















  
# 인기도 A그룹 전처리 

aggregate(delta ~ 예매방식코드, data = train_dfn, FUN = function(x) c(Delta_0 = sum(x == 0), Delta_1 = sum(x == 1)))

unique(train_dfn$좌석수)
unique(train_dfn$매출액)
unique(train_dfn$좌석점유율)


## original data

train_dfn$입장권고유번호 <- NULL

train_dfn$group <- factor(train_dfn$group)
train_dfn$성별 <- factor(train_dfn$성별)

train_dfn$세부장르명 <- factor(train_dfn$세부장르명)
train_dfn$미세먼지등급 <- factor(train_dfn$미세먼지등급)
train_dfn$공연시설인기 <- factor(train_dfn$공연시설인기)
train_dfn$성별 <- factor(train_dfn$성별)
train_dfn$공연_요일 <- factor(train_dfn$공연_요일)
train_dfn$공연_평일주말 <- factor(train_dfn$공연_평일주말)
train_dfn$공연_월 <- factor(train_dfn$공연_월)

train_dfn$예매_요일 <- factor(train_dfn$예매_요일)
train_dfn$예매_평일주말 <- factor(train_dfn$예매_평일주말)
train_dfn$예매_월 <- factor(train_dfn$예매_월)

train_dfn$단독판매여부 <- factor(train_dfn$단독판매여부)


train_dfn$결제수단코드 <- factor(train_dfn$결제수단코드)
train_dfn$예매방식코드 <- factor(train_dfn$예매방식코드)



# EN 에서 coef = 0으로 추정됨
train_dfn$팔로워합 <- NULL


# 잠만
train_dfn$일일확진자 <- NULL
train_dfn$좌석수 <- NULL
train_dfn$매출액 <- NULL
train_dfn$예매금액 <- NULL


#coxph시 NULL값으로 추정됨
train_dfn$공연_평일주말 <- NULL
train_dfn$예매_평일주말 <- NULL
train_dfn$시설합 <- NULL
train_dfn$좌석점유율 <- NULL
train_dfn$매출액 <- NULL
train_dfn$좌석수 <- NULL

#train_dfn$공연시설인기 <- NULL
#train_dfn$세부장르명<- NULL
# train_dfn$예매방식코드 <- droplevels(train_dfn$예매방식코드, exclude=4)


# test
test_dfn$입장권고유번호 <- NULL

test_dfn$group <- factor(test_dfn$group)
test_dfn$성별 <- factor(test_dfn$성별)

test_dfn$세부장르명 <- factor(test_dfn$세부장르명)
test_dfn$미세먼지등급 <- factor(test_dfn$미세먼지등급)
test_dfn$공연시설인기 <- factor(test_dfn$공연시설인기)
test_dfn$성별 <- factor(test_dfn$성별)
test_dfn$공연_요일 <- factor(test_dfn$공연_요일)
#test_dfn$공연_평일주말 <- factor(test_dfn$공연_평일주말)
test_dfn$공연_월 <- factor(test_dfn$공연_월)
test_dfn$예매_요일 <- factor(test_dfn$예매_요일)
#test_dfn$예매_평일주말 <- factor(test_dfn$예매_평일주말)
test_dfn$예매_월 <- factor(test_dfn$예매_월)

test_dfn$단독판매여부 <- factor(test_dfn$단독판매여부)


test_dfn$결제수단코드 <- factor(test_dfn$결제수단코드)
test_dfn$예매방식코드 <- factor(test_dfn$예매방식코드)

test_dfn$공연_평일주말 <- NULL
test_dfn$예매_평일주말 <- NULL
#test_dfn$예매방식코드 <- droplevels(test_dfn$예매방식코드, exclude=4)
test_dfn$좌석수 <- NULL
test_dfn$매출액 <- NULL
test_dfn$예매금액 <- NULL
test_dfn$팔로워합 <- NULL
test_dfn$일일확진자 <- NULL

test_dfn$좌석점유율 <- NULL
test_dfn$시설합 <- NULL
#test_dfn$공연시설인기 <- NULL
#test_dfn$세부장르명<- NULL




# valid
valid_dfn$입장권고유번호 <- NULL

valid_dfn$group <- factor(valid_dfn$group)
valid_dfn$성별 <- factor(valid_dfn$성별)

valid_dfn$세부장르명 <- factor(valid_dfn$세부장르명)
valid_dfn$미세먼지등급 <- factor(valid_dfn$미세먼지등급)
valid_dfn$공연시설인기 <- factor(valid_dfn$공연시설인기)
valid_dfn$성별 <- factor(valid_dfn$성별)
valid_dfn$공연_요일 <- factor(valid_dfn$공연_요일)
#valid_dfn$공연_평일주말 <- factor(valid_dfn$공연_평일주말)
valid_dfn$공연_월 <- factor(valid_dfn$공연_월)
valid_dfn$예매_요일 <- factor(valid_dfn$예매_요일)
#valid_dfn$예매_평일주말 <- factor(valid_dfn$예매_평일주말)
valid_dfn$예매_월 <- factor(valid_dfn$예매_월)

valid_dfn$단독판매여부 <- factor(valid_dfn$단독판매여부)


valid_dfn$결제수단코드 <- factor(valid_dfn$결제수단코드)
valid_dfn$예매방식코드 <- factor(valid_dfn$예매방식코드)



#valid_dfn$예매방식코드 <- droplevels(valid_dfn$예매방식코드, exclude=4)

# EN 에서 coef = 0으로 추정됨
valid_dfn$팔로워합 <- NULL

# 잠만
valid_dfn$좌석수 <- NULL
valid_dfn$매출액 <- NULL
valid_dfn$예매금액 <- NULL
valid_dfn$일일확진자 <- NULL


valid_dfn$좌석점유율 <- NULL
valid_dfn$시설합 <- NULL
#valid_dfn$공연시설인기 <- NULL
#valid_dfn$세부장르명<- NULL

valid_dfn$공연_평일주말 <- NULL
valid_dfn$예매_평일주말 <- NULL






# COX PHM


set.seed(1004)

# Cox 비례 위험 모델 적합 및 회귀계수, p-value 저장 

cox_original <- coxph(Surv(y,delta) ~ ., data = train_dfn)
summary(cox_original)


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


cox_o_sum <- cox_summary_to_df(cox_original)

cox_o_sum


write.csv(cox_o_sum, file = "A_cox.csv", row.names = TRUE)















# Cox ElasticNet (dummy화된 DATASET 이용)

test_dfnd <- read.csv('test_A_dummy_nonfiltering.csv')
train_dfnd <- read.csv('train_A_dummy_nonfiltering.csv')
valid_dfnd <- read.csv('val_A_dummy_nonfiltering.csv')


# z scaled
x <- train_dfnd[,-c(1,2)]
x_mat <- as.matrix(x)
y <- train_dfnd[,c(1,2)]

surv_obj <- with(y, Surv(y,delta))


cvfit <- cv.glmnet(x_mat,surv_obj, family="cox", type.measure="C",nfolds=5)

best_alpha <- cvfit$glmnet.fit$alpha
best_lambda <- cvfit$lambda.min

opt_fit <- glmnet(x_mat, surv_obj,  lambda=best_lambda, family = "cox")
coef(opt_fit)
EN_result <- round(coef(opt_fit, s=best_lambda),3)

EN_result <- round(coef(opt_fit_o, s=best_lambda2),6)


as.matrix(EN_result)


EN_result_exp <- exp(coef(opt_fit_o, s=best_lambda2))
EN_result_exp

plot(fit)
plot(cvfit_o)
round(best_lambda2,10)




dgma_non<-as(EN_result, "matrix")
df_dgma_non<-as.data.frame(dgma_non)
write.csv(df_dgma_non, file = "A_optimal_coef.csv", row.names = TRUE)








# 결과 저장

# Z-scaled 결과 저장
write.csv(z_scaled_result, file = "A_EN_z_scaled.csv")

# Original scale 결과 저장
write.csv(original_scale_result, file = "A_EN_original_scale.csv")

write.csv(cox_o_sum, file = "A_original")
write.csv(cox_z_sum, file = "A_z")



# 이전 분석 결과 불러오기
cox_o <- read.csv('A_original.csv')
cox_z <- read.csv('A_z.csv')

















# Valid

## 1. AUC

# 성과지표용 Valid fitting

cox_v<- coxph(Surv(y,delta) ~ ., data = valid_dfn)
# summary(cox_v)


# valid_dfn기준 ! 
cox.lp <- predict(cox_v, type = "lp")

# CD1 - survivalROC 사용
t.ROC <- survivalROC(Stime = valid_dfn$y, status = valid_dfn$delta, marker = cox.lp, predict.time = 7, method = "KM")

AUC <- t.ROC$AUC # ROC 커브의 area under the curve , 0.67

# X축: False positive rate, Y축: True positive rate
plot(t.ROC$FP, t.ROC$TP, type="l", xlab="1 - Specificity", ylab="Sensitivity", main=bquote('ROC at time t=7'))
abline(x=y=1)
c_index <- survConcordance(Surv(y, delta) ~ cox.lp, data = valid_dfn)






## 2. brier_score

# 예측 시간 (예: t=7)
predict_time <- 7

# fit
cox_v<- coxph(Surv(y,delta) ~ ., data = valid_dfn)


# 예측값 계산
valid_predictions <- predict(cox_v, newdata = valid_dfn, type = "expected")

# Brier 점수 계산 함수
calculate_brier_score <- function(predicted_probs, actual_events, time_point) {
  time_indices <- which(valid_dfn$y <= time_point) # 해당 시간 이전의 데이터 인덱스 선택
  predicted_probs <- predicted_probs[time_indices] # 해당 시간 이전의 예측값 선택
  actual_events <- actual_events[time_indices]       # 해당 시간 이전의 실제 이벤트 선택
  brier_score <- mean((actual_events - predicted_probs)^2)
  return(brier_score)
}

# 특정 시점 (예: t=7) 에 대한 Brier 점수 계산
brier_score_7 <- calculate_brier_score(valid_predictions, valid_dfc_n$delta, predict_time)

brier_score_7

 



## 3. C - index
library(survival)
library(survAUC)
train.fit <- coxph(Surv(y,delta)~., x=TRUE, y=TRUE, method='breslow', data=train_dfn)

lp <- predict(train.fit)
lpnew <- predict(train.fit, newdata=valid_dfn)
GHCI(lpnew)



