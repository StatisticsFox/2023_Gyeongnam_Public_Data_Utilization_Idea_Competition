library(olsrr)
library(MASS)
library(car)

gn = read.csv("C:/Users/sos35/OneDrive/문서/R/변수종합-이상치 처리.csv", header=TRUE, sep=",")

gn[,7] <- gsub(",", "", gn[, 7]) # 쉼표 제거
gn[,7] <- as.numeric(gn[,7]) # 수치형으로 변환
gn[,8] <- gsub(",", "", gn[, 8]) # 쉼표 제거
gn[,8] <- as.numeric(gn[,8]) # 수치형으로 변환
gn[,9] <- gsub(",", "", gn[, 8]) # 쉼표 제거
gn[,9] <- as.numeric(gn[,8]) # 수치형으로 변환


# 모든 컬럼 이름 변경
names(gn) <- c("행정동", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12")

gn = data.frame(gn)
gn = scale(gn[-1])

# 산점도 확인 
plot(gn[-1])

# 변수 치환
X1 = gn$X1
X2 = gn$X2
X3 = gn$X3
X4 = gn$X4
X5 = gn$X5
X6 = gn$X6
X7 = gn$X7
X8 = gn$X8
X9 = gn$X9
X10 = gn$X10
X11 = gn$X11
X12 = gn$X12


# 다중공선성 확인(Vif)
fit = lm(X7~X1+X2+X3+X4+X5+X6+X8+X9+X10+X11+X12)
vif(fit)
# ols_coll_diag(fit)


# StepAIC 방법을 적용
stepAIC(lm(X7~1, data=gn), X7~X1+X2+X3+X4+X5+X6+X8+X9+X10+X11+X12, direcion="both")


# 마지막으로 추가된 X2변수를 넣은것과 안넣은 것에 대해 anova 검정
reg1 = lm(X7~0+X8+X11+X1+X5+X2)
reg2 = lm(X7~X8+X11+X1+X5)
anova(reg1,reg2) # 귀무 기각 -> 완전모형이 유의(따라서 stepAIC 결과와 동일한 결과 출력)

summary(reg1)



# backward 방식
reg0 = lm(X7~X1+X2+X3+X4+X5+X6+X8+X9+X10+X11+X12)
ols_step_backward_aic(reg0, progress=T,details=T) # backward 방식 사용하여도 stepAIC와 동일하게 결과 출력 
reg1 = lm(X7~0+X1+X2+X5+X8+X9+X11+X12)
summary(reg1)



# reg1 = lm(X10~X11+X14+X12+X15+X8+X2)
# reg2 = lm(X10~X11+X14+X12+X15+X8+X2+X4)
# anova(reg1,reg2) # 귀무 기각X -> 축소모형(따라서 backward 결과 그대로 사용)



# 전진선택법 - stepAIC와 동일한 결과
reg.F = lm(X7~X1+X2+X3+X4+X5+X6+X8+X9+X10+X11+X12)
ols_step_forward_aic(reg.F, progress = F, details = F) # 비교 기준이 AIC가 됨, 



# 최종 모형(회귀진단)
reg1 = lm(X7~0+X8+X11+X1+X5+X2) # 전진선택, stepAIC
reg2 = lm(X7~0+X1+X2+X5+X8+X9+X11+X12) # 후진소거
reg3 = lm(X7~0+X8+X9+X11) # 라쏘

fit = lm(X7~0+X8+X11+X1+X5+X2, x=T, y=T)
summary(fit)
par(mfrow = c(2, 3))
plot(fit, which = 1:6)


fit1 = lm(X7~0+X1+X2+X5+X8+X9+X11+X12, x=T, y=T)
summary(fit1)
plot(fit1, which = 1:6)

fit2 = lm(X7~0+X8+X9+X11, x=T, y=T)
summary(fit2)
plot(fit1, which = 1:6)

