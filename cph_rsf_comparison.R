### packages

library(xtable)
library(tidyverse)
library(dplyr)
library(pec)
library(rms)
library(glmnet)
library(vimp)
library(randomForestSRC)
library(ggRandomForests)
library(coxed)
library(boot)

### data

data(pbc, package = "randomForestSRC")
pbc <- pbc[complete.cases(pbc),]
set.seed(1998)
index <- sample(1:276, size = trunc(.8 * 276)) # 80:20 train-test split
pbc_train <- pbc %>%
  filter(row_number() %in% index)
pbc_test <- pbc %>%
  filter(!(row_number() %in% index))

### compare full cox and RSF predictions

# fit and predict cox

cox.fit = coxph(Surv(days, status) ~ ., data = pbc_train, x = TRUE, y = TRUE)
summary(cox.fit)
cox.coef <- (format(coef(cox.fit), scientific=TRUE))
cox.pred <- predictSurvProb(cox.fit, newdata = pbc_test, times = 10 * 365.25) # predicted 10 year survival probabilities for individuals in test set (n=56)

# fit and predict RSF

rsf.fit <- rfsrc(Surv(days, status) ~ ., pbc_train)
print(rsf.fit)
rsf.pred <- predictSurvProb(rsf.fit, newdata = pbc_test, times = 10 * 365.25) # predicted 10 year survival probabilities for individuals in test set (n=56)

# select 3 individuals to make predictions for

which.min(pbc_test$age) # row 20 has individual with min age
which.max(pbc_test$age) # row 43 has individual with max age
quantile(pbc_test$age, .5, type = 1) # 16941 days is median age
which(pbc_test$age == 16941) # row 46 has individual with median age

# predictions for 3 individuals

lapply(c(20,46,43), function(x) {
  (predictSurvProb(cox.fit, newdata = pbc_test[x, ], times = 10 * 365.25))*100
}) # predicted 10 year survival probabilities with Cox for 3 test individuals of different ages

lapply(c(20,46,43), function(x) {
  (predictSurvProb(rsf.fit, newdata = pbc_test[x, ], times = 10 * 365.25) )*100
}) # predicted 10 year survival probabilities with RSF for 3 test individuals of different ages

# predicted survival curves for 3 individuals

m <- matrix(c(1,2,3,4,4,4),nrow = 2,ncol = 3,byrow = TRUE)
layout(mat = m,height0s = c(0.4,0.2))

lapply(c(20,46,43), function(x) {
  plotPredictSurvProb(cox.fit, newdata = pbc_test[x, ], lty = 1, legend = TRUE, percent = TRUE) 
  plotPredictSurvProb(rsf.fit, newdata = pbc_test[x, ], add = TRUE, lty = 2, legend = TRUE, percent = TRUE)
}) 

plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "bottom",inset = 0,
       legend = c("Cox regression", "RSF"), lty=1:2, lwd=1, cex=1, horiz = TRUE)

# concordance index

concordance(cox.fit, newdata = pbc_test) # concordance function cannot be applied to rsf
rsf.pred <- predict(rsf.fit, newdata = pbc_test, importance = 'permute', cse = TRUE) # can also make predictions for RSF using predict.rfsrc from randomForestSRC package

### compare lasso cox and rsf vimp

# lasso cox variable selection 

x <- as.matrix(pbc[3:ncol(pbc)])
y <- Surv(pbc$days, pbc$status)

lasso.fit <- glmnet(x, y, family="cox", alpha=1)
plot(lasso.fit, label=T)

lasso.cv <- cv.glmnet(x, y, family="cox", alpha=1)
lasso.cv$lambda.1se
plot(lasso.cv)

lasso.coef <- format(as.matrix(coef(lasso.fit, s = lasso.cv$lambda.1se)), scientific=TRUE)
cox.coef.table <- cbind(cox.coef, lasso.coef)
xtable(cox.coef.table, type = "latex")

# cox vimp

cox.vimp <- anova(cox.fit)
plot(cox.vimp,
     what="proportion R2", # proportion of overall model R2 that is due to each predictor
     xlab=NULL, pch=16,
     rm.totals=TRUE, rm.ia=FALSE, rm.other=NULL,
     sort="descending", margin='P',
     pl=TRUE, trans=NULL, ntrans=40, height=NULL, width=NULL)

plot(anova(cox.fit))

# alternative cox vimp

x <- as.data.frame(x)
vimp_rsquared(y, x)

# rsf vimp 

rsf.fit <- rfsrc(Surv(days, status) ~ ., data = pbc)

print(vimp(rsf.fit)$importance) # permutation vimp
print(vimp(rsf.fit, perf.type = "brier")$importance) # vimp using brier prediction error
print(vimp(rsf.fit, importance = "random")$importance) # Random daughter vimp
print(vimp(iris.obj, joint = TRUE)$importance) # Joint permutation vimp

rsf_vimp <- gg_vimp(rsf.fit)
plot(rsf_vimp)

### compare cox and rsf bootstrap c-index estimates (not finished)

# cox

n = length(pbc$status)
B = 999
result = rep(NA, B)
for (i in 1:B) {
  boot.sample = sample(n, replace = TRUE)
  fit = coxph(Surv(days, status) ~ treatment, data = pbc_train)
  pred = predictSurvProb(cox.fit, newdata = pbc_test, times = 10 * 365.25) # predict 0/1 or probabilities?
  result[i] = DescTools::Cstat(pred, pbc_test$status) # resp argument must be 0/1
}
