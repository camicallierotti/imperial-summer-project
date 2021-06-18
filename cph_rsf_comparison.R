### packages

library(tidyverse)
library(dplyr)
library(pec)
library(rms)
library(glmnet)
library(vimp)
library(randomForestSRC)
library(ggRandomForests)
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

cox.fit = coxph(Surv(days, status) ~ ., data = pbc_train, x = TRUE)
summary(cox.fit)
write.table(format(coef(cox.fit), scientific=TRUE))
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

par(mfrow = c(1, 3))

lapply(c(20,46,43), function(x) {
  plotPredictSurvProb(cox.fit, newdata = pbc_test[x, ], lty = 1) 
  plotPredictSurvProb(rsf.fit, newdata = pbc_test[x, ], add = TRUE, lty = 2) 
}) # predicted survival curves with Cox for 3 test individuals of different ages

### compare cox and rsf vimp

# penalised cox variable selection 

x <- as.matrix(pbc[3:ncol(pbc)])
y <- Surv(pbc$days, pbc$status)

lasso.fit <- glmnet(x, y, family="cox", alpha=1)
plot(lasso.fit, label=T)

lasso.cv <- cv.glmnet(x, y, family="cox", alpha=1)
lasso.cv$lambda.1se # lambda = 0.1343436
plot(lasso.cv)

coef(lasso.fit, s = "lambda.1se")

# cox vimp

cox.vimp <- anova(full.cox)
plot(cox.vimp,
     what="proportion R2", #proportion of overall model R2 that is due to each predictor
     xlab=NULL, pch=16,
     rm.totals=TRUE, rm.ia=FALSE, rm.other=NULL,
     sort="descending", margin='P',
     pl=TRUE, trans=NULL, ntrans=40, height=NULL, width=NULL)

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

### compare cox and rsf bootstrap c-index estimates 

# cox 1

foo = function(Data, ind){
  fit = coxph(Surv(days, status) ~ treatment, data = pbc[ind,])
  DescTools::Cstat(fit)
}

set.seed(1998)
myBootstrap = boot(pbc, foo, R=999)

# cox 2

n = length(pbc$status)
B = 999
result = rep(NA, B)
for (i in 1:B) {
  boot.sample = sample(n, replace = TRUE)
  fit = coxph(Surv(days, status) ~ treatment, data = pbc)
  result[i] = DescTools::Cstat(fit)
}
