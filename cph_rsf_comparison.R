### packages

library(rms)
library(glmnet)
library(randomForestSRC)
library(ggRandomForests)
library(boot)

### data

data(pbc, package = "randomForestSRC")
pbc <- pbc[complete.cases(pbc),]

### single cox regression

full.cox = coxph(Surv(days, status) ~ ., data = pbc)
summary(full.cox)
write.csv(round(coef(full.cox), 2))

### single rsf

rsf <- rfsrc(Surv(days, status) ~ ., pbc)
print(rsf)

### compare cox lasso variable selection and rsf vimp

# penalised cox variable selection 

x <- as.matrix(pbc[3:ncol(pbc)])
y <- Surv(pbc$days, pbc$status)

cox.fit <- glmnet(x, y, family="cox", alpha=1)
plot(cox.fit, label=T)

cox.cv <- cv.glmnet(x, y, family="cox", alpha=1)
cox.cv$lambda.1se # lambda = 0.1343436
plot(cox.cv)

coef(cv.fit, s = "lambda.1se")

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
