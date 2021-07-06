### censoring rates simulation

# packages

library(coxed)
library(dplyr)
library(randomForestSRC)

# generate data

cens <- lapply(c(.01,.25,.50,.75,1), function(x) {
  sim.survdata(N=1000, T=100, xvars=1, censor=x, num.data.frames = 1) 
}) # data stored in : cens[[x]]$data

# train test split

set.seed(1998)

index <- sample(1:nrow(cens[[1]]$data), size = trunc(.8 * nrow(cens[[1]]$data))) # 80:20 train-test split

cens_train <- lapply(c(1,2,3,4,5), function(x) {
  filter(cens[[x]]$data, row_number() %in% index)
})

cens_test <- lapply(c(1,2,3,4,5), function(x) {
  filter(cens[[x]]$data, !row_number() %in% index)
})

# fit RSF on training set and predict on test set

rsf.fit <- lapply(c(1,2,3,4,5), function(x){
  rfsrc(Surv(y, failed) ~ X, cens_train[[x]])
})

print(rsf.fit)
rsf.pred <- lapply(c(1,2,3,4,5), function(x) {
  # predicted 10 year survival probabilities for individuals in test set
  predictSurvProb(rsf.fit[[x]], newdata = cens_test[[x]], times = 10 * 365.25)
})




