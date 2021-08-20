### censoring rates simulation

# packages

library(coxed)
library(dplyr)
library(survival)
library(pec)

# generate data

cens <- lapply(c(.01,.25,.50,.75,.99), function(x) {
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

# fit cox ph on training set and predict on test set

cox.fit <- lapply(c(1,2,3,4,5), function(x){
  coxph(Surv(y, failed) ~ X, cens_train[[x]], x=T)
})

cox.pred <- lapply(c(1,2,3,4,5), function(x) {
  # predicted 10 year survival probabilities for individuals in test set
  (predictSurvProb(cox.fit[[x]], newdata = cens_test[[x]], times = 10 * 365.25))*100
})

# get concordance index

cindex <- lapply(c(1,2,3,4,5), function(x) {
  concordance(cox.fit[[x]], newdata = cens_test[[x]])
})

# plot predicted survival curves for individual with covariate x=0 using 5 RSFs

newData <- data.frame(X=0, y=100, failed=FALSE)

plotPredictSurvProb(x = rsf.fit[[1]],
                    newdata = newData,
                    times = 100,
                    lty = 1,
                    legend = TRUE,
                    percent = TRUE
) # this method needs to select an individual to predict for


