### censoring rates simulation

# packages

library(coxed)
library(dplyr)
library(survival)
install.packages("pacman")
pacman::p_load_gh("IyarLin/survXgboost")
library(xgboost)
library(pec)

# generate data

cens <- lapply(c(.01,.25,.50,.75,.99), function(x) {
  sim.survdata(N=1000, T=100, xvars=2, censor=x, num.data.frames = 1) # coxboost needs >=2 covariates
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

# fit xgboost on training set

surv <- Surv(cens_train[[1]]$y, cens_train[[1]]$failed)
is.Surv(surv)

xgb.train <- xgboost::xgboost(data = as.matrix(cens_train[[1]]), 
                              label = surv, #cens_train[[1]]$failed
                              max.depth = 2, 
                              eta = 1, 
                              nthread = 2, 
                              nrounds = 2, 
                              objective = "survival:cox")

