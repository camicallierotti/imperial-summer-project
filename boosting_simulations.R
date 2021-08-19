### censoring rates simulation

# packages

library(coxed)
library(dplyr)
library(survival)
library(xgboost)
install.packages("pacman")
pacman::p_load_gh("IyarLin/survXgboost")
library(dynpred)
library(pec)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("survcomp")

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

xgb.fit <- lapply(c(1,2,3,4,5), function(x){
  # create survival label (y)
  y <- ifelse(cens_train[[x]]$failed == TRUE, cens_train[[x]]$y, -cens_train[[x]]$y) # equivalent to surv object
  # create predictors (X)
  X <- cens_train[[x]] %>% select(-y,-failed)
  # set parameters
  params <- list(objective = "survival:cox", # type of dependent variable
                 eval_metric = "cox-nloglik" # evaluation metric for survival problems
  )
  # run xgboost
  model <- xgboost(data = as.matrix(X), # xgboost library requires data as matrix
                   label = y,
                   params = params,
                   nrounds = 20, # number of times xgboost runs
                   verbose = 1,
  )
})

# predict on test set

xgb.pred <- lapply(c(1,2,3,4,5), function(x) {
  # create predictors (X)
  X <- cens_test[[x]] %>% select(-y,-failed)
  # model
  model <- xgb.fit[[x]]
  # predict
  pred <- predict(model, as.matrix(X))
})

# c-index

surv <- lapply(c(1,2,3,4,5), function(x) {
  Surv(cens_test[[x]]$y, cens_test[[x]]$failed)
})

cindex <- lapply(c(1,2,3,4,5), function(x) {
  survConcordance(surv[[x]] ~ xgb.pred[[x]]) 
})

# c-index with dynpred package

