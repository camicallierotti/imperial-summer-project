### censoring rates simulation

# packages

library(coxed)
library(dplyr)
library(survival)
library(xgboost)
install.packages("pacman")
pacman::p_load_gh("IyarLin/survXgboost")
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

# train an xgboost survival model using the function from the survXgboost package rather than the xgboost package

cens_train[[1]]$failed <- ifelse(cens_train[[1]]$failed == TRUE, 1, 0) # 1 if event occurred, 0 if censored
label <- ifelse(cens_train[[1]]$failed == 1, cens_train[[1]]$y, -cens_train[[1]]$y) # equivalent to surv object

val_ind <- sample.int(nrow(cens_train[[1]]), 0.1 * nrow(cens_train[[1]]))
x_train <- as.matrix(cens_train[[1]][-val_ind, !names(cens_train[[1]]) %in% c("y", "status")])
x_train <- x_train[,-3] # remove "failed" feauture
x_label <- label[-val_ind]
x_val <- xgb.DMatrix(as.matrix(cens_train[[1]][val_ind, !names(cens_train[[1]]) %in% c("y", "status")]),
                     label = label[val_ind])

surv_xgboost_model <- xgb.train.surv(
  params = list(objective = "survival:cox",
                eval_metric = "cox-nloglik",
                eta = 0.05), # larger eta leads to algorithm not converging, resulting in NaN predictions
  data = x_train, label = x_label,
  watchlist = list(val2 = x_val),
  nrounds = 1000, early_stopping_rounds = 30
)

# predict full survival curves

cens_test[[1]] <- xgb.DMatrix(as.matrix(cens_test[[1]][1:2]), label=cens_test[[1]]$y)

times <- seq(10, 1000, 50)
survival_curves <- predict(object = surv_xgboost_model, newdata = cens_test[[1]], type = "surv", times = times)
matplot(times, t(survival_curves[1:5, ]), type = "l")


### apply same thing to all 5 datasets with different censoring rates

surv_xgboost_model <- lapply(c(1,2,3,4,5), function(x) {
  cens_train[[x]]$status <- ifelse(cens_train[[x]]$failed == TRUE, 1, 0) # 1 if event occurred, 0 if censored
  label <- ifelse(cens_train[[x]]$status == 1, cens_train[[x]]$y, -cens_train[[x]]$y) # equivalent to surv object
  val_ind <- sample.int(nrow(cens_train[[x]]), 0.1 * nrow(cens_train[[x]]))
  x_train <- as.matrix(cens_train[[x]][-val_ind, !names(cens_train[[x]]) %in% c("y", "status")])
  x_label <- label[-val_ind]
  x_val <- xgb.DMatrix(as.matrix(cens_train[[x]][val_ind, !names(cens_train[[x]]) %in% c("y", "status")]),
                       label = label[val_ind])
  xgb.train.surv(params = list(objective = "survival:cox",
                               eval_metric = "cox-nloglik",
                               eta = 0.05), # larger eta leads to algorithm not converging, resulting in NaN predictions
                 data = x_train[[x]], label = x_label[[x]],
                 watchlist = list(val2 = x_val[[x]]),
                 nrounds = 1000, early_stopping_rounds = 30)
})


