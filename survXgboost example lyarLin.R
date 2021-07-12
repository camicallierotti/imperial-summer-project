### example 1

pacman::p_load_gh("IyarLin/survXgboost")
pacman::p_load("survival")
pacman::p_load("xgboost")
data("lung")
lung <- lung[complete.cases(lung), ] # doesn't handle missing values at the moment
lung$status <- lung$status - 1 # format status variable correctly such that 1 is event/death and 0 is censored/alive
label <- ifelse(lung$status == 1, lung$time, -lung$time)

val_ind <- sample.int(nrow(lung), 0.1 * nrow(lung))
x_train <- as.matrix(lung[-val_ind, !names(lung) %in% c("time", "status")])
x_label <- label[-val_ind]
x_val <- xgb.DMatrix(as.matrix(lung[val_ind, !names(lung) %in% c("time", "status")]),
                     label = label[val_ind])

# train surv_xgboost
surv_xgboost_model <- xgb.train.surv(
  params = list(
    objective = "survival:cox",
    eval_metric = "cox-nloglik",
    eta = 0.05 # larger eta leads to algorithm not converging, resulting in NaN predictions
  ), data = x_train, label = x_label,
  watchlist = list(val2 = x_val),
  nrounds = 1000, early_stopping_rounds = 30
)

# predict survival curves
times <- seq(10, 1000, 50)
survival_curves <- predict(object = surv_xgboost_model, newdata = x_train, type = "surv", times = times)
matplot(times, t(survival_curves[1:5, ]), type = "l")

# predict risk score
risk_scores <- predict(object = surv_xgboost_model, newdata = x_train, type = "risk")
hist(risk_scores)

# source: https://github.com/IyarLin/survXgboost

### example 2

data(agaricus.train, package='xgboost')
train <- agaricus.train
dtrain <- xgb.DMatrix(train$data, label=train$label)
xgb.DMatrix.save(dtrain, 'xgb.DMatrix.data')
dtrain <- xgb.DMatrix('xgb.DMatrix.data')
if (file.exists('xgb.DMatrix.data')) file.remove('xgb.DMatrix.data')

# source: https://www.rdocumentation.org/packages/xgboost/versions/1.4.1.1/topics/xgb.DMatrix
