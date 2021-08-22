### packages

library(dplyr)
library(survXgboost)
library(xgboost)

### data

data(pbc, package = "randomForestSRC")
pbc <- pbc[complete.cases(pbc), ] # doesn't handle missing values at the moment
label <- ifelse(pbc$status == 0, -pbc$days, pbc$days)

val_ind <- sample.int(nrow(pbc), 0.1 * nrow(pbc))
x_train <- as.matrix(pbc[-val_ind, !names(pbc) %in% c("days", "status")])
x_label <- label[-val_ind]
x_val <- xgb.DMatrix(as.matrix(pbc[val_ind, !names(pbc) %in% c("days", "status")]),
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

### predict 

# find individuals to predict for
x_train_df <- as.data.frame(x_train)
which(x_train_df$age == 10550) # row 76
which(x_train_df$age == 16941) # row 206
which(x_train_df$age == 25006) # row 195

# predict full survival curves
times <- seq(10, 4000, 50)
survival_curves <- predict(object = surv_xgboost_model, newdata = x_train, type = "surv", times = times)
survival_curves_selected <- survival_curves[c(76, 206, 195),] # select curves for 3 individuals
matplot(times, t(survival_curves_selected),
        type = "l", lty = "solid", lwd = 2, cex = 2.5,
        col = c("orange", "blue", "red"),
        xlab = "Time", ylab = "Survival probability")
legend(x = "bottomleft",inset = 0,
       legend = c("Patient 1", "Patient 2", "Patient 3"),
       col = c("orange", "blue", "red"),
       lty = "solid", lwd = 2, cex = 1)

# predict risk score
risk_scores <- predict(object = surv_xgboost_model, newdata = x_train[c(76, 206, 195),], type = "risk")
hist(risk_scores)

# predict 10-year survival prob

