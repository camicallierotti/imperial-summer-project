### packages

library(dplyr)
library(survXgboost)
library(xgboost)

### data

data(pbc, package = "randomForestSRC")
pbc <- pbc[complete.cases(pbc),]

# split 

set.seed(1998)
index <- sample(1:276, size = trunc(.8 * 276)) # 80:20 train-test split
pbc_train <- pbc %>%
  filter(row_number() %in% index)
pbc_test <- pbc %>%
  filter(!(row_number() %in% index))

# create survival label (y) for training
y_train <- ifelse(pbc_train$status == 1, pbc_train$days, -pbc_train$days) # equivalent to surv object

# create predictors (X) for training
X_train <- pbc_train %>% select(-days,-status)
X_train <- as.matrix(X_train)

# create survival label (y) for testing
y_test <- ifelse(pbc_test$status == 1, pbc_test$days, -pbc_test$days) # equivalent to surv object

# create predictors (X) for testing
X_test <- pbc_test %>% select(-days,-status)
X_test <- as.matrix(X_test)

### train surv_xgboost

surv_xgboost_model <- xgb.train.surv(
  params = list(
    objective = "survival:cox",
    eval_metric = "cox-nloglik",
    eta = 0.05 # larger eta leads to algorithm not converging, resulting in NaN predictions
  ), data = X_train, label = y_train,
  nrounds = 1000
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

xgb.pred <- predict(surv_xgboost_model, as.matrix(x_train)) # perform the prediction of risk scores <- RESULTS>1?
print(length(xgb.pred)) # size of the prediction vector (confirm 56 same as test set)
print(head(xgb.pred)) # display first predictions: probabilities that a datum will be classified as 1 (event happening)
xgb.binary.pred <- as.numeric(xgb.pred > 0.5) # convert to probability of dead or alive

# c-index

y_for_cindex <- ifelse(label < 0, 0, 1) # convert censored to 0 and death to 1
cindex <- survConcordance(y_for_cindex ~ xgb.binary.pred) # 0.17???
