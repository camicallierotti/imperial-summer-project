### packages

library(dplyr)
library(xgboost)
library(survXgboost)
### data

data(pbc, package = "randomForestSRC")
pbc <- pbc[complete.cases(pbc),]
set.seed(1998)
index <- sample(1:276, size = trunc(.8 * 276)) # 80:20 train-test split
pbc_train <- pbc %>%
  filter(row_number() %in% index)
pbc_test <- pbc %>%
  filter(!(row_number() %in% index))

### datasets

# create survival label (y) for training
y_train <- ifelse(pbc_train$status == 1, pbc_train$days, -pbc_train$days) # equivalent to surv object

# create predictors (X) for training
X_train <- pbc_train %>% select(-days,-status)

# create survival label (y) for testing
y_test <- ifelse(pbc_test$status == 1, pbc_test$days, -pbc_test$days) # equivalent to surv object

# create predictors (X) for testing
X_test <- pbc_test %>% select(-days,-status)

### train xgboost

# set parameters
params <- list(objective = "survival:cox", # what type of dependent variable we have
               eval_metric = "cox-nloglik" # evaluation metric for survival problems
)

# run xgboost
xgboost_model <- xgboost(data = as.matrix(X_train), # xgboost library requires data as matrix
                 label = y_train,
                 params = params,
                 max_depth = 2, # don't need deep trees bc small dataset
                 nrounds = 348, # number of times we want xgboost to run
                 eta = 1,
                 verbose = 1
                 )

### make predictions

xgb.pred <- predict(xgboost_model, as.matrix(X_test)) # perform the prediction of risk scores <- RESULTS>1?
print(length(xgb.pred)) # size of the prediction vector (confirm 56 same as test set)
print(head(xgb.pred)) # display first predictions: probabilities that a datum will be classified as 1 (event happening)
xgb.binary.pred <- as.numeric(xgb.pred > 0.5) # convert to probability of dead or alive

### c-index 

y_for_cindex <- ifelse(y_test < 0, 0, 1) # convert censored to 0 and death to 1
cindex <- survConcordance(y_for_cindex ~ xgb.binary.pred) # 0.17???

### variable importance

# shap values
xgb.plot.shap(data = as.matrix(X_train),
              model = model,
              top_n = 4,
              cex.lab=1.5,
              cex.axis=1.5)

