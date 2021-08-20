### packages

library(dplyr)
library(xgboost)

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

### xgboost

# set parameters
params <- list(objective = "survival:cox", # what type of dependent variable we have
               eval_metric = "cox-nloglik" # evaluation metric for survival problems
)

# run xgboost
model <- xgboost(data = as.matrix(X_train), # xgboost library requires data as matrix
                 label = y_train,
                 params = params,
                 nrounds = 20, # number of times we want xgboost to run
                 verbose = 1
)

# shap values (characteristics that correlate the most)
xgb.plot.shap(data = as.matrix(X),
              model = model,
              top_n = 4,
              cex.lab=1.5,
              cex.axis=1.5)

# make predictions
pred <- predict(model, as.matrix(X_test))

# c-index of prediction
cindex <- survConcordance(y_test ~ pred) # 0.33 bc pred is surv probabilities but y_test is time+status

### 10-year survival probabilities

# find row numbers with the 3 individuals to make predictions for
which.min(X_test$age) # row 20 has individual with min age
which.max(X_test$age) # row 43 has individual with max age
quantile(X_test$age, .5, type = 1) # 16941 days is median age
which(X_test$age == 16941) # row 46 has individual with median age

pred[20] # min
pred[43] # max
pred[46] # median
