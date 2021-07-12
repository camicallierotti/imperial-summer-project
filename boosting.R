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

### xgboost

# create survival label (y)
y <- ifelse(pbc$status == 1, pbc$days, -pbc$days) # equivalent to surv object

# create predictors (X)
X <- pbc %>% select(-days,-status)

# set parameters
params <- list(objective = "survival:cox", # what type of dependent variable we have
               eval_metric = "cox-nloglik" # evaluation metric for survival problems
)

# run xgboost
model <- xgboost(data = as.matrix(X), # xgboost library requires data as matrix
                 label = y,
                 params = params,
                 nrounds = 20, # number of times we want xgboost to run
                 verbose = 1
)

# shap values (characteristics that correlate the most)
xgb.plot.shap(data = as.matrix(X),
              model = model,
              top_n = 5)
