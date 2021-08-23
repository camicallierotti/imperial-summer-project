library(pacman)
p_load(
  tidyverse,
  survival,
  rms,
  Hmisc, # for rcorr.cens, which computes Harrell's c-statistic
  glmnet,
  randomForestSRC,
  coxed,
  pec,
  xgboost,
  survXgboost
)

data('pbc', package='randomForestSRC')
pbc <- pbc %>% filter(complete.cases(.))
pbc <- pbc %>% 
  mutate(stage = as.factor(stage),
         edema = as.factor(edema),
         age = age/365.25,
         treatment = as.factor(treatment)) 
set.seed(1998)
index <- sample(1:276, size = trunc(.8 * 276)) # 80:20 train-test split
pbc_train <- pbc %>%
  filter(row_number() %in% index)
pbc_test <- pbc %>%
  filter(!(row_number() %in% index))
X_train <- model.matrix(Surv(days, status)~., data=pbc_train)
y_train <- Surv(pbc_train$days, pbc_train$status)
X_test <- model.matrix(Surv(days, status)~., data=pbc_test)
y_test <- Surv(pbc_test$days, pbc_test$status)

m1 <- coxph(Surv(days, status)~., data=pbc_train)
m2 <- randomForestSRC::rfsrc(Surv(days, status)~., data=pbc_train)

## LASSO

lasso_fit <- glmnet(X_train, y_train, family='cox')
m3 <- cv.glmnet(X_train, y_train, family="cox", alpha=1) # optimized via CV

## XGBoost

label_train <-  ifelse(y_train[,2]==1, y_train[,1], -y_train[,1])
x_val <- xgb.DMatrix(X_train, label=label_train)
m4 <- xgb.train.surv(
  params = list(
    objective = "survival:cox",
    eval_metric = "cox-nloglik",
    eta = 0.05 # larger eta leads to algorithm not converging, resulting in NaN predictions
  ),
  data = X_train,
  label = label_train,
  watchlist=list(val2=x_val),
  nrounds=1000,
  early_stopping_rounds=30
)

# dtrain = xgb.DMatrix(X_train)
# y_lower = y_train[,1]
# y_upper = ifelse(y_train[,2]==0, +Inf, y_train[,1])
# dtrain = xgb.DMatrix(X_train,
#                      info = list('labels_lower_bound'=y_lower,
#                                   'labels_upper_bound'=y_upper))
# setinfo(dtrain, 'label_lower_bound', y_lower)
# setinfo(dtrain, 'label_upper_bound', y_upper)
# m5 <- xgb.train(
#   params = list(
#     objective = 'survival:aft',
#     aft_loss_distribution = 'normal',
#     eval_metric = "aft-nloglik",
#     tree_method='hist',
#     learning_rate=0.1,
#     max_depth=2
#   ),
#   dtrain,
#   nrounds = 50000,
#   early_stopping_rounds=10,
#   watchlist=list(val2=dtrain)
# )

mods <- list(m1,m2,m3,m4)

c_indices <- c(
  'coxph'=1-rcorr.cens(predict(m1, pbc_test), y_test)[1],
  'rsf' = 1-rcorr.cens(predict(m2, pbc_test)$predicted, y_test)[1],
  'coxph_lasso' = 1 - rcorr.cens(predict(m3, X_test), y_test)[1],
  'xgboost' = 1 - rcorr.cens(predict(m4, X_test), y_test)[1]
)

survcurves = predict(m4, newdata=X_test, times=365.25*c(9,10), type='surv')
survcurves[c(20,46,43),2]
