library(pacman)
p_load(
  tidyverse,
  mlr3,
  
  mlr3proba,
  mlr3tuning,
  mlr3pipelines,
  survivalmodels
)
p_load_gh("mlr-org/mlr3extralearners")

data("pbc", package = "randomForestSRC")
pbc <- pbc %>% filter(complete.cases(.))
pbc <- pbc %>%
  mutate(
    stage = as.factor(stage),
    edema = as.factor(edema),
    age = age / 365.25,
    treatment = as.factor(treatment)
  )
set.seed(1998)
index <- sample(1:276, size = trunc(.8 * 276)) # 80:20 train-test split
pbc_train <- pbc %>%
  filter(row_number() %in% index)
pbc_test <- pbc %>%
  filter(!(row_number() %in% index))
X_train <- model.matrix(Surv(days, status) ~ ., data = pbc_train)
y_train <- Surv(pbc_train$days, pbc_train$status)
X_test <- model.matrix(Surv(days, status) ~ ., data = pbc_test)
y_test <- Surv(pbc_test$days, pbc_test$status)


nafld <- nafld1 %>% 
  mutate(case = ifelse(id==case.id,1,0)) %>% 
  select(age, male, bmi, futime, status) %>% 
  filter(complete.cases(.))

# Following https://towardsdatascience.com/neural-networks-for-survival-analysis-in-r-1e0421584ab
reticulate::use_virtualenv("./.venv", required = T)
set_seed(1000)
pbc_task1 <- TaskSurv$new("pbc", pbc_train, time = "days", event = "status")
pbc_task2 <- TaskSurv$new('pbc_test', pbc_test, time='days', event='status')
nafld_task <- TaskSurv$new('nafld' , nafld, time='futime', event='status')
tasks <- list(pbc_task)

library(paradox)
search_space <- ps(
  ## p_dbl for numeric valued parameters
  dropout = p_dbl(lower = 0, upper = 1),
  weight_decay = p_dbl(lower = 0, upper = 0.5),
  learning_rate = p_dbl(lower = 0, upper = 1),
  ## p_int for integer valued parameters
  nodes = p_int(lower = 1, upper = 32),
  k = p_int(lower = 1, upper = 4)
)

search_space$trafo <- function(x, param_set) {
  x$num_nodes <- rep(x$nodes, x$k)
  x$nodes <- x$k <- NULL
  return(x)
}

library(mlr3tuning)
create_autotuner <- function(learner) {
  AutoTuner$new(
    learner = learner,
    search_space = search_space,
    resampling = rsmp("holdout"),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = 2),
    tuner = tnr("random_search")
  )
}
learners <- lrns(
  paste0("surv.", c("coxtime", "deephit", "deepsurv", "loghaz", "pchazard")),
  frac = 0.3, early_stopping = TRUE, epochs = 10, optimizer = "adam"
)
# apply our function
learners <- lapply(learners, create_autotuner)
create_pipeops <- function(learner) {
  po("encode") %>>% po("scale") %>>% po("learner", learner)
}
## apply our function
learners <- lapply(learners, create_pipeops)

# deepsurv
learners[[3]]$train(pbc_task1)
bl <- learners[[3]]$predict(pbc_task2)
1 - rcorr.cens(bl$surv.deepsurv.tuned.output$crank, y_test)[1] # 0.576

# deephit
learners[[2]]$train(pbc_task1)
bl <- learners[[2]]$predict(pbc_task2)
1 - rcorr.cens(bl$surv.deephit.tuned.output$crank, y_test)[1] # 0.635



# resampling <- rsmp("cv", folds = 5)
# learners <- c(learners, lrns(c("surv.coxph")))
# design <- benchmark_grid(tasks, learners, resampling)
# bm <- benchmark(design, store_models = TRUE)
# msrs <- msrs(c("surv.cindex"))
# bm$aggregate(msrs)[,c(3,4,7)]

mod <- deepsurv(Surv(days, status)~., data = pbc_train)
pred_probs=predict(mod, newdata = pbc_test, type='survival')
pred_probs = as.data.frame(t(pred_probs)) %>% rownames_to_column('times') %>% 
  mutate(times = as.numeric(times))
pred_probs[,c(1,20,46,43)]

approx(pred_probs$times, pred_probs[,21], 365.25*10)
approx(pred_probs$times, pred_probs[,47], 365.25*10)
approx(pred_probs$times, pred_probs[,44], 365.25*10)
