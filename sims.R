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

set.seed(1998)

cens <- lapply(c(.01,.25,.50,.75,.99), function(x) {
  d = sim.survdata(N=1000, T=200, xvars=3, censor=x, num.data.frames = 1, beta = c(0.2, -0.2,0)) 
  as.data.frame(d$data) %>% mutate(indx = 1:n())
}) 

cens_train <- lapply(1:5, function(x){
  cens[[x]] %>% group_by(failed) %>% slice_sample(prop=0.8) %>% ungroup()
})

cens_test <- map2(cens, cens_train, function(x,y){
  indx = setdiff(x$indx, y$indx)
  x[indx,]
})

cph.fit = map(cens_train, ~coxph(Surv(y,failed)~., data=.))
cph.pred = map2(cph.fit, cens_test, ~predict(.x, newdata=.y))
cph.c = map2_dbl(cph.pred, cens_test, ~1 - rcorr.cens(.x, Surv(.y$y, .y$failed))[1])

rsf.fit = map(cens_train, ~rfsrc(Surv(y, failed)~., data=.))
rsf.pred = map2(rsf.fit, cens_test, ~predict(.x, newdata =.y)$predicted)
rsf.c = map2_dbl(rsf.pred, cens_test, ~1 - rcorr.cens(.x, Surv(.y$y, .y$failed))[1])

prep_xgboost <- function(dat){
  label_y = ifelse(dat$failed, dat$y, -dat$y)
  x_val = xgb.DMatrix(as.matrix(dat[,1:3]), label=label_y)
  xgb.train.surv(params = list(objective = 'survival:cox', eval_metric='cox-nloglik', eta = 0.05),
                 data = as.matrix(dat[,1:3]),
                 label = label_y,
                 nrounds = 10000
  )
}

xgb.fit = map(cens_train, prep_xgboost)
