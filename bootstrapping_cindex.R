# packages

library(boot)
library(rms)
library(randomForestSRC)

# data

data(pbc, package = "randomForestSRC")
pbc <- pbc[complete.cases(pbc),]

# cox ph model

foo = function(data, indices){
  fit = cph(Surv(days, status) ~ treatment, data = pbc[indices,])
  DescTools::Cstat(fit)
}

set.seed(1998)
myBootstrap = boot(pbc,foo,R=50)
