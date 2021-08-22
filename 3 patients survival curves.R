### three individuals survival curves

# make df with 3 individuals

data(pbc, package = "randomForestSRC")
pbc <- pbc[complete.cases(pbc),]
patient1 <- pbc[270,]
patient2 <- pbc[104,]
patient3 <- pbc[253,]

#quantile(pbc$age, .5, type = 1) 
#which(pbc$age == 18137) # row 104

pbc_patients <- pbc[c(270,104,253),]

# cox

cox.fit = coxph(Surv(days, status) ~ ., data = pbc, x = TRUE, y = TRUE)

# rsf

rsf.fit <- rfsrc(Surv(days, status) ~ ., data = pbc)

# xgb



# plot for patient 1

plotPredictSurvProb(cox.fit, newdata = patient1, lty = 1, legend = TRUE, percent = TRUE) 
plotPredictSurvProb(rsf.fit, newdata = patient1, lty = 1, legend = TRUE, percent = TRUE) 

predict(object = surv_xgboost_model, newdata = patient1, type = "surv", times = times)
