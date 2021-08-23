### three individuals survival curves

## pbc data

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

## xgboost plot data

xgb_plt_dat <- readRDS("~/OneDrive - Imperial College London/AstraZeneca/imperial-summer-project/xgb_survpred_plotdata.RDS")

## cox ph plot data

cox.fit = coxph(Surv(days, status) ~ ., data = pbc_train, x = TRUE, y = TRUE)
cox.pred <- predictSurvProb(cox.fit, newdata = pbc_test[c(20,46,43),], times = 365.25*seq(0,10,by=0.5), type='surv') 
cox.pred <- as.data.frame(t(cox.pred))
cox.pred <- cbind(xgb_plt_dat$times, cox.pred)
colnames(cox.pred)[1] <-"times"

## rsf plot data

rsf.fit <- rfsrc(Surv(days, status) ~ ., pbc_train)
rsf.pred <- predictSurvProb(rsf.fit, newdata = pbc_test[c(20,46,43),], times = 365.25*seq(0,10,by=0.5), type='surv') 
rsf.pred <- as.data.frame(t(rsf.pred))
rsf.pred <- cbind(xgb_plt_dat$times, rsf.pred)
colnames(rsf.pred)[1] <-"times"

## plots

plot1 <- ggplot(NULL, aes(times, V1)) + 
  geom_step(data = cox.pred, color='red') +
  geom_step(data = rsf.pred, color='blue') +
  geom_step(data = xgb_plt_dat, color='green') +
  labs(x="Time (years)", y="Survival probability", title="Probability of survival of minimum age") +
  theme_bw()

plot2 <- ggplot(NULL, aes(times, V2)) + 
  geom_step(data = cox.pred, color='red') +
  geom_step(data = rsf.pred, color='blue') +
  geom_step(data = xgb_plt_dat, color='green') +
  labs(x="Time (years)", y="Survival probability", title="Probability of survival of median age") +
  theme_bw()

plot3 <- ggplot(NULL, aes(times, V3)) + 
  geom_step(data = cox.pred, color='red') +
  geom_step(data = rsf.pred, color='blue') +
  geom_step(data = xgb_plt_dat, color='green') +
  labs(x="Time (years)", y="Survival probability", title="Probability of survival of maximum age") +
  theme_bw()

require(gridExtra)
grid.arrange(plot1, plot2, plot3, ncol=3)


