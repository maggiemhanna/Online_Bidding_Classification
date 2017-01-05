## Classification

library(caret)

## Parallel Processing

library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

## Cross Validation Options

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7

## Test Metric

metric <- "Accuracy"

## Data Preprocessing

preProcess=c("center", "scale")

## Training 

# Logistic Regression
set.seed(seed)
fit.glm <- train(outcome~., data=training, method="glm", metric=metric, trControl=control, na.action=na.exclude)

# SVM Radial
set.seed(seed)
fit.svmRadial <- train(outcome~., data=training, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE,na.action=na.exclude)

# Random Forest
set.seed(seed)
fit.rf <- train(outcome~., data=training, method="rf", metric=metric, trControl=control,na.action=na.exclude)

# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(outcome~., data=training, method="gbm", metric=metric, trControl=control, verbose=FALSE,na.action=na.exclude)

# Model Selection

results <- resamples(list(logistic=fit.glm, svm=fit.svmRadial,rf=fit.rf, gbm=fit.gbm))

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

# Table comparison
summary(results)

# Prediction

outcome_probs <- predict(fit.glm, newdata = testing, type = "prob")

outcome_class <- predict(fit.glm, newdata = testing, type = "raw")

outcome_class <- as.data.frame(outcome_class)

Submission <- cbind(bidder_id_t, outcome = factor(outcome_class, label=c("Human","Robot")))


stopCluster(cl)

