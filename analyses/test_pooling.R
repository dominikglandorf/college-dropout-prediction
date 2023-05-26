get_rf_auc = function(dat) {
  # Split randomly into training and test data
  sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.7,0.3))
  train <- dat[sample, ]
  test <- dat[!sample, ]
  
  # Fit Random Forest
  rf <- randomForest(dropout ~ ., train)
  predictions.rf <- predict(rf, newdata=test, type="prob")[,2]
  roc.rf <- roc(test$dropout, predictions.rf)
  auc.rf = auc(roc.rf)
  return(auc.rf)
}

datasets = complete(tempData, "all")
fit <- lapply(datasets, get_rf_auc)
est1 <- pool(fit)

