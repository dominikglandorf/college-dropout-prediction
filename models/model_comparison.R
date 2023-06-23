if(!require(randomForest)) install.packages('randomForest')
if(!require(e1071)) install.packages('e1071') # for SVM

setwd("~/EWS")
source("read_data.R")
source('models/evaluation.R')


train_and_evaluate = function (dat) {
  print("Next dataset")
  dat = dat %>%
    mutate_if(is.character, as.factor)
  
  dat = dat %>% select(-major_name_1,
                       -uc_total_score,
                       -first_credits,
                       -first_credits_major,
                       -term_span)
  
  # to speed up training, a subsample could be taken here
  dat = dat %>% sample_n(5000)
  
  # Split randomly into training and test data
  sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.7,0.3))
  train <- dat[sample, ]
  test <- dat[!sample, ]
  
  true_labels = test$dropout
  
  # Fit Logistic Regression
  print("Logistic Regression")
  lr = glm(dropout ~ ., train, family = 'binomial')
  predictions.lr = predict(lr, test, type = "response")
  lr_pfi = feature_importance(function(x) predict(lr, x, type = "response"), test)
  lr_metrics = get_all_metrics(predictions.lr, true_labels)
  
  # Fit Random Forest
  print("Random Forest")
  rf <- randomForest(as.factor(dropout) ~ ., train)
  predictions.rf <- predict(rf, newdata=test, type="prob")[,2]
  rf_pfi = feature_importance(function(x) predict(rf, newdata=x, type = "prob")[,2], test)
  rf_metrics = get_all_metrics(predictions.rf, true_labels)
  
  # combine metrics
  return(list(
    scores=data.frame(rbind(lr_metrics, rf_metrics)),
    lr_pfi=lr_pfi,
    rf_pfi=rf_pfi))
}

datasets = get_imputed_features(1)
eval_results = lapply(datasets[1:2], train_and_evaluate)

# Fit Support Vector Machine 
# this does not really work so far
svm.fit = svm(dropout ~ ., train, type = 'C-classification', kernel = 'polynomial', probability = T)
predictions.svm = attr(predict(svm.fit, test, probability=T), 'probabilities')[,1]
svm_metrics = get_all_metrics(predictions.svm, true_labels)