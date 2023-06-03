if(!require(pROC)) install.packages('pROC') # for ROC
if(!require(MLmetrics)) install.packages('MLmetrics') # for ROC

source('read_data.R')
data = get_aggregated_features(up_to_year=1) %>% 
  select(dropout)

# test set is entire data because we need no data
predictions.baseline <- rep(mean(data$dropout), nrow(data))

# ROC and AUC
roc.baseline <- roc(data$dropout, predictions.baseline)
auc.baseline = auc(roc.baseline)
ggroc(roc.baseline)

# F2 score
F2_Score = function(y_true, y_pred, positive, beta = 2) {
  Precision <- Precision(y_true, y_pred, positive)
  Recall <- Recall(y_true, y_pred, positive)
  Fbeta_Score <- (1 + beta^2) * (Precision * Recall)/
    (beta^2 * Precision + Recall)
  return(Fbeta_Score)
}
  
  
F2score_thres = function(threshold) {
  return (F2_Score(y_pred = predictions.baseline > threshold,
                       y_true = data$dropout,
                       positive = TRUE))
}

sapply(seq(0,1,0.1), F2score_thres)

# if there are no positive examples in the prediction, we cannot calculate precision