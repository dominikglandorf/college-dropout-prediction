for (i in 1:9) {
  print(i)
  UP_TO_SPAN = i
  datasets = get_imputed_features_by_span(UP_TO_SPAN)
  
  # to speed up training, a subsample could be taken here
  #datasets = lapply(datasets, function(dat) dat %>% sample_n(10000))
  
  # drop dependent features
  datasets = lapply(datasets, function(dat) dat %>%
                      mutate_if(is.character, as.factor) %>%
                      select(-uc_total_score,
                             -major_name_1,
                             -first_credits,
                             -first_credits_major,
                             -term_span))
  data_dim = ncol(datasets[[1]])
  outer_split = get_outer_split(datasets[[1]])
  
  i=1
  train_predict_lr = function(train_data, param_combo) {
    lr = glm(dropout ~ ., train_data, family = 'binomial')
    function(test) predict(lr, test, type = "response")
  }
  train_predict = train_predict_lr
  dat=datasets[[1]]
  
  print(paste("Outer fold", i))
  train = dat[outer_split != i,]
  test = dat[outer_split == i,]
  
  # Train model with best hyperparameters on train set
  my_predict = train_predict(train, best_combo)
  
  predicted_scores = my_predict(test)
  hist(predicted_scores)
  true_labels = as.logical(test$dropout)
  
  # Performance Estimation on test set
  accuracies = sapply(thresholds, function(t) Accuracy(predicted_scores>t, true_labels))
  acc_data = data.frame(threshold=thresholds, metric=accuracies)
  best_thres = thresholds[which.max(accuracies)]
  return(list(accuracies = acc_data,
              best_threshold = best_thres,
              best_metric = max(accuracies)))
  plot_PR(predicted_scores, true_labels)
  print(get_all_metrics(predicted_scores, true_labels))
}
