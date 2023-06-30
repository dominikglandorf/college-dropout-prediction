
if(!require(randomForest)) install.packages('randomForest')

# tune hyperparameters
# logic: all students split into 70% training and 30% test set
# Cross Validation: 3 fold test on 5 imputations: 15 trainings per parameter set
# Parameter set with best average validation score is used for final training and evaluation on test set
# which metric to use? Area under PR Curve or F2 Score?

# Parameters to tune
# Logistic Regression: regularization?


# Random Forest: 



setwd("~/EWS")
source("read_data.R")
source('models/evaluation.R')

NUM_FOLDS = 3
NUM_INNER_FOLDS =3 

datasets = get_imputed_features(1)

# to speed up training, a subsample could be taken here
#datasets = lapply(datasets, function(dat) dat %>% sample_n(1000))

# drop dependent features
datasets = lapply(datasets, function(dat) dat %>%
                    mutate_if(is.character, as.factor) %>%
                    select(-major_name_1,
                           -uc_total_score,
                           -first_credits,
                           -first_credits_major,
                           -term_span))

# general tuning and evaluation routine
tune = function(dat, train_predict, param_combos) {
  # inner loop: do another CV for hyperparameter tuning on each train set
  inner_split = sample(1:NUM_INNER_FOLDS, nrow(dat), replace=TRUE)
  
  # train with each parameter set and evaluate on all val sets and mean score
  score = sapply(1:nrow(param_combos), function(i) {
    print(param_combos[i,])
    return(mean(sapply(1:NUM_FOLDS, function(k) {
      cv_train = dat[inner_split != k,]
      cv_val = dat[inner_split == k,]
      
      my_predict = train_predict(cv_train, param_combos[i,])
      return(get_auprc(my_predict(cv_val), cv_val$dropout))
    })))
  })
  
  # choose best hyperparameters
  best_combo = param_combos[which.max(score),]
  print("Best parameters found")
  print(best_combo)
  return(best_combo)
}

tune_and_test = function(train, test, train_predict, param_combos) {
  best_combo = tune(train, train_predict, param_combos)
  # return test score for model trained on whole test with best params
  my_predict = train_predict(train, best_combo)
  return(get_all_metrics(my_predict(test), as.logical(test$dropout)))
}


tune_and_pfi = function(dat, train_predict, param_combos) {
  best_combo = tune(dat, train_predict, param_combos)
  #print(best_combo)
  
  # inner loop: do another CV for feature importance on each test set
  inner_split = sample(1:NUM_INNER_FOLDS, nrow(dat), replace=TRUE)
  pfi_cvs = (sapply(1:NUM_FOLDS, function(k) {
    cv_train = dat[inner_split != k,]
    cv_val = dat[inner_split == k,]
    
    my_predict = train_predict(cv_train, best_combo)
    pfi = feature_importance(my_predict, cv_val)
    return(pfi)
  }))
  avg_pfi = data.frame(predictor=pfi_cvs[,1]$predictor,
                       avg_score=rowMeans(sapply(1:NUM_FOLDS, function(i) pfi_cvs[,i]$score)))
  return(avg_pfi[order(avg_pfi$avg_score),])
}

# iterate through imputations
outer_split = sample(1:NUM_FOLDS, nrow(datasets[[1]]), replace=TRUE)


imputations_loop = function(train_predict, param_combos, num_imps=length(datasets)) {
  # Performance Estimation
  metrics = sapply(1:num_imps, function(j) {
    print(j)
    dat = datasets[[j]]
    
    # outer loop: do CV for train / test, to estimate performance
    return(sapply(1:NUM_FOLDS, function(i) {
      print(paste("Outer fold", i))
      train = dat[outer_split != i,]
      test = dat[outer_split == i,]
      metrics = tune_and_test(train, test, train_predict, param_combos)
      return(metrics)
    }))
  })
  results=rowMeans(sapply(1:num_imps, function(i) rowMeans(matrix(as.numeric(metrics[,i]), ncol=NUM_FOLDS))))
  names(results) = c("F2 Score", "AUPRC", "Accuracy", "AUROC")
  print(results)
  
  # Feature Importance
  pfi_imps = sapply(1:num_imps, function(k) {
    print(k)
    # inner loop on whole dataset to estimate feature importance
    return(tune_and_pfi(datasets[[k]], train_predict, param_combos))
  })
  avg_pfi = data.frame(predictor=pfi_imps[,1]$predictor,
                       avg_score=rowMeans(sapply(1:num_imps, function(i) pfi_imps[,i]$avg_score)))
  print(avg_pfi[order(avg_pfi$avg_score),])
  
  return(list(metrics=results,
              pfi=avg_pfi[order(avg_pfi$avg_score),]))
}

# logistic regression
train_predict_lr = function(train_data, param_combo) {
  lr = glm(dropout ~ ., train_data, family = 'binomial')
  function(test) predict(lr, test, type = "response")
}
param_combos_lr = expand.grid(para1 = list(1))

lr_results = imputations_loop(train_predict_lr, param_combos_lr)

# random forest
train_predict_rf = function(train_data, param_combo) {
  rf <- randomForest(as.factor(dropout) ~ ., train_data,
                     ntree=as.numeric(param_combo$ntree),
                     mtry=as.numeric(param_combo$mtry))
  function(test) predict(rf, newdata=test, type="prob")[,"TRUE"]
}

ntrees = c(50)#, 100)
mtrys = c(2)#, 3)
param_combos_rf = expand.grid(ntree = ntrees, mtry = mtrys)

imputations_loop(train_predict_rf, param_combos_rf, num_imps=1)

# runtime
#5 * NUM_FOLDS * NUM_PARAM_COMBOS * NUM_INNER_FOLDS
# 5 * 3 * 10 * 3
