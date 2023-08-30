setwd("~/EWS")
source('models/evaluation.R')

NUM_FOLDS = 3
NUM_INNER_FOLDS =3 

# general tuning and evaluation routine
tune = function(dat, train_predict, param_combos, sample_size=F) {
  if (nrow(param_combos) == 1) {
    print("Only one hyperparameter combination - skip tuning")
    return(param_combos[1,])
  }
  
  # inner loop: do another CV for hyperparameter tuning on each train set
  inner_split = sample(1:NUM_INNER_FOLDS, nrow(dat), replace=TRUE)
  if (sample_size!=F) subindices = sapply(1:NUM_INNER_FOLDS,
                                          function(i) sample(1:sum(inner_split!=i),
                                                             size = sample_size,
                                                             replace = FALSE))
  
  # train with each parameter set and evaluate on all val sets and mean score
  score = sapply(1:nrow(param_combos), function(i) {
    print(param_combos[i,])
    return(mean(sapply(1:NUM_INNER_FOLDS, function(k) {
      cv_train = dat[inner_split != k,]
      if (sample_size!=F) cv_train = cv_train[subindices[,k],]
      cv_val = dat[inner_split == k,]
      
      my_predict = train_predict(cv_train, param_combos[i,])
      return(get_auprc(my_predict(cv_val), cv_val$dropout))
    })))
  })
  for (param in names(param_combos)) {
    plot(param_combos[[param]], score, ylab="AUPRC", xlab=param)
  }

  # choose best hyperparameters
  best_combo = param_combos[which.max(score),]
  print("Best parameters found")
  print(best_combo)
  return(best_combo)
}

get_outer_split = function(dat) sample(1:NUM_FOLDS, nrow(dat), replace=TRUE)

extract_metrics = function(results, FUN=mean) {
  extract_statistic = function(metrics, name) FUN(unlist(lapply(metrics, '[[', name)))
  metrics = lapply(results, '[[', "metrics")
  list(F2score=extract_statistic(metrics, "F2score"),
       AUPRC=extract_statistic(metrics, "AUPRC"),
       accuracy=extract_statistic(metrics, "accuracy"),
       AUROC=extract_statistic(metrics, "AUROC"))
}
extract_pfi = function(results, sd=F) {
  FUN = ifelse(sd, function(x) c(mean = mean(x), sd = sd(x)), mean)
  aggregate(score ~ predictor,
            do.call(rbind, lapply(results, '[[', "pfi")),
            FUN)
}

imputations_loop = function(datasets, outer_split, train_predict, param_combos, num_imps=length(datasets), sample_size=F, transform_dat=F, no_pfi=F) {
  imp_results = lapply(1:num_imps, function(j) {
    print(j)
    dat = datasets[[j]]
    if (is.function(transform_dat)) dat = transform_dat(dat)
    
    # outer loop: do CV on train / use test to estimate performance and feature importance
    outer_results = lapply(1:NUM_FOLDS, function(i) {
      print(paste("Outer fold", i))
      train = dat[outer_split != i,]
      test = dat[outer_split == i,]
      # Hyperparameter Tuning on train set for first imputation
      #if (j == 1)
      #best_combo = param_combos[1,]
      best_combo = tune(train, train_predict, param_combos, sample_size)
      
      # Train model with best hyperparameters on train set
      my_predict = train_predict(train, best_combo)
      
      # Performance Estimation on test set
      metrics = get_all_metrics(my_predict(test), as.logical(test$dropout))
      if (no_pfi) return(list(metrics=metrics, hyperparameters = best_combo))
      
      # Feature Importance on test set
      pfi = feature_importance(my_predict, test)
      
      list(metrics = metrics, pfi = pfi, hyperparameters = best_combo)
    })
    if(no_pfi) return(list(metrics=extract_metrics(outer_results),
                      hyperparameters = lapply(outer_results, '[[', "hyperparameters")))
    list(metrics = extract_metrics(outer_results),
         pfi = extract_pfi(outer_results),
         hyperparameters = lapply(outer_results, '[[', "hyperparameters"))
  })
  metrics = cbind(mean=extract_metrics(imp_results),
                  sd=extract_metrics(imp_results, FUN=sd))
  if (no_pfi) return(list(metrics=metrics,
                     hyperparameters = lapply(imp_results, '[[', "hyperparameters")))
  
  avg_pfi = extract_pfi(imp_results, sd=T)
  list(metrics=metrics,
       pfi=avg_pfi[order(avg_pfi$score[,"mean"]),],
       hyperparameters=lapply(imp_results, '[[', "hyperparameters"))
}


