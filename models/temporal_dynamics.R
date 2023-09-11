if(!require(randomForest)) install.packages('randomForest')

setwd("~/EWS")
source("read_data.R")
source('models/hyperparameter_tuning.R')

print_to_file = function(path, content) {
  file_conn <- file(path, "w")
  sink(file_conn) # Redirect the output to the file
  print(content)  # Print to the console
  sink() # Stop redirecting the output and close the file connection
  close(file_conn)
}

# logistic regression
run_lr = function(datasets, outer_split, filename) {
  train_predict_lr = function(train_data, param_combo) {
    lr = glm(dropout ~ ., train_data, family = 'binomial')
    function(test) predict(lr, test, type = "response")
  }
  param_combos_lr = expand.grid(para1 = list(1)) # this is just for generality of the functions
  lr_results = imputations_loop(datasets, outer_split, train_predict_lr, param_combos_lr,
                                num_imps=10, no_pfi=F)
  print_to_file(paste0("models/results/", filename), lr_results)
  lr_results
}

# random forest
run_rf = function(datasets, outer_split, filename) {
  train_predict_rf = function(train_data, param_combo) {
    rf <- randomForest(as.factor(dropout) ~ ., train_data,
                       ntree=param_combo$ntree,
                       mtry=param_combo$mtry)
    function(test) predict(rf, newdata=test, type="prob")[,"TRUE"]
  }
  param_combos_rf = expand.grid(ntree = c(500, 1000, 1500),
                                mtry =  c(3, 5, 6, 7) )
  rf_results = imputations_loop(datasets, outer_split, train_predict_rf, param_combos_rf,
                                num_imps=10, no_pfi=F)
  print_to_file(paste0("models/results/", filename), rf_results)
  rf_results
}

run_for_span = function(span) {
  print(span)
  datasets = get_imputed_features_by_span(span)
  
  # drop dependent features
  datasets = lapply(datasets, function(dat) dat %>%
                      mutate_if(is.character, as.factor) %>%
                      select(-major_name_1,
                             -uc_total_score,
                             -first_credits,
                             -first_credits_major,
                             -term_span
                      ))
  # drop NA features
  datasets = lapply(datasets, function(dat) {
    na_cols <- colnames(dat)[apply(dat, 2, function(x) any(is.na(x)))]
    if (length(na_cols)>0) print(paste("Dropped", na_cols, "because of NAs"))
    return(dat[, !(names(dat) %in% na_cols)])
  })
 
  outer_split = get_outer_split(datasets[[1]])
  #lr_results = run_lr(datasets, outer_split, paste0("max_", span, "_lr.txt"))
  rf_results = run_rf(datasets, outer_split, paste0("max_", span, "_rf.txt"))
  
  save(outer_split, rf_results, file=paste0("models/results/max_", span, "_results.Rdata"))
}

# temporarily excluding 1 to 7
sapply(0:9, run_for_span)
