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
                                num_imps=1, no_pfi=F)
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
  param_combos_rf = expand.grid(ntree = c(1000),
                                mtry =  c(6) )
  rf_results = imputations_loop(datasets, outer_split, train_predict_rf, param_combos_rf,
                                num_imps=3, no_pfi=F)
  print_to_file(paste0("models/results/", filename), rf_results)
  rf_results
}



UP_TO_SPAN = 4
datasets = get_imputed_features_by_span(UP_TO_SPAN)
data_to_impute = get_data_to_impute(UP_TO_SPAN)
# to speed up training, a subsample could be taken here
#datasets = lapply(datasets, function(dat) dat %>% sample_n(10000))

# drop dependent features
datasets = lapply(datasets, function(dat) dat %>%
                    mutate_if(is.character, as.factor) %>%
                    select(-major_name_1,
                           -uc_total_score,
                           -first_credits,
                           -first_credits_major,
                           -term_span))

# drop NA features
datasets = lapply(datasets, function(dat) {
  na_cols <- colnames(dat)[apply(dat, 2, function(x) any(is.na(x)))]
  if (length(na_cols)>0) print(paste("Dropped", na_cols, "because of NAs"))
  return(dat[, !(names(dat) %in% na_cols)])
})

# URM
urm_datasets = lapply(datasets, function(dat) dat %>%
                        filter(!is.na(data_to_impute$ethnicity_smpl)) %>%
                        filter(ethnicity_smpl == "Asian / Asian American" | ethnicity_smpl == "White non-Hispanic") %>%
                        select(-ethnicity_smpl))
urm_outer_split = get_outer_split(urm_datasets[[1]])
#urm_lr_results = run_lr(urm_datasets, urm_outer_split, "urm_lr.txt")
urm_rf_results = run_rf(urm_datasets, urm_outer_split, paste0("urm_rf_span_", UP_TO_SPAN, ".txt"))

non_urm_datasets = lapply(datasets, function(dat) dat %>%
                            filter(!is.na(data_to_impute$ethnicity_smpl)) %>%
                            filter(ethnicity_smpl != "Asian / Asian American" & ethnicity_smpl != "White non-Hispanic") %>%
                            select(-ethnicity_smpl))
non_urm_outer_split = get_outer_split(non_urm_datasets[[1]])
#non_urm_lr_results = run_lr(non_urm_datasets, non_urm_outer_split, "non_urm_lr.txt")
non_urm_rf_results = run_rf(non_urm_datasets, non_urm_outer_split, paste0("non_urm_rf_span_", UP_TO_SPAN, ".txt"))

save(urm_outer_split, non_urm_outer_split, urm_rf_results, non_urm_rf_results, file=paste0("models/results/urm_results_span_", UP_TO_SPAN, ".Rdata"))

# STEM major vs non STEM majors
stem_datasets = lapply(datasets, function(dat) dat %>%
                         filter(any_major_stem) %>%
                         select(-any_major_stem))
stem_outer_split = get_outer_split(stem_datasets[[1]])
#stem_lr_results = run_lr(stem_datasets, stem_outer_split, "stem_lr.txt")
stem_rf_results = run_rf(stem_datasets, stem_outer_split, paste0("stem_rf_span_", UP_TO_SPAN, ".txt"))

non_stem_datasets = lapply(datasets, function(dat) dat %>%
                         filter(!any_major_stem) %>%
                         select(-any_major_stem))
non_stem_outer_split = get_outer_split(non_stem_datasets[[1]])
#non_stem_lr_results = run_lr(non_stem_datasets, non_stem_outer_split, "non_stem_lr.txt")
non_stem_rf_results = run_rf(non_stem_datasets, non_stem_outer_split, paste0("non_stem_rf_span_", UP_TO_SPAN, ".txt"))

save(stem_outer_split, non_stem_outer_split, stem_rf_results, non_stem_rf_results, file=paste0("models/results/stem_results_span_", UP_TO_SPAN, ".Rdata"))

# is female
female_datasets = lapply(datasets, function(dat) dat %>%
                            filter(!is.na(data_to_impute$female)) %>% 
                            filter(female) %>%
                            select(-female))
 female_outer_split = get_outer_split(female_datasets[[1]])
 #female_lr_results = run_lr(female_datasets, female_outer_split, "female_lr.txt")
 female_rf_results = run_rf(female_datasets, female_outer_split, paste0("female_rf_span_", UP_TO_SPAN, ".txt"))

non_female_datasets = lapply(datasets, function(dat) dat %>%
                             filter(!is.na(data_to_impute$female)) %>%
                             filter(!female) %>%
                             select(-female))
non_female_outer_split = get_outer_split(non_female_datasets[[1]])
#non_female_lr_results = run_lr(non_female_datasets, non_female_outer_split, "non_female_lr.txt")
non_female_rf_results = run_rf(non_female_datasets, non_female_outer_split, paste0("non_female_rf_span_", UP_TO_SPAN, ".txt"))

save(female_outer_split, non_female_outer_split, female_rf_results, non_female_rf_results, file=paste0("models/results/female_results_span_", UP_TO_SPAN, ".Rdata"))

# low income
low_income_datasets = lapply(datasets, function(dat) dat %>%
                               filter(!is.na(data_to_impute$low_income)) %>%
                               filter(low_income) %>%
                               select(-low_income))
low_income_outer_split = get_outer_split(low_income_datasets[[1]])
#low_income_lr_results = run_lr(low_income_datasets, low_income_outer_split, "female_lr.txt")
low_income_rf_results = run_rf(low_income_datasets, low_income_outer_split, paste0("low_income_rf_span_", UP_TO_SPAN, ".txt"))

non_low_income_datasets = lapply(datasets, function(dat) dat %>%
                                   filter(!is.na(data_to_impute$low_income)) %>%
                               filter(!low_income) %>%
                               select(-low_income))
non_low_income_outer_split = get_outer_split(non_low_income_datasets[[1]])
#non_low_income_lr_results = run_lr(non_low_income_datasets, non_low_income_outer_split, "non_low_income_lr.txt")
non_low_income_rf_results = run_rf(non_low_income_datasets, non_low_income_outer_split, paste0("non_low_income_rf_span_", UP_TO_SPAN, ".txt"))

save(low_income_outer_split, non_low_income_outer_split, low_income_rf_results, non_low_income_rf_results, file=paste0("models/results/low_income_results_span_", UP_TO_SPAN, ".Rdata"))



# first generation
first_generation_datasets = lapply(datasets, function(dat) dat %>%
                                     filter(!is.na(data_to_impute$first_generation)) %>%
                        filter(first_generation) %>%
                        select(-first_generation))
first_generation_outer_split = get_outer_split(first_generation_datasets[[1]])
#first_generation_lr_results = run_lr(first_generation_datasets, first_generation_outer_split, "first_generation_lr.txt")
first_generation_rf_results = run_rf(first_generation_datasets, first_generation_outer_split, paste0("first_generation_rf_span_", UP_TO_SPAN, ".txt"))

non_first_generation_datasets = lapply(datasets, function(dat) dat %>%
                                         filter(!is.na(data_to_impute$first_generation)) %>%
                            filter(!first_generation) %>%
                            select(-first_generation))
non_first_generation_outer_split = get_outer_split(non_first_generation_datasets[[1]])
#non_first_generation_lr_results = run_lr(non_first_generation_datasets, non_first_generation_outer_split, "non_first_generation_lr.txt")
non_first_generation_rf_results = run_rf(non_first_generation_datasets, non_first_generation_outer_split, paste0("non_first_generation_rf_span_", UP_TO_SPAN, ".txt"))

save(first_generation_outer_split, non_first_generation_outer_split, first_generation_rf_results, non_first_generation_rf_results, file=paste0("models/results/first_generation_results_span_", UP_TO_SPAN, ".Rdata"))
