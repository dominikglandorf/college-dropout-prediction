# this script imputes missing data using MICE
if(!require(mice)) install.packages('mice')

# read feature dataset
source('read_data.R')
data = get_aggregated_features()

# WHAT NOT TO IMPUTE (analysis see analyses/data_imputation.Rmd)
# IELTS score: almost always NA
data = data %>% select(-ielts_score)
# TOEFL score: almost always NA for non-international students, availability be combined with int student status
data = data %>%
  mutate(int_student = as.factor(if_else(int_student==1, 
                               if_else(is.na(toefl_score), "yesNoToefl", "yesWithToefl"),
                               "no"))) %>% select(-toefl_score)

# availability of sport at admission correlates highly with the zero dropout student id ranges
data = data %>% select(-sport_at_admission)

# AP exams: for students with no exams, we have no performance score -> summarize info
data = data %>%
  mutate(ap_best = if_else(number_ap > 0, best_ap, 0)) %>% 
  select(-number_ap, -passed_ap_rel, -avg_ap, -best_ap)

# parameters of mice
# m: the number of imputed datasets
if (!exists("nr_imputed_datasets")) nr_imputed_datasets = 5
# maxit: the number of iterations in each imputation
# meth: imputation method (rf means random forest)
# checked out '2l.lmer', gives a lot of warnings
tempData <- mice(data,
                 m=nr_imputed_datasets,
                 maxit=5,
                 meth='rf')
# use with to pool results
for (i in 1:nr_imputed_datasets) {
  features_imp = complete(tempData, i)
  
  # save to data directory
  write_csv(data_imp, file.path(path_data, paste0('features_imputed_', i, '.csv')))
  #md.pattern(data[,1:10])
}

