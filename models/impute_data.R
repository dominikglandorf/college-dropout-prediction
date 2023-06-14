# this script imputes missing data using MICE
if(!require(mice)) install.packages('mice')

# read feature dataset
source('read_data.R')
data = get_aggregated_features(1) %>%
  mutate_if(is.character, as.factor)

# WHAT NOT TO IMPUTE (analysis see analyses/data_imputation.Rmd)
# IELTS score & foster care: almost always NA
data = data %>% select(-ielts_score, -foster_care)
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
# prefer number of AP or best AP score or passed AP rel

data = data %>% select(-single_parent, -household_size_app)

#data = data %>% select(-major_name_1, -major_school_name_1, -first_major, -first_school)

write_csv(data, file.path(path_data, paste0('data_to_impute.csv')))

# parameters of mice
# m: the number of imputed datasets
if (!exists("nr_imputed_datasets")) nr_imputed_datasets = 1
# maxit: the number of iterations in each imputation
# meth: imputation method (rf means random forest)
# checked out '2l.lmer', gives a lot of warnings
imp <- mice(data,
            m=nr_imputed_datasets,
            maxit=5,
            meth='rf')
# use with to pool results
datasets = lapply(1:nr_imputed_datasets, function(i) complete(imp, i))
written = lapply(1:nr_imputed_datasets, function(i) write_csv(datasets[[i]], file.path(path_data, paste0('features_imputed_', i, '.csv'))))
