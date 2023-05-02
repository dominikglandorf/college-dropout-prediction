# this script imputes missing data using MICE
if(!require(mice)) install.packages('mice')

# read feature dataset
source('read_data.R')
data = get_aggregated_features()
features = data %>% select(-dropout) # exclude label from data imputation

# parameters of mice
# m: the number of imputed datasets
# maxit: the number of iterations in each imputation
# meth: imputation method = random forest
system.time(tempData <- mice(features, m=1, maxit=5, meth='rf', seed=500))
features_imp = complete(tempData, 1)

# join label again
features_imp$dropout = data$dropout
# filter to rows where dropout data
data_imp = features_imp %>% filter(complete.cases(features_imp))

# save to data directory
write_csv(data_imp, file.path(path_data, 'features_imputed.csv'))#
#md.pattern(data[,1:10])
