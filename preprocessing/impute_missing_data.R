# this script imputes missing data using MICE
if(!require(mice)) install.packages('mice')

# read feature dataset
source('read_data.R')
data = get_aggregated_features()

# parameters of mice
# m: the number of imputed datasets
if (!exists("nr_imputed_datasets")) nr_imputed_datasets = 5
# maxit: the number of iterations in each imputation
# meth: imputation method (rf means random forest)
system.time(tempData <- mice(data,
                             m=nr_imputed_datasets,
                             maxit=1,
                             meth='rf',
                             seed=500))

for (i in 1:nr_imputed_datasets) {
  features_imp = complete(tempData, i)

  # remove students with unknown outcome
  data_imp = features_imp[!is.na(data$dropout),]
  
  # save to data directory
  write_csv(data_imp, file.path(path_data, paste0('features_imputed_', i, '.csv')))
  #md.pattern(data[,1:10])
}

