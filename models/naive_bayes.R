# Naive Bayes guide: https://www.r-bloggers.com/2021/04/naive-bayes-classification-in-r/
# important: categorical predictors (so discretization)
# works better in case of independent predictors

if(!require(e1071)) install.packages('e1071') # for naiveBayes
if(!require(recipes)) install.packages('recipes') # for discretization

setwd("~/EWS")
source("read_data.R")
source('models/evaluation.R')


datasets = get_imputed_features(1)

dat = datasets[[5]]

dat = dat %>%
  mutate_if(is.character, as.factor)

dat = dat %>% select(-major_name_1,
                     -uc_total_score,
                     -first_credits,
                     -first_credits_major,
                     -term_span)

# to speed up training, a subsample could be taken here
#dat = dat %>% sample_n(20000)

# Split randomly into training and test data
sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.7,0.3))
train <- dat[sample, ]
test <- dat[!sample, ]

model <- naiveBayes(dropout ~ ., data = train, usekernel = T)
predictions.nB <- predict(model, test, type = 'raw')[,'TRUE']

true_labels = test$dropout

nB_pfi = feature_importance(function(x) predict(model, x, type = 'raw')[,'TRUE'], test)
nB_metrics = get_all_metrics(predictions.nB, true_labels)
plot_PR(predictions.nB, true_labels)
