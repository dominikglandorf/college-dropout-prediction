if(!require(e1071)) install.packages('e1071') # for SVM

setwd("~/EWS")
source("read_data.R")

datasets = get_imputed_features(1)

dat = datasets[[1]]

dat = dat %>%
  mutate_if(is.character, as.factor)

dat = dat %>% select(-major_name_1,
                     -uc_total_score,
                     -first_credits,
                     -first_credits_major,
                     -term_span)

# to speed up training, a subsample could be taken here
dat = dat %>% sample_n(5000)

# Split randomly into training and test data
sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.7,0.3))
train <- dat[sample, ]
test <- dat[!sample, ]

train$dropout = as.factor(train$dropout)


# this does not really work so far
#svm.fit = svm(dropout ~ ., train, type = 'C-classification', kernel = 'radial', probability = T)
svm.fit = svm(dropout ~ ., train, kernel = 'radial', probability = T, cost=1)

predictions.svm = attr(predict(svm.fit, test, probability=T), 'probabilities')[,1]
true_labels = as.factor(test$dropout)
svm_metrics = get_all_metrics(predictions.svm, true_labels)

