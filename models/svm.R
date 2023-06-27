if(!require(e1071)) install.packages('e1071') # for SVM

setwd("~/EWS")
source("read_data.R")
source('models/evaluation.R')

datasets = get_imputed_features(1)

dat = datasets[[4]]

dat = dat %>%
  mutate_if(is.character, as.factor)

dat$dropout = factor(dat$dropout, levels = c(FALSE, TRUE))

dat = dat %>% select(-major_name_1,
                     -uc_total_score,
                     -first_credits,
                     -first_credits_major,
                     -term_span)

# to speed up training, a subsample could be taken here
dat = dat %>% sample_n(20000)

# Split randomly into training and test data
sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.7,0.3))
train <- dat[sample, ]
test <- dat[!sample, ]


# this does not really work so far
#svm.fit = svm(dropout ~ ., train, type = 'C-classification', kernel = 'radial', probability = T)
system.time(svm.fit <- svm(dropout ~ ., train, kernel = 'radial', probability = T, cost=0.1,
              class.weights=c("TRUE"=1, "FALSE"=1)))



# check for train data

predictions.svm = attr(predict(svm.fit, train, probability=T), 'probabilities')[,"TRUE"]
true_labels = as.logical(train$dropout)
get_all_metrics(predictions.svm, true_labels)


predictions.svm = attr(predict(svm.fit, test, probability=T), 'probabilities')[,"TRUE"]
true_labels = as.logical(test$dropout)
get_all_metrics(predictions.svm, true_labels)

tune.out <- tune(svm, RESPONSE~., data = train, kernel="radial", 
                 ranges = list(gamma=c(0.1,0.5,1,2,4), 
                               cost = c(0.1,1,10,100,1000)
                 ), 
                 class.weights= c("0" = 1, "1" = 10))

