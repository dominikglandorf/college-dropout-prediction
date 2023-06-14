if(!require(randomForest)) install.packages('randomForest')
if(!require(e1071)) install.packages('e1071') # for SVM

setwd("~/EWS")
source("read_data.R")
source('models/evaluation.R')
dat = get_imputed_features()[[1]] %>%
  mutate_if(is.character, as.factor)

dat = dat %>% select(-major_name_1, -first_major, -first_school)

# Split randomly into training and test data
sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.7,0.3))
train <- dat[sample, ]
test <- dat[!sample, ]

true_labels = test$dropout

# Fit Random Forest
rf <- randomForest(as.factor(dropout) ~ ., train)
predictions.rf <- predict(rf, newdata=test, type="prob")[,2]

rf_metrics = get_all_metrics(predictions.rf, true_labels)
varImpPlot(rf)

# Fit Logistic Regression
lr = glm(dropout ~ ., train, family = 'binomial')
predictions.lr = predict(lr, test, type = "response")
lr_metrics = get_all_metrics(predictions.lr, true_labels)



# Fit Support Vector Machine 
svm.fit = svm(dropout ~ ., train, type = 'C-classification', kernel = 'polynomial', probability = T)
predictions.svm = attr(predict(svm.fit, test, probability=T), 'probabilities')[,1]
svm_metrics = get_all_metrics(predictions.svm, true_labels)


data.frame(rbind(lr_metrics, rf_metrics, svm_metrics)) 

plot_multiple_ROC(list("LR"=predictions.lr,
                       "RF"=predictions.rf,
                       "SVM"=predictions.svm),
                  true_labels)

plot_multiple_PRC(list("LR"=predictions.lr,
                       "RF"=predictions.rf,
                       "SVM"=predictions.svm),
                  true_labels)

