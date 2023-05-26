if(!require(randomForest)) install.packages('randomForest')
if(!require(e1071)) install.packages('e1071') # for SVM
if(!require(pROC)) install.packages('pROC') # for ROC

setwd("~/EWS")
source("read_data.R")
dat = get_imputed_features() # should not contain any missing data

# Split randomly into training and test data
sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.7,0.3))
train <- dat[sample, ]
test <- dat[!sample, ]

# Fit Random Forest
rf <- randomForest(dropout ~ ., train)
predictions.rf <- predict(rf, newdata=test, type="prob")[,2]
roc.rf <- roc(test$dropout, predictions.rf)
auc.rf = auc(roc.rf)
varImpPlot(rf)

# Fit Logistic Regression
lr = glm(dropout ~ ., train, family = 'binomial')
predictions.lr = predict(lr, test, type = "response")
roc.lr <- roc(test$dropout, predictions.lr)
auc.lr = auc(roc.lr)

odds_ratios = data.frame(OR=exp(lr$coefficients))
ggplot(odds_ratios, aes(x=row.names(odds_ratios),y=OR))+
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1)) +
  scale_y_continuous(trans='log') +
  xlab("Predictor")

# Fit Support Vector Machine 
svm.fit = svm(dropout ~ ., train, type = 'C-classification', kernel = 'polynomial', probability = T)
predictions.svm = attr(predict(svm.fit, test, probability=T), 'probabilities')[,2]
roc.svm <- roc(test$dropout, predictions.svm)
auc.svm = auc(roc.svm)


labels = paste0(c("Random Forest","Logistic Reg.", "Linear SVM"), " (AUC: ", round(c(auc.rf, auc.lr, auc.svm), 2), ")")
ggroc(list(roc.rf, roc.lr, roc.svm), aes = c("color")) +
  scale_color_manual(labels=labels, values = c("blue","darkgreen","orange")) +
  labs(title="ROC of dropout prediction methods ", color="Method", x="Specificity", y="Sensitivity") +
  geom_abline(intercept = 1, slope = 1, color = "black", linetype = "dashed") +
  theme_minimal()


labels = paste0(c("Random Forest","Logistic Reg."), " (AUC: ", round(c(auc.rf, auc.lr), 2), ")")
ggroc(list(roc.rf, roc.lr), aes = c("color")) +
  scale_color_manual(labels=labels, values = c("blue","darkgreen")) +
  labs(title="ROC of dropout prediction methods ", color="Method", x="Specificity", y="Sensitivity") +
  geom_abline(intercept = 1, slope = 1, color = "black", linetype = "dashed") +
  theme_minimal()


