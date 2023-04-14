if(!require(randomForest)) install.packages('randomForest')
if(!require(e1071)) install.packages('e1071')
if(!require(pROC)) install.packages('pROC')

setwd("~/EWS")
source("read_data.R")
students = get_student_sub()

predictors = c("age_at_enrolment",
               "uc_total_score",
               "uc_math_score",
               "uc_read_score",
               "uc_writing_score",
               "number_ap",
               "passed_ap_rel",
               "best_ap",
               "avg_ap",
               "int_student",
               "ethnicity_smpl",
               "first_generation",
               "low_income",
               "father_edu_level_code",
               "mother_edu_level_code",
               "ell",
               "single_parent",
               "foster_care",
               "household_size_app",
               "distance_from_home",
               "sport_at_admission",
               "cal_res_at_app",
               "hs_gpa")
dat = students %>% select(all_of(c(predictors, "dropout")))

# sanitizing data
dat$low_income[is.na(dat$low_income)]=F
dat$household_size_app[dat$household_size_app>6] = 6
dat$father_edu_level_code[dat$father_edu_level_code==4]=NA
saa = table(dat$sport_at_admission)
dat$sport_at_admission[dat$sport_at_admission %in% names(saa[saa<nrow(dat)/1000])] = "other"

categorical_cols <- c("int_student","ethnicity_smpl","first_generation","father_edu_level_code","mother_edu_level_code",  "low_income","ell","single_parent","foster_care","household_size_app","sport_at_admission","cal_res_at_app")
dat[categorical_cols] <- lapply(dat[categorical_cols], as.character)
dat[categorical_cols][is.na(dat[categorical_cols])] = "unknown"
# delete incomplete rows
dat = dat[complete.cases(dat),]
dat[categorical_cols] <- lapply(dat[categorical_cols], as.factor)

dat$dropout = as.factor(dat$dropout)

sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.7,0.3))
train <- dat[sample, ]
test <- dat[!sample, ]


rf <- randomForest(dropout ~ ., train)
predictions.rf <- predict(rf, newdata=test, type="prob")[,2]
roc.rf <- roc(test$dropout, predictions.rf)
auc.rf = auc(roc.rf)

# check for train preds
#predictions.rf <- predict(rf, type="prob")[,2]
#roc.rf <- roc(train$dropout, predictions.rf)
#auc(roc.rf)

# compare to log.reg
lr = glm(dropout ~ ., train, family = 'binomial')
predictions.lr = predict(lr, test, type = "response")
roc.lr <- roc(test$dropout, predictions.lr)
auc.lr = auc(roc.lr)

# check for train preds
#predictions.lr <- predict(lr, train, type="response")
#roc.lr <- roc(train$dropout, predictions.lr)
#auc(roc.lr)

# compare to linear SVM
svm.fit = svm(dropout ~ ., train, type = 'C-classification', kernel = 'polynomial', probability = T)
predictions.svm = attr(predict(svm.fit, test, probability=T), 'probabilities')[,2]
roc.svm <- roc(test$dropout, predictions.svm)
auc.svm = auc(roc.svm)

aera_theme = theme_minimal() +
  theme(plot.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 18, color="black"),
        axis.text = element_text(size = 14, color="black"),
        axis.text.x = element_text(vjust = +2.5),
        axis.text.y = element_text(hjust = +1.25))

labels = paste0(c("Random Forest","Logistic Reg.", "Linear SVM"), " (AUC: ", round(c(auc.rf, auc.lr, auc.svm), 2), ")")

ggroc(list(roc.rf, roc.lr, roc.svm), aes = c("color")) +
  scale_color_manual(labels=labels, values = c("blue","darkgreen","orange")) +
  labs(title="ROC of dropout prediction methods ", color="Method", x="Specificity", y="Sensitivity") +
  geom_abline(intercept = 1, slope = 1, color = "black", linetype = "dashed") +
  aera_theme
