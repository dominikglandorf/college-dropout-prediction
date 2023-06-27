if(!require(class)) install.packages('class') # for kNN
if(!require(PRROC)) install.packages('PRROC') # for PR curve
if(!require(recipes)) install.packages('recipes')

setwd("~/EWS")
source("read_data.R")
source('models/evaluation.R')

dat = get_imputed_features(1)[[1]] %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.logical, as.numeric)

dat = dat %>% select(-major_name_1,
                     -uc_total_score,
                     -first_credits,
                     -first_credits_major,
                     -term_span)

#dat = dat[sample(nrow(dat), 5000),]

dummies <-  recipe(dropout ~ ., data = dat) %>%
  step_dummy(citizenship_app, ethnicity_smpl, int_student, geo_category, cal_res_at_app, year_study, major_school_name_1, first_year_study) %>%
  step_normalize(all_predictors()) %>% 
  prep(training = dat)
dat <- bake(dummies, new_data = NULL)


# Split randomly into training and test data
sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.7,0.3))
train <- dat[sample, ]
test <- dat[!sample, ]

# run kNN classifier
predictions.kNN <- knn(train %>% select(-dropout),
                       test %>% select(-dropout),
                       cl=train$dropout,
                       k=10, prob=T)
scores.kNN = attr(predictions.kNN, "prob")
# we have to inverse the probs where no probability was predicted
scores.kNN[predictions.kNN==0] = 1 - scores.kNN[predictions.kNN==0]

get_all_metrics(scores.kNN, as.logical(test$dropout))

