if(!require(nnet)) install.packages('nnet')
if(!require(recipes)) install.packages('recipes')

setwd("~/EWS")
source("read_data.R")
source("models/evaluation.R")
dat = get_imputed_features()[[1]] %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.logical, as.factor)

dat = dat %>% select(-major_name_1, -first_major)

dat = dat[sample(nrow(dat), 3000),]

dummies <-  recipe(dropout ~ ., data = dat) %>%
  step_dummy(female, int_student, ethnicity_smpl, first_generation, low_income, ell, cal_res_at_app, year_study, any_major_stem, major_school_name_1, first_year_study, first_school) %>%
  step_normalize(all_predictors()) %>% 
  prep(training = dat)
dat <- bake(dummies, new_data=NULL)

# Split randomly into training and test data
sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.7,0.3))
train <- dat[sample, ]
test <- dat[!sample, ]

model = nnet(formula=dropout ~ .,
             data=train,
             size=c(5,5))


model = neuralnet(
  dropout ~ .,
  data=train,
  err.fct='ce',
  hidden=c(3),
  linear.output = FALSE,
  stepmax=3000
)

# check overfitting
#predictions.nn <- compute(model, train)$net.result[,2]
predictions.nn = predict(model, train)[,1]
true_labels = as.logical(train$dropout)
get_all_metrics(predictions.nn, true_labels)
plot_ROC(predictions.nn, true_labels)
plot_PR(predictions.nn, true_labels)

#plot(model,rep = "best")

predictions.nn = predict(model, test)[,1]
true_labels = as.logical(test$dropout)

nn_metrics = get_all_metrics(predictions.nn, true_labels)
plot_ROC(predictions.nn, true_labels)
plot_PR(predictions.nn, true_labels)
