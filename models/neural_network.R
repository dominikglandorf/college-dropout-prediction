if(!require(neuralnet)) install.packages('neuralnet')
if(!require(PRROC)) install.packages('PRROC') # for PR curve
if(!require(recipes)) install.packages('recipes')

setwd("~/EWS")
source("read_data.R")
dat = get_imputed_features()[[1]] %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.logical, as.factor)

dat = dat %>% select(-major_name_1, -first_major)

dat = dat[sample(nrow(dat), 10000),]

dummies <-  recipe(dropout ~ ., data = dat) %>%
  step_dummy(female, int_student, ethnicity_smpl, first_generation, low_income, ell, cal_res_at_app, year_study, any_major_stem, major_school_name_1, first_year_study, first_school) %>%
  step_normalize(all_predictors()) %>% 
  prep(training = dat)
dat <- bake(dummies, new_data=NULL)

# Split randomly into training and test data
sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.7,0.3))
train <- dat[sample, ]
test <- dat[!sample, ]

model = neuralnet(
  dropout ~ .,
  data=train,
  hidden=c(15,15),
  linear.output = FALSE
)

#plot(model,rep = "best")

predictions <- compute(model, test)
dropout_pred = predictions$net.result[,2]

pr_curve <- pr.curve(scores.class0 = dropout_pred, weights.class0 = as.numeric(test$dropout)-1, curve=T)
ggplot(as.data.frame(pr_curve$curve), aes(x=V1, y=V2)) +
  geom_line() +
  ylim(0, 1) +
  geom_hline(yintercept=mean(as.numeric(test$dropout)-1)) +
  labs(title=paste0("PR-Curve (AUC: ", round(pr_curve$auc.davis.goadrich, 2), ")"),
       x="Recall",
       y="Precision")
