if(!require(reticulate)) install.packages('reticulate')
use_virtualenv("r-reticulate")

if(!require(recipes)) install.packages('recipes')
if(!require(keras)) install.packages('keras')
#if(!require(caret)) install.packages('caret')
if(!require(tensorflow)) install.packages('tensorflow')


setwd("~/EWS")
source("read_data.R")
source("models/evaluation.R")
dat = get_imputed_features(1)[[1]] %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.logical, as.numeric)

dat$dropout = as.logical(dat$dropout)

dat = dat %>% select(-major_name_1,
                     -uc_total_score,
                     -first_credits,
                     -first_credits_major,
                     -term_span)

#dat = dat[sample(nrow(dat), 3000),]

# Split randomly into training and test data
sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.7,0.3))
train <- dat[sample, ]
test <- dat[!sample, ]

prep_recipe <-  recipe(dropout ~ ., dat) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_normalize(all_predictors()) %>% 
  prep(training = dat)

train = bake(prep_recipe, new_data = train)
test = bake(prep_recipe, new_data = test)

X_train = as.matrix(train %>% select(-dropout))
y_train = to_categorical(train$dropout, num_classes=2)
X_test = as.matrix(test %>% select(-dropout))
y_test = to_categorical(test$dropout, num_classes=2)

model <- keras_model_sequential(input_shape = ncol(X_train))

# Network design
model %>%
  layer_dense(units = 256, activation = "relu") %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = ncol(y_train), activation = "softmax")

model %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = "accuracy"
)

history = model %>% fit(X_train, y_train, epochs = 20, batch_size = 128, validation_split = 0.2)

predictions.nn = predict(model, X_test)[,2]
true_labels = as.logical(test$dropout)
get_all_metrics(predictions.nn, true_labels)
