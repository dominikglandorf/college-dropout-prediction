if(!require(reticulate)) install.packages('reticulate')
use_virtualenv("r-reticulate")
if(!require(keras)) install.packages('keras')
if(!require(tensorflow)) install.packages('tensorflow')
if(!require(randomForest)) install.packages('randomForest')
if(!require(e1071)) install.packages('e1071') # for SVM
if(!require(class)) install.packages('class') # for kNN
if(!require(recipes)) install.packages('recipes') # for dummy coding

setwd("~/EWS")
source("read_data.R")
source('models/evaluation.R')


train_and_evaluate = function (dat, split) {
  print("Next dataset")
  train <- dat[split, ]
  test <- dat[!split, ]
  
  true_labels = test$dropout
  
  # Fit Logistic Regression
  print("Logistic Regression")
  lr = glm(dropout ~ ., train, family = 'binomial')
  predictions.lr = predict(lr, test, type = "response")
  lr_pfi = feature_importance(function(x) predict(lr, x, type = "response"), test)
  lr_metrics = get_all_metrics(predictions.lr, true_labels)
  
  # Fit Random Forest
  print("Random Forest")
  rf <- randomForest(as.factor(dropout) ~ ., train)
  predictions.rf <- predict(rf, newdata=test, type="prob")[,"TRUE"]
  rf_pfi = feature_importance(function(x) predict(rf, newdata=x, type = "prob")[,"TRUE"], test)
  rf_metrics = get_all_metrics(predictions.rf, true_labels)
  
  # Fit SVM
  print("SVM")
  train$dropout = factor(train$dropout, levels=c(FALSE, TRUE))
    
  svm.fit = svm(dropout ~ ., train, kernel = 'radial', probability = T, cost=0.1,
                class.weights=c("TRUE"=1, "FALSE"=1))
  predictions.svm <- attr(predict(svm.fit, test, probability=T), 'probabilities')[,"TRUE"]
  svm_pfi = feature_importance(function(x) attr(predict(svm.fit, x, probability=T), 'probabilities')[,"TRUE"], test)
  svm_metrics = get_all_metrics(predictions.svm, true_labels)
  
  # Fit NaiveBayes
  print("Naive Bayes")
  model <- naiveBayes(dropout ~ ., data = train, usekernel = T)
  predictions.nB <- predict(model, test, type = 'raw')[,'TRUE']
  nB_metrics = get_all_metrics(predictions.nB, true_labels)
  
  nB_pfi = feature_importance(function(x) predict(model, x, type = 'raw')[,'TRUE'], test)
  
  
  # Fit kNN
  print("kNN")
  dat = dat %>%
    mutate_if(is.logical, as.numeric)
  train <- dat[split, ]
  test <- dat[!split, ]
  
  # first transform to dummy variables and normalize to have good Euclidian distances
  prep_recipe <-  recipe(dropout ~ ., dat) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_normalize(all_predictors()) %>% 
    prep(training = dat)

  
  kNN_predict = function(test) {
    predictions.kNN <- knn(bake(prep_recipe, new_data = train %>% select(-dropout)),
                           bake(prep_recipe, new_data = test %>% select(-dropout)),
                           cl=train$dropout,
                           k=10, prob=T)
    scores.kNN = attr(predictions.kNN, "prob")
    # we have to inverse the probs where no probability was predicted
    scores.kNN[predictions.kNN==0] = 1 - scores.kNN[predictions.kNN==0]
    return(scores.kNN)
  }
  
  kNN_pfi = feature_importance(kNN_predict, test)
  kNN_metrics = get_all_metrics(kNN_predict(test), true_labels)
  
  # Neural Network
  print("Neural network")
  train = bake(prep_recipe, new_data = train)
  X_train = as.matrix(train %>% select(-dropout))
  y_train = to_categorical(train$dropout, num_classes=2)
  
  model <- keras_model_sequential(input_shape = ncol(X_train)) %>%
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
  
  history = model %>% fit(X_train, y_train, epochs = 5, batch_size = 128, validation_split = 0.2)
  
  predict_nn = function(test) {
    test = bake(prep_recipe, new_data = test)
    X_test = as.matrix(test %>% select(-dropout))
    y_test = to_categorical(test$dropout, num_classes=2)
    return (predict(model, X_test)[,2])
  }
  nn_pfi = feature_importance(predict_nn, test)
  
  test = bake(prep_recipe, new_data = test)
  X_test = as.matrix(test %>% select(-dropout))
  y_test = to_categorical(test$dropout, num_classes=2)
  predictions.nn = predict(model, X_test)[,2]
  nn_metrics = get_all_metrics(predictions.nn, true_labels)
  
  # combine metrics
  return(list(
    scores=data.frame(rbind(lr_metrics, rf_metrics, svm_metrics, kNN_metrics, nB_metrics, nn_metrics)),
    lr_pfi=lr_pfi,
    rf_pfi=rf_pfi,
    svm_pfi=svm_pfi,
    kNN_pfi=kNN_pfi,
    nB_pfi=nB_pfi,
    nn_pfi=nn_pfi))
}


datasets = get_imputed_features(1)

# to speed up training, a subsample could be taken here
datasets = lapply(datasets, function(dat) dat %>% sample_n(1000))

# drop dependent features
datasets = lapply(datasets, function(dat) dat %>%
                    mutate_if(is.character, as.factor) %>%
                    select(-major_name_1,
                           -uc_total_score,
                           -first_credits,
                           -first_credits_major,
                           -term_span))

# Split randomly into training and test data
split <- sample(c(TRUE, FALSE), nrow(datasets[[1]]), replace=TRUE, prob=c(0.7,0.3))

eval_results = lapply(datasets[1:5], train_and_evaluate, split=split)

scores = lapply(eval_results, function(x) x$scores)
score = tapply(unlist(scores), rep(1:24, 5), FUN = mean)
metric_methods = names(unlist(scores))[1:24]
data.frame(mm=metric_methods, s=score)
