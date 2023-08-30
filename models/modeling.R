if(!require(reticulate)) install.packages('reticulate')
use_virtualenv("r-reticulate")
if(!require(keras)) install.packages('keras')
if(!require(tensorflow)) install.packages('tensorflow')
if(!require(recipes)) install.packages('recipes') # for dummy coding
if(!require(randomForest)) install.packages('randomForest')
if(!require(e1071)) install.packages('e1071') # for SVM
if(!require(class)) install.packages('class') # for kNN

setwd("~/EWS")
source("read_data.R")
source('models/hyperparameter_tuning.R')

UP_TO_YEAR = 2
datasets = get_imputed_features(UP_TO_YEAR)

print_to_file = function(path, content) {
  file_conn <- file(path, "w")
  sink(file_conn) # Redirect the output to the file
  print(content)  # Print to the console
  sink() # Stop redirecting the output and close the file connection
  close(file_conn)
}

# to speed up training, a subsample could be taken here
#datasets = lapply(datasets, function(dat) dat %>% sample_n(10000))

# drop dependent features
datasets = lapply(datasets, function(dat) dat %>%
                    mutate_if(is.character, as.factor) %>%
                    select(-major_name_1,
                           -uc_total_score,
                           -first_credits,
                           -first_credits_major,
                           -term_span))
data_dim = ncol(datasets[[1]])
outer_split = get_outer_split(datasets[[1]])

# logistic regression
train_predict_lr = function(train_data, param_combo) {
  lr = glm(dropout ~ ., train_data, family = 'binomial')
  function(test) predict(lr, test, type = "response")
}
param_combos_lr = expand.grid(para1 = list(1)) # this is just for generality of the functions
lr_results = imputations_loop(datasets, outer_split, train_predict_lr, param_combos_lr,
                              num_imps=10, no_pfi=F)
print_to_file(paste0("models/results/year_", UP_TO_YEAR, "_lr.txt"), lr_results)

# random forest
train_predict_rf = function(train_data, param_combo) {
  rf <- randomForest(as.factor(dropout) ~ ., train_data,
                     ntree=param_combo$ntree,
                     mtry=param_combo$mtry)
  function(test) predict(rf, newdata=test, type="prob")[,"TRUE"]
}
param_combos_rf = expand.grid(ntree = c(500, 1000, 1500),
                              mtry =  c(3, 5, 6, 7) )
rf_results = imputations_loop(datasets, outer_split, train_predict_rf, param_combos_rf,
                              num_imps=10, no_pfi=F, sample_size=10000)
print_to_file(paste0("models/results/year_", UP_TO_YEAR, "_rf.txt"), rf_results)

# SVM
train_predict_svm = function(train_data, param_combo) {
  svm.fit = svm(as.factor(dropout) ~ ., train_data, probability = T,
                class.weights=c("TRUE"=param_combo$class_weight_true, "FALSE"=1),
                kernel = param_combo$kernel,
                cost = param_combo$cost,
                gamma = param_combo$gamma)
  function(test) attr(predict(svm.fit, test, probability=T), 'probabilities')[,"TRUE"]
}
param_combos_svm = expand.grid(cost=c(0.1, 0.5, 1),
                               kernel=c('radial'), # 'linear', 'polynomial'
                               class_weight_true=c(3, 5), #1
                               gamma=c(0.01 / data_dim, 0.1/data_dim, 1/data_dim)) #10
svm_results = imputations_loop(datasets, outer_split, train_predict_svm, param_combos_svm,
                               num_imps=10, sample_size=5000, no_pfi=F)
print_to_file(paste0("models/results/year_", UP_TO_YEAR, "_svm.txt"), svm_results)

# NaiveBayes
train_predict_nB =  function(train_data, param_combo) {
  nB.fit = naiveBayes(as.factor(dropout) ~ ., train_data,
                      usekernel = T, laplace=param_combo$laplace)
  function(test) predict(nB.fit, test, type = 'raw')[,'TRUE']
}
param_combos_nb = expand.grid(laplace=c(0, 0.1, 0.5, 1), x=c(1))
nB_results = imputations_loop(datasets, outer_split, train_predict_nB, param_combos_nb,
                              num_imps=10, no_pfi=T)
print_to_file(paste0("models/results/year_", UP_TO_YEAR, "_nb.txt"), nB_results)

# kNN
transform_dat = function(dat) {
  dat = dat %>%
    mutate_if(is.logical, as.numeric)
}

train_predict_kNN = function(train_data, param_combo) {
  # first transform to dummy variables and normalize to have good Euclidian distances
  prep_recipe <-  recipe(dropout ~ ., train_data) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_normalize(all_predictors()) %>% 
    step_pca(all_predictors(), threshold = .80) %>% 
    prep(training = train_data)
  baken_train = bake(prep_recipe, new_data = train_data %>% select(-dropout))
  function(test) {
    kNN.preds = knn(baken_train,
                    bake(prep_recipe, new_data = test %>% select(-dropout)),
                    cl=train_data$dropout, prob=T,
                    k=param_combo$k)
    scores.kNN = attr(kNN.preds, "prob")
    scores.kNN[kNN.preds==0] = 1 - scores.kNN[kNN.preds==0]
    return(scores.kNN)
  }
}
param_combos_kNN = expand.grid(k=c(9, 19, 39, 59, 99, 199, 299), x=c(1))
kNN_results = imputations_loop(datasets, outer_split, train_predict_kNN, param_combos_kNN,
                               num_imps=10, transform_dat=transform_dat, no_pfi=T)
print_to_file(paste0("models/results/year_", UP_TO_YEAR, "_knn.txt"), kNN_results)

# Neural Networks
train_predict_nn = function(train_data, param_combo) {
  # first transform to dummy variables and normalize to have good Euclidian distances
  prep_recipe <-  recipe(dropout ~ ., train_data) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_normalize(all_predictors()) %>% 
    prep(training = train_data)
  
  train = bake(prep_recipe, new_data = train_data)
  X_train = as.matrix(train %>% select(-dropout))
  y_train = to_categorical(train$dropout, num_classes=2)
  
  model <- keras_model_sequential(input_shape = ncol(X_train)) %>%
    layer_dense(units = param_combo$first_layer, activation = "relu") %>% 
    layer_dropout(rate = param_combo$dropout) %>% 
    layer_dense(units = param_combo$first_layer * param_combo$reduction, activation = "relu") %>%
    layer_dropout(rate = param_combo$dropout) %>%
    layer_dense(units = ncol(y_train), activation = "softmax")
  
  model %>% compile(
    optimizer = "adam",
    loss = "categorical_crossentropy",
    metrics = "accuracy"
  )
  
  history = model %>% fit(X_train, y_train, epochs = param_combo$epochs, batch_size = 128, validation_split = 0.2, verbose=0)
  
  function(test) {
    test = bake(prep_recipe, new_data = test)
    X_test = as.matrix(test %>% select(-dropout))
    y_test = to_categorical(test$dropout, num_classes=2)
    return (predict(model, X_test)[,2])
  }
}
param_combos_nn = expand.grid(first_layer=c(256, 512, 1024),
                              reduction=c(0.25, 0.5),
                              dropout=c(0, 0.5),
                              epochs=c(5, 10))
nn_results = imputations_loop(datasets, outer_split, train_predict_nn, param_combos_nn,
                               num_imps=10, transform_dat=transform_dat, sample_size=10000)
print_to_file(paste0("models/results/year_", UP_TO_YEAR, "_nn.txt"), nn_results)

save(outer_split, lr_results, rf_results, nB_results, kNN_results, nn_results, svm_results, file=paste0("models/results/year_", UP_TO_YEAR, "_results.Rdata"))
