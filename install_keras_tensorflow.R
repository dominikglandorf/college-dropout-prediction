

library(reticulate)
path_to_python <- install_python()
virtualenv_create("r-reticulate", python = path_to_python)
use_virtualenv("r-reticulate")

library(tensorflow)
#install_tensorflow(envname = "r-reticulate")

library(keras)
#install_keras(envname = "r-reticulate")


tf$constant("Hello Tensorflow!")

