# setup the python environment for the machine learning packages tensorflow and keras
if(!require(reticulate)) install.packages('reticulate')

library(reticulate)
path_to_python <- install_python()
virtualenv_create("r-reticulate", python = path_to_python)
use_virtualenv("r-reticulate")

if(!require(tensorflow)) install.packages('tensorflow')
library(tensorflow)
install_tensorflow(envname = "r-reticulate")

if(!require(keras)) install.packages('keras')
library(keras)
install_keras(envname = "r-reticulate")


tf$constant("Hello Tensorflow!")

