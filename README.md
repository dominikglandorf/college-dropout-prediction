# Temporal and Between-Group Variability in College Dropout Prediction
This analysis corresponds to the full paper with the above title at Learning Analytics & Knowledge 2024. The project aims to investigate college dropout prediction from administrative data. The supplementary material is located [here](https://github.com/dominikglandorf/college-dropout-prediction/blob/main/Supplementary_Material.pdf).

# Prerequisites
To reproduce the preprocessing and the analyses, you must have [R](https://www.r-project.org) installed. Most scripts will automatically install the required packages. You should always set the working directory to the project's main repository. You also need to request access to the data, which we unfortunately cannot share with persons other than research group members of the [UCI Measuring Undergraduate Success Trajectories project](https://sites.uci.edu/ucimustproject/).

# Project structure

## Data preprocessing
For the preprocessing, you need the following:
- Locate the CSV files on your machine containing the student background, term, and course data for the students. 
- Create a copy of `config.R.example` named `config.R` and change the variables to match your local directory structure. In case of doubt, check `read_data.R` for the collection of functions that accesses datasets and contains the required filenames.

Execute the entire preprocessing by executing `pipeline.R`. The file invokes the scripts in the directory `preprocessing`, which may be executed individually. They may depend on intermediate results from previous scripts.

## Descriptive analyses
The directory `analyses` contains scripts to create various descriptive analyses.

## Modeling
You can use the file `install_keras_tensorflow.R` to set up the required Python environment with reticulate. The directory `models` contains scripts that model the statistical structure of student dropout. First, you have to run `impute_data.R` to predict complete observations for all different observation periods. Then, retrain the models by executing the R scripts that correspond to the research questions. The files with the suffix `results` contain the code to create the results tables based on the model evaluations. The remaining files are auxiliary scripts or initial isolated tests that we left inside the repository for reference.

## Miscellaneous
The folder `data` contains mappings and dataset information for further analyses. The folder `documentation` contains plots in the paper and additional plots for reference.
