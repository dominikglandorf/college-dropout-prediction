# run this script to create a dataset of a subset of students with imputed missing data based on these three information levels:
# 1. student level
# 2. student x term level
# 3. student x term x course level

# CONFIGURATION
# this property defines up to which point in the time (=the term number) for each respective student the final dataset is assembled
until_term = 3

setwd() # set to source file directory

# derives variables from student background and term data
source('preprocessing/calculate_student_vars.R')

# selects cohorts admitted after start of data recording, starting in a fall term as a freshman at least 6 years ago
source('preprocessing/subset.R')

# derives and copies variables from term data
source('preprocessing/calculate_term_features.R')

# derives variables from course data
source('preprocessing/calculate_course_features.R')

# aggregate variables and merge all datasets until a specific term
source('preprocessing/merge_features.R')

# impute missing data
source('preprocessing/impute_missing_data.R')
