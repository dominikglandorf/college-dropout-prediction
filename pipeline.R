# run this script to create a dataset of a subset of students with imputed missing data based on these three information levels:
# 1. student level
# 2. student x term level
# 3. student x term x course level

# CONFIGURATION
# this property defines up to which point in the time (=the term number) for each respective student the final dataset is assembled

# setwd() # set to repository file directory

# derives variables from student background and term data
source('preprocessing/01_background_data.R')
rm(list = ls())

# selects cohorts admitted after start of data recording, starting in a fall term as a freshman in a cohort specified above
first_cohort = 2011
last_cohort = 2016
source('preprocessing/02_subset.R')
rm(list = ls())

# preprocesses term level data
source('preprocessing/03_term_data.R')
rm(list = ls())

# preprocesses course level data
source('preprocessing/04_course_data.R')
rm(list = ls())

# aggregates variables and merge datasets from all levels until a specific term
until_term = 3
source('preprocessing/05_merge_levels.R')
rm(list = ls())