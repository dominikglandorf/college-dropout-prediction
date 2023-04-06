# run this script to create the three datasets:
# 1. student level
# 2. student x term level
# 3. student x term x course level

setwd() # set to source file directory

# derives variables from student background and term data
source('calculate_student_vars.R')

# selects cohorts admitted after start of data recording, starting in a fall term as a freshman at least 6 years ago
source('subset.R')

# derives and copies variables from term data
source('calculate_term_features.R')

# derives variables from course data
source('calculate_course_features.R')
