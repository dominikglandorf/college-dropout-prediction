# set wd to source file directory
source('read_data.R')

student_vars = get_student_vars()

admit_year = as.integer(substr(student_vars$admitdate, 2,3))
filter_admit = str_detect(student_vars$admitdate, "F08") | 
  admit_year >= 9 &
  admit_year < 50
filter_admit[is.na(filter_admit)] = F

filter_fall_cohorts = student_vars$first_term == 92 &
  student_vars$first_year < 2017 &
  student_vars$start_as_freshman
filter_fall_cohorts[is.na(filter_fall_cohorts)] = F

student_sub = student_vars[filter_admit & filter_fall_cohorts,]

# save to file
write_csv(student_sub, file.path(path_data, 'student_vars_subset.csv'))
