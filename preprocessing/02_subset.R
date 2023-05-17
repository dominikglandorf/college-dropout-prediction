#setwd('~/EWS') # set wd to root file directory
source('read_data.R')
bg = get_student_vars()

# CONFIGURATION
if (!exists("first_cohort")) first_cohort = 2008
if (!exists("last_cohort")) last_cohort = 2016

filter_year = bg$cohort >= first_cohort & bg$cohort <= last_cohort & !is.na(bg$cohort)

# first letter of admitdate indicates starting term
filter_fall = !is.na(bg$admitdate) & substr(bg$admitdate, 1, 1) == 'F'

filter_freshman = !is.na(bg$start_as_freshman) & bg$start_as_freshman

filter = filter_year & filter_fall & filter_freshman

student_sub = bg[filter, ]

# save to file
write_csv(student_sub, file.path(path_data, 'background_subset.csv'))
