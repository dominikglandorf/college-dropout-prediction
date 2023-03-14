# set wd to source file directory
source('read_data.R')

students = get_student_sub()
terms = get_term_data(ids = students$mellon_id)
courses = get_course_data(ids = students$mellon_id)

# course was taken in school of majors
courses = courses %>%
  mutate(school=dept_schools[dept_name_abbrev]) %>%
  left_join(terms %>% select(c('mellon_id','term_code', paste0('major_school_name_abbrev_', c(1,2,3,4)))),
            by=c("mellon_id","term_code")) %>% 
  mutate(in_school = (school == major_school_name_abbrev_1 |
                      (!is.na(major_school_name_abbrev_2) & school == major_school_name_abbrev_2) |
                      (!is.na(major_school_name_abbrev_3) & school == major_school_name_abbrev_3) |
                      (!is.na(major_school_name_abbrev_4) & school == major_school_name_abbrev_4)
                     ))

# save to file
write_csv(courses, file.path(path_data, 'course_features_subset.csv'))
