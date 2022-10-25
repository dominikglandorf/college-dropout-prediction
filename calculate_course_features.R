# set wd to source file directory
source('read_data.R')

students = get_student_sub()
terms = get_term_data(ids = students$mellon_id)
courses = get_course_data(ids = students$mellon_id)

courses$index = paste(courses$mellon_id, courses$term_code, courses$course_code, "_")
courses$index = as.factor(courses$index)
  

# course was taken in school of major 1
df_dept_schools = data.frame(dept=names(dept_schools), school=dept_schools)
courses_schools = merge(courses,
                        df_dept_schools,
                        by.x="dept_name_abbrev",
                        by.y="dept",
                        all.x=T)
courses_schools = merge(courses_schools,
                        terms[,c('mellon_id','term_code',
                                 'major_school_name_abbrev_1',
                                 'major_school_name_abbrev_2',
                                 'major_school_name_abbrev_3',
                                 'major_school_name_abbrev_4')],
                        by=c("mellon_id","term_code"),
                        all.x=T)

courses_schools$in_school = courses_schools$school == courses_schools$major_school_name_abbrev_1 | (!is.na(courses_schools$major_school_name_abbrev_2) & courses_schools$school == courses_schools$major_school_name_abbrev_2) | (!is.na(courses_schools$major_school_name_abbrev_3) & courses_schools$school == courses_schools$major_school_name_abbrev_3)| (!is.na(courses_schools$major_school_name_abbrev_4) & courses_schools$school == courses_schools$major_school_name_abbrev_4)

course_features = merge(courses,
                courses_schools[,c('index','in_school')],
                by='index')


# save to file
write_csv(course_features, file.path(path_data, 'course_features_subset.csv'))
