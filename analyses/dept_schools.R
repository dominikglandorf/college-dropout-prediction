# set wd to source file directory
source('read_data.R')

students = get_student_sub()
terms = get_term_data(ids = students$mellon_id)
courses = get_course_data(ids = students$mellon_id)

mean(is.na(courses$course_dept_code_and_num))
table(courses$course_dept_code_and_num)
paste0("'", unique(courses$dept_name_abbrev), "' = '',", collapse="")

table(terms$major_school_name_abbrev_1)

# final results see config.example.R
