# set wd to source file directory
source('read_data.R')

students = get_student_sub()
terms = get_term_data(ids = students$mellon_id)
courses = get_course_data(ids = students$mellon_id)

courses_dropout = merge(courses, students[,c("mellon_id","dropout")], by="mellon_id")

course_dropout_rates = do.call(data.frame, aggregate(dropout ~ course_dept_code_and_num, courses_dropout, FUN = function(x) c(mean = mean(x), n = length(x))))

hist(course_dropout_rates$dropout.mean, breaks=100)
course_dropout_rates[order(-course_dropout_rates$dropout.mean),]

course_dropout_rates = do.call(data.frame, aggregate(dropout ~ course_code, courses_dropout, FUN = function(x) c(mean = mean(x), n = length(x))))

hist(course_dropout_rates$dropout.mean, breaks=100)
course_dropout_rates[order(-course_dropout_rates$dropout.mean),]
