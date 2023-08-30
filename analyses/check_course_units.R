setwd("~/EWS")
source("read_data.R")
courses = get_course_data()

View(courses %>% select(mellon_id, term_code, course_title, units_completed) %>% arrange(mellon_id, term_code, course_title))

mean(is.na(courses$units_completed))

missings = colMeans(is.na(courses))
names(missings[missings<0.25])
