source('read_data.R')
students = get_student_sub()
courses = get_course_data(ids = students$mellon_id)

# select columns of interest
courses2 = courses %>%
  select(mellon_id, term_code, course_title, course_dept_code_and_num, final_grade, units_completed,course_code, course_type)

# manual inspection
View(courses2 %>% 
       arrange(mellon_id, term_code, course_dept_code_and_num))

nrow(courses2 %>% filter(final_grade == "CR")) /nrow(courses2)
courses %>% filter(final_grade!="CR") %>% summarize(mean_na = mean(is.na(course_type)))
# see if units are always 0 for final_grade=CR
mean(is.na(courses2 %>% filter(final_grade == "CR") %>% select(units_completed)))
# if final grade==CR, then always units_completed==0

# check if there is always another course with credits for any final_grade=CR
# this hack matches 25% of the ungraded courses to the grades
courses2 = courses2 %>% mutate(course_title = str_replace(course_title, " LEC", ""))

nrow(courses %>% filter((is.na(course_title) & final_grade=="CR"))) / nrow(courses %>% filter(final_grade == "CR"))
nrow(courses %>% filter((is.na(course_dept_code_and_num) & final_grade=="CR"))) / nrow(courses %>% filter(final_grade == "CR"))
# this exclusion fixes another 20%
courses2 = courses2 %>% filter(!(is.na(course_title) & final_grade=="CR"))

courses2 %>%
  group_by(mellon_id, term_code, course_title) %>% 
  summarize(n = n(),
            not_only_CR = any(final_grade != "CR"),
            no_final_grade = all(is.na(final_grade))) %>% 
  ungroup() %>% 
  summarize(mean_entries = mean(n),
            mean_not_only_CR = mean(not_only_CR, na.rm=T),
            mean_no_final_grade = mean(no_final_grade))
# in 0.9% of the cases a course with a CR final grade is the only existing

# inspect courses where CR is the only final grade
courses_only_CR = courses2 %>%
  group_by(mellon_id, term_code, course_title) %>% 
  summarize(only_CR = all(final_grade == "CR")) %>%
  filter(only_CR) %>% ungroup()
nrow(courses_only_CR) / nrow(courses2 %>% filter(final_grade=="CR"))

# also matching dept code and num helps for around 25% of cases
courses_only_CR_by_dept_code_num = courses2 %>%
  group_by(mellon_id, term_code, course_dept_code_and_num) %>% 
  summarize(only_CR = all(final_grade == "CR")) %>%
  filter(only_CR) %>% ungroup()
nrow(courses_only_CR_by_dept_code_num) / nrow(courses2 %>% filter(final_grade=="CR"))

still_dont_know = courses2 %>%
  inner_join(courses_only_CR %>%
               select(mellon_id, term_code, course_title)
             ) %>%
  inner_join(courses_only_CR_by_dept_code_num %>%
               select(mellon_id, term_code, course_dept_code_and_num)
             )
nrow(still_dont_know) / nrow(courses2 %>% filter(final_grade=="CR"))
mean(is.na(still_dont_know$course_type))
table(still_dont_know$course_type)
mean(is.na(courses$course_type))

nrow(courses2)
courses3 = courses2 %>% 
  inner_join(courses_only_CR %>% select(mellon_id, term_code) %>% distinct()) %>%
  inner_join(courses_only_CR_by_dept_code_num %>% select(mellon_id, term_code) %>% distinct())
  
mean(is.na(courses3$course_type))
View(courses3 %>% arrange(mellon_id, term_code, course_code))

View(courses %>% filter(course_type=="ACT"))
