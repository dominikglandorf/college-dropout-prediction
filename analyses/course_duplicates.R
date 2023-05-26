source('read_data.R')
students = get_student_sub()
courses = get_course_data(ids = students$mellon_id)

# select columns of interest
courses2 = courses %>%
  select(mellon_id, term_code, course_title, course_dept_code_and_num, final_grade, units_completed,course_code)

# manual inspection
View(courses2 %>% 
       arrange(mellon_id, term_code, course_dept_code_and_num))

# see if units are always 0 for final_grade=CR
mean(is.na(courses2 %>% filter(final_grade == "CR") %>% select(units_completed)))
# if CR is the final grade there is never a number of units completed

# check if there is always another course with credits for any final_grade=CR
# this hack fixes 25% of the ungraded courses
courses2 = courses2 %>% mutate(course_title = str_replace(course_title, " LEC", ""))
# this exclusion fixes another 20%
courses2 = courses2 %>% filter(!(is.na(course_title)&is.na(units_completed)&final_grade=="CR"))
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
  summarize(n = n(),
            not_only_CR = any(final_grade != "CR"),
            no_final_grade = all(is.na(final_grade)),
            units_completed = sum(units_completed)) %>%
  filter(!not_only_CR) %>% ungroup()

# also matching dept code and num helps for around only 10% of cases
courses_only_CR_by_dept_code_num = courses2 %>%
  group_by(mellon_id, term_code, course_dept_code_and_num) %>% 
  summarize(n = n(),
            not_only_CR = any(final_grade != "CR"),
            no_final_grade = all(is.na(final_grade)),
            units_completed = sum(units_completed)) %>%
  filter(!not_only_CR) %>% ungroup()


courses3 = courses2 %>%
  inner_join(courses_only_CR %>% select(mellon_id, term_code)) %>%
  inner_join(courses_only_CR_by_dept_code_num %>% select(mellon_id, term_code))

View(courses3 %>% arrange(mellon_id, term_code, course_title))
