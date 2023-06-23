source('read_data.R')
students = get_student_sub()
courses = get_course_data(ids = students$mellon_id)

table(courses$final_grade)
# does the amount of P vs NP has changed in covid?
table(courses$term_code)
covid_start = 202014
covid_end = 202103

courses %>% 
  mutate(covid_affected = term_code >= covid_start &
                          term_code < covid_end,
         pass_no_pass = final_grade %in% c("NP", "P")) %>% 
  group_by(covid_affected) %>% 
  summarize(pass_no_pass = mean(pass_no_pass)) %>% 
  ggplot(aes(x=as.factor(term_code), y=pass_no_pass)) +
  geom_bar(stat="identity")

# figure out if number of letter grades correlates with cohort
courses = get_course_data()

courses %>% 
  mutate(covid_affected = term_code >= covid_start &
           term_code < covid_end,
         pass_no_pass = final_grade %in% c("NP", "P")) %>% 
  group_by(term_code) %>% 
  summarize(pass_no_pass = mean(pass_no_pass)) %>% 
  ggplot(aes(x=as.factor(term_code), y=pass_no_pass)) +
  geom_bar(stat="identity")
