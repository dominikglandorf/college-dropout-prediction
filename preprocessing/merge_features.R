# this file is executed as part of pipeline.R to create a single csv file that can be entered into the machine learning methods

# todo: set working directory to source location
source('read_data.R')

# check if temporal restriction was set
if (!exists("until_term")) until_term = 3

## STUDENT DATA
# get student data, this data is almost fine for student-level prediction
students = get_student_sub()

# delete unsuitable student predictors (that should not be used as predictor) 
students = students %>% select(-start_as_freshman,
                               -admitdate,
                               -passed_ap_abs,
                               -last_code,
                               -first_year,
                               -first_term,
                               -graduated,
                               -number_of_years)

## TERM DATA
terms = get_term_features()
# filter term information up to this point
terms = terms %>% filter(term_num == until_term)
# join information from last term, right join to drop students that dropped out earlier
students = students %>%
  right_join(terms %>% select(mellon_id,
                             num_majors,
                             #major_1,
                             school_1,
                             cum_avg_credits,
                             cum_avg_credits.rel_term_num,
                             cum_avg_credits.rel_major))

## COURSE DATA
courses = get_course_features()

# first step: aggregate course information on term level
term_courses = courses %>%
  group_by(mellon_id, term_code) %>%
  summarize(n_courses = n(),
            in_school = mean(in_school),
            ttl_stu_crs = mean(ttl_stu_crs),
            rel_owngen_crs = mean(rel_owngen_crs),
            rel_ownethnicity_crs = mean(rel_ownethnicity_crs),
            rel_ownfirstgen_crs = mean(rel_ownfirstgen_crs)) %>%
# second step: cumulatively aggregate over terms
  arrange(term_code) %>%
  group_by(mellon_id) %>%
  mutate(term_num = row_number()) %>%
  mutate(cum_avg_n_courses = cumsum(n_courses) / term_num,
         cum_avg_in_school = cumsum(in_school) / term_num,
         cum_avg_ttl_stu_crs = cumsum(ttl_stu_crs) / term_num,
         cum_avg_rel_owngen_crs = cumsum(rel_owngen_crs) / term_num,
         cum_avg_rel_ownethnicity_crs = cumsum(rel_ownethnicity_crs) / term_num,
         cum_avg_rel_ownfirstgen_crs = cumsum(rel_ownfirstgen_crs) / term_num)

# select course data until given term
term_courses = term_courses %>%
  filter(term_num == until_term)
# join to student data
students = students %>%
  left_join(term_courses %>% select(c(mellon_id, cum_avg_n_courses, cum_avg_in_school, cum_avg_ttl_stu_crs, cum_avg_rel_owngen_crs, cum_avg_rel_ownethnicity_crs, cum_avg_rel_ownfirstgen_crs)))

# remove mellon id from students
students = students %>% select(-mellon_id)

# save to storage
write_csv(students, file.path(path_data, 'features_aggregated.csv'))  
