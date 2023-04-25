# this file is executed as part of pipeline.R to create a single csv file that can be entered into the machine learning methods

# todo: set working directory to source location
source('read_data.R')

# get student data, this data is fine for student-level prediction
students = get_student_sub()
terms = get_term_features()
courses = get_course_features()

# check if temporal restriction was set
if (!exists("until_term")) until_term = 3
# filter term and course information up to this point
terms = terms %>% filter(term_num == until_term)
courses = courses %>%
  left_join(terms %>% select(mellon_id, term_code, term_num)) %>% 
  filter(term_num == until_term)

# delete unsuitable predictors (that should not be used as predictor) 
students = students %>% select(-start_as_freshman,
                               -admitdate,
                               -passed_ap_abs,
                               -last_code,
                               -first_term,
                               -graduated,
                               -number_of_years)

# join term information from last term
students = students %>%
  left_join(terms %>% select(mellon_id,
                             num_majors,
                             major_1,
                             school_1,
                             cum_avg_credits,
                             cum_avg_credits.rel_term_num,
                             cum_avg_credits.rel_major))

# aggregate course information
student_course_info = courses %>%
  group_by(mellon_id) %>%
  summarize(n_courses = n())

students = students %>%
  left_join(student_course_info)

write_csv(students, file.path(path_data, 'features_aggregated.csv'))  
