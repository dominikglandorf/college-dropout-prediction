# this file is executed as part of pipeline.R to create a single csv file of all three temporal levels and merge information from them

# todo: set working directory to source location
source('read_data.R')

# check if temporal restriction was set
if (!exists("until_term")) until_term = 3

## STUDENT DATA
# get student data, this data is almost fine for student-level prediction
students = get_student_sub()
# delete unsuitable predictors
students = students %>% select(-cohort,
                               -start_as_freshman,
                               -admitdate,
                               -passed_ap_abs
)

## TERM DATA
terms = get_term_features()

# merge data from background and term levels

# age at enrollment: month of first term - birth date
student_term_info = terms %>% group_by(mellon_id) %>% 
  summarise(first_code = min(term_code),
            last_code = max(term_code)) %>%
  # first four digits are the year of the term, latter two are the term
  mutate(first_year = as.integer(substr(first_code, 0, 4)),
         first_term = substr(first_code, 5, 6)) %>% 
  # join month information for terms
  left_join(term_part_codes %>% select(code, start_month),
            by=c("first_term"="code"))
students = students %>%
  left_join(student_term_info %>% select(mellon_id, first_year, start_month)) %>%
  mutate(age_at_enrolment = first_year - birth_year + (start_month - birth_month) / 12) %>% 
  select(-birth_year, -birth_month, -start_month, -first_year)

# dropout
max_time_frame = max(student_term_info$last_code) - 100 # at least one year
students = students %>%
  left_join(student_term_info %>% select(mellon_id, last_code)) %>% 
  # drop students that are still enrolled
  filter(last_code < max_time_frame) %>% 
  mutate(dropout = !graduated) %>% 
  select(-last_code, -graduated) # drop helper variables

## COURSE DATA
courses = get_course_features()

# course features depending on term info
# add feature in_school: course was taken in one school of majors
courses = courses %>%
  # add schools of all four majors
  left_join(terms %>% select(c('mellon_id','term_code', paste0('major_school_name_abbrev_', 1:4)))) %>%
  # add school for department of course from mapping in config.R
  mutate(school=dept_schools[dept_name_abbrev]) %>%
  # in_school reflects whether the school of the course's department is one of the schools of the students major
  mutate(in_school = (school == major_school_name_abbrev_1 |
                        (!is.na(major_school_name_abbrev_2) & school == major_school_name_abbrev_2) |
                        (!is.na(major_school_name_abbrev_3) & school == major_school_name_abbrev_3) |
                        (!is.na(major_school_name_abbrev_4) & school == major_school_name_abbrev_4)
  )) %>% 
  # drop helper variables
  select(-school, -paste0('major_school_name_abbrev_', 1:4))

# term level features aggregated from course level
term_course_info = courses %>%
  left_join(terms %>% select(mellon_id, term_code, term_num)) %>% 
  filter(term_num <= until_term) %>% # to speed up computation
  group_by(mellon_id, term_code) %>% 
  summarize(n_courses = n(),
            units_completed = sum(units_completed, na.rm=TRUE),
            gpa = mean(final_grade_num, na.rm=TRUE),
            passed = mean(passed, na.rm=TRUE),
            in_school = mean(in_school, na.rm=TRUE),
            ttl_stu_crs = mean(ttl_stu_crs, na.rm=TRUE),
            rel_owngen_crs = mean(rel_owngen_crs, na.rm=TRUE),
            rel_ownethnicity_crs = mean(rel_ownethnicity_crs, na.rm=TRUE),
            rel_ownfirstgen_crs = mean(rel_ownfirstgen_crs, na.rm=TRUE))

# cumulative term stats normalized by term number
terms = terms %>% 
  filter(term_num <= until_term) %>% # to speed up computation
  left_join(term_course_info) %>% 
  group_by(mellon_id) %>% 
  arrange(term_num) %>%
  mutate(cum_avg_credits = cumsum(units_completed) / term_num,
         cum_avg_n_courses = cumsum(n_courses) / term_num,
         cum_avg_gpa = cumsum(gpa) / term_num,
         cum_avg_passed = cumsum(passed) / term_num,
         cum_avg_in_school = cumsum(in_school) / term_num,
         cum_avg_ttl_stu_crs = cumsum(ttl_stu_crs) / term_num,
         cum_avg_rel_owngen_crs = cumsum(rel_owngen_crs) / term_num,
         cum_avg_rel_ownethnicity_crs = cumsum(rel_ownethnicity_crs) / term_num,
         cum_avg_rel_ownfirstgen_crs = cumsum(rel_ownfirstgen_crs) / term_num) %>% 
  ungroup()

# add relative stats to term number, major and their interaction
units_term_num = terms %>%
  group_by(term_num) %>% 
  summarize(units_term = mean(cum_avg_credits, na.rm=T))
units_major = terms %>%
  group_by(major_name_1) %>% 
  summarize(units_major = mean(cum_avg_credits, na.rm=T))
units_term_major = terms %>%
  group_by(term_num, major_name_1) %>% 
  summarize(units_term_major = mean(cum_avg_credits, na.rm=T))
# join to terms
terms = terms %>% 
  left_join(units_term_num) %>% 
  left_join(units_major) %>% 
  left_join(units_term_major) %>% 
  mutate(cum_avg_credits_rel_term = cum_avg_credits - units_term,
         cum_avg_credits_rel_major = cum_avg_credits - units_major,
         cum_avg_credits_rel_term_major = cum_avg_credits - units_term_major) %>% 
  select(-units_term, -units_major, -units_term_major)


# term data aggregation to student level

# create change scores
terms_changes = terms %>%
  group_by(mellon_id) %>% 
  arrange(term_num) %>% 
  summarise(cum_avg_credits_lin = last(cum_avg_credits)-first(cum_avg_credits))

# create score for information in first term
terms_first = terms %>%
  filter(term_num == 1)

# select course data until shorter periods
#terms_until_x = terms %>%
#  filter(term_num == x)

# filter term information up to this point
terms = terms %>% filter(term_num == until_term)

# join term data to student, right join to drop students that dropped out earlier
students = students %>%
  inner_join(terms %>% select(mellon_id,
                              year_study,
                              num_majors,
                              any_major_stem,
                              major_name_1,
                              major_school_name_1,
                              cum_avg_credits,
                              cum_avg_credits_rel_major,
                              cum_avg_n_courses,
                              cum_avg_gpa,
                              cum_avg_passed,
                              cum_avg_in_school,
                              cum_avg_ttl_stu_crs,
                              cum_avg_rel_owngen_crs,
                              cum_avg_rel_ownethnicity_crs,
                              cum_avg_rel_ownfirstgen_crs)) %>% 
  inner_join(terms_changes) %>% 
  inner_join(terms_first %>%
              select(mellon_id, year_study, major_name_1, major_school_name_1, cum_avg_credits,
                     cum_avg_credits_rel_major) %>%
              rename(first_year_study = year_study,
                     first_major = major_name_1,
                     first_school = major_school_name_1,
                     first_credits = cum_avg_credits,
                     first_credits_major = cum_avg_credits_rel_major))

students = students %>% select(-mellon_id)

# save to storage
write_csv(students, file.path(path_data, 'features_aggregated.csv'))  
