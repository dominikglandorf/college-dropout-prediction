# set wd to parent directory

# read datasets
source('read_data.R')
students = get_student_sub()
terms = get_term_data(ids = students$mellon_id)
courses = get_course_data(ids = students$mellon_id)

# courses with final grade CR are usually copies, W is withdrawn
courses = courses %>% filter(final_grade != "CR" &
                               final_grade != "W")

# add feature in_school: course was taken in one school of majors
courses_features = courses %>%
  # add school for department of course from mapping in config.R
  mutate(school=dept_schools[dept_name_abbrev]) %>%
  # add schools of all four majors
  left_join(terms %>% select(c('mellon_id','term_code', paste0('major_school_name_abbrev_', 1:4)))) %>% 
  # in_school reflects whether the school of the course's department is one of the schools of the students major
  mutate(in_school = (school == major_school_name_abbrev_1 |
                      (!is.na(major_school_name_abbrev_2) & school == major_school_name_abbrev_2) |
                      (!is.na(major_school_name_abbrev_3) & school == major_school_name_abbrev_3) |
                      (!is.na(major_school_name_abbrev_4) & school == major_school_name_abbrev_4)
                     )) %>% 
  # drop helper variables
  select(-paste0('major_school_name_abbrev_', 1:4))

### generate course peer composition: gender ###
# join student gender
courses_features = courses_features %>% 
  left_join(students %>% select(mellon_id, female))

composition_data <- courses_features %>%  
  # add n.o. total students and per gender in term x course
  group_by(term_code, course_code) %>% 
  summarize(ttl_stu_crs = n(),
            ttl_female_crs = sum(female==T, na.rm=T),
            ttl_male_crs = sum(female==F, na.rm=T)) %>% 
  # add ratios of females and males
  mutate(rel_female_crs = ttl_female_crs/ttl_stu_crs,
         rel_male_crs = ttl_male_crs/ttl_stu_crs)
# print(composition_data, n=100)

# join composition data to courses features
courses_features <- courses_features %>% 
  left_join(composition_data) %>% 
  # make own group explicit
  mutate(
    ttl_owngen_crs=if_else(female==T, ttl_female_crs, ttl_male_crs),
    rel_owngen_crs=if_else(female==T, rel_female_crs, rel_male_crs)
    )
#print(courses_features[,c('female','ttl_stu_crs','ttl_female_crs','ttl_male_crs','rel_female_crs','rel_male_crs','ttl_owngen_crs','rel_owngen_crs')],n=150)

### generate course peer composition: ethnicity_smpl ###
# join student ethnicity
courses_features = courses_features %>% 
  left_join(students %>% select(mellon_id, ethnicity_smpl))

ASIAN="Asian / Asian American"
BLACK="Black"
HISPANIC="Hispanic"
INDIGENOUS="Indigenous"
WHITE="White non-Hispanic"
composition_data <- courses_features %>%  
  # add n.o. total students and per category in term x course
  group_by(term_code, course_code) %>% 
  summarize(ttl_stu_crs = n(),
            ttl_asian_crs = sum(ethnicity_smpl == ASIAN, na.rm=T),
            ttl_black_crs = sum(ethnicity_smpl == BLACK, na.rm=T),
            ttl_hispanic_crs = sum(ethnicity_smpl == HISPANIC, na.rm=T),
            ttl_indigenous_crs = sum(ethnicity_smpl == INDIGENOUS, na.rm=T),
            ttl_white_crs = sum(ethnicity_smpl == WHITE, na.rm=T)) %>% 
  # add ratios of ethnicities
  mutate(rel_asian_crs = ttl_asian_crs/ttl_stu_crs,
         rel_black_crs = ttl_black_crs/ttl_stu_crs,
         rel_hispanic_crs = ttl_hispanic_crs/ttl_stu_crs,
         rel_indigenous_crs = ttl_indigenous_crs/ttl_stu_crs,
         rel_white_crs = ttl_white_crs/ttl_stu_crs)
#print(composition_data, n=100)

switch_ethnicity = Vectorize(function(ethnicity, asian, black, hispanic, indigenous, white) as.numeric(switch(ethnicity,
                "Asian / Asian American"=asian,
                "Black"=black,
                "Hispanic"=hispanic,
                "Indigenous"=indigenous,
                "White non-Hispanic"=white,
                NA)))

# join composition data to courses features
courses_features <- courses_features %>% 
  left_join(composition_data) %>%
  mutate(ttl_ownethnicity_crs = switch_ethnicity(ethnicity_smpl, ttl_asian_crs, ttl_black_crs, ttl_hispanic_crs, ttl_indigenous_crs, ttl_white_crs),
         rel_ownethnicity_crs = switch_ethnicity(ethnicity_smpl, rel_asian_crs, rel_black_crs, rel_hispanic_crs, rel_indigenous_crs, rel_white_crs))
#print(courses_features2[,c('ethnicity_smpl','ttl_stu_crs','ttl_asian_crs','ttl_black_crs','ttl_hispanic_crs','ttl_indigenous_crs','ttl_white_crs','rel_asian_crs','rel_black_crs','rel_hispanic_crs','rel_indigenous_crs','rel_white_crs','ttl_ownethnicity_crs','rel_ownethnicity_crs')],n=150)


### generate course peer composition: first_generation ###
# join student first generation status
courses_features = courses_features %>% 
  left_join(students %>% select(mellon_id, first_generation))

composition_data <- courses_features %>%  
  # add n.o. total students and per category in term x course
  group_by(term_code, course_code) %>% 
  summarize(ttl_stu_crs = n(),
            ttl_first_gen_crs = sum(first_generation==T, na.rm=T),
            ttl_non_first_gen_crs = sum(first_generation==F, na.rm=T)) %>% 
  # add ratios of first and non first gen
  mutate(rel_first_gen_crs = ttl_first_gen_crs/ttl_stu_crs,
         rel_non_first_gen_crs = ttl_non_first_gen_crs/ttl_stu_crs)
#print(composition_data, n=100)

# join composition data to courses features
courses_features <- courses_features %>% 
  left_join(composition_data) %>% 
  # make own group explicit
  mutate(
    ttl_ownfirstgen_crs=if_else(first_generation==T, ttl_first_gen_crs, ttl_non_first_gen_crs),
    rel_ownfirstgen_crs=if_else(first_generation==T, rel_first_gen_crs, rel_non_first_gen_crs)
  )
#print(courses_features[,c('first_generation','ttl_stu_crs','ttl_first_gen_crs','ttl_non_first_gen_crs','rel_first_gen_crs','rel_non_first_gen_crs','ttl_ownfirstgen_crs','rel_ownfirstgen_crs')],n=150)

# grades see https://www.reg.uci.edu/services/transcripts/notations.html
courses_features = courses_features %>% 
  mutate(final_grade_num = recode(final_grade,
                                  "A+"=4,"A"=4,"A-"=3.7,
                                  "B+"=3.3,"B"=3,"B-"=2.7,
                                  "C+"=2.3,"C"=2,"C-"=1.7,
                                  "D+"=1.3,"D"=1,"D-"=0.7,
                                  "H"=-1, "F"=0, "I"=-1, "NP"=0)) %>% 
  mutate(final_grade_num = ifelse(final_grade_num == -1, NA, final_grade_num),
         passed = final_grade_num > 0 | final_grade == "P")
#courses2 = courses_features %>% arrange(mellon_id, term_code, course_code) %>% select(mellon_id, term_code, course_code, course_dept_code_and_num, final_grade, final_grade_num, units_completed, passed)

courses_features = courses_features %>% select(mellon_id,
                                               term_code, 
                                               course_code,
                                               in_school,
                                               ttl_stu_crs,
                                               rel_owngen_crs,
                                               rel_ownethnicity_crs,
                                               rel_ownfirstgen_crs,
                                               honors_course,
                                               online_course,
                                               final_grade_num,
                                               passed,
                                               units_completed)

# save to file
write_csv(courses_features, file.path(path_data, 'course_features_subset.csv'))
