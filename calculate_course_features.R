# set wd to source file directory
source('read_data.R')

students = get_student_sub()
terms = get_term_data(ids = students$mellon_id)
courses = get_course_data(ids = students$mellon_id)
index = c("mellon_id","term_code","course_code")

# add whether course was taken in one school of majors
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

# To do: add first_generation and ethnicity composition. More?

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
  # add ratios of females and males
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


# save to file
write_csv(courses_features, file.path(path_data, 'course_features_subset.csv'))
