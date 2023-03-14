# set wd to source file directory
source('read_data.R')

students = get_student_sub()
terms = get_term_data(ids = students$mellon_id)
courses = get_course_data(ids = students$mellon_id)

courses$index = paste(courses$mellon_id, courses$term_code, courses$course_code, "_")
courses$index = as.factor(courses$index)
  

# course was taken in school of major 1
df_dept_schools = data.frame(dept=names(dept_schools), school=dept_schools)
courses_schools = merge(courses,
                        df_dept_schools,
                        by.x="dept_name_abbrev",
                        by.y="dept",
                        all.x=T)
courses_schools = merge(courses_schools,
                        terms[,c('mellon_id','term_code',
                                 'major_school_name_abbrev_1',
                                 'major_school_name_abbrev_2',
                                 'major_school_name_abbrev_3',
                                 'major_school_name_abbrev_4')],
                        by=c("mellon_id","term_code"),
                        all.x=T)

courses_schools$in_school = courses_schools$school == courses_schools$major_school_name_abbrev_1 | (!is.na(courses_schools$major_school_name_abbrev_2) & courses_schools$school == courses_schools$major_school_name_abbrev_2) | (!is.na(courses_schools$major_school_name_abbrev_3) & courses_schools$school == courses_schools$major_school_name_abbrev_3)| (!is.na(courses_schools$major_school_name_abbrev_4) & courses_schools$school == courses_schools$major_school_name_abbrev_4)

course_features = merge(courses,
                courses_schools[,c('index','in_school')],
                by='index')

# generate course peer composition: gender

# get gender information from student background and merge
student_background <- get_student_background_data()
merged_data <- left_join(course_features, 
                         student_background %>% select(mellon_id, female),
                         by = "mellon_id")
#colnames(student_background)

# add counts for students total, own gender and frequency own gender
merged_data <- merged_data %>%  
  group_by(term_code, course_code) %>% 
  add_tally(name = "ttl_stu_crs") %>% # n.o. students in course
  ungroup() %>% 
  group_by(term_code, course_code, female) %>% 
  add_tally(name = "ttl_owngen_crs") %>% # n.o. students of own gender in course
  ungroup() %>% 
  mutate(rel_owngen_crs = ttl_owngen_crs/ttl_stu_crs) # frequency of own gender in course

# check  
# print(merged_data[, c("term_code",
#                       "course_code",
#                       "mellon_id",
#                       "female",
#                       "ttl_stu_crs",
#                       "ttl_owngen_crs",
#                       "rel_owngen_crs")],
#       n=150)

# calculate n.o. students per gender in term x course
composition_data <- merged_data %>% 
  group_by(term_code,
           course_code,
           female) %>% 
  summarise(count_gender = n()) %>% 
  ungroup()

# create data frame for merging
temp <- data.frame(mellon_id = merged_data$mellon_id,
                   term_code = merged_data$term_code,
                   course_code = merged_data$course_code,
                   ttl_stu_crs = merged_data$ttl_stu_crs,
                   ttl_owngen_crs = merged_data$ttl_owngen_crs, 
                   rel_owngen_crs = merged_data$rel_owngen_crs,
                   female = rep("yes",
                                length.out = length(merged_data$term_code))
)

# join with composition table, create var for total n.o. females, prepare for next step
temp <- left_join(temp,
                  composition_data) %>% 
  rename(ttl_female_crs = count_gender) %>%
  mutate(female = replace(female,
                          female=="yes",
                          "no")) 

# join again, now create var for total n.o. males, calculate gender frequencies
temp <- left_join(temp,
                  composition_data) %>% 
  rename(ttl_male_crs = count_gender) %>%
  mutate(rel_female_crs = ttl_female_crs/ttl_stu_crs) %>%
  mutate(rel_male_crs = ttl_male_crs/ttl_stu_crs) %>% 
  select(-female) 

# join with course features
course_features <- left_join(course_features,
                             temp)

#check
# head(course_features[,
#                      c("term_code",
#                        "course_code",
#                        "mellon_id",
#                        "ttl_stu_crs",
#                        "ttl_owngen_crs",
#                        "ttl_female_crs",
#                        "ttl_male_crs", 
#                        "rel_female_crs", 
#                        "rel_male_crs")],
#      n=100)

# To do: add first_generation and ethnicity composition. More?


# save to file
write_csv(course_features, file.path(path_data, 'course_features_subset.csv'))
