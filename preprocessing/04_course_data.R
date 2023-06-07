# set wd to parent directory

# read datasets
source('read_data.R')
students = get_student_sub()
courses = get_course_data(ids = students$mellon_id)

# courses with final grade CR are usually copies, W is withdrawn
courses2 = courses %>% filter(final_grade != "CR" &
                               final_grade != "W")

# find those courses where CR is the only final_grade (usually only one)
# by course title
courses_only_CR_by_title = courses %>%
  filter(!is.na(course_title)) %>% 
  group_by(mellon_id, term_code, course_title) %>% 
  summarize(only_CR = all(final_grade == "CR")) %>% 
  filter(only_CR) %>% 
  ungroup() %>% 
  select(mellon_id, term_code, course_title)
courses_CR_1 = courses %>%
  inner_join(courses_only_CR_by_title)

# by course dept and num
courses_only_CR_by_code = courses %>%
  filter(!is.na(course_dept_code_and_num)) %>% 
  group_by(mellon_id, term_code, course_dept_code_and_num) %>% 
  summarize(only_CR = all(final_grade == "CR")) %>% 
  filter(only_CR) %>% 
  ungroup() %>% 
  select(mellon_id, term_code, course_dept_code_and_num)
courses_CR_2 = courses %>%
  inner_join(courses_only_CR_by_code)

# only courses that are identified by both criteria as CR only should be added
courses_CR = inner_join(courses_CR_1, courses_CR_2)

# add back this intersection to all courses
courses = bind_rows(courses2, courses_CR)

# join demographic information
courses = courses %>% 
  left_join(students %>% select(mellon_id,
                                female,
                                ethnicity_smpl,
                                first_generation))

### generate course peer compositions ###
composition_data <- courses %>%  
  # add n.o. total students and per group in term x course
  group_by(term_code, course_code) %>% 
  summarize(ttl_stu_crs = n(),
            rel_female_crs = mean(female==T, na.rm=T),
            rel_male_crs = mean(female==F, na.rm=T),
            rel_asian_crs = mean(ethnicity_smpl == "Asian / Asian American", na.rm=T),
            rel_black_crs = mean(ethnicity_smpl == "Black", na.rm=T),
            rel_hispanic_crs = mean(ethnicity_smpl == "Hispanic", na.rm=T),
            rel_indigenous_crs = mean(ethnicity_smpl == "Indigenous", na.rm=T),
            rel_white_crs = mean(ethnicity_smpl == "White non-Hispanic", na.rm=T),
            rel_first_gen_crs = mean(first_generation==T, na.rm=T),
            rel_non_first_gen_crs = mean(first_generation==F, na.rm=T))
# print(composition_data, n=100)

# join composition data to courses features
courses <- courses %>% 
  left_join(composition_data) %>% 
  # make own group explicit
  mutate(
    rel_owngen_crs=if_else(female==T,
                           rel_female_crs,
                           rel_male_crs),
    rel_ownethnicity_crs = case_when(
      ethnicity_smpl == "Asian / Asian American" ~ rel_asian_crs,
      ethnicity_smpl == "Black" ~ rel_black_crs,
      ethnicity_smpl == "Hispanic" ~ rel_hispanic_crs,
      ethnicity_smpl == "Indigenous" ~ rel_indigenous_crs,
      ethnicity_smpl == "White non-Hispanic" ~ rel_white_crs,
      TRUE ~ as.numeric(NA)),
    rel_ownfirstgen_crs=ifelse(first_generation==T,
                                rel_first_gen_crs,
                                rel_non_first_gen_crs)
    )
#View(courses %>% select(starts_with("ttl"),starts_with("rel")))

# grades, see https://www.reg.uci.edu/services/transcripts/notations.html
courses = courses %>% 
  mutate(final_grade_num = recode(final_grade,
                                  "A+"=4,"A"=4,"A-"=3.7,
                                  "B+"=3.3,"B"=3,"B-"=2.7,
                                  "C+"=2.3,"C"=2,"C-"=1.7,
                                  "D+"=1.3,"D"=1,"D-"=0.7,
                                  "H"=-1, "F"=0, "I"=-1, "NP"=0)) %>% 
  mutate(final_grade_num = ifelse(final_grade_num == -1, NA, final_grade_num),
         passed = final_grade_num > 0 | final_grade == "P")
#courses2 = courses %>% arrange(mellon_id, term_code, course_code) %>% select(mellon_id, term_code, course_code, course_dept_code_and_num, final_grade, final_grade_num, units_completed, passed)

courses = courses %>% select(mellon_id,
                             term_code, 
                             course_code,
                             dept_name_abbrev,
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
write_csv(courses, file.path(path_data, 'courses_subset.csv'))
