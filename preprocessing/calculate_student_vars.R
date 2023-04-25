# set wd to source file directory
source('read_data.R')

bg = get_student_background_data()

## feature calculation ##
# start as freshman
bg$start_as_freshman = (bg$application_status == "Freshmen")

# pre-university scores
bg$uc_math_score = apply(bg[,c("sat_math_score","uc_math_score","admitdate")], 1, FUN=function(x) {
  if(!is.na(x[2])) return(as.integer(x[2])) # prioritize UC score
  if(is.na(x[3])) return(NA) # if admitdate is unknown, return NA
  # translate SAT to UC score depending on whether admission was before 2012
  return(get_uc_from_sat(as.integer(x[1]), as.integer(substr(x[3],2,3)) < 12))
})

# AP exams
bg$number_ap = rowSums(!is.na(bg[,paste0('ap_score_',1:20,"")]))
bg$passed_ap_abs = rowSums((bg[,paste0('ap_score_',1:20,"")])>2, na.rm=T)
bg$passed_ap_rel = bg$passed_ap_abs / bg$number_ap
bg$best_ap = suppressWarnings(pmap_dbl(select(bg, paste0('ap_score_',1:20,"")), max, na.rm = TRUE))
bg$best_ap[bg$best_ap<0] = NA
bg$avg_ap = apply(bg[,paste0('ap_score_',1:20,"")], 1, FUN=function(x) mean(x, na.rm=T))

# cast demographic features to logical
bg$female = bg$female == "yes"
bg$low_income = bg$low_income == "yes"
bg$ethnicity_smpl = bg$ethnicity
bg$ethnicity_smpl[bg$ethnicity_smpl=="American Indian / Alaskan Native"] = "Indigenous"
bg$ethnicity_smpl[bg$ethnicity_smpl=="Pacific Islander"] = "Indigenous"

# clip very large households
bg$household_size_app[bg$household_size_app > 7] = 7
# delete very rare categories
bg$father_edu_level_code[bg$father_edu_level_code==4] = NA
bg$mother_edu_level_code[bg$mother_edu_level_code==4] = NA
# summarize rare sports categories (less than 1%)
saa = table(bg$sport_at_admission)
bg$sport_at_admission[bg$sport_at_admission %in% names(saa[saa<nrow(bg)/100])] = "other"


# select predictors
student_vars = bg %>% select(mellon_id,
                             # administrative
                             start_as_freshman,
                             admitdate,
                             sport_at_admission,
                             # scores
                             hs_gpa,
                             uc_total_score,
                             uc_read_score,
                             uc_writing_score,
                             uc_math_score,
                             toefl_score,
                             ielts_score,
                             # AP
                             number_ap,
                             passed_ap_abs,
                             passed_ap_rel,
                             best_ap,
                             avg_ap,
                             # demographics
                             female,
                             int_student,
                             ethnicity_smpl,
                             first_generation,
                             low_income,
                             father_edu_level_code,
                             mother_edu_level_code,
                             ell,
                             single_parent,
                             foster_care,
                             household_size_app,
                             distance_from_home,
                             cal_res_at_app)


# number of years enrolled
term_data <- get_term_data()
term_summary = group_by(term_data, mellon_id) %>%
  summarise(first_code = min(term_code),
            last_code = max(term_code),
            num_terms = n()) %>%
  # first four digits are the year of the term, latter two are the term
  mutate(first_year = as.integer(substr(first_code, 0, 4)),
         first_term = substr(first_code, 5, 6),
         last_year = as.integer(substr(last_code, 0, 4)),
         last_term = substr(last_code, 5, 6)) %>% 
  # join month information for terms
  left_join(term_part_codes %>% select(code, start_month),
            by=c("first_term"="code")) %>% 
  left_join(term_part_codes %>% select(code, end_month),
            by=c("last_term"="code")) %>% 
  mutate(number_of_years = last_year - first_year + (end_month - start_month)/12)
student_vars = student_vars %>%
  merge(term_summary %>% select(mellon_id, number_of_years, last_code, first_term, first_year))

# age at enrollment
birth_data = bg %>%
  select(mellon_id, birth_year, birth_month) %>% 
  merge(term_summary) %>% 
  mutate(age_at_enrolment = first_year - birth_year + (start_month - birth_month) / 12)
student_vars = student_vars %>%
  merge(birth_data %>% select(mellon_id, age_at_enrolment))


## target/label: dropout ##
# check whether there is any non-NA in the four major_graduated columns per students
graduate_data = group_by(term_data, mellon_id) %>% summarise(
  graduated_1 = sum(!is.na(major_graduated_1)),
  graduated_2 = sum(!is.na(major_graduated_2)),
  graduated_3 = sum(!is.na(major_graduated_3)),
  graduated_4 = sum(!is.na(major_graduated_4)))
# aggregate per student if any of the columns has a non-NA entry
graduate_data$graduated = graduate_data$graduated_1 > 0 |
  graduate_data$graduated_2 > 0 |
  graduate_data$graduated_3 > 0 |
  graduate_data$graduated_4 > 0
# merge graduated variable into student vars
student_vars = merge(student_vars, graduate_data[,c('mellon_id', 'graduated')], by="mellon_id")
# create dropout variable
student_vars$dropout = NA
student_vars$dropout[student_vars$graduated] = F

# create filter for students not enrolled in last two years
four_terms_padding = student_vars$last_code < max(student_vars$last_code-100)
# decide for students in filter:
# CONDITION: no graduation -> DROPOUT = TRUE
# CONDITION: graduation -> DROPOUT = FALSE
student_vars$dropout[four_terms_padding] = !student_vars$graduated[four_terms_padding]



# save to file
write_csv(student_vars, file.path(path_data, 'student_vars.csv'))
