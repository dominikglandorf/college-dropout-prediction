# set wd to source file directory
source('read_data.R')

bg = get_student_background_data()
term_data <- get_term_data()

## feature calculation ##
# first_term, last_term
student_vars = group_by(term_data, mellon_id) %>% summarise(first_code = min(term_code), last_code = max(term_code), num_terms = n())
student_vars$first_year = as.integer(substr(student_vars$first_code, 0, 4))
student_vars$first_term = substr(student_vars$first_code, 5, 6)
student_vars$first_month = term_part_codes$start_month[match(student_vars$first_term, term_part_codes$code)]
student_vars$first_term_desc = paste(term_part_codes$desc[match(student_vars$first_term, term_part_codes$code)], student_vars$first_year)

student_vars$last_year = as.integer(substr(student_vars$last_code, 0, 4))
student_vars$last_term = substr(student_vars$last_code, 5, 6)
student_vars$last_term_desc = paste(term_part_codes$desc[match(student_vars$last_term, term_part_codes$code)], student_vars$last_year)
student_vars$last_month = term_part_codes$end_month[match(student_vars$last_term, term_part_codes$code)]


# number of years enrolled
student_vars$number_of_years <- with(student_vars, {last_year - first_year + (last_month - first_month)/12})

# age at enrollment
birth_data <- bg %>% select(mellon_id, birth_year, birth_month)
student_vars <- merge(student_vars, birth_data, by="mellon_id")
student_vars$age_at_enrolment = with(student_vars, {
  first_year - birth_year + (first_month - birth_month) / 12
})

# started as freshman
transfer_data = data.frame(
  mellon_id = bg$mellon_id,
  start_as_freshman = (bg$application_status == "Freshmen"),
  appl_status = bg$application_status)
student_vars = merge(student_vars, transfer_data, by="mellon_id")

# pre-university scores
scores = bg[,c("mellon_id","uc_total_score","uc_read_score","uc_writing_score")]
scores$uc_math_score = apply(bg[,c("sat_math_score","uc_math_score","admitdate")], 1, FUN=function(x) {
  if(!is.na(x[2])) return(as.integer(x[2])) # prioritize UC score
  if(is.na(x[3])) return(NA) # if admitdate is unknown, return NA
  return(get_uc_from_sat(as.integer(x[1]), as.integer(substr(x[3],2,3)) < 12))
})
student_vars = merge(student_vars, scores, by="mellon_id")

# ap classes
number_ap = rowSums(!is.na(bg[,paste0('ap_score_',1:20,"")]))
passed_ap = rowSums((bg[,paste0('ap_score_',1:20,"")])>2, na.rm=T) / number_ap
best_ap = apply(bg[,paste0('ap_score_',1:20,"")], 1, FUN=function(x) max(x, na.rm=T))
avg_ap = apply(bg[,paste0('ap_score_',1:20,"")], 1, FUN=function(x) mean(x, na.rm=T))
ap_data = data.frame(mellon_id = bg$mellon_id, number_ap=number_ap, passed_ap=passed_ap,
                     best_ap=best_ap, avg_ap=avg_ap)
student_vars = merge(student_vars, ap_data, by="mellon_id")

## target/label: dropout ##
# count non-NA in the four major_graduated columns per students
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


# copy variables from background_data
student_vars = merge(student_vars,
                     bg[,c("mellon_id",
                                                "admitdate",
                                                #"female",
                                                "int_student",
                                                "ethnicity",
                                                "first_generation",
                                                "low_income",
                                                "father_edu_level_code",
                                                "mother_edu_level_code",
                                                "ell",
                                                "single_parent",
                                                "foster_care",
                                                "household_size_app",
                                                "distance_from_home",
                                                "sport_at_admission",
                                                "cal_res_at_app",
                                                "hs_gpa",
                                                "toefl_score",
                                                "ielts_score" )],
                     by="mellon_id")
#student_vars$female = student_vars$female == "yes"
student_vars$low_income = student_vars$low_income == "yes"


# save to file
write_csv(student_vars, file.path(path_data, 'student_vars.csv'))
