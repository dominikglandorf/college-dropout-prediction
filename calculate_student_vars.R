# set wd to source file directory
source('read_data.R')

student_background_data = get_student_background_data()
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
birth_data <- student_background_data %>% select(mellon_id, birth_year, birth_month)
student_vars <- merge(student_vars, birth_data, by="mellon_id")
student_vars$age_at_enrolment = with(student_vars, {
  first_year - birth_year + (first_month - birth_month) / 12
})

# started as freshman
transfer_data = data.frame(
  mellon_id = student_background_data$mellon_id,
  start_as_freshman = (student_background_data$application_status == "Freshmen"),
  appl_status = student_background_data$application_status)
student_vars = merge(student_vars, transfer_data, by="mellon_id")

# pre-university scores
scaled_total_scores = data.frame(act=student_background_data$act_total_score/36,
                           sat=student_background_data$sat_total_score/2400,
                           uc=student_background_data$uc_total_score/300)
num_total_scores = rowSums(!is.na(scaled_total_scores))

scaled_math_scores = data.frame(act=student_background_data$act_math_score/36,
                                sat=student_background_data$sat_math_score / 800,
                                uc=student_background_data$uc_math_score / 100)
num_math_scores = rowSums(!is.na(scaled_math_scores))

scaled_writing_scores = data.frame(act=student_background_data$act_write_score/36,
                                   sat=student_background_data$sat_writing_score / 800,
                                   uc=num_total_scores$uc_writing_score / 100)
num_writing_scores = rowSums(!is.na(scaled_writing_scores))

scores = data.frame(mellon_id=student_background_data$mellon_id,
                    total_score = rowSums(scaled_total_scores, na.rm=T) / num_total_scores,
                    math_score = rowSums(scaled_math_scores, na.rm=T) / num_math_scores,
                    writing_score = rowSums(scaled_writing_scores, na.rm=T) / num_writing_scores)
student_vars = merge(student_vars, scores, by="mellon_id")

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
student_vars$dropout_alt = student_vars$dropout
# create filter for students not enrolled in last two years
two_years_padding = student_vars$last_code < max(student_vars$last_code-200)
four_terms_padding = student_vars$last_code < max(student_vars$last_code-100)
# decide for students in filter:
# CONDITION: no graduation -> DROPOUT = TRUE
# CONDITION: graduation -> DROPOUT = FALSE
student_vars$dropout[two_years_padding] = !student_vars$graduated[two_years_padding]
student_vars$dropout_alt[four_terms_padding] = !student_vars$graduated[four_terms_padding]

# add admitdate
student_vars = merge(student_vars, student_background_data[,c("mellon_id","admitdate")], by="mellon_id") 

# save to file
write_csv(student_vars, file.path(path_data, 'student_vars.csv'))
