source('read_data.R')

student_background_data = get_student_background_data()
term_data <- get_term_data()

# first_term, last_term
student_vars = group_by(term_data, mellon_id) %>% summarise(first_code = min(term_code), last_code = max(term_code))
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
  start_as_freshman = (student_background_data$application_status == "Freshmen"))
student_vars = merge(student_vars, transfer_data, by="mellon_id")

# dropout
graduate_data = group_by(term_data, mellon_id) %>% summarise(
  graduated_1 = sum(!is.na(major_graduated_1)),
  graduated_2 = sum(!is.na(major_graduated_2)),
  graduated_3 = sum(!is.na(major_graduated_3)),
  graduated_4 = sum(!is.na(major_graduated_4)))
graduate_data$graduated = graduate_data$graduated_1 > 0 |
  graduate_data$graduated_2 > 0 |
  graduate_data$graduated_3 > 0 |
  graduate_data$graduated_4 > 0
student_vars = merge(student_vars, graduate_data[,c('mellon_id', 'graduated')], by="mellon_id")
student_vars$dropout = NA
term_filter = student_vars$last_code < 202000
student_vars$dropout[term_filter] = !student_vars$graduated[term_filter]

# save to file
write_csv(student_vars, file.path(path_data, 'student_vars.csv'))