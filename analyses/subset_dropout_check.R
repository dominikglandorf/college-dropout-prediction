
names(bg)[names(bg) == 'last_term'] <- 'last_term_bg'
bg2 = bg %>% left_join(term_summary)
bg3 = bg2[bg2$first_year < 2017,]


student_vars$mellon_group = round(student_vars$mellon_id/10000)*10000
freshman_rate = aggregate(start_as_freshman ~ mellon_group, student_vars[student_vars$mellon_id<500000,], FUN=mean)
plot(freshman_rate)

hist(student_vars$mellon_id[student_vars$first_year<2017])

term_data2 = term_data[(term_data$mellon_id > 375000) & (term_data$mellon_id < 385000),]
term_data2 = term_data2[order(term_data2$mellon_id),]
bgd = bg[(bg$mellon_id > 375000) & (bg$mellon_id < 385000),]

bg %>% select(mellon_id, )

# netpayer fee often just 0
# pascd is number for application status
# levelcd is year_study code
# active student means if student is still enrolled 
# full time is only meaningful for active students
# major count often NA
bg_infos = bg %>% select(mellon_id, cohort, application_status, application_term_code, pascd, admitdate, entry_units_completed_transfer, grad_major_1, grad_major_2, grad_major_3, graduated_term, start_as_freshman)
term_infos = term_data %>% select(mellon_id, term_code, levelcd, registration_status, total_terms_enrolled_excluding_s, major_school_name_1, major_school_name_2, major_graduated_1, major_graduated_2)
merged = courses %>% left_join(term_infos) %>% left_join(bg_infos) %>% left_join(student_vars %>% select(mellon_id, dropout))
merged = merged %>% arrange(mellon_id, term_code, levelcd)
merged2 = merged %>% filter(mellon_id < 395000) %>% filter(mellon_id > 375000) %>% 
  select(mellon_id, start_as_freshman, dropout, levelcd, application_status, admitdate, graduated_term,major_graduated_1, major_graduated_2,term_code, major_school_name_1, major_school_name_2,course_code, course_title, dept_name_abbrev, units_completed, final_grade)
merged3 = merged2 %>% filter(start_as_freshman) %>% select(-start_as_freshman,-application_status)
