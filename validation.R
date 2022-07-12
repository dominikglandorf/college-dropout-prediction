source("read_data.R")
# Added so that if package is not loaded or even installed the script will run: ####
if(!require(psych)) install.packages('psych')

# find out difference between raw and cleaned
data_bg_raw = read_csv(file.path(path_data, "..", "raw", "Student_Background_20220512.csv"))
problems() # 245 problems
dim(data_bg_raw) # 138636 x 168

data_bg_clean = read_csv(file.path(path_data, "..", "cleaned", "cleaned_student_background_data(20220512).csv"), col_types = col_types_background)
problems()
dim(data_bg_clean)  # 138636 x 176, no new rows but new variables
table(data_bg_clean$foster_care) # there is one Y

setdiff(names(data_bg_raw), names(data_bg_clean)) # "#student_sid" renamed to "student_sid" and "low_Income_desc" renamed to "low_income_desc"
setdiff(names(data_bg_clean), names(data_bg_raw)) # new variables: "female","asian","hispanic","black","white","cohort","parent_educ","mellon_yr"

# check existence of variables
data_bg = get_student_background_data()

describe(data_bg)
str(data_bg)
describe(data_bg[,c('birth_year','birth_month')])

table(data_bg$foster_care)/nrow(data_bg) # known for 66%, very few foster care students
table(data_bg$gender)/nrow(data_bg) # most stuents M or F, very few X or U
table(data_bg$cohort) # cohorts with more than 1000 students from 2004 on 
table(data_bg$mellon_yr)/nrow(data_bg) # three values

# check term data
term_data = read_csv(file.path(path_data, "cleaned_student_term_data(20220512).csv"), col_types = cols(
  new_student=col_character(),
  enrollment_open_date=col_character(),
  enrollment_close_date=col_character(),
  instruction_start_date=col_character(),
  instruction_end_date=col_character(),
  major_code_3=col_character(),
  major_code_4=col_character()
))
problems()
columns = c('mellon_id','group_a','group_b','mellon_enr_dt_a','mellon_enr_dt_b','term_code','term_desc','term_part_code','term_part_desc','enrollment_open_date','enrollment_close_date','instruction_start_date','instruction_end_date','citizenship','city_residence','zip_code','levelcd','pascd','new_student','ferpa_blocked','year_study','second_baccalaureate','dual_degree','dual_degree_active','honors_program','deans_list','concurrent_law','ucdc','absentia','rotc','reduced_ed_fee','education_abroad','education_opportunity','esl_program','sport','sport_gender','entry_level_writing','american_history_desc','american_intitutions_desc','probation_status','registration_status','veteran_status','major_code_1','major_name_1','major_name_abbrev_1','major_school_code_1','major_school_name_1','major_school_name_abbrev_1','major_subcampus_1','major_funding_1','major_stem_1','major_degree_granting_1','major_code_2','major_name_2','major_name_abbrev_2','major_school_name_2','major_school_name_abbrev_2','major_subcampus_2','major_code_3','major_name_3','major_name_abbrev_3','major_school_name_3','major_school_name_abbrev_3','major_subcampus_3','major_code_4','major_name_4','major_name_abbrev_4','major_school_name_4','major_school_name_abbrev_4','major_subcampus_4','created_dttm','household_size','state_residence','country_residence','housing_status','housing_status_desc','housing_complex_name','currently_enrolled','cumulative_term_enroll','active_student','full_time','honors','pell_eligible_flag','major_count','netpayer_fee_assessed','current_units_attempted_grade','current_units_attempted_pnp','current_units_attempted_lowerdiv','current_units_attempted_upperdiv','current_units_attempted_graduate','current_units_attempted_online','current_units_attempted_oncampus','current_units_attempted_transfer','current_units_attempted_total','current_units_completed_graded','current_units_completed_pnp','current_units_completed_lowerdiv','current_units_completed_upperdiv','current_units_completed_graduate','current_units_completed_online','current_units_completed_oncampus','current_units_completed_transfer','current_units_completed_total','cumulative_units_attempted_grade','cumulative_units_attempted_pnp','cumulative_units_attempted_lower','cumulative_units_attempted_upper','cumulative_units_attempted_gradu','cumulative_units_attempted_onlin','cumulative_units_attempted_oncam','cumulative_units_attempted_trans','cumulative_units_attempted_total','cumulative_units_completed_grade','cumulative_units_completed_pnp','cumulative_units_completed_lower','cumulative_units_completed_upper','cumulative_units_completed_gradu','cumulative_units_completed_onlin','cumulative_units_completed_oncam','cumulative_units_completed_trans','cumulative_units_completed_total','total_terms_enrolled_excluding_s','total_terms_enrolled_including_s','gpa_term','gpa_cumulative','major_graduated_1','major_school_code_2','major_funding_2','major_stem_2','major_degree_granting_2','major_graduated_2','major_school_code_3','major_funding_3','major_stem_3','major_degree_granting_3','major_graduated_3','major_school_code_4','major_funding_4','major_stem_4','major_degree_granting_4','major_graduated_4','major_cip_code_1','major_cip_code_historical_1','major_cip_name_1','major_cip_series_code_1','major_cip_series_name_1','major_cip_category_code_1','major_cip_category_name_1','major_cip_code_2','major_cip_code_historical_2','major_cip_name_2','major_cip_series_code_2','major_cip_series_name_2','major_cip_category_code_2','major_cip_category_name_2','major_cip_code_3','major_cip_code_historical_3','major_cip_name_3','major_cip_series_code_3','major_cip_series_name_3','major_cip_category_code_3','major_cip_category_name_3','major_cip_code_4','major_cip_code_historical_4','major_cip_name_4','major_cip_series_code_4','major_cip_series_name_4','major_cip_category_code_4','major_cip_category_name_4','pell_grant_eligibility','pell_grant_received','major_minor','current_ind','md5','effective_date','end_effective_date','mapping_nm','maj_tag1','maj_tag2','maj_tag3','maj_tag4','acadyr')
columns[65]
names(term_data)
table(term_data$new_student)

term_data = get_term_data()
table(term_data$major_graduated_1)
table(term_data$major_graduated_2)
table(term_data$major_graduated_3)
table(term_data$major_graduated_4)
mellon_ids = unique(term_data$mellon_id)
sample_ids = sample(mellon_ids, size=50)
for (id in sample_ids) {
  print(id)
  print(term_data[term_data$mellon_id==id,c('term_desc','major_graduated_1','major_graduated_2'
                                            ,'major_graduated_3','major_graduated_4')])
  print(student_vars[student_vars$mellon_id==id,])
}
term_data[313,'mellon_id']
print(term_data[term_data$mellon_id=='173683',c('term_desc','major_graduated_1','major_graduated_2'
                                          ,'major_graduated_3','major_graduated_4')])
which(!is.na(term_data$major_graduated_3))

print(term_data[term_data$mellon_id==term_data$mellon_id[1079441],c('term_desc','major_graduated_1','major_graduated_2'
                                                ,'major_graduated_3','major_graduated_4')])
i=0
j = 0
for (id in mellon_ids) {
  term_codes = term_data$term_code[term_data$mellon_id==id]
  maximum_diff = max(diff(term_codes[order(term_codes)]))
  if (maximum_diff > 200) {
    i = i+1
    print(paste(id, ":", maximum_diff))
  }
  j = j+1
  if (j%%1000 == 0) {
    print(j)
  }    
}
print(i) # 2064
print(term_data[term_data$mellon_id=='178376',c('term_desc','major_graduated_1','major_graduated_2'
                                          ,'major_graduated_3','major_graduated_4')])

sum(!student_vars$graduated) == sum(student_vars$dropout, na.rm=T) + sum((student_vars$last_code >= max(student_vars$last_code)-200))
sum(is.na(student_vars$dropout) | !student_vars$dropout) == sum(student_vars$graduated) + sum(student_vars$last_code > (max(student_vars$last_code)-200))

# admit date
sum(is.na(student_vars$admitdate))
admitdates = student_vars$admitdate
admitdates[is.na(admitdates)] = "NA"
table(admitdates) / length(admitdates)

tab_dates = table(paste(admitdates, student_vars$first_term_desc))
tab_dates[order(-tab_dates)]

# Code put here and commented out because not executable without prior operations (and data loading): ####
# check if variable calculation has worked out
# mellon_ids = unique(term_data$mellon_id)
# sample_ids = sample(mellon_ids, size=10)
# 
# for (id in sample_ids) {
#   print(term_data[term_data$mellon_id == id,"term_desc"])
#   print(first_and_last_term[first_and_last_term$mellon_id == id,c("first_term_enroled","last_term_enroled","number_of_years")])
#   print(enrolment_data[enrolment_data$mellon_id == id,c("birth_year","birth_month","age_at_enrolment")])
# }

student_sub = get_student_sub()
table(student_vars$start_as_freshman)
sum(is.na(student_vars$start_as_freshman))

student_sub = get_student_sub()
dim(student_sub)

# validate pre-university scores
names(student_background_data)

     
