# setwd('~/EWS') # set wd to root file directory

# read data about students
source('read_data.R')
bg = get_student_background_data()

## feature calculation ##
# start as freshman
bg$start_as_freshman = (bg$application_status == "Freshmen")

# pre-university scores
# convert SAT scores to UC scores if only available
bg = bg %>% mutate(uc_math_score_recode = ifelse(!is.na(uc_math_score),
                                                 uc_math_score,
                                                 get_uc_from_sat(sat_math_score, cohort)))

# AP exams
bg$number_ap = rowSums(!is.na(bg[,paste0('ap_score_',1:20,"")]))
bg$passed_ap_abs = rowSums((bg[,paste0('ap_score_',1:20,"")])>2, na.rm=T)
bg$passed_ap_rel = bg$passed_ap_abs / bg$number_ap
bg$best_ap = suppressWarnings(pmap_dbl(select(bg, paste0('ap_score_',1:20,"")), max, na.rm = TRUE))
bg$best_ap[bg$best_ap<0] = NA
bg$avg_ap = apply(bg[,paste0('ap_score_',1:20,"")], 1, FUN=function(x) mean(x, na.rm=T))

# cast demographic features to logical
bg$female = (bg$female == "yes")
bg$low_income = (bg$low_income == "yes")
bg$urm = (bg$urm == 1)

# create a simplified version of ethnicity where very small groups are summarized as Indigenous
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

# check possible range for test scores
bg$toefl_score[bg$toefl_score>120] = NA

# graduation data
bg$graduated = !is.na(bg$graduated_term)

# select relevant variables 
bg = bg %>% select(mellon_id,
                   # administrative
                   cohort,
                   start_as_freshman,
                   admitdate,
                   sport_at_admission,
                   graduated,
                   # scores
                   hs_gpa,
                   uc_total_score,
                   uc_read_score,
                   uc_writing_score,
                   uc_math_score_recode,
                   toefl_score,
                   ielts_score,
                   # AP
                   number_ap,
                   passed_ap_abs,
                   passed_ap_rel,
                   best_ap,
                   avg_ap,
                   # demographics
                   birth_year,
                   birth_month,
                   female,
                   citizenship_app,
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
                   cal_res_at_app,
                   geo_category)

binary = c("ell", "first_generation","foster_care","int_student","single_parent")
bg = bg %>% mutate(across(all_of(binary),  ~ as.logical(.)))

factors = c("ethnicity_smpl","sport_at_admission","cal_res_at_app","father_edu_level_code","mother_edu_level_code")
bg = bg %>% mutate(across(all_of(factors), factor))

# save to file
write_csv(bg, file.path(path_data, 'background.csv'))
