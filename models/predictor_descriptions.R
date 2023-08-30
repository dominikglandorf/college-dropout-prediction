# Function to translate predictor names in a dataframe column to human-readable names
translate_predictor_names <- function(column_contents) {
  # Define a mapping of predictor names to their human-readable names
  predictor_mapping <- list(
    "num_terms" = "Num Terms",
    "avg_passed" = "Avg Passed Courses",
    "year_study" = "Curr. Year of Study",
    "ell" = "English Learner",
    "major_school_name_1" = "Primary School",
    "ethnicity_smpl" = "Ethnicity",
    "avg_grade" = "Avg Grade",
    "citizenship_app" = "Citizenship at App.",
    "num_schools" = "Num Schools",
    "avg_in_school" = "Avg Students in School",
    "int_student" = "Int. Student",
    "female" = "Female",
    "avg_num_students" = "Avg Students in Courses",
    "uc_read_score" = "Univ. Reading Score",
    "any_major_stem" = "Any Major in STEM",
    "first_year_study" = "Year of Study 1st Term",
    "cal_res_at_app" = "Residency at App.",
    "avg_own_ethnicity" = "Students Same Ethnicity",
    "uc_math_score_recode" = "Univ. Math Score",
    "school_changes" = "Change of Major/School",
    "avg_credits" = "Avg Credits",
    "avg_own_gender" = "Avg Students Same Gender",
    "distance_from_home" = "Distance to Univ.",
    "mother_edu_level_code" = "Mother's Edu. Level",
    "avg_credits_rel_major" = "Credits Relative to Major",
    "first_generation" = "First Gen. Student",
    "uc_writing_score" = "Univ. Writing Score",
    "avg_n_courses" = "Num Courses per Term",
    "low_income" = "Low Income",
    "major_changes" = "Change of Major",
    "three_honors_terms" = "Honors for 3 Terms",
    "avg_credits_lin" = "Linear Change in Credits",
    "num_majors" = "Num Declared Majors",
    "geo_category" = "Geographic Category",
    "hs_gpa" = "High School GPA",
    "avg_own_first_gen" = "Avg First Gen. Students",
    "ap_best" = "Best AP Exam Score",
    "father_edu_level_code" = "Father's Edu. Level",
    "age_at_enrolment" = "Age at Enrollment"
  )
  
  # Translate the predictor names to human-readable names using the mapping
  for (predictor in names(predictor_mapping)) {
    column_contents <- gsub(predictor, predictor_mapping[[predictor]], column_contents, ignore.case = TRUE)
  }
  
  return(column_contents)
}

# Example usage:
#column_contents <- c("num_terms", "avg_passed", "year_study", "ell", "major_school_name_1", "avg_grade")
#translated_contents <- translate_predictor_names(column_contents)
#print(translated_contents)
