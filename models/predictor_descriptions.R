# Function to translate predictor names in a dataframe column to human-readable names
translate_predictor_names <- function(column_contents) {
  # Define a mapping of predictor names to their human-readable names
  predictor_mapping <- list(
    "num_terms" = "Number of enrolled terms",
    "avg_passed" = "Passed courses",
    "year_study" = "Current year of study",
    "ell" = "English language learner",
    "major_school_name_1" = "Primarily affiliated school",
    "ethnicity_smpl" = "Ethnicity",
    "avg_grade" = "College GPA",
    "citizenship_app" = "Citizenship",
    "num_schools" = "Number of school affiliations",
    "avg_in_school" = "Courses in primary school",
    "int_student" = "International student",
    "female" = "Female",
    "avg_num_students" = "Number of students in courses",
    "uc_read_score" = "Pre-entry reading score",
    "any_major_stem" = "STEM major",
    "first_year_study" = "1st term year of study",
    "cal_res_at_app" = "Residency at application",
    "avg_own_ethnicity" = "Same ethnicity in courses",
    "uc_math_score_recode" = "Pre-entry math score",
    "school_changes" = "Changes of school",
    "avg_credits" = "Number of credits",
    "avg_own_gender" = "Same gender in courses",
    "distance_from_home" = "Distance to university",
    "mother_edu_level_code" = "Mother's edu. level",
    "avg_credits_rel_major" = "Number of credits rel. to major",
    "first_generation" = "First generation",
    "uc_writing_score" = "Pre-entry writing score",
    "avg_n_courses" = "Courses per term",
    "low_income" = "Low-income family",
    "major_changes" = "Changes of major",
    "three_honors_terms" = "Honors",
    "avg_credits_lin" = "Change in credits",
    "num_majors" = "Number of declared majors",
    "geo_category" = "Geographic category",
    "hs_gpa" = "High school GPA",
    "avg_own_first_gen" = "Same first gen. in courses",
    "ap_best" = "Best AP exam",
    "father_edu_level_code" = "Father's edu. level",
    "age_at_enrolment" = "Age at enrollment",
    "first_credits"="Credits in first term",
    "first_credits_major"="First credits relative to major",
    "term_span"="Span of enrolled terms",
    "uc_total_score" = "Total pre-entry score",
    "major_name_1"="Primary major name"
  )
  
  # Translate the predictor names to human-readable names using the mapping
  for (predictor in names(predictor_mapping)) {
    match_indices <- match(column_contents, predictor, nomatch = 0)
    replace_indices <- match_indices != 0
    column_contents[replace_indices] <- predictor_mapping[[predictor]]
  }
  
  return(column_contents)
  

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
