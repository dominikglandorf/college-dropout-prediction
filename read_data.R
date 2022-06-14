library(tidyverse)

source("config.R")

col_types_background = cols(
  foster_care = col_character(),
  best_subject_code_1 = col_character(),
  best_subject_code_1 = col_character(), 
  best_subject_code_2 = col_character(), 
  best_subject_code_3 = col_character(),
  best_subject_score_1 = col_integer(),
  best_subject_score_2 = col_integer(),
  best_subject_score_3 = col_integer(),
  ap_essay_flag_1 = col_double(),
  ap_essay_flag_2 = col_double(),
  ap_essay_flag_3 = col_double(),
  ap_essay_flag_4 = col_double(),
  ap_essay_flag_5 = col_double(),
  ap_essay_flag_6 = col_double(),
  ap_essay_flag_7 = col_double(),
  ap_essay_flag_8 = col_double(),
  ap_essay_flag_9 = col_double(),
  ap_essay_flag_10 = col_double(),
  ap_essay_flag_11 = col_double(),
  ap_essay_flag_12 = col_double(),
  ap_essay_flag_13 = col_double(),
  ap_essay_flag_14 = col_double(),
  ap_essay_flag_15 = col_double(),
  ap_essay_flag_16 = col_double(),
  ap_essay_flag_17 = col_double(),
  ap_essay_flag_18 = col_double(),
  ap_essay_flag_19 = col_double(),
  ap_essay_flag_20 = col_double(),
  ap_code_1 = col_character(),
  ap_code_2 = col_character(),
  ap_code_3 = col_character(),
  ap_code_4 = col_character(),
  ap_code_5 = col_character(),
  ap_code_6 = col_character(),
  ap_code_7 = col_character(),
  ap_code_8 = col_character(),
  ap_code_9 = col_character(),
  ap_code_10 = col_character(),
  ap_code_11 = col_character(),
  ap_code_12 = col_character(),
  ap_code_13 = col_character(),
  ap_code_14 = col_character(),
  ap_code_15 = col_character(),
  ap_code_16 = col_character(),
  ap_code_17 = col_character(),
  ap_code_18 = col_character(),
  ap_code_19 = col_character(),
  ap_code_20 = col_character(),
  ap_score_1 = col_double(),
  ap_score_2 = col_double(),
  ap_score_3 = col_double(),
  ap_score_4 = col_double(),
  ap_score_5 = col_double(),
  ap_score_6 = col_double(),
  ap_score_7 = col_double(),
  ap_score_8 = col_double(),
  ap_score_9 = col_double(),
  ap_score_10 = col_double(),
  ap_score_11 = col_double(),
  ap_score_12 = col_double(),
  ap_score_13 = col_double(),
  ap_score_14 = col_double(),
  ap_score_15 = col_double(),
  ap_score_16 = col_double(),
  ap_score_17 = col_double(),
  ap_score_18 = col_double(),
  ap_score_19 = col_double(),
  ap_score_20 = col_double(),
  ap_year_1 = col_integer(),
  ap_year_2 = col_integer(),
  ap_year_3 = col_integer(),
  ap_year_4 = col_integer(),
  ap_year_5 = col_integer(),
  ap_year_6 = col_integer(),
  ap_year_7 = col_integer(),
  ap_year_8 = col_integer(),
  ap_year_9 = col_integer(),
  ap_year_10 = col_integer(),
  ap_year_11 = col_integer(),
  ap_year_12 = col_integer(),
  ap_year_13 = col_integer(),
  ap_year_14 = col_integer(),
  ap_year_15 = col_integer(),
  ap_year_16 = col_integer(),
  ap_year_17 = col_integer(),
  ap_year_18 = col_integer(),
  ap_year_19 = col_integer(),
  ap_year_20 = col_integer()
)

get_student_background_data = function () {
  data = read_csv(file.path(path_data, file_background_data), col_types = col_types_background)
  data$foster_care[data$foster_care=='Y'] = 1
  data$foster_care = as.integer(data$foster_care)
  return(data)
}

get_term_data = function () {
  data = read_csv(file.path(path_data, file_term_data), col_types = cols(
    new_student=col_character(),
    enrollment_open_date=col_character(),
    enrollment_close_date=col_character(),
    instruction_start_date=col_character(),
    instruction_end_date=col_character(),
    major_code_3=col_character(),
    major_code_4=col_character()
  ))
  return(data)
}

get_student_vars = function () {
  data = read_csv(file.path(path_data, 'student_vars.csv'))
  return(data)
}
