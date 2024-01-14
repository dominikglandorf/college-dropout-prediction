# This file provides convenience functions to access the original and processed datasets
if(!require(tidyverse)) install.packages('tidyverse')

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
  ap_code1_desc = col_character(),
  ap_code2_desc = col_character(),
  ap_code3_desc = col_character(),
  ap_code4_desc = col_character(),
  ap_code5_desc = col_character(),
  ap_code6_desc = col_character(),
  ap_code7_desc = col_character(),
  ap_code8_desc = col_character(),
  ap_code9_desc = col_character(),
  ap_code10_desc = col_character(),
  ap_code11_desc = col_character(),
  ap_code12_desc = col_character(),
  ap_code13_desc = col_character(),
  ap_code14_desc = col_character(),
  ap_code15_desc = col_character(),
  ap_code16_desc = col_character(),
  ap_code17_desc = col_character(),
  ap_code18_desc = col_character(),
  ap_code19_desc = col_character(),
  ap_code20_desc = col_character(),
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
  ap_year_20 = col_integer(),
  ncessch = col_character(),
  grad_major_1 = col_character(),
  grad_major_2 = col_character(),
  grad_major_3 = col_character(),
  grad_major_1_desc = col_character(),
  grad_major_2_desc = col_character(),
  grad_major_3_desc = col_character(),
)

get_student_background_data = function (ids = c()) {
  data = read_csv(file.path(path_data, file_background_data), col_types = col_types_background)
  data[data$mellon_id==335510,c("uc_read_score","uc_math_score","uc_writing_score","uc_total_score")] = NA
  if (length(ids)) { return(data[data$mellon_id %in% ids,])}
  return(data)
}

get_term_data = function (ids = c(), exclude_term_parts=summer_part_codes) {
  data = read_csv(file.path(path_data, file_term_data), col_types = cols(
    new_student=col_character(),
    enrollment_open_date=col_character(),
    enrollment_close_date=col_character(),
    instruction_start_date=col_character(),
    instruction_end_date=col_character(),
    major_code_1=col_character(),
    major_code_2=col_character(),
    major_code_3=col_character(),
    major_code_4=col_character(),
    major_cip_name_1=col_character(),
    major_cip_name_2=col_character(),
    major_cip_name_3=col_character(),
    major_cip_name_4=col_character(),
    major_cip_code_1=col_character(),
    major_cip_code_2=col_character(),
    major_cip_code_3=col_character(),
    major_cip_code_4=col_character(),
    major_cip_category_name_1=col_character(),
    major_cip_category_name_2=col_character(),
    major_cip_category_name_3=col_character(),
    major_cip_category_name_4=col_character(),
    major_cip_category_code_1=col_character(),
    major_cip_category_code_2=col_character(),
    major_cip_category_code_3=col_character(),
    major_cip_category_code_4=col_character(),
    major_cip_series_name_1=col_character(),
    major_cip_series_name_2=col_character(),
    major_cip_series_name_3=col_character(),
    major_cip_series_name_4=col_character(),
    major_cip_series_code_1=col_character(),
    major_cip_series_code_2=col_character(),
    major_cip_series_code_3=col_character(),
    major_cip_series_code_4=col_character(),
    major_cip_code_historical_1=col_character(),
    major_cip_code_historical_2=col_character(),
    major_cip_code_historical_3=col_character(),
    major_cip_code_historical_4=col_character(),
    major_graduated_1=col_character(),
    major_graduated_2=col_character(),
    major_graduated_3=col_character(),
    major_graduated_4=col_character(),
    major_name_3=col_character(),
    major_name_abbrev_3=col_character(),
    major_school_code_3=col_character(),
    major_school_name_3=col_character(),
    major_school_name_abbrev_3=col_character(),
    major_subcampus_3=col_character(),
    major_name_4=col_character(),
    major_name_abbrev_4=col_character(),
    major_school_code_4=col_character(),
    major_school_name_4=col_character(),
    major_school_name_abbrev_4=col_character(),
    major_subcampus_4=col_character(),
  ))
  data = data[!(data$term_part_code %in% exclude_term_parts),]
  if (length(ids) > 0) { return(data[data$mellon_id %in% ids,])}
  return(data)
}

get_course_data = function (ids = c(), exclude_term_parts=summer_part_codes) {
  data = read_csv(file.path(path_data, file_course_data), show_col_types = FALSE)
  data = data[!(substr(data$term_code,5,6)  %in% exclude_term_parts),]
  if (length(ids) > 0) { return(data[data$mellon_id %in% ids,])}
  return(data)
}

get_student_vars = function () read_csv(file.path(path_data, 'background.csv'), show_col_types = FALSE)

get_student_sub = function () read_csv(file.path(path_data, 'background_subset.csv'), show_col_types = FALSE)

get_term_features = function() read_csv(file.path(path_data, 'terms_subset.csv'),  col_types = cols(
  major_name_3=col_character(),
  major_name_4=col_character(),
  major_subcampus_3=col_character(),
  major_subcampus_4=col_character(),
  major_school_name_3=col_character(),
  major_school_name_4=col_character(),
  major_school_name_abbrev_3=col_character(),
  major_school_name_abbrev_4=col_character()
  ), show_col_types = FALSE)

get_course_features = function() read_csv(file.path(path_data, 'courses_subset.csv'), show_col_types = FALSE)

get_aggregated_features = function(up_to_year) read_csv(file.path(path_data, paste0('features_aggregated_year_', up_to_year, '.csv')), show_col_types = FALSE)

get_aggregated_features_by_span = function(up_to_span) read_csv(file.path(path_data, paste0('features_aggregated_max_', up_to_span, '.csv')), show_col_types = FALSE)

get_data_to_impute = function(up_to_span=3) read_csv(file.path(path_data, paste0("data_to_impute_max_", up_to_span, ".csv")), show_col_types = FALSE)

get_imputed_features = function(up_to_year=1) {
  file_names <- list.files(path_data, pattern = paste0("^features_year_", up_to_year, "_imp_nr_.*"), full.names = TRUE)
  data_list <- lapply(file_names, read_csv, show_col_types = FALSE)
  return(data_list)
}

get_imputed_features_by_span =  function(span=1) {
  file_names <- list.files(path_data, pattern = paste0("^features_max_", span, "_imp_nr_.*"), full.names = TRUE)
  data_list <- lapply(file_names, read_csv, show_col_types = FALSE)
  return(data_list)
}
