# setwd('~/EWS') # set wd to root file directory

# read data about terms
source('read_data.R')
students = get_student_sub()
terms = get_term_data(ids = students$mellon_id)

# select relevant variables
terms = terms %>% select(mellon_id,
                         term_code,
                        # demographic
                        citizenship,
                        city_residence,
                        state_residence,
                        zip_code,
                        # administrative
                        year_study,
                        esl_program,
                        sport,
                        registration_status,
                        honors,
                        gpa_cumulative,
                        gpa_term,
                        acadyr,
                        starts_with("major_name_"),
                        -starts_with("major_name_abbrev_"),
                        starts_with("major_school_name_"),
                        starts_with("major_stem_"))
# enumerate terms
terms = terms %>% group_by(mellon_id) %>%
  arrange(term_code) %>% mutate(term_num = row_number()) %>% ungroup()

# replace rare majors (less than 0.1% of terms)
major_freqs = table(terms$major_name_1)
rare_majors = major_freqs[major_freqs<nrow(terms)/1000]
terms = terms %>%
  mutate(across(starts_with("major_name_"),
                ~ case_when(. %in% names(rare_majors) ~ "OTHER",
                            TRUE ~ as.character(.))))

# summarize undeclared and unaffiliated to one category
unde_una = "UNDECL-UNAFF"
terms = terms %>%
  mutate(across(starts_with("major_name_"),
                ~ case_when(. %in% c("UNDECLARED", "UNAFFILIATED") ~ unde_una,
                            TRUE ~ as.character(.))))

# count number of majors
majors = terms %>% select(starts_with("major_name_"))
terms$num_majors = rowSums(!is.na(majors) & majors != unde_una)

# annotate STEM majors
terms = terms %>% mutate(any_major_stem = rowSums(select(., starts_with("major_stem_")), na.rm=T)>0) %>% 
  select(-starts_with("major_stem_"))

# save to file
write_csv(terms, file.path(path_data, 'terms_subset.csv'))
