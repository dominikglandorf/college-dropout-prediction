# number of years enrolled
term_summary = group_by(term_data, mellon_id) %>% 
  arrange(term_code) %>%
  summarise(first_code = min(term_code),
            last_code = max(term_code),
            num_terms = n()) %>%
  # first four digits are the year of the term, latter two are the term
  mutate(first_year = as.integer(substr(first_code, 0, 4)),
         first_term = substr(first_code, 5, 6),
         last_year = as.integer(substr(last_code, 0, 4)),
         last_term = substr(last_code, 5, 6)) %>% 
  # join month information for terms
  left_join(term_part_codes %>% select(code, start_month),
            by=c("first_term"="code")) %>% 
  left_join(term_part_codes %>% select(code, end_month),
            by=c("last_term"="code")) %>% 
  mutate(number_of_years = last_year - first_year + (end_month - start_month)/12)
