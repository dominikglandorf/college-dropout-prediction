mellon_ids = unique(term_data$mellon_id)
sample_ids = sample(mellon_ids, size=10)

for (id in sample_ids) {
  print(term_data[term_data$mellon_id == id,"term_desc"])
  print(first_and_last_term[first_and_last_term$mellon_id == id,c("first_term_enroled","last_term_enroled","number_of_years")])
  print(enrolment_data[enrolment_data$mellon_id == id,c("birth_year","birth_month","age_at_enrolment")])
}
