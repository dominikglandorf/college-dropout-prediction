#install.packages("gridExtra")
require(gridExtra)

source('read_data.R')
bg = get_student_background_data()
students = get_student_vars() %>%
  left_join(bg %>% select(mellon_id, student_sid, graduated_term)) %>% 
  mutate(id_range = round(as.integer(student_sid)/100000))

admit_year = as.integer(substr(students$admitdate, 2,3))
filter_admit = str_detect(students$admitdate, "F08") | 
  admit_year >= 9 &
  admit_year < 50
filter_admit[is.na(filter_admit)] = F

filter_fall_cohorts = students$first_term == 92 &
  students$first_year < 2017 &
  students$start_as_freshman
filter_fall_cohorts[is.na(filter_fall_cohorts)] = F

students = students[filter_admit & filter_fall_cohorts,]

dropout_rates = do.call(data.frame, aggregate(dropout ~ id_range, students, FUN = function(x) c(mean = mean(x), n = length(x))))

plot1 = ggplot(data = subset(dropout_rates, dropout.n>5), aes(y=id_range, x=dropout.mean)) +
  geom_point(aes(size = dropout.n), stat="identity") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropout rate by student sid",
       x = "",
       y = "First three digits of student sid") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(breaks = seq(134, 145, len = 12))
plot2 <- ggplot(students, aes(x=admitdate, y=id_range)) + 
  geom_boxplot() + 
  labs(title="Distribution of student sid",
       y="First three digits of student sid") +
  scale_y_continuous(breaks = seq(134, 145, len = 12))
grid.arrange(plot2, plot1, ncol=2)

num_years = do.call(data.frame, aggregate(number_of_years ~ id_range, students, FUN = function(x) c(mean = mean(x), n = length(x))))
plot3 = ggplot(data = subset(num_years, number_of_years.n>10), aes(x=id_range, y=number_of_years.mean)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Number of years by student sid",
       x = "",
       y = "") +coord_flip()+
  scale_x_continuous(breaks = seq(132, 147, len = 16))

grid.arrange(plot1, plot3, ncol=2)

dropout_rates_admission = do.call(data.frame, aggregate(dropout ~ admitdate, students, FUN = function(x) c(mean = mean(x), n = length(x))))

ggplot(students %>% filter(id_range>140 & id_range < 141.5), aes(x=admitdate)) +
  geom_bar()

dropout_rates = do.call(data.frame, aggregate(dropout ~ id_range, students %>% filter(admitdate=="F16"), FUN = function(x) c(mean = mean(x), n = length(x))))

term_data = get_term_data(ids=students$mellon_id)
courses = get_course_data(ids=students$mellon_id)

bg_infos = bg %>% select(student_sid, mellon_id, cohort, application_status, application_term_code, pascd, admitdate, entry_units_completed_transfer, grad_major_1, grad_major_2, grad_major_3, graduated_term)
term_infos = term_data %>% select(mellon_id, term_code, levelcd, registration_status, total_terms_enrolled_excluding_s, major_school_name_1, major_school_name_2, major_graduated_1, major_graduated_2)
merged = courses %>% left_join(term_infos) %>% left_join(bg_infos) %>% left_join(students %>% select(mellon_id, dropout))

merged = merged %>% arrange(student_sid, term_code, course_code)

merged2 = merged %>% filter(student_sid > 14000000 & student_sid < 14200000) %>% 
  select(mellon_id, admitdate, dropout, levelcd, term_code, graduated_term,final_grade, course_title, course_code,dept_name_abbrev, units_completed, ,major_graduated_1, major_graduated_2, major_school_name_1, major_school_name_2)


dropout_rates = do.call(data.frame, aggregate(sport_at_admission=="SP" ~ id_range, students, FUN = function(x) c(mean = mean(x), n = length(x))))
mean(is.na(students$int_student))
do.call(data.frame, aggregate(int_student ~ id_range, students, FUN = function(x) c(mean = mean(x), n = length(x))))

do.call(data.frame, aggregate(number_of_years ~ id_range, students, FUN = function(x) c(mean = mean(x), n = length(x))))

do.call(data.frame, aggregate(!is.na(graduated_term)==graduated ~ id_range, students, FUN = function(x) c(mean = mean(x), n = length(x))))

mean(!is.na(students$graduated_term)==students$graduated)
mean(is.na(students$graduated_term)==students$dropout, na.rm=T)

mean(is.na(students$dropout+1))
grad_info = students$dropout+1
grad_info[is.na(grad_info)] = 4
plot(rank(students$mellon_id), rank(students$student_sid), cex = 0.1, pch=20, col=grad_info)
legend("topright", legend = c("Graduation", "Dropout", "Uncertain"), col = c(1,2,4), pch = 20, bty = "n")

years=unique(students$first_year)
years = years[order(years)]
admitcol= match(students$first_year, years)
plot(rank(students$mellon_id), rank(students$student_sid), cex = 0.1, pch=20, col=admitcol)
legend("left", legend = years, col = 1:16, pch = 20, bty = "n")
