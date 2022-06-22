# library(psych)
# library(ggplot2)
# Customized so that even if the installation is not present, the script will run: ####
if(!require(psych)) install.packages('psych')
if(!require(ggplot2)) install.packages('ggplot2')

source('read_data.R')
student_background_data = get_student_background_data()

table(student_background_data$gender)/length(student_background_data$gender)
table(student_background_data$female)
describe(student_background_data$household_size_app)
table(student_background_data$ethnicity)/length(student_background_data$ethnicity)
table(student_background_data$asian)
table(student_background_data$first_language)/length(student_background_data$first_language)

student_vars = get_student_vars()
#####plot first and last term enrolled####
ggplot(data = student_vars, aes(y = reorder(first_term_desc, -first_code), fill = first_term_desc)) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = ..count..), stat = "count", size = 2.5, nudge_x = 300) +
  labs(title = "When did students enrol for the first time?",
       x = "",
       y = "") +
  theme(text = element_text(size = 16))

ggplot(data = student_vars, aes(y = reorder(last_term_desc, -last_code), fill = last_term_desc)) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = ..count..), stat = "count", size = 2.5, nudge_x = 500, nudge_y=0.2) +
  labs(title = "When did students enrol for the last time?",
       x = "",
       y = "") +
  theme(text = element_text(size = 16))

# age at enrollment
ggplot(data = student_vars, aes(x = as.integer(age_at_enrolment), fill = "red")) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = ..count..), stat = "count", nudge_y = 3000,angle=90) +
  labs(title = "How old are students when they enrol for the first time?",
       x = "",
       y = "") +
  xlim(15, 75) +
  theme(text = element_text(size = 16))
summary(student_vars$age_at_enrolment)
sd(student_vars$age_at_enrolment) #On average, students are 19.24 years old at their first enrollment (SD = 2.94).

#####descriptive data####
student_vars$age_at_enrolment_disc = as.integer(student_vars$age_at_enrolment)
table(student_vars$age_at_enrolment_disc)/length(student_vars$age_at_enrolment_disc)
table(student_background_data$household_size_app)/length(student_background_data$household_size_app)

# number of years
ggplot(data = student_vars, aes(x = round(number_of_years*4)/4, fill = "red")) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = ..count..), stat = "count", size = 1.5, nudge_y = 300) +
  labs(title = "How long are students enroled?",
       x = "number of years",
       y = "")

summary(student_vars$number_of_years)
# will not run because object first_and_last_term does not exist ####
#sd(first_and_last_term$number_of_years) #On average, students are enrolled for 2.86 years (SD = 1.29).

# transfer students
table(student_background_data$application_status)/length(student_vars$start_as_freshman)
sum(is.na(student_background_data$application_status))/length(student_background_data$application_status)
table(student_vars$start_as_freshman)
ggplot(data = student_vars, aes(x = as.integer(number_of_years), fill = start_as_freshman)) +
  geom_bar() +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  theme_minimal() +
  theme(legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=8.5)) +
  labs(title = "How long are students enroled?",
       x = "number of years",
       y = "",
       caption = "One bar corresponds to one term")

# enrollment data split by cohort
terms = unique(student_vars$first_term_desc)
fall_terms = terms[str_detect(terms, 'Fall')]
for (term in fall_terms[order(fall_terms)]) {
  cohort <- student_vars[student_vars$first_term_desc == term,]
  print(ggplot(data = cohort, aes(x = number_of_years, fill = start_as_freshman)) +
    geom_bar() +
    theme_minimal() +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
    theme(legend.key.size = unit(0.3, 'cm'),
          legend.title = element_text(size=8.5)) +
    labs(title = "How long are students enroled?",
         x = "number of years",
         y = "",
         subtitle = paste("Cohort started in", term),
         caption = "One bar corresponds to one term"))
}

# TODO: select cohorts in focus
cohorts <- filter(student_vars, first_term_desc == "Fall 2010" | first_term_desc == "Fall 2011" | first_term_desc == "Fall 2012" | first_term_desc == "Fall 2013" | first_term_desc == "Fall 2014")
ggplot(data = cohorts, aes(x = number_of_years, fill = transfer_student)) +
  geom_bar() +
  theme_minimal() +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  theme(legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=8.5)) +
  labs(title = "How long are students enroled?",
       subtitle = "sorted by start cohorts",
       x = "number of years",
       y = "",
       caption = "One bar corresponds to one term") +
  facet_wrap(~ first_term_enroled)

# dropout
summary(student_vars$dropout)
