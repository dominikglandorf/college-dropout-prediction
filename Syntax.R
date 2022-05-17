####Pakete laden####
library(readr)
library(tidyverse)
library(psych)
library(stringr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
# configuration
source("config.R")
setwd(path_wd)
# read data
student_background_data <- read_csv("cleaned_student_background_data(20210622).csv")
student_term_course_section <- read_csv("cleaned_student_term_course_section(20210817).csv")
term_data <- read_csv("cleaned_student_term_data(20210809).csv")

#####deskriptive Daten####
table(student_background_data$gender)/length(student_background_data$gender)
table(student_background_data$female)
describe(student_background_data$household_size_app)
table(student_background_data$ethnicity)/length(student_background_data$ethnicity)
table(student_background_data$asian)
table(student_background_data$first_language)/length(student_background_data$first_language)

#####Variablenkodierung: first_term_enroled, last_term_enroled####
table(term_data[,c("term_part_code", "term_part_desc")])
term_part_codes = data.frame(code=c("03","14","25","51","92"),
                             desc=c("Winter","Spring","Summer","Summer","Fall"),
                             start_month=c(1, 4, 7, 7, 9),
                             end_month=c(3, 6, 8, 8, 12))

first_and_last_term = group_by(term_data, mellon_id) %>% summarise(first_code = min(term_code), last_code = max(term_code))
first_and_last_term$first_year = as.integer(substr(first_and_last_term$first_code, 0, 4))
first_and_last_term$first_term = substr(first_and_last_term$first_code, 5, 6)
first_and_last_term$first_month = term_part_codes$start_month[match(first_and_last_term$first_term, term_part_codes$code)]
first_and_last_term$first_term_enroled = paste(term_part_codes$desc[match(first_and_last_term$first_term, term_part_codes$code)], first_and_last_term$first_year)
first_and_last_term$last_year = as.integer(substr(first_and_last_term$last_code, 0, 4))
first_and_last_term$last_term = substr(first_and_last_term$last_code, 5, 6)
first_and_last_term$last_term_enroled = paste(term_part_codes$desc[match(first_and_last_term$last_term, term_part_codes$code)], first_and_last_term$last_year)
first_and_last_term$last_month = term_part_codes$end_month[match(first_and_last_term$last_term, term_part_codes$code)]

#####Grafiken erstellen####
ggplot(data = first_and_last_term, aes(y = reorder(first_term_enroled, -first_code), fill = first_term_enroled)) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = ..count..), stat = "count", size = 2.5, nudge_x = 300) +
  labs(title = "When did students enrol for the first time?",
       x = "",
       y = "")

ggplot(data = first_and_last_term, aes(y = reorder(last_term_enroled, -last_code), fill = last_term_enroled)) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none", axis.text = element_text(size = 4.3)) +
  geom_text(aes(label = ..count..), stat = "count", size = 1.5, nudge_x = 300) +
  labs(title = "When did students enrol for the last time?",
       x = "",
       y = "") +
  scale_x_continuous(limit = c(0, 8000)) # Was ist der Grund f?r diese Zeile?

#####Variablenkodierung: age at enrolment####
birth_data <- student_background_data %>% select(mellon_id, birth_year, birth_month)
enrolment_data <- merge(birth_data, first_and_last_term, by="mellon_id")
enrolment_data$age_at_enrolment = with(enrolment_data, {
  first_year - birth_year + (first_month - birth_month) / 12
})

ggplot(data = enrolment_data, aes(x = as.integer(age_at_enrolment), fill = "red")) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = ..count..), stat = "count", size = 1.4, nudge_y = 500) +
  labs(title = "How old are students when they enrol for the first time?",
       x = "",
       y = "")

#####Variablenkodierung: number of years####
first_and_last_term$number_of_years <- with(first_and_last_term, {last_year - first_year + (last_month - first_month)/12})

ggplot(data = first_and_last_term, aes(x = round(number_of_years*4)/4, fill = "red")) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = ..count..), stat = "count", size = 1.5, nudge_y = 300) +
  labs(title = "How long are students enroled?",
       x = "number of years",
       y = "")

#####deskriptive Daten####
summary(first_and_last_term$number_of_years)
sd(first_and_last_term$number_of_years) #On average, students are enrolled for 2.86 years (SD = 1.29).
summary(enrolment_data$age_at_enrolment)
sd(enrolment_data$age_at_enrolment) #On average, students are 19.24 years old at their first enrolment (SD = 2.94).

#####Variablenkodierung: transfer_student####

# Die Zahlen erscheinen bei der Codierung über entry_units_completed_transfer unrealistisch, daher über application status:
student_background_data$transfer_student <- recode_factor(student_background_data$application_status,
                                                          "Transfer" = "yes", "Freshmen" = "no", "Senior" = "no"
                                                          )
  summary(student_background_data$transfer_student)
  # sehr viele Missings, hierbei stimmt also auch was nicht, dennoch (sofern Variable korrigiert) vermutlich der richtige Weg

# student_background_data$transfer_student <- case_when(
#   student_background_data$entry_units_completed_transfer == 0 ~ "no",
#   student_background_data$entry_units_completed_transfer > 0 ~ "yes"
# )
first_and_last_term$transfer_student <- student_background_data$transfer_student
ggplot(data = first_and_last_term, aes(x = number_of_years, fill = transfer_student)) +
  geom_bar() +
  theme_minimal() +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  theme(legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=8.5)) +
  labs(title = "How long are students enroled?",
       x = "number of years",
       y = "",
       caption = "One bar corresponds to one term")

#####descriptive data####
enrolment_data$age_at_enrolment_disc = as.integer(enrolment_data$age_at_enrolment)
table(enrolment_data$age_at_enrolment_disc)/length(enrolment_data$age_at_enrolment_disc)
table(student_background_data$household_size_app)/length(student_background_data$household_size_app)

#####enrolment data splitted by cohort####
cohort_2010 <- filter(first_and_last_term, first_term_enroled == "Fall 2010")
cohort_2011 <- filter(first_and_last_term, first_term_enroled == "Fall 2011")
cohort_2012 <- filter(first_and_last_term, first_term_enroled == "Fall 2012")
cohort_2013 <- filter(first_and_last_term, first_term_enroled == "Fall 2013")
cohort_2014 <- filter(first_and_last_term, first_term_enroled == "Fall 2014")
ggplot(data = cohort_2010, aes(x = number_of_years, fill = transfer_student)) +
  geom_bar() +
  theme_minimal() +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  theme(legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=8.5)) +
  labs(title = "How long are students enroled?",
       x = "number of years",
       y = "",
       subtitle = "First term fall 2010",
       caption = "One bar corresponds to one term")
ggplot(data = cohort_2011, aes(x = number_of_years, fill = transfer_student)) +
  geom_bar() +
  theme_minimal() +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  theme(legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=8.5)) +
  labs(title = "How long are students enroled?",
       x = "number of years",
       y = "",
       subtitle = "First term fall 2011",
       caption = "One bar corresponds to one term")
ggplot(data = cohort_2012, aes(x = number_of_years, fill = transfer_student)) +
  geom_bar() +
  theme_minimal() +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  theme(legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=8.5)) +
  labs(title = "How long are students enroled?",
       x = "number of years",
       y = "",
       subtitle = "First term fall 2012",
       caption = "One bar corresponds to one term")
ggplot(data = cohort_2013, aes(x = number_of_years, fill = transfer_student)) +
  geom_bar() +
  theme_minimal() +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  theme(legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=8.5)) +
  labs(title = "How long are students enroled?",
       x = "number of years",
       y = "",
       subtitle = "First term fall 2013",
       caption = "One bar corresponds to one term")
ggplot(data = cohort_2014, aes(x = number_of_years, fill = transfer_student)) +
  geom_bar() +
  theme_minimal() +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  theme(legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=8.5)) +
  labs(title = "How long are students enroled?",
       x = "number of years",
       y = "",
       subtitle = "First term fall 2014",
       caption = "One bar corresponds to one term")
cohorts <- filter(first_and_last_term, first_term_enroled == "Fall 2010" | first_term_enroled == "Fall 2011" | first_term_enroled == "Fall 2012" | first_term_enroled == "Fall 2013" | first_term_enroled == "Fall 2014")
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

