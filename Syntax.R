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
table(student_background_data$gender)
table(student_background_data$female)
describe(student_background_data$household_size_app)
table(student_background_data$ethnicity)
table(student_background_data$asian)
table(student_background_data$first_language)

#####Variablenkodierung: first_term_enroled, last_term_enroled####
table(term_data[,c("term_part_code", "term_part_desc")])
term_part_codes = data.frame(code=c("03","14","25","51","92"),
                             desc=c("Winter","Spring","Summer","Summer","Fall"),
                             month=c(1, 3, 6, 6, 9))

first_and_last_term = group_by(term_data, mellon_id) %>% summarise(first_code = min(term_code), last_code = max(term_code))
first_and_last_term$first_year = as.integer(substr(first_and_last_term$first_code, 0, 4))
first_and_last_term$first_term = substr(first_and_last_term$first_code, 5, 6)
first_and_last_term$first_month = term_part_codes$month[match(first_and_last_term$first_term, term_part_codes$code)]
first_and_last_term$first_term_enroled = paste(term_part_codes$desc[match(first_and_last_term$first_term, term_part_codes$code)], first_and_last_term$first_year)
first_and_last_term$last_year = as.integer(substr(first_and_last_term$last_code, 0, 4))
first_and_last_term$last_term = substr(first_and_last_term$last_code, 5, 6)
first_and_last_term$last_term_enroled = paste(term_part_codes$desc[match(first_and_last_term$last_term, term_part_codes$code)], first_and_last_term$last_year)
first_and_last_term$last_month = term_part_codes$month[match(first_and_last_term$last_term, term_part_codes$code)]

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
  scale_x_continuous(limit = c(0, 8000))

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

ggplot(data = first_and_last_term, aes(x = number_of_years, fill = "red")) +
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
