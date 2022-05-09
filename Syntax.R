#Daten einlesen und Pakete laden####
library(readr)
library(tidyverse)
library(psych)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
setwd("C:/Users/Irion/Documents/EWS/Daten")
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
term_data$term_desc_factor <- factor(term_data$term_desc)
levels(term_data$term_desc_factor)
term_data$term_desc_numeric <- case_when(
  term_data$term_desc == "Winter 2008" ~ 1,
  term_data$term_desc == "Spring 2008" ~ 2,
  term_data$term_desc == "Summer 2008" ~ 3,
  term_data$term_desc == "Fall 2008" ~ 4,
  term_data$term_desc == "Winter 2009" ~ 5,
  term_data$term_desc == "Spring 2009" ~ 6,
  term_data$term_desc == "Summer 2009" ~ 7,
  term_data$term_desc == "Fall 2009" ~ 8,
  term_data$term_desc == "Winter 2010" ~ 9,
  term_data$term_desc == "Spring 2010" ~ 10,
  term_data$term_desc == "Summer 2010" ~ 11,
  term_data$term_desc == "Fall 2010" ~ 12,
  term_data$term_desc == "Winter 2011" ~ 13,
  term_data$term_desc == "Spring 2011" ~ 14,
  term_data$term_desc == "Summer 2011" ~ 15,
  term_data$term_desc == "Fall 2011" ~ 16,
  term_data$term_desc == "Winter 2012" ~ 17,
  term_data$term_desc == "Spring 2012" ~ 18,
  term_data$term_desc == "Summer 2012" ~ 19,
  term_data$term_desc == "Fall 2012" ~ 20,
  term_data$term_desc == "Winter 2013" ~ 21,
  term_data$term_desc == "Spring 2013" ~ 22,
  term_data$term_desc == "Summer 2013" ~ 23,
  term_data$term_desc == "Fall 2013" ~ 24,
  term_data$term_desc == "Winter 2014" ~ 25,
  term_data$term_desc == "Spring 2014" ~ 26,
  term_data$term_desc == "Summer 2014" ~ 27,
  term_data$term_desc == "Fall 2014" ~ 28,
  term_data$term_desc == "Winter 2015" ~ 29,
  term_data$term_desc == "Spring 2015" ~ 30,
  term_data$term_desc == "Summer 2015" ~ 31,
  term_data$term_desc == "Fall 2015" ~ 32,
  term_data$term_desc == "Winter 2016" ~ 33,
  term_data$term_desc == "Spring 2016" ~ 34,
  term_data$term_desc == "Summer 2016" ~ 35,
  term_data$term_desc == "Fall 2016" ~ 36,
  term_data$term_desc == "Winter 2017" ~ 37,
  term_data$term_desc == "Spring 2017" ~ 38,
  term_data$term_desc == "Summer 2017" ~ 39,
  term_data$term_desc == "Fall 2017" ~ 40,
  term_data$term_desc == "Winter 2018" ~ 41,
  term_data$term_desc == "Spring 2018" ~ 42,
  term_data$term_desc == "Summer 2018" ~ 43,
  term_data$term_desc == "Fall 2018" ~ 44,
  term_data$term_desc == "Winter 2019" ~ 45,
  term_data$term_desc == "Spring 2019" ~ 46,
  term_data$term_desc == "Summer 2019" ~ 47,
  term_data$term_desc == "Fall 2019" ~ 48,
  term_data$term_desc == "Winter 2020" ~ 49,
  term_data$term_desc == "Spring 2020" ~ 50,
  term_data$term_desc == "Summer 2020" ~ 51,
  term_data$term_desc == "Fall 2020" ~ 52,
  term_data$term_desc == "Winter 2021" ~ 53,
  term_data$term_desc == "Spring 2021" ~ 54,
  term_data$term_desc == "Summer 2021" ~ 55,
  term_data$term_desc == "Fall 2021" ~ 56
)
term_data <- term_data %>% group_by(mellon_id) %>% mutate(
  first_term_enroled_numeric = min(term_desc_numeric)
)
term_data <- term_data %>% group_by(mellon_id) %>% mutate(
  last_term_enroled_numeric = max(term_desc_numeric)
)
term_data$first_term_enroled <- case_when(
  term_data$first_term_enroled_numeric == 1 ~ "Winter 2008",
  term_data$first_term_enroled_numeric == 2 ~ "Spring 2008",
  term_data$first_term_enroled_numeric == 3 ~ "Summer 2008",
  term_data$first_term_enroled_numeric == 4 ~ "Fall 2008",
  term_data$first_term_enroled_numeric == 5 ~ "Winter 2009",
  term_data$first_term_enroled_numeric == 6 ~ "Spring 2009",
  term_data$first_term_enroled_numeric == 7 ~ "Summer 2009",
  term_data$first_term_enroled_numeric == 8 ~ "Fall 2009",
  term_data$first_term_enroled_numeric == 9 ~ "Winter 2010",
  term_data$first_term_enroled_numeric == 10 ~ "Spring 2010",
  term_data$first_term_enroled_numeric == 11 ~ "Summer 2010",
  term_data$first_term_enroled_numeric == 12 ~ "Fall 2010",
  term_data$first_term_enroled_numeric == 13 ~ "Winter 2011",
  term_data$first_term_enroled_numeric == 14 ~ "Spring 2011",
  term_data$first_term_enroled_numeric == 15 ~ "Summer 2011",
  term_data$first_term_enroled_numeric == 16 ~ "Fall 2011",
  term_data$first_term_enroled_numeric == 17 ~ "Winter 2012",
  term_data$first_term_enroled_numeric == 18 ~ "Spring 2012",
  term_data$first_term_enroled_numeric == 19 ~ "Summer 2012",
  term_data$first_term_enroled_numeric == 20 ~ "Fall 2012",
  term_data$first_term_enroled_numeric == 21 ~ "Winter 2013",
  term_data$first_term_enroled_numeric == 22 ~ "Spring 2013",
  term_data$first_term_enroled_numeric == 23 ~ "Summer 2013",
  term_data$first_term_enroled_numeric == 24 ~ "Fall 2013",
  term_data$first_term_enroled_numeric == 25 ~ "Winter 2014",
  term_data$first_term_enroled_numeric == 26 ~ "Spring 2014",
  term_data$first_term_enroled_numeric == 27 ~ "Summer 2014",
  term_data$first_term_enroled_numeric == 28 ~ "Fall 2014",
  term_data$first_term_enroled_numeric == 29 ~ "Winter 2015",
  term_data$first_term_enroled_numeric == 30 ~ "Spring 2015",
  term_data$first_term_enroled_numeric == 31 ~ "Summer 2015",
  term_data$first_term_enroled_numeric == 32 ~ "Fall 2015",
  term_data$first_term_enroled_numeric == 33 ~ "Winter 2016",
  term_data$first_term_enroled_numeric == 34 ~ "Spring 2016",
  term_data$first_term_enroled_numeric == 35 ~ "Summer 2016",
  term_data$first_term_enroled_numeric == 36 ~ "Fall 2016",
  term_data$first_term_enroled_numeric == 37 ~ "Winter 2017",
  term_data$first_term_enroled_numeric == 38 ~ "Spring 2017",
  term_data$first_term_enroled_numeric == 39 ~ "Summer 2017",
  term_data$first_term_enroled_numeric == 40 ~ "Fall 2017",
  term_data$first_term_enroled_numeric == 41 ~ "Winter 2018",
  term_data$first_term_enroled_numeric == 42 ~ "Spring 2018",
  term_data$first_term_enroled_numeric == 43 ~ "Summer 2018",
  term_data$first_term_enroled_numeric == 44 ~ "Fall 2018",
  term_data$first_term_enroled_numeric == 45 ~ "Winter 2019",
  term_data$first_term_enroled_numeric == 46 ~ "Spring 2019",
  term_data$first_term_enroled_numeric == 47 ~ "Summer 2019",
  term_data$first_term_enroled_numeric == 48 ~ "Fall 2019",
  term_data$first_term_enroled_numeric == 49 ~ "Winter 2020",
  term_data$first_term_enroled_numeric == 50 ~ "Spring 2020",
  term_data$first_term_enroled_numeric == 51 ~ "Summer 2020",
  term_data$first_term_enroled_numeric == 52 ~ "Fall 2020",
  term_data$first_term_enroled_numeric == 53 ~ "Winter 2021",
  term_data$first_term_enroled_numeric == 54 ~ "Spring 2021",
  term_data$first_term_enroled_numeric == 55 ~ "Summer 2021",
  term_data$first_term_enroled_numeric == 56 ~ "Fall 2021"
)
term_data$last_term_enroled <- case_when(
  term_data$last_term_enroled_numeric == 1 ~ "Winter 2008",
  term_data$last_term_enroled_numeric == 2 ~ "Spring 2008",
  term_data$last_term_enroled_numeric == 3 ~ "Summer 2008",
  term_data$last_term_enroled_numeric == 4 ~ "Fall 2008",
  term_data$last_term_enroled_numeric == 5 ~ "Winter 2009",
  term_data$last_term_enroled_numeric == 6 ~ "Spring 2009",
  term_data$last_term_enroled_numeric == 7 ~ "Summer 2009",
  term_data$last_term_enroled_numeric == 8 ~ "Fall 2009",
  term_data$last_term_enroled_numeric == 9 ~ "Winter 2010",
  term_data$last_term_enroled_numeric == 10 ~ "Spring 2010",
  term_data$last_term_enroled_numeric == 11 ~ "Summer 2010",
  term_data$last_term_enroled_numeric == 12 ~ "Fall 2010",
  term_data$last_term_enroled_numeric == 13 ~ "Winter 2011",
  term_data$last_term_enroled_numeric == 14 ~ "Spring 2011",
  term_data$last_term_enroled_numeric == 15 ~ "Summer 2011",
  term_data$last_term_enroled_numeric == 16 ~ "Fall 2011",
  term_data$last_term_enroled_numeric == 17 ~ "Winter 2012",
  term_data$last_term_enroled_numeric == 18 ~ "Spring 2012",
  term_data$last_term_enroled_numeric == 19 ~ "Summer 2012",
  term_data$last_term_enroled_numeric == 20 ~ "Fall 2012",
  term_data$last_term_enroled_numeric == 21 ~ "Winter 2013",
  term_data$last_term_enroled_numeric == 22 ~ "Spring 2013",
  term_data$last_term_enroled_numeric == 23 ~ "Summer 2013",
  term_data$last_term_enroled_numeric == 24 ~ "Fall 2013",
  term_data$last_term_enroled_numeric == 25 ~ "Winter 2014",
  term_data$last_term_enroled_numeric == 26 ~ "Spring 2014",
  term_data$last_term_enroled_numeric == 27 ~ "Summer 2014",
  term_data$last_term_enroled_numeric == 28 ~ "Fall 2014",
  term_data$last_term_enroled_numeric == 29 ~ "Winter 2015",
  term_data$last_term_enroled_numeric == 30 ~ "Spring 2015",
  term_data$last_term_enroled_numeric == 31 ~ "Summer 2015",
  term_data$last_term_enroled_numeric == 32 ~ "Fall 2015",
  term_data$last_term_enroled_numeric == 33 ~ "Winter 2016",
  term_data$last_term_enroled_numeric == 34 ~ "Spring 2016",
  term_data$last_term_enroled_numeric == 35 ~ "Summer 2016",
  term_data$last_term_enroled_numeric == 36 ~ "Fall 2016",
  term_data$last_term_enroled_numeric == 37 ~ "Winter 2017",
  term_data$last_term_enroled_numeric == 38 ~ "Spring 2017",
  term_data$last_term_enroled_numeric == 39 ~ "Summer 2017",
  term_data$last_term_enroled_numeric == 40 ~ "Fall 2017",
  term_data$last_term_enroled_numeric == 41 ~ "Winter 2018",
  term_data$last_term_enroled_numeric == 42 ~ "Spring 2018",
  term_data$last_term_enroled_numeric == 43 ~ "Summer 2018",
  term_data$last_term_enroled_numeric == 44 ~ "Fall 2018",
  term_data$last_term_enroled_numeric == 45 ~ "Winter 2019",
  term_data$last_term_enroled_numeric == 46 ~ "Spring 2019",
  term_data$last_term_enroled_numeric == 47 ~ "Summer 2019",
  term_data$last_term_enroled_numeric == 48 ~ "Fall 2019",
  term_data$last_term_enroled_numeric == 49 ~ "Winter 2020",
  term_data$last_term_enroled_numeric == 50 ~ "Spring 2020",
  term_data$last_term_enroled_numeric == 51 ~ "Summer 2020",
  term_data$last_term_enroled_numeric == 52 ~ "Fall 2020",
  term_data$last_term_enroled_numeric == 53 ~ "Winter 2021",
  term_data$last_term_enroled_numeric == 54 ~ "Spring 2021",
  term_data$last_term_enroled_numeric == 55 ~ "Summer 2021",
  term_data$last_term_enroled_numeric == 56 ~ "Fall 2021"
)
term_data$mellon_id_factor <- factor(term_data$mellon_id)
first_and_last_term <- term_data %>% select(mellon_id, first_term_enroled, last_term_enroled)
first_and_last_term <- first_and_last_term[!duplicated(first_and_last_term),]


#####Grafiken erstellen####
ggplot(data = first_and_last_term, aes(y = first_term_enroled, fill = first_term_enroled)) +
  geom_bar() +
  scale_y_discrete(limits = c("Spring 2016", "Spring 2015", "Winter 2015",
                              "Fall 2014", "Spring 2014", "Winter 2014",
                              "Fall 2013", "Spring 2013", "Winter 2013",
                              "Fall 2012", "Winter 2012", "Fall 2011",
                              "Spring 2011", "Winter 2011", "Fall 2010",
                              "Winter 2010", "Winter 2008")) +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = ..count..), stat = "count", size = 2.5, nudge_x = 300) +
  labs(title = "When did students enrol for the first time?",
       x = "",
       y = "")

ggplot(data = first_and_last_term, aes(y = last_term_enroled, fill = last_term_enroled)) +
  geom_bar() +
  theme_minimal() +
  scale_y_discrete(limits = c("Fall 2021", "Summer 2021", "Spring 2021",
                              "Winter 2021", "Fall 2020", "Summer 2020",
                              "Spring 2020", "Winter 2020", "Fall 2019",
                              "Summer 2019", "Spring 2019", "Winter 2019",
                              "Fall 2018", "Spring 2018", "Winter 2018",
                              "Fall 2017", "Spring 2017", "Winter 2017",
                              "Fall 2016", "Spring 2016", "Winter 2016",
                              "Fall 2015", "Spring 2015", "Winter 2015",
                              "Fall 2014", "Spring 2014", "Winter 2014",
                              "Fall 2013", "Spring 2013", "Winter 2013",
                              "Fall 2012", "Spring 2012", "Winter 2012", 
                              "Fall 2011", "Spring 2011", "Winter 2011", 
                              "Fall 2010", "Spring 2010")) +
  theme(legend.position = "none", axis.text = element_text(size = 4.3)) +
  geom_text(aes(label = ..count..), stat = "count", size = 1.5, nudge_x = 300) +
  labs(title = "When did students enrol for the last time?",
       x = "",
       y = "") +
  scale_x_continuous(limit = c(0, 8000))

#####Variablenkodierung: age at enrolment####
birth_data <- student_background_data %>% select(mellon_id, birth_year, birth_month)
enrolment_data <- cbind(birth_data, first_and_last_term)
enrolment_data <- enrolment_data[, -4]
enrolment_data$first_year_enroled <- case_when(
  enrolment_data$first_term_enroled == "Winter 2008" ~ 2008,
  enrolment_data$first_term_enroled == "Spring 2008" ~ 2008,
  enrolment_data$first_term_enroled == "Summer 2008" ~ 2008,
  enrolment_data$first_term_enroled == "Fall 2008" ~ 2008,
  enrolment_data$first_term_enroled == "Winter 2009" ~ 2009,
  enrolment_data$first_term_enroled == "Spring 2009" ~ 2009,
  enrolment_data$first_term_enroled == "Summer 2009" ~ 2009,
  enrolment_data$first_term_enroled == "Fall 2009" ~ 2009,
  enrolment_data$first_term_enroled == "Winter 2010" ~ 2010,
  enrolment_data$first_term_enroled == "Spring 2010" ~ 2010,
  enrolment_data$first_term_enroled == "Summer 2010" ~ 2010,
  enrolment_data$first_term_enroled == "Fall 2010" ~ 2010,
  enrolment_data$first_term_enroled == "Winter 2011" ~ 2011,
  enrolment_data$first_term_enroled == "Spring 2011" ~ 2011,
  enrolment_data$first_term_enroled == "Summer 2011" ~ 2011,
  enrolment_data$first_term_enroled == "Fall 2011" ~ 2011,
  enrolment_data$first_term_enroled == "Winter 2012" ~ 2012,
  enrolment_data$first_term_enroled == "Spring 2012" ~ 2012,
  enrolment_data$first_term_enroled == "Summer 2012" ~ 2012,
  enrolment_data$first_term_enroled == "Fall 2012" ~ 2012,
  enrolment_data$first_term_enroled == "Winter 2013" ~ 2013,
  enrolment_data$first_term_enroled == "Spring 2013" ~ 2013,
  enrolment_data$first_term_enroled == "Summer 2013" ~ 2013,
  enrolment_data$first_term_enroled == "Fall 2013" ~ 2013,
  enrolment_data$first_term_enroled == "Winter 2014" ~ 2014,
  enrolment_data$first_term_enroled == "Spring 2014" ~ 2014,
  enrolment_data$first_term_enroled == "Summer 2014" ~ 2014,
  enrolment_data$first_term_enroled == "Fall 2014" ~ 2014,
  enrolment_data$first_term_enroled == "Winter 2015" ~ 2015,
  enrolment_data$first_term_enroled == "Spring 2015" ~ 2015,
  enrolment_data$first_term_enroled == "Summer 2015" ~ 2015,
  enrolment_data$first_term_enroled == "Fall 2015" ~ 2015,
  enrolment_data$first_term_enroled == "Winter 2016" ~ 2016,
  enrolment_data$first_term_enroled == "Spring 2016" ~ 2016,
  enrolment_data$first_term_enroled == "Summer 2016" ~ 2016,
  enrolment_data$first_term_enroled == "Fall 2016" ~ 2016,
  enrolment_data$first_term_enroled == "Winter 2017" ~ 2017,
  enrolment_data$first_term_enroled == "Spring 2017" ~ 2017,
  enrolment_data$first_term_enroled == "Summer 2017" ~ 2017,
  enrolment_data$first_term_enroled == "Fall 2017" ~ 2017,
  enrolment_data$first_term_enroled == "Winter 2018" ~ 2018,
  enrolment_data$first_term_enroled == "Spring 2018" ~ 2018,
  enrolment_data$first_term_enroled == "Summer 2018" ~ 2018,
  enrolment_data$first_term_enroled == "Fall 2018" ~ 2018,
  enrolment_data$first_term_enroled == "Winter 2019" ~ 2019,
  enrolment_data$first_term_enroled == "Spring 2019" ~ 2019,
  enrolment_data$first_term_enroled == "Summer 2019" ~ 2019,
  enrolment_data$first_term_enroled == "Fall 2019" ~ 2019,
  enrolment_data$first_term_enroled == "Winter 2020" ~ 2020,
  enrolment_data$first_term_enroled == "Spring 2020" ~ 2020,
  enrolment_data$first_term_enroled == "Summer 2020" ~ 2020,
  enrolment_data$first_term_enroled == "Fall 2020" ~ 2020,
  enrolment_data$first_term_enroled == "Winter 2021" ~ 2021,
  enrolment_data$first_term_enroled == "Spring 2021" ~ 2021,
  enrolment_data$first_term_enroled == "Summer 2021" ~ 2021,
  enrolment_data$first_term_enroled == "Fall 2021" ~ 2021
)
enrolment_data$first_month_enroled <- case_when(
  enrolment_data$first_term_enroled == "Winter 2008" ~ 1,
  enrolment_data$first_term_enroled == "Spring 2008" ~ 3,
  enrolment_data$first_term_enroled == "Fall 2008" ~ 9,
  enrolment_data$first_term_enroled == "Winter 2009" ~ 1,
  enrolment_data$first_term_enroled == "Spring 2009" ~ 3,
  enrolment_data$first_term_enroled == "Fall 2009" ~ 9,
  enrolment_data$first_term_enroled == "Winter 2010" ~ 1,
  enrolment_data$first_term_enroled == "Spring 2010" ~ 3,
  enrolment_data$first_term_enroled == "Fall 2010" ~ 9,
  enrolment_data$first_term_enroled == "Winter 2011" ~ 1,
  enrolment_data$first_term_enroled == "Spring 2011" ~ 3,
  enrolment_data$first_term_enroled == "Fall 2011" ~ 9,
  enrolment_data$first_term_enroled == "Winter 2012" ~ 1,
  enrolment_data$first_term_enroled == "Spring 2012" ~ 3,
  enrolment_data$first_term_enroled == "Fall 2012" ~ 9,
  enrolment_data$first_term_enroled == "Winter 2013" ~ 1,
  enrolment_data$first_term_enroled == "Spring 2013" ~ 3,
  enrolment_data$first_term_enroled == "Fall 2013" ~ 9,
  enrolment_data$first_term_enroled == "Winter 2014" ~ 1,
  enrolment_data$first_term_enroled == "Spring 2014" ~ 3,
  enrolment_data$first_term_enroled == "Fall 2014" ~ 9,
  enrolment_data$first_term_enroled == "Winter 2015" ~ 1,
  enrolment_data$first_term_enroled == "Spring 2015" ~ 3,
  enrolment_data$first_term_enroled == "Fall 2015" ~ 9,
  enrolment_data$first_term_enroled == "Winter 2016" ~ 1,
  enrolment_data$first_term_enroled == "Spring 2016" ~ 3,
  enrolment_data$first_term_enroled == "Fall 2016" ~ 9,
  enrolment_data$first_term_enroled == "Winter 2017" ~ 1,
  enrolment_data$first_term_enroled == "Spring 2017" ~ 3,
  enrolment_data$first_term_enroled == "Fall 2017" ~ 9,
  enrolment_data$first_term_enroled == "Winter 2018" ~ 1,
  enrolment_data$first_term_enroled == "Spring 2018" ~ 3,
  enrolment_data$first_term_enroled == "Fall 2018" ~ 9,
  enrolment_data$first_term_enroled == "Winter 2019" ~ 1,
  enrolment_data$first_term_enroled == "Spring 2019" ~ 3,
  enrolment_data$first_term_enroled == "Fall 2019" ~ 9,
  enrolment_data$first_term_enroled == "Winter 2020" ~ 1,
  enrolment_data$first_term_enroled == "Spring 2020" ~ 3,
  enrolment_data$first_term_enroled == "Fall 2020" ~ 9,
  enrolment_data$first_term_enroled == "Winter 2021" ~ 1,
  enrolment_data$first_term_enroled == "Spring 2021" ~ 3,
  enrolment_data$first_term_enroled == "Fall 2021" ~ 9
)
enrolment_data$age_at_enrolment_without_month <- enrolment_data$first_year_enroled - enrolment_data$birth_year
enrolment_data$month_difference <- enrolment_data$first_month_enroled - enrolment_data$birth_month
enrolment_data$month_difference_binary <- case_when(
  enrolment_data$month_difference < 0 ~ -1,
  enrolment_data$month_difference >= 0 ~ 0
)
enrolment_data$age_at_enrolment <- enrolment_data$age_at_enrolment_without_month + enrolment_data$month_difference_binary
ggplot(data = enrolment_data, aes(x = age_at_enrolment, fill = "red")) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = ..count..), stat = "count", size = 2.0, nudge_y = 500) +
  labs(title = "How old are students when they enrol for the first time?",
       x = "",
       y = "")
