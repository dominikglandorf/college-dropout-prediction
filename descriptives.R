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
table(student_background_data$application_status)
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


# filter by date of admissions and enrolled in a fall term from 2008 on
admit_year = as.integer(substr(student_vars$admitdate, 2,3))
filter_admit = str_detect(student_vars$admitdate, "F08") | 
         admit_year >= 9 &
         admit_year < 50
table(filter_admit)

## distribution of NAs
sum(is.na(filter_admit))/nrow(student_vars)
ggplot(as.data.frame(table(substr(student_vars$first_term_desc[is.na(admit_year)],1,4))), aes(x=Var1, y = Freq)) + 
  geom_bar(stat="identity") + labs(x="First term recorded",title="First term of missing admit date")+theme_minimal()

## filter properties
table(filter_admit)
table(filter_admit)/length(filter_admit)
ggplot(data = student_vars, aes(x = admitdate, fill = filter_admit)) +
  geom_bar() +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1)) +
  labs(title = "Date of admission",
       x = "Code",
       y = "", fill="Included")

filter_admit[is.na(filter_admit)] = F
student_vars_2 = student_vars[filter_admit,]
ggplot(data = student_vars_2, aes(x = first_term_desc, fill = start_as_freshman)) +
  geom_bar() +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "First term enrolled and application status",
       x = "Term",
       y = "", fill="Freshman")
sum(is.na(student_vars_2$start_as_freshman))/nrow(student_vars_2)
filter_fall_cohorts = student_vars_2$first_term == 92 &
                      student_vars_2$first_year < 2018 &
                      student_vars_2$start_as_freshman
filter_fall_cohorts[is.na(filter_fall_cohorts)] = F
table(filter_fall_cohorts)
table(filter_fall_cohorts)/length(filter_fall_cohorts)
student_vars_3 = student_vars_2[filter_fall_cohorts,]
ggplot(data = student_vars_3, aes(x = first_term_desc, fill = dropout)) +
  geom_bar() +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "Selected cohorts",
       x = "Start",
       y = "", fill="Dropout")

# dropout for transfer
dropout_rates_transfer = aggregate(student_vars_2$dropout, by=list(student_vars_2$start_as_freshman),FUN=function(x) {
  x[is.na(x)]=F
  return(mean(x))
})
ggplot(data = dropout_rates_transfer, aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  geom_text(aes(label=round(x,2)), vjust = -0.5) + 
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "Dropout rates by start as freshman",
       x = "Start as freshman",
       y = "Dropout rate")

###############################
# FROM NOW ANALYSIS WITH SUBSET
###############################
student_sub = get_student_sub()
dim(student_sub)
term_data = get_term_data()
term_sub = term_data[term_data$mellon_id %in% student_sub$mellon_id,]
course_data = get_course_data()
course_sub = course_data[course_data$mellon_id %in% student_sub$mellon_id,]
bg_sub = merge(student_background_data, student_sub[,c("mellon_id","admitdate")], by="mellon_id")

# age at enrollment
ggplot(data = student_sub, aes(x = as.integer(age_at_enrolment))) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = ..count..), stat = "count", nudge_y = 3000, nudge_x=-0.08,angle=90) +
  labs(title = "Distribution of age at enrolment",
       x = "Age",
       y = "Frequency") +
  xlim(16, 24) +
  ylim(0, 42000) +
  theme(text = element_text(size = 16))
summary(student_sub$age_at_enrolment)
sd(student_sub$age_at_enrolment) #On average, students are 19.24 years old at their first enrollment (SD = 2.94).

# gender
table(bg_sub$gender)/length(bg_sub$gender)
table(bg_sub$ethnicity)/length(bg_sub$ethnicity)*100
ggplot(data = bg_sub, aes(x = ethnicity)) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust=1)) +
  geom_text(aes(label = ..count..), stat = "count", nudge_y = 3000, nudge_x=-0.0,angle=0) +
  ylim(0,32000)+
  labs(title = "Distribution of ethnicity",
       x = "Ethnicity",
       y = "Frequency")

# dropout rates over cohorts
dropout_rates = aggregate(student_sub$dropout_alt, by=list(student_sub$first_term_desc),FUN=function(x) {
  x[is.na(x)]=F
  return(mean(x))
})
ggplot(data = dropout_rates, aes(x =Group.1, y=x)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  geom_text(aes(label=round(x,2)), vjust = -0.5) + 
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "Dropout by cohort",
       x = "Cohort",
       y = "Dropout rate") +
  ylim(0, 0.15)

dropout_rates_alt = aggregate(student_sub$dropout_alt, by=list(student_sub$first_term_desc),FUN=function(x) {
  x[is.na(x)]=F
  return(mean(x))
})

# dropout rates over age at enrollment
dropout_rates_age = aggregate(student_sub$dropout, by=list(as.integer(student_sub$age_at_enrolment)),FUN=function(x) {
  x[is.na(x)]=F
  return(mean(x))
})
ggplot(data = dropout_rates_age, aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  geom_text(aes(label=round(x,2)), vjust = -0.5) + 
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "Dropout rates by age",
       x = "Age at enrollment",
       y = "Dropout rate")

# dropout rates over period from first to last term
dropout_rates_period = aggregate(student_sub$dropout, by=list(as.integer(student_sub$number_of_years)),FUN=function(x) {
  x[is.na(x)]=F
  return(mean(x))
})
ggplot(data = dropout_rates_period, aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  geom_text(aes(label=round(x,2)), vjust = -0.5) + 
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "Dropout rates by time span of enrollment",
       x = "Time span from first to last term",
       y = "Dropout rate")


# Compare dropout variables
table(student_sub$dropout)
sum(is.na(student_sub$dropout))

table(student_sub$dropout_alt)
sum(is.na(student_sub$dropout_alt)) / length(student_sub$dropout_alt)

sum(table(student_vars_3$dropout)-table(student_sub$dropout))
sum(is.na(student_vars_3$dropout))-sum(is.na(student_sub$dropout))

# number of terms
ggplot(data = student_sub[!is.na(student_sub$dropout),], aes(x = num_terms, fill = dropout)) +
  geom_bar() +
  theme_minimal() +
  theme(text=element_text(size=16)) +
  geom_text(aes(label = ..count..), stat = "count", nudge_y = 300) +
  labs(title = "How long are students enrolled?",
       x = "number of terms",
       y = "", fill="Dropout")


avg_credits = aggregate(term_data$current_units_completed_total, by=list(term_data$mellon_id), FUN=mean)
student_sub = merge(student_sub, avg_credits, by.x="mellon_id",by.y="Group.1")
cor(student_sub$dropout, student_sub$x, use="complete.obs")
