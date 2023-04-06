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

bg_sub = get_student_background_data(ids = student_sub$mellon_id)
term_sub = get_term_data(ids = student_sub$mellon_id)
course_sub = get_course_data(ids = student_sub$mellon_id)

#bg_sub = merge(bg_sub, student_sub[,c("mellon_id","admitdate")], by="mellon_id")

## LABEL: DROPOUT ##
mean(is.na(student_sub$dropout)) # amount of missing data

# plot unconditional distribution of dropout
ggplot(student_sub, aes(x=dropout)) +
  geom_bar() + theme_minimal() + labs(x="",y="")

# dropout rate by cohort
dropout_rates = aggregate(dropout ~ admitdate, student_sub,FUN=function(x) mean(x, na.rm=T))
sd(dropout_rates$dropout)
dropout_rates$admitdate = paste0("20", substring(dropout_rates$admitdate,2))

ggplot(data = dropout_rates, aes(x=admitdate, y=dropout)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  #geom_text(aes(label=round(dropout,2)), vjust = -0.5) +
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size = 18, color="black"),
        axis.text = element_text(size = 14, color="black"),
        axis.text.x = element_text(vjust = +2.5),
        axis.text.y = element_text(hjust = +1.25)) +
  labs(title = "Dropout by cohort",
       x = "Admission year",
       y = "Dropout rate") +
  scale_y_continuous(labels = scales::percent_format())
par()
# Compare dropout variables / obsolete because differences were very low
table(student_sub$dropout)
sum(is.na(student_sub$dropout))

table(student_sub$dropout_alt)
sum(is.na(student_sub$dropout_alt)) / length(student_sub$dropout_alt)

sum(table(student_vars_3$dropout)-table(student_sub$dropout))
sum(is.na(student_vars_3$dropout))-sum(is.na(student_sub$dropout))


# age at enrollment
mean(is.na(student_sub$age_at_enrolment))

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

# dropout rates over age at enrollment
ggplot(data = student_sub, aes(x = age_at_enrolment)) +
  geom_bar(data=subset(student_sub,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  geom_bar(data=subset(student_sub,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of age at enrolment",
       x = "Age",
       y = "Frequency") +
  xlim(17, 22) +
  theme(text = element_text(size = 16))

dropout_rates_age = do.call(data.frame, aggregate(dropout ~ as.integer(age_at_enrolment), student_sub, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates_age, aes(x=as.integer.age_at_enrolment., y=dropout.mean, fill=dropout.n > 46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  #geom_text(aes(label=round(x,2)), vjust = -0.5) + 
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  theme(legend.position = "none") +
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

# scores
mean(is.na(student_sub$uc_total_score))
ggplot(data = student_sub, aes(x=5*round(uc_total_score/5))) +
  geom_bar(data=subset(student_sub,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  geom_bar(data=subset(student_sub,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  theme_minimal() +
  labs(title = "Distribution of UC total scores",
       x = "Score",
       y = "Frequency",
       fill="Dropout") +
  theme(text = element_text(size = 16))

student_sub$total_score_buckets =5*round(student_sub$uc_total_score/5)
dropout_rates_total_score = do.call(data.frame, aggregate(dropout ~ total_score_buckets, student_sub, FUN = function(x) c(mean = mean(x), n = length(x))))
dropout_rates_total_score = dropout_rates_total_score[4:nrow(dropout_rates_total_score),]
ggplot(data = dropout_rates_total_score, aes(x=total_score_buckets, y=dropout.mean, fill=dropout.n > 46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  #geom_text(aes(label=round(x,2)), vjust = -0.5) + 
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "Dropout rates by UC total score",
       x = "Score",
       y = "Dropout rate",
       fill="n>1???")

mean(is.na(student_sub$uc_math_score))
ggplot(data = student_sub, aes(x=5*round(uc_math_score/5))) +
  geom_bar(data=subset(student_sub,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  geom_bar(data=subset(student_sub,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of UC math scores",
       x = "Score",
       y = "Frequency") +
  theme(text = element_text(size = 16))

student_sub$math_score_buckets =5*round(student_sub$uc_math_score/5)
dropout_rates_math_score = do.call(data.frame, aggregate(dropout ~ math_score_buckets, student_sub, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates_math_score, aes(x=math_score_buckets, y=dropout.mean, fill=dropout.n > 46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  #geom_text(aes(label=round(x,2)), vjust = -0.5) + 
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by UC math score",
       x = "Score",
       y = "Dropout rate")

# read
mean(is.na(student_sub$uc_read_score))
ggplot(student_sub, aes(x=5*round(uc_read_score/5))) +
  geom_bar(data=subset(student_sub,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  geom_bar(data=subset(student_sub,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of UC read scores",
       x = "Score",
       y = "Frequency") +
  theme(text = element_text(size = 16))

student_sub$read_score_buckets =5*round(student_sub$uc_read_score/5)
dropout_rates_read_score = do.call(data.frame, aggregate(dropout ~ read_score_buckets, student_sub, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(dropout_rates_read_score, aes(x=read_score_buckets, y=dropout.mean, fill=dropout.n > 46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by UC read score",
       x = "Score",
       y = "Dropout rate")

# writing
mean(is.na(student_sub$uc_writing_score))
ggplot(student_sub, aes(x=5*round(uc_writing_score/5))) +
  geom_bar(data=subset(student_sub,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  geom_bar(data=subset(student_sub,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of UC writing scores",
       x = "Score",
       y = "Frequency") +
  theme(text = element_text(size = 16))

student_sub$writing_score_buckets =5*round(student_sub$uc_writing_score/5)
dropout_rates_writing_score = do.call(data.frame, aggregate(dropout ~ writing_score_buckets, student_sub, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(dropout_rates_writing_score, aes(x=writing_score_buckets, y=dropout.mean, fill=dropout.n > 46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by UC writing score",
       x = "Score",
       y = "Dropout rate")

### TERM FEATURES ANALYSIS ###
# number of terms
ggplot(data = student_sub, aes(x = num_terms)) +
  geom_bar(data=subset(student_sub,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  geom_bar(data=subset(student_sub,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  theme_minimal() +
  theme(text=element_text(size=16)) +
  labs(title = "How long are students enrolled?",
       x = "number of terms",
       y = "", fill="Dropout")

# chance of dropout when having complete term n
n_terms = 1:20
rates = sapply(n_terms, FUN=function(i) mean(student_sub$dropout[student_sub$num_terms >= i], na.rm=T))
n_obs = sapply(n_terms, FUN=function(i) sum(student_sub$num_terms >= i))
ggplot(data.frame(x=n_terms, y=rates, n=n_obs), aes(x=x, y=y, fill=n>46)) + 
  geom_bar(stat="identity") + 
  labs(x="Number of terms") +
  theme_minimal() +
  theme(legend.position = "none")


term_features = get_term_features()
term_features = merge(term_features, student_sub[,c("mellon_id", "dropout", "admitdate")], by="mellon_id")
term_features_1 = subset(term_features, term_num==1)
dropout_rates_major = do.call(data.frame, aggregate(dropout ~ major_1, term_features_1, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates_major, aes(x=major_1, y=dropout.mean, fill=dropout.n > 46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust=1)) +
  labs(title = "Dropout rates by Major",
       x = "Major in first term",
       y = "Dropout rate",
       fill="n>1???") 

# units completed
term_features = merge(term_features, student_sub[,c("mellon_id", "dropout", "admitdate")], by="mellon_id")
mean(is.na(term_features$units_completed))
aggregate(is.na(units_completed)~admitdate, term_features, FUN=mean)

# for all terms
ggplot(data = term_features, aes(x = as.integer(units_completed))) +
  geom_bar(data=subset(term_features,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of units completed",
       x = "Units completed",
       y = "Frequency") +
  xlim(0, 24) +
  theme(text = element_text(size = 16))

# dropout rates per units completed
term_features$dropout_num = as.integer(term_features$dropout)
dropout_rates_units_completed = do.call(data.frame, aggregate(dropout ~ as.integer(units_completed), term_features, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates_units_completed, aes(x=as.integer.units_completed., y=dropout.mean, fill=dropout.n > 46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by units completed",
       x = "Units completed",
       y = "Dropout rate") +
  xlim(-1, 32)

# only for first term

ggplot(data = term_features_1, aes(x = as.integer(cum_avg_credits))) +
  geom_bar(data=subset(term_features_1,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features_1,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by units completed in 1st term",
       x = "Units completed",
       y = "Frequency") +
  xlim(0, 24) +
  theme(text = element_text(size = 16))

# cumulative from first two terms
mean(is.na(term_features$cum_avg_credits))
term_features_2 = subset(term_features, term_num==2)
ggplot(data = term_features_2, aes(x = as.integer(cum_avg_credits))) +
  geom_bar(data=subset(term_features_2,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features_2,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by avg units completed till 2nd term",
       x = "Units completed",
       y = "Frequency") +
  xlim(0, 24) +
  theme(text = element_text(size = 16))

term_features_3 = subset(term_features, term_num==3)
ggplot(data = term_features_3, aes(x = as.integer(cum_avg_credits))) +
  geom_bar(data=subset(term_features_2,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features_2,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by avg units completed till 3rd term",
       x = "Units completed",
       y = "Frequency") +
  xlim(0, 24) +
  theme(text = element_text(size = 16))

# units completed relative to term
mean(is.na(term_features$units_completed.rel_term_num)) # same as before

ggplot(data = term_features, aes(x = as.integer(units_completed.rel_term_num))) +
  geom_bar(data=subset(term_features,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of current units",
       x = "Units completed",
       y = "Frequency") +
  theme(text = element_text(size = 16))

dropout_rates_units_completed_rel_term_num = do.call(data.frame, aggregate(dropout ~ as.integer(units_completed.rel_term_num), term_features, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates_units_completed_rel_term_num, aes(x=as.integer.units_completed.rel_term_num., y=dropout.mean, fill=dropout.n > 46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by units completed",
       x = "Units completed",
       y = "Dropout rate")

ggplot(data = term_features_1, aes(x = as.integer(cum_avg_credits.rel_term_num))) +
  geom_bar(data=subset(term_features_1,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features_1,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dist of relative cum avg credits",
       x = "Units completed",
       y = "Frequency") +
  theme(text = element_text(size = 16))

ggplot(data = term_features_2, aes(x = as.integer(cum_avg_credits.rel_term_num))) +
  geom_bar(data=subset(term_features_2,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features_2,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by avg units completed till 2nd term",
       x = "Units completed",
       y = "Frequency") +
  theme(text = element_text(size = 16))

# relative to major
mean(is.na(term_features$units_completed.rel_major)) # same as before

ggplot(data = term_features, aes(x = as.integer(units_completed.rel_major))) +
  geom_bar(data=subset(term_features,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of current units rel to major",
       x = "Units completed",
       y = "Frequency") +
  xlim(-10,10) +
  theme(text = element_text(size = 16))

dropout_rates_units_completed_rel_major = do.call(data.frame, aggregate(dropout ~ as.integer(units_completed.rel_major), term_features, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates_units_completed_rel_major, aes(x=as.integer.units_completed.rel_major., y=dropout.mean, fill=dropout.n > 46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by units completed rel to major",
       x = "Units completed",
       y = "Dropout rate")

ggplot(data = term_features_1, aes(x = as.integer(cum_avg_credits.rel_major))) +
  geom_bar(data=subset(term_features_1,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features_1,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dist of relative cum avg credits rel to major",
       x = "Units completed",
       y = "Frequency") +
  theme(text = element_text(size = 16))

ggplot(data = term_features_2, aes(x = as.integer(cum_avg_credits.rel_major))) +
  geom_bar(data=subset(term_features_2,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features_2,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by avg units completed till 2nd term rel to major",
       x = "Units completed",
       y = "Frequency") +
  theme(text = element_text(size = 16))

ggplot(data = term_features_3, aes(x = as.integer(cum_avg_credits.rel_major))) +
  geom_bar(data=subset(term_features_2,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features_2,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by avg units completed till 3rd term rel to major",
       x = "Units completed",
       y = "Frequency") +
  theme(text = element_text(size = 16))

# relative to major x term number
mean(is.na(term_features$units_completed.rel_major_termnum)) # same as before

ggplot(data = term_features, aes(x = as.integer(units_completed.rel_major_termnum))) +
  geom_bar(data=subset(term_features,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of current units rel to major and term number",
       x = "Units completed",
       y = "Frequency") +
  xlim(-10,10) +
  theme(text = element_text(size = 16))

dropout_rates_units_completed_rel_major_termnum = do.call(data.frame, aggregate(dropout ~ as.integer(units_completed.rel_major_termnum), term_features, FUN = function(x) c(mean = mean(x), n = length(x))))

ggplot(data = dropout_rates_units_completed_rel_major_termnum, aes(x=as.integer.units_completed.rel_major_termnum., y=dropout.mean, fill=dropout.n > 46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by units completed rel to major and term number",
       x = "Units completed",
       y = "Dropout rate")

ggplot(data = term_features_1, aes(x = as.integer(cum_avg_credits.rel_major_termnum))) +
  geom_bar(data=subset(term_features_1,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features_1,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dist of relative cum avg credits rel to major and term number",
       x = "Units completed",
       y = "Frequency") +
  theme(text = element_text(size = 16))

ggplot(data = term_features_2, aes(x = as.integer(cum_avg_credits.rel_major_termnum))) +
  geom_bar(data=subset(term_features_2,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features_2,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by avg units completed till 2nd term rel to major and term number",
       x = "Units completed",
       y = "Frequency") +
  theme(text = element_text(size = 16))

ggplot(data = term_features_3, aes(x = as.integer(cum_avg_credits.rel_major_termnum))) +
  geom_bar(data=subset(term_features_2,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features_2,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by avg units completed till 3rd term rel to major and term number",
       x = "Units completed",
       y = "Frequency") +
  theme(text = element_text(size = 16))

# major
stud_major =  do.call(data.frame, aggregate(major_name_1 ~ mellon_id, terms, FUN=function(x) c(first=x[1], second=x[2], third=x[3])))
stud_major = merge(stud_major, student_sub[,c("mellon_id", "dropout")], by="mellon_id")

avg_dropout_major = do.call(data.frame, aggregate(dropout ~ major_name_1.first, stud_major, FUN = function(x) c(mean = mean(x), n = length(x))))
avg_dropout_major$major_name_1.first = factor(avg_dropout_major$major_name_1.first,levels = avg_dropout_major$major_name_1.first[order(-avg_dropout_major$dropout.n)])
ggplot(avg_dropout_major, aes(x=major_name_1.first, y=dropout.mean, fill=dropout.n>46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title = "Mean of dropout per first term major",
       x = "Major",
       y = "Dropout rate", fill="")+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(legend.position = "none",text = element_text(size = 8))

avg_dropout_major = do.call(data.frame, aggregate(dropout ~ major_name_1.second, stud_major, FUN = function(x) c(mean = mean(x), n = length(x))))
avg_dropout_major$major_name_1.second = factor(avg_dropout_major$major_name_1.second,levels = avg_dropout_major$major_name_1.second[order(-avg_dropout_major$dropout.n)])
ggplot(avg_dropout_major, aes(x=major_name_1.second, y=dropout.mean, fill=dropout.n>46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title = "Mean of dropout per second term major",
       x = "Major",
       y = "Dropout rate", fill="")+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(legend.position = "none",text = element_text(size = 8))

avg_dropout_major = do.call(data.frame, aggregate(dropout ~ major_name_1.third, stud_major, FUN = function(x) c(mean = mean(x), n = length(x))))
avg_dropout_major$major_name_1.third = factor(avg_dropout_major$major_name_1.third,levels = avg_dropout_major$major_name_1.third[order(-avg_dropout_major$dropout.n)])
ggplot(avg_dropout_major, aes(x=major_name_1.third, y=dropout.mean, fill=dropout.n>46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title = "Mean of dropout per third term major",
       x = "Major",
       y = "Dropout rate", fill="")+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(legend.position = "none",text = element_text(size = 8))

# school
stud_school = do.call(data.frame, aggregate(major_school_name_1 ~ mellon_id, terms, FUN=function(x) c(first=x[1], second=x[2], third=x[3])))
stud_school = merge(stud_school, student_sub[,c("mellon_id", "dropout")], by="mellon_id")

avg_dropout_school = do.call(data.frame, aggregate(dropout ~ major_school_name_1.first, stud_school, FUN = function(x) c(mean = mean(x), n = length(x))))
avg_dropout_school$major_school_name_1.first = factor(avg_dropout_school$major_school_name_1.first,levels = avg_dropout_school$major_school_name_1.first[order(-avg_dropout_school$dropout.n)])
ggplot(avg_dropout_school, aes(x=major_school_name_1.first, y=dropout.mean)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title = "Mean of dropout per first term school",
       x = "Major",
       y = "Dropout rate", fill="")+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(legend.position = "none",text = element_text(size = 14))

avg_dropout_school = do.call(data.frame, aggregate(dropout ~ major_school_name_1.second, stud_school, FUN = function(x) c(mean = mean(x), n = length(x))))
avg_dropout_school$major_school_name_1.second = factor(avg_dropout_school$major_school_name_1.second,levels = avg_dropout_school$major_school_name_1.second[order(-avg_dropout_school$dropout.n)])
ggplot(avg_dropout_school, aes(x=major_school_name_1.second, y=dropout.mean)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title = "Mean of dropout per second term school",
       x = "Major",
       y = "Dropout rate", fill="")+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(legend.position = "none",text = element_text(size = 14))

avg_dropout_school = do.call(data.frame, aggregate(dropout ~ major_school_name_1.third, stud_school, FUN = function(x) c(mean = mean(x), n = length(x))))
avg_dropout_school$major_school_name_1.third = factor(avg_dropout_school$major_school_name_1.third,levels = avg_dropout_school$major_school_name_1.third[order(-avg_dropout_school$dropout.n)])
ggplot(avg_dropout_school, aes(x=major_school_name_1.third, y=dropout.mean)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title = "Mean of dropout per third term school",
       x = "Major",
       y = "Dropout rate", fill="")+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(legend.position = "none",text = element_text(size = 14))

# number of majors

stud_num_majors = merge(stud_num_majors, student_sub[,c("mellon_id", "dropout")], by="mellon_id")

ggplot(data = stud_num_majors, aes(x = num_majors)) +
  geom_bar(data=subset(stud_num_majors,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(stud_num_majors,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of number of majors",
       x = "Majors",
       y = "Frequency") +
  theme(text = element_text(size = 16))

dropout_rates_num_major = do.call(data.frame, aggregate(dropout ~ num_majors, stud_num_majors, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates_num_major, aes(x=num_majors, y=dropout.mean, fill=dropout.n > 46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by number of majors",
       x = "Number of majors",
       y = "Dropout rate")

ggplot(data = term_features_1, aes(x = num_majors)) +
  geom_bar(data=subset(term_features_1,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features_1,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of number of majors",
       x = "Number of majors",
       y = "Frequency") +
  theme(text = element_text(size = 16))

ggplot(data = term_features_2, aes(x = num_majors)) +
  geom_bar(data=subset(term_features_1,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features_1,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of number of majors",
       x = "Number of majors",
       y = "Frequency") +
  theme(text = element_text(size = 16))

ggplot(data = term_features_3, aes(x = num_majors)) +
  geom_bar(data=subset(term_features_1,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(term_features_1,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of number of majors",
       x = "Number of majors",
       y = "Frequency") +
  theme(text = element_text(size = 16))

