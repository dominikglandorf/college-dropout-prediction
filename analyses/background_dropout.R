source('read_data.R')
students = get_student_sub()
dim(students) # 46410 x 36

# GENDER
mean(is.na(students$female)) # same as before

ggplot(data = students, aes(x = female)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropouts vs Non-Dropouts by Gender",
       x = "Is female",
       y = "Frequency") +
  theme(text = element_text(size = 16))

dropout_rates_female = do.call(data.frame, aggregate(dropout ~ female, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates_female, aes(x=female, y=dropout.mean, fill="blue")) +
  geom_bar(stat="identity", fill="#54BCC2") +
  theme_minimal() +
  #geom_text(aes(label=round(x,2)), vjust = -0.5) + 
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  theme(legend.position = "none") +
  labs(title = "",
       x = "",
       y = "Dropout rate")

# INTERNATIONAL STUDENT
mean(is.na(students$int_student)) 

ggplot(data = students, aes(x = as.integer(int_student))) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of international students",
       x = "International student",
       y = "Frequency") +
  theme(text = element_text(size = 16))

is_int = as.integer(students$int_student)
dropout_rates_int = do.call(data.frame, aggregate(dropout ~ is_int, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates_int, aes(x=is_int, y=dropout.mean)) +
  geom_bar(stat="identity", fill="#54BCC2") +
  theme_minimal() +
  #geom_text(aes(label=round(x,2)), vjust = -0.5) + 
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  theme(legend.position = "none") +
  labs(title = "",
       x = "",
       y = "Dropout rate")

# ethnicity
mean(is.na(students$ethnicity))

ggplot(data = students, aes(x = ethnicity)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropouts vs Non-Dropouts by Ethnicity",
       x = "",
       y = "Frequency") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 60, hjust = 1)) #+
  scale_x_discrete(label=abbreviate)

dropout_rates = do.call(data.frame, aggregate(dropout ~ ethnicity, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates, aes(x=ethnicity, y=dropout.mean)) +
  geom_bar(stat="identity", fill="#54BCC2") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") +
  scale_x_discrete(label=abbreviate)

# first_generation
unique(students$first_generation)
mean(is.na(students$first_generation))

ggplot(data = students, aes(x = as.logical(first_generation))) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropouts vs Non-Dropouts by First Generation",
       x = "First generation",
       y = "Frequency") +
  theme(text = element_text(size = 16))

dropout_rates = do.call(data.frame, aggregate(dropout ~ first_generation, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates, aes(x=first_generation, y=dropout.mean)) +
  geom_bar(stat="identity", fill="#54BCC2") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") +
  scale_x_discrete(label=abbreviate)

# low_income
unique(students$low_income)
mean(is.na(students$low_income))

ggplot(data = students, aes(x = low_income)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropouts vs Non-Dropouts by Low Income",
       x = "Low Income",
       y = "Frequency") +
  theme(text = element_text(size = 16))

dropout_rates = do.call(data.frame, aggregate(dropout ~ low_income, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates, aes(x=low_income, y=dropout.mean)) +
  geom_bar(stat="identity", fill="#54BCC2") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") +
  scale_x_discrete(label=abbreviate)

# father_edu_level_code
unique(students$father_edu_level_code)
mean(is.na(students$father_edu_level_code))

ggplot(data = students, aes(x = father_edu_level_code)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") +
  theme(text = element_text(size = 16))

dropout_rates = do.call(data.frame, aggregate(dropout ~ father_edu_level_code, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates, aes(x=father_edu_level_code, y=dropout.mean)) +
  geom_bar(stat="identity", fill="#54BCC2") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") 

# mother_edu_level_code
unique(students$mother_edu_level_code)
mean(is.na(students$mother_edu_level_code))

ggplot(data = students, aes(x = mother_edu_level_code)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") +
  theme(text = element_text(size = 16))

dropout_rates = do.call(data.frame, aggregate(dropout ~ mother_edu_level_code, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates, aes(x=mother_edu_level_code, y=dropout.mean)) +
  geom_bar(stat="identity", fill="#54BCC2") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") 

# ell
unique(students$ell)
mean(is.na(students$ell))

ggplot(data = students, aes(x = ell)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") +
  theme(text = element_text(size = 16))

dropout_rates = do.call(data.frame, aggregate(dropout ~ ell, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates, aes(x=ell, y=dropout.mean)) +
  geom_bar(stat="identity", fill="#54BCC2") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") 

# single_parent
unique(students$single_parent)
mean(is.na(students$single_parent))

ggplot(data = students, aes(x = single_parent)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") +
  theme(text = element_text(size = 16))

dropout_rates = do.call(data.frame, aggregate(dropout ~ single_parent, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates, aes(x=single_parent, y=dropout.mean)) +
  geom_bar(stat="identity", fill="#54BCC2") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") 

# foster_care
unique(students$foster_care)
mean(is.na(students$foster_care))

ggplot(data = students, aes(x = foster_care)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") +
  theme(text = element_text(size = 16))

dropout_rates = do.call(data.frame, aggregate(dropout ~ foster_care, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates, aes(x=foster_care, y=dropout.mean)) +
  geom_bar(stat="identity", fill="#54BCC2") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") 

# raised_by_single_parent
unique(students$raised_by_single_parent)
mean(is.na(students$raised_by_single_parent))
# 100% missing data

# household_size_app
unique(students$household_size_app)
mean(is.na(students$household_size_app))

ggplot(data = students, aes(x = household_size_app)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") +
  theme(text = element_text(size = 16))

dropout_rates = do.call(data.frame, aggregate(dropout ~ household_size_app, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates, aes(x=household_size_app, y=dropout.mean, fill=dropout.n>46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") 

# distance_from_home
unique(students$distance_from_home)
mean(is.na(students$distance_from_home))

students$dists = pmin(1000, 20* round(as.integer(students$distance_from_home)/20))
ggplot(data = students, aes(x = dists)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropouts vs Non-Dropouts over distance from home",
       x = "Distance [miles]",
       y = "Frequency") +
  theme(text = element_text(size = 16))

dropout_rates = do.call(data.frame, aggregate(dropout ~dists, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates, aes(x=dists, y=dropout.mean, fill=dropout.n>46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") 

# sport_at_admission
unique(students$sport_at_admission)
mean(is.na(students$sport_at_admission))

ggplot(data = students, aes(x = sport_at_admission)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") +
  theme(text = element_text(size = 16))

dropout_rates = do.call(data.frame, aggregate(dropout ~sport_at_admission, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates, aes(x=sport_at_admission, y=dropout.mean, fill=dropout.n>46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") 

# cal_res_at_app
unique(students$cal_res_at_app)
mean(is.na(students$cal_res_at_app))

ggplot(data = students, aes(x = cal_res_at_app)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") +
  theme(text = element_text(size = 16))+
  scale_x_discrete(label=abbreviate)

dropout_rates = do.call(data.frame, aggregate(dropout ~cal_res_at_app, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates, aes(x=cal_res_at_app, y=dropout.mean)) +
  geom_bar(stat="identity", fill="#54BCC2") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") +
  scale_x_discrete(label=abbreviate)

# hs_gpa
unique(students$hs_gpa)
mean(is.na(students$hs_gpa))

students$gpa = round(students$hs_gpa, 1)
ggplot(data = students, aes(x = gpa)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") +
  theme(text = element_text(size = 16))

dropout_rates = do.call(data.frame, aggregate(dropout ~gpa, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates, aes(x=gpa, y=dropout.mean, fill=dropout.n>46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") 

# toefl_score
unique(students$toefl_score)
mean(is.na(students$toefl_score))
# 90% missing data

# ielts_score
unique(students$ielts_score)
mean(is.na(students$ielts_score))
# 99.7% missing data


### AP exams
my_agg = function(formula, data) do.call(data.frame, aggregate(formula, data, FUN=function(x) c(mean = mean(x), n = length(x))))
dropped = subset(students,dropout == T)
ndropped = subset(students,dropout == F)
                                         
theme_set(theme_minimal() +
          theme(legend.position = "none",
                text = element_text(size = 16)))

# number of AP exams
ggplot(data=students, aes(x = number_ap, y = after_stat(count)/sum(after_stat(count)))) +
  geom_bar(data=subset(students,dropout == T),alpha=0.4,fill="red") +
  geom_bar(data=subset(students,dropout == F),alpha=0.4,fill="darkgreen")

ggplot(data =  my_agg(dropout ~ number_ap, students), aes(x=number_ap, y=dropout.mean, fill=dropout.n > 46)) +
  geom_bar(stat="identity")

# ratio of passed AP exams
students$passed_ap_disc = round(students$passed_ap*10)/10
mean(is.na(students$passed_ap_disc))

ggplot(data=students, aes(x =passed_ap_disc, y = after_stat(count)/sum(after_stat(count)))) +
  geom_bar(data=subset(students,dropout == T),alpha=0.4,fill="red") +
  geom_bar(data=subset(students,dropout == F),alpha=0.4,fill="darkgreen")

ggplot(data =  my_agg(dropout ~ passed_ap_disc, students), aes(x=passed_ap_disc, y=dropout.mean)) +
  geom_bar(stat="identity", fill="#54BCC2")


# best AP score
mean(students$best_ap<0)

ggplot(data=students, aes(x=best_ap, y=after_stat(count)/sum(after_stat(count)))) +
  geom_bar(data=subset(students,dropout == T), alpha=0.4, fill="red") +
  geom_bar(data=subset(students,dropout == F), alpha=0.4, fill="darkgreen")

best_ap_dropout_rates=my_agg(dropout ~ best_ap, students)
best_ap_dropout_rates[1,1]=0
ggplot(data = best_ap_dropout_rates, aes(x=best_ap, y=dropout.mean)) +
  geom_bar(stat="identity", fill="#54BCC2")

# mean AP score
students$avg_ap_disc = round(students$avg_ap*4)/4
mean(is.na(students$avg_ap_disc))

ggplot(data=students, aes(x=avg_ap_disc, y=after_stat(count)/sum(after_stat(count)))) +
  geom_bar(data=subset(students,dropout == T), alpha=0.4, fill="red") +
  geom_bar(data=subset(students,dropout == F), alpha=0.4, fill="darkgreen")

avg_ap_dropout_rates=my_agg(dropout ~ avg_ap_disc, students)
ggplot(data =avg_ap_dropout_rates, aes(x=avg_ap_disc, y=dropout.mean)) +
  geom_bar(stat="identity",fill="#54BCC2")

# mellon ids
students$ids = 10000*round(as.integer(students$mellon_id)/10000)
students$ids[students$ids>500000] = students$ids[students$ids>500000] - 2500000
ggplot(data = students, aes(x = ids)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropouts vs Non-Dropouts over distance from home",
       x = "Distance [miles]",
       y = "Frequency") +
  theme(text = element_text(size = 16))

dropout_rates = do.call(data.frame, aggregate(dropout ~ ids, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates, aes(x=ids, y=dropout.mean, fill=dropout.n>46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "Dropout rate by mellon id",
       x = "",
       y = "") 
aggregate(mellon_id ~ admitdate, students, FUN=mean)
aggregate(dropout ~ admitdate, students, FUN=mean)
aggregate(dropout ~ id_group, students, FUN=mean)
students = merge(students, terms[,c("mellon_id","school_1")])

students$id_group = 2
students$id_group[students$mellon_id < 370000] = 1
students$id_group[students$mellon_id > 400000] = 1
table(students$id_group, students$admitdate)
table(students$id_group, students$school_1)
aggregate(dropout ~ school_1, students, FUN=mean)

ggplot(data = students[students$school_1=="Div of Undergraduate Education",], aes(x = ids)) +
  geom_bar(data=subset(students[students$school_1=="Div of Undergraduate Education",],dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students[students$school_1=="Div of Undergraduate Education",],dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Dropouts vs Non-Dropouts over distance from home",
       x = "Distance [miles]",
       y = "Frequency") +
  theme(text = element_text(size = 16))

# compare id group 1 vs 2
group1 = students$id_group == 1
for (name in names(students)) {
  print(name)
  if (class(students[,name])=="character") {
    plot(table(students[,c(name,"id_group")]),main=name)
  } else {
    print(ggplot(data = students, aes_string(x = name)) +
      geom_bar(data=subset(students,group1),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
      geom_bar(data=subset(students,!group1),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
      theme_minimal() +
      theme(legend.position = "none"))
  }
}
