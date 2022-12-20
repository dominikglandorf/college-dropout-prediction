source('read_data.R')
students = get_student_sub()
dim(students) # 46408 x 43

# GENDER
mean(is.na(students$female)) # same as before

ggplot(data = students, aes(x = female)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of gender",
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
  labs(title = "",
       x = "",
       y = "") +
  theme(text = element_text(size = 16)) +
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

ggplot(data = students, aes(x = first_generation)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") +
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
  labs(title = "",
       x = "",
       y = "") +
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

students$dists = pmin(1000, 10* round(as.integer(students$distance_from_home)/10))
ggplot(data = students, aes(x = dists)) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") +
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
