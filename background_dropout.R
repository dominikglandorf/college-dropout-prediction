source('read_data.R')
students = get_student_sub()
dim(students) # 46408 x 25

mean(is.na(students$female)) # same as before

ggplot(data = students, aes(x = as.integer(female == "yes"))) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of gender",
       x = "Is female",
       y = "Frequency") +
  theme(text = element_text(size = 16))

is_female = as.integer(students$female == "yes")
dropout_rates_female = do.call(data.frame, aggregate(dropout ~ is_female, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates_female, aes(x=is_female, y=dropout.mean, fill="blue")) +
  geom_bar(stat="identity") +
  theme_minimal() +
  #geom_text(aes(label=round(x,2)), vjust = -0.5) + 
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by UC total score",
       x = "Score",
       y = "Dropout rate")

mean(is.na(students$int_student)) # same as before

ggplot(data = students, aes(x = as.integer(int_student))) +
  geom_bar(data=subset(students,dropout == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(students,dropout == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of international students",
       x = "International student",
       y = "Frequency") +
  theme(text = element_text(size = 16))

is_female = as.integer(students$female == "yes")
dropout_rates_female = do.call(data.frame, aggregate(dropout ~ is_female, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates_female, aes(x=is_female, y=dropout.mean, fill="blue")) +
  geom_bar(stat="identity") +
  theme_minimal() +
  #geom_text(aes(label=round(x,2)), vjust = -0.5) + 
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  theme(legend.position = "none") +
  labs(title = "Dropout rates by UC total score",
       x = "Score",
       y = "Dropout rate")

