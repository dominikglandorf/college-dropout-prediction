if(!require(ggplot2)) install.packages('ggplot2')

# prepare theming
aera_theme = theme_minimal() +
  theme(plot.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 18, color="black"),
        axis.text = element_text(size = 14, color="black"),
        axis.text.x = element_text(vjust = +2.5),
        axis.text.y = element_text(hjust = +1.25))
red = "#DC3220"
blue = "#005AB5"

# prepare data
source('read_data.R')
students = get_student_sub()
term_features = get_term_features()
term_features = term_features %>% merge(students %>% select(mellon_id, dropout))

# dropout rate by cohort
dropout_rates = aggregate(dropout ~ admitdate, students, FUN=function(x) mean(x, na.rm=T))
dropout_rates$admitdate = paste0("20", substring(dropout_rates$admitdate, 2))
ggplot(data = dropout_rates, aes(x=admitdate, y=dropout)) +
  geom_bar(stat="identity") +
  aera_theme +
  labs(title = "Dropout by cohort",
       x = "Admission year",
       y = "Dropout rate") +
  scale_y_continuous(labels = scales::percent_format())

# dropout vs non-dropout over terms
ggplot(term_features %>% filter(!is.na(dropout)), aes(x = term_num, fill = dropout)) +
  aera_theme +
  geom_histogram(binwidth = 1) +
  labs(title="Dropout vs non-dropouts over terms",
       x = "Term number",
       y = "Number of students",
       fill = "Dropout") +
  scale_fill_manual(values = c(blue, red),
                    labels = c("No", "Yes"))+
  xlim(0, 16)

## demographics ##
# gender
ggplot(data = students %>% filter(!is.na(dropout)), aes(x = female, fill = dropout)) +
  geom_bar(aes(y =(..count..)/sum(..count..))) +
  aera_theme +
  labs(title = "Distribution of gender",
       x = "",
       y = "",
       fill="Dropout") +
  scale_fill_manual(values = c(blue, red),
                    labels = c("No", "Yes"))+
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels=c("Non-female", "Female", "Unknown"))

# ethnicity
ggplot(data = students %>% filter(!is.na(dropout)), aes(x = ethnicity_smpl, fill = dropout)) +
  geom_bar(aes(y =(..count..)/sum(..count..))) +
  aera_theme +
  labs(title = "Distribution of ethnicity",
       x = "",
       y = "",
       fill="Dropout") +
  scale_fill_manual(values = c(blue, red),
                    labels = c("No", "Yes"))+
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels=c("Asian", "Black", "Hispanic", "Indigenous", "White", "Unknown"))

# first generation status
ggplot(data = students %>% filter(!is.na(dropout)), aes(x = as.character(first_generation), fill = dropout)) +
  geom_bar(aes(y =(..count..)/sum(..count..))) +
  aera_theme +
  labs(title = "Distribution of first-generation status",
       x = "",
       y = "",
       fill="Dropout") +
  scale_fill_manual(values = c(blue, red),
                    labels = c("No", "Yes"))+
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels=c("Non first-generation", "First-generation", "Unknown"))

# low income
ggplot(data = students %>% filter(!is.na(dropout) & !is.na(low_income)), aes(x = as.character(low_income), fill = dropout)) +
  geom_bar(aes(y =(..count..)/sum(..count..))) +
  aera_theme +
  labs(title = "Distribution of low-income students",
       x = "",
       y = "",
       fill="Dropout") +
  scale_fill_manual(values = c(blue, red),
                    labels = c("No", "Yes"))+
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels=c("No", "Yes", "Unknown"))

## RQ2: dropout rate by UC total score
students$total_score_buckets = 5*round(students$uc_total_score/5)
ggplot(students %>% filter(!is.na(dropout)), aes(x=total_score_buckets, fill=dropout)) +
  geom_bar() +
  aera_theme +
  scale_fill_manual(values = c(blue, red),
                    labels = c("No", "Yes"))+
  labs(title = "Distribution of UC total scores",
       x = "Score",
       y = "",
       fill = "Dropout") +
  xlim(80, 300)

dropout_rates_total_score = do.call(data.frame, aggregate(dropout ~ total_score_buckets, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(dropout_rates_total_score %>% filter(dropout.n>46), aes(x=total_score_buckets, y=dropout.mean)) +
  geom_bar(stat="identity", fill=red) +
  aera_theme +
  labs(title = "Dropout rates by UC total score",
       x = "Score",
       y = "Dropout rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  xlim(80, 300)

# distance from home
students$dist_home_buckets = 10*round(students$distance_from_home/10)
ggplot(students %>% filter(!is.na(dropout)), aes(x=dist_home_buckets, fill=dropout)) +
  geom_bar() +
  aera_theme +
  scale_fill_manual(values = c(blue, red),
                    labels = c("No", "Yes"))+
  labs(title = "Distribution of distance from home",
       x = "Distance from home [miles]",
       y = "",
       fill = "Dropout") +
  xlim(0, 500)

dropout_rates_dist_home = do.call(data.frame, aggregate(dropout ~ dist_home_buckets, students, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(dropout_rates_dist_home %>% filter(dropout.n>46), aes(x=dist_home_buckets, y=dropout.mean)) +
  geom_bar(stat="identity", fill=red) +
  aera_theme +
  labs(title = "Dropout rates by distance from home",
       x = "Distance from home [miles]",
       y = "Dropout rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  xlim(0, 500)

## RQ3: dropout rate by major
term_features_1 = subset(term_features, term_num==1)
dropout_rates_major = do.call(data.frame, aggregate(dropout ~ major_1, term_features_1, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates_major %>% filter(dropout.n > 46) %>% arrange(desc(dropout.mean)) %>%
  mutate(major_1=fct_reorder(major_1,dropout.mean)), aes(x=major_1, y=dropout.mean)) +
  geom_bar(stat="identity", fill=red) +
  aera_theme +
  theme(axis.text.y = element_text(size=12, angle = 0, hjust=1, vjust = 0.5)) +
  labs(title = "Dropout rates by major declared for first term",
       x = "",
       y = "Dropout rate")  +
  scale_y_continuous(labels = scales::percent_format())+
  coord_flip()


  