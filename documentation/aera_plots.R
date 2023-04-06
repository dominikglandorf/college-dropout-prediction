if(!require(ggplot2)) install.packages('ggplot2')

aera_theme = theme_minimal() +
  theme(plot.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 18, color="black"),
        axis.text = element_text(size = 14, color="black"),
        axis.text.x = element_text(vjust = +2.5),
        axis.text.y = element_text(hjust = +1.25))


source('read_data.R')
students = get_student_sub()

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
term_features = get_term_features()
term_features = term_features %>% merge(students %>% select(mellon_id, dropout))
ggplot(term_features %>% filter(!is.na(dropout)), aes(x = term_num, fill = dropout)) +
  aera_theme +
  geom_histogram(binwidth = 1) +
  labs(title="Dropout vs non-dropouts over terms",
       x = "Term number",
       y = "Number of students",
       fill = "Dropout") +
  scale_fill_manual(values = c("#7bba70", "#db6d6b"),
                    labels = c("No", "Yes"))+
  xlim(0, 16)

## demographics ##
# gender
ggplot(data = students %>% filter(!is.na(dropout)), aes(x = female, fill = dropout)) +
  geom_bar(aes(y =(..count..)/sum(..count..))) +
  aera_theme +
  labs(title = "Distribution of gender",
       x = "Gender",
       y = "Relative frequency",
       fill="Dropout") +
  scale_fill_manual(values = c("#7bba70", "#db6d6b"),
                    labels = c("No", "Yes"))+
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels=c("Non-female", "Female", "Unknown"))

# ethnicity
ggplot(data = students %>% filter(!is.na(dropout)), aes(x = ethnicity_smpl, fill = dropout)) +
  geom_bar(aes(y =(..count..)/sum(..count..))) +
  aera_theme +
  labs(title = "Distribution of ethnicity",
       x = "Ethnicity",
       y = "Relative frequency",
       fill="Dropout") +
  scale_fill_manual(values = c("#7bba70", "#db6d6b"),
                    labels = c("No", "Yes"))+
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels=c("Asian", "Black", "Hispanic", "Indigenous", "White", "Unknown"))

# first generation status
ggplot(data = students %>% filter(!is.na(dropout)), aes(x = as.character(first_generation), fill = dropout)) +
  geom_bar(aes(y =(..count..)/sum(..count..))) +
  aera_theme +
  labs(title = "Distribution of first-generation status",
       x = "First generation status",
       y = "Relative frequency",
       fill="Dropout") +
  scale_fill_manual(values = c("#7bba70", "#db6d6b"),
                    labels = c("No", "Yes"))+
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels=c("Non first-generation", "First-generation", "Unknown"))

# low income
ggplot(data = students %>% filter(!is.na(dropout) & !is.na(low_income)), aes(x = as.character(low_income), fill = dropout)) +
  geom_bar(aes(y =(..count..)/sum(..count..))) +
  aera_theme +
  labs(title = "Distribution of low-income students",
       x = "Low income student",
       y = "Relative frequency",
       fill="Dropout") +
  scale_fill_manual(values = c("#7bba70", "#db6d6b"),
                    labels = c("No", "Yes"))+
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels=c("No", "Yes", "Unknown"))
  