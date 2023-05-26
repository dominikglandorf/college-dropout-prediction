# read data about terms
source('read_data.R')
students = get_student_sub()
students %>%
  select(where(~ any(is.na(.))), cohort) %>% 
  group_by(cohort) %>%
  summarize(across(everything(), ~ mean(is.na(.)))) %>% 
  pivot_longer(cols = -cohort, names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = as.factor(cohort), y = value, fill=cohort<=2010)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ variable, ncol = 4) +
    xlab("Variable") +
    ylab("NA") +
    ggtitle("Missingness by cohorts and variable")+
  scale_y_continuous(labels = scales::percent_format())
