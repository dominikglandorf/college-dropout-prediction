setwd("~/EWS")
source('read_data.R')
data = get_data_to_impute(3)
source('models/predictor_descriptions.R')

colors = c("#8fc3f7", "white","#f73b3b")
rescaler <- function(x, from) {
  scales::rescale(x, from=from, to = c(0.05, 3))
}

# Create facet labels
my_label = function(names) {
  amount_na = round(colMeans(is.na(data[,names])) * 100)
  return (paste0(translate_predictor_names(names), " (NA: ", amount_na, "%)"))
}

# Extract numeric columns
numeric_cols <- data %>%
  select(where(is.numeric), dropout)

lower_limit = function(x) {
  pos_comma = str_locate(x, ",")[1, "start"]
  lower = substr(x, 2, pos_comma-1)
  return(paste0(">", lower))
}

lower_limit_list = function(x) {
  return(lapply(x, lower_limit))
}

plot_numeric = function(data, levels) {
  # Plot numeric columns as histograms
  print(data %>% 
    gather(key = "variable", value = "value", -dropout) %>% 
    mutate(dropout = as.numeric(dropout),
           variable = factor(variable, levels=levels)) %>% 
    drop_na() %>%
    group_by(variable) %>% 
    mutate(grp = cut(value, breaks=10)) %>% 
    group_by(variable, grp) %>% 
    summarize(n=n(),
              dropout=mean(dropout)) %>%
    ggplot(aes(x=grp, y=n, fill=dropout)) +
    geom_bar(stat="identity") +
    facet_wrap(~variable, scales = "free", labeller=labeller(variable=my_label), ncol=2) +
    scale_fill_gradientn(colours = colors, rescaler=rescaler,
                         labels = scales::percent_format(scale = 100, accuracy = 1)  # This line formats the labels as percentages
    )+
    geom_text(aes(y=n*0.5+1000, label = scales::percent(dropout, accuracy = 1, trim = FALSE)), size=3) +
    theme(#axis.text.x=element_text(angle = 45, hjust = 1), #remove x axis labels
          axis.ticks.x=element_blank(), #remove x axis ticks
          axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank(),  #remove y axis ticks
    legend.position = "bottom")) +
    scale_x_discrete(label=lower_limit_list) +
    labs(y="Frequency", x="Interval", fill="Dropout rate")
}

translated_names <- sapply(names(numeric_cols), translate_predictor_names)
names(translated_names) <- names(numeric_cols)

# Order the columns based on the translated names
sorted_cols <- numeric_cols[, order(translated_names)]

plot_numeric(sorted_cols %>% select(1:13, dropout), names(numeric_cols)[order(translated_names)[1:13]])
plot_numeric(sorted_cols %>% select(14:25, dropout), names(numeric_cols)[order(translated_names)[14:25]])
plot_numeric(sorted_cols %>% select(26:30, dropout), names(numeric_cols)[order(translated_names)[26:30]])
 

# Extract non-numeric columns
non_numeric_cols <- data %>%
  select(where(function(x) !is.numeric(x)))

translated_names <- sapply(names(non_numeric_cols), translate_predictor_names)
names(translated_names) <- names(non_numeric_cols)

# Order the columns based on the translated names
sorted_cols <- non_numeric_cols[, order(translated_names)]

# Plot non-numeric columns as bar plots
data3 = sorted_cols %>% 
  gather(key = "variable", value = "value", -dropout) %>% 
  group_by(variable, value) %>% 
  summarize(n=n(),
            dropout=mean(dropout))# %>% 
  mutate(dropout = pmin(dropout, 0.3))
  
data3 %>% 
  filter(!str_detect(variable, 'major|school') | variable=="any_major_stem") %>% 
  ggplot(aes(x = value, y=n, fill=dropout)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=n/2, label = scales::percent(dropout, accuracy = 1, trim = FALSE)), size=3) +
  facet_wrap(~variable, scales = "free", labeller=labeller(variable=my_label))+
  scale_fill_gradientn(colours = colors, rescaler=rescaler) +
  theme(axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        legend.position = "bottom")+
  scale_x_discrete(label=function(x) stringr::str_trunc(x, 7))+
  labs(y="Frequency", x="Value", fill="Dropout rate")

data3 %>% 
  filter(str_detect(variable, 'major|school') & variable!="any_major_stem") %>% 
  ggplot(aes(x = value, y=n, fill=dropout)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=n/2, label = scales::percent(dropout, accuracy = 1, trim = FALSE)), size=3, angle = 90) +
  facet_wrap(~variable, scales = "free", labeller=labeller(variable=my_label))+
  scale_x_discrete(label=function(x) stringr::str_trunc(x, 25)) +
  scale_fill_gradientn(colours = colors, rescaler=rescaler) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(), 
        legend.position = "bottom")+
  labs(y="Frequency", x="Value", fill="Dropout rate")
