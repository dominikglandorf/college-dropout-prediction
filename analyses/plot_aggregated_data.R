source('read_data.R')
data = get_aggregated_features()

colors = c("#8fc3f7", "white","#f73b3b")
rescaler <- function(x, from) {
  scales::rescale(x, from=from, to = c(0.05, 3))
}

# Create facet labels
amount_na = paste0(names(data), " (NA: ", round(colMeans(is.na(data))*100),"%)")
names(amount_na) = names(data)

# Extract numeric columns
numeric_cols <- data %>%
  select(where(is.numeric), dropout)

plot_numeric = function(data) {
  # Plot numeric columns as histograms
  print(data %>% 
    gather(key = "variable", value = "value", -dropout) %>% 
    mutate(dropout = as.numeric(dropout)) %>% 
    drop_na() %>%
    group_by(variable) %>% 
    mutate(grp = cut(value, breaks=10)) %>% 
    group_by(variable, grp) %>% 
    summarize(n=n(),
              dropout=mean(dropout)) %>% 
    # mutate(dropout = pmin(dropout, 0.3)) %>% 
    ggplot(aes(x=grp, y=n, fill=dropout)) +
    geom_bar(stat="identity") +
    facet_wrap(~variable, scales = "free", labeller=labeller(variable=amount_na)) +
    scale_fill_gradientn(colours = colors, rescaler=rescaler)+
    geom_text(aes(y=n/2, label = scales::percent(dropout, accuracy = 1, trim = FALSE)), size=3) +
    theme(axis.text.x=element_blank(), #remove x axis labels
          axis.ticks.x=element_blank(), #remove x axis ticks
          axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank()  #remove y axis ticks
    ))
}

plot_numeric(numeric_cols %>% select(1:16, dropout))
plot_numeric(numeric_cols %>% select(16:31, dropout))
 

# Extract non-numeric columns
non_numeric_cols <- data %>%
  select(where(function(x) !is.numeric(x)))

# Plot non-numeric columns as bar plots
data3 = non_numeric_cols %>% 
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
  facet_wrap(~variable, scales = "free", labeller=labeller(variable=amount_na))+
  scale_fill_gradientn(colours = colors, rescaler=rescaler) +
  theme(axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) +  #remove y axis ticks
  scale_x_discrete(label=function(x) stringr::str_trunc(x, 7))

data3 %>% 
  filter(str_detect(variable, 'major|school') & variable!="any_major_stem") %>% 
  ggplot(aes(x = value, y=n, fill=dropout)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=n/2, label = scales::percent(dropout, accuracy = 1, trim = FALSE)), size=3, angle = 90) +
  facet_wrap(~variable, scales = "free", labeller=labeller(variable=amount_na))+
  scale_x_discrete(label=function(x) stringr::str_trunc(x, 15)) +
  scale_fill_gradientn(colours = colors, rescaler=rescaler) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) +
  labs(y="")
