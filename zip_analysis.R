# Read data
zip_data = read.csv("nanda_ses_zcta_2008-2017_02P.csv")
source('read_data.R')

students = get_student_sub()
bg = get_student_background_data(students$mellon_id)

# Filter for the US
filter_not_us = bg$country_residence_app != "USA" | is.na(bg$country_residence_app)
mean(is.na(bg$country_residence_app))
mean(filter_not_us)

usa_bg = bg[!filter_not_us,]
mean(is.na(usa_bg$zip_code_app))

# Most frequent zip codes
zipcodes = as.data.frame(table(usa_bg$zip_code_app))
zipcodes[order(-zipcodes$Freq),]

# Cleaning function
normalize_zip = function(codes) {
  codes = gsub("^0+", "", codes)
  codes = sub("-.+", "", codes)
  codes = substr(codes,1,5)
  return(as.integer(codes))
}

# Matching Analysis
codes = normalize_zip(usa_bg$zip_code_app)
mean(codes %in% zip_data$zcta10)
length(codes[!(codes %in% zip_data$zcta10)])
unique(usa_bg$zip_code_app[!(codes %in% zip_data$zcta10)])
length(unique(usa_bg$zip_code_app[!(codes %in% zip_data$zcta10)]))

# Merging
bg$zcta10 = normalize_zip(bg$zip_code_app)
bg[filter_not_us,'zcta10']=NA
bg2 = merge(bg, zip_data[,c('zcta10','disadvantage08_12','disadvantage2_08_12','affluence08_12')], by="zcta10")
bg2$distance_from_home
bg2 = merge(bg2, students[,c("mellon_id","dropout")],by="mellon_id")
cor(bg2[,c('disadvantage08_12','disadvantage2_08_12','affluence08_12','dropout')], use="complete.obs")

cor(bg2[,c('distance_from_home','disadvantage08_12','disadvantage2_08_12','affluence08_12','dropout')], use="complete.obs")

bg3 = bg2[,c('distance_from_home','disadvantage08_12','disadvantage2_08_12','affluence08_12','dropout')]
bg3$dis_high = bg3$disadvantage08_12>=median(bg3$disadvantage08_12,na.rm=T)
bg3$aff_high = bg3$disadvantage08_12>=median(bg3$disadvantage08_12,na.rm=T)
library(ggplot2)

bg3$dists = pmin(1000, 15* round(as.integer(bg3$distance_from_home)/15))

dropout_rates = do.call(data.frame, aggregate(cbind(dropout,disadvantage08_12) ~dists, bg3, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates) +
  geom_bar(stat="identity",aes(x=dists, y=dropout.mean, fill="red", alpha = 0.4)) +
  geom_bar(stat="identity",aes(x=dists, y=disadvantage08_12.mean*2, fill="green", alpha = 0.4)) +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  labs(title = "",
       x = "Distance from home in miles",
       y = "") + ylim(0,0.33)+scale_fill_discrete(name = "Variable", labels = c("Dropout prob.", "Avg. disadvantage"))


disadv_dists = do.call(data.frame, aggregate(disadvantage08_12~dists, bg3, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = disadv_dists, aes(x=dists, y=disadvantage08_12.mean, fill=disadvantage08_12.n>46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") 
