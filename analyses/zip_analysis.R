# Read data
setwd("~/EWS")
zip_data = read.csv("data/nanda_ses_zcta_2008-2017_02P.csv")
source('read_data.R')

students = get_student_sub()
bg = get_student_background_data(students$mellon_id)

# Filter for the US
mean(is.na(bg$country_residence_app))
filter_not_us = bg$country_residence_app != "USA" | is.na(bg$country_residence_app)
mean(filter_not_us)

usa_bg = bg[!filter_not_us,]
mean(is.na(usa_bg$zip_code_app))

# Most frequent zip codes
zipcodes = as.data.frame(table(usa_bg$zip_code_app))
zipcodes[order(-zipcodes$Freq),]

# Cleaning function
normalize_zip = function(codes) {
  leading_zeros = "^0+"
  codes = gsub(leading_zeros, "", codes)
  dash_and_remainder = "-.+"
  codes = sub(dash_and_remainder, "", codes)
  codes = substr(codes,1,5) # maximum length five
  return(as.integer(codes))
}

# Matching Analysis
codes = normalize_zip(usa_bg$zip_code_app)
mean(codes %in% zip_data$zcta10)
length(codes[!(codes %in% zip_data$zcta10)])
unique(codes[!(codes %in% zip_data$zcta10)])

# Merging
bg$zcta10 = normalize_zip(bg$zip_code_app)
bg[filter_not_us,'zcta10']=NA
bg2 = merge(bg, zip_data[,c('zcta10','disadvantage08_12','disadvantage2_08_12','affluence08_12')], by="zcta10")
bg2$distance_from_home
bg2 = merge(bg2, students[,c("mellon_id","dropout")],by="mellon_id")
cor(bg2[,c('disadvantage08_12','disadvantage2_08_12','affluence08_12','dropout')], use="complete.obs")


summary(bg2[,c('disadvantage08_12','disadvantage2_08_12','affluence08_12','dropout')])

cor(bg2[,c('distance_from_home','disadvantage08_12','disadvantage2_08_12','affluence08_12','dropout')], use="complete.obs")

bg3 = bg2[,c('distance_from_home','disadvantage08_12','disadvantage2_08_12','affluence08_12','dropout')]
bg3$dis_high = bg3$disadvantage08_12>=median(bg3$disadvantage08_12,na.rm=T)
bg3$aff_high = bg3$disadvantage08_12>=median(bg3$disadvantage08_12,na.rm=T)

library(ggplot2)
bg3$dists = pmin(1000, 25* round(as.integer(bg3$distance_from_home)/25))

dropout_rates = do.call(data.frame, aggregate(cbind(dropout,disadvantage08_12) ~dists, bg3, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = dropout_rates) +
  geom_bar(stat="identity",aes(x=dists, y=dropout.mean, fill="red", alpha = 0.4)) +
  geom_bar(stat="identity",aes(x=dists, y=disadvantage08_12.mean*2, fill="green", alpha = 0.4)) +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  labs(title = "",
       x = "Distance from home in miles",
       y = "") + ylim(0,0.33)+scale_fill_discrete(name = "Variable", labels = c("Dropout prob.", "Avg. disadvantage"))
bg2

disadv_dists = do.call(data.frame, aggregate(disadvantage08_12~dists, bg3, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = disadv_dists, aes(x=dists, y=disadvantage08_12.mean, fill=disadvantage08_12.n>46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") 

# Low income to distance from home
bg2$dists = pmin(1000, 15* round(as.integer(bg2$distance_from_home)/15))
bg2$low_inc = bg2$low_income == "yes"
bgtest = bg2[!is.na(bg2$low_inc) & !is.na(bg2$dists),c("low_inc","dists")]
low_inc_dists = do.call(data.frame, aggregate(low_inc~dists, bgtest, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = low_inc_dists, aes(x=dists, y=low_inc.mean, fill=low_inc.n>46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") 

# First generation to distance from home
fg_dists = do.call(data.frame, aggregate(first_generation~dists, bg2, FUN = function(x) c(mean = mean(x), n = length(x))))
ggplot(data = fg_dists, aes(x=dists, y=first_generation.mean, fill=first_generation.n>46)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(title = "",
       x = "",
       y = "") 


table(bg2$zcta10[(bg2$distance_from_home>350) & (bg2$distance_from_home<375)])
bg2$zcta10
# Use ZIP Code to predict dropout
zip_dropout = do.call(data.frame, aggregate(dropout~zcta10, bg2, FUN = function(x) c(mean = mean(x), n = length(x))))
zip_dropout2 = zip_dropout[zip_dropout$dropout.n>46,]
zip_dropout2 = merge(zip_dropout2, zip_data[,c('zcta10','disadvantage08_12','disadvantage2_08_12','affluence08_12')], by="zcta10")
zip_dropout2[order(zip_dropout2$dropout.mean),c('zcta10','dropout.mean','disadvantage08_12')]
plot(1:nrow(zip_dropout2),zip_dropout2[order(zip_dropout2$dropout.mean),c('disadvantage08_12')])
plot(1:nrow(zip_dropout2),zip_dropout2[order(zip_dropout2$dropout.mean),c('disadvantage2_08_12')])
zip_dropout2[order(zip_dropout2$dropout.mean),c('zcta10','dropout.mean','affluence08_12')]
plot(1:nrow(zip_dropout2),zip_dropout2[order(zip_dropout2$dropout.mean),c('affluence08_12')])

# highest: 90003 around Broadway Manchester, LA. 38% dropout, 0.7% white population, 50% average salary of states average, average age 28 years
# lowest: 91306 Winnetka 1.7% dropout, interestingly not really above average in terms of demographics
# another very low: 91770 2.6% dropout  Rosemead, mainly Asian, only 1.3% unmarried compared to 49% in state. median age 42 vs 37

# maybe highschool is also indicative?
mean(is.na(bg2$hs_name))
hs_dropout = bg2 %>%
  group_by(hs_name) %>%
  summarise(mdropout=mean(dropout,na.rm=T),
            n=sum(!is.na(dropout)),
  ) %>%
  filter(n>46) %>%
  arrange(mdropout)
# same observation as with zip code

bg2$zip3 = substring(bg2$zcta10,1,3)

## aggregate all demographic variables to see if there is a pattern
zip_dropout = bg2 %>%
  group_by(zcta10) %>%
  summarise(mdropout=mean(dropout,na.rm=T),
            n=sum(!is.na(dropout)),
            aa=mean(ethnicity=="Asian / Asian American", na.rm=T)) %>%
  filter(n>46) %>%
  arrange(-mdropout)%>%ggplot() + geom_point(aes(x=aa, y=mdropout))



table(bg2$ethnicity)
zip_dropout = do.call(data.frame, aggregate(dropout~zcta10, bg2, FUN = function(x) c(mean = mean(x), n = length(x))))

sim = function() map_int(zip_dropout$n, \(x) rbinom(1, x, 0.12)) / zip_dropout$n
plot(density(sim()))
for ( i in 1:50) lines(density(sim()))
lines(density(zip_dropout$mdropout), col="red")
