if(!require(ggcorrplot)) install.packages('ggcorrplot')
source('read_data.R')
student_background_data = get_student_background_data()
student_sub = get_student_sub()

bg_sub = merge(student_background_data, student_sub[,c("mellon_id","admitdate")], by="mellon_id")

# pre university scores
sat_cols = c("sat_math_score", "sat_verb_score" ,"sat_writing_score","sat_total_score")
summary(bg_sub[,sat_cols])
sat_not_NA = as.data.frame(!is.na(bg_sub[,sat_cols]))
sat_not_NA$sat_count_non_NA = rowSums(sat_not_NA)
summary(bg_sub[sat_not_NA$sat_count_non_NA==1,sat_cols])
summary(bg_sub[sat_not_NA$sat_count_non_NA==2,sat_cols])
cor(bg_sub[sat_not_NA$sat_count_non_NA==2,c("sat_writing_score","sat_total_score")], use="complete.obs")
summary(bg_sub[sat_not_NA$sat_count_non_NA==3,sat_cols])
cor(bg_sub[sat_not_NA$sat_count_non_NA==3,c("sat_writing_score","sat_math_score","sat_total_score")], use="complete.obs")
cor(bg_sub[sat_not_NA$sat_count_non_NA==3,"sat_total_score"],bg_sub[sat_not_NA$sat_count_non_NA==3,"sat_writing_score"]+bg_sub[sat_not_NA$sat_count_non_NA==3,"sat_math_score"], use="complete.obs")

ggcorrplot(cor(bg_sub[sat_not_NA$sat_count_non_NA==4,sat_cols], use="complete.obs"), type = "lower", lab = TRUE)

ggplot(data = sat_not_NA, aes(x = sat_count_non_NA)) +
  geom_bar() +
  theme_minimal() +
  theme(text=element_text(size=16)) +
  geom_text(aes(label = ..count..), stat = "count", nudge_y = 400) +
  labs(title = "How many SAT scores do we know?",
       x = "number of scores",
       y = "")

act_cols = c("act_english_score","act_math_score","act_reading_score","act_scireason_score","act_write_score","act_total_score")
summary(bg_sub[,act_cols])
act_not_NA = as.data.frame(!is.na(bg_sub[,act_cols]))
act_not_NA$act_count_non_NA = rowSums(act_not_NA)
ggplot(data = act_not_NA, aes(x = act_count_non_NA)) +
  geom_bar() +
  theme_minimal() +
  theme(text=element_text(size=16)) +
  geom_text(aes(label = ..count..), stat = "count", nudge_y = 700) +
  labs(title = "How many ACT scores do we know?",
       x = "number of scores",
       y = "")
ggcorrplot(cor(bg_sub[act_not_NA$act_count_non_NA==6,act_cols], use="complete.obs"), type = "lower", lab = TRUE)

uc_cols = c("uc_read_score","uc_math_score","uc_writing_score","uc_total_score")
summary(bg_sub[,uc_cols])
ggplot(bg_sub, aes(x=uc_total_score)) + 
  geom_histogram(binwidth=5)
bg_sub$uc_read_score[bg_sub$uc_read_score>100] = NA
bg_sub$uc_math_score[bg_sub$uc_math_score>100] = NA
bg_sub$uc_total_score[bg_sub$uc_total_score>300] = NA

uc_not_NA = as.data.frame(!is.na(bg_sub[,uc_cols]))
uc_not_NA$uc_count_non_NA = rowSums(uc_not_NA)
ggplot(data = uc_not_NA, aes(x = uc_count_non_NA)) +
  geom_bar() +
  theme_minimal() +
  theme(text=element_text(size=16)) +
  geom_text(aes(label = ..count..), stat = "count", nudge_y = 800) +
  labs(title = "How many UC scores do we know?",
       x = "number of scores",
       y = "")
ggcorrplot(cor(bg_sub[,uc_cols], use="complete.obs"), type = "lower", lab = TRUE)

summary(bg_sub[,"toefl_score"])
summary(bg_sub[,"ielts_score"])

pre_not_NA = data.frame(act = act_not_NA$act_count_non_NA>0, sat = sat_not_NA$sat_count_non_NA>0, uc=uc_not_NA$uc_count_non_NA>0)
pre_not_NA$sum = rowSums(pre_not_NA)
ggplot(data = pre_not_NA, aes(x = sum)) +
  geom_bar() +
  theme_minimal() +
  theme(text=element_text(size=16)) +
  geom_text(aes(label = ..count..), stat = "count", nudge_y = 700) +
  labs(title = "How many tests do we know scores from?",
       x = "number of scores",
       y = "")


scaled_scores = data.frame(act=bg_sub$act_total_score/36,sat=bg_sub$sat_total_score/2400,uc=bg_sub$uc_total_score/300)
num_scores = rowSums(!is.na(scaled_scores))

# this calcualtion does not make much sense
total_score = rowSums(scaled_scores, na.rm=T) / num_scores

ggplot() +
  aes(total_score) +
  geom_histogram() +
  labs(title = "Distribution of total score")
summary(total_score) / length(total_score)

scores = data.frame(bg_sub[,c("act_total_score","sat_total_score","uc_total_score")], t=total_score)
scores[30:40,]

scaled_math_scores = data.frame(act=bg_sub$act_math_score/36,sat=bg_sub$sat_math_score / 800,uc=bg_sub$uc_math_score / 100)
num_math_scores = rowSums(!is.na(scaled_math_scores))
math_score = rowSums(scaled_math_scores, na.rm=T) / num_math_scores

ggplot() +
  aes(math_score) +
  geom_histogram() +
  labs(title = "Distribution of math score")
summary(math_score) / length(math_score)

math_scores = data.frame(bg_sub[,c("act_math_score","sat_math_score","uc_math_score")], m=math_score)
math_scores[30:50,]

scaled_writing_scores = data.frame(act=bg_sub$act_write_score/36,sat=bg_sub$sat_writing_score / 800,uc=bg_sub$uc_writing_score / 100)
num_writing_scores = rowSums(!is.na(scaled_writing_scores))
writing_score = rowSums(scaled_writing_scores, na.rm=T) / num_writing_scores

ggplot() +
  aes(writing_score) +
  geom_histogram() +
  labs(title = "Distribution of writing score")
summary(writing_score) / length(writing_score)

writing_scores = data.frame(bg_sub[,c("act_write_score","sat_writing_score","uc_writing_score")], w=writing_score)
writing_scores[40070:41090,]

compound_scores = data.frame(student_sub, total=total_score, math=math_score, writing=writing_score)

ggplot(data = compound_scores, aes(x=admitdate, fill=!is.na(total))) +
  geom_bar() +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "Total score availability by admission",
       x = "Admission",
       y = "Number of scores",
       fill = "Available")

ggplot(data = compound_scores, aes(x=admitdate, fill=!is.na(math))) +
  geom_bar() +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "Math score availability by admission",
       x = "Admission",
       y = "Number of scores",
       fill = "Available")

ggplot(data = compound_scores, aes(x=admitdate, fill=!is.na(writing))) +
  geom_bar() +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "Writing score availability by admission",
       x = "Admission",
       y = "Number of scores",
       fill = "Available")

ggplot(data = bg_sub, aes(x=admitdate.x, fill=!is.na(uc_total_score))) +
  geom_bar() +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "UC total score availability by admission",
       x = "Admission",
       y = "Number of scores",
       fill = "Available")

ggplot(data = bg_sub, aes(x=admitdate.x, fill=!is.na(uc_total_score))) +
  geom_bar() +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "UC total score availability by admission",
       x = "Admission",
       y = "Number of scores",
       fill = "Available")

ggplot(data = bg_sub, aes(x=admitdate.x, fill=(!is.na(act_total_score)|!is.na(sat_total_score)))) +
  geom_bar() +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "SAT/ACT total score availability by admission",
       x = "Admission",
       y = "Number of scores",
       fill = "Available")

ggplot(data = bg_sub, aes(x=admitdate.x, fill=!is.na(uc_read_score))) +
  geom_bar() +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "UC reading score availability by admission",
       x = "Admission",
       y = "Number of scores",
       fill = "Available")

ggplot(data = bg_sub, aes(x=admitdate.x, fill=(!is.na(act_reading_score)|!is.na(sat_verb_score)))) +
  geom_bar() +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "SAT/ACT reading score availability by admission",
       x = "Admission",
       y = "Number of scores",
       fill = "Available")

ggplot(data = bg_sub, aes(x=admitdate.x, fill=!is.na(uc_writing_score))) +
  geom_bar() +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "UC writing score availability by admission",
       x = "Admission",
       y = "Number of scores",
       fill = "Available")

ggplot(data = bg_sub, aes(x=admitdate.x, fill=(!is.na(act_write_score)|!is.na(sat_writing_score)))) +
  geom_bar() +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "SAT/ACT reading score availability by admission",
       x = "Admission",
       y = "Number of scores",
       fill = "Available")

ggplot(data = bg_sub, aes(x=admitdate.x, fill=!is.na(uc_math_score))) +
  geom_bar() +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "UC math score availability by admission",
       x = "Admission",
       y = "Number of scores",
       fill = "Available")

ggplot(data = bg_sub, aes(x=admitdate.x, fill=(!is.na(act_math_score)|!is.na(sat_math_score)))) +
  geom_bar() +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "SAT/ACT math score availability by admission",
       x = "Admission",
       y = "Number of scores",
       fill = "Available")


dropout_scores = aggregate(compound_scores[,c("total","math","writing")], by=list(student_sub$dropout),FUN=function(x) {
  x[is.na(x)]=F
  return(mean(x))
})
library(reshape2)
means.long<-melt(dropout_scores,id.vars="Group.1")
ggplot(data = means.long, aes(x=variable,y=value,fill=factor(Group.1))) +
  geom_bar(stat="identity",position="dodge") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.key.size = unit(0.3, 'cm'),
        axis.text.x=element_text(size=10,angle=90, hjust=1,vjust=0.5)) +
  labs(title = "Pre-university test score by dropout",
       x = "Test",
       y="Score",
       fill="Dropout")

model =lm(uc_total_score ~ sat_total_score + act_total_score, bg_sub)
summary(model)
summary(lm(sat_total_score ~ act_total_score, bg_sub))

data = bg_sub[!is.na(bg_sub$uc_total_score) & rowSums(!is.na(bg_sub[,act_cols])==0),c(uc_cols,sat_cols,act_cols)]
lm(uc_total_score ~ act_total_score, data)
data[order(-data$uc_math_score)[900:1100],c("uc_math_score","act_math_score","act_scireason_score","sat_math_score")]
cor(data)
scores = bg_sub[,c(uc_cols,act_cols,sat_cols)]
names(scores)=substr(names(scores),1,7)
sort_act = scores[order(scores$sat_mat),]
sort_act[,]
ucests = data.frame(est_uc = 0.1 * scores[is.na(scores$act_tot),]$sat_to + 60, uc=scores[is.na(scores$act_tot),]$uc_tota)
table(ucests$uc == ucests$est_uc)


# UC_read=0.1*sat_ver+20, UC_mat=0.1*sat_mat+20, UC_writ=0.1*sat_wri+20

# UC_read=2*act_read+28, UC_mat=2*act_mat+28, UC_writ=2*act_wri+28
# 51, 11
# 53, 12
# 54, 13
# 56, 14
# 58, 15
# 60, 16
# 62, 17
# 63, 18
# 65, 19
# 67, 20
# 69, 21
# 71, 22
# 73, 23
# 75, 24
# 77, 25
# 79, 26
# 81, 27
# 83, 28
# 85, 29
# 87, 30
# 89, 31
# 92, 32
# 94, 33
# 97, 34
# 100, 35

d=c(51,53,54,56,58,60,62,63,65,67,69,71,73,75,77,79,81,83,85,87,89,92,94,97,100)

c=11:35

plot(c,d, xlab="ACT_score", ylab="UC_score", pch=4, main="Relationship between ACT and UC scores")
lm(d ~ c)
lines(c,1.993*c+27.799)
lines(c,round(1.993*c+27.8), col="red")
lines(c,round(2*c+28), col="red")
d-round(2*c+28)

scores

# try to recover scores
ucests = data.frame(est_comb = 0.5 * (0.1 * scores$sat_mat + 20 + 2 * scores$act_mat + 28),
                    est_sat = 0.1 * scores$sat_mat + 20,
                    est_act = 2 * scores$act_mat + 28,
                    uc=scores$uc_mat)
hist(ucests$est_comb - ucests$uc, breaks=50, main="Deviation of combined estimate", xlab="Math estimate minus UC Math score")
median(ucests$est_comb - ucests$uc, na.rm=T)
mean(ucests$est_comb - ucests$uc, na.rm=T)
sd(ucests$est_comb - ucests$uc, na.rm=T)
table(ucests$uc[!is.na(scores$uc_math)] == round(ucests$est_comb[!is.na(scores$uc_math)]))/(8540+741)

hist(ucests$est_sat - ucests$uc, breaks=50, main="Deviation of SAT estimate", xlab="Math estimate minus UC Math score")
median(ucests$est_sat - ucests$uc, na.rm=T)
mean(ucests$est_sat - ucests$uc, na.rm=T)
sd(ucests$est_sat - ucests$uc, na.rm=T)
table(ucests$uc[!is.na(scores$uc_math)&is.na(scores$act_mat)] == ucests$est_sat[!is.na(scores$uc_math)&is.na(scores$act_mat)])/length(ucests$uc[!is.na(scores$uc_math)&is.na(scores$act_mat)])

hist(ucests$est_act - ucests$uc, breaks=50, main="Deviation of ACT estimate", xlab="Math estimate minus ACT Math score")
median(ucests$est_act - ucests$uc, na.rm=T)
mean(ucests$est_act - ucests$uc, na.rm=T)
sd(ucests$est_act - ucests$uc, na.rm=T)

table(ucests$uc[!is.na(scores$uc_math)&is.na(scores$sat_mat)] == ucests$est_act[!is.na(scores$uc_math)&is.na(scores$sat_mat)])/length(ucests$uc[!is.na(scores$uc_math)&is.na(scores$sat_mat)])


# how many scores could we actually recover?
# = number of students where act or sat is known but UC score is missing
nrow(scores[is.na(scores$uc_mat),])
nrow(scores[is.na(scores$uc_mat) & (!is.na(scores$act_mat) | !is.na(scores$sat_mat)),])
nrow(scores[is.na(scores$uc_writ),])
nrow(scores[is.na(scores$uc_writ) & (!is.na(scores$act_wri) | !is.na(scores$sat_wri)),])
nrow(scores[is.na(scores$uc_read),])
nrow(scores[is.na(scores$uc_read) & (!is.na(scores$act_rea) | !is.na(scores$sat_ver)),])
nrow(scores[is.na(scores$uc_tot),])
nrow(scores[is.na(csores$uc_read) & (!is.na(scores$act_tot) | !is.na(scores$sat_tot)),])
nrow(scores[is.na(scores$uc_tot),]) / nrow(scores)
