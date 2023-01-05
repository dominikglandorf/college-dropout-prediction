setwd("~/EWS")
source('read_data.R')
if(!require(ggplot2)) install.packages('ggplot2')
if(!require(plyr)) install.packages('plyr')

# read data
students = get_student_sub()
bg = get_student_background_data(students$mellon_id)

conversion_table = read.csv2("data/Ap_Code_Conversion.csv")[,-3]
for (i in 1:20) {
  bg[[paste0('ap_desc_',i)]] =  mapvalues(bg[[paste0('ap_code_',i)]], from = conversion_table$Ap.Exam.Code, to = conversion_table$Description)
}

# plot frequencies
code_freqs = table(unlist(bg[bg$admitdate=="F16",paste0('ap_desc_',1:20,"")]))

df_freqs = as.data.frame(code_freqs)
ggplot(df_freqs) + 
  geom_bar(aes(x=Var1, y=Freq), stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# number of AP exams
number_ap = rowSums(!is.na(bg[,paste0('ap_score_',1:20,"")]))
hist(number_ap)

# ratio passed
passed_ap = rowSums((bg[,paste0('ap_score_',1:20,"")])>2, na.rm=T) / number_ap
hist(passed_ap)

# best ap grade
best_ap = apply(bg[,paste0('ap_score_',1:20,"")], 1, FUN=function(x) max(x, na.rm=T))
hist(best_ap)

# avg ap_grade
avg_ap = apply(bg[,paste0('ap_score_',1:20,"")], 1, FUN=function(x) mean(x, na.rm=T))
hist(avg_ap)
