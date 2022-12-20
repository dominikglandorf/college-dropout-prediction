source('read_data.R')
library(ggplot2)

# read data
students = get_student_sub()
bg = get_student_background_data(students$mellon_id)


code_freqs = table(unlist(bg[bg$admitdate=="F16",paste0('ap_code',1:20,"_desc")]))

df_freqs = as.data.frame(code_freqs)
ggplot(df_freqs) + 
  geom_bar(aes(x=Var1, y=Freq), stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
