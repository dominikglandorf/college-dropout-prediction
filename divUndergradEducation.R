# set wd to source file directory
source('read_data.R')

students = get_student_sub()
bg_sub = get_student_background_data(ids = student_sub$mellon_id)
terms = get_term_data(ids = students$mellon_id)
courses = get_course_data(ids = students$mellon_id)

table(bg_sub$int_student) / length(bg_sub$int_student)
table(terms$major_school_name_1)

terms = merge(terms, bg_sub[,c('mellon_id','int_student')])
students = merge(students, bg_sub[,c('mellon_id','int_student')])
confus = table(students[,c('int_student','dropout')])
confus / rowSums(confus)

ggplot(data = terms, aes(x = major_school_name_1)) +
  geom_bar(data=subset(terms,int_student == T),aes(y = (..count..)/sum(..count..)),fill = "red", alpha = 0.4) +
  geom_bar(data=subset(terms,int_student == F),aes(y = (..count..)/sum(..count..)),fill = "darkgreen", alpha = 0.4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of school",
       x = "School",
       y = "Frequency") +
  theme(text = element_text(size = 16),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

table(terms[,c('major_name_1','major_school_name_1')])

source('inspect_student.R')

for (s_id in na.omit(ids)) {
  inspect_student(s_id)
}

term_features = get_term_features()
term_features = merge(term_features, students[,c("mellon_id", "dropout", "admitdate")], by="mellon_id")

term_features_1 = subset(term_features, term_num==1)


for (s_id in na.omit(sample(term_features_1[term_features_1$school_1=="Div of Undergraduate Education",]$mellon_id, 10))) {
  inspect_student(s_id)
}

for (s_id in na.omit(sample(term_features_1[term_features_1$major_1=="UNDECLARED",]$mellon_id, 10))) {
  inspect_student(s_id)
}

mean(term_features_1[term_features_1$school_1=="Div of Undergraduate Education",'dropout'], na.rm=T)
mean(term_features_1[term_features_1$major_1=="UNDECLARED",'dropout'], na.rm=T)
mean(term_features_1[term_features_1$major_1=="UNAFFILIATED",'dropout'], na.rm=T)

undergraddiv_year=aggregate(term_features_1$school_1=="Div of Undergraduate Education"~term_code,
                            term_features_1,
                             FUN=sum)
names(undergraddiv_year)=c("term","num")
ggplot(undergraddiv_year, aes(x=as.factor(term), y=num))+
  geom_bar(stat="identity")
