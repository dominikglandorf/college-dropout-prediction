# set wd to source file directory
source('read_data.R')

students = get_student_sub()
terms = get_term_data(ids = students$mellon_id)
courses = get_course_data(ids = students$mellon_id)

# enumerate terms
term_features = terms[order(terms$term_code),c('mellon_id','term_code')]
term_features$term_num = ave(term_features$mellon_id,
                             term_features$mellon_id,
                             FUN=seq_along)

# make to factors
term_features$mellon_id = as.factor(term_features$mellon_id)

# number of units completed per term
units_from_courses=aggregate(units_completed~term_code+mellon_id,
                             courses,
                             FUN=sum)
term_features = merge(term_features,
                      units_from_courses,
                      by=c("mellon_id","term_code"), all.x=TRUE) 

# average number of units completed till term n
library(data.table)
term_table = data.table(term_features) # create data table
cum_credits = term_table[order(term_num), cumulative_units_completed := cumsum(replace_na(units_completed, 0)), by = .(mellon_id)]
cum_credits$cum_avg_credits = cum_credits$cumulative_units_completed / cum_credits$term_num

term_features = merge(term_features,
                      cum_credits[,c("mellon_id","term_code","cumulative_units_completed","cum_avg_credits")],
                      by=c('mellon_id','term_code'))

# add relative stats to term number, major and their interaction
term_features_avg = merge(term_features,
                          terms[,c("mellon_id","term_code","major_name_1")],
                          by=c("mellon_id","term_code"))
avg_units_per_term_num = aggregate(cbind(units_completed,cum_avg_credits)~term_num,
                                   term_features,
                                   FUN=mean)
names(avg_units_per_term_num) = c("term_num", "units_completed.term", "cum_avg_credits.term")
term_features_avg = merge(term_features_avg,
                          avg_units_per_term_num,
                          by="term_num")

avg_units_per_major = aggregate(cbind(units_completed,cum_avg_credits)~major_name_1,
                                term_features_avg,
                                FUN=mean)
names(avg_units_per_major) = c("major_name_1", "units_completed.major", "cum_avg_credits.major")
term_features_avg = merge(term_features_avg,
                          avg_units_per_major,
                          by="major_name_1")

avg_units_per_major_term_num = aggregate(cbind(units_completed,cum_avg_credits)~major_name_1+term_num,
                                term_features_avg,
                                FUN=mean)
names(avg_units_per_major_term_num) = c("major_name_1", "term_num", "units_completed.major_termnum", "cum_avg_credits.major_termnum")
term_features_avg = merge(term_features_avg,
                          avg_units_per_major_term_num,
                          by=c("major_name_1","term_num"))

term_features_avg$units_completed.rel_term_num = term_features_avg$units_completed -  term_features_avg$units_completed.term
term_features_avg$cum_avg_credits.rel_term_num = term_features_avg$cum_avg_credits -  term_features_avg$cum_avg_credits.term

term_features_avg$units_completed.rel_major = term_features_avg$units_completed -  term_features_avg$units_completed.major
term_features_avg$cum_avg_credits.rel_major = term_features_avg$cum_avg_credits -  term_features_avg$cum_avg_credits.major

term_features_avg$units_completed.rel_major_termnum = term_features_avg$units_completed -  term_features_avg$units_completed.major_termnum
term_features_avg$cum_avg_credits.rel_major_termnum = term_features_avg$cum_avg_credits -  term_features_avg$cum_avg_credits.major_termnum

term_features = merge(term_features,
                      term_features_avg[,c("mellon_id","term_code",
                                           "units_completed.rel_term_num",
                                           "cum_avg_credits.rel_term_num",
                                           "units_completed.rel_major",
                                           "cum_avg_credits.rel_major",
                                           "units_completed.rel_major_termnum",
                                           "cum_avg_credits.rel_major_termnum")],
                      by=c("mellon_id","term_code"))

# major_1, school_1, number of majors
terms_na_majors = terms[,c("major_name_1","major_name_2","major_name_3","major_name_4")]
terms_na_majors[terms_na_majors=="UNDECLARED"] = NA
terms_na_majors[terms_na_majors=="UNAFFILIATED"] = NA

term_majors = data.frame(mellon_id=terms$mellon_id,
                         term_code=terms$term_code,
                         major_1=terms$major_name_1,
                         school_1=terms$major_school_name_1,
                         num_majors=rowSums(!is.na(terms_na_majors)))

term_features = merge(term_features,
                      term_majors,
                      by=c("mellon_id","term_code"))


# too experimental because of missing data
#courses_schools = merge(courses,
#                        terms[,c("mellon_id","term_code","major_school_name_abbrev_1")],
#                        by=c("mellon_id","term_code"),
#                        all.x=T)
#courses_schools$in_school = courses_schools$school_name_abbrev == courses_schools$major_school_name_abbrev_1

# save to file
write_csv(term_features, file.path(path_data, 'term_features_subset.csv'))
