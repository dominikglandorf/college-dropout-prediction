###This script should be executed after 01 Preprocessing!

glimpse(st_term)
glimpse(TermTable)
glimpse(st_background)
glimpse(st_term_course)

#For time to graduation, we are using student term course data
#Arranging terms in the right order is crucial in order to 
#identify the last term in which each students took courses (proxy for last term at UCI)
#Always check whether the term_code_new from the Term Table makes sense as we will use it
#for arranging the courses in the right order later

TermTable <- readRDS("term_tab.rds")
TermTable

#Use to recode in student_term_course data
term_key <- TermTable$term_id_new

names(term_key) <- TermTable$term_code
names(term_key)

st_term_course <- st_term_course %>%
  mutate(term_code_new = recode(term_code, !!!term_key))

#######################################################################
#######################################################################
###Figure out when students stop taking courses########################
#i.e. proxy for last term at UCI and graduation########################
#######################################################################
#Calculate no of courses per term and student

CoursesperTermLong <- st_term_course %>%
  #filter summer terms
  filter(!grepl("Summer", term_desc)) %>%
  group_by(mellon_id, term_code_new) %>%
  mutate(Nocourses = length(unique(course_code))) %>%
  select(mellon_id, term_desc, term_code_new, Nocourses) %>%
  #drop duplicates
  distinct(mellon_id, term_code_new, .keep_all = TRUE) %>%
  #arrange by student id and term_code_new
  arrange(mellon_id, term_code_new) %>%
  ungroup()

glimpse(CoursesperTermLong)

#create course variables
#including the cumulative sum of courses, max cumsum of courses and the last term with courses
#ATTENTION: getting the order of terms right with arrange is crucial here
#otherwise LastTermCourses will display the wrong term!!!!

CumSumCourses <- CoursesperTermLong %>%
  #arrange by student id and term_code_new
  arrange(mellon_id, term_code_new) %>%
  group_by(mellon_id) %>%
  mutate(CumSumCourses = cumsum(Nocourses),
         MaxNoCourses = max(CumSumCourses),
         LastTermCourses = ifelse(CumSumCourses == MaxNoCourses, term_code_new, NA),
         CumSumTerms = n()) %>%
  select(-Nocourses)

glimpse(CumSumCourses)

LastTermCourses <- CumSumCourses %>%
  select(mellon_id, MaxNoCourses, LastTermCourses, CumSumTerms, CumSumCourses) %>%
  drop_na()

length(unique(LastTermCourses$mellon_id)) == nrow(LastTermCourses)

#plausibility check: mellon_id: is it unique? --> sample

#######################################################################
#######################################################################
###Merge with dataset that includes admitdate##########################
#######################################################################

st_bgr_hon <- readRDS("st_bgr_hon.rds")

TimeToGraduation <- st_bgr_hon %>%
  left_join(LastTermCourses, by = "mellon_id") %>%
  #filter students who were admitted after 2015 as we cannot calculate Grad4 and Grad6 for them
  filter(admit_year_1 < 2015) %>%
  #filter students whose last term is 2021_2022_01 as this is the last term with data
  #filter(LastTermCourses != "2021_2022_01") %>%
  #filter students with a very small number of cumulated terms as they have probably dropped out
  #filter(CumSumTerms >= 6) %>%
  mutate(Grad3 = ifelse(CumSumTerms <= 9, "yes", "no"),
         Grad4 = ifelse(CumSumTerms >9 & CumSumTerms <= 12, "yes", "no"))

#check if Grad3 and 4 numbers are plausible

PlausibilityCheck <- TimeToGraduation %>%
  select(mellon_id, honors_student, honors_group, 
         admit_term, admit_ac_year, CumSumCourses, CumSumTerms, LastTermCourses)

glimpse(PlausibilityCheck)

PlausibilityCheck %>%
  group_by(LastTermCourses) %>%
  summarise(n = n())


hist(PlausibilityCheck$CumSumTerms)
hist(PlausibilityCheck$CumSumCourses)

PlausibilityCheck %>%
  summarise(min = min(CumSumCourses), 
            max = max(CumSumCourses),
            mean = mean(CumSumCourses), 
            mdn = median(CumSumCourses))

PlausibilityCheck %>%
  ggplot(aes(x = honors_student, y = CumSumCourses)) +
  geom_boxplot()

PlausibilityCheck %>%
  ggplot(aes(x = honors_student, y = CumSumTerms)) +
  geom_boxplot()


#######################################################################
#######################################################################
###Calculate Graduation Rates##########################################
#######################################################################

Grad3rate <- TimeToGraduation %>%
  group_by(honors_student, Grad3) %>%
  summarise(n = n()) %>%
  group_by(honors_student) %>%
  mutate(rate = n / sum(n))


Grad4rate <- TimeToGraduation %>%
  group_by(honors_student, Grad4) %>%
  summarise(n = n()) %>%
  group_by(honors_student) %>%
  mutate(rate = n / sum(n))


  
#################################################################################
#####OLD CODE####################################################################
#################################################################################
unique(st_background$grad_major_1_desc)

st_term %>%
  filter(total_terms_enrolled_excluding_s > 0) %>%
  group_by(term_desc) %>%
  summarise(n = n()) %>%
  janitor::adorn_totals(where = "row")


#availabe data on total terms: create subset without missing data
totalTermsESGreater0 <- st_term %>%
  filter(total_terms_enrolled_excluding_s > 0)

#how many terms?

length(unique(totalTermsESGreater0$term_code))

#how many students?

length(unique(totalTermsESGreater0$mellon_id))

totalTermsESGreater0$term_code


#create a dataset that displays per student:
#the max number of terms enrolled

#max number of terms enrolled by student in terms 201903-202114 (excluding summer terms)
MaxTermsEnrolled <- totalTermsESGreater0 %>%
  select(mellon_id, term_code, total_terms_enrolled_excluding_s) %>%
  filter(!(term_code %in% c("202192"))) %>%
  arrange(term_code) %>%
  spread(key = term_code, value = total_terms_enrolled_excluding_s) %>%
  select(-ends_with("51")) %>%
  rowwise() %>%
  #find term with max value of total terms enrolled by student (= their last term)
  mutate(max_terms_enrolled = max(c_across(2:9), na.rm = TRUE)) %>%
  ungroup() %>%
  select(mellon_id, max_terms_enrolled)

glimpse(MaxTermsEnrolled)

###############################################################################
#create a dataset that displays per student:
#the max number of terms enrolled
#the last term the student was enrolled (term(s) in which number of total terms enrolled is max)
LastTermEnrolled <- totalTermsESGreater0 %>%
  ungroup() %>%
  #filter summer terms
  filter(!grepl("Summer", term_desc)) %>%
  #exclude winter 2021 because number of total terms enrolled seems to be not up to data (lower values than in previous terms)
  filter(!(term_code %in% c("202192"))) %>%
  select(mellon_id, term_code, total_terms_enrolled_excluding_s) %>%
  #arrange by student id and term_code
  arrange(mellon_id, term_code) %>%
  group_by(mellon_id) %>%
  #create variables
  mutate(max_terms_enrolled = max(total_terms_enrolled_excluding_s),
         last_term_enrolled = ifelse(total_terms_enrolled_excluding_s == max_terms_enrolled, term_code, NA)) %>%
  select(mellon_id, max_terms_enrolled, last_term_enrolled) %>%
  ungroup() %>%
  #filter rows with NA (duplicated student ids)
  filter(!is.na(last_term_enrolled)) %>%
  #drop duplicated ids (keeps only first row per case)
  filter (!duplicated(mellon_id))

which(duplicated(LastTermEnrolled$mellon_id))

glimpse(LastTermEnrolled)

#join with student background data

st_background_total_enrolled <- st_background %>%
  left_join(LastTermEnrolled, by = "mellon_id")

hist(st_background_total_enrolled$max_terms_enrolled)


Potential3YearGrad <- st_background_total_enrolled %>%
  filter(max_terms_enrolled <= 12)

Potential4YearGrad <- st_background_total_enrolled %>%
  filter(max_terms_enrolled <= 18)


# #calculate cumulative sums by row
# 
# library(matrixStats)
# library(data.table)
# 
# rowCumsums <- rowCumsums(as.matrix(CoursesperTermWide[, 2:43]))
# 
# rowCumsums
# 
# CoursesperTermWidedt <- setDT(CoursesperTermWide)
# CoursesperTermWidedt[, (2:43) := as.data.table(rowCumsums(as.matrix(.SD)))]
# 
# #change data from long to wide format 
# #to display how the number of courses per student changes over the terms
# CoursesperTerm %>%
#   select(mellon_id, starts_with("20")) %>%
#   group_by (mellon_id) %>%
#   group_split() %>%
#   #arrange by student id and term_code
#   map(., ~.x %>% 
#         filter(!duplicated(mellon_id))) %>%
#   bind_rows() 
#   #wide table
#   spread(key = term_code, value = Nocourses)
# 
# #check if there are no duplicated students in the dataset
# 
# length(unique(CoursesperTermWide$mellon_id))
# 
# which(duplicated(CoursesperTermWide$mellon_id))
#   
# #totals of courses per student number of courses per student
# 
# CoursesperTermWide
# 
# CumulativeCourses <- CoursesperTermWide %>%
#   mutate(totalCourses = rowSums(select(., -mellon_id), na.rm = TRUE))
# 
# CumulativeCourses %>%
#   select(mellon_id, totalCourses)
