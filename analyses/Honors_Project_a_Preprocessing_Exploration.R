############################################################################
########UCI Mellon Honors Project###########################################
############################################################################
.rs.restartR()

library(readxl)
library (reshape2)
library(tidyverse)
library(stringr)

setwd('D:/UCI honors project/data')

`%notin%` = function(x,y) !(x %in% y)

recode_if <- function(x, condition, ...) {
  if_else(condition, recode(x, ...), x)
}

#############################################################################
###1. Getting Started########################################################
#############################################################################
###Read in data files

st_background <- read.csv(file = 'cleaned_student_background_data(20211110).csv')
#View(st_background)

st_term <- read.csv(file = 'cleaned_student_term_data(20211110).csv')
#View(st_term)

st_term_course <- read.csv(file = 'cleaned_student_term_course_section(20211021).csv')
#View(st_term_course)

#########################################################################
###2. Data Preprocessing################################################
#######################################################################
#######################################################################
###Bring student term from long to wide format########################
#######################################################################

#Have a look at the datasets
glimpse(st_background)
glimpse(st_term)

#Have a look at the term descriptions

st_term %>%
  select(term_code, term_desc)

length(unique(st_term$term_code))
length(unique(st_term$term_desc))
#Clean variable types 
#st_term$mellon_id <- as.factor(st_term$mellon_id)
#st_term$honors <- as.factor(st_term$honors)

#Have a look at the frequencies of honors/ non-honors per term
st_term %>%
  select(honors, term_desc) %>%
  group_by(term_desc, honors) %>%
  filter(!is.na(honors)) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n) * 100) 


honors_freq <- st_term %>%
  select(honors, term_desc) %>%
  group_by(term_desc, honors) %>%
  filter(!is.na(honors)) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n) * 100,
         honors =  recode(honors, '0' = '%non_honors', '1' = '%honors')) %>%
  select(-n) %>%
  spread(honors, perc)

saveRDS(honors_freq, "honors_freq.rds")


all_term_codes <- unique(st_term$term_code)

#######################################################################
#######################################################################
###Data Wrangling######################################################
######################################################################
#Student term data is in long format. Honors status can change from term
#to term. That means we need to figure out whether and when students become
#honors students in order to create the honors group variable (freshmenn, transfer,
#UCI students who enter the honors program at a later point than their admit academic year)

#As a first step, the term data  needs to be converted from long to wide format: 
#One row per students, one variable per term, 
#0 = not an honors student in the respective term, 1= honors student.

#first create term identifiers containing academic years

st_term <- st_term %>%
  mutate(tyear = as.integer(str_sub(term_code, 1, 4)),
         tcode = str_sub(term_code, 5, 6),
         tname = recode(tcode, '03' = 'winter',
                        '14' = 'spring', '51' = 'summer',
                        '92' = 'fall'),
         tcode_new = recode(tcode, '03' = '02',
                            '14' = '03', '51' = '04',
                            '92' = '01'),
         tyear_1 = as.integer(ifelse(tname == "fall", paste(tyear), paste(tyear - 1))),
         tyear_2 = tyear_1 + 1,
         academic_year = paste(tyear_1, tyear_2, sep = "_"),
         term_id = paste(academic_year, tcode, sep = "_"),
         term_id_new = paste(academic_year, tcode_new, sep = "_"))
  

st_term_honors_wide <- dcast(st_term, mellon_id ~ term_id, value.var="honors")
#st_term_honors_wide2 <- dcast(st_term, mellon_id ~ term_desc, value.var="honors")

#saveRDS(st_term_honors_wide, "st_term_honors_wide.rds")
#new dataset contains honors per semester
glimpse(st_term_honors_wide)

#create term table inlcuding all the codes as a lookup table
TermTable <- st_term %>%
  group_by(term_code) %>%
  summarise(term_code = unique(term_code),
            tname = unique(tname),
            academic_year = unique(academic_year),
            term_id = unique(term_id),
            term_id_new = unique(term_id_new)) %>%
  arrange(term_id_new)

TermTable

saveRDS(TermTable, "term_tab.rds")

#######################################################################
#######################################################################
###Figure out honors status over all terms#############################
#i.e. in how many terms receives each student honors status?###########
#######################################################################

#honors students: how often do they get status "honors"?
#note that NAs are counted as 0 in rowSums
#st_term_honors_wide$rowSums_honors <- rowSums(st_term_honors_wide[-1], na.rm = TRUE)

st_term_honors_wide <- st_term_honors_wide %>%
  mutate(rowSums_honors = rowSums(select(., -mellon_id), na.rm = TRUE))

#check: min, max, mean in all students
RowSumsAllStudents <- st_term_honors_wide %>%
  select(rowSums_honors) %>%
  summarise(min = min(rowSums_honors),
            max = max(rowSums_honors),
            mean = mean(rowSums_honors),
            n = n())

RowSumsAllStudents

#check: min, max, mean in honors students
RoWSumsHonorsStudents <- st_term_honors_wide %>%
  filter(rowSums_honors > 0) %>%
  select(rowSums_honors) %>%
  summarise(min = min(rowSums_honors),
            max = max(rowSums_honors),
            mean = mean(rowSums_honors),
            n = n())

RoWSumsHonorsStudents

##plot: honors status count 
#12 should be a frequent value because 12 trimesters == 4 years

HonorsStatusCount <- st_term_honors_wide %>%
  filter(rowSums_honors > 0) %>%
  select(rowSums_honors) %>%
  ggplot(aes(x = rowSums_honors)) +
  geom_histogram(bins = 50) + 
  labs(x = "number of terms with honors status", 
       y = "count honors students", 
       title = "Honors students: Honors status count")

HonorsStatusCount
#######################################################################
#######################################################################
###Merge with student background data to create honors groups #########
###based on admitdates################################################
######################################################################
#create honors_student status variable and merge with background dataset
#all students who have honors status for at least 1 term

st_background_merge <- st_term_honors_wide %>%
  mutate(honors_student = ifelse(rowSums_honors > 0, "yes", "no")) %>%
  merge(st_background, by = "mellon_id")

StudentCountsByAdmitdate <- st_background_merge %>%
  group_by(admitdate, honors_student) %>%
  summarise(n = n()) %>%
  mutate(honors_student = recode(honors_student, 'yes' = 'honors student',
                                 'no' = 'non-honors student')) %>%
  spread(key = honors_student, value = n)

StudentCountsByAdmitdate

#clean application status.
st_background_merge <- st_background_merge %>%
  mutate(application_status_1 = na_if(application_status, ""),
         application_status_clean = recode(application_status_1, 'Freshman' = 'Freshmen')) %>%
  select(-application_status_1) %>%
  #create admit_ac_year based on admitdate
  #this also helps to identify honors groups.
  mutate(admitdate = na_if(admitdate, ""),
         admit_term = str_sub(admitdate, 1, 1),
         admit_term = recode(admit_term, 
                             'F' = 'fall', 'W' = 'winter', 'S' = 'spring'),
         admit_year_str = ifelse(!is.na(admitdate), paste("20", str_sub(admitdate, 2, 3), sep = ""), NA_character_),
         #admit_year_str_2 = paste("20", admit_year_str, sep = ""),
         #admit_year_str_2 = recode(admit_year_str_2, '20' = NA_character_),
         admit_year_dirty = as.integer(admit_year_str),
         admit_year = ifelse(admit_year_dirty > 2021, admit_year_dirty - 100, admit_year_dirty),
         admit_year_1 = ifelse(admit_term == "fall", admit_year, admit_year - 1),
         admit_year_2 = admit_year_1 + 1,
         admit_ac_year = ifelse(!is.na(admit_year_1), 
                                paste(admit_year_1, admit_year_2, sep = "_"), NA_character_)) %>%
  select(-admit_year_str, -admit_year_dirty, -admit_year) 

#check admit academic years
st_background_merge %>%
  select(contains("admit"))


saveRDS(st_background_merge, "st_background_merge.rds")

#######################################################################
#######################################################################
###Frequencies: Number of students by their admit academic years and### 
#their application status#############################################
######################################################################

###take a look at admitdates
#admitdate refers to term admitted to UCI

StudensAdmittedPerTerm <- st_background_merge %>%
  ggplot(aes(y = admit_ac_year, fill = admit_term)) +
  geom_bar() +
  xlab('Count of Students') +
  ylab('Admit Academic Year')

StudensAdmittedPerTerm

ggsave("StudentsAdmittedAllAdmitYears.png")

#plot students admitted per academic year
#starting from 2007

admit_ac_year_count <- st_background_merge %>%
  filter(admit_year_1 >= 2007) %>%
  group_by(admit_ac_year, honors_student) %>%
  count(count = n()) %>%
  ggplot(aes(x = factor(admit_ac_year), y = n, fill = honors_student)) +
  geom_bar(position="stack", stat="identity") +
  xlab('Admit Academic Year')

admit_ac_year_count

ggsave("StudentsAdmittedByAdmitYear.png")

#plot application status per academic year

app_status_clean <- st_background_merge %>%
  filter(admit_year_1 >= 2007) %>%
  group_by(admit_ac_year, application_status_clean) %>%
  count(count = n()) %>%
  ggplot(aes(x = factor(admit_ac_year), y = n, fill = application_status_clean)) +
  geom_bar(position="stack", stat="identity") +
  xlab('Admit Academic Year') +
  ylab('Number of Students Admitted')

app_status_clean

ggsave("ApplicationStatusByAdmitYear.png")

#######################################################################
#######################################################################
###Detect honors groups by entry into honors program##################
######################################################################
###1) incoming freshmen, 2) incoming transfer students, 3) UCI students (later entry)
##############################################################################
###split the dataset by admit academic year into a list: 
#one list element = students who entered in one specific ac year
#filter NAs and admitdates before ac year 2007/2008
#kick out summer terms as there are only 2 of them
#as we have only term data from winter 2008 onwards.

SplitByAdmitYear <- st_background_merge %>%
  filter(!is.na(admit_ac_year)) %>%
  filter(admit_year_1 >= 2007) %>%
  select(-contains("summer")) %>%
  group_by(admit_ac_year) %>%
  group_split()

glimpse(SplitByAdmitYear)

#rename list items
AdmitYearsInList <- map(SplitByAdmitYear, ~.x %>% 
      select(admit_ac_year) %>%
      unique()) %>%
  unlist() %>%
  setNames(1:length(SplitByAdmitYear))

names(SplitByAdmitYear) <- AdmitYearsInList

saveRDS(SplitByAdmitYear, "split_by_admityear.rds")


##############################################################################
###identify whether students entered the honors program within their first ac year
#note. this is only done for the academic years 2007-2021 as we have only term data from winter 2008 onwards.

new_list <- list()

#unname vector, otherwise error is thrown
names(AdmitYearsInList) <- NULL

#hon_freshmen = those who enter UCI and get honors status within their first academic year
for (year in 1:length(AdmitYearsInList)) {
  tmp <- SplitByAdmitYear[[AdmitYearsInList[year]]]
  tmp_2 <- tmp %>% 
    mutate(rowSumsAdmitYear = rowSums(select(., contains(AdmitYearsInList[year]))),
           hon_freshmen = ifelse(rowSumsAdmitYear >= 1, "yes", "no"))           
  new_list[[AdmitYearsInList[year]]] <- tmp_2
  }

#bind rows together again to new dataset
st_bgr_hon <- as_tibble()

for (year in 1:length(AdmitYearsInList)) {
  tmp <- tmp <- new_list[[AdmitYearsInList[year]]]
  st_bgr_hon <- bind_rows(st_bgr_hon, tmp)
}

#######################################################################
#######################################################################
###Check###############################################################

st_bgr_hon %>%
  group_by(honors_student, rowSumsAdmitYear) %>%
  tally(sort = T) %>%
  spread(honors_student, n)

#missing data in honors students
st_bgr_hon %>%
  filter(is.na(rowSumsAdmitYear)) %>%
  group_by(admit_ac_year, honors_student) %>%
  count() %>%
  ungroup %>%
  ggplot(aes(x = factor(admit_ac_year), y = n, fill = honors_student)) +
  geom_bar(position="stack", stat="identity") +
  xlab('Admit Academic Year') +
  ylab('Number of Students with missing honors status info')

#missing data in 2018/2019 and 2019/2020
st_bgr_hon %>%
  filter(admit_ac_year %in% c("2018_2019", "2019_2020")) %>%
  group_by(rowSumsAdmitYear, admit_ac_year) %>%
  tally() %>%
  spread(admit_ac_year, n)

#missing data in all academic years

hon_count_adm <- map(new_list, ~.x %>%
                   group_by(rowSumsAdmitYear) %>%
                   tally() %>%
                   mutate(perc = n/ sum(n)*100,
                          perc = round(perc, 2)))

hon_count_adm

saveRDS(hon_count_adm, "hon_count_adm.rds")

st_bgr_hon %>%
  group_by(application_status_clean) %>%
  tally(sort = T) %>%
  mutate(percent = round(100*n/sum(n), 3))

##################################################
###some basis checks before we create honors groups

#honors students
st_bgr_hon %>%
  group_by(honors_student) %>%
  tally(sort = T) %>%
  mutate(percent = round(100*n/sum(n), 3))

#appl status = Freshmen and Transfer by honors status
st_bgr_hon %>%
  filter(application_status_clean %in% c("Freshmen", "Transfer")) %>%
  group_by(honors_student, application_status_clean) %>%
  tally(sort = T) %>%
  mutate(honors_student = recode(honors_student, 'yes' = 'honors student', 'no' = 'non-honors student')) %>%
  spread(honors_student, n)

#identify incoming honors students
st_bgr_hon %>%
  filter(honors_student == "yes") %>%
  group_by(hon_freshmen) %>%
  tally(sort = T) %>%
  mutate(hon_freshmen = recode(hon_freshmen, 'yes' = 'honors student in 1st year', 'no' = 'later entry')) %>%
  spread(hon_freshmen, n)

#compare to honors freshmen without transfers
st_bgr_hon %>%
  filter(honors_student == "yes" & application_status_clean %notin% c("Transfer")) %>%
  group_by(hon_freshmen) %>%
  tally(sort = T) %>%
  mutate(hon_freshmen = recode(hon_freshmen, 'yes' = 'honors student in 1st year', 'no' = 'later entry')) %>%
  spread(hon_freshmen, n)

#create honors groups and compare
st_bgr_hon %>%
  mutate(honors_group = "unknown",
         honors_group = case_when(application_status_clean == "Transfer" & hon_freshmen == "yes" ~ "incoming transfer",
                                  application_status_clean %notin% c("Transfer") & hon_freshmen == "yes" ~ "incoming freshmen", 
                                  honors_student == "yes" & hon_freshmen == "no" ~ "UCI students")) %>%
  group_by(honors_group) %>%
  tally(sort = T)

#save changes
st_bgr_hon <- st_bgr_hon %>%
  mutate(honors_group = "unknown",
         honors_group = case_when(application_status_clean == "Transfer" & hon_freshmen == "yes" ~ "incoming transfer",
                                  application_status_clean %notin% c("Transfer") & hon_freshmen == "yes" ~ "incoming freshmen", 
                                  honors_student == "yes" & hon_freshmen == "no" ~ "UCI students"))

st_bgr_hon %>%
  group_by(honors_group) %>%
  summarise(n = n())

saveRDS(st_bgr_hon, "st_bgr_hon.rds")

################################################################
################################################################
################################################################
###############################################################
####OLD CODE#####################################################
################################################################
