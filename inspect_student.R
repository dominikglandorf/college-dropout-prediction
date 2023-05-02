source('read_data.R')

#students = get_student_sub()
bg = get_student_background_data(ids = students$mellon_id)
terms = get_term_data(ids = students$mellon_id)
courses = get_course_data(ids = students$mellon_id)


get_majors = function(term) paste(na.omit(c(term['major_name_1'], term['major_name_2'], term['major_name_3'], term['major_name_4'])), collapse=", ")
get_schools = function(term) paste(na.omit(unique(c(term['major_school_name_abbrev_1'], term['major_school_name_abbrev_2'], term['major_school_name_abbrev_3'], term['major_school_name_abbrev_4']))), collapse=", ")
get_courses = function(term, s_courses) {
  s_t_courses = s_courses[s_courses$term_code==term['term_code'],]
  return(paste(unique(s_t_courses$course_dept_code_and_num), collapse=",  "))
}
get_ua = function(term, s_courses) {
  s_t_courses = s_courses[s_courses$term_code==term['term_code'],]
  return(sum(s_t_courses$units_attempted, na.rm=T))
}
get_uc = function(term, s_courses) {
  s_t_courses = s_courses[s_courses$term_code==term['term_code'],]
  return(sum(s_t_courses$units_completed, na.rm=T))
}
inspect_student = function(s_id) {
  student = students[students$mellon_id==s_id,]
  cat(paste0("Student ", s_id, " was admitted in ", student$admitdate, "\n"))
  s_terms = terms[terms$mellon_id==s_id,]
  s_terms = s_terms[order(s_terms$term_code),]
  s_courses = courses[courses$mellon_id==s_id,]
  tor = data.frame(year=substr(s_terms$term_code,1,4),
                   term=s_terms$term_part_desc,
                   majors=apply(s_terms, 1, FUN=get_majors),
                   schools=apply(s_terms, 1, FUN=get_schools),
                   courses=apply(s_terms, 1, FUN=get_courses, s_courses=s_courses),
                   u_att=apply(s_terms, 1, FUN=get_ua, s_courses=s_courses),
                   u_com=apply(s_terms, 1, FUN=get_uc, s_courses=s_courses))
  print(tor, right=F)
  term = s_terms[nrow(s_terms),]
  grad_majors = unique(toupper(na.omit(c(term$major_graduated_1, term$major_graduated_2, term$major_graduated_3, term$major_graduated_4))))
  cat(if(student$dropout) "dropped out" else paste0("graduated in ", paste(grad_majors, collapse=", ")))
  cat("\n\n")
}

inspect_graduated = function(s_id) {
  student = students[students$mellon_id==s_id,]
  cat(paste0("Student ", s_id, " was admitted in ", student$admitdate, "\n"))
  s_terms = terms[terms$mellon_id==s_id,]
  s_terms = s_terms[order(s_terms$term_code),]
  tor = data.frame(year=substr(s_terms$term_code,1,4),
                   term=s_terms$term_part_desc,
                   majors=apply(s_terms, 1, FUN=get_majors),
                   major_grad_1=s_terms$major_graduated_1,
                   major_grad_2=s_terms$major_graduated_2,
                   major_grad_3=s_terms$major_graduated_3,
                   major_grad_4=s_terms$major_graduated_4)
  print(tor, right=F)
  cat("\n\n")
}


sample(students$mellon_id, 30)
#s_id = "2992237"
#inspect_student(s_id)

for (s_id in sample(students$mellon_id, 10)) {
  inspect_graduated(s_id)
}
