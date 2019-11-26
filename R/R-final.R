
#### On Linux server:
library(DBI)
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={ODBC Driver 17 for SQL Server};server=192.168.1.1;
database=COLLEGE;uid=dsuser02;
pwd=DSuser02!", timeout = 10)

#### On Windows:
library(DBI)
con <- dbConnect(odbc::odbc(), .connection_string = "DSN=COLLEGE;Trusted_Connection=yes;", timeout = 10)

## Get the whole table:
Classrooms <- dbReadTable(con, "Classrooms")
Courses <- dbReadTable(con, "Courses")
Students <- dbReadTable(con, "Students")
Teachers <- dbReadTable(con, "Teachers")
Departments <- dbReadTable(con, "Departments")

library(dplyr)


## Questions

##############
## Q1. Count the number of students on each department
##############


df<-dplyr::left_join(Classrooms, Courses, by = "CourseId")
df<-rename(df, DepartmentId = DepartmentID)
df<-dplyr::left_join(df, Departments, by = "DepartmentId")

res<-df %>% select(StudentId, DepartmentName) %>% distinct %>%
  group_by(DepartmentName) %>%
  summarise(count<-n())

print(res)

##############
## Q2. How many students have each course of the English department and the 
##     total number of students in the department?
##############
df<-dplyr::left_join(Classrooms, Courses, by = "CourseId")
df<-rename(df, DepartmentId = DepartmentID)
df<-dplyr::left_join(df, Departments, by = "DepartmentId")

#students in each course
res<-df %>% filter(DepartmentName == 'English') %>% select(StudentId, CourseName) %>%
  group_by(CourseName) %>% summarise(count=n())

print(res)

#students in English department
res_all<-df %>% filter(DepartmentName == 'English') %>% select(StudentId) %>%
  distinct %>% summarise(count=n())

print(res_all)

##############
## Q3. How many small (<22 students) and large (22+ students) classrooms are 
##     needed for the Science department?
##############
df<-dplyr::left_join(Classrooms, Courses, by = "CourseId")
df<-rename(df, DepartmentId = DepartmentID)
df<-dplyr::left_join(df, Departments, by = "DepartmentId")

res<-df %>% filter(DepartmentName == 'Science') %>% select(StudentId, CourseName) %>%
  group_by(CourseName) %>% summarise(count=n()) %>%
  mutate(size = ifelse(count >= 22,'BIG','SMALL')) %>% select(size)

res<-table(res)

print(res)


##############
## Q4. A feminist student claims that there are more male than female in the 
##     College. Justify if the argument is correct
##############

table(Students$Gender)

##############
## Q5. For which courses the percentage of male/female students is over 70%?
##############
df<-dplyr::left_join(Classrooms, Courses, by = "CourseId")
df<-dplyr::left_join(df, Students, by = "StudentId")

res<-df %>% select(CourseName, StudentId, Gender) %>%
  mutate(male = ifelse(Gender == 'M',1,0)) %>% 
  group_by(CourseName) %>% 
  summarise(male_percentage = sum(male)/n() * 100,
         female_percenatge = 100 - male_percentage) %>% filter(male_percentage > 70 | female_percenatge > 70)

print(res)


##############
## Q6. For each department, how many students passed with a grades over 80?
##############

df<-dplyr::left_join(Classrooms, Courses, by = "CourseId")
df<-rename(df, DepartmentId = DepartmentID)
df<-dplyr::left_join(df, Departments, by = "DepartmentId")

#Assumption - graduation degree = average degree of all the courses belonging to that department
#same as in SQL exercize
res<-df %>%  
  group_by(StudentId,DepartmentName) %>% mutate(avg=mean(degree)) %>% 
  distinct(StudentId, DepartmentName, avg) %>% 
  group_by(DepartmentName) %>% 
  filter(avg>80) %>%
  summarise(count=n())

print(res)

##############
## Q7. For each department, how many students passed with a grades under 60?
##############
df<-dplyr::left_join(Classrooms, Courses, by = "CourseId")
df<-rename(df, DepartmentId = DepartmentID)
df<-dplyr::left_join(df, Departments, by = "DepartmentId")

#Assumption - graduation degree = average degree of all the courses belonging to that department
#same as in SQL exercize
res<-df %>%  
  group_by(StudentId,DepartmentName) %>% mutate(avg=mean(degree)) %>% 
  distinct(StudentId, DepartmentName, avg) %>% 
  group_by(DepartmentName) %>% 
  filter(avg<60) %>%
  summarise(count=n())

print(res)


##############
## Q8. Rate the teachers by their average student's grades (in descending order).
##############
df<-dplyr::left_join(Classrooms, Courses, by = "CourseId")
df<-rename(df, DepartmentId = DepartmentID)

res<-df %>%  
  group_by(TeacherId) %>% mutate(students_avg_grade=mean(degree)) %>% 
  distinct(TeacherId, students_avg_grade) %>% 
  arrange(desc(students_avg_grade))


res<-dplyr::left_join(res, Teachers, by = "TeacherId")

  
print(res)




##############
## Q9. Create a dataframe showing the courses, departments they are associated with, 
##     the teacher in each course, and the number of students enrolled in the course 
##     (for each course, department and teacher show the names).
##############
df<-dplyr::left_join(Classrooms, Courses, by = "CourseId")
df<-rename(df, DepartmentId = DepartmentID)
df<-dplyr::left_join(df, Departments, by = "DepartmentId")

res<-df %>%
  group_by(CourseName, DepartmentName, TeacherId) %>%
  summarise(students = n())

res<-dplyr::left_join(res, Teachers, by = "TeacherId") 

res <- res %>% select(-c(TeacherId, Gender))

print(res)

##############
## Q10. Create a dataframe showing the students, the number of courses they take, 
##      the average of the grades per class, and their overall average (for each student 
##      show the student name).
##############

df<-dplyr::left_join(Classrooms, Courses, by = "CourseId")
df<-rename(df, DepartmentId = DepartmentID)
df<-dplyr::left_join(df, Departments, by = "DepartmentId")


print(df)

res<- df %>%
  group_by(StudentId,DepartmentName) %>% 
  mutate(avg=mean(degree)) %>%
  group_by(StudentId) %>% 
  mutate(total_avg=mean(degree), courses=n()) %>%
  select(StudentId,DepartmentName, avg, total_avg, courses) %>%
  distinct() %>%
  arrange(desc(StudentId)) 


library(tidyr)

res<-pivot_wider(res, names_from = DepartmentName, values_from = avg)
res<-dplyr::left_join(res, Students, by = "StudentId")
res<-res%>%select(StudentId, FirstName, LastName, courses, total_avg, Science, Arts, Sport, English) %>%
  arrange(FirstName, LastName)

print(res)


