--	Question 2.a
--	Numbeer of students per department
select dd.DepartmentName as DepartmentName, count(distinct(cr.StudentId)) as students_num 
from Classrooms cr
join Courses cs on cr.CourseId = cs.CourseId
join Departments dd on dd.DepartmentId = cs.DepartmentId
Group by dd.DepartmentName


--	Question 2.b
--	How many student there in each course 
select cs.CourseName, count(*) num_students from Classrooms cr
join Courses cs on cr.CourseId = cs.CourseId
where cr.CourseId in 
(
	select CourseId from Courses cs
	join Departments d
	on d.DepartmentId = cs.DepartmentId
	where d.DepartmentName = 'English'
)
group by cs.CourseName

--	and how many altogether 
select count(distinct(cr.StudentId)) from Classrooms cr
where cr.CourseId in 
(
	select CourseId from Courses cs
	join Departments d
	on d.DepartmentId = cs.DepartmentId
	where d.DepartmentName = 'English'
)

--	Question 2.c
--	How many classes with less than 22 students

select 
	CASE
		WHEN students_num > 22 THEN 'BIG'
		ELSE 'SMALL'
	END as room_category, count(*)
from
	(select cs.CourseName, count(*) as students_num
	from Classrooms cr
	join Courses cs on cr.CourseId = cs.CourseId
	where cr.CourseId in 
	(
		select CourseId from Courses cs
		join Departments d
		on d.DepartmentId = cs.DepartmentId
		where d.DepartmentName = 'Science'
	)
	group by cs.CourseName) as ttt
group by 
(
	CASE
		WHEN students_num > 22 THEN 'BIG'
		ELSE 'SMALL'
	END
)

--	Question 2.d
--	Check whether there are more men than weman
select st.Gender, count(*) from Students st
group by st.Gender

--	Question 2.e
--	Check which courses has female/male percentrage higher than 70% 
select t2.CourseName, t2.Gender, (t2.st_num*1.0)/(t3.total_students*1.0) as per from 
(select cr.CourseId as CourseId, crs.CourseName as CourseName, st.Gender as Gender, count(*) as st_num 
from Students st
join Classrooms cr on st.StudentId = cr.StudentId
join Courses crs on cr.CourseId = crs.CourseId
group by cr.CourseId, st.Gender, crs.CourseName
) as t2
join 
(select cr.CourseId, count(*) as total_students 
from Students st
join Classrooms cr on st.StudentId = cr.StudentId
join Courses crs on cr.CourseId = crs.CourseId
group by cr.CourseId
) as t3 on t2.CourseId = t3.CourseId
where (t2.st_num*1.0)/(t3.total_students*1.0) > 0.7
order by t2.CourseId


--	Question 2.f
--	In each Department, check how many students (absolute number and percentage) graduated with degree higher than 80
--  Assumption - graduation degree = average degree of all the courses belonging to that department

select dpr.DepartmentName, super_students, total_students, ((super_students * 1.0) / (total_students * 1.0)) * 100  as percentage from 
(select t5.DepartmentId as DepartmentId , count(*) as super_students
from (select dpr.DepartmentId as DepartmentId, cr.StudentId as StudentId, AVG(cr.degree) avg_degree
from Classrooms cr
join Courses crs on cr.CourseId = crs.CourseId
join Departments dpr on crs.DepartmentID = dpr.DepartmentId 
group by dpr.DepartmentId, cr.StudentId) as t5
where t5.avg_degree > 80.0
group by t5.DepartmentId) as t6 
join (select dpr.DepartmentId as DepartmentId, count(distinct(cr.StudentId)) as total_students 
from Classrooms cr
join Courses crs on cr.CourseId = crs.CourseId
join Departments dpr on crs.DepartmentID = dpr.DepartmentId 
group by dpr.DepartmentId) as t4 on t4.DepartmentId = t6.DepartmentId
join Departments dpr on dpr.DepartmentId = t6.DepartmentId

--	Question 2.g
--	In each Department, check how many students (absolute number and percentage) didn't graduate (with degree less than 60)
--  Assumption - graduation degree = average degree of all the courses belonging to that department

select dpr.DepartmentName, bad_students, total_students, ((bad_students * 1.0) / (total_students * 1.0)) * 100  as percentage 
from (select t5.DepartmentId as DepartmentId , count(*) as bad_students
	  from (select dpr.DepartmentId as DepartmentId, cr.StudentId as StudentId, AVG(cr.degree) avg_degree
			from Classrooms cr
			join Courses crs on cr.CourseId = crs.CourseId
			join Departments dpr on crs.DepartmentID = dpr.DepartmentId 
			group by dpr.DepartmentId, cr.StudentId) as t5
	   where t5.avg_degree < 60.0
	   group by t5.DepartmentId) as t6 
join (select dpr.DepartmentId as DepartmentId, count(distinct(cr.StudentId)) as total_students 
	  from Classrooms cr
	  join Courses crs on cr.CourseId = crs.CourseId
	  join Departments dpr on crs.DepartmentID = dpr.DepartmentId 
	  group by dpr.DepartmentId) as t4 on t4.DepartmentId = t6.DepartmentId
join Departments dpr on dpr.DepartmentId = t6.DepartmentId


--	Question 2.h
--	Rank the teachers according to the average of grades (from the higher to lower)
select crs.CourseName, tchr.LastName, avg(cr.degree) avg_degree from Classrooms cr
join Courses crs on cr.CourseId = crs.CourseId
join Teachers tchr on tchr.TeacherId = crs.TeacherId
group by crs.CourseName, tchr.LastName
order by avg_degree desc



--	Question 3.a
--	Create a view that includes courses, their departments, their teachers and number of students
create view CoursesV as
select crs.CourseName, dpr.DepartmentName, tchr.LastName, count(*) num_students from Classrooms cr
join Courses crs on cr.CourseId = crs.CourseId
left outer join Departments dpr on crs.DepartmentID = dpr.DepartmentId
left outer join Teachers tchr on crs.TeacherId = tchr.TeacherId
group by crs.CourseName, dpr.DepartmentName, tchr.LastName

--	Question 3.b
--	Create a view that includes students, their average per department and ovverall average
create view StudentsV as
select st.FirstName, st.LastName, total.num_courses, total.avg_total, dpr.DepartmentName, AVG(cr.degree) as department_avg from Students st
left outer join Classrooms cr on st.StudentId = cr.StudentId
left outer join 
(select cr.StudentId StudentId, count(cr.CourseId) num_courses, avg(cr.degree) avg_total 
from Classrooms cr
group by cr.StudentId ) as total on st.StudentId = total.StudentId
left outer join Courses crs on cr.CourseId = crs.CourseId
left outer join Departments dpr on crs.DepartmentID = dpr.DepartmentId
group by st.FirstName, st.LastName, total.num_courses, total.avg_total, dpr.DepartmentName



