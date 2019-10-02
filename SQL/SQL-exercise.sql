--  Question 2.a 
--  Numbeer of students per department 
SELECT dd.departmentname               AS DepartmentName, 
       Count(DISTINCT( cr.studentid )) AS students_num 
FROM   classrooms cr 
       JOIN courses cs 
         ON cr.courseid = cs.courseid 
       JOIN departments dd 
         ON dd.departmentid = cs.departmentid 
GROUP  BY dd.departmentname 

--  Question 2.b 
--  How many student there in each course  
SELECT cs.coursename, 
       Count(*) num_students 
FROM   classrooms cr 
       JOIN courses cs 
         ON cr.courseid = cs.courseid 
WHERE  cr.courseid IN (SELECT courseid 
                       FROM   courses cs 
                              JOIN departments d 
                                ON d.departmentid = cs.departmentid 
                       WHERE  d.departmentname = 'English') 
GROUP  BY cs.coursename 

--  and how many altogether  
SELECT Count(DISTINCT( cr.studentid )) 
FROM   classrooms cr 
WHERE  cr.courseid IN (SELECT courseid 
                       FROM   courses cs 
                              JOIN departments d 
                                ON d.departmentid = cs.departmentid 
                       WHERE  d.departmentname = 'English') 

--  Question 2.c 
--  How many classes with less than 22 students 
SELECT CASE 
         WHEN students_num > 22 THEN 'BIG' 
         ELSE 'SMALL' 
       END AS room_category, 
       Count(*) 
FROM   (SELECT cs.coursename, 
               Count(*) AS students_num 
        FROM   classrooms cr 
               JOIN courses cs 
                 ON cr.courseid = cs.courseid 
        WHERE  cr.courseid IN (SELECT courseid 
                               FROM   courses cs 
                                      JOIN departments d 
                                        ON d.departmentid = cs.departmentid 
                               WHERE  d.departmentname = 'Science') 
        GROUP  BY cs.coursename) AS ttt 
GROUP  BY ( CASE 
              WHEN students_num > 22 THEN 'BIG' 
              ELSE 'SMALL' 
            END ) 

--  Question 2.d 
--  Check whether there are more men than weman 
SELECT st.gender, 
       Count(*) 
FROM   students st 
GROUP  BY st.gender 

--  Question 2.e 
--  Check which courses has female/male percentrage higher than 70%  
SELECT t2.coursename, 
       t2.gender, 
       ( t2.st_num * 1.0 ) / ( t3.total_students * 1.0 ) AS per 
FROM   (SELECT cr.courseid    AS CourseId, 
               crs.coursename AS CourseName, 
               st.gender      AS Gender, 
               Count(*)       AS st_num 
        FROM   students st 
               JOIN classrooms cr 
                 ON st.studentid = cr.studentid 
               JOIN courses crs 
                 ON cr.courseid = crs.courseid 
        GROUP  BY cr.courseid, 
                  st.gender, 
                  crs.coursename) AS t2 
       JOIN (SELECT cr.courseid, 
                    Count(*) AS total_students 
             FROM   students st 
                    JOIN classrooms cr 
                      ON st.studentid = cr.studentid 
                    JOIN courses crs 
                      ON cr.courseid = crs.courseid 
             GROUP  BY cr.courseid) AS t3 
         ON t2.courseid = t3.courseid 
WHERE  ( t2.st_num * 1.0 ) / ( t3.total_students * 1.0 ) > 0.7 
ORDER  BY t2.courseid 

--  Question 2.f 
--  In each Department, check how many students (absolute number and percentage) graduated with degree higher than 80
--  Assumption - graduation degree = average degree of all the courses belonging to that department
SELECT dpr.departmentname, 
       super_students, 
       total_students, 
       ( ( super_students * 1.0 ) / ( total_students * 1.0 ) ) * 100 AS 
       percentage 
FROM   (SELECT t5.departmentid AS DepartmentId, 
               Count(*)        AS super_students 
        FROM   (SELECT dpr.departmentid AS DepartmentId, 
                       cr.studentid     AS StudentId, 
                       Avg(cr.degree)   avg_degree 
                FROM   classrooms cr 
                       JOIN courses crs 
                         ON cr.courseid = crs.courseid 
                       JOIN departments dpr 
                         ON crs.departmentid = dpr.departmentid 
                GROUP  BY dpr.departmentid, 
                          cr.studentid) AS t5 
        WHERE  t5.avg_degree > 80.0 
        GROUP  BY t5.departmentid) AS t6 
       JOIN (SELECT dpr.departmentid                AS DepartmentId, 
                    Count(DISTINCT( cr.studentid )) AS total_students 
             FROM   classrooms cr 
                    JOIN courses crs 
                      ON cr.courseid = crs.courseid 
                    JOIN departments dpr 
                      ON crs.departmentid = dpr.departmentid 
             GROUP  BY dpr.departmentid) AS t4 
         ON t4.departmentid = t6.departmentid 
       JOIN departments dpr 
         ON dpr.departmentid = t6.departmentid 

--  Question 2.g 
--  In each Department, check how many students (absolute number and percentage) didn't graduate (with degree less than 60)
--  Assumption - graduation degree = average degree of all the courses belonging to that department
SELECT dpr.departmentname, 
       bad_students, 
       total_students, 
       ( ( bad_students * 1.0 ) / ( total_students * 1.0 ) ) * 100 AS percentage 
FROM   (SELECT t5.departmentid AS DepartmentId, 
               Count(*)        AS bad_students 
        FROM   (SELECT dpr.departmentid AS DepartmentId, 
                       cr.studentid     AS StudentId, 
                       Avg(cr.degree)   avg_degree 
                FROM   classrooms cr 
                       JOIN courses crs 
                         ON cr.courseid = crs.courseid 
                       JOIN departments dpr 
                         ON crs.departmentid = dpr.departmentid 
                GROUP  BY dpr.departmentid, 
                          cr.studentid) AS t5 
        WHERE  t5.avg_degree < 60.0 
        GROUP  BY t5.departmentid) AS t6 
       JOIN (SELECT dpr.departmentid                AS DepartmentId, 
                    Count(DISTINCT( cr.studentid )) AS total_students 
             FROM   classrooms cr 
                    JOIN courses crs 
                      ON cr.courseid = crs.courseid 
                    JOIN departments dpr 
                      ON crs.departmentid = dpr.departmentid 
             GROUP  BY dpr.departmentid) AS t4 
         ON t4.departmentid = t6.departmentid 
       JOIN departments dpr 
         ON dpr.departmentid = t6.departmentid 

--  Question 2.h 
--  Rank the teachers according to the average of grades (from the higher to lower) 
SELECT crs.coursename, 
       tchr.lastname, 
       Avg(cr.degree) avg_degree 
FROM   classrooms cr 
       JOIN courses crs 
         ON cr.courseid = crs.courseid 
       JOIN teachers tchr 
         ON tchr.teacherid = crs.teacherid 
GROUP  BY crs.coursename, 
          tchr.lastname 
ORDER  BY avg_degree DESC 

--  Question 3.a 
--  Create a view that includes courses, their departments, their teachers and number of students
CREATE VIEW coursesv 
AS 
  SELECT crs.coursename, 
         dpr.departmentname, 
         tchr.lastname, 
         Count(*) num_students 
  FROM   classrooms cr 
         JOIN courses crs 
           ON cr.courseid = crs.courseid 
         LEFT OUTER JOIN departments dpr 
                      ON crs.departmentid = dpr.departmentid 
         LEFT OUTER JOIN teachers tchr 
                      ON crs.teacherid = tchr.teacherid 
  GROUP  BY crs.coursename, 
            dpr.departmentname, 
            tchr.lastname 

--  Question 3.b 
--  Create a view that includes students, their average per department and ovverall average 
CREATE VIEW studentsv 
AS 
  SELECT st.firstname, 
         st.lastname, 
         total.num_courses, 
         total.avg_total, 
         dpr.departmentname, 
         Avg(cr.degree) AS department_avg 
  FROM   students st 
         LEFT OUTER JOIN classrooms cr 
                      ON st.studentid = cr.studentid 
         LEFT OUTER JOIN (SELECT cr.studentid       StudentId, 
                                 Count(cr.courseid) num_courses, 
                                 Avg(cr.degree)     avg_total 
                          FROM   classrooms cr 
                          GROUP  BY cr.studentid) AS total 
                      ON st.studentid = total.studentid 
         LEFT OUTER JOIN courses crs 
                      ON cr.courseid = crs.courseid 
         LEFT OUTER JOIN departments dpr 
                      ON crs.departmentid = dpr.departmentid 
  GROUP  BY st.firstname, 
            st.lastname, 
            total.num_courses, 
            total.avg_total, 
            dpr.departmentname 