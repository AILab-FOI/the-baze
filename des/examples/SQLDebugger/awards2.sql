-- SQL Debugger
-- November 2011
-- Example 2 Awards
-- Wrong tuple 'Anna'
-- Refer to example2UserTrace.txt for a user trace and example2DevelopmentTrace.txt for a development trace
--
-- Declarative Debugging of Wrong and Missing Answers for SQL Views
-- FLOPS 2012

/abolish
/multiline on
/development off
-- Turn development on to inspect more info
-- /development on

/* Tables */
DROP TABLE IF EXISTS grants, payment, registration, courses;

CREATE TABLE grants (
   student varchar(45) DEFAULT NULL,
   amount integer default 0
);
   
CREATE TABLE courses (
  idcourses varchar(11) NOT NULL,
  style varchar(45) DEFAULT NULL,
  level int(11) DEFAULT NULL,
  teacher varchar(10) DEFAULT NULL,
  PRIMARY KEY (`idcourses`)
);

INSERT INTO `courses` (`idcourses`, `style`, `level`, `teacher`) VALUES ('c1', 'salsa',    1, 'teach1');
INSERT INTO `courses` (`idcourses`, `style`, `level`, `teacher`) VALUES ('c2', 'salsa',    2, 'teach2');
INSERT INTO `courses` (`idcourses`, `style`, `level`, `teacher`) VALUES ('c3', 'Merengue', 3, 'teach3');
INSERT INTO `courses` (`idcourses`, `style`, `level`, `teacher`) VALUES ('c4', 'salsa',    3, 'teach1');
INSERT INTO `courses` (`idcourses`, `style`, `level`, `teacher`) VALUES ('c5', 'salsa',    3, 'teach1');
INSERT INTO `courses` (`idcourses`, `style`, `level`, `teacher`) VALUES ('c6', 'salsa',    3, 'teach3');
INSERT INTO `courses` (`idcourses`, `style`, `level`, `teacher`) VALUES ('c7', 'salsa',    0, 'teach1');


CREATE TABLE registration (
  idregistration int(11) NOT NULL,
  student varchar(45) DEFAULT NULL,
  course varchar(11) DEFAULT NULL,
  PRIMARY KEY (`idregistration`)
);

INSERT INTO `registration` (`idregistration`, `student`, `course`) VALUES (1, 'Juan', 'c1');
INSERT INTO `registration` (`idregistration`, `student`, `course`) VALUES (2, 'Juan', 'c2');
INSERT INTO `registration` (`idregistration`, `student`, `course`) VALUES (3, 'Juan', 'c6');
INSERT INTO `registration` (`idregistration`, `student`, `course`) VALUES (4, 'Juan', 'c5');
INSERT INTO `registration` (`idregistration`, `student`, `course`) VALUES (5, 'Pedro', 'c2');
INSERT INTO `registration` (`idregistration`, `student`, `course`) VALUES (6, 'Ana', 'c1');
INSERT INTO `registration` (`idregistration`, `student`, `course`) VALUES (7, 'Ana', 'c2');
INSERT INTO `registration` (`idregistration`, `student`, `course`) VALUES (8, 'Ana', 'c5');
INSERT INTO `registration` (`idregistration`, `student`, `course`) VALUES (9, 'Mica', 'c6');
INSERT INTO `registration` (`idregistration`, `student`, `course`) VALUES (10, 'Juan', 'c7');
INSERT INTO `registration` (`idregistration`, `student`, `course`) VALUES (11, 'Mica', 'c2');
INSERT INTO `registration` (`idregistration`, `student`, `course`) VALUES (12, 'Mica', 'c1');

CREATE TABLE payment (
  `id` int(11) NOT NULL,
  `student` varchar(45) DEFAULT NULL,
  `state` varchar(11) DEFAULT NULL,
  PRIMARY KEY (`id`)
);

INSERT INTO `payment` (`id`, `student`, `state`) VALUES (1, 'Miguel', 'unpaid');

/* Views */

/* Salsa courses */
create or replace view salsaCourses(id, level, teacher) as
  select c.idcourses, c.level, c.teacher
  from courses c
  where c.style = 'salsa';  

/* Students of salsa courses */
create or replace view salsaStudents(student, level, teacher) as
   select R.student, C.level, C.teacher
   from salsaCourses C, registration R
   where C.id = R.course;


/* Students of intensive course (level = 0) */
create or replace view intensiveStudents(student) as
   select S.student
   from salsaStudents S
   where S.level = 0;
 
/* Students of first three courses with different teachers */
/* This is the ill-defined view */

create or replace view basicLevelStudents(student) as
select a1.student
from salsaStudents a1, salsaStudents a2, salsaStudents a3 
where a1.student = a2.student and a2.student = a3.student and 
(a1.level = 1) and (a2.level = 2) and (a3.level = 3) and
(a1.teacher <> a2.teacher) and (a2.teacher <> a3.teacher);

/* Students of basic level with different teachers and no intensive level */
/* Two equivalent formulations below (the first one is not accepted by MySQL) */

/*create or replace view candidates(student) as
  select student
  from  basicLevelStudents
except
  select student
  from  intensiveStudents;
*/
create or replace view candidates(student) as
  select student
  from  basicLevelStudents
where student not in (
  select student
  from  intensiveStudents);

/* Candidates with no past due payments */

create or replace view awards(student) as
  select student
  from candidates c
  where  
      c.student not in (select student
                    from payment
                    where state = 'unpaid')
      OR  /* UNION */
      c.student in (select student from grants);
      