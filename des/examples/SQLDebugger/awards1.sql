-- SQL Debugger
-- November 2011
-- Example 1 Awards
-- Missing tuple 'Anna', c.f. Section 4 of FLOPS paper
-- Refer to example1UserTrace.txt for a user trace and example1DevelopmentTrace.txt for a development trace
--
-- Declarative Debugging of Wrong and Missing Answers for SQL Views
-- FLOPS 2012

/abolish
/multiline on
/development off

/* Tables */
DROP TABLE IF EXISTS registration;
DROP TABLE IF EXISTS courses;
DROP TABLE IF EXISTS allInOneCourse;

CREATE TABLE allInOneCourse (
  student varchar(45) NOT NULL,
  pass integer,
  PRIMARY KEY (student)
);


CREATE TABLE courses (
  id varchar(11) NOT NULL,
  level int(11) DEFAULT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE registration (
  student varchar(45) NOT NULL,
  course varchar(11) NOT NULL,
  pass integer,
  FOREIGN KEY(course) references courses(id),
  PRIMARY KEY (student,course)
);

INSERT INTO allInOneCourse(student,pass)  VALUES ('Adrian',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Alba',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Alisha',0);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Amaya',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Arabella',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Ava',0);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Dexter',0);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Emma',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Evelyn',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Gabriel',0);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Gavin',0);
INSERT INTO allInOneCourse(student,pass)  VALUES ('George',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Harper',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Henry',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('James',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Jennifer',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Maggy',0);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Miguel',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Nicolas',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Noah',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Olivia',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Owen',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Paul',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Rocco',0);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Shane',0);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Stella',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Tosca',1);
INSERT INTO allInOneCourse(student,pass)  VALUES ('Violet',1);


INSERT INTO courses(id,level)  VALUES ('c1',  1);
INSERT INTO courses(id,level)  VALUES ('c2',  2);
INSERT INTO courses(id,level)  VALUES ('c3',  3);
INSERT INTO courses(id,level)  VALUES ('c4',  4);
INSERT INTO courses(id,level)  VALUES ('c5',  5);
INSERT INTO courses(id,level)  VALUES ('c0',  0);

INSERT INTO registration(student,course,pass) VALUES  ('Alba','c1',0);
INSERT INTO registration(student,course,pass) VALUES  ('Alba','c2',1);
INSERT INTO registration(student,course,pass) VALUES  ('Anna','c0',1);
INSERT INTO registration(student,course,pass) VALUES  ('Anna','c1',1);
INSERT INTO registration(student,course,pass) VALUES  ('Anna','c2',1);
INSERT INTO registration(student,course,pass) VALUES  ('Anna','c3',0);
INSERT INTO registration(student,course,pass) VALUES  ('Carla','c0',1);
INSERT INTO registration(student,course,pass) VALUES  ('James','c0',1);
INSERT INTO registration(student,course,pass) VALUES  ('James','c1',1);
INSERT INTO registration(student,course,pass) VALUES  ('James','c2',1);
INSERT INTO registration(student,course,pass) VALUES  ('James','c3',1);
INSERT INTO registration(student,course,pass) VALUES  ('Juan','c1',1);
INSERT INTO registration(student,course,pass) VALUES  ('Juan','c2',0);
INSERT INTO registration(student,course,pass) VALUES  ('Juan','c5',1);
INSERT INTO registration(student,course,pass) VALUES  ('Mica','c1',1);
INSERT INTO registration(student,course,pass) VALUES  ('Mica','c2',0);
INSERT INTO registration(student,course,pass) VALUES  ('Miguel','c2',1);
INSERT INTO registration(student,course,pass) VALUES  ('Paul','c0',1);
INSERT INTO registration(student,course,pass) VALUES  ('Pedro','c2',1);

/* Views */

/* Students, level of their courses and  pass/fail flag */
create or replace view standard(student, level, pass) as
   select R.student, C.level, R.pass
   from courses C, registration R
   where C.id = R.course;

/* Students from the basic course (level = 0) */
create or replace view basic(student) as
   select S.student
   from standard S
   where S.level = 0 and S.pass=1;

/* Intensive students */
create or replace view intensive(student) as
   (select A.student
    from allInOneCourse A  where A.pass=1)
   union
   (select A1.student
    from standard A1, standard A2, standard A3
    where A1.student = A2.student and A2.student = A3.student
          and
          A1.level = 1  and A2.level = 2 and A3.level = 3);

/* Awards are only for students from the basic level */
create or replace view awards(student) as
  select student from  basic
  except
  select student from  intensive;

/*
create or replace view awards(student) as
select student
  from  basic
  where student not in (select student from intensive);
*/