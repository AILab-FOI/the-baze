--
-- Aggregates
--
-- SQL Formulation
--

/multiline on

create or replace table employee(name string, department string, salary int);
insert into employee values('anderson','accounting',1200);
insert into employee values('andrews','accounting',1200);
insert into employee values('arlingon','accounting',1000);
insert into employee values('nolan',null,null);
insert into employee values('norton',null,null);
insert into employee values('randall','resources',800);
insert into employee values('sanders','sales',null);
insert into employee values('silver','sales',1000);
insert into employee values('smith','sales',1000);
insert into employee values('steel','sales',1020);
insert into employee values('sullivan','sales',null);
select * from employee;
select count(*) from employee;
select count(salary) from employee;
select count(*),count(salary),min(salary),max(salary),sum(salary),avg(salary),times(salary) 
  from employee;
select * from employee where name = (select min(name) from employee);
select min(name),max(name),avg(salary) from employee;
select department,count(salary),sum(salary),avg(salary),min(salary),max(salary)
 from employee 
 group by department;
create or replace table parking(name string, lot string);
insert into parking values('anderson','a-1');
insert into parking values('randall','r-1');
insert into parking values('silver','s-1');
create or replace view ds(a,b) as
  select department,max(salary) 
  from employee 
  group by department;
select * from ds;
select * from employee natural inner join parking;
select department,max(lot)
  from employee natural inner join parking 
  group by department;
/development off
/dbschema
/development on
/dbschema
/development off
/multiline off
