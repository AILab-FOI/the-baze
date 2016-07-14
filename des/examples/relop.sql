--
-- Relational Algebra Operations
--
-- SQL Formulation

-- Creating tables
create or replace table a(a string);
create or replace table b(b string);
create or replace table c(a string,b string);
-- Listing the database schema
/dbschema
-- Inserting values into tables
insert into a values ('a1');
insert into a values ('a2');
insert into a values ('a3');
insert into b values ('b1');
insert into b values ('b2');
insert into b values ('a1');
insert into c values ('a1','b2');
insert into c values ('a1','a1');
insert into c values ('a2','b2');
-- Testing the just inserted values
select * from a;
select * from b;
select * from c;
-- Projection
select a from c;
-- Selection
select a from a where a>='a2';
-- Cartesian product
select * from a,b;
-- Inner Join
select a from a inner join b on a.a=b.b;
-- Left Join
select * from a left join b on a.a=b.b;
-- Right Join
select * from a right join b on a.a=b.b;
-- Full Join
select * from a full join b on a.a=b.b;
-- Union 
select * from a union select * from b;
-- Difference
select * from a except select * from b;
