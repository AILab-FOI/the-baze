-- SQL Debugger
-- November 2011
-- Example 3
--
-- UNION query
-- v(2) incorrect
-- t(2) incorrect
/abolish
/development on
create table t(a int);
create table s(a int);
create view v(a) as select * from t where a>1 union select * from s where a>2;
insert into t values (1);
insert into t values (2);
insert into s values (1);
-- /debug_sql v trust_tables(no)