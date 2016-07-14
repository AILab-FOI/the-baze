-- SQL Debugger
-- November 2011
-- Example 2
--
-- Scenario 1: Code 5.2 test fails
--   Missing v(1)
--   t(1) in t but not in v
--   Buggy: v
-- Scenario 2: Code 
--   Wrong v(2)
--   t(2) should not be in t
--   Buggy: t
/abolish
/development on
create table t(a int);
create view v(a) as select * from t where a>1;
insert into t values (1);
insert into t values (2);
-- /debug_sql v trust_tables(no)