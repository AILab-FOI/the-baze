-- SQL Debugger
-- November 2011
-- Example 4
--
-- Trust file. u does not match with u in e4.sql
create view u(a) as select * from s where a>=1 or a>0 union select 3;