-- SQL Debugger
-- November 2011
-- Example 4
--
-- Trust files:
--   e4trust.sql (u trusted)
--   e4trust2.sql (u trusted: non-valid w.r.t new u)
/abolish
/multiline on
/development on
create or replace table t(a int);
create or replace table s(a int);
create or replace view u(a) as select * from s where a>=1;
create or replace view v(a) as select * from t where a>1 union select * from u where a>2;
insert into t values (1);
insert into t values (2);
insert into s values (1);
-- /debug_sql v trust_tables(no) trust_file('examples/SQLDebugger/e4trust')
-- /debug_sql v trust_tables(no) trust_file('examples/SQLDebugger/e4trust2')
