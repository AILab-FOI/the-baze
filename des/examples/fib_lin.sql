-- Linear Fibonacci
-- SQL Formulation
-- Date: Dec-10th-2013

/abolish
/multiline on

-- function f(a, b, n)
--  { if(n <= 1) return b;
--    else return f(b, a+b, n-1);
--  }

-- function fib(n)
--  { return f(0, 1, n); }
 
-- create view lf(a,b,n) as
--   select 0, 1, 0
--   union all
--   select 1, 1, 1
--   union all
--   select b, a+b, n+1 from lf where n>=1 and n<20;

-- create or replace view fib(n,f) as
--   select n,b from lf;
  
create or replace view fib(n,f) as
  WITH fib3 (a,b,n) AS (
    SELECT 0,1,0
    UNION ALL
    SELECT 1,1,1
    UNION ALL
    SELECT b,a+b,n+1
    FROM fib3
    WHERE n<20
  )
  SELECT n,b FROM fib3;

  

-- DES> select * from fib;       

-- answer(fib.n:number(integer),fib.f:number(integer)) ->
-- {
--   answer(0,1),
--   answer(1,1),
--   answer(2,2),
--   answer(3,3),
--   answer(4,5),
--   answer(5,8),
--   answer(6,13),
--   answer(7,21),
--   answer(8,34),
--   answer(9,55),
--   answer(10,89),
--   answer(11,144),
--   answer(12,233),
--   answer(13,377),
--   answer(14,610),
--   answer(15,987),
--   answer(16,1597),
--   answer(17,2584),
--   answer(18,4181),
--   answer(19,6765),
--   answer(20,10946)
-- }
-- Info: 21 tuples computed.          

