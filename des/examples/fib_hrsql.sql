/multiline on
/abolish

-- fibev(n int, f int) := SELECT 0,1;
-- fibod(n int, f int) := SELECT 1,1;

-- fib(n int, f int) :=
--   ASSUME
--     SELECT fibev.n+2, fibev.f + fibod.f
--     FROM fibev, fibod WHERE fibod.n = fibev.n+1 AND fibev.n<10 IN fibev(n, f),
--     SELECT fibod.n+2, fibev.f + fibod.f
--     FROM fibev, fibod WHERE fibev.n = fibod.n+1 AND fibod.n<10 IN fibod(n, f)
--   SELECT * FROM fibev UNION SELECT * FROM fibod;

-- fib(n int, f int) :=
--   ASSUME
--     SELECT 0,1 UNION 
--     SELECT fibev.n+2, fibev.f + fibod.f
--     FROM fibev, fibod WHERE fibod.n = fibev.n+1 AND fibev.n<10 IN fibev(n,f),
--     SELECT 1,1 UNION 
--     SELECT fibod.n+2, fibev.f + fibod.f
--     FROM fibev, fibod WHERE fibev.n = fibod.n+1 AND fibod.n<10 IN fibod(n,f)
--   SELECT * FROM fibev UNION SELECT * FROM fibod;

fib(n int, f int) := 
  WITH
    fibev(n,f) AS
      SELECT 0,1 UNION 
      SELECT fibev.n+2, fibev.f + fibod.f
      FROM fibev, fibod WHERE fibod.n = fibev.n+1 AND fibev.n<10,
    fibod(n,f) AS
      SELECT 1,1 UNION 
      SELECT fibod.n+2, fibev.f + fibod.f
      FROM fibev, fibod WHERE fibev.n = fibod.n+1 AND fibod.n<10
  SELECT * FROM fibev UNION SELECT * FROM fibod;

SELECT * FROM fib;