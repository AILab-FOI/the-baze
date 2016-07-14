% Naive Fibonacci

/abolish
/multiline on
/timing on
/running_info off
/display_answer off

create view fib(n,f) as
  select 0,1.0
  union
  select 1,1.0
  union
  select fib1.n+1,fib1.f+fib2.f
  from fib fib1, fib fib2
  where fib1.n=fib2.n+1 and fib1.n<10;
  
-- fib(0,1).
-- fib(1,1).
-- fib(N,F) :-
--   N>1,
--   N2 is N-2,
--   fib(N2,F2),
--   N1 is N-1,
--   fib(N1,F1),
--   F is F2+F1.
  
/multiline off
