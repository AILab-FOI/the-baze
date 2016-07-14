% Naive Fibonacci
% SQL formulation
% To avoid non-termination, use top-N queries

/multiline on

create view fib(n,f) as
  select 0,1
  union
  select 1,1
  union
  select fib1.n+1,fib1.f+fib2.f
  from fib fib1, fib fib2
  where fib1.n=fib2.n+1;
  
-- select top 10 * from fib;
  
/multiline off
