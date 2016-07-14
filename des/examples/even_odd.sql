/multiline on
 
WITH 
  even(x) AS
  SELECT 0
  UNION ALL
  SELECT odd.x+1
  FROM odd,

  odd(x) AS
  SELECT even.x+1
  FROM even
       
--  SELECT top 10 x FROM even
  SELECT top 10 x FROM odd;