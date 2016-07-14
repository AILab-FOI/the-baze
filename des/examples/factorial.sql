--
-- Factorial. SQL definition
--

WITH factorial AS SELECT 0,1 UNION SELECT n+1,(n+1)*f FROM factorial SELECT TOP 5 * FROM factorial; 
