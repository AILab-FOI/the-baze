-- Database Systems. The Complete Book. H. Garcia-Molina, J.D. Ullman, J. Widom. 2002
-- Non-stratifiable program

CREATE TABLE r(x int);
--CREATE TABLE p(x int);
--CREATE TABLE q(x int);
WITH RECURSIVE p(x) AS (SELECT * FROM r) EXCEPT (SELECT * FROM q), RECURSIVE q(x) AS (SELECT * FROM r) EXCEPT (SELECT * FROM p) SELECT * FROM p;
-- Alternative coding with NOT IN instead of EXCEPT
WITH RECURSIVE p(x) AS (SELECT * FROM r WHERE x NOT IN SELECT * FROM q), RECURSIVE q(x) AS (SELECT * FROM r WHERE x NOT IN SELECT * FROM p) SELECT * FROM p;
