-- File: jobs.sql
-- Date: 09/17/2013
-- Author: Fernando Sáenz-Pérez
-- Description:
--   A company has employees, employee positions, and dependencies between positions.
--   Constraints: 
--     * A given employee cannot have more than a boss
--     * Position dependencies can not be circular

-- Note: A Datalog formulation can be found in jobs.dl

/abolish
/multiline on
/duplicates off

-- Job Positions
CREATE TABLE p(pos STRING);
INSERT INTO p values ('p1');
INSERT INTO p values ('p2');
INSERT INTO p values ('p3');
INSERT INTO p values ('p4');
INSERT INTO p values ('p5');

-- Job Position Dependencies
CREATE TABLE d(bos STRING, sub STRING);
INSERT INTO d values ('p1','p2');
INSERT INTO d values ('p2','p3');
INSERT INTO d values ('p2','p4');
INSERT INTO d values ('p1','p5');

-- Positions of Employees
CREATE TABLE ep(emp STRING, pos STRING);
INSERT INTO ep values ('e1','p1');
INSERT INTO ep values ('e2','p2');
INSERT INTO ep values ('e3','p3');
INSERT INTO ep values ('e4','p4');
INSERT INTO ep values ('e5','p5');


-- Direct Boss
CREATE VIEW db(bos,sub) AS
  (SELECT ep1.emp, ep2.emp 
   FROM d, ep AS ep1, ep AS ep2 
   WHERE d.bos=ep1.pos AND d.sub=ep2.pos);

-- Boss
CREATE VIEW b(bos,sub) AS
  (SELECT * FROM db)
  UNION 
  (SELECT b1.bos, b2.sub
   FROM b AS b1, b AS b2
   WHERE b1.sub=b2.bos);

-- Employees that have more than one direct boss
CREATE VIEW nb(emp,num) AS
	SELECT sub, COUNT(*)
	FROM db
	GROUP BY sub
	HAVING COUNT(*)>1;

-- May position 4 depend on position 5?
-- No, because employee 4 would have 2 bosses
ASSUME SELECT 'p5','p4' IN d
SELECT * FROM nb;
-- answer(nb.emp:string(varchar),nb.num:number(integer)) ->
-- {
--   answer(e4,2)
-- }
-- Info: 1 tuple computed.          

-- May employee 6 be assigned to position 2?
-- No, because employees 3 and 4 would have 2 bosses
ASSUME SELECT 'e6','p2' IN ep
SELECT * FROM nb;
-- answer(nb.emp:string(varchar),nb.num:number(integer)) ->
-- {
--   answer(e3,2),
--   answer(e4,2)
-- }
-- Info: 2 tuples computed.          

-- May position 1 depend on position 4?
-- No, because it would imply a circular boss dependency
ASSUME SELECT 'p4','p1' IN d
SELECT * FROM b 
WHERE b.bos=b.sub;
-- answer(b.bos:string(varchar),b.sub:string(varchar)) ->
-- {
--   answer(e1,e1),
--   answer(e2,e2),
--   answer(e4,e4)
-- }
-- Info: 3 tuples computed.          


-- Mutual recursion in a WITH statement
-- Direct boss positions and
-- Direct subordinate positions
WITH 
  dbp(bos,sub) AS
    SELECT bos,sub FROM d
    UNION ALL
    SELECT bos,sub from dsp,
  dsp(sub,bos) AS
    SELECT sub,bos from dbp
  SELECT * FROM dbp;
  
-- Mutually recursive with assume  
WITH 
  dbp(bos,sub) AS
    ASSUME SELECT bos,sub FROM d IN dsp(bos,sub)
    SELECT bos,sub from dsp,
  dsp(sub,bos) AS
    SELECT sub,bos from dbp
  SELECT * FROM dbp;
  
-- answer(dbp.bos:string(varchar),dbp.sub:string(varchar)) ->
-- {
--   answer(p1,p2),
--   answer(p1,p5),
--   answer(p2,p3),
--   answer(p2,p4)
-- }
-- Info: 4 tuples computed.          

-- The same as above, but creating views:
-- CREATE VIEW dbp(bos,sub) AS
--     SELECT bos,sub FROM d
--     UNION ALL
--     SELECT bos,sub from dsp;
--     
-- CREATE VIEW dsp(sub,bos) AS
--     SELECT sub,bos from dbp;
--     
-- SELECT * FROM dbp;
  
-- Mutual recursion in a WITH statement
-- Direct boss employees and
-- Direct subordinate employees

WITH 
  dbe(bos,sub) AS
    SELECT bos,sub FROM db
    UNION ALL
    SELECT bos,sub from dse,
  dse(sub,bos) AS
    SELECT sub,bos from dbe
  SELECT * FROM dbe;

-- answer(dbe.bos:string(varchar),dbe.sub:string(varchar)) ->
-- {
--   answer(e1,e2),
--   answer(e1,e5),
--   answer(e2,e3),
--   answer(e2,e4)
-- }
-- Info: 4 tuples computed.          

-- The same as above, but creating views:
-- CREATE VIEW dbe(bos,sub) AS
--     SELECT bos,sub FROM db
--     UNION ALL
--     SELECT bos,sub from dse;
--     
-- CREATE VIEW dse(sub,bos) AS
--     SELECT sub,bos from dbe;
--     
-- SELECT * FROM dbe;

-- Hypothetical query in mutual recursive definitions

-- Assuming that employee e2 is assigned to position 4, is there a circular boss dependency?
-- ASSUME SELECT 'e2','p4' IN ep
-- SELECT dbe.bos,dbe.sub FROM dbe,b WHERE b.bos=b.sub

-- DES> ep(e2,p4)=>dbe(B,S),b(B,B).

-- Info: Processing:
--   answer(B,S) :-
--     ep(e2,p4)=>dbe(B,S),b(B,B).
-- {                                           
--   answer(e2,e2),
--   answer(e2,e3),
--   answer(e2,e4)
-- }
-- Info: 3 tuples computed.          
