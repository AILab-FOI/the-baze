-- Recursive views
-- Example taken from:
-- [GUW02] H. Garcia-Molina, J. D. Ullman, J. Widom, "Database Systems: The Complete Book", Prentice-Hall, 2002.
/multiline on
:-persistent(flights(airline:string,frm:string,to:string,departs:int,arrives:int),mysql).
create table flights(airline string,frm string,to string,departs int,arrives int);
insert into flights values ('UA','SF','DEN',930,1230);
insert into flights values ('AA','SF','DAL',900,1430);
insert into flights values ('UA','DEN','CHI',1500,1800);
insert into flights values ('UA','DEN','DAL',1400,1700);
insert into flights values ('AA','DAL','CHI',1530,1730);
insert into flights values ('AA','DAL','NY',1500,1930);
insert into flights values ('AA','CHI','NY',1900,2200);
insert into flights values ('UA','CHI','NY',1830,2130);
select * from flights;
-- P.493 [GUW02]
-- WITH RECURSIVE Reaches(frm,to) AS
--   (SELECT frm,to FROM flights) 
--    UNION 
--    (SELECT R1.frm,R2.to FROM Reaches AS R1, Reaches AS R2 WHERE R1.to=R2.frm)
--   SELECT * FROM Reaches;
-- We create a view
CREATE VIEW Reaches2(frm,to) AS
  (SELECT frm,to FROM flights) 
   UNION 
   (SELECT R1.frm,R2.to FROM Reaches2 AS R1, Reaches2 AS R2 WHERE R1.to=R2.frm);
-- P.495 [GUW02]
WITH 
 Triples(airline,frm,to) AS 
   SELECT airline,frm,to 
   FROM flights,
 RECURSIVE Reaches(airline,frm,to) AS 
   (SELECT * FROM Triples) 
   UNION 
   (SELECT Triples.airline,Triples.frm,Reaches.to
    FROM Triples,Reaches
    WHERE Triples.to = Reaches.frm AND Triples.airline=Reaches.airline) 
(SELECT frm,to FROM Reaches WHERE airline = 'UA') 
EXCEPT 
(SELECT frm,to FROM Reaches WHERE airline = 'AA');
-- View with simplified formulation
CREATE VIEW Reach(frm,to) AS
  WITH Triples(airline,frm,to) AS
    SELECT airline,frm,to FROM flights, 
    RECURSIVE Reaches(airline,frm,to) AS
      (SELECT * FROM Triples)
      UNION
      (SELECT Triples.airline,Triples.frm,Reaches.to 
       FROM Triples,Reaches 
       WHERE Triples.to = Reaches.frm AND Triples.airline=Reaches.airline)
  (SELECT frm,to FROM Reaches WHERE airline = 'UA') 
  EXCEPT 
  (SELECT frm,to FROM Reaches WHERE airline = 'AA');
