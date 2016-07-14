%
% ODBC connection 'access' is required
% 
/abolish
/multiline on 
/duplicates on 
/show_compilations on

/open_db access
drop view flight;
drop table flight_des_table;
drop table flight_des_metadata;
/use_db $des

-------
-- First example: Assuming a tuple in a persisted relation
-------
CREATE TABLE flight(origin string, destination string, duration real);
INSERT INTO flight VALUES('lon','ny',9.0);
INSERT INTO flight VALUES('mad','par',1.5);
INSERT INTO flight VALUES('par','ny',10.0);

CREATE OR REPLACE VIEW travel(origin,destination,duration) AS
 WITH connected(origin,destination,duration) AS 
   SELECT * FROM flight 
 UNION 
   SELECT flight.origin,connected.destination,
          flight.duration+connected.duration 
   FROM flight,connected 
   WHERE flight.destination = connected.origin 
 SELECT * FROM connected;

:-persistent(flight(origin:string,destination:string,duration:float),access).

ASSUME SELECT 'mad','lon',2.0
  IN flight(origin,destination,duration)
  SELECT * FROM travel;
 
-- Expected result:

-- answer(travel.origin:string(varchar),travel.destination:string(varchar),travel.duration:number(float)) ->
-- {                                                             
--   answer(lon,ny,9.0),
--   answer(mad,lon,2.0),
--   answer(mad,ny,11.0),
--   answer(mad,ny,11.5),
--   answer(mad,par,1.5),
--   answer(par,ny,10.0)
-- }
-- Info: 6 tuples computed.          


-------
-- Second example: Assuming a recursive statement
-------

CREATE VIEW connect(origin,destination) AS
     SELECT origin,destination FROM flight;
     
ASSUME 
       (SELECT flight.origin,connect.destination
        FROM flight,connect
        WHERE flight.destination = connect.origin) 
     IN 
       connect(origin,destination) 
     SELECT * FROM connect;         

-- Expected result:
-- answer(connect.origin:string(varchar),connect.destination:string(varchar)) ->
-- {
--   answer(lon,ny),
--   answer(mad,ny),
--   answer(mad,par),
--   answer(par,ny)
-- }
-- Info: 4 tuples computed.          

