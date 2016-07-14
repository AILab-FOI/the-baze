%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hypothetical SQL Queries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/abolish
/multiline on
/hypothetical on
/show_compilations on
/compact_listings on

CREATE OR REPLACE TABLE flight(origin string, destination string, time real);

INSERT INTO flight VALUES('london','ny',9.0);
INSERT INTO flight VALUES('mad','par',1.5);
INSERT INTO flight VALUES('par','ny',10.0);

CREATE OR REPLACE VIEW travel(origin,destination,time) AS
  WITH connected(origin,destination,time) AS 
    SELECT * FROM flight 
   UNION 
    SELECT flight.origin,connected.destination,flight.time+connected.time 
    FROM flight,connected 
    WHERE flight.destination = connected.origin SELECT * FROM connected;

----------------------------------------------------
-- Assuming a new flight: flight('mad','london',2.0)
----------------------------------------------------

-- ASSUME syntax
ASSUME 
  SELECT 'mad','london',2.0 
IN 
  flight(origin,destination,time) 
SELECT * 
FROM travel;

-- WITH syntax (equivalent to the former)
WITH flight(origin,destination,flight) AS 
       SELECT 'mad','london',2.0 
SELECT time 
FROM travel;

----------------------------------------------------
-- Assuming a recursive case
----------------------------------------------------

-- If travel only contains the non-recursive case:
CREATE OR REPLACE VIEW travel(origin,destination,time) AS
 SELECT * FROM flight;

-- ASSUME syntax
ASSUME
  (SELECT flight.origin,travel.destination,flight.time+travel.time 
   FROM flight,travel  
   WHERE flight.destination = travel.origin)
IN
  travel(origin,destination,time)
SELECT * 
FROM travel;

-- WITH syntax
WITH travel(origin,destination,time) AS 
(SELECT flight.origin,travel.destination,flight.time+travel.time 
 FROM flight,travel 
 WHERE flight.destination = travel.origin
)
SELECT * FROM travel;
