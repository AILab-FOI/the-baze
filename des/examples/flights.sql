%
% Flights
%
% SQL formulation

/show_compilations on
/multiline on

CREATE OR REPLACE TABLE flight(origin string, destination string, time real);
CREATE OR REPLACE TABLE travel(origin string, destination string, time real);

INSERT INTO flight VALUES('london','ny',9.0);
INSERT INTO flight VALUES('mad','par',1.5);
INSERT INTO flight VALUES('par','ny',10.0);

CREATE OR REPLACE VIEW travel(origin,destination,time) AS 
  WITH connected(origin,destination,time) AS 
       SELECT * FROM flight 
       UNION 
       SELECT flight.origin,connected.destination,flight.time+connected.time 
       FROM flight,connected 
       WHERE flight.destination = connected.origin 
  SELECT * FROM connected;

SELECT * FROM travel;

/multiline off
