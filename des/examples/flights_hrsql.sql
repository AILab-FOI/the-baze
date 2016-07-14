/multiline on
/abolish

flight(ori varchar(10), des varchar(10), time float) :=
  SELECT 'MAD', 'TFN', 2.0
  UNION
  SELECT 'MAD', 'LPA', 3.0
  UNION
  SELECT 'MP', 'RES',  1.0;

boat(ori varchar(10), des varchar(10), time float) :=
  SELECT 'SPC', 'TFN', 2.0
  UNION
  SELECT 'TFS', 'GMZ', 1.0
  UNION
  SELECT 'GMZ', 'VDE', 1.5;

bus(ori varchar(10), des varchar(10), time float) :=
  SELECT 'TFN', 'TFS', 2.5
  UNION
  SELECT 'LPA', 'MP',  2.0;

link(ori varchar(10), des varchar(10), time float) :=
  SELECT * FROM flight
  UNION 
  SELECT * FROM boat
  UNION 
  SELECT * FROM bus;

travel(ori varchar(10), des varchar(10), time float):=
  SELECT * FROM link
  UNION SELECT link.ori, travel.des, link.time + travel.time
  FROM link, travel WHERE link.des=travel.ori;

limited_travel(ori varchar(10), des varchar(10), time float):=
  SELECT link.ori, link.des, link.time FROM link
    UNION
  SELECT link.ori, limited_travel.des, link.time + limited_travel.time 
  FROM link, limited_travel
  WHERE link.des=limited_travel.ori AND
   link.time + limited_travel.time <= (SELECT SUM(time) FROM link);

CREATE OR REPLACE VIEW limited_travel(ori, des, time) AS
  WITH RECURSIVE cte(ori, des, time) AS
  (SELECT link.ori, link.des, link.time FROM link
   UNION ALL
   SELECT link.ori, cte.des, link.time + cte.time FROM cte, link WHERE link.des=cte.ori)
  SELECT * FROM cte WHERE cte.time < (SELECT SUM(time) FROM link);

reachable(ori varchar(10), des varchar(10)) :=
  SELECT link.ori, link.des FROM link
  UNION
  SELECT link.ori, reachable.des FROM link,reachable WHERE link.des = reachable.ori;

departFromMad(ori varchar(10), des varchar(10)) :=
  SELECT reachable.ori, reachable.des FROM reachable
  WHERE (reachable.ori = 'mad' OR reachable.des = 'mad');

avoidMad(ori varchar(10), des varchar(10)) :=
  SELECT reachable.ori, reachable.des FROM reachable
  EXCEPT 
  SELECT departFromMad.ori, departFromMad.des FROM departFromMad;

-- volcano_reachable(ori varchar(10),des varchar(10)) :=
--   ASSUME 
--     SELECT * FROM bus WHERE bus.ori = 'VDE' UNION SELECT * FROM flight NOT IN link,
--     SELECT 'RES','SPC',1.5 IN boat
--   SELECT * FROM reachable;
  
volcano_travel(ori varchar(10),des varchar(10), time float) :=
  ASSUME 
--    SELECT * FROM bus WHERE bus.ori = 'VDE' UNION SELECT * FROM flight NOT IN link,
    SELECT 'RES','SPC',1.5 IN boat
  SELECT * FROM travel;
  
min_travel(ori varchar(10), des varchar(10), time float):=
  SELECT volcano_travel.ori, volcano_travel.des, MIN(time) 
  FROM volcano_travel GROUP BY volcano_travel.ori, volcano_travel.des;
  
inc_min_travel(ori varchar(10), des varchar(10), time float):=
  SELECT travel.ori, travel.des, MIN(travel.time)-MIN(volcano_travel.time)
  FROM travel, volcano_travel
  WHERE travel.ori=volcano_travel.ori AND travel.des=volcano_travel.des
  GROUP BY travel.ori, travel.des;
  