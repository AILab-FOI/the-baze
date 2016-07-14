/abolish
/multiline on
/type_casting on
/order_answer off

boat(ori varchar(10),des varchar(10), time float) :=
  select 'spc','tfn',2   from dual
  union all
  select 'tfs','gmz',1   from dual
  union all
  select 'gmz','vde',1.5 from dual;

flight(ori varchar(10),des varchar(10), time float) :=
  select 'mad','tfn',2   from dual
  union all
  select 'mad','lpa',3   from dual
  union all
  select 'mp','vde',1    from dual;

bus(ori varchar(10),des varchar(10), time float) :=
  select 'tfn','tfs',2.5 from dual
  union 
  select 'lpa','mp',2    from dual
  union 
  select 'vde','res',1   from dual;

link(ori varchar(10),des varchar(10), time float) := 
  select * from flight 
  union all select * from boat
  union all select * from bus;

travel(ori varchar(10),des varchar(10), time float) :=
  select * from link
  union
  select link.ori, travel.des, link.time+travel.time
  from  link, travel where link.des=travel.ori; 

reachable(ori varchar(10), des varchar(10)) :=
  SELECT link.ori, link.des FROM link
  UNION
  SELECT link.ori, reachable.des FROM link,reachable WHERE link.des = reachable.ori;

departFromMad(ori varchar(10), des varchar(10)) :=
  select reachable.ori, reachable.des FROM reachable
  WHERE (reachable.ori = 'mad' OR reachable.des = 'mad');

avoidMad(ori varchar(10), des varchar(10)) :=
  SELECT reachable.ori, reachable.des FROM reachable
  EXCEPT
  SELECT departFromMad.ori, departFromMad.des FROM departFromMad;

mad_to_vde(time float) :=
  ASSUME
  SELECT * FROM boat WHERE boat.time > 1   NOT IN link
  SELECT travel.time FROM travel WHERE travel.ori = 'mad' and travel.des = 'vde';

-- Volcanic eruption at El Hierro:
new_travel(ori varchar(10), des varchar(10), time float) :=
  ASSUME
    (SELECT * FROM bus WHERE bus.ori = 'vde' UNION SELECT * FROM flight) NOT IN link,
    SELECT 'res','spc',1.5 IN boat
  SELECT * FROM link
  UNION
  SELECT link.ori, new_travel.des, link.time + new_travel.time 
  FROM link, new_travel
  WHERE link.des = new_travel.ori;

"new_travel'"(ori varchar(10), des varchar(10), time float) :=
  ASSUME
  SELECT * FROM bus WHERE bus.ori = 'vde' UNION SELECT * FROM flight NOT IN link,
  SELECT 'res','spc',1.5 IN boat
  SELECT * FROM travel;

limited_travel(ori varchar(10), des varchar(10), time float):=
  SELECT link.ori, link.des, link.time FROM link
  UNION
  SELECT link.ori, limited_travel.des, link.time + limited_travel.time
  FROM link, limited_travel
  WHERE link.des=limited_travel.ori AND
  link.time + limited_travel.time <= (SELECT SUM(time) FROM link);

-- Equivalent to the former
CREATE VIEW "limited_travel'"(ori, des, time) AS
  WITH RECURSIVE cte(ori, des, time) AS
  (SELECT link.ori, link.des, link.time FROM link
  UNION ALL
  SELECT link.ori, cte.des, link.time + cte.time FROM cte, link WHERE link.des=cte.ori)
  SELECT * FROM cte WHERE cte.time < (SELECT SUM(time) FROM link);
  
-- Aggregates and assumptions:
min_travel(ori varchar(10), des varchar(10), time float):=
  SELECT new_travel.ori, new_travel.des, MIN(time)
  FROM new_travel GROUP BY new_travel.ori, new_travel.des;

inc_min_travel(ori varchar(10), des varchar(10), time float):=
  SELECT travel.ori, travel.des, MIN(travel.time)-MIN(new_travel.time)
  FROM travel, new_travel
  WHERE travel.ori=new_travel.ori AND travel.des=new_travel.des
  GROUP BY travel.ori, travel.des;

-- Testing the relations:
select * from boat;
select * from flight;
select * from bus;
select * from link;
select * from travel;
select * from reachable;
select * from departFromMad;
select * from avoidMad;
select * from mad_to_vde;
select * from new_travel;
select * from "new_travel'";
select * from limited_travel;
select * from "limited_travel'";
select * from min_travel;
select * from inc_min_travel;

/type_casting off
