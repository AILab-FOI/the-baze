%
% ODBC connection 'mysql' is required
% 
/abolish
/multiline on 
/duplicates on 
/show_compilations on
/open_db mysql
drop view edge;
drop table edge;
drop table edge_des_table;
drop table edge_des_metadata;
drop view path;
drop table path;
drop table path_des_table;
drop table path_des_metadata;
/use_db $des

CREATE TABLE edge(a int, b int);

INSERT INTO edge VALUES(1,2);
INSERT INTO edge VALUES(2,3);
INSERT INTO edge VALUES(1,3);

:-persistent(edge(a:int,b:int),mysql).
:-persistent(path(a:int,b:int),mysql).

WITH RECURSIVE path(a, b) AS
  SELECT * FROM edge
  UNION -- Discarding duplicates (ALL is not required)
  SELECT p1.a,p2.b
  FROM path p1, path p2
  WHERE p1.b=p2.a
SELECT * FROM path;

WITH RECURSIVE path(a, b) AS
  SELECT * FROM edge
  UNION ALL -- Keeping duplicates
  SELECT p1.a,p2.b
  FROM path p1, path p2
  WHERE p1.b=p2.a
SELECT * FROM path;
  