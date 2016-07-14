%
% Shortest Paths in a Graph
%
% SQL Formulation

/abolish
/multiline on
/show_compilations on

create table edge(origin string,destination string);

insert into edge values('a','b');
insert into edge values('a','c');
insert into edge values('b','a');
insert into edge values('b','d');

create or replace view spaths(origin,destination,length) as
  with recursive path(origin,destination,length) as
    (select edge.*,1 from edge)
   union
    (select path.origin,edge.destination,path.length+1 
     from path,edge 
     where path.destination=edge.origin and
           path.length<(select count(*) from edge))
  select origin,destination,min(length) 
  from path 
  group by origin,destination;

select * from spaths;

/multiline off
