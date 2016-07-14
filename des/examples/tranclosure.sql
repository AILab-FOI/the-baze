%
% Transitive closure of relations p and q
%
% SQL Formulation

/multiline on

create table p(x string,y string);
create table q(x string,y string);

insert into p values ('a','b');
insert into p values ('c','d');

insert into q values ('b','c');
insert into q values ('d','e');

create view pqs(x,y) as
  select * from p 
 union 
  select * from q 
 union 
  select pqs.x,p.y from pqs,p where pqs.y=p.x 
 union 
  select pqs.x,q.y from pqs,q where pqs.y=q.x;
  
select * from pqs;

/multiline off
