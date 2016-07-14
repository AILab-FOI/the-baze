%
% Russell's Paradox
%
% SQL Formulation

/abolish
/multiline on
/show_compilations on

create table man(man string);
insert into man values ('barber');
insert into man values ('mayor');

create view shaves(man,to) as 
	  select 'barber',man 
	  from man 
  except 
    select * 
    from shaves 
    where man=to;
    
select * from shaves;

/multiline off
