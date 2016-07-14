%
% Family
%
% SQL Formulation

/multiline on
/abolish
/show_compilations on

create table father(father string,child string);
create table mother(mother string,child string);

insert into father values('tom','amy');
insert into father values('jack','fred');
insert into father values('tony','carolII');
insert into father values('fred','carolIII');

insert into mother values('grace','amy');
insert into mother values('amy','fred');
insert into mother values('carolI','carolII');
insert into mother values('carolII','carolIII');

create view parent(parent,child) as
  select * from father union select * from mother;
  
create or replace view ancestor(ancestor,descendant) as
  with recursive rec_ancestor(ancestor,descendant) as
     select * from parent
    union 
     select parent,descendant 
     from parent,rec_ancestor 
     where parent.child=rec_ancestor.ancestor 
  select * from rec_ancestor;
  
-- View 'ancestor' can be rewritten in the following simplified equivalent view:
create or replace view ancestor(ancestor,descendant) as
   select parent,child from parent 
  union
   select parent,descendant 
   from parent,ancestor 
   where parent.child=ancestor.ancestor;

/development off
/dbschema
/development on
/dbschema

-- Tom's descendants:
select * from ancestor where ancestor='tom';
   
/development off
/dbschema
/development on
/dbschema
-- Tom's descendants:
select * from ancestor where ancestor='tom';
-- Children and their parents:
select father.child,father,mother 
 from father,mother 
 where father.child=mother.child;
