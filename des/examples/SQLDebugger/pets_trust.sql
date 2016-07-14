% Case distinction
% Single-line statements
% natural join -> natural inner join

%/sql

%--- 
create or replace view AnimalOwner(id,aname,specie) as select O.id, P.name, P.specie from Owner O, Pet P, PetOwner PO where O.id = PO.id and P.code = PO.code;
create or replace view LessThan6(id) as select id from AnimalOwner where specie='cat' or specie='dog' group by id having count(*)<6;

%--------- correct version of CatsAndDogsOwner
create or replace view CatsAndDogsOwner(id,aname) as select AO1.id,AO1.aname from AnimalOwner AO1, AnimalOwner AO2 where AO1.id = AO2.id and ((AO1.specie='dog' and AO2.specie='cat') or (AO2.specie='dog' and AO1.specie='cat'));
%--------- incorrect version of CatsAndDogsOwner
-- create or replace view CatsAndDogsOwner(id,aname) as select AO1.id,AO1.aname from AnimalOwner AO1, AnimalOwner AO2 where AO1.id = AO2.id and AO1.specie='dog' and AO2.specie='cat';

create or replace view NoCommonName(id) as  (select id from CatsAndDogsOwner)  except  (select B.id from CatsAndDogsOwner A, CatsAndDogsOwner B where A.id <> B.id and A.aname = B.aname);
create or replace view Guest(id,name) as select distinct O.id, O.name from Owner O, NoCommonName N, LessThan6 L where O.id = N.id and N.id = L.id;

