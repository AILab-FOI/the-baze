-- File: hubs_auth.dl
-- Date: 10/03/2013

-- Example: Hubs and Authorities
-- c.f. Jennifer Widom Slides
-- Non-stratifiable program
-- Note: An equivalen Datalog code is in file hubs_auth.dl

-- Description:
-- Intuition: A web page is important if important pages link to it.
-- A Hub is a node that points to enough authorities (let's say, 3)
-- An Authority is a node that is pointed by enough hubs (let's say, 3)

-- EDB:
--   Link(src, dest): Defines the graph
--   HubStart(node) : Start hub nodes (already known as hubs)
--   AuthStart(node): Start authority nodes (already known as authorities)

-- Hub points to >= 3 Authority
-- Authority points to >= 3 Hub

/multiline on
/abolish

create table Link(src string, dest string);
create table HubStart(node string);
create table AuthStart(node string);

with recursive

  Hub(node) as 
    (select node from HubStart
       union
     select src as node from Link L
     where dest in (select node from Auth)
     group by src having count(*) >= 3),
     
  Auth(node) as 
    (select node from AuthStart
       union
     select dest as node from Link L
     where src in (select node from Hub)
     group by dest having count(*) >= 3)
     
select * from Hub;

