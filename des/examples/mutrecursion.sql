--
-- Mutual recursion
--
-- SQL formulation

/assert p(a)
/assert p(b)
/assert q(c)
/assert q(d)

-- View q must be given a prototype for view p to be defined
create view q(x) as select * from q;
create or replace view p(x) as select * from q;
create or replace view q(x) as select * from p;

select * from p;
select * from q;
