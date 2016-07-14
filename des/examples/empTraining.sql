--
-- Understanding Relational Database Query Languages, S. W. Dietrich, Prentice Hall, 2001.
--
-- ---------------------------------------------------------------------------------------
--                 		SQL
-- ---------------------------------------------------------------------------------------
--          EMPLOYEE TRAINING Enterprise
--
--  employee(eID, eLast, eFirst, eTitle, eSalary)	key: eID
--  technologyArea(aID, aTitle, aURL, aLeadID)	  key: aID
--  trainingCourse(cID, cTitle, cHours, areaID)	  key: cID
--  takes(eID, cID, tYear, tMonth, tDay)	        key: eID, cID
--
-- ---------------------------------------------------------------------------------------
--

/multiline on
/abolish
/show_compilations on

create table employee (eID string primary key, eLast string, eFirst string, eTitle string, eSalary int);
create table technologyArea(aID string primary key, aTitle string, aURL string, aLeadID string);
create table trainingCourse(cID string primary key, cTitle string, cHours int, areaID string);
create table takes(eID string, cID string, tYear int, tMonth int, tDay int, primary key (eID, cID));

insert into employee values ('111','Last111','First111','Database Administrator',75111);
insert into employee values ('222','Last222','First222','Software Engineer',51722);
insert into employee values ('321','Last321','First321','Database Administrator',68321);
insert into employee values ('333','Last333','First333','Sr Software Engineer',60333);
insert into employee values ('345','Last345','First345','Sr Software Engineer',59345);
insert into employee values ('369','Last369','First369','Software Engineer',36369);
insert into employee values ('444','Last444','First444','Software Engineer',44444);
insert into employee values ('456','Last456','First456','Software Engineer',45456);
insert into employee values ('555','Last555','First555','Sr Software Engineer',55555);
insert into employee values ('654','Last654','First654','Coach',60654);
insert into employee values ('666','Last666','First666','Coach',66666);
insert into employee values ('678','Last678','First678','Coach',67678);
insert into employee values ('693','Last693','First693','Coach',69693);
insert into employee values ('777','Last777','First777','Database Administrator',77777);
insert into employee values ('789','Last789','First789','Database Administrator',78789);
insert into employee values ('888','Last888','First888','Database Administrator',88888);
insert into employee values ('963','Last963','First963','Manager',98963);
insert into employee values ('987','Last987','First987','Manager',99987);
insert into employee values ('999','Last999','First999','Manager',100999);

insert into technologyArea values ('DB','Database','http://www.company.intranet/technology/db','321');
insert into technologyArea values ('JA','Java','http://www.company.intranet/technology/java','333');
insert into technologyArea values ('SE','Software Engineering','http://www.company.intranet/technology/software','345');
insert into technologyArea values ('WW','Web','http://www.company.intranet/technology/web','369');

insert into trainingCourse values ('DB01','Microsoft Access',8,'DB');
insert into trainingCourse values ('DB02','Query Languages',16,'DB');
insert into trainingCourse values ('DB03','Database Modeling',8,'DB');
insert into trainingCourse values ('DB04','Transactions',8,'DB');
insert into trainingCourse values ('JA01','Introduction to Java',8,'JA');
insert into trainingCourse values ('JA02','JavaBeans',16,'JA');
insert into trainingCourse values ('JA03','Enterprise JavaBeans',32,'JA');
insert into trainingCourse values ('JA04','JDBC',8,'JA');
insert into trainingCourse values ('SE01','Patterns',16,'SE');
insert into trainingCourse values ('SE02','Validation & Verification',32,'SE');
insert into trainingCourse values ('SE03','Software Components',40,'SE');
insert into trainingCourse values ('WW01','Dynamic HTML',8,'WW');
insert into trainingCourse values ('WW02','XML',16,'WW');
insert into trainingCourse values ('WW03','XSLT',24,'WW');

insert into takes values ('321','JA01',2000,7,24);
insert into takes values ('333','JA04',2000,7,27);
insert into takes values ('333','SE01',2000,6,1);
insert into takes values ('345','SE01',2000,6,1);
insert into takes values ('345','SE02',2000,6,2);
insert into takes values ('345','WW01',2000,8,1);
insert into takes values ('369','JA04',2000,7,27);
insert into takes values ('369','WW01',2000,8,1);
insert into takes values ('369','WW02',2000,8,2);
insert into takes values ('369','WW03',2000,8,3);
insert into takes values ('444','DB01',2000,5,1);
insert into takes values ('444','DB02',2000,9,15);
insert into takes values ('456','JA01',2001,1,15);
insert into takes values ('456','WW01',2001,3,1);
insert into takes values ('555','DB03',2000,9,22);
insert into takes values ('666','JA01',2000,1,1);
insert into takes values ('678','JA01',2000,1,1);
insert into takes values ('678','JA02',2000,1,2);
insert into takes values ('693','JA01',2000,7,24);
insert into takes values ('693','JA02',2000,1,2);
insert into takes values ('693','JA03',2000,12,12);
insert into takes values ('693','JA04',2000,12,4);
insert into takes values ('777','DB01',2000,5,1);
insert into takes values ('777','DB02',2000,9,15);
insert into takes values ('777','DB03',2000,9,22);
insert into takes values ('777','DB04',2000,9,29);
insert into takes values ('789','WW01',2001,3,1);
insert into takes values ('888','DB01',2000,5,1);
insert into takes values ('888','JA01',2000,7,24);
insert into takes values ('888','SE01',2000,6,1);
insert into takes values ('888','WW01',2000,8,1);
insert into takes values ('987','JA01',2000,7,24);
insert into takes values ('999','DB01',2000,5,1);
insert into takes values ('999','SE01',2000,6,1);

% SQL: Fundamental EMPLOYEE TRAINING Queries

create view qSelection as 
    select * 
    from employee E 
    where E.eSalary > 100000; 

create view qProjection as
    select distinct E.eLast, E.eFirst, E.eTitle 
    from employee E;

create view qUnion as
    select E.eID from employee E where E.eTitle='Manager' 		
    union 										
    select E.eID from employee E where E.eTitle='Coach';

% Alternative for qUnion: using or in where condition
create view qUnionA as
    select E.eID 
    from employee E 
    where E.eTitle='Manager' or E.eTitle='Coach';

create view qDifference as
    select E.eID from employee E where E.eTitle='Manager' 		
   except
    select T.eID from takes T;

create view qProduct as
    select E.eID, C.cID from employee E, trainingCourse C;


% SQL: Additional EMPLOYEE TRAINING Queries

create view qIntersection as
    select E.eID from employee E where E.eTitle='Manager'
    intersect
    select T.eID from takes T;

create view qJoin as
    select *
    from employee E, technologyArea A
    where E.eID=A.aLeadID;

create view qNaturalJoin as
    select distinct C.cTitle, T.tYear, T.tMonth, T.tDay
    from trainingCourse C, takes T
    where C.cID=T.cID;

% SQL: Exists Queries

create view qDifferenceA as
   select    E.eID   
   from      employee E   
   where     E.eTitle='Manager' and not exists  
       (select    *   
        from      takes T   
        where     T.eID=E.eID);

create view qIntersectionA as
select    E.eID   
   from      employee E   
   where     E.eTitle='Manager' and exists  
       (select    *   
        from      takes T   
        where     T.eID=E.eID);

% Division: see query Q6 below (see also the separate ABSTRACT DIVISION enterprise)

% SQL: Safety Example

create view leads as
    select E.eID 
    from  employee E
    where exists 
	(select *
	 from technologyArea A 
	 where A.aLeadID=E.eID);

% Alternative for leads: renaming attributes
create view leadsA(eID) as
    select aLeadID
    from technologyArea;

create view qSafety as
   select    E.eID   
   from      employee E   
   where     not exists  
       (select    *   
        from      leads L   
        where     L.eID=E.eID);

% SQL: Example EMPLOYEE TRAINING Queries

% Q1: What training courses are offered in the `Database' technology area?
%     (cID, cTitle, cHours)

create view dbCourse as 
   select    C.cID, C.cTitle, C.cHours  
   from      trainingCourse C  
   where     exists 
      (select    *  
       from      technologyArea A  
       where     A.aID = C.areaID and A.aTitle = 'Database'); 

% Q2: Which employees have taken a training course offered in the 
%     'Database' technology area?
%     (eID, eLast, eFirst, eTitle)

create view dbEmployee as
   select    E.eID, E.eLast, E.eFirst, E.eTitle  
   from      employee E   
   where     exists  
      (select    *   
       from      takes T, dbCourse D   
       where     T.eID=E.eID and T.cID=D.cID);

% Q3: Which employees have not taken any training courses?
%     (eID, eLast, eFirst, eTitle)

create view q3 as
   select    E.eID, E.eLast, E.eFirst, E.eTitle  
   from      employee E   
   where     not exists  
      (select    *   
       from      takes T   
       where     T.eID=E.eID);

% Q4: Which employees took courses in more than one technology area?  
%     (eID, eLast, eFirst, eTitle)

create view q4 as
   select    E.eID, E.eLast, E.eFirst, E.eTitle  
   from      employee E   
   where     exists  
      (select    *   
       from      takes T1, takes T2, trainingCourse C1, trainingCourse C2   
       where     T1.eID=E.eID and T2.eID=E.eID and   
                 T1.cID=C1.cID and T2.cID=C2.cID and   
                 C1.areaID  <>  C2.areaID); 

% Q5: Which employees have the minimum salary?
%     (eID, eLast, eFirst, eTitle, eSalary)

create view q5 as
   select    E.eID, E.eLast, E.eFirst, E.eTitle  
   from      employee E   
   where     not exists  
      (select    *   
       from      employee S   
       where     S.eSalary  <  E.eSalary);

% Q6: Which employees took all of the training courses offered 
%     in the `Database' technology area?  
%     (eID, eLast, eFirst, eTitle)

create view q6 as
   select    E.eID, E.eLast, E.eFirst, E.eTitle  
   from      employee E   
   where    exists (select * from dbEmployee B where B.eID=E.eID) and
      not exists  
      (select    *   
       from      dbCourse D   
       where     not exists   
                 (select    *   
                  from      takes T   
                  where     T.eID=E.eID and T.cID=D.cID));

%---------------------------------  Additional Features of the Query Language -------------------------------------------------------

% Sorting

create view o1 as
   select      E.eLast, E.eTitle, E.eSalary 
   from        employee E
   order by    E.eLast;

% Aggregation

create view a1(minSalary, maxSalary, avgSalary, sumSalary, countEmps) as
   select      min(E.eSalary), max(E.eSalary), avg(E.eSalary), 
  	       sum(E.eSalary), count(*) 
   from        employee E;

create view a2(numEmpsTakenDB) as
   select      count(distinct T.eID) 
   from        dbCourse D, takes T 
   where       D.cID = T.cID; 

% Grouping

create view g1(eTitle, minSalary, maxSalary, avgSalary) as
   select      E.eTitle, min(E.eSalary), max(E.eSalary), avg(E.eSalary) 
   from        employee E 
   group by    E.eTitle;

% WinRDBI does not support SQL syntax for selective column renaming
create view g2(aID, aTitle, numEmps) as
   select      A.aID, A.aTitle, count(distinct T.eID) 
   from        technologyArea A, trainingCourse C, takes T 
   where       A.aID = C.areaID and 
               C.cID = T.cID 
   group by    A.aID, A.aTitle;

% Having

create view numEmpsTakenArea as
   select      *
   from        g2;

create view h1 as 
  select A.aID, A.aTitle, count(distinct T.eID) as numEmps
  from technologyArea A, trainingCourse C, takes T
  where A.aID = C.areaID and
              C.cID = T.cID
  group by A.aID, A.aTitle
  having numEmps >= 4
  order by numEmps desc;

  
create view h2 as
   select      * 
   from        numEmpsTakenArea 
   where       numEmps  >=  4 
   order by    numEmps desc;

create view q4Ai as
  select T.eID, count(distinct areaID) as numAreas
  from takes T, trainingCourse C
  where T.cID = C.cID
  group by T.eID
  having numAreas > 1;

create view q4Aii as
  select E.eID, E.eLast, E.eFirst, E.eTitle
  from employee E
  where exists
              (select *
               from q4Ai Q
               where Q.eID=E.eID);

% Nested subqueries

% Alternative for q1: nested subqueries equality comparison
create view q1A as 
   select    C.cID, C.cTitle, C.cHours 
   from      trainingCourse C 
   where     C.areaID = 
      (select    A.aID 
       from      technologyArea A 
       where     A.aTitle = 'Database'); 


% Alternative for q5: nested subqueries equality comparison
% Since WinRDBI does not support aggregation in nested subqueries,
% this query is broken down into two queries
create view q5Ai(minSalary) as 
   select    min(E.eSalary) 
   from      employee E; 

create view q5Aii as
   select    E.eID, E.eLast, E.eFirst, E.eTitle, E.eSalary
   from      employee E, q5Ai Q
   where     E.eSalary = Q.minSalary;

% Alternative for q2: nested subqueries set membership comparison
create view q2A as
   select    E.eID, E.eLast, E.eFirst, E.eTitle
   from      employee E 
   where     E.eID in
      (select    T.eID 
       from      takes T, dbCourse D 
       where     T.cID=D.cID); 

% Alternative for q3: nested subqueries set membership comparison
create view q3A as
   select    E.eID, E.eLast, E.eFirst, E.eTitle
   from      employee E 
   where     E.eID not in
      (select    T.eID 
       from      takes T); 

% Joined Tables

% Alternative for q1 (Q1B): join specified in where condition
create view q1B as
    select 	C.cID, C.cTitle, C.cHours 
    from   	trainingCourse C, technologyArea A 
    where  	C.areaID = A.aID and A.aTitle = 'Database'; 

create view q1C as
    select 	C.cID, C.cTitle, C.cHours 
    from   	(trainingCourse C inner join technologyArea A on areaID=aID)
    where  	A.aTitle = 'Database'; 

% Query Optimization

% q2 query definition: nested correlated subquery
% Commented out here since it appears earlier as the definition of dbEmployee
%create view q2 as
%    select    E.eID, E.eLast, E.eFirst, E.eTitle  
%    from      employee E   
%    where     exists  
%       (select    *   
%        from      takes T, dbCourse D   
%        where     T.eID=E.eID and T.cID=D.cID);

% Alternative for q2 (Q2A): nested uncorrelated query
% Commented out here since it appears earlier as an example of a nested subquery using set membership comparison
%create view q2A as
%    select 	E.eID, E.eLast, E.eFirst, E.eTitle
%    from   	employee E 
%    where  	E.eID in
%	 (select 	T.eID 
%   	 from   	takes T, dbCourse D 
%   	 where  	T.cID=D.cID); 

create view q2B as
  select distinct E.eID, E.eLast, E.eFirst, E.eTitle
  from (employee E natural inner join (takes T natural inner join dbCourse D));

% Alternative for q2 (Q2C): join condition in the where clause
create view q2C as
    select 	distinct E.eID, E.eLast, E.eFirst, E.eTitle
    from   	employee E, takes T, dbCourse D 
    where  	E.eID = T.eID and T.cID=D.cID; 

-----------------------------------------End EMPLOYEE TRAINING Enterprise--------------------------------------

select * from qSelection;
select * from qProjection;
select * from qUnion;
select * from qUnionA;
select * from qDifference;
select * from qProduct;
select * from qIntersection;
select * from qJoin;
select * from qNaturalJoin;
select * from qDifferenceA;
select * from qIntersectionA;
select * from leads;
select * from leadsA;
select * from qSafety;
select * from dbCourse;
select * from dbEmployee;
select * from q3;
select * from q4;
select * from q5;
select * from q6;
select * from o1;
select * from a1;
select * from a2;
select * from g1;
select * from g2;
select * from numEmpsTakenArea;
select * from h1;
select * from h2;
select * from q4Ai;
select * from q4Aii;
select * from q1A;
select * from q5Ai;
select * from q5Aii;
select * from q2A;
select * from q3A;
select * from q1B;
select * from q1C;
select * from q2B;
select * from q2C;

/multiline off