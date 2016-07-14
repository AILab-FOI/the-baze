/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.4.1                 */
/*                                                       */
/*    SQL Subsystem                                      */
/*                                                       */
/*                                                       */
/*                                                       */
/*                    Fernando Saenz-Perez (c) 2004-2016 */
/*                                         DISIA GPD UCM */
/*             Please send comments, questions, etc. to: */
/*                                     fernan@sip.ucm.es */
/*                                Visit the Web site at: */
/*                           http://des.sourceforge.net/ */
/*                                                       */
/* This file is part of DES.                             */
/*                                                       */
/* DES is free software: you can redistribute it and/or  */
/* modify it under the terms of the GNU Lesser General   */
/* Public License as published by the Free Software      */
/* Foundation, either version 3 of the License, or (at   */
/* your option) any later version.                       */
/*                                                       */
/* DES is distributed in the hope that it will be useful,*/
/* but WITHOUT ANY WARRANTY; without even the implied    */
/* warranty of MERCHANTABILITY or FITNESS FOR A          */
/* PARTICULAR PURPOSE. See the GNU Lesser General Public */
/* License for more details.                             */
/*                                                       */
/* You should have received a copy of the GNU Lesser     */
/* General Public License and GNU General Public License */
/* along with this program. If not, see:                 */
/*                                                       */
/*            http://www.gnu.org/licenses/               */
/*********************************************************/

/*********************************************************/
/* Features:                                             */
/* - CREATE [OR REPLACE] TABLE                           */
/* - CREATE [OR REPLACE] VIEW                            */
/* - DROP TABLE                                          */
/* - DROP VIEW                                           */
/* - INSERT INTO ... VALUES ...                          */
/* - INSERT INTO ... SQL                                 */
/* - DELETE FROM ... [WHERE ...]                         */
/* - Subqueries defining relations                       */
/* - Relation and attribute autorenaming                 */
/* - Correlated subqueries in EXISTS and IN conditions   */
/* - UNION, INTERSECT, EXCEPT|MINUS                      */ 
/* - WITH for recursive views                            */
/* - Projection list wildcards: Relation.*, *            */
/* - Subqueries in comparisons (=, <, >, ...)            */
/* - Expressions in projection list                      */ 
/* - NULL values and outer joins following SQL-2 standard*/
/* - Aggregate functions in projection list and having   */
/*   condition                                           */
/* - GROUP BY clauses                                    */
/* - FROM-less statements for computing expressions      */
/* - Multiset answers: Enable duplicates with command    */
/*********************************************************/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% SQL Grammar for Valid SQL statements in EBNF-like syntax:
% Here, terminal symbols are: parentheses, commas, semicolons, 
% single dots, asterisks, and apostrophes.
% Other terminal symbols are written in capitals.
% Alternations are grouped with brackets instead of parentheses.
% Percentage symbols (%) start line comments

% CAVEAT: Computable SQL statements follow the grammar in the manual.
%         The following grammar parses extra features which cannot
%         be computed yet (all, order by, ...)

% SQLstmt ::=
%   DDLstmt[;]
%   |
%   DMLstmt[;]
%   |
%   DQLstmt[;]
%   |
%   ISLstmt[;]

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DDL (Data Definition Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DDLstmt ::=
%   CREATE [OR REPLACE] TABLE CompleteConstrainedSchema
%   |
%   CREATE [OR REPLACE] TABLE TableName LIKE TableName
%   |
%   CREATE [OR REPLACE] VIEW Schema AS DQLstmt
%   |
%   ALTER TABLE TableName [ADD|DROP] CONSTRAINT TableConstraint
%   |
%   RENAME TABLE TableName TO TableName
%   |
%   RENAME VIEW ViewName TO ViewName
%   |
%   DROP TABLE [IF EXISTS] TableName{,TableName} % Extended syntax following MySQL 5.6 
%   |
%   DROP VIEW [IF EXISTS] ViewName
%   |
%   DROP DATABASE
%   |
%   CompleteSchema := DQLstmt                    % Addition to support HR-SQL syntax 
%   
% Schema ::=
%   RelationName
%   |
%   RelationName(Att,...,Att)
%   
% CompleteConstrainedSchema ::=
%   RelationName(Att Type [ColumnConstraint {ColumnConstraint}] {,Att Type [ColumnConstraint {ColumnConstraint}]} [, TableConstraints])
%   
% CompleteSchema ::=
%   RelationName(Att Type {,...,Att Type})

% Type ::=
%   CHAR(n)  % fixed-length string of n characters
%   |
% %   CHARACTER(n)  % equivalent to the former
% %   |
%   CHAR  % fixed-length string of 1 character
%   |
%   VARCHAR(n)  % variable-length string of up to n characters
%   |
%   VARCHAR2(n)  % Oracle's variable-length string of up to n characters
%   |
%   VARCHAR  % variable-length string of up to the maximum length of the underlying Prolog atom
%   |
%   STRING  % As VARCHAR
%   |
% %   CHARACTER VARYING(n)  % equivalent to the former
% %   |
%   INT
%   |
%   INTEGER  % equivalent to the former
%   |
% %   SMALLINT
% %   |
% %   NUMERIC(p,d) % a total of p digits, where d of those are in the decimal place
% %   |
% %   DECIMAL(p,d) % Synonymous for NUMERIC
% %   |
%   NUMBER(p,d) % Synonymous for NUMERIC. For supporting Oracle NUMBER
%   |
%   REAL
%   |
%   FLOAT % Synonymous for REAL
%   |
%   DECIMAL % Synonymous for REAL (added to support DECIMAL LogiQL Type). Not SQL standard
%   |
% %   DOUBLE PRECISION  % equivalent to the former
% %   |
% %   FLOAT(n)  % with precision of at least n digits
% %   |
% %   DATE % four digit year, month and day
% %   |
% %   TIME % hours, minutes and seconds
% %   | 
% %   TIMESTAMP % combination of date and time


% ColumnConstraint ::=
%   NOT NULL
%   |
%   PRIMARY KEY
%   |
%   UNIQUE
%   |
%   CANDIDATE KEY                   % Not in the standard 
%   |
%   REFERENCES TableName[(Att)]
%   |
%   CHECK CheckConstraint
%   
% TableConstraints ::=
%   TableConstraint{,TableConstraint}

% TableConstraint ::=
%   NOT NULL Att                    % Not in the standard 
%   |
%   UNIQUE (Att {,Att})
%   |
%   CANDIDATE KEY (Att {,Att})      % Not in the standard 
%   |
%   PRIMARY KEY (Att {,Att})
%   |
%   FOREIGN KEY (Att {,Att}) REFERENCES TableName[(Att {,Att})]
%   |
%   CHECK CheckConstraint
%   
% CheckConstraint ::=
%   WhereCondition
%   |
%   (Att {,Att}) DETERMINED BY (Att {,Att}) % Not in the standard 
%  
% RelationName is a user identifier for naming tables, views and aliases
% TableName is a user identifier for naming tables
% ViewName is a user identifier for naming views
% Att is a user identifier for naming relation attributes

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DML (Data Manipulation Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DMLstmt ::=
%   INSERT INTO TableName[(Att {,Att})] VALUES (Cte {,Cte}) {, (Cte {,Cte})}
%   |
%   INSERT INTO TableName[(Att {,Att})] DQLstmt
%   |
%   DELETE FROM TableName [[AS] Identifier] [WHERE Condition]
%   |
%   UPDATE TableName [[AS] Identifier] SET Att=Expr {,Att=Expr} [WHERE Condition]

% % Cte is a constant 

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DQL (Data Query Language) statements:
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DQLstmt ::=
%   (DQLstmt) 
%   |
%   UBSQL

% UBSQL ::= 
%   SELECTstmt
%   |
%   DQLstmt UNION [ALL] DQLstmt
%   |
%   DQLstmt EXCEPT DQLstmt
%   |
%   DQLstmt MINUS DQLstmt
%   |
%   DQLstmt INTERSECT DQLstmt
%   |
%   WITH LocalViewDefinition {,LocalViewDefinition} DQLstmt
%   |
%   ASSUME LocalAssumption {,LocalAssumption} DQLstmt

% LocalViewDefinition ::=
%   [RECURSIVE] Schema AS DQLstmt
%   |
%   [RECURSIVE] DQLstmt NOT IN Schema

% LocalAssumption ::=
%   DQLstmt [NOT] IN Schema

% SELECTstmt ::=
%   SELECT [TOP Integer] [[ALL|DISTINCT]] SelectExpressionList
%   [FROM Rels
%    [WHERE WhereCondition]
%    [GROUP BY Atts]
%    [HAVING HavingCondition]
%    [ORDER BY OrderDescription]
%    [FETCH FIRST Integer ROWS ONLY]]

% Atts ::=
%   Att {,Att}

% OrderDescription ::=
%   Att [ASC|DESC] {,Att [ASC|DESC]}

% SelectExpressionList ::= 
%   *
%   |
%   SelectExpression {,SelectExpression}

% SelectExpression ::=
%   UnrenamedSelectExpression
%   |
%   RenamedExpression

% UnrenamedSelectExpression ::=
%   Att
%   |
%   RelationName.Att
%   |
%   RelationName.*
%   |
%   ArithmeticExpression 
%   |
%   DQLstmt 

% RenamedExpression ::=
%   UnrenamedExpression [AS] Identifier

% ArithmeticExpression ::=
%   Op1 ArithmeticExpression
%   |
%   ArithmeticExpression Op2 ArithmeticExpression
%   |
%   ArithmeticFunction(ArithmeticExpression
%                      {, ArithmeticExpression})
%   |
%   Number
%   |
%   Att
%   |
%   RelationName.Att
%   |
%   ArithmeticConstant
%   |
%   DQLstmt

% Op1 ::=
%   - | \ 

% Op2 ::=
%   ^ | ** | * | / | // | rem | \/ | # | + | - | /\ | << | >> 

% ArithmeticFunction ::=
%     sqrt/1 | ln/1 | log/1 | log/2 | sin/1 | cos/1 | tan/1 | cot/1
%   | asin/1 | acos/1 | atan/1 | acot/1 | abs/1 | float/1 
%   | integer/1 | sign/1 | gcd/2 | min/2 | max/2 | truncate/1 
%   | float_integer_part/1 | float_fractional_part/1 
%   | round/1 | floor/1 | ceiling/1

% % Aggregate Functions:
% % The argument may include a prefix "distinct" for all but "min" and "max":
% %   avg/1 | count/1 | count/0 | max/1 | min/1 | sum/1 | times/1

% ArithmeticConstant ::=
%   pi | e

% Rels ::=
%   Rel {,Rel}

% Rel ::=
%   UnrenamedRel
%   |
%   RenamedRel

% UnrenamedRel ::=
%   TableName
%   |
%   ViewName
%   |
%   DQLstmt
%   |
%   JoinRel
%   |
%   DivRel 

% RenamedRel ::=
%   UnrenamedRel [AS] Identifier

% JoinRel ::=
%   Rel [NATURAL] JoinOp Rel [JoinCondition]

% JoinOp ::=
%   INNER JOIN
%   |
%   LEFT [OUTER] JOIN
%   |
%   RIGHT [OUTER] JOIN
%   |
%   FULL [OUTER] JOIN

% JoinCondition ::=
%   ON WhereCondition
%   |
%   USING (Atts)

% DivRel ::=
%   Rel DIVISION Rel          % Not in the standard

% WhereCondition ::=
%   BWhereCondition
%   |
%   UBWhereCondition

% HavingCondition 
%   % As WhereCondition, but including aggregate functions 

% BWhereCondition ::=
%   (WhereCondition)

% UBWhereCondition ::=
%   TRUE
%   |
%   FALSE
%   |
%   EXISTS DQLstmt
%   |
%   NOT (WhereCondition)
%   |
%   (AttOrCte{,AttOrCte}) [NOT] IN [DQLstmt|(Cte{,Cte})|((Cte{,Cte}){,(Cte{,Cte})})]  % Extension for lists of tuples 
%   |
%   WhereExpression IS [NOT] NULL
%   |
%   WhereExpression [NOT] IN DQLstmt
%   |
%   WhereExpression ComparisonOp [[ALL|ANY]] WhereExpression 
%   |
%   WhereCondition [AND|OR] WhereCondition
%   |
%   WhereExpression BETWEEN WhereExpression AND WhereExpression

% WhereExpression ::=
%   Att
%   |
%   Cte
%   |
%   ArithmeticExpression
%   |
%   DQLstmt

% AggrArithmeticExpression ::=
%   [AVG|MIN|MAX|SUM]([DISTINCT] Att)
%   |
%   COUNT([*|[DISTINCT] Att])

% AttOrCte ::=
%   Att 
%   |
%   Cte

% ComparisonOp ::=
%   = | <> | != | < | > | >= | <= 

% Cte ::=
%   Number
%   |
%   'String'
%   |
%   NULL

% % Number is an integer or floating-point number 

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % ISL (Information Schema Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ISLstmt ::=
%   SHOW TABLES
%   |
%   SHOW VIEWS
%   |
%   SHOW DATABASES
%   |
%   DESCRIBE [TableName|ViewName]
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Database Schema
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DB is an ODBC connection name
% my_table(DB,RelationName,Arity)
:- dynamic(my_table/3).
% my_view(DB,RelationName,Arity,QuerySyntacticTree,Language,DatalogRules,CompiledRulesId,LocalViewDefinitions,StringConstants)
:- dynamic(my_view/9).
% my_attribute(DB,Position,RelationName,AttributeName,DataType)
:- dynamic(my_attribute/5).
% my_not_nullables(DB,TableName,AttributeNames)
:- dynamic(my_not_nullables/3).
% my_primary_key(DB,TableName,AttributeNames)
:- dynamic(my_primary_key/3).
% my_candidate_key(DB,TableName,AttributeNames)
:- dynamic(my_candidate_key/3).
% my_foreign_key(DB,TableName,AttributeNames,ForeignTableName,ForeignAttributeNames,RuleIdList)
:- dynamic(my_foreign_key/6).
% my_functional_dependency(DB,TableName,AttributeNames,DependentAttributeNames)
:- dynamic(my_functional_dependency/4).
% my_integrity_constraint(DB,Predicates,Constraint,NVs,Rule,Ids,SQL,TableName)
:- dynamic(my_integrity_constraint/8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predefined 'dual' table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% my_table('$des',dual,0).
%my_attribute('$des',dual,void,void). % No need for any attribute

% For ODBC connections and solving SQL in DES:
add_dual_to_schema(Connection) :-
  create_dual_table_if_not_exist(Connection),
  my_odbc_get_dbms(Connection,DBMS),
  member(DBMS,[mysql]),
  !,
  assertz(my_table(Connection,dual,0)).
%  assertz(my_table(Connection,'DUAL',0)).
add_dual_to_schema(_Connection).
  
remove_dual_from_schema(Connection) :-
  retract(my_table(Connection,dual,_)),
%  retract(my_table(Connection,'DUAL',_)),
  !.
remove_dual_from_schema(_Connection).

% Create dual table only for those which really need it
create_dual_table_if_not_exist(Connection) :-
  \+ require_dual_table(Connection),
  !.
create_dual_table_if_not_exist(Connection) :-
  (my_odbc_exists_table(Connection,dual)
   ->
    my_odbc_dql_query_wo_schema(Connection,"SELECT COUNT(*) FROM dual",[answer(N)]),
    (N==0
     ->
      my_odbc_ddl_query(Connection,"INSERT INTO dual VALUES(0)")
     ;
      true
    )
   ;
    catch((
%            my_odbc_ddl_query(Connection,"CREATE TABLE DUAL(VOID INT)"),
%            my_odbc_ddl_query(Connection,"INSERT INTO DUAL VALUES(0)")),
           my_odbc_ddl_query(Connection,"CREATE TABLE dual(void INT)"),
           my_odbc_ddl_query(Connection,"INSERT INTO dual VALUES(0)")),
          _Message,
          true)).

% drop_dual_table_if_exist(Connection) :-
%   (my_odbc_exists_table(Connection,dual)
%    ->
%     catch((
%            my_odbc_ddl_query(Connection,"DROP TABLE dual")),
%            _Message,
%            true)
%    ; 
%     true).

require_dual_table(Connection) :-
  opened_db(Connection,_,DBMS),
  \+ (DBMS==oracle
      ;
      DBMS==mysql).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Metadata from external DBMSs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The following is automatically added to des.pl at start-up:
% % Tables
% my_table(ConnectionName,TableName,Arity) :-
%   opened_db(ConnectionName),
%   ConnectionName \== '$des',
%   my_odbc_get_table_arity(ConnectionName,TableName,Arity).
% % Attributes
% my_attribute(ConnectionName,Position,RelationName,AttributeName,DESDataType) :-
%   opened_db(ConnectionName),
%   ConnectionName \== '$des',
%   my_odbc_get_colnames(ConnectionName,RelationName,ColNames),
%   my_nth1_member(AttributeName,Position,ColNames),
%   my_odbc_get_type(ConnectionName,RelationName,AttributeName,DESDataType).
% % Views
% % my_view(DB,RelationName,Arity,QuerySyntacticTree,Language,DatalogRules,CompiledRulesId,LocalViewDefinitions,StringConstants)
% my_view(ConnectionName,ViewName,Arity,SQLst,sql,[],[],[],[]) :-
%   ConnectionName \== '$des',
%   my_table(ConnectionName,ViewName,Arity),
%   get_sql_view_text_from_connection(ConnectionName,ViewName,SQLstr),
%   parse_sql_query(SQLst,SQLstr,"").

% my_table(TableName,Arity) :-
%   atom(TableName),
%   current_db(ConnectionName),
%   my_table(ConnectionName,TableName,Arity).
my_table(TableName,Arity) :-
  atom(TableName),
  current_db(ConnectionName),
  (my_table(ConnectionName,TableName,Arity)
   ;
   (ConnectionName \== '$des', des_sql_solving(on)),
   my_table('$des',TableName,Arity)
  ).
  
% my_attribute(Position,RelationName,AttributeName,DESDataType) :-
%   current_db(ConnectionName),
%   my_attribute(ConnectionName,Position,RelationName,AttributeName,DESDataType).
my_attribute(Position,RelationName,AttributeName,DESDataType) :-
  current_db(ConnectionName),
  (my_attribute(ConnectionName,Position,RelationName,AttributeName,DESDataType)
  ;
   (ConnectionName \== '$des', des_sql_solving(on)),
   my_attribute('$des',Position,RelationName,AttributeName,DESDataType)
  ).
  
 
get_sql_view_text_from_connection(Connection,ViewName,SQLstr) :-
  opened_db(Connection,_,mysql),
  !,
  atom_codes(ViewName,ViewNameStr),
  concat_lists(["select table_schema,view_definition from information_schema.views where table_name='",ViewNameStr,"'"],QueryStr),
  my_odbc_dql_query_wo_schema(Connection,QueryStr,[answer(DB,DBSQL)]),
  delimited_sql_identifier(Connection,DB,StrDelDB),
  append(StrDelDB,".",StrDelDBDot),
  atom_codes(DBSQL,DBSQLstr),
  replace_all_string(DBSQLstr,StrDelDBDot,"",SQLstr).
get_sql_view_text_from_connection(Connection,ViewName,SQLstr) :-
  opened_db(Connection,_,oracle),
  !,
  atom_codes(ViewName,ViewNameStr),
  concat_lists(["select text from user_views where view_name='",ViewNameStr,"'"],QueryStr),
  my_odbc_dql_query_wo_schema(Connection,QueryStr,[answer(SQL)]),
  atom_codes(SQL,SQLstr).
get_sql_view_text_from_connection(Connection,ViewName,SQLstr) :-
  opened_db(Connection,_,db2),
  !,
  atom_codes(ViewName,ViewNameStr),
%  to_uppercase_char_list(ViewNameStr,UViewNameStr),
  concat_lists(["SELECT CAST(TEXT AS VARCHAR(2048)) FROM SYSCAT.VIEWS WHERE VIEWNAME = UPPER('",ViewNameStr,"') AND VIEWSCHEMA = (SELECT (VALUES CURRENT SCHEMA) FROM DUAL)"],QueryStr),
  my_odbc_dql_query_wo_schema(Connection,QueryStr,[answer(SQL)]),
  atom_codes(SQL,VSQLStr),
  db2_extract_query_from_view(VSQLStr,LSQLstr),
  to_uppercase_char_list(LSQLstr,SQLstr).
get_sql_view_text_from_connection(Connection,ViewName,SQLstr) :-
  opened_db(Connection,_,postgresql),
  !,
  atom_codes(ViewName,ViewNameStr),
  concat_lists(["select definition from pg_views where viewname='",ViewNameStr,"'"],QueryStr),
  my_odbc_dql_query_wo_schema(Connection,QueryStr,[answer(SQL)]),
  atom_codes(SQL,SQLstr).
 
db2_extract_query_from_view -->
  my_create_or_replace(_),
  my_sql_blanks,
  my_kw("VIEW"),
  my_sql_blanks,
  my_create_view_schema(_),
  my_sql_blanks,
  my_kw("AS"),
  my_sql_blanks,
  !.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RDB external data sources via ODBC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Enabling external data sources amounts to add a datalog clause with body
% This body is responsible of providing tuples from the external DBMS

% datalog(Rule,NVs,RuleId,Lines,FileId,source)
enable_rdb_datasource(Connection) :-
  opened_db(Connection),
  !,
  RDBDS = ':-'(    datalog(Rule,[],RuleId,[],[],rdb(Connection),source),
               datalog_rdb(Rule,[],RuleId,[],rdb(Connection),source)),
  (Connection\=='$des' ->
    (retract(RDBDS) -> true ; true),
    assertz(RDBDS)
   ;
    true).
enable_rdb_datasource(_).

% Enable all the opened connections
enable_rdb_datasources :-
  opened_db(Connection),
  enable_rdb_datasource(Connection),
  fail.
enable_rdb_datasources.

% Disabling external data sources simply amounts to remove the datalog clause
% providing external tuples
disable_rdb_datasource(Connection) :-
  RDBDS = ':-'(    datalog(Rule,[],RuleId,[],[],rdb(Connection),source),
               datalog_rdb(Rule,[],RuleId,[],rdb(Connection),source)),
  retract(RDBDS).

% Disable all the opened connections
disable_rdb_datasources :-
  opened_db(Connection),
  disable_rdb_datasource(Connection),
  fail.
disable_rdb_datasources.

% Data source for external RDBMSs
% Data source for a single non-persisted relation (table/view)
datalog_rdb(R,NVs,RId,Ls,FId,Source) :-
  datalog_rdb_single_np(R,NVs,RId,Ls,FId,Source).
% Data source for all the non-persisted relations (tables/views) in the RDB
datalog_rdb(R,NVs,RId,Ls,FId,Source) :-
  datalog_rdb_all_np(R,NVs,RId,Ls,FId,Source).
% Data source for a single persisted predicate in the RDB
datalog_rdb(R,NVs,RId,Ls,FId,Source) :-
  datalog_rdb_single_p(R,NVs,RId,Ls,FId,Source).
% Data source for all persisted predicates in the RDB
datalog_rdb(R,NVs,RId,Ls,FId,Source) :-
  datalog_rdb_all_p(R,NVs,RId,Ls,FId,Source).


% Data source for a single non-persisted relation (table/view)
datalog_rdb_single_np(R,[],RuleId,[],rdb(Connection),source) :-
  nonvar(R),
  RuleId=rdb_id(_,_),
%  opened_db(Connection),
  current_db(Connection),
  Connection\=='$des',
  R \= ':-'(_H,_T),
  R=..[TableName|Columns],
  functor(R,TableName,Arity),
%  functor(PR,TableName,Arity),
%  \+ my_persistent(Connection,PR),  
  length(Columns,Arity),
  my_odbc_get_table_arity(Connection,TableName,Arity),
  functor(TypedSchema,TableName,Arity),
  \+ my_persistent(Connection,TypedSchema),  
  prepare_rdb_ruleid(TableName),
  build_sql_rdb_datasource(Connection,TableName,Columns,SQLstr),
  display_string_list_sql_on([SQLstr]),
  my_odbc_dql_query_fetch_row(Connection,SQLstr,Row),
  Row=..[_AnswerRel|Columns],
  get_rdb_ruleid(TableName,RuleId).
  
% Data source for all the non-persisted relations (tables/views) in the RDB
datalog_rdb_all_np(R,[],RuleId,[],rdb(Connection),source) :-
  var(R),
  RuleId=rdb_id(_,_),
  current_db(Connection),
  Connection\=='$des',
  my_odbc_get_table_and_view_names(Connection,TableNames),
  member(TableName,TableNames),
  my_odbc_get_table_arity(Connection,TableName,Arity),
  functor(TypedSchema,TableName,Arity),
  \+ my_persistent(Connection,TypedSchema),  
  length(Columns,Arity),
  R=..[TableName|Columns],
  length(Columns,Arity),
  datalog_rdb(R,[],RuleId,[],rdb(Connection),source).
  
% Data source for a single persisted predicate in the RDB
datalog_rdb_single_p(R,[],RuleId,[],rdb(Connection),source) :-
  nonvar(R),
  RuleId=rdb_id(_,_),
  my_persistent(Connection,TypedSchema),
  functor(TypedSchema,TableName,Arity),
  R \= ':-'(_H,_T),
  R =.. [TableName|Columns],
  length(Columns,Arity),
%  prepare_rdb_ruleid(TableName)
  (var(RuleId) -> prepare_rdb_ruleid(TableName) ; true),
  schema_to_colnames(TypedSchema,ColNames),
  build_sql_rdb_datasource(Connection,TableName,ColNames,Columns,SQLstr),
  display_string_list_sql_on([SQLstr]),
  my_odbc_dql_query_fetch_row(Connection,SQLstr,Row),
  Row=..[_AnswerRel|Columns],
  (var(RuleId) -> get_rdb_ruleid(TableName,RuleId) ; true).
%  get_rdb_ruleid(TableName,RuleId).
  
% Data source for all persisted predicates in the RDB
datalog_rdb_all_p(R,[],RuleId,[],rdb(Connection),source) :-
  var(R),
  RuleId=rdb_id(_,_),
  my_persistent(Connection,TypedSchema),
  functor(TypedSchema,TableName,Arity),
  length(Columns,Arity),
  R=..[TableName|Columns],
  datalog_rdb(R,[],RuleId,[],rdb(Connection),source).


prepare_rdb_ruleid(TableName) :-
  my_retract_all_facts(rdb_id(TableName,_)),
  assertz(rdb_id(TableName,0)).
  
get_rdb_ruleid(TableName,RuleId) :-
  retract(rdb_id(TableName,OID)),
  ID is OID+1,
  RuleId=rdb_id(TableName,ID),
  assertz(RuleId).
  
build_sql_rdb_datasource(Connection,TableName,Columns,SQLstr) :-
  my_odbc_get_colnames(Connection,TableName,Colnames),
  build_sql_rdb_datasource(Connection,TableName,Colnames,Columns,SQLstr).
  
build_sql_rdb_datasource(Connection,TableName,Colnames,Columns,SQLstr) :-
  build_where_cond(Connection,Columns,Colnames,CondStr),
  delimited_sql_identifier(Connection,TableName,StrDelimitedTableName),
  concat_lists(["SELECT * FROM ",StrDelimitedTableName],SQLstr1),
  (CondStr==""
   ->
    SQLstr=SQLstr1
   ;
    concat_lists([SQLstr1," WHERE ",CondStr],SQLstr)
  ).

build_where_cond(Connection,Columns,Colnames,CondStr) :-  
  where_conditions(Connection,Columns,Colnames,CondStrList),
  concat_strs_with(CondStrList," AND ",CondStr).
  
where_conditions(_Connection,[],[],[]).  
where_conditions(Connection,[Col|Cols],[Name|Names],[CondStr|CondStrList]) :-
  nonvar(Col),
  !,
  (Col = '$NULL'(_)
   ->
    StrComp = " IS ", 
    ValStr="NULL"
   ;
    StrComp = "=", 
    quote_value(Col,ValStr)
  ),
  delimited_sql_identifier(Connection,Name,NameStr),
  concat_lists([NameStr,StrComp,ValStr],CondStr),
  where_conditions(Connection,Cols,Names,CondStrList).
% Aliases:
where_conditions(Connection,[Col|Cols],[Name|Names],[CondStr|CondStrList]) :-
  find_var_colname(Col,Cols,Names,AName),
  !,
  delimited_sql_identifier(Connection,Name,NameStr),
  delimited_sql_identifier(Connection,AName,ANameStr),
  concat_lists([NameStr,"=",ANameStr],CondStr),  
  where_conditions(Connection,Cols,Names,CondStrList).
where_conditions(Connection,[_Col|Cols],[_Name|Names],CondStrList) :-
  where_conditions(Connection,Cols,Names,CondStrList).
  
quote_value('$NULL'(_),"NULL") :-
  !.
quote_value(NCol,ValStr) :-
  atom(NCol),
  !,
  atom_codes(NCol,ColStr),
  concat_lists(["'",ColStr,"'"],ValStr).
quote_value(NCol,ValStr) :-
  ensure_atom(NCol,Atom),
  atom_codes(Atom,ValStr).

find_var_colname(Col,[Col1|_Cols],[Name|_Names],Name) :-
  Col==Col1,
  !.
find_var_colname(Col,[_Col1|Cols],[_Name|Names],Name) :-
  find_var_colname(Col,Cols,Names,Name).
  
  
% DBMS Identifier
% Either '$des' or the ODBC DBMS 
% get_dbms('$des') :-
%   current_db('$des'),
%   !.
% get_dbms(DBMS) :-
%   my_odbc_get_dbms(DBMS).
  
% Retrieves only one result at most, closing dangling queries
once_datalog(R,NVs,RId,CId,Ls,FId,Rs) :-
  once(datalog(R,NVs,RId,CId,Ls,FId,Rs)),
  my_odbc_dangling_query_close.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parse_sql_query(SQLsc). Parses an SQL string and gets its 
%   syntactic tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% parse_sql_query(SQLsc,SQLStr) :-
%   parse_sql_query(SQLsc,SQLStr,""),
%   !,
%   \+ semantic_error.

parse_sql_query(SQLsc) -->
  my_sql_blanks_star, % The ODBC driver may return blanks
  {%reset_syntax_error,
   my_retract_all_facts(dictionary(_)),
   assertz(dictionary([]))}, % WARNING: ONLY FOR TEST CASE GENERATION
%  my_sql_blanks_star, 
  push_syntax_error(['Expected valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME)'],Old),
  my_SQL(SQLsc),
  pop_syntax_error(Old),
  my_sql_blanks_star, 
  my_optional(";"), 
  my_sql_blanks_star, % The ODBC driver may return blanks
  !.  

parse_dql_query(SQLsc) -->
  {%reset_syntax_error,
   my_retract_all_facts(dictionary(_)),
   assertz(dictionary([]))}, % WARNING: ONLY FOR TEST CASE GENERATION
  my_sql_blanks_star, 
  my_DQL(SQLsc),
  my_sql_blanks_star, 
  my_optional(";"), 
%  my_sql_blanks_star,
  !.  

% DQL Statement
my_SQL(SQLsc) -->
  my_DQL(SQLsc).
% DML Statement
my_SQL(SQLsc) -->
  my_DML(SQLsc).
% DDL Statement
my_SQL(SQLsc) -->
  my_DDL(SQLsc).
% ISL Statement
my_SQL(SQLsc) -->
  my_ISL(SQLsc).

% DDL Statements
% CREATE TABLE
my_DDL(CRTSchema) -->
  my_create_or_replace(CR),
  my_sql_blanks,
  push_syntax_error(['Expected TABLE or VIEW'],Old1),
  my_kw("TABLE"),
  my_sql_blanks,
  pop_syntax_error(Old1),
  push_syntax_error(['Expected typed schema'],Old2),
  my_complete_constrained_typed_schema(Schema,Ctrs),
  pop_syntax_error(Old2),
  {atom_concat(CR,'_table',CRT),
   CRTSchema=..[CRT,Schema,Ctrs]},
  push_syntax_error(['Expected end of statement']).
   
% CREATE TABLE LIKE
my_DDL(CRTSchema) -->
  my_create_or_replace(CR),
  push_syntax_error(['Expected TABLE or VIEW'],Old1),
  my_sql_blanks,
  my_kw("TABLE"),
  pop_syntax_error(Old1),
  my_sql_blanks_star,
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  push_syntax_error(['Expected table identifier'],Old2),
  my_sql_user_identifier(TableName),
  my_sql_blanks,
  pop_syntax_error(Old2),
  my_kw("LIKE"),
  push_syntax_error(['Expected table identifier'],Old3),
  my_sql_blanks,
  my_sql_user_identifier(ExistingTableName),
  my_sql_blanks_star,
  pop_syntax_error(Old3),
  my_closing_parentheses_star(N),
  {atom_concat(CR,'_table_like',CRT),
   CRTSchema=..[CRT,TableName,ExistingTableName]},
  push_syntax_error(['Expected end of statement']).
   
% ALTER TABLE
my_DDL(alter_table(TableName,AD,Constraint)) -->
  my_kw("ALTER"),
  push_syntax_error(['Expected TABLE'],Old1),
  my_sql_blanks,
  my_kw("TABLE"),
  pop_syntax_error(Old1),
  my_sql_blanks,
  push_syntax_error(['Expected table identifier'],Old2),
  my_sql_user_identifier(TableName),
  my_sql_blanks,
  pop_syntax_error(Old2),
  push_syntax_error(['Expected ADD or DROP'],Old3),
  my_add_or_drop(AD),
  my_sql_blanks,
  pop_syntax_error(Old3),
  push_syntax_error(['Expected CONSTRAINT'],Old4),
  my_kw("CONSTRAINT"),
  my_sql_blanks,
  pop_syntax_error(Old4),
  push_syntax_error(['Expected valid table constraint'],Old5),
  my_table_constraint(Constraint),
  pop_syntax_error(Old5),
  push_syntax_error(['Expected end of statement']).
   
% RENAME TABLE
my_DDL(rename_table(TableName,NewTableName)) -->
  my_kw("RENAME"),
  push_syntax_error(['Expected TABLE or VIEW'],Old1),
  my_sql_blanks,
  my_kw("TABLE"),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected table identifier'],Old2),
  my_sql_blanks,
  my_sql_user_identifier(TableName),
  my_sql_blanks,
  pop_syntax_error(Old2),
  push_syntax_error(['Expected TO'],Old3),
  my_kw("TO"),
  pop_syntax_error(Old3),
  my_sql_blanks,
  push_syntax_error(['Expected table identifier'],Old4),
  my_sql_user_identifier(NewTableName),
  pop_syntax_error(Old4),
  push_syntax_error(['Expected end of statement']).

% RENAME VIEW
my_DDL(rename_view(Viewname,NewViewname)) -->
  my_kw("RENAME"),
  push_syntax_error(['Expected TABLE or VIEW'],Old1),
  my_sql_blanks,
  my_kw("VIEW"),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected view identifier'],Old2),
  my_sql_blanks,
  my_sql_user_identifier(Viewname),
  my_sql_blanks,
  pop_syntax_error(Old2),
  push_syntax_error(['Expected TO'],Old3),
  my_kw("TO"),
  pop_syntax_error(Old3),
  my_sql_blanks,
  push_syntax_error(['Expected table identifier'],Old4),
  my_sql_user_identifier(NewViewname),
  pop_syntax_error(Old4),
  push_syntax_error(['Expected end of statement']).

% CREATE VIEW
my_DDL(CRVSchema) -->
  my_create_or_replace(CR),
  push_syntax_error(['Expected TABLE or VIEW'],Old1),
  my_sql_blanks,
  my_kw("VIEW"),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected view schema'],Old2),
  my_sql_blanks,
  my_create_view_schema(Schema),
  pop_syntax_error(Old2),
  push_syntax_error(['Expected AS'],Old3),
  my_sql_blanks,
  my_kw("AS"),
  pop_syntax_error(Old3),
  push_syntax_error(['Expected valid SQL DQL statement (SELECT, WITH or ASSUME)'],Old4),
  my_sql_blanks,
  my_DQL((LSQLst,Schema)),
  pop_syntax_error(Old4),
  {atom_concat(CR,'_view',CRVF),
   CRVSchema =.. [CRVF,sql,(LSQLst,_AS),Schema]},
  push_syntax_error(['Expected end of statement']).
% my_DDL(CRVSchema) -->
%   my_create_or_replace(CR),
%   my_sql_blanks,
%   my_kw("VIEW"),
%   my_sql_blanks,
%   my_sql_user_identifier(_Name),
%   my_sql_blanks,
%   my_kw("AS"),
%   my_sql_blanks,
%   my_DQL((LSQLst,Schema)),
% %  {Schema=..[Name|_]}, % The schema should be built, but this information in not yet known
%   {atom_concat(CR,'_view',CRVF),
%    CRVSchema =.. [CRVF,(LSQLst,_AS),Schema]}.
% HR-SQL CREATE VIEW syntax
my_DDL(CRVSchema) -->
%  push_syntax_error(['Expected typed schema'],Old1),
  my_hrsql_typed_schema(Schema), % No constraints
%  pop_syntax_error(Old1),
  my_sql_blanks_star,
  ":",
  my_sql_blanks_star,
  "=",
  push_syntax_error(['Expected select statement'],Old2),
  my_sql_blanks_star,
  my_DQL((SQLst,Schema)),
  pop_syntax_error(Old2),
  {CRVSchema =.. [create_or_replace_view,hrsql,(SQLst,_AS),Schema]},
  push_syntax_error(['Expected end of statement']).

% DROP TABLE
my_DDL(drop_table(Name,IfExists)) -->
  my_kw("DROP"),
  my_sql_blanks,
  push_syntax_error(['Expected TABLE, VIEW or DATABASE'],Old1),
  my_kw("TABLE"),
  my_sql_blanks,
  pop_syntax_error(Old1),
  my_optional_if_exists(IfExists),
  my_sql_blanks_star,
  push_syntax_error(['Expected table name'],Old2),
  my_sql_user_identifier(Name),
  pop_syntax_error(Old2),
%  my_sql_user_identifier_tuple(Names).
  push_syntax_error(['Expected end of statement']).

% DROP VIEW
my_DDL(drop_view(Name,IfExists)) -->
  my_kw("DROP"),
  my_sql_blanks,
  push_syntax_error(['Expected TABLE, VIEW or DATABASE'],Old1),
  my_kw("VIEW"),
  my_sql_blanks,
  pop_syntax_error(Old1),
  my_optional_if_exists(IfExists),
  my_sql_blanks_star,
  push_syntax_error(['Expected view name'],Old2),
  my_sql_user_identifier(Name),
  pop_syntax_error(Old2),
  push_syntax_error(['Expected end of statement']).

% DROP SCHEMA
my_DDL(drop_database) -->
  my_kw("DROP"),
  my_sql_blanks,
  push_syntax_error(['Expected TABLE, VIEW or DATABASE'],Old),
  my_kw("DATABASE"),
  pop_syntax_error(Old),
  push_syntax_error(['Expected end of statement']).

my_add_or_drop(add) -->
  my_kw("ADD").
my_add_or_drop(drop) -->
  my_kw("DROP").
  
my_optional_if_exists(false) -->
  [].
my_optional_if_exists(true) -->
  my_kw("IF"),
  my_sql_blanks,
  push_syntax_error(['Expected EXISTS'],Old),
  my_kw("EXISTS"),
  my_sql_blanks,
  pop_syntax_error(Old).
  
my_create_or_replace(create_or_replace) -->
  my_kw("CREATE"),
  my_sql_blanks,
  push_syntax_error(['Expected OR REPLACE'],Old1),
  my_kw("OR"),
  my_sql_blanks,
  pop_syntax_error(Old1),
  push_syntax_error(['Expected REPLACE'],Old2),
  my_kw("REPLACE"),
  pop_syntax_error(Old2).
my_create_or_replace(create) -->
  my_kw("CREATE").

my_create_view_schema(Schema) -->
  my_complete_untyped_schema(Schema),
  {!}.
my_create_view_schema(Name) -->
  my_sql_user_identifier(Name).

  
my_complete_untyped_schema(Schema) -->
  my_sql_user_identifier(Name),
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  my_untyped_columns(Cs),
  my_sql_blanks_star,
  ")",
  {Schema =.. [Name|Cs]}.

my_column_name_list([C]) --> 
  my_untyped_column(C).
my_column_name_list([C|Cs]) -->
  my_untyped_column(C),
  my_sql_blanks_star, 
  ",", 
  my_sql_blanks_star, 
  my_column_name_list(Cs).
 
my_untyped_columns([C:_T]) --> 
  my_untyped_column(C).
my_untyped_columns([C:_T|CTs]) -->
  my_untyped_column(C),
  my_sql_blanks_star, 
  ",", 
  my_sql_blanks_star, 
  my_untyped_columns(CTs).
 
my_untyped_column(C) --> 
  my_sql_user_identifier(C).

my_sql_user_identifier_tuple([C]) --> 
  my_sql_user_identifier(C).
my_sql_user_identifier_tuple([C|Cs]) -->
  my_sql_user_identifier(C),
  my_sql_blanks_star, 
  ",", 
  my_sql_blanks_star, 
  my_sql_user_identifier_tuple(Cs).

my_complete_constrained_typed_schema(Schema,Ctrs) -->
  my_sql_user_identifier(Name),
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  my_constrained_typed_columns(Cs,CCtrs),
  my_sql_blanks_star,
  my_optional_table_constraints(TCtrs),
  my_sql_blanks_star,
  ")",
  {Schema =.. [Name|Cs],
   append(CCtrs,TCtrs,Ctrs)}.

my_constrained_typed_columns([C:T],Ctrs) --> 
  my_constrained_typed_column(C:T,Ctrs).
my_constrained_typed_columns([C:T|CTs],Ctrs) -->
  my_constrained_typed_column(C:T,CCtrs),
  my_sql_blanks_star, 
  ",", 
  my_sql_blanks_star, 
  my_constrained_typed_columns(CTs,RCtrs),
  {append(CCtrs,RCtrs,Ctrs)}.

my_constrained_typed_column(C:T,Ctrs) --> 
  my_sql_user_identifier(C),
  push_syntax_error(['Invalid type'],Old1),
  my_sql_blanks,
  my_sql_type(T),
  my_sql_blanks,
  pop_syntax_error(Old1),
  push_syntax_error(['Invalid column constraints'],Old2),
  my_column_constraints(C,Ctrs),
  pop_syntax_error(Old2).
my_constrained_typed_column(C:T,[true]) --> 
  my_sql_user_identifier(C),
  my_sql_blanks,
  push_syntax_error(['Invalid type'],Old),
  my_sql_type(T),
  pop_syntax_error(Old).

my_column_constraints(C,[Ctr]) -->
  my_column_constraint(C,Ctr).
my_column_constraints(C,[Ctr|Ctrs]) -->
  my_column_constraint(C,Ctr),
  my_sql_blanks,
  my_column_constraints(C,Ctrs).

my_column_constraint(C,not_nullables([C])) -->
  my_kw("NOT"),
  push_syntax_error(['Expected NULL'],Old),
  my_sql_blanks,
  my_kw("NULL"),
  pop_syntax_error(Old).
my_column_constraint(C,primary_key([C])) -->
  my_kw("PRIMARY"),
  push_syntax_error(['Expected KEY'],Old),
  my_sql_blanks,
  my_kw("KEY"),
  pop_syntax_error(Old).
my_column_constraint(C,candidate_key([C])) -->
  my_kw("UNIQUE").
my_column_constraint(C,foreign_key([C],TableName,[Att])) -->
  my_kw("REFERENCES"),
  push_syntax_error(['Expected table name'],Old1),
  my_sql_blanks,
  my_sql_user_identifier(TableName),
  pop_syntax_error(Old1),
  my_sql_blanks_star, 
  "(",
  push_syntax_error(['Expected a column name between parentheses'],Old2),
  my_sql_blanks_star,
  my_untyped_column(Att),
  my_sql_blanks_star,
  ")",
  pop_syntax_error(Old2).
my_column_constraint(C,foreign_key([C],TableName,[C])) -->
  my_kw("REFERENCES"),
  push_syntax_error(['Expected table name'],Old),
  my_sql_blanks,
  my_sql_user_identifier(TableName),
  pop_syntax_error(Old).
my_column_constraint(C,default(C,Value)) -->
  my_kw("DEFAULT"),
  push_syntax_error(['Expected constant'],Old),
  my_sql_blanks,
  my_sql_constant(Value),
  pop_syntax_error(Old).
my_column_constraint(_C,CheckCtr) -->
  my_kw("CHECK"),
  push_syntax_error(['Invalid check constraint'],Old),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  my_check_constraint(CheckCtr),
  my_closing_parentheses_star(N),
  pop_syntax_error(Old).
my_column_constraint(C,candidate_key([C])) -->
  my_kw("CANDIDATE"),
  push_syntax_error(['Expected KEY'],Old),
  my_sql_blanks,
  my_kw("KEY"),
  pop_syntax_error(Old).
my_column_constraint(C,fd([Att],[C])) -->
  my_kw("DETERMINED"),
  push_syntax_error(['Expected BY'],Old1),
  my_sql_blanks,
  my_kw("BY"),
  pop_syntax_error(Old1),
  my_sql_blanks,
  push_syntax_error(['Expected column name'],Old2),
  my_untyped_column(Att),
  pop_syntax_error(Old2).

my_optional_table_constraints(Ctrs) -->
  ",",
  my_sql_blanks_star,
  my_table_constraints(Ctrs).
my_optional_table_constraints([]) -->
  [].
  
my_table_constraints([Ctr]) -->
  my_table_constraint(Ctr).  
my_table_constraints([Ctr|Ctrs]) -->
  my_table_constraint(Ctr),
  my_sql_blanks_star,
  ",",
  my_sql_blanks_star,
  my_table_constraints(Ctrs).  

% Not in the standard: ALTER TABLE TableName ADD CONSTRAINT NOT NULL ColumnName
% ADDED TO STD: NOT NULL table constraint
my_table_constraint(not_nullables(Cs)) -->
  my_kw("NOT"),
  push_syntax_error(['Expected NULL'],Old1),
  my_sql_blanks,
  my_kw("NULL"),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected sequence of column names between parentheses'],Old2),
  my_column_tuple_lblank(Cs),
  pop_syntax_error(Old2).
my_table_constraint(primary_key(Cs)) -->
  my_kw("PRIMARY"),
  push_syntax_error(['Expected KEY'],Old1),
  my_sql_blanks,
  my_kw("KEY"),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected sequence of column names between parentheses'],Old2),
  my_column_tuple_lblank(Cs),
  pop_syntax_error(Old2).
my_table_constraint(candidate_key(Cs)) -->
  my_kw("UNIQUE"),
  push_syntax_error(['Expected sequence of column names between parentheses'],Old),
  my_column_tuple_lblank(Cs),
  pop_syntax_error(Old).
my_table_constraint(foreign_key(Cs,FTableName,FCs)) -->
  my_kw("FOREIGN"),
  push_syntax_error(['Expected KEY'],Old1),
  my_sql_blanks,
  my_kw("KEY"),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected a sequence of column names between parentheses'],Old2),
  my_column_tuple_lrblank(Cs),
  pop_syntax_error(Old2),
  push_syntax_error(['Expected REFERENCES'],Old3),
  my_kw("REFERENCES"),
  pop_syntax_error(Old3),
  push_syntax_error(['Expected table name'],Old4),
  my_sql_blanks,
  my_sql_user_identifier(FTableName),
  pop_syntax_error(Old4),
  my_sql_blanks_star,
  "(",
  push_syntax_error(['Expected a sequence of column names between parentheses'],Old5),
  my_sql_blanks_star,
  my_column_name_list(FCs),
  my_sql_blanks_star,
  ")",
  pop_syntax_error(Old5).
my_table_constraint(foreign_key(Cs,FTableName,Cs)) -->
  my_kw("FOREIGN"),
  push_syntax_error(['Expected KEY'],Old1),
  my_sql_blanks,
  my_kw("KEY"),
  pop_syntax_error(Old1),
  my_column_tuple_lrblank(Cs),
  push_syntax_error(['Expected REFERENCES'],Old2),
  my_kw("REFERENCES"),
  pop_syntax_error(Old2),
  push_syntax_error(['Expected table name'],Old3),
  my_sql_blanks,
  my_sql_user_identifier(FTableName),
  pop_syntax_error(Old3).
my_table_constraint(CheckCtr) -->
  my_kw("CHECK"),
  push_syntax_error(['Invalid check constraint'],Old),
  my_sql_blanks_star,
  my_opening_parentheses_star(N),
  my_sql_blanks_star,
  my_check_constraint(CheckCtr),
  my_sql_blanks_star,
  my_closing_parentheses_star(N),
  pop_syntax_error(Old).
my_table_constraint(candidate_key(Cs)) -->
  my_kw("CANDIDATE"),
  push_syntax_error(['Expected KEY'],Old1),
  my_sql_blanks,
  my_kw("KEY"),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected a sequence of column names between parentheses'],Old2),
  my_column_tuple_lblank(Cs),
  pop_syntax_error(Old2).
  
my_check_constraint(fd(Ls,Rs)) -->
  my_column_tuple_rblank(Rs),
  my_kw("DETERMINED"),
  push_syntax_error(['Expected BY'],Old1),
  my_sql_blanks,
  my_kw("BY"),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected a sequence of column names between parentheses'],Old2),
  my_column_tuple_lblank(Ls),
  pop_syntax_error(Old2).
my_check_constraint(my_sql_check_constraint(Condition)) -->
  my_where_condition(Condition).
   
my_hrsql_typed_schema(Schema) -->
  my_sql_user_identifier(Name),
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  my_hrsql_typed_columns(Cs),
  my_sql_blanks_star,
  ")",
  {Schema =.. [Name|Cs]}.

my_hrsql_typed_columns([C:T]) --> 
  my_hrsql_typed_column(C:T).
my_hrsql_typed_columns([C:T|CTs]) -->
  my_hrsql_typed_column(C:T),
  my_sql_blanks_star, 
  ",", 
  my_sql_blanks_star, 
  my_hrsql_typed_columns(CTs).
  
my_hrsql_typed_column(C:T) --> 
  my_sql_user_identifier(C),
  my_sql_blanks,
  my_sql_type(T).


% Types
% char(n)
my_sql_type(string(char(N))) -->
  my_kw("CHAR"), 
  my_sql_blanks_star, 
  "(",
  push_syntax_error(['Expected a positive integer'],Old),
  my_sql_blanks_star, 
  my_positive_integer(N),
  pop_syntax_error(Old),
  my_sql_blanks_star, 
  ")".
% char  
my_sql_type(string(char(1))) -->
  my_kw("CHAR").
% varchar(n)
my_sql_type(string(varchar(N))) -->
  my_kw("VARCHAR"), 
  my_sql_blanks_star, 
  "(",
  push_syntax_error(['Expected a positive integer'],Old),
  my_sql_blanks_star, 
  my_positive_integer(N),
  pop_syntax_error(Old),
  my_sql_blanks_star, 
  ")".
my_sql_type(string(varchar(N))) -->
  my_kw("VARCHAR2"), 
  my_sql_blanks_star, 
  "(",
  my_sql_blanks_star, 
  push_syntax_error(['Expected a positive integer'],Old),
  my_positive_integer(N),
  pop_syntax_error(Old),
  my_sql_blanks_star, 
  ")".
my_sql_type(string(varchar)) -->
  my_kw("VARCHAR"). 
my_sql_type(string(varchar)) -->
  my_kw("TEXT"). 
my_sql_type(string(varchar)) -->
  my_kw("STRING"). 
% integer
my_sql_type(number(integer)) -->
  my_kw("INTEGER"),
  my_optional_integer_range(_).
my_sql_type(number(integer)) -->
  my_kw("NUMBER"),
  my_optional_integer_range(_).
% int
my_sql_type(number(integer)) -->
  my_kw("INT"),
  my_optional_integer_range(_).
% real
my_sql_type(number(float)) -->
  my_kw("REAL").
my_sql_type(number(float)) -->
  my_kw("FLOAT").
my_sql_type(number(float)) -->
  my_kw("NUMBER"),
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  push_syntax_error(['Expected a positive integer'],Old1),
  my_positive_integer(_Int),
  pop_syntax_error(Old1),
  my_sql_blanks_star,
  ",",
  push_syntax_error(['Expected a positive integer'],Old2),
  my_sql_blanks_star,
  my_positive_integer(_Frac),
  pop_syntax_error(Old2),
  my_sql_blanks_star,
  ")".
% % decimal
% my_sql_type(number(decimal)) -->
%   my_kw("DECIMAL").

  
my_optional_integer_range(R) -->
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  push_syntax_error(['Expected a positive integer'],Old),
  my_positive_integer(R),
  pop_syntax_error(Old),
  my_sql_blanks_star,
  ")".
my_optional_integer_range(_R) -->
  [].

% DML Statements
% DELETE FROM 
my_DML(delete_from(Table,true)) -->
  my_kw("DELETE"),
  push_syntax_error(['Expected FROM'],Old1),
  my_sql_blanks,
  my_kw("FROM"),
  pop_syntax_error(Old1),
  my_sql_blanks,
  push_syntax_error(['Expected table name'],Old2),
  my_p_ren_tablename(Table),
  pop_syntax_error(Old2).
% DELETE FROM ... WHERE 
my_DML(delete_from(Table,Condition)) -->
  my_kw("DELETE"),
  my_sql_blanks,
  push_syntax_error(['Expected FROM'],Old1),
  my_kw("FROM"),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected table name'],Old2),
  my_sql_blanks,
  my_p_ren_tablename(Table),
  pop_syntax_error(Old2),
  my_sql_blanks,
  push_syntax_error(['Expected WHERE'],Old3),
  my_kw("WHERE"),
  pop_syntax_error(Old3),
  push_syntax_error(['Expected valid where condition'],Old4),
%  my_sql_blanks,
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  my_where_condition(Condition),
  pop_syntax_error(Old4),
  my_closing_parentheses_star(N),
  push_syntax_error(['Expected end of statement']).

% INSERT INTO Table [VALUES(...) | SQL]
my_DML(insert_into(TableName,Colnames,Vs)) -->
  my_kw("INSERT"),
  push_syntax_error(['Expected INTO'],Old1),
  my_sql_blanks,
  my_kw("INTO"),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected table name'],Old2),
  my_sql_blanks,
  my_tablename(TableName),
  pop_syntax_error(Old2),
  push_syntax_error(['Expected VALUES or select statement'],Old3),
  my_sql_blanks,
  {(get_relation_arity(TableName,L) -> true ; true)},
  my_insert_values_sql(L,Vs),
  pop_syntax_error(Old3),
  {get_table_untyped_arguments(TableName,Colnames)},
  push_syntax_error(['Expected end of statement']).
% INSERT INTO Table(Columns) [VALUES(...) | SQL]
my_DML(insert_into(TableName,Colnames,Vs)) -->
  my_kw("INSERT"),
  push_syntax_error(['Expected INTO'],Old1),
  my_sql_blanks,
  my_kw("INTO"),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected table name'],Old2),
  my_sql_blanks,
  my_tablename(TableName),
  pop_syntax_error(Old2),
  push_syntax_error(['Expected a sequence of columns between parentheses'],Old3),
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  my_column_name_list(Colnames),
  my_sql_blanks_star,
  ")",
  pop_syntax_error(Old3),
  push_syntax_error(['Expected VALUES or select statement'],Old4),
  my_sql_blanks,
  {length(Colnames,L)},
  my_insert_values_sql(L,Vs),
  pop_syntax_error(Old4),
  push_syntax_error(['Expected end of statement']).

% UPDATE ... SET ... [WHERE ]
my_DML(update(Table,Assignments,true)) -->
  my_kw("UPDATE"),
  push_syntax_error(['Expected table name'],Old1),
  my_sql_blanks,
  my_p_ren_tablename(Table),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected SET'],Old2),
  my_sql_blanks,
  my_kw("SET"),
  pop_syntax_error(Old2),
  my_sql_blanks,
  push_syntax_error(['Expected sequence of column assignments Col=Expr'],Old3),
  my_update_assignments(Assignments),
  pop_syntax_error(Old3),
  push_syntax_error(['Expected end of statement']).
my_DML(update(Table,Assignments,Condition)) -->
  my_kw("UPDATE"),
  push_syntax_error(['Expected table name'],Old1),
  my_sql_blanks,
  my_p_ren_tablename(Table),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected SET'],Old2),
  my_sql_blanks,
  my_kw("SET"),
  pop_syntax_error(Old2),
  my_sql_blanks,
  push_syntax_error(['Expected sequence of column assignments Col=Expr'],Old3),
  my_update_assignments(Assignments),
  pop_syntax_error(Old3),
  push_syntax_error(['Expected WHERE'],Old4),
  my_sql_blanks,
  my_kw("WHERE"),
  pop_syntax_error(Old4),
  push_syntax_error(['Expected valid where condition'],Old5),
%  my_sql_blanks,
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  my_where_condition(Condition),
  my_closing_parentheses_star(N),
  pop_syntax_error(Old5),
  push_syntax_error(['Expected end of statement']).
my_DML(update(_Table,_Assignments,_Condition)) -->
  my_kw("UPDATE"),
  my_sql_blanks,
  my_kw("TABLE"),
  my_sql_blanks,
  push_syntax_error(['TABLE is not allowed after UPDATE'],sentence,_),
  {fail}.
  
my_update_assignments([Column,Expression]) -->
  my_update_assignment(Column,Expression).
my_update_assignments([Column,Expression|Assignments]) -->
  my_update_assignment(Column,Expression),
  my_sql_blanks_star,
  ",",
  my_sql_blanks_star,
  my_update_assignments(Assignments).

my_update_assignment(expr(ColumnName,_,string),Expression) -->
  my_column(attr(_T,ColumnName,_AS)),
  my_sql_blanks_star,
  "=",
  my_sql_blanks_star,
  my_sql_proj_expression(Expression,_Type).
  
my_insert_values_sql(L,Ts) -->
  my_kw("VALUES"),
  {!},
  push_syntax_error(['Expected a sequence of constants between parentheses'],Old),
  my_sql_blanks_star,
  my_sql_ground_tuple_list(L,Ts),
  pop_syntax_error(Old).
my_insert_values_sql(_L,SQLst) -->
%  push_syntax_error(['Expected a select statement'],Old),
  my_DQL(SQLst).
%  pop_syntax_error(Old).

% my_sql_ground_tuple_list(L,[T,T1|Ts]) -->
%   my_sql_ground_tuple_list(L,[T]),
%   my_sql_blanks_star,
%   ",",
%   {!},
%   my_sql_blanks_star,
%   my_sql_ground_tuple_list(L,[T1|Ts]).
% my_sql_ground_tuple_list(L,[T]) -->
%   my_sql_ground_tuple(T),
%   {length(T,TL),
%    (L=TL
%     -> 
%      true
%     ;
%      my_raise_exception(generic,syntax(['Unmatching number of values: ',TL,' (must be ',L,').']),[])
%    )
%   }.
my_sql_ground_tuple_list(L,[T|Ts]) -->
  my_sql_ground_tuple(L,T),
  my_sql_blanks_star, 
  my_remaining_sql_ground_tuple_list(L,Ts).

my_remaining_sql_ground_tuple_list(L,Ts) -->
  my_sql_blanks_star,
  ",", 
  {!},
  my_sql_blanks_star, 
  my_sql_ground_tuple_list(L,Ts).
my_remaining_sql_ground_tuple_list(_,[]) -->
  [].

  
my_sql_ground_tuple(Cs) -->
  my_sql_ground_tuple(_,Cs).

my_sql_ground_tuple(L,Cs) -->
  "(",
  my_sql_blanks_star,
  my_sql_constants(Cs),
  my_sql_blanks_star,
  ")",
  {length(Cs,TL),
   (L=TL
    ->
     true
    ;
     my_raise_exception(generic,syntax(['Unmatching number of values: ',TL,' (must be ',L,').']),[])
   )
  },
  {!}.

build_complete_values(TableName,Arity,Colnames,IVs,Vs,Error) :-
  length(Vs,Arity),
  length(IVs,Nbr_values),
  (length(Colnames,Nbr_values)
   ->
   Nbr_nulls is Arity-Nbr_values,
   length(NULLs,Nbr_nulls),
   build_null_list(Nbr_nulls,NULLs),
   concrete_nulls(NULLs,NULLs,_Grounded),
   get_att_positions(TableName,Colnames,Positions),
   replace_positions(Positions,IVs,Vs,Vs),
   list_between(1,Arity,AllPositions),
   my_set_diff(AllPositions,Positions,NullPositions),
   replace_positions(NullPositions,NULLs,Vs,Vs)
   ;
   write_error_log(['Incorrect number of values (must be ',Arity,').']),
   Error=true
  ).
  

% DQL Statements
my_DQL(SQLst) -->
  my_b_DQL(SQLst).
my_DQL(SQLst) -->
  my_ub_DQL(SQLst).
 
my_b_DQL(SQLst) -->
  "(",
  my_sql_blanks_star,
  my_DQL(SQLst),
  my_sql_blanks_star,
  ")".
  
% ASSUME
% my_ub_DQL((with(SQLst,[(ASQLst,Schema)]),_AS)) -->
%   my_kw("ASSUME"),
%   my_sql_blanks,
%   {!},
%   my_DQL((ASQLst,Schema)),
%   my_sql_blanks,
%   my_kw("IN"),
%   my_sql_blanks,
%   my_complete_untyped_schema(Schema),
%   my_sql_blanks,
%   my_DQL(SQLst).
my_ub_DQL((with(SQLst,SQLsts),_AS)) -->
  my_kw("ASSUME"),
  push_syntax_error(['Expected list of assumptions'],Old1),
  my_sql_blanks,
  {!},
  my_assume_list(SQLsts),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected SELECT statement'],Old2),
  my_sql_blanks,
  my_DQL(SQLst),
  pop_syntax_error(Old2),
  {allowed_with_schemas(SQLsts)},
  push_syntax_error(['Expected end of ASSUME statement']).
% WITH
my_ub_DQL((with(SQLst,SQLsts),_AS)) -->
  my_kw("WITH"),
  push_syntax_error(['Expected list of temporary view definitions'],Old1),
  my_sql_blanks,
  {!},
  my_local_view_definition_list(SQLsts),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected SELECT statement'],Old2),
  my_sql_blanks,
  my_DQL(SQLst),
  pop_syntax_error(Old2),
  {allowed_with_schemas(SQLsts)},
  push_syntax_error(['Expected end of ASSUME statement']).
% SELECT
my_ub_DQL(SQLst) -->
  my_select_DQL(SQLst),
  push_syntax_error(['Expected end of SELECT statement']).
% UNION
my_ub_DQL((union(D,R1,R2),_AS)) -->
  my_b_DQL(R1),
  my_sql_blanks_star,
  my_union_stmt(D),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  push_syntax_error(['Expected select statement'],Old),
  my_DQL(R2),
  pop_syntax_error(Old),
  my_closing_parentheses_star(N),
  push_syntax_error(['Expected end of UNION statement']).
my_ub_DQL((union(D,R1,R2),_AS)) -->
  my_select_DQL(R1),
  my_sql_blanks,
  my_union_stmt(D),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  push_syntax_error(['Expected select statement'],Old),
  my_DQL(R2),
  pop_syntax_error(Old),
  my_closing_parentheses_star(N),
  push_syntax_error(['Expected end of UNION statement']).
% EXCEPT
my_ub_DQL((except(_D,R1,R2),_AS)) -->
  my_b_DQL(R1),
  my_sql_blanks_star,
  my_set_difference_kw, % EXCEPT or MINUS
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  push_syntax_error(['Expected select statement'],Old),
  my_DQL(R2),
  pop_syntax_error(Old),
  my_closing_parentheses_star(N),
  push_syntax_error(['Expected end of EXCEPT/MINUS statement']).
my_ub_DQL((except(_D,R1,R2),_AS)) -->
  my_select_DQL(R1),
  my_sql_blanks,
  my_set_difference_kw, % EXCEPT or MINUS
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  push_syntax_error(['Expected select statement'],Old),
  my_DQL(R2),
  pop_syntax_error(Old),
  my_closing_parentheses_star(N),
  push_syntax_error(['Expected end of EXCEPT/MINUS statement']).
% INTERSECT
my_ub_DQL((intersect(_D,R1,R2),_AS)) -->
  my_b_DQL(R1),
  my_sql_blanks_star,
  my_kw("INTERSECT"),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  push_syntax_error(['Expected select statement'],Old),
  my_DQL(R2),
  pop_syntax_error(Old),
  my_closing_parentheses_star(N),
  push_syntax_error(['Expected end of INTERSECT statement']).
my_ub_DQL((intersect(_D,R1,R2),_AS)) -->
  my_select_DQL(R1),
  my_sql_blanks,
  my_kw("INTERSECT"),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  push_syntax_error(['Expected select statement'],Old),
  my_DQL(R2),
  pop_syntax_error(Old),
  my_closing_parentheses_star(N),
  push_syntax_error(['Expected end of INTERSECT statement']).

allowed_with_schemas(Ss) :-
  hypothetical(off),
  !,
  allowed_normal_with_schemas(Ss).
allowed_with_schemas(Ss) :-
  allowed_hyp_with_schemas(Ss,Ss).
  
allowed_normal_with_schemas([]).
allowed_normal_with_schemas([(_,S)|Schemas]) :-
  (member((_,S),Schemas)
   ->
    typed_schema_to_untyped_schema(S,US),
    my_raise_exception(generic,syntax(['Repeated schema: ',US]),[])
   ;
    (S==dual
     ->
      my_raise_exception(generic,syntax(['Cannot redefine built-in dual.']),[])
     ;
      allowed_normal_with_schemas(Schemas)
    )
  ).
  
allowed_hyp_with_schemas([],_Schemas).
allowed_hyp_with_schemas([(_,S1)|_Ss],Schemas) :-
  functor(S1,N,A1),
  A1>0,
  (member((_,S2),Schemas)
  ;
   get_table_typed_schema(N,S2),
   \+ is_prototype(N)
  ),
  functor(S2,N,A2),
  A2>0,
	typed_schema_to_untyped_schema(S1,US1),
	typed_schema_to_untyped_schema(S2,US2),
	(A1\==A2
	 ->
	  my_raise_exception(generic,syntax(['Incorrect number of arguments: ',US1,' vs. ',US2]),[])
	 ;
	  (US1\==US2
	   ->
	    my_raise_exception(generic,syntax(['Incorrect argument names: ',US1,' vs. ',US2]),[])
	   ;
	    (S1==dual
	     ->
	      my_raise_exception(generic,syntax(['Cannot redefine built-in dual.']),[])
	     ;
	      fail
	    )
	  )
	).
allowed_hyp_with_schemas([_|Ss],Schemas) :-
  allowed_hyp_with_schemas(Ss,Schemas).

is_prototype(N) :-
  my_table('$des',N,A),
  var(A).

my_set_difference_kw -->
  my_kw("EXCEPT").
my_set_difference_kw -->
  my_kw("MINUS").
  
% Local view definitions in WITH
my_local_view_definition_list([V]) -->
  my_local_view_definition(V).
my_local_view_definition_list([V|Vs]) -->
  my_local_view_definition(V),
  my_sql_blanks_star,
  ",",
  my_sql_blanks_star,
  my_local_view_definition_list(Vs).

my_local_view_definition((SQLst,Schema)) -->
  my_inline_optional("RECURSIVE"),
  push_syntax_error(['Expected schema'],Old1),
%  my_create_view_schema(Schema),
  my_assume_schema(Schema),
  pop_syntax_error(Old1),
  my_sql_blanks,
  push_syntax_error(['Expected AS'],Old2),
  my_kw("AS"),
  my_sql_blanks,
  pop_syntax_error(Old2),
  my_DQL((SQLst,Schema)).
my_local_view_definition((SQLst,Schema)) -->
%  push_syntax_error(['This use of WITH requires NOT IN after the SELECT statement'],Old),
  my_assume_not_in((SQLst,Schema)).
%  pop_syntax_error(Old).

% Atoms and clauses assumed in ASSUME
my_assume_list([V]) -->
  my_assume(V).
my_assume_list([V|Vs]) -->
  my_assume(V),
  my_sql_blanks_star,
  ",",
  my_sql_blanks_star,
  my_assume_list(Vs).

my_assume((SQLst,Schema)) -->
  my_DQL((SQLst,Schema)),
  my_sql_blanks,
  my_kw("IN"),
  my_sql_blanks,
  my_assume_schema(Schema).
my_assume((not(SQLst),Schema)) -->
  my_assume_not_in((SQLst,Schema)).
  
my_assume_not_in((SQLst,Schema)) -->
  my_DQL((SQLst,Schema)),
  my_sql_blanks,
  my_kw("NOT"),
  my_sql_blanks,
  push_syntax_error(['Expected IN after NOT'],Old),
  my_kw("IN"),
  my_sql_blanks,
  pop_syntax_error(Old),
  my_assume_schema(Schema).

my_assume_schema(Schema) -->
  my_complete_untyped_schema(Schema),
  {!}.
my_assume_schema(Schema) -->
  my_sql_user_identifier(Name),
%  {get_table_typed_schema(Name,Schema)}.
  {get_table_untyped_arguments(Name,Colnames),
   length(Colnames,L),
   length(Types,L),
   my_zipWith(':',Colnames,Types,TypedCols),
   Schema=..[Name|TypedCols]}.

my_optional(KW) -->
  my_kw(KW),
  my_sql_blanks_star.
my_optional(_KW) -->
  [].

my_inline_optional(KW) -->
  my_kw(KW),
  my_sql_blanks.
my_inline_optional(_KW) -->
  [].

% SELECT
my_select_DQL((select(DistinctAll,TopN,ProjList,
               from(Relations),
               where(WhereCondition),
               group_by(GroupList),
               having(HavingCondition),
               order_by(OrderArgs,OrderSpecs)),_AS)) -->
  my_select_stmt(DistinctAll,TopN),
  push_syntax_error(['Invalid SELECT list'],Old1),
  my_projection_list(ProjList),
  pop_syntax_error(Old1),
  my_sql_blanks,
  push_syntax_error(['Expected FROM clause'],Old2),
  my_kw("FROM"),
  pop_syntax_error(Old2),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  {!},
  my_relations(Relations),
  my_where_clause(WhereCondition),
  my_group_by_clause(GroupList),
  my_having_clause(HavingCondition),
  my_order_by_clause(OrderArgs,OrderSpecs),
  my_optional_fetch_first(TopN),
  my_closing_parentheses_star(N),
  {set_topN_default(TopN)},
  push_syntax_error(['Expected end of SELECT statement']).
% FROM-less SELECT
my_select_DQL((select(DistinctAll,TopN,ProjList,
               from([(dual,_Ren)]),
               where(true),
               group_by([]),
               having(true),
               order_by([],[])),_AS)) -->
  my_select_stmt(DistinctAll,TopN),
  push_syntax_error(['Invalid SELECT list'],Old1),
  my_projection_list(ProjList),
  {set_topN_default(TopN)},
  pop_syntax_error(Old1),
  push_syntax_error(['Expected end of SELECT statement']).


set_topN_default(top(all)) :-
  !.
set_topN_default(top(_N)).
  
my_where_clause(WhereCondition) -->
  my_sql_blanks_star,
  my_kw("WHERE"),
  push_syntax_error(['Invalid WHERE condition'],Old),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  {!},
  my_where_condition(WhereCondition),
  my_closing_parentheses_star(N),
  pop_syntax_error(Old).
my_where_clause(true) -->
  [].

my_group_by_clause(GroupList) -->
  my_sql_blanks,
  my_kw("GROUP"),
  my_sql_blanks,
  my_kw("BY"),
  push_syntax_error(['Invalid GROUP BY criteria'],Old),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  {!},
  my_column_list(GroupList),
  my_closing_parentheses_star(N),
  pop_syntax_error(Old).
my_group_by_clause([]) -->
  [].

my_having_clause(HavingCondition) -->
  my_sql_blanks,
  my_kw("HAVING"),
  push_syntax_error(['Invalid HAVING condition'],Old),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
 {!},
  my_sql_having_condition(HavingCondition),
  my_closing_parentheses_star(N),
  pop_syntax_error(Old).
my_having_clause(true) -->
  [].

my_order_by_clause(OrderArgs,OrderSpecs) -->
  my_sql_blanks,
  my_kw("ORDER"),
  push_syntax_error(['Expected BY'],Old1),
  my_sql_blanks,
  my_kw("BY"),
  pop_syntax_error(Old1),
  push_syntax_error(['Invalid ORDER BY criteria'],Old2),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  {!},
  my_order_list(OrderArgs,OrderSpecs),
  my_closing_parentheses_star(N),
  pop_syntax_error(Old2).
my_order_by_clause([],[]) -->
  [].
  
my_optional_fetch_first(top(N)) -->
  my_sql_blanks,
  my_kw("FETCH"),
  push_syntax_error(['Expected FIRST'],Old1),
  my_sql_blanks,
  {!},
  my_kw("FIRST"),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected an integer'],Old2),
  my_sql_blanks,
  my_integer(N1),
  pop_syntax_error(Old2),
  push_syntax_error(['Expected ROWS'],Old3),
  my_sql_blanks,
  my_kw("ROWS"),
  pop_syntax_error(Old3),
  push_syntax_error(['Expected ONLY'],Old4),
  my_sql_blanks,
  my_kw("ONLY"),
  pop_syntax_error(Old4),
%  remaining_chars(L),
  {N=N1
   ->
    true
   ;
    my_raise_exception(generic,syntax(['Number of maximum returned rows does not match in both specifications (',N,' vs. ',N1,').']),[])
%    set_semantic_error(['Number of maximum returned rows does not match in both specifications (',N,' vs. ',N1,').'],L)
   }.
%  pop_syntax_error(Old).
%   {N=N1
%    ->
%     true
%    ;
%     set_syntax_error(message(['Number of maximum returned rows does not match in both specifications (',N,' vs. ',N1,').']),L),
%     fail}.
my_optional_fetch_first(top(_N)) -->
  [].

  
% my_select_stmt(all,top(all)) -->
%   my_kw("SELECT ").
my_select_stmt(DistinctAll,TopN) -->
  my_kw("SELECT"),
  my_sql_blanks,
  my_optional_select_modifiers(DistinctAll,TopN).

% my_optional_select_modifiers(all,top(all)) -->
%   [],
%   !.
my_optional_select_modifiers(DistinctAll,TopN) -->
  my_select_distinct_all(DistinctAll),
  my_select_top_n(TopN),
  !.
my_optional_select_modifiers(DistinctAll,TopN) -->
  my_select_top_n(TopN),
  my_select_distinct_all(DistinctAll),
  !.
my_optional_select_modifiers(DistinctAll,top(_N)) -->
  my_select_distinct_all(DistinctAll),
  !.
my_optional_select_modifiers(all,TopN) -->
  my_select_top_n(TopN),
  !.
my_optional_select_modifiers(all,top(_N)) -->
  my_sql_blanks_star,
  !.
    
my_select_top_n(top(N)) -->
  my_sql_blanks_star,
  my_kw("TOP"),
  my_sql_blanks,
  my_integer(N),
  my_sql_blank,
  {!},
  my_sql_blanks_star.
% my_select_top_n(top(_N)) -->
%   [].

my_select_distinct_all(all) -->
  my_sql_blanks_star,
  my_kw("ALL"),
  my_sql_blanks,
  {!}.
my_select_distinct_all(distinct) -->
  my_sql_blanks_star,
  my_kw("DISTINCT"),
  my_sql_blanks,
  {!}.
% my_select_distinct_all(all) -->
%   [].

my_union_stmt(DistinctAll) -->
  my_kw("UNION"),
  my_union_distinct_all(DistinctAll).

my_union_distinct_all(all) -->
  my_sql_blanks,
  my_kw("ALL").
my_union_distinct_all(distinct) -->
  my_sql_blanks,
  my_kw("DISTINCT").
my_union_distinct_all(distinct) -->
  [].

my_sql_constants([C|Cs]) -->
  push_syntax_error(['Expected a constant'],Old),
  my_sql_constant(C),
  pop_syntax_error(Old),
  my_sql_blanks_star, 
  my_remaining_sql_constants(Cs).

my_remaining_sql_constants(Cs) -->
  my_sql_blanks_star,
  ",", 
  {!},
  my_sql_blanks_star, 
  my_sql_constants(Cs).
my_remaining_sql_constants([]) -->
  [].

my_relations([R|Rs]) --> 
  my_p_ren_relation(R), 
  my_sql_blanks_star,
  my_remaining_relations(Rs).

my_remaining_relations(Rs) -->
  ",", 
%  {!},   Does not work with WITH statements, where commas separate local view definitions
  my_sql_blanks_star, 
  my_relations(Rs).
my_remaining_relations([]) -->
  [].

my_p_ren_leading_relation(R) -->
  my_opening_parentheses_star(N),
  my_p_ren_relation(R),
  my_closing_parentheses_star(N),
  my_optional_sql_blanks(N).
  
my_p_ren_relation(R) --> 
  my_relation(R).
my_p_ren_relation(R) --> 
  my_ren_relation(R).

my_ren_relation((R,[J|Args])) -->
  my_opening_parentheses_star(N),
  my_relation((R,[J|Args])),
  my_closing_parentheses_star(N),
  my_optional_sql_blanks(N),
  my_inline_optional("AS"),
  my_sql_user_identifier(I),
  {ignore_autorenaming(R,I,J)}.

ignore_autorenaming(I,I,_) :- % Ignore user renaming
  !.
ignore_autorenaming(_,I,I). % Use user renaming

my_relation(R) --> 
  my_opening_parentheses_star(N),
  my_ub_relation(R),
  my_closing_parentheses_star(N).
  
% my_relation(R) --> 
%   my_b_relation(R).
% my_relation(R) --> 
%   my_ub_relation(R).

% my_b_relation(R) --> 
%   "(",
%   my_sql_blanks_star,
%   my_relation(R),
%   my_sql_blanks_star,
%   ")".

my_ub_relation((R,_AS)) --> 
  my_join_relation(R).
my_ub_relation((R,_AS)) --> 
  my_division_relation(R).
%,
%  {!}.
my_ub_relation(R) --> 
  my_non_join_relation(R).

% The following clause overrides next two clauses
my_non_join_relation((T,_)) -->
  my_sql_user_identifier(T).
%  ,{\+ sql_keyword(T)}.
% my_non_join_relation((T,_)) -->
%   my_tablename(T).
% %  , {!}. % This is commented because of SQL statements in the FROM list
% my_non_join_relation((T,_)) -->
%   my_viewname(T).
% %,  {!}.
my_non_join_relation((R,AS)) -->
  my_DQL((R,AS)).

% DIVISION
my_division_relation(DR,SIn,SOut) :-
  look_ahead_division_op(SIn,SOut1),
  my_list_diff(SIn,SOut1,SDiff),
%  my_p_ren_relation(LR,SDiff,[]),
  my_p_ren_leading_relation(LR,SDiff,[]),
  my_remainder_division_relation(LR,DR,SOut1,SOut).

% look_ahead_division_op looks the input list for a division operator. 
% This way, parsing may fail in advance and avoid cycling
look_ahead_division_op(SIn,SOut) :-
  my_chars(_Cs,SIn,SOut),
  my_division_operator(SOut,_SOut2).

my_division_operator -->
  my_kw("DIVISION").
  
my_remainder_division_relation(LR,JR) -->
%  my_sql_blanks,
  my_kw("DIVISION"),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  my_p_ren_relation(RR),
  my_closing_parentheses_star(N),
  {JR =.. [division,LR,RR]}.

% JOINs
my_join_relation(JR,SIn,SOut) :-
  look_ahead_join_op(JOp,SIn,SOut1),
  my_list_diff(SIn,SOut1,SDiff),
%  my_p_ren_relation(LR,SDiff,[]),
  my_p_ren_leading_relation(LR,SDiff,[]),
  my_remainder_join_relation(LR,JOp,JR,SOut1,SOut).

% look_ahead_join_op looks the input list for a join operator. 
% This way, parsing may fail in advance and avoid cycling
look_ahead_join_op(JOp,SIn,SOut) :-
  my_chars(_Cs,SIn,SOut),
%  my_sql_blanks(SOut,SOut1),
  my_kw("NATURAL",SOut,SOut2),
  my_sql_blanks(SOut2,SOut3),
%  !, % WARNING this cut
  my_join_operator(JOp,SOut3,_SOut4).
look_ahead_join_op(JOp,SIn,SOut) :-
  my_chars(_Cs,SIn,SOut),
  my_join_operator(JOp,SOut,_SOut2).

% L1-L2=LO LO+L2=L1
my_list_diff(L1,L2,LO) :- 
  append(LO,L2,L1).

% NATURAL
my_remainder_join_relation(LR,JOp,JR) -->
%  my_sql_blanks,
  my_kw("NATURAL"),
  my_sql_blanks,
%  push_syntax_error(['Expected join operator'],Old1),
  my_join_operator(JOp),
%  pop_syntax_error(Old1),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
%  !, % WARNING this cut
%  push_syntax_error(['Expected right relation in join'],Old2),
  my_p_ren_relation(RR),
%  pop_syntax_error(Old2),
  my_closing_parentheses_star(N),
  {JR =.. [JOp,LR,RR,equijoin(natural)]}.
% ON
my_remainder_join_relation(LR,JOp,JR) -->
%  my_sql_blanks,
  my_join_operator(JOp),
  my_sql_blanks_star,
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  my_p_ren_relation(RR),
  my_closing_parentheses_star(N),
  my_optional_join_condition(N,Cond),
  {JR =.. [JOp,LR,RR,Cond]}.
  
my_optional_join_condition(N,Cond) -->
  my_optional_sql_blanks(N),
  my_join_condition(Cond).
my_optional_join_condition(_N,true) -->
  [].


my_join_operator(inner_join) -->
  my_inline_optional("INNER"),
  my_kw("JOIN").
my_join_operator(Outer_join) -->
  my_outer_kind(Outer_join),
  my_inline_optional("OUTER"),
  my_kw("JOIN").

my_outer_kind(left_join) -->
  my_kw("LEFT"),
  my_sql_blanks,
  {!}.
my_outer_kind(right_join) -->
  my_kw("RIGHT"),
  my_sql_blanks,
  {!}.
my_outer_kind(full_join) -->
  my_kw("FULL"),
  my_sql_blanks.

    
my_join_condition(Condition) -->
  my_kw("ON"),
  push_syntax_error(['Expected valid ON condition'],Old),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  {!},
  my_on_condition(Condition),
  my_closing_parentheses_star(N),
  pop_syntax_error(Old).
my_join_condition(equijoin(Atts)) -->
  my_kw("USING"),
  push_syntax_error(['Expected a column sequence between parentheses'],Old),
  my_sql_blanks_star,
  "(",
  {!},
  my_sql_blanks_star,
  my_column_list(Atts),
  my_sql_blanks_star,
  ")",
  pop_syntax_error(Old).
my_join_condition(true) -->
  [].

my_where_condition(C) --> 
  my_sql_condition(C).

my_on_condition(C) --> 
  my_sql_condition(C).

my_sql_having_condition(C) --> 
  my_sql_condition(C).


my_sql_condition(F) -->
  my_sql_condition(1200,F).
   
my_sql_condition(PP,To) -->
  my_cond_factor(L), 
  my_r_sql_condition(PP,0,L/To).
my_sql_condition(PP,To) -->
  "(", 
  my_sql_blanks_star, 
  push_syntax_error(['Expected valid SQL condition'],Old),
  my_sql_condition(1200,T), 
  pop_syntax_error(Old),
  my_sql_blanks_star, 
  my_right_parenthesis,
  !,
  my_r_sql_condition(PP,0,T/To).
my_sql_condition(PP,To) -->
  {my_sql_operator(P,FX,SOP,OP),
   prefix(P,FX,PR),
   P=<PP},
  my_kw(SOP),
  push_syntax_error(['Expected valid SQL condition'],Old),
  my_right_spaced(SOP),
  my_sql_condition(PR,T), 
  pop_syntax_error(Old),
  {NT=..[OP,T]},
  my_r_sql_condition(PP,P,NT/To).

my_r_sql_condition(PP,Pi,Ti/To) -->
  {my_sql_operator(P,YFX,SOP,OP),
   infix(P,YFX,PL,PR),
   P=<PP,
   Pi=<PL,
   NT=..[OP,Ti,T]},
  push_syntax_error(['Expected infix SQL operator'],Old1),
  my_left_spaced(SOP), 
  my_kw(SOP),
  pop_syntax_error(Old1),
  my_right_spaced(SOP), 
  my_sql_condition(PR,T), 
  my_r_sql_condition(PP,P,NT/To).
% my_r_sql_condition(PP,Pi,Ti/To) --> % No posfix operators in SQL conditions
%   {my_sql_operator(P,FX,SOP,OP),
%    posfix(P,FX,PL),
%    P=<PP,
%    Pi=<PL,
%    NT=..[OP,Ti]},
%   my_sql_blanks_star, 
%   my_kw(SOP),
%   my_r_sql_condition(PP,P,NT/To).
my_r_sql_condition(_,_,Ti/Ti) -->
  [].
  
my_sql_operator(1100,xfy,     "OR",'or').
%my_sql_operator(1050,xfy,     "->",'->').
%my_sql_operator(1050,xfy,"IMPLIES",'->').
my_sql_operator(1000,xfy,   "AND",'and').
my_sql_operator( 900, fy,   "NOT",'not').
%my_sql_operator( 900, fy,    "IN",'in').

my_cond_factor(E) -->
  my_b_sql_condition(E).
my_cond_factor(true) --> 
  my_kw("TRUE").
my_cond_factor(false) --> 
  my_kw("FALSE").
my_cond_factor(is_null(R)) --> 
  my_sql_expression(R,_T), 
  my_sql_blanks, 
  my_kw("IS"), 
  my_sql_blanks, 
  my_kw("NULL").
my_cond_factor(not(is_null(R))) --> 
  my_sql_expression(R,_T), 
  my_sql_blanks, 
  my_kw("IS"), 
  my_sql_blanks, 
  my_kw("NOT"), 
  my_sql_blanks, 
  my_kw("NULL").
% my_cond_factor(not(C)) -->
%   my_kw("NOT"),
%   my_opening_parentheses_star(N),
%   my_optional_sql_blanks(N),
% %  my_cond_factor(C),
%   my_cond_factor(C),
%   my_closing_parentheses_star(N).
my_cond_factor(exists(R)) --> 
  my_kw("EXISTS"),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  my_DQL(R),
  my_closing_parentheses_star(N).
my_cond_factor(and('<='(L,C),'<='(C,R))) --> 
  my_sql_expression(C,_CT),
  my_sql_blanks,
  my_kw("BETWEEN"),
  my_sql_blanks,
  my_sql_expression(L,_LT),
  my_sql_blanks,
  my_kw("AND"),
  my_sql_blanks,
  my_sql_expression(R,_RT).
my_cond_factor(in(L,R)) --> 
  my_column_or_constant_tuple(L,A),
  my_sql_blanks,
  my_kw("IN"),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  my_DQL_or_constant_tuples(A,R),
  my_closing_parentheses_star(N).
my_cond_factor(not_in(L,R)) --> 
  my_column_or_constant_tuple(L,A),
  my_sql_blanks,
  my_kw("NOT"),
  my_sql_blanks,
  my_kw("IN"),
  my_opening_parentheses_star(N),
  my_optional_sql_blanks(N),
  my_DQL_or_constant_tuples(A,R),
  my_closing_parentheses_star(N).
my_cond_factor(C) --> 
  my_sql_expression(L,_LT), 
  my_sql_blanks_star, 
  push_syntax_error(['Expected comparison operator'],Old),
  my_relop(O), 
  pop_syntax_error(Old),
  my_sql_blanks_star, 
  my_sql_expression(R,_RT),
  {C=..[O,L,R]}.
my_cond_factor(true) --> 
  {current_db(_,mysql)},
  my_sql_constant(_C).

% Bracketed SQL condition
my_b_sql_condition(SQLst) -->
  "(",
  my_sql_blanks_star,
  my_sql_condition(SQLst),
  my_sql_blanks_star,
  my_right_parenthesis.

my_DQL_or_constant_tuples(_A,R) -->
  my_DQL(R).
my_DQL_or_constant_tuples(A,R) -->
  "(",
  my_sql_ground_tuple_list(A,Ts),
  ")",
  {in_tuples_to_DQL(Ts,R)}.
my_DQL_or_constant_tuples(_,R) -->
  my_sql_ground_tuple(_,Cs),
  {list_to_list_of_lists(Cs,L),
   in_tuples_to_DQL(L,R)}.
my_DQL_or_constant_tuples(1,R) -->
  my_sql_constant(C),
  {in_tuples_to_DQL([[C]],R)}.

in_tuples_to_DQL([Cs],(select(all,top(all),Es,from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),_)) :-
  args_to_exprs(Cs,Es).
in_tuples_to_DQL([C1,C2|Cs],(union(all,SQL1,SQL2),_)) :-
  in_tuples_to_DQL([C1],SQL1),
  in_tuples_to_DQL([C2|Cs],SQL2).
  
args_to_exprs([],[]).
args_to_exprs([C|Cs],[expr(C,_,_)|Es]) :-
  args_to_exprs(Cs,Es).

my_column_or_constant_tuple(Cs,A) --> 
  "(",
  my_sql_blanks_star,
  my_column_or_constant_list(Cs),
  my_sql_blanks_star,
  ")",
  {length(Cs,A)}.
my_column_or_constant_tuple([C],1) --> 
%  my_column_or_constant(C).
  my_sql_expression(C,_T).

my_column_tuple_lblank(Cs) --> 
  my_sql_blank,
  my_sql_blanks_star,
  my_column_tuple(Cs,1).
my_column_tuple_lblank(Cs) --> 
  my_sql_blanks_star,
  my_column_tuple(Cs,0).
  
my_column_tuple_rblank(Cs) --> 
  my_column_tuple(Cs,1),
  my_sql_blank,
  my_sql_blanks_star.
my_column_tuple_rblank(Cs) --> 
  my_column_tuple(Cs,0),
  my_sql_blanks_star.
  
my_column_tuple_lrblank(Cs) --> 
  my_sql_blank,
  my_sql_blanks_star,
  my_column_tuple(Cs,1),
  my_sql_blanks.
my_column_tuple_lrblank(Cs) --> 
  my_sql_blanks_star,
  my_column_tuple(Cs,0),
  my_sql_blanks_star.
  
my_column_tuple(Cs) --> 
  my_column_tuple(Cs,_N).

my_column_tuple(Cs,0) --> 
  "(",
  my_sql_blanks_star,
  my_column_name_list(Cs),
  my_sql_blanks_star,
  ")".
my_column_tuple([C],1) --> 
  my_column_name(C).

my_column_name(C) -->
  my_untyped_column(C).
  
my_column_list([C,C2|Cs]) -->
  my_column(C),
  my_sql_blanks_star,
  ",",
  my_sql_blanks_star,
  my_column_list([C2|Cs]).
my_column_list([C]) -->
  my_column(C).

my_column_or_constant_list([C,C2|Cs]) -->
%  my_column_or_constant(C),
  my_sql_expression(C,_T),
  my_sql_blanks_star,
  ",",
  my_sql_blanks_star,
  my_column_or_constant_list([C2|Cs]).
my_column_or_constant_list([C]) -->
%  my_column_or_constant(C).
  my_sql_expression(C,_T).

my_column_or_constant(C) --> 
  my_column(C).
my_column_or_constant(C) --> 
  my_sql_constant(C).

my_order_list([C,C2|Cs],[O,O2|Os]) -->
  my_expr_order(C,O),
  my_sql_blanks_star,
  ",",
  my_sql_blanks_star,
  my_order_list([C2|Cs],[O2|Os]).
my_order_list([C],[O]) -->
  my_expr_order(C,O).

my_expr_order(C,O) -->
  my_sql_proj_expression(C,_T),
  my_optional_order(O).
  
my_optional_order(O) -->
  my_sql_blanks,
  my_order(O).
my_optional_order(a) -->
  [].
    
my_order(d) -->
  my_kw("DESCENDING"),
  !.
my_order(d) -->
  my_kw("DESC").
my_order(a) -->
  my_kw("ASCENDING"),
  !.
my_order(a) -->
  my_kw("ASC").

my_relop(RO) --> 
  my_set_op(RO).
my_relop(RO) --> 
  my_tuple_op(RO).

my_set_op(SO) -->
  my_tuple_op(TO),
  my_sql_blanks_star,
  my_kw("ALL"),
  {atom_concat(TO,'_all',SO)}.
my_set_op(SO) -->
  my_tuple_op(TO),
  my_sql_blanks_star,
  my_kw("ANY"),
  {atom_concat(TO,'_any',SO)}.
  
my_tuple_op(RO) --> 
  {map_cond(RO,_), 
   atom_codes(RO,SRO)},
  my_string(SRO).

my_projection_list(*) --> 
  "*".
my_projection_list([A|As]) --> 
  my_p_ren_argument(A), 
  my_sql_blanks_star, 
  ",", 
%  {!},  % It could be part of a WITH definition, so no cut is allowed
  my_sql_blanks_star, 
  my_projection_list(As).
my_projection_list([A]) --> 
  my_p_ren_argument(A).

my_p_ren_argument(A) --> 
  my_ren_argument(A).
my_p_ren_argument(A) --> 
  my_sql_argument(A,_AS).

my_ren_argument(Arg) -->
  my_sql_argument(Arg,AS),
  my_sql_blanks, 
  my_inline_optional("AS"), 
  my_sql_user_identifier(AS).

%my_sql_argument(attr(R,C,AS),AS) --> % Identifers are assumed to be references to table or view attributes, even when they do not exist already (because of the view construction)
%  my_column(attr(R,C,AS)).           % In sql_to_ra, references to expressions are known, so that incorrectly assumed attributes can be changed to such references
my_sql_argument((R,(*)),'$') -->  % Cannot be renamed
  my_relname(R),
  ".*".
my_sql_argument(E,AS) -->
  my_sql_proj_expression(E,AS).
  
my_sql_proj_expression(expr(E,AS,Type),AS) -->
  my_sql_expression(E,Type).

my_column(attr(R,C,_AS)) --> 
  my_relname(R),
  ".",
  my_colname(C).
my_column(attr(_T,C,_AS)) --> 
  my_colname(C).
%   ,
%   {\+ my_sql_identifier(C)}.

my_relname(T) --> 
  my_sql_user_identifier(T).
%   ,
%   {\+ my_sql_identifier(T)}. % But allow [sql_identifier] as a table name (Access)

my_p_ren_tablename((T,_R)) -->
  my_tablename(T).
my_p_ren_tablename(T) --> 
  my_ren_tablename(T).

my_ren_tablename((T,[I|Args])) -->
  my_tablename(T),
  my_sql_blanks, 
  my_inline_optional("AS"),
  my_sql_user_identifier(I),
  {my_table('$des',T,A),
   length(Args,A)}.

my_tablename(T) --> 
  my_sql_user_identifier(T).
% The following is omitted to work without schema (due to ODBC connections)
%   {my_table('$des',T,_TA),
%    \+ (my_view('$des',T,_VA,_Q,_L,_DLs,_ODLIds,_LVDs,_SCs))}.

my_viewname(V) --> 
  my_sql_user_identifier(V).
%  {my_view('$des',V,_VA,_SQL,_,_DLs,_ODLIds,_LVDs,_SCs)}. % Maybe under construction and not yet known

my_colname(C) --> 
  my_sql_user_identifier(C).
%  {my_attribute('$des',_Pos,_T,C,_Type)}. % Maybe from a view under construction and not yet known

allowed_view_definition(TableName) :-
  atom(TableName), % tablename and arity (when known) are checked afterwards
  !.
allowed_view_definition(Schema) :-
  allowed_schema(Schema).

allowed_schema(Schema) :-
  Schema =.. [TableName|ColnameTypeList],
  length(ColnameTypeList,Arity),
  allowed_tablename(TableName,Arity),
  allowed_colnametype_list(ColnameTypeList).
  
allowed_colnametype_list([]) :-
  my_raise_exception(generic,syntax(['Schema with no columm.']),[]).
allowed_colnametype_list(ColnameTypeList) :-
  my_unzip(ColnameTypeList,Colnames,_Types),
  allowed_colnames(Colnames).

allowed_tablename(T,A) :-
  (datalog_metapredicate(T,A)
   ->
    my_raise_exception(unallowed_identifier(table,T,A),syntax(['']),[])
   ;
    true
  ).
% allowed_tablename(TableName) :-
%   (my_sql_keyword(TableName)
%    ->
%    my_raise_exception(unallowed_identifier(table,TableName),syntax(['']),[])
%    ;
%    true
%   ).

allowed_viewname(T,A) :-
  (datalog_metapredicate(T,A)
   ->
   my_raise_exception(unallowed_identifier(table,T,A),syntax(['']),[])
   ;
   true
  ).
% allowed_viewname(Viewname) :-
%   (my_sql_keyword(Viewname)
%    ->
%    my_raise_exception(unallowed_identifier(view,Viewname),syntax(['']),[])
%    ;
%    true
%   ).

allowed_colname(_).
% allowed_colname(Colname) :-
%   var(Colname),
%   !.
% allowed_colname(Colname) :-
%   (my_sql_keyword(Colname)
%    ->
%    my_raise_exception(unallowed_identifier(column,Colname),syntax(['']),[])
%    ;
%    true
%   ).

allowed_colnames(Colnames) :-
  my_map_1(allowed_colname,Colnames),
  my_remove_duplicates_sort(Colnames,SColnames),
  length(Colnames,L),
  (length(SColnames,L)
   ->
    true
   ;
    duplicates_in_list(Colnames,Ds),
    my_raise_exception(generic,syntax(['Duplicated column names: ',Ds,'.']),[])
    %write_error_log(['Duplicated column names.'])
   ).
   
%my_sql_user_identifier: An identifier either: 
% - starting by a letter, followed by letters, digits or underscores
% - characters enclosed between marks
% - starting with $, a system-generated identifier
% Returns an atom
% my_sql_user_identifier(DolarI) --> 
%   "$",
%   my_sql_user_identifier(I),
%   {atom_concat('$',I,DolarI)}.
% my_sql_user_identifier(I) --> 
%   my_alfa(A),
%   my_alfanums(Is),
% %  remaining_chars(N),
% %   {name(I,[A|Is])}.
%   {name(I,[A|Is]),
%    to_lowercase(I,LI),
%    to_uppercase(I,UI),
%    ((LI==not
%      ;
%      (LI\==dual,
%       LI\==select_not_null,
%       my_builtin_relation(LI,_,_,_))
%      ;
% %      fail
%       sql_keyword(UI)
%      )
%     ->
% %      language(L),
% %      (L==datalog -> M='a Datalog query' ; (L==sql -> M='an SQL statement'; M='an RA expression')),
% %      set_syntax_error(message(['Incorrect use of built-in "',I,'" as a user identifier when parsing input as ',M,'.']),N),
%      fail
%     ;
%      true
%    )
% my_sql_user_identifier(I) --> 
%   my_sql_left_quotation_mark(Mark),
%   my_mark_enclosed_chars(Mark,Is),
%   my_sql_right_quotation_mark(Mark),
% %  {name(I,Is)}.
%   {name(I,Is),
%    \+ my_sql_keyword(I)
%   }.
my_sql_user_identifier(DolarI) --> 
  "$",
  my_sql_user_identifier(I),
  {atom_concat('$',I,DolarI)}.
my_sql_user_identifier(I) --> 
  my_alfa(A),
  my_alfanums(Is),
  {atom_codes(I,[A|Is]),
   to_lowercase(I,LI),
   LI\==not}.
my_sql_user_identifier(I) --> 
  my_sql_left_quotation_mark(Mark),
  my_mark_enclosed_chars(Mark,Is),
  my_sql_right_quotation_mark(Mark),
 {atom_codes(I,Is)}.
  
% my_mark_enclosed_chars: one or more characters enclosed between SQL delimiter marks
my_mark_enclosed_chars(Mark,[C]) -->
  my_non_mark_char(Mark,C).
my_mark_enclosed_chars(Mark,[C|Cs]) -->
  my_non_mark_char(Mark,C),
  my_mark_enclosed_chars(Mark,Cs).

% my_non_quote_char: one character inside a quoted atom
my_non_mark_char(Mark,C) --> % A pair of marks inside a marked atom denoting a single mark
  my_sql_left_quotation_mark(Mark),
  my_sql_left_quotation_mark(Mark),
  {my_sql_left_quotation_mark(Mark,[C],[])}.
my_non_mark_char(Mark,C) --> % A pair of marks inside a marked atom denoting a single mark
  my_sql_right_quotation_mark(Mark),
  my_sql_right_quotation_mark(Mark),
  {my_sql_right_quotation_mark(Mark,[C],[])}.
my_non_mark_char(Mark,C) --> % A single mark is not allowed inside a mark-delimited atom
  [C],
  {\+ my_sql_left_quotation_mark(Mark,[C],[]),
   \+ my_sql_right_quotation_mark(Mark,[C],[])}.

my_sql_left_quotation_mark(square_brackets) -->
  "[".
my_sql_left_quotation_mark(double_quotes) -->
  """".
my_sql_left_quotation_mark(back_quotes) -->
  "`".

my_sql_left_quotation_mark("[",ACCESS) :-
  to_uppercase(ACCESS,'ACCESS'),
  !. 
my_sql_left_quotation_mark("",DB2) :-
  to_uppercase(DB2,'DB2'),
  !.
my_sql_left_quotation_mark("`",MYSQL) :-
  to_uppercase(MYSQL,'MYSQL'),
  !.
my_sql_left_quotation_mark("""",ORACLE) :-
  to_uppercase(ORACLE,'ORACLE'),
  !. 
my_sql_left_quotation_mark("""",SQLSERVER) :-
  to_uppercase(SQLSERVER,'SQLSERVER'),
  !. 
my_sql_left_quotation_mark("[",SYBASE) :-
  to_uppercase(SYBASE,'SYBASE'),
  !. 
my_sql_left_quotation_mark("""",_).
  
my_sql_right_quotation_mark(square_brackets) -->
  "]".
my_sql_right_quotation_mark(double_quotes) -->
  """".
my_sql_right_quotation_mark(back_quotes) -->
  "`".

my_sql_right_quotation_mark("]",ACCESS) :-
  to_uppercase(ACCESS,'ACCESS'),
  !. 
my_sql_right_quotation_mark("",DB2) :-
  to_uppercase(DB2,'DB2'),
  !.
my_sql_right_quotation_mark("`",MYSQL) :-
  to_uppercase(MYSQL,'MYSQL'),
  !.
my_sql_right_quotation_mark("""",ORACLE) :-
  to_uppercase(ORACLE,'ORACLE'),
  !. 
my_sql_right_quotation_mark("""",SQLSERVER) :-
  to_uppercase(SQLSERVER,'SQLSERVER'),
  !. 
my_sql_right_quotation_mark("]",SYBASE) :-
  to_uppercase(SYBASE,'SYBASE'),
  !. 
my_sql_right_quotation_mark("""",_).

% my_sql_identifier(I) :-
%   to_uppercase(I,CI),
%   sql_identifier(CI).

my_sql_keyword(I) :-
  to_uppercase(I,CI),
  sql_keyword(CI).

delimited_sql_identifier_list(_,[],[]).
delimited_sql_identifier_list(Connection,[Identifier|Identifiers],[Str|Strs]) :-
  delimited_sql_identifier(Connection,Identifier,Str),
  delimited_sql_identifier_list(Connection,Identifiers,Strs).
  
delimited_sql_identifier(Connection,Identifier,StrDelimitedIdentifier) :-
  my_odbc_get_dbms(Connection,DBMS),
  delimited_dbms_sql_str_identifier(Identifier,DBMS,StrDelimitedIdentifier).

delimited_dbms_sql_identifier(Identifier,DBMS,DelimitedIdentifier) :-
  delimited_dbms_sql_str_identifier(Identifier,DBMS,StrDelimitedIdentifier),
  atom_codes(DelimitedIdentifier,StrDelimitedIdentifier).

delimited_dbms_sql_identifier_list([],_DBMS,[]).
delimited_dbms_sql_identifier_list([X|Xs],DBMS,[Y|Ys]) :-
  delimited_dbms_sql_identifier(X,DBMS,Y) ,
  delimited_dbms_sql_identifier_list(Xs,DBMS,Ys).
  
delimited_dbms_sql_str_identifier(Identifier,DBMS,StrDelimitedIdentifier) :-
  my_odbc_dbms_relation_name(DBMS,Identifier,ODBCIdentifier),
  atom_codes(ODBCIdentifier,StrODBCIdentifier),
  my_sql_left_quotation_mark(StrLeftDelim,DBMS),
  my_sql_right_quotation_mark(StrRightDelim,DBMS),
  concat_lists([StrLeftDelim,StrODBCIdentifier,StrRightDelim],StrDelimitedIdentifier).

sql_keyword('ALL').
sql_keyword('ADD').
sql_keyword('ALTER').
sql_keyword('AND').
sql_keyword('ANY').
sql_keyword('ANSWER').
sql_keyword('AS').
sql_keyword('ASC').
sql_keyword('ASCENDING').
sql_keyword('ASSUME').
sql_keyword('AVG').
sql_keyword('BETWEEN').
sql_keyword('BY').
sql_keyword('CANDIDATE').
sql_keyword('CHECK').
sql_keyword('CONSTRAINT').
sql_keyword('COUNT').
sql_keyword('CREATE').
sql_keyword('DATABASE').
sql_keyword('DATABASES').
sql_keyword('DECIMAL').
sql_keyword('DELETE').
sql_keyword('DESC').
sql_keyword('DESCENDING').
sql_keyword('DESCRIBE').
sql_keyword('DETERMINED').
sql_keyword('DISTINCT').
sql_keyword('DIVISION').
sql_keyword('DROP').
sql_keyword('EXCEPT').
sql_keyword('EXISTS').
sql_keyword('FALSE').
sql_keyword('FETCH').
sql_keyword('FIRST').
sql_keyword('FOREIGN').
sql_keyword('FROM').
sql_keyword('FULL').
sql_keyword('GROUP').
sql_keyword('HAVING').
sql_keyword('IF').
sql_keyword('IN').
sql_keyword('INNER').
sql_keyword('INSERT').
sql_keyword('INTERSECT').
sql_keyword('INTO').
sql_keyword('JOIN').
sql_keyword('KEY').
sql_keyword('LEFT').
sql_keyword('LIKE').
sql_keyword('MINUS').
sql_keyword('NATURAL').
sql_keyword('NOT').
sql_keyword('NULL').
sql_keyword('NUMBER').
sql_keyword('ON').
sql_keyword('ONLY').
sql_keyword('OR').
sql_keyword('ORDER').
sql_keyword('OUTER').
sql_keyword('PRIMARY').
sql_keyword('RECURSIVE').
sql_keyword('REFERENCES').
sql_keyword('REPLACE').
sql_keyword('RIGHT').
sql_keyword('ROWS').
sql_keyword('SELECT').
sql_keyword('SHOW').
sql_keyword('SOME').
sql_keyword('TABLE').
sql_keyword('TABLES').
sql_keyword('TO').
sql_keyword('TRUE').
sql_keyword('UNION').
sql_keyword('UNIQUE').
sql_keyword('USING').
sql_keyword('VALUES').
sql_keyword('VIEW').
sql_keyword('VIEWS').
sql_keyword('WHERE').
sql_keyword('WITH').

sql_identifier(K) :-
  sql_keyword(K).
sql_identifier(C) :-
  arithmetic_constant(_Value,LC,_Text),
  to_uppercase(LC,C).
sql_identifier(F) :-
  arithmetic_function(LF,_PrologF,_Text,_Kind,_Type,_Arity),
  to_uppercase(LF,F).

my_alfanums([A|As]) --> 
  my_alfanum(A),
  {!},
  my_alfanums(As).
my_alfanums([]) --> 
  [].

my_alfa(C) --> 
  my_lowercase(C),
  !. % :::WARNING
my_alfa(C) --> 
  my_uppercase(C).

my_alfa(C) :- 
  my_lowercase(C).
my_alfa(C) :- 
  my_uppercase(C).

my_alfanum(C) --> 
  my_alfa(C).
my_alfanum(C) --> 
  my_digit(C).
my_alfanum(C)--> 
  "_",
  {"_" = [C]}.

my_sql_non_arithmetic_constant(cte(C,string(S))) -->
  my_sql_string_constant(cte(C,string(S))).
my_sql_non_arithmetic_constant(cte('$NULL'(N),_T)) -->
  my_kw("NULL"),
  {get_null_id(N)}.
  
my_sql_constant(cte(C,number(N))) --> 
  my_number(C),
  {float(C) -> N=float ; true}.
my_sql_constant(cte(C,T)) --> 
  my_sql_non_arithmetic_constant(cte(C,T)).
% my_sql_constant(cte(C,string(S))) --> 
%   my_sql_string_constant(cte(C,string(S))).
% my_sql_constant(cte('$NULL'(N),_T)) --> 
%   my_kw("NULL"),
%   {get_null_id(N)}.

my_sql_string_constant(cte(C,string(_S))) --> 
  "'",
  push_syntax_error(['Expected an ending single quote'],Old),
  my_sql_string(Cs),
%  {name(C,Cs)},
  {atom_codes(C,Cs)},
  "'",
  pop_syntax_error(Old),
  {add_to_dictionary(C)}. % WARNING: ONLY FOR TEST CASE GENERATION
  
% SQL strings allowing escaped single quotes
my_sql_string([]) -->
  [].
my_sql_string([C|Cs]) -->
  [C],
  {"'" =\= [C]},
  my_sql_string(Cs).
my_sql_string([Q|Cs]) -->
  [Q,Q],
  {"'" = [Q]},
  my_sql_string(Cs).
  
:- dynamic(dictionary/1). 
add_to_dictionary(SC) :-
  (retract(dictionary(SCs)) ->
   (member(SC,SCs) ->
     assertz(dictionary(SCs))
    ;
     assertz(dictionary([SC|SCs]))
   )
   ;
   assertz(dictionary([SC]))).
   
% my_optional_semicolon -->
%   ";",
%   {!}.
% my_optional_semicolon -->
%   [].

% SHOW TABLES
my_ISL(show_tables) -->
  my_kw("SHOW"),
  my_sql_blanks,
  my_kw("TABLES").

% SHOW VIEWS
my_ISL(show_views) -->
  my_kw("SHOW"),
  my_sql_blanks,
  my_kw("VIEWS").

% SHOW DATABASES
my_ISL(show_databases) -->
  my_kw("SHOW"),
  my_sql_blanks,
  my_kw("DATABASES").

% DESCRIBE
my_ISL(describe(Name)) -->
  my_kw("DESCRIBE"),
  my_sql_blanks,
  my_sql_user_identifier(Name).



%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing SQL expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Type casting for PostgreSQL:
my_sql_expression(E,Type) -->
  {current_db(_,postgresql)},
  my_sql_expression_aux(E,Type),
  "::",
  my_sql_type(_T).
my_sql_expression(E,Type) -->
  push_syntax_error(['Expected valid SQL expression'],Old),
  my_sql_expression_aux(E,Type),
  pop_syntax_error(Old).

% Non-arithmetic constant expression
my_sql_expression_aux(cte(C,T),T) -->
  my_sql_non_arithmetic_constant(cte(C,T)).
% Arithmetic expression
my_sql_expression_aux(E,_Type) -->
  my_sql_arithmetic_expression(E).
  
my_sql_arithmetic_expression(E) -->
  my_sql_arithmetic_expression(1200,E),
  !.

% SQL Arithmetic expressions

my_sql_arithmetic_expression(PP,To) -->
  my_sql_blanks_star,
  my_sql_factor(L), 
  my_r_sql_arithmetic_expression(PP,0,L/To).
my_sql_arithmetic_expression(PP,To) -->
  "(", 
  my_sql_blanks_star,
  my_sql_arithmetic_expression(1200,T), 
  my_sql_blanks_star,
  ")",
  !, % WARNING
  my_r_sql_arithmetic_expression(PP,0,(T)/To).
% my_sql_arithmetic_expression(_PP,_To) -->
%   "--", % Two minuses are intended for single-line comments, neither two applications of the minus prefix nor one to a negative number
%   {!,
%    fail}.
my_sql_arithmetic_expression(PP,To) -->
  {my_arithmetic_operator(P,FX,SOP,OP),
   prefix(P,FX,PR),
   P=<PP},
  my_string(SOP),
  my_sql_blanks_star, 
  my_sql_arithmetic_expression(PR,T), 
  {NT=..[OP,T]},
  my_r_sql_arithmetic_expression(PP,P,NT/To).
  
my_r_sql_arithmetic_expression(_,_,Ti/Ti) -->
  parse_sql_single_line_remark,
  !.
my_r_sql_arithmetic_expression(PP,Pi,Ti/To) -->
  {my_arithmetic_operator(P,YFX,SOP,OP),
   infix(P,YFX,PL,PR),
   P=<PP,
   Pi=<PL},
  my_sql_blanks_star, 
  my_string(SOP),
  my_sql_blanks_star, 
  my_sql_arithmetic_expression(PR,T), 
  {NT=..[OP,Ti,T]}, 
  my_r_sql_arithmetic_expression(PP,P,NT/To).
% my_r_sql_arithmetic_expression(PP,Pi,Ti/To) --> % No posfix operators yet
%   {my_arithmetic_operator(P,FX,SOP,OP),
%    posfix(P,FX,PL),
%    P=<PP,
%    Pi=<PL,
%    NT=..[OP,Ti]},
%   my_sql_blanks_star, 
%   my_string(SOP),
%   my_r_sql_arithmetic_expression(PP,P,NT/To).
my_r_sql_arithmetic_expression(_,_,Ti/Ti) -->
  [].

my_sql_factor('$NULL'(ID)) -->
  my_kw("NULL"),
  {get_null_id(ID)}.
my_sql_factor(cte(N,number(_T))) -->
  my_number(N),
  !.  % WARNING: This cut is only for improving parsing performance
my_sql_factor(E) --> 
  my_DQL(E),
  !.
my_sql_factor(Aggr) -->
  my_sql_special_aggregate_function(Aggr),
  !.  % WARNING: This cut is only for improving parsing performance
my_sql_factor(T) --> 
  {my_arithmetic_function(SF,F,Arity),
   to_uppercase_char_list(SF,USF)},
  my_kw(USF), 
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  my_sql_function_arguments(Arity,As),
  my_sql_blanks_star,
  ")",
  {!,
   T=..[F|As]}.
my_sql_factor(E) -->
  "(",
  my_sql_blanks_star,
  my_sql_arithmetic_expression(E),
  my_sql_blanks_star,
  ")",
  {!}. % WARNING: This whole clause is only for improving parsing performance
my_sql_factor(C) -->
  my_column(C),
  {C=attr(_,Name,_), \+ arithmetic_constant(_,Name,_)}.
my_sql_factor(C) -->
  my_arithmetic_constant(C).

my_sql_function_arguments(1,[E]) -->
  {!},
  my_sql_arithmetic_expression(E).
my_sql_function_arguments(A,[E|Es]) -->
  {A>1},
  my_sql_arithmetic_expression(E),
  my_sql_blanks_star,
  ",",
  my_sql_blanks_star,
  {A1 is A-1},
  my_sql_function_arguments(A1,Es).

% COUNT(*)
my_sql_special_aggregate_function(count) -->
  my_kw("COUNT"),
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  "*",
  my_sql_blanks_star,
  ")".
% MIN/MAX(DISTINCT Column) behaves as MIN/MAX(Column), as allowed by SQL2 Standard
my_sql_special_aggregate_function(min(C)) -->
  my_kw("MIN"),
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  my_kw("DISTINCT"),
  my_sql_blanks,
  my_column(C),
  my_sql_blanks_star,
  ")".
my_sql_special_aggregate_function(max(C)) -->
  my_kw("MAX"),
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  my_kw("DISTINCT"),
  my_sql_blanks,
  my_column(C),
  my_sql_blanks_star,
  ")".
% Aggr(DISTINCT Column)
my_sql_special_aggregate_function(T) -->
  {my_aggregate_function(_,PF,1),
   atom_concat(F,'_distinct',PF),
   atom_codes(F,SF),
   to_uppercase_char_list(SF,USF)},
  my_kw(USF), 
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  my_kw("DISTINCT"),
  my_sql_blanks,
  my_column(C),
  my_sql_blanks_star,
  ")",
  {T=..[PF,C]}.
  
my_sql_distinct_aggregate_function(min_distinct).
my_sql_distinct_aggregate_function(max_distinct).
my_sql_distinct_aggregate_function(count_distinct).
my_sql_distinct_aggregate_function(avg_distinct).
my_sql_distinct_aggregate_function(sum_distinct).
my_sql_distinct_aggregate_function(times_distinct).

% SQL Separators:

my_sql_blank -->
  parse_multi_line_remark,
  my_blank.
my_sql_blank -->
  parse_sql_single_line_remark,
  my_blank.
my_sql_blank -->
  my_blank.
  
my_sql_blanks -->
  parse_multi_line_remark,
  my_blanks_star.
my_sql_blanks -->
  parse_sql_single_line_remark,
  my_blanks_star.  
my_sql_blanks -->
  my_blanks.
  
my_sql_blanks_star -->
  parse_multi_line_remark,
  my_blanks_star,
  !. % :::WARNING
my_sql_blanks_star -->
  parse_sql_single_line_remark,
  my_blanks_star,
  !. % :::WARNING
my_sql_blanks_star -->
  my_blanks_star.

% An operator with alpha name requires either blanks or parenthesis before and/or after it
% This consumes spaces, but no parentheses
% a->b (a)->(b)
% a implies b, (a)implies(b), not(a), not (a)
my_right_spaced(_) -->
  my_blanks,
  !.
my_right_spaced([C|_]) -->
  {\+ my_alfa(C),
   !}.
my_right_spaced(_,[OP|L],[OP|L]) :- % The opening parenthesis is considered as a void blank separator, as in: not(false)
  [OP]="(".
  
my_left_spaced(_) -->
  my_blanks,
  !.
my_left_spaced([C|_]) -->
  {\+ my_alfa(C),
   !},
  [].
  
my_opening_parentheses_star(N) -->
  my_opening_parentheses_star(0,N).
  
my_opening_parentheses_star(N,NN) -->
  "(",
  my_sql_blanks_star,
  {N1 is N+1},
  my_opening_parentheses_star(N1,NN).
my_opening_parentheses_star(N,N) -->
  [].
  
my_closing_parentheses_star(N) -->
  my_closing_parentheses_star(0,N),
  !. % :::WARNING
  
my_closing_parentheses_star(N,NN) -->
  my_sql_blanks_star,
  ")",
  {N1 is N+1},
  my_closing_parentheses_star(N1,NN).
my_closing_parentheses_star(N,N) -->
  [].
  
my_optional_sql_blanks(N) -->
  {N==0},
  my_sql_blanks,
  {!}.
my_optional_sql_blanks(N) -->
  {N>0},
  my_sql_blanks_star,
  {!}.
  
          
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solve_sql_query(+SQLstr,+SQLst) Solves an SQL query
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_sql_query(QueryStr,Query) :-
  ((current_db('$des')
    ;
    (des_sql_solving(on),
     is_dql_statement(QueryStr,Query)
    )
   )
   ->
    solve_des_sql_query(sql,Query)
   ;
    solve_rdb_sql_query(QueryStr,Query)).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solve_rdb_sql_query(+SQLst) Solves an RDB SQL query
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_rdb_sql_query(QueryStr,Query) :-
  current_db(Connection),
  solve_rdb_sql_query(Connection,QueryStr,Query).

solve_rdb_sql_query(Connection,QueryStr,Query) :-
  is_dql_statement(QueryStr,Query),
  !,
  solve_rdb_dql_query(Connection,QueryStr).
solve_rdb_sql_query(Connection,QueryStr,Query) :-
  is_dml_statement(QueryStr,Query,Type),
  !,
  processC(clear_et,[],_NVs,yes),
  solve_rdb_dml_query(Connection,QueryStr,Type).
solve_rdb_sql_query(Connection,QueryStr,Query) :-
  (is_ddl_statement(QueryStr,Query,Object,Action,SQLst,SuccessMessage)
   ;
   is_isl_statement(QueryStr,Query,Message),
   Object=void,
   Action=display
  ),
  !,
  translate_rdb_trusted_oracle_views(Query,QueryStr,TQueryStr,TSchema),
  object_to_trusted_object(Object,TSchema,TObject),
  solve_rdb_ddl_query(Connection,TQueryStr,TObject,Action,SQLst,SuccessMessage,Message).
  
% solve_rdb_dql_query(QueryStr) :-
%   current_db(Connection),
%   solve_rdb_dql_query(Connection,QueryStr).
  
solve_rdb_dql_query(Connection,QueryStr) :-
  my_odbc_dql_query(Connection,QueryStr,Schema,Rows),
  store_elapsed_time(computation),
  display_rdb_answer_schema(Schema),
  display_solutions(Rows),
  display_elapsed_time,
  write_tapi_eot.

% solve_rdb_dml_query(QueryStr) :-
%   current_db(Connection),
%   solve_rdb_dml_query(Connection,QueryStr).
  
solve_rdb_dml_query(Connection,QueryStr,Type) :-
  my_odbc_dml_query(Connection,QueryStr,NumberOfRows),
  store_elapsed_time(computation),
  display_nbr_of_tuples(NumberOfRows,Type,_Error),
  display_elapsed_time.

% solve_rdb_dql_query(QueryStr) :-
%   current_db(Connection),
%   solve_rdb_dql_query(Connection,QueryStr).
  
solve_rdb_ddl_query(Connection,TQueryStr,TObject,Action,SQLst,SuccessMessage,Message) :-
  object_name(TObject,Name),
  object_type(TObject,Type),
  (((Type==table, table_exists(Name))
    ;
    (Type==view, view_exists(Name)))
   ->
    Existed=true 
   ;
    Existed=false),
%   (relation_exists(Name)
%    ->
%     Existed=true 
%    ;
%     Existed=false),
  my_odbc_ddl_query(Connection,TQueryStr),
  store_elapsed_time(computation),
  rdb_ddl_message(TObject,Action,SuccessMessage,Change,Existed,Message),
  write_info_verb_log([Message,'.']),
  (Change == true
   ->
    update_rdb_pdg_object_action_query(TObject,Action,Existed,SQLst),
    processC(clear_et,[],[],yes) 
   ;
    true),
  display_elapsed_time.
  
% rdb_ddl_message(+Object,+Action,+SuccessMessage,-Change(true or false),+Existed(true or false),-Message)
rdb_ddl_message(_Object,create,Message,true,_,Message) :-
  !.
% rdb_ddl_message(Object,create,Message,true,false,Message) :-
%   once((object_name_arity(Object,Name,Arity) % Can't rely on existing PDG
%         ;
%         object_name(Object,Name)
%       )),
%   pdg((Nodes,_Arcs)),
% %   to_uppercase(Name,UName),
% %   to_uppercase_pred_list(Nodes,UNodes),
%   \+ rdb_pred_memberchk(Name/Arity,Nodes),
%   !.
rdb_ddl_message(Object,create,_Message,false,false,'Table not created') :-
  object_type(Object,table),
  !.
rdb_ddl_message(Object,create,_Message,false,false,'View not created') :-
  object_type(Object,view),
  !.
rdb_ddl_message(Object,drop,_Message,_,false,'Table not dropped') :-
  object_type(Object,table),
  !.
rdb_ddl_message(Object,drop,_Message,_,false,'View not dropped') :-
  object_type(Object,view),
  !.
rdb_ddl_message(Object,drop,Message,true,_,Message) :-
%   pdg((Nodes,_Arcs)), % Can't rely on existing PDG
%   once((object_name_arity(Object,Name,Arity)
%         ;
%         object_name(Object,Name)
%       )),
% %   to_uppercase(Name,UName),
% %   to_uppercase_pred_list(Nodes,UNodes),
% %   memberchk(UName/Arity,UNodes),
%   rdb_pred_memberchk(Name/Arity,Nodes),
  object_name(Object,Name),
  \+ relation_exists(Name),
  !.
rdb_ddl_message(_Object,create_or_replace,CreateMessage,true,Existed,Message) :-
%   once((object_name_arity(Object,Name,Arity)
%         ;
%         object_name(Object,Name)
%       )),
%   pdg((Nodes,_Arcs)),
% %   to_uppercase(Name,UName),
% %   to_uppercase_pred_list(Nodes,UNodes),
% %  (memberchk(UName/Arity,UNodes)
%   (rdb_pred_memberchk(Name/Arity,Nodes)
%    ->
%     atom_concat(M,' created',CreateMessage),
%     atom_concat(M,' replaced',Message),
%     Existed=true
%    ;
%     Message=CreateMessage,
%     Existed=false
%   ),
  (Existed==true
   ->
    atom_concat(M,' created',CreateMessage),
    atom_concat(M,' replaced',Message)
   ;
    Message=CreateMessage
  ),

  !.
rdb_ddl_message(_Object,_Action,Message,true,false,Message).

object_name(table(T),T) :-
  atom(T),
  !.
object_name(table(T/_A),T) :-
  !.
object_name(table(S),T) :-
  !,
  S=..[T|_].
object_name(view(V),V) :-
  atom(V),
  !.
object_name(view(S),T) :-
  S=..[T|_],
  !.
object_name(O,O).
  
object_name_arity(table(T),_,_) :-
  atom(T),
  !,
  fail.
object_name_arity(table(T/A),T,A) :-
  !.
object_name_arity(table(S),TableName,Arity) :-
  S=..[TableName|Args],
  length(Args,Arity).
object_name_arity(view(V),_,_) :-
  atom(V),
  !,
  fail.
object_name_arity(view(S),ViewName,Arity) :-
  S=..[ViewName|Args],
  length(Args,Arity).

  
object_type(table(_),table).
object_type(view(_),view).
object_type(void,void).

% A DQL query: select, union, except, intersect
is_dql_statement(_,(SELECT,_Ren)) :-
  SELECT=..[select|_],
  !.
is_dql_statement(_,(with(_,_),_Ren)).
is_dql_statement(_,(union(_,_,_),_Ren)).
is_dql_statement(_,(except(_,_,_),_Ren)).
is_dql_statement(_,(intersect(_,_,_),_Ren)).
is_dql_statement(QueryStr,unknown) :-
  my_guessed_dql_statement(QueryStr,_Remainder),
  !.
  
% A DML query: insert, delete or update
is_dml_statement(_,insert_into(_,_,_),inserted).
is_dml_statement(_,delete_from(_,_),deleted).
is_dml_statement(_,update(_,_,_),updated).
is_dml_statement(QueryStr,unknown,Message) :-
  my_guessed_dml_statement(Message,QueryStr,_Remainder),
  !.

% A DDL query: create table, rename table, drop table ...
is_ddl_statement(_,create_table(T,_),table(T),create,void,Message) :-
  object_name(table(T),TableName),
  atom_concat_list(['Table ''',TableName,''' created'],Message).
is_ddl_statement(_,create_or_replace_table(T,_),table(T),create_or_replace,void,Message) :-
  object_name(table(T),TableName),
  atom_concat_list(['Table ''',TableName,''' created'],Message).
is_ddl_statement(_,alter_table(T,_,_),table(T),alter,void,Message) :-
  object_name(table(T),TableName),
  atom_concat_list(['Table ''',TableName,''' changed'],Message).
is_ddl_statement(_,rename_table(T,_),table(T),rename,void,Message) :-
  object_name(table(T),TableName),
  atom_concat_list(['Table ''',TableName,''' renamed'],Message).
is_ddl_statement(_,rename_view(V,_),view(V),rename,void,Message) :-
  object_name(view(V),ViewName),
  atom_concat_list(['View ''',ViewName,''' renamed'],Message).
is_ddl_statement(_,create_view(_,(SQLst,_),V),view(V),create,SQLst,Message) :-
  object_name(view(V),ViewName),
  atom_concat_list(['View ''',ViewName,''' created'],Message).
is_ddl_statement(_,create_or_replace_view(_,(SQLst,_),V),view(ViewName),create_or_replace,SQLst,Message) :-
  object_name(view(V),ViewName),
  atom_concat_list(['View ''',ViewName,''' created'],Message).
is_ddl_statement(_,drop_table(T,_),table(T/A),drop,void,Message) :-
  (my_odbc_get_table_arity(T,A) -> true ; true),
  atom_concat_list(['Table ''',T,''' dropped'],Message).
is_ddl_statement(_,drop_view(V,_),view(V),drop,void,Message) :-
  object_name(view(V),ViewName),
  atom_concat_list(['View ''',ViewName,''' dropped'],Message).
is_ddl_statement(_,drop_database,database,drop,void,'Database dropped').
is_ddl_statement(QueryStr,unknown,void,void,void,Message) :-
  my_guessed_ddl_statement(Message,QueryStr,_Remainder),
  !.

% An ISL query: show, describe
is_isl_statement(QueryStr,unknown,Message) :-
  my_guessed_isl_statement(Message,QueryStr,_Remainder),
  !.
  
% Guess whether it is an SQL statement
my_guessed_sql_statement -->
  my_guessed_dql_statement.
my_guessed_sql_statement -->
  my_guessed_dml_statement(_).
my_guessed_sql_statement -->
  my_guessed_isl_statement(_).
my_guessed_sql_statement -->
  my_guessed_sgbd_statement.
my_guessed_sql_statement -->
  my_guessed_ddl_statement(X),
  {
   !,
   X\=='Statement has been executed'
  }.


% Guess whether it is a DQL statement
my_guessed_dql_statement -->
  my_sql_blanks_star,
  my_opening_parenthesis_star,
  my_sql_blanks_star,
  my_kw("SELECT"),
  my_sql_blanks.
my_guessed_dql_statement -->
  my_sql_blanks_star,
  my_opening_parenthesis_star,
  my_sql_blanks_star,
  my_kw("WITH"),
  my_sql_blanks.
my_guessed_dql_statement -->
  my_sql_blanks_star,
  my_opening_parenthesis_star,
  my_sql_blanks_star,
  my_kw("ASSUME"),
  my_sql_blanks.
  
% Guess whether it is a DML statement
my_guessed_dml_statement(inserted) -->
  my_sql_blanks_star,
  my_kw("INSERT"),
  my_sql_blanks.
my_guessed_dml_statement(deleted) -->
  my_sql_blanks_star,
  my_kw("DELETE"),
  my_sql_blanks.
my_guessed_dml_statement(updated) -->
  my_sql_blanks_star,
  my_kw("UPDATE"),
  my_sql_blanks.
  
% Guess whether it is a DDL statement
my_guessed_ddl_statement('Table created') -->
  my_sql_blanks_star,
  my_kw("CREATE"),
  my_sql_blanks,
  my_kw("TABLE"),
  my_sql_blanks.
my_guessed_ddl_statement('Table created') -->
  my_sql_blanks_star,
  my_kw("CREATE"),
  my_sql_blanks,
  my_kw("OR"),
  my_sql_blanks,
  my_kw("REPLACE"),
  my_sql_blanks,
  my_kw("TABLE"),
  my_sql_blanks.
my_guessed_ddl_statement('View created') -->
  my_sql_blanks_star,
  my_kw("CREATE"),
  my_sql_blanks,
  my_kw("VIEW"),
  my_sql_blanks.
my_guessed_ddl_statement('View created') -->
  my_sql_blanks_star,
  my_kw("CREATE"),
  my_sql_blanks,
  my_kw("OR"),
  my_sql_blanks,
  my_kw("REPLACE"),
  my_sql_blanks,
  my_kw("VIEW"),
  my_sql_blanks.
% Guess whether it is a DDL HR-SQL statement
my_guessed_ddl_statement('Relation created') -->
  my_hrsql_typed_schema(_),
  my_sql_blanks_star,
  ":=".
my_guessed_ddl_statement('Database created') -->
  my_sql_blanks_star,
  my_kw("CREATE"),
  my_sql_blanks,
  my_kw("DATABASE"),
  my_sql_blanks.
my_guessed_ddl_statement('Table renamed') -->
  my_sql_blanks_star,
  my_kw("RENAME"),
  my_sql_blanks,
  my_kw("TABLE"),
  my_sql_blanks.
my_guessed_ddl_statement('View renamed') -->
  my_sql_blanks_star,
  my_kw("RENAME"),
  my_sql_blanks,
  my_kw("VIEW"),
  my_sql_blanks.
my_guessed_ddl_statement('Table dropped') -->
  my_sql_blanks_star,
  my_kw("DROP"),
  my_sql_blanks,
  my_kw("TABLE"),
  my_sql_blanks.
my_guessed_ddl_statement('View dropped') -->
  my_sql_blanks_star,
  my_kw("DROP"),
  my_sql_blanks,
  my_kw("VIEW"),
  my_sql_blanks.
my_guessed_ddl_statement('Database dropped') -->
  my_sql_blanks_star,
  my_kw("DROP"),
  my_sql_blanks,
  my_kw("DATABASE"),
  my_sql_blanks_star.
my_guessed_ddl_statement('Table modified') -->
  my_sql_blanks_star,
  my_kw("ALTER"),
  my_sql_blanks,
  my_kw("TABLE"),
  my_sql_blanks.
my_guessed_ddl_statement('Object modified') -->
  my_sql_blanks_star,
  my_kw("ALTER"),
  my_sql_blanks.
my_guessed_ddl_statement('Object dropped') -->
  my_sql_blanks_star,
  my_kw("DROP"),
  my_sql_blanks.
my_guessed_ddl_statement('Object created') -->
  my_sql_blanks_star,
  my_kw("CREATE"),
  my_sql_blanks.
% Other (unknown) statements will be sent without expecting any result set  
my_guessed_ddl_statement('Statement has been executed') -->
  [].

% Guess whether it is a ISL statement
my_guessed_isl_statement('Statement has been executed') -->
  my_sql_blanks_star,
  my_kw("SHOW"),
  my_sql_blanks.
my_guessed_isl_statement('Statement has been executed') -->
  my_sql_blanks_star,
  my_kw("DESCRIBE"),
  my_sql_blanks.

my_guessed_sgbd_statement -->
  my_sql_blanks_star,
  my_kw("USE"),
  my_sql_blanks,
  my_sql_user_identifier(_DB),
  my_sql_blanks_star.

my_opening_parenthesis_star -->
  "(",
  my_sql_blanks_star,
  my_opening_parenthesis_star.
my_opening_parenthesis_star -->
  [].

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Retrieving info with SQL queries and relations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tuple in SQL query
tuple_in_SQL_query(Tuple,Query) :-
  save_et_st(ET,S),
  (solve_des_sql_query_k(sql,(Query,_),_Schema,_ColTypes,_TableRen,DLQuery,_DLsts,_RNVss,_Undefined,_OrderBy),
   Tuple=..[_Relation|Args],
   DLQuery=..[_Answer|Args],
   et(DLQuery,_)
   ->
    restore_et_st(ET,S)
   ;
    restore_et_st(ET,S),
    !,
    fail
  ).
  
% Cardinality of a tuple in the result of an SQL query
tuple_SQL_query_cardinality(Tuple,Query,Cardinality) :-
  get_tuples_from_SQL_query(Query,Tuples),
  Tuple=..[_RelName|Args],
  findall(X,(member(X,Tuples), X=..[_|Args]),Xs),
  length(Xs,Cardinality).

% Tuple in SQL relation. 
% Succeeds if Tuple belongs to Relation
tuple_in_SQL_relation(Tuple,RelationName) :-
  get_tuples_in_relation(RelationName,Tuples),
  my_member_chk(Tuple,Tuples).
  
% Empty SQL answer  
is_empty_sql_answer(Query) :-
  save_et_st(ET,S),
  (solve_des_sql_query_k(sql,(Query,_),_Schema,_ColTypes,_TableRen,DLQuery,_DLsts,_RNVss,_Undefined,_OrderBy),
   \+ et(DLQuery,_)
   ->
    restore_et_st(ET,S)
   ;
    restore_et_st(ET,S),
    !,
    fail
  ).

% Get tuples from SQL query
get_tuples_from_SQL_query(Query,Tuples) :-
  current_db('$des'),
  !,
  save_et_st(ET,S),
  solve_des_sql_query_k(sql,(Query,_),_Schema,ColTypes,_TableRen,DLQuery,_DLsts,_RNVss,_Undefined,_OrderBy),
  length(ColTypes,Arity),
  length(Args,Arity),
  DLQuery=..[answer|Args],
  findall(DLQuery,et(DLQuery,_),Tuples),
  restore_et_st(ET,S).
get_tuples_from_SQL_query(Query,Tuples) :-
  current_db(Connection),
  display_to_string(display_sql(Query,0),QueryStr),
  my_odbc_dql_query(Connection,QueryStr,_Schema,Tuples).

% Answer cardinality for a relation
relation_cardinality(RelationName,Cardinality) :-
  current_db('$des'),
  !,
  get_tuples_in_relation(RelationName,Tuples),
  length(Tuples,Cardinality),
  !.
relation_cardinality(RelationName,Cardinality) :-
  current_db(Connection),
  atom_codes(RelationName,StrRelationName),
  append("SELECT COUNT(*) FROM ",StrRelationName,QueryStr),
  my_odbc_dql_query(Connection,QueryStr,_Schema,[Row]),
  Row=..[_,Cardinality].
  
% Get tuples in a relation as its solutions
get_tuples_in_relation(RelationName,Tuples) :-
  current_db(Connection),
  my_table(Connection,RelationName,Arity),
  get_tuples_in_relation(Connection,RelationName,Arity,Tuples).

get_tuples_in_relation(RelationName,Arity,Tuples) :-
  current_db(Connection),
  get_tuples_in_relation(Connection,RelationName,Arity,Tuples).

get_tuples_in_relation('$des',RelationName,Arity,Tuples) :-
  !,
  functor(Query,RelationName,Arity),
  compute_datalog(Query),
  get_ordered_solutions(Query,Tuples).
get_tuples_in_relation(Connection,RelationName,_Arity,Tuples) :-
  atom_codes(RelationName,RelationNameStr),
  append("SELECT * FROM ",RelationNameStr,QueryStr),
  my_odbc_dql_query(Connection,QueryStr,_Schema,AnswerRows),
  replace_functor_list(answer,RelationName,AnswerRows,UTuples),
  my_sort(UTuples,Tuples).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solve_des_sql_query(+Language,+SQLst) Solves a DES SQL query
% submitted from an SQL or RA statement
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DDL Statements
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% CREATE TABLE TableName
solve_des_sql_query(_Lang,create_table(Schema,Ctrs)) :-
  !,
  allowed_schema(Schema),
  functor(Schema,TableName,_Arity),
  (des_relation_exists(TableName) -> 
   write_error_log(['Relation ''',TableName,''' already defined.'])
   ;
   create_table(Schema,Ctrs),
   store_elapsed_time(computation),
   display_elapsed_time,
   write_tapi_success
  ).
% CREATE OR REPLACE TABLE TableName
solve_des_sql_query(_Lang,create_or_replace_table(Schema,Ctrs)) :-
  !,
  allowed_schema(Schema),
  functor(Schema,TableName,_Arity),
  (des_table_exists(TableName)
   -> 
   drop_table_k_no_warn(TableName)
   ;
   true
  ),
  create_table(Schema,Ctrs),
  store_elapsed_time(computation),
  display_elapsed_time,
  write_tapi_success.
% CREATE TABLE TableName LIKE TableName
solve_des_sql_query(_Lang,create_table_like(TableName,ExistingTableName)) :-
  !,
  exist_table(ExistingTableName,Arity),
  (TableName == ExistingTableName
   ->
   write_warning_log(['Table not created: both table identifiers are the same.'])
   ;
   allowed_tablename(TableName,Arity),
   (des_relation_exists(TableName)
    -> 
     write_error_log(['Relation ''',TableName,''' already defined.'])
    ;
     create_table_like(TableName,ExistingTableName),
     store_elapsed_time(computation),
     display_elapsed_time,
     write_tapi_success
   )
  ).
% CREATE OR REPLACE TABLE TableName LIKE TableName
solve_des_sql_query(_Lang,create_or_replace_table_like(TableName,ExistingTableName)) :-
  !,
  exist_table(ExistingTableName,Arity),
  (TableName == ExistingTableName
   ->
    write_warning_log(['Table not created: both table identifiers are the same.'])
   ;
    allowed_tablename(TableName,Arity),
    (des_relation_exists(TableName)
     -> 
      drop_table_k(TableName)
     ;
      true
    ),
    create_table_like(TableName,ExistingTableName),
    store_elapsed_time(computation),
    display_elapsed_time,
    write_tapi_success
  ).
% CREATE VIEW Schema/ViewName
solve_des_sql_query(_Lang,create_view(Lang,SQLst,Schema)) :-
  !,
  allowed_view_definition(Schema),
  translate_trusted_oracle_views(SQLst,Schema,TSQLst,TSchema),
  TSchema=..[TTableName|_Args],
  (my_table('$des',TTableName,_)
   -> 
    write_error_log(['Relation ''',TTableName,''' already defined.'])
   ;
    create_view(Lang,TSQLst,TSchema,[]),
    store_elapsed_time(computation),
    display_elapsed_time,
    write_tapi_success
  ).
% CREATE OR REPLACE VIEW ViewName
solve_des_sql_query(_Lang,create_or_replace_view(Lang,SQLst,Schema)) :-
  !,
  allowed_view_definition(Schema),
  translate_trusted_oracle_views(SQLst,Schema,TSQLst,TSchema),
  create_or_replace_view(Lang,TSQLst,TSchema),
  store_elapsed_time(computation),
  display_elapsed_time,
  write_tapi_success.
% ALTER TABLE
solve_des_sql_query(_Lang,alter_table(TableName,add,Constraint)) :-
  !,
  save_et_st(ET,S),
  (post_table_constraints(TableName,[Constraint],check,Error), 
   Error\==true
   ->
    write_info_verb_log(['Constraint added.'])
   ;
    write_error_verb_log(['Imposing constraint.'])
  ),
  retract_hyp_programs_k,
  restore_et_st(ET,S).
solve_des_sql_query(_Lang,alter_table(TableName,drop,Constraint)) :-
  !,
  save_et_st(ET,S),
  constraint_ic(Constraint,TableName,ICCtr),
  (drop_ic(ICCtr,_NVs,Error),
   Error\==true
   ->
    true
   ;
    write_error_verb_log(['Dropping constraint.'])
  ),
  retract_hyp_programs_k,
  restore_et_st(ET,S).
% RENAME TABLE TableName TO TableName
solve_des_sql_query(_Lang,rename_table(ExistingTableName,NewTableName)) :-
  !,
  exist_table(ExistingTableName,Arity),
  (NewTableName == ExistingTableName
   ->
   write_warning_log(['Table not renamed: both table identifiers are the same.'])
   ;
   allowed_tablename(NewTableName,Arity),
   (des_relation_exists(NewTableName) -> 
    write_error_log(['Relation ''',NewTableName,''' already defined.'])
    ;
    rename_table(ExistingTableName,NewTableName),
    store_elapsed_time(computation),
    display_elapsed_time,
    compute_stratification,
    write_tapi_success
   )
  ).
% RENAME VIEW Viewname TO Viewname
solve_des_sql_query(_Lang,rename_view(ExistingViewname,NewViewname)) :-
  !,
  exist_view(ExistingViewname,Arity),
  (NewViewname == ExistingViewname
   ->
   write_warning_log(['View not renamed: both view identifiers are the same.'])
   ;
   allowed_viewname(NewViewname,Arity),
   (des_relation_exists(NewViewname) -> 
    write_error_log(['Relation ''',NewViewname,''' already defined.'])
    ;
    rename_view(ExistingViewname,NewViewname),
    store_elapsed_time(computation),
    display_elapsed_time,
    compute_stratification,
    write_tapi_success
   )
  ).
% DROP TABLE TableName
solve_des_sql_query(_Lang,drop_table(TableName,IfExists)) :-
  !,
  %drop_table_if_exists_list([TableName],IfExists,_Dropped).
  drop_table_if_exists(TableName,IfExists,_Dropped).
  
% DROP VIEW ViewName
solve_des_sql_query(_Lang,drop_view(TableName,IfExists)) :-
  !,
  (\+ (my_view('$des',TableName,_A,_SQL,_L,_DL,_ODLIds,_LVs,_StrCs)) -> 
    (IfExists==false
     ->
      write_error_log(['View not defined.']),
      display_view_alternatives(TableName)
     ;
      true
    )
   ;
   drop_view(TableName,Warning),
   store_elapsed_time(computation),
   display_elapsed_time,
   (Warning==true -> true ; write_tapi_success)
  ).

% DROP DATABASE
solve_des_sql_query(_Lang,drop_database) :-
  tapi(on),
  !,
  drop_database,
  write_tapi_success.
solve_des_sql_query(_Lang,drop_database) :-
  !,
  write_info_log(['This will drop all views, tables, constraints and Datalog rules.',nl,'      Do you want to proceed? (y/n) [n]: ','$tbc']),
  user_input_string(Str),
  (Str==[] ->
    write_info_verb_log(['Nothing dropped'])
   ;
    (Str == "y" ->
      reset_elapsed_time,
      store_elapsed_time(parsing),
      drop_database,
      store_elapsed_time(computation),
      display_elapsed_time
     ;
      write_info_verb_log(['Nothing dropped'])
    ) 
  ).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DML Statements
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% INSERT INTO TableName VALUES(...)
solve_des_sql_query(_Lang,insert_into(TableName,Colnames,Ts)) :-
  my_is_list(Ts),
  !,
  ((\+ my_view('$des',TableName,_Arity,_SQLst,_L,_DLs,_ODLIds,_LVDs,_SCs),
       exist_table(TableName,Arity),
       exist_atts(TableName,Colnames)
    ;
       is_persistent_predicate(TableName/Arity))
   ->
    insert_tuple_into_table_list(Ts,TableName,Arity,Colnames)
  ;
   write_error_log(['Cannot insert into views.'])
  ).
  
% INSERT INTO TableName SQLStmt
solve_des_sql_query(Lang,insert_into(TableName,Colnames,SQLst)) :-
  !,
  ((\+ my_view('$des',TableName,_Arity,_SQLst,_L,_DLs,_ODLIds,_LVDs,_SCs),
       exist_table(TableName,Arity),
       exist_atts(TableName,Colnames)
    ;
       is_persistent_predicate(TableName/Arity))
   ->
    current_stratification(S),
    solve_des_sql_query_k(Lang,SQLst,_Schema,ColTypes,_TableRen,_Query,RNVss,CRNVss,_Undefined,_OrderBy),
    display_compiled_sql(RNVss,CRNVss),
    length(Colnames,NbrColumns),
    (length(ColTypes,NbrColumns)
     ->
      insert_tuples(TableName,Arity,NbrColumns,Colnames),
      store_elapsed_time(computation),
      display_statistics,
      display_elapsed_time
     ;
      write_error_log(['Incorrect number of values (must be ',NbrColumns,').'])
    ),
    retract_hyp_programs_k,
    clear_et,
    restore_stratification(S)
   ;
    write_error_log(['Cannot insert into views.'])
  ).

% DELETE FROM
solve_des_sql_query(Lang,delete_from((TableName,Ren),Condition)) :-
  !,
  ((\+ my_view('$des',TableName,_Arity,_SQLst,_L,_DLs,_ODLIds,_LVDs,_SCs),
       exist_table(TableName,Arity)
    ;
       is_persistent_predicate(TableName/Arity))
   ->
    current_stratification(S),
    solve_des_sql_query_k(Lang,(select(all,_TopN,*,from([(TableName,Ren)]),where(Condition),group_by([]),having(true),order_by([],[])),_AS),_Schema,_ColTypes,_TableRen,_Query,RNVss,CRNVss,_Undefined,_OrderBy),
    display_compiled_sql(RNVss,CRNVss),
    delete_tuples(TableName,Arity),
    store_elapsed_time(computation),
    display_statistics,
    display_elapsed_time,
    retract_hyp_programs_k,
    clear_et,
    restore_stratification(S)
   ;
    write_error_log(['Cannot delete from views.'])
  ).
  
% UPDATE 
solve_des_sql_query(Lang,update((TableName,Ren),Assignments,Condition)) :-
  !,
  ((\+ my_view('$des',TableName,_Arity,_SQLst,_L,_DLs,_ODLIds,_LVDs,_SCs),
       exist_table(TableName,Arity)
    ;
       is_persistent_predicate(TableName/Arity))
   ->
    % The following will leave in the ET tuples for the updated table with the following information:
    %   Table name and arity, and the column names and values of tuples that need to be updated: 
    %    answer(OldVal1,...,OldValArity,
    %           Colname1,NewValI1,...,ColnameN,ValIM) : I1,...,IM in {1..Arity}
%     (annotate_types_term('$des',TableName,Condition)
%      ->
    current_stratification(S),
    solve_des_sql_query_k(Lang,(select(all,_TopN,[(TableName,(*))|Assignments],from([(TableName,Ren)]),where(Condition),group_by([]),having(true),order_by([],[])),_AS),_Schema,_ColTypes,_TableRen,_Query,RNVss,CRNVss,_Undefined,_OrderBy),
    display_compiled_sql(RNVss,CRNVss),
    update_tuples(TableName,Arity),
%      ;
%       true
%     ),
    store_elapsed_time(computation),
    display_statistics,
    display_elapsed_time,
    retract_hyp_programs_k,
    clear_et,
    restore_stratification(S)
   ;
    write_error_log(['Cannot update views.'])
  ).
   
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ISL Statements
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SHOW TABLES
solve_des_sql_query(_Lang,show_tables) :-
  !,
  processC(list_tables,[],_NVs,yes).

% SHOW VIEWS
solve_des_sql_query(_Lang,show_views) :-
  !,
  processC(list_views,[],_NVs,yes).

% SHOW DATABASES
solve_des_sql_query(_Lang,show_databases) :-
  !,
  processC(show_dbs,[],_NVs,yes).

% DESCRIBE
solve_des_sql_query(_Lang,describe(Name)) :-
  !,
  processC(dbschema,[Name],_NVs,yes).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DQL Statements
%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_des_sql_query(Lang,SQLst) :-
  save_et_st(ET,S),
  (solve_des_sql_query_k(Lang,SQLst,Schema,ColTypes,TableRen,Query,RNVss,CRNVss,Undefined,OrderBy)
   ->
    store_elapsed_time(computation),
    display_compiled_sql(RNVss,CRNVss),
%    logiql_output(select(RNVss)),
    display_des_answer_schema(Schema,ColTypes,TableRen),
    display_solutions(Query,Undefined,OrderBy),
    display_statistics,
    display_elapsed_time,
    write_tapi_eot,
    retract_hyp_programs_k,
    restore_et_st(ET,S)
   ;
    retract_hyp_programs_k,
    restore_et_st(ET,S),
    !,
    fail
  ).
  
insert_tuple_into_table_list(Ts,TableName,Arity,Colnames) :-
  insert_tuple_into_table_list(Ts,[],TableName,Arity,Colnames,_Error).
  
insert_tuple_into_table_list([],Ts,TableName,Arity,_Colnames,Error) :-
  !,
  insert_tuple_into_table_list_end(Ts,TableName,Arity,Error).
insert_tuple_into_table_list([T|Ts],ITs,TableName,Arity,Colnames,Error) :-
  insert_tuple_into_table(T,TableName,Arity,Colnames,Error),
  var(Error),
  !,
  insert_tuple_into_table_list(Ts,[T|ITs],TableName,Arity,Colnames,Error).
insert_tuple_into_table_list(_Ts,ITs,TableName,Arity,_Colnames,Error) :-
  insert_tuple_into_table_list_end(ITs,TableName,Arity,Error).
  
insert_tuple_into_table_list_end(Ts,TableName,Arity,Error) :-
  ((tapi(off)
    ;
    var(Error))%, logiql(off)
   ->
    display_nbr_of_tuples(Ts,inserted,Error)
   ;
    true),
  functor(G,TableName,Arity),
  (var(Error)
   -> 
    my_idx_retractall(complete_flag(_P,G,_CF,_FCId)),
    compute_stratification_add_fact(TableName/Arity)
   ;
    true % WARNING: Insertions should be redone for atomicity
  ),
  store_elapsed_time(computation),
  (tapi(off)
   ->
    display_elapsed_time
   ;
    true).

insert_tuple_into_table(T,TableName,Arity,Colnames,Error) :-
  my_nf_bagof(Cte,Type^member(cte(Cte,Type),T),CVs),
  build_complete_values(TableName,Arity,Colnames,CVs,Vs,Error),
  (var(Error)
   ->
   (length(Vs,Arity) 
    ->
    Tuple=..[TableName|Vs],
    assert_rule((Tuple,[]),[],sql(TableName),[simplify],_CRNVs,_ODLIds2,_Unsafe,Error)
%    logiql_output(insert_values(Tuple,TableName))
    ;
    write_error_log(['Incorrect number of values (must be ',Arity,').'])
   )
  ;
   true
  ).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Displaying
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Display compiled statements as Datalog clauses
% Stmt = Kind of statement: select, insert, delete, update
% From SQL:
display_compiled_sql(RNVss,CRNVss) :-
%  display_compiled_ra(SQLst),
  display_compiled_language('SQL',RNVss,CRNVss).
  
% From RA:
% display_compiled_ra(RNVss,CRNVss) :-
%   display_compiled_language('RA',RNVss,CRNVss).
% Display RA compiled statements as SQL statements
display_compiled_ra(SQLst) :-
  language(ra),
  show_sql(on),
  !,
  write_info_log(['Equivalent SQL query:']),
  nl_compact_log,
  display_sql(SQLst,0,'$des'),
  nl_compact_log.
display_compiled_ra(_SQLst).

% From DRC:
display_compiled_drc(RNVss,CRNVss) :-
  display_compiled_language('DRC',RNVss,CRNVss).

display_compiled_language(RLanguage,RNVss,CRNVss) :-
  language(DLanguage),
  (DLanguage == datalog -> SLanguage = RLanguage ; SLanguage=DLanguage),
  to_uppercase(SLanguage,Language),
  (show_compilations(on),
   tapi(off)
   ->
    (Language == 'SQL' 
     ->
      Sentence = statement 
     ;
      (Language == 'RA'
       ->
        Sentence = expression
       ;
        ((Language == 'DRC' ; Language == 'TRC')
         ->
          Sentence = statement
         ;
          Sentence = 'UNKNOWN'
        )
      )
    ),
    write_info_log([Language,' ',Sentence,' compiled to:']),
    nl_compact_log,
    (development(off) -> DRNVss=RNVss ; DRNVss=CRNVss),
    display_ruleNVs_list(DRNVss,0),
    nl_compact_log
   ;
    true).
  
% delete_tables used by des_tc.pl
delete_tables([]).
delete_tables([T|Ts]) :-
  solve_des_sql_query(sq,delete_from((T,_Ren),true)),
  delete_tables(Ts).
  
% Display answer schema for ODBC RDB
display_rdb_answer_schema(Schema) :-
  display_answer(on),
  %logiql(off),
  !,
  write_log_list([Schema,' ->',nl]).
display_rdb_answer_schema(_Schema).

% Display answer schema for DES DDB
% No arguments in the schema (propositional table):
display_des_answer_schema([_TableName],_TableRen) :-
  display_answer(on),
  %logiql(off),
  !,
  write_log_list(['answer ->',nl]).
display_des_answer_schema(_,_).
  
% Arguments in the schema (relational table):
display_des_answer_schema([RelRen|Args],ColTypes,TableRen) :-
  display_answer(on),
  %logiql(off),
  !,
  get_answer_schema(answer,RelRen,Args,ColTypes,TableRen,Answer),
  (tapi(off)
   ->
   write_log_list([Answer,' ->',nl])
   ;
   Answer=..[answer|ColNamesColTypes],
   display_tapi_answer_schema(answer,ColNamesColTypes)
   ).
display_des_answer_schema(_,_,_).
  
get_answer_schema(RelationName,RelationRenaming,Args,ColTypes,TableRen,Answer) :-
  get_table_dot_colname_from_proj_schema(RelationRenaming,Args,TableRen,ColNames),
  internal_typename_to_user_typename_list(ColTypes,DColTypes),
  my_zipWith(':',ColNames,DColTypes,ColNamesColTypes),
  Answer=..[RelationName|ColNamesColTypes].
   
internal_schema_to_user_schema(Schema,USchema) :-
  Schema=..[RelName|ColTypeNames],
  my_unzip(ColTypeNames,ColNames,ColTypes),
  internal_typename_to_user_typename_list(ColTypes,DColTypes),
  my_zipWith(':',ColNames,DColTypes,ColNamesColTypes),
  USchema=..[RelName|ColNamesColTypes].

get_table_dot_colname_from_proj_schema(RelationRenaming,Args,TableRen,ColNames) :-
  (development(on) ->
    HArgs=Args
    ;
    hide_nulls(Args,HArgs)),
  internal_colname_to_user_colname_list(HArgs,RelationRenaming,TableRen,ColNames).

display_tapi_answer_schema(Relation,ColNamesColTypes) :-
  write_log_list([Relation,nl]),
  member(ColName:ColType,ColNamesColTypes),
  write_log_list([ColName,nl,ColType,nl]),
  fail.
display_tapi_answer_schema(_Relation,_ColNamesColTypes).  


% displayed_type_names(ColTypes,DColTypes) :-
%   display_types(user),
%   !,
%   internal_typename_to_user_typename_list(ColTypes,DColTypes),
% displayed_type_names(ColTypes,ColTypes).
 

internal_typename_to_user_typename_list([],[]).
internal_typename_to_user_typename_list([T|Ts],[UT|UTs]) :-
  internal_typename_to_user_typename(T,UT),
  internal_typename_to_user_typename_list(Ts,UTs).

internal_typename_to_user_typename(string(varchar),string)
  :- !.
internal_typename_to_user_typename(string(String),String).
internal_typename_to_user_typename(number(integer),int) :-
  !.
internal_typename_to_user_typename(number(Number),Number) :-
  !.
internal_typename_to_user_typename(T,T).

internal_colname_to_user_colname_list([],_RelRen,_TableRen,[]).
internal_colname_to_user_colname_list([IColName|IColNames],RelRen,TableRen,[ColName|ColNames]) :-
  internal_colname_to_user_colname(IColName,RelRen,TableRen,ColName),
  internal_colname_to_user_colname_list(IColNames,RelRen,TableRen,ColNames).
  
internal_colname_to_user_colname(expr(expr_ref(_Rel,A),AS,_Type),_RelRen,_TableRen,C) :-
  visible_column_name(A,AS,C),
  !.
internal_colname_to_user_colname(expr(attr(_RT,C,_R),AS,_Type),_RelRen,_TableRen,AS) :-
  var(C),
  !.
internal_colname_to_user_colname(expr(attr(RT,C,R),_AS,_Type),RelRen,TableRen,ColName) :-
  !,
  internal_colname_to_user_colname(attr(RT,C,R),RelRen,TableRen,ColName).
internal_colname_to_user_colname(expr(_E,AS,_Type),_RelRen,_TableRen,AS).
internal_colname_to_user_colname(attr(_RT,C,R),_RelRen,_TableRen,R) :-
  var(C),
  !.
internal_colname_to_user_colname(attr(RT,A,AS),RelRen,TableRen,ColName) :-
  find_table_name(RT,TableRen,TableRen,T),
  setof((T,RT),member((T,RT),TableRen),[(T,RT)]), % Unambiguous only-one occurrence of table T in the renaming
  !,
  visible_column_name(A,AS,C),
  visible_relation_name(RT,RelRen,T,VT),
  ((\+ is_system_identifier(AS),
    is_system_identifier(RT))
   ->
    ColName=C
   ;
    atomic_concat_list([VT,'.',C],ColName)).
internal_colname_to_user_colname(attr(RT,A,AS),RelRen,TableRen,ColName) :-
  find_table_name(RT,TableRen,TableRen,T),
  !,
  visible_column_name(A,AS,C),
  visible_relation_name(RT,RelRen,T,VT),
  ((atom_concat('$t',_N,VT)
    ;
    \+ is_system_identifier(AS))
   ->
    ColName=C
   ;
    atom_concat('$t',N,RT),
    atomic_concat_list([T,'_',N,'.',C],ColName)
  ).
internal_colname_to_user_colname(attr(T,A,AS),RelRen,_TableRen,ColName) :- % Lost renamings
  visible_column_name(A,AS,C),
  visible_relation_name(T,RelRen,RelRen,VT),
  ((atom_concat('$t',_N,VT)
    ;
    (\+ is_system_identifier(AS),
     is_system_identifier(T),
     is_system_identifier(RelRen)
     ))
   ->
   ColName=C
  ;
   atomic_concat_list([VT,'.',C],ColName)
  ).  
 
% Returns the visible relation name from a relation name, its renaming and the its source relation
visible_relation_name(A,AS,O,VA) :-
  select_non_system_identifier(A,AS,R),
  select_non_system_identifier(O,R,VA).

% Attribute visible qualified name
visible_qualified_attr(attr(Rel,A,AS),V) :-
  visible_attr_name(attr(Rel,A,AS),VA),
  ((var(Rel);is_system_identifier(Rel))
   ->
    V=VA
   ;
    atomic_concat_list([Rel,'.',VA],V)
  ).

% Attribute visible name
visible_attr_name(attr(_,A,AS),VA) :-
  visible_column_name(A,AS,VA).
    
% Returns the visible column name from a column name and its renaming 
visible_column_name(A,AS,A) :-
  var(AS),
  !.
visible_column_name(A,AS,VA) :-
  select_non_system_identifier(A,AS,VA).
  
select_non_system_identifier(A,AS,A) :-
  is_system_identifier(AS),
  !.
select_non_system_identifier(_A,AS,AS).

find_table_name(RT,[(T,RT)|_RTTs],_TableRen,T) :-
  \+ (atom_concat('$',_,T)),
  !.
find_table_name(RT,[(RT,RT1)|_RTTs],TableRen,T) :-
  atom_concat('$',_,RT),
  find_table_name(RT1,TableRen,TableRen,T).
find_table_name(RT,[(RT2,_RT1)|RTTs],TableRen,T) :-
  RT \= RT2,
  find_table_name(RT,RTTs,TableRen,T).
  
% Solving SQL queries, untouching ET
% WITH
% Regular local view: 
% - Do not store schema 
% - Create prototype view
% - Create view schema (prototype view is dropped)
% - Compute view
% - Drop view
% Hypothetical local view with the name of an existing TABLE: 
% - Store the table schema 
% - Create prototype view (local view with table schema is dropped)
% - Create view schema (prototype view is dropped)
% - Compute view
% - Drop view
% - Restore table schema
% Hypothetical local view with the name of an existing VIEW: 
% - Store the view schema (...,my_view). It will not be removed up to the final drop view
% - Prototype local view is not created
% - View schema is created
% - Compute view
% - Drop view
% - Restore view schema
% solve_des_sql_query_k(Lang,(with(SQLst,SQLsts),_AS),Schema,ColTypes,TableRen,Query,DLsts,Undefined,OrderBy) :-
% %  !,
% %  processC(list_persistent,[],[],yes),
%   current_stratification((CurrentPDG,CurrentStrata)),
%   get_persistent_assertions(Assertions),
%   % Create view prototype (if hypothetical, do nothing as it already exists)
%   create_prototype_view_list(SQLsts,NewViewNames,OldSchemas),
%   (create_or_replace_view_list_k(Lang,SQLsts,NewViewNames) ->
%   (catch(
%     solve_des_sql_query_k(Lang,SQLst,Schema,ColTypes,TableRen,Query,DLsts,Undefined,OrderBy),
%     Message,
%     (
%      !,
%      drop_view_k_list(SQLsts,no_warn),
%      assertz_list(OldSchemas),
%      throw(Message)
%     )
%         ),
%    drop_view_k_list(SQLsts,no_warn),
% %   drop_viewname_k_list(NewViewNames,no_warn),
%    assertz_list(OldSchemas)
%    )
%   ;
%    drop_view_k_list(SQLsts,no_warn),
%    assertz_list(OldSchemas),
%    !,
%    throw(des_exception(''))
%   ),
%   % WARNING: 
%   restore_persistent_assertions(Assertions),
%   restore_stratification((CurrentPDG,CurrentStrata)).

% solve_des_sql_query_k(Lang,(with(SQLst,SQLsts),AS),Schema,ColTypes,TableRen,Query,DVs,RNVss,Undefined,OrderBy) :-
%   !,
%   create_prototype_view_list(SQLsts,LocalViews,OldSchemas),
%   solve_des_sql_query_k1(Lang,(with(SQLst,SQLsts),AS),Schema,ColTypes,TableRen,Query,DVs,RNVss,Undefined,OrderBy),
%   drop_schema_list('$des',LocalViews),
%   assertz_list(OldSchemas).
% solve_des_sql_query_k(Lang,SQLst,Schema,ColTypes,TableRen,Query,DVs,RNVss,Undefined,OrderBy) :-
%   solve_des_sql_query_k1(Lang,SQLst,Schema,ColTypes,TableRen,Query,DVs,RNVss,Undefined,OrderBy).

solve_des_sql_query_k(Lang,SQLst,Schema,ColTypes,TableRen,Query,DVs,RNVss,Undefined,OrderBy) :-
%  !,
  check_complete_schemas_in_with(SQLst),
  local_view_definitions(SQLst,SQLsts),
  create_prototype_view_list(SQLsts,LocalViews,OldSchemas),
  solve_des_sql_query_k1(Lang,SQLst,Schema,ColTypes,TableRen,Query,DVs,RNVss,Undefined,OrderBy),
  drop_schema_list('$des',LocalViews),
  assertz_list(OldSchemas),
  !.

local_view_definitions(SQLst,SQLsts) :-
  my_nf_setof(
    (LSQLst,LV),
    WSQLst^WSQLsts^AS^
      (my_member_term((with(WSQLst,WSQLsts),AS),SQLst),
       member((LSQLst,LV),WSQLsts),
       nonvar(LV),
       \+ atom(LV) % Must be a complete schema, not only the name
       ),
    SQLsts).

% SELECT, ...
%solve_des_sql_query_k1(+Lang,+SQLst,-Schema,-ColTypes,-TableRen,-Query,-DVs,-RNVss,-Undefined,-OrderBy)
solve_des_sql_query_k1(Lang,SQLst,Schema,ColTypes,TableRen,Query,DVs,RNVss,Undefined,OrderBy) :-
  sql_to_dl(Lang,SQLst,Schema,TableRen,UDLsts),
  solve_des_sql_query_k2(Lang,UDLsts,[],ColTypes,Query,DVs,RNVss,Undefined,OrderBy).
  
%solve_des_sql_query_k2(+Lang,+UDLsts,+NVs,-ColTypes,-Query,-DVs,-RNVss,-Undefined,-OrderBy)
solve_des_sql_query_k2(Lang,UDLsts,NVs,ColTypes,Query,DVs,RNVss,Undefined,OrderBy) :-
  once((UDLsts=[':-'(Head,_)|_] ; UDLsts=[Head|_])),
  functor(Head,Pred,Arity),
  replace_predicate_names_and_assert(Pred,Arity,answer,UDLsts,NVs,_DLsts,DVs,RNVss,ODLIds,Unsafe,Error),
%  ruleNVs_to_rule_list(CRNVs,DLsts),
%  DLsts = [Rule|_],
  RNVss = [(Rule,_)|_],
  retractall(last_autoview(_)),
  assertz(last_autoview(Rule)),
  clear_et, 
  update_stratification_add_ruleNVs(RNVss),
  (var(Error), 
   is_setvar_safe(Unsafe), 
   is_rc_safe(Lang,Unsafe)
   ->
    ruleNVs_to_rule_list(DVs,Rs),
    rule_to_open_head_rule_list(Rs,ORs),
    (infer_types_rule_list(ORs,InferredTypes,_ITypedArgs,_ExtraTypes)
     ->
      swap_uct_ict(UTypes,InferredTypes),
      close_types(UTypes,ColTypes),
      functor(Query,answer,Arity),
      order_by_query(Query,OrderBy),
      catch(solve_datalog_query(Query,[],[],Undefined),
            Message,
            (
             !,
             retract_rule_by_id_list(ODLIds,_),
             throw(Message)
            ))
      ;
      retract_rule_by_id_list(ODLIds,_),
      throw(des_exception(''))
      )
   ;
%    true
    retract_rule_by_id_list(ODLIds,_),
    throw(des_exception(''))
   ),
  %retract_source_dlrules_list(DVs).
  retract_rule_by_id_list(ODLIds,_Error2).
  
% Views for DRC and TRC should be safe to be created
is_rc_safe(Lang,Unsafe) :-
  \+ (nonvar(Unsafe), (Lang==drc ; Lang==trc)).

% retract_source_dlrules_list(DVs) :-
%   get_object_dlrules_list(DVs,SDVs),
%   retract_dlrule_list(SDVs,_Error).

rule_to_open_head_rule_list([],[]).
rule_to_open_head_rule_list([R|Rs],[OR|ORs]) :-
  rule_to_open_head_rule(R,OR),
  rule_to_open_head_rule_list(Rs,ORs).
  
rule_to_open_head_rule(R,OR) :-
  (R=(H:-B) ; R=H, B=true),
  !,
  open_head(H,OH,Eqs),
  (Eqs==[]
   ->
    RB=B
   ;
    my_list_to_tuple(Eqs,G),
    append_goals(G,B,RB)),
  (RB==true -> OR = H ; OR = (OH:-RB)).
  
open_head(H,OH,Eqs) :-
  H=..[F|Args],
  open_head_args(Args,RArgs,[],Eqs),
  OH=..[F|RArgs].
  
open_head_args([],[],Eqs,Eqs).
open_head_args([Arg|Args],[Arg|RArgs],IEqs,OEqs) :-
  var(Arg),
  !,
  open_head_args(Args,RArgs,IEqs,OEqs).
open_head_args([Arg|Args],[Var|RArgs],IEqs,[Var=Arg|OEqs]) :-
  open_head_args(Args,RArgs,IEqs,OEqs).
  
%%%%%%%%%%%%%%%%%
% Create table
%%%%%%%%%%%%%%%%%
create_table(Schema,Ctrs) :-
%  save_et(ET),
%   current_stratification(S),
  Schema =.. [TableName|Args],
  assert_table_schema(TableName,Args),
  length(Args,Arity),
  (no_tuples_in_ddb(TableName,Arity) -> Check=no_check ; true),
  (post_table_constraints(TableName,Ctrs,Check,Error), 
   !,
   Error\==true
   ->
    write_info_verb_log(['Table created.'])
   ;
%    drop_table_k(TableName),
    drop_schema('$des',TableName),
    drop_constraints(TableName,Ctrs)
%    write_error_log(['Imposing constraints.'])
  ),
  retract_hyp_programs_k,
  compute_stratification.
%  restore_et(ET),
%  restore_stratification(S).

assert_table_schema(TableName,ColnameTypes) :-
  length(ColnameTypes,Arity),
  assertz(my_table('$des',TableName,Arity)),
  assert_attr_types(TableName,ColnameTypes).
%  logiql_output(create_table(TableName,ColnameTypes)).

post_table_constraints(_TableName,[],_Check,_Error).   
post_table_constraints(_TableName,_Ctrs,_Check,Error) :-
  Error==true,
  !.
post_table_constraints(TableName,[Ctr|Ctrs],Check,Error) :-
  post_table_constraint(TableName,Ctr,Check,Error),
  post_table_constraints(TableName,Ctrs,Check,Error).   
  
post_table_constraint(_TableName,true,_Check,_Error).  
post_table_constraint(_TableName,default(_,_),_Check,_Error).  
%
% Not nullables
%
post_table_constraint(TableName,not_nullables(Atts),Check,_Error) :-
  exist_atts(TableName,Atts),
%  nn_consistent(TableName,Atts),
  ((Check==no_check
   ;
    check_ctr(my_not_nullables('$des',TableName,Atts))) 
   ->
    (retract(my_not_nullables('$des',TableName,CAtts))
     ->
      my_set_union(CAtts,Atts,NAtts)
     ;
      NAtts=Atts),
    sort_columns_by_relation_def(TableName,NAtts,OAtts), 
    assertz(my_not_nullables('$des',TableName,OAtts))
   ;
    tapi(off),
    write_info_log(['Constraint has not been asserted.']),
    fail
  ).
post_table_constraint(_TableName,not_nullables(_Atts),_Check,true).
%
% Primary key
%
post_table_constraint(TableName,primary_key(Atts),Check,_Error) :-
  exist_atts(TableName,Atts),
%  pk_consistent(TableName,Atts),
  sort_columns_by_relation_def(TableName,Atts,OAtts), 
  ((Check==no_check ; check_ctr(my_primary_key('$des',TableName,OAtts))) ->
    my_retract_all_facts(my_primary_key('$des',TableName,_)),
    assertz(my_primary_key('$des',TableName,OAtts))
   ;
    tapi(off),
    write_info_log(['Constraint has not been asserted.']),
    fail
  ).
post_table_constraint(_TableName,primary_key(_Atts),_Check,true).
%
% Candidate key
%
post_table_constraint(TableName,candidate_key(Atts),Check,_Error) :-
  exist_atts(TableName,Atts),
  sort_columns_by_relation_def(TableName,Atts,OAtts), 
%  ck_consistent(TableName,OAtts),
  ((Check==no_check ; check_ctr(my_candidate_key('$des',TableName,OAtts)),
   \+ my_candidate_key('$des',TableName,OAtts)) ->
    assertz(my_candidate_key('$des',TableName,OAtts))
   ;
    tapi(off),
    write_info_log(['Constraint has not been asserted.']),
    fail
  ).
post_table_constraint(_TableName,candidate_key(_Atts),_Check,true).
%
% Foreign key
%
post_table_constraint(TableName,foreign_key(Atts,FTableName,FAtts),Check,_Error) :-
  (my_table('$des',FTableName,_Arity)
   ->
    true
   ;
%    write_error_log(['Table ''',FTableName,''' is unknown.']),
    write_exception_message(unknown_table(FTableName),syntax(_Message),_R_V),
    fail),
  (\+ (my_view('$des',FTableName,_A,_S,_La,_D,_ODLIds,_L,_SC))
   ->
    true
   ;
    write_error_log(['Referenced object ''',FTableName,''' is a view, not a table.']),
    fail),
  (TableName\==FTableName
   ->
    true
   ;
    write_error_log(['Autoreference for ''',TableName,''' is not allowed.']),
    fail),
  exist_atts(TableName,Atts),
  remove_duplicates(Atts,RAtts),
  length(Atts,L),
  (length(RAtts,L)
   ->
    true
   ;
    write_error_log(['Duplicated columns in column list ''',TableName,'''.',Atts,'.']),
    fail),
  exist_atts(FTableName,FAtts),
  remove_duplicates(FAtts,RFAtts),
  length(FAtts,FL),
  (length(RFAtts,FL)
   ->
    true
   ;
    write_error_log(['Duplicated columns in referenced column list ''',FTableName,'''.',FAtts,'.']),
    fail),
  (L==FL 
   ->
    true
   ;
    write_error_log(['Different number of arguments in referenced colummn list ''',FTableName,'''.',FAtts,'.']),
   fail),
  same_type_atts(TableName,Atts,FTableName,FAtts),
%  sort_columns_by_relation_def(TableName,FAtts,OFAtts), 
  my_sort(FAtts,OFAtts), 
  (my_primary_key('$des',FTableName,OFAtts)
   ->
    true
   ;
    write_error_log(['Referenced column list ''',FTableName,'''.',FAtts,' is not a primary key.']),
    fail),
  sort_columns_by_relation_def(TableName,Atts,OAtts), 
  (my_foreign_key('$des',TableName,OAtts,FTableName,OFAtts,_RIds)
   ->
    write_error_log(['Trying to reassert an existing integrity constraint.']),
    fail
   ;
    true
  ),
  ((Check==no_check ; check_ctr(my_foreign_key('$des',TableName,Atts,FTableName,FAtts,_)))
   ->
    assert_limited_domain_rulesNVs(my_foreign_key('$des',TableName,Atts,FTableName,FAtts,RIds)),
    assertz(my_foreign_key('$des',TableName,Atts,FTableName,FAtts,RIds)),
    my_table('$des',TableName,Arity),
    (become_limited_domain_predicate(TableName/Arity) % If the predicate becomes domain limited, recompute the PDG and strata
     ->
      write_info_verb_log(['Predicate ',TableName/Arity,' becomes limited domain.']),
      limited_domain_predicates(Ps),
      set_flag(limited_domain_predicates([TableName/Arity|Ps])),
      compute_stratification_silent
     ;
      true
    )
   ;
    tapi(off),
    write_info_log(['Constraint has not been asserted.']),
    fail
  ).
post_table_constraint(_TableName,foreign_key(_Atts,_FTableName,_FAtts),_Check,true).  
%
% Functional dependency
%
post_table_constraint(TableName,fd(Atts,DepAtts),Check,_Error) :-
  (\+ my_table('$des',TableName,_Arity)
   ->
    write_error_log(['Relation ''',TableName,''' has not been typed yet.']),
    fail
   ;
    true),
  (my_set_diff(DepAtts,Atts,[]) ->
    write_error_log(['Trivial functional dependency.']),
    fail
   ;
    true),
  exist_atts(TableName,Atts),
  exist_atts(TableName,DepAtts),
  sort_columns_by_relation_def(TableName,Atts,OAtts), 
  sort_columns_by_relation_def(TableName,DepAtts,ODepAtts), 
  (my_functional_dependency('$des',TableName,OAtts,ODepAtts) ->
    write_error_log(['Trying to reassert an existing integrity constraint.']),
    fail
   ;
    true),
  ((Check==no_check ; check_ctr(my_functional_dependency('$des',TableName,OAtts,ODepAtts))) ->
    assertz(my_functional_dependency('$des',TableName,OAtts,ODepAtts))
   ;
    tapi(off),
    write_info_log(['Constraint has not been asserted.']),
    fail
  ).
post_table_constraint(_TableName,fd(_Atts,_DepAtts),_Check,true).
  
 %%%%
% process_predef_constraint(fd(Pred,Columns,DepColumns),_NVs,_Ls,_Fid,Error) :-
%   (my_table('$des',Pred,_Arity)
%    ->
%     (my_set_diff(DepColumns,Columns,[]) ->
%       write_warning_log(['Trivial functional dependency. Not asserted.']),
%       Error=true
%      ;
%       post_table_constraint(Pred,fd(Columns,DepColumns),check,Error),
%       (var(Error), verbose(on) ->
%         write_info_log(['Functional dependency integrity constraint successfully imposed.',nl,'      Resulting schema: ']),
%         list_schema(Pred),
%         nl_compact_log
%        ;
%         true
%       )
%     )
%   ;
%    write_error_log(['Relation ',Pred,' has not been typed yet.'])
%   ).
 %%%% 
%
% User-defined integrity constraints
%
post_table_constraint(TableName,my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,SQL,OwnerTableName),Check,Error) :-
  post_table_constraint_assert(TableName,my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,SQL,OwnerTableName),Check,assert,Error).
post_table_constraint(_TableName,my_integrity_constraint('$des',_Preds,_Constraint,_NVs,_Head,_Ids,_SQL,_OwnerTableName),_Check,true).
%
% SQL CHECK integrity constraints
%
post_table_constraint(TableName,my_sql_check_constraint(SQLCondition),Check,Error) :-
  catch(
    sql_to_dl(sql,(select(all,top(all),*,from([(TableName,_TNS)]),where(not(SQLCondition)),group_by([]),having(true),order_by([],[])),_AS),_Schema,_TableRen,DLs),
    _Message,
    (Error=true,
     fail)),
  DLs=[DL|_],
  DL=':-'(UHead,_Body),
  assign_variable_names(DL,NVs),
  functor(UHead,Pred,Arity),
  atom_concat('ic_',TableName,ICName),
  get_new_predicate_name(ICName,IC),
  replace_predicate_names_and_assert(Pred,Arity,IC,DLs,[],RDLs,_DVs,_CRNVs,Ids,_Unsafe,_Error1),
  reachable_user_predicates_rule_list(DLs,Preds),
  RDLs=[RDL|_],
  RDL=':-'(Head,Body),
  post_table_constraint_assert(TableName,my_integrity_constraint('$des',Preds,Body,NVs,Head,Ids,SQLCondition,TableName),Check,no_assert,_Error),
  !.
post_table_constraint(_TableName,my_sql_check_constraint(_SQLCondition),_Check,true).

post_table_constraint_assert(_TableName,my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,SQL,TableName),Check,Assert,_Error) :-
  (my_integrity_constraint('$des',Preds,Constraint,NVs,_,_,_,_) ->
    write_error_log(['Trying to reassert an existing integrity constraint.']),
    fail
   ;
    true
  ),
  (Assert==no_assert
   ->
    true
   ;
    push_flag(check_ic,off,CheckStatus),
    assert_rule((':-'(Head,Constraint),NVs),[],datalog,_,_,Ids,_,_),
    pop_flag(check_ic,CheckStatus)
  ),
  compute_stratification,
  ((Check==no_check ; check_ctr(my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,SQL,TableName))) ->
    assertz(my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,SQL,TableName)),
    write_info_verb_log(['User-defined integrity constraint successfully imposed.'])
   ;
    (retract_source_rule(':-'(Head,Constraint),_ErrorRetract) -> true ; true),
    tapi(off),
    write_info_log(['Constraint has not been asserted.']),
    fail
  ).

%%%%%%%%%%%%%%%%%
% Rename table
%%%%%%%%%%%%%%%%%

rename_table(TableName,NewTableName) :-
% WARNING: Existing tuples are not checked for consistency
  my_table('$des',TableName,Arity),
  % Change view definitions involving the old table name
  rename_goals_in_view_definitions(TableName/Arity,NewTableName),
  % Change name
  my_retract_all_facts(my_table('$des',TableName,Arity)),
  assertz(my_table('$des',NewTableName,Arity)),
  % Change attributes
  (
   retract(my_attribute('$des',Pos,TableName,Att,Type)),
   assertz(my_attribute('$des',Pos,NewTableName,Att,Type)),
   fail
  ;
   true
  ),
  % Change predefined integrity constraints:
  % - Not nullables
  (
   retract(my_not_nullables('$des',TableName,NNAtts)),
   assertz(my_not_nullables('$des',NewTableName,NNAtts)),
   fail
  ;
   true
  ),
  % - Primary key
  (
   retract(my_primary_key('$des',TableName,PKAtts)),
   assertz(my_primary_key('$des',NewTableName,PKAtts)),
   fail
  ;
   true
  ),
  % - Candidate keys
  (
   retract(my_candidate_key('$des',TableName,CKAtts)),
   assertz(my_candidate_key('$des',NewTableName,CKAtts)),
   fail
  ;
   true
  ),
  % - Foreign keys
  (
   retract(my_foreign_key('$des',TableName,PAtts,FKTableName,FKAtts,RIds)),
   replace_functor_RNVs_by_RId_list(TableName,NewTableName,RIds),
   assertz(my_foreign_key('$des',NewTableName,PAtts,FKTableName,FKAtts,RIds)),
   fail
  ;
   true
  ),
  % - Referenced by other table's foreign keys
  (
   retract(my_foreign_key('$des',OtherTableName,PAtts,TableName,FKAtts,RIds)),
   replace_functor_RNVs_by_RId_list(TableName,NewTableName,RIds),
   assertz(my_foreign_key('$des',OtherTableName,PAtts,NewTableName,FKAtts,RIds)),
   fail
  ;
   true
  ),
  % - Functional dependency
  (
   retract(my_functional_dependency('$des',TableName,AttNames,DepAttNames)),
   assertz(my_functional_dependency('$des',NewTableName,AttNames,DepAttNames)),
   fail
  ;
   true
  ),
  % Change user-defined integrity constraints
  (
   my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,SQL,TableName),
   my_member_chk(TableName/Arity,Preds),
   replace_functor_dlrules_from(head,Head,TableName,NewTableName),
   replace_functor(TableName,NewTableName,Preds,NewPreds),
   replace_functor(TableName,NewTableName,Constraint,NewConstraint),
   retract(my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,SQL,TableName)),
   assertz(my_integrity_constraint('$des',NewPreds,NewConstraint,NVs,Head,Ids,SQL,TableName)),
   fail
  ;
   true
  ), 
  % Change tuples
  (
   length(Args,Arity),
   Fact=..[TableName|Args],
   datalog(Fact,NVs,RId,CId,Ls,FId,Rs),
   NewFact=..[NewTableName|Args],
   functor(NewFact,NewTableName,Arity),
   retract(datalog(Fact,NVs,RId,CId,Ls,FId,Rs)),
   assertz(datalog(NewFact,NVs,RId,CId,Ls,FId,Rs)),
   fail
  ;
   true
  ).
  
% Rename goals in bodies of view definitions that include calls to RelationName/Arity, where TableName is
% being changed to NewTableName
rename_goals_in_view_definitions(RelationName/Arity,NewRelationName) :-
  dependent_relations(RelationName/Arity,Preds),
  replace_functor_dlrules_from_list(namearity,Preds,RelationName,NewRelationName),
  change_dependent_relations_in_view_list(Preds,RelationName,NewRelationName).
  
change_dependent_relations_in_view_list([],_RelationName,_NewRelationName).
change_dependent_relations_in_view_list([Viewname/Arity|Preds],RelationName,NewRelationName) :-
  change_dependent_relations_in_view(Viewname/Arity,RelationName,NewRelationName),
  change_dependent_relations_in_view_list(Preds,RelationName,NewRelationName).

change_dependent_relations_in_view(Viewname/Arity,RelationName,NewRelationName) :-
  retract(my_view('$des',Viewname,Arity,SQL,Lang,DL,ODLIds,LVs,StrCs)),
  !,
  replace_functor(RelationName,NewRelationName,SQL,NewSQL),
  replace_functor(RelationName,NewRelationName,DL,NewDL),
  assertz(my_view('$des',Viewname,Arity,NewSQL,Lang,NewDL,ODLIds,LVs,StrCs)).
change_dependent_relations_in_view(_Viewname/_Arity,_RelationName,_NewRelationName).

% Get tables/relations relations that directly depend on predicate RelationName/Arity, i.e., there exist an arc in the pdg connecting them
dependent_relations(RelationName/Arity,Preds) :-
  !,
  get_pdg((_Nodes,Arcs)),
  findall(Pred,
          (member(Arc,Arcs),
           from_to_arc(Arc,RelationName/Arity,Pred),
           Pred=(N/A),
           functor(G,N,A),
           once((datalog(G,_,_,_,_,_,_) ; datalog(':-'(G,_),_,_,_,_,_,_)))
           ),
          UPreds),
  my_mergesort(UPreds,Preds).
dependent_relations(RelationName,RelationNames) :-
  !,
  dependent_relations(RelationName/_Arity,Preds),
  my_unzip(Preds,RelationNames,_Arities).

% Get tables/relations with foreign keys referencing the given RelationName/Arity
referenced_relations(RelationName/Arity,Preds) :-
  !,
  findall(OtherTableName/OtherArity,
          (my_foreign_key('$des',OtherTableName,_PAtts,RelationName,_FKAtts,_RIds),
           my_table('$des',RelationName,Arity),
           my_table('$des',OtherTableName,OtherArity)),
          UPreds),
  my_mergesort(UPreds,Preds).
referenced_relations(RelationName,RelationNames) :-
  !,
  findall(OtherTableName,
          my_foreign_key('$des',OtherTableName,_PAtts,RelationName,_FKAtts,_RIds),
          URelationNames),
  my_mergesort(URelationNames,RelationNames).

% Sorts columns by their occurrence in relation definition
% If undefined, return them as input
sort_columns_by_relation_def(Relation,Columns,OColumns) :-
  get_att_positions(Relation,Columns,Positions),
  my_zipWith(',',Positions,Columns,PCs),
  my_remove_duplicates_sort(PCs,OPCs),
  my_unzip(OPCs,_,OColumns),
  !.
sort_columns_by_relation_def(_Relation,Columns,Columns).

  
%%%%%%%%%%%%%%%%%
% Rename view
%%%%%%%%%%%%%%%%%

rename_view(Viewname,NewViewname) :-
  % Change table name
  my_retract_all_facts(my_table('$des',Viewname,Arity)),
  assertz(my_table('$des',NewViewname,Arity)),
  % Change attributes
  (
   retract(my_attribute('$des',Pos,Viewname,Att,Type)),
   assertz(my_attribute('$des',Pos,NewViewname,Att,Type)),
   fail
  ;
   true
  ),
  % Change view
  retract(my_view('$des',Viewname,Arity,SQL,Lang,DL,ODLIds,LVs,StrCs)),
  replace_functor(Viewname,NewViewname,SQL,NewSQL),
  replace_functor(Viewname,NewViewname,DL,NewDL),
  assertz(my_view('$des',NewViewname,Arity,NewSQL,Lang,NewDL,ODLIds,LVs,StrCs)),
%  replace_functor_dlrules_from_list(name,[Viewname|LVs],Viewname,NewViewname),
  replace_functor_dlrules_from(name,Viewname,Viewname,NewViewname),
  % Change view definitions involving the old view name
  rename_goals_in_view_definitions(Viewname/Arity,NewViewname).
  

%%%%%%%%%%%%%%%%%%%%
% Create table LIKE
%%%%%%%%%%%%%%%%%%%%

create_table_like(NewTableName,TableName) :-
% WARNING: Existing tuples are not checked for consistency
  my_table(_,TableName,Arity),
  assertz(my_table('$des',NewTableName,Arity)),
  % Create attributes
  (
   my_attribute('$des',Pos,TableName,Att,Type),
   assertz(my_attribute('$des',Pos,NewTableName,Att,Type)),
   fail
  ;
   true
  ),
  % Create predefined integrity constraints:
  % - Not nullables
  (
   my_not_nullables('$des',TableName,NNAtts),
   assertz(my_not_nullables('$des',NewTableName,NNAtts)),
   fail
  ;
   true
  ),
  % - Primary key
  (
   my_primary_key('$des',TableName,PKAtts),
   assertz(my_primary_key('$des',NewTableName,PKAtts)),
   fail
  ;
   true
  ),
  % - Candidate keys
  (
   my_candidate_key('$des',TableName,CKAtts),
   assertz(my_candidate_key('$des',NewTableName,CKAtts)),
   fail
  ;
   true
  ),
  % - Foreign keys
  (
   my_foreign_key('$des',TableName,PAtts,FKTableName,FKAtts,RIds),
   replace_functor_RNVs_by_RId_list(TableName,NewTableName,RIds),
   assertz(my_foreign_key('$des',NewTableName,PAtts,FKTableName,FKAtts,RIds)),
   fail
  ;
   true
  ),
  % - Referenced by other table's foreign keys
  (
   my_foreign_key('$des',OtherTableName,PAtts,TableName,FKAtts,RIds),
   replace_functor_RNVs_by_RId_list(TableName,NewTableName,RIds),
   assertz(my_foreign_key('$des',OtherTableName,PAtts,NewTableName,FKAtts,RIds)),
   fail
  ;
   true
  ),
  % - Functional dependency
  (
   my_functional_dependency('$des',TableName,AttNames,DepAttNames),
   assertz(my_functional_dependency('$des',NewTableName,AttNames,DepAttNames)),
   fail
  ;
   true
  ),
  % Create user-defined integrity constraints
  (
   my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,SQL,TableName),
   my_member_chk(TableName/Arity,Preds),
   get_object_dlrules(head,Head,DLs),
   dlrule_to_rule_list(DLs,Rs),
   Head=..[FHead|Args],
   atom_codes(FHead,FHeadCs),
   atom_codes('$ic_',ICCs),
   atom_codes(TableName,TableNameCs),
   atom_codes(NewTableName,NewTableNameCs),
   append(ICCs,TableNameCs,HCs),
   append(HCs,TCs,FHeadCs),
   concat_lists([ICCs,NewTableNameCs,TCs],NewFHeadCs),
   atom_codes(NewFHead,NewFHeadCs),
   NewHead=..[NewFHead|Args],
   replace_functor(TableName,NewTableName,Rs,RRs),
   replace_functor(FHead,NewFHead,RRs,NewRs),
   rule_to_ruleNVs_list(NewRs,RNVss),
   assert_rules(RNVss,[],sql(_Q),[],_,ODLIds,_,_),
   replace_functor(TableName,NewTableName,Preds,NewPreds),
   replace_functor(TableName,NewTableName,Constraint,NewConstraint),
   my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,SQL,TableName),
   assertz(my_integrity_constraint('$des',NewPreds,NewConstraint,NVs,NewHead,ODLIds,SQL,NewTableName)),
   fail
  ;
   true
  ),
  compute_stratification.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Checking integrity constraints in the context of the database
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Check database w.r.t. its integrity constraints
% A database may become inconsistent w.r.t. its integrity constraints if 
% constraint checking is disabled and inconsistent tuples and/or integrity 
% constraints are added along this
check_db :-
  current_db('$des'),
  !,
  push_flag(check_ic,on,CF),
  write_info_log(['Checking type constraints...']),
  get_type_ctrs(TCtrs),
  check_type_ctr_list(TCtrs),
  write_info_log(['Checking existency constraints...']),
  findall(my_not_nullables('$des',A,B),my_not_nullables('$des',A,B),NNCtrs),
  check_ctr_failing_list(NNCtrs,CId),
  write_info_log(['Checking primary key constraints...']),
  findall(my_primary_key('$des',A,B),my_primary_key('$des',A,B),PKCtrs),
  check_ctr_failing_list(PKCtrs,CId),
  write_info_log(['Checking candidate key constraints...']),
  findall(my_candidate_key('$des',A,B),my_candidate_key('$des',A,B),CKCtrs),
  check_ctr_failing_list(CKCtrs,CId),
  write_info_log(['Checking foreign key constraints...']),
  findall(my_foreign_key('$des',A,B,C,D,E),my_foreign_key('$des',A,B,C,D,E),FKCtrs),
  check_ctr_failing_list(FKCtrs,CId),
  write_info_log(['Checking functional dependency constraints...']),
  findall(my_functional_dependency('$des',A,B,C),my_functional_dependency('$des',A,B,C),FDCtrs),
  check_ctr_failing_list(FDCtrs,CId),
  write_info_log(['Checking user-defined integrity constraints...']),
  findall(my_integrity_constraint('$des',A,B,C,D,E,F,G),my_integrity_constraint('$des',A,B,C,D,E,F,G),ICCtrs),
  check_ctr_failing_list(ICCtrs,CId),
  pop_flag(check_ic,CF).
check_db :-
  write_error_log(['This command does not support ODBC connections, yet']).

get_type_ctrs(TCtrs) :-
  get_tablenames(TableNames),
  get_type_ctr_list(TableNames,TCtrs).
  
get_type_ctr_list([],[]).
get_type_ctr_list([TableName|TableNames],[my_types('$des',TableName,ColnamesTypes)|TCtrs]) :-
  get_table_typed_arguments(TableName,ColnamesTypes),
  get_type_ctr_list(TableNames,TCtrs).
  
check_type_ctr_list([]).
check_type_ctr_list([TCtr|TCtrs]) :- 
  check_type_ctr(TCtr),  
  check_type_ctr_list(TCtrs).
  
check_type_ctr(my_types('$des',TableName,ColnameTypes)) :-
  length(ColnameTypes,Arity),
  % Remove the type schema:
  pop_type_declaration(TableName,Arity,_DeclaredTypes,_ColnameTypes),
  % Check whether the types are consistent with the loaded database
  (check_ctr(my_types('$des',TableName,ColnameTypes)) ->
    % Consistent
    true
   ;
    Schema=..[TableName,ColnameTypes],
    write_error_log(['In type constraint: ',Schema,nl])
  ),
  push_type_declaration(TableName,Arity,ColnameTypes).
  
%%%%%
% Check hard constraints
%%%%  

check_ctr(Ctr) :-
  check_ctr(Ctr,[]). % For top-level Context

%
% Types
%
% check_ctr(my_types('$des',TableName,ColnameTypes)) :-
%   (verbose(on) -> write_info_log(['Checking types over database for relation ''',TableName,'''.']) ; true),
%   length(ColnameTypes,Arity),
%   get_object_dlrules(namearity,TableName/Arity,DLs),
%   dlrule_to_rule_list(DLs,Rules),
%   findall(Types,member(_Colname:Types,ColnameTypes),DeclaredTypes),
%   check_rule_types_list(TableName,Rules,DeclaredTypes).

check_ctr(my_types('$des',TableName,ColnameTypes),_CId) :-
  check_ic(on),
  !,
  exec_if_verbose_on(write_info_log(['Checking types over database for relation ''',TableName,'''.'])),
  findall(Types,member(_Colname:Types,ColnameTypes),DeclaredTypes),
  length(ColnameTypes,Arity),
  check_pred_types(TableName/Arity,DeclaredTypes).
check_ctr(my_types('$des',_TableName,_ColnameTypes),_CId).

%
% Nullability
%
check_ctr(my_not_nullables('$des',TableName,NN_AttNames),CId) :-
  check_ic(on),
  !,
  exec_if_verbose_on(write_info_log(['Checking nullability over database for relation ''',TableName,'''.'])),
  my_table('$des',TableName,Arity),
  functor(Fact,TableName,Arity),
  build_PK_goal(Fact,TableName,NN_AttNames,NN_Vars,Goal),
  Head=..[nn|NN_Vars],
  my_list_to_list_of_lists(NN_Vars,NN_Vars_List),
  my_univ_list('is_null',NN_Vars_List,Conditions),
  my_list_to_disjunction(Conditions,Condition),
  Query=':-'(Head,(Goal,Condition)),
  get_answer(Query,CId,Facts),
  (bagof(NN,member(NN,Facts),NNs) ->
    write_error_log(['Null values found for ''',TableName,'''.',NN_AttNames,nl,
                     '       Offending values in database: ',NNs]),
    !,
    fail
   ;
    true
  ).
check_ctr(my_not_nullables('$des',_TableName,_NN_AttNames),_CId).
  
%
% Primary Key
%
check_ctr(my_primary_key('$des',TableName,PK_AttNames),CId) :-
  check_ctr(my_unique_key('$des',TableName,PK_AttNames),pk,CId).
%
% Candidate Key
%
check_ctr(my_candidate_key('$des',TableName,K_AttNames),CId) :-
  check_ctr(my_unique_key('$des',TableName,K_AttNames),ck,CId).
%
% Foreign Key
%
check_ctr(my_foreign_key('$des',TableName,FK_AttNames,FTableName,PK_AttNames,_RIds),CId) :-
  check_ic(on),
  !,
  exec_if_verbose_on(write_info_log(['Checking foreign key over database for relation ''',TableName,'''.'])),
  my_table('$des',TableName,Arity),
%  length(FK_AttNames,Arity),
  functor(TableGoal,TableName,Arity),
  build_FK_goal(TableGoal,FK_AttNames,FTableName,PK_AttNames,FK_Vars,FTableGoal),
  Head=..[fk|FK_Vars],
  Query=':-'(Head,(TableGoal,not(group_by(FTableGoal,FK_Vars,0>=0)))),
  get_answer(Query,CId,Facts),
  (bagof(FK,member(FK,Facts),FKs) ->
    write_error_log(['Foreign key violation ',TableName,'.',FK_AttNames,'->',FTableName,'.',PK_AttNames,nl,
                     '       Offending values in database: ',FKs]),
    !,
    fail
   ;
    true
  ).
check_ctr(my_foreign_key('$des',_TableName,_FK_AttNames,_FTableName,_PK_AttNames,_RIds),_CId).

%
% Functional Dependency
%
check_ctr(my_functional_dependency('$des',TableName,AttNames,DepAttNames),CId) :-
  check_ic(on),
  !,
  exec_if_verbose_on(write_info_log(['Checking functional dependency over database for relation ''',TableName,'''.'])),
  my_table('$des',TableName,Arity),
  functor(Fact,TableName,Arity),
  build_FD_goal(Fact,AttNames,DepAttNames,FDGoal),
  FDGoal=':-'(H,B),
  Goal=':-'(H,(Fact,B)),
  get_answer(Goal,CId,FDs),
  (FDs == [] ->
    true
   ;
    write_error_log(['Functional dependency violation ',TableName,'.',AttNames,'->',TableName,'.',DepAttNames,nl,
                     '       Offending values in database: ',FDs]),
    !,
    fail
  ).
check_ctr(my_functional_dependency('$des',_TableName,_AttNames,_DepAttNames),_CId).

%
% User-defined Integrity Constraint
%
check_ctr(my_integrity_constraint('$des',_Preds,Constraint,NVs,Head,_Ids,_SQL,_TableName),CId) :-
  check_ic(on),
  !,
  exec_if_verbose_on( 
    write_info_log(['Checking user-defined integrity constraint over database.']),
    write_datalog_rule((':-'(Constraint),NVs),7),
    nl_log),
  get_answer(Head,CId,Witnesses),
  (Witnesses==[] ->
    true
   ;
    write_error_log(['Integrity constraint violation.',nl]),
    (development(on) ->
      RHead=Head,
      RWitnesses=Witnesses
     ;
      replace_functor_term_list([Head|Witnesses],ic,[RHead|RWitnesses])
    ),
    write_datalog_rule((':-'(RHead,Constraint),NVs),7),
    nl_log,
    functor(Head,_,A),
    (A>0 -> % There are relevant variables to show
      write_log_list(['       Offending values in database: ',RWitnesses,nl]),
      write_tapi_eot
     ; 
      true
    ),
    !,
    fail
  ).
check_ctr(my_integrity_constraint('$des',_Preds,_Constraint,_NVs,_Head,_Ids,_SQL,_TableName),_CId).
    
%
% Unique Key
%
check_ctr(my_unique_key('$des',TableName,AttNames),Kind,CId) :-
  check_ic(on),
  !,
  (Kind=pk ->
    Message='primary key',
    UMessage='Primary key'
   ;
    Message='candidate key',
    UMessage='Candidate key'
  ),
  check_ctr(my_not_nullables('$des',TableName,AttNames),CId),
  exec_if_verbose_on(write_info_log(['Checking ',Message,' over database for relation ''',TableName,'''.'])),
  my_table('$des',TableName,Arity),
  functor(Fact,TableName,Arity),
  build_PK_goal(Fact,TableName,AttNames,UK_Vars,Goal),
  Head=..[Kind|UK_Vars],
  Query=':-'(Head,group_by(Goal,UK_Vars,count>1)),
  get_answer(Query,CId,Facts),
  (bagof(UK,member(UK,Facts),UKs) ->
    write_error_log([UMessage,' violation ',TableName,'.',AttNames,nl,
                     '       Offending values in database: ',UKs]),
    !,
    fail
   ;
    true
  ).
check_ctr(my_unique_key('$des',_TableName,_AttNames),_Kind,_CId).

check_tuple_type_ctr_list([]).  
check_tuple_type_ctr_list([Fact|Facts]) :-
%  infer_rule_types(Fact,_,_),
  check_rule_types(Fact),
  !,
  check_tuple_type_ctr_list(Facts).
check_tuple_type_ctr_list([Fact|_Facts]) :-
  functor(Fact,T,_A),
  get_table_types(T,DeclTypes),
  write_error_log(['Type mismatch ',DeclTypes,' (table declaration)',nl,
                   '       Witness fact: ',Fact]),
  !,
  fail.
  
  
% Testing whether a given table exists for current connection
% Used for syntax checking. An exception is raised should the arguments do not exist
exist_table(TableName) :-
  exist_table(TableName,_Arity).
  
exist_table(TableName,Arity) :-
  current_db('$des'),
%  (current_db('$des') ; des_sql_solving(on)),
%  des_sql_solving(off),
  !,
  (des_table_exists(TableName,Arity)
   ->
    true
   ;
    my_raise_exception(unknown_table(TableName),syntax(''),[])
  ).
exist_table(TableName,_Arity) :-
  (my_odbc_exists_table(TableName)
   ->
    true
   ;
    my_raise_exception(unknown_table(TableName),syntax(''),[])
  ).
  
des_table_exists(TableName) :-
  des_table_exists(TableName,_Arity).

des_table_exists(TableName,Arity) :-
  my_table('$des',TableName,Arity),
  \+ my_view('$des',TableName,_A,_S,_La,_D,_ODLIds,_L,_SC).


des_relation_exists(Relationname) :-
  des_relation_exists(Relationname,_Arity).

des_relation_exists(Relationname,Arity) :-
  my_table('$des',Relationname,Arity).

% Testing whether a given view exists for current connection
% Used for syntax checking. An exception is raised should the arguments do not exist
exist_view(Viewname) :-
  exist_view(Viewname,_Arity).
  
exist_view(Viewname,Arity) :-
  current_db('$des'),
%  (current_db('$des') ; des_sql_solving(on)),
%  des_sql_solving(off),
  !,
  (my_view('$des',Viewname,Arity,_S,_La,_D,_ODLIds,_L,_SC)
   ->
    true
   ;
    my_raise_exception(unknown_view(Viewname),syntax(''),[])
  ).
exist_view(Viewname,_Arity) :-
  (my_odbc_exists_view(Viewname)
   ->
    true
   ;
    my_raise_exception(unknown_view(Viewname),syntax(''),[])
  ).

% Testing whether a given relation exists for current connection
% Used for syntax checking. An exception is raised should the arguments do not exist
exist_relation(Relation) :-
  current_db(Connection),
  exist_relation(Connection,Relation).
  
exist_relation(Connection,RelationName/Arity) :-
  !,
  exist_relation(Connection,RelationName,Arity).
exist_relation(Connection,RelationName) :-
  exist_relation(Connection,RelationName,_Arity).
  
exist_relation('$des',Relation,Arity) :-
  !,
  (my_table('$des',Relation,Arity) 
   ->
    true
   ;
    (var(Arity) -> MObj=Relation ; MObj=Relation/Arity),
    my_raise_exception(unknown_relation(MObj),syntax(''),[])
  ).
exist_relation(Connection,Viewname,_Arity) :-
  ((my_odbc_exists_view(Connection,Viewname)
    ;
    my_odbc_exists_table(Connection,Viewname))
   ->
    true
   ;
    my_raise_exception(unknown_view(Viewname),syntax(''),[])
  ).

% Testing whether a given predicate (Functor/Arity) exists for current connection
% Used for syntax checking. An exception is raised should the arguments do not exist
exist_user_predicate(Predicate) :-
  pdg_user_predicates(UserPredicates),
  (member(Predicate,UserPredicates) 
   ->
   true
   ;
   my_raise_exception(unknown_user_predicate(Predicate),syntax(''),[])
  ).

% Testing whether a given argument does exist for a given table
% An exception is raised should the arguments do not exist
exist_att(TableName,Att) :-
  current_db(ConnectionName),
  exist_att(ConnectionName,TableName,TableName,Att).

exist_att(TableName,VarName,Att):-
  current_db(ConnectionName),
  exist_att(ConnectionName,TableName,VarName,Att).
  
exist_att(ConnectionName,TableName,VarName,Att) :-
  (my_attribute(ConnectionName,_Pos,TableName,Att,_Type)
   ->
    true
   ;
    my_raise_exception(unknown_column(VarName,Att),syntax(''),[])
  ).

% Testing whether a given set of arguments does exist for a given table for '$des'
% Used for syntax checking. Fail if some of the arguments do not exist
exist_atts(_TableName,[]).  
exist_atts(TableName,[Att|Atts]) :-
  (my_attribute('$des',_Pos,TableName,Att,_Type)
   ->
   exist_atts(TableName,Atts)
  ;
   write_error_log(['Unknown column ''',Att,'''.']),
   display_column_alternatives(TableName,Att),
   !,
   fail
%   my_raise_exception(unknown_column(TableName,Att),syntax(''),[])
  ).

% Testing whether a given relation does exist in the current connection
relation_exists(Relation) :-
  current_db(ConnectionName),
  relation_exists(ConnectionName,Relation).
  
relation_exists('$des',Relation) :-
  !,
  my_table('$des',Relation,_Arity).
relation_exists(Connection,Relation) :-
  my_odbc_exists_table(Connection,Relation).
relation_exists(Connection,Relation) :-
  my_odbc_exists_view(Connection,Relation).
  
  
view_exists(ViewName) :-
  current_db(ConnectionName),
  view_exists(ConnectionName,ViewName).
  
view_exists('$des',ViewName) :-
  !,
  my_view('$des',ViewName,_,_,_,_,_,_,_).
view_exists(Connection,ViewName) :-
  my_odbc_exists_view(Connection,ViewName).

table_exists(TableName) :-
  current_db(ConnectionName),
  table_exists(ConnectionName,TableName).
  
table_exists('$des',TableName) :-
  !,
  des_table_exists(TableName).
table_exists(Connection,TableName) :-
  my_odbc_exists_table(Connection,TableName).

% Use view_arity instead of:
% view_exists('$des',ViewName,Arity) :-
%   !,
%   my_view('$des',ViewName,Arity,_,_,_,_,_,_).
% view_exists(Connection,ViewName,Arity) :-
%   my_odbc_exists_view(Connection,ViewName),
%   my_odbc_get_table_arity(Connection,ViewName,Arity).
  
% relation_exists('$des',Relation) :-
%   !,
%   my_table('$des',Relation,_Arity).
% relation_exists(db2,Relation) :-
%   to_uppercase(Relation,URelation),
%   my_relation_exists(db2,URelation),
%   !.
% relation_exists(ConnectionName,Relation) :-
%   my_relation_exists(ConnectionName,Relation),
%   !.
%   
% my_relation_exists(ConnectionName,Relation) :-
%   my_odbc_exists_table(ConnectionName,Relation).
% my_relation_exists(ConnectionName,Relation) :-
%   my_odbc_exists_view(ConnectionName,Relation).
  
% relation_does_not_exist_list(_ConnectionName,[]).
% relation_does_not_exist_list(ConnectionName,[R|Rs]) :-
%   \+ relation_exists(ConnectionName,R),
%   relation_does_not_exist_list(ConnectionName,Rs).

% nn_consistent(_Pred,_NN_AttNames) :-   
%   % WARNING: TODO. There may exist stored tuples that do not meet the constraints
%   !,
%   true.
% nn_consistent(Pred,NN_AttNames) :-   
%   write_error_log(['Not null assertion failed for relation ',Pred,nl,
%                    '        There are null values for columns ',NN_AttNames]).

% pk_consistent(_Pred,_PK_AttNames) :-   
%   % WARNING: TODO
%   !,
%   true.
% pk_consistent(Pred,PK_AttNames) :-   
%   write_error_log(['Primary key assertion failed for relation ',Pred,nl,
%                    '        There are repeated entries for columns ',PK_AttNames]).

% ck_consistent(Pred,PK_AttNames) :-   
%   pk_consistent(Pred,PK_AttNames).

same_type_atts(_TableName,[],_FTableName,[]).
same_type_atts(TableName,[Att|Atts],FTableName,[FAtt|FAtts]) :-
  my_attribute('$des',_Pos,TableName,Att,Type),
  my_attribute('$des',_FPos,FTableName,FAtt,FType),
  (Type==FType ->
   same_type_atts(TableName,Atts,FTableName,FAtts)
   ;
   write_error_log(['Type mismatch ',TableName,'.',Att,':',Type,' <> ',FTableName,'.',FAtt,':',FType,'.']),
   !,
   fail).

assert_attr_types(Table,TypedAttrs) :-
  assert_attr_types(1,Table,TypedAttrs).
        
assert_attr_types(_I,_Table,[]) :- !.
assert_attr_types(I,Table,[C:T|CTs]) :-
  assertz(my_attribute('$des',I,Table,C,T)),
  I1 is I+1,
  assert_attr_types(I1,Table,CTs).

% Create view
create_view(Lang,SQLst,Schema,LVDs) :-
  create_view_k(Lang,SQLst,Schema,LVDs),
  clear_et, 
  compute_stratification.
  
% Create view, untouching ET, no stratification computation
% drop table schema, create view schema, compute view, restore table schema
% create_view_k(Lang,(with(SQLst,SQLsts),WSchema),Schema,LVDs) :-
%   !,
%   % Create view prototype (if hypothetical, do nothing as it already exists)
%   create_prototype_view_list(SQLsts,LocalViews,OldSchemas),
%   create_view_k1(Lang,(with(SQLst,SQLsts),WSchema),Schema,LVDs),
%   Schema=..[Name|_Args],
%   remove_from_list(Name,LocalViews,RLocalViews),
%   drop_schema_list('$des',RLocalViews),
%   assertz_list(OldSchemas).
% create_view_k(Lang,SQLst,Schema,LVDs) :-
%   !,
%   create_view_k1(Lang,SQLst,Schema,LVDs).
create_view_k(Lang,SQLst,Schema,LVDs) :-
  !,
%  hrsql_preprocess(SQLst,RSQLst,SQLsts),
  RSQLst=SQLst, SQLsts=[],
  local_view_definitions(RSQLst,LSQLsts), % LSQLsts: Nested ASSUME Extracted
  % Create view prototype (if hypothetical, do nothing as it already exists)
  create_prototype_view_list(LSQLsts,LocalViews,OldSchemas), % LocalViews: List of assumed relations
  % WARNING: Drop LSQLsts when dropping the view
  create_view_k_list(Lang,SQLsts,LVDs),
  create_view_k1(Lang,RSQLst,Schema,LVDs),
  Schema=..[Name|_Args],
  remove_from_list(Name,LocalViews,RLocalViews),
  drop_schema_list('$des',RLocalViews),
  assertz_list(OldSchemas).
  
  
% sqlsts_to_reldefs([],[]).
% sqlsts_to_reldefs([(SQLst,AS)|SQLsts],[create_view(sql,SQLst,AS)|RelDefs]) :-
%   sqlsts_to_reldefs(SQLsts,RelDefs).
  
create_view_k_list(_Lang,[],_LVDs).
create_view_k_list(Lang,[(SQLst,Schema)|SQLsts],LVDs) :-
  create_view_k(Lang,(SQLst,_AS),Schema,LVDs),
  create_view_k_list(Lang,SQLsts,LVDs).

 
% create_view_k(Lang,(with(SQLst,SQLsts),Schema),Schema,_LVDs) :-
%   !,
%   % Create view prototype (if hypothetical, do nothing as it already exists)
%   create_prototype_view_list(SQLsts,LocalViews,OldSchemas),
%   catch((create_or_replace_view_list_k(Lang,SQLsts,LocalViews),
%          create_view_k(Lang,SQLst,Schema,LocalViews),
%          drop_schema_list('$des',LocalViews),
%          assertz_list(OldSchemas)),
%         Message,
%         (Schema =.. [ViewName|_Args],
%          drop_viewname_k_list([ViewName|LocalViews]),
%          assertz_list(OldSchemas),
%          !,
%          throw(Message))
%         ).
% Schema-less view definition (no assumptions): only view name
% (assumptions require complete schema)
create_view_k1(Lang,(SQLst,Schema),ViewName,LocalViews) :-
  atom(ViewName),
  TableName=ViewName,
  catch(compile_to_dl(Lang,SQLst,Schema,DLsts,NVs),
        Message,
        (
         create_view_k_error(ViewName),
         !,
         throw(Message)
        )
       ),
  length(Schema,Arity1),
  Arity is Arity1-1,
  allowed_tablename(TableName,Arity),
%  once((DLsts=[':-'(Head,_)|_] ; DLsts=[Head|_])),
  DLsts=[Rule|_],
  rule_head(Rule,Head),
  functor(Head,Pred,Arity), 
%   (functor(Head,Pred,Arity) 
%    ->
   replace_predicate_names_and_assert(Pred,Arity,TableName,DLsts,NVs,_RDLsts,DVs,CRNVs,ODLIds,Unsafe,_Error1),
   length(TypedArgs,Arity),
   (dictionary(SCs) -> true ; SCs=[]),
   schema_to_colnametypes(Schema,TypedArgs),
   catch(allowed_colnametype_list(TypedArgs), 
        _M, 
         (retract_rule_by_id_list(ODLIds,_Error2),
          create_view_k_error(ViewName),
          throw(des_exception('')))),
   (is_setvar_safe(Unsafe), 
    is_rc_safe(Lang,Unsafe),
    infer_types_and_assert_schema(DVs,TypedArgs)
    ->
     my_retract_all_facts(my_view('$des',TableName,Arity,_,_,_,_,_,_)), % From persisted predicates
     assertz(my_view('$des',TableName,Arity,SQLst,Lang,DVs,ODLIds,LocalViews,SCs)),
%     assertz(my_view('$des',TableName,Arity,SQLst,Lang,CRNVs,ODLIds,LocalViews,SCs)),
     display_compiled_sql(DVs,CRNVs),
%     logiql_output(create_view(TableName,TypedArgs,CRNVs)),
     write_info_verb_log(['View created.'])
    ;
     !,
     retract_rule_by_id_list(ODLIds,_Error3),
     create_view_k_error(ViewName),
     throw(des_exception(''))
   ),
%   ;
%    create_view_k_error(ViewName),
%    my_raise_exception(generic,syntax(['Incorrect number of columns in data provider (must be ',Arity,').']),[])
%   ),
  !.
% View definition with schema
create_view_k1(Lang,(SQLst,AS),Schema,LocalViews) :-
  functor(Schema,TableName,Arity),
  % Assert provisional schema for view if not yet available: colnames given but no types yet
  % This is needed for the translation from SQL to Datalog
  (my_table('$des',TableName,Arity)
   ->
    get_table_typed_arguments(TableName,TypedArgs),
    (Schema =.. [TableName|TypedArgs]
     ->
      Assuming=true
     ;
      SchemaM =.. [TableName|TypedArgs],
      write_error_log(['Schema mismatch: ',Schema,' : ', SchemaM])
    )
   ;
    assertz(my_table('$des',TableName,Arity)),
    Schema =.. [TableName|TypedArgs],
    assert_attr_types(TableName,TypedArgs)
  ),
  catch(compile_to_dl(Lang,SQLst,AS,DLsts,NVs),
        Message,
        (
%         create_view_k_error(AS),
         create_view_k_error(Schema),
         !,
         throw(Message)
        )
       ),
  % Remove provisional schema for view, saving older one if exists to restore upon further type error
  % Needed for asserting rules and avoid inferring types with provisional schema
  collect_schema_facts('$des',TableName,Arity,SchemaFactList),
  my_retract_all_facts_list(SchemaFactList),
%  once((DLsts=[':-'(Head,_)|_] ; DLsts=[Head|_])),
  DLsts=[Rule|_],
  rule_head(Rule,Head),
  (functor(Head,Pred,Arity) 
   ->
   replace_predicate_names_and_assert(Pred,Arity,TableName,DLsts,NVs,_RDLsts,DVs,CRNVs,ODLIds,Unsafe,_Error1),
   (dictionary(SCs) -> true ; SCs=[]),
   (is_setvar_safe(Unsafe), 
    is_rc_safe(Lang,Unsafe),
    infer_types_and_assert_schema(DVs,TypedArgs)
    ->
     my_retract_all_facts(my_view('$des',TableName,Arity,_,_,_,_,_,_)), % From persisted predicates
     assertz(my_view('$des',TableName,Arity,SQLst,Lang,DVs,ODLIds,LocalViews,SCs)),
%     assertz(my_view('$des',TableName,Arity,SQLst,Lang,CRNVs,ODLIds,LocalViews,SCs)),
     % Local view schemas are no longer needed
%     schema_to_colnametypes(Schema,ColTypes),
     display_compiled_sql(DVs,CRNVs),
%     logiql_output(create_view(TableName,TypedArgs,CRNVs)),
     write_info_verb_log(['View created.'])
    ;
     %write_error_log(['Type conflict(s).']),
     !,
     retract_rule_by_id_list(ODLIds,_Error2),
     create_view_k_error(Schema),
     % Recover old definitions:
     (Assuming==true
      ->
       assertz_list(SchemaFactList)
      ;
       true
     ),
     throw(des_exception(''))
   )
   ;
   create_view_k_error(Schema),
  language_acronym(Lang,ULang),
   my_raise_exception(generic,syntax(['(',ULang,') Incorrect number of arguments (must be ',Arity,').']),[])
  ).
create_view_k1(_Lang,_SQLst,Schema,_LVDs) :-
  create_view_k_error(Schema),
  !,
  fail.
  
create_view_k_error(Schema) :-
  Schema =.. [TableName|_Args],
  (retract(my_view('$des',TableName,_ArityV,_SQLst,_Lang,_DVs,_ODLIds,LocalViews,_SCs))
   ->
    drop_viewname_k_list([TableName|LocalViews])
   ;
    my_retract_all_facts(my_table('$des',TableName,_ArityT)),
    my_retract_all_facts(my_attribute('$des',_Pos,TableName,_Att,_Type))
  ).

  
collect_schema_facts(DB,TableName,Arity,SchemaFactList) :-
  findall(SchemaFact,
    (SchemaFact = my_table(DB,TableName,Arity),
     call(SchemaFact)
    ;
     SchemaFact = my_attribute(DB,_Pos,TableName,_Colname,_DVs),
     call(SchemaFact)
    ;
     SchemaFact = my_view(DB,TableName,Arity,_,_,_,_,_,_),
     call(SchemaFact)
    ),
    SchemaFactList).

% Try to infer types and assert schema on success
% infer_types_and_assert_schema(+DVs,?TypedArgs)
infer_types_and_assert_schema(DVs,TypedArgs) :-
  ruleNVs_to_rule_list(DVs,Rs),
  Rs=[R|_TRs],
  get_rule_table_name_arity(R,TableName,Arity),
  infer_types_rule_list(Rs,InferredTypes,ITypedArgs,_ExtraTypes),
  close_types(InferredTypes,CInferredTypes),
  (nonvar(TypedArgs) -> type_to_coltype_list(DeclaredTypes,TypedArgs) ; true),
%  type_to_coltype_list(DeclaredTypes,TypedArgs),
  ((\+ my_ground(TypedArgs)
    ;
    type_subsumed_list(CInferredTypes,DeclaredTypes)
   )
   ->
    swap_ucnt_icnt(TypedArgs,ITypedArgs),
    (my_table('$des',TableName,Arity) ->
      true % Already available from a persistent predicate
     ;
      assertz(my_table('$des',TableName,Arity)),
      assert_attr_types(TableName,TypedArgs)
    )
   ;
    assign_NVs(CInferredTypes,NVs),
    write_error_log(['Type mismatch ','$NVs'(CInferredTypes,NVs),' (inferred types) vs. ',DeclaredTypes,' (declared types).']),
    !,
    fail
  ).


% Predicate,Arity,NewPredicate,Rules,ReplacedRules,RNVss,Error
% DLsts: Input Rs
% RDLsts: Output replaced names in Rs 
% DVs: RNVs before preprocessing
% CRNVs: RNVs after preprocessing
% replace_predicate_names_and_assert(P,Arity,Q,DLsts,RDLsts,DVs,CRNVs,ODLIds,Unsafe,Error) :-
%   replace_functor(P,Q,DLsts,ARDLsts),
% %   number_codes(Arity,SArity),
% %   "_"=[US],
% %   atom_codes(AArity,[US|SArity]),
% %   atom_codes(P,[P1,P2|_]),
% %   atom_codes(AP,[P1,P2]),
% %   atom_concat(Q,AArity,NQ),
% %   atom_concat(NQ,'_',NLV),
% %   replace_functor_substring(AP,NLV,ARDLsts,FRDLsts),
% %   assign_variable_names_list(FRDLsts,SUDVs),
%   replace_dependent_system_predicates(P,Arity,Q,ARDLsts,FRDLsts),
%   assign_variable_names_list(FRDLsts,SUDVs),
%   rename_singleton_vars_RNVss(SUDVs,UDVs),
% %  replace_functor('$eq','=',[FRDLsts,UDVs],[RDLsts,DVs]),
%   replace_functor('$eq','=',[ARDLsts,UDVs],[RDLsts,DVs]),
%   push_flag(safety_warnings,off,SW),
% %  push_flag(reorder_goals,on,RSW),
% %  assert_rules(DVs,[],sql(Q),[simplify,safety,reorder,unfold],CRNVs,ODLIds,Unsafe,Error),
% %  assert_rules(UDVs,[],sql(Q),[simplify,safety,reorder,replace_eqs,unfold],CRNVs,ODLIds,Unsafe,Error),
%   assert_rules(UDVs,[],sql(Q),[simplify,safety,reorder,replace_eqs],CRNVs,ODLIds,Unsafe,Error),
% %  pop_flag(reorder_goals,RSW),
%   pop_flag(safety_warnings,SW).
replace_predicate_names_and_assert(P,Arity,Q,DLsts,NVs,RDLsts,DVs,CRNVs,ODLIds,Unsafe,Error) :-
  replace_predicate_names(P,Arity,Q,DLsts,RDLsts,FRDLsts),
  assign_variable_names_list(FRDLsts,NVs,SUDVs),
  rename_singleton_vars_RNVss(SUDVs,UDVs),
  replace_functor('$eq','=',UDVs,DVs),
  (language(sql)->push_flag(safety_warnings,off,SW);true),
  assert_rules(UDVs,[],sql(Q),[simplify,safety,reorder,replace_eqs],CRNVs,ODLIds,Unsafe,Error),
  (language(sql)->pop_flag(safety_warnings,SW);true).

replace_predicate_names(P,Arity,Q,DLsts,RDLsts,FRDLsts) :-
  replace_functor(P,Q,DLsts,ARDLsts),
  replace_dependent_system_predicates(P,Arity,Q,ARDLsts,FRDLsts),
  replace_functor('$eq','=',FRDLsts,RDLsts).

replace_dependent_system_predicates(P,_Arity,_Q,DLsts,DLsts) :-
  \+ is_system_identifier(P),
  !.
replace_dependent_system_predicates(P,Arity,Q,DLsts,RDLsts) :-
  number_codes(Arity,SArity),
  "_"=[US],
  atom_codes(AArity,[US|SArity]),
  atom_codes(P,[P1,P2|_]),
  atom_codes(AP,[P1,P2]),
  atom_concat(Q,AArity,NQ),
  atom_concat(NQ,'_',NLV),
  replace_functor_substring(AP,NLV,DLsts,RDLsts).

rename_singleton_vars_RNVss([],[]).
rename_singleton_vars_RNVss([(R,NVs)|RNVss],[(R,RNVs)|RRNVss]) :-
  singletons(R,SVs),
  rename_singleton_vars(NVs,SVs,RNVs),
  rename_singleton_vars_RNVss(RNVss,RRNVss).
  
rename_singleton_vars([],_SVs,[]).
rename_singleton_vars([N=V|NVs],SVs,[RN=V|RNVs]) :-
  my_member_var(V,SVs),
  !,
  (atom_concat('_',_,N) 
   ->
    RN=N
   ;
    atom_concat('_',N,RN)),
  rename_singleton_vars(NVs,SVs,RNVs).
rename_singleton_vars([NV|NVs],SVs,[NV|RNVs]) :-
  rename_singleton_vars(NVs,SVs,RNVs).
  
% Create views in a list, untouching ET, no stratification computation
create_or_replace_view_list_k(_Lang,[],_NewViewNames).
create_or_replace_view_list_k(Lang,[(SQLst,Schema)|Schemas],NewViewNames) :-
  create_or_replace_view_k(Lang,(SQLst,Schema),Schema,NewViewNames),
  create_or_replace_view_list_k(Lang,Schemas,NewViewNames).

% Create prototype views from SQL schemas. 
% Return the list of new view names (existing ones correspond to hypothetical query), 
% and defines the prototype view schema as facts
create_prototype_view_list(Schemas,NewViewnames,OldSchemas) :-
  check_complete_schemas(Schemas),
  check_no_redefinitions(Schemas),
  old_schemas(Schemas,DOldSchemas),
  remove_duplicates(DOldSchemas,OldSchemas),
  build_prototype_view_list(Schemas,DNewViewnames),
  remove_duplicates(DNewViewnames,NewViewnames).

check_complete_schemas_in_with((with(_SQLst,SQLsts),_AS)) :-
  check_complete_schemas(SQLsts),
  !.
check_complete_schemas_in_with(_SQLst).

check_complete_schemas([]).
check_complete_schemas([(_SQLst,Schema)|Vs]) :-
  (atom(Schema)
   ->
    my_raise_exception(generic,syntax(['Complete schema required for local view definition: ',Schema]),[])
   ;
    check_complete_schemas(Vs)).

% check_no_redefinitions(_Schemas) :-
%   hypothetical(on),
%   !.
% check_no_redefinitions([]).
% check_no_redefinitions([(_SQLst,Schema)|Vs]) :-
%   functor(Schema,TableName,_Arity),
%   (my_table('$des',TableName,Arity)
%    -> 
%     my_raise_exception(generic,syntax(['Syntax error. Trying to redefine ',TableName,'/',Arity]),[])
%    ;
%     check_no_redefinitions(Vs)).

check_no_redefinitions(_Schemas) :-
  hypothetical(on),
  !.
check_no_redefinitions([]).
check_no_redefinitions([(_SQLst,Schema)|Vs]) :-
  functor(Schema,TableName,_Arity),
  check_no_redefinition(TableName),
  check_no_redefinitions(Vs).

check_no_redefinition(TableName) :-
  (my_table('$des',TableName,Arity) -> 
   my_raise_exception(generic,syntax(['Syntax error. Trying to redefine ',TableName,'/',Arity]),[])
  ;
   true).
    
    
old_schemas(_Schemas,[]) :-
  hypothetical(off),
  !.
old_schemas([],[]).
old_schemas([(_SQLst,Schema)|Schemas],OldSchemas) :-
  functor(Schema,TableName,Arity),
  Table=my_table('$des',TableName,Arity),
  call(Table), % Hypothetical
  !,
  findall(my_attribute('$des',Pos,TableName,Attr,Type),my_attribute('$des',Pos,TableName,Attr,Type),Atts),
  View=my_view('$des',TableName,Arity,_,_,_RNVss,_,_,_),
  (call(View)
   ->
    OldSchema=[Table,View|Atts]
   ;
    OldSchema=[Table|Atts]
  ),
  old_schemas(Schemas,OldSchemas1),
  my_set_union(OldSchema,OldSchemas1,OldSchemas).
old_schemas([_Schema|Schemas],OldSchemas) :-
  old_schemas(Schemas,OldSchemas).
    
% Build new prototype schemas
% If hypothetical enabled schema might exist already
% Returns new view names, but not existing ones 
build_prototype_view_list([],[]).
%build_prototype_view_list([(_SQLst,Schema)|Vs],TableNames) :-
build_prototype_view_list([(_SQLst,Schema)|Vs],[TableName|TableNames]) :-
  functor(Schema,TableName,Arity),
  my_table('$des',TableName,Arity), % Hypothetical. Existing view
  my_view('$des',TableName,Arity,_SQL,_Lang,_DVs,_ODLIds,_LVDs,_SCs),
  !,
  build_prototype_view_list(Vs,TableNames).
%build_prototype_view_list([(_SQLst,Schema)|Vs],TableNames) :-
build_prototype_view_list([(_SQLst,Schema)|Vs],[TableName|TableNames]) :-
  functor(Schema,TableName,Arity),
  my_table('$des',TableName,Arity), % Hypothetical. Existing table
  !,
  build_prototype_view_list(Vs,TableNames).
build_prototype_view_list([(_SQLst,Schema)|Vs],[TableName|TableNames]) :-
  Schema =.. [TableName|Args],
  length(Args,Arity),
  assertz(my_table('$des',TableName,Arity)),
  assert_attr_types(TableName,Args),
  assertz(my_view('$des',TableName,Arity,_SQL,sql,_DVs,_ODLIds,_LVDs,_SCs)),
  build_prototype_view_list(Vs,TableNames).

% Create or replace view
create_or_replace_view(Lang,(SQLst,AS),Schema) :-
  !,
  Schema=..[TableName|_Args],
  drop_view_k_if_exists(TableName),
  create_view(Lang,(SQLst,AS),Schema,[]).
create_or_replace_view(Lang,SQLst,Schema) :-
  !,
  create_or_replace_view(Lang,(SQLst,_AS),Schema).
  
drop_view_k_if_exists(TableName) :-
  (my_table('$des',TableName,_)
   -> 
    drop_view_k(TableName,no_warn),
    !
   ;
    true).
    
% Create or replace view, untouching ET, no stratification computation
create_or_replace_view_k(Lang,(SQLst,Schema),Schema,NewViewNames) :-
  functor(Schema,Viewname,_Arity),
  (my_table('$des',Viewname,_)
   -> 
    ((member(Viewname,NewViewNames)
      ;
      hypothetical(off)
     )
     ->
      drop_view_k(Viewname,no_warn)
     ;
      true
    ),
    !
   ;
    true
  ),
  create_view_k(Lang,(SQLst,Schema),Schema,[]).


% Drop tables if exists
drop_table_if_exists_list(TableNames) :-
  drop_table_if_exists_list(TableNames,true,_Dropped).
  
drop_table_if_exists_list([],_IfExists,Dropped) :-
  (Dropped==true
   ->
    clear_et, 
    compute_stratification
   ;
    true).
drop_table_if_exists_list([TableName|TableNames],IfExists,Dropped) :-
  drop_table_if_exists(TableName,IfExists,Dropped),
  drop_table_if_exists_list(TableNames,IfExists,Dropped).
  
drop_table_if_exists(TableName,IfExists,Dropped) :-
  (\+ (my_table('$des',TableName,_))
   -> 
    (IfExists==false
     ->
      write_error_log(['Table ''',TableName,''' not defined.']),
      display_table_alternatives(TableName)
     ;
      true
    )
   ;
    (\+ view_arity('$des',TableName,_)
     ->
      drop_table(TableName,Warning),
      Dropped=true,
      store_elapsed_time(computation),
      display_elapsed_time,
      (Warning==true -> true ; write_tapi_success)
     ;
      write_error_log(['Cannot drop a view with DROP TABLE. Use DROP VIEW instead.'])
    )
  ).
  
% % Drop tables if exists
% drop_table_if_exists_list([],_IfExists,Dropped) :-
%   (Dropped==true
%    ->
%     clear_et, 
%     compute_stratification
%    ;
%     true).
% drop_table_if_exists_list([TableName|TableNames],IfExists,Dropped) :-
%   drop_table_if_exists(TableName,IfExists,Dropped),
%   drop_table_if_exists_list(TableNames,IfExists,Dropped).
%   
% drop_table_if_exists(TableName,IfExists,Dropped) :-
%   (\+ (my_table('$des',TableName,_))
%    -> 
%     (IfExists==false
%      ->
%       write_error_log(['Table ''',TableName,''' not defined.']),
%       display_table_alternatives(TableName)
%      ;
%       true
%     )
%    ;
%     (\+ view_arity('$des',TableName,_)
%      ->
%       drop_table_k(TableName,Warning),
%       Dropped=true,
%       store_elapsed_time(computation),
%       display_elapsed_time,
%       (Warning==true -> true ; write_tapi_success)
%      ;
%       write_error_log(['Cannot drop a view with DROP TABLE. Use DROP VIEW instead.'])
%     )
%   ).
%   
% Drop table
drop_table(TableName) :-
  drop_table(TableName,warn,_,yes).

drop_table(TableName,Warning) :-
  drop_table(TableName,warn,Warning,yes).

drop_table_list([]).
drop_table_list([TableName|TableNames]) :-
  drop_table(TableName),
  drop_table_list(TableNames).

% Drop table, untouching ET, no stratification computation
drop_table_k(TableName) :-
  drop_table(TableName,warn,_Warning,no).
  
drop_table_k(TableName,Warning) :-
  drop_table(TableName,warn,Warning,no).
  
drop_table_k_no_warn(TableName) :-
  drop_table(TableName,no_warn,_Warning,no).

drop_table(TableName,Warn,Warning,ClearET) :-
  current_db(Conn),
  drop_table(Conn,TableName,Warn,Warning,ClearET).
  
drop_table('$des',TableName,Warn,Warning,ClearET) :-
  !,
  my_table('$des',TableName,Arity),
  my_retract_all_facts(my_table('$des',TableName,Arity)),
  my_retract_all_facts(my_attribute('$des',_Pos,TableName,_Attr,_Type)),
  my_retract_all_facts(my_not_nullables('$des',TableName,_NNAtts)),
  my_retract_all_facts(my_primary_key('$des',TableName,_PKAtts)),
  my_retract_all_facts(my_candidate_key('$des',TableName,_CKAtts)),
  my_retract_all_facts(my_foreign_key('$des',TableName,_PAtts,_FKTableName,_FKAtts,_RIds)),
  my_retract_all_facts(my_foreign_key('$des',_TableName,_Atts,TableName,_FAtts,_FRIds)),
  my_retract_all_facts(my_functional_dependency('$des',TableName,_FDAtts,_FDDetAtts)),
  drop_all_ic(TableName/Arity),
  get_object_dlrules(namearity,TableName/Arity,ODLs),
  retract_dlrule_list(ODLs,_Error),
  drop_table_end(TableName,Arity,Warn,Warning,ClearET).
drop_table(Conn,TableName,Warn,Warning,ClearET) :-
  table_arity(TableName,Arity),
  drop_rdb_table(Conn,TableName),
  drop_table_end(TableName,Arity,Warn,Warning,ClearET).

drop_table_end(TableName,Arity,Warn,Warning,ClearET) :-
  (ClearET==yes
   ->
    clear_et, 
    update_stratification_remove_rules(_DLsts)
   ;
    true),
  (Warn==warn
   ->
    dependent_relations_warning(TableName,Arity,Warning)
   ;
    true
  ),
  write_info_verb_log(['Table ''',TableName,''' dropped.']).

% Drop view, clearing ET and stratification computation
drop_view(ViewName) :-
  drop_view(ViewName,_Warning).
  
drop_view(ViewName,Warning) :-
  drop_view(ViewName,warn,Warning,yes).

% Drop view, untouching ET, no stratification computation
drop_view_k(ViewName) :-
  drop_view_k(ViewName,warn).
  
drop_view_k(ViewName,Warn) :-
  drop_view(ViewName,Warn,_,no).
drop_view_k(_ViewName,_Warn).

% drop_view(ViewName,ClearET) :-
%   drop_view(ViewName,warn,ClearET).
  
drop_view(ViewName,Warn,Warning,ClearET) :-
  current_db(Conn),
  drop_view(Conn,ViewName,Warn,Warning,ClearET).
  
drop_view('$des',ViewName,Warn,Warning,ClearET) :-
  !,
  my_table('$des',ViewName,Arity),
  my_view('$des',ViewName,Arity,_SQLst,_Lang,RNVss,ODLIds,_LVDs,_SCs),
%  drop_viewname_k_list(LVDs,no_warn),
  functor(PredSchema,ViewName,Arity),
  (my_persistent(_Connection,PredSchema)
   ->
%     ruleNVs_to_rule_list(RNVss,Rs),
%     retract_rule_list(Rs,_Error1),
    drop_persistent_relations(ViewName),
    drop_persistent_flags(ViewName/Arity)
   ;
    true
  ),
  retract_rule_by_id_list(ODLIds,_Error2),
  drop_schema('$des',ViewName,Arity),
  (ClearET==yes
   ->
    update_stratification_remove_rules(RNVss), % still todo
    processC(clear_et,[],[],yes)
   ;
    true),
  (Warn==warn
   ->
    dependent_relations_warning(ViewName,Arity,Warning)
   ;
    true),  
  write_info_verb_log(['View ''',ViewName,''' dropped.']).
drop_view(Connection,ViewName,_Warn,_Warning,ClearET) :-
%  Connection\=='$des',
  (my_odbc_get_table_arity(ViewName,Arity)
   ->
    (is_persistent_predicate(ViewName/Arity)
     ->
      drop_persistent_relations(ViewName),
      drop_persistent_flags(ViewName/Arity),
      % WARNING: Is the following needed? Yes, it is for persistence
      drop_schema('$des',ViewName,Arity),
      compute_stratification
     ;
      drop_rdb_view(Connection,ViewName),
      update_rdb_pdg_object_action_query(view(ViewName),drop,true,_Query)
    ),
%     % WARNING: Is the following needed? Yes, it is for persistence. Moved above
%     drop_schema('$des',ViewName,Arity),
    (ClearET==yes
     ->
      processC(clear_et,[],[],yes) 
     ;
      true
    ),
    write_info_verb_log(['View ''',ViewName,''' dropped.'])
   ;
    write_error_log(['View ''',ViewName,''' does not exist.'])
  ).

drop_schema_list(_DB,[]).
drop_schema_list(DB,[Name|Names]) :-
  drop_schema(DB,Name),
  drop_schema_list(DB,Names).

drop_schema(DB,TableName) :-
  drop_schema(DB,TableName,_Arity).

drop_schema(DB,TableName,Arity) :-
  my_retract_all_facts(my_attribute(DB,_Pos,TableName,_Attr,_Type)),
  my_retract_all_facts(my_table(DB,TableName,Arity)),
  my_retract_all_facts(my_view(DB,TableName,Arity,_SQLst,_Lang,_RNVss,_ODLIds,_LVDs,_SCs)).

dependent_relations_warning(TableName,Arity,Warning) :-
  dependent_relations(TableName/Arity,Preds),
  (Preds\==[]
   ->
    write_warning_log(['Dangling relations: ',Preds]),
    Warning=true
   ; 
    true).

% % Drop view, untouching ET, no stratification computation
% drop_view_k(TableName) :-
%   my_table('$des',TableName,Arity),
%   my_retract_all_facts(my_table('$des',TableName,Arity)),
%   my_retract_all_facts(my_attribute('$des',_Pos,TableName,_Attr,_Type)),
%   my_view('$des',TableName,Arity,SQLst,Lang,RNVss,ODLIds,LVDs,SCs),
%   drop_viewname_k_list(LVDs),
%   functor(PredSchema,TableName,Arity),
%   (my_persistent(_Connection,PredSchema)
%    ->
%     ruleNVs_to_rule_list(RNVss,Rs),
%     retract_rule_list(Rs,_Error1)
% %     drop_persistent_relations(TableName),
% %     drop_persistent_flags(TableName/Arity)
%    ;
%     retract_rule_by_id_list(ODLIds,_Error2)
%   ),
%   my_retract_all_facts(my_view('$des',TableName,Arity,SQLst,Lang,RNVss,ODLIds,LVDs,SCs)),
%   write_info_verb_log(['View ''',TableName,''' dropped.']).
% drop_view_k(_TableName).

% Drop views in a list, untouching ET
drop_view_k_list(L) :-
  drop_view_k_list(L,warn).
  
drop_view_k_list([],_Warn).
drop_view_k_list([(_SQLst,Schema)|Vs],Warn) :-
  Schema =.. [TableName|_Args],
  drop_view_k(TableName,Warn),
  drop_view_k_list(Vs,Warn).

% Drop view names in a list, untouching ET
drop_viewname_k_list(L) :-
  drop_viewname_k_list(L,warn).

drop_viewname_k_list([],_Warn).
drop_viewname_k_list([ViewName|VNs],Warn) :-
  drop_view_k(ViewName,Warn),
  drop_viewname_k_list(VNs,Warn).

% Drop view names in a list, untouching ET (no), no warnings (no_warn), but incrementally updating the PDG
drop_viewname_u_list([]).
drop_viewname_u_list([ViewName|VNs]) :-
  drop_view(ViewName,no_warn,_Warning,no),
  drop_viewname_u_list(VNs).

drop_viewname_u_if_exists_list([]).
drop_viewname_u_if_exists_list([ViewName|VNs]) :-
  view_arity(ViewName,_),
  !,
  drop_view(ViewName,no_warn,_Warning,no),
  drop_viewname_u_if_exists_list(VNs).
drop_viewname_u_if_exists_list([_|VNs]) :-
  !,
  drop_viewname_u_if_exists_list(VNs).

% Drop RDB views
drop_rdb_view_if_exists_list(_Connection,[]).
drop_rdb_view_if_exists_list(Connection,[Name|Names]) :-
  delimited_sql_identifier(Connection,Name,StrDelimitedName),
%  atom_codes(Name,StrDelimitedName),
  concat_lists(["DROP VIEW ",StrDelimitedName," IF EXISTS"],StrDropView),
  display_string_list_sql_on([StrDropView]),
  drop_rdb_view_if_exists(Connection,Name),
  drop_rdb_view_if_exists_list(Connection,Names).
  
  
% Drop a view if it exists
drop_rdb_view_if_exists(TableName) :-
  current_db(Connection),
  drop_rdb_view_if_exists(Connection,TableName).
  
drop_rdb_view_if_exists(Connection,TableName) :-
  (my_odbc_exists_view(Connection,TableName) ->
    drop_rdb_view(Connection,TableName)
   ;
    true).
      
% Drop a view
drop_rdb_view(TableName) :-
  current_db(Connection),
  drop_rdb_view(Connection,TableName).
  
drop_rdb_view(Connection,TableName) :-	    
%  atom_codes(TableName,TableNameStr),
  my_odbc_identifier_name(Connection,TableName,ODBCTableName),
  delimited_sql_identifier(Connection,ODBCTableName,TableNameStr),
  my_odbc_get_dbms(Connection,DBMS),
  (DBMS == access ->
    concat_lists(["DROP TABLE ",TableNameStr],DropStr)
   ;
    concat_lists(["DROP VIEW ",TableNameStr],DropStr)
  ),
  my_odbc_ddl_query(Connection,DropStr).


% Drop a table
drop_rdb_table(TableName) :-
  current_db(Connection),
  drop_rdb_table(Connection,TableName).
  
drop_rdb_table(Connection,TableName) :-	    
%  atom_codes(TableName,TableNameStr),
  my_odbc_identifier_name(Connection,TableName,ODBCTableName),
  delimited_sql_identifier(Connection,ODBCTableName,TableNameStr),
  concat_lists(["DROP TABLE ",TableNameStr],DropStr),
  my_odbc_ddl_query(Connection,DropStr).


% Drop database
drop_database :-
  reset_database.

drop_tablename_k_list([]).
drop_tablename_k_list([TableName|TableNames]) :-
  drop_table_k(TableName),
  drop_tablename_k_list(TableNames).

get_tablenames(TableNames) :-
  get_tablenames(_TableName,TableNames).

get_viewnames(ViewNames) :-
  get_viewnames(_ViewName,ViewNames).

get_relationnames(RelationNames) :-
  get_tablenames(TableNames),
  get_viewnames(ViewNames),
  append(TableNames,ViewNames,RelationNames).
  
% Testing whether the des database is empty
empty_des_rdb :-
  get_tablenames(Ts),
  !,
%  (Ts==[] ; Ts==[dual]),
  Ts==[],
  get_viewnames(Vs),
  !,
  Vs==[].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Database Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*********************************************************************/
/* Drop all tables: drop_all_tables(-TableNames)                     */
/*********************************************************************/

drop_all_tables(TableNames) :-
  get_tablenames(TableNames), % WARNING: For external DBMS's, dependencies (IR constraints) should guide the drop order
    my_remove(dual,TableNames,RTableNames),
    my_remove('DUAL',RTableNames,RRTableNames),
  (RRTableNames==[]
   ->
    write_warning_log(['No tables found.'])
   ;
    push_flag(check_ic,off,OldFlag),
    drop_tablename_k_list(RRTableNames),
    pop_flag(check_ic,OldFlag)
  ).

/*********************************************************************/
/* Drop all views: drop_all_views(-ViewNames)                        */
/*********************************************************************/

drop_all_views(ViewNames) :-
  get_viewnames(ViewNames),
  (ViewNames==[]
   ->
    write_warning_log(['No views found.'])
   ;
    view_arity_list(ViewNames,Nodes),
    sort_by_topological_order(Nodes,ONodes),
    my_reverse(ONodes,RNodes),
    push_flag(check_ic,off,OldFlag),
    my_unzip(RNodes,OViewNames,_),
    drop_viewname_k_list(OViewNames),
    pop_flag(check_ic,OldFlag)
  ).
  
/*********************************************************************/
/* Drop all views: drop_all_relations(-RelationNames)                */
/*********************************************************************/

drop_all_relations(Rs) :-
  drop_all_views(Vs),
  drop_all_tables(Ts),
  append(Ts,Vs,Rs).

/*********************************************************************/
/* Listing tables: list_relations/0                                  */
/*********************************************************************/

% list_relations :-
%   system_mode(hrsql),
%   !,
%   list_hrsql_relations.
list_relations :-
  get_tablenames(_,TableNames),
  get_viewnames(_,ViewNames),
  append(TableNames,ViewNames,UnorderedRelationNames),
  my_mergesort(UnorderedRelationNames,RelationNames),
  display_lines(RelationNames),
  write_tapi_eot.

/*********************************************************************/
/* Listing tables: list_tables/0                                     */
/*********************************************************************/

list_tables :-
  get_tablenames(_,UnorderedTableNames),
  my_mergesort(UnorderedTableNames,TableNames),
  display_lines(TableNames),
  write_tapi_eot.

/*********************************************************************/
/* Listing table schema: list_table_schemas/0                        */
/*********************************************************************/

list_table_schemas :-
  get_tablenames(_Name,UnorderedTableNames),
  my_mergesort(UnorderedTableNames,TableNames),
  list_table_schema_list(TableNames),
  write_tapi_eot.

/*********************************************************************/
/* Listing tables: list_views/0                                      */
/*********************************************************************/

list_views :-
  get_viewnames(_Name,UnorderedViewNames),
  my_mergesort(UnorderedViewNames,ViewNames),
  display_lines(ViewNames),
  write_tapi_eot.

/*********************************************************************/
/* Listing view schema: list_view_schemas/0                          */
/*********************************************************************/

list_view_schemas :-
  get_viewnames(_Name,UnorderedViewNames),
  my_mergesort(UnorderedViewNames,ViewNames),
  list_table_schema_list(ViewNames),
  write_tapi_eot.

/*********************************************************************/
/* Listing the DB schema: list_schema/0                              */
/*********************************************************************/

list_schema :-
  list_schema(_N).

/*********************************************************************/
/* Listing the DB schema for either the whole database, or a given   */
/* table or view: list_schema/2                                      */
/*********************************************************************/

:- dynamic(boolean/1).

list_schema(Name) :-
  current_db(Connection),
  list_schema(Connection,Name).

list_schema(Connection,Name) :-
  (opened_db(Connection)
   ->
    write_info_log(['Database ''',Connection,'''']),
%%SAMER
%     my_odbc_get_dbms_name(Connection,DBMSName),
%     write_info_log(['Database name ''',DBMSName,'''']),
%%
    get_tablenames(Connection,Name,UnorderedTableNames),
    my_mergesort(UnorderedTableNames,TableNames),
    (TableNames \== [] ->
     (var(Name)
      -> 
       write_notapi_info_log(['Table(s):'])
      ;
       write_notapi_info_log(['Table:'])
     ),
     list_schema_list(Connection,TableNames)
     ;
     (var(Name) -> 
       (tapi(off)
        ->
         write_info_log(['No tables.'])
        ;
         true
       )
       ;
       TableNotFound=true)
    ),
    get_viewnames(Connection,Name,UnorderedViewNames),
    my_mergesort(UnorderedViewNames,ViewNames),
    (ViewNames \== [] ->
     (var(Name)
      -> 
       write_notapi_info_log(['View(s):'])
      ;
       write_notapi_info_log(['View:'])
     ),
     list_schema_list(Connection,ViewNames)
     ;
     (var(Name) -> 
       write_notapi_info_log(['No views.'])
       ;
       ViewNotFound=true)
    ),
    (nonvar(Name), TableNotFound == true, ViewNotFound == true ->
       write_error_log(['No table or view found with name ''',Name,'''.'])
       ;
       true
     ),
    (var(Name) ->
      set_flag(boolean,false),
      (
       my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,no_sql,'$no_table$'),
       (Preds=[Table/Arity], my_table('$des',Table,Arity) -> fail ; true),
       (boolean(false) -> write_info_log(['Integrity constraint(s):']) ; true),
       set_flag(boolean,true),
       Indent=3,
       write_integrity_constraint(my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,no_sql,'$no_table$'),Indent),
       fail
      ;
       true
      ),
      (boolean(false)
       ->
        write_notapi_info_log(['No integrity constraints.']) 
       ;
        true)
     ;
      true
    ),
    write_tapi_eot
   ;
    write_error_log(['Database ''',Connection,''' is not open'])
  ).

get_tablenames(Name,TableNames) :-
  current_db(Connection),
  get_tablenames(Connection,Name,TableNames).
  
% Get table names. First argument, if bound, represent a table for which it is asked if exists
% First clause deals with '$des' database   
get_tablenames('$des',Name,TableNames) :-
  my_nf_bagof(Name,
        Arity^SQLst^Lang^DLs^ODLIds^LVDs^SCs^
        (my_table('$des',Name,Arity),
         Name/Arity\==dual/0,
%         Name/Arity\=='DUAL'/0,
         Name/Arity\==select_not_null/3,
         \+ (my_view('$des',Name,Arity,SQLst,Lang,DLs,ODLIds,LVDs,SCs))),
        TableNames).
% Next clauses deal with ODBC databases
get_tablenames(Connection,Name,TableNames) :-
  var(Name),
  !,
  my_odbc_get_tablenames(Connection,DTableNames),
  opened_db(Connection,_Handle,DBMS),
  (DBMS==oracle
   ->
    remove_from_list('DUAL',DTableNames,TableNames)
   ;
    TableNames=DTableNames).
get_tablenames(Connection,Name,[Name]) :-
  my_odbc_exists_table(Connection,Name),
  !.
get_tablenames(_Connection,_Name,[]).

get_viewnames(Name,ViewNames) :-
  current_db(Connection),
  get_viewnames(Connection,Name,ViewNames).

get_viewnames('$des',Name,ViewNames) :-
  !,
  my_nf_bagof(Name,
        Arity^SQLst^Lang^DLs^ODLIds^LVDs^SCs^
        my_view('$des',Name,Arity,SQLst,Lang,DLs,ODLIds,LVDs,SCs),
        ViewNames).
% Next clauses deal with ODBC databases
get_viewnames(Connection,Name,ViewNames) :-
  var(Name),
  !,
  my_odbc_get_viewnames(Connection,ViewNames).
get_viewnames(Connection,Name,[Name]) :-
  my_odbc_exists_view(Connection,Name),
  !.
get_viewnames(_Connection,_Name,[]).

% get_localviewnames(ViewNames) :-
%   current_db('$des'),
%   !,
%   my_nf_bagof(LVDs,
%         Name^Arity^SQLst^Lang^DLs^ODLIds^SCs^
%         my_view('$des',Name,Arity,SQLst,Lang,DLs,ODLIds,LVDs,SCs),
%         ListViewNames),
%   concat_lists(ListViewNames,DViewNames),
%   remove_duplicates(DViewNames,ViewNames).
% get_localviewnames([]).


% List the schema of a list of tables
% Schema: table(col:type,...,col:type)
list_table_schema_list([]).
list_table_schema_list([TableName|TableNames]) :-
  get_table_typed_schema(TableName,Table),
  schema_to_user_schema(Table,DTable),
  write_log_list([DTable,nl]),
  list_table_schema_list(TableNames).

% List the schema of a relation
list_relation_schema(RelationName) :-
  current_db(Connection),
  list_relation_schema(Connection,RelationName).

list_relation_schema(Connection,RelationName) :-
  tapi(on),
  !,
   get_table_typed_arguments(Connection,RelationName,ColnameTypes),
   (my_view(Connection,RelationName,_Arity,_SQLst,Language,_RNVss,_ODLIds,_LVDs,_SCs)
    -> 
     to_lowercase(Language,LLanguage),
     atom_concat('$',LLanguage,AL),
     write_log_list(['$view',nl,AL,nl]) 
    ;
     write_log_list(['$table',nl]) 
   ),
   write_log_list([RelationName,nl]),
   (repeat,
     (
      (member(Colname:Type,ColnameTypes),
       internal_typename_to_user_typename(Type,DType),
       write_log_list([Colname,nl,DType,nl]),
       fail
      )
     ;
      true
     )
   ).
list_relation_schema(Connection,RelationName) :-
  get_table_typed_schema(Connection,RelationName,Relation),
  schema_to_user_schema(Relation,DRelation),
%   write_log_list([ColnameTypes,nl])
  write_log_list([DRelation,nl]).

schema_to_user_schema(Relation,DRelation) :-
  Relation=..[Name|ColnameTypeList],
  my_unzip(ColnameTypeList,ColnameList,TypeList),
  internal_typename_to_user_typename_list(TypeList,DTypeList),
  my_zipWith(':',ColnameList,DTypeList,DColnameTypeList),
  DRelation=..[Name|DColnameTypeList].

% list_schema_list(TableNames) :-
%   current_db(Connection),
%   list_schema_list(Connection,TableNames).

list_schema_list(Connection,TableNames) :-
  opened_db(Connection,_Handle,DBMS),
  push_flag(current_db,[Connection,DBMS],OldFlag),
  list_schema_list_aux(Connection,TableNames),
  pop_flag(current_db,OldFlag).
  
list_schema_list_aux(_Connection,[]).
list_schema_list_aux(Connection,[TableName|TableNames]) :-
%  get_table_typed_schema(TableName,Table),
  (Connection == '$des'
   ->
    (my_view(Connection,TableName,_Arity,Query,Language,RNVss,_,_,_)
     ->
      Type = view
     ;
      Type = table
    )
   ;
    (view_arity(Connection,TableName,Arity),
     my_view(Connection,TableName,Arity,Query,Language,RNVss,_,_,_)
     ->
      Type = view
     ;
      Type = table
    )
  ),
%  write_notapi_log_list([' * ',Table,nl]), 
  write_notapi_log_list([' * ']), 
  list_relation_schema(Connection,TableName),
  (tapi(on)
   ->
   Indent=0
   ;
   Indent=8
  ),
  (Type==view
   -> 
    write_tapi_delimiter,
    (Language == sql
     ->
      write_notapi_log_list(['    - Defining SQL statement:',nl]),
      display_sql(Query,Indent,'$des')
     ;
      (Language == ra
       ->
        write_notapi_log_list(['    - Defining RA statement:',nl]),
        display_ra(Query,Indent)
       ;
        (Language == drc
         ->
          write_notapi_log_list(['    - Defining DRC statement:',nl]),
          display_drc(Query,Indent)
         ;
          (Language == trc
           ->
            write_notapi_log_list(['    - Defining TRC statement:',nl]),
            display_trc(Query,Indent)
           ;
            true
          )
        )
%         (Language == hrsql
%          ->
%           write_notapi_log_list(['    - Relation definition:',nl]),
%           display_sql(Query,Indent,hrsql)
%          ;
%           true
%         )
       )
     )
   ,
   write_tapi_delimiter,
%   ((development(off) ; Language\==datalog)
   (development(off)
    ->
     ORNVss=RNVss
    ;
     get_object_dlrules_list(RNVss,ODLs),
     dlrule_to_ruleNVs_list(ODLs,ORNVss)
   ),
   (ORNVss \== []
    ->
     write_notapi_log_list(['    - Datalog equivalent rules:',nl]),
     display_ruleNVs_list(ORNVss,Indent)
    ;
     true
   )
%    ,
%    (LVDs \== [] ->
%      write_notapi_log_list(['    - Local view definitions:',nl,'        ',LVDs,nl])
%     ;
%      true
%    )
   ; 
   true
  ),
  write_tapi_delimiter,
  list_table_constraints(Connection,TableName),
  list_schema_list_aux(Connection,TableNames).

list_table_constraints(TableName) :-
  current_db(Connection),
  list_table_constraints(Connection,TableName).

list_table_constraints('$des',TableName) :-
  !,
  ((my_not_nullables('$des',TableName,NNAtts),
    (tapi(off) 
     ->
      write_log_list(['    - NN: ',NNAtts,nl])
     ;
      write_log_list([NNAtts,nl])
    ),
    fail)% To undo bindings in NNAtts
   ;
   true),
  !,
  write_tapi_delimiter,
  (my_primary_key('$des',TableName,Atts),
   (tapi(off) 
    ->
     write_log_list(['    - PK: ',Atts,nl])
    ;
     write_log_list([Atts,nl])
   ),
   fail % To undo bindings in Atts
   ;
   true),
  write_tapi_delimiter,
  ((my_candidate_key('$des',TableName,CKAtts),
   (tapi(off) 
    ->
     write_log_list(['    - CK: ',CKAtts,nl])
    ;
     write_log_list([CKAtts,nl])
    ),
    fail)
   ;
   true),
  write_tapi_delimiter,
  ((my_foreign_key('$des',TableName,FKAtts,FTableName,RFKAtts,_RIds),
    (tapi(off) 
     ->
      write_log_list(['    - FK: ',TableName,'.',FKAtts,' -> ',FTableName,'.',RFKAtts,nl])
%      , write_verb(['$exec'(display_ruleNVs_list(RNVss,10))]) % For limited domain predicates
     ;
      write_log_list([TableName,'.',FKAtts,' -> ',FTableName,'.',RFKAtts,nl])
    ),
    fail)
   ;
   true),
  write_tapi_delimiter,
  ((my_functional_dependency('$des',TableName,Atts,DepAtts),
    (tapi(off) 
     ->
      write_log_list(['    - FD: ',Atts,' -> ',DepAtts,nl])
     ;
      write_log_list([Atts,' -> ',DepAtts,nl])
    ),
    fail)
   ;
   true),
  write_tapi_delimiter,
  ((my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,SQL,DefTableName),
    my_table('$des',TableName,Arity),
    (DefTableName = TableName ; Preds=[TableName/Arity], DefTableName=='$no_table$'),
    (tapi(off) 
     ->
      write_log_list(['    - IC:',nl]),
      write_integrity_constraint(my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,SQL,TableName),6)
     ;
      write_integrity_constraint(my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,SQL,TableName),0)
    ),
    fail)
   ;
   true).   
list_table_constraints(_Connection,_TableName). % TODO: External DBMSs

write_integrity_constraint(IC) :-
  write_integrity_constraint(IC,0).
   
write_integrity_constraint(my_integrity_constraint('$des',_Preds,Constraint,NVs,_Head,_Ids,_SQL,_TableName),_I) :-
  tapi(on),
  !,
  push_flag(pretty_print,off,PP),
  write_datalog_rule((':-'(Constraint),NVs),0),
  nl_log,
  pop_flag(pretty_print,PP).
write_integrity_constraint(my_integrity_constraint('$des',_Preds,Constraint,NVs,_Head,Ids,SQL,_TableName),I) :-
    (development(off)
     ->
      (SQL==no_sql 
       ->
        %write_log_list(['$tab'(I)]),
        write_datalog_rule((':-'(Constraint),NVs),I),
        nl_log
       ;
        write_log_list(['$tab'(I)]),
        write_sql_cond(SQL,I,'$des'),
        nl_log
      )
     ;
      (SQL==no_sql
       ->
        I1=I
%        list_rules_from_head_wo_number(Head,I1)
       ;
        write_log_list(['$tab'(I),'+ SQL Check:',nl]),
        I1 is I+2,
        write_log_list(['$tab'(I1)]),
        write_sql_cond(SQL,I1,'$des'),
        nl_log,
        write_log_list(['$tab'(I),'+ Datalog Check:',nl])
      ),
      %list_rules_from_head_wo_number(Head,I1),
      display_dlrules_by_ids(Ids,I1)       
    ).

   
get_table_untyped_schema(TableName,Table) :-
  current_db(Connection),
  get_table_untyped_schema(Connection,TableName,Table).
  
get_table_untyped_schema(Connection,TableName,Table) :-
  get_table_untyped_arguments(Connection,TableName,Colnames),
  Table =.. [TableName|Colnames].
  
get_table_typed_schema(TableName,Table) :-
  current_db(Connection),
  get_table_typed_schema(Connection,TableName,Table).

get_table_typed_schema(Connection,TableName,Table) :-
  my_table(Connection,TableName,Arity),
  get_table_typed_arguments(Connection,TableName,ColnamesTypes),
  length(ColnamesTypes,Arity),
  Table =.. [TableName|ColnamesTypes].

get_table_untyped_arguments(TableName,Colnames) :-
  current_db(Connection),
  get_table_untyped_arguments(Connection,TableName,Colnames).
  
get_table_untyped_arguments('$des',TableName,Colnames) :-
  !,
  my_nf_setof((Pos,Colname),Type^my_attribute('$des',Pos,TableName,Colname,Type),PosColnames),
  findall(Colname,(member((Pos,Colname),PosColnames)),Colnames).
%  my_nf_bagof(Colname,Pos^(member((Pos,Colname),PosColnames)),Colnames).
get_table_untyped_arguments(Connection,TableName,Colnames) :-
  my_odbc_get_colnames(Connection,TableName,Colnames).
get_table_untyped_arguments(_Connection,TableName,Colnames) :-
%  Connection\=='$des',
  des_sql_solving(on),
  get_table_untyped_arguments('$des',TableName,Colnames).
  
get_table_typed_arguments(TableName,ColnameTypes) :-
  current_db(Connection),
  get_table_typed_arguments(Connection,TableName,ColnameTypes).

get_table_typed_arguments('$des',TableName,ColnameTypes) :-
  !,
  my_nf_setof((Pos,Colname,Type),Type^my_attribute('$des',Pos,TableName,Colname,Type),PosColnameTypes),
  findall(Colname:Type,(member((Pos,Colname,Type),PosColnameTypes)),ColnameTypes).
get_table_typed_arguments(Connection,TableName,ColnameTypes) :-
  my_odbc_get_table_typed_arguments(Connection,TableName,ColnameTypes).
% get_table_typed_arguments(_Connection,TableName,ColnameTypes) :-
%   des_sql_solving(on),
%   get_table_typed_arguments('$des',TableName,ColnameTypes).
  
get_table_types(TableName,TypeNames) :-
  current_db(Connection),
  get_table_types(Connection,TableName,TypeNames).

get_table_types('$des',TableName,TypeNames) :-
  !,
  my_table('$des',TableName,_Arity) ->
  my_nf_setof((Pos,Colname,Type),Type^my_attribute('$des',Pos,TableName,Colname,Type),PosColnameTypes),
  findall(Type,(member((Pos,Colname,Type),PosColnameTypes)),TypeNames).
get_table_types(Connection,TableName,TypeNames) :-
  my_odbc_get_table_typenames(Connection,TableName,TypeNames).

get_relation_arity(Relation,Arity) :-
  current_db(Connection),
  get_relation_arity(Connection,Relation,Arity).
  
get_relation_arity('$des',Relation,Arity) :-
  !,
  my_table('$des',Relation,Arity). % Both tables and views
get_relation_arity(Connection,Relation,Arity) :-
  my_odbc_get_table_arity(Connection,Relation,Arity).
  
attr_internal_representation(ColName,attr(_R,ColName,_Type)).

attr_internal_representation_list([],[]). 
attr_internal_representation_list([ColName|ColNames],[Attr|Attrs]) :-
  attr_internal_representation(ColName,Attr),
  attr_internal_representation_list(ColNames,Attrs).
%% List constraint rules %%

list_constraint_rules :-
  list_type_rules,
  list_nn_rules,
  list_pk_rules,
  list_ck_rules,
  list_fk_rules,
  list_fd_rules,
  list_ic_rules.
  
list_type_rules :-
  get_relationnames(TableNames),
  my_nf_setof(
    [':-'(type(Schema)),'.',nl], 
    TableName^IRTableTypes^ColumnName^ColumnType^IRColumnType^TypedColumns^TypedColumns^
    (
    member(TableName,TableNames),
    get_table_typed_arguments(TableName,IRTableTypes),
    findall(ColumnName:ColumnType,
            (
             member(ColumnName:IRColumnType,IRTableTypes),
             ((type_equivalence(IRColumnType,ColumnType,_)  % From a Datalog declaration
              ;
               type_equivalence(IRColumnType,_,ColumnType)  % From an SQL declaration
              ;
               IRColumnType=ColumnType)                     % From an external ODBC datasource (unlisted type)
              -> true ; fail)
            ),
            TypedColumns),
    Schema=..[TableName|TypedColumns]
    ),
    TypeCtrs),
  my_map_1(write_quoted_log_list,TypeCtrs).
  
list_nn_rules :-
  my_nf_setof([':-'(nn(TableName,Atts)),'.',nl], my_not_nullables('$des',TableName,Atts), NNs),
  my_map_1(write_quoted_log_list,NNs).

list_pk_rules :-
  my_nf_setof([':-'(pk(TableName,Atts)),'.',nl], my_primary_key('$des',TableName,Atts), PKs),
  my_map_1(write_quoted_log_list,PKs).

list_ck_rules :-
  my_nf_setof([':-'(ck(TableName,Atts)),'.',nl], my_candidate_key('$des',TableName,Atts), CKs),
  my_map_1(write_quoted_log_list,CKs).

list_fk_rules :-
  my_nf_setof([':-'(fk(TableName,FKAtts,FTableName,RFKAtts)),'.',nl], RIds^my_foreign_key('$des',TableName,FKAtts,FTableName,RFKAtts,RIds), FKs),
  my_map_1(write_quoted_log_list,FKs).

list_fd_rules :-
  my_nf_setof([':-'(fd(TableName,Atts,DepAtts)),'.',nl], my_functional_dependency('$des',TableName,Atts,DepAtts), FDs),
  my_map_1(write_quoted_log_list,FDs).

list_ic_rules :-
  my_nf_setof(my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,SQL,Table), my_integrity_constraint('$des',Preds,Constraint,NVs,Head,Ids,SQL,Table), ICs),
  my_map_1(write_integrity_constraint,ICs).

  
% For ODBC connections:
% MS Access do not accept "drop view". Instead, it uses "drop table"
rewrite_sql_statement(Connection,SQLstr,RSQLstr) :-
  my_odbc_get_dbms(Connection,access),
  my_guessed_ddl_statement('View dropped',SQLstr,_),
  !,
  replace_str_first("VIEW","TABLE",SQLstr,RSQLstr).
%nl,write_string_log(SQLstr),nl.  
rewrite_sql_statement(_Connection,SQLstr,SQLstr).

% GNU Prolog does not support user-defined DCG expansion
% Also, it does not support a variable as a literal in a DCG head
% % replace_str
% replace_str_first(Old,New,Str,RStr) :-
%   phrase(replace(id,Old,New,first),Str,RStr).

% replace_str_all(Old,New,Str,RStr) :-
%   phrase(replace(id,Old,New,all),Str,RStr).

% % replace_kw
% replace_kw_first(Old,New,Str,RStr) :-
%   phrase(replace(my_kw,Old,New,first),Str,RStr).
%   
% replace_kw_all(Old,New,Str,RStr) :-
%   phrase(replace(my_kw,Old,New,all),Str,RStr).
%   
% replace(_Pattern,_Old,_New,_All) -->
%   call(eos), 
%   !.
% replace(Pattern,Old,New,all), New -->
%   ho_dcg(Pattern,[Old]),
% %  my_kw(Old),
%   !,
%   replace(Pattern,Old,New,all).
% replace(Pattern,Old,New,first), New -->
%   ho_dcg(Pattern,[Old]),
% %  my_kw(Old),
%   !.
% replace(Pattern,Old,New,All), [C] -->
%   [C],
%   replace(Pattern,Old,New,All).

% eos([], []). 

% ho_dcg(Function,Arguments) -->
%   {F=..[Function|Arguments]},
%   F.
%   
% id(F) -->
%   F.

% So, we resort to this other formulation or replace_kw_first

replace_str_first(OldStr,NewStr,InStr,OutStr) :-
  replace_str_first(OldStr,NewStr,OutStr,InStr,[]).

replace_str_first(_OldStr,_NewStr,[]) -->
  [].
% Ciao does not allow variables as a DCG literal:
% So, instead using:
% replace_str_first(OldStr,NewStr,OutStr) -->
%   OldStr,
%   !,
%   replace_str_first(OldStr,OldStr,TailStr),
%   {append(NewStr,TailStr,OutStr)}.
% Use its compiled form:
replace_str_first(A, B, C, D, E) :-
  phrase(A, D, F), 
  !,
  replace_str_first(A, A, G, F, H),
  append(B, G, C),
  E=H.
replace_str_first(OldStr,NewStr,[C|OutStr]) -->
  [C],
  replace_str_first(OldStr,NewStr,OutStr).
  
replace_str_all(OldStr,NewStr,InStr,OutStr) :-
  replace_str_all(OldStr,NewStr,OutStr,InStr,[]).
  
replace_str_all(_OldStr,_NewStr,[]) -->
  [].
% Ciao does not allow variables as a DCG literal:
% So, instead using:
% replace_str_all(OldStr,NewStr,OutStr) -->
%   OldStr,
%   !,
%   replace_str_all(OldStr,NewStr,TailStr),
%   {append(NewStr,TailStr,OutStr)}.
% Use its compiled form:
replace_str_all(A, B, C, D, E) :-
  phrase(A, D, F), 
  !,
  replace_str_all(A, B, G, F, H),
  append(B, G, C),
  E=H.
replace_str_all(OldStr,NewStr,[C|OutStr]) -->
  [C],
  replace_str_all(OldStr,NewStr,OutStr).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sql_to_dl(+Lang,+SQLst,-Schema,-TableRen,-DLsts) 
% Translates an SQL Syntactic Tree
% (which can come from either an SQL or an RA statement)
% into a list of Datalog Syntactic Trees
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sql_to_dl(Lang,SQLst,Schema,TableRen,Rs) :-
  to_uppercase(Lang,LM),
  write_info_verb_log(['Compiling ',LM,' statement to Datalog rules...']),
  (Lang==ra -> push_flag(des_sql_solving,on,OldValue) ; true),
  sql_to_ra1(SQLst,RAst,[],_Ren),
  (RAst = (_RA,Schema) ; true),
  ra_to_cra(RAst,CRAst),
  !,
%  show_sql_cra(CRAst),
  cra_to_dl(CRAst,0,_,[],_OMap,[],TableRen,DDLsts),
  no_input_arguments_list(DDLsts,IArgsListi),
  rule_to_ruleNVs_list(DDLsts,[],DRuleNVsList),
  disjunctive_to_conjunctive_ruleNVs_list(DRuleNVsList,CRuleNVsList,IArgsListi,_IArgsListo,_Exploded),
  ruleNVs_to_rule_list(CRuleNVsList,CRs),
  force_simplify_rules(CRs,SRs,_Simplified),
%   drop_false_rules(SRs,DRs),
  reorder_goals_by_efficiency_rule_list(SRs,FRs),
  unfold_rules(FRs,URs),
  safe_top_calls_rule_list(URs,Rs),
  (Lang==ra -> pop_flag(des_sql_solving,OldValue) ; true),
  !,
  write_info_verb_log([LM,' statement successfully compiled.']).

% drop_false_rules([],[]).
% drop_false_rules([(_:-false)|Rs],DRs) :-
%   !,
%   drop_false_rules(Rs,DRs).
% drop_false_rules([R|Rs],[R|DRs]) :-
%   drop_false_rules(Rs,DRs).
  
  
sql_to_ra1((SQLst,[RR|RArgs]),RAst,IRen,ORen) :-
  nonvar(RArgs),
  !,
  get_renamings_schema(RArgs,Renamings1),
  sql_to_ra((SQLst,[RR|_]),(RRAst,[RR|Args]),IRen,ORen),
  get_renamings_schema(Args,Renamings2),
  check_column_renamings(Renamings1,Renamings2),
  replace_functors(Renamings2,Renamings1,(RRAst,[RR|Args]),RAst).
sql_to_ra1((SQLst,[RR|RArgs]),RAst,IRen,ORen) :-
  sql_to_ra((SQLst,[RR|RArgs]),RAst,IRen,ORen),
  !.
sql_to_ra1(SQLst,RAst,IRen,ORen) :-
  sql_to_ra((SQLst,[_RR|_RArgs]),RAst,IRen,ORen).
    
check_column_renamings(R1,R2) :-
  length(R1,L1),
  length(R2,L2),
  (L1==L2
   ->
    remove_duplicates(R1,RR1),
    (length(RR1,L1)
     -> 
      true
     ;
      duplicates_in_list(R1,Ds),
      my_raise_exception(generic,syntax(['Duplicated column names: ',Ds,'.']),[])
    )
   ;
    my_raise_exception(generic,syntax(['The number of columns in the renaming (',L1,') does not match with those of the relation (',L2,').']),[])
  ).
  
duplicates_in_list(Cs,Ds) :-
  my_remove_duplicates_sort(Cs,SCs),
  my_bag_diff(Cs,SCs,DDs),
  my_remove_duplicates(DDs,Ds).

sql_to_ra1_list([],[],Ren,Ren).
sql_to_ra1_list([(R,Schema)|Rs],[AR|ARs],IRen,ORen) :-
  !,
  Schema=..[N|_],
  sql_to_ra1((R,[N|_]),AR,IRen,TRen),
  sql_to_ra1_list(Rs,ARs,TRen,ORen).
sql_to_ra1_list([R|Rs],[AR|ARs],IRen,ORen) :-
  sql_to_ra1(R,AR,IRen,TRen),
  sql_to_ra1_list(Rs,ARs,TRen,ORen).

% Safe Top-N calls ensures that calls to predicate argument are open
% - A shared variable between a top goal and the head must be postponed after this goal, 
%   but for the answer predicate, which is always open
% - A shared variable between two top goals must be postponed after the second goal
% An equality built-in '$eq'(X,X) is used to prevent simplification in a further stage
safe_top_calls_rule_list([],[]).
safe_top_calls_rule_list([R|Rs],[SR|SRs]) :-
  safe_top_calls_rule(R,SR),
  safe_top_calls_rule_list(Rs,SRs).

% Facts (do nothing):
safe_top_calls_rule(R,R) :-
  R \= ':-'(_,_),
  !.
% Answer:
safe_top_calls_rule(':-'(H,B),':-'(H,SB)) :-
  (functor(H,'$p0',_)
   ->
    Vs=[]
   ;
    term_variables(H,Vs)),
  my_list_to_tuple(Bs,B),
  safe_top_calls_rule(Bs,Vs,_Vso,SBs),
  my_list_to_tuple(SBs,SB).
  
safe_top_calls_rule([],Vs,Vs,[]).
safe_top_calls_rule([top(N,G)|Gs],Vsi,Vso,[top(N,SG)|SGs]) :-
  term_variables(G,GVs),
  my_set_inter(GVs,Vsi,UVs),
  (UVs \== []
   ->
    length(UVs,L),
    length(NVs,L),
    my_zipWith('$eq',UVs,NVs,Eqs),
    replace_term_list(UVs,NVs,G,SG),
    append(Vsi,GVs,Vs1),
    safe_top_calls_rule(Gs,Vs1,Vso,RGs),
    append(Eqs,RGs,SGs)
   ;
    append(Vsi,GVs,Vs1),
    SG=G,
    safe_top_calls_rule(Gs,Vs1,Vso,SGs)).
safe_top_calls_rule([G|Gs],Vsi,Vso,[G|SGs]) :-
  safe_top_calls_rule(Gs,Vsi,Vso,SGs).

  
% show_sql_cra(CRA) :-
%   language(sql),
%   show_ra(on),
%   !,
%   cra_to_ra(CRA,RA),
%   write_info_log(['Equivalent RA query:']),
%   nl_compact_log,
%   display_ra(RA,0),
%   nl_compact_log.
% show_sql_cra(_CRA).

% cra_to_ra((R,[RelName|Attrs]),rename(Schema,RR)) :-
%   !,
%   cra_to_ra(R,RR),
%   attr_renamings(Attrs,RAttrs),
%   Schema=..[RelName|RAttrs].
% cra_to_ra(sigma(true,R),RRA) :-
%   !,
%   cra_to_ra(R,RRA).
% cra_to_ra(sigma(C,R),select(RC,RRA)) :-
%   !,
%   cra_to_ra(R,RRA),
%   cra_cond_to_ra_cond(C,RC).
% cra_to_ra(pi(D,T,PL,R,G,H,O),RA) :-
%   !,
%   cra_to_ra(R,R1),
% %  R2=project(PL,R1),
%   cra_to_ra(distinct(D),R2,R3),
%   cra_to_ra(O,R3,R4),
%   cra_to_ra(T,R4,R5),
%   cra_to_ra(G,H,PL,R1,R2,R5,RA).
% cra_to_ra(union(D,[L,R]),R1) :-
%   !,
%   cra_to_ra(distinct(D),union(RL,RR),R1),
%   cra_to_ra(L,RL),
%   cra_to_ra(R,RR).
% cra_to_ra(times(L,R),product(RL,RR)) :-
%   !,
%   cra_to_ra(L,RL),
%   cra_to_ra(R,RR).
% cra_to_ra(CRA,RA) :-
%   CRA =.. [Op,L,R,C],
%   ra_sql_join_operator(Op,ROp),
%   !,
%   RA =.. [ROp,RC,RL,RR],
%   cra_to_ra(L,RL),
%   cra_to_ra(R,RR),
%   cra_cond_to_ra_cond(C,RC).
% % Natural full join
% cra_to_ra(full_join(L,R,C,_Cs),RA) :-
%   cra_to_ra(full_join(L,R,C),RA).
% cra_to_ra(RA,R1) :-
%   RA =.. [Op,D,L,R],
%   ra_sql_set_operator(Op,ROp),
%   !,
%   E2 =.. [ROp,RL,RR],
%   cra_to_ra(distinct(D),E2,R1),
%   cra_to_ra(L,RL),
%   cra_to_ra(R,RR).  
% cra_to_ra(R,R).

% % infix operator for relations: set operators (union, difference, intersect), product and njoin
% cra_to_ra(distinct(all),R,R) :-
%   !.
% cra_to_ra(distinct(distinct),R,distinct(R)) :-
%   !.
% cra_to_ra(top(all),R,R) :-
%   !.
% cra_to_ra(top(N),R,top(N,R)) :-
%   !.
% cra_to_ra(order_by([],[]),R,R) :-
%   !.
% cra_to_ra(order_by(OL,CL),R,sort(OL,CL,R)) :-
%   !.

% cra_to_ra(group_by([]),having(true),PL,R1,project(RPL,R1),R,R) :-
%   !,
% %   pl_to_rpl(PL,RPL).
%   cra_cond_to_ra_cond(PL,RPL).
% cra_to_ra(group_by(G),having(H),PL,R1,R1,R,group_by(G,RPL,H,R)) :-
%   !,
% %   pl_to_rpl(PL,RPL).
%   cra_cond_to_ra_cond(PL,RPL).
%   
% ra_sql_set_operator(union,union).
% ra_sql_set_operator(minus,difference).
% ra_sql_set_operator(intersect,intersect).

% ra_sql_join_operator(inner_join,zjoin).
% ra_sql_join_operator(left_join,ljoin).
% ra_sql_join_operator(right_join,rjoin).
% ra_sql_join_operator(full_join,fjoin).

% attr_renamings([],[]).
% attr_renamings([attr(_,R1,R)|Attrs],[R|RAttrs]) :-
%   \+ is_system_identifier(R1),
%   !,
%   attr_renamings(Attrs,RAttrs).
% attr_renamings([attr(_,R1,_R)|Attrs],[R1|RAttrs]) :-
%   attr_renamings(Attrs,RAttrs).
% attr_renamings([expr(_,R,_)|Attrs],[R|RAttrs]) :-
%   attr_renamings(Attrs,RAttrs).
% % attr_renamings(Attrs,RAttrs) :-
% %   replace_pattern_term(attr(T,_,R),attr(T,R,R),true,Attrs,RAttrs).
%   
% % pl_to_rpl([],[]).
% % pl_to_rpl([attr(_,_,R)|PL],[R|RPL]) :-
% %   !,
% %   pl_to_rpl(PL,RPL).
% % pl_to_rpl([R|PL],[R|RPL]) :-
% %   pl_to_rpl(PL,RPL).
%   
% cra_cond_to_ra_cond(C,RC) :-
% %  replace_pattern_term(attr(T,R1,R),attr(T,R,R),(\+ is_system_identifier(R1)),C,RC).
%   replace_pattern_term(attr(T,_,R),attr(T,R,R),true,C,RC).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sql_to_ra(+SQLst,-RAst,+IRen,-ORen) 
% Translates an SQL Syntactic Tree (SQLST)
% into an (Extended) Relational Algebra Syntactic Tree (RAST)
% Table and subquery autorenaming is done for unrenamed 
% tables and subqueries. All renamings are annotated in ORen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SQLst ::=
%   (
%    not(SQLst),   % For negative assumptions
%    Renaming
%   )
%
%   |
%
%   (
%    with(SQLst,SQLsts),
%    Renaming
%   )
%
%   |
%
%   (
%    select(AllDistinct, 
%           TopN,
%           Args,
%           from(Rels),
%           where(Cond),
%           group_by(Cols),
%           having(Cond),
%           order_by(Cols,OrderSpecs)
%          ),
%    Renaming
%   )
%   |
%   (
%    union(AllDistinct,SQL,SQL),
%    Renaming
%   )
%   |
%   (
%    except(_,SQL,SQL),
%    Renaming
%   )
%   |
%   (
%    intersect(_,SQL,SQL),
%    Renaming
%   )
%
% AllDistinct ::=
%   all
%   |
%   distinct
%
% Args ::=
%   *
%   |
%   [Arg,...,Arg]
%
% Arg ::=
%   attr(RelName,Attr,Renaming)
%   |
%   expr(Expression,Renaming,Type)
%
% Expression ::=
%   Arithmetic expression
%   |
%   SQLstmt
%
% Rels ::=
%   [Rel,...,Rel]
%
% Rel ::=
%   (Table,Renaming)
%   |
%   SQLst
%   |
%   JoinOp(Rel,Rel,JoinCond)
%
% JoinCond ::=
%   Cond
%   |
%   equijoin(natural)
%   |
%   equijoin([Attr,...,Attr])
%
% JoinOp ::=
%   inner_join
%   |
%   left_join
%   |
%   right_join
%   |
%   full_join
%
% Cond ::=
%   exists(SQLst)
%   |
%   in([AttrCte,...,AttrCte],SQLst)
%   |
%   not_in([AttrCte,...,AttrCte],SQLst)
%   |
%   AttrCte Operator AttrCte 
%   |
%   AttrCte Operator SQLst
%   |
%   SQLst Operator AttrCte
%   |
%   SQLst Operator SQLst
%   |
%   and(Cond,Cond)
%   |
%   or(Cond,Cond)
%   |
%   not(Cond)
%   |
%   true
%   |
%   false

sql_to_ra((not(SQLst),Schema),(not(AR),Schema),IRen,ORen) :-
  !,
  sql_to_ra((SQLst,Schema),(AR,Schema),IRen,ORen).
sql_to_ra((with(R,Rs),[RR|RAS]),(with(AR,ARs),[RR|RAS]),IRen,ORen) :-
  !,
  sql_to_ra1(R,AR,IRen,Ren),
  sql_to_ra1_list(Rs,ARs,Ren,ORen),
  relation_autorenaming(RR),
  relation_arguments(AR,RAS),
  arguments_autorenaming(RR,RAS).
%sql_to_ra((union(D,R1,R2),[RR|RAS]),(union(D,AR1,AR2),[RR|RAS]),IRen,ORen) :-
sql_to_ra((union(D,R1,R2),[RR|RAS]),(union(D,AR1,AR2),[RR|UAS]),IRen,ORen) :-
  !,
  sql_to_ra1(R1,AR1,IRen,Ren),
  sql_to_ra1(R2,AR2,Ren,ORen),
  compatible_schemas(AR1,AR2),
  relation_autorenaming(RR),
  relation_arguments(AR1,RAS),
  void_proj_list(RAS,UAS),
  arguments_autorenaming(RR,RAS).
sql_to_ra((except(D,R1,R2),[RR|RAS]),(minus(D,AR1,AR2),[RR|RAS]),IRen,ORen) :-
  !,
  sql_to_ra1(R1,AR1,IRen,Ren),
  sql_to_ra1(R2,AR2,Ren,ORen),
  compatible_schemas(AR1,AR2),
  relation_autorenaming(RR),
  relation_arguments(AR1,RAS),
  arguments_autorenaming(RR,RAS).
sql_to_ra((intersect(D,R1,R2),[RR|RAS]),(intersect(D,AR1,AR2),[RR|RAS]),IRen,ORen) :-
  !,
  sql_to_ra1(R1,AR1,IRen,Ren),
  sql_to_ra1(R2,AR2,Ren,ORen),
  compatible_schemas(AR1,AR2),
  relation_autorenaming(RR),
  relation_arguments(AR1,RAS),
  arguments_autorenaming(RR,RAS).
sql_to_ra((select(DistinctAll,
            TopN,
            UProjList,
            from(SQLRelations),
            where(WhereCondition),
            group_by(GroupList),
            having(HavingCondition),
            order_by(UOrderArgs,OrderSpecs)),
           [RR|_RArgs]),
           (pi(DistinctAll,
               TopN,
               SProjList,
               sigma(RACondition,RARelation),
               group_by(GroupList),
               having(RAHavingCondition),
               order_by(SOrderArgs,OrderSpecs)),
           [RR|SProjList]),
           IRen,ORen) :-
  !,
  sql_cond_to_ra_cond(where,WhereCondition,URACondition,IRen,Ren11),
  sql_cond_to_ra_cond(having,HavingCondition,RAHavingCondition,Ren11,Ren1),
  sql_rel_to_ra(SQLRelations,RARelation,Ren1,Ren2),
  check_relation_renamings(Ren2),
  relation_autorenaming(RR),
  simplify_arglist_expr(UProjList,SSProjList),
  arguments_completion(SSProjList,project,RARelation,SSProjList,Ren2,Ren3,ProjList),
  arguments_completion(UOrderArgs,order,RARelation,SSProjList,Ren3,Ren4,OrderArgs),
  arguments_autorenaming(RR,ProjList), 
  expr_argument_completion(URACondition,sigma,RARelation,SSProjList,Ren4,ORen,RACondition),
%  arguments_autorenaming(_GR,GroupList),
%  arguments_autorenaming(_OR,OrderArgs),
  simplify_arglist_expr(ProjList,SProjList),
  simplify_arglist_expr(OrderArgs,SOrderArgs).
% sql_to_ra(SQLst,RAst,IRen,ORen) :-
%   SQLst \= (_,_),
%   sql_to_ra((SQLst,[_RR|_RArgs]),RAst,IRen,ORen).

check_relation_renamings(Ren) :-
  filter_system_renamings(Ren,FRen),
  extract_duplicates_var(FRen,Ds),
  (Ds==[]
   ->
    true
   ;
    my_raise_exception(generic,syntax(['Duplicated relation renamings: ',Ds,'.']),[])
  ).
  
filter_system_renamings([],[]).
filter_system_renamings([(A,B)|Rs],FRs) :-
  (is_system_identifier(A) ; is_system_identifier(B)),
  !,
  filter_system_renamings(Rs,FRs).
filter_system_renamings([R|Rs],[R|FRs]) :-
  filter_system_renamings(Rs,FRs).

void_proj_list([],[]).
void_proj_list([Arg|Args],[VArg|VArgs]) :-
  void_arg(Arg,VArg),
%  argument_autorenaming(Ren),
  void_proj_list(Args,VArgs).

void_arg(attr(R,C,Ren),attr(R,C,Ren)) :- !.
void_arg(expr(expr_ref(Rel,AS),AS,T),expr(expr_ref(Rel,AS),AS,T)) :- !.
void_arg(expr(_,Ren,Type),expr(expr_ref(_,Ren),Ren,Type)) :- !.
%void_arg(expr(_,Ren,_Type),attr(_,Ren,Ren)) :- !.
void_arg(A,A).

schema_arguments((_,Args),Args).

compatible_schemas((_SQLst1,S1),(_SQLst2,S2)) :-
  length(S1,L),
  length(S2,L),
  !.
compatible_schemas(_,_) :-
  my_raise_exception(generic,syntax(['Incompatible schemas in set operation.']),[]).
  
% % WARNING: This does apparently nothing:
% union_schema([],[]).
% union_schema([attr(T,C,R)|As],[attr(T,C,R)|RAs]) :-
%   !,
%   union_schema(As,RAs).
% union_schema([expr(_,C,_)|As],[expr(_,C,_)|RAs]) :-
%   !,
%   union_schema(As,RAs).
% union_schema([_|As],[_|RAs]) :-
%   union_schema(As,RAs).

build_ren_arguments(_T,[],[]).
build_ren_arguments(T,[A|As],[attr(T,A,_RA)|RAs]) :-
  build_ren_arguments(T,As,RAs).

relation_arguments((_R,[_RR|RAS]),RAS).

relation_name((_R,[RR|_RAS]),RR).

relation_autorenaming(AS) :-
  (var(AS)
   ->
    get_new_predicate_name(t,AS)
   ;
    true).

get_renamings_schema([],[]).
get_renamings_schema([Arg|Args],[Renaming|Renamings]) :-
  (Arg=attr(_,_,Renaming)
  ;
   Arg=expr(_,Renaming,_)),
  get_renamings_schema(Args,Renamings).
  
arguments_autorenaming(Rel,RArgs) :-
  arguments_autorenaming1(Rel,_Renamings,RArgs).
  
arguments_autorenaming(Rel,Args,RArgs) :-
  get_renamings_schema(Args,Renamings),
  !,
  arguments_autorenaming1(Rel,Renamings,RArgs).
   
arguments_autorenaming1(_RR,[],[]) :-
  !.
arguments_autorenaming1(Rel,[AS|Renamings],[expr(E,AS,_Type)|Args]) :-
  argument_autorenaming(AS),
  !,
  expr_arguments_autorenaming(E),
  arguments_autorenaming1(Rel,Renamings,Args).
arguments_autorenaming1(Rel,[AS|Renamings],[attr(Rel,_A,AS)|Args]) :-
  argument_autorenaming(AS),
  !,
  arguments_autorenaming1(Rel,Renamings,Args).
arguments_autorenaming1(RR,[AS|Renamings],[attr(_Rel,_A,AS)|Args]) :-
  argument_autorenaming(AS),
  !,
  arguments_autorenaming1(RR,Renamings,Args).
arguments_autorenaming1(RR,[_AS|Renamings],[_|Args]) :-
  arguments_autorenaming1(RR,Renamings,Args).

expr_arguments_autorenaming(E) :-
  var(E),
  !.
expr_arguments_autorenaming(E) :-
  (number(E) ; atomic(E)),
  !.
expr_arguments_autorenaming(expr_ref(_Rel,_AS)) :- 
  !.
expr_arguments_autorenaming(attr(_Rel,_A,AS)) :- 
  !,
  argument_autorenaming(AS).
expr_arguments_autorenaming(E) :- 
  E =.. [_F|Args],
  !, 
  expr_arguments_autorenaming_list(Args).

expr_arguments_autorenaming_list([]) :-
  !.
expr_arguments_autorenaming_list([E|Es]) :-
  !, 
  expr_arguments_autorenaming(E), 
  expr_arguments_autorenaming_list(Es).

argument_autorenaming(AS) :-
  (var(AS) ->
   get_new_predicate_name(a,AS)
   ;
   true).

rel_arguments_completion(R,_Arity,AS,RArgs,ProjList) :-
  var(RArgs),
  !,
  my_nf_bagof(attr(AS,C,CRen),
             I^Type^(my_attribute('$des',I,R,C,Type),
                     argument_autorenaming(CRen)),
             ProjList).        
rel_arguments_completion(R,Arity,AS,RArgs,ProjList) :-
  check_syntax_arguments_renaming(Arity,RArgs),
  rel_nth_arguments_completion(R,AS,RArgs,1,ProjList).

rel_nth_arguments_completion(_R,_AS,[],_I,[]).
rel_nth_arguments_completion(_R,AS,[attr(R,C,RC)|RArgs],I,[attr(AS,C,RC)|ProjList]) :-
%  my_attribute('$des',I,R,C,_Type),
  my_attribute(I,R,C,_Type),
  I1 is I+1,
  rel_nth_arguments_completion(R,AS,RArgs,I1,ProjList).
  
arguments_completion(*,_,(pi(_D,_T,_ProjList,_S,_G,_H,_O),[RR|RArgs]),_PL,IRen,ORen,RProjList) :-
  !,
  replace_exprs_by_refs(RR,RArgs,RRArgs),
  rel_ren_projlist(RRArgs,RR,RProjList,IRen,ORen).
arguments_completion(*,_,(T,[AS|RArgs]),_PL,Ren,Ren,ProjList) :-
%  my_table('$des',T,Arity),
  my_table(T,Arity),
  !,
  rel_arguments_completion(T,Arity,AS,RArgs,ProjList).
arguments_completion(*,_,(_Rel,[AS|RArgs]),_PL,Ren,Ren,RArgs) :-
  is_system_identifier(AS),
  !.
arguments_completion(*,_,(_Rel,[AS|RArgs]),_PL,Ren,Ren,ProjList) :-
  !,
  replace_attr_rel_ren(RArgs,AS,ProjList).
arguments_completion(*,Scope,times(T1,T2),_PL,IRen,ORen,ProjList) :-
  arguments_completion(*,Scope,T1,[],IRen,Ren1,PL1),
  arguments_completion(*,Scope,T2,[],Ren1,ORen,PL2),
  append(PL1,PL2,ProjList).
arguments_completion(As,Scope,Rel,IProjList,IRen,ORen,ProjList) :-
  arguments_completion_list(As,Scope,Rel,IProjList,IRen,ORen,[],ProjList).
  
arguments_completion_list([],_Scope,_Rel,_ProjList,Ren,Ren,OPL,OPL).
arguments_completion_list([A|As],Scope,Rel,ProjList,IRen,ORen,IPL,OPL) :-
  argument_completion(A,Scope,Rel,ProjList,IRen,Ren1,PL),
  !,
  append(IPL,PL,PL1),
  arguments_completion_list(As,Scope,Rel,ProjList,Ren1,ORen,PL1,OPL).
arguments_completion_list([(Rel,(*))|_As],_Scope,_Rel,_ProjList,_IRen,_ORen,_IPL,_OPL) :-
  my_raise_exception(generic,syntax(['Unknown relation name ''',Rel,''' in context ''',Rel,'.*''.']),[]).
arguments_completion_list([attr(Rel,A,_AS)|_As],Scope,_Rel,_ProjList,_IRen,_ORen,_IPL,_OPL) :-
  my_raise_exception(unknown_column(Rel,A,Scope),syntax(''),[]).

argument_completion((Rel,(*)),Scope,times(T1,_T2),PL,IRen,ORen,OPL) :-
  argument_completion((Rel,(*)),Scope,T1,PL,IRen,ORen,OPL),
  !.
argument_completion((Rel,(*)),Scope,times(_T1,T2),PL,IRen,ORen,OPL) :-
  argument_completion((Rel,(*)),Scope,T2,PL,IRen,ORen,OPL),
  !.
argument_completion((Rel,(*)),_Scope,(_RAst,[Rel|RArgs]),_PL,Ren,Ren,OPL) :-
  !,
  replace_attr_rel_ren(RArgs,Rel,OPL).
argument_completion((Rel,(*)),_Scope,RAst,_PL,IRen,ORen,OPL) :-
  member(Ren,IRen),
  (Ren=(Rel,_) ; Ren=(_,Rel)),
  rel_arguments(Rel,RAst,IRen,ORen,RArgs),
  !,
  replace_attr_rel_ren(RArgs,Rel,OPL).
% An incorrectly assumed attribute (which is in fact a reference to an expression)
% is translated into a reference to an expression
%arguments_completion([attr(_Rel,A,AS)|As],RAst,PL,IRen,ORen,[expr(expr_ref(A),AS,_Type)|CAs]) :-
argument_completion(attr(Rel,A,AS),Scope,_RAst,PL,Ren,Ren,[expr(expr_ref(Rel,A),AS,_Type)]) :-
  Scope==order,
  pl_expr_member(expr(_E,A,_T),PL),
  !.
argument_completion(attr(Rel,A,AS),Scope,RAst,_PL,Ren,Ren,[attr(Rel,A,AS)]) :-
  argument_completion(attr(Rel,A,AS),Scope,RAst), 
  !.
% References to renamed attributes
argument_completion(attr(_Rel,A,AS),_Scope,_RAst,PL,Ren,Ren,[attr(R,C,AS)]) :-
  pl_attr_member(attr(R,C,A),PL),
  !.
argument_completion(expr(E,AS,Type),Scope,RAst,PL,IRen,ORen,[expr(RE,AS,Type)]) :-
  expr_argument_completion(E,Scope,RAst,PL,IRen,ORen,RE).

replace_attr_rel_ren([],_Ren,[]).
replace_attr_rel_ren([attr(_Rel,N,A)|As],Ren,[attr(Ren,N,A)|RAs]) :- 
  !, 
  replace_attr_rel_ren(As,Ren,RAs).
replace_attr_rel_ren([A|As],Ren,[A|RAs]) :-  
  replace_attr_rel_ren(As,Ren,RAs).
  
replace_exprs_by_refs(_Rel,[],[]).
% replace_exprs_by_refs(Rel,[expr(_E,AS,T)|Args],[expr(expr_ref(Rel,AS),AAS,T)|RArgs]) :-
%   !,
%   argument_autorenaming(AAS),
%   replace_exprs_by_refs(Rel,Args,RArgs).
replace_exprs_by_refs(Rel,[expr(_E,AS,T)|Args],[expr(expr_ref(Rel,AS),AS,T)|RArgs]) :-
  !,
%  argument_autorenaming(AAS),
  replace_exprs_by_refs(Rel,Args,RArgs).
% replace_exprs_by_refs(Rel,[attr(T,C,_R)|Args],[attr(T,C,AS)|RArgs]) :-
%   argument_autorenaming(AS),
%   replace_exprs_by_refs(Rel,Args,RArgs).
replace_exprs_by_refs(Rel,[Attr|Args],[Attr|RArgs]) :-
  replace_exprs_by_refs(Rel,Args,RArgs).

% Projection list member. Checks whether the attribute attr(Rel,A,AS) can be found in the projection list.
% Note that the attribute can be a variable
% Reference to the attribute name:
pl_attr_member(attr(Rel,A,_AS),[attr(Rel,Arg,_RArg)|_PL]) :-
  A==Arg,
  !.
% Reference to the attribute renaming (excluding autorenamings):
pl_attr_member(attr(Rel,A,AS),[attr(Rel,A,RArg)|_PL]) :-
  AS==RArg,
  A\==AS,
  !.
pl_attr_member(attr(Rel,A,AS),[_Attr|PL]) :-
  pl_attr_member(attr(Rel,A,AS),PL).
  
pl_expr_member(expr(E,A,Type),[expr(E,AS,Type)|_PL]) :-
  A==AS,
  !.
pl_expr_member(expr(E,A,Type),[_Arg|PL]) :-
  pl_expr_member(expr(E,A,Type),PL).

% expr_argument_completion(expr(attr(R,C,A),_,_),RAst,PL,IRen,ORen,E) :- 
%   expr_argument_completion(attr(R,C,A),RAst,PL,IRen,ORen,E).
expr_argument_completion(E,_Scope,_RAst,_PL,Ren,Ren,E) :- 
  (number(E) ; atomic(E)),
  !.
%expr_argument_completion(attr(_Rel,A,_AS),_RAst,PL,expr_ref(A)) :- 
expr_argument_completion(attr(Rel,A,_AS),_Scope,_RAst,PL,Ren,Ren,expr_ref(Rel,A)) :- 
  var(Rel),
  pl_expr_member(expr(_E,A,_Type),PL),
  !.
%expr_argument_completion(attr(_Rel,A,_AS),_RAst,PL,attr(R,C,A)) :- 
expr_argument_completion(attr(Rel,A,_AS),_Scope,_RAst,PL,Ren,Ren,attr(R,C,A)) :- 
  var(Rel),
  pl_attr_member(attr(R,C,A),PL),
  !.
% expr_argument_completion(attr(R,A,AS),_RAst,PL,Ren,Ren,attr(R,A,AS)) :- 
%   var(R),
%   member(attr(R,A,AS),PL),
%   !.
%expr_argument_completion(cte(Cte,Type),_RAst,_PL,cte(Cte,Type)) :- 
expr_argument_completion(cte(Cte,Type),_Scope,_RAst,_PL,Ren,Ren,cte(Cte,Type)) :- 
  !.
expr_argument_completion(attr(Rel,A,AS),Scope,RAst,_PL,Ren,Ren,attr(Rel,A,AS)) :- 
  argument_completion(attr(Rel,A,AS),Scope,RAst),
  !.
expr_argument_completion(attr(Rel,A,_AS),Scope,_RAst,_PL,Ren,Ren,_CA) :- 
%   write_error_log(['Unknown column ',A]),
%   !,
%   fail.
  my_raise_exception(unknown_column(Rel,A,Scope),syntax(''),[]).
expr_argument_completion((SQLst,RR),_Scope,_RAst,_PL,IRen,ORen,RAst) :-
  var(RR),
  !,
  sql_to_ra1((SQLst,RR),RAst,IRen,ORen).  
expr_argument_completion(E,Scope,RAst,PL,IRen,ORen,RE) :- 
  E =.. [F|Args],
  !, 
  expr_argument_completion_list(Args,Scope,RAst,PL,IRen,ORen,RArgs),
  RE =.. [F|RArgs].
  
  
expr_argument_completion_list([],_Scope,_RAst,_PL,Ren,Ren,[]) :-
  !.
expr_argument_completion_list([E|Es],Scope,RAst,PL,IRen,ORen,[RE|REs]) :-
  !, 
  expr_argument_completion(E,Scope,RAst,PL,IRen,IRen1,RE), 
  expr_argument_completion_list(Es,Scope,RAst,PL,IRen1,ORen,REs).

% argument_completion(attr(Rel,A,AS),RAst) :-
%   (var(Rel) -> argument_completion_ng(attr(Rel,A,AS),RAst) ; true).

% argument_completion_ng(attr(RT,C,_AS),(T,[RT|_RArgs])) :-
%   my_attribute('$des',_Nth,T,C,_Type),
%   !.
% argument_completion_ng(A,times(R1,R2)) :-
%   argument_completion_ng(A,R1),
%   !
%   ;
%   argument_completion_ng(A,R2),
%   !.
% argument_completion_ng(A,(inner_join(R1,R2,_C),_AS)) :-
%   argument_completion_ng(A,R1),
%   !
%   ;
%   argument_completion_ng(A,R2),
%   !.
% argument_completion_ng(A,(left_join(R1,R2,_C),_AS)) :-
%   argument_completion_ng(A,R1),
%   !
%   ;
%   argument_completion_ng(A,R2),
%   !.
% argument_completion_ng(A,(right_join(R1,R2,_C),_AS)) :-
%   argument_completion_ng(A,R1),
%   !
%   ;
%   argument_completion_ng(A,R2),
%   !.
% argument_completion_ng(A,(full_join(R1,R2,_C),_AS)) :-
%   argument_completion_ng(A,R1),
%   !
%   ;
%   argument_completion_ng(A,R2),
%   !.
% argument_completion_ng(attr(AS,A,_AS),(pi(_D,_T,ProjList,_S,_G,_H,_O),[AS|_Args])) :-
%   find_argument(A,ProjList),
%   !.
% %argument_completion_ng(expr(_Expr,ExprAS,_Type),(pi(_D,_T,ProjList,_S,_G,_H,_O),[_AS|_Args])) :-
% argument_completion_ng(attr(AS,ExprAS,_AS),(pi(_D,_T,ProjList,_S,_G,_H,_O),[AS|_Args])) :-
%   find_expr_renaming(ExprAS,ProjList),
%   !.
% argument_completion_ng(attr(AS,A,_AS),(_,[AS|ProjList])) :-
%   find_argument(A,ProjList),
%   !.
% argument_completion_ng(attr(AS,A,_AS),(_,[AS|ProjList])) :-
%   find_expr_renaming(A,ProjList),
%   !.

argument_completion(attr(Rel,A,AS),_Scope,RAst) :-
  (var(Rel)
   ->
    findall(attr(Rel,A,AS),argument_completion_ng(attr(Rel,A,AS),RAst),L),
    remove_duplicates(L,RL),
    (RL=[attr(Rel,A,AS)]
     ->
      check_ambiguous_attrs(attr(Rel,A,AS),RAst)
     ;
      (L==[]
       ->
        fail
       ;
        \+ \+ check_ambiguous_attrs(attr(Rel,A,AS),RAst),
        check_ambiguous_attrs(attr(Rel,A,AS),RL,RAst),
        [attr(Rel,A,AS)|_]=L
      )
    )
   ;
    check_ambiguous_attrs(attr(Rel,A,AS),RAst)).

check_ambiguous_attrs(_A,As,R) :-
  join_conditions(R,Cs),
  equalities_to_var_equalities(Cs,[],Dict,VCs),
  call_list(VCs),
  atts_to_simple_atts(As,SAs),
  lookup_dict_list(SAs,Dict,Vs),
  remove_duplicates_var(Vs,[_]),
  !.
check_ambiguous_attrs(Attr,_L,_RA) :-
  visible_qualified_attr(Attr,VA),
  my_raise_exception(generic,syntax(['Ambiguous column name ''',VA,'''.']),[]).
  
check_ambiguous_attrs(attr(Rel,A,AS),(_,[Rel|Atts])) :-
  !,
  visible_column_name(A,AS,VC),
  findall(VC,(member(attr(_,N,RN),Atts),visible_column_name(N,RN,VC)),VCs),
  (VCs=[_,_|_]
   ->
    visible_qualified_attr(attr(Rel,A,AS),VA),
    my_raise_exception(generic,syntax(['Ambiguous column name ''',VA,'''.']),[])
   ;
    true).
check_ambiguous_attrs(_,_).  

atts_to_simple_atts([],[]).
atts_to_simple_atts([A|As],[SA|SAs]) :-
  att_to_simple_att(A,SA),
  atts_to_simple_atts(As,SAs).

att_to_simple_att(attr(R,A,As),attr(R,C)) :-
  visible_column_name(A,As,C).
  
join_conditions((R,Sch),Cs) :-
  R=..[JOp,LR,RR,C],
  join_operator(JOp),
  join_conditions(LR,LCs),
  join_conditions(RR,RCs),
  equijoin_conditions(C,Eqs),
  sch_equijoin_conditions(Sch,SEqs),
  concat_lists([Eqs,SEqs,LCs,RCs],Cs),
  !.
join_conditions((_R,Sch),Cs) :-
  sch_equijoin_conditions(Sch,Cs).
  
% equijoin_conditions(C,Cs) :-
%   findall(L=R,
%    (my_member_term(LA=RA,C),
%     (LA@<RA -> L=LA, R=RA ; L=RA, R=LA)
%    ),
%   Cs).
equijoin_conditions(C,Cs) :-
  findall(SL=SR,(my_member_term(L=R,C), atts_to_simple_atts([L,R],[SL,SR])), Cs).
  
sch_equijoin_conditions([Rel|Atts],Eqs) :-
  sch_equijoin_conditions(Rel,Atts,Eqs).
  
sch_equijoin_conditions(_Rel,[],[]).
sch_equijoin_conditions(Rel,[attr(AR,A,As)|Atts],[L=R|Eqs]) :-
  !,
%   LA=attr(AR,C,As),
%   RA=attr(Rel,C,As),
%   (AR@<Rel -> L=LA, R=RA ; L=RA, R=LA),
  visible_column_name(A,As,C),
  L=attr(AR,C),
  R=attr(Rel,C),
  sch_equijoin_conditions(Rel,Atts,Eqs).
sch_equijoin_conditions(Rel,[_|Atts],Eqs) :-
  sch_equijoin_conditions(Rel,Atts,Eqs).

equalities_to_var_equalities([],Dict,Dict,[]).
equalities_to_var_equalities([L=R|Eqs],IDict,ODict,[VL=VR|VEqs]) :-
  add_to_var_dict(L,IDict,VL,IDict1),
  add_to_var_dict(R,IDict1,VR,IDict2),
  equalities_to_var_equalities(Eqs,IDict2,ODict,VEqs).
  
add_to_var_dict(A,Dict,V,Dict) :-
  lookup_dict(A,Dict,V),
  !.
add_to_var_dict(A,IDict,V,[V=A|IDict]).

lookup_dict_list([],_Dict,[]).
lookup_dict_list([A|As],Dict,[V|Vs]) :-
  lookup_dict(A,Dict,V),
  lookup_dict_list(As,Dict,Vs).

lookup_dict(A,Dict,V) :-
  member(V=A,Dict).

argument_completion_ng(attr(RT,C,As),(T,[RT|RArgs])) :-
  non_join_relation(T),
  find_argument(C,As,RArgs),
%  my_attribute('$des',_Nth,T,C,_Type).
  my_attribute(_Nth,T,C,_Type).
argument_completion_ng(A,times(R1,R2)) :-
  argument_completion_ng(A,R1)
  ;
  argument_completion_ng(A,R2).
argument_completion_ng(attr(VR,A,CAS),(inner_join(R1,R2,_C),[RAS|_Args])) :-
  argument_completion_ng(attr(R,A,CAS),R1),
  visible_join_relation_name(RAS,R,VR)
  ;
  argument_completion_ng(attr(R,A,CAS),R2),
  visible_join_relation_name(RAS,R,VR).
argument_completion_ng(attr(VR,A,CAS),(left_join(R1,R2,_C),[RAS|_Args])) :-
  argument_completion_ng(attr(R,A,CAS),R1),
  visible_column_name(RAS,R,VR)
  ;
  argument_completion_ng(attr(R,A,CAS),R2),
  visible_column_name(RAS,R,VR).
argument_completion_ng(attr(VR,A,CAS),(right_join(R1,R2,_C),[RAS|_Args])) :-
  argument_completion_ng(attr(R,A,CAS),R1),
  visible_column_name(RAS,R,VR)
  ;
  argument_completion_ng(attr(R,A,CAS),R2),
  visible_column_name(RAS,R,VR).
argument_completion_ng(attr(VR,A,CAS),(full_join(R1,R2,_C),[RAS|_Args])) :-
  argument_completion_ng(attr(R,A,CAS),R1),
  visible_column_name(RAS,R,VR)
  ;
  argument_completion_ng(attr(R,A,CAS),R2),
  visible_column_name(RAS,R,VR).
argument_completion_ng(attr(AS,A,As),(pi(_D,_T,ProjList,_S,_G,_H,_O),[AS|_Args])) :-
  find_argument(A,As,ProjList).
%argument_completion_ng(expr(_Expr,ExprAS,_Type),(pi(_D,_T,ProjList,_S,_G,_H,_O),[_AS|_Args])) :-
argument_completion_ng(attr(AS,ExprAS,_AS),(pi(_D,_T,ProjList,_S,_G,_H,_O),[AS|_Args])) :-
  find_expr_renaming(ExprAS,ProjList).
argument_completion_ng(attr(AS,A,As),(R,[AS|ProjList])) :-
  non_join_relation(R),
  find_argument(A,As,ProjList).
argument_completion_ng(attr(AS,A,_AS),(_,[AS|ProjList])) :-
  find_expr_renaming(A,ProjList).

visible_join_relation_name(R,_R,R) :-
  \+ is_system_identifier(R),
  !.
visible_join_relation_name(_R,R,R) :-
  \+ is_system_identifier(R),
  !.
visible_join_relation_name(R,_R,R).

non_join_relation(R) :-
  R\=inner_join(_,_,_),
  R\=left_join(_,_,_),
  R\=right_join(_,_,_),
  R\=full_join(_,_,_).
  
find_argument(A,_As,[attr(_Rel,A,Ren)|_Args]) :-
  is_system_identifier(Ren),
  !.
find_argument(RenA,_RenA,[attr(_Rel,_A,RenA)|_As]) :-
  !.
find_argument(A,As,[_|Args]) :-
  find_argument(A,As,Args).
  
find_expr_renaming(ExprAS,[expr(_Expr,ExprAS,_Type)|_As]) :-
  !.
find_expr_renaming(A,[_|As]) :-
  find_expr_renaming(A,As).
  
rel_ren_projlist([],_AS,[],Ren,Ren).
%rel_ren_projlist([attr(Rel,_Col,_ColRen)|As],AS,[attr(AS,_Col,_ColRen)|RAs],IRen,ORen) :-
rel_ren_projlist([attr(Rel,Col,ColRen)|As],AS,[attr(AS,Col,ColRen)|RAs],IRen,ORen) :-
  !,
  my_set_union([(AS,Rel)],IRen,Ren1),
  rel_ren_projlist(As,AS,RAs,Ren1,ORen).
rel_ren_projlist([E|As],AS,[E|RAs],IRen,ORen) :- % Expressions
  rel_ren_projlist(As,AS,RAs,IRen,ORen).
%rel_ren_projlist([expr(attr(Rel,C,_RC),AS,_Type)|As],AS,IRen,ORen) :-  %::: Warning 2011-10-14
%  rel_ren_projlist(As,AS,IRen,ORen).

rel_arguments(Rel,RAst,IRen,ORen,RAs) :-
  arguments_completion(*,_,RAst,[],IRen,ORen,ProjList),
  (member((Rel,RelId),ORen)
   -> 
    (is_system_identifier(RelId) % Can be used if not renamed
     ->
      true
     ;
      fail
    )
    %true 
   ;
    RelId = Rel),
  filter_rel_arg(RelId,ProjList,RAs),
  (RAs==[]
   -> 
     set_syntax_error(['Unknown relation name ''',Rel,''' in context ''',Rel,'.*".'],0,statement),
     fail
%     my_raise_exception(generic,syntax(['Unknown relation name ''',Rel,''' in context ''',Rel,'.*''.']),[])
   ;
    true).

filter_rel_arg(_RelId,[],[]).
filter_rel_arg(RelId,[attr(RelId,A,AS)|RAs],[attr(RelId,A,AS)|FRAs]) :-
  !, 
  filter_rel_arg(RelId,RAs,FRAs).
filter_rel_arg(RelId,[_|RAs],FRAs) :-
  filter_rel_arg(RelId,RAs,FRAs).


% SQL condition to RA condition
sql_cond_to_ra_cond(_Scope,exists(Rel),exists(RRel),IRen,ORen) :-
  !,
  sql_to_ra1(Rel,RRel,IRen,ORen).
sql_cond_to_ra_cond(Scope,not_in(Args,Rel),not_in(Args,RRel),IRen,ORen) :-
  !,
  arguments_autorenaming(_RR,Args),
  check_sql_condition(Scope,Args),
  sql_to_ra1(Rel,RRel,IRen,ORen).
sql_cond_to_ra_cond(Scope,in(Args,Rel),in(Args,RRel),IRen,ORen) :-
  !,
% WARNING:
  arguments_autorenaming(_RR,Args),
  check_sql_condition(Scope,Args),
  sql_to_ra1(Rel,RRel,IRen,ORen),
  check_sql_in_condition(Args,RRel).
sql_cond_to_ra_cond(Scope,is_null(Rel),is_null(RRel),IRen,ORen) :-
  !,
  sql_cond_to_ra_cond(Scope,Rel,RRel,IRen,ORen).
sql_cond_to_ra_cond(Scope,not(C),not(RC),IRen,ORen) :-
  !,
  sql_cond_to_ra_cond(Scope,C,RC,IRen,ORen).
sql_cond_to_ra_cond(_Scope,(SQLst,RR),RAst,IRen,ORen) :-
  var(RR),
  !,
  sql_to_ra1((SQLst,RR),RAst,IRen,ORen).
sql_cond_to_ra_cond(Scope,SQLC,RAC,IRen,ORen) :-
  SQLC =.. [Op,L,R],
% WARNING: UNREMARKED because of "select * from t where a <> null"
%  my_sql_op(Op),
  my_sql_op(Op),
  !,
  sql_cond_to_ra_cond(Scope,L,LRRel,IRen,Ren),
  sql_cond_to_ra_cond(Scope,R,RRRel,Ren,ORen),
  RAC =.. [Op,LRRel,RRRel].
% sql_cond_to_ra_cond(A,A,Ren,Ren) :-
%   arguments_autorenaming(_R,[A]),
%   !.
% sql_cond_to_ra_cond(C,C,Ren,Ren).
sql_cond_to_ra_cond(Scope,E,E,Ren,Ren) :-
  check_sql_condition(Scope,E),
  expr_arguments_autorenaming(E).
  
check_sql_condition(having,_SQLCond) :-
  !.
check_sql_condition(_Scope,C) :-
  \+ include_aggregate(C),
  !.
check_sql_condition(Scope,C) :-
  (Scope==where -> Clause='WHERE' ; Clause='ON'),
  my_raise_exception(generic,syntax(['This aggregate is not allowed in the ',Clause,' clause: ','$exec'(write_expr(C))]),[]).

check_sql_in_condition(Args,(_Rel,[_Name|RArgs])) :-
  length(Args,L1),
  length(RArgs,L2),
  (L1 == L2 
   ->
    true
   ;
    my_raise_exception(generic,syntax(['The number of columns to the left of the "in" condition (',L1,') does not match with those to the right (',L2,').']),[])).

% expr_arguments_autorenaming(A) :-
%   arguments_autorenaming(_R,[A]),
%   !.
% expr_arguments_autorenaming(E) :-
%   E=..[_F|Args],
%   expr_arguments_autorenaming_list(Args).
% 
% expr_arguments_autorenaming_list([]).
% expr_arguments_autorenaming_list([A|As]):-
%   expr_arguments_autorenaming(A),
%   expr_arguments_autorenaming_list(As).

% SQL join condition to RA condition
% Full natural outer join:
sql_join_cond_to_ra_cond(equijoin(natural),full_join,C,[LR|LArgs],[RR|RArgs],IProjList,OProjList,RCommonAtts,IRen,ORen) :-
  !,
  eq_common_atts(LR,LArgs,RR,RArgs,C),
  remove_common_atts(IProjList,full_join,CommonAtts,TProjList),
  relation_autorenaming(RId), % RId for the full join
  replace_common_atts_snn(TProjList,CommonAtts,RCommonAtts,RId,IRen,ORen,OProjList).
sql_join_cond_to_ra_cond(equijoin(natural),JoinOp,C,[LR|LArgs],[RR|RArgs],IProjList,OProjList,CommonAtts,Ren,Ren) :-
  !,
  eq_common_atts(LR,LArgs,RR,RArgs,C),
  remove_common_atts(IProjList,JoinOp,CommonAtts,OProjList).
% Other natural joins:
sql_join_cond_to_ra_cond(equijoin(Atts),_JoinOp,C,LAS,RAS,ProjList,ProjList,_,Ren,Ren) :-
  !,
  build_equijoin(Atts,LAS,RAS,ProjList,C).
sql_join_cond_to_ra_cond(SQLCondition,_JoinOp,RACondition,_LAS,_RAS,ProjList,ProjList,_,IRen,ORen) :-
  sql_cond_to_ra_cond(on,SQLCondition,RACondition,IRen,ORen).

replace_common_atts_snn(Atts,[],[],_,Ren,Ren,Atts).
replace_common_atts_snn([attr(R,N,A)|Atts],[attr(R,N,A)|CAtts],[attr(RId,V,V)|RCAtts],RId,IRen,[(select_not_null,RId)|Oren],[attr(RId,V,V)|RAtts]) :-
  visible_column_name(N,A,V),
  replace_common_atts_snn(Atts,CAtts,RCAtts,RId,IRen,Oren,RAtts).

eq_common_atts(LR,LArgs,RR,RArgs,C) :-
  list_eq_common_atts(LR,LArgs,RR,RArgs,Cs),
  conjunctive_cond(Cs,C).
  
list_eq_common_atts(_LR,[],_RR,_RArgs,[]).
list_eq_common_atts(LR,[LArg|LArgs],RR,RArgs,[RLArg=RRArg|C]) :-
  only_one_same_arg_name(LR,LArg,RR,RArgs,RLArg,RRArg),
  !,
  list_eq_common_atts(LR,LArgs,RR,RArgs,C).
list_eq_common_atts(LR,[_LArg|LArgs],RR,RArgs,C) :-
  list_eq_common_atts(LR,LArgs,RR,RArgs,C).

only_one_same_arg_name(LR,LArg,RR,RArgs,RLArg,RRArg) :-
  findall((RLArg,RRArg),same_arg_name(LR,LArg,RR,RArgs,RLArg,RRArg),[(RLArg,RRArg)|Ts]),
  (Ts==[]
   ->
    true
   ;
    visible_attr_name(LArg,Name),
    my_raise_exception(generic,syntax(['More than one occurrence of common column ''',Name,''' in right relation.']),[])
  ).
  
% same_arg_name(+LR,+LArg,+RR,+RArgs,-RLArg,-RRArg) :-
same_arg_name(LR,LArg,RR,RArgs,RLArg,RRArg) :-
  arg_name_or_ren(LR,LArg,Name,RLArg),
  member(RArg,RArgs),
  arg_name_or_ren(RR,RArg,Name,RRArg).
  
% arg_name_or_ren(attr(_,Name,_),Name).
% arg_name_or_ren(attr(_,_,Name),Name).
% arg_name_or_ren(expr(_,Name,_),Name).
arg_name_or_ren(Rel,attr(Table,Name,Renaming),RefAtt,attr(RefTable,Name,Renaming)) :-
  reference_attr(Name,Renaming,RefAtt),
  reference_table(Rel,Table,RefTable).
%arg_name_or_ren(_Rel,expr(E,Name,Type),Name,expr(E,Name,Type)).
arg_name_or_ren(_Rel,expr(E,Name,_Type),Name,E).

reference_attr(Name,Renaming,Name) :-
  is_system_identifier(Renaming),
  !.
reference_attr(_Name,Renaming,Renaming).

reference_table(Rel,_Table,Rel) :-
  \+ is_system_identifier(Rel),
  !.
reference_table(_Rel,Table,Table) :-
  is_system_identifier(Table),
  !.
reference_table(Rel,_Table,Rel).

remove_common_atts(IAtts,right_join,CAtts,OAtts) :-
  !,
  remove_common_rev_atts(IAtts,CAtts,OAtts).
remove_common_atts(IAtts,_,CAtts,OAtts) :-
  my_reverse(IAtts,RIAtts),
  remove_common_rev_atts(RIAtts,RCAtts,RRIAtts),
  my_reverse(RCAtts,CAtts),
  my_reverse(RRIAtts,OAtts).
  
remove_common_rev_atts([],[],[]).
remove_common_rev_atts([Att|Atts],[RAtt|CAtts],RAtts) :-
  member(RAtt,Atts),
  same_arg_name(Att,RAtt),
  !,
  remove_common_rev_atts(Atts,CAtts,RAtts).
remove_common_rev_atts([Att|Atts],CAtts,[Att|RAtts]) :-
  remove_common_rev_atts(Atts,CAtts,RAtts).
  
same_arg_name(A1,A2) :-
  reference_attr(A1,RA),
  reference_attr(A2,RA).
 
reference_attr(attr(_,N,R),N) :-
  is_system_identifier(R),
  !.
reference_attr(attr(_,_,R),R).
reference_attr(expr(_,N,_),N).


renamed_arg(attr(_,_,Renaming)) :-
  \+ is_system_identifier(Renaming).
renamed_arg(expr(_,_,Renaming)) :-
  \+ is_system_identifier(Renaming).


build_sql_division_arguments(L,R,LExprs,[Expr|Exprs]) :-
  copy_term(L,L0),
  copy_term(R,R0),
  sql_rel_to_ra(L0,(_RALR,[LR|LAtts]),[],_),
  sql_rel_to_ra(R0,(_RARR,[RR|RAtts]),[],_),
%  append(LAtts,RAtts,AllAtts),
%   arguments_autorenaming(LR,LAtts),
%   arguments_autorenaming(RR,RAtts),
  sql_diff_atts(LAtts,LR,RAtts,RR,Atts),
  attr_expr_list(Atts,[Expr|Exprs]),
  attr_expr_list(LAtts,LExprs),
  length(LAtts,LL),
  length(RAtts,RL),
  length([Expr|Exprs],EL),
  EL is LL-RL,
  EL > 0,
  !.
build_sql_division_arguments(_L,_R,_LExprs,_Exprs) :-
  my_raise_exception(generic,syntax(['Incompatible schemas in division operation.']),[]).

build_sql_division_ra_expression(L,R,LExprs,Exprs,(RA,RASchema),IRen,ORen) :-
  copy_term(Exprs,Exprs2),
  copy_term(Exprs,Exprs3),
  copy_term(LExprs,LExprs2),
  copy_term(LExprs,LExprs3),
  copy_term(L,L1),
%  L=(LRN,[LRAs|_]),
%   (nonvar(LRAs)->atom_concat(LRAs,'_1',LRAs1);true),
%  L1=(LRN,[LRAs1|_]),
  copy_term(L,L2),
%   (nonvar(LRAs)->atom_concat(LRAs,'_2',LRAs2);true),
%  L2=(LRN,[LRAs2|_]),
  LAS = [_|RRAtts],
  Schema = [_|RRAtts],
  SQL = 
  (except(distinct,
  (select(distinct,top(all),Exprs,from([L]),where(true),group_by([]),having(true),order_by([],[])),_),
  (select(distinct,top(all),Exprs2,from([(
    except(distinct,
    (select(distinct,top(all),LExprs2,from([
      (select(distinct,top(all),Exprs3,from([L1]),where(true),group_by([]),having(true),order_by([],[])),_), R]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(distinct,top(all),LExprs3,from([L2]),where(true),group_by([]),having(true),order_by([],[])),_)),_)
    ]),where(true),group_by([]),having(true),order_by([],[])),_)),LAS), 
  sql_to_ra(SQL,(RA,Schema),IRen,ORen),
  RASchema=[Rel|Atts],
  replace_rel_attr_list(RRAtts,Rel,Atts).
% select a from t
% except
% (select a from 
%  ((select * from (select a from t),s)
%   except
%  (select * from t)));
  
replace_rel_attr_list([],_,[]).
replace_rel_attr_list([attr(_,C,A)|Atts],Rel,[attr(Rel,C,A)|RAtts]) :-
  !,
  replace_rel_attr_list(Atts,Rel,RAtts).
replace_rel_attr_list([Arg|Atts],Rel,[Arg|RAtts]) :-
  !,
  replace_rel_attr_list(Atts,Rel,RAtts).

% attr_name(attr(_,Name,_),Name).

attr_expr_list([],[]).
attr_expr_list([attr(_,A,AS)|Attrs],[expr(attr(_,C,_),_,_)|Names]) :-
% attr_expr_list([Attr|Attrs],[expr(attr(_,Name,_),_,_)|Names]) :-
%  attr_name(Attr,Name),
  visible_column_name(A,AS,C),
  !,
  attr_expr_list(Attrs,Names).
attr_expr_list([Expr|Attrs],[Expr|Names]) :-
  attr_expr_list(Attrs,Names).

sql_diff_atts([],_LR,_Atts2,_RR,[]).
sql_diff_atts([Att1|Atts1],LR,Atts2,RR,Atts) :-
  member(RAtt,Atts2),
  same_arg_name(Att1,RAtt),
  !,
  sql_diff_atts(Atts1,LR,Atts2,RR,Atts).
sql_diff_atts([Att1|Atts1],LR,Atts2,RR,[Att1|Atts]) :-
  sql_diff_atts(Atts1,LR,Atts2,RR,Atts).

% sql_att_member(Att,LR,[Att2|_Atts],RR,AllAtts) :-
%   same_arg_name(LR,Att,RR,AllAtts,_,Att2),
%   !.
% sql_att_member(Att,LR,[_Att2|Atts],RR,AllAtts) :-
%   sql_att_member(Att,LR,Atts,RR,AllAtts).
  
build_equijoin([],_LAS,_RAS,_ProjList,true).
build_equijoin([attr(_RAtt,Att,_ArrRen)],[LAS|_LRArgs],[RAS|_RRArgs],ProjList,attr(LAS,Att,RA)=attr(RAS,Att,RB)) :-
  (member(attr(LAS,Att,RA),ProjList) -> true ; my_raise_exception(unknown_column(LAS,Att,using),syntax(''),[])),
  (member(attr(RAS,Att,RB),ProjList) -> true ; my_raise_exception(unknown_column(RAS,Att,using),syntax(''),[])).
build_equijoin([Att1,Att2|Atts],LAS,RAS,ProjList,and(C1,C2)) :-
  build_equijoin([Att1],LAS,RAS,ProjList,C1),
  build_equijoin([Att2|Atts],LAS,RAS,ProjList,C2).

% Building a conjunctive condition from a list of conditions
conjunctive_cond([],true) :-
  !.
conjunctive_cond([C],C) :-
  !.
conjunctive_cond([C1,C2],and(C1,C2)) :- 
  !.
conjunctive_cond([C|Cs],and(C,CC)) :- 
  conjunctive_cond(Cs,CC).

% SQL relation to RA relation
sql_rel_to_ra([R],CR,IRen,ORen) :-
  sql_rel_to_ra(R,CR,IRen,ORen).
%  sql_to_ra1(R,CR,IRen,ORen).
sql_rel_to_ra([A,B|Rs],times(CA,RRs),IRen,ORen) :-
  sql_rel_to_ra(A,CA,IRen,Ren),
  sql_rel_to_ra([B|Rs],RRs,Ren,ORen).
sql_rel_to_ra((SQJOIN,[RR|_Args]),
              (RAJOIN,[RR|ProjList]),
              IRen,ORen) :-
  SQJOIN=..[JoinOp,R1,R2,C],
  join_operator(JoinOp),
  !,
  (JoinOp=full_join, C=equijoin(natural) -> CAs=[CommonAtts] ; CAs=[]),
  RAJOIN=..[JoinOp,AR1,AR2,ARC|CAs],
  sql_rel_to_ra(R1,AR1,IRen,Ren1),
  sql_rel_to_ra(R2,AR2,Ren1,Ren2),
  relation_autorenaming(RR),
  arguments_completion(*,_,times(AR1,AR2),[],Ren2,Ren3,Atts),
  schema_arguments(AR1,LAS),
  schema_arguments(AR2,RAS),
%   sql_join_cond_to_ra_cond(C,JoinOp,ARC,LAS,RAS,Atts,ProjList,CommonAtts,Ren3,ORen).
  sql_join_cond_to_ra_cond(C,JoinOp,UARC,LAS,RAS,Atts,ProjList,CommonAtts,Ren3,Ren4),
  expr_argument_completion(UARC,on,times(AR1,AR2),[],Ren4,ORen,ARC).
sql_rel_to_ra((division(L,R),Schema),(RA,Schema),IRen,ORen) :-
  !,
  Schema=[RR|_],
  relation_autorenaming(RR),
  build_sql_division_arguments(L,R,LExprs,Exprs),
  build_sql_division_ra_expression(L,R,LExprs,Exprs,(RA,Schema),IRen,ORen).
sql_rel_to_ra((T,AS),RA,IRen,ORen) :-
  my_table(T,_Arity),
  !,
  sql_table_view_to_ra((T,AS),RA,IRen,ORen).
sql_rel_to_ra(SQL,RA,IRen,ORen) :-
  sql_to_ra1(SQL,RA,IRen,ORen).
sql_rel_to_ra((T,[_RR|_RArgs]),_RA,_IRen,_ORen) :-
  my_raise_exception(unknown_relation(T),syntax(''),[]).


sql_table_view_to_ra((T,[RR|RArgs]),(T,[RR|RArgs]),Ren,[(T,RR)|Ren]) :-
  ground((T,[RR|RArgs])), % Already done in des_dbigen.pl
  !.
sql_table_view_to_ra((T,[RR|RArgs]),(T,[RR|RArgs]),Ren,[(T,RR)|Ren]) :-
  check_arguments_renaming(T,RArgs),
  relation_autorenaming(RR),
  get_table_untyped_arguments(T,Args),
  build_ren_arguments(T,Args,RArgs),
  arguments_autorenaming(RR,RArgs).
  
check_arguments_renaming(_R,RArgs) :-
  var(RArgs),
  !.
check_arguments_renaming(R,RArgs) :-
  table_arity(R,Arity),
  check_syntax_arguments_renaming(Arity,RArgs).
  
check_syntax_arguments_renaming(Arity,RArgs) :-
  length(RArgs,L),
  (L==Arity
   ->
    true 
   ;
    my_raise_exception(generic,syntax(['Incorrect number of attributes in renaming: ',L,' (expected ',Arity,').']),[])).
  

join_operator(inner_join).  
join_operator(left_join).  
join_operator(right_join).  
join_operator(full_join).  

% sql_join_arg_to_ra((T,AS),RA,IRen,ORen) :-
%   my_table(T,_Arity),
%   !,
%   sql_table_view_to_ra((T,AS),RA,IRen,ORen).
% sql_join_arg_to_ra((JOIN,AS),RA,IRen,ORen) :-
%   JOIN=..[JoinOp,_,_,_],
%   join_operator(JoinOp),
%   !,
%   sql_to_ra1((select(all,
%               _TopN,
%               *,
% %              from([(inner_join(R1,R2,C),AS)]),
%               from([(JOIN,AS)]),
%               where(true),
%               group_by([]),
%               having(true),
%               order_by([],[])),
%               AS),
%               RA,IRen,ORen).
% sql_join_arg_to_ra(SQL,RA,IRen,ORen) :-
%   !,
%   sql_to_ra1(SQL,RA,IRen,ORen).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ra_to_cra(+RAst,-CRAst) Translates a 
% Relational Algebra Syntactic Tree into a
% Canonical Relational Algebra Syntactic Tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% NOT IN in an ASSUME clause
ra_to_cra((not(R),AS),
          (not(CR),AS)) :-
  !,
  ra_to_cra((R,AS),(CR,AS)).

% WITH clause
ra_to_cra((with(R,Rs),AS),
          (with(CR,CRs),AS)) :-
  !,
  ra_to_cra(R,CR),
  ra_to_cra_list(Rs,CRs).

% EXISTS condition
ra_to_cra((pi(D,T,As,sigma(exists(Rel),Rs),G,H,O),AS),
          (pi(D,T,As,sigma(exists(CRel),CRs),G,H,O),AS)) :-
  !,
  ra_to_cra(Rel,CRel),
  ra_to_cra(Rs,CRs).

% IN condition
ra_to_cra((pi(D,T,As,sigma(in(Args,Rel),Rs),G,H,O),AS),
          (pi(D,T,As,sigma(in(Args,CRel),CRs),G,H,O),AS)) :-
  !,
  ra_to_cra(Rel,CRel),
  ra_to_cra(Rs,CRs).

% NOT IN condition
ra_to_cra((pi(D,T,As,sigma(not_in(Args,Rel),Rs),G,H,O),AS),
          (pi(D,T,As,sigma(not_in(Args,CRel),CRs),G,H,O),AS)) :-
  !,
  ra_to_cra(Rel,CRel),
  ra_to_cra(Rs,CRs).

% IS NULL condition
ra_to_cra((pi(D,T,As,sigma(is_null(Rel),Rs),G,H,O),AS),
          (pi(D,T,As,sigma(is_null(CRel),CRs),G,H,O),AS)) :-
  !,
  ra_to_cra(Rel,CRel),
  ra_to_cra(Rs,CRs).

% Sigma condition
ra_to_cra((pi(D,T,As,sigma(C,Rs),G,H,O),AS),
          (pi(D,T,As,sigma(SC,CRs),G,H,O),AS)) :-
  !,
  simplify_cond(C,SC),
  ra_to_cra(Rs,CRs).
    
% TIMES
ra_to_cra(times(R1,R2),times(CR1,CR2)) :-
  !,
  ra_to_cra(R1,CR1),
  ra_to_cra(R2,CR2).

% INNER JOIN
ra_to_cra((inner_join(R1,R2,C),AS),(inner_join(CR1,CR2,C),AS)) :-
  !,
  ra_to_cra(R1,CR1),
  ra_to_cra(R2,CR2).
  
% OUTER JOINS
ra_to_cra((left_join(R1,R2,C),AS),(left_join(CR1,CR2,C),AS)) :-
  !,
  ra_to_cra(R1,CR1),
  ra_to_cra(R2,CR2).
ra_to_cra((right_join(R1,R2,C),AS),(right_join(CR1,CR2,C),AS)) :-
  !,
  ra_to_cra(R1,CR1),
  ra_to_cra(R2,CR2).  
ra_to_cra((full_join(R1,R2,C),AS),(full_join(CR1,CR2,C),AS)) :-
  !,
  ra_to_cra(R1,CR1),
  ra_to_cra(R2,CR2).
  
% Table
ra_to_cra((T,AS),(T,AS)) :-
%  my_table('$des',T,_),
  my_table(T,_),
  !.

% UNION operator
% ra_to_cra((union(D,RA1,RA2),AS),(union(D,CRA1,CRA2),AS)) :-
%   !,
%   ra_to_cra(RA1,CRA1),
%   ra_to_cra(RA2,CRA2). 
% WARNING: Renamings are lost for internal unions
ra_to_cra((union(D,RA1,RA2),AS),(union(D,CRAS),AS)) :-
  !,
  union_ra_to_cras((union(D,RA1,RA2),AS),D,CRAS). 

% MINUS operator
ra_to_cra((minus(D,RA1,RA2),AS),(minus(D,CRA1,CRA2),AS)) :-
  !,
  ra_to_cra(RA1,CRA1),
  ra_to_cra(RA2,CRA2). 

% INTERSECT operator
ra_to_cra((intersect(D,RA1,RA2),AS),(intersect(D,CRA1,CRA2),AS)) :-
  !,
  ra_to_cra(RA1,CRA1),
  ra_to_cra(RA2,CRA2). 


% Argument
ra_to_cra(A,A) :-
  !.
  
  
ra_to_cra_list([],[]). 
ra_to_cra_list([R|Rs],[CR|CRs]) :- 
  ra_to_cra(R,CR),
  ra_to_cra_list(Rs,CRs).

  
union_ra_to_cras((union(D,RA1,RA2),_AS),D,CRAS) :-
  !,
  union_ra_to_cras(RA1,D,CRA1S),
  union_ra_to_cras(RA2,D,CRA2S),
  append(CRA1S,CRA2S,CRAS).
union_ra_to_cras(RA,_D,[CRA]) :-
  ra_to_cra(RA,CRA).


% Basic condition
%basic_condition(true) :-
%  !.
%basic_condition(false) :-
%  !.
%basic_condition(C) :-
%  C=..[Op,LA,RA],
%  map_cond(Op,_),
%  my_sql_constant_or_column(LA),
%  my_sql_constant_or_column(RA),
%  !.
%basic_condition(not(C)) :-
%  basic_condition(C),
%  !.

% Simplify condition
simplify_cond(true,true) :-
  !.
simplify_cond(false,false) :-
  !.
simplify_cond(not(true),false) :-
  !.
simplify_cond(not(false),true) :-
  !.
simplify_cond(not(not(C)),SC) :-
  !,
  simplify_cond(C,SC).
simplify_cond(not(and(C1,C2)),SC) :-
  !,
  complement_cond(C1,NC1),
  complement_cond(C2,NC2),
  simplify_cond(or(NC1,NC2),SC).
simplify_cond(not(or(C1,C2)),SC) :-
  !,
  complement_cond(C1,NC1),
  complement_cond(C2,NC2),
  simplify_cond(and(NC1,NC2),SC).
simplify_cond(not(C),CC) :-
  complement_cond(C,CC),
  !.
simplify_cond(and(true,C),SC) :-
  !,
  simplify_cond(C,SC).
simplify_cond(and(C,true),SC) :-
  !,
  simplify_cond(C,SC).
simplify_cond(and(C1,C2),and(SC1,SC2)) :-
  !,
  simplify_cond(C1,SC1),
  simplify_cond(C2,SC2).
simplify_cond(or(true,_C),true) :-
  !.
simplify_cond(or(_C,true),true) :-
  !.
simplify_cond(or(C1,C2),or(SC1,SC2)) :-
  !,
  simplify_cond(C1,SC1),
  simplify_cond(C2,SC2).
simplify_cond(C,C).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cra_to_dl(+CRAst,+Number,-LastNbr,+Mapping,-OMapping,
%             +Renaming,-ORenaming,-DLsts) 
% Translates a Canonical Relational Algebra Syntactic Tree 
% CRAst into a list of Datalog Syntactic Trees DLsts
% Mapping holds the correspondence between table columns and
% goal arguments 
% Renaming holds the already computed table and subquery renamings 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cra_to_dl((not(R),[RR|RArgs]),N,LN,IMap,OMap,IRen,ORen,DLsts) :-
  cra_to_dl((R,[RR|RArgs]),N,LN,IMap,OMap,IRen,ORen,DLsts1),
  length(RArgs,Ar),
  negate_head_rule_list(DLsts1,RR/Ar,DLsts).
cra_to_dl((with(R,Rs),[RR|RArgs]),N,LN,IMap,OMap,IRen,ORen,DLsts) :-
  !,
  N1 is N+1,
  cra_to_dl_with_list(Rs,N1,N2,IRen,[],DLsts1),
  IMap1=IMap,IRen1=IRen,
  rules_from_hyp_program(CRs,DLsts1),
  N3 is N2+1,
  cra_to_dl(R,N3,LN,IMap1,_Map2,IRen1,ORen,DLsts2),
  DLsts2 = [':-'(H2,_)|_],
  H2 =.. [_|A2s],
  head_name(RR,N,_,PN),
  HH2 =.. [PN|A2s],
  append([':-'(HH2,'=>'(CRs,H2))],DLsts2,DLsts),
%   simplify_arglist_expr(RArgs,SRArgs),
%   map_rel_id_var(RR,SRArgs,A2s,Map2,OMap).
  build_mapping_from_schema_and_head_vars(RR,RArgs,A2s,OMap).
% cra_to_dl((union(distinct,R1,R2),[RR|RArgs]),N,LN,IMap,OMap,IRen,ORen,[':-'(L,distinct(G))|DLsts]) :-
%   !,
%   head_name(RR,N,N1,P),
%   get_new_predicate_name(p,N1,N2,UP),
%   cra_to_dl((union(all,R1,R2),[UP|RArgs]),N2,LN,IMap,Map1,IRen,ORen,DLsts),
%   DLsts=[':-'(H,_B)|_],
%   H=..[F|Args],
%   G=..[F|Args],
%   L=..[P|Args],
%   simplify_arglist_expr(RArgs,SRArgs),
%   map_rel_id_var(RR,SRArgs,Args,Map1,OMap).
% cra_to_dl((union(all,R1,R2),[RR|RArgs]),N,LN,IMap,OMap,IRen,ORen,DLsts) :-
%   !,
%   N1 is N+1,
%   cra_to_dl(R1,N1,N2,IMap,Map1,IRen,SRen,DLsts1),
%   N3 is N2+1,
%   DLsts1 = [':-'(H1,_)|_],
%   cra_to_dl(R2,N3,LN,IMap,_Map2,SRen,ORen,DLsts2),
%   DLsts2 = [':-'(H2,_)|_],
%   H1 =.. [_|A1s],
%   H2 =.. [_|A2s],
%   head_name(RR,N,_,PN),
%   HH1 =.. [PN|A1s],
%   HH2 =.. [PN|A2s],
%   append([':-'(HH1,H1),':-'(HH2,H2)|DLsts1],DLsts2,DLsts),
%   simplify_arglist_expr(RArgs,SRArgs),
%   map_rel_id_var(RR,SRArgs,A1s,Map1,OMap).
cra_to_dl((union(distinct,Rs),[RR|RArgs]),N,LN,IMap,OMap,IRen,ORen,[':-'(L,distinct(G))|DLsts]) :-
  !,
  head_name(RR,N,N1,P),
  get_new_predicate_name(p,N1,N2,UP),
  cra_to_dl((union(all,Rs),[UP|RArgs]),N2,LN,IMap,_Map1,IRen,ORen,DLsts),
  DLsts=[':-'(H,_B)|_],
  H=..[F|Args],
  length(Args,Arity),
  length(GArgs,Arity),
  bind_constant_list(Args,GArgs),
  G=..[F|GArgs],
  L=..[P|GArgs],
%  simplify_arglist_expr(RArgs,SRArgs),
%   map_rel_id_var(RR,SRArgs,Args,Map1,OMap).
  build_mapping_from_schema_and_head_vars(RR,RArgs,GArgs,OMap).
cra_to_dl((union(all,Rs),[RR|RArgs]),N,LN,IMap,OMap,IRen,ORen,DLsts) :-
  !,
  head_name(RR,N,_,PN),
  cra_to_dl_list(Rs,RR,PN,N,LN,IMap,_Map1,_OMap,IRen,ORen,DLsts),
%  simplify_arglist_expr(RArgs,SRArgs),
  DLsts = [':-'(H,_)|_],
  H =.. [_|As],
%  map_rel_id_var(RR,SRArgs,As,Map1,OMap).
  build_mapping_from_schema_and_head_vars(RR,RArgs,As,OMap).
cra_to_dl((minus(_D,R1,R2),[RR|RArgs]),N,LN,IMap,OMap,IRen,ORen,DLsts) :-
  !,
  N1 is N+1,
  cra_to_dl(R1,N1,N2,IMap,Map1,IRen,SRen,DLsts1),
  N3 is N2+1,
  DLsts1 = [':-'(SG1,_)|_],
  cra_to_dl(R2,N3,LN,Map1,_Map2,SRen,ORen,DLsts2),
  DLsts2 = [':-'(H2,_)|_],
  SG1 =.. [_|A1s],
  H2 =.. [P2|_],
  SG2 =.. [P2|A1s],
  head_name(RR,N,_,PN),
  HH1 =.. [PN|A1s],
  append([':-'(HH1,(SG1,not(SG2)))|DLsts1],DLsts2,DLsts),
%   simplify_arglist_expr(RArgs,SRArgs),
%   map_rel_id_var(RR,SRArgs,A1s,Map2,OMap).
  build_mapping_from_schema_and_head_vars(RR,RArgs,A1s,OMap).
cra_to_dl((intersect(_D,R1,R2),[RR|RArgs]),N,LN,IMap,OMap,IRen,ORen,DLsts) :-
  !,
  N1 is N+1,
  cra_to_dl(R1,N1,N2,IMap,Map1,IRen,SRen,DLsts1),
  N3 is N2+1,
  DLsts1 = [':-'(SG1,_)|_],
  cra_to_dl(R2,N3,LN,Map1,_Map2,SRen,ORen,DLsts2),
  DLsts2 = [':-'(H2,_)|_],
  SG1 =.. [_|A1s],
  H2 =.. [P2|_],
  SG2 =.. [P2|A1s],
  head_name(RR,N,_,PN),
  HH1 =.. [PN|A1s],
  append([':-'(HH1,(SG1,SG2))|DLsts1],DLsts2,DLsts),
%   simplify_arglist_expr(RArgs,SRArgs),
%   map_rel_id_var(RR,SRArgs,A1s,Map2,OMap).
  build_mapping_from_schema_and_head_vars(RR,RArgs,A1s,OMap).
% Build the datalog rule for a pi operator:
% - rel_subgoals builds the conjunction of source relations (times)
%   It also computes the mapping of variables' subgoals to columns
% - exprs_subgoals builds the subgoals for expressions in the projection list
% - map_cols maps the projection list (ArgList) with the head arguments
% - build_id generates the name of the predicate (head)
% - cond_subgoals adds the condition to the subgoals
cra_to_dl((pi(DistinctAll,TopN,ArgList,
           sigma(Condition,Relation),
           group_by(GroupByList),
           having(HavingCondition),
           order_by(OrderByList,OrderingList)),
           [RR|ArgList]),
            N,LN,IMap,OMap,IRen,ORen,[':-'(Head,Body)|DLsts]) :-
  !,
  head_name(RR,N,N1,PN),
  rel_subgoals(Relation,N1,N2,RSGs,DLsts1,IMap,Map1,IRen,Ren1),
  simplify_arglist_expr(ArgList,SArgList),
  exprs_subgoals(project,SArgList,RR,IsAggr,N2,N21,Map1,Map2,Ren1,_Ren11,DLsts11,ESGs),
  exprs_subgoals(order,OrderByList,RR,IsAggr,N2,_N21,Map2,Map3,Ren1,Ren111,DLsts12,OBSGs),
  map_cols(project,SArgList,Map3,Ren1,Args),
  map_cols(group,GroupByList,Map3,Ren1,GroupByArgs),
  map_cols(order,OrderByList,Map3,Ren1,OrderByArgs),
  Head =.. [PN|Args],
  cond_subgoals(sigma,Condition,N21,N3,Map1,_Map3,Ren1,_Ren2,RSGs,DLsts2,WSGs),
  cond_subgoals(having,HavingCondition,N3,N4,Map3,_OMap,Ren111,_ORen,RSGs,DLsts3,HSGs),
  build_mapping_from_schema_and_head_vars(RR,ArgList,Args,OMap),
  ORen=Ren1,
%  ORen=IRen,  ::WARNING
  concat_lists([DLsts1,DLsts11,DLsts12,DLsts2,DLsts3],DLsts5),
  (GroupByList==[],HSGs=true -> true ; IsAggr=true),
  apply_group_by(IsAggr,RSGs,ESGs,OBSGs,WSGs,HSGs,GroupByArgs,Body1),
  apply_distinct(DistinctAll,N4,N5,Args,Body1,Body2,DLsts5,DLsts6),
  apply_order_by(N5,LN,OrderByArgs,OrderingList,Body2,Body3,DLsts6,DLsts),
  apply_top(TopN,Body3,Body).
  
  
cra_to_dl_list([],_,_,N,N,Map,_,Map,Ren,Ren,[]).
cra_to_dl_list([R|Rs],RR,PN,N,LN,IMap,Map1,OMap,IRen,ORen,[':-'(HH,H)|DLsts]) :-
  !,
  N1 is N+1,
  cra_to_dl(R,N1,N2,IMap,Map1,IRen,SRen,DLsts1),
  DLsts1 = [':-'(H,_)|_],
  H =.. [_|As],
  HH =.. [PN|As],
  cra_to_dl_list(Rs,RR,PN,N2,LN,IMap,_,OMap,SRen,ORen,DLsts2),
  append(DLsts1,DLsts2,DLsts).

  
bind_constant_list([],[]).
bind_constant_list([X|Xs],[Y|Ys]) :-
  ground(X),
  !,
  X=Y,
  bind_constant_list(Xs,Ys).
bind_constant_list([_X|Xs],[_Y|Ys]) :-
  bind_constant_list(Xs,Ys).


head_name(R,N,N1,HN) :-
  (atom_concat('$',_,R)
   ->
    get_new_predicate_name(p,N,N1,HN)
   ;
    HN=R,
    N1=N).
  
negate_head_rule_list([],_NA,[]). 
negate_head_rule_list([':-'(H,B)|Rs],N/A,[':-'('-'(H),B)|NRs]) :-
  functor(H,N,A),
  !,
  negate_head_rule_list(Rs,N/A,NRs). 
negate_head_rule_list([H|Rs],N/A,['-'(H)|NRs]) :-
  functor(H,N,A),
  !,
  negate_head_rule_list(Rs,N/A,NRs). 
negate_head_rule_list([R|Rs],N/A,[R|NRs]) :-
  negate_head_rule_list(Rs,N/A,NRs). 

cra_to_dl_with_list([],N,N,_Ren,DLsts,DLsts).
cra_to_dl_with_list([R|Rs],IN,ON,Ren,IDLsts,ODLsts) :-
  cra_to_dl(R,IN,TN,[],_OMap,Ren,_ORen,DLsts),
  append(IDLsts,DLsts,TDLsts),
  cra_to_dl_with_list(Rs,TN,ON,Ren,TDLsts,ODLsts).

    
build_mapping_from_schema_and_head_vars(_T,[],[],[]).
build_mapping_from_schema_and_head_vars(T,[attr(_T,C,R)|Args],[Var|Vars],[(Var,T,C,R)|Maps]) :-
  !,
  build_mapping_from_schema_and_head_vars(T,Args,Vars,Maps).
build_mapping_from_schema_and_head_vars(T,[expr(_E,C,_R)|Args],[Var|Vars],[(Var,T,C)|Maps]) :-
  build_mapping_from_schema_and_head_vars(T,Args,Vars,Maps).

  
apply_group_by(IsAggr,RSGs,ESGs,OBSGs,WSGs,_HSGs,_GroupByArgs,Body) :-
  var(IsAggr),
  !, 
  append_goals(RSGs,ESGs,SGs1),
  append_goals(SGs1,OBSGs,SGs2),
  append_goals(SGs2,WSGs,SGs),
  reorder_goals(SGs,Body).
%apply_group_by(true,RSGs,ESGs,OBSGs,WSGs,HSGs,GroupByArgs,group_by(OGSGs,Ps,GroupByArgs,OCSGs)) :-
apply_group_by(true,RSGs,ESGs,OBSGs,WSGs,HSGs,GroupByArgs,group_by(OGSGs,GroupByArgs,OCSGs)) :-
  append_goals(RSGs,OBSGs,SGs1),
  append_goals(SGs1,WSGs,GSGs),
  reorder_goals(GSGs,OGSGs),
  append_goals(ESGs,HSGs,CSGs),
  reorder_goals(CSGs,OCSGs).
%   ,
%   group_by_positions(GroupByArgs,OGSGs,Ps).
  
group_by_positions(Vs,G,Ps) :-
  G=..[_|Args],
  get_arg_position_list(Vs,Args,Ps).

apply_order_by(N,N,[],_Os,Body,Body,DLsts,DLsts).
apply_order_by(N,N,Args,Os,Goal,order_by(Goal,Args,Os),DLsts,DLsts) :-
  my_literal(Goal),
  !.
apply_order_by(N,LN,Args,Os,Body,order_by(Head,Args,Os),DLsts,[':-'(Head,Body)|DLsts]) :-
  get_new_predicate_name(p,N,LN,PN),
  relevant_vars(Body,BVs),
  Head =.. [PN|BVs].
  
apply_distinct(all,N,N,_Args,Body,Body,DLsts,DLsts).
apply_distinct(distinct,N,N,Args,Goal,distinct(Goal),DLsts,DLsts) :-
  my_literal(Goal),
  term_variables(Args,Vars1),
  term_variables(Goal,Vars2),
  Vars1==Vars2,
  !.
apply_distinct(distinct,N,LN,Args,Body,distinct(Head),DLsts,[':-'(Head,Body)|DLsts]) :-
  get_new_predicate_name(p,N,LN,PN),
  Head =.. [PN|Args].
  
apply_top(top(all),Body,Body) :-
  !.
apply_top(top(TopN),Body,top(TopN,Body)).
  
% Simplification of the projection list  
simplify_arglist_expr(*,*).
simplify_arglist_expr([],[]).
simplify_arglist_expr([expr(attr(Rel,C,_RC),AS,_Type)|Args],[attr(Rel,C,AS)|SArgs]) :-
  !,
  simplify_arglist_expr(Args,SArgs).
simplify_arglist_expr([expr(expr_ref(Rel,C),AS,_Type)|Args],[attr(Rel,C,AS)|SArgs]) :-
  !,
  simplify_arglist_expr(Args,SArgs).
simplify_arglist_expr([Arg|Args],[Arg|SArgs]) :-
  !,
  simplify_arglist_expr(Args,SArgs).

% Subgoals built for the expressions in the projection list
exprs_subgoals(Scope,ArgList,RR,IsAggr,N,LN,IMap,OMap,IRen,ORen,DLs,SGs) :-
  build_exprs_mappings(ArgList,RR,IMap,IMap1),
  build_exprs_subgoals(Scope,ArgList,IsAggr,N,LN,IMap1,OMap,IRen,ORen,DLs,SGs).

build_exprs_mappings([],_RR,Map,Map).
build_exprs_mappings([expr(_SQLExpr,AS,_Type)|Args],RR,IMap,OMap) :-
  build_exprs_mappings(Args,RR,[(_Var,RR,AS)|IMap],OMap).
build_exprs_mappings([_Arg|Args],RR,IMap,OMap) :-
  build_exprs_mappings(Args,RR,IMap,OMap).

build_exprs_subgoals(_Scope,[],_IsAggr,N,N,Map,Map,Ren,Ren,[],true).
build_exprs_subgoals(Scope,[expr(SQLExpr,AS,Type)|Args],IsAggr,N,LN,IMap,OMap,IRen,ORen,DLs,SGs) :-
  !,
  build_expr_subgoals(Scope,expr(SQLExpr,AS,Type),IsAggr,N,N1,IMap,IMap1,IRen,IRen1,EDLs,ESGs),
  build_exprs_subgoals(Scope,Args,IsAggr,N1,LN,IMap1,OMap,IRen1,ORen,NDLs,NSGs),
  append(EDLs,NDLs,DLs),
  append_goals(ESGs,NSGs,SGs).
build_exprs_subgoals(Scope,[_Arg|Args],IsAggr,N,LN,IMap,OMap,IRen,ORen,DLs,SGs) :-
  build_exprs_subgoals(Scope,Args,IsAggr,N,LN,IMap,OMap,IRen,ORen,DLs,SGs).

build_expr_subgoals(Scope,expr(SQLExpr,AS,_Type),IsAggr,N,LN,IMap,OMap,IRen,ORen,EDLsts,SGs) :-
  translate_expr_varcte(Scope,SQLExpr,IsAggr,N,LN,IMap,OMap,IRen,ORen,EDLsts,DLExpr,ESGs),
  (var(DLExpr)
   ->
    Var=DLExpr,
    SGs=ESGs
   ;
    append_goals((Var=DLExpr),ESGs,SGs)
  ),
%  translate_expr(SQLExpr,DLExpr,IsAggr,Map,Ren),
  ((var(DLExpr),    % Expression reference
    member((Var,_,AS),OMap),
    DLExpr\==Var)   % Avoid autoreferences
  ;
   member((Var,_,AS),OMap)
  ),
  !. % WARNING. 10/08/2014
% build_expr_subgoals(expr(SQLExpr,AS,Type),IsAggr,Map,Ren,SG) :-
%   translate_expr(SQLExpr,DLExpr,IsAggr,Map,Ren),
%   member((Var,_,AS),Map),
%   (Type = number(_N) ->
%     ((abstract_nulls(DLExpr,NDLExpr),my_ground(NDLExpr)) ->
%      Var is DLExpr,
%      SG=true
%     ;
%      SG=is(Var,DLExpr))
%    ;
%     SG='='(Var,DLExpr)
%    ).

translate_expr(_Scope,cte(Cte,_Type),Cte,_IsAggr,_Map,_Ren) :-
  !.
translate_expr(Scope,attr(Rel,Col,AS),Var,_IsAggr,Map,Ren) :-
  !,
  map_cols(Scope,[attr(Rel,Col,AS)],Map,Ren,[Var]).
% Reference to an expression
% translate_expr(expr_ref(Rel,AS),Var,_IsAggr,Map,_Ren) :-
%   !,
%   member((Var,Rel,AS),Map).
translate_expr(_Scope,expr_ref(Rel,AS),Var,_IsAggr,Map,_Ren) :- % Use attribute if available
  member((Var,Rel,AS,_),Map),
  !.
translate_expr(_Scope,expr_ref(Rel,AS),Var,_IsAggr,Map,_Ren) :-
  member((Var,Rel,AS),Map),
  !.
% % CRA statement
% translate_expr(CRA,DLE,IsAggr,Map,Ren) :- 
%   is_CRA(CRA),
%   !, 
%   cra_to_dl(CRA,DLE,IsAggr,1,_LN,[],Map,[],Ren,DLE).
% Expressions, possibly including aggregates, CRAs, ctes, columns
translate_expr(Scope,SQLE,DLE,IsAggr,Map,Ren) :- 
  SQLE =.. [F|SQLArgs],
  !, 
  length(SQLArgs,A),
  (arithmetic_function(F,_,_,aggregate,_,A) ->
   IsAggr=true
   ;
   true
  ),
  translate_expr_list(Scope,SQLArgs,DLArgs,IsAggr,Map,Ren),
  DLE =.. [F|DLArgs].

translate_expr_list(_Scope,[],[],_IsAggr,_Map,_Ren) :-
  !.
translate_expr_list(Scope,[T|Ts],[RT|RTs],IsAggr,Map,Ren) :-
  !, 
  translate_expr(Scope,T,RT,IsAggr,Map,Ren), 
  translate_expr_list(Scope,Ts,RTs,IsAggr,Map,Ren).

% Testing whether input is a Canonical Relational algebra syntax tree
% (Soft test)
% is_CRA((pi(D,T,As,sigma(exists(CRel),CRs),G,H,O),AS)).
% is_CRA((union(D,CRA1,CRA2),AS)).
% is_CRA((minus(D,CRA1,CRA2),AS)).
% is_CRA((intersect(D,CRA1,CRA2),AS)).

% Subgoals for the SQL relation
rel_subgoals(times(RelA,B),N,LN,(SGA,As),DLsts,IMap,OMap,IRen,ORen) :-
  !, 
  rel_subgoals(RelA,N,N1,SGA,DLsts1,IMap,SGMap,IRen,SRen), 
  rel_subgoals(B,N1,LN,As,DLsts2,SGMap,OMap,SRen,ORen),
  append(DLsts1,DLsts2,DLsts).
rel_subgoals((inner_join(RelA,RelB,C),[RR|ArgList]),N,LN,G,DLsts,IMap,OMap,IRen,ORen) :-
  !, 
  rel_subgoals(RelA,N,N1,SGA,DLstsA,IMap,Map1,IRen,Ren1), 
  rel_subgoals(RelB,N1,N2,SGB,DLstsB,Map1,Map2,Ren1,Ren2), 
  apply_eq_ren(RR,ArgList,Map2,Map3),
  on_subgoals(C,N2,LN,Map3,_OMap,Ren2,ORen,DLstsC,SGC),
  concat_lists([DLstsA,DLstsB,DLstsC],DLsts),
  append_goals_list([SGA,SGB,SGC],G),
  map_cols(project,ArgList,Map3,Ren1,Args),
  build_mapping_from_schema_and_head_vars(RR,ArgList,Args,Map4),
  (is_system_identifier(RR)
   ->
    append(Map3,Map4,OMap)
   ;
    OMap=Map4).
rel_subgoals((left_join(RelA,RelB,C),[RR|ArgList]),N,LN,lj(SGA,SGB,SGC),DLsts,IMap,OMap,IRen,ORen) :-
  !, 
  rel_subgoals(RelA,N,N1,SGA,DLstsA,IMap,Map1,IRen,Ren1), 
  rel_subgoals(RelB,N1,N2,SGB,DLstsB,Map1,Map2,Ren1,Ren2), 
  apply_eq_ren(RR,ArgList,Map2,Map3),
  on_subgoals(C,N2,LN,Map3,_OMap,Ren2,ORen,DLstsC,SGC),
  concat_lists([DLstsA,DLstsB,DLstsC],DLsts),
  map_cols(project,ArgList,Map3,Ren1,Args),
  build_mapping_from_schema_and_head_vars(RR,ArgList,Args,Map4),
  (is_system_identifier(RR)
   ->
    append(Map3,Map4,OMap)
   ;
    OMap=Map4).
rel_subgoals((right_join(RelA,RelB,C),[RR|ArgList]),N,LN,rj(SGA,SGB,SGC),DLsts,IMap,OMap,IRen,ORen) :-
  !, 
  rel_subgoals(RelA,N,N1,SGA,DLstsA,IMap,Map1,IRen,Ren1), 
  rel_subgoals(RelB,N1,N2,SGB,DLstsB,Map1,Map2,Ren1,Ren2), 
  apply_eq_ren(RR,ArgList,Map2,Map3),
  on_subgoals(C,N2,LN,Map3,_OMap,Ren2,ORen,DLstsC,SGC),
  concat_lists([DLstsA,DLstsB,DLstsC],DLsts),
  map_cols(project,ArgList,Map3,Ren1,Args),
  build_mapping_from_schema_and_head_vars(RR,ArgList,Args,Map4),
  (is_system_identifier(RR)
   ->
    append(Map3,Map4,OMap)
   ;
    OMap=Map4).
rel_subgoals((full_join(RelA,RelB,C),[RR|ArgList]),N,LN,fj(SGA,SGB,SGC),DLsts,IMap,OMap,IRen,ORen) :-
  !, 
  rel_subgoals(RelA,N,N1,SGA,DLstsA,IMap,Map1,IRen,Ren1), 
  rel_subgoals(RelB,N1,N2,SGB,DLstsB,Map1,Map2,Ren1,Ren2), 
  apply_eq_ren(RR,ArgList,Map2,Map3),
  on_subgoals(C,N2,LN,Map3,_OMap,Ren2,ORen,DLstsC,SGC),
  concat_lists([DLstsA,DLstsB,DLstsC],DLsts),
  map_cols(project,ArgList,Map3,Ren1,Args),
  build_mapping_from_schema_and_head_vars(RR,ArgList,Args,Map4),
  (is_system_identifier(RR)
   ->
    append(Map3,Map4,OMap)
   ;
    OMap=Map4).
% Natural full join:
rel_subgoals((full_join(RelA,RelB,C,Cs),[RR|ArgList]),N,LN,(fj(SGA,SGB,SGC),SNN),DLsts,IMap,OMap,IRen,ORen) :-
  !, 
  rel_subgoals(RelA,N,N1,SGA,DLstsA,IMap,Map1,IRen,Ren1), 
  rel_subgoals(RelB,N1,N2,SGB,DLstsB,Map1,Map2,Ren1,Ren2), 
  apply_eq_ren(RR,ArgList,Map2,Map3),
  on_subgoals(C,N2,N3,Map3,Map4,Ren2,Ren3,DLstsC,SGC),
  nfj_on_subgoals(C,Cs,N3,LN,Map4,Map5,Ren3,ORen,SNN),
  concat_lists([DLstsA,DLstsB,DLstsC],DLsts),
  map_cols(project,ArgList,Map5,Ren1,Args),
  build_mapping_from_schema_and_head_vars(RR,ArgList,Args,Map6),
  (is_system_identifier(RR)
   ->
    append(Map5,Map6,OMap)
   ;
    OMap=Map6).
rel_subgoals(Rel,N,LN,SG,DLsts,IMap,OMap,IRen,ORen) :-
  build_rel_subgoal(Rel,N,LN,SG,DLsts,IMap,OMap,IRen,ORen).

build_rel_subgoal((Table,[TableId|RArgs]),N,N,SG,[],IMap,OMap,IRen,ORen) :-
%  my_table('$des',Table,Arity), 
  my_table(Table,Arity), 
  !,
  Length is Arity+1, 
  length(SGs,Length),
  SGs=[Table|Vars], 
  SG=..SGs, 
  get_renamings_schema(RArgs,ARens),
  my_set_union([(Table,TableId)],IRen,ORen),
  map_table_id_var((Table,TableId),Vars,ARens,1,IMap,OMap,ORen).
build_rel_subgoal((pi(D,T,ArgList,R,G,H,O),[RR|RArgs]),N,LN,SG,DLsts,IMap,OMap,IRen,IRen) :-
  !,
  N1 is N+1,
  cra_to_dl((pi(D,T,ArgList,R,G,H,O),[RR|RArgs]),N1,LN,IMap,Map1,IRen,_ORen,DLsts),
  append(IMap,Map1,Map2),
  DLsts = [':-'(SG,_)|_],
  SG=..[_|Vars],
  map_rel_id_var(RR,ArgList,Vars,Map2,OMap).
build_rel_subgoal(Rel,N,LN,SG,DLsts,IMap,OMap,IRen,ORen) :-
  Rel=(_,[RR|ArgList]),
  N1 is N+1,
  cra_to_dl(Rel,N1,LN,IMap,Map1,IRen,ORen,DLsts),
  append(IMap,Map1,Map2),
  DLsts = [':-'(SG,_)|_],
  SG=..[_|Vars],
  map_rel_id_var(RR,ArgList,Vars,Map2,OMap).
% build_rel_subgoal(Rel,N,LN,SG,DLsts,IMap,OMap,IRen,ORen) :-
%   N1 is N+1,
%   cra_to_dl(Rel,N1,LN,IMap,Map1,IRen,ORen,DLsts),
%   append(IMap,Map1,OMap),
%   DLsts = [':-'(SG,_)|_].

% map_rel_id_var(+AS,+ArgList,+Vars,+IMap,-OMap)
% Maps an SQL relation id to a variable
map_rel_id_var(_AS,[],[],Mapping,Mapping).
% map_rel_id_var(AS,[attr(_,Col,ColRen)|Cols],[Var|Vars],IMap,OMap) :-
%   !,
%   map_rel_id_var(AS,Cols,Vars,[(Var,AS,Col),(Var,AS,ColRen)|IMap],OMap).
% map_rel_id_var(AS,[attr(_Rel,Col,ColRen)|Cols],[Var|Vars],IMap,OMap) :-
%   !,
%   Map1 = (Var,AS,Col,ColRen),
% %  Map2 = (Var,Rel,Col,ColRen),
%   add_to_mapping_list([Map1],IMap,IMap1),
% %  add_to_mapping_list([Map1,Map2],IMap,IMap1),
%   map_rel_id_var(AS,Cols,Vars,IMap1,OMap).
map_rel_id_var(AS,[attr(Rel,Col,ColRen)|Cols],[Var|Vars],IMap,OMap) :-
  !,
  (is_system_identifier(AS)
   ->
    Map1 = (Var,Rel,Col,ColRen)
   ;
    Map1 = (Var,AS,Col,ColRen)
  ),
  add_to_mapping_list([Map1],IMap,IMap1),
  map_rel_id_var(AS,Cols,Vars,IMap1,OMap).
map_rel_id_var(AS,[expr(_E,Ren,_Type)|Cols],[Var|Vars],IMap,OMap) :-
  Map = (Var,AS,Ren,Ren),
  (\+ member(Map,IMap) -> IMap1=[Map|IMap] ; IMap1=IMap),
  map_rel_id_var(AS,Cols,Vars,IMap1,OMap).

apply_eq_ren(_RR,[],Map,Map).
apply_eq_ren(RR,[attr(R,C,CR)|Args],IMap,OMap) :-
%  is_system_identifier(RR),
  !,
  add_to_mapping_list([(V,RR,C,CR)],IMap,Map1),
  add_to_mapping_list([(V,R,C,CR)],Map1,Map2),
  apply_eq_ren(RR,Args,Map2,OMap).
apply_eq_ren(RR,[_|Args],IMap,OMap) :-
  apply_eq_ren(RR,Args,IMap,OMap).
  
add_to_mapping_list([],Map,Map).
add_to_mapping_list([Map|Maps],IMap,OMap) :-
  add_to_mapping(Map,IMap,IMap1),
  add_to_mapping_list(Maps,IMap1,OMap).
  
% add_to_mapping(Map,IMap,OMap) :-
%   (\+ member(Map,IMap) -> OMap=[Map|IMap] ; OMap=IMap).
add_to_mapping(Map,IMap,OMap) :-
  (member(Map,IMap) -> OMap=IMap ; OMap=[Map|IMap]).
  
% map_table_id_var(+(Table,TableId),+Vars,+AttrRen,+Nth,+IMap,-OMap,+Renaming)
% Maps a table (or table id) to a variable
map_table_id_var((_Table,_TableId),[],[],_,Mapping,Mapping,_Renaming).
map_table_id_var((Table,TableId),[Var|Vars],[ARen|ARens],N,Mapping,Mapping,Renaming) :-
  member((Table,TableId),Renaming),
%  my_attribute('$des',N,Table,Col,_Type), 
  my_attribute(N,Table,Col,_Type), 
  member((Var,TableId,Col,ARen),Mapping), 
  !,
  N1 is N+1, 
  map_table_id_var((Table,TableId),Vars,ARens,N1,Mapping,Mapping,Renaming).
map_table_id_var((Table,TableId),[Var|Vars],[ARen|ARens],N,IMapping,OMapping,Renaming) :-
  member((Table,TableId),Renaming),
%  my_attribute('$des',N,Table,Col,_Type), 
  my_attribute(N,Table,Col,_Type), 
  N1 is N+1, 
  map_table_id_var((Table,TableId),Vars,ARens,N1,[(Var,TableId,Col,ARen)|IMapping],OMapping,Renaming).

% map_cols(+Scope,+Cols,+Mapping,+Renaming,-Arguments)
% maps a list of projected arguments with the arguments of the head
map_cols(_Scope,[],_Mapping,_Renaming,[]).
map_cols(Scope,[Col|Cols],Mapping,Renaming,[Var|Vars]) :-
  map_col(Scope,Col,Mapping,Renaming,Var),
  map_cols(Scope,Cols,Mapping,Renaming,Vars).

map_col(_Scope,cte(Constant,_Type),_Mapping,_Renaming,Constant) :-
  !.
map_col(_Scope,attr(TableId,Col,ColRen),Mapping,_Renaming,Var) :-
  member((Var,TableId,Col,ColRen),Mapping),
  !.
map_col(_Scope,attr(TableId,Col,_ColRen),Mapping,_Renaming,Var) :-
  member((Var,TableId,_,Col),Mapping),
  !.
map_col(Scope,attr(TableId,Col,_ColRen),Mapping,_Renaming,Var) :-
  (Scope=project ; Scope=order),
  member((Var,TableId,Col,ColRen),Mapping),
  is_system_identifier(ColRen),  % The original column name can be used if it has not been renamed
  !.
map_col(Scope,attr(TableId,Col,_ColRen),Mapping,_Renaming,Var) :-
  (Scope=project ; Scope=order),
  member((Var,TableId,Col),Mapping),
  !.
map_col(Scope,expr(Expr,ColRen,_Type),Mapping,_Renaming,Var) :-
  (Scope=project ; Scope=order),
  member((Var,TableId,ColRen),Mapping), 
  (my_member_term(expr_ref(TableId,ColRen),Expr)  % Circular references are not allowed
   ->
    my_raise_exception(generic,syntax(['Circular reference due to alias ''',ColRen,'''.']),[])
   ;
    true
  ),
  !.
% Table renamings:
map_col(Scope,attr(TableId,Col,ColRen),Mapping,Renaming,Var) :-
  (Scope \== project -> (is_system_identifier(ColRen) ; var(ColRen)) ; true), % For scopes other than project and order_by, the original column name can be used if it has not been renamed
  member(Ren,Renaming),
  (
   (Ren = (Table,TableId),
    is_system_identifier(TableId),  % The original relation name can be used if it has not been renamed
    (
     member((Var,Table,Col,_),Mapping)
     ;
     member((Var,TableId,Col,_),Mapping)
    )
   )
   ;
   (Ren = (TableId,Table),
    is_system_identifier(Table),  % The original relation name can be used if it has not been renamed
    (
     member((Var,TableId,Col,_),Mapping)
     ;
     member((Var,Table,Col,_),Mapping)
    )
   )
  ),
  !.
% map_col(_Scope,attr(TableId,Col,_ColRen),Mapping,Renaming,Var) :-
%   member(Ren,Renaming),
%   (Ren = (Table,TableId),
%    is_system_identifier(TableId)  % The original relation name can be used if it has not been renamed
%   ;
%    Ren = (TableId,Table),
%    is_system_identifier(Table)  % The original relation name can be used if it has not been renamed
%   ),
%   (member((Var,Table,Col,_),Mapping) 
%    ;
%    member((Var,TableId,Col,_),Mapping)
%   ),
%   !.
% Column renamings:
map_col(Scope,attr(TableId,Col,ColRen),Mapping,_Renaming,Var) :-
  Scope\=project,
  is_system_identifier(ColRen),  % The original column name can be used if it has not been renamed
  member((Var,TableId,Col,CR),Mapping), 
  is_system_identifier(CR), % The original attribute can be used if its has not been renamed
  !.
% Alias in join contexts:
map_col(_Scope,attr(TableId,Col,_ColRen),Mapping,Renaming,Var) :-
  member((Table,TableId),Renaming),
  member((Table,Alias),Renaming),
  Alias\==TableId,
  member((Var,Alias,Col,_),Mapping),
  !.  
% map_col(Scope,attr(TableId,_Col,ColRen),Mapping,_Renaming,Var) :-
%   Scope\=project,
%   nonvar(TableId),
%   nonvar(ColRen),
%   member((Var,TableId,_,ColRen),Mapping), 
%   !.
% % map_col(attr(TableId,Col,Col),Mapping,_Renaming,Var) :-
% %   member((Var,TableId,Col,_ColRen),Mapping), 
% %   !.
% % map_col(attr(TableId,Col,_ColRen),Mapping,_Renaming,Var) :-
% %   member((Var,TableId,Col,ColRen),Mapping), 
% %   is_system_identifier(ColRen),
% %   !.
% map_col(Scope,attr(TableId,Col,_ColRen),Mapping,_Renaming,Var) :-
%   Scope\=project,
%   member((Var,TableId,_,Col),Mapping), 
%   !.
% map_col(Scope,attr(Table,Col,_ColRen),Mapping,Renaming,Var) :-
%   Scope\=project,
%   member((TableId,Table),Renaming),
%   member((Var,TableId,Col,_),Mapping), 
%   !.
% References to expressions:
map_col(Scope,attr(Table,Col,_ColRen),Mapping,_Renaming,Var) :-
  Scope\=project,
  member((Var,Table,Col),Mapping), 
  !.
% % map_col(expr(_Expr,ColRen,_Type),Mapping,_Renaming,Var) :-
% %   member((Var,_TableId,ColRen),Mapping), 
% %   !.
map_col(Scope,attr(Table,Col,ColRen),_Mapping,_Renaming,_Var) :-
  ((is_system_identifier(ColRen) ; var(ColRen)) -> ColName=Col ; ColName=ColRen),
  my_raise_exception(unknown_column(Table,ColName,Scope),syntax(''),[]).
map_col(_Scope,expr(_Expr,ColRen,_Type),_Mapping,_Renaming,_Var) :-
  my_raise_exception(invalid_use(ColRen),syntax(''),[]).

% Subgoals for the natural full outer join
nfj_on_subgoals(A1=A2,[attr(R,C,AS)],N,LN,IMap,OMap,IRen,ORen,select_not_null(V1,V2,V)) :-
  !,
  translate_cond(on,A1=A2,N,N1,IMap,Map1,IRen,Ren1,_RelSGs,_DLsts,V1=V2),
  add_to_mapping((V,R,C,AS),Map1,Map2),
  translate_expr(on,attr(R,C,AS),_IsAggr,N1,LN,Map2,Map3,[(select_not_null,R)|Ren1],ORen,[],V,_),
  replace_var_mapping_list([A1,A2],[V,V],Map3,OMap).
nfj_on_subgoals(and(C,Cs),[Attr|Attrs],N,LN,IMap,OMap,IRen,ORen,(SGC,SGCs)) :-
  nfj_on_subgoals(C,[Attr],N,N1,IMap,Map1,IRen,Ren1,SGC),
  nfj_on_subgoals(Cs,Attrs,N1,LN,Map1,OMap,Ren1,ORen,SGCs).

replace_var_mapping_list([],[],Map,Map).
replace_var_mapping_list([A|As],[V|Vs],IMap,OMap) :-
  replace_var_mapping(A,V,IMap,Map1),
  replace_var_mapping_list(As,Vs,Map1,OMap).
  
replace_var_mapping(attr(R,N,A),V,IMap,[(V,R,N,A)|Map1]) :-
  remove_one_element_from_list((_,R,N,A),IMap,Map1).
  

% Subgoals for the ON condition (JOIN clauses)
on_subgoals(C,N,LN,IMap,OMap,IRen,ORen,DLsts,SGs) :-
  cond_subgoals(on,C,N,LN,IMap,OMap,IRen,ORen,true,DLsts,SGs).
  %,
  %(CSGs == ('.') -> OSGs = true ; OSGs = CSGs).

% Subgoals for the WHERE condition (SELECT statements)
cond_subgoals(SOH,C,N,LN,IMap,OMap,IRen,ORen,RelSGs,DLsts,SGs) :-
  translate_cond(SOH,C,N,LN,IMap,OMap,IRen,ORen,RelSGs,DLsts,SGs).

translate_cond(_SOH,true,N,N,Map,Map,Ren,Ren,_RelSGs,[],true) :- 
  !.
translate_cond(_SOH,false,N,N,Map,Map,Ren,Ren,_RelSGs,[],(false)) :- 
  !.
% translate_cond(sigma,'='(A,B),N,N,Map,Map,Ren,Ren,[],(true)) :-
%  map_cols([A,B],Map,Ren,[VA,VB]),
%  (var(VA); var(VB)), !,
%  VA=VB.
%translate_cond(SOH,not(exists(Rel)),N,LN,IMap,IMap,IRen,IRen,RelSGs,DLsts,not(top(1,Goal))) :-
translate_cond(SOH,not(exists(Rel)),N,LN,IMap,IMap,IRen,IRen,RelSGs,DLsts,not(Goal)) :-
  !,
  translate_in_exists_cond(SOH,exists(Rel),N,LN,IMap,_OMap,IRen,_ORen,CorrVars,RelSGs,DLsts1,Goal),
  add_correlated_goals(CorrVars,RelSGs,DLsts1,DLsts).
% translate_cond(SOH,not(in(Args,Rel)),N,LN,IMap,OMap,IRen,ORen,RelSGs,DLsts,not(Goal)) :-
%   !,
%   translate_cond(SOH,not_in(Args,Rel),N,LN,IMap,OMap,IRen,ORen,RelSGs,DLsts,not(Goal)).
% translate_cond(SOH,not_in(Args,Rel),N,LN,IMap,OMap,IRen,ORen,RelSGs,DLsts,not(Goal)) :-
%   translate_in_exists_cond(SOH,in(Args,Rel),N,LN,IMap,OMap,IRen,ORen,CorrVars,RelSGs,DLsts1,Goal),
%   add_correlated_goals(CorrVars,RelSGs,DLsts1,DLsts2),
%   copy_term(DLsts2,DLsts).
translate_cond(SOH,not_in(Args,Rel),N,LN,IMap,OMap,IRen,ORen,RelSGs,DLsts,not(Goal)) :-
  translate_cond(SOH,not(in(Args,Rel)),N,LN,IMap,OMap,IRen,ORen,RelSGs,DLsts,not(Goal)).
translate_cond(SOH,not(C),N,LN,IMap,IMap,IRen,IRen,RelSGs,DLsts,not(NC)) :-
  !,
  translate_cond(SOH,C,N,LN,IMap,_OMap,IRen,_ORen,RelSGs,DLsts,NC).
%translate_cond(_SOH,and(C1,C2),N,LN,IMap,OMap,IRen,ORen,RelSGs,DLsts,NC) :-
translate_cond(_SOH,and(C1,C2),N,LN,Map,Map,IRen,IRen,RelSGs,DLsts,NC) :-
  translate_cond(SOH,C1,N,N1,Map,_Map1,IRen,_Ren1,RelSGs,DLsts1,NC1),
%  translate_cond(SOH,C2,N1,LN,Map1,OMap,Ren1,ORen,RelSGs,DLsts2,NC2),
  translate_cond(SOH,C2,N1,LN,Map,_OMap,IRen,_ORen,RelSGs,DLsts2,NC2),
  append_goals(NC1,NC2,NC),
  append(DLsts1,DLsts2,DLsts).
% translate_cond(_SOH,or(C1,C2),N,LN,IMap,OMap,IRen,IRen,RelSGs,DLsts,(NC1;NC2)) :-
% %translate_cond(on,or(C1,C2),N,LN,IMap,OMap,IRen,IRen,RelSGs,DLsts,(NC1;NC2)) :-
%   translate_cond(SOH,C1,N,N1,IMap,Map1,IRen,_Ren1,RelSGs,DLsts1,NC1),
%   translate_cond(SOH,C2,N1,LN,Map1,OMap,IRen,_ORen,RelSGs,DLsts2,NC2),
%   append(DLsts1,DLsts2,DLsts).
translate_cond(SOH,or(C1,C2),N,LN,IMap,OMap,IRen,IRen,RelSGs,DLsts,or(NC1,NC2)) :-
%   SOH\==on,
  translate_cond(SOH,C1,N,N1,IMap,Map1,IRen,_Ren1,RelSGs,DLsts1,NC1),
  translate_cond(SOH,C2,N1,LN,Map1,OMap,IRen,_ORen,RelSGs,DLsts2,NC2),
  append(DLsts1,DLsts2,DLsts).
translate_cond(SOH,in(Args,Rel),N,LN,IMap,OMap,IRen,ORen,RelSGs,DLsts,Goal) :-
  translate_in_exists_cond(SOH,in(Args,Rel),N,LN,IMap,OMap,IRen,ORen,_CorrVars,RelSGs,DLsts,Goal).
translate_cond(SOH,exists(Rel),N,LN,IMap,IMap,IRen,IRen,RelSGs,DLsts,Goal) :-
  translate_in_exists_cond(SOH,exists(Rel),N,LN,IMap,_OMap,IRen,_ORen,_CorrVars,RelSGs,DLsts,Goal).
%translate_cond(_SOH,is_null(Arg),N,N,Map,Map,Ren,Ren,[],(Var='$NULL'(_ID))) :-
translate_cond(Scope,is_null(Arg),N,N,Map,Map,Ren,Ren,_RelSGs,[],(is_null(Var))) :-
  my_sql_constant_or_column(Arg),
  !,
  map_cols(Scope,[Arg],Map,Ren,[Var]).
translate_cond(_SOH,is_null(Rel),N,LN,IMap,OMap,IRen,IRen,_RelSGs,DLsts,(Goal,GVar='$NULL'(_ID))) :-
  !,
  N1 is N+1,
  get_new_predicate_name(p,N1,_,_PN),
  cra_to_dl(Rel,N1,LN,IMap,OMap,IRen,_ORen,UDLsts),
  add_correlated_vars(IMap,UDLsts,DLsts),
  DLsts = [':-'(Goal,_)|_],
  Goal =.. [_F,GVar].
translate_cond(Scope,C,N,LN,IMap,IMap,IRen,IRen,_RelSGs,DLsts,Goals) :-
  C=..[RelOp,Exp1,Exp2],
  map_cond(RelOp,DLOp),
  !,
  translate_expr_varcte(Scope,Exp1,N,N1,IMap,_Map1,IRen,_Ren1,UDLsts1,VarCte1,UGoals1),
  add_correlated_vars(IMap,UDLsts1,DLsts1),
  add_correlated_vars_goals(UGoals1,DLsts1,Goals1),
  translate_expr_varcte(Scope,Exp2,N1,LN,IMap,_OMap,IRen,_ORen,UDLsts2,VarCte2,UGoals2),
  add_correlated_vars(IMap,UDLsts2,DLsts2),
  add_correlated_vars_goals(UGoals2,DLsts2,Goals2),
  append(DLsts1,DLsts2,DLsts),
  append_goals(Goals1,Goals2,Goals3),
  % WARNING: VarCte1=VarCte2. Incorrect for unions
  % :-type(t(a:int,b:int)),type(s(a:int,b:int))
  % select a from s where b not in ((select a from t where t.a=s.a) union (select a from t where b=1))
  (false,RelOp=(=),var(VarCte1),var(VarCte2) ->
    VarCte1=VarCte2,
    Goals=Goals3
   ;
    NC=..[DLOp,VarCte1,VarCte2],
    append_goals(Goals3,NC,Goals)
  ),
  !.
 
translate_in_exists_cond(Scope,in(Args,Rel),N,LN,IMap,OMap,IRen,ORen,CorrVars,_RelSGs,DLsts,Goal) :-
  !,
  N1 is N+1,
  get_new_predicate_name(p,N1,_,_PN),
  map_cols(Scope,Args,IMap,IRen,Vars),
  cra_to_dl(Rel,N1,LN,IMap,OMap,IRen,ORen,UDLsts),
  add_correlated_vars(IMap,UDLsts,CorrVars,DLsts),
  DLsts = [':-'(Goal,_)|_],
  Goal =.. [_F|GVars],
  append(Vars,_CorrVars,GVars).
translate_in_exists_cond(_SOH,exists(Rel),N,LN,IMap,OMap,IRen,ORen,CorrVars,_RelSGs,DLsts,Goal) :-
  N1 is N+1,
  get_new_predicate_name(p,N1,_,_PN),
  cra_to_dl(Rel,N1,LN,IMap,OMap,IRen,ORen,UDLsts),
  UDLsts=[':-'(Head,_)|_],
  functor(Head,H,_),
  replace_head_list(H,H,UDLsts,RUDLsts),
  add_correlated_vars(IMap,RUDLsts,CorrVars,DLsts),
  DLsts=[':-'(Goal,_)|_].

% add_correlated_vars_goals(true,_DLs,true) :-
%   !.
add_correlated_vars_goals((UGoal,UGoals),DLs,(Goal,Goals)) :-
  !,
  add_correlated_vars_subgoal(UGoal,DLs,Goal),
  add_correlated_vars_goals(UGoals,DLs,Goals).
add_correlated_vars_goals(UGoal,DLs,Goal) :-
  add_correlated_vars_subgoal(UGoal,DLs,Goal).
  
add_correlated_vars_subgoal(UGoal,DLs,Goal) :-
  UGoal=..[F|_UArgs],
  member(':-'(Goal,_),DLs),
  Goal=..[F|_Args],
  !.
add_correlated_vars_subgoal(Goal,_DLs,Goal).
 
translate_expr_varcte(Scope,Exp,N,N1,IMap,OMap,IRen,ORen,DLsts,VarCte,Goals) :-
  translate_expr_varcte(Scope,Exp,_IsAggr,N,N1,IMap,OMap,IRen,ORen,DLsts,VarCte,Goals).
   
translate_expr_varcte(Scope,Exp,IsAggr,N,N1,IMap,OMap,IRen,ORen,DLsts,VarCte,Goals) :-   
  translate_expr(Scope,Exp,IsAggr,N,N1,IMap,OMap,IRen,ORen,DLsts,DLExp,Goals1),
  (my_var_or_constant(DLExp) ->
    VarCte=DLExp,
    Goals=Goals1
    ;
%    tuple_append(VarCte is DLExp,Goals1,Goals)).
    append_goals(VarCte = DLExp,Goals1,Goals)).
   
% my_var_or_constant(T) :-
%   (var(T) ; number(T) ; atom(T) ; T='$NULL'(_ID)), !.
  
% Translating SQL expressions
% Renaming:
% translate_expr(attr(Rel,Col,As),_IsAggr,N,N,Map,Map,Ren,Ren,[],Var,true) :-
%   (member((Var,Rel,Col,_),Map),
%    is_system_identifier(As)
%   ;
%    member((Var,Rel,_,Col),Map)
%   ;
%    member((Var,Rel,Col),Map)
%   ),
%   !.
% Constant or Column:
translate_expr(Scope,C,_IsAggr,N,N,Map,Map,Ren,Ren,[],Var,true) :-
%  my_sql_constant_or_column(C),
  (my_sql_constant(C)
   ;
   C=attr(_,_,_)
  ),
  !,
  map_cols(Scope,[C],Map,Ren,[Var]).
% Unknown attribute
translate_expr(Scope,attr(R,C,As),_IsAggr,_N,_LN,_IMap,_OMap,_IRen,_ORen,_DLsts,_DLE,_Goals) :- 
  (is_system_identifier(As) -> Col=C ; Col=As),
  my_raise_exception(unknown_column(R,Col,Scope),syntax(''),[]).
% Reference to an expression
translate_expr(_Scope,expr_ref(Rel,AS),_IsAggr,N,N,Map,Map,Ren,Ren,[],Var,true) :- % Use attribute if available
  member((Var,Rel,AS,_),Map),
  !.
translate_expr(Scope,expr_ref(Rel,AS),_IsAggr,N,N,Map,Map,Ren,Ren,[],Var,true) :-
  !,
  (member((Var,Rel,AS),Map),
   !
  ;
   my_raise_exception(unknown_column(Rel,AS,Scope),syntax(''),[])).
translate_expr(Scope,expr(expr_ref(Rel,AS),_,_),_IsAggr,N,N,Map,Map,Ren,Ren,[],Var,true) :-
  !,
  (member((Var,Rel,AS),Map),
   !
  ;
   my_raise_exception(unknown_column(Rel,AS,Scope),syntax(''),[])).
% DQL statement
translate_expr(_Scope,Rel,_IsAggr,N,LN,IMap,OMap,IRen,ORen,DLsts,Var,Goal) :-
  my_dql_relation(Rel),
  !,
  N1 is N+1,
  get_new_predicate_name(p,N1,_,_PN),
  cra_to_dl(Rel,N1,LN,IMap,Map1,IRen,ORen,DLsts),
  DLsts=[':-'(Goal,_Body)|_Rules],
  Goal =.. [_F,Var],
  append(IMap,Map1,OMap).
% SQL aggregate or function
translate_expr(Scope,SQLE,IsAggr,N,LN,IMap,OMap,IRen,ORen,DLsts,DLE,Goals) :- 
  SQLE =.. [F|SQLArgs],
  !, 
  length(SQLArgs,A),
  (arithmetic_function(F,_,_,aggregate,_,A) ->
   IsAggr=true
   ;
   true
  ),
  translate_expr_list(Scope,SQLArgs,IsAggr,N,LN,IMap,OMap,IRen,ORen,DLsts,DLArgs,Goals),
  DLE =.. [F|DLArgs].

%translate_expr_list([],N,N,Map,Map,Ren,Ren,[],[],('.')) :-
translate_expr_list(_Scope,[],_IsAggr,N,N,Map,Map,Ren,Ren,[],[],true) :-
  !.
translate_expr_list(Scope,[T|Ts],IsAggr,N,LN,IMap,OMap,IRen,ORen,DLsts,[RT|RTs],Goals) :-
  !, 
  translate_expr(Scope,T,IsAggr,N,N1,IMap,Map1,IRen,Ren1,DLsts1,RT,Goals1), 
  translate_expr_list(Scope,Ts,IsAggr,N1,LN,Map1,OMap,Ren1,ORen,DLsts2,RTs,Goals2),
  append(DLsts1,DLsts2,DLsts),
%  tuple_append(Goals1,Goals2,Goals).
  append_goals(Goals1,Goals2,Goals).
  
my_dql_relation((_SQLst,RR)) :-
  RR \= (_C,_R).

my_sql_constant_or_column(Arg) :-
  my_sql_constant(Arg).
my_sql_constant_or_column(Arg) :-
  my_sql_column(Arg).
  
my_sql_constant(cte(_Cte,_Type)).
my_sql_column(attr(_RT,C,_R)) :-
%  my_attribute('$des',_P,_T,C,_Type),
  my_attribute(_P,_T,C,_Type),
  !.

my_sql_op(Op) :-
  map_cond(Op,_).
my_sql_op(and).
my_sql_op(or).
%my_sql_op(not).
%my_sql_op(in).
%my_sql_op(exists).
%my_dl_op(Op) :-
%  map_cond(_,Op).

% map_cond(+RelationalOperator,-DatalogOperator).
map_cond('<=','=<').
map_cond('=','=').
map_cond('<>','\\='). 
map_cond('!=','\\='). 
map_cond('>=','>=').
map_cond('>','>').
map_cond('<','<').

map_cond('<=_all','=<').
map_cond('=_all','=').
map_cond('<>_all','\\='). 
map_cond('>=_all','>=').
map_cond('>_all','>').
map_cond('<_all','<').

map_cond('<=_any','=<').
map_cond('=_any','=').
map_cond('<>_any','\\='). 
map_cond('>=_any','>=').
map_cond('>_any','>').
map_cond('<_any','<').

% complement
complement_cond(true,false) :-
  !.
complement_cond(false,true) :-
  !.
complement_cond(not(C),CC) :-
  simplify_cond(C,CC),
  !.
complement_cond(and(C1,C2),or(CC1,CC2)) :-
  !,
  complement_cond(C1,CC1),
  complement_cond(C2,CC2).
complement_cond(or(C1,C2),and(CC1,CC2)) :-
  !,
  complement_cond(C1,CC1),
  complement_cond(C2,CC2).
complement_cond(C,NC) :-
  C=..[Op|As],
  map_cond(Op,_),
  !,
  complement_RA_op(Op,NOp),
  NC=..[NOp|As].

% complemented RA operator
complement_RA_op('<=','>').
complement_RA_op('=','<>').
complement_RA_op('<>','='). 
complement_RA_op('>=','<').
complement_RA_op('>','<=').
complement_RA_op('<','>=').

% Add correlated goals (for not(exist) and not_in )
% add_correlated_goals(+CorrVars,+RelSGs,+DLs,-CDLs).
add_correlated_goals(CorrVars,RelSGs,DLs,CDLs) :-
  my_list_to_tuple(RelSGsList,RelSGs),
  goals_with_vars(RelSGsList,CorrVars,GoalList),
  (GoalList==[]
   ->
    CDLs=DLs
   ;
    my_list_to_tuple(GoalList,Goals),
    append_body_goals_list(Goals,CorrVars,DLs,CDLs)).
    
append_body_goals_list(_Goals,_CorrVars,[],[]).
append_body_goals_list(Goals,CorrVars,[':-'(H,B)|DLs],[':-'(H,GsB)|CDLs]) :-
  my_list_to_tuple(Bs,B),
  demanding_goals_with_vars(Bs,CorrVars,DGs),
  DGs\==[],
  append_goals(Goals,B,GsB),
  !,
  append_body_goals_list(Goals,CorrVars,DLs,CDLs).
append_body_goals_list(Goals,CorrVars,[':-'(H,B)|DLs],[':-'(H,B)|CDLs]) :-
  append_body_goals_list(Goals,CorrVars,DLs,CDLs).

goals_with_vars([],_CorrVars,[]).
goals_with_vars([G|Gs],CorrVars,Bs) :-
  term_variables(G,Vs),
  my_set_inter(Vs,CorrVars,[]),
  !,
  goals_with_vars(Gs,CorrVars,Bs).
goals_with_vars([G|Gs],CorrVars,[G|Bs]) :-
  goals_with_vars(Gs,CorrVars,Bs).

demanding_goals_with_vars(Bs,CorrVars,DGs) :-
  demanding_goals_with_vars(Bs,CorrVars,[],_,DGs).
demanding_goals_with_vars([],_CorrVars,SVs,SVs,[]).
demanding_goals_with_vars([not(exists(_Vs,G))|Gs],CorrVars,ISVs,OSVs,DGs) :-
  !,
  demanding_goals_with_vars([not(G)|Gs],CorrVars,ISVs,OSVs,DGs).
demanding_goals_with_vars([G|Gs],CorrVars,ISVs,OSVs,[G|Bs]) :-
  functor(G,F,A),
  is_demanded_predicate(F/A),
  term_variables(G,Vs),
  my_set_inter(Vs,CorrVars,IVs),
  IVs\==[],
  my_set_diff(Vs,ISVs,DVs),
  DVs\==[],
  !,
  demanding_goals_with_vars(Gs,CorrVars,ISVs,OSVs,Bs).
demanding_goals_with_vars([G|Gs],CorrVars,ISVs,OSVs,Bs) :-
  functor(G,F,A),
  is_non_demanded_predicate(F/A),
  !,
  term_variables(G,Vs),
  append(ISVs,Vs,TSVs),
  demanding_goals_with_vars(Gs,CorrVars,TSVs,OSVs,Bs).
demanding_goals_with_vars([_G|Gs],CorrVars,ISVs,OSVs,Bs) :-
  demanding_goals_with_vars(Gs,CorrVars,ISVs,OSVs,Bs).

% Add correlated variables
% add_correlated_vars(_Map,DLsts,DLsts) :-
%   correlation(off),
%   !.
add_correlated_vars(Map,UDLsts,DLsts) :-
  term_variables(Map,MapVars),
  lfp_add_rule_correlated_vars(MapVars,[],CHs,[':-'('$p0','$p1')|UDLsts],[_|CDLsts]),
  add_head_correlated_void_vars_list(CDLsts,CHs,DLsts).
  
% This also returns which are the correlated variables
% add_correlated_vars(+Map,+UDLsts,-CorrVars,-DLsts) :-
add_correlated_vars(Map,UDLsts,CorrVars,DLsts) :-
  add_correlated_vars(Map,UDLsts,DLsts),
  (DLsts==[] ->
    CorrVars=[]
   ;
    UDLsts=[':-'(UH,_)|_],
    DLsts=[':-'(H,_)|_],
    term_variables(UH,UVs),
    term_variables(H,Vs),
    my_set_diff(Vs,UVs,CorrVars)
  ).

add_head_correlated_void_vars_list([],_CHs,[]).
add_head_correlated_void_vars_list([':-'(H,B)|UDLsts],CHs,[':-'(CH,B)|TDLsts]) :-
  add_head_correlated_void_vars(H,CHs,CH),
  add_head_correlated_void_vars_list(UDLsts,CHs,TDLsts).
add_head_correlated_void_vars_list([H|UDLsts],CHs,[H|TDLsts]) :-
  add_head_correlated_void_vars_list(UDLsts,CHs,TDLsts).

add_head_correlated_void_vars(H,CHs,CH) :-
  functor(H,F,A),
  member((F,A1,Vs),CHs),
  !,
  length(Vs,LVs),
  N is A1+LVs-A,
  void_list(N,Voids),
  H=..[F|Args],
  append(Args,Voids,ExtArgs),
  CH=..[F|ExtArgs].
add_head_correlated_void_vars(H,_CHs,H).

void_list(0,[]) :-
  !.
void_list(N,[void|Voids]) :-
  N1 is N-1,
  void_list(N1,Voids).

lfp_add_rule_correlated_vars(MapVars,CHsi,CHso,DLstsi,DLstso) :-
  add_head_correlated_vars_list(MapVars,DLstsi,CHsi,CHs1,DLsts1),
%  set_head_void_args_list(DLsts1,DLsts2),
  DLsts1=DLsts2,
  add_body_correlated_vars_list(DLsts2,CHs1,DLsts3),
  (DLstsi==DLsts3 ->
   DLstso=DLsts3,
   CHso=CHsi
   ;
   lfp_add_rule_correlated_vars(MapVars,CHs1,CHso,DLsts3,DLstso)).

add_head_correlated_vars_list(_MapVars,[],CPs,CPs,[]).
add_head_correlated_vars_list(MapVars,[':-'(H,B)|UDLsts],ICPs,OCPs,[':-'(CH,B)|TDLsts]) :-
  !,
  add_head_correlated_vars(MapVars,H,B,ICPs,NICPs,CH),
  add_head_correlated_vars_list(MapVars,UDLsts,NICPs,OCPs,TDLsts).
add_head_correlated_vars_list(MapVars,[H|UDLsts],ICPs,OCPs,[CH|TDLsts]) :-
  add_head_correlated_vars(MapVars,H,true,ICPs,NICPs,CH),
  add_head_correlated_vars_list(MapVars,UDLsts,NICPs,OCPs,TDLsts).

add_head_correlated_vars(MapVars,H,B,ICPs,OCPs,CH) :-
  term_variables(H,HVars),
  term_variables(B,BVars),
  my_subtract_var(BVars,HVars,FCVars),
  my_intersect_var(FCVars,MapVars,CVars),  
  functor(H,F,A),
  (member((F,AI,Vs),ICPs) ->
    my_union_var(Vs,CVars,UOCVars),
    my_mergesort(UOCVars,'@<',OCVars),
    add_atom_vars(OCVars,H,AI,CH),
    replace_list((F,AI,Vs),(F,AI,OCVars),ICPs,OCPs)
    ;
    my_mergesort(CVars,'@<',OCVars),
    add_atom_vars(OCVars,H,A,CH),
    OCPs=[(F,A,OCVars)|ICPs]).
  
add_atom_vars(Vars,H,A,HC) :-
  H =.. [P|HVars],
  take_N(A,HVars,OVars),
  append(OVars,Vars,HCVars),
  HC =.. [P|HCVars].

% set_head_void_args_list([],[]).
% set_head_void_args_list([':-'(CH,B)|UDLsts],[':-'(VCH,B)|TDLsts]) :-
%   !,
%   set_head_void_args(CH,B,VCH),
%   set_head_void_args_list(UDLsts,TDLsts).
% set_head_void_args_list([CH|UDLsts],[VCH|TDLsts]) :-
%   set_head_void_args(CH,true,VCH),
%   set_head_void_args_list(UDLsts,TDLsts).

% set_head_void_args(CH,B,VCH) :-
%   term_variables(CH,CHVars),
%   term_variables(B,BVars),
%   my_subtract_var(CHVars,BVars,UVars),
%   replace_unsafe_vars(CH,UVars,VCH).
%   
% replace_unsafe_vars(CH,UVars,VCH) :-
%   CH=..[F|Args],
%   replace_unsafe_vars_list(Args,UVars,RArgs),
%   VCH=..[F|RArgs].

% replace_unsafe_vars_list([],_UVars,[]).
% replace_unsafe_vars_list([A|Args],UVars,[void|RArgs]) :-
%   my_membervar(A,UVars),
%   !,
%   replace_unsafe_vars_list(Args,UVars,RArgs).
% replace_unsafe_vars_list([A|Args],UVars,[A|RArgs]) :-
%   replace_unsafe_vars_list(Args,UVars,RArgs).

add_body_correlated_vars_list([],_CHs,[]).
add_body_correlated_vars_list([':-'(H,B)|UDLsts],CHs,[':-'(H,CB)|TDLsts]) :-
  !,
  add_body_correlated_vars(B,CHs,CB),
  add_body_correlated_vars_list(UDLsts,CHs,TDLsts).
add_body_correlated_vars_list([H|UDLsts],CHs,[H|TDLsts]) :-
  add_body_correlated_vars_list(UDLsts,CHs,TDLsts).

add_body_correlated_vars((B,Bs),CHs,(CB,CBs)) :-
  !,
  add_body_correlated_vars(B,CHs,CB),
  add_body_correlated_vars(Bs,CHs,CBs).
add_body_correlated_vars(B,CHs,CB) :-
  B=distinct(G),
  !,
  G=..[F|As],
  (member((F,AI,Vs),CHs),
   add_atom_vars(Vs,G,AI,CG),
   G\==CG   
    ->
    CB=distinct(As,CG)
    ;
    CB=B).
add_body_correlated_vars(B,CHs,CB) :-
  B=distinct(DVs,G),
  !,
  G=..[F|_As],
  (member((F,AI,Vs),CHs) ->
   add_atom_vars(Vs,G,AI,CG),
   CB=distinct(DVs,CG)
  ;
   CB=B
  ).
add_body_correlated_vars(B,CHs,CB) :-
  (B=not(G),
   CB=not(CG)
  ;
   G=B,
   CB=CG
   ),
  !,
  functor(G,F,_A),
  (member((F,AI,Vs),CHs) ->
    add_atom_vars(Vs,G,AI,CG)
    ;
    CG=G).
  

% meta_pred(not/1).
% meta_pred(distinct/1).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% disjunctive_to_conjunctive_ruleNVs_list(+DRuleNVsList,
%   -CRuleNVsList,+IArgsListi,-IArgsListo,-Exploded) 
% Translates a list of Datalog rules with disjunctions (DRuleNVsList)
% into a list of Datalog rules without disjunctions (CRuleNVsList)
% True goals are removed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disjunctive_to_conjunctive_ruleNVs_list([],[],[],[],_Exploded).
disjunctive_to_conjunctive_ruleNVs_list([DRuleNVs|DRuleNVsList],CRuleNVsList,[IArgs|IArgsListi],IArgsListo,Exploded) :-
  DRuleNVs = (':-'(H,B),NVs),
  !,
  disjunctive_to_conjunctive_rule(H,('.'),B,NVs,IArgs,[],RuleNVsList,[],IArgsList1,Exploded),
  disjunctive_to_conjunctive_ruleNVs_list(DRuleNVsList,TRuleNVsList,IArgsListi,IArgsList2,Exploded),
  append(RuleNVsList,TRuleNVsList,CRuleNVsList),
  append(IArgsList1,IArgsList2,IArgsListo).
disjunctive_to_conjunctive_ruleNVs_list([RuleNVs|DRuleNVsList],[RuleNVs|CRuleNVsList],[IArgs|IArgsListi],[IArgs|IArgsListo],Exploded) :-
  disjunctive_to_conjunctive_ruleNVs_list(DRuleNVsList,CRuleNVsList,IArgsListi,IArgsListo,Exploded).
  
disjunctive_to_conjunctive_rule(H,LBs,(B,RBs),NVs,IArgs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,Exploded) :-
  !,
  disjunctive_to_conjunctive_rule_6(H,LBs,B,RBs,NVs,IArgs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,Exploded).
disjunctive_to_conjunctive_rule(H,Bs,B,NVs,IArgs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,Exploded) :-
  disjunctive_to_conjunctive_rule_6(H,Bs,B,('.'),NVs,IArgs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,Exploded).

disjunctive_to_conjunctive_rule_6(H,LBs,(LB,RB),RBs,NVs,IArgs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,Exploded) :-
  !,
  tuple_append(RB,RBs,NRBs),
  disjunctive_to_conjunctive_rule_6(H,LBs,LB,NRBs,NVs,IArgs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,Exploded).
disjunctive_to_conjunctive_rule_6(H,LBs,(LB;RB),RBs,NVs,IArgs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,true) :-
  !,
  copy_term((H,LBs,LB,RBs,NVs),(CLH,CLLBs,CLLB,CLRBs,CLNVs)),
  disjunctive_to_conjunctive_rule(CLH,CLLBs,(CLLB,CLRBs),CLNVs,IArgs,RuleNVsListi,RuleNVsList1,IArgsListi,IArgsList1,Exploded),
  copy_term((H,LBs,RB,RBs,NVs),(CRH,CRLBs,CRLB,CRRBs,CRNVs)),
  disjunctive_to_conjunctive_rule(CRH,CRLBs,(CRLB,CRRBs),CRNVs,IArgs,RuleNVsList1,RuleNVsListo,IArgsList1,IArgsListo,Exploded).
disjunctive_to_conjunctive_rule_6(H,LBs,B,('.'),NVs,IArgs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,_Exploded) :-  
  !,
  tuple_append(LBs,B,NLBs),
  append(RuleNVsListi,[(':-'(H,NLBs),NVs)],RuleNVsListo),
  append(IArgsListi,[IArgs],IArgsListo).
disjunctive_to_conjunctive_rule_6(H,LBs,B,RBs,NVs,IArgs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,Exploded) :-  
  !,
  tuple_append(LBs,B,NLBs),
  disjunctive_to_conjunctive_rule(H,NLBs,RBs,NVs,IArgs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,Exploded).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ancillary Stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Conversion

typed_schema_to_untyped_schema(TSchema,USchema) :-
  TSchema=..[F|ColnameTypes],
  coltype_to_col_list(ColnameTypes,Colnames),
  USchema=..[F|Colnames].
  
coltype_to_col_list([],[]).
coltype_to_col_list([Col:_Type|ColTypes],[Col|Cols]) :-
  coltype_to_col_list(ColTypes,Cols).
  
schema_to_colnametypes(Schema,ColTypes) :-
  schema_to_colnametypes(noren,Schema,ColTypes).

schema_ren_to_colnametypes(Schema,ColTypes) :-
  schema_to_colnametypes(ren,Schema,ColTypes).
  
schema_to_colnametypes(Ren,[_Table|Args],ColTypes) :-
  proj_expr_to_coltype_list(Ren,Args,ColTypes),
  !.
schema_to_colnametypes(_Ren,Schema,ColTypes) :-
  Schema=..[_|ColTypes].
  
proj_expr_to_coltype_list(_,[],[]).
proj_expr_to_coltype_list(Ren,[Arg|Args],[ColType|ColTypes]) :-
  proj_expr_to_coltype(Ren,Arg,ColType),
  proj_expr_to_coltype_list(Ren,Args,ColTypes).
  
% For DRC
proj_expr_to_coltype(_,Var,NewCol:_Type) :-
  var(Var),
  !,
  argument_autorenaming(NewCol).
% Return renamings (for queries)
proj_expr_to_coltype(ren,attr(_,_,ColRen),ColRen:_Type) :-
  !.
proj_expr_to_coltype(_,expr(_,ColRen,_),ColRen:_Type) :-
  !.
% Return view schema (for view creation)
proj_expr_to_coltype(_,attr(_,Col,ColRen),Col:_Type) :-
  atom_concat('$',_,ColRen).
proj_expr_to_coltype(_,attr(_,_Col,ColRen),ColRen:_Type).
%proj_expr_to_coltype(_,expr(_,ColRen,_),ColRen:_Type). % As above


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Metadata

% Get the view arity, if exists the view. Otherwise, it fails  
view_arity(ViewName,Arity) :-
  current_db(Connection),
  view_arity(Connection,ViewName,Arity).
  
view_arity('$des',ViewName,Arity) :-
  !,
  my_view('$des',ViewName,Arity,_,_,_,_,_,_).
view_arity(Connection,ViewName,Arity) :-
  my_odbc_exists_view(Connection,ViewName),
  my_odbc_get_table_arity(Connection,ViewName,Arity).
  
view_arity_list([],[]).
view_arity_list([V|Vs],[V/A|VAs]) :-
  view_arity(V,A),
  view_arity_list(Vs,VAs).
  
% Get the SQL definition for a view name
view_sql(ViewName,SQLst) :-
  current_db(Connection),
  my_view(Connection,ViewName,_Arity,SQLst,_,_,_,_,_).
  
view_sql_list([],[]).
view_sql_list([ViewName|ViewNames],[SQLst|SQLsts]) :-
  view_sql(ViewName,SQLst),
  view_sql_list(ViewNames,SQLsts).

view_sql(ViewName,Arity,SQLst) :-
  current_db(Connection),
  my_view(Connection,ViewName,Arity,SQLst,_,_,_,_,_).  
  
% Get the table arity, if exists the table. Otherwise, it fails  
table_arity(TableName,Arity) :-
  current_db('$des'),
  !,
  my_table('$des',TableName,Arity).
table_arity(TableName,Arity) :-
  my_odbc_get_table_arity(TableName,Arity).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_new_predicate_name(+Name,+InputId,-OutputId,-IdName) 
% Returns an identifier of the form: '$<Name><InputId>' and
% also the next Id number .
% e.g., get_new_predicate_name(p,1,2,'$p1')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_new_predicate_name(O,IId,OId,Id) :-
  OId is IId+1,
  atom_concat('$',O,TO),
  atom_codes(TO,STO),
  number_codes(IId,SIId),
  append(STO,SIId,SId),
  atom_codes(Id,SId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_new_predicate_name(+Name,-IdName) 
% Returns an identifier of the form: '$<Name><Id>' s.t.
% there is no other such predicate already defined
% e.g., get_new_predicate_name(p,'$p1')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_new_predicate_name(O,Id) :-
  get_pred_id(O,OId),
  build_predicate_name_id(O,OId,PId),
  (O==p, 
  % WARNING::
%    (datalog(P,_,_,_,_,_,_);datalog(':-'(P,_),_,_,_,_,_,_)),
%    functor(P,PId,_)
  % Look at the PDG instead of at the DDB which may involve calls to RDB
   pdg((Nodes,_)),
   memberchk(PId/_,Nodes)
    ->
    get_new_predicate_name(O,Id)
   ;
    Id=PId
  ).
  
build_predicate_name_id(O,OId,Id) :-
  atom_concat('$',O,TO),
  atom_codes(TO,STO),
  number_codes(OId,SOId),
  append(STO,SOId,SId),
  atom_codes(Id,SId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is_empty_relation(+RelationName)) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_empty_relation(Relation) :-
  get_relation_arity(Relation,Arity),
  functor(Query,Relation,Arity),
  get_answer(Query,[]).

no_tuples_in_ddb(TableName,Arity) :-
  length(Args,Arity),
  Query=..[TableName|Args],
  datalog(Query,_,_,_,_,_,_),
  !,
  fail.
no_tuples_in_ddb(_TableName,_Arity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insert_tuples(+TableName,+Arity,+NbrColumns,+ColNames)) 
% Insert computed tuples (kept in the extension table)
% from an SQL statement into a table
% NbrColumns is the number of columns for which values are 
% expected, as provided
% by the user in an INSERT INTO table(Col1,...,ColN)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_tuples(TableName,Arity,NbrColumns,Colnames) :-
  length(Args,NbrColumns),
  Witness=..[answer|Args],
  et(Witness,_Ids),
  !,
  bagof((Fact,[]),
        Ids^IVs^Vs^ETFact^BError^(
        (
        length(IVs,NbrColumns),
        ETFact=..[answer|IVs],
        (et(ETFact,Ids)
         ; 
         et(not(ETFact),Ids)
        ),
        build_complete_values(TableName,Arity,Colnames,IVs,Vs,BError),
        Fact=..[TableName|Vs]
        ) 
        ),
        Bag),
  assert_rules(Bag,[],sql(TableName),[simplify,no_safety],_CRNVs,_ODLIds,_Unsafe,Error),
  (var(Error)
   -> 
   display_nbr_of_tuples(Bag,inserted,Error)
   ;
   display_nbr_of_tuples([],inserted,Error)
  ).
insert_tuples(_TableName,_Arity,_NbrColumns,_Colnames) :-
  write_warning_log(['No tuple met the ''where'' condition for inserting.']).

% Replaces all occurrences of a head matching a given functor in a list of rules by a term
replace_head_list(_O,_N,[],[]).
replace_head_list(O,N,[':-'(H,B)|DLs],[':-'(N,B)|RDLs]) :-
  functor(H,O,_),
  !,
  replace_head_list(O,N,DLs,RDLs).
replace_head_list(O,N,[H|DLs],[N|RDLs]) :-
  H \= ':-'(_,_),
  functor(H,O,_),
  !,
  replace_head_list(O,N,DLs,RDLs).
replace_head_list(O,N,[DL|DLs],[DL|RDLs]) :-
  replace_head_list(O,N,DLs,RDLs).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% delete_tuples(+TableName,+Arity)) 
% Delete computed tuples (kept in the extension table)
% from an SQL statement from a table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete_tuples(TableName,Arity) :-
  N=answer,
  et(R,_Ids),
  functor(R,N,A),
  !,
  (A==Arity 
   ->
   bagof((Fact,[]),
         F^Ids^(
         (et(Fact,Ids), functor(Fact,N,A)); 
         (et(Fact,Ids), (Fact=not(F)), functor(F,N,A)) 
         ),
         Set),
   replace_functor(N,TableName,Set,RSet),
   retract_rule_list(RSet,Error),
   display_nbr_of_tuples(RSet,deleted,Error)
   ;
   write_error_log(['Incorrect number of values (must be ',Arity,').'])
  ).
delete_tuples(_TableName,_Arity) :-
  write_warning_log(['No tuple met the ''where'' condition for deleting.']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update_tuples(+TableName,+Arity)) 
% Update computed tuples kept in the extension table as 
%   answer(OldVal1,...,OldValArity,
%          Colname1,NewValI1,...,ColnameN,ValIM) 
%   : I1,...,IM in {1..Arity}
% from an SQL statement in a table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_tuples(TableName,Arity) :-
  bagof(
        ((OldFact,[]),(NewFact,[])),
        Answer^Ids^A^Args^OldValues^Assignments^NewValues^
         (
          (et(Answer,Ids) ; (et(A,Ids), A=not(Answer))),
          Answer=..[answer|Args],
          split_list(Arity,Args,OldValues,Assignments),
          build_new_values(TableName,Assignments,OldValues,NewValues),
          OldFact=..[TableName|OldValues],
          NewFact=..[TableName|NewValues]
         ),
        Bag
       ),
  !,
  bagof(DelTuple,NewTuple^member((DelTuple,NewTuple),Bag),DelTuples),
  bagof(InsTuple,OldTuple^member((OldTuple,InsTuple),Bag),InsTuples),
  retract_and_assert_rules_list(DelTuples,InsTuples),
  display_nbr_of_tuples(InsTuples,updated,_Error).
update_tuples(_TableName,_Arity) :-
  write_warning_log(['No tuple met the ''where'' condition for updating.']).

retract_and_assert_rules_list([],[]).
retract_and_assert_rules_list([DelTuple|DelTuples],[InsTuple|InsTuples]) :-
  push_flag(check_ic,off,CF),
  retract_rule(DelTuple,Error),
  pop_flag(check_ic,CF),
  functor(DelTuple,TableName,_),
  (var(Error) -> 
    assert_rule(InsTuple,[],sql(TableName),[simplify],_,_,_,Error),
    (var(Error) -> 
      retract_and_assert_rules_list(DelTuples,InsTuples)
     ;
      assert_rule(DelTuple,[],sql(TableName),[simplify],_,_,_,_),
      write_error_log(['Inserting modified tuple ',InsTuple]))
   ;
    write_error_log(['When deleting tuples during updating.'])).
  
build_new_values(TableName,Assignments,OldValues,NewValues) :-
  split_list_odd_even(Assignments,ColumnNames,Values),
  get_att_positions(TableName,ColumnNames,Positions),
  replace_positions(Positions,Values,OldValues,NewValues).
  
replace_positions(Positions,Values,OldValues,NewValues) :-
  my_zipWith(',',Positions,Values,PosVals),
  my_mergesort(PosVals,OPosVals),
  my_unzip(OPosVals,OPositions,OValues),
  replace_positions(OPositions,OValues,1,OldValues,NewValues).

replace_positions([],[],_Position,Xs,Xs) :-
  !.
replace_positions([Position|Positions],[Value|Values],Position,[_X|Xs],[Value|Ys]) :-
  !,
  NewPosition is Position+1,
  replace_positions(Positions,Values,NewPosition,Xs,Ys).
replace_positions(Positions,Values,Position,[X|Xs],[X|Ys]) :-
  NewPosition is Position+1,
  replace_positions(Positions,Values,NewPosition,Xs,Ys).
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ODBC-related stuff, common to several Prolog systems
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ODBC relation name
% Some DBMS's are sensitive to case, others not, and others
% translate to either uppercase or lowercase. Examples on Windows O.S.:
% * To uppercase
%   - DB2
% * To lowercase
%   - MySQL
%   - PostgreSQL
% * Case sensitive
%   - Sybase
% * Case insensitive. Keep case
%   - Access
%   - SQL Server
% MySQL
my_odbc_identifier_name(Name,ODBCName) :-
  current_db(_Connection,DBMS),
%   current_db(Connection),
%   opened_db(Connection,_Handle,DBMS),
  my_odbc_identifier_name(DBMS,Name,ODBCName).

my_odbc_identifier_name(Connection,Name,ODBCName) :-
  opened_db(Connection,_Handle,DBMS),
  my_odbc_dbms_relation_name(DBMS,Name,ODBCName).

my_odbc_dbms_relation_name(_DBMS,RelName,RelName) :-
  var(RelName),
  !.
my_odbc_dbms_relation_name(DBMS,RelName,ODBCRelName) :-
  memberchk(DBMS,[db2,oracle]),
  !,
  to_uppercase(RelName,ODBCRelName).
my_odbc_dbms_relation_name(DBMS,RelName,ODBCRelName) :-
  memberchk(DBMS,[mysql,postgresql]),
  !,
  to_lowercase(RelName,ODBCRelName).
my_odbc_dbms_relation_name(_DBMS,RelName,RelName).

% Lowercase DBMS name - Cased DBMS name
ldbms_dbms(postgresql,'PostgreSQL') :- !.
ldbms_dbms(db2,'DB2') :- !.
ldbms_dbms(oracle,'Oracle') :- !.
ldbms_dbms(mysql,'MySQL') :- !.
ldbms_dbms(sybase,'Sybase') :- !.
ldbms_dbms(sqlserver,'SQLServer') :- !.
ldbms_dbms(access,'Access') :- !.
ldbms_dbms(sqlanywhere,'SQLAnywhere') :- !.
ldbms_dbms(DBMS,DBMS).

% Types for different RDBMS's
internal_type_RDBMS_type(number(integer),_,'INTEGER').
internal_type_RDBMS_type(number(float),_,'FLOAT').
internal_type_RDBMS_type(string(S),_,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Display an SQL statement from its syntactic tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_sql_show_sql_on(SQL) :-
  (show_sql(on) -> display_sql(SQL,0,'$des'), nl_compact_log ; true).

display_sql(SQL,I) :-
  current_db(_Conn,DBMS),
  display_sql(SQL,I,DBMS).
  
display_sql(SQL,I,DBMS) :-
  push_flag(language,sql,L),
  (pretty_print(off) -> write_indent(I) ; true),
  write_sql(SQL,I,DBMS),
  write_log(';'),
  nl_log,
  pop_flag(language,L),
  !.

display_sql_list(SQL,I) :-
  current_db(_Conn,DBMS),
  display_sql_list(SQL,I,DBMS).

display_sql_list([],_I,_DBMS).
display_sql_list([SQL|SQLs],I,DBMS) :-
  display_sql(SQL,I,DBMS),
  nl_compact_log,
  display_sql_list(SQLs,I,DBMS).

write_schema_typed_cols_sequence([C:T],Ctrs,DBMS) :-
  internal_type_RDBMS_type(T,DBMS,RT),
  delimited_system_identifier(C,DBMS,DC),
  (column_ctr(C,Ctrs,Ctr)
   ->
    L=[DC,' ',RT,' ',Ctr]
   ;
    L=[DC,' ',RT]),
  write_log_list(L).
write_schema_typed_cols_sequence([C:T,CT1|CTs],Ctrs,DBMS) :-
  write_schema_typed_cols_sequence([C:T],Ctrs,DBMS),
  write_log(', '),
  write_schema_typed_cols_sequence([CT1|CTs],Ctrs,DBMS).
  
column_ctr(C,Ctrs,'NOT NULL') :-
  memberchk(not_nullables(Cs),Ctrs),
  memberchk(C,Cs).
  
delimited_system_identifier(C,DBMS,DC) :-
  is_system_identifier(C),
  !,
  delimited_dbms_sql_identifier(C,DBMS,SC),
  py_escape_double_quotes(SC,DBMS,DC).
delimited_system_identifier(C,_DBMS,C).

write_sql((SQL,_C),I,DBMS) :-
  !,
  write_sql(SQL,I,DBMS).
% % write_sql(create_view(sql,SQLst,Schema),I,DBMS) :-
% %   DBMS=hrsql,
% %   !,
% %   pp_indent(I),
% %   Schema=..[RelName|CTs],
% %   write_log_list([RelName,'(']),
% %   write_schema_typed_cols_sequence(CTs,[],DBMS),
% %   write_log_list([') :=']),
% %   pp_nl_or_blank,
% %   I1 is I+2,
% %   write_sql(SQLst,I1,DBMS).
write_sql(create_view(sql,SQLst,Schema),I,DBMS) :-
  !,
  pp_indent(I),
  write_log('CREATE VIEW '),
  typed_schema_to_untyped_schema(Schema,USchema),
%   write_log_list([USchema,' AS']),
  write_untyped_sql_schema(USchema,DBMS),
  write_log_list([' AS']),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql(SQLst,I1,DBMS).
write_sql(create_or_replace_view(sql,SQLst,Schema),I,DBMS) :-
  !,
  pp_indent(I),
  write_log('CREATE OR REPLACE VIEW '),
  typed_schema_to_untyped_schema(Schema,USchema),
%   write_log_list([USchema,' AS']),
  write_untyped_sql_schema(USchema,DBMS),
  write_log_list([' AS']),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql(SQLst,I1,DBMS).
write_sql(create_table(Schema,Ctrs),I,DBMS) :-
  !,
  pp_indent(I),
  write_log('CREATE TABLE '),
  Schema=..[TableName|CTs],
  write_log_list([TableName,'(']),
  write_schema_typed_cols_sequence(CTs,Ctrs,DBMS),
  (Ctrs == [] -> true ; write_log(', '), write_table_ctrs(Ctrs,DBMS)),
  write_log_list([')']).
write_sql(insert_into(RelName,ColNames,Tuples),I,DBMS) :-
  my_is_list(Tuples),
  !,
  pp_indent(I),
  write_log('INSERT INTO '),
  USchema=..[RelName|ColNames],
%  write_log_list([USchema,' VALUES']),
  write_untyped_sql_schema(USchema,DBMS),
  write_log_list([' VALUES']),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql_value_tuple_list(Tuples,I1).
% % write_sql(with(SQLst,SQLsts),I,DBMS) :-
% %   system_mode(hrsql),
% %   !,
% %   pp_indent(I),
% %   write_string_log("ASSUME"),
% %   pp_nl_or_blank,
% %   I1 is I+2,
% %   write_sql_assume_list(SQLsts,I1,DBMS),
% %   pp_nl_or_blank,
% %   write_sql(SQLst,I,DBMS).
write_sql(with(SQLst,SQLsts),I,DBMS) :-
  !,
  pp_indent(I),
  write_string_log("WITH"),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql_assume_list(SQLsts,I1,DBMS),
  pp_nl_or_blank,
  write_sql(SQLst,I,DBMS).
write_sql(select(DistinctAll,top(N),As,from(Rs),where(Cs),group_by(G),having(H),order_by(OAs,OSs)),I,DBMS) :-
  !,
  pp_indent(I),
  write_log('SELECT '),
  write_distinct_all_clause(DistinctAll,DBMS),
  write_proj_list(As,I,DBMS),
  write_sql_from(Rs,I,DBMS,I1),
  (Cs == true ->
   true
   ;
   pp_nl_or_blank,
   pp_indent(I),
   write_sql_cond('WHERE ',Cs,I1,DBMS)
   ),
  (G==[] ->
   true
   ;
    pp_nl_or_blank,
    pp_indent(I),
    write_log('GROUP BY '),
    write_attr_list(G)),
  (H==true ->
   true
   ;
    pp_nl_or_blank,
    pp_indent(I),
    write_sql_cond('HAVING ',H,I1,DBMS)),
  (OAs==[]
   ->
   true
   ;
    pp_nl_or_blank,
    pp_indent(I),
    write_log('ORDER BY '),
    write_expr_ord_list(OAs,OSs,DBMS)),
  (N==all
   ->
   true
   ;
    pp_nl_or_blank,
    pp_indent(I),
    write_log('FETCH FIRST '),
    write_log_list([N]),
    write_log(' ROWS ONLY')
  ).
write_sql(SetSQL,I,DBMS) :-
  SetSQL =.. [SetOp,ALL,LSQLst,RSQLst],
  set_operator_name(SetOp,ALL,SetOps,DBMS),
  !,
  I1 is I+2,
%   pp_indent_or_blank(I),
%   write_log('('),
%   pp_nl_or_blank,
  write_left_parenthesis(I,DBMS),
  write_sql(LSQLst,I1,DBMS),
  pp_nl_or_blank,
  write_right_parenthesis(I,DBMS),
  pp_nl_or_blank,
  pp_indent(I),
  write_string_log(SetOps),
  pp_nl_or_blank,
  write_left_parenthesis(I,DBMS),
  write_sql(RSQLst,I1,DBMS),
  pp_nl_or_blank,
  write_right_parenthesis(I,DBMS).
%   pp_nl_or_blank,
%   pp_indent(I),
%   write_log(')').
% Default case:
write_sql(SQL,I,_DBMS) :-
  pp_indent_or_blank(I),
  write_log(SQL).
  
write_left_parenthesis(_I,mysql) :-
  !.
write_left_parenthesis(I,_DBMS) :-
  pp_indent(I),
  write_log('('),
  pp_nl_or_blank.
  
write_right_parenthesis(_I,mysql) :-
  !.
write_right_parenthesis(I,_DBMS) :-
  pp_indent(I),
  write_log(')').
  
% write_distinct_all_clause(_DistinctAll,hrsql) :-
%   !.
write_distinct_all_clause(DistinctAll,_DBMS) :-
  (DistinctAll=distinct -> write_string_log("DISTINCT ") ; write_string_log("ALL ")).

% write_sql_from([(dual,_)],I,hrsql,I) :-
%   !.
write_sql_from(Rs,I,DBMS,I1) :-
  pp_nl_or_blank,
  pp_indent(I),
  write_string_log("FROM "),
  I1 is I+2,
  write_rel_list(Rs,I1,DBMS).

write_sql_assume_list([],_I,_DBMS).
write_sql_assume_list([(not(SQLst),AS)],I,DBMS) :-
  !,
  write_sql((SQLst,AS),I,DBMS),
  write_log(' NOT IN '),
%  write_log(' NOT IN'),
%  pp_nl,
%  I1 is I+2,
%  pp_indent_or_blank(I1),
  write_assumed_sql_schema(AS,DBMS).
% write_sql_assume_list([(SQLst,AS)],I,DBMS) :-
%   system_mode(hrsql),
%   !,
%   write_sql((SQLst,AS),I,DBMS),
%   write_log(' IN '),
%   write_assumed_sql_schema(AS,DBMS).
write_sql_assume_list([(SQLst,AS)],I,DBMS) :-
  pp_indent_or_blank(I),
  write_sql_assume_schema(AS,DBMS),
  write_log(' AS'),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql((SQLst,AS),I1,DBMS).
write_sql_assume_list([(SQLst,AS),SQLst2|SQLsts],I,DBMS) :-
  write_sql_assume_list([(SQLst,AS)],I,DBMS),
  write_log(','),
  pp_nl_or_blank,
  write_sql_assume_list([SQLst2|SQLsts],I,DBMS).
  
% write_assumed_sql_schema(AS,hrsql) :-
%   !,
%   functor(AS,N,_),
%   write_log(N).
write_assumed_sql_schema(AS,_DBMS) :-
  write_typed_sql_schema(AS).

write_sql_assume_schema(AS,DBMS) :-
  my_ground(AS),
  !,
  schema_to_delimited_schema(AS,DBMS,DAS),
  write_log(DAS).
write_sql_assume_schema(AS,DBMS) :-
%  write_typed_sql_schema(AS,DBMS).
  typed_schema_to_untyped_schema(AS,UAS),
  write_untyped_sql_schema(UAS,DBMS).
  
write_untyped_sql_schema(AS,DBMS) :-
  untyped_schema_to_delimited_schema(AS,DBMS,DAS),
  write_log(DAS).
  
untyped_schema_to_delimited_schema(AS,_DBMS,AS).

write_typed_sql_schema(AS,DBMS) :-
  typed_schema_to_delimited_typed_schema(AS,DBMS,DAS),
  write_log(DAS).

typed_schema_to_delimited_typed_schema(AS,_DBMS,AS).

pp_indent(I) :-
  (pretty_print(on) -> write_indent(I) ; true).

pp_indent_or_blank(I) :-
  (pretty_print(on) -> write_indent(I) ; write_log(' ')).

pp_nl_or_blank :-
  (pretty_print(on)
    -> 
     nl_log
    ;
     write_log(' ')
  ).

pp_blank :-
  (pretty_print(on)
    -> 
     write_log(' ')
    ;
     true
  ).

pp_nl :-
  (pretty_print(on) -> 
     nl_log
     ;
     true
  ).

% verbose(on) shows table names from autorenaming

write_proj_list(L,I) :-
  write_proj_list(L,I,'$des').

write_proj_list(*,_I,_DBMS) :-
  write_log(*).
write_proj_list([],_I,_DBMS).
write_proj_list([expr(E,AS,_Type)],I,DBMS) :-
  !,
  write_expr(E,I,DBMS),
  (nonvar(AS) -> 
%    (atom_concat('$',_,AS), verbose(off) -> 
    (is_system_identifier(AS)
     -> 
      true 
     ;
      (language(ra) -> write_log(' as ') ; write_log(' AS ')),
      write_log(AS)
    )
  ;
   true
  ).
write_proj_list([attr(T,A,R)],_I,_DBMS) :-
  !,
  write_attr(attr(T,A,R)).
write_proj_list([cte(Cte,_Type)],_I,_DBMS) :-
  write_log(Cte).
write_proj_list([(T,(*))],_I,_DBMS) :-
  !,
  write_log_list([T,'.',*]).
write_proj_list([A],_I,_DBMS) :-
  write_log(A).
write_proj_list([A1,A2|As],I,DBMS) :-
  write_proj_list([A1],I,DBMS),
  write_log(','),
  pp_blank,
  write_proj_list([A2|As],I,DBMS).

write_expr_ord_list(A,O) :-
  current_db(DBMS),
  write_expr_ord_list(A,O,DBMS).

write_expr_ord_list([],[],_DBMS).  
write_expr_ord_list([A],[O],DBMS) :-
  write_expr(A,0,DBMS),
  to_uppercase(O,UO),
  write_log(' '),
  write_ord(UO).
write_expr_ord_list([A1,A2|As],[O1,O2|Os],DBMS) :-
  write_expr_ord_list([A1],[O1],DBMS),
  write_log(', '),
  write_expr_ord_list([A2|As],[O2|Os],DBMS).
    
% RA:
write_ord('A') :-
  language(ra),
  !,
  write_log('asc').
write_ord('D') :-
  language(ra),
  !,
  write_log('desc').
% SQL:
write_ord('A') :-
  write_log('ASC').
write_ord('D') :-
  write_log('DESC').

write_attr_list([]).  
write_attr_list([A]) :-
  write_attr(A).  
write_attr_list([A1,A2|As]) :-
  write_attr(A1),
  write_log(', '),
  write_attr_list([A2|As]).
    
write_attr(attr(T,A,_R)) :-
  var(T),
  !,
  write_log(A).
write_attr(attr(T,A,_R)) :-
%  (atom_concat('$',_,T), verbose(off) -> 
  (is_system_identifier(T) -> 
   true 
   ;
   write_log(T),
   write_log('.')),
  write_log(A).

write_expr(Expr) :-
  write_expr(Expr,0,'$des').

write_expr(expr(E,_AS,_Type),I,DBMS) :-
  write_expr(E,I,DBMS).
write_expr(cte('$NULL'(_ID),_Type),_I,_DBMS) :-
  !,
%   ((development(on) ; var(ID)) -> 
%    write_log('$NULL'(ID)) 
%    ;
%    write_log(null)).
  write_log(null).
write_expr(cte(N,number(_N)),_I,_DBMS) :-
  !,
  write_log(N).
write_expr(cte(S,string(_S)),_I,_DBMS) :-
  !,
  write_log_list(['''',S,'''']).
write_expr(attr(Rel,A,AS),_I,_DBMS) :- 
  !,
  write_attr(attr(Rel,A,AS)).
write_expr(E,I,DBMS) :- 
  E =.. [Op,Arg],
  unary_operator(Op,_POp,_D),
  !,
  write_log_list([Op,'(']),
  write_expr(Arg,I,DBMS),
  write_log(')').
write_expr(E,I,DBMS) :- 
  is_DQL(E),
  !,
  write_log('('),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql(E,I1,DBMS),
  write_log(' )').
write_expr(E,I,DBMS) :- 
  E =.. [Op,Arg1,Arg2],
  my_infix_arithmetic(Op,_,POp,_,_,_,_),
  POp\==low,
  !,
  write_infix_arg_expr(Arg1,I,DBMS),
  write_log_list([' ',Op,' ']),
  write_infix_arg_expr(Arg2,I,DBMS).
write_expr(E,I,DBMS) :- 
  E =.. [Op,Arg1,Arg2],
  my_infix_arithmetic(Op,_,_,_,_,_,_),
  !,
  write_expr(Arg1,I,DBMS),
  write_log_list([' ',Op,' ']),
  write_expr(Arg2,I,DBMS).
write_expr(E,I,DBMS) :- 
  E =.. [F,Arg],
  my_sql_distinct_aggregate_function(F),
  !, 
  atom_concat(NF,'_distinct',F),
  write_log_list([NF,'(DISTINCT ']),
  write_expr(Arg,I,DBMS),
  write_log(')').
write_expr(E,I,DBMS) :- 
  E =.. [F,Arg1|Args],
  !,
  lang_capitalize_builtin(F,CF), 
  write_log_list([CF,'(']),
  write_expr_list([Arg1|Args],I,DBMS),
  write_log(')').
write_expr(count,_I,_DBMS) :- 
  write_log('count(*)').
write_expr(E,_I,_DBMS) :- 
  write_log(E).

lang_capitalize_builtin(F,CF) :-
  language(sql),
  !,
  to_uppercase(F,CF).
lang_capitalize_builtin(F,F).

write_expr_list([E],I,DBMS) :-
  write_expr(E,I,DBMS).
write_expr_list([E1,E2|Es],I,DBMS) :-
  !, 
  write_expr(E1,I,DBMS),
  write_log(','), 
  write_expr_list([E2|Es],I,DBMS).

write_infix_arg_expr(T,I,DBMS) :-
  nonvar(T),
  functor(T,F,2),
  my_arithmetic_operator(_P,YFX,_SF,F),
  (YFX=yfx ; YFX=xfy),
  !,
  write_log('('), 
  write_expr(T,I,DBMS),
  write_log(')').
write_infix_arg_expr(T,I,DBMS) :-
  write_expr(T,I,DBMS).

write_sql_value_tuple_list([T],I) :-
  pp_indent(I),
  write_sql_value_tuple(T).
write_sql_value_tuple_list([T1,T2|Ts],I) :-
  write_sql_value_tuple_list([T1],I),
  write_log(','),
  pp_nl_or_blank,
  write_sql_value_tuple_list([T2|Ts],I).

write_sql_value_tuple(T) :-
  write_log('('),
  write_sql_value_values(T),
  write_log(')').
  
write_sql_value_values([V]) :-
  write_expr(V).
write_sql_value_values([V1,V2|Vs]) :-
  write_sql_value_values([V1]),
  write_log(','),
  write_sql_value_values([V2|Vs]).

% write_sql_arg_list([attr(T,A,_R)],I) :-
%   !,
%   (nonvar(T) ->
%     (atom_concat('$',_,T), verbose(off) -> 
%      true 
%      ;
%      write_log(T),
%      write_log('.')
%     )
%   ;
%    true
%   ),
%   write_sql_arg(A,I).
% write_sql_arg_list([A],I) :-
%   write_sql_arg(A,I).
% write_sql_arg_list([A1,A2|As],I) :-
%   write_sql_arg_list([A1],I),
%   write_log(', '),
%   write_sql_arg_list([A2|As],I).

% write_sql_arg(cte(N,number(_T)),_I) :-
%   !,
%   write_log(N).
% write_sql_arg(cte(S,string(_S)),_I) :-
% %   atomic(A), 
% %   sql_cte(A),
%   !,
%   write_log_list(['\'',S,'\'']).
% write_sql_arg((SQLst,RR),I) :-
%   !,
%   pp_nl,
%   pp_indent(I),
%   write_log('('),
%   pp_nl,
%   I1 is I+2,
%   write_sql((SQLst,RR),I1),
%   write_log(')').
% write_sql_arg(A,_I) :-
%   write_log(A).

% sql_cte(C) :-
%   \+ (sql_object(C)).

% sql_object(C) :-
%   my_attribute('$des',_P,_T,C,_Type),
%   !.
% sql_object(C) :-
%   my_table('$des',C,_A).

write_rel_list([],_I,_DBMS).
write_rel_list([R],I,DBMS) :-
  write_pren_rel(R,I,DBMS).
write_rel_list([R1,R2|Rs],I,DBMS) :-
  write_pren_rel(R1,I,DBMS),
  write_log(', '),
  write_rel_list([R2|Rs],I,DBMS).
  
write_pren_rel((R,[RR|_RArgs]),I,DBMS) :-
  !,
  (%member(DBMS,[mysql]),
   \+ atom(R) 
   ->
    write_log('('),
    write_rel(R,I,DBMS),
    write_log(')')
   ;
    write_rel(R,I,DBMS)),
  (DBMS==postgresql,
   \+ atom(R)
   ->
    (var(RR) -> relation_autorenaming(RR) ; true),
    atom_concat('$',Ren,RR),
    atom_concat(r,Ren,DRR)
   ;
    DRR=RR
  ),
  (nonvar(DRR)
   ->
%    (atom_concat('$',_,RR), verbose(off) -> 
    (is_system_identifier(DRR)
     ->
      true
     ;
      write_log_list([' AS ',DRR])
    )
  ;
   true
  ).
write_pren_rel(R,_I,_DBMS) :-
  write_log(R).

write_par_rel(I,_I1,(TableOrView,AS),DBMS) :-
  my_table(TableOrView,_Arity),
  !,
  write_pren_rel((TableOrView,AS),I,DBMS).
write_par_rel(_I,I1,(R,AS),DBMS) :-
  R=..[J,_,_,_],
  join_name(J,_),
  !,
  write_rel((R,AS),I1,DBMS).
write_par_rel(_I,I1,R,DBMS) :-
  pp_nl,
  pp_indent(I1),
  write_log('('),
  write_rel(R,I1,DBMS),
  pp_nl,
  pp_indent(I1),
  write_log(')').
  
write_rel((SQL,_C),I,DBMS) :-
  !,
  write_rel(SQL,I,DBMS).
write_rel(TableOrView,I,_DBMS) :-
%  my_table(TableOrView,_Arity),
  atom(TableOrView), % Local views are not globally known as views
  !,
  pp_nl,
  pp_indent(I),
  write_log(TableOrView).
write_rel(JoinRel,I,DBMS) :-
  JoinRel=..[J,LR,RR,equijoin(natural)],
  join_name(J,JN),
  !,
  I1 is I+2,
  write_log('('),
  write_par_rel(I,I1,LR,DBMS),
  pp_nl_or_blank,
  pp_indent(I),
  write_log('NATURAL '),
  write_string_log(JN),
  write_log(' '),
  write_par_rel(I,I1,RR,DBMS),
  pp_nl,
  pp_indent(I),
  write_log(')').
write_rel(division(LR,RR),I,DBMS) :-
  !,
  I1 is I+2,
  write_log('('),
  write_par_rel(I,I1,LR,DBMS),
  pp_nl_or_blank,
  pp_indent(I),
  write_log('DIVISION '),
  write_par_rel(I,I1,RR,DBMS),
  pp_nl,
  pp_indent(I),
  write_log(')').
write_rel(JoinRel,I,DBMS) :-
  JoinRel=..[J,LR,RR,equijoin(Attrs)],
  join_name(J,JN),
  !,
  I1 is I+2,
  write_log('('),
  write_par_rel(I,I1,LR,DBMS),
  pp_nl_or_blank,
  pp_indent(I),
  write_string_log(JN),
  write_log(' '),
  write_par_rel(I,I1,RR,DBMS),
  pp_nl_or_blank,
  pp_indent(I),
  write_log('USING ('),
  write_attr_list(Attrs),
  write_log(')'),
  pp_nl,
  pp_indent(I),
  write_log(')').
write_rel(JoinRel,I,DBMS) :-
  JoinRel=..[J,LR,RR,C],
  join_name(J,JN),
  !,
  I1 is I+2,
  pp_nl,
  pp_indent(I),
  write_log('('),
  write_par_rel(I,I1,LR,DBMS),
  pp_nl_or_blank,
  pp_indent(I),
  write_string_log(JN),
  write_log(' '),
  write_par_rel(I,I1,RR,DBMS),
  (C==true ->
    true
   ;
    pp_nl_or_blank,
    pp_indent(I),
    write_sql_cond('ON ',C,I,DBMS)),
  pp_nl,
  pp_indent(I),
  write_log(')').
write_rel(SQL,I,DBMS) :-
%  I1 is I+2,
  pp_nl,
%   pp_indent(I),
%   write_log('('),
  write_sql(SQL,I,DBMS),
%   write_log(')'),
  !.
% Default case:
write_rel(Rel,I,_DBMS) :-
  pp_indent(I),
  write_log(Rel).

join_name(inner_join,"INNER JOIN").
join_name(left_join,"LEFT JOIN").
join_name(right_join,"RIGHT JOIN").
join_name(full_join,"FULL JOIN").

set_operator_name(union,distinct,"UNION",_DBMS).
set_operator_name(union,all,"UNION ALL",_DBMS).
set_operator_name(except,_,"MINUS",oracle) :-
  !.
set_operator_name(except,_,"EXCEPT",_DBMS).
set_operator_name(intersect,_,"INTERSECT",_DBMS).

write_sql_cond(Clause,Condition,Indent,DBMS) :-
  write_log(Clause),
  write_sql_cond(Condition,Indent,DBMS).
  
write_sql_cond(Condition,Indent) :-
  write_sql_cond(Condition,Indent,'$des').
  
write_sql_cond(and(C1,C2),I,DBMS) :-
  !,
  write_log('('),
  write_sql_cond(C1,I,DBMS),
  write_log(' AND '),
  write_sql_cond(C2,I,DBMS),
  write_log(')').
write_sql_cond(or(C1,C2),I,DBMS) :-
  !,
  write_log('('),
  write_sql_cond(C1,I,DBMS),
  write_log(' OR '),
  write_sql_cond(C2,I,DBMS),
  write_log(')').
write_sql_cond(exists((SQL,_C)),I,DBMS) :-
  !,
  write_log('EXISTS ('),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql(SQL,I1,DBMS),
  write_log(' )').
write_sql_cond(in(Args,SQL),I,DBMS) :-
  !,
  write_log('('),
%  write_sql_arg_list(Args,I),
  write_proj_list(Args,I,DBMS),
  write_log(') IN ('),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql(SQL,I1,DBMS),
  write_log(' )').
write_sql_cond(not_in(Args,SQL),I,DBMS) :-
  !,
  write_log('('),
%  write_sql_arg_list(Args,I),
  write_proj_list(Args,I,DBMS),
  write_log(') NOT IN ('),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql(SQL,I1,DBMS),
  write_log(' )').
write_sql_cond(not(C),I,DBMS) :-
  !,
  write_log('NOT'),
  write_log('('),
  write_sql_cond(C,I,DBMS),
  write_log(')').
write_sql_cond(C,I,DBMS) :-
  C =.. [Op,A1,A2],
  !,
  write_expr(A1,I,DBMS),
  write_log(' '),
  write_op(Op),
  write_log(' '),
  write_expr(A2,I,DBMS).
write_sql_cond(is_null(SQL),I,DBMS) :-
  is_DQL(SQL),
  !,
  write_log('('),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql(SQL,I1,DBMS),
  write_log(' ) IS NULL').
write_sql_cond(is_null(Expr),I,DBMS) :-
  !,
  write_expr(Expr,I,DBMS),
  write_log(' IS NULL').
write_sql_cond(C,_I,_DBMS) :-
  write_log(C).

is_DQL((_SQL,_AS)).
  
write_op(Op) :-
  atom_concat(ROP,'_all',Op),
  !,
  write_log_list([ROP,' ALL']).  
write_op(Op) :-
  atom_concat(ROP,'_any',Op),
  !,
  write_log_list([ROP,' ANY']).
write_op(Op) :-
  write_log(Op).
  
%write_ra_cond(exists((_RA,AS,_SQL)),I) :-
%  !,
%  write_indent(I),
%  write_log('EXISTS ('),
%  write_log(AS),
%  write_log(')').
%write_ra_cond(C,I) :-
%  write_sql_cond(C,I).

write_table_ctrs(Ctrs,DBMS) :-
  filter_column_ctrs(Ctrs,TCtrs),
  write_only_table_ctrs(TCtrs,DBMS).
 
filter_column_ctrs(Ctrs,TCtrs) :-
  remove_from_list(not_nullables(_),Ctrs,TCtrs).
  
write_only_table_ctrs([Ctr],DBMS) :-
  write_table_ctr(Ctr,DBMS).
write_only_table_ctrs([Ctr1,Ctr2|Ctrs],DBMS) :-
  write_table_ctr(Ctr1,DBMS),
  write_log(','),
  write_only_table_ctrs([Ctr2|Ctrs],DBMS).
  
%write_table_ctr(not_nullables(_L),_DBMS). % Only as a column ctr and displayed in the schema
write_table_ctr(primary_key(L),_DBMS) :-
  write_log(' PRIMARY KEY'),
  write_sql_value_tuple(L).
write_table_ctr(foreign_key(Cs,T,RCs),_DBMS) :-
  write_log(' FOREIGN KEY'),
  write_sql_value_tuple(Cs),
  write_log(' REFERENCES '),
  write_log(T),
  write_sql_value_tuple(RCs).

write_indent(0) :-
  !.
write_indent(I) :- 
  write_log(' '),
  I1 is I-1,
  write_indent(I1).


%%%%%%%%%%%%%%%  END des_sql.pl  %%%%%%%%%%%%%%%
