/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.4.1                 */
/*                                                       */
/*    SYSTEM DEPENDENT PREDICATES 2                      */
/*    Tested for SWI-Prolog 7.2.3                        */
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


% Prolog system identification
prolog_system(swi,Version) :-
  current_prolog_flag(version_data,swi(Ma,Mi,P,_)),
  atomic_list_concat(['SWI-Prolog ',Ma,'.',Mi,'.',P],Version).

% 
:- on_signal('SIGINT',_,sigint_signal).
sigint_signal(_) :-
%  throw('$aborted').  
  my_raise_exception(sigint,user_break,[]).
  
% FD constraint library
:- use_module(library(clpfd)).

% File copy
%:- use_module(library(filesex),[copy_file/2]). % Cannot load under MS Windows

% OS interaction via exec/3
:- use_module(library(dialect/sicstus/system),[exec/3]).

% OS identification
% Sets a flag indicating which OS we are running DES on (windows, unix)
:- dynamic(os/1).
:- retractall(os(_OS)),
   ((getenv('COMSPEC',C), exists_file(C)) -> 
      assertz(os(windows)),
      (win_has_menu ->
        des_version(DESVersion),
        prolog_system(_Swi,PrologVersion),
        atomic_list_concat(['DES',DESVersion,' running on ',PrologVersion],WindowTitle),
        window_title(_OWT,WindowTitle)
       ;
        true
      )
     ;
      assertz(os(unixes))).

      

%%%%%%%% Input history %%%%%%%%%%%

:- prolog_history(enable).  

%%%%%%%% Indexing %%%%%%%%%%%

my_term_hash(F,Hash) :-
  term_hash(F,Hash).	   
	   
%%%%%%% Prolog Debugging %%%%%%%%

my_spy(Goal) :-
  spy(Goal).

my_nospyall :-
  nospyall.

my_nospy(Goal) :-
  nospy(Goal).

%%%%%%% END Prolog Debugging %%%%%%%%


%%%%%%% ODBC Connections %%%%%%%%

% Open an ODBC connection
% Avoid informational ODBC messages: silent(true)
%:- set_prolog_flag(verbose,silent).
my_open_odbc(Connection,UOptions) :-
  translate_open_odbc_options(UOptions,Options),
%  MyOptions = [open(once),cursor_type(dynamic),silent(true)|Options],
  MyOptions = [open(once),silent(true)|Options],
  (odbc_connect(Connection,Handle,MyOptions)
   ->
    assertz(opened_db(Connection,(Handle,MyOptions),_,_)),
    my_odbc_get_dbms(Connection,DBMS),
    retract(opened_db(Connection,(Handle,MyOptions),_,_)),
    (DBMS == sqlserver,
     \+ member(mars(true),Options)
     ->
      SQLServerOption = mars(true),
      odbc_disconnect(Handle),
      odbc_connect(Connection,MarsHandle,[SQLServerOption|MyOptions]),
%      retract(opened_db(Connection,_,_)),
      assertz(opened_db(Connection,(MarsHandle,[SQLServerOption|MyOptions]),DBMS,UOptions))
%       SQLServerOption = cursor_type(dynamic),
%       retract(opened_db(Connection,_)),
%       assertz(opened_db(Connection,(Handle,[SQLServerOption|MyOptions]))),
%       odbc_set_connection(Handle,SQLServerOption)
     ;
      assertz(opened_db(Connection,(Handle,MyOptions),DBMS,UOptions))
    ),
    add_dual_to_schema(Connection),
    write_info_verb_log(['ODBC connection ''',Connection,''' successfully opened.'])
   ;
    write_error_log(['Opening connection ''',Connection,'''.'])
  ).

translate_open_odbc_options([],[]).
translate_open_odbc_options(['MultipleActiveResultSets=true'|UOptions],[mars(true)|Options]) :-  
  !,
  translate_open_odbc_options(UOptions,Options).
translate_open_odbc_options([Option|UOptions],[Option|Options]) :-  
  translate_open_odbc_options(UOptions,Options).

% Close an ODBC connection
my_close_odbc(Connection,CurrConnection) :-
  (retract(opened_db(Connection,(Handle,_Options),_DBMS,_))
   ->
    (Connection==CurrConnection
     ->
      set_default_db
     ;
      true
    ),
    odbc_disconnect(Handle),
    disable_rdb_datasource(Connection),
    remove_dual_from_schema(Connection),
    write_info_verb_log(['ODBC connection ''',Connection,''' successfully closed.'])
   ;
   write_error_log(['Connection ''',Connection,''' not found.'])
  ).
  
  
%%%%%%% ODBC Statement Executions %%%%%%%%

% Execute a SELECT statement returning the answer schema and all the answer tuples
my_odbc_dql_query(SQLstr,Schema,Rows) :-  
  current_db(Connection),
  my_odbc_dql_query(Connection,SQLstr,Schema,Rows).
  
my_odbc_dql_query(Connection,SQLstr,Schema,Rows) :-  
  opened_db(Connection,(Handle,_Options)),
  name(SQL,SQLstr),
  findall(Answer,(odbc_query(Handle,SQL,Row,[null('$NULL'(_))]),Row=..[row|Args],Answer=..[answer|Args]),Rows),
  concrete_nulls(Rows),
  my_odbc_get_query_typed_arguments(Connection,SQLstr,ColNameTypes),
  Schema=..[answer|ColNameTypes].
% Second (worser) method to get column schema:
%   (odbc_query(Handle,SQL,Row,[null('$NULL'(_)),source(true)]) ->
%     Row=..[_Row|Columns],
%     get_columns_schema(Columns,ColumnsSchema),
%     Schema=..[answer|ColumnsSchema]
%    ;
%     Schema=answer).

% Execute a SELECT statement returning all the answer tuples
my_odbc_dql_query_wo_schema(SQLstr,Rows) :-  
  current_db(Connection),
  my_odbc_dql_query_wo_schema(Connection,SQLstr,Rows).
  
my_odbc_dql_query_wo_schema(Connection,SQLstr,Rows) :-  
  opened_db(Connection,(Handle,_Options)),
  name(SQL,SQLstr),
  findall(Answer,(odbc_query(Handle,SQL,Row,[null('$NULL'(_))]),Row=..[row|Args],Answer=..[answer|Args]),Rows),
  concrete_nulls(Rows).


% Execute a SELECT statement returning one answer tuple at a time via backtracking
my_odbc_dql_query_fetch_row(SQLstr,Row) :-
  current_db(Connection),
  my_odbc_dql_query_fetch_row(Connection,SQLstr,Row).
    
my_odbc_dql_query_fetch_row(Connection,SQLstr,Row) :-
  opened_db(Connection,(Handle,_Options)),
  name(SQL,SQLstr),
  odbc_query(Handle,SQL,Row,[null('$NULL'(_))]).
  
% Execute a DML statement returning the number of tuples affected
my_odbc_dml_query(SQLstr,NumberOfRows) :-  
  current_db(Connection),
  my_odbc_dml_query(Connection,SQLstr,NumberOfRows).
  
my_odbc_dml_query(Connection,SQLstr,NumberOfRows) :-  
  opened_db(Connection,(Handle,_Options)),
  name(SQL,SQLstr),
  % The following call fails for the DB2 connnector if no tuples are affected
  (odbc_query(Handle,SQL,affected(NumberOfRows))
   ->
    true
   ;
    NumberOfRows=0).
    
% Execute a DDL statement returning nothing
my_odbc_ddl_query(SQLstr) :-  
  current_db(Connection),
  my_odbc_ddl_query(Connection,SQLstr).
  
my_odbc_ddl_query(Connection,SQLstr) :-  
  set_flag(recompute_pdg(no)),
  opened_db(Connection,(Handle,_Options)),
  name(SQL,SQLstr),
  (odbc_query(Handle,SQL)
   ->
    true
   ;
    true
  ),
  set_flag(recompute_pdg(yes)).
    
% Closing a query
% my_odbc_dangling_query_close(Message) :-
%   nonvar(Message),
%   Message = error(odbc('S1000',0,M),_),
%   atom_concat(_,'Connection is busy',M1),
%   atom_concat(M1,_,M),
%   !,
%   current_db(Connection,(Handle,Options)),
%   odbc_disconnect(Handle),
%   odbc_connect(Connection,NHandle,[open(once)|Options]),
%   set_flag(current_db,Connection,(NHandle,Options)).
% my_odbc_dangling_query_close(_Message).

my_odbc_dangling_query_close :-
  current_db(Connection),
  retract(opened_db(Connection,(Handle,Options),DBMS,Options)),
  !,
  odbc_disconnect(Handle),
  odbc_connect(Connection,NHandle,Options),
  assertz(opened_db(Connection,(NHandle,Options),DBMS,Options)).
my_odbc_dangling_query_close.


%%%%%%% ODBC Error Display %%%%%%%%

my_display_odbc_error(error(odbc(State,Native,M),_)) :-
  'remove_\r\n'(M,RM),
  write_error_log(['ODBC State ',State,' Code ',Native,': ',RM]).

  
%%%%%%% ODBC Metadata %%%%%%%%

% ODBC relation name

% Get table names
my_odbc_get_tablenames(TableNames) :-
  current_db(Connection),
  my_odbc_get_tablenames(Connection,TableNames).
  
% SQL Server
my_odbc_get_tablenames(Connection,TableNames) :-
%  my_odbc_get_dbms(Connection,sqlserver),
  opened_db(Connection,(ConnectionHandle,_EnvHandle),sqlserver),
  !,
  findall(TableName,
    odbc_current_table(ConnectionHandle,TableName,owner(dbo)),
    TableNames).
% Oracle
my_odbc_get_tablenames(Connection,ViewNames) :-
%  my_odbc_get_dbms(Connection,oracle),
  opened_db(Connection,_Handle,oracle),
  !,
  my_odbc_oracle_get_tablenames(Connection,ViewNames).
% SQL Anywhere
my_odbc_get_tablenames(Connection,TableNames) :-
  opened_db(Connection,_Handle,sqlanywhere),
  !,
  my_odbc_sqlanywhere_get_tablenames(Connection,TableNames).
% Others
my_odbc_get_tablenames(Connection,TableNames) :-
  opened_db(Connection,(Handle,_Options)),
  findall(TableName,odbc_current_table(Handle,TableName),TableNames).

% Oracle
my_odbc_oracle_get_tablenames(Connection,TableNameList) :-
  SQLstr="select table_name from user_tables order by table_name;", 
  my_odbc_dql_query_wo_schema(Connection,SQLstr,Rows),
  rows_to_names(Rows,TableNameList).

% SQL Anywhere  
my_odbc_sqlanywhere_get_tablenames(Connection,TableNameList) :-
  SQLstr="select table_name from systable where primary_root=0 and creator=1 and table_type='BASE' order by table_name;", 
  my_odbc_dql_query_wo_schema(Connection,SQLstr,Rows),
  rows_to_names(Rows,TableNameList).
  
% Ask whether a given table does exist
my_odbc_exists_table(TableName) :-
  current_db(Connection),
  my_odbc_exists_table(Connection,TableName).
  
my_odbc_exists_table(Connection,TableName) :-
%  my_odbc_get_dbms(Connection,DBMS),
  opened_db(Connection,(Handle,_Options),DBMS),
  DBMS\==oracle,
  !,
%  opened_db(Connection,(Handle,_Options)),
  my_odbc_identifier_name(Connection,TableName,ODBCTableName),
  odbc_current_table(Handle,ODBCTableName).
% Getting Oracle metadata is rather slow on some systems. Better use a select on data dictionary
my_odbc_exists_table(Connection,TableName) :-
  my_odbc_identifier_name(Connection,TableName,ODBCTableName),
  name(ODBCTableName,StrTableName),
  concat_lists(["select 1 from user_tables where table_name='",StrTableName,"';"],SQLstr),
  my_odbc_dql_query_wo_schema(Connection,SQLstr,Rows),
  !,
  Rows\==[].

% Get view names
my_odbc_get_viewnames(ViewNames) :-
  current_db(Connection),
  my_odbc_get_viewnames(Connection,ViewNames).
  
% Oracle
my_odbc_get_viewnames(Connection,ViewNames) :-
%  my_odbc_get_dbms(Connection,oracle),
  opened_db(Connection,_Handle,oracle),
  !,
  my_odbc_oracle_get_viewnames(Connection,ViewNames).
% SQL Anywhere
my_odbc_get_viewnames(Connection,ViewNames) :-
  opened_db(Connection,_Handle,sqlanywhere),
  !,
  my_odbc_sqlanywhere_get_viewnames(Connection,ViewNames).
% Others
my_odbc_get_viewnames(Connection,ViewNames) :-
  opened_db(Connection,(Handle,_Options),DBMS),
%  my_odbc_get_dbms(Connection,DBMS),
  (DBMS==sqlserver -> Owner=dbo ; true), % Filter user tables and views for SQL Server
  findall(ViewName,odbc:odbc_tables(Handle,row(_Qualifier,Owner,ViewName,'VIEW',_Y)),ViewNames).

% Oracle
my_odbc_oracle_get_viewnames(Connection,ViewNameList) :-
  SQLstr="select view_name from user_views order by view_name;", 
  my_odbc_dql_query_wo_schema(Connection,SQLstr,Rows),
  rows_to_names(Rows,ViewNameList).

% SQL Anywhere  
my_odbc_sqlanywhere_get_viewnames(Connection,ViewNameList) :-
  SQLstr="select table_name from systable where primary_root=0 and creator=1 and table_type='VIEW' order by table_name;", 
  my_odbc_dql_query_wo_schema(Connection,SQLstr,Rows),
  rows_to_names(Rows,ViewNameList).
    
rows_to_names([],[]).
rows_to_names([Row|Rows],[N|Ns]) :-
  Row =.. [_,N],
  rows_to_names(Rows,Ns).

% Ask whether a given view does exist
my_odbc_exists_view(ViewName) :-
  current_db(Connection),
  my_odbc_exists_view(Connection,ViewName).
  
my_odbc_exists_view(Connection,ViewName) :-
%  my_odbc_get_dbms(Connection,DBMS),
  opened_db(Connection,(Handle,_Options),DBMS),
  DBMS\==oracle,
  !,
%  opened_db(Connection,(Handle,_Options)),
  my_odbc_identifier_name(Connection,ViewName,ODBCViewName),
  (DBMS==sqlserver -> Owner=dbo ; true), % Filter user tables and views for SQL Server
  odbc:odbc_tables(Handle,row(_Qualifier,Owner,ODBCViewName,'VIEW',_Y)).
% Getting Oracle metadata is rather slow on some systems. Better use a select on data dictionary
my_odbc_exists_view(Connection,ViewName) :-
  my_odbc_identifier_name(Connection,ViewName,ODBCViewName),
  name(ODBCViewName,StrViewName),
  concat_lists(["select 1 from user_views where view_name='",StrViewName,"';"],SQLstr),
  my_odbc_dql_query_wo_schema(Connection,SQLstr,Rows),
  !,
  Rows\==[].
  
% Get both table and view names
my_odbc_get_table_and_view_names(TableNames) :-
  current_db(Connection),
  my_odbc_get_table_and_view_names(Connection,TableNames).
  
my_odbc_get_table_and_view_names(Connection,TableNames) :-
  opened_db(Connection,(Handle,_Options),DBMS),
%  my_odbc_get_dbms(Connection,DBMS),
  (DBMS==sqlserver -> Owner=dbo ; true), % Filter user tables and views for SQL Server
  findall(TableName,
          (odbc:odbc_tables(Handle,row(_Qualifier,Owner,TableName,Type,_Comment)),(Type='TABLE';Type='VIEW')),
          TableNames).
  
% Get table arity
% Current DB
my_odbc_get_table_arity(TableName,Arity) :-
  current_db(Connection),
  my_odbc_get_table_arity(Connection,TableName,Arity).
% For a given DB
my_odbc_get_table_arity(Connection,TableName,Arity) :-
  opened_db(Connection,(Handle,_Options),_DBMS),
  my_odbc_identifier_name(Connection,TableName,ODBCTableName),
  odbc_current_table(Handle,ODBCTableName,arity(Arity)).
  
% Get table/arity list
my_odbc_get_table_arity_list(TableNameArityList) :-
  current_db(Connection),
  my_odbc_get_table_arity_list(Connection,TableNameArityList).
  
my_odbc_get_table_arity_list(Connection,TableNameArityList) :-
%  my_odbc_get_dbms(Connection,DBMS),
  opened_db(Connection,_,DBMS),
  (DBMS==oracle,
   !,
   my_odbc_oracle_get_table_arity_list(Connection,TableNameArityList)
  ;
   DBMS==sqlanywhere,
   !,
   my_odbc_sqlanywhere_get_table_arity_list(Connection,TableNameArityList)
  ;
   DBMS==sqlserver,
   !,
   my_odbc_sqlserver_get_table_arity_list(Connection,TableNameArityList)
  ;
   my_odbc_generic_get_table_arity_list(Connection,TableNameArityList)
  ).
  
% Oracle:
my_odbc_oracle_get_table_arity_list(Connection,TableNameArityList) :-
  SQLstr="select table_name,COUNT(COLUMN_NAME) from user_tab_columns group by table_name order by table_name;", 
  my_odbc_dql_query_wo_schema(Connection,SQLstr,Rows),
  rows_to_tablename_arity(Rows,TableNameArityList).
  
% SQL Anywhere  
my_odbc_sqlanywhere_get_table_arity_list(Connection,TableNameArityList) :-
  SQLstr="select table_name,count(column_name) arity from systable,syscolumn where systable.table_id=syscolumn.table_id and primary_root=0 and creator=1 and table_type='BASE' group by table_name;", 
  my_odbc_dql_query_wo_schema(Connection,SQLstr,Rows),
  rows_to_tablename_arity(Rows,TableNameArityList).
  
% SQL Server
my_odbc_sqlserver_get_table_arity_list(Connection,TableNameArityList) :-
  opened_db(Connection,(Handle,_Options),_DBMS),
  findall(TableName/Arity,
    (
     Tuple=row(_Qualifier,dbo,TableName,_Type,_Comment),
     odbc:odbc_tables(Handle,Tuple),
     odbc:table_facet(arity(Arity), Handle, Tuple)
    ),
    TableNameArityList).

% Generic
:- dynamic(nbr_read_relations/1).
my_odbc_generic_get_table_arity_list(Connection,TableNameArityList) :-
  opened_db(Connection,(Handle,_Options),_DBMS),
  set_flag(nbr_read_relations,0),
  (my_log([_|_]) -> Log=log ; Log=no_log),
  findall(TableName/Arity,get_and_display_nbr_relation(Handle,Log,TableName,Arity),TableNameArityList).
  
rows_to_tablename_arity([],[]).
rows_to_tablename_arity([Row|Rows],[N/A|NAs]) :-
  Row =.. [_,N,FA],
  A is integer(FA),
  rows_to_tablename_arity(Rows,NAs).


get_and_display_nbr_relation(ConnectionHandle,Log,TableName,Arity) :-
  odbc_current_table(ConnectionHandle,TableName,arity(Arity)),
  inc_flag(nbr_read_relations,NbrReadRelations),
  display_nbr_running_read_relations(Log,NbrReadRelations).


  
% my_odbc_get_table_arity_list(TableNameArityList) :-
%   current_db(Connection),
%   my_odbc_get_table_arity_list(Connection,TableNameArityList).
%   
% :- dynamic(nbr_read_relations/1).
% my_odbc_get_table_arity_list(Connection,TableNameArityList) :-
%   opened_db(Connection,(Handle,_Options)),
%   set_flag(nbr_read_relations,0),
%   (my_log(_,_) -> Log=log ; Log=no_log),
%   findall(TableName/Arity,get_and_display_nbr_relation(Handle,Log,TableName,Arity),TableNameArityList).

% get_and_display_nbr_relation(Handle,Log,TableName,Arity) :-
%   odbc_current_table(Handle,TableName,arity(Arity)),
%   inc_flag(nbr_read_relations,NbrReadRelations),
%   display_nbr_running_read_relations(Log,NbrReadRelations).

% Get table column names. It is expected to get them in the same order they were defined via the create SQL statement
my_odbc_get_colnames(TableName,ColNames) :-
  current_db(Connection),
  my_odbc_get_colnames(Connection,TableName,ColNames).
  
my_odbc_get_colnames(Connection,TableName,ColNames) :-
  opened_db(Connection,(Handle,_Options),_DBMS),
  my_odbc_identifier_name(Connection,TableName,ODBCTableName),
  findall(ColName,odbc_table_column(Handle,ODBCTableName,ColName),ColNames),
  ColNames=[_|_].

% The following allows SWI to detect predicate current_stream as needed for building the executable  
my_void_odbc_table_column(Handle,TableName,ColName) :-
  odbc_table_column(Handle,TableName,ColName).  
  
% Get the list of ColumnName:Type for a table/view  
my_odbc_get_table_typed_arguments(TableName,ColNameTypes) :-
  current_db(Connection),
  my_odbc_get_table_typed_arguments(Connection,TableName,ColNameTypes).
  
my_odbc_get_table_typed_arguments(Connection,TableName,ColNameTypes) :-
  findall(ColName:Type,my_odbc_get_type(Connection,TableName,ColName,Type),DColNameTypes),
  remove_duplicates(DColNameTypes,ColNameTypes).
  
% Get the type in the form: typename(length)
my_odbc_get_type(TableName,ColName,Type) :-
  current_db(Connection),
  my_odbc_get_type(Connection,TableName,ColName,Type).
  
my_odbc_get_type(Connection,TableName,ColName,Type) :-
  opened_db(Connection,(Handle,_Options),_DBMS),
  my_odbc_identifier_name(Connection,TableName,ODBCTableName),
  odbc_table_column(Handle,ODBCTableName,ColName,type_name(TypeName)),
  odbc_table_column(Handle,ODBCTableName,ColName,length(TypeLength)),
  Type=..[TypeName,TypeLength].
%% SAMER
%   odbc_table_column(Handle,ODBCTableName,ColName,precision(Precision)),
%   Type=..[TypeName,TypeLength,Precision].
%%

% The following allows SWI to detect predicate current_stream as needed for building the executable  
my_void_odbc_table_column(Handle,TableName,ColName,type_name(TypeName)) :-
  odbc_table_column(Handle,TableName,ColName,type_name(TypeName)).  

% Get the list of type names for a table/view  
my_odbc_get_table_typenames(TableName,TypeNames) :-
  current_db(Connection),
  my_odbc_get_table_typenames(Connection,TableName,TypeNames).
  
my_odbc_get_table_typenames(Connection,TableName,TypeNames) :-
  opened_db(Connection,(Handle,_Options),_DBMS),
  my_odbc_identifier_name(Connection,TableName,ODBCTableName),
  findall(TypeName,odbc_table_column(Handle,ODBCTableName,_ColName,type_name(TypeName)),TypeNames).

% Get the query schema (a list of ColName:TypeName). 
% First method: Create a view, get its schema, and drop the view.
my_odbc_get_query_typed_arguments(SQLstr,ColNameTypes) :-  
  current_db(Connection),
  my_odbc_get_query_typed_arguments(Connection,SQLstr,ColNameTypes).
  
my_odbc_get_query_typed_arguments(Connection,SQLstr,ColNameTypes) :-  
  TableName='des__temporary',
  drop_rdb_view_if_exists(Connection,TableName),
  name(TableName,TableNameStr),
  %WARNING: delimited_sql_identifier(Connection,TableName,TableNameStr),
  concat_lists(["CREATE VIEW ",TableNameStr," AS ",SQLstr],CreateStr),
  catch(
   (
	  my_odbc_ddl_query(Connection,CreateStr),
	  my_odbc_get_table_typed_arguments(Connection,TableName,ColNameTypes),
	  drop_rdb_view_if_exists(Connection,TableName)
	 ),
	 _,
	 ColNameTypes=[]
	    ).
	    
	    
% % Drop a view if it exists
% my_odbc_drop_view_if_exists(TableName) :-
%   current_db(Connection),
%   my_odbc_drop_view_if_exists(Connection,TableName).
%   
% my_odbc_drop_view_if_exists(Connection,TableName) :-
%   (my_odbc_exists_view(Connection,TableName) ->
%     my_odbc_drop_view(Connection,TableName)
%    ;
%     true).
%       
% % Drop a view
% my_odbc_drop_view(TableName) :-
%   current_db(Connection),
%   my_odbc_drop_view(Connection,TableName).
%   
% my_odbc_drop_view(Connection,TableName) :-	    
%   name(TableName,TableNameStr),
%   my_odbc_get_dbms(Connection,DBMS),
%   (DBMS == access ->
%     concat_lists(["DROP TABLE ",TableNameStr],DropStr)
%    ;
%     concat_lists(["DROP VIEW ",TableNameStr],DropStr)
%   ),
%   my_odbc_ddl_query(Connection,DropStr).


% Get the query schema
get_columns_schema([],[]).
get_columns_schema([column(Table,Column,_Value)|Columns],[TableDotColumn|ColumnsSchema]) :-
atomic_list_concat([Table,'.',Column],TableDotColumn),
%  atom_concat(Table,'.',TableDot),
%  atom_concat(TableDot,Column,TableDotColumn),
  get_columns_schema(Columns,ColumnsSchema).
    
  
%%%%%%% ODBC. Others %%%%%%%%

%% SAMER
my_odbc_get_dbms_name(Connection,DBMSName) :-
  current_db(Connection),
  opened_db(Connection,(Handle,_Options),_DBMS),
  odbc_get_connection(Handle,dbms_name(DBMSName)).
%%
  
% Get the DBMS for the current connection
my_odbc_get_dbms(DBMS) :-
  current_db(Connection),
  my_odbc_get_dbms(Connection,DBMS).
  
my_odbc_get_dbms('$des','$des') :-
  !.
my_odbc_get_dbms(Connection,DBMS) :-
  opened_db(Connection,(Handle,_Options),_DBMS),
  odbc_get_connection(Handle,dbms_name(DBMSName)),
  get_dbms_from_DSN(DBMSName,DBMS),
  !.
  
get_dbms_from_DSN(DSN,access) :-
  is_DBMS(DSN,'ACCESS').
get_dbms_from_DSN(DSN,db2) :-
  is_DBMS(DSN,'DB2').
get_dbms_from_DSN(DSN,mysql) :-
  (is_DBMS(DSN,'MYSQL')
  ;
   is_DBMS(DSN,'LIBMYODBC')
  ),
  !.
get_dbms_from_DSN(DSN,oracle) :-
  is_DBMS(DSN,'ORACLE').
get_dbms_from_DSN(DSN,postgresql) :-
  is_DBMS(DSN,'POSTGRESQL').
get_dbms_from_DSN(DSN,sqlserver) :-
  is_DBMS(DSN,'SQL SERVER').
get_dbms_from_DSN(DSN,sybase) :-
  is_DBMS(DSN,'ADAPTIVE SERVER ENTERPRISE').
get_dbms_from_DSN(DSN,sqlanywhere) :-
  is_DBMS(DSN,'SQL ANYWHERE').
get_dbms_from_DSN(DSN,DSN).
  
is_DBMS(DSN,Text) :-
  to_uppercase(DSN,UDSN),
  sub_atom(UDSN,_,_,_,Text).
  
%%%%%%% End ODBC  %%%%%%%%

  
%%%%%%%% FD Constraint Solving %%%%%%%% 

% FD constraints
my_fd_domain(List,Min,Max) :-
  List ins Min..Max.
    
% FD enumeration
my_fd_labeling(Vars) :-
  labeling([ff],Vars).
    
my_fd_labeling(LabOptions,Vars) :-
  labeling(LabOptions,Vars).
    
% Optimization
my_fd_maximize(my_fd_labeling(LabOptions,Vars),Cost) :-
  once(labeling([max(Cost)|LabOptions], Vars)).
  
% All different
my_fd_all_different(L) :-
  all_different(L).
    
% Current lower bound of the domain of a variable
my_fd_min(X,M) :-
  fd_inf(X,M).
    
% FD Equality
% Avoid integer type checking
my_fd_eq(X,Y) :-
  X = Y.
  
% Testing whether the argument is a var (FD or not)
my_var_or_fd_var(X) :-
  var(X).
  
% FD equivalence operator
% my_fd_equiv(L,R) :-
%   L #<==> R.
:- op(760, yfx, #<=>). 
L #<=> R :-
   L #<==> R.

      
%%%%%%%% END FD Constraint Solving %%%%%%%% 


%%%%%%%% Attributed variables  %%%%%%%% 

my_put_type_att(Var,Type) :-
  my_get_type_atts(Var,Types),
  add_type_attr(Type,Types,NTypes),
  put_attr(Var,des_atts,types(NTypes)).
  

add_type_attr(Type,Types,Types) :-
  memberchk(Type,Types),
  !.
add_type_attr(Type,Types,[Type|Types]).

  
my_get_type_atts(Var,Types) :-
  get_attr(Var,des_atts,types(Types)),
  !.
my_get_type_atts(_Var,[]).
  
%%%%%%%% END Attributed variables  %%%%%%%% 


% false predicate: Always fails
% Not needed in SWI-Prolog 6.2.1
%false :-
%  fail.

% Current opened streams
my_current_stream(St) :-
  my_nf_bagof(OSt,X^Y^current_stream(X,Y,OSt),OSts),
  my_member_chk(St,OSts), 
  !.

% The following allows SWI to detect predicate current_stream as needed for building the executable  
my_void_current_stream(X,Y,OSt) :- 
  current_stream(X,Y,OSt).

% Skip end-of-line char
my_skip_line.

% Executing operating system commands
% Sync \in {sync,async} for either synchronous or asynchronous shell
% my_shell(C,S) :-
%   name(C,SC),
%   ((shell(SC,0), S=0);                                             %Unix
%    (concat_lists(["cmd.exe /C ",SC],SCC), shell(SCC,0), S=0);      %Windows NT/XP/Vista/7
%    (concat_lists(["command.exe /C ",SC],SCC), shell(SCC,0), S=0);  %Windows 98
%    S=1),
%   !.
%
% The following fails due to process_create for calls as  process_create(path(cmd), ['/C',copy,'a.dl','b.dl','/y'],[]).
% It seems that the arguments are not properly handled. This was posted but no luck.
% So, we resort to the old shell.
%
% my_shell(Command,Sync,Exit_code) :-
%   os(windows),
%   !,
%     %% MS Windows:
%     my_working_directory(Path),
%     %% Get the list of arguments from Command
%     get_cmd_line_args(Command,Args),
% %   nl,
% %   write(Args),
% %   nl,
%     %% Create process and pass its stdout and stderr to pipes
%     process_create(path(cmd), ['/C'|Args],
% %    process_create(path(Cmd), Args,
%                    [
%                     stdin(null), 
% %                    stdin(pipe(Stdin)), 
% %                    stdin(std), 
%                     stderr(pipe(Stderr)),
%                     stdout(pipe(Stdout)),
%                     cwd(Path),
%                     window(false), 
%                     process(ProcessId)]),
%     (Sync==sync
%      ->
%   	  %% Read from the pipe until EOF and then wait
%   	  %% for the process to exit.
%       repeat,
%          read_line_to_codes(Stdout, Line),
%          ( Line == end_of_file ->
%            close(Stdout)
%          ; otherwise ->
%            forall(member(C,Line),(my_put_code(C),write_only_to_log([C]))),
%            nl_log,
%            fail
%          ),
%       repeat,
%          read_line_to_codes(Stderr, ErrLine),
%          ( ErrLine == end_of_file ->
%            close(Stderr)
%          ; otherwise ->
%            forall(member(ErrC,ErrLine),(my_put_code(ErrC),write_only_to_log([ErrC]))),
%            nl_log,
%            fail
%          ),
%   	  !,
%   	  process_wait(ProcessId, exit(Exit_code)),
%   	  (Exit_code==0 -> 
%         true
%        ;
%         write_log_list([nl,'Error: Operating system command returned exit status: ',Exit_code,nl])
%       )
%      ;
%       true).
my_shell(Command,Sync,Exit_code) :-
  os(windows),
  !,
  %% Create process and pass their streams to pipes
  (exists_file('cmd.exe') ->
    true
   ;
    shell('cmd /c copy %COMSPEC% .',_S)
%      getenv('COMSPEC',COMSPEC), % Cannot load filesex
%      copy_file(COMSPEC,'.')
  ),
  atom_concat('cmd /c ',Command,Shell),
  exec(Shell,
       [null, 
        pipe(Stdout),
        pipe(Stderr)],
       Process),
  (Sync==sync
   ->
	  %% Read from the pipes until EOF and then wait
	  %% for the process to exit.
	  repeat,
	     read_line_to_codes(Stdout, Line),
	     ( Line == end_of_file ->
	       close(Stdout)
	     ; otherwise ->
	       forall(member(C,Line),
	              (my_put_code(C),
	               write_only_to_log([C]))),
	       nl_log,
	       fail
	     ),
	  repeat,
	     read_line_to_codes(Stderr, ErrLine),
	     ( ErrLine == end_of_file ->
	       close(Stderr)
	     ; otherwise ->
	       forall(member(ErrC,ErrLine),
	              (my_put_code(ErrC),
	               write_only_to_log([ErrC]))),
	       nl_log,
	       fail
	     ),
	  !,
  	  process_wait(Process, exit(Exit_code)),
  	  (Exit_code==0 -> 
        true
       ;
        write_log_list([nl,'Error: Operating system command returned exit status: ',Exit_code,nl])
      )
     ;
      true).
my_shell(Command,Sync,Exit_code) :-
    % Unixes:
   (Sync==sync
    -> 
     shell(Command,Exit_code),
     (Exit_code==0 -> 
       true
      ;
       write_log_list([nl,'Error: Operating system command returned exit status: ',Exit_code,nl])
     )
    ;
     atom_concat(Command,' &',ACommand),
     shell(ACommand,_Exit_code)
   ).

  
my_put_code(C) :-
  (output(on)
   ->
    put_code(C)
   ;
    true
  ).
  
% my_with_output_to_codes(Goal,String) :-
%   with_output_to_codes(Goal,String).
  
% get_cmd_line_args(Command,Args) :-
%   atom_codes(Command,SCommand),
%   phrase(cmd_args(Args),SCommand),
%   !.
  
% cmd_args([Arg|Args]) -->
%   cmd_separator_star,
%   cmd_arg(Arg),
%   cmd_separator,
%   cmd_args(Args).    
% cmd_args([Arg]) -->
%   cmd_arg(Arg),
%   cmd_separator_star.
% % cmd_args([file(Arg)|Args]) -->
% %   cmd_separator_star,
% %   cmd_arg(Arg),
% %   cmd_separator,
% %   cmd_args(Args).    
% % cmd_args([file(Arg)]) -->
% %   cmd_arg(Arg),
% %   cmd_separator_star.
  
% cmd_separator_star -->
%   my_blanks_star.
%   
% cmd_separator -->
%   my_blanks.
%   
% cmd_arg(Arg) -->
%   "'",
%   cmd_id(UQArg),
%   "'",
%   {!,
%    Arg=UQArg}.
% cmd_arg(Arg) -->
%   """",
%   file_spec(UQArg),
%   """",
%   {!,
%    Arg=UQArg}.
% %  concat_atom(['"',UQArg,'"'],Arg)}.
% cmd_arg(Arg) -->
%   cmd_id(Arg).
% cmd_arg(Arg) -->
%   file_spec(Arg).
%   
% cmd_id(Arg) -->
%   my_non_single_quote_chars(SArg),
%   {atom_codes(Arg,SArg)}.
%   
% file_spec(Arg) -->
%   my_non_double_quote_chars(SArg),
%   {atom_codes(Arg,SArg)}.
%   
% my_non_double_quote_chars([C]) -->
%   my_non_double_quote_char(C).
% my_non_double_quote_chars([C|Cs]) -->
%   my_non_double_quote_char(C),
%   my_non_double_quote_chars(Cs).
%   
% my_non_double_quote_char(C) -->
%   [C],
%   {[DQ]="""",
%    C\==DQ}.
%   
% my_non_single_quote_chars([C]) -->
%   my_non_single_quote_char(C).
% my_non_single_quote_chars([C|Cs]) -->
%   my_non_single_quote_char(C),
%   my_non_single_quote_chars(Cs).
%   
% my_non_single_quote_char(C) -->
%   [C],
%   {[DQ]="'",
%    C\==DQ}.
  
    
% Looks for FileName in the Windows semi-colon-separated list of paths DisjunctivePath,
% returning only the first occurrence, if any, otherwise fails

% Why does not work this?
% my_Windows_which(FileName,AbsoluteFileName) :-
%   file_alias_path(path,P),
%   concat_atom([P,'/',FileName],AbsoluteFileName),
%   exists_file(AbsoluteFileName),
%   !.

% This works but not needed
% my_Windows_which(FileName,AbsoluteFileName) :-
%   getenv('PATH',DisjunctivePath),
%   extract_paths(DisjunctivePath,Paths),
%   member(Path,Paths),
%   concat_atom([Path,'\\',FileName],AbsoluteFileName),
%   exists_file(AbsoluteFileName),
%   !.
%   
% extract_paths(Path,Paths) :-
%   atom_codes(Path,SPaths),
%   extract_str_paths(SPaths,Paths).
%   
% extract_str_paths([],[]) :-
%   !.
% extract_str_paths(SPaths,[Path|Paths]) :-
%   [SC]=";",
%   append(SPath,[SC|TSPaths],SPaths),
%   !,
%   atom_codes(Path,SPath),
%   extract_str_paths(TSPaths,Paths).
% extract_str_paths(SPath,[Path]) :-
%   B="\\",
%   append(LSPath,B,SPath),
%   !,
%   atom_codes(Path,LSPath).
% extract_str_paths(SPath,[Path]) :-
%   atom_codes(Path,SPath).
  
% Date and time
my_datetime((Y,M,D,H,Mi,S)) :-
  get_time(TS),
  stamp_date_time(TS,date(Y,M,D,H,Mi,Se,_,_,_),local),
  S is integer(Se).
  
% Sorting a list, keeping duplicates
my_sort(List,Orderedlist) :-
  msort(List,Orderedlist).

% Sorting a list, removing duplicates
my_remove_duplicates_sort(List,Orderedlist) :-
  sort(List,Orderedlist).

% Removing a file
my_remove_file(FileName) :-
  delete_file(FileName).
  
% Changing the current directory
my_change_directory(Path) :-
  chdir(Path).

% Testing whether exists the given directory
my_directory_exists(Path) :-
  exists_directory(Path).

% Testing whether exists the given file
my_file_exists(File) :-
  exists_file(File).
  
file_exists(File) :-
  exists_file(File).

% Getting the current directory
my_working_directory(Path) :-
  absolute_file_name(., Path).

% Getting the ordered list of files from the given path
my_directory_files(Path, Files) :-
  absolute_file_name(., WorkingPath),
  absolute_file_name(Path, AbsolutePath),
  chdir(AbsolutePath),
  expand_file_name('*', FNs),
  (setof(FN, AFN^(member(FN,FNs),my_absolute_filename(FN,AFN),my_is_file(AFN)), Files) ->
   true ; Files = []),
  chdir(WorkingPath).

% Getting the ordered list of directories from the given path
my_directory_directories(Path, Directories) :-
  absolute_file_name(., WorkingPath),
  absolute_file_name(Path, AbsolutePath),
  chdir(AbsolutePath),
  expand_file_name('*', FNs),
  (setof(FN, AFN^(member(FN,FNs),my_absolute_filename(FN,AFN),my_is_directory(AFN)), Directories) ->
   true ; Directories = []),
  chdir(WorkingPath).

% Testing whether the input is a file
my_is_file(File) :-
  exists_file(File).

% Testing whether the input is a directory
my_is_directory(Path) :-
  exists_directory(Path).

% Getting the absolute filename for a file
my_absolute_file_name(Path,AbsolutePath) :-
  my_absolute_filename(Path,AbsolutePath).
  
my_absolute_file_name(Path,RelativeTo,AbsolutePath) :-
  my_absolute_filename(Path,RelativeTo,AbsolutePath).

my_absolute_filename(Path,AbsolutePath) :-
  absolute_file_name(Path,AbsolutePath).

my_absolute_filename(Path,RelativeTo,AbsolutePath) :-
  absolute_file_name(Path,AbsolutePath,[relative_to(RelativeTo)]).

% Extracting the absolute path and file from an absolute file name
my_dir_file(AFN,AP,FN) :-
  file_directory_name(AFN,AP),
%  atomic_list_concat([AP,'/',FN],AFN). % FN cannot be non-ground
   atom_concat(AP,'/',PS),
   atom_concat(PS,FN,AFN).

% Gets a byte from the handle (Edinburgh version)
my_get0(HIn,C) :-
  get0(HIn,C).

% Gets a byte from the current handle (Edinburgh version)
my_get0(C) :-
  get0(C).

% Reading terms along with variable names and its scope in line numbers
% TODO: Get max line numbers
my_read(Term, VariableNames, (Min,Max)) :-
  current_input(CurrentStream),
  stream_property(CurrentStream,position(CurrentPos)),
  stream_position_data(line_count,CurrentPos,Min),
  read_term(Term, [variable_names(NAVariableNames),variables(Variables),term_position(NewPos)]),
  complete_anonymous_variables(Variables,NAVariableNames,VariableNames),
  stream_position_data(line_count,NewPos,Max).
  
my_read_message(Fid,error(syntax_error(ErrorTxt),file(_,Line,Column,_))) :-
  file_table(F,Fid),
  write_error_log(['Syntax error: ',ErrorTxt,nl,'       File: ',F,nl,'       Line: ',Line, ', Column: ',Column]).  

% Timing: Resetting and displaying the elapsed time
:- dynamic(elapsed_time/1).

reset_elapsed_time :-
  get_time(T),
  retractall(elapsed_time(_)),
  retractall(elapsed_time(_,_,_)),
  assertz(elapsed_time(_,_,_)),
  assertz(elapsed_time(T)).

update_elapsed_time(Elapsed) :-
  retract(elapsed_time(T1)),
  get_time(T2),
  assertz(elapsed_time(T2)),
  Elapsed is integer(1000*(T2-T1)).

% my_get_time returns an absolute time in milliseconds
my_get_time(MillisecondsTime) :-
  get_time(SecondsTime),
  MillisecondsTime is integer(1000*SecondsTime).
  
%%%%%%%% BEGIN Others %%%%%%%% 

% nonmember
nonmember(X,Y) :-
  \+ member(X,Y).

% Consulting Prolog programs
% my_consult(X) :-
%   ensure_loaded(X).
  
% Loading a module
my_load_module(File) :-
  load_files([File],[redefine_module(true)]).

% Conversions
my_atom_number(A,N) :-
  atom_number(A,N).

my_term_to_string_pl(T,Opts,S,NVs) :-
  copy_term((T,NVs),(CT,CNVs)),
%  my_map(nf_id,CNVs),
  var_to_names(CNVs),
  write_term_to_codes(CT,S,[numbervars(true)|Opts]).
    
var_to_names([]) :-
  !.
var_to_names([=(Name,Var)|RestofPairs]) :-
  (var(Var)
   ->
    Var = '$VAR'(Name)
   ;
    true
  ),
  var_to_names(RestofPairs).  
  
my_fact(Fact) :-
  clause(Fact,true).

my_subsumes_chk(X,Y) :-
  subsumes_chk(X,Y).

% my_limited_xor(X1,X0,Max,X2) :-
%   X2 is xor(X1,X0) /\ Max.

% Timeout in seconds
system_timeout(Goal,Time,Result) :-
  catch(call_with_time_limit(Time,Goal),time_limit_exceeded,Result=time_out).

% Strongly connected components of a graph
my_scc(Nodes,Arcs,Scc) :-
  findall(arc(A,B),(member(AB,Arcs), once((AB=(A+B);AB=(A-B)))),Edges),
%  my_nf_setof(arc(A,B),(member(A+B,Arcs);member(A-B,Arcs)),Edges),
  nodes_arcs_sccs(Nodes,Edges,Scc).
  
% Save state
my_save_state(_Persistents,_DBs,_Options,_CDB) :-
  predicate_property(P,dynamic),
  \+ predicate_property(P,built_in),
  \+ predicate_property(P,volatile),
  functor(P,F,A),
  F \== batch,
  write(':- abolish('),
  writeq(F),
  write_list(['/',A,').']),
  nl,
%   (F/A==batch/4
%    ->
%     write(':- dynamic '),
%     writeq(F),
%     write_list(['/',A,'.']),
%     nl,
%     nl
%    ;
    listing(F/A),
%   ),
  fail.
my_save_state(Persistents,DBs,Options,CDB) :-
  my_save_state_common(Persistents,DBs,Options,CDB).
 
% Restore state
my_restore_state(AFN) :-
  [AFN].
  

%%%%%%%% END Others %%%%%%%% 

%%%%%%%% BEGIN VERBATIM COPY OF Markus Triska's scc.pl %%%%%%%% 
% Module declaration has been commented

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Strongly connected components of a graph.
   Written by Markus Triska (triska@gmx.at), May 2011
   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%:- module(scc, [nodes_arcs_sccs/3]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Example:

   %?- nodes_arcs_sccs([a,b,c,d], [arc(a,b),arc(b,a),arc(b,c)], SCCs).
   %@ SCCs = [[a, b], [c], [d]].

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


nodes_arcs_sccs(Ns, As0, Ss) :-
        length(Ns, L),
        length(Vs, L),
        pairs_keys_values(Ps, Ns, Vs),
        catch((maplist(attach_arc(Ps), As0),
               scc(Vs, successors),
               maplist(v_with_link(Ps), Vs, Ls0),
               keysort(Ls0, Ls1),
               group_pairs_by_key(Ls1, Ss0),
               pairs_values(Ss0, Ss),
               % reset all attributes
               throw(scc(Ss))),
              scc(Ss),
              true).

v_with_link(Ps, V0, L-V) :-
        get_attr(V0, lowlink, L),
        member(V-X, Ps),
        X == V0,
        !.

successors(V, Vs) :-
        (   get_attr(V, successors, Vs) -> true
        ;   Vs = []
        ).

attach_arc(Ps, arc(X,Y)) :-
        memberchk(X-VX, Ps),
        memberchk(Y-VY, Ps),
        (   get_attr(VX, successors, Vs) -> true
        ;   Vs = []
        ),
        put_attr(VX, successors, [VY|Vs]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Tarjan's strongly connected components algorithm.

   DCGs are used to implicitly pass around the global index, stack
   and the predicate relating a vertex to its successors.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

scc(Vs, Succ) :- phrase(scc(Vs), [s(0,[],Succ)], _).

scc([])     --> [].
scc([V|Vs]) -->
        (   vindex_defined(V) -> scc(Vs)
        ;   scc_(V), scc(Vs)
        ).

scc_(V) -->
        vindex_is_index(V),
        vlowlink_is_index(V),
        index_plus_one,
        s_push(V),
        successors(V, Tos),
        each_edge(Tos, V),
        (   { get_attr(V, index, VI),
              get_attr(V, lowlink, VI) } -> pop_stack_to(V, VI)
        ;   []
        ).

vindex_defined(V) --> { get_attr(V, index, _) }.

vindex_is_index(V) -->
        state(s(Index,_,_)),
        { put_attr(V, index, Index) }.

vlowlink_is_index(V) -->
        state(s(Index,_,_)),
        { put_attr(V, lowlink, Index) }.

index_plus_one -->
        state(s(I,Stack,Succ), s(I1,Stack,Succ)),
        { I1 is I+1 }.

s_push(V)  -->
        state(s(I,Stack,Succ), s(I,[V|Stack],Succ)),
        { put_attr(V, in_stack, true) }.

vlowlink_min_lowlink(V, VP) -->
        { get_attr(V, lowlink, VL),
          get_attr(VP, lowlink, VPL),
          VL1 is min(VL, VPL),
          put_attr(V, lowlink, VL1) }.

successors(V, Tos) --> state(s(_,_,Succ)), { call(Succ, V, Tos) }.

pop_stack_to(V, N) -->
        state(s(I,[First|Stack],Succ), s(I,Stack,Succ)),
        { del_attr(First, in_stack) },
        (   { First == V } -> []
        ;   { put_attr(First, lowlink, N) },
            pop_stack_to(V, N)
        ).

each_edge([], _) --> [].
each_edge([VP|VPs], V) -->
        (   vindex_defined(VP) ->
            (   v_in_stack(VP) ->
                vlowlink_min_lowlink(V, VP)
            ;   []
            )
        ;   scc_(VP),
            vlowlink_min_lowlink(V, VP)
        ),
        each_edge(VPs, V).

v_in_stack(V) --> { get_attr(V, in_stack, true) }.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   DCG rules to access the state, using right-hand context notation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

state(S), [S] --> [S].

state(S0, S), [S] --> [S0].

%%%%%%%% END VERBATIM COPY OF Markus Triska's scc.pl %%%%%%%% 

%%%%%%%%%%%%%%%  END des_glue.pl  %%%%%%%%%%%%%%%
