/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.4.1                 */
/*                                                       */
/*    Database generator                                 */
/*                                                       */
/*                                                       */
/*                                  Fernando Saenz-Perez */
/*                                         (c) 2004-2016 */
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

/* Use example:

DES> /generate_db 4 10 40 5 3 p.sql

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extra parameters as flags:
:-dynamic(gen_number_of_table_columns/1).
:-dynamic(gen_column_type/1).
:-dynamic(gen_children_density/1). % Name of the mutated view
gen_number_of_table_columns(4).
gen_column_type(number(integer)).
gen_children_density(70). % Probability (0-100) of having MaxChildren in each node
% Other parameters as dynamic predicates
:-dynamic(gen_number_of_tables/1).
:-dynamic(gen_number_of_views/1).
:-dynamic(gen_buggy/1). % Name of the mutated view
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Debug SQL bench
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

debug_sql_bench(NbrTables,TableSize,NbrViews,MaxDepth,MaxChildren,FileName) :-
  set_flag(error(0)),
  debug_sql_bench(NbrTables,TableSize,NbrViews,MaxDepth,MaxChildren,FileName,3). % Up to three tries before giving up
  
debug_sql_bench(_NbrTables,_TableSize,_NbrViews,_MaxDepth,_MaxChildren,_FileName,0) :-
  write_error_log(['Unable to generate a mutated database.']),
  !,
  set_flag(error(1)).
debug_sql_bench(NbrTables,TableSize,NbrViews,MaxDepth,MaxChildren,FileName,_Tries) :-
  set_flag(gen_number_of_tables,NbrTables),
  set_flag(gen_number_of_views,NbrViews),
  filename_trusted(FileName,TrustFileName),
  random_seed(Seed),
  set_flag(debug_random_seed,Seed),
  generate_db_instance(NbrTables,TableSize,NbrViews,MaxDepth,MaxChildren,TrustFileName,gen_no_inserts,without_tables,without_commands,Tables,Inserts,Views),
  !,
  catch(
    mutate_db(Tables,Inserts,Views,FileName),
    M,
    (write_error_log([M,nl]),
     set_flag(error(1))
    )  ),
  !.
debug_sql_bench(NbrTables,TableSize,NbrViews,MaxDepth,MaxChildren,FileName,Tries) :-
  Tries1 is Tries-1,
  write_info_log(['Unable to mutate database. Retrying (',Tries1,' left).']),
  debug_sql_bench(NbrTables,TableSize,NbrViews,MaxDepth,MaxChildren,FileName,Tries1).

% debug_sql_bench(2,10,3,2,2,'p.sql').
% debug_sql_bench(4,10,40,5,3,'p.sql').
% /debug_sql_bench 2 10 3 2 2 p.sql
% /debug_sql_bench 4 10 10 3 3 p.sql


filename_trusted(FileName,TrustFileName) :-
  atomic_concat(Name,'.sql',FileName),
  name_trusted(Name,'_trust',NTrustFileName),
  atomic_concat(NTrustFileName,'.sql',TrustFileName),
  !.
filename_trusted(_FileName,_TrustFileName) :-
  write_error_log(['Filename without extension .sql']),
  !,
  fail.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mutate DB
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mutate_db(Tables,Inserts,Views,FileName) :-
  write_info_log(['Mutating the database...']),
  append(_OtherViews,[RootView],Views),
  get_create_view_name(RootView,RootName),
  get_tuples_in_relation(RootName,RootTuples),
  mutate_view(Views,Views,3,RootName,RootTuples,View,MView),
  append(LViews,[View|RViews],Views),
  append(LViews,[MView|RViews],MViews),
  generate_file(FileName,Tables,Inserts,MViews),
  !.
  
get_create_view_name(create_view(_Lang,_SQL,Schema),Name) :-
  Schema=..[Name|_].

mutate_view(_Views,_AllViews,0,_RootName,_RootTuples,_View,_MView) :-
  !,
  write_error_log(['Unable to mutate.']),
  set_flag(gen_buggy('**ERROR**')),
  fail.
mutate_view([],AllViews,N,RootName,RootTuples,View,MView) :-
  !,
  N1 is N-1,
  write_info_log(['Unable to mutate. Retrying (',N1,' left).']),
  mutate_view(AllViews,AllViews,N1,RootName,RootTuples,View,MView).
mutate_view(Views,AllViews,N,RootName,RootTuples,View,MView) :-
  select_random_list(Views,RView), 
  RView=create_view(sql,SQL,Schema),
  get_create_view_name(RView,Name),
  length(RootTuples,NbrRootTuples),
  write_info_log(['Mutating view ',Name,': ',NbrRootTuples,'$tbc']),
(Name='v3'->deb;true),
  (mutate_query(SQL,MSQL)
   ->
%    Schema=..[Name|_],
    create_or_replace_view((MSQL,Schema)),
%     drop_view_k(Name,no_warn),
%     create_view_k(sql,(MSQL,Schema),Schema,[]),
%     clear_et,
%     compute_stratification,
    !,
    get_tuples_in_relation(RootName,MRootTuples),
%   write_log(RootTuples), 
%   write_log(MRootTuples), 
    (MRootTuples=RootTuples
     ->
      write_log_list(['.',nl]),
      create_or_replace_view((SQL,Schema)),
%       drop_view_k(Name,no_warn),
%       create_view_k(sql,(SQL,Schema),Schema,[]),
%   %     clear_et,
%   %     compute_stratification,
      remove_from_list(RView,Views,RViews),
      !,
      mutate_view(RViews,AllViews,N,RootName,RootTuples,View,MView)
     ;
      length(MRootTuples,NbrMRootTuples),
      write_log_list(['.',NbrMRootTuples,' (',RootName,')']),
      View=RView,
      MView=create_view(sql,MSQL,Schema),
      Schema=..[Name|_],
      set_flag(gen_buggy(Name))
    )
   ;
    nl_log,
    remove_from_list(RView,Views,RViews),
    !,
    mutate_view(RViews,AllViews,N,RootName,RootTuples,View,MView)).
  
mutate_query((Query,_),AQuery) :-
  mutate_query(Query,AQuery).
mutate_query(
            select(DistinctAll,Top,Exprs,from(Rs),where(Cs),group_by([]),having(true),order_by([],[])),
            select(DistinctAll,Top,Exprs,from(Rs),where(ACs),group_by([]),having(true),order_by([],[]))) :-
%   my_unzip(Rs,SQLs,_), % To mutate table and view prefixed attributes
%   filter_atoms(SQLs,RelNames),
  mutate_where_cond(Cs,ACs).
mutate_query(
            union(DistinctAll,(SQL1,Schema1),(SQL2,Schema2)),
            union(DistinctAll,(ASQL1,Schema1),(SQL2,Schema2))) :-
  !,
  mutate_query(SQL1,ASQL1).
mutate_query(
            union(DistinctAll,(SQL1,Schema1),(SQL2,Schema2)),
            union(DistinctAll,(SQL1,Schema1),(ASQL2,Schema2))) :-
  !,
  mutate_query(SQL2,ASQL2).
mutate_query(
            intersect(DistinctAll,SQL1,SQL2),
            union(DistinctAll,SQL1,SQL2)) :-
  !.
mutate_query(
            except(DistinctAll,SQL1,SQL2),
            union(DistinctAll,SQL1,SQL2)) :-
  !.
    
mutate_where_cond(Cond,MCond) :-
  findall(X,my_member_term(X,Cond),Xs),
  filter_non_mutable(Xs,Ys),
  select_random_list(Ys,X),
  mutate_where_basic(X,MX), 
%  replace_term(X,MX,Cond,MCond).
  replace_var_term(X,MX,Cond,MCond).
  
% Do not include non-mutable objects
filter_non_mutable([],[]).
filter_non_mutable([X|Xs],Ys)  :-
  non_mutable(X),
  !,
  filter_non_mutable(Xs,Ys).
filter_non_mutable([X|Xs],[X|Ys])  :-
  filter_non_mutable(Xs,Ys).
  
non_mutable(true) :-
  !.
non_mutable(Ren) :-
  atom(Ren),
  atom_concat('$',_,Ren),
  !.
non_mutable(X) :-
  atom(X),
  (my_odbc_identifier_name(t,P)   % Table
   ;
   my_odbc_identifier_name(v,P)), % View
  atom_concat(P,_,X),
  !.

mutate_where_basic(true,false) :-
  !.
% mutate_where_basic(true,Cond) :-
%   !,
%   fail. % Select another 
%   select_random_list(['=','<','>','>=','<=','<>'],Op),
%   get_random_integer(10,N),
%   select_random_list([a,b,c,d],A),
%   Cond=..[Op,attr(_T,A,_R),N].
mutate_where_basic(attr(T,A,R),attr(T,MA,R)) :-
  !,
  gen_table_column_names(T,Cs),
  remove_from_list(A,Cs,L),
  select_random_list(L,MA).
mutate_where_basic(and(C1,C2),or(C1,C2)) :-
  !.
mutate_where_basic(or(C1,C2),and(C1,C2)) :-
  !.
mutate_where_basic(not(C),C) :-
  !.
mutate_where_basic(Comp,MComp) :-
  Comp=..[Op,E1,E2],
  !,
  my_set_diff(['=','<','>','>=','<=','<>'],Op,Ops),
  select_random_list(Ops,MOp),
  MComp=..[MOp,E1,E2].
mutate_where_basic(N,M) :-
  number(N),
  !,
  get_random_integer(10,R),
  M is N+R-5.
% mutate_where_basic(T,MT) :-
%   (atom_concat(t,N,T), % Table
%    gen_number_of_tables(Max),
%    Type=t
%    ;
%    atom_concat(v,N,T), % View
%    gen_number_of_views(Max),
%    Type=v),
%   !,
%   list_between(1,Max,Ns),
%   remove_from_list(N,Ns,L),
%   select_random_list(L,M),
%   atom_concat(Type,M,MT).
mutate_where_basic(A,B) :-
  atom(A),
  !,
  gen_number_of_table_columns(Arity),
  gen_column_names(Arity,Columns),
  remove_from_list(A,Columns,L),
  select_random_list(L,B).
mutate_where_basic(N,_M) :-
  write_error_log(['Cannot mutate ',N]),
  !,
  fail.
  
gen_table_column_names(T,Cs) :-
  gen_number_of_table_columns(Arity),
  gen_typed_schema(T,Arity,Schema),
  Schema=..[_|CTs],
  my_unzip(CTs,Cs,_Ts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Database generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DB Generation for command /generate_db
generate_db_instance(NbrTables,TableSize,NbrViews,MaxDepth,MaxChildren,FileName) :-
  set_flag(error(0)),
  generate_db_instance(NbrTables,TableSize,NbrViews,MaxDepth,MaxChildren,FileName,gen_inserts,with_tables,without_commands,_Tables,_Inserts,_AViews),
  !.
generate_db_instance(_NbrTables,_TableSize,_NbrViews,_MaxDepth,_MaxChildren,_FileName) :-
  set_flag(error(1)).
  
generate_db_instance(NbrTables,TableSize,NbrViews,MaxDepth,MaxChildren,FileName,GenInserts,WithTables,WithCommands,Tables,Inserts,AViews) :-
%  set_random_seed, % To set a seed depending on the time
%  set_fixed_random_seed, % To reproduce the same results
  check_generate_parameters(NbrTables,NbrViews,MaxChildren),
  write_info_log(['Generating the database.']),
  generate_table_nodes(NbrTables,TableNodes),
  generate_view_nodes(NbrViews,ViewNodes),
  generate_pdg(TableNodes,ViewNodes,NbrViews,MaxDepth,MaxChildren,PDG),
  generate_tables(TableNodes,Tables),
  generate_inserts(Tables,TableSize,Inserts),
  set_flag(pdg(PDG)), % Only to see the PDG in ACIDE
  sort_by_topological_order(ViewNodes,OViewNodes),
%  my_reverse(OViewNodes,ROViewNodes),
  generate_views(OViewNodes,PDG,TableSize,Views),
  generate_file(FileName,Tables,Inserts,Views),
  write_info_log(['Processing the generated database.']),
  process_gen_file(FileName),
  write_info_log(['Ensuring non-empty views.']),
  ensure_non_empty_views(OViewNodes,Views,AViews),
  (GenInserts==gen_inserts -> FInserts=Inserts ; FInserts=[]),
  generate_file(FileName,Tables,FInserts,AViews,WithTables,WithCommands),
  !.

check_generate_parameters(NbrTables,NbrViews,MaxChildren) :-
  (NbrTables<1 -> write_error_log(['There must be one table at least.']), fail ; true),
  (NbrViewsMax is NbrViews*MaxChildren, NbrTables>NbrViewsMax -> write_error_log(['Too much tables.']), fail ; true),
  (gen_number_of_table_columns(NbrColumns), NbrColumns<2 -> write_error_log(['At least, two columns are required. Set this number with /set_flag gen_number_of_table_columns NbrColumns']), fail ; true),
%  (gen_column_type(ColumnType), type_equivalence(ColumnType,_,_) -> true ; setof(ColumnType,A^B^type_equivalence(ColumnType,A,B),ColumnTypes), write_error_log(['Invalid internal type. Possible values are: ',ColumnTypes,'. Set one of this values with /set_flag gen_column_type ColumnType']), fail).
  (gen_column_type(ColumnType), type_equivalence(ColumnType,_,_) -> true ; write_error_log(['Invalid internal type. Possible values are: number(integer), number(float), string(varchar), string(varchar(N)), and string(char(N)), where N is the number of characters. Set one of these values with /set_flag gen_column_type ColumnType']), fail),
  (gen_children_density(Density), Density>=0, Density=<100 -> true ; write_error_log(['Invalid children density. Possible values are in the range [0,..,100]. Set one of these values with /set_flag gen_children_density Density']), fail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate table and view nodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_table_nodes(NbrTables,TableNodes) :-
  table_prefix(T),
  generate_object_nodes(T,NbrTables,1,TableNodes),
  exec_if_development_on(write_info_log(['Table nodes: ',TableNodes])).
  
generate_view_nodes(NbrViews,ViewNodes) :-
  view_prefix(V),
  generate_object_nodes(V,NbrViews,1,ViewNodes),
  exec_if_development_on(write_info_log(['View nodes: ',ViewNodes])).
  
object_node(Object,Number,ObjectName/NbrColumns) :-
  gen_number_of_table_columns(NbrColumns),
  atomic_concat(Object,Number,ObjectName).

generate_object_nodes(Object,Nbr,Nbr,[ObjectNode]) :-
  object_node(Object,Nbr,ObjectNode).
generate_object_nodes(Object,Nbr1,Nbr2,[ObjectNode|ObjectNodes]) :-
  Nbr2<Nbr1,
  object_node(Object,Nbr2,ObjectNode),
  Nbr3 is Nbr2+1,
  generate_object_nodes(Object,Nbr1,Nbr3,ObjectNodes).

table_prefix(T) :-
  my_odbc_identifier_name('t',T).
  
view_prefix(V) :-
  my_odbc_identifier_name('v',V).
  
first_column_name(C) :- 
  my_odbc_identifier_name('a',C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate create table statements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% FK's: t1 <- t2 <- ... <- tn
generate_tables([TableNode|TableNodes],GTables) :-
  GTables=[Table|Tables],
  generate_table(TableNode,Table),
  generate_tables(TableNodes,TableNode,Tables),
  % write_info_verb_log(['Tables: ',GTables]),
  exec_if_development_on(display_sql_list(GTables,2)).
  
generate_tables([],_,[]).
generate_tables([TableNode2|TableNodes],TableNode1,[Table2|Tables]) :-
  generate_table(TableNode2,TableNode1,Table2),
  generate_tables(TableNodes,TableNode2,Tables).
  
generate_table(TName/Arity,Table) :-
  gen_typed_schema(TName,Arity,Schema),
  gen_create_table_stmt(Schema,Table).
  
generate_table(TName/Arity,_,Table) :- % With no FK
  gen_typed_schema(TName,Arity,Schema),
  gen_create_table_stmt(Schema,Table).
% generate_table(TName/Arity,RefTName/_,Table) :- % With FK
%   gen_typed_schema(TName,Arity,Schema),
%   Table=create_table(Schema,[primary_key([a]),foreign_key([b],RefTName,[a])]).
  
gen_create_table_stmt(Schema,create_table(Schema,[primary_key([C]),not_nullables([C])])) :-
  current_db(db2),
  !,
  first_column_name(C).
gen_create_table_stmt(Schema,create_table(Schema,[primary_key([C])])) :-
  first_column_name(C).
  
gen_typed_schema(Name,Arity,Schema) :-
  gen_typed_columns(Arity,TypedCols),
  Schema=..[Name|TypedCols].

gen_typed_columns(Arity,TypedCols) :-
  first_column_name(C),
  gen_typed_columns(Arity,1,C,TypedCols).

gen_typed_columns(Arity,Arity,ColName,[TypedCol]) :-
  gen_typed_column(ColName,TypedCol).
gen_typed_columns(Arity,N,ColName,[TypedCol|TypedCols]) :-
  N<Arity,
  N1 is N+1,
  gen_typed_column(ColName,TypedCol),
  gen_next_colname(ColName,NextColName),
  gen_typed_columns(Arity,N1,NextColName,TypedCols).
  
gen_typed_column(ColName,ColName:Type) :-
  gen_column_type(Type).

gen_next_colname(ColName,NextColName) :-
  atom_codes(ColName,[C1]),
  C2 is C1+1,
  atom_codes(NextColName,[C2]).
  
gen_column_names(Arity,Columns) :-
  first_column_name(C),
  gen_column_names(Arity,0,C,Columns).
  
gen_column_names(Arity,Arity,_,[]).
gen_column_names(Arity,N,Column,[Column|Columns]):-
  gen_next_colname(Column,NextColumn),
  N1 is N+1,
  gen_column_names(Arity,N1,NextColumn,Columns).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate insert statements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_inserts(Tables,TableSize,Inserts) :-
  generate_inserts(Tables,TableSize,Inserts,[]),
  % write_info_verb_log(['Inserts: ',Inserts]),
  exec_if_development_on(display_sql_list(Inserts,2)).
  
generate_inserts([],_TableSize) -->
  [].
generate_inserts([create_table(Schema,_Ctrs)|Tables],TableSize) -->
  {Schema=..[TableName|TypedCols],
   length(TypedCols,Arity),
   coltype_to_col_list(TypedCols,Cols)},
  generate_table_inserts(TableName,Cols,Arity,TableSize,1),
  generate_inserts(Tables,TableSize).

generate_table_inserts(_TableName,_ColNames,_Arity,N1,N2) -->
  {N2>N1},
  [].
generate_table_inserts(TableName,ColNames,Arity,TableSize,N) -->
  {TableSize>=N,
   N1 is N+1,
   gen_value_list(Arity,N,TableSize,Values)},
  [insert_into(TableName,ColNames,[Values])],
  generate_table_inserts(TableName,ColNames,Arity,TableSize,N1).
  
gen_value_list(Arity,N,TableSize,[N,N1|Values]) :-
  N1 is TableSize-N+1,
  L is Arity-2,
  int_random_gen_list(L,TableSize,Values).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate PDG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_pdg(TableNodes,[ViewNodes],1,1,MaxChildren,(Nodes,Arcs)) :-
  !,
  append(TableNodes,[ViewNodes],Nodes),
  connect_viewnodes([[ViewNodes]],TableNodes,MaxChildren,OArcs),
  connect_nonused_tablenodes(TableNodes,[ViewNodes],MaxChildren,OArcs,Arcs).
generate_pdg(TableNodes,ViewNodes,NbrViews,MaxDepth,MaxChildren,(Nodes,Arcs)) :-
  append(TableNodes,ViewNodes,Nodes),
  distribute_viewnodes(NbrViews,MaxDepth,MaxChildren,Grid),
  concat_lists(Grid,ViewNodes),
  length(ViewNodes,NbrViewNodes),
  generate_view_nodes(NbrViewNodes,ViewNodes),
  connect_viewnodes(Grid,TableNodes,MaxChildren,OArcs),
  connect_nonused_tablenodes(TableNodes,ViewNodes,MaxChildren,OArcs,Arcs),
  !,
  exec_if_development_on(display_pdg((Nodes,Arcs))).
%generate_pdg(_TableNodes,_ViewNodes,_NbrViews,_MaxDepth,_MaxChildren,_).

% A triangular-shaped pdg:
distribute_viewnodes(NbrViews,MaxDepth,MaxChildren,[[Root]|Grid]) :-
  (NbrViews>=MaxDepth -> true ; write_error_log(['Not enough views.']),fail),
  MaxDepth1 is MaxDepth-1,
  length(Grid,MaxDepth1),
  NbrViews1 is NbrViews-MaxDepth,
  TotalViews is NbrViews-MaxDepth,
  distribute_level_viewnodes(NbrViews1,TotalViews,MaxDepth1,MaxChildren,2,1,Grid),
  exec_if_development_on(write_info_log([[[Root]|Grid]])).

distribute_level_viewnodes(NbrViews,_TotalViews,_MaxDepth,MaxChildren,_Level,NbrNodes,[LevelNodes]) :-
  Max is NbrNodes*MaxChildren,
  NbrViews1 is NbrViews+1,
  (NbrViews1>Max -> write_error_log(['Too many views.']),fail ; true),
  length(LevelNodes,NbrViews1).
distribute_level_viewnodes(NbrViews,TotalViews,MaxDepth,MaxChildren,Level,NbrNodes,[LevelNodes|Grid]) :-
  Max is NbrNodes*MaxChildren,
  PNbrLevelNodes is integer((TotalViews*2/MaxDepth/MaxDepth)*(Level-1))+1,
  (PNbrLevelNodes>Max -> NbrLevelNodes=Max ; NbrLevelNodes=PNbrLevelNodes),
  length(LevelNodes,NbrLevelNodes),
  Level1 is Level+1,
  NbrViews1 is NbrViews-NbrLevelNodes+1,
  distribute_level_viewnodes(NbrViews1,TotalViews,MaxDepth,MaxChildren,Level1,NbrLevelNodes,Grid).

connect_viewnodes(Grid,TableNodes,MaxChildren,Arcs) :-
  connect_adjacent_viewnodes(Grid,TableNodes,MaxChildren,[],T1Arcs), % Ensure pdg depth, including tables for leaves
  connect_arbitrary_viewnodes(Grid,MaxChildren,TableNodes,T1Arcs,T2Arcs),
  connect_childless_viewnodes(Grid,MaxChildren,TableNodes,T2Arcs,Arcs),
  !.
connect_viewnodes(_Grid,_TableNodes,_MaxChildren,_Arcs) :-
  write_error_log(['Error connecting nodes.']),
  fail.
  
connect_adjacent_viewnodes([],_TableNodes,_MaxChildren,Arcs,Arcs).
connect_adjacent_viewnodes([ParentNodes],TableNodes,MaxChildren,IArcs,OArcs) :- % Last view level
  connect_parent_child_nodes(ParentNodes,TableNodes,MaxChildren,IArcs,OArcs).
connect_adjacent_viewnodes([ParentNodes,ChildNodes|Levels],TableNodes,MaxChildren,IArcs,OArcs) :-
  connect_child_parent_nodes(ChildNodes,ParentNodes,MaxChildren,IArcs,I1Arcs),
  connect_adjacent_viewnodes([ChildNodes|Levels],TableNodes,MaxChildren,I1Arcs,OArcs).

connect_parent_child_nodes([],_ChildNodes,_MaxChildren,Arcs,Arcs).
connect_parent_child_nodes([ParentNode|ParentNodes],ChildNodes,MaxChildren,IArcs,OArcs) :-
  length(ChildNodes,Max),
  (Max>0 -> true ; write_error_log(['Cannot connect nodes.']),fail),
  get_random_integer(Max,Index),
  my_nth_member(ChildNode,Index,ChildNodes),
  connect_parent_child_nodes(ParentNodes,ChildNodes,MaxChildren,[ParentNode+ChildNode|IArcs],OArcs).
  
connect_child_parent_nodes([],_ParentNodes,_MaxChildren,Arcs,Arcs).
connect_child_parent_nodes([ChildNode|ChildNodes],ParentNodes,MaxChildren,IArcs,OArcs) :-
  possible_parents(ParentNodes,IArcs,MaxChildren,PossibleParentNodes),
  length(PossibleParentNodes,Max),
  (Max>0 -> true ; write_error_log(['Cannot connect nodes.']),fail),
  get_random_integer(Max,Index),
  my_nth_member(ParentNode,Index,PossibleParentNodes),
  connect_child_parent_nodes(ChildNodes,ParentNodes,MaxChildren,[ParentNode+ChildNode|IArcs],OArcs).
  
possible_parents([],_Arcs,_MaxChildren,[]).
possible_parents([ParentNode|ParentNodes],Arcs,MaxChildren,[ParentNode|PossibleNodes]) :-
  findall(ParentNode,member(ParentNode+_,Arcs),Nodes),
  length(Nodes,Count),
  Count<MaxChildren,
  !,
  possible_parents(ParentNodes,Arcs,MaxChildren,PossibleNodes).
possible_parents([_ParentNode|ParentNodes],Arcs,MaxChildren,PossibleNodes) :-
  possible_parents(ParentNodes,Arcs,MaxChildren,PossibleNodes).
  
connect_arbitrary_viewnodes([],_MaxChildren,_TableNodes,Arcs,Arcs).
connect_arbitrary_viewnodes([Level|Levels],MaxChildren,TableNodes,IArcs,OArcs) :-
  concat_lists([TableNodes|Levels],PossibleNodes),
  connect_arbitrary_viewnodes_list(Level,PossibleNodes,MaxChildren,IArcs,I1Arcs),
  connect_arbitrary_viewnodes(Levels,MaxChildren,TableNodes,I1Arcs,OArcs).

connect_arbitrary_viewnodes_list([],_PossibleNodes,_MaxChildren,Arcs,Arcs).
connect_arbitrary_viewnodes_list([Node|Nodes],PossibleNodes,MaxChildren,IArcs,OArcs) :-
  connect_arbitrary_viewnode(Node,PossibleNodes,MaxChildren,IArcs,I1Arcs),
  connect_arbitrary_viewnodes_list(Nodes,PossibleNodes,MaxChildren,I1Arcs,OArcs).

connect_arbitrary_viewnode(Node,PossibleNodes,MaxChildren,IArcs,OArcs) :-
  findall(Node,member(Node+_,IArcs),Nodes),
  length(Nodes,L),
  L<MaxChildren,
  !,
  Max is MaxChildren-L,
  get_random_integer(Max,ExtraChildren),
  connect_viewnode_random(Node,PossibleNodes,ExtraChildren,1,IArcs,OArcs).
connect_arbitrary_viewnode(_Node,_PossibleNodes,_MaxChildren,Arcs,Arcs).
  
connect_viewnode_random(_Node,[],_MaxChildren,_N,Arcs,Arcs) :- % Not enough remaining possible nodes
  !.
connect_viewnode_random(_Node,_PossibleNodes,MaxChildren,N,Arcs,Arcs) :-
  N>MaxChildren,
  !.
connect_viewnode_random(Node,PossibleNodes,MaxChildren,N,IArcs,OArcs) :-
  length(PossibleNodes,Max),
  get_random_integer(Max,Index),
  split_list(Index,PossibleNodes,L,[ChildNode|R]), % Don't use ChildNode again for the same parent
  append(L,R,NewPossibleNodes),
  N1 is N+1,
  connect_viewnode_random(Node,NewPossibleNodes,MaxChildren,N1,[Node+ChildNode|IArcs],OArcs).

connect_childless_viewnodes([],_MaxChildren,_TableNodes,Arcs,Arcs).
connect_childless_viewnodes([Level|Levels],MaxChildren,TableNodes,IArcs,OArcs) :-
  connect_childless_viewnodes_list(Level,TableNodes,MaxChildren,IArcs,I1Arcs),
  connect_childless_viewnodes(Levels,MaxChildren,TableNodes,I1Arcs,OArcs).

connect_childless_viewnodes_list([],_TableNodes,_MaxChildren,Arcs,Arcs).
connect_childless_viewnodes_list([Node|Nodes],TableNodes,MaxChildren,IArcs,OArcs) :-
  connect_childless_viewnode(Node,TableNodes,MaxChildren,IArcs,I1Arcs),
  connect_childless_viewnodes_list(Nodes,TableNodes,MaxChildren,I1Arcs,OArcs).

connect_childless_viewnode(Node,_TableNodes,_MaxChildren,Arcs,Arcs) :-
  memberchk(Node+_,Arcs),
  !.
connect_childless_viewnode(Node,TableNodes,MaxChildren,IArcs,OArcs) :-
  Max1 is MaxChildren-1,
  get_random_integer(Max1,N),
  NbrChildren is N+1,
  connect_childless_viewnode_random(Node,TableNodes,MaxChildren,NbrChildren,IArcs,OArcs).
  
connect_childless_viewnode_random(_Node,[],_MaxChildren,_N,_IArcs,_OArcs) :-
  write_error_log(['Not enough tables.']),
  fail.
connect_childless_viewnode_random(_Node,_TableNodes,_MaxChildren,0,Arcs,Arcs) :-
  !.
connect_childless_viewnode_random(Node,TableNodes,MaxChildren,N,IArcs,OArcs) :-
  length(TableNodes,Max),
  get_random_integer(Max,Index),
  split_list(Index,TableNodes,L,[ChildNode|R]), % Don't use ChildNode again for the same parent
  append(L,R,NewTableNodes),
  N1 is N-1,
  connect_childless_viewnode_random(Node,NewTableNodes,MaxChildren,N1,[Node+ChildNode|IArcs],OArcs).


connect_nonused_tablenodes(TableNodes,ViewNodes,MaxChildren,IArcs,OArcs) :-
  dependent_nodes_list(ViewNodes,IArcs,[],DepNodes),
  my_set_diff(TableNodes,DepNodes,UnconnectedTableNodes),
  possible_parents(ViewNodes,IArcs,MaxChildren,PossibleParentNodes),
  connect_child_parent_nodes(UnconnectedTableNodes,PossibleParentNodes,MaxChildren,IArcs,OArcs).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate Views
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_views(ViewNodes,PDG,TableSize,Views) :-
  generate_view_list(ViewNodes,PDG,TableSize,Views),
%  write_log_list([Views]),
  exec_if_development_on(display_sql_list(Views,2)).

generate_view_list([],_PDG,_,[]).
generate_view_list([ViewNode|ViewNodes],PDG,TableSize,[View|Views]) :-
  generate_view(ViewNode,PDG,TableSize,View),
  generate_view_list(ViewNodes,PDG,TableSize,Views).

generate_view(View/Arity,PDG,TableSize,create_view(sql,SQLst,Schema)) :-
%  select_weighted_random_list([(select,0.7),(intersect,0.15),(union,0.15)],Type),
  select_weighted_random_list([(select,1),(intersect,1),(except,1),(union,1)],Type),
%   select_random_list([select],Type),
  gen_typed_schema(View,Arity,Schema),
  generate_DQL_type_stmt(Type,View/Arity,PDG,TableSize,SQLst).
  
generate_DQL_type_stmt(intersect,View/Arity,PDG,TableSize,intersect(DistinctAll,(SQLstL,_),(SQLstR,_))) :-
  select_random_list([distinct,all],DistinctAll),
  generate_split_pdg(View/Arity,PDG,PDGL,PDGR),
  generate_DQL_type_stmt(select,View/Arity,PDGL,TableSize,SQLstL),
  generate_DQL_type_stmt(select,View/Arity,PDGR,TableSize,SQLstR).
generate_DQL_type_stmt(except,View/Arity,PDG,TableSize,except(DistinctAll,(SQLstL,_),(SQLstR,_))) :-
  select_random_list([distinct,all],DistinctAll),
  generate_split_pdg(View/Arity,PDG,PDGL,PDGR),
  generate_DQL_type_stmt(select,View/Arity,PDGL,TableSize,SQLstL),
  generate_DQL_type_stmt(select,View/Arity,PDGR,TableSize,SQLstR).
generate_DQL_type_stmt(union,View/Arity,PDG,TableSize,union(DistinctAll,(SQLstL,_),(SQLstR,_))) :-
  select_random_list([distinct,all],DistinctAll),
  generate_split_pdg(View/Arity,PDG,PDGL,PDGR),
  generate_DQL_type_stmt(select,View/Arity,PDGL,TableSize,SQLstL),
  generate_DQL_type_stmt(select,View/Arity,PDGR,TableSize,SQLstR).
generate_DQL_type_stmt(select,View/Arity,(_Nodes,Arcs),TableSize,select(DistinctAll,top(all),Exprs,from(Rs),where(Cs),group_by([]),having(true),order_by([],[]))) :-
  select_random_list([distinct,all],DistinctAll),
  dependent_nodes(View/Arity,Arcs,DepNodes),
  my_unzip(DepNodes,Names,_As),
  gen_column_names(Arity,Columns),
  generate_proj_list(Names,Columns,Arity,0,Exprs),
  generate_from_list(Names,URs),
  my_remove_duplicates_sort(URs,Rs), % WARNING: If duplicates are allowed, use renamings instead
  generate_where_condition(Names,TableSize,Cs).
  
generate_split_pdg(Node,(_,Arcs),(LNodes,LArcs),(RNodes,RArcs)) :-
  dependent_nodes(Node,Arcs,DepNodes),
  length(DepNodes,Max),
  Max1 is Max-1,
  Max1>0,
  !,
  get_random_integer(Max1,Index),
  Index1 is Index+1,
  split_list(Index1,DepNodes,LNodes,RNodes),
  length(LNodes,LL),
  length(LNodes,RL),
  length(LTos,LL),
  length(RTos,RL),
  my_map_1(=(Node),LTos),
  my_map_1(=(Node),RTos),
  my_zipWith('+',LTos,LNodes,LArcs),
  my_zipWith('+',RTos,RNodes,RArcs).
generate_split_pdg(_Node,PDG,PDG,PDG). % Only one dependent node

generate_proj_list(_Names,_Columns,Arity,Arity,[]).
generate_proj_list(Names,Columns,Arity,N,[attr(Name,Column,_)|Exprs]) :-
  select_random_list(Names,Name),
  select_random_list(Columns,Column),
  N1 is N+1,
  generate_proj_list(Names,Columns,Arity,N1,Exprs).
  
generate_from_list(Names,Rs) :-
  my_remove_duplicates(Names,UNames),
  length(UNames,L),
  length(ASs,L),
  my_zipWith(',',UNames,ASs,VRs),
  table_or_view_in_from_clause_list(VRs,Rs).

table_or_view_in_from_clause_list([],[]).
table_or_view_in_from_clause_list([VR|VRs],[R|Rs]) :-
  table_or_view_in_from_clause(VR,R),
  table_or_view_in_from_clause_list(VRs,Rs).

table_or_view_in_from_clause((T,[RR|RArgs]),(T,[RR|RArgs])) :-
  atom(T),
  !,
  check_arguments_renaming(T,RArgs),
  relation_autorenaming(RR),
  gen_table_column_names(T,Args),
  build_ren_arguments(T,Args,RArgs),
  arguments_autorenaming(RR,RArgs).
table_or_view_in_from_clause(T,T).

dependent_nodes_list([],_Arcs,Nodes,Nodes).
dependent_nodes_list([Node|Nodes],Arcs,DepNodesI,DepNodesO) :-
  dependent_nodes(Node,Arcs,DepNodesT),
  append(DepNodesI,DepNodesT,DepNodesI1),
  dependent_nodes_list(Nodes,Arcs,DepNodesI1,DepNodesO).

dependent_nodes(ViewNode,Arcs,DepNodes) :-
  findall(DepNode,member(ViewNode+DepNode,Arcs),DepNodes).
   
generate_where_condition(Names,TableSize,Cond) :-
  generate_correlate_condition_list(Names,C1s),
  generate_random_basic_condition(Names,TableSize,C2s),
  yfx_connect_with(C1s,and,C1),
  yfx_random_connect(C2s,[and,or],C2),
  simplify_cond(and(C1,C2),Cond).
  
yfx_random_connect([X],_L,X).
yfx_random_connect([X1,X2|Xs],L,CX1X2Xs) :-
  select_random_list(L,Op),
  CX1X2=..[Op,X1,X2],
  yfx_random_connect([CX1X2|Xs],L,CX1X2Xs).

generate_correlate_condition_list([],[true]).
generate_correlate_condition_list([_],[true]).
generate_correlate_condition_list([R1,R2|Rs],[attr(R1,C1,_)=attr(R2,C2,_)|Cs]) :-
  my_odbc_identifier_name(a,C1),
  my_odbc_identifier_name(b,C2),
  generate_correlate_condition_list([R2|Rs],Cs).
  
generate_random_basic_condition([],_TableSize,[]).
generate_random_basic_condition([RelName|RelNames],TableSize,[C|Cs]) :-
  gen_number_of_table_columns(NbrColumns),
  gen_column_names(NbrColumns,Columns),
  select_random_list(Columns,Column),
  get_random_integer(TableSize,Value),
  select_random_list(['=','>','<','>=','<='],Op),
  C=..[Op,attr(RelName,Column,_),Value],
  generate_random_basic_condition(RelNames,TableSize,Cs).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate File
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_file(FileName,Tables,Inserts,Views) :-
  generate_file(FileName,Tables,Inserts,Views,with_tables,with_commands).
  
generate_file(FileName,Tables,Inserts,Views,WithTables,WithCommands) :-
  push_flag(output,only_to_log,OutputFlag),
  push_flag(pretty_print,off,PPFlag),
  disable_log(Logs),
  processC(log,[write,silent,FileName],[],yes),
  (WithCommands==with_commands
   ->
%    write_log_list(['/multiline on',nl,'/abolish',nl,'/drop_all_relations',nl,'/output off',nl,nl])
    write_log_list(['/multiline on',nl,'/abolish',nl,'/drop_all_relations',nl,'/output on',nl,nl])
   ;
    true),
  (WithTables==with_tables
   ->
    display_sql_list(Tables,0),
    display_sql_list(Inserts,0)
   ;
    true),
  display_sql_list(Views,0),
  (WithCommands==with_commands
   ->
    write_log_list(['/output on',nl])
   ;
    true),
  processC(nolog,[FileName],[],yes),
  resume_log(Logs),
  pop_flag(pretty_print,PPFlag),
  pop_flag(output,OutputFlag).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Process SQL File (local database)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_gen_file(FileName) :-
  write_info_log(['Creating the database.']),
  processC(process,[FileName],_NVs,yes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ensure that each view outcome is not empty
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ensure_non_empty_views([],[],[]).
ensure_non_empty_views([N/_A|ViewNodes],[V|Views],[V|AViews]) :-
  relation_cardinality(N,Cardinality),
  write_info_log(['Checking cardinality of ',N,': ',Cardinality,'$tbc']),
  Cardinality>0,
  nl_log,
  !,
  ensure_non_empty_views(ViewNodes,Views,AViews).
ensure_non_empty_views([N/A|ViewNodes],[create_view(sql,SQL,Schema)|Views],[create_view(sql,ASQL,Schema)|AViews]) :-
%(N=v5->deb;true),
  enlarge_query_results(N/A,2,SQL,ASQL), % Two tries
  !,
  create_or_replace_view((ASQL,Schema)),
%   drop_view_k(N,no_warn),
%   create_view_k(sql,(ASQL,Schema),Schema,[]),
%   clear_et,
%   compute_stratification,
%   solve_des_sql_query(sql,drop_view(N,false)),
%   solve_des_sql_query(sql,create_view(sql,(ASQL,_),Schema)),
  ensure_non_empty_views(ViewNodes,Views,AViews).
ensure_non_empty_views([ViewNode|_ViewNodes],_View,_AViews) :-
  write_error_log([nl,'No way to get a non-empty outcome for view ',ViewNode,'.']),
  !,
  fail.
  
% A couple of opportunities for adjusting the query
enlarge_query_results(N/A,T,Query,MQuery) :-
  modify_query_enlarge(Query,AQuery),
  EQuery=select(all,top(all),*,from([(AQuery,_)]),where(true),group_by([]),having(true),order_by([],[])), % AQuery must be encapsulated in a SELECT * FROM (e.g., for a union)
  CQuery=select(all,top(all),[expr(count,_,_)],from([(EQuery,_)]),where(true),group_by([]),having(true),order_by([],[])),
  get_tuples_from_SQL_query(CQuery,[answer(Cardinality)]),
  write_log_list(['..',Cardinality]),
  ((Cardinality>0 ; T=1)
   ->
    MQuery=AQuery,
    nl_log
   ;
    T1 is T-1,
    enlarge_query_results(N/A,T1,AQuery,MQuery)).

modify_query_enlarge((Query,_),AQuery) :-
  modify_query_enlarge(Query,AQuery).
modify_query_enlarge(
            select(DistinctAll,Top,Exprs,from(Rs),where(Cs),group_by([]),having(true),order_by([],[])),
            select(DistinctAll,Top,Exprs,from(Rs),where(ACs),group_by([]),having(true),order_by([],[]))) :-
  modify_where_cond_enlarge(Cs,ACs).
modify_query_enlarge(
            union(DistinctAll,(SQL1,Schema1),(SQL2,Schema2)),
            union(DistinctAll,(ASQL1,Schema1),(SQL2,Schema2))) :-
  !,
  modify_query_enlarge(SQL1,ASQL1).
modify_query_enlarge(
            union(DistinctAll,(SQL1,Schema1),(SQL2,Schema2)),
            union(DistinctAll,(SQL1,Schema1),(ASQL2,Schema2))) :-
  !,
  modify_query_enlarge(SQL2,ASQL2).
modify_query_enlarge(
            intersect(DistinctAll,SQL1,SQL2),
            union(DistinctAll,SQL1,SQL2)) :-
  !.
modify_query_enlarge(
            except(DistinctAll,SQL1,SQL2),
            union(DistinctAll,SQL1,SQL2)) :-
  !.
modify_query_enlarge(
            union(DistinctAll,SQL1,SQL2),
            union(DistinctAll,SQL1,SQL2)) :-
  !,
  fail.
    
modify_where_cond_enlarge(and(C1s,C2s),and(C1s,AC2s)) :-
  remove_inner_and(C2s,AC2s),
  !.
modify_where_cond_enlarge(_C,true).

  
remove_inner_and(and(C1,C2),C1) :-
  sql_operand(C1),
  sql_operand(C2),
  !.
remove_inner_and(and(C1,C2),and(C,C2)) :-
  remove_inner_and(C1,C),
  !.
remove_inner_and(and(C1,C2),and(C1,C)) :-
  remove_inner_and(C2,C),
  !.
remove_inner_and(or(C1,C2),or(C,C2)) :-
  remove_inner_and(C1,C),
  !.
remove_inner_and(or(C1,C2),or(C1,C)) :-
  remove_inner_and(C2,C),
  !.
  
sql_operand(O) :-
  functor(O,Op,2),
  Op\=and,
  Op\=or,
  Op\=not.
  
  
create_or_replace_view((SQL,Schema)) :-
  current_db('$des'),
  !,
  functor(Schema,ViewName,_),
  drop_view_k(ViewName,no_warn),
  create_view_k(sql,(SQL,Schema),Schema,[]),
  clear_et,
  compute_stratification.
create_or_replace_view((SQL,Schema)) :-
  current_db(Connection,DBMS),
  memberchk(DBMS,[mysql,postgresql]),
  !,
  display_to_string(display_sql(create_or_replace_view(sql,(SQL,Schema),Schema),0),QueryStr),
  solve_rdb_sql_query(Connection,QueryStr,create_or_replace_view(sql,(SQL,Schema),Schema)).
create_or_replace_view((SQL,Schema)) :-
  current_db(Connection),
  functor(Schema,ViewName,_),
  drop_rdb_view_if_exists(Connection,ViewName),
  display_to_string(display_sql(create_view(sql,(SQL,Schema),Schema),0),QueryStr),
  solve_rdb_sql_query(Connection,QueryStr,create_view(sql,(SQL,Schema),Schema)).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Pseudorandom number generator for 64 bit integer numbers
%% Based on Xorshift RNGs
%% http://www.jstatsoft.org/v08/i14/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

int_random_gen_list(0,_Max,[]).
int_random_gen_list(L,Max,[Value|Values]) :-
  L>0,
  L1 is L-1,
  get_random_integer(Max,Value),
  int_random_gen_list(L1,Max,Values).

set_fixed_random_seed :-
  set_random_seed(1785770).

set_random_seed :-
  get_random_seed(Seed),
  set_random_seed(Seed).
  
:-dynamic(random_seed/1).
% random_seed(9832794817623948). % Set at start-up with set_random_seed

set_random_seed(Seed) :-
  set_flag(random_seed(Seed)).

get_random_seed(Seed) :-
  my_datetime((_Y,_M,D,H,Mi,S)),
  Seed is D*24*60*60+H*60*60+Mi*60+S.
  
random_max(Max) :-
  Max is integer(2**64)-1.

get_random(Rnd) :-
  random_seed(X0),
  random_max(Max),
  X1 is X0<<21,
% SICStus 4.2.3 does not feature xor/2 in is/2
%  my_limited_xor(X1,X0,Max,X2),
  X2 is xor(X1,X0) /\ Max,
  X3 is X2>>35,
%  my_limited_xor(X3,X2,Max,X4),
  X4 is xor(X3,X2) /\ Max,
  X5 is X4<<4,
%  my_limited_xor(X5,X4,Max,Rnd),
  Rnd is xor(X5,X4) /\ Max,
  set_random_seed(Rnd).
   
% Integer \in 0..Size-1
get_random_integer(Size,Integer) :-
  get_random(Rnd),
  random_max(Max),
  Integer is truncate(Rnd*Size/Max).
  
% Randomly select a member of the list
select_random_list(Xs,X) :-
  select_random_list(Xs,_N,X).
  
select_random_list(Xs,N,X) :-
  length(Xs,Size),
  get_random_integer(Size,N),
  my_nth_member(X,N,Xs).
  
% Randomly select a member of the list, with weights for each member
% e.g., select_weighted_random_list([(a,3),(b,2),(c,4),(d,1)],X). Weights do not need to be normalized
select_weighted_random_list(Xs,X) :-
  select_weighted_random_list(Xs,void,-1,X).
  
select_weighted_random_list([],X,_,X).
select_weighted_random_list([(X,W)|Ps],_LX,LN,Y) :-
  get_random_integer(10000000,R),
  N is R*W,
  N>LN,
  !,
  select_weighted_random_list(Ps,X,N,Y).
select_weighted_random_list([_P|Ps],LX,LN,Y) :-
  select_weighted_random_list(Ps,LX,LN,Y).
  
% Equiprobability Test:
% i(99),p(1000000),listing(rnd).
% :-dynamic(rnd/2).
% i(-1).
% i(N) :-
%   N > -1,
%   set_p_flag(rnd(N),0),
%   N1 is N-1,
%   i(N1).
%   
% p(0).
% p(N) :-
%   N>0,
%   get_random_integer(100,I),
%   rnd(I,D),
%   D1 is D+1,
%   set_p_flag(rnd(I),D1),
%   N1 is N-1,
%   p(N1).  
  
% p(X):-
%   set_random_seed(X),
%   select_random_list([a,b],_),
%   !,
%   X1 is X+1,
%   p(X1).
% p(X) :-
%   nl, write(X).