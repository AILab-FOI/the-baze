/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.4.1                 */
/*                                                       */
/*    Persistent Predicates                              */
/*                                                       */
/*                                                       */
/*  Includes an adaptation of Draxler's pl2sql compiler: */
/*            (C) Copyright by Christoph Draxler, Munich */
/*                Version 1.1 of Dec. 21st 1992          */
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

% Flags:
% my_persistent(ODBC_Name,PredicateSchema,DependentNodes)
% Ex: my_persistent(mysql,p(a:int))
%
% datalog_persistent(Predicate,PersistentRules,LocalPersistentDLRules)
%   PersistentDLRules
% Ex: datalog_persistent(u/1, 
%                       [datalog((p(A):-q(A),r(A)),['X'=A],4,[],asserted((2012,4,13,13,12,22)),source)], 
%                       [datalog((p(B):-v(B)),[],0,[],asserted((2012,4,13,10,38,41)),source)]).
% Here, p(B):-v(B) is kept in the local database as this rule belongs to a recursive cycle
% (e.g., there exists the rule v(X):-p(X))

my_persistent(Connection,PredSchema) :-
  my_persistent(Connection,PredSchema,_DepNodes).

%%%%%%%%%%%%%%%%%%%% 
% DROP ASSERTION
%%%%%%%%%%%%%%%%%%%% 

drop_persistent_assertion(persistent(PredSchema,Connection)) :-
  \+ my_persistent(Connection,PredSchema),
  !,
  ir_to_dl_pred_schema(PredSchema,DLPredSchema),
  write_warning_log(['Predicate schema ',DLPredSchema,' has not been made persistent in connection ''',Connection,'''. ']),
  display_persistent_predicate_schema_alternatives(Connection,PredSchema).
drop_persistent_assertion(persistent(PredSchema,Connection)) :-
  process_drop_persistent_assertion(persistent(PredSchema,Connection)),
  compute_stratification.
  
process_drop_persistent_assertion(persistent(PredSchema,Connection)) :-
  drop_flag(my_persistent(Connection,PredSchema,DepNodes)),
  unpersist_pred(Connection,PredSchema),
%  compute_stratification,
  drop_dependent_persistent_assertions(DepNodes),
  write_info_verb_log(['Assertion :-persistent(',PredSchema,') dropped']).
  
  
drop_dependent_persistent_assertions([]).
drop_dependent_persistent_assertions([Name/Arity|DepNodes]) :-
  functor(PredSchema,Name,Arity),
  my_persistent(Connection,PredSchema,NADepNodes),
  !,
  process_drop_persistent_assertion(persistent(PredSchema,Connection)),
  drop_dependent_persistent_assertions(NADepNodes),
  drop_dependent_persistent_assertions(DepNodes).
drop_dependent_persistent_assertions([_Node|DepNodes]) :-
  % Already dropped
  drop_dependent_persistent_assertions(DepNodes).

display_persistent_predicate_schema_alternatives(Connection,PredSchema) :-
  functor(PredSchema,Name,Arity),
  functor(PS,Name,Arity),
  findall(PS,my_persistent(Connection,PS),PredSchemas),
  (PredSchemas == []
   ->
    true
   ;
    ir_to_dl_pred_schema_list(PredSchemas,DLPredSchemas),
    write_info_log(['Alternatives: ',DLPredSchemas])).
  
ir_to_dl_pred_schema_list([],[]).
ir_to_dl_pred_schema_list([PredSchema|PredSchemas],[DLPredSchema|DLPredSchemas]) :-
  ir_to_dl_pred_schema(PredSchema,DLPredSchema),
  ir_to_dl_pred_schema_list(PredSchemas,DLPredSchemas).    

ir_to_dl_pred_schema(PredSchema,DLPredSchema) :-
  PredSchema=..[P|CTs],
  my_unzip(CTs,Cs,Ts),
  type_equivalence_list(Ts,DLs,_),
  my_zipWith(':',Cs,DLs,CDLs),
  DLPredSchema=..[P|CDLs].
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROCESSING ASSERTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_persistent_datalog_assertion(persistent(_PredSchema,'$des'),_Ls,_FId,_Action,_Error) :-
  !,
  write_error_log(['Default database cannot be used to persist predicates. Open or select an ODBC connection instead.']).
process_persistent_datalog_assertion(persistent(PredSchema,_Connection),_Ls,_FId,_Action,true) :-
  pred_schema(Pred,PredSchema),
  is_persistent_predicate(Pred),
  !,
  my_persistent(Connection,PredSchema),
  write_error_log(['Predicate already made persistent in connection ''',Connection,'''']).
process_persistent_datalog_assertion(persistent(PredSchema,Connection),Ls,FId,Action,Error) :-
  \+ opened_db(Connection),
  !,
  % Try to open the connection as it is not opened already
%  processC(open_db,[Connection],_NVs,yes),
  my_open_odbc(Connection,[]),
  (opened_db(Connection)
   ->
    enable_rdb_datasource(Connection),
    process_persistent_datalog_assertion(persistent(PredSchema,Connection),Ls,FId,Action,Error)
   ;
    true
  ).
%  write_error_log(['ODBC connection ',Connection,' is not opened.']).
process_persistent_datalog_assertion(persistent(PredSchema,Connection),_Ls,_FId,_Action,_Error) :-
  persist_pred(Connection,PredSchema,Error),
  (var(Error)
   ->
    processC(clear_et,[],[],yes),
    compute_stratification,
    write_info_verb_log(['Assertion successfully processed.'])
   ;
    write_error_log(['Trying to make ',PredSchema,' persistent'])
  ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLOSING PERSISTENCE CONNECTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% close_single_persistent closes the single connection to a persistent predicate
close_single_persistent :-
  my_nf_setof(PredName,Connection^Arity^PredSchema^(my_persistent(Connection,PredSchema),functor(PredSchema,PredName,Arity)),PredNames),
  close_single_persistent(PredNames).
  
close_single_persistent([]) :-
  write_error_log(['No connection to persistent predicate found.']).
close_single_persistent([Name]) :-
  close_persistent(Name).
close_single_persistent([N1,N2|Ns]) :-
  write_error_log(['Multiple connections to persistent predicates (',[N1,N2|Ns],'). Select one and issue this command followed by the predicate name.',nl]).

% close_persistent(+N), where N is a predicate name
close_persistent(N) :-
  (is_persistent_predicate(N/A)
   ->
    functor(PredSchema,N,A),
    drop_flag(my_persistent(_Connection,PredSchema,_DepNodes)),
    retract(datalog_persistent(N/A,_RDLs,UPDLs)),
    retract_dlrule_list(UPDLs,_Error),
    retract((my_view('$des',N,A,_,_,_,_,_,_):-true)),
    clear_et,
    write_info_verb_log(['Persistent predicate connection closed.'])
   ;
    write_error_log(['Predicate ''',N,''' has not been made persistent yet.'])
  ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATING/RESTORING via :-persistent(Predicate)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% persist_pred(+PredSchema,-Error) makes Pred (with schema Predname(ColName1,...,ColNameN)) to persist
% on an external data manager (RDBs, XLS, CSVs, ...) connected through ODBC, typically a
% relational database management system
% This is called from processing an assertion of either the form:
% :-persistent(PredName/Arity[,Connection]) 
% or:
% :-persistent(Predname(ColName1,...,ColNameN)[,Connection]) 
% where Connection is optional (the current connection is used if no connection is provided)
%   
% persist_pred(+PredSchema,-Error)
persist_pred(PredSchema,Error) :-
  current_db(Connection),
  persist_pred(Connection,PredSchema,Error).
  

% persist_pred(+Connection,+PredSchema,-Error)
% Similar to persist_pred(+PredSchema,-Error) but referring to a concrete connection.

% To make a predicate to persist, all of its rules are collected and translated into a view definition
% as the union of each rule's SQL translation. Its facts are stored in a table, and the view contains
% this table as data source.
% This view is created through the ODBC connection and is made available as the data source for the 
% predicate, instead of its defining Datalog rules
% Another table is created to keep metadata information about the original Datalog rules. This allows
% to remove persistency from a predicate, restoring its original rules.
% Making a predicate persistent means that all predicates in the sub PDG for this predicate
% should be typed and known to the external database: either they are made persistent or exist already as 
% tables or views. If any of these predicates are not known, then they are made persistent automatically.

% FIRST case: RESTORING persistency. A predicate that has been made persistent in a previous session
%   - Connect to the external data source
%   - Update external data with in-memory rules and facts
persist_pred(Connection,PredSchema,Error) :-
  functor(PredSchema,RelName,_Arity),
%  Node = RelName/Arity,
  % Check whether the relation is persistent already
  check_persistent_relation(Connection,RelName,Error),
  !,
  recover_persist_pred(Connection,PredSchema,Error).
% SECOND case: CREATING persistency. A predicate is going to be made persistent for the first time
% All of its rules and dependant predicates are made persistent unless they exist already in the external DB
persist_pred(Connection,PredSchema,Error) :-
  update_persistent_pred(Connection,PredSchema,Error).
persist_pred(_Connection,_PredSchema,true).

recover_persist_pred(_Connection,_PredSchema,Error) :-
  Error == true,
  !.
recover_persist_pred(Connection,PredSchema,Error) :-
  once(check_persistent_types(Connection,PredSchema,Error)),
  Error == true,
  !.
recover_persist_pred(Connection,PredSchema,Error) :-
  assert_types_from_schema(PredSchema,_Asserted),
  functor(PredSchema,RelName,_Arity),
  write_info_log(['Recovering existing metadata from external database for ''',RelName,'''...']),
  load_metadata(Connection,RelName),
%   delimited_sql_identifier(Connection,RelName,StrDelimitedTableName),
%   append("SELECT * FROM ",StrDelimitedTableName,SQLStr),
%   build_my_view_assertion_list([RelName/Arity],[SQLStr],_,[MyView]),
%   assertz(MyView),
  update_persistent_pred(Connection,PredSchema,Error).

update_persistent_pred(Connection,PredSchema,_Error) :-
  % PPNodes are the predicates which Node depends on
  check_persist_pred(Connection,PredSchema,Node,PPNodes,ExtraTypes),
  !,
  % Restore predicates in PPNodes which are persistent already
  % NYPNodes are those PPNodes that have been not made persistent and need to be
  pred_schema(Pred,PredSchema),
  assertz(datalog_persistent(Pred,[],[])), % WARNING: Workaround for mutually recursive predicates
  restore_already_persistent_predicates(Connection,PPNodes,NYPNodes),
  retractall(datalog_persistent(Pred,[],[])),
  Nodes = [Node|NYPNodes],
  PredSchema =.. [Name|ColnameTypes],
  setof(Type,member(Type,[my_types('$des',Name,ColnameTypes)|ExtraTypes]),Types),
  predicate_types(PredSchema,Types,FExtraTypes),
  assert_my_types_list(FExtraTypes),
  sort_by_topological_order(Nodes,SNodes),
  translate_non_persistent_predicates(Connection,SNodes,NPNodes,DLRulesList,DLFactsList,UPDLRulesList,RPDLRulesList,SQLCreateViews,SQLInsertTablesList),
  my_unzip(NPNodes,ViewNames,_),
  my_reverse(ViewNames,RViewNames),
  drop_rdb_view_if_exists_list(Connection,RViewNames),
  compose_create_table_statements(Connection,NPNodes,SQLCreateTables),
  compose_create_metadata_table_statements(Connection,NPNodes,SQLCreateMetadataTables),
  assert_my_persistent_assertion_list(Connection,Node,NPNodes,FExtraTypes),
  build_my_view_assertion_list(NPNodes,SQLCreateViews,RPDLRulesList,MyViews),
  exec_and_show_rdb_sql_list(Connection,SQLCreateTables),
  exec_and_show_rdb_sql_list(Connection,SQLCreateMetadataTables),
  exec_and_show_rdb_sql_list(Connection,SQLCreateViews),
  exec_and_show_rdb_sql_list(Connection,SQLInsertTablesList),
  retract_predicate_view_list(NPNodes),
  assertz_list(MyViews),
  concat_lists(UPDLRulesList,UPDLRules),
  assertz_list(UPDLRules),
  change_datalog_rules_to_persistent(Connection,NPNodes,DLRulesList,DLFactsList,UPDLRulesList,RPDLRulesList).
update_persistent_pred(_Connection,_PredSchema,true).

sort_by_topological_order(Nodes,SNodes) :-
  pdg((PNodes,Arcs)),
  my_set_union(Nodes,PNodes,AllNodes), % Ensure to include all given nodes, even if they are not in the PDG
  topological_order((AllNodes,Arcs),OrderedNodes),
  filter(OrderedNodes,Nodes,SNodes).
  
% 
restore_already_persistent_predicates(_Connection,[],[]).
restore_already_persistent_predicates(Connection,[Name/Arity|PPNodes],Nodes) :-
  % If already persistent in the local database, do nothing
  is_persistent_predicate(Name/Arity),
  !,
  restore_already_persistent_predicates(Connection,PPNodes,Nodes).
restore_already_persistent_predicates(Connection,[Name/_Arity|PPNodes],Nodes) :-
%  check_persistent_relation(Connection,Name,_Error1),
  persistent_relation_exists(Connection,Name),
  !,
  get_table_types(Connection,Name,Types),
  type_equivalence_list(ITypes,_,Types),
  get_table_untyped_arguments(Connection,Name,Colnames),
  my_zipWith(':',Colnames,ITypes,ColnameTypes),
  PredSchema =.. [Name|ColnameTypes],
  recover_persist_pred(Connection,PredSchema,_Error2),
  restore_already_persistent_predicates(Connection,PPNodes,Nodes).
restore_already_persistent_predicates(Connection,[Name/_Arity|PPNodes],Nodes) :-
  % If it is an external relation, do nothing
  relation_exists(Connection,Name),
  !,
  restore_already_persistent_predicates(Connection,PPNodes,Nodes).
restore_already_persistent_predicates(Connection,[Node/Arity|PPNodes],[Node/Arity|Nodes]) :-
%  write_warning_log(['Relation ',Node/Arity,' is referenced from a persistent predicate but it is not defined.']),
  restore_already_persistent_predicates(Connection,PPNodes,Nodes).


% Check whether persisting a predicate is allowed:
% - Known schema for a new persistent predicate
% - Used relations are known in the external database
%   (either they are persistent already or exists as external relations)
% - Types are correct
% check_persist_pred(+Connection,+PredSchema,-Node,-Nodes,-ExtraTypes) 
% Node is the Node corresponding to PredSchema
% Return the nodes in the sub-PDG for Pred (but Pred itself), and all the extra
% inferred types of predicates which Pred depends on
check_persist_pred(Connection,PredSchema,Node,Nodes,ExtraTypes) :-
  check_existing_pred_schema(PredSchema),
  functor(PredSchema,Name,Arity),
  Node = Name/Arity,
%  sub_pdg(Name/Arity,PDG),
% %  check_non_recursive(PDG),
%  PDG=(AllNodes,_Arcs),
  % If a rule is being asserted, the PDG is not computed yet
  length(Args,Arity),
  Atom=..[Name|Args],
  reachable_predicates_atom(Atom,program,[],AllNodes),
  remove_one_element_if_exists_from_list(Name/Arity,AllNodes,TNodes),
  remove_builtin_preds(TNodes,Nodes),
  my_unzip(Nodes,Names,_Arities),
  setof(NName,member(NName,[Name|Names]),NNames),
  check_persist_relations(Connection,NNames),
  assert_types_from_schema(PredSchema,Asserted),
  (infer_dl_types(Connection,Name/Arity,InferredTypes,CExtraTypes)
   ->
    true
   ;
    write_error_log(['Type errors. Check existing rules']),
    !,
    fail
  ),
  close_my_types(CExtraTypes,ExtraTypes),
  retract_pred_types(Name/Arity,Asserted),
  schema_to_types(PredSchema,DeclaredTypes),
  PredSchema=..[Name|_ColnameTypes],
  check_persist_typing(Name,DeclaredTypes,InferredTypes),
  check_ground_types(ExtraTypes).

remove_builtin_preds([],[]).  
remove_builtin_preds([P|Ps],Qs) :-
  my_builtin_pred(P),
  !,
  remove_builtin_preds(Ps,Qs).
remove_builtin_preds([P|Ps],[P|Qs]) :-
  remove_builtin_preds(Ps,Qs).
  
% check_non_recursive(PDG) :-
%   recursive_predicates_from_pdg(PDG,RNAs),
%   (RNAs == []
%    ->
%     true
%    ;
%     write_warning_log(['Recursive rules cannot be made persistent for predicates: ',RNAs])
%   ).
  
check_ground_types(Types) :-
  ground(Types),
  !.
check_ground_types(Types) :-
  filter_pred(nonground,Types,NGTypes),
  write_error_log(['Unable to infer types for: ',NGTypes,nl,'$tab'(7),'Assertion rejected.']),
  !,
  fail.
  
nonground(X) :-
  \+ ground(X).
  
filter_pred(_Pred,[],[]).
filter_pred(Pred,[X|Xs],[X|Ys]) :-
  Goal =.. [Pred,X],
  call(Goal),
  !,
  filter_pred(Pred,Xs,Ys).
filter_pred(Pred,[_X|Xs],Ys) :-
  filter_pred(Pred,Xs,Ys).

compose_create_metadata_table_statements(_Connection,[],[]).
compose_create_metadata_table_statements(Connection,[Name/_Arity|Nodes],SQLs) :-
  persistent_metadata_table_name(Name,MDTableName),
  relation_exists(Connection,MDTableName),
  !,
  compose_create_metadata_table_statements(Connection,Nodes,SQLs).
compose_create_metadata_table_statements(Connection,[Name/_Arity|Nodes],[SQL|SQLs]) :-
  persistent_metadata_table_name(Name,MDTableName),
  delimited_sql_identifier(Connection,MDTableName,StrMDTableName),
%  concat_lists(["CREATE TABLE ",StrMDTableName,"(rule TEXT)"],SQL),
  build_rule_sql_type(Connection,StrType),
%  concat_lists(["CREATE TABLE ",StrMDTableName,"(txtrule ",StrType," NVs ",StrType,")"],SQL),
  concat_lists(["CREATE TABLE ",StrMDTableName,"(txtrule ",StrType,")"],SQL),
  compose_create_metadata_table_statements(Connection,Nodes,SQLs).

build_rule_sql_type(Connection,StrType) :-
  my_odbc_get_dbms(Connection,DBMS),
  (DBMS == access, StrType = "MEMO"
   ;
   StrType = "VARCHAR(2047)"
  ). 
% compose_insert_metadata_table_statements([],[],[]).
% compose_insert_metadata_table_statements([Name/_Arity|Nodes],[DLs|DLsList],[SQLs|SQLsList]) :-
%   compose_insert_metadata_table(Name,DLs,SQLs),
%   compose_insert_metadata_table_statements(Nodes,DLsList,SQLsList).
%   
% compose_insert_metadata_table(_Name,[],[]).
% compose_insert_metadata_table(Name,[DL|DLs],[SQL|SQLs]) :-
%   dlrule_to_ruleNVs_list([DL],[RNVs]),
%   my_term_to_string(RNVs,StrRNVs),
%   concat_lists([],SQL),
%   compose_insert_metadata_table(Name,DLs,SQLs).

  
% Check whether the relation is persistent already
check_persistent_relation(Connection,RelName,Error) :-
  persistent_relation_exists(Connection,RelName,RequiredRelations,ExistingRelations),
  (RequiredRelations == ExistingRelations 
   ->
    true
   ;
    (ExistingRelations == []
     ->
      fail
     ;
      write_error_log(['There exist required relations that are used already in the external media: ',ExistingRelations]),
      Error = true
    )
  ).
  
  
persistent_relation_exists(Connection,RelName) :-
  persistent_relation_exists(Connection,RelName,Relations,Relations).
  
persistent_relation_exists(Connection,RelName,RequiredRelations,ExistingRelations) :-
  persistent_table_name(RelName,TableName),
  persistent_metadata_table_name(RelName,MDTableName),
  RequiredRelations = [RelName,TableName,MDTableName],
  relation_exists_list(Connection,RequiredRelations,ExistingRelations).


relation_exists_list(_Connection,[],[]).
relation_exists_list(Connection,[Relation|RequiredRelations],[Relation|ExistingRelations]) :-
  relation_exists(Connection,Relation),
  !,
  relation_exists_list(Connection,RequiredRelations,ExistingRelations).
relation_exists_list(Connection,[_ReqRelation|RequiredRelations],ExistingRelations) :-
  relation_exists_list(Connection,RequiredRelations,ExistingRelations).

% Check whether the types of the external relation match those of the persistent predicate
check_persistent_types(Connection,PredSchema,Error) :-
  PredSchema =.. [TableName|_],
  schema_to_types(PredSchema,DeclaredTypes),
  get_table_types(Connection,TableName,SQLTypeNames),
  (SQLTypeNames==[]
   ->
    write_error_log(['In external relation ''', TableName,'''. Missing referenced relations?']),
    Error=true
   ;
    type_equivalence_list(TypeNames,_DLTypes,SQLTypeNames),
    (most_concrete_type(DeclaredTypes,TypeNames,TypeNames)
     ->
      true
     ;
      write_error_log(['External types ',TypeNames,' do not match declared types ',DeclaredTypes,'.']),
      Error=true
    )
  ).
  
retract_predicate_view_list([]).
retract_predicate_view_list([Predicate|Predicates]) :-
  retract_predicate_view(Predicate),
  retract_predicate_view_list(Predicates).
  
retract_predicate_view(Name/Arity) :-
  my_retract_all_facts(my_view('$des',Name,Arity,_SQLst,_Lang,_RNVss,_ODLIds,_LVDs,_SCs)).

% Check whether the predicate schema exists already
check_existing_pred_schema(PredSchema) :-
  PredSchema =.. [Name|ColnameTypes],
  relation_exists('$des',Name),
  !,
  get_table_types('$des',Name,DTypes),
  my_unzip(ColnameTypes,_,Types),
  type_subsumed_list(Types,DTypes),
  my_retract_all_facts(my_table('$des',Name,_)),
  my_retract_all_facts(my_attribute('$des',_,Name,_,_)).
check_existing_pred_schema(_PredSchema).

% Already asserted
assert_types_from_schema(PredSchema,false) :-
  functor(PredSchema,Name,Arity),
%  my_table(_,Name,Arity),
  my_table('$des',Name,Arity),
  !.
% Assert types
assert_types_from_schema(PredSchema,true) :-
  my_ground(PredSchema),
  !,
  PredSchema=..[Name|ColnameTypes],
  assert_my_types(my_types('$des',Name,ColnameTypes)).
% Types not provided:
assert_types_from_schema(_PredSchema,false).

retract_pred_types(_,false) :-
  !.
retract_pred_types(Name/Arity,true) :-
  retract_table_schema(Name,Arity).

% RootNode is the predicate that require other predicates to persist
assert_my_persistent_assertion_list(Connection,RootNode,Nodes,Types) :-
  my_set_union(Nodes,[RootNode],AllNodes),
  my_set_diff(Nodes,[RootNode],DepNodes),
  assert_my_persistent_assertion_list(Connection,RootNode,DepNodes,AllNodes,Types).

assert_my_persistent_assertion_list(_Connection,_RootNode,_DepNodes,[],_Types).
assert_my_persistent_assertion_list(Connection,RootNode,DepNodes,[Name/Arity|Nodes],Types) :-
  (member(my_types(_Connection,Name,ColnameTypes),Types)
  ;
   length(ColnameTypes,Arity)),
  !,
  PredSchema =.. [Name|ColnameTypes],
  (RootNode=Name/Arity
   ->
    RDepNodes=DepNodes
   ;
    RDepNodes=[]
  ),
  assert_my_persistent_assertion(Connection,Name,PredSchema,RDepNodes),
  assert_my_persistent_assertion_list(Connection,RootNode,DepNodes,Nodes,Types).
    
assert_my_persistent_assertion(Connection,Name,PredSchema,DepNodes) :-
  \+ my_persistent(Connection,PredSchema),
  !,
  (ground(PredSchema) -> true ; get_table_typed_schema(Name,PredSchema)),
  retractall(my_persistent(Connection,PredSchema,_)),
  assertz(my_persistent(Connection,PredSchema,DepNodes)).
assert_my_persistent_assertion(_Connection,_Name,_PredSchema,_DepNodes).

  
build_my_view_assertion_list([],[],[],[]).
%build_my_view_assertion_list([Viewname/Arity|Nodes],[SQLstr|SQLstrs],[my_view('$des',Viewname,Arity,(SQLst,Schema),sql,RNVss,[],[],[])|MyViews]) :-
build_my_view_assertion_list([Viewname/Arity|Nodes],[SQLstr|SQLstrs],[DLs|DLsList],[my_view('$des',Viewname,Arity,SQLst,sql,RNVss,ODLIds,[],[])|MyViews]) :-
  parse_sql_query(SQLst,SQLstr,[]),
%  parse_sql_query((SQLst,_UntypedSchema),SQLstr,[]),
%  get_table_typed_schema(Viewname,Schema),
  RNVss = [],
  dlrule_id_list(DLs,ODLIds),
  build_my_view_assertion_list(Nodes,SQLstrs,DLsList,MyViews).
 
% Use declared types (if declared) for the predicate to be made persistent
% Otherwise use inferred types   
predicate_types(_PredSchema,[],[]) :-
  !.
predicate_types(PredSchema,Types,[my_types(Connection,Name,PColnameTypes)|FTypes]) :-
  PredSchema=..[Name|DeclColnameTypes],
  member(my_types(Connection,Name,ColnameTypes),Types),
  findall(my_types(PConnection,PName,PColnameTypes),
          (PConnection = '$des',
           member(my_types(PConnection,PName,PColnameTypes),Types),
           PName \== Name),
          FTypes),
  my_unzip(DeclColnameTypes,DeclColnames,DeclTypes),
  (\+ my_ground(DeclTypes)
   ->
    my_unzip(ColnameTypes,_InfColnames,InfTypes),
    my_zipWith(':',DeclColnames,InfTypes,PColnameTypes)
   ;
    PColnameTypes=DeclColnameTypes).
  

check_persist_relations(_Connection,[]).
check_persist_relations(Connection,[Name|Names]) :-
  % If not persistent already but exists in the external database already, do nothing
  \+ (my_persistent(Connection,PredSchema), 
      functor(PredSchema,Name,_Arity)),
  relation_exists(Connection,Name),
  !,
  check_persist_relations(Connection,Names).
check_persist_relations(Connection,[Name|_Names]) :-
  % If not persistent already, check availability of relation names in the external database
  \+ (my_persistent(Connection,PredSchema), 
      functor(PredSchema,Name,_Arity)),
  persistent_table_name(Name,TableName),
  persistent_metadata_table_name(Name,MDTableName),
  (relation_exists(Connection,Name)
  ;
   relation_exists(Connection,TableName)
  ;
   relation_exists(Connection,MDTableName)
  ),
  !,
  write_error_log(['Relation ',Name,' is declared in the external database already.']),
  fail.
check_persist_relations(Connection,[_Name|Names]) :-
  check_persist_relations(Connection,Names).
  
persistent_table_name(Name,TableName) :-
  atom_concat(Name,'_des_table',TableName).
  
persistent_metadata_table_name(Name,MDTableName) :-
  atom_concat(Name,'_des_metadata',MDTableName).


schema_to_types(Schema,Types) :-
  Schema=..[_Pred|Args],
  my_unzip(Args,_ColNames,Types).
  
schema_to_colnames(Schema,ColNames) :-
  Schema=..[_Pred|Args],
  my_unzip(Args,ColNames,_Types).
  
check_persist_typing(_Pred,[],[]).
check_persist_typing(Pred,[DecType|_DecTypes],[InfType|_InfTypes]) :-
  var(DecType),
  var(InfType),
  !,
  write_error_log(['To make the empty predicate ',Pred,' to persist, either persistent assertion must include types or types can be inferred from asserted rules.']),
  fail.
check_persist_typing(Pred,[DecType|DecTypes],[InfType|InfTypes]) :-
  most_concrete_type(DecType,InfType,InfType),
  !,
  check_persist_typing(Pred,DecTypes,InfTypes).
check_persist_typing(Pred,[DecType|_DecTypes],[InfType|_InfTypes]) :-
  write_error_log(['Declared type ',DecType,' for ',Pred,' does not cover its inferred type ',InfType]),
  fail.
  
% Change DL rules datalog(...) by datalog_persistent(...) for given predicates
% This allows to omit seeking in-memory datalog rules. Instead, they are retrived
% from the external database
% Datalog rules are kept in datalog_persistent as they might be recovered upon 
% unpersisting, and Datalog facts are simply removed as they are stored in a table

% change_datalog_rules_to_persistent(+Connection,+Predicates,+DLRulesList,+DLFactsList,+UPDLRulesList,+RPDLRulesList).
% DLRulesList: List of datalog rules from translating each predicate in Predicates
% DLFactsList: List of datalog facts from translating each predicate in Predicates
% UPDLRulesList: List of non persistent rules from translating each predicate in Predicates (already persistent and must be made non persistent: a rule that becomes recursive)
% RPDLRulesList: List of rejected rules and facts from translating each predicate in Predicates
change_datalog_rules_to_persistent(_Connection,[],[],[],[],[]).
change_datalog_rules_to_persistent(Connection,[Name/Arity|Preds],
 [DLs|DLsList],[DLFs|DLFsList],[UDLs|UDLsList],[RDLs|RDLsList]) :-
  (retract(datalog_persistent(Name/Arity,_PDLs,UPDLsi))
   ->
    true
   ;
    UPDLsi = []
  ),
  concat_lists([RDLs,UDLs,UPDLsi],DUPDLs), 
  remove_duplicates(DUPDLs,LDLs), % Already persistent, rejected rules must not be redumped
  assertz(datalog_persistent(Name/Arity,DLs,LDLs)),
  my_map(retractall,DLs),
  my_map(retractall,DLFs),
  append(DLs,LDLs,MDLs),
  dump_metadata(Connection,Name,MDLs),
  change_datalog_rules_to_persistent(Connection,Preds,DLsList,DLFsList,UDLsList,RDLsList).

% Write metadata to an external database for a predicate 
dump_metadata(_Connection,_Name,[]) :-
  !.
dump_metadata(Connection,Name,DLs) :-
  persistent_metadata_table_name(Name,MDTableName),
  delimited_sql_identifier(Connection,MDTableName,StrMDTableName),
  concat_lists(["DELETE FROM ",StrMDTableName],StrDelete),
  exec_and_show_rdb_sql_list(Connection,[StrDelete]),
  dump_metadata_rules(Connection,StrMDTableName,DLs).
    
dump_metadata_rules(_Connection,_StrName,[]).
dump_metadata_rules(Connection,StrName,[DL|DLs]) :-
  dlrule_to_ruleNVs_list([DL],[(Rule,NVs)]),
  my_term_to_string(Rule,StrRule,NVs),
  replace_str_all("'","''",StrRule,TwiceQuotedStrRule),
  concat_lists(["INSERT INTO ",StrName," VALUES('",TwiceQuotedStrRule,".')"],SQLInsert),
  exec_and_show_rdb_sql_list(Connection,[SQLInsert]),
  dump_metadata_rules(Connection,StrName,DLs).

dump_metadata_rules_list(_Connection,[],[]).
dump_metadata_rules_list(Connection,[Name/_Arity|Nodes],[DLs|DLsList]) :-
  delimited_sql_identifier(Connection,Name,StrName),
  dump_metadata_rules(Connection,StrName,DLs),
  dump_metadata_rules_list(Connection,Nodes,DLsList).
  

% Read metadata stored in external database for a predicate that is persistent already
load_metadata(Connection,Name) :-
  persistent_metadata_table_name(Name,MDTableName),
  delimited_sql_identifier(Connection,MDTableName,StrMDTableName),
  push_flag(undef_pred_warnings,off,OldValue),
  load_metadata_rules(Connection,StrMDTableName),
  pop_flag(undef_pred_warnings,OldValue).
    
load_metadata_rules(Connection,StrName) :-
  concat_lists(["SELECT txtrule FROM ",StrName],SQLSelect),
  display_string_list_sql_on([SQLSelect]),
  my_odbc_dql_query(Connection,SQLSelect,_Schema,Rows),
  findall((Rule,NVs),
          (member(Row,Rows),
           Row =.. [_,AtomRNVs],
           atom_codes(AtomRNVs,StrRule),
%           read_term_from_codes(StrRule,Rule,[])
           read_term_from_codes(StrRule,Rule,[variable_names(NVs)])
          ),
          Rules),
%  build_and_assert_datalog_rule_list(Rules),
  build_and_assert_datalog_ruleNVs_list(Rules),
  processC(clear_et,[],[],yes),
  compute_stratification.

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DROPPING via /drop_assertion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unpersist_pred(Connection,PredSchema) :-
  functor(PredSchema,RelationName,Arity),
  datalog_persistent(RelationName/Arity,RDLs,UPDLs),
  assertz_list(RDLs),
  retract(datalog_persistent(RelationName/Arity,RDLs,UPDLs)),
  persistent_table_name(RelationName,TableName),
  delimited_sql_identifier(Connection,TableName,StrDelimitedTableName),
  concat_lists(["SELECT * FROM ",StrDelimitedTableName],StrSelectTable),
% my_odbc_dangling_query_close, % SQL Server
  my_odbc_dql_query(Connection,StrSelectTable,_Schema,Rows),
  replace_functor_term_list(Rows,RelationName,Facts),
  build_and_assert_datalog_rule_list(Facts),
  drop_persistent_relations(Connection,RelationName),
  retract((my_view('$des',RelationName,Arity,_,_,_,_,_,_):-true)).
  
% Drop View, Table and Metadata external relations for a persistent predicate
drop_persistent_relations(Connection,RelationName) :-
  delimited_sql_identifier(Connection,RelationName,StrDelimitedRelationName),
  concat_lists(["DROP VIEW ",StrDelimitedRelationName," IF EXISTS"],StrDropView),
  display_string_list_sql_on([StrDropView]),
  drop_rdb_view_if_exists(Connection,RelationName),
  persistent_table_name(RelationName,TableName),
  delimited_sql_identifier(Connection,TableName,StrDelimitedTableName),
  concat_lists(["DROP TABLE ",StrDelimitedTableName],StrDropTable),
  display_string_list_sql_on([StrDropTable]),
  my_odbc_ddl_query(Connection,StrDropTable),
  persistent_metadata_table_name(RelationName,MDTableName),
  delimited_sql_identifier(Connection,MDTableName,StrDelimitedMDTableName),
  concat_lists(["DROP TABLE ",StrDelimitedMDTableName],StrDropMDTable),
  display_string_list_sql_on([StrDropMDTable]),
  my_odbc_ddl_query(Connection,StrDropMDTable).

drop_persistent_relations(RelationName) :-
  my_persistent(Connection,PredSchema),
  functor(PredSchema,RelationName,_Arity),
  drop_persistent_relations(Connection,RelationName).

build_and_assert_datalog_rule_list([]).
build_and_assert_datalog_rule_list([Rule|Rules]) :-
  my_datetime(DT),
  get_rule_id(RId),
  assign_variable_names(Rule,[],NVs),
  assertz(datalog(Rule, NVs, RId, [], [], asserted(DT), source)),
  build_and_assert_datalog_rule_list(Rules).

build_and_assert_datalog_ruleNVs_list([]).
build_and_assert_datalog_ruleNVs_list([(Rule,NVs)|Rules]) :-
  my_datetime(DT),
  get_rule_id(RId),
  assertz(datalog(Rule, NVs, RId, [], [], asserted(DT), source)),
  build_and_assert_datalog_ruleNVs_list(Rules).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UPDATING via assert
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% PARTICULAR CASE: Assumptions are not made persistent
update_persistent_preds_from_dlrules(_DLs,[_|_]) :-
  !.
% PARTICULAR CASE: A ground fact
update_persistent_preds_from_dlrules([DL|DLs],CId) :-
%  dlrule_is_fact(DL),
  dlrule_is_positive_fact(DL),
  ground(DL),
  pred_dlrule(Pred,DL),
  is_persistent_predicate(Pred),
  !,
  Pred = Name/Arity,
  functor(PredSchema,Name,Arity),
  my_persistent(Connection,PredSchema),
  translate_predicate_facts(Connection,[DL],_TDLFacts,_RDLFacts,SQLInsertTablesList),  
  exec_and_show_rdb_sql_list(Connection,SQLInsertTablesList),
  retractall(DL),
  update_persistent_preds_from_dlrules(DLs,CId).
% GENERAL CASE
% First rule is always of the predicate compilation root
% It belongs to a possible persistent predicate
update_persistent_preds_from_dlrules([DL|_DLs],_CId) :-
  pred_dlrule(Pred,DL),
  is_persistent_predicate(Pred),
  !,
%  push_flag(output,off,CurrentValue),
  compute_stratification, % This builds the PDG, which is needed to know dependent predicates that might be made persistent
%  pop_flag(output,CurrentValue),
  Pred = Name/Arity,
  functor(PredSchema,Name,Arity),
  my_persistent(Connection,PredSchema),
%  persistent_predicates_in_local_db(Pred,DependentLocalPersistentPreds),
  update_persistent_pred(Connection,PredSchema,_Error).
%  update_persist_pred_list(Connection,DependentLocalPersistentPreds).
%  persist_pred(Connection,PredSchema,_Error).
update_persistent_preds_from_dlrules(_DLs,_CId).

% update_persist_pred_list(_Connection,[]).
% update_persist_pred_list(Connection,[Name/_Arity|Preds]) :-
%   get_table_typed_schema(Name,PredSchema),
%   update_persistent_pred(Connection,PredSchema,_Error),
%   update_persist_pred_list(Connection,Preds).

create_or_replace_rdb_view(Connection,ViewName,SQLCreateView) :-
  (my_odbc_exists_view(Connection,ViewName)
   ->
    drop_rdb_view(Connection,ViewName)
   ;
    true
  ),
  my_odbc_ddl_query(Connection,SQLCreateView).

% translate_non_persistent_predicates(+Connection,+Predicates,-NPPredicates,-DLRules,-DLFacts,-UPDLRules,-RPDLRules,-SQLViews,-SQLInserts)
% Translating non-persistent predicates.
% As, in general, a predicate depends on others, persisting a predicate means to also persist those others
% + Connection: ODBC connection name
% + Predicates: Predicates to be translated
% - NPPredicates: Predicates which have been not made persistent yet. For these, there will be applied persistency
translate_non_persistent_predicates(Connection,Predicates,NPPredicates,DLRules,DLFacts,UPDLRules,RPDLRules,SQLViews,SQLInserts) :-
  translate_non_persistent_predicates(Connection,Predicates,NPPredicates,DLRules,DLFacts,UPDLRules,RPDLRules,SQLViews,[],SQLInserts).
  
translate_non_persistent_predicates(_Connection,[],[],[],[],[],[],[],SQLInserts,SQLInserts).
% translate_non_persistent_predicates(Connection,[Name/Arity|Predicates],NPPredicates,DLRules,DLFacts,SQLViews,SQLInsertsIn,SQLInsertsOut) :-
%   % A relation made persistent already
%   functor(PredSchema,Name,Arity),
%   my_persistent(_,PredSchema),
%   !,
%   translate_non_persistent_predicates(Connection,Predicates,NPPredicates,DLRules,DLFacts,SQLViews,SQLInsertsIn,SQLInsertsOut).
% translate_non_persistent_predicates(Connection,[Name/Arity|Predicates],[Name/Arity|NPPredicates],DLRules,DLFacts,SQLViews,SQLInsertsIn,SQLInsertsOut) :-
%   % A relation made persistent already which is to be restored
%   functor(PredSchema,Name,Arity),
%   \+ my_persistent(_,PredSchema),
%   check_persistent_relation(Connection,Name,_Error),
%   !,
%   append_one_list(SQLViews,[[]],RemSQLViews),
%   append(SQLInsertsIn,[[]],SQLInsertsIn1),
%   translate_non_persistent_predicates(Connection,Predicates,NPPredicates,DLRules,DLFacts,RemSQLViews,SQLInsertsIn1,SQLInsertsOut).
translate_non_persistent_predicates(Connection,[Name/_Arity|Predicates],NPPredicates,DLRules,DLFacts,UPDLRules,RPDLRules,SQLViews,SQLInsertsIn,SQLInsertsOut) :-
  % Built-ins are not translated
  my_builtin_pred(Name),
  !,
  translate_non_persistent_predicates(Connection,Predicates,NPPredicates,DLRules,DLFacts,UPDLRules,RPDLRules,SQLViews,SQLInsertsIn,SQLInsertsOut).
translate_non_persistent_predicates(Connection,[Name/Arity|Predicates],NPPredicates,DLRules,DLFacts,UPDLRules,RPDLRules,SQLViews,SQLInsertsIn,SQLInsertsOut) :-
  % A relation which is defined already in the external database and not made persistent
  functor(PredSchema,Name,Arity),
  \+ my_persistent(_,PredSchema),
  \+ persistent_relation_exists(Connection,Name),
%  \+ check_persistent_relation(Connection,Name,_Error),
  relation_exists(Connection,Name),
  !,
  %Error\==true,
  translate_non_persistent_predicates(Connection,Predicates,NPPredicates,DLRules,DLFacts,UPDLRules,RPDLRules,SQLViews,SQLInsertsIn,SQLInsertsOut).
translate_non_persistent_predicates(Connection,[Predicate|Predicates],[Predicate|NPPredicates],[PredDLRules|DLRules],[PredDLFacts|DLFacts],[PredUPDLRules|UPDLRules],[PredRPDLRules|RPDLRules],SQLViews,SQLInsertsIn,SQLInsertsOut) :-
  translate_predicate(Connection,Predicate,PredDLRules,PredDLFacts,PredUPDLRules,PredRPDLRules,PredSQLViews,PredSQLInserts),
  !,
%  write_info_verb_log(['Predicate ',Predicate,' made persistent.']),
  append_one_list(SQLViews,PredSQLViews,RemSQLViews),
  append(SQLInsertsIn,PredSQLInserts,SQLInsertsIn1),
  translate_non_persistent_predicates(Connection,Predicates,NPPredicates,DLRules,DLFacts,UPDLRules,RPDLRules,RemSQLViews,SQLInsertsIn1,SQLInsertsOut).
translate_non_persistent_predicates(Connection,[Predicate|Predicates],NPPredicates,DLRules,DLFacts,UPDLRules,RPDLRules,SQLViews,SQLInsertsIn,SQLInsertsOut) :-
  write_error_log(['Translating predicate ',Predicate,nl]),
  translate_non_persistent_predicates(Connection,Predicates,NPPredicates,DLRules,DLFacts,UPDLRules,RPDLRules,SQLViews,SQLInsertsIn,SQLInsertsOut).


% translate_predicate(+Connection,+Name/+Arity,-DLRules,-DLFacts,-DLUnpersistentRules,-DLRejectedRules,-[SQLView],-SQLInserts) :-
translate_predicate(Connection,Name/Arity,DLRules,DLFacts,DLUnpersistentRules,DLRejectedRules,[SQLView],SQLInserts) :-
  %get_dlrules_from_predicate(Name/Arity,DLs), % In-memory
  get_local_and_persistent_dlrules(namearity,Name/Arity,DLs,PDLs),
  sub_pdg(Name/Arity,PDG),
  recursive_predicates_from_pdg(PDG,RecPreds),
  translate_predicate_facts_and_rules(Connection,Name/Arity,DLs,PDLs,RecPreds,DLRules,DLFacts,DLUnpersistentRules,DLRejectedRules,[SQLView],SQLInserts).
  
% translate_predicate_facts_and_rules(+Connection,+Name/+Arity,+DLs,+PDLs,+RecPreds,,-DLRules,-DLFacts,-DLUnpersistentRules,-DLRejectedRules,,-[SQLView],-SQLInserts) :-
translate_predicate_facts_and_rules(Connection,Name/Arity,DLs,PDLs,RecPreds,TDLRules,TDLFacts,DLUnpersistentRules,DLRejectedRules,[SQLView],SQLInserts) :-
  filter_dl_facts_rules(DLs,DLFacts,PRDLRules),
  filter_r_nr_rules(Name/Arity,PRDLRules,RecPreds,RDLRules,DLRules),
  filter_recursive_persistent_rules(RDLRules,PDLs,DLUnpersistentRules),
  my_odbc_get_dbms(Connection,DBMS),
  translate_predicate_facts(Connection,DLFacts,TDLFacts,RDLFacts,SQLInserts),  
  translate_predicate_rules(Connection,DBMS,Name/Arity,DLRules,TDLRules,RNRDLRules,DQLs),
  compose_view(Connection,DBMS,Name/Arity,DQLs,SQLView),
  concat_lists([RDLFacts,RDLRules,RNRDLRules],DLRejectedRules). % Recursive and non-recursive rejected rules and facts

% 
% Filter recursive (RDLRules) and non-recursive (NRDLRules) DL rules
% filter_nr_rules(+DLRules,+RecPreds,-RDLRules,-NRDLRules)
%
filter_r_nr_rules(_Name/_Arity,[],_RecPreds,[],[]).
filter_r_nr_rules(Name/Arity,[DLRule|DLRules],RecPreds,RDLRules,[DLRule|NRDLRules]) :-
  non_recursive_dlrule(DLRule,RecPreds),
  !,
  filter_r_nr_rules(Name/Arity,DLRules,RecPreds,RDLRules,NRDLRules).
filter_r_nr_rules(Name/Arity,[DLRule|DLRules],RecPreds,[DLRule|RDLRules],NRDLRules) :-
  (datalog_persistent(Name/Arity,_,EDLs) -> true ; EDLs = []),
  (member(DLRule,EDLs)
   ->
    true % Do not rewarn
   ;
    dlrule_to_ruleNVs_list([DLRule],[RuleNVs]),
    write_info_verb_log(['This recursive rule cannot be delegated to external database (kept in local database for its processing):',nl,'$exec'(write_datalog_rule(RuleNVs,0))])
  ),
  filter_r_nr_rules(Name/Arity,DLRules,RecPreds,RDLRules,NRDLRules).

% Filter persistent rules in RDLRules which become part of a recursive cycle
% filter_recursive_persistent_rules(+RDLRules,+PDLs,DLRejectedPersistentRules)
filter_recursive_persistent_rules(RDLRules,PDLs,DLRejectedPersistentRules) :-
 my_set_inter(RDLRules,PDLs,DLRejectedPersistentRules).

% Check whether the rule is non recursive
non_recursive_dlrule(DLRule,RecPreds) :-  
  dlrule_to_rule_list([DLRule],[':-'(H,B)]),
  functor(H,N,A),
  pdg_arcs_from_to(B,N/A,Arc),
  rhs_nodes([Arc],Nodes),
  member(Node,Nodes),
  member(Node,RecPreds),
  !,
  fail.
non_recursive_dlrule(_DLRule,_RecPreds).
  
filter_dl_facts_rules([],[],[]).
filter_dl_facts_rules([DL|GRs],Fs,[DL|Rs]) :- % Restricting facts are not proper facts
  DL=datalog(R,_NVs,_RId,_CId,_Ls,_FId,_C),
  (R=':-'(_H,_B) ; R= -(_)),
  !,
  filter_dl_facts_rules(GRs,Fs,Rs).
filter_dl_facts_rules([F|GRs],[F|Fs],Rs) :-
  !,
  filter_dl_facts_rules(GRs,Fs,Rs).
  
% filter_facts_rules([],[],[]).
% filter_facts_rules([':-'(H,B)|GRs],Fs,[':-'(H,B)|Rs]) :-
%   !,
%   filter_facts_rules(GRs,Fs,Rs).
% filter_facts_rules([F|GRs],[F|Fs],Rs) :-
%   !,
%   filter_facts_rules(GRs,Fs,Rs).

translate_predicate_facts(_Connection,[],[],[],[]).
translate_predicate_facts(Connection,[DLFact|DLFacts],[TDLFact|TDLFacts],RDLFacts,[SQLInsert|SQLInserts]) :-
  dlrule_to_ruleNVs_list([DLFact],[NVFact]),
  ensure_safe_ruleNVs(NVFact),
  !,
  copy_term(DLFact,TDLFact),
  NVFact = (Fact,_NVs),
  Fact =.. [Name|Values],
  persistent_table_name(Name,TableName),
  delimited_sql_identifier(Connection,TableName,StrName),
  compose_sql_cs_values(Values,StrValues,""),
  concat_lists(["INSERT INTO ",StrName," VALUES (",StrValues,")"],SQLInsert),
  translate_predicate_facts(Connection,DLFacts,TDLFacts,RDLFacts,SQLInserts).
translate_predicate_facts(Connection,[DLFact|DLFacts],TDLFacts,[DLFact|RDLFacts],SQLInserts) :-
  % DLFact has not been able to be translated (errors are shown where they occur)
  translate_predicate_facts(Connection,DLFacts,TDLFacts,RDLFacts,SQLInserts).

compose_sql_cs_values([Value]) -->
  {nonvar(Value),
   Value='$NULL'(_)},
  !,
  "NULL".
% Ciao does not allow variables as a DCG literal:
% % So, instead using:
% compose_sql_cs_values([Value]) -->
%   {number(Value),
%    !,
%    number_codes(Value,StrValue)},
%   StrValue.
% compose_sql_cs_values([Value]) -->
%   {atom_codes(Value,StrValue),
%    replace_str_all("'","''",StrValue,TwiceQuotedStrValue)},
%   "'",
%   TwiceQuotedStrValue,
%   "'".
% Use its compiled form:
compose_sql_cs_values([A], B, E) :-
  number(A), !,
  number_codes(A, C),
  D=B,
  phrase(C, D, E).
compose_sql_cs_values([A], C, H) :-
  atom_codes(A, B),
  [I]="'",
  replace_str_all([I], [I, I], B, E),
  D=C,
  D=[I|F],
  phrase(E, F, G),
  G=[I|H].
compose_sql_cs_values([Value1,Value2|Values]) -->
  compose_sql_cs_values([Value1]),
  ",",
  compose_sql_cs_values([Value2|Values]).
  
translate_predicate_rules(_Connection,_DBMS,_Pred,[],[],[],[]). 
translate_predicate_rules(Connection,DBMS,Pred,[DLRule|DLRules],[TDLRule|TDLRules],RDLRules,[DQL|DQLs]) :-
  copy_term(DLRule,TDLRule),
  translate_predicate_rule(Connection,DBMS,DLRule,DQL),
  !,
  translate_predicate_rules(Connection,DBMS,Pred,DLRules,TDLRules,RDLRules,DQLs).
translate_predicate_rules(Connection,DBMS,Pred,[DLRule|DLRules],TDLRules,[DLRule|RDLRules],DQLs) :-
  (datalog_persistent(Pred,_,EDLs) -> true ; EDLs = []),
  (member(DLRule,EDLs)
   ->
    true % Do not rewarn
   ;
    dlrule_to_ruleNVs_list([DLRule],[RuleNVs]),
    write_info_verb_log(['This rule cannot be delegated to external database (kept in local database for its processing):',nl,'$exec'(write_datalog_rule(RuleNVs,0))])
  ),
  % DLRule is rejected: it has not been able to be translated (errors are shown where they occur)
  translate_predicate_rules(Connection,DBMS,Pred,DLRules,TDLRules,RDLRules,DQLs).
 
translate_predicate_rule(Connection,DBMS,DLRule,DQL) :-
  dlrule_to_ruleNVs_list([DLRule],[NVRule]),
  NVRule = (Rule,NVs),
  dx_simplify_rule(Rule,SRule),
  my_translate(Connection,DBMS,(SRule,NVs),DQL).

my_translate(Connection,DBMS,(Rule,NVs),DQL) :-
  (Rule = ':-'(Head,Body),
   !
  ;
   Head = Rule,
   (DBMS == access
    ->
     Body =  dual(_) 
    ;
     Body = true
   )
  ), 
  ensure_safe_ruleNVs((Rule,NVs)),
  translate(Connection,Head,Body,DQL). 
  

compose_view(Connection,DBMS,Name/_Arity,DQLs,StrSQLView) :-
  (DBMS==access
   ->
%   % WARNING: MS Access fails to create a delimited view name
%    delimited_sql_identifier(Connection,Name,StrViewPrototype)
    name(Name,StrViewPrototype)
   ;
    compose_view_prototype(Connection,Name,StrViewPrototype)
  ),
  persistent_table_name(Name,TableName),
  delimited_sql_identifier(Connection,TableName,StrTableName),
%  atom_codes(TableName,StrTableName),
  (DQLs == []
   ->
    StrTail = []
   ;
    compose_unions(Connection,DQLs,StrUnions),
    concat_lists([" UNION ALL ",StrUnions],StrTail)
  ),
  concat_lists(["CREATE VIEW ",StrViewPrototype," AS SELECT * FROM ",StrTableName,StrTail],StrSQLView).

compose_view_prototype(Connection,Name,StrPrototype) :-
  get_table_untyped_arguments('$des',Name,Colnames),
  Colnames \== [],
  !,
  build_str_delimited_schema(Connection,Name,Colnames,StrPrototype).
%  my_term_to_string_unquoted(USchema,StrPrototype).
compose_view_prototype(Connection,Name,StrPrototype) :-
  delimited_sql_identifier(Connection,Name,StrPrototype).
%  my_term_to_string_unquoted(Name,StrPrototype).

build_str_delimited_schema(Connection,Name,Colnames,StrSchema) :-
  delimited_sql_identifier(Connection,Name,StrName),
  delimited_sql_identifier_list(Connection,Colnames,StrColnameList),
  build_str_tuple(StrColnameList,StrColnameTuple),
  concat_lists([StrName,"(",StrColnameTuple,")"],StrSchema).

build_str_tuple(StrList,StrTuple) :-
  build_str_tuple(StrList,StrTuple,[]).
  
% Ciao does not allow variables as a DCG literal:
% % So, instead using:
% build_str_tuple([S]) -->
%   S.
% build_str_tuple([S1,S2|Ss]) -->
%   S1,
%   ",",
%   build_str_tuple([S2|Ss]).
% Use its compiled form:
build_str_tuple([A], B, C) :-
  phrase(A, B, C).
build_str_tuple([A, D|E], B, G) :-
  phrase(A, B, C),
  [H]=",",
  C=[H|F],
  build_str_tuple([D|E], F, G).
                
compose_unions(Connection,[DQL],StrQuery) :-
  queries_atom(Connection,DQL,Query),
  name(Query,StrQuery).
compose_unions(Connection,[DQL1,DQL2|DQLs],StrQuery) :-
  compose_unions(Connection,[DQL1],StrQuery1),
  compose_unions(Connection,[DQL2|DQLs],StrQuery2),
  concat_lists([StrQuery1," UNION ALL ",StrQuery2],StrQuery).

% Compose create table statements: Only when a predicate is firstly making persistent
compose_create_table_statements(_Connection,[],[]). 
compose_create_table_statements(Connection,[Name/_Arity|Predicates],SQLCreateTables) :-
  persistent_table_name(Name,TableName),
  relation_exists(Connection,TableName),
  !,
  compose_create_table_statements(Connection,Predicates,SQLCreateTables).
compose_create_table_statements(Connection,[Predicate|Predicates],[SQL|SQLCreateTables]) :-
  compose_create_table_statement(Connection,Predicate,SQL), 
  write_info_verb_log(['Predicate ',Predicate,' made persistent.']),
  compose_create_table_statements(Connection,Predicates,SQLCreateTables).
  
compose_create_table_statement(Connection,Name/_Arity,StrSQL) :-
  compose_typed_arguments(Connection,Name,StrTypedArguments),
  persistent_table_name(Name,TableName),
  delimited_sql_identifier(Connection,TableName,StrTableName),
  concat_lists(["CREATE TABLE ",StrTableName,"(",StrTypedArguments,")"],StrSQL).

compose_typed_arguments(Connection,Name,StrTypedArguments) :-
  get_table_typed_arguments('$des',Name,ColnameTypes),
  compose_typed_arguments(ColnameTypes,Connection,"",StrTypedArguments).

compose_typed_arguments([],_Connection,StrTypedArguments,StrTypedArguments).  
compose_typed_arguments([C:T],Connection,StrTypedArgsIn,StrTypedArguments) :-
  type_equivalence(Connection,T,_,SQLT),
  atom_codes(C,Cs),
  my_term_to_string(SQLT,Ts),
  concat_lists([StrTypedArgsIn,Cs," ",Ts],StrTypedArguments). 
compose_typed_arguments([C:T,CT|CTs],Connection,StrTypedArgsIn,StrTypedArguments) :-
  compose_typed_arguments([C:T],Connection,StrTypedArgsIn,StrTypedArgsIn1),
  append(StrTypedArgsIn1,",",StrTypedArgsIn2),
  compose_typed_arguments([CT|CTs],Connection,StrTypedArgsIn2,StrTypedArguments).

% Equality simplifications
% A rule as p(X):-X=1 is not handled by the translation
  
dx_simplify_rule_list([],[]).
dx_simplify_rule_list([R|Rs],[SR|SRs]) :-
  dx_simplify_rule(R,SR),
  dx_simplify_rule_list(Rs,SRs).
 
dx_simplify_rule(':-'(H,B),SR) :-
  !,
  dx_simplify_body(B,SB),
  (SB == true
   ->
    SR = H
   ;
    SR = ':-'(H,SB)
  ).
dx_simplify_rule(R,R).
  
dx_simplify_body((true;Bs),SBs) :-
  !,
  dx_simplify_body(Bs,SBs).
dx_simplify_body((Bs;true),SBs) :-
  !,
  dx_simplify_body(Bs,SBs).
dx_simplify_body((dual;Bs),SBs) :-
  !,
  dx_simplify_body(Bs,SBs).
dx_simplify_body((Bs;dual),SBs) :-
  !,
  dx_simplify_body(Bs,SBs).
dx_simplify_body((B,Bs),SBody) :-
  !,
  dx_simplify_goal(B,SB),
  dx_simplify_body(Bs,SBs),
  append_goals(SB,SBs,SBody).
dx_simplify_body(B,SB) :-
  !,
  dx_simplify_goal(B,SB).
dx_simplify_goal(A is B,G) :-
  my_ground(B),
  !,
  eval_expr(B,EB,_),
  dx_simplify_goal(A=EB,G).
dx_simplify_goal(A=B,G) :-
  !,
  (my_ground(A) -> eval_expr(A,EA,_) ; EA = A),
  (my_ground(B) -> eval_expr(B,EB,_) ; EB = B),
  (my_noncompound_term(EA),
   my_noncompound_term(EB)
   ->
    (EA=EB -> G=true ; G=false)
   ;
    G=(EA=EB)).
dx_simplify_goal(not(true),false) :-
  !.
dx_simplify_goal(not(dual),false) :-
  !.
dx_simplify_goal(not(false),true) :-
  !.
dx_simplify_goal(dual,true) :-
  !.
dx_simplify_goal(G,G).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DROPPING a rule or fact via retract
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

drop_persistent_rule(Connection,R) :-
  drop_persistent_rule(Connection,R,[]).
  
drop_persistent_rule(Connection,R,NVs) :-
  %hide_nulls(R,SR),
  SR=R,
  %dx_simplify_rule(NR,SR),
  (rule_is_positive_fact(SR)
   ->
    drop_persistent_fact(Connection,SR)
   ;
    pred_rule(Name/Arity,SR),
    drop_persistent_non_positive_fact(Connection,Name,Arity,SR,NVs)
  ).

drop_persistent_non_positive_fact(Connection,Name,Arity,R,NVs) :-
  datalog_persistent(Name/Arity,DLs,_UPDLs),
  filter_dl_facts_rules(DLs,_DLFacts,DLRules),
  copy_term(DLRules,CDLRules),
%  dlrule_to_rule_list(CDLRules,Rules),
%  dx_simplify_rule_list(Rules,SRules),
  my_odbc_get_dbms(Connection,DBMS),
  translate_predicate_rules(Connection,DBMS,Name/Arity,CDLRules,_,_,DQLs),
  compose_view(Connection,DBMS,Name/Arity,DQLs,SQLCreateView),
  display_string_list_sql_on([SQLCreateView]),
  create_or_replace_rdb_view(Connection,Name,SQLCreateView),
  persistent_metadata_table_name(Name,MDTableName),
  my_term_to_string(R,StrUndottedR,NVs),
  append(StrUndottedR,".",StrR),
  replace_str_all("'","''",StrR,TwiceQuotedStrR),
  atom_codes(TwiceQuotedR,TwiceQuotedStrR),
  delete_top_1_from_table(Connection,MDTableName,[TwiceQuotedR]),
  build_my_view_assertion_list([Name/Arity],[SQLCreateView],_,[MyView]),
  retract(my_view('$des',Name,Arity,_,_,_,_,_,_)),
  assertz(MyView).

drop_persistent_fact(Connection,Fact) :-
  Fact =.. [Name|Columns],
  persistent_table_name(Name,TableName),
  delete_top_1_from_table(Connection,TableName,Columns).
  
delete_top_1_from_table(Connection,TableName,Columns) :-
% DELETE TOP(1) and LIMIT does not work with all RDBMSs
% TODO: Write RDBMS-specific code for deleting top 1 tuples
  build_sql_rdb_datasource(Connection,TableName,Columns,SQLstr),
  replace_str_first("SELECT *","SELECT COUNT(*)",SQLstr,StrSQLCount),
  display_string_list_sql_on([StrSQLCount]),
% WARNING: It should be only one return tuple. 
% [CountRow|_] is a workaround for a bug in SICStus 4.2.0
  my_odbc_dql_query(Connection,StrSQLCount,_Schema,[CountRow|_]),
  CountRow =.. [_|Count],
  (Count > 0
   ->
    replace_str_first("SELECT *","DELETE",SQLstr,StrSQLDelete),
    exec_and_show_rdb_sql_list(Connection,[StrSQLDelete]),
    build_sql_rdb_insert_values(Connection,TableName,Columns,StrSQLInsert),
    Count1 is Count-1,
    my_n_repeat(Count1, exec_and_show_rdb_sql_list(Connection,[StrSQLInsert]))
   ;
    write_error_log(['Nothing deleted']),
    !,
    fail
  ).

build_sql_rdb_insert_values(Connection,TableName,Columns,StrSQLInsert) :-
  delimited_sql_identifier(Connection,TableName,StrDelimitedTableName),
  my_list_to_tuple(Columns,TColumns),
  my_term_to_string(TColumns,StrValues),
  concat_lists(["INSERT INTO ",StrDelimitedTableName," VALUES (",StrValues,")"],StrSQLInsert).
  
  
/*********************************************************/
/* BEGIN OF Ancillary Stuff */
 
% Testing whether a predicate is persistent already
is_persistent_predicate(Predicate) :-
  datalog_persistent(Predicate,_,_).
  
get_persistent_preds(Predicates) :-
  findall(Predicate,datalog_persistent(Predicate,_DLs,_UPDLs),Predicates).

pred_schema(Name/Arity,Schema) :-
  Schema =.. [Name|Args],
  length(Args,Arity).
  
get_persistent_assertions(Assertions) :-
  findall(persistent(PredSchema,Connection),my_persistent(Connection,PredSchema),Assertions).
  
restore_persistent_assertions(OldAssertions) :-
  get_persistent_assertions(CurrentAssertions),
  my_set_diff(OldAssertions,CurrentAssertions,DroppedAssertions), % Dropped assertions to be restored
  my_set_diff(CurrentAssertions,OldAssertions,AddedAssertions),   % Added assertions to be dropped
  my_map(assert_assertion,DroppedAssertions),
  my_map(drop_assertion,AddedAssertions), 
  drop_view_k_from_assertion_list(AddedAssertions).
   
drop_view_k_from_assertion_list([]).
drop_view_k_from_assertion_list([persistent(PredSchema,_Connection)|Assertions]) :-
  pred_schema(ViewName/_Arity,PredSchema),
  drop_view_k(ViewName),
  drop_view_k_from_assertion_list(Assertions).
  
/* END OF Ancillary Stuff */
/*********************************************************/


/*********************************************************/
/* BEGIN OF PROLOG TO SQL COMPILER pl2sql (adapted)      */
/*            (C) Copyright by Christoph Draxler, Munich */
/*                Version 1.1 of Dec. 21st 1992          */
/*                                                       */
/* [Drax92]	Draxler, Chr., A Powerful Prolog to SQL      */
/* Compiler, CIS-Bericht-92-61, Centrum für Informations */
/* und Sprachverarbeitung, Ludwig-Maximilians-Universität*/
/* München, 1992.                                        */
/*                                                       */

% --------------------------------------------------------------------------------------
%
% Top level predicate translate/3 organizes the compilation and constructs a
% Prolog term representation of the SQL query. 
%
% --------------------------------------------------------------------------------------


% FSP: Added Connection to build DBMS-specific code, so that translate/3 becomes translate/4
translate(Connection,ProjectionTerm,DatabaseGoal,SQLQueryTerm):-
   % --- initialize variable identifiers and range variables for relations -----
   init_gensym(var),
   init_gensym(rel),

   % --- tokenize projection term and database goal ----------------------------
   tokenize_term(DatabaseGoal,TokenDatabaseGoal),
   tokenize_term(ProjectionTerm,TokenProjectionTerm),

   % --- lexical analysis: reordering of goals for disjunctive normalized form -
   disjunction(TokenDatabaseGoal,Disjunction),

   % --- code generation ---------------------------------------------------------------
   query_generation(Connection,Disjunction,TokenProjectionTerm,SQLQueryTerm).





% --- disjunction(Goal,Disjunction) ----------------------------------------------------
%
% turns original goal into disjunctive normalized form by computing all conjunctions
% and collecting them in a list
%
% --------------------------------------------------------------------------------------

disjunction(Goal,Disjunction):-
   findall(Conjunction,linearize(Goal,Conjunction),Disjunction).




% --- linearize(Goal,ConjunctionList) --------------------------------------------------
%
% Returns a conjunction of base goals for a complex disjunctive or conjunctive goal
% Yields several solutions upon backtracking for disjunctive goals
%
% --------------------------------------------------------------------------------------

linearize(((A,B),C),(LinA,(LinB,LinC))):-
   % --- transform left-linear to right-linear conjunction (',' is associative) ----
   linearize(A,LinA),
   linearize(B,LinB),
   linearize(C,LinC).

linearize((A,B),(LinA,LinB)):-
   A \= (_,_),
   % --- make sure A is not a conjunction ------------------------------------------
   linearize(A,LinA),
   linearize(B,LinB).

linearize((A;_B),LinA):-
   linearize(A,LinA).

linearize((_A;B),LinB):-
   linearize(B,LinB).

linearize(not(A), not(LinA)):-
   linearize(A,LinA).

linearize(Var^A, Var^LinA):-
   linearize(A,LinA).

linearize(A,A):-
   A \= (_,_),
   A \= (_;_),
   A \= _^_,
   A \= not(_).




% --- tokenize_term(Term,TokenizedTerm) -------------------------------------------------
%
% If Term is a 
%
%  - variable, then this variable is instantiated with a unique identifier 
%    of the form '$var$'(VarId), and TokenizedTerm is bound to the same 
%    term '$var$'(VarId). 
%
%  - constant, then TokenizedTerm is bound to '$const$'(Term).
%
%  - complex term, then the term is decomposed, its arguments are tokenized,
%    and TokenizedTerm is bound to the result of the composition of the original
%    functor and the tokenized arguments.
%
% --------------------------------------------------------------------------------------

tokenize_term('$var$'(VarId),'$var$'(VarId)):-
   var(VarId),
   % --- uninstantiated variable: instantiate it with unique identifier.
   gensym(var,VarId).

tokenize_term('$var$'(VarId),'$var$'(VarId)):-
   nonvar(VarId).

tokenize_term(Constant,'$const$'(Constant)):-
   nonvar(Constant),
   functor(Constant,_,0).

tokenize_term(Term,TokenizedTerm):-
   nonvar(Term),
   Term \= '$var$'(_),
   Term \= '$const$'(_),
   Term =.. [Functor|Arguments],
   Arguments \= [],
   tokenize_arguments(Arguments,TokenArguments),
   TokenizedTerm =.. [Functor|TokenArguments].



% --- tokenize_arguments(Arguments,TokenizedArguments) ---------------------------------
%
% organizes tokenization of arguments by traversing list and calling tokenize_term
% for each element of the list.
%
% --------------------------------------------------------------------------------------

tokenize_arguments([],[]).

tokenize_arguments([FirstArg|RestArgs],[TokFirstArg|TokRestArgs]):-
   tokenize_term(FirstArg,TokFirstArg),
   tokenize_arguments(RestArgs,TokRestArgs).







% --- query_generation(Connection,ListOfConjunctions, ProjectionTerm, ListOfQueries) -------------- 
%
% For each Conjunction translate the pair (ProjectionTerm,Conjunction) to an SQL query
% and connect each such query through a UNION-operator to result in the ListOfQueries.
%
% A Conjunction consists of positive or negative subgoals. Each subgoal is translated 
% as follows:
%  - the functor of a goal that is not a comparison operation is translated to
%    a relation name with a range variable
%  - negated goals are translated to NOT EXISTS-subqueries with * projection
%  - comparison operations are translated to comparison operations in the WHERE-clause
%  - aggregate function terms are translated to aggregate function (sub)queries
% 
% The arguments of a goal are translated as follows:
%  - variables of a goal are translated to qualified attributes
%  - variables occurring in several goals are translated to equality comparisons
%    (equi join) in the WHERE-clause
%  - constant arguments are translated to equality comparisons in the WHERE-clause
% 
% Special treatment of arithmetic functions:
%  - arithmetic functions are identified through the Prolog is/2 operator
%  - an arithmetic function may contain an unbound variable only on its left side
%  - the right side of the is/2 operator may consist of 
%    * bound variables (bound through occurrence within a positive database goal, or 
%      bound through preceeding arithmetic function), or of 
%    * constants (numbers, i.e. integers, reals)
% 
% The following RESTRICTION holds:
%
%  - the binding of variables follows Prolog: variables are bound by positive base goals
%    and on the left side of the is/2 predicate - comparison operations, negated goals
%    and right sides of the is/2 predicate do not return variable bindings and may even 
%    require all arguments to be bound for a safe evaluation.
%
% --------------------------------------------------------------------------------------

query_generation(_Connection,[],_,[]).

query_generation(Connection,[Conjunction|Conjunctions],ProjectionTerm,[Query|Queries]):-
   projection_term_variables(ProjectionTerm,InitDict),
   translate_conjunction(Connection,Conjunction,SQLFrom,SQLWhere,InitDict,Dict),
   translate_projection(ProjectionTerm,Dict,SQLSelect),
   Query = query(SQLSelect,SQLFrom,SQLWhere),
   query_generation(Connection,Conjunctions,ProjectionTerm,Queries).



% --- translate_goal(Connection,Goal,SQLFrom,SQLWhere,Dict,NewDict) -------------------------------
%
% translates a
%
%   - positive database goal to the associated FROM- and WHERE clause of an SQL query
%   - a negated goal to a negated existential subquery
%   - an arithmetic goal to an arithmetic expression or an aggregate function query
%   - a comparison goal to a comparison expression
%   - a negated comparison goal to a comparison expression with the opposite comparison
%     operator
%
% --------------------------------------------------------------------------------------

% FSP. Added to deal with facts (true bodies)
translate_goal(_Connection,'$const$'(true),[],[],Dict,Dict) :-
  !.

translate_goal(Connection,Result is Expression,[],SQLWhere,Dict,NewDict):-
   translate_arithmetic_function(Connection,Result,Expression,SQLWhere,Dict,NewDict).

translate_goal(Connection,not(NegatedGoals),[],SQLNegatedSubquery,Dict,Dict):-
   % --- negated goals do not bind variables - hence Dict is returned unchanged --------
   functor(NegatedGoals,Functor,_),
   \+ comparison(Functor,_),
   translate_conjunction(Connection,NegatedGoals,SQLFrom,SQLWhere,Dict,_),
   SQLNegatedSubquery = [negated_existential_subquery([*],SQLFrom,SQLWhere)].

translate_goal(Connection,not(ComparisonGoal),[],SQLCompOp,Dict,Dict):-
   % --- comparison operations do not bind variables - Dict is returned unchanged ------
   ComparisonGoal =.. [ComparisonOperator,LeftArg,RightArg],
   comparison(ComparisonOperator,SQLOperator),
   negated_comparison(SQLOperator,SQLNegOperator),
   translate_comparison(Connection,LeftArg,RightArg,SQLNegOperator,Dict,SQLCompOp).

% FSP. Added this clause to support the translation of distinct/1
% translate_goal(Connection,distinct(Goal),SQLSubquery,[],Dict,NewDict):-
%    translate_goal(Connection,Goal,SQLFrom,SQLWhere,Dict,NewDict),
%   SQLSubquery = [query(['DISTINCT *'],SQLFrom,SQLWhere)].

% % FSP. Added this clause to support the translation of top/2
% translate_goal(Connection,top(N,Goal),SQLSubquery,[],Dict,NewDict):-
%    translate_goal(Connection,Goal,SQLFrom,SQLWhere,Dict,NewDict),
%    SQLSubquery = [top_query(N,[*],SQLFrom,SQLWhere)].

translate_goal(Connection,ComparisonGoal,[],SQLCompOp,Dict,Dict):-
   % --- comparison operations do not bind variables - Dict is returned unchanged ------
   ComparisonGoal =.. [ComparisonOperator,LeftArg,RightArg],
   comparison(ComparisonOperator,SQLOperator),
   translate_comparison(Connection,LeftArg,RightArg,SQLOperator,Dict,SQLCompOp).

% FSP. Other built-ins are rejected to be translated
translate_goal(_Connection,BuiltInGoal,_,_,_,_):-
   functor(BuiltInGoal,Name,Arity),
   my_builtin_pred(Name/Arity),
%   write_error_log(['Unsupported built-in in the translation: ',Name/Arity]),
   !,
   fail.

% FSP. This rule is moved here to deal with built-ins first
translate_goal(Connection,SimpleGoal,[SQLFrom],SQLWhere,Dict,NewDict):-
   % --- positive goal binds variables - these bindings are held in the dictionary -----
   functor(SimpleGoal,Functor,Arity),
   translate_functor(Connection,Functor,Arity,SQLFrom),
   SimpleGoal =.. [Functor|Arguments],
   translate_arguments(Connection,Arguments,SQLFrom,1,SQLWhere,Dict,NewDict).




% --- translate_conjunction(Connection,Conjunction,SQLFrom,SQLWhere,Dict,NewDict) -----------------
% 
% translates a conjunction of goals (represented as a list of goals preceeded by 
% existentially quantified variables) to FROM- and WHERE-clause of an SQL query.  
% A dictionary containing the associated SQL table and attribute names is built up
% as an accumulator pair (arguments Dict and NewDict)
%
% --------------------------------------------------------------------------------------

translate_conjunction(Connection,'$var$'(VarId)^Goal,SQLFrom,SQLWhere,Dict,NewDict):-
   % --- add info on existentially quantified variables to dictionary here -------------
   add_to_dictionary(VarId,_,_,_,existential,Dict,TmpDict),
   translate_conjunction(Connection,Goal,SQLFrom,SQLWhere,TmpDict,NewDict).

translate_conjunction(Connection,Goal,SQLFrom,SQLWhere,Dict,NewDict):-
   Goal \= (_,_),
   translate_goal(Connection,Goal,SQLFrom,SQLWhere,Dict,NewDict).

translate_conjunction(Connection,(Goal,Conjunction),SQLFrom,SQLWhere,Dict,NewDict):-
   translate_goal(Connection,Goal,FromBegin,WhereBegin,Dict,TmpDict),
   translate_conjunction(Connection,Conjunction,FromRest,WhereRest,TmpDict,NewDict),
   append(FromBegin,FromRest,SQLFrom),
   append(WhereBegin,WhereRest,SQLWhere).





% --- translate_arithmetic_function(Connection,Result,Expression,SQLWhere,Dict,NewDict) -----------
%
% Arithmetic functions (left side of is/2 operator is bound to value of expression on
% right side) may be called with either
%
% - Result unbound: then Result is bound to the value of the evaluation of Expression
% - Result bound: then an equality condition is returned between the value of Result
%   and the value of the evaluation of Expression.
%
% Only the equality test shows up in the WHERE clause of an SQLquery.
%
% --------------------------------------------------------------------------------------

translate_arithmetic_function(Connection,'$var$'(VarId),Expression,[],Dict,NewDict):-
   % assigment of value of arithmetic expression to variable - does not
   % show up in WHERE-part, but expression corresponding to
   % variable must be stored in Dict for projection translation

   evaluable_expression(Connection,Expression,Dict,ArithExpression,Type),
   add_to_dictionary(VarId,is,ArithExpression,Type,all,Dict,NewDict).


translate_arithmetic_function(Connection,'$var$'(VarId),Expression,ArithComparison,Dict,Dict):-
   % --- test whether left side evaluates to right side: return equality comparison ----
   % Left side consists of qualified attribute, i.e. range variable must not be
   % arithmetic operator is/2 

   lookup(VarId,Dict,PrevRangeVar,PrevAtt,PrevType),
   \+ (PrevRangeVar = (is)),

   % test whether type of attribute is numeric - if not, there's no sense in 
   % continuing the translation

   type_compatible(PrevType,number),
   evaluable_expression(Connection,Expression,Dict,ArithExpression,ExprType),
   type_compatible(ExprType,number),
   ArithComparison = [comp(att(PrevRangeVar,PrevAtt),'=',ArithExpression)].


translate_arithmetic_function(Connection,'$var$'(VarId),Expression,ArithComparison,Dict,Dict):-
   % --- test whether left side evaluates to right side: return equality comparison ----
   % Left side consists of arithmetic expression, i.e. VarId is stored in Dict as 
   % belonging to arithmetic expression which is expressed as RangeVar-argument 
   % of lookup returning is/2. Type information is implicit through the is/2 functor

   lookup(VarId,Dict,is,LeftExpr,Type),
   type_compatible(Type,number),
   evaluable_expression(Connection,Expression,Dict,RightExpr,ExprType),
   type_compatible(ExprType,number),
   ArithComparison = [comp(LeftExpr,'=',RightExpr)].


translate_arithmetic_function(Connection,'$const$'(Constant),Expression,ArithComparison,Dict,Dict):-
   % --- is/2 used to test whether left side evaluates to right side -------------------
   dx_get_type('$const$'(Constant),ConstantType),
   type_compatible(ConstantType,number),
   evaluable_expression(Connection,Expression,Dict,ArithExpression,ExprType),
   type_compatible(ExprType,number),
   ArithComparison = [comp('$const$'(Constant),'=',ArithExpression)].



% --- translate_comparison(Connection,LeftArg,RightArg,CompOp,Dict,SQLComparison) ---------
%
% translates the left and right arguments of a comparison term into the
% appropriate comparison operation in SQL. The result type of each 
% argument expression is checked for type compatibility
%
% ------------------------------------------------------------------------------

translate_comparison(Connection,LeftArg,RightArg,CompOp,Dict,Comparison):-
   evaluable_expression(Connection,LeftArg,Dict,LeftTerm,LeftArgType),
   evaluable_expression(Connection,RightArg,Dict,RightTerm,RightArgType),
   type_compatible(LeftArgType,RightArgType),
   Comparison = [comp(LeftTerm,CompOp,RightTerm)].







% --- translate_functor(Functor,QualifiedTableName) ------------------------------------
%
% translate_functor searches for the matching relation table name for
% a given functor and creates a unique range variable to result in
% a unique qualified relation table name.
%
% --------------------------------------------------------------------------------------

translate_functor(Connection,Functor,Arity,rel(TableName,RangeVariable)):-
   dx_relation(Connection,Functor,Arity,TableName),
   gensym(rel,RangeVariable),
   !.
%FSP. Added to deal with non persistent predicates
%translate_functor(_Connection,Functor,Arity,rel(_TableName,_RangeVariable)):-
%   write_error_log(['Predicate ',Functor,'/',Arity,' has not been made persistent yet.']),
%   fail.



% --- translate_arguments(Connection,Arguments,RelTable,ArgPos,Conditions,Dict) -------------------
%
% translate_arguments organizes the translation of term arguments. One
% term argument after the other is taken from the list of term arguments
% until the list is exhausted. 
%
% --------------------------------------------------------------------------------------

translate_arguments(_Connection,[],_,_,[],Dict,Dict).

translate_arguments(Connection,[Arg|Args],SQLTable,Position,SQLWhere,Dict,NewDict):-
   translate_argument(Connection,Arg,SQLTable,Position,Where,Dict,TmpDict),
   NewPosition is Position + 1,
   translate_arguments(Connection,Args,SQLTable,NewPosition,RestWhere,TmpDict,NewDict),
   append(Where,RestWhere,SQLWhere).




% --- translate_argument(Connection,Argument,RelTable,Position,Condition,Dict) --------------------
%
% The first occurrence of a variable leads to its associated SQL attribute information
% to be recorded in the Dict. Any further occurrence creates an equi-join condition 
% between the current attribute and the previously recorded attribute.
% Constant arguments always translate to equality comparisons between an attribute and 
% the constant value.
%
% --------------------------------------------------------------------------------------

translate_argument(Connection,'$var$'(VarId),rel(SQLTable,RangeVar),Position,[],Dict,NewDict):-
   dx_attribute(Connection,Position,SQLTable,Attribute,Type),
   add_to_dictionary(VarId,RangeVar,Attribute,Type,all,Dict,NewDict).

translate_argument(Connection,'$var$'(VarId),rel(SQLTable,RangeVar),Position,AttComparison,Dict,Dict):-
   % --- Variable occurred previously - retrieve first occurrence data from dictionary -
   lookup(VarId,Dict,PrevRangeVar,PrevAtt,PrevType),
   dx_attribute(Connection,Position,SQLTable,Attribute,Type),
   type_compatible(PrevType,Type),
   AttComparison = [comp(att(RangeVar,Attribute),=,att(PrevRangeVar,PrevAtt))].

translate_argument(Connection,'$const$'(Constant),rel(SQLTable,RangeVar),Position,ConstComparison,Dict,Dict):-
   % --- Equality comparison of constant value and attribute in table ------------------
   dx_attribute(Connection,Position,SQLTable,Attribute,Type),
   dx_get_type('$const$'(Constant),ConstType),
   type_compatible(ConstType,Type),
   ConstComparison = [comp(att(RangeVar,Attribute),=,'$const$'(Constant))].





% --- projection_term_variables(ProjectionTerm,Dict) -----------------------------------
%
% extracts all variables from the ProjectionTerm and places them into the
% Dict as a dict/4 term with their Identifier, a non instantiated RangeVar and 
% Attribute argument, and the keyword existential for the type of quantification
%
% --------------------------------------------------------------------------------------

projection_term_variables('$const(_)$',[]).

projection_term_variables('$var$'(VarId),[dict(VarId,_,_,_,existential)]).

projection_term_variables(ProjectionTerm,ProjectionTermVariables):-
   ProjectionTerm =.. [Functor|ProjectionTermList],
   \+ (Functor = '$var$'),
   \+ (ProjectionTermList = []),
   projection_list_vars(ProjectionTermList,DupProjectionTermVariables),
   % FSP. Remove duplicated variables for the initial dictionary. Otherwise, the translation does not work well, as in the projection term p(X,X) and conjunction r(X),s(X) , where the equality condition for the equijoin is not generated
   remove_duplicates(DupProjectionTermVariables,ProjectionTermVariables).


projection_list_vars([],[]).
projection_list_vars(['$var$'(VarId)|RestArgs],[dict(VarId,_,_,_,existential)|RestVars]):-
   projection_list_vars(RestArgs,RestVars).
projection_list_vars(['$const$'(_)|RestArgs],Vars):-
   projection_list_vars(RestArgs,Vars).






% --------------------------------------------------------------------------------------
% RESTRICTION! ProjectionTerm underlies the following restrictions:
%
%  - ProjectionTerm must have a functor other than the built-in
%    operators, i.e. ',',';', etc. are not allowed
%
%  - only variables and constants are allowed as arguments,
%    i.e. no structured terms
%
% FSP: Seems that variables cannot be repeated in the projection term
%
% --------------------------------------------------------------------------------------

translate_projection('$var$'(VarId),Dict,SelectList):-
   projection_arguments(['$var$'(VarId)],SelectList,Dict).

translate_projection('$const$'(Const),_,['$const$'(Const)]).

translate_projection(ProjectionTerm,Dict,SelectList):-
   ProjectionTerm =.. [Functor|Arguments],
   \+ (Functor = '$var$'),
   \+ (Functor = '$const$'),
   \+ (Arguments = []),
   projection_arguments(Arguments,SelectList,Dict).



projection_arguments([],[],_).

projection_arguments([Arg|RestArgs],[Att|RestAtts],Dict):-
   retrieve_argument(Arg,Att,Dict),
   projection_arguments(RestArgs,RestAtts,Dict).




% - retrieve_argument(Argument,SQLAttribute,Dictionary) --------------------------------
%
% retrieves the mapping of an argument to the appropriate SQL construct, i.e.
%
%  - qualified attribute names for variables in base goals
%  - arithmetic expressions for variables in arithmetic goals
%  - constant values for constants
% 
% --------------------------------------------------------------------------------------

retrieve_argument('$var$'(VarId),Attribute,Dict):-
   lookup(VarId,Dict,TableName,AttName,_),
   (
    TableName = (is) ->
      Attribute = AttName
   ;
      Attribute = att(TableName,AttName)
   ).

retrieve_argument('$const$'(Constant),'$const$'(Constant),_).





% --- lookup(Key,Dict,Value) -----------------------------------------------------------

% lookup(VarId,Dict,RangeVar,Attribute,Type):-
%    member(dict(VarId,RangeVar,Attribute,Type,Quant),Dict),
%    (
%     Quant = all ->
%       true
%    ;
%       nonvar(RangeVar),
%       nonvar(Attribute)
%    ).

% FSP
% | ?- translate(Connection,ss(X),((X=a),s(X)),C).      
% no
% | ?- translate(Connection,ss(X),(s(X),X=a),C).      
% C = [query([att(rel1,a)],[rel(s,rel1)],[comp(att(rel1,a),=,'$const$'(a))])],
% X = '$var$'(var1) ? 
% 
lookup(VarId,Dict,RangeVar,Attribute,Type):-
  member(dict(VarId,RangeVar,Attribute,Type,_Quant),Dict).


% --- add_to_dictionary(Key,RangeVar,Attribute,Quantifier,Dict,NewDict) ----------------

add_to_dictionary(Key,RangeVar,Attribute,Type,_,Dict,Dict):-
   member(dict(Key,RangeVar,Attribute,Type,existential),Dict).

add_to_dictionary(Key,RangeVar,Attribute,Type,Quantifier,Dict,NewDict):-
   \+ member(dict(Key,_,_,_,_),Dict),
   NewDict = [dict(Key,RangeVar,Attribute,Type,Quantifier)|Dict].




% --- aggregate_function(AggregateFunctionTerm,Dict,AggregateFunctionQuery) ------------
%
% aggregate_function discerns five Prolog aggregate function terms: count, avg, min,
% max, and sum. Each such term is has two arguments: a variable indicating the attribute
% over which the function is to be computed, and a goal argument which must contain in 
% at least one argument position the variable:
%
%    e.g.  avg(Seats,plane(Type,Seats))
%
% These aggregate function terms correspond to the SQL built-in aggregate functions.
% 
% RESTRICTION: AggregateGoal may only be conjunction of (positive or negative) base 
% goals
% 
% --------------------------------------------------------------------------------------

aggregate_function(Connection,AggregateFunctionTerm,Dict,AggregateFunctionExpression):-
   AggregateFunctionTerm =..[AggFunctor,AggVar,AggGoal],
   aggregate_functor(AggFunctor,SQLFunction),
   conjunction(AggGoal,AggConjunction),
   aggregate_query_generation(Connection,SQLFunction,AggVar,AggConjunction,Dict,AggregateFunctionExpression).


conjunction(Goal,Conjunction):-
   disjunction(Goal,[Conjunction]).




% --- aggregate_query_generation(Connection,Function,FunctionVariable,AggGoal,Dict,AggregateQuery) 
%
% compiles the function variable (representing the attribute over which the aggregate 
% function is to be computed) and the aggregate goal (representing the selection and 
% join conditions for the computation of the aggregate function) to an SQL aggregate 
% function subquery.
% 
% --------------------------------------------------------------------------------------

aggregate_query_generation(Connection,count,'$const$'('*'),AggGoal,Dict,AggregateQuery):-
   translate_conjunction(Connection,AggGoal,SQLFrom,SQLWhere,Dict,_TmpDict),

   % ATTENTION! It is assumed that in count(*) aggregate query terms there cannot be
   % free variables because '*' stands for "all arguments"

   AggregateQuery = agg_query(_Function,(count,['$const$'(*)]),SQLFrom,SQLWhere,[]).


aggregate_query_generation(Connection,Function,FunctionVariable,AggGoal,Dict,AggregateQuery):-
   translate_conjunction(Connection,AggGoal,SQLFrom,SQLWhere,Dict,TmpDict),

   % --- only variables occurring in the aggregate goal are relevant to the translation
   % of the function variable and the free variables in the goal.
   % Thus subtract from TmpDict all entries of Dict
   set_difference(TmpDict,Dict,AggDict),
 
   translate_projection(FunctionVariable,AggDict,SQLSelect),
   translate_grouping(FunctionVariable,AggDict,SQLGroup),
   AggregateQuery = agg_query(Function,SQLSelect,SQLFrom,SQLWhere,SQLGroup).




% --- translate_grouping(FunctionVariable,Dict,SQLGroup) -------------------------------
%
% finds the free variables in the aggregate function term and collects their
% corresponding SQL qualified attributes in the SQLGroup list.
% 
% --------------------------------------------------------------------------------------

translate_grouping(FunctionVariable,Dict,SQLGroup):-
   free_vars(FunctionVariable,Dict,FreeVariables),
   translate_free_vars(FreeVariables,SQLGroup).




% --- free_vars(FunctionVariable,Dict,FreeVarList) -------------------------------------
%
% A Variable is free if it neither occurs as the FunctionVariable, nor is stored as
% existentially quantified (through ^/2 in the original goal) in the dictionary
% 
% FreeVars contains for each variable the relevant attribute and relation information 
% contained in the dictionary
% 
% --------------------------------------------------------------------------------------

free_vars(FunctionVariable,Dict,FreeVarList):-
  projection_term_variables(FunctionVariable,FunctionVariableList),
  findall((Var,Table,Attribute),
      (member(dict(Var,Table,Attribute,_Type,all),Dict),
       \+ member(dict(Var,_,_,_,_),FunctionVariableList)
      ),
      FreeVarList).


% --- function_variable_list(FunctionVariable,FunctionVariableList) --------------------
%
% extracts the list of variables which occur in the function variable term
%
% RESTRICTION: FunctionVariable may only contain one single variable.
% 
% --------------------------------------------------------------------------------------

function_variable_list('$var$'(VarId),[VarId]).




% --- translate_free_vars(FreeVars,SQLGroup) -------------------------------------------
%
% translates dictionary information on free variables to SQLGroup of aggregate
% function query
% 
% --------------------------------------------------------------------------------------

translate_free_vars([],[]).
translate_free_vars([(_VarId,Table,Attribute)|FreeVars],[att(Table,Attribute)|SQLGroups]):-
   translate_free_vars(FreeVars,SQLGroups).




% --- evaluable_expression(Connection,ExpressionTerm,Dictionary,Expression,Type) --------------------
%
% evaluable_expression constructs SQL arithmetic expressions with qualified attribute names
% from the Prolog arithmetic expression term and the information stored in the dictionary.
%
% The type of an evaluable function is returned in the argument Type.
%
% The dictionary is not changed because it is used for lookup only. 
% 

evaluable_expression(Connection,AggregateFunctionTerm,Dictionary,AggregateFunctionExpression,number):-
   aggregate_function(Connection,AggregateFunctionTerm,Dictionary,AggregateFunctionExpression).

evaluable_expression(Connection,LeftExp + RightExp,Dictionary,LeftAr + RightAr,number):-
   evaluable_expression(Connection,LeftExp,Dictionary,LeftAr,number),
   evaluable_expression(Connection,RightExp,Dictionary,RightAr,number).

evaluable_expression(Connection,LeftExp - RightExp,Dictionary,LeftAr - RightAr,number):-
   evaluable_expression(Connection,LeftExp,Dictionary,LeftAr,number),
   evaluable_expression(Connection,RightExp,Dictionary,RightAr,number).

evaluable_expression(Connection,LeftExp * RightExp,Dictionary,LeftAr * RightAr,number):-
   evaluable_expression(Connection,LeftExp,Dictionary,LeftAr,number),
   evaluable_expression(Connection,RightExp,Dictionary,RightAr,number).

evaluable_expression(Connection,LeftExp / RightExp,Dictionary,LeftAr / RightAr,number):-
   evaluable_expression(Connection,LeftExp,Dictionary,LeftAr,number),
   evaluable_expression(Connection,RightExp,Dictionary,RightAr,number).

evaluable_expression(Connection,ArithExp1,Dictionary,ArithAr1,number):-
   ArithExp1=..[F,Exp],
   arithmetic_function(F,_,_,_,_,1),
   evaluable_expression(Connection,Exp,Dictionary,Ar,number),
   ArithAr1=..[F,Ar].

evaluable_expression(_Connection,'$var$'(VarId),Dictionary,att(RangeVar,Attribute),Type):-
   lookup(VarId,Dictionary,RangeVar,Attribute,Type),
   RangeVar \= (is).

evaluable_expression(_Connection,'$var$'(VarId),Dictionary,ArithmeticExpression,Type):-
   lookup(VarId,Dictionary,is,ArithmeticExpression,Type).

evaluable_expression(_Connection,'$const$'(Const),_,'$const$'(Const),ConstType):-
   dx_get_type('$const$'(Const),ConstType).





% % --------------------------------------------------------------------------------------
% %
% % Output to screen predicates - rather crude at the moment
% %
% % --------------------------------------------------------------------------------------


% % --- printqueries(Code) ---------------------------------------------------------------

% printqueries([Query]):-
%    nl,
%    print_query(Query),
%    write(';'),
%    nl,
%    nl.

% printqueries([Query|Queries]):-
%    \+ (Queries = []),
%    nl,
%    print_query(Query),
%    nl,
%    write('UNION'),
%    nl,
%    printqueries(Queries).



% % --- print_query(QueryCode) -----------------------------------------------------------

% print_query(query([agg_query(Function,Select,From,Where,Group)],_,_)):-
%    % --- ugly rule here: aggregate function only in SELECT Part of query ----
%    !,
%    print_query(agg_query(Function,Select,From,Where,Group)).

% print_query(query(Select,From,Where)):-
%    print_clause('SELECT',Select,','),
%    nl,
%    print_clause('FROM',From,','),
%    nl,
%    print_clause('WHERE',Where,'AND'),
%    nl.

% print_query(agg_query(Function,Select,From,Where,Group)):-
%    print_clause('SELECT',Function,Select,','),
%    nl,
%    print_clause('FROM',From,','),
%    nl,
%    print_clause('WHERE',Where,'AND'),
%    nl,
%    print_clause('GROUP BY',Group,',').

% print_query(negated_existential_subquery(Select,From,Where)):-
%    write('NOT EXISTS'),
%    nl,
%    write('('),
%    print_clause('SELECT',Select,','),
%    nl,
%    print_clause('FROM',From,','),
%    nl,
%    print_clause('WHERE',Where,'AND'),
%    nl,
%    write(')').




% % --- print_clause(Keyword,ClauseCode,Separator) ---------------------------------------
% %
% % with 
% % Keyword    one of SELECT, FROM, WHERE, or GROUP BY, 
% % ClauseCode the code corresponding to the appropriate clause of an SQL query, and 
% % Separator  indicating the character(s) through which the items of a clause
% %            are separated from each other (',' or 'AND').
% % 
% % --------------------------------------------------------------------------------------

% print_clause(_Keyword,[],_).

% print_clause(Keyword,[Column|RestColumns],Separator):-
%    write(Keyword),
%    write(' '),
%    print_clause([Column|RestColumns],Separator).

% print_clause(Keyword,Function,[Column],Separator):-
%    write(Keyword),
%    write(' '),
%    write(Function),
%    write('('),
%    print_clause([Column],Separator),
%    write(')').





% % --- print_clause(ClauseCode,Separator) -----------------------------------------------

% print_clause([Item],_):-
%    print_column(Item).

% print_clause([Item,NextItem|RestItems],Separator):-
%    print_column(Item),
%    write(' '),
%    write(Separator),
%    write(' '),
%    print_clause([NextItem|RestItems],Separator).




% % --- print_column(ColumnCode) --------------------------------

% print_column('*'):-
%    write('*').

% print_column(att(RangeVar,Attribute)):-
%    write(RangeVar),
%    write('.'),
%    write(Attribute).

% print_column(rel(Relation,RangeVar)):-
%    write(Relation),
%    write(' '),
%    write(RangeVar).

% print_column('$const$'(String)):-
%    dx_get_type('$const$'(String),string),
%    %FSP. Changed string delimiters to commit to SQL syntax
%    write(''''),
%    write(String),
%    write('''').

% print_column('$const$'(Number)):-
%    dx_get_type('$const$'(Number),NumType),
%    type_compatible(NumType,number),
%    write(Number).

% print_column(comp(LeftArg,Operator,RightArg)):-
%    print_column(LeftArg),
%    write(' '),
%    write(Operator),
%    write(' '),
%    print_column(RightArg).

% print_column(LeftExpr * RightExpr):-
%    print_column(LeftExpr),
%    write('*'),
%    print_column(RightExpr).

% print_column(LeftExpr / RightExpr):-
%    print_column(LeftExpr),
%    write('/'),
%    print_column(RightExpr).

% print_column(LeftExpr + RightExpr):-
%    print_column(LeftExpr),
%    write('+'),
%    print_column(RightExpr).

% print_column(LeftExpr - RightExpr):-
%    print_column(LeftExpr),
%    write('-'),
%    print_column(RightExpr).

% print_column(agg_query(Function,Select,From,Where,Group)):-
%    nl,
%    write('('),
%    print_query(agg_query(Function,Select,From,Where,Group)),
%    write(')').

% print_column(negated_existential_subquery(Select,From,Where)):-
%    print_query(negated_existential_subquery(Select,From,Where)).





% --- queries_atom(Queries,QueryAtom) ---------------------------- 
%
% queries_atom(Queries,QueryAtom) returns in its second argument
% the SQL query as a Prolog atom. For efficiency reasons, a list
% of ASCII codes is ceated as a difference list, and it is then 
% transformed to an atom by name/2
% ---------------------------------------------------------------- 

%FSP: Added Connection so that queries_atom/2 becomes queries_atom/3
queries_atom(Connection,Queries,QueryAtom):-
   queries_atom(Connection,Queries,QueryList,[]),
   name(QueryAtom,QueryList).



queries_atom(Connection,[Query],QueryList,Diff):-
   my_query_atom(Connection,Query,QueryList,Diff).

queries_atom(Connection,[Query|Queries],QueryList,Diff):-
   Queries \= [],
   my_query_atom(Connection,Query,QueryList,X1),
   column_atom(Connection,'UNION',X1,X2),
   queries_atom(Connection,Queries,X2,Diff).

% FSP. Added:
my_query_atom(_Connection,Query,_QueryList,_Diff) :-
  var(Query),
  !,
  write_error_log(['Unsafe query: ',Query,'. Cannot translate to SQL.']),
  fail.
my_query_atom(Connection,Query,QueryList,Diff) :-
  query_atom(Connection,Query,QueryList,Diff).
  
% --- query_atom(QueryCode) --------------------------------

query_atom(Connection,query([agg_query(Function,Select,From,Where,Group)],_,_),QueryList,Diff):-
   % --- ugly rule here: aggregate function only in SELECT Part of query ----
   !,
   query_atom(Connection,agg_query(Function,Select,From,Where,Group),QueryList,Diff).

query_atom(Connection,query(Select,[rel(dual,_)],_Where),QueryList,Diff):-
   from_less_connection(Connection),
   !,
   clause_atom(Connection,'SELECT',Select,',',QueryList,Diff).

query_atom(Connection,query(Select,From,Where),QueryList,Diff):-
   clause_atom(Connection,'SELECT',Select,',',QueryList,X1),
   clause_atom(Connection,'FROM',From,',',X1,X2),
   clause_atom(Connection,'WHERE',Where,'AND',X2,Diff).

query_atom(Connection,agg_query(Function,Select,From,Where,Group),QueryList,Diff):-
   clause_atom(Connection,'SELECT',Function,Select,',',QueryList,X1),
   clause_atom(Connection,'FROM',From,',',X1,X2),
   clause_atom(Connection,'WHERE',Where,'AND',X2,X3),
   clause_atom(Connection,'GROUP BY',Group,',',X3,Diff).

query_atom(Connection,negated_existential_subquery(Select,From,Where),QueryList,Diff):-
   column_atom(Connection,'NOT EXISTS(',QueryList,X1),   
   clause_atom(Connection,'SELECT',Select,',',X1,X2),
   clause_atom(Connection,'FROM',From,',',X2,X3),
   clause_atom(Connection,'WHERE',Where,'AND',X3,X4),
   column_atom(Connection,')',X4,Diff).

% % FSP. Added this clause to support the translation of top/2
% query_atom(Connection,top_query(N,Select,From,Where),QueryList,Diff):-
%    top_N_clause(Connection,N,SELECT,END),
%    clause_atom(Connection,SELECT,Select,',',QueryList,X1),
%    clause_atom(Connection,'FROM',From,',',X1,X2),
%    clause_atom(Connection,'WHERE',Where,'AND',X2,X3),
%    column_atom(Connection,END,X3,Diff).

% % FSP. Added this clause to support the translation of top/2
% % Syntax depends on concrete DBMS
% top_N_clause(Connection,'$const$'(N),SELECT,'') :-
%    my_odbc_get_dbms(Connection,DBMS),
%    memberchk(DBMS,[access,sqlserver,sqlanywhere]),
%    atomic_concat('SELECT TOP ',N,SELECT).
% top_N_clause(_Connection,'$const$'(N),'SELECT',END) :-
%    atomic_concat_list(['FETCH FIRST ',N,' ROWS ONLY'],END).


% --- clause_atom(Connection,Keyword,ClauseCode,Junctor,CurrAtom,QueryAtom) -------------
%
% with 
% Keyword    one of SELECT, FROM, WHERE, or GROUP BY, 
% ClauseCode the code corresponding to the appropriate clause of an SQL query, and 
% Junctor    indicating the character(s) through which the items of a clause
%            are separated from each other (',' or 'AND').

% FSP: Added first clause to deal with FROM-less statements and DUAL table
clause_atom(Connection,'FROM',[],Junctor,QueryList,Diff) :-
  non_from_less_connection(Connection),
  !,
  clause_atom(Connection,'FROM',['DUAL'],Junctor,QueryList,Diff).
%  clause_atom(Connection,'FROM',['dual'],Junctor,QueryList,Diff). % FSP: For Sybase ASE
  
clause_atom(_Connection,_Keyword,[],_,QueryList,QueryList).

clause_atom(Connection,Keyword,[Column|RestColumns],Junctor,QueryList,Diff):-
   column_atom(Connection,Keyword,QueryList,X1),
   column_atom(Connection,' ',X1,X2),
   clause_atom(Connection,[Column|RestColumns],Junctor,X2,X3),
   column_atom(Connection,' ',X3,Diff).

clause_atom(Connection,Keyword,Function,[Column],Junctor,QueryList,Diff):-
   column_atom(Connection,Keyword,QueryList,X1),
   column_atom(Connection,' ',X1,X2),
   column_atom(Connection,Function,X2,X3),
   column_atom(Connection,'(',X3,X4),
   clause_atom(Connection,[Column],Junctor,X4,X5),
   column_atom(Connection,') ',X5,Diff).

from_less_connection(Connection) :-
  \+ non_from_less_connection(Connection).
  
non_from_less_connection(_Connection).
% non_from_less_connection(Connection) :-
%   my_odbc_get_dbms(Connection,DBMS),
%   memberchk(DBMS,[access,db2,oracle]).
  
% Access     : (partially) from-less (dual is not defined)
% MySQL      : from-less
% PostgreSQL : from-less
% SQL Server : from-less
% Sybase     : from-less
% DB2        : non-from-less (dual is not defined)
% Oracle     : non-from-less (dual is defined)

% --- clause_atom(Connection,ClauseCode,Junctor) --------------------------------

clause_atom(Connection,[Item],_,QueryList,Diff):-
   column_atom(Connection,Item,QueryList,Diff).

clause_atom(Connection,[Item,NextItem|RestItems],Junctor,QueryList,Diff):-
   column_atom(Connection,Item,QueryList,X1),
   column_atom(Connection,' ',X1,X2),
   column_atom(Connection,Junctor,X2,X3),
   column_atom(Connection,' ',X3,X4),
   clause_atom(Connection,[NextItem|RestItems],Junctor,X4,Diff).



column_atom(Connection,att(RangeVar,Attribute),QueryList,Diff):-
   column_atom(Connection,RangeVar,QueryList,X1),
   column_atom(Connection,'.',X1,X2),
   column_atom(Connection,Attribute,X2,Diff).

% column_atom(Connection,rel(dual,_RangeVar),QueryList,Diff):-
%    from_less_connection(Connection),
%    !,
%    column_atom(Connection,' ',QueryList,Diff).

column_atom(Connection,rel(Relation,RangeVar),QueryList,Diff):-
   %FSP. Added delimiters to relation identifiers
   delimited_sql_identifier(Connection,Relation,StrDelimitedRelation),
   atom_codes(DelimitedRelation,StrDelimitedRelation),
   column_atom(Connection,DelimitedRelation,QueryList,X1),
   column_atom(Connection,' ',X1,X2),
   column_atom(Connection,RangeVar,X2,Diff).

column_atom(Connection,'$const$'(String),QueryList,Diff):-
   dx_get_type('$const$'(String),string),
   %FSP. Changed string delimiters to commit to SQL syntax
   column_atom(Connection,'''',QueryList,X1),
   column_atom(Connection,String,X1,X2),
   column_atom(Connection,'''',X2,Diff).

column_atom(Connection,'$const$'(Number),QueryList,Diff):-
   dx_get_type('$const$'(Number),NumType),
   type_compatible(NumType,number),
   column_atom(Connection,Number,QueryList,Diff).

column_atom(Connection,comp(LeftArg,Operator,RightArg),QueryList,Diff):-
   column_atom(Connection,LeftArg,QueryList,X1),
   column_atom(Connection,' ',X1,X2),
   column_atom(Connection,Operator,X2,X3),
   column_atom(Connection,' ',X3,X4),
   column_atom(Connection,RightArg,X4,Diff).

column_atom(Connection,LeftExpr * RightExpr,QueryList,Diff):-
   column_atom(Connection,LeftExpr,QueryList,X1),
   column_atom(Connection,'*',X1,X2),
   column_atom(Connection,RightExpr,X2,Diff).

column_atom(Connection,LeftExpr + RightExpr,QueryList,Diff):-
   column_atom(Connection,LeftExpr,QueryList,X1),
   column_atom(Connection,'+',X1,X2),
   column_atom(Connection,RightExpr,X2,Diff).

column_atom(Connection,LeftExpr - RightExpr,QueryList,Diff):-
   column_atom(Connection,LeftExpr,QueryList,X1),
   column_atom(Connection,'-',X1,X2),
   column_atom(Connection,RightExpr,X2,Diff).

column_atom(Connection,LeftExpr / RightExpr,QueryList,Diff):-
   column_atom(Connection,LeftExpr,QueryList,X1),
   column_atom(Connection,'/',X1,X2),
   column_atom(Connection,RightExpr,X2,Diff).

% FSP. Added next clause to support arithmetic functions
column_atom(Connection,ArithFun1,QueryList,Diff):-
   ArithFun1=..[F,A],
   column_atom(Connection,F,QueryList,X1),
   column_atom(Connection,'(',X1,X2),
   column_atom(Connection,A,X2,X3),
   column_atom(Connection,')',X3,Diff).
   
column_atom(Connection,agg_query(Function,Select,From,Where,Group),QueryList,Diff):-
   column_atom(Connection,'(',QueryList,X1),
   my_query_atom(Connection,agg_query(Function,Select,From,Where,Group),X1,X2),
   column_atom(Connection,')',X2,Diff).

column_atom(Connection,negated_existential_subquery(Select,From,Where),QueryList,Diff):-
   my_query_atom(Connection,negated_existential_subquery(Select,From,Where),QueryList,Diff).

% % FSP. Added next clause to support top-N queries
% column_atom(Connection,top_query(N,Select,From,Where),QueryList,Diff):-
%    column_atom(Connection,'(',QueryList,X1),
%    my_query_atom(Connection,top_query(N,Select,From,Where),X1,X2),
%    column_atom(Connection,')',X2,Diff).

% % FSP. Added next clause to support subqueries in FROM
% column_atom(Connection,query(Select,From,Where),QueryList,Diff):-
%    column_atom(Connection,'(',QueryList,X1),
%    my_query_atom(Connection,query(Select,From,Where),X1,X2),
%    column_atom(Connection,')',X2,Diff).

column_atom(_Connection,Atom,List,Diff):-
% FSP. Changed to atomic(Atom) to allow numbers in the projection list.
%   atom(Atom) 
   atomic(Atom),
   name(Atom,X1),
   append(X1,Diff,List).






% --- gensym(Root,Symbol) ----------------------------------------------------
%
% SEPIA 3.2. version - other Prolog implementations provide gensym/2
% and init_gensym/1 as built-ins. */
%
% (C) Christoph Draxler, Aug. 1992
%
% 

gensym(Root,Symbol):-
   nonvar(Root),
   var(Symbol),
   atom_concat('$',Root,DollarRoot),
   get_flag(DollarRoot,Counter),
   NewCounter is Counter + 1,
   my_atom_number(Atom,NewCounter),
   atom_concat(Root,Atom,Symbol),
   set_flag(DollarRoot,NewCounter).
   

init_gensym(Root):-
   nonvar(Root),
   atom_concat('$',Root,DollarRoot),
   set_flag(DollarRoot,0).

%:- dynamic(var/1).
%:- dynamic(rel/1).

% --- auxiliary predicates (some of them may be built-in... --------------------

% append([],L,L).
% append([H1|L1],L2,[H1|L3]):-
%    append(L1,L2,L3).



% member(X,[X|_]).
% member(X,[_|T]):-
%    member(X,T).



repeat_n(N):-
   integer(N),
   N > 0,
   repeat_1(N).

repeat_1(1):-!.
repeat_1(_).
repeat_1(N):-
   N1 is N-1,
   repeat_1(N1).



% % --- benchmarking programs --------------------------------------------
% %
% % taken from R. O'Keefe: The Craft of Prolog, MIT Press 1990
% %
% % Sepia Prolog version

% my_cpu_time(Time):-
%    my_get_time(Time).


% my_cpu_time(Goal,Duration):-
%    !,
%    my_get_time(T1),
%    (call(Goal) -> true; true),
%    my_get_time(T2),
%    Duration is T2 - T1.

% my_cpu_time(N,Goal,Duration):-
%    !,
%    my_cpu_time((repeat_n(N),(Goal -> fail);true),D1),
%    my_cpu_time((repeat_n(N),(true -> fail);true),D2),
%    Duration is D1 - D2.



% % --- set_difference(SetA,SetB,Difference) --------------------------------------------
% %
% % SetA - SetB = Difference

% set_difference([],_,[]).

% set_difference([Element|RestSet],Set,[Element|RestDifference]):-
%    \+ member(Element,Set),
%    set_difference(RestSet,Set,RestDifference).

% set_difference([Element|RestSet],Set,RestDifference):-
%    member(Element,Set),
%    set_difference(RestSet,Set,RestDifference).


% % --- benchmarks of sample queries ---------

% benchmark(N,1,D):-
%    my_cpu_time(N,
%      (translate(flight(No,Dep,Dest,Type),flight(No,Dep,Dest,Type),Code),
%       nl, write(Code), nl,
% 	    printqueries(Code)),
%    D).

% benchmark(N,2,D):-
%    my_cpu_time(N,
%      (translate(capacity(No,Dep,Dest,Type,Seats),
% 	    (flight(No,Dep,Dest,Type),
% 		 plane(Type,Seats),
% 		 Type='b-737'),Code),
% 	   printqueries(Code)),
%    D).

% benchmark(N,3,D):-
%    my_cpu_time(N,
%       (translate(no_planes(No,Dep,Dest,Type),
% 	      (flight(No,Dep,Dest,Type),
% 		     not(plane(Type,_Seats))),Code),
% 	   printqueries(Code)),
% 	D).

% benchmark(N,4,D):-
%    my_cpu_time(N,(translate(X,X is count(S,plane(_P,S)),Code),printqueries(Code)),D).

% benchmark(N,5,D):-
%    my_cpu_time(N,
%       (translate(big_planes(munich,Dest,Type,Seats),
% 	      FNo^(flight(FNo,munich,Dest,Type),
% 		       plane(Type,Seats),
% 			   Seats > avg(S, T^plane(T,S))),Code),
% 	  printqueries(Code)),
%    D).

% benchmark(N,6,D):-
%    my_cpu_time(N,(
%      translate(big_planes(munich,Dest,Type,Seats),
% 	     FNo^(flight(FNo,munich,Dest,Type),
% 		      plane(Type,Seats),
% 			  Seats > avg(S, T^plane(T,S))),Code),
% 		 printqueries(Code)),
%    D).

% benchmark(N,7,D):-
%    my_cpu_time(N,(
%      translate(big_planes(munich,Dest,Type,Seats),
% 	     FNo^(flight(FNo,munich,Dest,Type),
% 		      plane(Type,Seats),
% 			  Seats > avg(S, T^plane(T,S))),Code),
% 		 queries_atom(Code,SQLQueryAtom),
% 		 writeq(query_atom(SQLQueryAtom)),
% 		 nl),
%    D).
%    
% % FSP. WARNING: Variables occurring once in head makes translate_projection fail  
% % Should raise an error
% benchmark(N,8,D):-
%    my_cpu_time(N,(
%      translate(
%        eu_flights(_Dep,Dest),
%        %
% 	     flight(_FNo,munich,Dest,_Type)
% 	     ,
% 			 %
% 			 Code),
% 		 printqueries(Code)
%   	),
%    D).

% % EXISTENTIAL variables: 
% benchmark(N,9,D):-
%    my_cpu_time(N,(
%      translate(
%        eu_flights(Dep,Dest),
%        %
% 	     flight(_FNo,Dep,Dest,_Type)
% 	     ,
% 			 %
% 			 Code),
% 		 printqueries(Code)
%   	),
%    D).

% % UNION:
% benchmark(N,10,D):-
%    my_cpu_time(N,(
%      translate(
%        eu_flights(Dep,Dest),
%        %
% 	     (flight(FNo,Dep,Dest,'b-747')
% 	      ;
% 	      flight(FNo,Dep,Dest,'a-320')
% 	     )
% 	     ,
% 			 %
% 			 Code),
% 		 printqueries(Code)
%   	),
%    D).

% % RECURSION:
% benchmark(N,11,D):-
%    my_cpu_time(N,(
%      translate(
%        travel(Dep,Dest),
%        %
% 	     (flight(_FNo,Dep,FDest,_Type)
% 	      ;
% 	      travel(FDest,Dest)
% 	     )
% 	     ,
% 			 %
% 			 Code),
% 		 printqueries(Code)
%   	),
%    D).

   
   


% --- Meta Database for schema definition of SQL DB in Prolog --------------------------
%
% maps Prolog predicates to SQL table names, Prolog predicate argument positions to SQL
% attributes, and Prolog operators to SQL operators. 
%
% ATTENTION! It is assumed that the arithmetic operators in Prolog and SQL are the same,
% i.e. + is addition in Prolog and in SQL, etc. If this is not the case, then a mapping
% function for arithmetic operators is necessary too.
% --------------------------------------------------------------------------------------


% --- dx_relation(+PrologFunctor,+Arity,-SQLTableName) ---------------------------------------
% FSP: Added Connection

dx_relation(Connection,Name,Arity,Name) :-
  my_table(Connection,Name,Arity),
  !.
dx_relation(_Connection,Name,Arity,Name) :-
  my_table('$des',Name,Arity), % Not yet created in the external DBMS
  !.
dx_relation(_Connection,dual,1,dual). % Dual table
%  exist_relation(Connection,Name,Arity).
%dx_relation(Name,Arity,Name) :-
%  my_table(_DB,Name,Arity).
  
% dx_relation(flight,4,'FLIGHT').
% dx_relation(plane,2,'PLANE').


% --- dx_attribute(PrologArgumentPosition,SQLTableName,SQLAttributeName) ------------------
% FSP: Added Connection and handling of external data types
dx_attribute(Connection,Position,RelationName,AttributeName,DXDataType) :-
  (my_attribute(Connection,Position,RelationName,AttributeName,DataType),
   !
   ;
   my_attribute('$des',Position,RelationName,AttributeName,DataType)
  ),
  (is_des_type(DataType)
   ->
    DESDataType=DataType
   ;
    type_equivalence(DESDataType,_,DataType)
  ),
  dx_des_datatype(DXDataType,DESDataType),
  !.
dx_attribute(_Connection,1,dual,a,integer).

is_des_type(Type) :-
  dx_des_datatype(_,Type).
    
% -- Type conversions:
dx_des_datatype(number,number(float)).
dx_des_datatype(integer,number(integer)).
dx_des_datatype(real,number(float)).
dx_des_datatype(string,string(varchar)).
dx_des_datatype(string,string(varchar(_))).
%dx_des_datatype(natural,number(integer)).

% dx_attribute(1,'FLIGHT','FLIGHT_NO',string).
% dx_attribute(2,'FLIGHT','DEPARTURE',string).
% dx_attribute(3,'FLIGHT','DESTINATION',string).
% dx_attribute(4,'FLIGHT','PLANE_TYPE',string).

% dx_attribute(1,'PLANE','TYPE',string).
% dx_attribute(2,'PLANE','SEATS',integer).


% --- Mapping of Prolog operators to SQL operators -------------------------------------

comparison(=,=).
comparison(<,<).
comparison(>,>).
comparison(@<,<).
comparison(@>,>).
% FSP. Added:
comparison(=<,<=).
comparison(>=,>=).
comparison(\=,'<>').



negated_comparison(=,'<>').
negated_comparison(\=,=).
negated_comparison(>,=<).
negated_comparison(=<,>).
negated_comparison(<,>=).
negated_comparison(>=,<).


% --- aggregate_function(PrologFunctor,SQLFunction) -----------------

aggregate_functor(avg,'AVG').
aggregate_functor(min,'MIN').
aggregate_functor(max,'MAX').
aggregate_functor(sum,'SUM').
aggregate_functor(count,'COUNT').



% --- type system --------------------------------------------------------------
%
% A rudimentary type system is provided for consistency checking during the
% translation and for output formatting
%
% The basic types are string and number. number has the subtypes integer and
% real.
%
% ------------------------------------------------------------------------------


type_compatible(Type,Type):-
   dx_is_type(Type).
type_compatible(SubType,Type):-
   subtype(SubType,Type).
type_compatible(Type,SubType):-
   subtype(SubType,Type).


% --- subtype(SubType,SuperType) -----------------------------------------------
%
% Simple type hierarchy checking
%
% ------------------------------------------------------------------------------

subtype(SubType,SuperType):-
   is_subtype(SubType,SuperType).

subtype(SubType,SuperType):-
   is_subtype(SubType,InterType),
   subtype(InterType,SuperType).



% --- dx_is_type(Type) ------------------------------------------------------------
%
% Type names
%
% ------------------------------------------------------------------------------

dx_is_type(number).
dx_is_type(integer).
dx_is_type(real).
dx_is_type(string).
dx_is_type(natural).


% --- is_subtype(SubType,SuperType) --------------------------------------------
%
% Simple type hierarchy for numeric types
%
% ------------------------------------------------------------------------------

is_subtype(integer,number).
is_subtype(real,number).
is_subtype(natural,integer).


% --- dx_get_type(Constant,Type) --------------------------------------------------
%
% Prolog implementation specific definition of type retrieval
% sepia Prolog version given here
%
% ------------------------------------------------------------------------------

% FSP:
% dx_get_type('$const$'(Constant),integer):-
%    number(Constant).
dx_get_type('$const$'(Constant),number):-
   number(Constant).

dx_get_type('$const$'(Constant),string):-
   atom(Constant).


/* END OF PROLOG TO SQL COMPILER pl2sql (adapted)        */
/*********************************************************/

