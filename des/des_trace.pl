/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.4.1                 */
/*                                                       */
/*    Datalog and SQL tracers                            */
/*                                                       */
/*                                                       */
/*                                   Yolanda Garcia-Ruiz */
/*                               Rafael Caballero-Roldan */
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tracing 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tracing Datalog predicates

trace_datalog(Query,NVs,Ordering) :-
  processC(clear_et,[],_NVs,_Yes),
  trace_solve_datalog(Query,NVs),
  functor(Query,Predicate,Arity),
  trace(datalog,Predicate/Arity,Ordering).
 
trace_solve_datalog(Query,NVs) :-
  push_flag(output,off,Output),
  solve_datalog_query_complete_fill(Query,NVs),
  pop_flag(output,Output).
  
  
% Tracing SQL Views
 
trace_sql(ViewName,Ordering) :-
  (view_arity(ViewName,Arity)
   ->
    (current_db('$des')  % Fill ET for a local database
     ->
      length(Args,Arity),
      Query=..[ViewName|Args],
      my_term_to_string(Query,QueryStr),
      push_flag(output,off,Output),
      process_datalog(QueryStr),
      pop_flag(output,Output)
     ;
      true
    ),
    trace(sql,ViewName/Arity,Ordering)
  ;
%    write_error_log(['View ''',ViewName,''' does not exist.']),
%    display_view_alternatives(ViewName)
   my_raise_exception(unknown_view(ViewName),syntax(''),[])
  ).

 
trace(datalog,Name/Arity,Ordering) :-
  pdg(PDG),
  sub_pdg(Name/Arity,PDG,SubPDG),
  (development(on)
   ->
    RSubPDG=SubPDG
   ;
    remove_nonuser_preds_from_pdg(SubPDG,RSubPDG)
  ),
  trace_pdg(datalog,Name/Arity,RSubPDG,Ordering).
trace(sql,Name/Arity,Ordering) :-
  rdg(PDG),
  sub_pdg(Name/Arity,PDG,SubPDG),
%  remove_nonrel_preds_from_pdg(RSubPDG,RNSubPDG),
  trace_pdg(sql,Name/Arity,SubPDG,Ordering).
  
trace_pdg(Language,Name/Arity,PDG,Ordering) :-
  pdg_to_pdt(Name/Arity,PDG,PDT),
  pdt_traverse_order(Ordering,PDT,DupOrderedNodes),
  remove_duplicates(DupOrderedNodes,OrderedNodes),
  trace_nodes(Language,OrderedNodes).
  
  
% Find an order for visiting nodes  
pdt_traverse_order(postorder,node(N/A,[]),[N/A]) :-
  !.
pdt_traverse_order(postorder,node(N/A,Children),NAs) :-
  pdt_traverse_order_list(postorder,Children,TNAs),
  append(TNAs,[N/A],NAs).

pdt_traverse_order(preorder,node(N/A,[]),[N/A]) :-
  !.
pdt_traverse_order(preorder,node(N/A,Children),[N/A|NAs]) :-
  pdt_traverse_order_list(preorder,Children,NAs).

pdt_traverse_order_list(postorder,[],[]).
pdt_traverse_order_list(postorder,[PDT|PDTs],NAs) :-
  pdt_traverse_order(postorder,PDT,NAs1),
  pdt_traverse_order_list(postorder,PDTs,NAs2),
  append(NAs1,NAs2,NAs).

pdt_traverse_order_list(preorder,[],[]).
pdt_traverse_order_list(preorder,[PDT|PDTs],NAs) :-
  pdt_traverse_order(preorder,PDT,NAs1),
  pdt_traverse_order_list(preorder,PDTs,NAs2),
  append(NAs1,NAs2,NAs).

filter_views_and_tables([],[]).
filter_views_and_tables([N/A|NAs],[N/A|Vs]) :-
  (view_arity(N,A)
   ;
   table_arity(N,A)
  ),
  !,
  filter_views_and_tables(NAs,Vs).
filter_views_and_tables([_NA|NAs],Vs) :-
  filter_views_and_tables(NAs,Vs).

% filter_sources([],[]).
% filter_sources([N/A|NAs],[N/A|Vs]) :-
%   length(Args,A),
%   Head=..[N|Args],
%   (Rule=Head ; Rule=':-'(Head,_Body)),
%   (datalog(Rule,_,_,_,_,_,source),
%    !
%    ;
%    \+ (datalog(Rule,_,_,_,_,_,_)),
%    \+ (my_builtin_pred(N)),
%    !
%   ),
%   filter_sources(NAs,Vs).
% filter_sources([_NA|NAs],Vs) :-
%   filter_sources(NAs,Vs).

trace_nodes(Language,Nodes) :-
  tapi(on),
  !,
  (Language==sql -> my_unzip(Nodes,List,_) ; List=Nodes),
  write_list_log(List),
  write_tapi_eot.
trace_nodes(Language,[]) :-
  node_type(Language,NodeType),
  write_info_log(['No more ',NodeType,'s to trace.']).
trace_nodes(Language,[N/A|NAs]) :-
  node_type(Language,NodeType),
  write_info_log(['Tracing ',NodeType,' ''',N,'''.']),
  ((Language==sql, verbose(on))
   ->
    current_db(Connection),
    list_schema_list(Connection,[N]) 
   ;
    true),
  trace_list_answers(Language,N/A),
  (NAs==[] ->
    trace_nodes(Language,[])
   ;
    write_info_log(['Remaining ',NodeType,'s: ',NAs]),
    write_log_list(['Input: Continue? (y/n) [y]: ']),
    user_input_string(Str),
    ((Str=[] ; Str=="y" ; Str=="Y") ->
      trace_nodes(Language,NAs)
      ;
      true
    )
  ).
  
node_type(Language,NodeType) :-  
  (Language==sql ->
    NodeType=view
   ;
    NodeType=predicate).

trace_list_answers(datalog,N/A) :-
  list_et_answers(N/A).
trace_list_answers(sql,N/A) :-
  functor(Query,N,A),
  trace_solve_datalog(Query,[]),
  list_et_answers(N/A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ancillary stuff for tracing Datalog and SQL queries 
% and debugging SQL views  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate dependency graph to Relation (views or tables) dependency tree
pdg_to_rdt(ViewName/Arity,PDG,RDT) :-
  pdg_to_pdt(ViewName/Arity,PDG,PDT),
  pdt_to_rdt(PDT,RDT).
  

% Predicate dependency graph to Predicate dependency tree
% Recursive calls do not occur in the tree
pdg_to_pdt(N/A,PDG,PDT) :-
  recursive_predicates_from_pdg(PDG,RNAs),
%  recursive_predicates(RNAs),
  pdg_to_pdt(N/A,PDG,PDT,RNAs,[],_Visited).
  
% Non-empty pdt  
% pdt ::= node(N,nodes)
% nodes ::= [pdt1,...,pdtn] , where pdti, i>=0, are children of N
pdg_to_pdt(N/A,(_NAs,[]),node(N/A,[]),RNAs,V,V) :-
  !,
  \+ ((visited_predicate(N/A,V),
       recursive_predicate(N/A,RNAs))).
pdg_to_pdt(N/A,(NAs,Es),node(N/A,Children),RNAs,Vin,Vout) :-
  my_nf_setof(DN/DA,
    (
     (member(N/A+DN/DA,Es)
       ;
      member(N/A-DN/DA,Es))
     ,
     \+ ((visited_predicate(DN/DA,[N/A|Vin]), 
          recursive_predicate(DN/DA,RNAs)))
    ),
    DNDAs),
  pdg_to_pdt_list(DNDAs,(NAs,Es),Children,RNAs,[N/A|Vin],Vout).

pdg_to_pdt_list([],_PDG,[],_RNAs,V,V).
pdg_to_pdt_list([N/A|NAs],PDG,PDT,RNAs,Vin,Vout) :-
  visited_predicate(N/A,Vin),
  recursive_predicate(N/A,RNAs),
  !,
  pdg_to_pdt_list(NAs,PDG,PDT,RNAs,Vin,Vout).
pdg_to_pdt_list([N/A|NAs],PDG,[PDT|PDTs],RNAs,Vin,Vout) :-  
  pdg_to_pdt(N/A,PDG,PDT,RNAs,Vin,Vout1),
  pdg_to_pdt_list(NAs,PDG,PDTs,RNAs,Vout1,Vout).
  
visited_predicate(N/A,V) :- 
  my_member_chk(N/A,V).
  
  
% PDG = ([a/1,b/2],[a/1+b/2])  -> []
% PDG = ([a/1,b/2],[a/1+a/1])  -> [a/1]
% PDG = ([a/1,b/2],[a/1+b/2,b/2+a/1])  -> [a/1,b/2]
% PDG = ([a/1,b/2,c/3],[a/1+b/2,b/2+a/1])  -> [a/1,b/2]
% PDG = ([a/1,b/2,c/3,d/4,e/5,f/6],[a/1+b/2,a/1+c/3,a/1+d/4,c/3+e/5,d/4+f/6])  -> []
% PDG = ([a/1,b/2,c/3,d/4,e/5,f/6],[a/1+b/2,a/1+c/3,a/1+d/4,c/3+e/5,d/4+f/6,f/6+d/4])  -> [d/4,f/6]
% PDG = ([a/1,b/2,c/3,d/4,e/5,f/6],[a/1+b/2,a/1+c/3,a/1+d/4,c/3+e/5,d/4+f/6,f/6+a/1])  -> [a/1,d/4,f/6]
% Get the recursive predicates in the form [N/A,...] from a PDG
recursive_predicates_from_pdg((NAs,Es),RNAs) :-
  lfp_recursive_predicates_from_pdg(NAs,Es,RNAs).

lfp_recursive_predicates_from_pdg(NAs,Es,RNAs) :-
  recursive_predicates_from_pdg(NAs,Es,NAso,Eso),
  (NAs==NAso ->
    RNAs=NAs
   ;
    lfp_recursive_predicates_from_pdg(NAso,Eso,RNAs)).

recursive_predicates_from_pdg([],Es,[],Es) :-
  !.
% Remove (non-recursive) nodes with no incoming edges  
recursive_predicates_from_pdg([N/A|NAs],Esi,NAso,Eso) :-
  no_incoming_edges(N/A,Esi),
  !,
  remove_edges_from_node(N/A,Esi,Eso1),
  recursive_predicates_from_pdg(NAs,Eso1,NAso,Eso).
recursive_predicates_from_pdg([NA|NAs],Esi,[NA|NAso],Eso) :-
  recursive_predicates_from_pdg(NAs,Esi,NAso,Eso).

no_incoming_edges(N/A,Es) :-
  \+ ((my_member_chk(N/A+_,Es)
       ;
       my_member_chk(N/A-_,Es))).
          
remove_edges_from_node(_NA,[],[]) :-
  !.
remove_edges_from_node(N/A,[_NA+N/A|Esi],Eso) :-
  !,
  remove_edges_from_node(N/A,Esi,Eso).
remove_edges_from_node(N/A,[NAt+NAf|Esi],[NAt+NAf|Eso]) :-
  !,
  remove_edges_from_node(N/A,Esi,Eso).
remove_edges_from_node(N/A,[_NA-N/A|Esi],Eso) :-
  !,
  remove_edges_from_node(N/A,Esi,Eso).
remove_edges_from_node(N/A,[NAt-NAf|Esi],[NAt-NAf|Eso]) :-
  !,
  remove_edges_from_node(N/A,Esi,Eso).
         
  
% pdt_to_rdt(PDT,RDT)

% Node N/A is assumed to be a relation
pdt_to_rdt(node(N/A,Children),node(N/A,RChildren)) :-
  pdt_to_rdt_list(Children,RChildren).

pdt_to_rdt_list([],[]).
pdt_to_rdt_list([node(N/A,Children)|Nodes],[node(N/A,RChildren)|RNodes]) :-
  sql_node_type(N/A,_NodeType),
  !,
  pdt_to_rdt_list(Children,RChildren),
  pdt_to_rdt_list(Nodes,RNodes).
pdt_to_rdt_list([node(_NA,Children)|Nodes],ONodes) :-
  pdt_to_rdt_list(Children,RChildren),
  pdt_to_rdt_list(Nodes,RNodes),
  append(RChildren,RNodes,TNodes),
  remove_duplicates(TNodes,ONodes).


