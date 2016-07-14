/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.4.1                 */
/*                                                       */
/*    Test Case Generator                                */
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

/*********************************************************************/
/* Dynamic Predicates                                                */
/*********************************************************************/

:- dynamic(tc_views/1).     % List of views for which test cases are generated
:- dynamic(tc_tables/1).    % List of tables for which test cases are generated
:- dynamic(tc_size/2).      % Number of tuples generated for each table between min and max
:- dynamic(tc_domain/2).    % Domain for integers, defined as an interval from min to max


/*********************************************************************/
/* Default Parameters                                                */
/*********************************************************************/

tc_size(2,7).
tc_domain(-5,5).


/*********************************************************************/
/* Processing the command /test_case                                 */
/*********************************************************************/

process_test_case(ViewName,Options) :-
  (my_view('$des',ViewName,_A,_S,_Lang,_D,_I,_L,_SC) ->
    (get_tc_options(Options,Class,Action) ->
      compute_test_case(ViewName,Class,Action)
     ;
     write_error_log(['Incorrect option(s): ',Options,'.'])
    )
   ;
    write_error_log(['View ''',ViewName,''' is not defined.']),
    display_view_alternatives(ViewName)
   ).

get_tc_options([],all,display).
get_tc_options([Opt],Class,Action) :-
  (Opt=Class,
   tc_class_option(Class),
   Action=display,
   !
  ;
   Opt=Action,
   tc_action_option(Action),
   Class=all).
get_tc_options([Opt1,Opt2],Class,Action) :-
  (Class=Opt1,
   Action=Opt2,
   tc_class_option(Opt1),
   tc_action_option(Opt2),
   !
  ;
   Class=Opt2,
   Action=Opt1,
   tc_class_option(Opt2),
   tc_action_option(Opt1)).
   
tc_class_option(all).
tc_class_option(positive).
tc_class_option(negative).

tc_action_option(display).
tc_action_option(replace).
tc_action_option(add).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main entry point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_test_case(ViewName,Class,Action) :-
  generate_test_case(ViewName,Class,Dic,TestCase),
  !,
  (var(TestCase),  
   !,
   write_info_log(['The required test case cannot be generated.'])
   ;
	  tcs_to_facts(TestCase,Facts),
		recover_str_ctes_tuples(Facts,Dic,RTestCase),
    (Action==display,
     !,
     write_info_log(['Test case over integers and strings:']),
     write_quoted_log_list([RTestCase,nl])
     ;
     Action==add,
     !,
     rule_to_ruleNVs_list(RTestCase,[],RNVs),
     assert_rules(RNVs,[],datalog,simplify,_,_,_,_)
     ;
     Action==replace,
     !,
     setof(TableName,TC^Args^(member(TC,RTestCase),TC=..[TableName|Args]),TableNames),
     delete_tables(TableNames),
     rule_to_ruleNVs_list(RTestCase,[],RNVs),
     assert_rules(RNVs,[],datalog,simplify,_,_,_,_)
     )
	).

replace_str_ctes(T,_CSs,T) :- 
  (var(T)),
  !.
replace_str_ctes(cte(CS,string(S)),CSs,cte(Index,number(index(string(S))))) :- 
  !,
  my_nth_member(CS,Index,CSs).
replace_str_ctes(C,CSs,RC) :- 
  C =.. [F|As],
  !, 
  replace_str_ctes_list(As,CSs,RAs),
  RC =.. [F|RAs].

replace_str_ctes_list([],_CSs,[]) :-
  !.
replace_str_ctes_list([T|Ts],CSs,[RT|RTs]) :-
  !, 
  replace_str_ctes(T,CSs,RT), 
  replace_str_ctes_list(Ts,CSs,RTs).
   

% recover_str_ctes_tuples(+Tuples,+ConstantStrings,-RecoveredTuples)
% Given a set of tuples where values in string columns have been previously 
% replaced by an index, it returns the tuples with the original string values.
% ConstantStrings is a list of constant strings (CSs). Each CS is mapped
% to the index it occurs in the list (starting from 0 at the head).
  
recover_str_ctes_tuples([],_CSs,[]).
recover_str_ctes_tuples([T|Ts],CSs,[RT|RTs]) :-
  T =.. [TableName|Cols],
  get_table_typed_schema(TableName,TypedTable),
  TypedTable =.. [TableName|TypedArgs],
  recover_str_ctes_cols(Cols,TypedArgs,CSs,RCols),
  RT =.. [TableName|RCols],
  recover_str_ctes_tuples(Ts,CSs,RTs).

% recover_str_ctes_cols(+Columns,+ColumnSchema,+ConstantStrings,-ReplacedColumns)  
% Given a list of columns where values in string columns have been previously 
% replaced by an index, it returns the list of columns with the original string values.
% If the index is out of the bounds of the list, the atom string(<Index>) is created
% as a new constant string
recover_str_ctes_cols([],[],_CSs,[]).
recover_str_ctes_cols([Index|Cols],[_ColName:string(_S)|TypedCols],CSs,[RCol|RCols]) :-
  length(CSs,L),
  Index>=0,
  Index<L,
  !,
  my_nth_member(RCol,Index,CSs),
  recover_str_ctes_cols(Cols,TypedCols,CSs,RCols).
recover_str_ctes_cols([Index|Cols],[_ColName:string(_S)|TypedCols],CSs,[RCol|RCols]) :-
  length(CSs,L),
  Index>=L,
  !,
  append(_, [Last], CSs),
  atom_codes(Last,StrLast),
  ZIndex is Index-L,
  number_codes(ZIndex,StrIndex),
  concat_lists([StrLast,"(",StrIndex,")"],StrRCol),
  atom_codes(RCol,StrRCol),
  recover_str_ctes_cols(Cols,TypedCols,CSs,RCols).
recover_str_ctes_cols([Index|Cols],[_ColName:string(_S)|TypedCols],CSs,[RCol|RCols]) :-
%  length(CSs,L),
%  Index<0,
  !,
  CSs=[First|_],
  (First == '' ->
    write_error_log(['Unable to generate a test case because there is no a string less than ''''.']),
    my_raise_exception(First,bounds,[]),
    !,
    fail
   ;
    true),
  atom_codes(First,[C|Cs]),
  C1 is C-1,
  StrFirst=[C1|Cs],
  tc_domain(Min,_Max),
  ZIndex is Index-Min,
  number_codes(ZIndex,StrZIndex),
  Bound is abs(Min),
  fill_zeroes(StrZIndex,Bound,StrIndex),
  concat_lists([StrFirst,"(-",StrIndex,")"],StrRCol),
  atom_codes(RCol,StrRCol),
  recover_str_ctes_cols(Cols,TypedCols,CSs,RCols).
recover_str_ctes_cols([Col|Cols],[_TypedCol|TypedCols],CSs,[Col|RCols]) :-
  recover_str_ctes_cols(Cols,TypedCols,CSs,RCols).

% recover_str_ctes(Table,_TypedArgs,CSs,CSs,T)
% For recreating views. Maybe not needed
%recover_str_ctes(T,_TypedArgs,CSs,CSs,T) :- 
%  var(T),
%  !.
%recover_str_ctes(cte(Index,number(index(Type))),_TypedArgs,ICSs,OCSs,cte(CS,Type)) :- 
%  !,
%  (my_nth_member(CS,Index,ICSs) ->
%    OCSs=ICSs
%   ;
%    length(ICSs,NIndex),
%    name(NIndex,CSs),
%    atom_codes(CS,CSs),
%    Type=string(_S),
%    append(ICSs,[CS],OCSs)
%   ).
%recover_str_ctes(C,TypedArgs,ICSs,OCSs,RC) :- 
%  C =.. [F|As],
%  !, 
%  recover_str_ctes_list(As,TypedArgs,ICSs,OCSs,RAs),
%  RC =.. [F|RAs].
%recover_str_ctes(T,_TypedArgs,CSs,CSs,T).
%
%recover_str_ctes_list([],_TypedArgs,CSs,CSs,[]) :-
%  !.
%recover_str_ctes_list([T|Ts],TypedArgs,ICSs,OCSs,[RT|RTs]) :-
%  !, 
%  recover_str_ctes(T,TypedArgs,ICSs,ICSs1,RT), 
%  recover_str_ctes_list(Ts,TypedArgs,ICSs1,OCSs,RTs).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% instanceTestCase(+V, -FLt, -FLv)
% generate a positive and negative test cases for the view V in the DB
% schema.
% Variable Instance contains the instance of tables and views 
% (only negative for V)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_test_case(MainV, Class, Dic, TestCase) :-    
	my_abolish(tc_tables/1), 
	my_abolish(tc_views/1), 
	dependencies(MainV, Tctab, Tcviw),
	assertz(tc_tables(Tctab)),
	assertz(tc_views(Tcviw)),
	make_dic(Tcviw, Dic, Error),
	(Error==true ->
	  true
	 ;
    tc_size(N, _),
	  tc(Class, N, MainV, Dic, TestCase)
	).  

tc(all, N, MainV, Dic, TestCase) :- 
  fullTest(N, MainV, Dic, TestCase).             
tc(positive, N, MainV, Dic, TestCase) :- 
  positiveTest(N, MainV,  Dic, TestCase). 
tc(negative, N, MainV, Dic,TestCase) :- 
  negativeTest(N, MainV, Dic, TestCase).      
            
make_dic(ViewNames, Dic, Error) :-    
	my_nf_bagof(SCs,
	           ViewName^A^B^C^D^E^F^
	           (member(ViewName,ViewNames),
	            my_view('$des',ViewName,A,B,C,D,E,F,SCs)),
	           SCss),
	my_flatten(SCss,FSCss),
	length(FSCss,L),
	tc_domain(Min,Max),
	(L =< Max-Min+1 ->
	  remove_duplicates(FSCss,UFSCss),
	  my_mergesort(UFSCss,Dic)
	 ;
	  write_error_log(['Test case domain has not enough cardinality for, at least, ', L, ' string constants.',nl,
	                  '       Extend its bounds with the command /tc_domain <Min> <Max>']),
	  Error=true).

%       
%  antiguo      
dependencies(V, Tc_tables, Ordered_views):- 
                   my_table('$des',V, Arity),
                   pdg(G), 
                   sub_pdg(V/Arity, G, (Nodos,Arcos)), 
                   analizeTables(Nodos, Tables, Tc_views),
                   tablesInFK(Tables, Tc_tables),
% ::WARNING                  filterArcs(Arcos, SubArcs),
                   filterArcs(Arcos, _SubArcs),
                   findall(X, my_table('$des',X, _), List),
                   filterviews(List, Tc_views, Ordered_views ).
                   %orderViews(SubArcs, Tc_views, Ordered_views).
% fin antiguo

filterviews([], _Views, [] ).    
filterviews([V|Resto], Views, [V|L] ):-
  member(V,Views), !, 
  filterviews(Resto, Views, L ).
filterviews([_V|Resto], Views, L ):-
  %member(V,Views), !, 
  filterviews(Resto, Views, L ).


tablesInFK([], []).
tablesInFK(T, OrderedT) :- 
	builtGraphFK(T,  Arcos), 
	builtNodesFK(Arcos, Nodes),
	append(Nodes, T, AllNodes),
	remove_duplicates(AllNodes,N),
	orderViews(Arcos, N, OrderedT).
      
      
      
builtGraphFK([], []).
builtGraphFK([T|R], M) :-
  get_foreign_key(T, L),
	tabFK(L, Tab), 
	appArcs(T, Tab, G),
	append(R, Tab, Laux), 
	builtGraphFK(Laux, Maux), 
	append(G, Maux, M).
	
tabFK([], []).
tabFK([(_L1, T, _L2)|R1], [T|R2]) :-
  tabFK(R1, R2).

appArcs(_T, [], []).
appArcs(T, [T2|R], [T/_+T2/_|Ra]) :- 
            appArcs(T, R, Ra).

builtNodesFK([], []).
builtNodesFK([O/_+D/_|R1], [O,D|R2]) :- builtNodesFK(R1, R2).

% nodos: [v_auth_gbyhav/2,bibauthors/4,vv_02/2,is/2,>= /2,bibuser/4],
% arcos: [v_auth_gbyhav/2+bibauthors/4,vv_02/2+v_auth_gbyhav/2,v_auth_gbyhav/2+is/2,vv_02/2+ >= /2,vv_02/2+bibuser/4]
analizeTables([], [], []).
analizeTables([T/_|Resto], Tc_Tables, [T|RViews]) :- 
             my_view('$des',T,_,_,_,_,_,_,_),  !,
             analizeTables(Resto, Tc_Tables, RViews).
analizeTables([T/_Ar|Resto], [T|RTc_Tables], LV) :- 
             my_table('$des',T, _), !, 
             analizeTables(Resto, RTc_Tables, LV).
analizeTables([_T/_Ar|Resto], Tc_tables, Tc_views) :-   %ni vista ni tabla
             analizeTables(Resto, Tc_tables, Tc_views).
             
% arcos: [v_auth_gbyhav/2+bibauthors/4,vv_02/2+v_auth_gbyhav/2,v_auth_gbyhav/2+is/2,vv_02/2+ >= /2,vv_02/2+bibuser/4]             
filterArcs([], []).


     
filterArcs([F+T|Arcos],[F+T|RestoArcs]) :-
     T =..[_Barra, Nombre|_Ar],  
     my_view('$des',Nombre,_,_,_,_,_,_,_), !, 
     filterArcs(Arcos,RestoArcs).
filterArcs([F-T|Arcos],[F+T|RestoArcs]) :-
     T =..[_Barra, Nombre|_Ar],  
     my_view('$des',Nombre,_,_,_,_,_,_,_), !, 
     filterArcs(Arcos,RestoArcs).
%FSP. filterArcs([_F+_T|Arcos],RestoArcs) :-      
filterArcs([_FdT|Arcos],RestoArcs) :-      
     filterArcs(Arcos,RestoArcs).
     
     

orderViews([],L,L) :- !.
orderViews(_G, [],[]) :- !.
orderViews(Arcs, [V|R1], [V|R2]) :- 
           isOrigin(Arcs, V, Exito), 
           Exito == false, 
           !, 
           deleteArc(Arcs, V, Gd), 
           orderViews(Gd, R1, R2).             
orderViews(Arcs, [V|R1], L) :- 
           append(R1, [V], M),
           orderViews(Arcs, M, L).
           
isOrigin([], _V, false).    % la vista V no depende de otras vistas
isOrigin([F+_T|_Arcos], V, true) :-   % es origen
    F =..[_Barra, V|_Ar], !.                             
isOrigin([F-_T|_Arcos], V, true) :-   % es origen
    F =..[_Barra, V|_Ar], !.                             
isOrigin([_F+_T|Arcos],V,Val) :- 
    isOrigin(Arcos, V, Val).
isOrigin([_F-_T|Arcos],V,Val) :- 
    isOrigin(Arcos, V, Val).
    
% borra los arcos en los que aparece como destino
deleteArc([], _V, []).
deleteArc([_F+T|Arcos], V, Gd) :-
      T =..[_Barra, V|_Ar],  my_table('$des',V, _), 
      deleteArc(Arcos, V, Gd).
deleteArc([F+T|Arcos], V, [F+T|Gd]) :-
        deleteArc(Arcos, V, Gd).
      
       
                           
%test(N, MainV, ListT) :- tc_size(_,Max),  M is Max + 1, M=N
fullTest(N, MainV, Dic, FLt) :-
   tc_size(_,Max),  
   M is Max + 1, 
   M=N
  ; 
   (  M is N+1,
      (   (setOfinstances(fullTC,MainV, Dic, FLt, ListT, _FLv, N), % pos and neg  
           my_fd_labeling(ListT),                                   
          !              
           )
       ; 
       fullTest(M, MainV,Dic, FLt)
       )
    ).
                        
positiveTest(N, MainV, Dic, FLt) :-
   tc_size(_,Max),
   M is Max + 1, 
   M=N             
  ; 
   (  M is N+1,
      (   (setOfinstances(posTC, MainV, Dic, FLt, ListT, _FLv, N), %only pos
           my_fd_labeling(ListT),                                   
           !              
           )
       ; 
       positiveTest(M, MainV,Dic, FLt)
       )
    ).
    
negativeTest(N, MainV, Dic, FLt) :-
   tc_size(_,Max),  
   M is Max + 1, 
   M=N             
  ; 
   (  M is N+1,
      (   (setOfinstances(negTC, MainV, Dic, FLt, ListT, _FLv, N), %only neg
           my_fd_labeling(ListT),                                   
           !              
           )
       ; 
       negativeTest(M, MainV,Dic, FLt)
       )
    ).  

setOfinstances(TipoTC, MainV, Dic, FLt, ListT, FLv, Cardinal) :- 
  tc_tables(T), 
  instancesOfTables(T,Lt, Cardinal),                    
  concat_lists(Lt,Lauxt),
  concat_lists(Lauxt, ListT), 
  tc_domain(Min, Max),
  my_fd_domain(ListT, Min, Max),
  generateCpk(T,T,Lt),
  generateCfk(T,T,Lt),                    
  format_instance(T,Lt,FLt), 
  tc_views(V),    % list of views whose test case will be generated
  instancesOfViews(TipoTC, MainV, Dic, FLt, V, Lv, Cardinal),
  format_instance(V,Lv,FLv).
                  
                
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% instancesOfTables(+Ltables, -Instance, -cardinal)
% generate an instance of relations in DB schema.
% The generated instance is a list of variables
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instancesOfTables([], [], _N).
instancesOfTables([T|RestTables],[Tuplas|RestoI], N) :- 
    instanceTable(T, Tuplas, N),
    instancesOfTables(RestTables,RestoI, N).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% instancesOfViews(TipoTC, +MainV, +/-Instance, +Lviews, -Ptc)
% generate a positive test case for each view in the DB
% schema and a positive and negative test case for the main view MainV
% Variable Instance contains the instance of tables and views
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instancesOfViews(_, _MainV, _Dic, _, [], [], _Card).
instancesOfViews(TipoTC,MainV, Dic, FLt, [V|R],[TuplasP|Pos], Card) :- 
    instanceView(TipoTC,MainV, Dic, FLt, V, TuplasP, Card),
    format_instance([V],[TuplasP],FLv),
    append(FLt, FLv, FLtt),      
    instancesOfViews(TipoTC,MainV, Dic, FLtt, R, Pos, Card).
     
      
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% instanceTable(+Name, -InsRelation)
% Generate an instance of a relation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
% genera N tuplas para una tabla T 
instanceTable(Name, Tuplas, N) :-  
   my_table('$des',Name, Arity),     
   tuplasN(Arity, N, Tuplas).
                                
                                
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% instanceView(TipoTc,  +MainV, +Instance, +V, -PtcV)
% Generate an instance of a view V
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                
                                
instanceView(TipoTC, MainV, Dic, FLt, MainV, Ptc, N) :- 
  instanceMainView(TipoTC, FLt, Dic, MainV, Ptc, N). %positive and negative
instanceView(_, MainV, Dic, FLt, V, Ptc, _Card) :- 
  MainV \= V,
  instanceIntView(FLt, V, Dic, Ptc).            
                              
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% instanceMainView( +Instance, +V, -PtcV, -N)
% Generate an instance of a view V
% N is the number of tuples of the instance.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                                        
instanceMainView(TipoTC, FLt, Dic, V, Ptc, N) :-
     my_view('$des',V,_Arity,SQLst,_La,_D,_I,_L,_SCs),
     replace_str_ctes(SQLst,Dic,ISQLst),
     ISQLst = select(_,    % ALL or DISTINCT
                     _,    % TOP-N
                                   ExpList,   % es * o una lista de expresiones
                                   from(Rels),
                                   where(Pred_W),
                                   group_by(G),
                                   having(Pred_H),
                                   order_by(_Lo1,_Lo2)
                            ),
            
         
     % Proyección -----------------------------------------------------------------------%                           
     cartesian_product(FLt, Rels, Lp1),
      
     calculateFields(ExpList, Rels, Exprs),   %[att(1), att(5), exp(att(4)-att(3))]
     calculateFields(G, Rels, GbExprs),       %G contiene solo lista de atributos          
     % Selección ------------------------------------------------------------------------%
     collectTables(Pred_W, WTables),
     append(Rels, WTables, AllTables),
     
     translatePred(ptc, Pred_W, AllTables, Pos_W),
     translatePred(ntc, Pred_W, AllTables, Neg_W),
     translatePred(ptc, Pred_H, AllTables, Pos_H),
     translatePred(ntc, Pred_H, AllTables, Neg_H),
     
     % en este punto el predicado está traducido a:(and, or, exists, notExists)
    
     constraintsOfMain(TipoTC, GbExprs,FLt, Rels, Lp1, Pos_W, Pos_H, Neg_W, Neg_H, LGrView, N),         
     %
%       valueListExpLt(Exprs, LGrView, Ptc).   
    
      projectExpLt(Exprs, LGrView, Ptc, GbExprs).
  % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% instanceIntView( +Instance, +V, -PtcV)
% Generate an instance of a internal view V
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                                        
instanceIntView(FLt, V, Dic, Ptc) :-
	my_view('$des',V,_Arity,SQLst,_La,_D,_I,_L,_SCs),
	replace_str_ctes(SQLst,Dic,ISQLst),
  ISQLst = select(_,    % ALL or DISTINCT
                  _,    % TOP-N
	                             ExpList,   % es * o una lista de expresiones
	                             from(Rels),
	                             where(Pred_W),
	                             group_by(G),
	                             having(Pred_H),
	                             order_by(_Lo1,_Lo2)
	                      ),
              
	% Projection -----------------------------------------------------------------------%                           
	cartesian_product(FLt, Rels, Lp1),
	calculateFields(ExpList, Rels, Exprs),   %[att(1), att(5), exp(att(4)-att(3))]
	calculateFields(G, Rels, GbExprs),       % G contiene solo lista de atributos       
	% Selection ------------------------------------------------------------------------%
	collectTables(Pred_W, WTables),
	append(Rels, WTables, AllTables),
	translatePred(ptc, Pred_W, AllTables, Pos_W),
	translatePred(ptc, Pred_H, AllTables, Pos_H),
	
	% At this point, the predicate is translated into:(and, or, exists, notExists)
	
	% Constraints for the positive test cases
	constraintsSelection(pos, FLt, Rels, Lp1, Pos_W, _B1, InstList),     % constrains for the positive test cases
	groupby(InstList, GbExprs, ProdGr),   % ProdGr =  [gr([[A,2,3],[4,2,6]]), gr([[A, B, C]])]
	constraintsGby(pos, FLt, Rels, ProdGr, Pos_H, _B2, InstView), 
	
	%valueListExpLt(Exprs, GbExprs,InstView, Ptc).     % constraints for the positive test cases
    
	projectExpLt(Exprs, InstView, Ptc, GbExprs).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% constraintsOfMain( -FLt,-Rels, -Lp1, -PositiveTC, -NegativeTC, +InstView, -N)
% Generate constraints for the main view
% Firstly, we try to generate positive and negative test cases for an 
% instance of N tuples. If, it isn't posible, we generate a new instance of N+1
% tuples. 
% At the end, we generate only positive or negative test cases in the case that was 
% imposible for an instance < cardinal_max.
% N is the number of tuples of the instance.
% Lp1 is a list of groups
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Positive and negative
         
constraintsOfMain(fullTC, GbExprs,FLt, Rels, Lp1, PositiveTC, Pos_H, Neg_TC, NegativeH, Instgr, N) :-
   constraintsSelection(pos,FLt, Rels, Lp1, PositiveTC, B1, InstList), 
   B1#>=1,
   
   groupby(InstList, GbExprs, ProdGr),   % ProdGr =  [gr([[A,2,3],[4,2,6]]), gr([[A, B, C]])]
   constraintsGby(pos, FLt, Rels, ProdGr, Pos_H, B3, Instgr), 
   B3#>=1, 
   
   auxNeg(FLt, Rels, Lp1,  Neg_TC, NegativeH, ProdGr, N). 
       
% Only positive
constraintsOfMain(posTC, GbExprs,FLt, Rels, Lp1, PositiveTC, Pos_H, _Neg_TC, _NegativeH, Instgr, _N) :-
   constraintsSelection(pos,FLt, Rels, Lp1, PositiveTC, B1, InstList), 
   B1#>=1,
   
   groupby(InstList, GbExprs, ProdGr),   
   constraintsGby(pos, FLt, Rels, ProdGr, Pos_H, B3, Instgr), 
   B3#>=1.
       
% Only negative: yoli nuevo
% ::WARNING constraintsOfMain(negTC, GbExprs,FLt, Rels, Lp1, PositiveTC, Pos_H, Neg_TC, NegativeH, Instgr, N) :-
constraintsOfMain(negTC, GbExprs,FLt, Rels, Lp1, PositiveTC, Pos_H, _Neg_TC, _NegativeH, Instgr, _N) :-
   constraintsSelection(pos,FLt, Rels, Lp1, PositiveTC, B1, InstList), 
   groupby(InstList, GbExprs, ProdGr),   % ProdGr =  [gr([[A,2,3],[4,2,6]]), gr([[A, B, C]])]
 
   constraintsGby(pos, FLt, Rels, ProdGr, Pos_H, B3, Instgr), 
   
   B1*B3#=0.
       
       
% If no positive instance has been generated, no constraints are added
%l_constraintsOfMain(_GbExprs, _FLt, _Rels, _Lp1, _PositiveTC, _PositiveH, _Neg_TC, _Neg_H, _, N) :-    
%       tc_size(_,N).     
%       % Yoli:De momento, en este punto no añada restricciones
%       % constraintsSelection(neg,FLt, Rels, Lp1, Neg_TC, B2, _), B2#>1.
%       % Yoli: hay que añadir el que no pase un grupo: 
  
   
auxNeg(FLt, Rels, Lp1,  Neg_TC, Neg_H, ProdGr, _N) :- 
   constraintsSelection(neg,FLt, Rels, Lp1, Neg_TC, B2, _), 
   B2#>=1 #<=> N1, 
   constraintsGby(neg, FLt, Rels, ProdGr, Neg_H, B4, _), 
   B4#>=1 #<=> N2,
   N1 + N2 #>= 1.     % Caso neg: hay una tupla negativa o un grupo negativo         
          
          
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% constraintsSelection(+FLt, +Ftables, +Prod, +B, +Predicate)
% Append constrains for the positive test case and negative test 
% case
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constraintsSelection(_, _FLt, _Ftables, Prod, true, 1, Prod) :- !.  
constraintsSelection(_, _FLt, _Ftables, _Prod, false, 0, []) :- !.   % ojo 


%constraintsSelection(TC, _FLt, _Ftables, Prod, bc(E1,Op,E2), B, InstView) :- !,            
%          apply(TC, _FLt, _Ftables, bc(E1,Op,E2), Prod, B, InstView).
% WARNING: Singleton variables marked with _ above
constraintsSelection(TC, FLt, Ftables, Prod, bc(E1,Op,E2), B, InstView) :-
  !,            
  apply(TC, FLt, Ftables, bc(E1,Op,E2), Prod, B, InstView).
constraintsSelection(TC, FLt, Ftables, Prod, and(C1,C2), B, InstView) :-
  !, 
  apply(TC, FLt, Ftables, and(C1,C2), Prod, B, InstView).
constraintsSelection(TC,FLt, Ftables, Prod, or(C1,C2), B, InstView) :- 
  apply(TC, FLt, Ftables, or(C1,C2), Prod, B, InstView).
constraintsSelection(TC,FLt, Ftables, Prod, exists(C), B, InstView) :- 
  apply(TC, FLt, Ftables, (exists(C)), Prod, B, InstView).
constraintsSelection(TC, FLt, Ftables, Prod, notExists(C), B, InstView) :- 
  apply(TC, FLt, Ftables, (notExists(C)), Prod, B, InstView).
          
% Equivalent to constraintsSelection but for groups    
% ------------------------------------------------      
constraintsGby(_, _FLt, _Rels, ProdGr, true, 1, ProdGr) :-
  !.          
constraintsGby(_, _FLt, _Rels, _ProdGr, false, 0, []) :-
  !.            
%constraintsGby(neg, _FLt, _Ftables, _Prod, true, 0, []) :- !.   % ojo 
%  constraintsGby(neg, _FLt, _Ftables, Prod, false, 1, Prod) :- !.   % ojo 
constraintsGby(TC, FLt, Ltab, ProdGr, Pred_H, B, InstViewGr) :- 
  applyEGby(TC, FLt, Ltab, Pred_H,  ProdGr, B, InstViewGr).            
          
          
           
%
apply(_, _FLt, _Ltables ,true, L, B, L) :-
  B#=1, %  At least, there exists a tuple in the product L
  !.
% At least, there exists a tuple matching the condition
apply(TC, _, _, bc(E1,Op,E2), Producto, B, InstView) :-
  applyE(TC, _, _, bc(E1,Op,E2),  Producto, B, InstView). 
apply(TC, FLt, Ltab, and(C1, C2), Producto, B, InstView) :-
  applyE(TC, FLt, Ltab, and(C1, C2),  Producto, B, InstView).
apply(TC, FLt, Ltab, or(C1, C2), Producto, B, InstView) :- 
  applyE(TC, FLt, Ltab, or(C1, C2),  Producto, B, InstView).
apply(TC, FLt, Ltab, (exists(C)), Prod, B, InstView) :-
  applyE(TC, FLt, Ltab, (exists(C)),  Prod, B, InstView).
apply(TC, FLt, Ltab, (notExists(C)), Prod, B, InstView) :-
  applyE(TC, FLt, Ltab, (notExists(C)),  Prod, B, InstView).

%----- Last argument contains the generated view instance
applyE(_, _FLt, _Ltables ,true, L, B, L) :- B#=1, !. 
applyE(_, _FLt, _Ltables ,false, _L, B, []) :- B#=0, !. 
%applyE(neg, _FLt, _Ltables ,false, L, B, L) :- B#=1, !. 
%applyE(neg, _FLt, _Ltables ,true, _L, B, []) :- B#=0, !. 

applyE(_ , _, _, bc(_E1,_Op,_E2), [], 0, []).
applyE(_ , _, _, bc(_E1,_Op,_E2), [], 0, []).
applyE(pos, _, _, bc(E1,Op,E2), [Tuple|Lprod], B, [Tuple|Rt]) :- 
  applyTuple(pos,_, _, bc(E1,Op,E2), Tuple, B1), B1#\=0,
  applyE(pos, _, _, bc(E1,Op,E2), Lprod, B2, Rt), 
  B #= B1+B2.
        
applyE(pos, _, _, bc(E1,Op,E2), [Tuple|Lprod], B, Rt) :- 
  applyTuple(pos,_, _, bc(E1,Op,E2), Tuple, B1), B1#=0,
  applyE(pos, _, _, bc(E1,Op,E2), Lprod, B2, Rt), 
  B #= B1+B2.
        
applyE(neg, _, _, bc(E1,Op,E2), [Tuple|Lprod], B, _) :- 
  applyTuple(neg,_, _, bc(E1,Op,E2), Tuple, B1), 
  applyE(neg, _, _, bc(E1,Op,E2), Lprod, B2, _), 
  B #= B1+B2.

%-----                

applyE(_, _, _, or(_C1, _C2), [], 0, []).
applyE(pos, FLt, Ltables, or(C1, C2), [Tuple|Lprod], B, [Tuple|Rt]) :- 
     applyTuple(pos,FLt, Ltables, or(C1, C2), Tuple, B1), 
     B1#\=0,
     applyE(pos, FLt, Ltables, or(C1, C2), Lprod, B2, Rt),  
     B #= B1+B2.  
     
applyE(pos, FLt, Ltables, or(C1, C2), [Tuple|Lprod], B, Rt) :- 
     applyTuple(pos,FLt, Ltables, or(C1, C2), Tuple, B1), 
     B1#=0,
     applyE(pos, FLt, Ltables, or(C1, C2), Lprod, B2, Rt),  
     B #= B1+B2.  

applyE(neg, FLt, Ltables, or(C1, C2), [Tuple|Lprod], B, _) :- 
     applyTuple(neg,FLt, Ltables, or(C1, C2), Tuple, B1), 
     applyE(neg, FLt, Ltables, or(C1, C2), Lprod, B2, _),  
     B #= B1+B2.    
         
%-----
applyE(_, _, _, and(_, _), [], 0, []).
applyE(pos, FLt, Ltables, and(C1, C2), [Tuple|Lprod], B, [Tuple|Rt]) :- 
     applyTuple(pos,FLt, Ltables, and(C1, C2), Tuple, B1), 
     B1#\=0,
     applyE(pos, FLt, Ltables, and(C1, C2), Lprod, B2, Rt),  
     B #= B1+B2.
     
applyE(pos, FLt, Ltables, and(C1, C2), [Tuple|Lprod], B, Rt) :- 
     applyTuple(pos,FLt, Ltables, and(C1, C2), Tuple, B1), 
     B1#=0,
     applyE(pos, FLt, Ltables, and(C1, C2), Lprod, B2, Rt),  
     B #= B1+B2.
     
applyE(neg, FLt, Ltables, and(C1, C2), [Tuple|Lprod], B, _) :- 
     applyTuple(neg,FLt, Ltables, and(C1, C2), Tuple, B1), 
     applyE(neg, FLt, Ltables, and(C1, C2), Lprod, B2, _),  
     B #= B1+B2.
     
%-----     
applyE(_, _FLt, _Ltab, (exists(_C)), [], 0, []).
applyE(pos, FLt, Ltab, (exists(C)), [Tuple|Lprod], B, [Tuple|Rt]) :- 
     applyTuple(pos,FLt, Ltab, (exists(C)), Tuple, B1), 
     B1#\=0,
     applyE(pos, FLt, Ltab, (exists(C)), Lprod, B2, Rt),  
     B #= B1+B2.
     
applyE(pos, FLt, Ltab, (exists(C)), [Tuple|Lprod], B, Rt) :- 
     applyTuple(pos,FLt, Ltab, (exists(C)), Tuple, B1), 
     B1#=0,
     applyE(pos, FLt, Ltab, (exists(C)), Lprod, B2, Rt),  
     B #= B1+B2.
     
applyE(neg, FLt, Ltab, (exists(C)), [Tuple|Lprod], B, _) :- 
     applyTuple(neg,FLt, Ltab, (exists(C)), Tuple, B1), 
     applyE(neg, FLt, Ltab, (exists(C)), Lprod, B2, _),  
     B #= B1+B2.
%-----     
applyE(_, _FLt, _Ltab, (notExists(_C)), [], 0, []).
applyE(pos, FLt, Ltab, (notExists(C)), [Tuple|Lprod], B, [Tuple|Rt]) :- 
     applyTuple(pos,FLt, Ltab, (exists(C)), Tuple, B1), 
     B1#=0#<=>B3, 
     B3#\=0,
     applyE(pos, FLt, Ltab, (notExists(C)), Lprod, B2, Rt),  
     B #= B3+B2.   
     
applyE(pos, FLt, Ltab, (notExists(C)), [Tuple|Lprod], B, Rt) :- 
     applyTuple(pos,FLt, Ltab, (exists(C)), Tuple, B1), 
     B1#=0#<=>B3, 
     B3#=0,
     applyE(pos, FLt, Ltab, (notExists(C)), Lprod, B2, Rt),  
     B #= B3+B2. 
     
applyE(neg, FLt, Ltab, (notExists(C)), [Tuple|Lprod], B, _) :- 
     applyTuple(neg,FLt, Ltab, (exists(C)), Tuple, B1), 
     B1#=0#<=>B3,
     applyE(neg, FLt, Ltab, (notExists(C)), Lprod, B2, _),  
     B #= B3+B2. 
     
% For groups --------------------------------------     %%%
     
applyEGby(_ , _, _, bc(_E1,_Op,_E2), [], 0, []).
applyEGby(pos, _, _, bc(E1,Op,E2), [Grupo|Lprod], B, [Grupo|Rt]) :- 
      applyGr(pos,_, _, bc(E1,Op,E2), Grupo, B1), B1#\=0,
      applyEGby(pos, _, _, bc(E1,Op,E2), Lprod, B2, Rt), 
      B #= B1+B2.
      
applyEGby(pos, _, _, bc(E1,Op,E2), [Grupo|Lprod], B, Rt) :- 
      applyGr(pos,_, _, bc(E1,Op,E2), Grupo, B1), B1#=0,
      applyEGby(pos, _, _, bc(E1,Op,E2), Lprod, B2, Rt), 
      B #= B1+B2.
      
applyEGby(neg, _, _, bc(E1,Op,E2), [Grupo|Lprod], B, _) :- 
      applyGr(neg,_, _, bc(E1,Op,E2), Grupo, B1), 
      applyEGby(neg, _, _, bc(E1,Op,E2), Lprod, B2, _), 
      B #= B1+B2.
   
   %%%
   
%----- and ------------

applyEGby(_, _, _, and(_, _), [], 0, []).
applyEGby(pos, FLt, Ltables, and(C1, C2), [G|Lprod], B, [G|Rt]) :- 
   applyGr(pos,FLt, Ltables, and(C1, C2), G, B1), B1#\=0,
   applyEGby(pos, FLt, Ltables, and(C1, C2), Lprod, B2, Rt),  
   B #= B1+B2.
   
applyEGby(pos, FLt, Ltables, and(C1, C2), [G|Lprod], B, Rt) :- 
   applyGr(pos,FLt, Ltables, and(C1, C2), G, B1), B1#=0,
   applyEGby(pos, FLt, Ltables, and(C1, C2), Lprod, B2, Rt),  
   B #= B1+B2.
   
applyEGby(neg, FLt, Ltables, and(C1, C2), [G|Lprod], B, _) :- 
   applyGr(neg,FLt, Ltables, and(C1, C2), G, B1), 
   applyEGby(neg, FLt, Ltables, and(C1, C2), Lprod, B2, _),  
   B #= B1+B2.   
   
   
%----- or --------------

applyEGby(_, _, _, or(_C1, _C2), [], 0, []).
applyEGby(pos, FLt, Ltables, or(C1, C2), [G|Lprod], B, [G|Rt]) :- 
   applyGr(pos,FLt, Ltables, or(C1, C2), G, B1), B1#\=0,
   applyEGby(pos, FLt, Ltables, or(C1, C2), Lprod, B2, Rt),  
   B #= B1+B2.  
   
applyEGby(pos, FLt, Ltables, or(C1, C2), [G|Lprod], B, Rt) :- 
   applyGr(pos,FLt, Ltables, or(C1, C2), G, B1), B1#=0,
   applyEGby(pos, FLt, Ltables, or(C1, C2), Lprod, B2, Rt),  
   B #= B1+B2.  

applyEGby(neg, FLt, Ltables, or(C1, C2), [G|Lprod], B, _) :- 
   applyGr(neg,FLt, Ltables, or(C1, C2), G, B1), 
   applyEGby(neg, FLt, Ltables, or(C1, C2), Lprod, B2, _),  
   B #= B1+B2.       

applyTuple(_,_, _, bc(E1, '>=', E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #>= V2) #<=> B.
applyTuple(_,_, _, bc(E1, '=>', E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #>= V2) #<=> B.
applyTuple(_,_, _, bc(E1, '=<', E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #=< V2) #<=> B.
applyTuple(_,_, _, bc(E1, '<=', E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #=< V2) #<=> B.
applyTuple(_,_, _, bc(E1, '>',  E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #>  V2) #<=> B.
applyTuple(_,_, _, bc(E1, '<',  E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #<  V2) #<=> B.
applyTuple(_,_, _, bc(E1, '=',  E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #=  V2) #<=> B.
applyTuple(_,_, _, bc(E1, '/=', E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #\= V2) #<=> B.
applyTuple(_,_, _, bc(E1, '<>', E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #\= V2) #<=> B.

applyTuple(Tc,FLt, Ltables, or(C1, C2), Tuple, B) :- 
                applyTuple(Tc,FLt, Ltables, C1, Tuple, B1), 
                applyTuple(Tc,FLt, Ltables, C2, Tuple, B2), 
                B #= B1+B2.

applyTuple(Tc,FLt, Ltables, and(C1, C2), Tuple, B) :- 
            applyTuple(Tc,FLt, Ltables, C1, Tuple, B1), 
            applyTuple(Tc,FLt, Ltables, C2, Tuple, B2), 
            B #= B1*B2.

applyTuple(Tc,FLt, Ltables1, (exists(select(_,_,_La, 
                                        from(Ltables2), 
                                        where(Predicate),
                                        group_by(_), 
                                        having(_H),
                                        order_by([],[])))), Tuple, B) :- 
            cartesian_product(FLt, Ltables2, Lp2), 
            productTupleAll(Tuple, Lp2, Prod),
            append(Ltables1, Ltables2, Tab),
            applyE(Tc, FLt, Tab, Predicate, Prod, B, _).
            
applyTuple(Tc,FLt, Ltables1, (notExists(select(_,_,_La,
                                    from(Ltables2), 
                                    where(Predicate),
                                    group_by(_), 
                                    having(_H),
                                    order_by([],[])))), Tuple, B) :- 
            cartesian_product(FLt, Ltables2, Lp2), 
            productTupleAll(Tuple, Lp2, Prod),
            append(Ltables1, Ltables2, Tab),
            applyE(Tc, FLt, Tab, Predicate, Prod, B1, _), 
            B1#=0#<=>B.
            
            
            
applyGr(_,_, _, bc(E1, '>=', E2), Gr, B) :- 
        valueExp(E1, Gr, V1),
        valueExp(E2, Gr, V2), 
        (V1 #>= V2) #<=> B.
        
applyGr(_,_, _, bc(E1, '=<', E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #=< V2) #<=> B.
applyGr(_,_, _, bc(E1, '<=', E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #=< V2) #<=> B.
applyGr(_,_, _, bc(E1, '>',  E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #> V2)  #<=> B.
applyGr(_,_, _, bc(E1, '<',  E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #< V2)  #<=> B.
applyGr(_,_, _, bc(E1, '=',  E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #= V2)  #<=> B.
applyGr(_,_, _, bc(E1, '/=', E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #\= V2) #<=> B.
applyGr(_,_, _, bc(E1, '<>', E2), Tuple, B) :- valueExp(E1, Tuple, V1), valueExp(E2, Tuple, V2), (V1 #\= V2) #<=> B.

applyGr(Tc,FLt, Ltables, and(C1, C2), G, B) :- 
            applyGr(Tc,FLt, Ltables, C1, G, B1), 
            applyGr(Tc,FLt, Ltables, C2, G, B2), 
            B #= B1*B2.
            
applyGr(Tc,FLt, Ltables, or(C1, C2), G, B) :- 
                applyGr(Tc,FLt, Ltables, C1, G, B1), 
                applyGr(Tc,FLt, Ltables, C2, G, B2), 
                B #= B1+B2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% op(+Op, +NOp)
% Op is the allowed operator for basic conditions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
myOp(=).
myOp(/=).
myOp('<>').
myOp(=<).
myOp(<=).
myOp(<).
myOp(>).
myOp(>=).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% negation(+Op, +NOp)
% NOp is the negation of the operator Op
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

negation('=', '/=') :- !.
negation('/=', '=') :- !.
negation('<>', '=') :- !.
negation('=<', '>') :- !.
negation('<=', '>') :- !.
negation('<', '>=') :- !.
negation('>', '=<') :- !.
negation('>=', '<') :- !.
negation(Otro, _) :-  
      nl,
      print_message(error, Otro), 
      raise_exception('Error: Unexpected operator').
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sub-Queries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
% mySqType()       
mySqType('=_all', '=', all).
mySqType('<>_all', '/=', all).
mySqType('<_all', '<', all).
mySqType('>_all', '>', all).
mySqType('>=_all', '>=', all).
mySqType('<=_all', '=<', all).
mySqType('=<_all', '=<', all).  
%---- Any  
mySqType('=_any', '=', any ).
mySqType('<>_any','/=', any).
mySqType('<_any', '<', any ).
mySqType('>_any', '>', any ).
mySqType('>=_any', '>=', any ).
mySqType('<=_any', '=<', any ).
mySqType('=<_any', '=<', any ).  

mySqType('in', 'in', in).  
mySqType('not_in', 'not_in', not_in).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% collectTables(+Predicate, -ListRelations)
% collect the relations occurring in a given predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collectTables(not(C), L) :- collectTables(C, L), !.
collectTables(true, []) :- !.
collectTables(false, []) :- !.
collectTables(select(_,_,_Latt, 
                    from(M), 
                    where(P), 
                    group_by(_), 
                    having(_H),
                    order_by([],[])), L) :- collectTables(P, L1), append(M, L1, L), !.
collectTables(and(C1, C2), L) :- !, collectTables(C1, L1), collectTables(C2, L2), append(L1, L2, L).
collectTables(or(C1, C2), L) :- !, collectTables(C1, L1), collectTables(C2, L2), append(L1, L2, L).

collectTables(exists((SQ, _)), L) :-        !, collectTables(SQ, L).
%collectTables(not(exists((SQ, _))), L) :-   !, collectTables(SQ, L).
  
collectTables(M, L) :-  M=..Res, Res=[Type, _E,(SQ, _ )], mySqType(Type, _, _), !, collectTables(SQ, L).

collectTables(M, []) :- M=..Res, Res=[Op, _E1,_E2],  myOp(Op), !.
                     
collectTables(Otro, _) :-  
      nl,
      print_message(error, Otro), 
      raise_exception('Error: Unexpected operator in where condition').
                                 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tuplasN(+Arity, +N, -tuple)
% Make N tuples of arity Arity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tuplasN(_Arity, 0, []).
tuplasN(Arity, N, [T|Resto]) :-
  N > 0, 
  M is N-1,
  tupla(Arity, T),
	tuplasN(Arity, M, Resto).
	
tupla(Arity,T) :-
  length(T,Arity).
% tupla(0, []) :- !.
% tupla(N, [_A|Rt]) :- 
%   N>0, 
%   M is N-1, 
%   tupla(M, Rt).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% constrains  pk
% generateCpk(+Relations, +Schema, +instance)
% Append the primary key constrains
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generateCpk([],_S,_L).
generateCpk([T|Rt], S,L) :-  
    
    my_primary_key('$des',T , LpkN),   !,
    
    attr_name_2_attr_pos_list(LpkN,T,Lpk),
    obtainInstance(S, T, L, Li),
    selecPk(Lpk,Li,Lc),
    generatePk(Lc),                       % todas las relaciones tienen pk 
    generateCpk(Rt, S,L).
    
generateCpk([_T|Rt], S,L) :- % todas las relaciones tienen pk 
    generateCpk(Rt, S,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% genera FK tabla por tabla  
% generateCpk(+Relations, +Schema, +instance)
% Append the foreign key constrains relation by relation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   
generateCfk([],_S,_L).
generateCfk([T|Rt], S,L) :- 
      %my_table('$des',T, _Arity, _LAttr, _Lpk, Lfk), 
      get_foreign_key(T, LfkN),
      
      attr_name_2_attr_pos_list_fk(LfkN,T,Lfk), 
      obtainInstance(S, T, L, Li),
      fkTuples(S,Lfk,L,Li),   
      generateCfk(Rt, S,L).
      
      

selecPk(_Pk, [], []).
selecPk(Pk,[T1|R], [C1|Rc]) :- 
      selecEach(Pk, T1, C1),
      selecPk(Pk, R, Rc).
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% selecEach(+Latt, +Tuple, -L)
% -L is part of the tuple +Tuple, which contains the attributes 
% given by +Latt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
                        
selecEach([], _T, []).       
selecEach([N|Resto], T1, Tc) :- 
      obtainAttr(N, T1, X1),     
      L1 = [X1],
      selecEach(Resto, T1, Tcr),
      append(L1, Tcr, Tc). 
                               
% selecEach([a:1, b:3, c:4], [1,2,3,4,5,6,7], L).   %para pruebas

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generatePk(+L).
% Append constraints associated to the primary key 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generatePk([]).
generatePk([U|R]) :- 
          one2rest(U,R), 
          generatePk(R).
%%%
one2rest(_U, []).        
one2rest(U,[V|R]) :-  
          different(U,V,B), 
          B#=0, 
          one2rest(U,R).
%%%                                  
different([],[],1).
different([A1|Ra], [A2|Rb], B) :- 
      A1 #=A2 #<=> B1, 
      different(Ra, Rb, B2),
      B#=B1*B2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  fkTuples(_S, [], _L, _Li).
  fkTuples(S, [(LAi, T2, LAf)|R], L ,Li) :- 
          obtainInstance(S, T2, L, Lf), 
          buidPart(LAi,  LAf, Li, Mli, Lf, Mlf),
          generateFKall(Mli, Mlf),
          fkTuples(S, R, L, Li).                                          
                                               
  % build Mli and Mlf as part of both Li and Lf
  buidPart(LAi,  LAf, Li, Mli, Lf, Mlf) :- 
          obtainAllAttr(LAi, Li, Mli),
          obtainAllAttr(LAf, Lf, Mlf).

  obtainAllAttr(_La, [], []).
  obtainAllAttr(La, [T|Rt], [Ta|Rta]) :- 
          selecEach(La, T, Ta), 
          obtainAllAttr(La, Rt, Rta).                     
           
  generateFKall([], _).                  
  generateFKall([Tuple|RTuples], Mlf) :- 
          generateFkTuple(Tuple, Mlf, B), 
          B#>=1, 
          generateFKall(RTuples, Mlf).              

  equals([],[],1).
  equals([A1|Ra], [A2|Rb], B) :- 
          A1 #=A2 #<=> B1, 
          equals(Ra, Rb, B2), 
          B#=B1*B2.  

  generateFkTuple(_T, [], 0).
  generateFkTuple(Tuple, [T|RMlf], B) :-  
          equals(Tuple, T, B1),  
          generateFkTuple(Tuple, RMlf, B2), 
          B#= B1+B2. 
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% obtainInstance
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
obtainInstance([],T, L, []) :- 
      nl,
      print_message(error, T), 
      print_message(error, L), 
      raise_exception('Error: in obtainInstance predicate').
obtainInstance([T|_Rt], T, [Li|_RL], Li) :- !.
obtainInstance([_T1|Rt], T, [_L|RL], Li) :- 
      obtainInstance(Rt, T, RL, Li).
%
obtainAttr(1, [X|_Resto], X).
obtainAttr(N, [_|Resto], X) :- 
          N > 1, 
          M is N-1, 
          obtainAttr(M, Resto, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write_instance(+tipo, +List)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%write_instance(_,[]) :- nl.
%write_instance(t, [L|Rl]) :- 
%      nl,
%      write_log('Relation: '), 
%      write_log(L),  
%      write_instance(t, Rl).
%write_instance(v, [L|Rl]) :- 
%      nl_log,
%      write_log('View: '), 
%      write_log(L), 
%      write_instance(v, Rl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% format_instance(+SchemaTables, +inst, -instanceFormated)
% Give format to the calculated instance 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_instance([],[],[]).
format_instance([T|RestoT], [Ltuplas|RestoTuplas], [M|Mresto]) :- 
      M=..[T,Ltuplas], 
      format_instance(RestoT,RestoTuplas, Mresto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cartesian_product(+instance, +Lfrom, -Product)
% Compute the Cartesian product of a list of relations 
%tests:
%cartesian_product([authors([[_927,_958,_989],[_1020,_1051,_1082],[_1113,_1144,_1175]]),projects([[_1206,_1237,_1268,_1299],[_1330,_1361,_1392,_1423],[_1454,_1485,_1516,_1547]]),publications([[_1578,_1609,_1640],[_1671,_1702,_1733],[_1764,_1795,_1826]])],[(authors,['$t0',attr(authors,id,'$a1'),attr(bibauthors,name,'$a2'),attr(bibauthors,salary,'$a3'),attr(bibauthors,city,'$a4')]),(projects,['$t1',attr(bibuser,id,'$a5'),attr(bibuser,user,'$a6'),attr(bibuser,city,'$a7'),attr(bibuser,pass,'$a8')])
%              ],_4889) ? 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cartesian_product(_FLt, [], []).
cartesian_product([M|_RestoT], [(T,[_Tas|_Lat])], Ltuplas) :- 
  M=..[T,Ltuplas], %Ltuplas = lista de listas
  !.                        
cartesian_product([_M|RestoT], [(T,[Tas|Lat])], Ltuplas) :- 
  cartesian_product(RestoT, [(T,[Tas|Lat])], Ltuplas), 
  !.
%cartesian_product(FLt, [(T,[Tas|_Lat])|Rest], Ltuplas) :- 
%  cartesian_product(FLt, [(T,[Tas|_Lat])], Prod1),
cartesian_product(FLt, [(T,[Tas|Lat])|Rest], Ltuplas) :- 
  cartesian_product(FLt, [(T,[Tas|Lat])], Prod1),
  cartesian_product(FLt, Rest, Prod2),
  product2Relations(Prod1, Prod2, Ltuplas).
                                          
product2Relations([], _L, []).
product2Relations([A|R], L, All) :- 
	productTupleAll(A,L,A1),
	product2Relations(R,L,Resto),
	append(A1, Resto, All).

productTupleAll(_A,[],[]).
productTupleAll(A,[B|Res],[R1|Rres]) :- 
  append(A, B, R1),
  productTupleAll(A, Res, Rres).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% calculateFields(+Exp, +Lfrom, +ExpTransf)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculateFields(*,_, all).  % yoli: ampliarlo para que obtenga all de las tablas del from.
calculateFields([],_, []).
calculateFields([Exp|R],LRels, [Rexp|Rexpresiones]) :- 
      convert(Exp, LRels, Rexp), 
      calculateFields(R,LRels, Rexpresiones).
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% convert(+Exp, +Lfrom, +ExpTransf)
%
%tests:  convert(expr(attr(t1, id, _V1), _V2),[(bibauthors,[t1|L]), (bibauthors,[t2|L2]),(bibuser, [t3|L3])], R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attr_name_2_attr_pos_list([],_T,[]).
attr_name_2_attr_pos_list([A|As],T,[P|Ps]) :-
  my_attribute('$des',P,T,A,_Type),
  attr_name_2_attr_pos_list(As,T,Ps).
  
attr_name_2_attr_pos_list_fk([],_T,[]).
attr_name_2_attr_pos_list_fk([(L1, N, L2)|As],T,[(LL1, N, LL2)|Ps]) :-
  attr_name_2_attr_pos_list(L1, T, LL1),
  attr_name_2_attr_pos_list(L2, N, LL2),
  attr_name_2_attr_pos_list_fk(As,T,Ps).
  
  
  
  
  

%convert(expr(attr(r1,a,'$a3'),e1,number(_46697)),[(t1,[r1,attr(t1,a,'$a1'),attr(t1,b,'$a2')])],_51977) ? 


convert(expr(E, _, _Var), L, C) :-            
     convert(E, L, C), !.   
% constante ------------------------------------------     
convert(cte(C, _T), _L, cons(C)) :-           %t(C, num), 
      number(C), !.     %constante
% attribute ------------------------------------------
convert(attr(AsT, NameAt, _V), [(Tab, [AsT|_])|_RestN], att(N)) :-  my_attribute('$des',N, Tab, NameAt,_), !.
convert(attr(Tab, NameAt, _V), [(Tab, [_AsT|_])|_RestN], att(N)) :-  my_attribute('$des',N, Tab, NameAt, _), !.
convert(attr(AsT, NameAt, V), [(S, [_AsS|_])|RestN], att(M)) :- 
        my_table('$des',S, Arity), !,        
        convert(attr(AsT, NameAt, V), RestN, att(Col)),
        M is Col + Arity.
        
convert(attr(Ast, _NameAt, _), [], _) :-   
        nl,
        print_message(error, Ast), 
        raise_exception('Error: Alias of table not found').



% Para las funciones de agregación: [exp(fsum(att(t1,1))), att(t1,2), fcount] 
convert(sum(Att), L, fsum(C)) :- convert(Att, L, C), !.
convert(avg(Att), L, favg(C)) :- convert(Att, L, C), !.
convert(count, _L, fcount) :- !.    
       
convert(E1 + E2, L, bc(C1, '+', C2)) :- !, convert(E1, L, C1), convert(E2, L, C2).   %sum
convert(E1 - E2, L, bc(C1, '-', C2)) :- !, convert(E1, L, C1), convert(E2, L, C2).   %minus
convert(E1 / E2, L, bc(C1, '/', C2)) :- !, convert(E1, L, C1), convert(E2, L, C2).   %division
convert(E1 * E2, L, bc(C1, '*', C2)) :- !, convert(E1, L, C1), convert(E2, L, C2).   %product
                                                                                           
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% valueListExpLt(+Exprs, +Lrels, +Lprod, -LTuplas)
% Make the projection over the product
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

projectExpLt(Exp, [gr(Gr)], Res, []) :-  valueListExpLt(Exp, Gr, Res, tuple), !.
projectExpLt(Exp, Inst, Res, _L) :- valueListExpLt(Exp, Inst, Res, grupo).   

valueListExpLt([fcount], L, [[Count]], tuple) :-  valueExp(fcount, gr(L), Count), !.
valueListExpLt(all, L, L, tuple).      % All tuples
valueListExpLt(all, [], [], grupo):- !.      % All tuples
valueListExpLt(all, _L, _M, grupo) :-   
      nl,
      print_message(error, valueListExpLt), 
      raise_exception('Error: All attributes in Select clause for group').
      
valueListExpLt(_Lexp, [], [], _).

% The result is a tuple for group
valueListExpLt(Lexp, [Gr|Rt], [TVal|RestoTVal], grupo) :- 
      valorListExp(Lexp, Gr, TVal), 
      valueListExpLt(Lexp, Rt, RestoTVal, grupo). 
      
      
% The result is a tuple for each tuple
valueListExpLt(Lexp, [Tuple|Rt], [TVal|RestoTVal], tuple) :- 
      valorListExp(Lexp, gr([Tuple]), TVal), 
      valueListExpLt(Lexp, Rt, RestoTVal, tuple). 
%%%%%%%      
valorListExp([], _, []).
valorListExp([Exp|Rexp], Gr, [Val|RestoVal]) :- 
      valueExp(Exp, Gr, Val), 
      valorListExp(Rexp, Gr, RestoVal).

% If a group has several tuple, att(N) must be in the group by,
% and, therefore, this attribute is common to all the tuples in the group

valueExp(att(N), gr([Tuple|_R]), M) :- 
      valor(att(N), Tuple, M), !.    
valueExp(exp(att(N)), gr([Tuple|_R]), M) :- 
      valor(att(N), Tuple, M), !.
        
%%%%%%
valueExp(att(N), Tuple, M) :- 
      valor(att(N), Tuple, M). 
valueExp(cons(C), _, M) :- 
      valor(cons(C), _, M).
valueExp(exp(att(N)), Tuple, M) :- 
      valor(att(N), Tuple, M).
valueExp(exp(cons(C)), _, M) :- 
      valor(cons(C), _, M).
      
valueExp(exp('-', E), G, M) :- 
      valueExp(E, G, R), 
      M #= R * (-1) .  
        
valueExp(exp(E1, '*', E2), Tuple, M) :- 
      valueExp(E1, Tuple, R1), 
      valueExp(E2, Tuple, R2), 
      M #= R1 * R2, !.         
valueExp(exp(E1, '/', E2), Tuple, M) :- 
      valueExp(E1, Tuple, R1), 
      valueExp(E2, Tuple, R2),
      M #= R1 / R2, !.                            
valueExp(exp(E1, '+', E2), Tuple, M) :-
      valueExp(E1, Tuple, R1), 
      valueExp(E2, Tuple, R2), 
      M #= R1 + R2, !.
valueExp(exp(E1, '-', E2), Tuple, M) :- 
      valueExp(E1, Tuple, R1), 
      valueExp(E2, Tuple, R2), 
      M #= R1 - R2, !.     
      
%%%%%%

valueExp(bc(E1, '*', E2), Tuple, M) :- 
      valueExp(E1, Tuple, R1), 
      valueExp(E2, Tuple, R2), 
      M #= R1 * R2, !.         
valueExp(bc(E1, '/', E2), Tuple, M) :- 
      valueExp(E1, Tuple, R1), 
      valueExp(E2, Tuple, R2),
      M #= R1 / R2, !.                            
valueExp(bc(E1, '+', E2), Tuple, M) :-
      valueExp(E1, Tuple, R1), 
      valueExp(E2, Tuple, R2), 
      M #= R1 + R2, !.
valueExp(bc(E1, '-', E2), Tuple, M) :- 
      valueExp(E1, Tuple, R1), 
      valueExp(E2, Tuple, R2), 
      M #= R1 - R2, !.        
      
%%%%%%      
% Aggregate functions applied to groups
%valueExp(fsum(att(2)), [[1,2],[2,3],[3,4],[4,5],[5,6]], M).      
 valueExp(fsum(att(N)), gr(Gr), M) :- 
      getAtt(N, Gr, LAtt),
      mysumlist(LAtt, M).
      %sumAtts(LAtt, M).
      
 valueExp(favg(att(N)), gr(Gr), Media) :- 
      getAtt(N, Gr, LAtt),
      length(Gr,L),
      mysumlist(LAtt, M),
      Media #= M / L.
      
 valueExp(fcount, gr(Gr), Count) :- 
      length(Gr,Count).
      
      
      
 %getAtt(2, [[1,2],[2,3],[3,4],[4,5],[5,6]], L) .
 getAtt(_N, [], []).
 getAtt(N, [T|R], L) :- 
    obtainAttr(N,T, Val),
    L1 =[Val],     
    getAtt(N, R, Laux),  
    append(L1, Laux, L)  .                             
%%%%%%%%
valor(cons(C), _ , C).    %constant
valor(att(N), [A|_Resto], A) :-  
       N = 1, !.
valor(att(N),[_A|Resto], L) :-  
       M is N-1 , 
       valor(att(M), Resto, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%productsTuples([], _Lp2, []).
%productsTuples([T|R], Lp2, [Pc|Rpc]) :- 
%     productC(T, Lp2, Pc), 
%     productsTuples(R, Lp2, Rpc).
%
%productC(_T, [], []).
%productC(T, [Tu|R], [Pi|Rpi]) :- 
%     append(T, Tu, Pi), 
%     productC(T, R, Rpi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% translatePred( +Type, +Predicate, +Lrelations, -PredTrans)
% Predicate can be a where condition or a having condition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%p(bcp, 1).
%p(bcn, 1).
%p(andp, 1).
%p(andn, 1).

translatePred( ptc , true, _, true).
translatePred( ptc , false, _, false).
translatePred( ntc , true, _, false).
translatePred( ntc , false, _,true).

% not basic condition
translatePred(ptc, not(Bc), Ltables, bc(C1, NOp, C2)) :- 
   Bc=..Res, Res=[Op, E1, E2], myOp(Op),
   negation(Op, NOp),
   convert(E1,Ltables,C1), 
   convert(E2,Ltables,C2).
   
translatePred(ntc, not(Bc), Ltables, C) :- 
   translatePred(ptc, Bc, Ltables, C).
   

% basic condition
translatePred(ptc, Bc, Ltables, bc(C1, Op, C2)) :- 
      Bc=..Res, Res=[Op, E1, E2], myOp(Op),
      convert(E1,Ltables,C1), 
      convert(E2,Ltables,C2).
      
translatePred(ntc, Bc, Ltables, bc(C1, NOp, C2)) :- 
      Bc=..Res, Res=[Op, E1, E2], myOp(Op),
      negation(Op, NOp),
      convert(E1,Ltables,C1), 
      convert(E2,Ltables,C2).      

%  NOT AND
translatePred(ptc, not(and(C1,C2)), Ltables, or(Ct1,Ct2)) :- 
      translatePred(ptc, not(C1),Ltables,Ct1),
      translatePred(ptc, not(C2),Ltables,Ct2).   
      
translatePred(ntc, not(and(C1,C2)), Ltables, and(Ct1,Ct2)) :- 
      translatePred(ptc, C1,Ltables,Ct1),
      translatePred(ptc, C2,Ltables,Ct2).
% AND
translatePred(ptc, and(C1,C2), Ltables, and(Ct1,Ct2)) :- 
      translatePred(ptc, C1,Ltables,Ct1),
      translatePred(ptc, C2,Ltables,Ct2).
      
translatePred(ntc, and(C1,C2), Ltables, or(Ct1,Ct2)) :- 
      translatePred(ntc, C1,Ltables,Ct1),
      translatePred(ntc, C2,Ltables,Ct2).
      
% NOT OR
translatePred(ptc, not(or(C1,C2)), Ltables, and(Ct1,Ct2)) :- 
      translatePred(ptc, not(C1),Ltables,Ct1),
      translatePred(ptc, not(C2),Ltables,Ct2).
      
translatePred(ntc, not(or(C1,C2)), Ltables, or(Ct1,Ct2)) :- 
      translatePred(ptc, C1,Ltables,Ct1),
      translatePred(ptc, C2,Ltables,Ct2).      
% OR
translatePred(ptc, or(C1,C2), Ltables, or(Ct1,Ct2)) :- 
      translatePred(ptc, C1,Ltables,Ct1),
      translatePred(ptc, C2,Ltables,Ct2).
      
translatePred(ntc, or(C1,C2), Ltables, and(Ct1,Ct2)) :- 
      translatePred(ntc, C1,Ltables,Ct1),
      translatePred(ntc, C2,Ltables,Ct2).

% EXISTS: exists((SQ, _))
translatePred(ptc, exists((select(_, _, L, from(M), where(C), group_by(Attr), having(Hc), order_by(_,_)),_)),
                   LTables, 
                   exists(select(_, _, L, from(M), where(Cond), group_by(Attr), having(Hc), order_by(_,_)))) :- 
      translatePred(ptc, C, LTables, Cond).
      
translatePred(ntc, exists((select(_, _, L, from(M), where(C), group_by(Attr), having(Hc), order_by(_,_)),_)),
                   LTables, 
                   notExists(select(_, _, L, from(M), where(Cond), group_by(Attr), having(Hc), order_by(_,_)))) :- 
      translatePred(ptc, C, LTables, Cond).
      
% NOT EXISTS : not(exists((SQ, _)))                                              
translatePred(ptc, not(exists((select(_, _, L, from(M), where(C), group_by(Attr), having(Hc), order_by(_,_)),_))),
                   LTables, 
                   notExists(select(_, _, L, from(M), where(Cond), group_by(Attr), having(Hc), order_by(_,_)))) :- 
      translatePred(ptc, C, LTables, Cond).  
      
translatePred(ntc, not(exists((select(_, _, L, from(M), where(C), group_by(Attr), having(Hc), order_by(_,_)),_))),
                   LTables, 
                   exists(select(_, _, L, from(M), where(Cond), group_by(Attr), having(Hc), order_by(_,_)))) :- 
      translatePred(ptc, C, LTables, Cond). 
        
% ANY: '=_any'(_E, (SQ, _ ))   
translatePred(ptc, AnyCond,
           LTables, 
           % WARNING: all?
           exists(select(_, _, all, from(M), where(and(Cond,bc(Ro1, Op, Ro2))), 
           group_by(AttGb), having(Pred_h),order_by(Oby1, Oby2)))) :-
           
      AnyCond=..Res, Res=[Type, E1, (SQ, _ )],  mySqType(Type, Op, any),
      SQ = select(_, _, [E2], from(M), where(C), group_by(AttGb), having(Pred_h),order_by(Oby1, Oby2)),         
      convert(E1, LTables, Ro1),
      convert(E2, LTables, Ro2),
      translatePred(ptc, C, LTables, Cond).
      
%translatePred(ntc, qAny(E1, Op, Sq),LTables, NCond) :-
%        translatePred(ptc, qNotAny(E1, Op, Sq), LTables, NCond).

 translatePred(ntc, AnyCond,  LTables,  SQt) :-
       AnyCond=..Res, Res=[Type, E1, (SQ, _ )],  mySqType(Type, Op, any),    
       negation(Op, NOp),  
       mySqType(NewType, NOp, all), 
       NewRes=[NewType, E1,(SQ, _ )],
       NewCond=..NewRes,       
       translatePred(ptc, NewCond, LTables, SQt).               

%% NOT ANY
translatePred(ptc, not(AnyCond),  LTables, SQt) :- 
       translatePred(ntc, AnyCond,  LTables,  SQt).
translatePred(ntc, not(AnyCond),  LTables, SQt) :-           
       translatePred(ptc, AnyCond,  LTables, SQt).            
  
% ALL: '=_all'(_E, (SQ, _ ))                                            
translatePred(ptc, AllCond,                        
              LTables, 
            notExists(select(_, _, Ro2, from(M), where(and(Cond,bc(Ro1,NOp,Ro2) )),
                      group_by(AttGb), having(Pred_h),order_by(Oby1, Oby2) ))) :- 
            
      AllCond=..Res, Res=[Type, E1, (SQ, _ )],  mySqType(Type, Op, all),
      SQ = select(_, _, [E2], from(M), where(C), group_by(AttGb), having(Pred_h),order_by(Oby1, Oby2)),         
      negation(Op, NOp),
      convert(E1, LTables, Ro1),
      convert(E2, LTables, Ro2),
      translatePred(ptc, C, LTables, Cond).

%translatePred(ntc, qAll(E1, Op, SQ),LTables, NCond) :-
%        translatePred(ptc, qNotAll(E1, Op, SQ), LTables, NCond).
      
translatePred(ntc, AllCond,                           %qAll(E1, Op, select([E2], from(M), where(C))),
            LTables, 
            SQt) :- 
            
      AllCond=..Res, Res=[Type, E1, (SQ, _ )],  mySqType(Type, Op, all),
      negation(Op, NOp),
      mySqType(NewType, NOp, any), 
      NewRes=[NewType, E1,(SQ, _ )],
      NewCond=..NewRes,       
      translatePred(ptc, NewCond, LTables, SQt).  
        
  
%% NOT ALL                  
translatePred(ptc, not(AllCond), LTables, SQt) :-              
	translatePred(ntc, AllCond, LTables, SQt).  
translatePred(ntc, not(AllCond), LTables, SQt) :-              
	translatePred(ptc, AllCond, LTables, SQt).
                              
                                          
% IN  :ALL: 'in'(_E, (SQ, _ ))
                                            
 translatePred(ptc, InCond ,LTables, SQe) :- 
	InCond =..Res, Res=[Type, E1, (SQ, _ )],  mySqType(Type, _Op, in),
	mySqType(NewType, =, any), 
	NewRes=[NewType, E1,(SQ, _ )],
	NewCond=..NewRes,       
	translatePred(ptc, NewCond, LTables, SQe).  
      
translatePred(ntc, InCond, LTables, NCond) :- 
	InCond=..Res, Res=[Type, E1, (SQ, _ )],  mySqType(Type, _Op, in),
	mySqType(NewType, /=, all), 
	NewRes=[NewType, E1,(SQ, _ )],
	NewCond=..NewRes,
	translatePred(ptc, NewCond, LTables, NCond). 
                                              
  
% NOT IN        
translatePred(ptc, not(InCond), LTables, NCond) :- 
	translatePred(ntc, InCond, LTables, NCond).
translatePred(ntc, not(InCond), LTables, NCond) :- 
	translatePred(ptc, InCond, LTables, NCond).
     
translatePred(ptc, NotInCond ,LTables, SQe) :- 
	NotInCond =..Res, Res=[Type, E1, (SQ, _ )],  mySqType(Type, _Op, not_in),
	mySqType(NewType, /=, all), 
	NewRes=[NewType, E1,(SQ, _ )],
	NewCond=..NewRes,       
	translatePred(ptc, NewCond, LTables, SQe).  
      
translatePred(ntc, NotInCond, LTables, NCond) :- 
	NotInCond=..Res, Res=[Type, E1, (SQ, _ )],  mySqType(Type, _Op, not_in),
	mySqType(NewType, _, in), 
	NewRes=[NewType, E1,(SQ, _ )],
	NewCond=..NewRes,
	translatePred(ptc, NewCond, LTables, NCond). 
                        
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% instantiation( +/-L )
% Try instantiate the calculated instance from the original instance
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
%instantiation([]).
%instantiation([H|T]) :- 
%  H=..[Name,Tuples],
%  data(Name,RealIns),
%  inOrigin(Tuples,RealIns),
%  instantiation(T).  
%
%inOrigin([], _L).
%inOrigin([T|R], L) :- 
%  member(T,L), 
%  inOrigin(R,L).
%inOrigin([_T|R], L) :- 
%	inOrigin(R,L). 

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic functions over views
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%get_attrView(select(_, _, Exp, 
%             from(_Tables), 
%             where(_Pred_w),  
%             group_by(_AttGb),
%             having(_Pred_h), 
%             order_by(_L1, _L2)), 
%             Exp).
%
%get_fromView(select(_, _, _Exp, 
%             from(Tables), 
%             where(_Pred_w),  
%             group_by(_AttGb),
%             having(_Pred_h), 
%             order_by(_L1, _L2)), 
%             Tables).
%                 
%get_whereView(select(_, _, _Exp, 
%              from(_Tables), 
%              where(Pred_w),  
%              group_by(_AttGb),
%              having(_Pred_h), 
%              order_by(_L1, _L2)),  
%              Pred_w).
%                 
%get_groupbyView(select(_, _, _Exp, 
%								from(_Tables), 
%								where(_Pred_w),  
%								group_by(AttGb),
%								having(_Pred_h), 
%								order_by(_L1, _L2)),  
%                AttGb).
%                 
%get_havingView(select(_, _, _Exp, 
%								from(_Tables), 
%								where(_Pred_w),  
%								group_by(_AttGb),
%								having(Pred_h), 
%								order_by(_L1, _L2)),
%                Pred_h).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Group generation
% 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% If no group by clause, there is only a group
groupby([], _, []) :- !.
groupby(Prod, [], [gr(Prod)]) :- !.
groupby(Prod, Agby, ProdGr) :- 

  groups(Prod, Lgroups),                       
  projectAgby(Agby, Lgroups, TupleGrAgb),  % From each group, we take the tuples with the attributes in group by (Ok)
  makegroups(Lgroups, ProdGr),             % Set [gr(L1), gr(L2)]            
  constGroupS1(TupleGrAgb),                % Each group meets the group conditions
  constGroupS2(TupleGrAgb).                % Check that no two distinct groups can be grouped


% Initially, each tuple is a group
  makegroups([],[]).
  makegroups([Gr|Rest], [gr(Gr)|RestGr]) :- makegroups(Rest, RestGr).
  
% Tranform groups into tuples
% undoGr([gr([[a, G], [J,M]]),gr([[A, B]]), gr([[E, M],[Y,U],[A,G]])],L).
% L = [[a,G],[J,M],[A,B],[E,M],[Y,U],[A,G]] ? ;
%undoGr([], []).
%undoGr([gr(L)|R], M) :- 
%	undoGr(R, M1), 
%	append(L, M1, M).

groups([X],[[X]]).     % X is a group gr([])
groups([X|Xs],Res) :-
	groups(Xs,Res0),
	attach(X,Res0,Res).

attach(X,[],[[X]]).
%attach(X,[G|Gs],[[X|G]|Gs]).
attach(X,[G|Gs0],[G|Gs1]) :-
	attach(X,Gs0,Gs1).
attach(X,[G|Gs],[[X|G]|Gs]).

%groupsAll([], []).
%groupsAll([A|R], [[A]|R]).
     
% constGroupS1(Lgrupos).
constGroupS1([]).
constGroupS1([Gr|Rest]) :-  % Gr is the list of tuples which form a group
	verifGr(Gr),          % each group verifies the conditions to be a group
	constGroupS1(Rest).
            
verifGr([]).
verifGr([_T1]) :- !.
verifGr([T1,T2|R]) :- 
       sameT(T1, T2),
       verifGr([T2|R]).        

% Constraints to make two tuples to be the same
sameT([],[]).
sameT([V1|R1],[V2|R2]) :- 
  V1#=V2, 
  sameT(R1,R2).


constGroupS2([_A]) :- !.  % If there is a single group
constGroupS2(LGr) :- 
  representatives(LGr, Rgr),
  generatePk(Rgr).

representatives([], []).
representatives([G|R], [T|Rt]) :-
  first_in_the_group(G, T),
  representatives(R, Rt).      
    
first_in_the_group([], []).       % This case is not possible
first_in_the_group([T|_R], T).    % First element in the group
 
 
 % projectAgby([att(1), att(3)], [[[3,5,6], [3,5,6], [3,5,6]]], L).

  projectAgby(_Latr, [], []).
  projectAgby(Latr, [G|R], [Fg|Fr]) :- 
        proyectGr(Latr, G, Fg),
        projectAgby(Latr, R, Fr).


  proyectGr(_Latr, [], []).
  proyectGr(Latr, [T|Rt], [Ft|FRt]) :- 
            selecAttGrby(Latr, T, Ft),
            proyectGr(Latr, Rt, FRt).
  
  selecAttGrby([], _T, []).       
  selecAttGrby([att(N)|Resto], T1, Tc) :- 
        obtainAttr(N, T1, X1),     
        L1 = [X1],
        selecAttGrby(Resto, T1, Tcr),
        append(L1, Tcr, Tc). 
        
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Suma los elementos de una lista de enteros
  mysumlist([], 0).
  mysumlist([I|R], M) :- 
     mysumlist(R, Maux), 
     M #= I + Maux.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_foreign_key(Tabla , Lista de Foreign_key),       
  get_foreign_key(T,L) :- 
               setof(RIds^(L1, RT, L2),my_foreign_key('$des',T ,L1, RT, L2, RIds),L), !.
  get_foreign_key(_T,[]).         % en el caso de que no tengo fk, ponemos [].
            

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%or3(X, Y, Z) :- Z #= (X + Y + X*Y) mod 2.
%or2(X, Y, Z) :- X #= 0 #/\ Y #= 0 #<=> Z #= 0.
%
%and1(X, Y, Z) :- X #>= 1 #/\ Y #>= 1 #<=> Z#=1.
%or1(X, Y, Z) :- X #>= 1 #\/ Y #>= 1 #<=> Z#=1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Conversion of formats. From test case format to list of facts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tcs_to_facts(TCs,Fs) :-
  tcs_to_facts(TCs,[],Fs).
  
tcs_to_facts([],Fs,Fs).
tcs_to_facts([TC|TCs],Fis,Fos) :-
  TC=..[TableName,Tuples],
  tc_to_facts(Tuples,TableName,F1s),
  append(F1s,Fis,F2s),
  tcs_to_facts(TCs,F2s,Fos).
  
tc_to_facts([],_Functor,[]).
tc_to_facts([L|Ls],Functor,[F|Fs]) :-
  F=..[Functor|L],
  tc_to_facts(Ls,Functor,Fs).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fill zeroes to the left wrt. a bound
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fill_zeroes(Str,Bound,ZStr) :-
  Log is log(Bound)/log(10),
  Places is integer(floor(Log))+1,
  fill_zeroes_aux(Str,Places,ZStr).

fill_zeroes_aux(Str,Places,Str) :-
  length(Str,L),
  L==Places,
  !.
fill_zeroes_aux(Str,Places,ZStr) :-
  length(Str,L),
  L<Places,
  !,
  "0"=[Z],
  fill_zeroes_aux([Z|Str],Places,ZStr).
fill_zeroes_aux(Str,_Places,Str).
  
%%%%%%%%%%%%%%%  END des_tc.pl  %%%%%%%%%%%%%%%
