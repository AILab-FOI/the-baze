/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.4.1                 */
/*                                                       */
/*    SQL Debugger                                       */
/*                                                       */
/*                                                       */
/*                                   Yolanda Garcia-Ruiz */
/*                               Rafael Caballero-Roldan */
/*                                  Fernando Saenz-Perez */
/*                                         (c) 2004-2016 */
/*                                    DSIC DISIA GPD UCM */
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
% Debugging SQL Views
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(buggy/1).   % buggy(Question)
:- dynamic(state/2).   % state(Question,Answer)

% Question:
% - all(RelationName)
% - in(Tuple,RelationName)
% - subset(RelationName1,RelationName2) % RelationName1 is the result of a slicing
% Answer:
% - valid
% - nonvalid
% - missing
% - missing(Tuple)
% - wrong
% - wrong(Tuple)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Command-line option handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

debug_sql(ViewName,Options) :-
  set_flag(debug_max_questions,0),
  get_debug_sql_options(Options,TrustTables,TrustFile,OracleFile,DebugType,TOrder),
  !,
  replace_order_option_for_plain_debugging(TOrder,DebugType,ViewName,Order),
  process_trust_file(TrustFile),
  process_oracle_file(OracleFile),
  debug_sql_code0(ViewName,[TrustTables,TrustFile,OracleFile,DebugType,Order]),
  clean_up_trusted_oracle_views,
  display_debug_statistics(sql).
debug_sql(_ViewName,_Options).

% replace_order_option_for_plain_debugging(order(cardinality),debug(plain),ViewName,order(cardinality)) :-
%   !.
replace_order_option_for_plain_debugging(order(topdown),debug(plain),ViewName,order(Relations)) :-
  !,
  get_view_dependent_relations(ViewName,Relations),
  (gen_buggy(Buggy)
   ->
    my_nth1_member(Buggy,N,Relations),
    rdg((_,Arcs)),
    findall(Child,(member(Buggy/_ + Child/_,Arcs) ; member(Buggy/_ - Child/_,Arcs)),DChildren),
    my_remove_duplicates(DChildren,Children),
    length(Children,L),
    Max is N+L
   ;
    length(Relations,Max)
  ),
  set_flag(debug_max_questions(Max)).
replace_order_option_for_plain_debugging(Order,_,_,Order).

process_trust_file(trust_file(FileName)) :-
  set_flag(trusted_views([])),
  process_trust_oracle_file(FileName,'_trust').

process_oracle_file(oracle_file(FileName)) :-
  set_flag(oracle_views([])),
  process_trust_oracle_file(FileName,'_oracle').

% Process trust file. If trust file is given, process it
process_trust_oracle_file(FileName,Suffix) :-
  (FileName==no(file)
   ->
    true
   ;
    set_flag(trusting,Suffix),
    (development(off) -> (output(Output),set_flag(output,off)) ; true),
    processC(process,[FileName],_NVs,_Continue),
    (development(off) -> set_flag(output,Output) ; true),
    set_flag(trusting,off)
  ).

% Get debug options(+,-,-,-)
get_debug_sql_options(Options,trust_tables(TT),trust_file(TF),oracle_file(OF),debug(D),order(O)) :-  
  get_cmd_options(Options,[trust_tables(TT),trust_file(TF),oracle_file(OF),debug(D),order(O)],
                          [debug_sql_trust_tables_option_test,debug_sql_trust_file_option_test,debug_sql_oracle_file_option_test,debug_sql_debug_option_test,debug_sql_order_option_test],
                          [trust_tables(yes),trust_file(no(file)),oracle_file(no(file)),debug(full),order(cardinality)]).
%                          [trust_tables(yes),trust_file(no(file)),oracle_file(no(file)),debug(full),order(topdown)]).
  
% Get command options
get_cmd_options([],Defaults,_Tests,Defaults).
get_cmd_options([CmdOption|CmdOptions],AllowedOptions,Tests,Defaults) :-
  remove_option(CmdOption,Test,AllowedOptions,NAllowedOptions,Tests,NTests,Defaults,NDefaults),
  !,
  my_map(Test,[CmdOption]),
  get_cmd_options(CmdOptions,NAllowedOptions,NTests,NDefaults).
get_cmd_options([CmdOption|_CmdOptions],_AllowedOptions,_Tests,_Defaults) :-
  write_error_log(['Incorrect argument: ',CmdOption]),
  !,
  fail.
  
% Removing an option
remove_option(X,T,[X|Xs],Xs,[T|Ts],Ts,[_U|Us],Us).
remove_option(X,T,[Y|Xs],[Y|Ys],[TY|Ts],[TY|Ss],[U|Us],[U|Vs]) :-
  remove_option(X,T,Xs,Ys,Ts,Ss,Us,Vs).

% Tests for debug_sql command options
debug_sql_trust_tables_option_test(trust_tables(O)) :-
  POpts=[yes,no],
  (member(O,POpts)
   ->
    true
   ;
    write_error_log(['Incorrect trust table option ''',O,'''. Possible values are: ',POpts]),
    fail).

debug_sql_trust_file_option_test(trust_file(F)) :-
  (my_file_exists_with_default_extensions(F,['.sql'],_FP)
   ->
    true
   ;
    fail).

debug_sql_oracle_file_option_test(oracle_file(F)) :-
  (my_file_exists_with_default_extensions(F,['.sql'],_FP)
   ->
    true
   ;
    fail).

debug_sql_debug_option_test(debug(O)) :-
  POpts=[plain,full],
  (member(O,POpts)
   ->
    true
   ;
    write_error_log(['Incorrect debug type option ''',O,'''. Possible values are: ',POpts]),
    fail).

debug_sql_order_option_test(order(O)) :-
  POpts=[topdown,cardinality],
  (member(O,POpts)
   ->
    true
   ;
    write_error_log(['Incorrect order option ''',O,'''. Possible values are: ',POpts]),
    fail).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Debugging algorithm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

debug_sql_code0(ViewName,Options) :-
  view_arity(ViewName,Arity),
  sub_rdg(ViewName/Arity,(Ns,_)),
  length(Ns,NbrDebugNodes),
  (debug_max_questions(0) -> set_flag(debug_max_questions,NbrDebugNodes) ; true),
  set_flag(debug_nodes,NbrDebugNodes),
  set_flag(debug_questions,0),
  set_flag(debug_tuples,0),
  remove_from_list(trust_file(_),Options,NoTrustFileOptions),
%  ask_oracle(all(ViewName),[],'n',Answer,_NewQuestion), % First enquiry: do not trust file
  ask_oracle(all(ViewName),[root(ViewName)|NoTrustFileOptions],'n',Answer,_NewQuestion), % First enquiry: do not trust file and tag the root viewname for statistics
  (Answer == abort
   ->
    write_info_log(['Debugging aborted by user.'])
   ;
    debug_sql_code1(ViewName,Answer,BuggyViewNames,Options,Abort,Error),
    clean_up_temporary_views,
    (Abort==true
     ->
      write_info_log(['Debugging aborted by user.'])
     ;
      (Error==true
       ->
        set_flag(error(1)),
        set_flag(debug_buggy('**ERROR**')),
        write_error_log(['Unable to locate a buggy node.'])
       ;
        display_sql_buggy_nodes(BuggyViewNames)
      )
    )
  ).

% ask_oracle(+Question,+Options,+DefaultAnswer,-Answer,-NewQuestion)
% Only to display the question
ask_oracle(Question,Options,DefaultAnswer,Answer,NewQuestion) :-
  exec_if_development_on(write_info_log(['Question: ',Question])),
  ask_oracle1(Question,Options,DefaultAnswer,Answer,NewQuestion).
  
% ask_oracle1(+Question,+Options,+DefaultAnswer,-Answer,-NewQuestion)
ask_oracle1(Question,Options,DefaultAnswer,Answer,Question) :-
  Question = all(RelationName),
  !,
  sql_node_type(RelationName/Arity,NodeType),
  input_ask_oracle_all(RelationName,Arity,NodeType,Options,DefaultAnswer,Answer).
ask_oracle1(Question,Options,_DefaultAnswer,Answer,Question) :-
  Question = in(Tuple,RelationName),
  !,
  sql_node_type(RelationName/_Arity,NodeType),
  input_ask_oracle_in(Tuple,RelationName,NodeType,Options,Answer).
ask_oracle1(Question,Options,DefaultAnswer,Answer,NewQuestion) :-
  Question = subset(RelationName1,RelationName2),
  !,
  sql_node_type(RelationName1/Arity1,NodeType1),
  sql_node_type(RelationName2/Arity2,NodeType2),
  input_ask_oracle_subset(RelationName1/Arity1,RelationName2/Arity2,NodeType1,NodeType2,Options,DefaultAnswer,Answer,NewQuestion).

% input_ask_oracle_all(+Rel,+Arity,+NodeType,+Opts,+DefaultAnswer,-Answer)
% Ask oracle for input on question "all"
% Trusted tables:
input_ask_oracle_all(_RelationName,_Arity,table,Options,_DefaultAnswer,valid) :-
  memberchk(trust_tables(yes),Options),
  !.
% Trust file:
input_ask_oracle_all(RelationName,Arity,NodeType,Options,_DefaultAnswer,Answer) :-
  memberchk(trust_file(FileName),Options),
  FileName \== no(file),
  name_trusted(RelationName,'_trust',TrustObjectName),
  rdg((Nodes,_Arcs)),
  rdb_pred_memberchk(TrustObjectName/Arity,Nodes),
  !,
  % Process the node RelationName/Arity:
  length(Args,Arity),
  Query=..[RelationName|Args],
  compute_datalog(Query),
  % Process the same node as RelationName/Arity from trust file:
  functor(TrustQuery,TrustObjectName,Arity),
%   my_term_to_string(TrustQuery,TrustQueryStr),
%   process_datalog(TrustQueryStr),
  compute_datalog(TrustQuery),
%  processC(list_et,[v22],_NVs,yes),
%  (RelationName=v22 -> deb ; true),
  (same_meaning(RelationName,TrustObjectName,Arity)
   ->
    Answer=valid,
    write_info_log([NodeType,' ''',RelationName,''' is valid w.r.t. the trusted file.',nl])
   ;
    Answer=nonvalid,
    write_info_log([NodeType,' ''',RelationName,''' is nonvalid w.r.t. the trusted file.',nl])
  ).
% Answer left to oracle:
input_ask_oracle_all(RelationName,Arity,NodeType,Options,DefaultAnswer,Answer) :-
  %sql_node_type(RelationName/Arity,NodeType),
  display_debugging_info(NodeType,RelationName,Options),
  length(Args,Arity),
  Query=..[RelationName|Args],
  compute_datalog(Query),
  get_ordered_solutions(Query,Solutions),
  number_solutions(Solutions,NumberedSolutions),
  display_bag(NumberedSolutions),
  write_log_list(['Input: Is this the expected answer for ',NodeType,' ''',RelationName,'''? (y/n/m/mT/w/wN/a/h) [',DefaultAnswer,']: ']),
  get_oracle_all_answer(RelationName,Arity,Solutions,NumberedSolutions,Options,StrAnswer),
  (StrAnswer==[]
   ->
    atom_codes(DefaultAnswer,StrDefaultAnswer),
    to_uppercase_char_list(StrDefaultAnswer,Str)
   ;
    Str=[UC|T],
    StrAnswer=[C|T],
    to_uppercase_char(C,UC)
  ),
  ((Str=[] 
   ; 
    Str=="Y")
   ->
    Answer=valid
    ;
    (Str=="N" ->
      Answer=nonvalid
     ;
      (Str=="A" ->
        Answer=abort
       ;
        (missing_tuple_answer(RelationName,Arity,Str,MAnswer)
         ->
          (MAnswer==error ->
            input_ask_oracle_all(RelationName,Arity,NodeType,Options,DefaultAnswer,Answer)
           ;
            Answer=MAnswer
          )
         ;
          (wrong_tuple_answer(Solutions,Str,WAnswer)
           ->
            (WAnswer==error ->
              input_ask_oracle_all(RelationName,Arity,NodeType,Options,DefaultAnswer,Answer)
             ;
              Answer=WAnswer
            )
           ;
            (Str=="H"
             ->
              help_input_ask_oracle,
              input_ask_oracle_all(RelationName,Arity,NodeType,Options,DefaultAnswer,Answer)
             ;
              write_error_log(['Invalid input']),
              help_input_ask_oracle,
              input_ask_oracle_all(RelationName,Arity,NodeType,Options,DefaultAnswer,Answer)
            )
          )
        )
      )
    )
  ).

% input_ask_oracle_in(+Tuple,+Rel,+NodeType,+Opts,-Answer)
% Ask oracle for input on question "in"
% Trusted tables:
input_ask_oracle_in(Tuple,RelationName,table,Options,Answer) :-
  memberchk(trust_tables(yes),Options),
  !,
 (tuple_in_SQL_relation(Tuple,RelationName)
  ->
    Answer = valid
   ;
    Answer = nonvalid).
% Answer left to oracle:
input_ask_oracle_in(Tuple,RelationName,NodeType,Options,Answer) :-
  Question=in(Tuple,RelationName),
  display_debugging_info(NodeType,RelationName,Options),
  Tuple=..[_|UTupleList],
  my_list_to_tuple(UTupleList,UTuple),
  write_log_list(['Input: Should ''',RelationName,''' include a tuple of the form ''',UTuple,'''? (y/n/a) [y]: ']),  
  get_oracle_in_answer(RelationName,UTuple,Options,StrAnswer),
  to_uppercase_char_list(StrAnswer,Str),
  ((Str=[] 
   ; 
    Str=="Y")
   ->
    Answer=valid
    ;
    (Str=="N" ->
      Answer=nonvalid
     ;
      (Str=="A" ->
        Answer=abort
       ;
        write_error_log(['Invalid input']),
        ask_oracle1(Question,Options,'y',Answer,Question)
      )
    )
  ). 
  
% input_ask_oracle_subset(+RelationName1/Arity1,+RelationName2/Arity2,+NodeType1,+NodeType2,+Opts,+DefaultAnswer,-Answer,-NewQuestion)
% Ask oracle for input on question "subset"
% Trusted tables:
input_ask_oracle_subset(RelationName1/Arity1,RelationName2/Arity2,NodeType1,NodeType2,Options,_DefaultAnswer,Answer,NewQuestion) :-
  (NodeType1==table ; NodeType2 == table),
  memberchk(trust_tables(yes),Options),
  !,
  NewQuestion = subset(RelationName1,RelationName2),
  write_info_verb_log(['Checking that the tuples in ',RelationName1,' are a subset of the tuples in the trusted table ',RelationName2,nl]),
  get_tuples_in_relation(RelationName1,Arity1,Solutions1),
  get_tuples_in_relation(RelationName2,Arity2,Solutions2),
  fact_to_tuple_list(Solutions1,Tuples1),
  fact_to_tuple_list(Solutions2,Tuples2),
  (my_set_diff(Tuples1,Tuples2,[])
   ->
    Answer = valid
   ;
    Answer = nonvalid).
% Answer left to oracle:
input_ask_oracle_subset(RelationName1/Arity1,RelationName2/Arity2,NodeType1,NodeType2,Options,DefaultAnswer,Answer,NewQuestion) :-
  Question = subset(RelationName1,RelationName2),
  get_tuples_in_relation(RelationName1,Arity1,Solutions1),
  (Solutions1=[SolTuple]
   ->
    SolTuple=..[_|Args],
    Tuple=..[RelationName2|Args],
    NewQuestion1 = in(Tuple,RelationName2),
    ask_oracle1(NewQuestion1,Options,DefaultAnswer,InAnswer,NewQuestion),
    (InAnswer == nonvalid
     ->
      Answer = wrong(Tuple)
     ;
      Answer = InAnswer
    )
   ;
    NewQuestion = Question,
    exec_if_development_on(
      write_info_log(['Debugging ',NodeType1,' ''',RelationName1,'''.']),
      nl_compact_log,
      display_definition(RelationName1,NodeType1)),
    number_solutions(Solutions1,NumberedSolutions1),
    display_bag(NumberedSolutions1),
    exec_if_verbose_on(
    (sql_node_type(RelationName2/Arity2,NodeType2),
     write_info_log(['Debugging ',NodeType2,' ''',RelationName2,'''.']),
     nl_compact_log,
     display_definition(RelationName2,NodeType2))),
    (development(on)
     ->
      get_tuples_in_relation(RelationName2,Arity2,Solutions2),
      display_bag(Solutions2),
      write_log_list(['Input: Is ''',RelationName1,''' included in the expected answer of ''',RelationName2,'''? (y/n/wN/a) [y]: '])
     ;
      write_log_list(['Input: Is the above set included in the expected answer of ''',RelationName2,'''? (y/n/wN/a) [y]: '])
    ),
    get_oracle_subset_answer(Solutions1,RelationName2/Arity2,Options,StrAnswer),
    to_uppercase_char_list(StrAnswer,Str),
    ((Str=[] 
     ; 
      Str=="Y")
     ->
      Answer=valid
      ;
      (Str=="N"
       ->
        Answer=nonvalid
       ;
       (wrong_tuple_answer(Solutions1,Str,WAnswer)
        ->
         (WAnswer==error ->
           ask_oracle(Question,Options,DefaultAnswer,Answer,NewQuestion)
          ;
           Answer=WAnswer
         )
        ;
	       (Str=="A"
	        ->
	          Answer=abort
	         ;
	          write_error_log(['Invalid input']),
	          ask_oracle(Question,Options,'y',Answer,NewQuestion)
	        )
	     )
      )
    )
  ).

get_oracle_all_answer(RelationName,_Arity,Solutions,_NumberedSolutions,Options,StrAnswer) :-
  inc_flag(debug_questions),
  length(Solutions,NbrTuples),
  add_to_flag(debug_tuples,NbrTuples),
  (memberchk(root(RelationName),Options)
   ->
    set_flag(debug_root_tuples,NbrTuples)
   ;
    true
  ),
  memberchk(oracle_file(FileName),Options),
  FileName == no(file),
  !,
  user_input_string(StrAnswer).
get_oracle_all_answer(RelationName,Arity,Solutions,_NumberedSolutions,Options,StrAnswer) :-
  memberchk(debug(DebugType),Options),
  name_trusted(RelationName,'_oracle',OracleRelName),
  length(Args,Arity),
  Query=..[OracleRelName|Args],
  compute_datalog(Query),
  get_ordered_solutions(Query,OracleSolutions),
  fact_to_tuple_list(Solutions,SolTuples),
  fact_to_tuple_list(OracleSolutions,OracleSolTuples),
  (SolTuples==OracleSolTuples
   ->
     StrAnswer="y"
   ;
     (DebugType==full
      ->
       (my_set_diff(OracleSolTuples,SolTuples,[MissingTuple|_MissingTuples])
        ->
         build_oracle_answer("m",MissingTuple,StrAnswer)
        ;
         (my_set_diff(SolTuples,OracleSolTuples,[WrongTuple|_WrongTuples])
          ->
           my_nth1_member(WrongTuple,N,SolTuples),
           number_codes(N,StrN),
           append("w",StrN,StrAnswer)
          ;
           StrAnswer="n"
         )
       )
      ;
       StrAnswer="n"
     )
  ),
  write_string_log(StrAnswer),
  nl_log.

get_oracle_in_answer(_RelationName,_Tuple,Options,StrAnswer) :-
  inc_flag(debug_questions),
  inc_flag(debug_tuples),
  memberchk(oracle_file(FileName),Options),
  FileName == no(file),
  !,
  user_input_string(StrAnswer).
get_oracle_in_answer(RelationName,Tuple,_Options,StrAnswer) :-
  name_trusted(RelationName,'_oracle',OracleRelName),
  my_list_to_tuple(Args,Tuple),
  OracleTuple=..[OracleRelName|Args],
  (tuple_in_SQL_relation(OracleTuple,OracleRelName)
   ->
     StrAnswer="y"
   ;
     StrAnswer="n"),
  write_string_log(StrAnswer),
  nl_log.
   
get_oracle_subset_answer(Solutions1,_Solutions2,Options,StrAnswer) :-
  inc_flag(debug_questions),
  length(Solutions1,NbrTuples),
  add_to_flag(debug_tuples,NbrTuples),
  memberchk(oracle_file(FileName),Options),
  FileName == no(file),
  !,
  user_input_string(StrAnswer).
get_oracle_subset_answer(Solutions1,RelationName2/Arity2,Options,StrAnswer) :-
  memberchk(debug(DebugType),Options),
  name_trusted(RelationName2,'_oracle',OracleRelName2),
  get_tuples_in_relation(OracleRelName2,Arity2,Solutions2),
  fact_to_tuple_list(Solutions1,Tuples1),
  fact_to_tuple_list(Solutions2,Tuples2),
  my_set_diff(Tuples1,Tuples2,DiffTuples),
  (DiffTuples==[]
   ->
    StrAnswer = "y"
   ;
    (DebugType==full % This should not be needed as this point is only reached when wrong or missing is previously answered
     ->
      DiffTuples=[WrongTuple|_],
      my_nth1_member(WrongTuple,N,Tuples1),
      number_codes(N,StrN),
      append("w",StrN,StrAnswer)
     ;
      StrAnswer = "n")).

build_oracle_answer(StrPre,Tuple,StrAnswer) :-
  my_list_to_tuple(Xs,Tuple),
  display_to_string(write_sql_value_values(Xs),StrTuple),
  append(StrPre,StrTuple,StrAnswer).
  

display_debugging_info(table,_RelationName,Options) :-
  memberchk(trust_tables(yes),Options),
  !.
display_debugging_info(NodeType,RelationName,_Options) :-
  write_info_log(['Debugging ',NodeType,' ''',RelationName,'''.']),
  exec_if_verbose_on(
   nl_compact_log,
   display_definition(RelationName,NodeType),
   nl_compact_log).

help_input_ask_oracle :-
  write_info_log(['Possible answers are:\n y (yes)\n n (no)\n m (some missing tuple(s) in answer)\n mT (missing tuple T as a comma-separated list of SQL constants or placeholders ''_'') \n w (wrong answer)\n wN (wrong tuple at position N)\n a (abort)\n h (this help) ']).
  
number_solutions(Solutions,NumberedSolutions) :-
  length(Solutions,L),
  from(1,L,Ns),
  my_zipWith('-',Ns,Solutions,NumberedSolutions).  
  
missing_tuple_answer(RelationName,Arity,Str,Answer) :-
  missing_tuple_answer(RelationName,Arity,Answer,Str,[]).
  
missing_tuple_answer(RelationName,Arity,missing(Tuple)) -->
  "M",
  {functor(Tuple,RelationName,Arity)}.
missing_tuple_answer(RelationName,Arity,Answer) -->
  "M",
  my_blanks_star,
  my_noncompound_terms(Cs,[],_Vo),
  {length(Cs,Arity)
   ->
    Tuple=..[RelationName|Cs],
    Answer=missing(Tuple)
   ;
    write_error_log(['Incorrect number of arguments. It must be ',Arity]),
    Answer=error}.

wrong_tuple_answer([],_Str,error) :-
  !, % No solution tuple. So, no way to be incorrect.
  write_error_log(['Empty relation. Cannot be incorrect. Maybe missing?']).
wrong_tuple_answer(Solutions,Str,Answer) :-
  wrong_tuple_answer(Solutions,Answer,Str,[]).  
wrong_tuple_answer([Solution],wrong(Solution)) -->
  "W".
wrong_tuple_answer([Solution|_Solutions],wrong(Tuple)) -->
  "W",
  {functor(Solution,RelationName,Arity),
   functor(Tuple,RelationName,Arity)}.
wrong_tuple_answer(Solutions,Answer) -->
  "W",
  my_number(N),
  {length(Solutions,L),
   (N>0,
    N=<L
    ->
     nth1(N,Solutions,Tuple),
     Answer=wrong(Tuple)
    ;
     write_error_log(['Invalid tuple number. It must be between 1 and ',L]),
     Answer=error
    )
   }.

display_definition(_RelationName,table) :-
  !.
display_definition(RelationName,view) :-
  get_sql_view_definition(RelationName,SQLst),
  display_sql(SQLst, 2).


display_sql_buggy_nodes(ViewNames) :-
  development(off),
  !,
  filter_temporary_views(ViewNames,UserViewNames),
  my_remove_duplicates_sort(UserViewNames,OrdUserViewNames),
  display_all_sql_buggy_nodes(OrdUserViewNames).
display_sql_buggy_nodes(ViewNames) :-
  my_remove_duplicates_sort(ViewNames,OrdViewNames),
  display_all_sql_buggy_nodes(OrdViewNames).
  
filter_temporary_views([],[]).
filter_temporary_views([ViewName|ViewNames],UserViewNames) :-
  is_temporary_view(ViewName),
  !,
  filter_temporary_views(ViewNames,UserViewNames).
filter_temporary_views([ViewName|ViewNames],[ViewName|UserViewNames]) :-
  filter_temporary_views(ViewNames,UserViewNames).

display_all_sql_buggy_nodes([BuggyViewName]) :-
  sql_node_type(BuggyViewName/_A,NodeType),
  write_info_log(['Buggy ',NodeType,' found: ''',BuggyViewName,'''.']),
  set_flag(debug_buggy(BuggyViewName)),
  !.
display_all_sql_buggy_nodes(BuggyViewNames) :-
  write_info_log(['Buggy relations: ',BuggyViewNames]).
  
% Clean up trusted views after trusted debugging. The extension table is cleared if there are such views
clean_up_trusted_oracle_views :-
  (trusted_views([_|_])
   ;
   oracle_views([_|_])),
  !,
%   (trusted_views([TVN|TVNs]) -> drop_viewname_k_list([TVN|TVNs]) ; true),
%   (oracle_views([OVN|OVNs])  -> drop_viewname_k_list([OVN|OVNs]) ; true),
  (trusted_views([TVN|TVNs]) -> drop_viewname_u_list([TVN|TVNs]) ; true),
  (oracle_views([OVN|OVNs])  -> drop_viewname_u_list([OVN|OVNs]) ; true),
  processC(clear_et,[],_NVs,_Yes).
%  compute_stratification.
clean_up_trusted_oracle_views.
  
% Clean up temporary views created along debugging (those containing '_slice' in their names)
clean_up_temporary_views :-
  current_db(Connection),
  findall(
    V,
    (view_exists(Connection,V),
     is_temporary_view(V)
    ),
    [V|Vs]),
  drop_viewname_k_list([V|Vs]),
  !,
  processC(clear_et,[],_NVs,_Yes),
  compute_stratification.
clean_up_temporary_views.
  % Don't recompute stratification if no temporary views were dropped
    
is_temporary_view(ViewName) :-
  atom_codes(ViewName,StrViewName),
  is_temporary_view_name(StrViewName,"").
  
is_temporary_view_name -->
  my_chars(_),
  my_kw("_SLICE"),
  my_chars(_).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code 1 debug(V,A)
% Input: V view name, A answer
% Output: Buggy view names

debug_sql_code1(ViewName,Answer,BuggyViewNames,Options,Abort,Error) :-
  initial_set_of_clauses(ViewName,Answer,Options,Program),
  debug_sql_code1_loop(Program,BuggyViewNames,Options,Abort,Error).
  
debug_sql_code1_loop(Program,BuggyViewNames,Options,Abort,Error) :-
  get_buggy(Program,CurrentBuggyViewNames),
  (CurrentBuggyViewNames==[]
   ->
    (choose_question_from_program(Program,Options,Question)
     ->
      ask_oracle(Question,Options,'y',NewAnswer,_NewQuestion),
      (NewAnswer==abort
       ->
        Abort=true
       ;
        process_answer(Question,NewAnswer,Options,Program,NewProgram),
%         exec_if_development_on(
%           write_info_log(['Current logic program:']),
%           display_rules_list(NewProgram,2),
%           nl_compact_log),
        debug_sql_code1_loop(NewProgram,BuggyViewNames,Options,Abort,Error)
      )
     ;
      Error=true
    )
   ;
    BuggyViewNames=CurrentBuggyViewNames,
    exec_if_development_on(
      write_info_log(['Final logic program:']),
      display_rules_list(Program,2),
      display_nbr_rules(Program),
      nl_compact_log)
  ).
  
get_buggy(Program,BuggyViewNames) :-
  retractall(buggy(_)),
  retractall(state(_,_)),
  my_map(assertz,Program),
  findall(Question,buggy(Question),Questions),
  questions_buggy_view_names(Questions,BuggyViewNames).
  
questions_buggy_view_names(X,X).

choose_question_from_program(Program,Options,Question) :-
  get_unsolved_questions(Program,UnsolvedQuestions),
  choose_question_from_questions(UnsolvedQuestions,Options,Question).

get_unsolved_questions(Program,UnsolvedQuestions) :-
  setof(Question,
    H^B^Program^Bs^BAnswer^FAnswer^Relation^
    (member(':-'(H,B),Program),
     my_list_to_tuple(Bs,B),
     member(state(Question,BAnswer),Bs),
     \+ member(state(Question,FAnswer),Program),  % Not solved already
     relation_in_question(Question,Relation),
     complete_path_including_no_valid(Relation,Program)
    ),
     UnsolvedQuestions).
% get_unsolved_questions(Program,UnsolvedQuestions) :-
%   findall(Question,
%     (member(':-'(_H,B),Program),
%      my_list_to_tuple(Bs,B),
%      member(state(Question,_BAnswer),Bs),
%      \+ member(state(Question,_FAnswer),Program),  % Not solved already
%      relation_in_question(Question,Relation),
%      complete_path_including_no_valid(Relation,Program)
%     ),
%      DUnsolvedQuestions),
%   my_remove_duplicates(DUnsolvedQuestions,UnsolvedQuestions).
  
relation_in_question(all(Name),Name).
relation_in_question(in(_Tuple,Name),Name).
relation_in_question(subset(_Name,Name),Name).

complete_path_including_no_valid(Node,Program) :-
  findall(ValidNode/_,member(state(all(ValidNode),valid),Program),ValidNodeList),
  complete_path_including_no_valid(Node/_,ValidNodeList,Program).
    
complete_path_including_no_valid(Node,ValidNodeList,_Program) :-
  % Dependency graph can be got from _Program, but we have computed it already 
  rdg((_,As)),
  complete_path_including_no_valid(Node,_,As,ValidNodeList,[],_,[],_).
  
complete_path_including_no_valid(F,T,As,VNL,IVAs,OVAs,IVNs,OVNs) :-
  pdg_arc_including_no_valid(F,T,As,VNL,IVAs,OVAs,IVNs,OVNs),
  last_node_in_path(T,As),
  !.
complete_path_including_no_valid(F,T,As,VNL,IVAs,OVAs,IVNs,OVNs) :-
  pdg_arc_including_no_valid(F,T1,As,VNL,IVAs,I1VAs,IVNs,I1VNs),
  complete_path_including_no_valid(T1,T,As,VNL,I1VAs,OVAs,I1VNs,OVNs).

last_node_in_path(F,As) :-
  from_to_arc(Arc,F,_),
  memberchk(Arc,As),
  !,
  fail.
last_node_in_path(_F,_As).
  
pdg_arc_including_no_valid(F,T,As,VNL,IVAs,[Arc|IVAs],IVNs,[F,T|IVNs]) :-
  from_to_arc(Arc,F,T),
  member(Arc,As),
  \+ memberchk(Arc,IVAs), % Arc not visited before
  \+ memberchk(T,VNL).    % 'To' node is not known as a valid node
 
choose_question_from_questions([Question],_Options,Question) :-
  !.
choose_question_from_questions(Questions,Options,Question) :- % Select the question following the order in the input list of questions
  memberchk(debug(plain),Options),
  memberchk(order(Order),Options),
  my_is_list(Order),
  !,
  choose_plain_question_from_questions(Questions,Order,Question).
choose_question_from_questions([Question|Questions],_Options,ChosenQuestion) :- % Select the question with lower cardinality
  question_cardinality(Question,QuestionCardinality),
  choose_question_from_questions(Questions,QuestionCardinality,Question,ChosenQuestion).

choose_question_from_questions([],_Cardinality,Question,Question).
choose_question_from_questions([Question|Questions],Cardinality,_CurrentBestQuestion,BestQuestion) :-
  question_cardinality(Question,QuestionCardinality),
  Cardinality>QuestionCardinality,
  !,
  choose_question_from_questions(Questions,QuestionCardinality,Question,BestQuestion).
choose_question_from_questions([_Question|Questions],Cardinality,CurrentBestQuestion,BestQuestion) :-
  choose_question_from_questions(Questions,Cardinality,CurrentBestQuestion,BestQuestion).

question_cardinality(in(_Tuple,_RelationName),1).
question_cardinality(subset(RelationName,_Rel),QuestionCardinality) :-
  relation_cardinality(RelationName,QuestionCardinality).
question_cardinality(all(RelationName),QuestionCardinality) :-
  relation_cardinality(RelationName,QuestionCardinality).

  
% Chosing as a naïve standard declarative debugger

choose_plain_question_from_questions(Questions,Order,all(Relation)) :-
  relations_in_all_questions(Questions,Relations),
  memberchk_list(Order,Relations,Relation).
  
relations_in_all_questions([],[]).
relations_in_all_questions([all(Relation)|Questions],[Relation|Relations]) :-
  !,
  relations_in_all_questions(Questions,Relations).
relations_in_all_questions([_|Questions],Relations) :-
  relations_in_all_questions(Questions,Relations).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code 2 initialSetOfClauses
% Input: V view name, A answer
% Output: A list of clauses

initial_set_of_clauses(ViewName,Answer,Options,Program) :-
  (memberchk(trust_tables(yes),Options)
   ->
    rdg(PDG),
    view_arity(ViewName,Arity),
    sub_pdg(ViewName/Arity,PDG,(Nodes,_)),
    findall(N,(member(N/A,Nodes),\+ view_arity(N,A)),TableNames),
    findall(state(all(TableName),valid),member(TableName,TableNames),ProgramIn)
   ;
    ProgramIn=[]
  ),
  push_flag(development,off,OldFlag),
  initialize_debug_logic_program(ViewName,ProgramIn,InitializedProgram),
  process_answer(all(ViewName),Answer,Options,InitializedProgram,Program),
  pop_flag(development,OldFlag),
  exec_if_development_on(
    write_info_log(['Initial logic program:']),
    display_rules_list(Program,2),
    display_nbr_rules(Program)).
     
initialize_debug_logic_program(ViewName,ProgramIn,ProgramOut) :-
  create_buggy_clause(ViewName,Clause),
  get_view_relations(ViewName,Relations),
  add_to_program([Clause],ProgramIn,ProgramIn1),
  initialize_debug_logic_program_list(Relations,ProgramIn1,ProgramOut).
  
initialize_debug_logic_program_list([],Program,Program).
initialize_debug_logic_program_list([Relation|Relations],ProgramIn,ProgramOut) :-
  initialize_debug_logic_program(Relation,ProgramIn,ProgramIn1),
  initialize_debug_logic_program_list(Relations,ProgramIn1,ProgramOut).
  
create_buggy_clause(ViewName,Clause) :-  
  get_view_relations(ViewName,Relations),
  build_relation_state_body(Relations,BodyList),
  my_list_to_tuple([state(all(ViewName),nonvalid)|BodyList],Body),
  Clause = ':-'(buggy(ViewName),Body).

build_relation_state_body([],[]).
build_relation_state_body([Relation|Relations],[state(all(Relation),valid)|Goals]) :-
  build_relation_state_body(Relations,Goals).

% Get the relations on which a given view depends (the view itself is also returned). Ordering is as a naive declarative debugger traverses the computation tree
get_view_dependent_relations(TableName,[TableName]) :-
  \+ view_exists(TableName),
  !.
get_view_dependent_relations(ViewName,[ViewName|Relations]) :-
  get_unordered_view_relations(ViewName,ViewRelations),
  get_view_dependent_relations_list(ViewRelations,DRelations),
  my_remove_duplicates(DRelations,Relations).
  
get_view_dependent_relations_list([],[]).
get_view_dependent_relations_list([ViewName|ViewNames],Relations) :-
  get_view_dependent_relations(ViewName,ViewRelations),
  get_view_dependent_relations_list(ViewNames,ViewsRelations),
  append(ViewRelations,ViewsRelations,Relations).
  
get_view_relations(ViewName,Relations) :-
  get_unordered_view_relations(ViewName,URelations),
  my_sort(URelations,Relations).

get_unordered_view_relations(ViewName,Relations) :-
  get_sql_view_definition(ViewName,SQL),
  !,
  get_view_relations_from_sql(SQL,Relations).
get_unordered_view_relations(_ViewName,[]).

get_view_relations_from_sql((SQL,_),Relations) :-
  get_view_relations_from_sql(SQL,Relations).
get_view_relations_from_sql(not(SQL),Relations) :-
  get_view_relations_from_sql(SQL,Relations).
get_view_relations_from_sql(SQL,Relations) :-
  SQL = select(_D,_T,_PL,_F,_W,_GB,_H,_OB),
  !,
  get_basic_view_relations_from_sql(SQL,Relations).
get_view_relations_from_sql(SQL,Relations) :-
  (SQL = union(_,SQLst1,SQLst2)
   ;
   SQL = except(_,SQLst1,SQLst2)
   ;
   SQL = intersect(_,SQLst1,SQLst2)),
  (SQLst1 = (SQL1,_) ; SQLst1=SQL1),
  (SQLst2 = (SQL2,_) ; SQLst2=SQL2),
  !,
  get_view_relations_from_sql(SQL1,Relations1),
  get_view_relations_from_sql(SQL2,Relations2),
  concat_lists([Relations1,Relations2],DupRelations),
%  my_remove_duplicates_sort(DupRelations,Relations).
  my_remove_duplicates(DupRelations,Relations).
get_view_relations_from_sql(SQL,Relations) :-
  SQL = with(SQLq,SQLs),
  get_view_relations_from_sql(SQLq,SQLRelations),
  get_view_relations_from_sql_list(SQLs,SQLsRelations),
  append(SQLRelations,SQLsRelations,DupRelations),
%  my_remove_duplicates_sort(DupRelations,Relations).
  my_remove_duplicates(DupRelations,Relations).
  
get_view_relations_from_sql_list(SQLs,SQLsRelations) :-
  get_view_relations_from_sql_list(SQLs,[],SQLsRelations).
  
get_view_relations_from_sql_list([],Rs,Rs).
get_view_relations_from_sql_list([SQL|SQLs],Rsi,Rso) :-
  get_view_relations_from_sql(SQL,Rsi1),
  append(Rsi,Rsi1,Rsi2),
  get_view_relations_from_sql_list(SQLs,Rsi2,Rso).
  
get_basic_view_relations_from_sql(SQL,Relations) :-
  SQL = select(_D,_T,_PL,From,Where,_GB,_H,_OB),
  get_from_relations(From,FromRelations),
  get_where_relations(Where,WhereRelations),
  concat_lists([FromRelations,WhereRelations],DupRelations),
%  my_remove_duplicates_sort(DupRelations,Relations).
  my_remove_duplicates(DupRelations,Relations).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code 3 processAnswer
% Input: Q question, A answer
% Output: Logic program

process_answer(Question,Answer,Options,Program,NewProgram) :-
  exec_if_development_on(write_info_log(['processAnswer(',Question,',',Answer,')'])),
  ((Answer==valid ; Answer==nonvalid)
   ->
    Clauses=[state(Question,Answer)]
   ;
   (Answer=missing(_) ; Answer=wrong(_))
    ->
     Clauses=[state(Question,nonvalid)]
    ;
     Clauses=[] 
  ),
  add_to_program(Clauses,Program,Program1),
  process_question(Question,Answer,Options,Program1,NewProgram).
 
 
process_question(in(Tuple,RelationName),Answer,Options,Program,NewProgram) :-
  (tuple_in_SQL_relation(Tuple,RelationName),
   Answer == nonvalid 
   ->
    process_answer(all(RelationName),wrong(Tuple),Options,Program,NewProgram)
   ;
    (\+ tuple_in_SQL_relation(Tuple,RelationName),
        Answer == valid
     ->
      process_answer(all(RelationName),missing(Tuple),Options,Program,NewProgram)
     ;
      NewProgram = Program
    )
  ),
  !.
process_question(subset(_ViewName,RelationName),wrong(Tuple),Options,Program,NewProgram) :-
  !,
  process_answer(all(RelationName),wrong(Tuple),Options,Program,NewProgram).
process_question(all(ViewName),Answer,Options,Program,NewProgram) :-
  (Answer = wrong(_)
   ;
   Answer = missing(_)
  ),
  get_sql_view_definition(ViewName,Question),
  !,
  slice(ViewName,Question,Answer,Options,Clauses),
  add_to_program(Clauses,Program,NewProgram).
process_question(_Query,_Answer,_Options,Program,Program).
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code 4 slice
% Input: V: view, Q query, A answer
% Output: Logic program

slice(ViewName,Query,Answer,Options,NewProgram) :-
  Program1=[],
% Missing tuple  
  (Answer=missing(Tuple)
   ->
    tuple_SQL_query_cardinality(Tuple,Query,Cardinality),
    (intersection_query(Query,Query1,Query2)
     ->
      (tuple_SQL_query_cardinality(Tuple,Query1,Cardinality1),
       Cardinality1==Cardinality
       ->
        slice(ViewName,Query1,Answer,Options,NewClauses1),
        add_to_program(NewClauses1,Program1,Program2)
       ;
        Program2=Program1
      ),
      (tuple_SQL_query_cardinality(Tuple,Query2,Cardinality2),
       Cardinality2==Cardinality
       ->
        slice(ViewName,Query2,Answer,Options,NewClauses2),
        add_to_program(NewClauses2,Program2,NewProgram)
       ;
        NewProgram=Program2
      )
     ;
      (difference_query(Query,Query1,Query2,D)
       ->
        (
          (tuple_SQL_query_cardinality(Tuple,Query1,Cardinality1),
           Cardinality1==Cardinality
           ->
            slice(ViewName,Query1,Answer,Options,NewClauses1),
            add_to_program(NewClauses1,Program1,NewProgram)
           ;
            Program2=Program1
          ),
          (D\==all,
           \+ \+ tuple_in_SQL_query(Tuple,Query2) % Do not unify Tuple as it can be a partial tuple
  %         total_tuple(Tuple)
           ->
            slice(ViewName,Query2,wrong(Tuple),Options,NewClauses2),
            add_to_program(NewClauses2,Program2,NewProgram)
           ;
            NewProgram=Program2
          )
        )
       ;
       (basic_query(Query)
        ->
         missing_basic(Program1,ViewName,Query,Tuple,NewProgram)
        ;
         NewProgram=Program1 % Nothing else added for missing(Tuple)
       )
      )
    )
   ;
% Wrong tuple  
    (Answer=wrong(Tuple)
     ->
      (get_union_query(Query,Query1,Query2)
       ->
        (tuple_in_SQL_query(Tuple,Query1)
         ->
          slice(ViewName,Query1,Answer,Options,NewClauses1),
          add_to_program(NewClauses1,Program1,Program2)
         ;
          Program2=Program1
        ),
        (tuple_in_SQL_query(Tuple,Query2)
         ->
          slice(ViewName,Query2,Answer,Options,NewClauses2),
          add_to_program(NewClauses2,Program2,NewProgram)
         ;
          NewProgram=Program2
        )
       ;
        (basic_query(Query)
         ->
           wrong_basic(Program1,ViewName,Query,Tuple,Options,NewProgram)
         ;
           NewProgram=Program1 % Nothing else added for wrong(Tuple)
        )
      )
    ;
     NewProgram=Program1 % Nothing else added for valid/nonvalid
    )
  ).
  
  
% Intersection query
intersection_query(
  select(D,T,PL,F,where(and(LC,RC)),GB,H,OB),
  select(D,T,PL,F,where(LC),GB,H,OB),
  select(D,T,PL,F,where(RC),GB,H,OB)).
intersection_query(
  intersect(_D,(LR,_),(RR,_)), % WARNING: distinct
  LR,
  RR).


% Difference query
difference_query(
  except(D,(LR,_),(RR,_)), 
  LR,
  RR,
  D).

                   
% Basic query
basic_query(Query) :-
  Query = select(_D,_T,_PL,_F,_W,group_by([]),_H,_OB).

  
% Union query
get_union_query(
  select(D,T,PL,F,where(or(LC,RC)),GB,H,OB),
  select(D,T,PL,F,where(LC),GB,H,OB),
  select(D,T,PL,F,where(RC),GB,H,OB)).
get_union_query(
  union(_D,(LR,_),(RR,_)), % WARNING: distinct
  LR,
  RR).

   
% Total tuple
total_tuple(Tuple) :-
  my_ground(Tuple).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code 5 missingBasic
% Input: L logic program, V view name, Q query, T tuple
% Output: Logic program

missing_basic(Program,ViewName,Query,Tuple,NewProgram) :-
  Query           = select(D,T,PL,F,where(_C),GB,H,OB),
  UnfilteredQuery = select(D,T,PL,F,where(true),GB,H,OB),
  (\+ tuple_in_SQL_query(Tuple,UnfilteredQuery)
   ->
    get_from_relations(F,Relations),
    add_missing_basic_list(Relations,ViewName,Query,Tuple,Program,Program1)
   ;
    Program1=Program
  ),
  NewProgram=Program1.
  
add_missing_basic_list([],_ViewName,_Query,_Tuple,Program,Program).
add_missing_basic_list([Relation|Relations],ViewName,Query,Tuple,Program,NewProgram) :-
  add_missing_basic(Relation,ViewName,Query,Tuple,Program,Program1),
  add_missing_basic_list(Relations,ViewName,Query,Tuple,Program1,NewProgram).
  
% This version is more efficient than the one in the FLOPS paper
add_missing_basic(Relation,ViewName,Query,Tuple,Program,NewProgram) :-
  generate_undefined(Relation,STuple),
  fill_missing_tuple(STuple,Relation,Query,Tuple), % WARNING. The renaming of the relation should be passed
  generate_condition(STuple,Relation,Condition),
  FilteredQuery = select(all,top(all),'*',from([(Relation,_Renaming)]),where(Condition),group_by([]),having(true),order_by([],[])),
  (is_empty_sql_answer(FilteredQuery)
   ->
    add_to_program(
      [':-'(buggy(ViewName),state(in(STuple,Relation),nonvalid))],
%       ':-'(state(all(Relation),nonvalid),state(in(STuple,Relation),valid))],
      Program,NewProgram)
   ;
    NewProgram=Program
  ).
  
% The following is as in the FLOPS paper
% add_missing_basic(Relation,ViewName,Query,Tuple,Program,NewProgram) :-
%   generate_undefined(Relation,STuple),
%   fill_missing_tuple(STuple,Relation,Query,Tuple),
%   (\+ tuple_in_SQL_relation(STuple,ViewName)
%    ->
%     add_to_program(
%       [':-'(buggy(ViewName),state(in(STuple,Relation),nonvalid)),
%        ':-'(state(all(Relation),nonvalid),state(in(STuple,Relation),valid))],
%       Program,NewProgram)
%    ;
%     NewProgram=Program
%   ).
  
% Fill missing tuple
fill_missing_tuple(STuple,Relation,Query,Tuple) :-
  get_projection_list(Query,PL),
  Tuple =.. [_|TArgs],
  fill_missing_tuple_arg_list(PL,Relation,TArgs,STuple).

% Get projection list as a list with either attr(Rel,Attr,Renaming) or other expressions
get_projection_list(Query,PL) :-
  sql_to_ra((Query,_Schema),(RA,_RASchema),[],_TableRen),
  RA=pi(_D,_T,RPL,_,_,_,_),
  PL=RPL.
%  apply_renamings(RPL,TableRen,PL).
  
% apply_renamings([],_TableRen,[]).  
% apply_renamings([attr(R,A,AS)|Args],TableRen,[attr(UR,A,AS)|UArgs]) :-
%   member((UR,R),TableRen),
%   !,
%   apply_renamings(Args,TableRen,UArgs).  
% apply_renamings([Arg|Args],TableRen,[Arg|UArgs]) :-
%   apply_renamings(Args,TableRen,UArgs).
  
fill_missing_tuple_arg_list([],_Relation,[],_).
fill_missing_tuple_arg_list([attr(Relation,Attr,_Ren)|Args],Relation,[Value|TArgs],STuple) :-
  nonvar(Value),
  !,
  set_attr_tuple(Relation,Attr,Value,STuple),
  fill_missing_tuple_arg_list(Args,Relation,TArgs,STuple).
fill_missing_tuple_arg_list([_Arg|Args],Relation,[_TArg|TArgs],STuple) :-
  fill_missing_tuple_arg_list(Args,Relation,TArgs,STuple).

set_attr_tuple(Relation,Attr,Value,STuple) :-
  my_attribute(_,I,Relation,Attr,_Type),
  arg(I,STuple,Value).

    
generate_undefined(Relation,Tuple) :-
  current_db(Connection),
  my_table(Connection,Relation,Arity),
  length(Args,Arity),
  Tuple =.. [Relation|Args].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code 6 generateCondition
% Input: S tuple, R relation
% Output: SQL Condition
% Former code for missingBasic

generate_condition(STuple,Relation,Condition) :-
  get_attributes(Relation,Attrs),
  STuple=..[_Relation|SValues],
  filter_undefined(SValues,Attrs,FSValues,FAttrs),
  my_zipWith('=',FAttrs,FSValues,AndConds),
  list_to_and_condition(AndConds,Condition).
  
get_attributes(Relation,Attrs) :-
  get_table_untyped_arguments(Relation,ColNames),
  attr_internal_representation_list(ColNames,Attrs).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code 6 wrongBasic
% Input: L logic program, V view name, Q query, T tuple
% Output: Logic program

wrong_basic(Program,ViewName,Query,Tuple,Options,NewProgram) :-
  Query = select(_D,_T,_PL,F,W,_GB,_H,_OB),
  get_all_from_relations(F,AllFromRelationsRenamings),
  remove_tables_if_trusted(AllFromRelationsRenamings,Options,FromRelationsRenamings),
  relevant_tuples_list(FromRelationsRenamings,Query,Tuple,ViewNames),
  get_where_relations(W,WhereRelations),
  my_unzip(FromRelationsRenamings,FromRelations,_FromRenamings),
  build_subset_answer_state_list(ViewNames,FromRelations,SubsetStates),
  build_all_answer_state_list(WhereRelations,AllStates),
  append(SubsetStates,AllStates,StateList),
  my_list_to_tuple(StateList,States),
  add_to_program([':-'(buggy(ViewName),States)],Program,NewProgram).

%remove_tables_if_trusted(R,_Options,R).
remove_tables_if_trusted([],_Options,[]).
remove_tables_if_trusted([(Relation,_Renaming)|RelationsRenamings],Options,FilteredRelationsRenamings) :-
  memberchk(trust_tables(yes),Options),
  sql_node_type(Relation/_,table),
  remove_tables_if_trusted(RelationsRenamings,Options,FilteredRelationsRenamings).
remove_tables_if_trusted([(Relation,Renaming)|RelationsRenamings],Options,[(Relation,Renaming)|FilteredRelationsRenamings]) :-
  remove_tables_if_trusted(RelationsRenamings,Options,FilteredRelationsRenamings).

  
relevant_tuples_list([],_Query,_Tuple,[]).
relevant_tuples_list([(Relation,Renaming)|Relations],Query,Tuple,[ViewName|ViewNames]) :-
  new_view_name(Relation,ViewName),
  relevant_tuples((Relation,Renaming),ViewName,Query,Tuple),
  relevant_tuples_list(Relations,Query,Tuple,ViewNames).
  
new_view_name(Relation,ViewName) :-
  atom_concat(Relation,'_slice',RelU),
  current_db(Connection),
  my_odbc_identifier_name(Connection,RelU,ORelU),
  findall(N,(view_exists(Connection,V),atom_concat(ORelU,AN,V),my_atom_number(AN,N)),Ns),
  Ns=[_|_],
  !,
  find_max(Ns,Max),
  N1 is Max+1,
  my_atom_number(AN1,N1),
  atom_concat(ORelU,AN1,ViewName).
new_view_name(Relation,ViewName) :-
  atom_concat(Relation,'_slice1',ViewName).
  
find_max([Max],Max) :-
  !.
find_max([N|Ns],Max) :-
  find_max(Ns,N,Max).

find_max([],Max,Max).
find_max([N|Ns],CMax,Max) :-
  N>=CMax,
  !,
  find_max(Ns,N,Max).
find_max([_|Ns],CMax,Max) :-
  find_max(Ns,CMax,Max).
  
  
build_subset_answer_state_list([],[],[]).
build_subset_answer_state_list([ViewName|ViewNames],[Relation|Relations],[state(subset(ViewName,Relation),valid)|States]) :-
  build_subset_answer_state_list(ViewNames,Relations,States).
  
build_all_answer_state_list([],[]).
build_all_answer_state_list([Relation|Relations],[state(all(Relation),valid)|States]) :-
  build_all_answer_state_list(Relations,States).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%º
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code 7 relevantTuples
% Input: R relation, V view name, Q query, T tuple
% Output: Creates a new view in the database schema

% WARNING: Add SQL identifier delimiters for table and column names

relevant_tuples((Relation,Renaming),ViewName,Query,Tuple) :-
  atom_codes(Relation,StrRelation),
  atom_codes(ViewName,StrViewName),
  get_str_relation_colname_tuple(Relation,StrViewColnameTuple),
  get_str_select(Query,StrSelect),
  get_str_from(Query,StrFrom),
  get_str_where(Query,StrWhere),
  str_equal_rels(Relation,Renaming,'R',StrEqualRels),
  str_equal_tups_select(Query,Tuple,StrEqualTups),
  current_db(_,DBMS),
  my_sql_left_quotation_mark(LQstr,DBMS),
  my_sql_right_quotation_mark(RQstr,DBMS),
  concat_lists(
    ["CREATE VIEW ", LQstr, StrViewName, RQstr, "(", StrViewColnameTuple, ") AS ",
     "( SELECT * FROM ", StrRelation, " AS R WHERE EXISTS ",
       "(",
         "SELECT ",StrSelect," FROM ",StrFrom, " ",
         "WHERE",StrWhere,StrEqualRels, " AND ", StrEqualTups,
%         "WHERE (",StrWhere,") AND ",StrEqualRels, " AND ", StrEqualTups,
       ")",
     ")"],
    StrQuery),
  exec_if_development_on(
    write_info_log(['Processing:']),
    parse_sql_query(SQL,StrQuery,""),
    display_sql(SQL,2)),
  process_sql(StrQuery).

get_str_select(Query,StrSelect) :-
  get_projection_list(Query,PL),
  current_db(_,DBMS),
  display_to_string(write_proj_list(PL,0,DBMS),StrSelect).

get_str_from(Query,StrFrom) :-
  Query = select(_D,_T,_PL,from(Rs),_C,_GB,_H,_OB),
  current_db(_,DBMS),
  display_to_string(write_rel_list(Rs,0,DBMS),StrFrom).
  
get_str_where(Query,StrWhere) :-
  Query = select(_D,_T,_PL,_F,where(Cs),_GB,_H,_OB),
  current_db(_,DBMS),
  display_to_string(write_sql_cond(Cs,0,DBMS),Str),
  (Str=="true"
   ->
    StrWhere=" "
   ;
    concat_lists([" (",Str,") AND "],StrWhere)).


str_equal_rels(Relation1,Renaming1,Relation2,StrCondition) :-
  get_table_untyped_arguments(Relation1,Colnames),
  visible_relation_name(Relation1,Renaming1,Renaming1,VRelation1),
  relation_dot_column_list(VRelation1,Colnames,RelDotCol1),
  relation_dot_column_list(Relation2,Colnames,RelDotCol2),
  my_zipWith('=',RelDotCol1,RelDotCol2,Equalities),
  str_and_condition(Equalities,StrCondition).
  
relation_dot_column_list(_R,[],[]).
relation_dot_column_list(R,[ColName|ColNames],[attr(R,ColName,ColName)|RDotColNames]) :-
  relation_dot_column_list(R,ColNames,RDotColNames).

str_and_condition(Equalities,StrCondition) :-
  list_to_and_condition(Equalities,Condition),
  current_db(_,DBMS),
  display_to_string(write_sql_cond(Condition,0,DBMS),StrCondition).  
  
str_equal_tups_select(Query,Tuple,StrCondition) :-
  get_projection_list(Query,PL),
  Tuple =.. [_Relation|TArgs],
  filter_undefined(TArgs,PL,FTArgs,FPL),
  surround_with_quotes(FPL,FTArgs,QTArgs),
  my_zipWith('=',PL,QTArgs,Equalities),
  str_and_condition(Equalities,StrCondition).

surround_with_quotes([],[],[]).
surround_with_quotes([attr(Rel,Col,_AS)|Attrs],[Value|Values],[QValue|QValues]) :- 
  get_attr_type(Rel,Col,Type),
  is_string_type(Type),
  !,
  atomic_concat_list(['''',Value,''''],QValue),
  surround_with_quotes(Attrs,Values,QValues).
surround_with_quotes([attr(_Rel,_Col,_AS)|Attrs],[Value|Values],[QValue|QValues]) :- 
  atom(Value),
  !,
  atomic_concat_list(['''',Value,''''],QValue),
  surround_with_quotes(Attrs,Values,QValues).
surround_with_quotes([_|Attrs],[Value|Values],[Value|QValues]) :-
  surround_with_quotes(Attrs,Values,QValues).
  
list_to_and_condition([],true).  
list_to_and_condition([C1],C1).  
list_to_and_condition([true,C2|Cs],ACs) :-
  !,
  list_to_and_condition([C2|Cs],ACs).
list_to_and_condition([C1,C2|Cs],and(C1,ACs)) :-
  list_to_and_condition([C2|Cs],ACs).

filter_undefined([],[],[],[]).
filter_undefined([V|Vs],[_A|As],FVs,FAs) :-
  var(V),
  !,
  filter_undefined(Vs,As,FVs,FAs).
filter_undefined([V|Vs],[A|As],[V|FVs],[A|FAs]) :-
  filter_undefined(Vs,As,FVs,FAs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% add_to_program. Add logic clauses to a logic program (in a list)
% Return the new program, ordered and without duplicates
add_to_program([],Program,Program) :-
  !.
add_to_program(Clauses,Program,NewProgram) :-
  my_sort(Clauses,OClauses),
  ordered_insert_list(OClauses,Program,NewProgram),
  exec_if_development_on((
    (Program==NewProgram
     ->
      true
     ;
      write_info_log(['Current logic program:']),
      display_rules_list(NewProgram,2),
      display_nbr_rules(NewProgram)))).
  
ordered_insert_list(Xs,[],Xs) :-
  !.
ordered_insert_list([],Ys,Ys).
ordered_insert_list([X|Xs],[Y|Ys],[X|Zs]) :-
  X@<Y,
  !,
  ordered_insert_list(Xs,[Y|Ys],Zs).
ordered_insert_list([X|Xs],[X|Ys],[X|Zs]) :-
  !,
  ordered_insert_list(Xs,Ys,Zs).
ordered_insert_list([X|Xs],[Y|Ys],[Y|Zs]) :-
  ordered_insert_list([X|Xs],Ys,Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% More DES-dependent code

get_from_relations(from(RelationList),Relations) :- 
  findall(Relation,(my_member_term((Relation,_Renaming),RelationList),my_relation(Relation)),DupRelations),
%  my_remove_duplicates_sort(DupRelations,Relations).
  my_remove_duplicates(DupRelations,Relations).
  
get_all_from_relations(from(RelationList),RelationsRenamings) :- 
  findall((Relation,Renaming),(my_member_term((Relation,[Renaming|_Args]),RelationList),my_relation(Relation)),RelationsRenamings).

get_where_relations(where(Cond),Relations) :-
  findall(FromRelations,
          (my_member_term(from(RelationList),Cond),
           get_from_relations(from(RelationList),FromRelations)),
          DupRelationsList),
  concat_lists(DupRelationsList,DupRelations),
%  my_remove_duplicates_sort(DupRelations,Relations).
  my_remove_duplicates(DupRelations,Relations).
  
get_query_schema(Query,AuxViewName,Schema) :-  
  solve_des_sql_query_k(sql,(Query,_),[_|Args],ColTypes,TableRen,_Query,_DLsts,_RNVss,_Undefined,_OrderBy),
  get_answer_schema(AuxViewName,Args,ColTypes,TableRen,Schema).
  
get_str_relation_colname_tuple(Relation,StrColnameTuple) :-
  get_table_untyped_arguments(Relation,Colnames),
  my_list_to_tuple(Colnames,ColnameTuple),
  my_term_to_string_unquoted(ColnameTuple,StrColnameTuple).
  
my_relation(Relation) :-
  atom(Relation),
  current_db(Connection),
  my_table(Connection,Relation,_).

get_sql_view_definition(ViewName,SQLst) :-
  current_db(Connection),
  my_view(Connection,ViewName,_,SQLst,_,_,_,_,_).

get_attr_type(TableName,ColName,Type) :-
  current_db(des),
  !,
  my_attribute('$des',_Pos,TableName,ColName,Type).
get_attr_type(TableName,ColName,Type) :-
  my_odbc_get_type(TableName,ColName,Type).

  
/***************************************/

% sql_node_type(N/A,NodeType) :-
%   view_arity(N,A), 
%   NodeType=view,
%   !.
% sql_node_type(N/A,NodeType) :-
%   table_arity(N,A),
%   NodeType=table.

sql_node_type(N/A,NodeType) :-
  current_db(Connection),
  view_arity(Connection,N,A),
  !,
  NodeType=view.
sql_node_type(N/A,table) :-
  current_db(Connection),
  my_table(Connection,N,A).

  
/**************************************************/
/* Trusted and Oracle view handling               */  
% Translate each original, trusted view name View into View_trust
translate_trusted_oracle_views(SQLst,Schema,SQLst,Schema) :-
  trusting(off),
  !.
translate_trusted_oracle_views(SQLst,Schema,TSQLst,TSchema) :- %:: WARNING
  translate_trusted_schema(Schema,ViewName,TSchema),
  translate_trusted_sql_st(SQLst,ViewName,TSQLst).

translate_rdb_trusted_oracle_views(Action,_QueryStr,TQueryStr,TSchema) :-
  trusting(T),
  T\==off,
  (Action = create_view(_,(SQLst,_),Schema), 
   RepStr="" 
  ;
   Action = create_or_replace_view(_,(SQLst,_),Schema), 
   RepStr=" OR REPLACE"),
  !,
  translate_trusted_oracle_views(SQLst,Schema,TSQLst,TSchema),
  display_to_string(display_sql(TSQLst,0),TSQLStr),
  typed_schema_to_untyped_schema(TSchema,UTSchema),
  my_term_to_string_unquoted(UTSchema,UTSchemaStr),
  concat_lists(["CREATE",RepStr," VIEW ",UTSchemaStr," AS ",TSQLStr],TQueryStr).
translate_rdb_trusted_oracle_views(Action,QueryStr,QueryStr,Schema) :-
  (Action = create_view(_,(_,_),Schema)
  ;
   Action = create_or_replace_view(_,(_,_),Schema)),
  !.
translate_rdb_trusted_oracle_views(_Action,QueryStr,QueryStr,_TSchema).

% The view name is translated into a trusted version provided 
% there is an existing view with the same name
% The trusted 'ViewName' becomes 'ViewName_trust'
translate_trusted_schema(Schema,ViewName,TSchema) :-
  Schema=..[ViewName|Args],
%  length(Args,Arity),
  current_db(Connection),
  (view_arity(Connection,ViewName,_Arity)
   ->
    name_trusted(ViewName,TViewName)
   ;
    TViewName=ViewName),
  TSchema=..[TViewName|Args],
  add_to_trusted_oracle_list(TViewName).

add_to_trusted_oracle_list(TViewName) :-
  trusting('_trust'),
  !,
  add_to_trusted_list(TViewName).
add_to_trusted_oracle_list(TViewName) :-
  trusting('_oracle'),
  !,
  add_to_oracle_list(TViewName).
  
% Add the trusted view to the list of all the trusted views 
% (they will be eventually dropped after trusted debugging)
add_to_trusted_list(TViewName) :-
  (retract(trusted_views(TVNs))
   ->
    true
   ;
    TVNs=[]),
  assertz(trusted_views([TViewName|TVNs])).
  
add_to_oracle_list(TViewName) :-
  (retract(oracle_views(TVNs))
   ->
    true
   ;
    TVNs=[]),
  assertz(oracle_views([TViewName|TVNs])).
  
% Replaces each view name with its trusted version name 
% (currently, appending '_trust' to the original name)
translate_trusted_sql_st(T,_V,T) :- 
  var(T),
  !.
translate_trusted_sql_st(attr(Rel,Name,Ren),_V,attr(Rel,Name,Ren)) :- 
  var(Rel),
  !.
translate_trusted_sql_st(attr(Rel,Name,Ren),V,attr(TRel,Name,Ren)) :- 
  !,
  translate_trusted_relation(Rel,V,TRel).
translate_trusted_sql_st((Rel,Ren),V,(TRel,Ren)) :- 
  atom(Rel),
  !,
  translate_trusted_relation(Rel,V,TRel).
translate_trusted_sql_st((Rel,Ren),_V,(Rel,Ren)) :- 
  var(Rel),
  !.
translate_trusted_sql_st(T,_V,T) :- 
  (number(T) ; atom(T)),
  !.
translate_trusted_sql_st(C,V,RC) :- 
  C =.. [F|As],
  !, 
  translate_trusted_sql_st_list(As,V,RAs),
  RC =.. [F|RAs].

translate_trusted_sql_st_list([],_V,[]) :-
  !.
translate_trusted_sql_st_list([T|Ts],V,[RT|RTs]) :-
  translate_trusted_sql_st(T,V,RT), 
  translate_trusted_sql_st_list(Ts,V,RTs).
  
% The relation name is translated into a trusted version provided 
% there is an existing view with the same trusted name
translate_trusted_relation(Rel,V,TRel) :-
  Rel==V,
  !,
  name_trusted(Rel,TRel).
translate_trusted_relation(Rel,_V,TRel) :-
  name_trusted(Rel,TRel),
  current_db(Connection),
  view_exists(Connection,TRel),
  !.
translate_trusted_relation(Rel,_V,Rel).
  
name_trusted(ViewName,TViewName) :-
  trusting(Suffix),
  name_trusted(ViewName,Suffix,TViewName).
  
name_trusted(ViewName,Suffix,TViewName) :-
  atom_concat(ViewName,Suffix,TViewName).
  
object_to_trusted_object(O,_N,O) :-
  trusting(off),
  !.
object_to_trusted_object(view(_V),N,view(N)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Debug Statistics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-dynamic(debug_statistics/1).  % Flag indicating whether debug statistics display is enabled
:-dynamic(debug_random_seed/1). % Random seed at the beginning of the database generation
:-dynamic(debug_nodes/1).       % Number of nodes in the debugging tree
:-dynamic(debug_questions/1).   % Number of questions along debugging
:-dynamic(debug_max_questions/1). % Maximum number of questions along plain debugging with topdown
:-dynamic(debug_tuples/1).      % Number of tuples that the user must inspect along debugging
:-dynamic(debug_root_tuples/1). % Number of tuples in the root node
:-dynamic(debug_buggy/1).       % Name of the buggy node

debug_statistics(on).
debug_nodes(0).
debug_questions(0).
debug_tuples(0).
debug_root_tuples(0).

display_debug_statistics(Language) :-
  debug_statistics(on),
  !,
  (Language==sql
   ->
    debug_nodes(NbrDebugNodes),
    debug_max_questions(MaxNbrDebugQuestions)
   ;
    true),
  debug_questions(NbrDebugQuestions),
  debug_tuples(NbrDebugTuples),
  debug_root_tuples(NbrDebugRootTuples),
  NbrNonDebugRootTuples is NbrDebugTuples-NbrDebugRootTuples,
  write_info_log(['Debug Statistics:']),
  (Language==sql
   ->
  write_info_log(['Number of nodes           : ',NbrDebugNodes]),
  write_info_log(['Max. number of questions  : ',MaxNbrDebugQuestions]) ; true),
  write_info_log(['Number of questions       : ',NbrDebugQuestions]),
  write_info_log(['Number of inspected tuples: ',NbrDebugTuples]),
  write_info_log(['Number of root tuples     : ',NbrDebugRootTuples]),
  write_info_log(['Number of non-root tuples : ',NbrNonDebugRootTuples]).
display_debug_statistics(_).
