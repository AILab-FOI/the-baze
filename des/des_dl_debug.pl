/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.4.1                         */
/*                                                       */
/*    Datalog Debugger                                   */
/*                                                       */
/*                                                       */
/*                              Yolanda Garcia-Ruiz (*)  */
/*                          Rafael Caballero-Roldan (*)  */
/*                             Fernando Saenz-Perez (**) */
/*                                         (c) 2004-2012 */
/*                                   GPD DSIC, DISIA UCM */
/*             Please send comments, questions, etc. to: */
/*                                     fernan@sip.ucm.es */
/*                                Visit the Web site at: */
/*                           http://des.sourceforge.net/ */
/*                                                       */
/* Distributed under the GNU General Public License      */
/* http://www.gnu.org/copyleft/gpl.html                  */
/*********************************************************/


/*********************************************************************/
/* BEGIN Debug version                                               */
/*********************************************************************/

:-use_module(library(chr)).

debug_dl_full(RelationName,Arity,File) :-
  my_absolute_file_name(File, AbsFile),
  processC(consult,[AbsFile],[],yes),
  exec_if_development_on(
    write_info_log(['Original Datalog program:']),
    cat_file(AbsFile),
    nl_log),
  !,
  set_flag(debug_questions,0),
  set_flag(debug_tuples,0),
  write_info_log(['Loading and transforming file...']),
  debug_dl_program_transform(RelationName,Arity,Predicates,TransformedRules),
  debug_dl_chr_bridge(Predicates,TransformedRules,BridgeRules),
  atomic_concat_list([AbsFile,'.t.dl'],OutTransformFile), % Transformed Datalog program
  atomic_concat_list([AbsFile,'.b.pl'],OutBridgeFile),    % Bridge Prolog program
  start_path(StartPath),
  my_absolute_file_name('des_pchr.pl',StartPath,CHRFile),
  debug_dl_write_out_file(OutTransformFile,TransformedRules),
  debug_dl_append_out_file(CHRFile,OutBridgeFile,BridgeRules),
  exec_if_development_on((
    write_info_log(['Transformed Datalog program:']),
    cat_file(OutTransformFile),
    nl_log,
    nl_log,
    write_info_log(['Bridge Prolog program:']),
    cat_file(OutBridgeFile),
    nl_log)),
  !,
%  retract(check:checker(list_redefined,_)),
% No catch!
%  catch(my_ensure_loaded(OutBridgeFile), _M, true),
%  catch(my_ensure_loaded(OutBridgeFile), M, (nl,write(M),nl)), 
  my_load_module(OutBridgeFile),
  push_flag(output,off,CurrentOutput),
  processC(consult,[OutTransformFile],[],yes),
  pop_flag(output,CurrentOutput),
  debug_dl_start_debug_dl(RelationName,Arity),
  delete_file(OutTransformFile),
  delete_file(OutBridgeFile).
  
get_debug_dl_options(Options,TrustTables,TrustFile,OracleFile,DebugType,Order) :-
  get_debug_sql_options(Options,TrustTables,TrustFile,OracleFile,DebugType,Order).

debug_dl_start_debug_dl(R,N) :-
  Question = eq(c(R/N),i(R)),
  Options = [root(R/N)],
  debug_dl_ask_user(Question,Options,Constraint,Abort,_Error),
%   debug_dl_get_and_display_meaning(RelationName,Arity,Solutions),
%   debug_dl_input_ask_user(RelationName,Arity,Solutions,Answer,Abort,_Error), 
  (nonvar(Abort)
   ->
    write_info_log(['Debugging aborted by user.'])
   ;
    % Answer is a goal:
    append_goals(cts,Constraint,Goal),
    debug_dl_loop(Goal,BuggyNodes,Goal,Abort,Error),
    (nonvar(Abort)
     ->
      write_info_log(['Debugging aborted by user.'])
     ;
      (nonvar(Error)
       ->
        write_error_log(['Unable to locate a buggy node.'])
       ;
        display_dl_buggy_nodes(BuggyNodes),
        display_debug_statistics(datalog)
      )
    )
  ).
  
display_dl_buggy_nodes([buggy([BuggyViewName],buggy,_)]) :-
  write_info_log(['Buggy relation found: ',BuggyViewName]),
  !.
% display_dl_buggy_nodes([BuggyNode|_BuggyNodes]) :-
%   write_info_log(['Bug source: ',BuggyNode]).
display_dl_buggy_nodes(BuggyNodes) :-
  write_info_log(['Bug source: ',BuggyNodes]).

  
% debug_dl_relation_get_and_display_meaning(RelationName,Arity,Solutions) :-
%   length(Args,Arity),
%   Query=..[RelationName|Args],
%   debug_dl_query_get_meaning(Query,Solutions).

debug_dl_query_get_meaning(Query,N/_A,Solutions) :-
  compute_datalog(Query),
  get_ordered_solutions(Query,USolutions),
  replace_functor('$pi',N,USolutions,Solutions).
  
debug_dl_display_numbered_solutions(Solutions) :-
  number_solutions(Solutions,NumberedSolutions),
  display_bag(NumberedSolutions).

% number_solutions(Solutions,NumberedSolutions) :-
%   length(Solutions,L),
%   from(1,L,Ns),
%   my_zipWith('-',Ns,Solutions,NumberedSolutions).  
  

%debug_dl_loop(+Goal,+BuggyNodes,+AccGoal,?Abort,?Error) :-
debug_dl_loop(Goal,BuggyNodes,AccGoal,Abort,Error) :-
  write_info_dev_log(['Current goal: ', AccGoal]),
  call((Goal,getBuggy(CurrentBuggyNodes))),
  (CurrentBuggyNodes == []
   ->
%    call(getQuestions(Questions)),
    getQuestions(Questions),
    write_info_dev_log(['Possible questions: ', Questions]),
    debug_dl_choose_question_with_constraint(Questions,Constraint,Abort,Error),
    (var(Abort),
     var(Error)
     ->
      write_info_dev_log(['Answer constraint: ',Constraint]),
      append_goals(AccGoal,Constraint,NewAccGoal),
      NewGoal = Constraint, 
      debug_dl_loop(NewGoal,BuggyNodes,NewAccGoal,Abort,Error)
     ;
      true
    )
   ;
    BuggyNodes = CurrentBuggyNodes
  ).
  
% CHR Rules to be added as a result of a question
% Select first the one with an automated answer (lines 5 and 6 in table 4)

debug_dl_choose_question_with_constraint(Questions,Constraint,Abort,Error) :- 
  debug_dl_choose_system_question_with_constraint(Questions,_Question,Constraint,Abort,Error),
  !.
%   ,
%   write_info_dev_log(['Automatically answered question: ', Question]).
debug_dl_choose_question_with_constraint(Questions,Constraint,Abort,Error) :- 
  debug_dl_choose_user_question_with_constraint(Questions,_Question,Constraint,Abort,Error).
%   ,
%   write_info_dev_log(['Chosen question for user: ', Question]).

%
% AUTOMATIC. Oracle is the system
%
% CHR Rule: q_unmatched, q_missing1
% Line: 5
debug_dl_choose_system_question_with_constraint(Questions,Question,Constraint,_Abort,_Error) :- 
%  Question = eq(c((Q,V)),empty),
  Question = eq(c(Q,V),empty),
  member(Question,Questions),
  my_list_to_tuple(Vs,V),
  (build_DL_query(Q,Vs,DLQuery),
%  (DLQuery =.. [Q|Vs],
   is_empty_dl_answer(DLQuery) 
   ->
    Answer = yes
   ;
    Answer = no),
%  Constraint = eq(c((Q,V)),empty,Answer),
  Constraint = eq(c(Q,V),empty,Answer),
  write_info_dev_log(['Automatic answer (',Answer,') to question: ',Question]).
% CHR Rule: q_wrong1
% Line: 6
debug_dl_choose_system_question_with_constraint(Questions,Question,in(V,c(R),Answer),_Abort,_Error) :- 
  Question = in(V,c(R)),
  member(Question,Questions),
  my_list_to_tuple(Vs,V),
  (DLQuery =.. [R|Vs],
   is_empty_dl_answer(DLQuery) 
   ->
    Answer = no
   ;
    Answer = yes),
  write_info_dev_log(['Automatic answer (',Answer,') to question: ',Question]).
 
build_DL_query(Q,Vs,DLQuery) :-
  pred_arity(Q,A),
  length(Vs,L),
  R is A-L,
  length(RVs,R),
  append(Vs,RVs,CVs),
  DLQuery =.. [Q|CVs].
  
pred_arity(Q,A) :-
  pdg((Ns,_)),
  member(Q/A,Ns).
   
 
%
% MANUAL. Oracle is the user
%
% % CHR Rule: q_buggy
% % Line: 1-4
% % CHR Rule: q_wrong6
% % Line: 7-12
% % CHR Rule: q_missing2, q_missing3
% % Line: 13-14
% Select the question with minimum complexity, ask the user, and return corresponding constraint
debug_dl_choose_user_question_with_constraint(Questions,Question,Constraint,Abort,Error) :-
  write_info_dev_log(['Computing minimum complexity...']),
  findall((UserQuestion,Complexity),
          (member(UserQuestion,Questions),
           dl_debug_question_complexity(UserQuestion,Complexity)),
          UQsCs),
%  my_quicksort_pred(UQsCs,second_tuple_arg_desc_order,[(Question,_)|_]),
  my_mergesort(UQsCs,second_tuple_arg_desc_order,[(Question,_)|_]),
  write_info_dev_log(['Computing minimum complexity...DONE']),
  write_info_dev_log(['Question chosen for the user: ',Question]),
  debug_dl_ask_user(Question,[],Constraint,Abort,Error).
  
  
% Get the complexity for the given question
% dl_debug_question_complexity(+Question,-Complexity)
dl_debug_question_complexity(Question,Complexity) :-
  question_dlquery(Question,DLQuery),
  dl_number_of_solutions(DLQuery,Complexity).

  
dl_number_of_solutions(DLQuery,Number) :-
  (DLQuery = ':-'(_,_) -> write_info_dev_log(['Program augmented with autoview: ','$NVs'(DLQuery,[])]) ; true),
  compute_datalog(DLQuery),
  get_solutions(DLQuery,Solutions),
  length(Solutions,Number).

is_empty_dl_answer(DLQuery) :-
  dl_number_of_solutions(DLQuery,0).

% Ask oracle
debug_dl_ask_user(Question,Options,Constraint,Abort,Error) :-
  question_predicate(Question,R/N),
  question_dlquery(Question,DLQuery),
  question_userquestion(Question,UserQuestion),
  debug_dl_query_get_meaning(DLQuery,R/N,Solutions),
  (memberchk(root(R/N),Options)
   ->
    length(Solutions,NbrTuples),
    set_flag(debug_root_tuples,NbrTuples)
   ;
    true
  ),
  question_possible_useranswers(Question,PossibleAnswers,DefaultAnswer),
  debug_dl_input_ask_user(Question,UserQuestion,R,N,Solutions,PossibleAnswers,DefaultAnswer,Answer,Abort,Error),
  question_answer_constraint(Question,Solutions,Answer,Constraint).

% Predicate under inspection in a query
question_predicate(eq(c(R/N),i(R)),R/N).
%question_predicate(subset(_Sign,pi(_N,M,c((_WS,_X))),i(S)),S/M).
question_predicate(subset(_Sign,pi(_N,M,c(_WS,_X)),i(S)),S/M).
question_predicate(eq(i(S,pi(_N,M,(c(_LS,_X)))),_Empty),S/M).

% Given a question, the question asked to user in natural language
question_userquestion(eq(c(R/N),i(R)),['Is this the expected answer for ',R,'/',N,'?']).
% question_userquestion(subset(pos,pi(_N,M,c((_WS,_X))),i(S)),['Do you expect that ALL these tuples be in ',S,'/',M,'?']).
% question_userquestion(subset(neg,pi(_N,M,c((_WS,_X))),i(S)),['Do you expect that NONE of these tuples be in ',S,'/',M,'?']).
question_userquestion(subset(pos,pi(_N,M,c(_WS,_X)),i(S)),['Do you expect that ALL these tuples be in ',S,'/',M,'?']).
question_userquestion(subset(neg,pi(_N,M,c(_WS,_X)),i(S)),['Do you expect that NONE of these tuples be in ',S,'/',M,'?']).
question_userquestion(eq(i(S,pi(_N,M,(c(_LS,_X)))),empty),['Should this set be empty for ',S,'/',M,'?']).
question_userquestion(eq(i(S,pi(_N,M,(c(_LS,_X)))),nonempty),['Should this set be NON-empty for ',S,'/',M,'?']).

% Datalog query corresponding to a question
question_dlquery(eq(c(R/N),i(R)),DLQuery) :-
  length(Args,N),
  DLQuery =.. [R|Args].
%question_dlquery(subset(_Sign,pi(_N,M,c((WS,V))),i(_S)),DLQuery) :-
question_dlquery(subset(_Sign,pi(_N,M,c(WS,V)),i(_S)),DLQuery) :-
  length(SArgs,M),
  my_list_to_tuple(SiArgs,V),
  append(SiArgs,SArgs,WSArgs),
%  WSGoal =.. [WS|WSArgs],
  build_DL_query(WS,WSArgs,WSGoal),
  PiHead =.. ['$pi'|SArgs],
  DLQuery = ':-'(PiHead,WSGoal).
question_dlquery(eq(i(_S,pi(_N,M,c(LS,V))),_Empty),DLQuery) :-
  length(SArgs,M),
  my_list_to_tuple(LSArgs,V),
  append(_,SArgs,LSArgs),
%  LSGoal =.. [LS|LSArgs],
  build_DL_query(LS,LSArgs,LSGoal),
  LSGoal =.. [LS|LSArgs],
  PiHead =.. ['$pi'|SArgs],
  DLQuery = ':-'(PiHead,LSGoal).

question_answer_constraint(eq(c(R/_N),i(R)),_Solutions,Answer,u_valid(R,Answer)) :-
  (Answer = yes ; Answer = no),
  !.
question_answer_constraint(eq(c(R/_N),i(R)),_Solutions,Answer,(u_valid(R,no),Answer)) :-
  !. % Answer is either u_missing(t,R) or u_wrong(t,R), so that u_valid(R,no) is also added
% question_answer_constraint(eq(c((Q,X)),empty),_Solutions,Answer,eq(c((Q,X)),empty,Answer)) :-
question_answer_constraint(eq(c(Q,X),empty),_Solutions,Answer,eq(c(Q,X),empty,Answer)) :-
  (Answer = yes ; Answer = no),
  !.
question_answer_constraint(in(X,c(Ri)),_Solutions,Answer,in(X,c(Ri),Answer)) :-
  (Answer = yes ; Answer = no),
  !.
%question_answer_constraint(subset(Sign,pi(N,M,c((WS,X))),i(S)),Solutions,Answer,Constraint) :-
question_answer_constraint(subset(Sign,pi(N,M,c(WS,X)),i(S)),Solutions,Answer,Constraint) :-
  (Answer = yes ; Answer = no),
  !,
%  SCtr = subset(Sign,pi(N,M,c((WS,X))),i(S),Answer),
  SCtr = subset(Sign,pi(N,M,c(WS,X)),i(S),Answer),
  (Solutions = [_],
   Sign == pos,
   Answer == no
   ->
    nth_wrong_tuple_constraint(1,Solutions,S,WCtr),
    Constraint = (SCtr, WCtr)
   ;
    Constraint = SCtr
  ).
%question_answer_constraint(subset(Sign,pi(N,M,c((WS,X))),i(S)),Answer,(subset(Sign,pi(N,M,c((WS,X))),i(S),no),u_valid(S,no),Answer)) :-
question_answer_constraint(subset(Sign,pi(N,M,c(WS,X)),i(S)),Answer,(subset(Sign,pi(N,M,c(WS,X)),i(S),no),u_valid(S,no),Answer)) :-
  !. % Answer is either u_missing(t,R) or u_wrong(t,R), so that subset(Sign,pi(N,M,c((WS,X))),i(S),no) is also added
question_answer_constraint(eq(i(S,pi(N,M,(c(LS,X)))),Empty),Answer,eq(i(S,pi(N,M,(c(LS,X)))),Empty,Answer)) :-
  (Answer = yes ; Answer = no),
  !.
question_answer_constraint(Question,Answer,Answer) :-
  write_error_log(['Unsupported Question: ',Question]).
    
question_possible_useranswers(eq(c(R/_N),i(R)),
                              [yes,no,u_missing(_,R),u_wrong(_,R)],
                              no).
%question_possible_useranswers(eq(c((_Q,_X)),empty),
question_possible_useranswers(eq(c(_Q,_X),empty),
                              [yes,no],
                              no).
question_possible_useranswers(in(_X,c(_Ri)),
                              [yes,no],
                              no).
%question_possible_useranswers(subset(_Sign,pi(_N,_M,c((_WS,_X))),i(S)),
question_possible_useranswers(subset(_Sign,pi(_N,_M,c(_WS,_X)),i(S)),
                              [yes,no,u_missing(_,S),u_wrong(_,S)],
                              no).
question_possible_useranswers(eq(i(_S,pi(_N,_M,(c(_LS,_X)))),_Empty),
                              [yes,no],
                              no).
question_possible_useranswers(Question,_PossibleAnswers,_DefaultAnswer) :-
  write_error_log(['Unsupported Question: ',Question]).

debug_dl_build_userquestion(UserQuestion,_Solutions,PossibleAnswers,DefaultAnswer,CompleteUserQuestion) :-
%  (Solutions = [_] -> WOption = '/w' ; WOption = ''),
  debug_dl_build_text_user_answers(PossibleAnswers,TextUserAnswers),
  debug_dl_answer_text_user_answers(DefaultAnswer,StrTextDefaultAnswer,_),
  atom_codes(TextDefaultAnswer,StrTextDefaultAnswer),
  concat_lists([['Input: '],UserQuestion,[' (',TextUserAnswers,'/a/h) ['],[TextDefaultAnswer],[']: ']],CompleteUserQuestion).
    
debug_dl_build_text_user_answers(PossibleAnswers,TextUserAnswers) :-
  findall(TextPossibleAnswer,
          (member(PossibleAnswer,PossibleAnswers), 
           debug_dl_answer_text_user_answers(PossibleAnswer,TextPossibleAnswer,_)),
          PossibleTextAnswerList
         ),
  my_connect_lists(PossibleTextAnswerList,"/",ConnectedPossibleTextAnswerList),
  concat_lists(ConnectedPossibleTextAnswerList,StrTextUserAnswers),
  atom_codes(TextUserAnswers,StrTextUserAnswers).
  
my_connect_lists([],_Cs,[]).
my_connect_lists([Xs],_Cs,[Xs]).
my_connect_lists([Xs,Ys|Zs],Cs,[Xs,Cs|CZs]) :-
  my_connect_lists([Ys|Zs],Cs,CZs).

% yes/no/u_missing(_T,_R)/u_wrong(_T,_R)
% y/n/mT/wN
debug_dl_answer_text_user_answers(yes,"y","").
debug_dl_answer_text_user_answers(no,"n","\n n (no)").
debug_dl_answer_text_user_answers(u_missing(_,_),"mT","\n mT (missing tuple T as a comma-separted list of Datalog constants or placeholders '_')").
debug_dl_answer_text_user_answers(u_wrong(_,_),"wN","\n wN (wrong tuple at position N)").
  
%debug_dl_input_ask_user(subset(_Sign,pi(_N,_M,c((_WS,_X))),i(_S)),_UserQuestion,_RelationName,_Arity,[],_PossibleAnswers,_DefaultAnswer,yes,_Abort,_Error) :-
debug_dl_input_ask_user(subset(_Sign,pi(_N,_M,c(_WS,_X)),i(_S)),_UserQuestion,_RelationName,_Arity,[],_PossibleAnswers,_DefaultAnswer,yes,_Abort,_Error) :-
  !.
debug_dl_input_ask_user(Question,UserQuestion,RelationName,Arity,Solutions,PossibleAnswers,DefaultAnswer,Answer,Abort,Error) :-
%  exec_if_verbose_on(list_rules_wo_number(RelationName,Arity,2,_ODLs)),
  debug_dl_display_numbered_solutions(Solutions),  
  debug_dl_build_userquestion(UserQuestion,Solutions,PossibleAnswers,DefaultAnswer,CompleteUserQuestion),
  write_log_list(CompleteUserQuestion),
  length(Solutions,NbrTuples),
  add_to_flag(debug_tuples,NbrTuples),
  inc_flag(debug_questions),
  user_input_string(IStr),
  (IStr==[]
   ->
    debug_dl_answer_text_user_answers(DefaultAnswer,StrShortAnswer,_),
    to_uppercase_char_list(StrShortAnswer,Str)
   ;
    Str=[UC|T],
    IStr=[C|T],
    to_uppercase_char(C,UC)
  ),
  (member(yes,PossibleAnswers),
   (Str == "Y"
   ;
    Str == [],
    yes == DefaultAnswer
   )
   ->
    Answer = yes
%    Constraint = u_valid(RelationName,yes)
   ;
    (member(yes,PossibleAnswers),
     (Str == "N"
     ;
      Str == [],
      no == DefaultAnswer
     )
       ->
        Answer = no
%        Constraint = u_valid(RelationName,no)
       ;
        (Str == "A"
         ->
          Abort=abort
         ;
          (member(u_missing(_,_),PossibleAnswers),
           dl_missing_tuple_answer(RelationName,Arity,Str,MAnswer)
            ->
            (MAnswer==error
             ->
              debug_dl_input_ask_user(Question,UserQuestion,RelationName,Arity,Solutions,PossibleAnswers,DefaultAnswer,Answer,Abort,Error)
             ;
              Answer = MAnswer
              % Constraint = u_missing(Tuple,RelationName)
            )
           ;
            (member(u_wrong(_,_),PossibleAnswers),
             dl_wrong_tuple_answer(RelationName,Arity,Solutions,Str,WAnswer)
              ->
              (WAnswer==error
               ->
                debug_dl_input_ask_user(Question,UserQuestion,RelationName,Arity,Solutions,PossibleAnswers,DefaultAnswer,Answer,Abort,Error)
               ;
                Answer = WAnswer
                % Constraint = u_wrong(Tuple,RelationName)
              )
             ;
              (Str == "H"
               ->
                help_debug_dl_input_ask_user(Solutions,PossibleAnswers),
                debug_dl_input_ask_user(Question,UserQuestion,RelationName,Arity,Solutions,PossibleAnswers,DefaultAnswer,Answer,Abort,Error)
               ;
                write_error_log(['Invalid input']),
                help_debug_dl_input_ask_user(Solutions,PossibleAnswers),
                debug_dl_input_ask_user(Question,UserQuestion,RelationName,Arity,Solutions,PossibleAnswers,DefaultAnswer,Answer,Abort,Error)
              )
            )
          )
      )
    )
  ).
  
help_debug_dl_input_ask_user(_Solutions,PossibleAnswers) :-
  %(Solutions = [_] -> WOption = '\n w (wrong single-tuple answer)' ; WOption = ''),
  findall(HelpTextPossibleAnswer,
          (member(PossibleAnswer,PossibleAnswers), 
           debug_dl_answer_text_user_answers(PossibleAnswer,_,HelpTextPossibleAnswer)),
          PossibleTextAnswerList
         ),
  concat_lists(PossibleTextAnswerList,StrPossibleTextAnswers),
  atom_codes(PossibleTextAnswers,StrPossibleTextAnswers),
  write_info_log(['Possible answers are:',PossibleTextAnswers,'\n a (abort)\n h (this help) ']).

dl_missing_tuple_answer(RelationName,Arity,Str,Answer) :-
  dl_missing_tuple_answer(RelationName,Arity,Answer,Str,[]).
  
dl_missing_tuple_answer(RelationName,Arity,Answer) -->
  "M",
  my_blanks_star,
  my_noncompound_terms(Cs,[],_Vo),
  {length(Cs,Arity)
   ->
    my_list_to_tuple(Cs,Tuple),
    Answer=u_missing(Tuple,RelationName)
   ;
    write_error_log(['Incorrect number of arguments. It must be ',Arity]),
    Answer=error}.

dl_wrong_tuple_answer([],_Str,error) :-
  !, % No solution tuple. So, no way to be incorrect.
  write_error_log(['Empty relation. Cannot be incorrect. Maybe missing?']).
dl_wrong_tuple_answer(RelationName,Arity,Solutions,Str,Answer) :-
  dl_wrong_tuple_answer(RelationName,Arity,Solutions,Answer,Str,[]).  
  
dl_wrong_tuple_answer(RelationName,_Arity,[Solution],u_wrong(Solution,RelationName)) -->
  "W".
% dl_wrong_tuple_answer([Solution|_Solutions],wrong(Tuple)) -->
%   "W",
%   {functor(Solution,RelationName,Arity),
%    functor(Tuple,RelationName,Arity)}.
dl_wrong_tuple_answer(RelationName,_Arity,Solutions,Answer) -->
  "W",
  my_number(N),
  {length(Solutions,L),
   (N>0,
    N=<L
    ->
     nth_wrong_tuple_constraint(N,Solutions,RelationName,Answer)
    ;
     write_error_log(['Invalid tuple number. It must be between 1 and ',L]),
     Answer=error
    )
   }.

nth_wrong_tuple_constraint(N,Solutions,RelationName,Answer) :-
  nth1(N,Solutions,Solution),
  Solution =.. [_|Cs],
  my_list_to_tuple(Cs,Tuple),
  Answer=u_wrong(Tuple,RelationName).
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHR Bridge
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% debug_dl_chr_bridge(+Predicates,+Rules,-BridgeRules)
debug_dl_chr_bridge(Predicates,Rules,BridgeRules) :-
  debug_dl_chr_bridge(Predicates,Rules,BridgeRules,[]).

debug_dl_chr_bridge(Predicates,Rules) -->
  debug_dl_build_rel_cts(Predicates),
  debug_dl_build_subrel_cts(Predicates,Rules),
  debug_dl_build_incomp_rel_cts(Predicates,Rules),
  debug_dl_build_body_cts(Predicates,Rules),
  debug_dl_build_connected_subgraph_cts,
%  debug_dl_build_rel_cts(Predicates),
  debug_dl_build_tail_clauses.
  
debug_dl_build_tail_clauses -->
  [(':-'(cts,(relCts, subRelCts, incompRelCts, bodyCts, connectedSubgraphCts)),[]),
   (':-'(getQuestions(L),(collectQ([],N),questions(N,L))),['L'=L,'N'=N]),
   (':-'(getBuggy(L),(collectBuggy(L))),['L'=L])].
  
debug_dl_build_rel_cts(Predicates) -->
  {findall(relation(Name,Arity),member(Name/Arity,Predicates),BodyList),
   my_list_to_tuple(BodyList,Body)},
  [(':-'(relCts,Body),[])].
  
debug_dl_build_subrel_cts(Predicates,Rules) -->
  {findall(subRel(BName,Name),
           (member(Name/Arity,Predicates),
            functor(H,Name,Arity),
            member((':-'(H,B),_NVs),Rules),
            functor(B,BName,Arity),
            atom_concat(Name,NumberAtom,BName),
            atom_codes(NumberAtom,Cs),
            my_positive_integer(_,Cs,[])
           ),
           BodyList
          ),
   body_list_to_tuple(BodyList,Body)},
  [(':-'(subRelCts,Body),[])].
  
debug_dl_build_incomp_rel_cts(Predicates,Rules) -->
  {findall(incompRel(Name,NHName),
           (member(Name/Arity,Predicates),
            atom_concat(nh,Name,NHName),
            functor(NH,NHName,Arity),
            member((':-'(NH,_B),_NVs),Rules)
           ),
           BodyList
          ),
   body_list_to_tuple(BodyList,Body)},
  [(':-'(incompRelCts,Body),[])].
  
debug_dl_build_body_cts(Predicates,Rules) -->
  {findall(bodyRel(BName,Tuples),
           (member(Name/Arity,Predicates),
            functor(H,Name,Arity),
            member((':-'(H,B),_),Rules),         % p(A):-p1(A)
            functor(B,BName,Arity),
            atom_concat(Name,NumberAtom,BName),
            is_number_atom(NumberAtom),
            (R = ':-'(B,Bi) ; R = B, Bi = true),
            member((R,_),Rules),        % p1(X):-s(X)
            body_list_to_tuple(Bis,Bi),
%            findall((LName,Sign,WrongName,MissingName,LitName),
            findall((LName,Sign,WrongName,MissingName),
                    (my_nth1_member(Lj,J,Bis),             % s(X)
                     (Lj = not(NL) -> Sign = neg, functor(NL,LName,LArity) ; Sign = pos, functor(Lj,LName,LArity)),
                     not_builtin(LName/LArity),
                     atomic_concat_list([w,BName,J],WrongName),
                     atomic_concat_list([m,BName,J],MissingName)
%                     atomic_concat_list([lit,BName,J],LitName)
%                      (
%                       member((':-'(Wrong,Bi),_),Rules),
%                       functor(Wrong,WrongName,_),
%                       atomic_concat_list([w,BName],WrongPredName),
%                       atomic_concat_list([WrongPredName,J],WrongName)
%                      ; % This disjunction should not be needed
%                       member((':-'(Missing,Bi),_),Rules),
%                       functor(Missing,MissingName,_),
%                       atomic_concat_list([m,BName],MissingPredName),
%                       atomic_concat_list([MissingPredName,J],MissingName)
%                      )
                     ),
                    Tuples
                   )
           ),
           BodyList
          ),
   body_list_to_tuple(BodyList,Body)},
  [(':-'(bodyCts,Body),[])].
  
% debug_dl_build_connected_subgraph_cts -->
%   {Body=true},
%   !,
%   [(':-'(connectedSubgraphCts,Body),[])].
debug_dl_build_connected_subgraph_cts -->
  {pdg(PDG),
   get_subgraphs(PDG,ASubGraphs),
   get_adjacents_to_subgraphs(ASubGraphs,PDG,AAdjacents),
   my_unzip_list(ASubGraphs,SubGraphs,_),  % Name/Arity -> Name
   my_unzip_list(AAdjacents,Adjacents,_),  % Name/Arity -> Name
   my_zipWith(connectedSubgraph,SubGraphs,Adjacents,BodyList),
   my_list_to_tuple(BodyList,Body)},
  [(':-'(connectedSubgraphCts,Body),[])].
  
  
get_subgraphs(PDG,SubGraphs) :-
  PDG = (Nodes,_Arcs),
  findall(SubGraph,
          (member(Node,Nodes),
           get_subgraph(Node,PDG,SubGraph)),
          DupSubGraphsList),
  concat_lists(DupSubGraphsList,DupSubGraphs),
  my_remove_duplicates_sort(DupSubGraphs,SubGraphs).

get_subgraph(Node,PDG,SubGraph) :-
  PDG = (_,Arcs),
  findall(Nodes,(path(Node,Node,Arcs,UNodes),my_remove_duplicates_sort(UNodes,Nodes)),SubGraph).
  
path(Node,Node,_Arcs,[Node]).
path(FromNode,ToNode,Arcs,[FromNode|Nodes]) :-
  remove_edge(FromNode,InNode,Arcs,OArcs),
  path(InNode,ToNode,OArcs,Nodes).

remove_edge(From,To,InArcs,OutArcs) :-
  remove_one_element_from_list(To+From,InArcs,OutArcs).
remove_edge(From,To,InArcs,OutArcs) :-
  remove_one_element_from_list(To-From,InArcs,OutArcs).

% Get adjacent nodes to subgraphs
% Find all predicates used for each predicate in a subgraph
get_adjacents_to_subgraphs(SubGraphs,PDG,AdjacentsList) :-
  findall(Adjacents,
          (member(SubGraph,SubGraphs),
           get_adjacents_to_subgraph(SubGraph,PDG,Adjacents)),
          AdjacentsList).
          
get_adjacents_to_subgraph(SubGraph,(_Nodes,Arcs),Adjacents) :-
  findall(FromNodes,
        (member(Node,SubGraph),
         findall(FromNode,
                 (member(Arc,Arcs),
                  (Arc = Node+FromNode ; Arc = Node-FromNode)
                 ),
                 FromNodes)
        ),
        FromNodesList),
  concat_lists(FromNodesList,DupAdjacents),
  my_remove_duplicates_sort(DupAdjacents,Adjacents).
  

  
  
is_number_atom(NumberAtom) :-
  atom_codes(NumberAtom,Cs),
  my_positive_integer(_,Cs,[]).
  
body_list_to_tuple([],true) :-
  !.
body_list_to_tuple(BodyList,Body) :-
  my_list_to_tuple(BodyList,Body).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Transform Datalog program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%debug_dl_program_transform(+RelationName,+Arity,+Predicates,-Rules)
debug_dl_program_transform(RelationName,Arity,Predicates,Rules) :-
  pdg(PDG),
  sub_pdg(RelationName/Arity,PDG,(Predicates,_Arcs)),
  exec_if_development_on(
    write_info_log(['Predicate dependency graph restricted to ',RelationName,'/',Arity,':']),
    processC(pdg,[RelationName/Arity],_NVs,yes)),
  debug_dl_predicate_transform_list(Predicates,Rules,[]).

debug_dl_predicate_transform_list([]) -->
  [].
debug_dl_predicate_transform_list([Predicate|Predicates]) -->
  debug_dl_predicate_transform(Predicate),
  debug_dl_predicate_transform_list(Predicates).
  
% Extensional predicates (Only facts)
debug_dl_predicate_transform(Predicate) -->
  {get_source_dlrules(namearity,Predicate,DLs),
   filter_dl_facts_rules(DLs,DLFacts,DLRules),
   DLRules==[],
   !,
%   dlrule_to_ruleNV_list(DLFacts,Facts)
   dlrule_to_ruleNVs_list(DLFacts,Facts)
  },
  Facts.
% Intensional predicates (Facts and rules)
debug_dl_predicate_transform(Predicate) -->
  {get_source_dlrules(namearity,Predicate,DLs),
%   dlrule_to_ruleNV_list(DLs,Rules)
   dlrule_to_ruleNVs_list(DLs,Rules)
  },
  debug_dl_rules_transform(Predicate,Rules).

debug_dl_rules_transform(Predicate,Rules) -->
  debug_dl_rules_transform_1(Predicate,Rules),
  debug_dl_rules_transform_2(Predicate,Rules),
  debug_dl_rules_transform_3(Predicate,Rules),
  debug_dl_rules_transform_4(Predicate,Rules),
  debug_dl_rules_transform_5a(Predicate,Rules),
  debug_dl_rules_transform_5b(Predicate,Rules).
%  debug_dl_rules_transform_5c(Predicate,Rules).
  
debug_dl_rules_transform_1(Name/Arity,Rules) -->
  {findall((':-'(R,Ri),NVs),
    (length(Xs,Arity),
     R =.. [Name|Xs],
     my_nth1_member(_,N,Rules),
     atomic_concat_list([Name,N],NB),
     Ri =.. [NB|Xs],
     assign_variable_names((R,Ri),[],NVs)
    ),
    TRules
   )
  },
  TRules.
     
debug_dl_rules_transform_2(Name/_Arity,Rules) -->
  {findall((TRule,NVs),
    (my_nth1_member((R,NVs),N,Rules),
     (R = ':-'(H,Bi), 
      TRule = ':-'(Ri,Bi) 
     ;
      R \= ':-'(H,Bi),
      R = H,
      TRule = Ri
     ),
     atomic_concat_list([Name,N],NB),
     H =.. [Name|Tis],
     Ri =.. [NB|Tis]
    ),
    TRules
   )
  },
  TRules.
     
debug_dl_rules_transform_3(Name/_Arity,Rules) -->
  {findall((HRi,NVs),
    (my_nth1_member((R,NVs),N,Rules),
     (R = ':-'(H,Bi)
     ;
      R \= ':-'(H,Bi),
      R = H
     ),
     atomic_concat_list([h,Name,N],NB),
     H =.. [Name|Tis],
     HRi =.. [NB|Tis]
    ),
    TRules
   )
  },
  TRules.
     
debug_dl_rules_transform_4(_Name/_Arity,[]) -->
  [].
debug_dl_rules_transform_4(Name/Arity,Rules) -->
  {length(Xs,Arity),
   atomic_concat_list([nh,Name],NH),
   NHR =.. [NH|Xs],
   bagof(not(HRi),
    Rule^N^NB^
    (my_nth1_member(Rule,N,Rules),
     atomic_concat_list([h,Name,N],NB),
     HRi =.. [NB|Xs]
    ),
    NHRis
   ),
   my_list_to_tuple(NHRis,NHR1m),
   assign_variable_names((NHR,NHR1m),[],NVs)
  },
  [(':-'(NHR,NHR1m),NVs)].

debug_dl_rules_transform_5a(_Name/_Arity,[]) -->
  [].
debug_dl_rules_transform_5a(Name/Arity,Rules) -->
  {debug_dl_rules_transform_2(Name/Arity,Rules,S2Rules,[]),
   findall((':-'(WRIJ,Bi),NVs),
     (my_nth1_member((':-'(Ri,Bi),NVs),I,S2Rules),
      Ri =.. [_|Tis],
      my_list_to_tuple(Bis,Bi),
      my_nth1_member(Lj,J,Bis),
      (Lj = not(S), ! ; Lj = S),
      functor(S,FLj,ALj),
      not_builtin(FLj/ALj),
      atomic_concat_list([w,Name,I,J],FWRIJ),
      S =.. [_FS|Sks],
      concat_lists([Tis,Sks],TiSks),
      WRIJ =.. [FWRIJ|TiSks]
     ),
     TRules
    )
  },
  TRules.

debug_dl_rules_transform_5b(_Name/_Arity,[]) -->
  [].
debug_dl_rules_transform_5b(Name/Arity,Rules) -->
  {debug_dl_rules_transform_2(Name/Arity,Rules,S2Rules,[]),
   findall((':-'(MRIJ,B),NVs),
     (my_nth1_member((':-'(Ri,Bi),NVs),I,S2Rules),
      Ri =.. [_|Tis],
      my_list_to_tuple(Bis,Bi),
      my_nth1_member(Lj,J,Bis),
      (Lj = not(S)
      ;
       Lj \= not(_),
       Lj = S
      ),
      S =.. [_FS|Sks],
%       my_term_variables(Sks,SksVs),
%       my_term_variables(Tis,TisVs),
      term_variables(Sks,SksVs),
      term_variables(Tis,TisVs),
      my_check_subset_var(SksVs,TisVs),
      functor(S,FLj,ALj),
      not_builtin(FLj/ALj),
      atomic_concat_list([m,Name,I,J],FMRIJ),
      concat_lists([Tis,Sks],TiSks),
      MRIJ =.. [FMRIJ|TiSks],
      build_not_literals_5b(S2Rules,Ri,1,I,NLs),
      append(NLs,[not(Lj)],Bs),
      my_list_to_tuple(Bs,B)
     ),
     TRules
    )
  },
  TRules.
  
% debug_dl_rules_transform_5c(_Name/_Arity,[]) -->
%   [].
% debug_dl_rules_transform_5c(Name/Arity,Rules) -->
%   {debug_dl_rules_transform_2(Name/Arity,Rules,S2Rules,[]),
%    findall((WRIJ,NVs),
%      (my_nth1_member((':-'(Ri,Bi),NVs),I,S2Rules),
%       Ri =.. [_|Tis],
%       my_list_to_tuple(Bis,Bi),
%       my_nth1_member(Lj,J,Bis),
%       (Lj = not(S), ! ; Lj = S),
%       functor(S,FLj,ALj),
%       not_builtin(FLj/ALj),
%       atomic_concat_list([lit,Name,I,J],FWRIJ),
%       S =.. [_FS|Sks],
%       concat_lists([Tis,Sks],TiSks),
%       WRIJ =.. [FWRIJ|TiSks]
%      ),
%      TRules
%     )
%   },
%   TRules.

build_not_literals_5b([],_Ri,_C,_I,[]).
build_not_literals_5b([_R|Rs],Ri,I,I,NLs) :-
  !,
  C1 is I+1,
  build_not_literals_5b(Rs,Ri,C1,I,NLs).
build_not_literals_5b([(R,_NVs)|Rs],Ri,C,I,[not(L)|NLs]) :-
  (R = ':-'(H,_) ; R \= ':-'(H,_), R = H),
  functor(H,Name,_Arity),
  atomic_concat_list([h,Name],FHRC),
  Ri =.. [_|Tis], 
  L =.. [FHRC|Tis],
  C1 is C+1,
  build_not_literals_5b(Rs,Ri,C1,I,NLs).
  
       
debug_dl_write_out_file(File,Rules) :-
  write_rules_file_mode(File,write,Rules).
  
debug_dl_append_out_file(InFile,OutFile,Rules) :-
%  my_copy_file(InFile,OutFile),
  copy_file(InFile,OutFile),
  write_rules_file_mode(OutFile,append,Rules).
  
write_rules_file_mode(File,Mode,Rules) :-
  disable_log(Logs),
  push_flag(output,on,O),
  current_output(CurrentStream),
  open(File,Mode,OutStream,[]),
  set_output(OutStream),
  (
   member(Rule,Rules),
   (write_datalog_rule(Rule,0)
    ->
     true
    ;
     write_with_NVs(Rule,[]),
     write('.')
   ),
   nl,
   fail
  ;
   set_output(CurrentStream),
   close(OutStream)
  ),
  pop_flag(output,O),
  resume_log(Logs).

/*********************************************************************/
/* END Full Debug version */
/*********************************************************************/


/*********************************************************************/
/* BEGIN Plain Debug version */
/*********************************************************************/

/* Dynamic Predicates                                                */

:- dynamic(gt/1).     % goal table (gt/1): dynamic predicate
% :- dynamic(rt/4).     % rule table (rt/4): dynamic predicate
% :- dynamic(ft/3).     % fact table (ft/3): dynamic predicate
:- dynamic(latestNameVar/1).


/*********************************************************************/
/* Processing the command /debug_datalog                             */
/*********************************************************************/

% Processing datalog debugging
debug_dl_plain(Goal,Level,NVs) :-
%  functor(Goal,F,A),
%  exist_user_predicate(F/A),
  (file_table(_F,_Fid)
   ->
    true
   ;
    set_p_flag(file_table(top_level),0)
  ),
  functor(Goal,Pred,Arity),
  pdg((Nodes,_Arcs)),
  (member(Pred/Arity,Nodes)
   ->
    ((Level == c ; Level == p)
     ->
      solve_datalog_query_complete_fill(Goal,NVs),
      !, 
      write_info_verb_log(['Starting declarative debugger...']), 
      initDebug(Goal, Level, NVs)
     ;
      write_error_log(['Unsupported option ''',Level,'''.'])
    )
   ;
    write_error_log(['Predicate ',Pred/Arity,' does not exist.'])
  ).



/******************************************************/
/*    CODE FOR BUILDING A COMPUTATIONAL GRAPH         */
/******************************************************/

initDebug(Goal, Level,  Vars) :-
%             my_abolish(rt/4),                % Remove all clauses of the predicate "rt"
%             my_abolish(ft/3),                % Remove all clauses of the predicate "ft"
  my_abolish(gt/1),                % Remove all clauses of the predicate "gt"
  retractalllatestNameVar,
  set_flag(debug_questions,0),
  set_flag(debug_tuples,0),
  build_debug_tree(Goal, Level, Vars),  % Building a debugging tree 
  !.
initDebug(_,_,_) :-
  write_log('Error: Unexpected error when debugging.'), 
  nl_log.

% build_debug_tree(_Goal,_Level,_Vars) :- 
%                  datalog(R,Vs,_Rid,_CId,L,Fid,_Rs),
%                  (
%                   R=':-'(_H,_B) -> assertz(rt(R, Vs, L, Fid))  % create rule table
%                  ;
%                   assertz(ft(R,L, Fid))  % create fact table
%                  ),
%                  fail.
build_debug_tree(Goal,Level, Vars) :-
  %doListFacts(LstFacts, Goal, LinesF),    %LstFacts is the list of facts which defined the predicate represented by Goal
  %doListRules(LstRules, Goal, LinesR),    %LstRules is the list of rules which defined the predicate represented by Goal
  doListFacts(LstFacts, Goal),             %LstFacts is the list of facts which defined the predicate represented by Goal
  doListRules(LstRules, Goal),
  build(Goal, Tuple_Input),
  %(LstFacts \== []; LstRules \==[]),
  %append(LinesF, LinesR, Tot_Lines),   %List of lines which define query Goal
  write_info_verb_log(['Building and writing XML tree file...']),

  cons_pr(Tuple_Input, Goal, LstFacts, LstRules, Tree),   %build the debug tree "Tree" for the goal Goal and the tuple input Goal

  all_variables([Tree], Lvariables),
  my_remove_duplicates(Lvariables, Lvar),
  generate_names(Lvar, LNames),
  xml_tree(Tree, Goal, Vars),           %write the XML representation of debug tree "Tree" in an xml file.
  level_of_debugging(Tree, Goal, LNames, Level).

/*********************************************************************/
doListFacts(LstFacts, G) :-
%    findall((H,L, Fid),ft(H, L, Fid), LstF),
    findall((H,L,Fid),
             (datalog(H,_Vs,_Rid,_CId,L,Fid,_Rs),
              H\=':-'(_,_)), 
             LstF),
    filterF(LstF,G,LstFacts).

doListRules(LstRules, G) :-
%    findall((R,Vs,L, Fid),rt(R, Vs, L, Fid), LstR),
    findall((':-'(H,B),Vs,L,Fid),
             datalog(':-'(H,B),Vs,_Rid,_CId,L,Fid,_Rs), 
             LstR),
    filterRules(LstR,G,LstRules).


/*********************************************************************/
/* filter rules and facts                                            */
/*********************************************************************/
filterF([],_G, []).
%filterF([(F,BL, EL)|Rs],not(G), [F|Resto]) :-
filterF([(F,Ls, Fid)|Rs],not(G), [(F,Ls, Fid)|Resto]) :-
  filterF([(F,Ls, Fid)|Rs],G, [(F,Ls, Fid)|Resto]).

                                  %G=..[Pred|Args],
                                  %   F=..[Pred|Args2], length(Args,L), length(Args2,L),
                                  %     !,
                                  %   filterF(Rs,G, Resto, Resto_lines), !.

filterF([(F,Ls, Fid)|Rs],G, [(F,Ls, Fid)|Resto]) :-
  G=..[Pred|Args],
  F=..[Pred|Args2], length(Args,L), length(Args2,L),
  !,
  filterF(Rs,G, Resto).



filterF([_R|Rs],G, Resto) :- filterF(Rs,G, Resto).
/********************************************************************/
filterRules([],_G, []).
filterRules([((H:-B),V, Ls, Fid)|Rs],not(G), [((H:-B),V, Ls, Fid)|Resto]) :-
  filterRules([((H:-B),V, Ls, Fid)|Rs],G, [((H:-B),V, Ls, Fid)|Resto] ).

filterRules([((H:-B),V, Ls, Fid)|Rs],G, [((H:-B),V, Ls, Fid)|Resto] ) :-
  G=..[Pred|Args], length(Args,Long), length(Args2,Long),
  H=..[Pred|Args2],!,
  filterRules(Rs,G, Resto).

%% en el caso de que sea una regla asertada, la lista con el número de líneas es []
% filterRules([((H:-B),V, [])|Rs],not(G), [((H:-B),V, BL, EL)|Resto], [BL, EL|Resto_lines]) :-
%             filterRules([((H:-B),V, [])|Rs],G, [((H:-B),V, BL, EL)|Resto],[BL, EL|Resto_lines] ).
%                                              %G=..[Pred|Args], length(Args,Long), length(Args2,Long),
%%                                              H=..[Pred|Args2],!,
%%                                              filterRules(Rs,G, Resto, Resto_lines), !.
% filterRules([((H:-B),V, [])|Rs],G, [((H:-B),V, BL, EL)|Resto],[BL, EL|Resto_lines] ) :-
%                                              G=..[Pred|Args], length(Args,Long), length(Args2,Long),
%                                              H=..[Pred|Args2],!,
%                                              filterRules(Rs,G, Resto, Resto_lines).

filterRules([_R|Rs],G, Resto) :- filterRules(Rs,G, Resto).


/*********************************************************************/
/* Case 10: The input parameter is an empty list                     */
/*********************************************************************/
cons_pr([], Goal, _LstFacts, _LstRules, pr([],Goal, [], [],_)).
%write_log('caso 10').

/*********************************************************************/
/* Case 15: The input parameter is a negative atom                   */
/*********************************************************************/
cons_pr(not(Input), not(Goal), LstFacts, LstRules, Tree) :-
  !,
%  write_log('Caso 15 '),
  cons_pr(Input, Goal, LstFacts, LstRules, Tree).
  
/**********************************************************************/
/* Case 20: The input parameter is a list                             */
/**********************************************************************/
cons_pr([E|Re], Goal, LstFacts, LstRules, pr_list([E|Re],Goal, LstOutput, LstTrees, _)) :-
  !,
%   write_log('Caso 20 '),
  et_entries_l([E|Re], LstOutput),
  cons_list_trees([E|Re], Goal, LstFacts, LstRules, LstTrees).

/**********************************************************************/
/* Case 30: The predicate represented by Goal is not defined in the   */
/*          program                                                    */
/**********************************************************************/
cons_pr(Tuple_Input, Goal, [], [], empty([Tuple_Input], Goal, LstOutput, [], _) ) :-
% write_log('Caso 30 '),
   ordered_et_entries_by_goal(Tuple_Input, LstOutput), !.

/**********************************************************************/
/* Case 4: Is a duplicated node, defined only by facts                */
/**********************************************************************/
cons_pr(Tuple_Input, Goal, _LstFacts, [], dup([Tuple_Input], Goal, LstOutput, [], _, _) ) :-
%write_log('Caso 4 '),
  my_subsumes(Goal, Tuple_Input),  %Goal is more general than  Tuple_Input
  is_duplicate(Tuple_Input),
  ! ,
  ft_entries(Tuple_Input,LstOutput).  %LstOutput unifies with the list of facts which define the Tuple_Input

/*********************************************************************/
/* Case 5: Is not a duplicated node and is defined only by facts     */
/*********************************************************************/
cons_pr(Tuple_Input, Goal, _LstFacts, [], pr([Tuple_Input], Goal, LstOutput, [Ch],  _) ) :-
%write_log('Caso 5 '),
  my_subsumes(Goal, Tuple_Input),  %Goal is more general than  Tuple_Input
  assertz(gt(Tuple_Input)),        %assertzs the new goal "Tuple_input"
  ft_entries(Tuple_Input,LstOutput), %LstOutput unifies with the list of facts which define the Tuple_Input
  ft_entries_lines(Tuple_Input,LstOutput_lines),
  Ch = fact([Tuple_Input], Goal, LstOutput_lines, [], _),
  !.

/*********************************************************************/
/* Case 6: It is a duplicated node and is defined by facts and rules */
/*********************************************************************/
cons_pr(Tuple_Input, Goal, _LstFacts,  _LstRules, dup([Tuple_Input], Goal, LstOutput, [],_,  _) ) :-
%write_log('Caso 6 '),
  my_subsumes(Goal, Tuple_Input),  %Goal is more general than  Tuple_Input
  is_duplicate(Tuple_Input),
  ! ,
  ordered_et_entries_by_goal(Tuple_Input, LstOutput). %LstOutput unifies with the list of elements form the extension
                                      % table which unify with tuple_input.

/*********************************************************************/
/* Case 7: It is not a duplicated node and is defined only by rules  */
/*********************************************************************/
cons_pr(Tuple_Input, Goal, [], LstRules, pr([Tuple_Input], Goal, LstOutput, ListChildren, _ )) :-
%write_log('Caso 7 '),
  my_subsumes(Goal, Tuple_Input),  %Goal is more general than  Tuple_Input
  build(Tuple_Input, H),           %Builds in H the same call with fresh variables
  assertz(gt(H)),        %assertzs the new goal "H"
  ordered_et_entries_by_goal(Tuple_Input, LstOutput),
  cons_cl_list(Tuple_Input, Goal, [], LstRules, ListChildren),  %build the ListChildren, as many children as rules.
  !.

/*********************************************************************/
/* Case 8: It is not a duplicated node and is defined by facts and rules  */
/*********************************************************************/
cons_pr(Tuple_Input, Goal, LstFacts, LstRules, pr([Tuple_Input], Goal, LstOutput, ListChildren, _) ) :-
%write_log('Caso 8 '),
  my_subsumes(Goal, Tuple_Input),  %Goal is more general than  Tuple_Input
  build(Tuple_Input, H),           %Builds in H the same call with fresh variables
  assertz(gt(H)),        %assertzs the new goal "H"
  ordered_et_entries_by_goal(Tuple_Input, LstOutput),
  ft_entries_lines(Tuple_Input, OutputFacts_lines),   %LstOutput unifies with the list of facts which define the Tuple_Input
  NodeFact = [fact([Tuple_Input], Goal, OutputFacts_lines, [], _)],     %Node corresponding to the facts
  cons_cl_list(Tuple_Input, Goal, LstFacts, LstRules, LstTrees),   %build the LstTrees, as many children as rules.
  append(NodeFact,LstTrees, ListChildren),
  !.
  
/*********************************************************************/
/* Case 9:  my_subsume fail: There are two posible reasons por it:   */
/*         -  Goal in no more general then Tuple_Input              */
/*         -  Goal and Tuple_input are different predicates          */
/*********************************************************************/
cons_pr(Tuple_Input, Goal, _LstFacts, _LstRules, pr([Tuple_Input],Goal, [], [], _)).
%write_log('caso 9').

/*********************************************************************/
/* Build a "cl" node for each rule.                          */
/*********************************************************************/
cons_cl_list(_Tuple_Input, _Goal, _LstFacts, [], []).  %  No rules

cons_cl_list(Tuple_Input, Goal, LstFacts, [(R,V, L, Fid)|RestoReglas], [Tree_cl|Resto_cl]) :-
   build(Tuple_Input, CopyGoal),
   cons_cl(Tuple_Input, CopyGoal, LstFacts, (R,V, L, Fid),  Tree_cl),    %node "cl" is associated to the rule R
   cons_cl_list(Tuple_Input, Goal, LstFacts, RestoReglas, Resto_cl), !.

cons_cl_list(Tuple_Input, Goal, LstFacts, [(_R,_V,_L, _Fid)|RestoReglas], Resto_cl) :-
   cons_cl_list(Tuple_Input, Goal, LstFacts, RestoReglas, Resto_cl).

cons_cl(Tuple_Input, Goal, _LstFacts, ((H:-B),V,L, Fid),  Tree) :-
   Goal = H , !,
   listPred_body(B,List_pred),    %List_pred is the list of goals which are in the body of the rule
   Tree = cl([Tuple_Input], Goal, (H:-B), V, L, Fid,  Sol, ChildrenCl, _),  %Sol is the list of solutions obtained by means of the rule (H:-B)
   cons_cl_Children([], List_pred, ChildrenCl, Sub_general), % ChildrenCl is the list of trees, as many trees as goals in the body of the rule
   make_input(Goal, Sub_general, S), % S is the list of tuples obtained by applied the substitution to the goal Goal
   my_remove_duplicates(S,Sol).


/****************************************************************************/
/* build a cl list, one cl_list for each predicate in the body of the rule  */
/****************************************************************************/
% si ya no quedan literales en el lado derecho de la cláusula
cons_cl_Children(Subs,  [], [], Subs).
cons_cl_Children(Subs,  [Pred|_Rp], [T_pred], []) :-
   (Subs = [] -> ( build(Pred, New_input), ordered_et_entries_by_goal(New_input,LstOutput))
               ;
                ( make_input(Pred, Subs, New_input),
                et_entries_l(New_input,LstOutput))
   ),
   LstOutput == [],
   !,
   doListFacts(LstFacts, Pred),
   doListRules(LstRules, Pred),
  % append(Lines1, Lines2, Tot_lines),

   cons_pr(New_input, Pred, LstFacts, LstRules, T_pred).


cons_cl_Children(Subs,  [Pred|Rp], [T_pred|T_Rp], Post_Sub) :-
   (Subs = [] -> (  build(Pred, New_input), ordered_et_entries_by_goal(New_input,LstOutput))
               ;
                 (  make_input(Pred, Subs, New_input),
                    et_entries_l(New_input,LstOutput))
   ),
   LstOutput \== [],
   doListFacts(LstFacts, Pred),
   doListRules(LstRules, Pred),
  % append(Lines1, Lines2, Tot_lines),

   cons_pr(New_input, Pred, LstFacts, LstRules, T_pred),
   obtain_subs(Pred,LstOutput, S),
   join(Subs, S, Previous_Sub),
   cons_cl_Children(Previous_Sub, Rp,  T_Rp, Post_Sub).

/*********************************************************************/
/* The input parameter is a list                                     */
/*********************************************************************/
cons_list_trees([], _Goal, _LstFacts, _LstRules, []).
cons_list_trees([E|Re], Goal, LstFacts, LstRules, [T|LstTrees]) :-
   build(E, Copy),
   cons_pr(E, Copy, LstFacts, LstRules, T ),
   doListFacts(LstFacts2, Goal),    %LstFacts is the list of facts which defined the predicate represented by Goal
   doListRules(LstRules2, Goal),    %LstRules is the list of rules which defined the predicate represented by Goal
   %append(Lf, Lr, Tot_Lines),
   cons_list_trees(Re, Goal, LstFacts2, LstRules2, LstTrees).

et_entries_l([],[]).
et_entries_l([X|Xs],LstOutput) :-
  et_entries_by_goal(X,L),
  et_entries_l(Xs,L2),
  append(L,L2,L3),
% my_remove_duplicates(L3,LstOutput).
  my_remove_duplicates_sort(L3,LstOutput).

ordered_et_entries_by_goal(X,L) :-
  et_entries_by_goal(X,UL),
  my_sort(UL,L).

/*************************************************************************/
/* builds a new input using the list of substitutions                    */
/*************************************************************************/
new_input(_Pred, [], []).

new_input(not(Pred), [Subs|Rs], [not(Input)|Rinput]) :-
  !,
  Pred =.. [Name|NArgs],
  apply_sub(Subs, NArgs, SArgs),
  Input =.. [Name|SArgs],
  new_input(not(Pred), Rs, Rinput).

new_input(Pred, [Subs|Rs], [Input|Rinput]) :-
  Pred =.. [Name|NArgs],
  apply_sub(Subs, NArgs, SArgs),
  Input =.. [Name|SArgs],
  new_input(Pred, Rs, Rinput).

/*************************************************************************/
make_input(Pred, S, L) :-
  new_input(Pred, S, Aux),
  my_remove_duplicates(Aux, L).

/**************************************************************************************/
/* To apply the substitution(first parameter) to the list of arguments (second parameter). */
/**************************************************************************************/
apply_sub(_Subs, [], []).
apply_sub(Subs, [X|Xs], [Y|Rsto]) :-
   var(X),
   is_member((X,Y), Subs), !,
   apply_sub(Subs, Xs, Rsto).

apply_sub(Subs, [X|Xs], [X|Rsto]) :-
   var(X), !,
   apply_sub(Subs, Xs, Rsto).

apply_sub(Subs, [X|Xs], [X|Rsto]) :-
   nonvar(X),
   apply_sub(Subs, Xs, Rsto).
   
/*************************************************************************/
/* Checks whether an argument 'X' is a variable and aditionaly,          */
/* the substitution contains 'X'.                                        */
/*************************************************************************/
is_member((X,C1),[(Y,C2)|_Xs]) :-
  var(X),var(Y), X==Y, C1=C2, !.
is_member(X,[_Y|Xs]) :-
  is_member(X,Xs).


/*******************************************************************************/
/* To obtain all distinct substitutions in order to build the set of solutions */
/*******************************************************************************/
obtain_subs(_Goal, [], []).
obtain_subs(not(Goal), [not(Sol)|RSol], [S|Rs]) :-
  !,
  Goal =.. [Name|ArgsG],
  Sol =.. [Name|ArgsSol],
  subs(ArgsG, ArgsSol, S),
  obtain_subs(not(Goal), RSol, Rs).

obtain_subs(Goal, [Sol|RSol], [S|Rs]) :-
  Goal =.. [Name|ArgsG],
  Sol =.. [Name|ArgsSol],
  subs(ArgsG, ArgsSol, S),
  obtain_subs(Goal, RSol, Rs).

/********************************************************************************************/
% DADA UNA LISTA DE ARGUMENTOS ARG1 CON VARIABLES Y DADA UNA LISTA DE ARGUMENTOS ARG2,
% OBTENEMOS LA SUSTITUCIÓN QUE SE APLICA. AMBAS LISTAS TIENEN EL MISMO NÚMERO DE ARGUMENTOS
/*************************************************************************/
subs([],[],[]).
subs([X|Xs],[Y|Ys],[(X,Y)|Resto]) :-
   var(X), !, subs(Xs, Ys, Resto).
subs([X|Xs],[_Y|Ys],Resto) :-
   nonvar(X), subs(Xs, Ys, Resto).


%ft_entries(S,L) :- bagof(S, ft(S,M), L), !.
ft_entries_lines(S,L) :-
  findall((S, Ls, Fid), 
%          ft(S,Ls, Fid),
          (datalog(S,_Vs,_Rid,_CId,Ls,Fid,_Rs),
           S\=':-'(_,_)),
          L), 
  !.
ft_entries_lines(_S,[]).

ft_entries(S,L) :-
  findall(S,
%          ft(S,_Ls, _Fid), 
          (datalog(S,_Vs,_Rid,_CId,_Ls,_Fid,_Rs),
           S\=':-'(_,_)),
          L),
   !.
ft_entries(_S,[]).


%create_list_var([not(G)|_R], Lvar) :-
%       !,
%       G =.. [_P|Args],
%       extract_vars(Args, Lvar).
%create_list_var([G|_R], Lvar) :-
%       G =.. [_P|Args],
%       extract_vars(Args, Lvar).

%extract_vars([],[]).
%extract_vars([V|RV],[V|Lv]) :- var(V), !, extract_vars(RV,Lv).
%extract_vars([_V|RV],Lv) :- extract_vars(RV,Lv).
/*************************************************************************/
%my_member2((X,Y), [(X,Y)|_R], true).
%my_member2((X,_Y), [(X,_Z)|_R], false).
%my_member2((X,Y), [_|R], Val) :- my_member2((X,Y), R, Val).

/*************************************************************************/
/* join */
/*************************************************************************/
join(L, [], L) :-!.
join([], L, L) :-!.
join(L1, L2, L3) :- cartesiano(L1,L2,L3).

cartesiano([],_Conjunto,[]).
cartesiano([Car|Cdr],Conjunto,Resultado) :-
  lineas(Car,Conjunto,LineasAux),
  my_filter(LineasAux, Lineas),
  cartesiano(Cdr,Conjunto,Resto),
  append(Lineas,Resto,Resultado).

lineas(_Elemento,[],[]).
lineas(Elemento,[Car|Cdr],[L|Resto]) :-
  append(Elemento,Car, Laux),
  my_remove_duplicates(Laux,L),
  lineas(Elemento,Cdr,Resto).

my_filter([], []).
my_filter([X|R1], [X|R2]) :-
  my_valid(X, true),!, my_filter(R1, R2).
my_filter([_X|R1], R2) :-
  my_filter(R1, R2).

my_valid([], true).
my_valid([(X,Y)|R], F ) :-
  (incorrect_line((X,Y),R),!, F=false; my_valid(R, F)).

incorrect_line((X,C1),[(Y,C2)|_Xs]) :-
  var(X),var(Y), X==Y, C1\=C2.
incorrect_line(X,[_Y|Xs]) :-
  incorrect_line(X,Xs).

/*************************************************************************/

my_remove_duplicates([], []).
my_remove_duplicates([Head|Tail1], [Head|Tail2]) :-
  my_delete(Tail1, Head, Residue),
  my_remove_duplicates(Residue, Tail2).

my_delete([], _, []).
my_delete([Head|Tail], Element, Rest) :-
  Head==Element, !,
  my_delete(Tail, Element, Rest).
my_delete([Head|Tail], Element, [Head|Rest]) :-
  my_delete(Tail, Element, Rest).

%-----------------------------------------------------------------------------%
% my_abolish(+Name/+Ar) where Name/Ar: name and arity of the term that we want
%                      to eliminate of the data base.
% This predicate is used to delete the term Name of the data base. If the term
% is in the data base then this predicate delete it, else this predicate don't
% show an error message (abolish would show an error message in this case).
%-----------------------------------------------------------------------------%
my_abolish(Name/Arity) :-
  functor(Term,Name,Arity),
  retractall(Term).
my_abolish(_).

/*********************************************************************/
/* building list of predicates from body of the rule */
/*********************************************************************/
listPred_body(B,[Prim|RestoPred]) :-
  (B=','(Prim,R) , listPred_body(R,RestoPred); B=Prim, RestoPred = []).

/*********************************************************************/
/* write the tree                                                    */
/*********************************************************************/
xml_tree(Tree, _Goal, _Vars) :-
   name('graph_', Lname),
   my_datetime((Y,M,D,H,Mi,S)),
   name(Y,LY),
   name(M, LM),
   name(D, LD),
   name(H, LH),
   name(Mi, LMi),
   name(S,LS),
   %name('_', Guion),

   append(Lname,LY, Lname1),
   append(Lname1,LM, Lname2),
   append(Lname2,LD, Lname3),
   append(Lname3, "_", Lname33),
   append(Lname33,LH, Lname4),
   append(Lname4,LMi, Lname5),
   append(Lname5,LS, Lname6),
   append(Lname6,".xml", L_Ftree),


   name(Ftree, L_Ftree),
  %name(Ftree, "tree.xml"),

   open(Ftree, write, TreeOut),
   write(TreeOut, '<?xml version="1.0" encoding="UTF-8"?>'),

   write(TreeOut,'<infoDebug>'),

   write_tree(TreeOut, [Tree]),
   nl(TreeOut),
   write_file(TreeOut),
   write(TreeOut,'</infoDebug>'),
   close(TreeOut).

% Empty list of trees -------------------------------------------------
write_tree(_Handle, []).

% PR_list node --------------------------------------------------------
write_tree(Handle, [pr_list(Input, Goal, Output, LstChildren, _State)|Rtrees]) :-
   write(Handle,'<tree>'),
   nl(Handle),
   %node
   write(Handle,'   <node type="pr_list">'),
      nl(Handle),
      write(Handle,'     <predicate_name>'),                           %5
         write(Handle,Goal),
      write(Handle,'     </predicate_name>'),
      nl(Handle),
%/%------INPUT ----------------------------------/
       write_Tup_input(Handle,Input),
       nl(Handle),
%/%------OUTPUT ---------------------------------/
       write_Tup_output(Handle,Output),
       nl(Handle),
   write(Handle,'   </node>'),
   nl(Handle),

   %children

   write(Handle,'   <children>'),
         nl(Handle),
         write_tree(Handle,LstChildren),
         nl(Handle),
   write(Handle,'   </children>'),
   nl(Handle),
   write(Handle,'</tree>'),
   nl(Handle),
   write_tree(Handle,Rtrees).


% PR node -----------------------------------------------------------
write_tree(Handle, [pr(Input, Goal, Output, LstChildren, _State)|Rtrees]) :-

   write(Handle,'<tree>'),
   nl(Handle),
   %node
   write(Handle,'   <node type="pr">'),
      nl(Handle),
      write(Handle,'     <predicate_name>'),                           %5
         write(Handle,Goal),
      write(Handle,'     </predicate_name>'),
      nl(Handle),
%/%------INPUT ----------------------------------/
       write_Tup_input(Handle,Input),
       nl(Handle),
%/%------OUTPUT ---------------------------------/
       write_Tup_output(Handle,Output),
       nl(Handle),
   write(Handle,'   </node>'),
   nl(Handle),

   %children

   write(Handle,'   <children>'),
         nl(Handle),
         write_tree(Handle,LstChildren),
         nl(Handle),
   write(Handle,'   </children>'),
   nl(Handle),
   write(Handle,'</tree>'),
   nl(Handle),
   write_tree(Handle,Rtrees).


% duplicate  node -------------------------------------------------------
write_tree(Handle, [dup(Input, Goal, Output, _LstChildren, Ref, _State)|Rtrees]) :-

   write(Handle,'<tree>'),
   nl(Handle),
%node
  write(Handle,'   <node type="dup">'),
      nl(Handle),
      write(Handle,'     <predicate_name>'),                           %5
         write(Handle,Goal),
      write(Handle,'     </predicate_name>'),
      nl(Handle),
%input
      write_Tup_input(Handle,Input),
       nl(Handle),
%output
     write_Tup_output(Handle,Output),
       nl(Handle),
%ref
      write(Handle,'     <reference>'),

         write(Handle, Ref),

      write(Handle,'     </reference>'),
      nl(Handle),
   write(Handle,'   </node>'),
   nl(Handle),

%children
   write(Handle,'   <children>'),
         nl(Handle),
       write(Handle,'   </children>'),
   nl(Handle),
   write(Handle,'</tree>'),
   nl(Handle),
   write_tree(Handle,Rtrees).

%fact node -------------------------------------------------------------
write_tree(Handle, [fact(Input, Goal, Output_fact, _LstChildren,  _State)|Rtrees]) :-

   write(Handle,'<tree>'),
   nl(Handle),
%node
  write(Handle,'   <node type="fact">'),
      nl(Handle),
      write(Handle,'     <predicate_name>'),                           %5
         write(Handle,Goal),
      write(Handle,'     </predicate_name>'),
      nl(Handle),
%input
      write_Tup_input(Handle,Input),
       nl(Handle),
%output
      write_Tup_output(Handle,Output_fact),
       nl(Handle),


   write(Handle,'   </node>'),
   nl(Handle),
%children
   write(Handle,'   <children>'),
         nl(Handle),
   write(Handle,'   </children>'),
   nl(Handle),
   write(Handle,'</tree>'),
   nl(Handle),
   write_tree(Handle,Rtrees).
%----------------------------------
%cl node
%----------------------------------
write_tree(Handle, [cl(Input, Goal, Rule, _V, (Beginline, Endline), _Fid, Output, LstChildren, _State)|Rtrees]) :-
   write(Handle,'<tree>'),
   nl(Handle),
%node
   write(Handle,'   <node type="cl">'),
      nl(Handle),
      write(Handle,'     <predicate_name>'),                           %5
         write(Handle,Goal),
      write(Handle,'     </predicate_name>'),
      nl(Handle),
%/%------INPUT ----------------------------------/
       write_Tup_input(Handle,Input),
       nl(Handle),
%/%------OUTPUT ---------------------------------/
       write_Tup_output(Handle,Output),
       nl(Handle),

%/%------REGLA ---------------------------------/
      write(Handle,'     <rule>'),

         write(Handle, Rule),

      write(Handle,'     </rule>'),
      nl(Handle),
%-------line of rule from file--------/
      write(Handle,'     <line_begin>'),
         write(Handle, Beginline),
      write(Handle,'     </line_begin>'),
      nl(Handle),
      write(Handle,'     <line_end>'),
         write(Handle, Endline),
      write(Handle,'     </line_end>'),
      nl(Handle),
    write(Handle,'   </node>'),
    nl(Handle),
%children

   write(Handle,'   <children>'),
         nl(Handle),
         write_tree(Handle,LstChildren),
         nl(Handle),
   write(Handle,'   </children>'),
   nl(Handle),
   write(Handle,'</tree>'),
   nl(Handle),
   write_tree(Handle,Rtrees).
%----------------------------------
%cl node which has a inserted rule
%----------------------------------

write_tree(Handle, [cl(Input, Goal, Rule, _V, [], _Fid, Output, LstChildren, _State)|Rtrees]) :-
   write(Handle,'<tree>'),
   nl(Handle),
%node
   write(Handle,'   <node type="cl">'),
      nl(Handle),
      write(Handle,'     <predicate_name>'),                           %5
         write(Handle,Goal),
      write(Handle,'     </predicate_name>'),
      nl(Handle),
%/%------INPUT ----------------------------------/
       write_Tup_input(Handle,Input),
       nl(Handle),
%/%------OUTPUT ---------------------------------/
       write_Tup_output(Handle,Output),
       nl(Handle),

%/%------REGLA ---------------------------------/
      write(Handle,'     <rule>'),

         write(Handle, Rule),

      write(Handle,'     </rule>'),
      nl(Handle),
%-------line of rule from file--------/
      write(Handle,'     <line_begin>'),
         nl(Handle),
      write(Handle,'     </line_begin>'),
      nl(Handle),
      write(Handle,'     <line_end>'),
         nl(Handle),
      write(Handle,'     </line_end>'),
      nl(Handle),
    write(Handle,'   </node>'),
    nl(Handle),
%children

   write(Handle,'   <children>'),
         nl(Handle),
         write_tree(Handle,LstChildren),
         nl(Handle),
   write(Handle,'   </children>'),
   nl(Handle),
   write(Handle,'</tree>'),
   nl(Handle),
   write_tree(Handle,Rtrees).

%empty node -----------------------------------------------------------
write_tree(Handle, [empty(_Input, _Goal, _Output, _LstChildren, _State)|Rtrees]) :-
% no hay echos ni reglas.
   nl(Handle),
   write_tree(Handle,Rtrees).

write_Tup_input(_Handle,[]).
write_Tup_input(Handle,[I|Ri]) :-
   write(Handle,'       <input_element>'),
   nl(Handle),
   write(Handle,I),
   nl(Handle),
   write(Handle,'       </input_element>'),
   nl(Handle),
   write_Tup_input(Handle,Ri).
write_Tup_output(_Handle,[]).
write_Tup_output(Handle,[I|Ri]) :-
   write(Handle,'       <output_element>'),
   nl(Handle),
   write(Handle,I),
   nl(Handle),
   write(Handle,'       </output_element>'),
   nl(Handle),
   write_Tup_output(Handle,Ri).



%unify_vars([]) :- !.
%unify_vars([X=Y|R]) :- var(Y), X=Y, unify_vars(R).
%unify_vars([_X=_Y|R]) :- unify_vars(R).

write_file(Handle) :-
  my_working_directory(Path),
  file_table(F, _Fid),
  write(Handle, '<file name="'), write(Handle, F), write(Handle, '">'),
  nl(Handle),
  write(Handle, '<namefile>'), write(Handle, Path), write(Handle, '</namefile>'),   nl(Handle),
  write(Handle, '</file>').
%  seen.              % Close the file

%****************************************************************/
/*           prepare sesion of debugging                     */
%****************************************************************/
level_of_debugging([], _Goal, _Vars, _Level) :-
  write_info_log('Nothing for debugging'), !.
level_of_debugging(Tree, Goal, Vars, Level) :-
%  write_verb([nl,'Info: Strategy for debugging: D&Q', nl, nl]),
  write_verb([nl]),
  nl_compact_verb,
  update_state(Tree, Tree, nonvalid),
%  Level = relation,
%  Level = rule,
  strategy(Tree, Vars, 1, Level, [Goal]).


/******************************************************************/
/*          start of debugging process                            */
/******************************************************************/
strategy([], _Vars, _N, _S, _L) :- 
  write_info_log_list('Nothing to debug.'), !.
strategy(Tree, Vars, 1, 'p', List_nonvalid) :-
  !,
  Type = [pr],
  debug_strategy(Tree, Tree, Vars, Type, List_nonvalid).
strategy(Tree, Vars, 1, 'c', List_nonvalid) :-
  !,
  Type = [pr],
                % the debugger has to consider these types of vertices:
                % pr, cl and fact
  debug_strategy_rule(Tree, Tree, Vars, Type, List_nonvalid).

/******************************************************************/
/*          start of debugging process                            */
/******************************************************************/
%strategy([], _Vars, _N) :- write_log('Nothing for debugging'), !.
%strategy(Tree, Vars, 1) :-
%                    write_verb([nl,'Info: Strategy for debugging: D&C', nl, nl]),
%                    update_state(Tree, Tree, nonvalid),
%                    debug_strategy(Tree, Tree, Vars).

%strategy(Tree, Vars, 2) :-
%                          write_verb([nl,'Info: Strategy for debugging: 2', nl, nl]),
%                          ciclos(C, Nc),
%                          write_log(C), nl_log, write_log(Nc), nl_log,
%                          update_state(Tree, Tree, nonvalid),  % start: root is not valid
%                          append(Nc, C, L),
%                          debug_strategy2(Tree, Vars, L).


/******************************************************************/
/* En esta estrategia se realizan las siguientes acciones         */
/* - Solo se tienen en cuenta los nodos 'pr'.                     */
/* - Se calcula el nodo 'pr' central                              */
/******************************************************************/
debug_strategy(Tree, Candidate , Vars, Type, List_nonvalid) :-
  %  Type= [pr],    %lista de tipos de nodos que hay que contar
  my_buggy([Candidate], Node_buggy, Found),
 (Found == true  ->  info_buggy_vertex(Node_buggy, Vars)
  ;
 (  size([pr,dup], Candidate, N),   %cuento los nodos de tipo Type que tienen estado sin asignar
    N>0 ->
    (  findNode(Type, N, Tree, Candidate, Node),     %busca el nodo (central)por el cual vamos a preguntar al usuario
       write_answer(Node,Vars), %write('  '),
       update_debug_tuples(Node),
       inc_flag(debug_questions),
       askState(Node, Op, State),
       (    ([Op] == "n" ; [Op] == "N") ->
              get_goal(Node, G),
              my_insert(G, List_nonvalid, New_list),
              update_state(Tree, Node, State), 
              debug_strategy(Tree, Node, Vars, Type, New_list )
            ;
            (([Op] == "v" ; [Op] == "V") ->
            update_state(Tree, Node, State),
            debug_strategy(Tree, Candidate, Vars, Type, List_nonvalid)
             ; 
             ([Op] == "a" ; [Op] == "A") -> true; fail)
         )
     )
     ;
     write_buggy_circuit(List_nonvalid),
     display_debug_statistics(datalog)

     %write_info_log(['The strategy cannot proceed. ')    %no ha encontrado un buggy node
     %buscar el buggy cycle
  ))
 .

debug_strategy_rule(_Tree, fact(Input, Goal, Output, LstChildren, State), Vars, _Type, _List_nonvalid) :-
  !,
  (State == nonvalid ->
        nl_log,
        write_buggy(fact(Input, Goal, Output, LstChildren, State), Vars),
        display_debug_statistics(datalog)

  ;
  true ).


debug_strategy_rule(Tree, Candidate , Vars, Type, List_nonvalid) :-
%  Type= [pr],    %lista de tipos de nodos que hay que contar
  my_buggy([Candidate], Node_buggy, Found),
 (Found == true  ->  info_buggy_vertex(Node_buggy, Vars)
  ;
 (  size([pr,dup], Candidate, N),   %cuento los nodos de tipo Type que tienen estado sin asignar

    N>0 ->
    (  findNode(Type, N, Tree, Candidate, Node),   %busca el nodo (central)por el cual vamos a preguntar al usuario
       write_answer(Node,Vars), %write('  '),
       update_debug_tuples(Node),
       inc_flag(debug_questions),
       askState(Node, Op, State),
       (    ([Op] == "n" ; [Op] == "N") ->
              update_state(Tree, Node, State),
              get_goal(Node, G),
              my_insert(G, List_nonvalid, New_list),
              ask_clauses(Tree, Node, Node_clause, Vars),
              ( var(Node_clause) -> !, true
                ;
                   %si el nodo es non-valid, alguna cláusula tiene que ser non-valid.
                   %si todas son valid, hay una inconsistencia en la información proporcionada por el usuario
                   %puede ser que el usuario no sepa contestar sobre la validez de alguna cláusula.

                debug_strategy_rule(Tree, Node_clause, Vars, Type, New_list)
               )
            ;
            (([Op] == "v" ; [Op] == "V") ->
            update_state(Tree, Node, State),
            debug_strategy_rule(Tree, Candidate, Vars, Type, List_nonvalid)
             ; ([Op] == "a" ; [Op] == "A") -> true; fail)
         )
     )
     ;
     write_buggy_circuit(List_nonvalid),
     display_debug_statistics(datalog)
     %write_info_log(['The strategy cannot proceed. ')    %no ha encontrado un buggy node
     %buscar el buggy cycle
  ))
 .

ask_clauses(_Tree, Node, Rule, Vars) :-
   get_children(Node, List_children),
   get_output(Node, Output),

   ask_wrong_or_missing(Output, Kind_error, c),
   ( [Kind_error] == "a" -> true    %abort
     ;
      analizar_li(List_children, Node_clause, _D,  Found, Kind_error , Vars),
     (Found ==true -> Rule = Node_clause
      ;
       (Found == false ->  Rule = Node
       ;
       true
       )
      )
   ).

info_buggy_vertex(Node_buggy, Vars) :-
   get_type(Node_buggy, T),
   T == pr, !,
   nl_compact_log,
   write_buggy(Node_buggy, Vars),
   nl_compact_log,
   display_debug_statistics(datalog),
   write_log(' More information?  '),
   ask_more_info(Op1),
   ( ([Op1] == "y" ; [Op1] == "Y") ->
           nl_compact_log,
           % if the answer is empty, node can not be wrong.
           get_output(Node_buggy, Output),

           ask_wrong_or_missing(Output, Op2, p),
           ( [Op2] == "a" -> true
            ;
                  buscar_wrong(Node_buggy, Rule, Found_cl, Op2, Vars),
                  nl_compact_log,
                  (Found_cl == true
                   ->
                    write_buggy(Rule, Vars), 
                    !,
                    display_debug_statistics(datalog)
                   ;
                     (Found_cl == false ->
                         write_info_log(['Inconsistency or no more information.',nl])
                      ;
                      true
                      )
                  )
          )
      ; true
     ).

info_buggy_vertex(Node_buggy, Vars) :-
   nl_compact_log,
   write_buggy(Node_buggy, Vars),
   display_debug_statistics(datalog).

buscar_wrong(Node_buggy, Rule, Found, Kind_error, Vars) :-
  % Op can has the two following values: w(wrong) or m(missing)
   get_children(Node_buggy, List_children),
   analizar_li(List_children, Rule , _D, Found, Kind_error, Vars).

% parto de un pr non-valid
analizar_li( [], _Rule, _D,  false, _Kind_error, _Vars). % no ha encontrado ningún CL o Fact
% si solo tiene un nodo y existe una wrong answer seguro que es non-valid
analizar_li([Node], Node, D,  true, Kind_error, _Vars) :-
   var(D),
   [Kind_error] == "w", !,
   get_state(Node, State),
   State = nonvalid.            % Si solo hay un nodo, o todos los anteriores son válidos, este tiene que ser falso


analizar_li([Node|Resto], Node_rule, D, Found, _Kind_error, Vars) :-
   get_state(Node, State),
   var(State), !,
   write_answer(Node,Vars), %write('  '),
   update_debug_tuples(Node),
   inc_flag(debug_questions),
   askState(Node, Op, State),
   (    ([Op] == "n" ; [Op] == "N") ->
            Found = true,
            %S = nonvalid,
            Node_rule = Node
         ;
        (([Op] == "v" ; [Op] == "V") ->
             analizar_li(Resto, Node_rule, D, Found, _Kind_error1, Vars)
         ;
         (([Op] == "d" ; [Op] == "D") ->
             analizar_li(Resto, Node_rule, d,  Found, _Kind_error2, Vars)      %alguno es dont-know
            ;
         ([Op] == "a"  ; [Op] == "A") -> true, !)
         )
   ).

analizar_li([_Node|Resto], Node_rule, D,  Found, Op, Vars) :-
   analizar_li(Resto, Node_rule, D, Found, Op, Vars).    % ya he preguntado antes por el stado de Nodo

%debug_strategy1bis(Tree, Vars) :-
%                      Type= [pr],    %lista de tipos de nodos que hay que contar
%                      buggy([Tree], Node_buggy, Found),
%                     (Found == true  ->  nl_log,
%                                         write_buggy(Node_buggy, Vars), !
%                      ;
%                     (  size([pr,dup], Tree, N),   %cuento los nodos de tipo Type que tienen estado sin asignar
%                        N>0 ->
%                        (  findNode(Type, N, Tree, Node),     %busca el nodo (central)por el cual vamos a preguntar al usuario
%                           write_answer(Node,Vars), %write('  '),
%                           askState(Node, Op, State),
%
%                           (    Op == 110 -> update_state(Tree, Node, State), debug_strategy(Node, Vars)
%                                ;
%                                update_state(Tree, Node, State),
%                                debug_strategy(Tree, Vars)
%                             )
%
%                         )
%                         ;
%                         write_info_log(['The strategy cannot proceed. ')
%                      ))
%                     .

%debug_strategy2(Tree, Vars, L) :-
%                      Type= [pr],    %lista de tipos de nodos que hay que contar
%                      buggy([Tree], Node_buggy, Found),
%                     (Found == true  ->  nl_log,
%                                         write_buggy(Node_buggy, Vars), !
%                      ;
%                     (
%                      size(Type, Tree, N),   %cuento los nodos de tipo Type que tienen estado sin asignar
%                      N>0 ->
%                        (  findNode2(Type, L, Tree, Node),
%                           write_answer(Node, Vars), %write('  '),
%                           askState(Node, Op, State),
%
%                           (    Op == 110 -> update_state(Tree, Node, State), debug_strategy2(Node, Vars, L)
%                                ;
%                                update_state(Tree, Node, State),
%                                debug_strategy2(Tree, Vars,L)
%                             )
%
%                         )
%                         ;
%                         write_log('***********************************************'),
%                         write_log('*     Info:   THE STRATEGY CANNOT PROCEED     *'),
%                         write_log('***********************************************')
%                      ))
%                     .

/**************************************************************/
/* Calcula el número de nodos cuyo estado es distinto de      */
/*                 - valid                                    */
/*                 - nonvalid                                 */
/**************************************************************/

/* Empty list of trees  */
size_l(_Type, [], N) :- !, N is 0.
size_l(Type, [T|Rt], N ) :- size(Type, T, M1),
                           size_l(Type, Rt, M2),
                           N is M1 + M2.
/**************************************************************/
/* para los nodos que hay que contar */
size( T, Node, N) :-
   get_type(Node, Type),
   get_state(Node, State),
   member(Type, T),

   !,
   (State == valid ->  get_children(Node,LstChildren),  size_l(T, LstChildren, N)
    ;
        (State == nonvalid -> get_children(Node,LstChildren), size_l(T, LstChildren, N)
          ;
         get_children(Node,LstChildren),
  %       winput(Node),nl_log,
         size_l(T, LstChildren, M),
         N is M+1
         )
   ).
/* si no hay contar el nodo */
size( T, Node, N) :-
   get_children(Node,LstChildren), size_l(T, LstChildren, N).

/*******************************************************************/
medio(N,Optimum) :- M is N//2,
                   (M == 0 -> Optimum is 1
                   ; Optimum = M
                   ).  % división entera.

/*******************************************************************/
/* Busca el elemento N-ésimo de una lista de nodos                 */
/* saltando los nodos valid y nonvalid                             */
/* Si el nodo central es un nodo duplicado, busco el nodo original */
/* y ese es el que devuelvo                                        */
/*******************************************************************/
findNode(_Type, N, Tree, SubTree,  Node) :-
  medio(N,Optimum),
  % write( 'num_nodos:   '), write(N),
  %nl_log,
  my_nth_tree([pr, dup], Optimum, [SubTree], Node1), !,
  ver_si_duplicado([Tree], Node1, Node).


%findNode2(Type, [Predicate|L], Tree, Node) :-
%   findPredicate(Type, Predicate, [Tree], Node, Find),
%   (Find \== true -> findNode2(Type, L, Tree, Node)
%   ; ! ).

%findPredicate(_Type, _Predicate, [], _ ,_ ).
%findPredicate(Type, Predicate, [pr(Input, Goal, Output, LstChildren, State)|_Rt], Node, Find) :-
%      get_type(pr(Input, Goal, Output, LstChildren, State), T),
%      State \== valid,
%      State \== nonvalid,
%      member(T, Type),
%      %Input =..[_, I|R],
%      functor(Goal,N,A),
%      Predicate == N/A, !,
%      Node = pr(Input, Goal, Output, LstChildren, State),
%      Find = true
%      .
%% lo busca en los hijos  --------------------
%findPredicate(Type, Predicate, [Tree|_Rt], Node, F) :-
%      get_children(Tree, Ch),
%      findPredicate(Type, Predicate, Ch, Node, F),
%      F == true,
%      !.
%% si ha fallado, lo busca en los hermanos ----
%findPredicate(Type, Predicate, [_Tree|Rt], Node, F) :-
%      findPredicate(Type, Predicate, Rt, Node, F).

ver_si_duplicado([Tree], dup(I,_G,_O,_Ch,_Ref, _State), Node) :-
  buscar_referencia([Tree], I, Node), !.

ver_si_duplicado([_Tree], Node, Node).

buscar_referencia([], _I, _).
buscar_referencia([pr([Input], Goal, Output, LstChildren, State)|_RTree], I, pr([Input], Goal, Output, LstChildren, State)) :-
  duplicate(Input ,I),  !.
buscar_referencia([T|RT], I, Node) :-
  get_children(T, Chil),
  buscar_referencia(Chil, I, Node),
  (var(Node) -> buscar_referencia(RT, I, Node)
  ; !).


%nodoValido( pr(_Input, _Goal, _Output, _LstChildren, State), Val) :- 
%  (State == valid -> Val = true; Val = false).
%nodoValido( fact(_Input, _Goal, _Output, _LstChildren, State), Val) :-
%  (State == valid-> Val = true; Val = false).
%nodoValido( dup(_Input, _Goal, _Output, _LstChildren, _Ref, State), Val) :-
%  (State == valid-> Val = true; Val = false).
%nodoValido(cl(I, G, R, V, Ls, Fid, O, LstChildren, _), Val) :-
%  hijosValidos(cl(I, G, R, V, Ls, Fid, O, LstChildren, _), LstChildren, Val).
%nodoValido(pr_list(I, G, O, LstChildren, _), Val) :-
%  hijosValidos(pr_list(I, G, O, LstChildren, _),LstChildren, Val).

/****************************************/
/* To find the N-nth node in the tree   */
/****************************************/
my_nth_tree(List_Types, 1, [T|R], Nodo) :-
   get_state(T, State),
   get_type(T, Type),
  ((
   member(Type, List_Types),
   State \== valid ,
   State \==nonvalid ) -> T = Nodo
   ;
   size(List_Types, T, Size),

   ( Size >= 1 -> ( get_children(T, Children),
                    my_nth_tree(List_Types, 1, Children, Nodo)
                  )
      ;
       my_nth_tree(List_Types,1, R, Nodo)
   )
   ).


my_nth_tree(List_Types, N, [T|R], Nodo) :-
   N>1,
   get_state(T, State),
   get_type(T, Type),
  ((
    member(Type, List_Types),
    State \== valid ,
    State \== nonvalid ) -> (  size(List_Types, T, Size), M is N-1,
                               ( Size >= N ->  get_children(T, Ch), my_nth_tree(List_Types, M, Ch, Nodo)
                                 ;
                                 M2 is N-Size,
                                 my_nth_tree(List_Types, M2, R, Nodo)
                                )
                             )
    ;
    size(List_Types,T, Size),
    ( Size >= N ->  (get_children(T, Children),
                    my_nth_tree(List_Types, N, Children, Nodo))
      ;
       M3 is N-Size,
       my_nth_tree(List_Types, M3, R, Nodo)
    )).

update_debug_tuples(pr(_Input, _Goal, Output, _LstChildren, _State)) :-
  !,
  length(Output,NbrTuples),
  add_to_flag(debug_tuples,NbrTuples).
update_debug_tuples(fact(_Input, _Goal, Output, _LstChildren, _State)) :-
  !,
  length(Output,NbrTuples),
  add_to_flag(debug_tuples,NbrTuples).
update_debug_tuples(cl(_Input,_Goal,_Rule, _V, _Ls, _Fid, Output,_Ch,_State)) :-
  !,
  length(Output,NbrTuples),
  add_to_flag(debug_tuples,NbrTuples).
update_debug_tuples(_).


/**************************************************************/
/* write answer */
/**************************************************************/
/* PR node */
write_answer( pr(Input, _Goal, Output, _LstChildren, _State), Vars) :- !,
%     nl_log,
   write_log('Is '),
   write_pred(Input, Vars),
   write_log(' = '),
   write_log('{'),
   write_pred(Output, Vars),
   write_log('} ').

write_answer( fact(Input, _Goal, Output_fact, _LstChildren, _State), Vars) :- !,
   nl_compact_log,
   write_log('In the Extensional Definition: '),
   nl_log,
   write_log('  Is '),
   write_pred(Input, Vars),
   write_log(' = '),
   write_log('{'),
   write_pred_fact(Output_fact, Vars),
   write_log('} ').

write_answer( cl(Input,_Goal,_Rule, _V, Ls, Fid, Output,_Ch,_State), Vars) :-
   findall((R,Var), datalog(R, Var, _Rid,_CId, (_B,_E), _F, _Rs), ListaReglas),
   ListaReglas \== [], !,
   ListaReglas = [(R, Var)],
   nl_log,
   write_log_list(['With the rule: ',nl]),
   write_datalog_rule((R, Var),2),
   nl_log,
   display_rule_info(Ls,Fid),
   %write_log(' Lines: '),
%     write_log(B),
%     write_log(' - '),
%     write_log(E),
   nl_log,
   write_log('  Is '),
   write_pred(Input, Vars),
   write_log(' = '),
   write_log('{'),
   write_pred(Output, Vars),
   write_log('} ').

write_answer( cl(Input,_Goal,Rule, _V, Ls, Fid, Output,_Ch,_State), Vars) :-
   !,
   nl_log,
   write_log_list(['With the rule: ',nl]),
   write_datalog_rule((Rule, Vars),2),
   nl_log,
   display_rule_info(Ls,Fid),
   %write_log(' Lines: '),
%     write_log(B),
%     write_log(' - '),
%     write_log(E),
   nl_log,
   write_log('  Is '),
   write_pred(Input, Vars),
   write_log(' = '),
   write_log('{'),
   write_pred(Output, Vars),
   write_log('} ').

write_answer( Node, _Vars) :- 
  write_log_list(['It is not a PR node',nl,Node]).

/****************************************************************/
/* write buggy */
/****************************************************************/
/* PR node */
write_buggy( pr(Input, Goal, Output, _LstChildren,  _State), Vars) :-
   write_log(' Error in relation:  '),
   functor(Goal,N,A),
   write_log(N), write_log('/'), write_log(A),
   nl_log,
   write_log(' Witness query    :  '),
   write_pred(Input, Vars),
   write_log(' -> '),
   write_log('{'),
   write_pred(Output, Vars),
   write_log('} '),
   nl_log.

write_buggy( fact(Input, Goal, Output_fact, _LstChildren, _State), Vars) :-
   write_log(' Error in relation:  '),
   functor(Goal,N,A),
   write_log(N), write_log('/'), write_log(A), nl_log,
   write_log(' Witness fact     :  '),
   write_pred(Input, Vars),
   write_log(' -> '),
   write_log('{'),
   write_pred_fact(Output_fact, Vars),
   write_log('} '),
   write_lines(Output_fact),
   nl_log.

write_buggy( cl(_Input,Goal,_Rule, _V, Ls, Fid, _Output,_Ch,_State), _Vars) :-
   findall((Re,Vari, Fid), datalog(Re, Vari,_Rid,_CId, Ls, Fid, _Rs), ListaReglas),
   ListaReglas \== [], !,
   ListaReglas = [(R, Var, F)],
   write_log(' Error in relation:  '),
   functor(Goal,N,A),
   write_log(N), write_log('/'), write_log(A), nl_log,
   write_log(' Error in rule    :  '),
   nl_log,
   write_datalog_rule((R, Var),2),
   nl_log,
   display_rule_info(Ls,F),
   nl_log.

write_buggy( _Node, _Vars) :-
   write_log('It is not a PR node').

/****************************************************************/
/* write buggy circuit*/
/****************************************************************/
write_buggy_circuit([]) :-!.
write_buggy_circuit(L) :-
   nl_log,
   write_log('   List of nonvalid relations:  '),
   find_relations(L, List_relations),
   my_remove_duplicates(List_relations, Circuit),
   write_relations(Circuit),
   nl_log.

find_relations([], []).
find_relations([Goal|Resto], [(N,A)|R]) :-
  functor(Goal,N,A),
  find_relations(Resto, R).

write_relations([]).
write_relations([(N,A)]) :- !,
  write_log(N), write_log('/'), write_log(A).

write_relations([(N,A)|R]) :-
  write_log(N), write_log('/'), write_log(A), write_log(','),
  write_relations(R).


/***********************************************************/
/* Write predicate                                         */
/***********************************************************/
write_pred([], _).
write_pred([P|R], Vars) :-
  P=..[Name|Args],
  write_log(Name),
  write_log('('), write_arg(Args, Vars), write_log(')'),
  (R \== [] -> write_log(','),  write_pred(R, Vars)
    ;
    write_pred(R, Vars)).

write_pred_fact([], _).
write_pred_fact([(P, _Ls, _Fid)|R], Vars) :-
  P=..[Name|Args],
  write_log(Name),
  write_log('('), write_arg(Args, Vars), write_log(')'),
  (R \== [] -> write_log(','),  write_pred_fact(R, Vars)
    ;
    write_pred_fact(R, Vars)).

write_lines([]).
write_lines([(_Fact, Ls, Fid)|Resto]) :-
   nl_log,
   display_rule_info(Ls,Fid),
   write_lines(Resto).


/***********************************************************/
/* Write the list of arguments                             */
/***********************************************************/
write_arg([], _Vars).
write_arg([X|Rarg], Vars) :-
  nonvar(X),!, write_log(X),
  (Rarg \== [] -> write_log(','), write_arg(Rarg, Vars)
  ;
  write_arg(Rarg, Vars)).

write_arg([X|Rarg], Vars) :-
  var(X),!,
  dl_find_name_var(X, Vars, Name),  write_log(Name),
  (Rarg \== [] -> write_log(','), write_arg(Rarg, Vars)
  ;
  write_arg(Rarg, Vars)).

/***********************************************************/
/* To find variable names                                  */
/***********************************************************/
dl_find_name_var(X, [], X).
dl_find_name_var(X, [Y=N|_RVars], N) :-
  X == Y, !.
dl_find_name_var(X, [_Y=_N|RVars], Name) :-
  dl_find_name_var(X, RVars, Name).

/***********************************************************/
/* Ask a question: valid(v)/nonvalid(n) [v]?               */
/***********************************************************/
askState(_Node, Op, State) :-
  write_log(' valid(v)/nonvalid(n)/abort(a) [v]? '),
  user_input_string(Str),
  (
  (Str == "v"; Str == "V";  Str=="" ) -> [Op] = "v" , State = valid, !
  ;
  (Str == "n" ; Str == "N") -> [Op] = "n" , State = nonvalid, !
  ;
  (Str == "a" ; Str == "A") -> [Op] = "a" , State = abort, !
  ;
  (Str == "d" ; Str == "D") -> [Op] = "d" , State = dontKnow, !

  ).

askState(Node, Op, State) :-
  askState(Node, Op, State). % Return a wrong answer



ask_more_info(Op) :-
  write_log(' (yes(y)/no(n)/abort(a)) [n]? '),
  user_input_string(Str),
  (
  (Str == "n";  Str == "N";  Str=="" ) -> [Op] = "n" , !
  ;
  (Str == "y"; Str == "Y") -> [Op] = "y" , !
  ;
  (Str == "a"; Str == "a") -> [Op] = "a" , !
   ).
ask_more_info( Op) :-
  ask_more_info(Op). % Return a wrong answer


ask_wrong_or_missing([], Op, _V) :- [Op] = "m", !.

ask_wrong_or_missing(_Output, Op, c) :-
 %  write_log(' (wrong answer(w)/missing answer(m)/abort(a)) [w]? '),
  inc_flag(debug_questions),
  write_log( 'Why is nonvalid, is a wrong answer(w)/missing answer(m)/abort(a) [w]? '),
  user_input_string(Str),
  (
  (Str == "w";  Str=="" ) -> [Op] = "w" , !
  ;
  (Str == "m") -> [Op] = "m" , !
  ;
  (Str == "a") -> [Op] = "a" , !
   ).
ask_wrong_or_missing(_Output, Op, p) :-
 %  write_log(' (wrong answer(w)/missing answer(m)/abort(a)) [w]? '),
  inc_flag(debug_questions),
  write_log( 'Is the witness query a wrong answer(w)/missing answer(m)/abort(a) [w]? '),
  user_input_string(Str),
  (
  (Str == "w";  Str=="" ) -> [Op] = "w" , !
  ;
  (Str == "m") -> [Op] = "m" , !
  ;
  (Str == "a") -> [Op] = "a" , !
   ).
ask_wrong_or_missing( Output, Op, V) :-
  ask_wrong_or_missing(Output,Op, V). % Return a wrong answer

/* PR node */
/**********************************************************************************/
/* update_state: */

update_state( Tree, pr(Input, _Goal, _Output, _LstChildren, State), New_State) :-
(
(State == valid; State == nonvalid; State == dontKnow) -> write_info_log(['Node has been updated already'])
 ;
 (State = New_State,
     update_tree([Tree], Input, New_State))
 ).

/* duplicate  node */
update_state( Tree, dup(Input, _Goal, _Output, _LstChildren, _Ref, State), New_State) :-
((State == valid; State == nonvalid; State == dontKnow) -> write_info_log(['Node has been updated already'])
 ;
 (State = New_State,
  update_tree([Tree], Input, New_State))
  ).

/* fact node */
update_state(_Tree, fact(_Input, _Goal, _Output, _LstChildren, State), New_State) :-
(
(State == valid; State == nonvalid; State == dontKnow) -> write_info_log(['Node has been updated already'])
 ;
 (State = New_State
 )
 ).

/******************************************************************************/
/* hijosValidos(+Tree, +Nodo, -Valid).  */
/* hijosValidos(+Tree, +[Nodos], -Valid).         */

hijosValidos( T, pr(_I,_G,_O,Ch,_State), Val) :- hijosValidos(T, Ch, Val).
hijosValidos( T, empty(_I,_G,_O,Ch,_State), Val) :- hijosValidos(T, Ch, Val).
hijosValidos( T, dup(_I,_G,_O,Ch,_Ref, _State), Val) :- hijosValidos(T, Ch, Val).
hijosValidos( T, fact(_I,_G,_O,Ch,_State), Val) :- hijosValidos(T, Ch, Val).
hijosValidos( T, cl(_I,_G,_Rule, _V, _Ls, _Fid, _O,Ch,_State), Val) :- hijosValidos(T, Ch, Val).
hijosValidos( T, pr_list(_I,_G,_O,Ch,_State), Val) :- hijosValidos(T, Ch, Val).

hijosValidos( _T, [], true).
hijosValidos( pr(I1, G1, O1, LstChildren, E1), [dup(I2,_G,_O,_Ch,_Ref, _E2)|Rc], ValR) :-
   my_subsumes(I2, I1), !, % son iguales salvo renombramiento de variables
   % no pregunto por el nodo duplicado
   hijosValidos(pr(I1, G1, O1, LstChildren, E1), Rc, ValR).

hijosValidos( pr(I1, G1, O1, LstChildren, E1), [fact(_I2,_G,_O,_Ch,_E2)|Rc], ValR) :-
   % no pregunto por el nodo fact
   hijosValidos(pr(I1, G1, O1, LstChildren, E1), Rc, ValR).

hijosValidos( pr(I1, G1, O1, LstChildren, E1), [empty(_I2,_G,_O,_Ch,_E2)|Rc], ValR) :-
   % no pregunto por el nodo empty
   hijosValidos(pr(I1, G1, O1, LstChildren, E1), Rc, ValR).
hijosValidos( T, [C|Rc], Val) :-
  nodoValido(T, C, Val1), hijosValidos(T, Rc, ValR),
  ((Val1 == true,  ValR ==true) -> Val = true
  ;
  Val = false).
  
/******************************************************************/
/* Valid Node is when its State is valid too.                     */
/******************************************************************/
nodoValido( _T, empty(_, _Goal, _LstOutput, _, _), true).
nodoValido( _T, pr(_Input, _Goal, _Output, _LstChildren, State), Val) :- (State == valid -> Val = true; Val = false).
nodoValido( _T, fact(_Input, _Goal, _Output, _LstChildren, State), Val) :- (State == valid-> Val = true; Val = false).
nodoValido( _T, dup(_Input, _Goal, _Output, _LstChildren, _Ref, State), Val) :- (State == valid-> Val = true; Val = false).
nodoValido(T, cl(_Input, _Goal, _Rule, _V, _Ls, _Fid, _Output, LstChildren, _), Val) :-
  hijosValidos(T, LstChildren, Val).
nodoValido(T, pr_list(_Input, _Goal, _Output, LstChildren, _), Val) :-
        hijosValidos(T, LstChildren, Val).

/*****************************************************************/
/* Find in the tree a node which is a wrong node.                */
/*****************************************************************/

/* Find buggy */
my_buggy([], _ , _). % no hemos encontrado ningún error.
% No estudio si un nodo duplicado es erroneo
my_buggy([T|R], Node_buggy , Found) :-
   get_type(T, Type),
   Type == dup, !,
   my_buggy(R, Node_buggy, Found).
my_buggy([T|R], Node_buggy , Found) :-
   get_state(T, State),
   (State == nonvalid -> ( hijosValidos(T, T, Val),
                            (Val == true -> Node_buggy = T, Found = true, !
                                            ;
                                            my_buggy(R, Node_buggy, Found),
                                           (Found == true -> !
                                                      ;
                                                     get_children(T, Children),
                                                     my_buggy(Children, Node_buggy, Found)
                                            )
                             )
                            )

   ;
   my_buggy(R, Node_buggy, Found),
     (Found == true -> !
        ;
      get_children(T, Children),
      my_buggy(Children, Node_buggy, Found)
      )
   ).

%unificar_dupl(Tree) :-   build_lists(Tree,O, D),  % en la lista O tenemos todos los nodos pr y fact
%                        update_state_tree(O, D),        % ahora, por cada uno de ellos, busco sus duplicados y unifico
%                                                       % la variable State si es posible
%                        write_log('Lista de originales:   '), nl_log,
%                        write_log(O),
%                        nl_log,
%                        write_log('Lista de duplicados:   '), nl_log,
%                        write_log(D),
%                        nl_log.

%actualizar_referencias(Tree, O, D) :- build_lists(Tree, O, D),
%                                     bound_ref(O,D).

/**************************************************************************/
/* build_lists: crea dos listas partiendo del árbol de depuración:        */
/*        L: lista de nodos de tipo 'pr' y 'fact'                         */
/*        D: lista de nodos de tipo 'dup'                                 */
/**************************************************************************/
%build_lists_l([], [], []).
%build_lists_l([T|Rt], L , D) :- build_lists(T, L1, D1),
%                               build_lists_l(Rt, L2, D2),
%                               append(L1,L2, L),
%                               append(D1,D2, D).
%
%/* PR_list node : This node is ignored  */
%build_lists(pr_list(_Input, _Goal, _Output, LstChildren, _State), L,D) :-
%       build_lists_l(LstChildren, L, D), !.
%
%/* PR node */
%build_lists( pr(Input, _Goal, _Output, LstChildren, State), [(Input, State)|L], D) :-
%       build_lists_l(LstChildren, L, D), !.
%
%/* duplicate  node */
%build_lists( dup(Input, _Goal, _Output, _LstChildren, Ref, State), _, [(Input, State, Ref)]) :- !.
%
%
%/* fact node */
%build_lists( fact(Input, _Goal, _Output, _LstChildren,  State), [(Input, State)], _) :- !.
%
%
%/* cl node */
%build_lists( cl(_Input, _Goal, _Rule, _V, _Ls, _Fid, _Output, LstChildren, _State), L, D) :-
%        build_lists_l(LstChildren, L, D), !.
%
%build_lists( _Resto_de_Nodos, _L, _D).   %Para los nodos de tipo Empty

/******************************************************************************/
/*  update_state_tree: Se recorre la lista de los duplicados y comprueba      */
/*                     su estado para modificar el estado de los originales   */
/*                     y viceversa                                            */
/*  bound_state: Busca un elemento duplicado en una lista de originales y     */
/*                          actualiza el Estado si es posible                 */
/******************************************************************************/
%update_state_tree(_O, []).
%update_state_tree(O, [D|Rd]) :- bound_state(D, O),
%                           update_state_tree(O, Rd).
%
%
%bound_state((_Dinput,_E, _Ref), []).
%bound_state((I1,E1, Ref), [(I2, E2)|R]) :-
%       E1==valid,
%       build(I1, H1),
%       (my_subsumes(H1, I2) -> (E2=valid, bound_state((I1,E1, Ref), R), !))
%       .
%
%bound_state((I1,E1, Ref), [(I2, E2)|R]) :-
%       E1==nonvalid,
%       build(I2, H2),
%      ( my_subsumes(H2, I1) -> (E2=nonvalid, bound_state((I1,E1, Ref), R), !))
%       .
%bound_state((I1,E1, Ref), [(I2, E2)|R]) :-
%       E2==valid,
%       build(I2, H2),
%       (my_subsumes(H2, I1) -> (E1=valid, bound_state((I1,E1, Ref), R), !))
%       .
%
%bound_state((I1,E1, Ref), [(I2, E2)|R]) :-
%       E2==nonvalid,
%       build(I1, H1),
%       (my_subsumes(H1, I2) -> (E1=nonvalid, bound_state((I1,E1, Ref), R), !))
%       .
%bound_state((Dinput,E, Ref), [_Y|R]) :- bound_state((Dinput,E, Ref), R).


/*********************************************************************************/
/* bound_ref: primer argumento: lista de nodos de tipo 'pr' y 'fact'              */
/*           segundo argumento: lista de nodos de tipo 'dup'.                    */
/* Busca los elementos duplicados en la lista de originales para actualizar el   */
/* campo 'ref'. Busca un nodo original cuyo input I subsuma al duplicado         */
/*********************************************************************************/
%bound_ref(_O,[]).
%bound_ref(O, [D|Rd] ) :- do_ref(D, O), bound_ref(O,Rd).
%
%do_ref((_Dinput,_E, _Ref), []).
%do_ref((Dinput,_E, Ref), [(I2, _E2)|_R]) :- my_subsumes(I2, Dinput),  Ref =  I2, !.
%do_ref((Dinput,E, Ref), [_Y|R]) :- do_ref((Dinput,E, Ref), R).
/*********************************************************************************/
/* El primer argumento son los nodos que queremos modoficar */
/* El segundo argumento es el nodo que hemos modificado.      */
    % si New_State = nonvalid -> modifico el estado de todos aquellos que sean más generales
    % si New_State = valid -> modifico el estado de aquellos que sean más particulares
/*********************************************************************************/
update_tree([],_I, _E).
/******************************************************************************************/
update_tree([empty(_, _, _, _, _)|Rtree], I,  E) :- !,
                     update_tree(Rtree, I, E ).



/* nonvalid */
update_tree([pr(Input1, G1, O1, Ch1, E1)|Rtree], Input2,  nonvalid) :-
   my_subsumes(Input1, Input2), !,
   ((var(E1); E1==nonvalid) ->
       (E1 = nonvalid,
       update_tree(Ch1, Input2, nonvalid ),
       update_tree(Rtree, Input2, nonvalid )
       )
    ;
    write_log('Error: Uncompatible state'), nl_log, write_buggy(pr(Input1, G1, O1, Ch1, E1), _)
       ).
update_tree([dup(Input1, G1, O1, Ch1, Ref, E1)|Rtree], Input2,  nonvalid) :-
   my_subsumes(Input1, Input2), !,
   ((var(E1); E1==nonvalid) ->
       (E1 = nonvalid,
        update_tree(Rtree, Input2, nonvalid )
       )
    ;
    write_log('Error: Uncompatible state'), nl_log, write_buggy(dup(Input1, G1, O1, Ch1, Ref, E1), _)
       ).
% si Input2 es más general que Input1
update_tree([T|Rtree], Input2,  nonvalid) :-
   get_children(T,Ch1),
   update_tree(Ch1, Input2, nonvalid ),
   update_tree(Rtree, Input2, nonvalid ).
/******************************************************************************************/
/* valid */
update_tree([pr(Input1, G1, O1, Ch1,E1)|Rtree], Input2,  valid) :-
   my_subsumes(Input2, Input1), !,
   ((var(E1); E1==valid) ->
       (E1 = valid,
       update_tree(Ch1, Input2, valid ),
       update_tree(Rtree, Input2, valid )
       )
    ;
    write_log('Error: Uncompatible state'), nl_log, write_buggy(pr(Input1, G1, O1, Ch1, E1),_)
       ).
update_tree([dup(Input1, G1, O1, Ch1, Ref, E1)|Rtree], Input2,  valid) :-
   my_subsumes(Input2, Input1), !,
   ((var(E1); E1==valid) ->
       (E1 = valid,
        update_tree(Rtree, Input2, valid )
       )
    ;
    write_log('Error: Uncompatible state'), nl_log, write_buggy(dup(Input1, G1, O1, Ch1, Ref, E1),_)
       ).
% si Input2 es más general que Input1


update_tree([T|Rtree], Input2,  valid) :-
   get_children(T,Ch1),
   update_tree(Ch1, Input2, valid ),
   update_tree(Rtree, Input2, valid ).



/*********************************************************************************/
/* devuelve el tipo de nodo en el segundo argumento               */
/*********************************************************************************/
get_type(pr(_Input, _Goal, _Output, _LstChildren, _State), pr).
get_type(empty(_Input, _Goal, _Output, _LstChildren, _State), empty).
get_type(dup(_Input, _Goal, _Output, _LstChildren, _Ref, _State), dup).
get_type(cl(_Input, _Goal, _Rule, _V, _Ls, _Fid, _Output, _LstChildren, _State), cl).
get_type(fact(_Input, _Goal, _Output, _LstChildren,  _State), fact).
get_type(pr_list(_Input, _Goal, _Output, _LstChildren, _State), pr_list).
/*********************************************************************************/
/* devuelve el objetivo               */
/*********************************************************************************/
get_goal(pr(_Input, Goal, _Output, _LstChildren, _State), Goal).
get_goal(empty(_Input, Goal, _Output, _LstChildren, _State), Goal).
get_goal(dup(_Input, Goal, _Output, _LstChildren, _Ref, _State), Goal).
get_goal(cl(_Input, Goal, _Rule, _V, _Ls, _Fid, _Output, _LstChildren, _State), Goal).
get_goal(fact(_Input, Goal, _Output, _LstChildren,  _State), Goal).
get_goal(pr_list(_Input, Goal, _Output, _LstChildren, _State), Goal).
/***********************************************************/
/* get children: devuelve la lista de hijos                */
/***********************************************************/
get_children(pr(_I,_G,_O,Ch,_State), Ch).
get_children(empty(_I,_G,_O,Ch,_State), Ch).
get_children(dup(_I,_G,_O,Ch, _Ref, _State), Ch).
get_children(fact(_I,_G,_O,Ch,_State), Ch).
get_children(pr_list(_Input, _Goal, _Output, Ch, _), Ch).
get_children(cl(_I, _G, _Rule, _V, _Ls, _Fid, _O, Ch, _), Ch).
/***********************************************************/
/* get OUTPUT: devuelve THE ANSWER                */
/***********************************************************/
get_output(pr(_I,_G,O,_Ch,_State), O).
get_output(empty(_I,_G,O,_Ch,_State), O).
get_output(dup(_I,_G,O,_Ch, _Ref, _State), O).
get_output(fact(_I,_G,O,_Ch,_State), O).
get_output(pr_list(_Input, _Goal, O, _Ch, _), O).
get_output(cl(_I, _G, _Rule, _V, _Ls, _Fid, O, _Ch, _), O).
/***********************************************************/
/* get estado: devuelve el estado del nodo                 */
/***********************************************************/
get_state(pr(_I,_G,_O,_Ch,State), State).
get_state(empty(_I,_G,_O,_Ch,State), State).
get_state(dup(_I,_G,_O,_Ch, _Ref, State), State).
get_state(fact(_I,_G,_O,_Ch,State), State).
get_state(pr_list(_Input, _Goal, _Output, _Ch, State), State).
get_state(cl(_I, _G, _Rule, _V, _Ls, _Fid, _O, _Ch, State), State).

% La lista C contiene los predicados que no forman parte de ciclos
% La lista NC contiene los predicados que forman parte de ciclos

%ciclos(C, NC) :-
%   pdg((N,A)),
%   caminos(N, (N,A), C, NC).
%
%caminos([], (_N, _A), [], []).
%caminos([P|Resto], (N,A) ,[P|Rc], NC) :- camino(P, P, (N,A)), !,
%                                   caminos(Resto, (N,A), Rc, NC).
%caminos([P|Resto], (N,A), C, [P|NC]) :- caminos(Resto, (N,A), C, NC).
%
%edge(F,T, A) :- arc(F,T,F+T,A).
%edge(F,T, A) :- arc(F,T,F-T,A).
%
%camino(P, Q, (_N,A)) :- edge(P,Q, A).
%camino(P,Q, (N,A)) :- member(R, N),
%                     edge(P, R, A),
%                     camino(R, Q, (N,A)).

%my_nth1(1, [X|_R], X) :- !.
%my_nth1(N, [_|R], X) :- M is N-1, my_nth1(M, R, X).

%my_insert(X, [], [X]) :- !.
my_insert(X, L, [X|L]).

/**********************************************************/
/* 'takeout(X,Z,W)' can also be interpreted as "insert X into W to produce Z". */
/**********************************************************/
%my_takeout(X,[X|R],R).
%my_takeout(X,[F|R],[F|S]) :- my_takeout(X,R,S).
%
%my_putin(X,L,R) :- my_takeout(X,R,L).
%
%winput(pr(I,_G,O,_Ch,_State)) :- write_log('pr'), write_log(I), write_log('->'), write_log(O).



is_duplicate(Pred) :-
  bagof(M, gt(M), Bag),
  duplicate(Pred, Bag).

duplicate(_Pred, []) :- fail.
duplicate(Pred, [Goal|_Rest]) :-
   Pred=..[Name|Args1],
   Goal=..[Name|Args2],
   see_arg(Args1, Args2), !.


duplicate(Pred, [_Goal|Rest]) :-
   duplicate(Pred, Rest).



see_arg([],[]).
see_arg([X|Resto1],[Y|Resto2]) :- var(X), var(Y),!, see_arg(Resto1,Resto2).
see_arg([X|Resto1],[Y|Resto2]) :- X==Y, see_arg(Resto1,Resto2).


/*--------------------------------------------------------------------------*/
/*               GENERAR NONBRES DE VARIABLES                               */
/*--------------------------------------------------------------------------*/
all_variables([], []).

all_variables([pr([I],_G,_O,Ch,_State)|Resto], Lvar) :-
  I=..[_Name|Args],
  are_variables(Args, L),
  all_variables(Ch,Lvarchildren),
  all_variables(Resto,LResto),
  append(L,Lvarchildren, Laux ),
  append(Laux, LResto, Lvar).

all_variables([empty([I],_G,_O,Ch,_State)|Resto], Lvar) :-
  I=..[_Name|Args],
  are_variables(Args, L),
  all_variables(Ch,Lvarchildren),
  all_variables(Resto,LResto),
  append(L,Lvarchildren, Laux ),
  append(Laux, LResto, Lvar).

all_variables([dup([I],_G,_O,Ch, _Ref, _State)|Resto], Lvar) :-
  I=..[_Name|Args],
  are_variables(Args, L),
  all_variables(Ch,Lvarchildren),
  all_variables(Resto,LResto),
  append(L,Lvarchildren, Laux ),
  append(Laux, LResto, Lvar).

all_variables([fact([I],_G,_O,Ch,_State)|Resto], Lvar) :-
  I=..[_Name|Args],
  are_variables(Args, L),
  all_variables(Ch,Lvarchildren),
  all_variables(Resto,LResto),
  append(L,Lvarchildren, Laux ),
  append(Laux, LResto, Lvar).

all_variables([pr_list(_Input, _Goal, _Output, Ch, _State)|Resto], Lvar) :-
%    I=[..[Name|Args]],
%    are_variables(Args, L),
  all_variables(Ch,Lvarchildren),
  all_variables(Resto,LResto),
  append(Lvarchildren, LResto, Lvar).

all_variables([cl([I], _G, _Rule, _V, _Ls,_Fid, _O, Ch, _State)|Resto], Lvar) :-
  I=..[_Name|Args],
  are_variables(Args, L),
  all_variables(Ch,Lvarchildren),
  all_variables(Resto,LResto),
  append(L,Lvarchildren, Laux ),
  append(Laux, LResto, Lvar).

assertlatestNameVar(Name) :- assertz(latestNameVar(Name)).
retractlatestNameVar(N) :- retract(latestNameVar(N)).
retractalllatestNameVar:- my_abolish(latestNameVar/1).


generate_names([], []).
generate_names([X|L], [(X=N)|LR]) :-
   generateVarName(N),
   generate_names(L, LR).

%----------------------------------------------------------------------------%
% nextVarName(-NameVar)
% devuelve nombre de variable aun no utilizado
%----------------------------------------------------------------------------%
nextVarName(NameVar) :-
 (
  % obtenemos el ultimo nombre de variable utilizado
  latestNameVar(LastNameVar),
  generateNewName(LastNameVar,NameVar)
 ;
  % si no hay ninguno empezamos por la "A"
  NameVar = "A"
 ),
 !.
%----------------------------------------------------------------------------%
% generateNewName(+LastNameVar,-NameVar)
% Genera un nuevo nombre de variable sabiendo que el ultimo fue LastNameVar
%----------------------------------------------------------------------------%
generateNewName(LastNameVar,NameVar) :-
 !,      % convertimos LastNameVar a numero
 nameToNumber(LastNameVar, 0, Number),
 NewNumber is Number+1,
 extractName(NewNumber,NameVar).

%----------------------------------------------------------------------------%
% nameToNumber(+Name,+PartialNumber, -Number)
% Genera un numero a partir de una cadena de caracteres, segun el orden
% lexicografico
%----------------------------------------------------------------------------%
nameToNumber([],PartialNumber,PartialNumber) :- !.
nameToNumber([X|Xs],PartialNumber,Number) :-
   !,
   PartialNumber2  is PartialNumber*26 + (X-65),
   nameToNumber(Xs,PartialNumber2,Number).

%----------------------------------------------------------------------------%
% extractName(+Num,-Name)
% generacion de nombres de las variables
%----------------------------------------------------------------------------%
extractName(0,[65]) :-!.
extractName(Num,[Code|L]) :-
   Rest is Num mod 26,
   Code is Rest + 65,
   Quotient is Num // 26,
   (Quotient > 0, P is Quotient-1,extractName(P,L);
   L=[]).
%--------------------------------------------------------------------

generateVarName(Var) :-
   !,
   nextVarName(Var1),
   name(Var,Var1),
   % guardamos el nuevo nombre en la B.D. borrando el antiguo
   (
    retractlatestNameVar(_)
   ;
    true
   ),
   assertlatestNameVar(Var1).

%/***********************************************************/
%/* are variables                       */
%/***********************************************************/
are_variables([], []).
are_variables([X|Rarg], Vars) :-
    nonvar(X),!, are_variables(Rarg, Vars).

are_variables([X|Rarg], [X|Vars]) :-
    var(X),!,
    are_variables(Rarg, Vars).


% %%%%%%%%%%%%%%%  END des_debug.pl  %%%%%%%%%%%%%%%
