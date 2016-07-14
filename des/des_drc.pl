/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.4.1                 */
/*                                                       */
/*    Domain Relational Calculus Processor               */
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PARSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Domain Relational Calculus Grammar. 
% Builds on [Diet01]  Understanding Relational Database Query Languages, S. W. Dietrich, Prentice Hall, 2001.
% Adds:
% - Implication and membership operators
% - Complete type checking (e.g., a numeric constant cannot be compared with a string field)
% - A comparison can occur before its data source
% - Syntax sugaring allowing constants in the result tuple
% - Better safety messages
% - Tuples can be built with constants, in addition to domain variables
% - Support for propositional relations
% - Parentheses surrounding quantified formulas are not required 
% - Parentheses surrounding quantifiers are not required 
% - Variables starting with an underscore are existentially quantified by default (the same as for anonymous variables)
% Fixes:
% - Invalid uses of variables in the scope of a universal quantifier

% % Domain Relational Calculus statement:
% DRCstmt ::= 
%   { VarsCtes | Formula }
%
% Formula ::=
%   Formula   AND   Formula
%   |
%   Formula   OR    Formula
%   |
%   Formula IMPLIES Formula
%   |
%   Formula   ->    Formula   % Synonymous for IMPLIES
%   |
%   (Formula)
%   |
%   Relation (VarsCtes)
%   |
%   VarsCtes [NOT] IN Relation 
%   |
%   NOT Formula
%   |
%   Condition
%   |
%   QuantifiedFormula
%
% QuantifiedFormula ::=
%   Quantifier Formula
%   |
%   Quantifier QuantifiedFormula
%
% Quantifier ::=
%   EXISTS Vars
%   |
%   FORALL Vars
%
% Condition ::=
%   VarCte RelationalOp VarCte
%
% VarCte ::=
%   Variable
%   |
%   Constant
%
% VarsCtes ::=
%   VarCte
%   |
%   VarCte, VarsCtes
%
% Vars ::=
%   Variable
%   |
%   Variable, Vars
%
% RelationalOp ::=
%     =
%   | >
%   | <
%   | <>
%   | !=
%   | >=
%   | <=
%   

% % View definition (assignment statement):
% DRCview ::=
%   [Schema | ViewName] := [DRCstmt | ViewName]

% Schema ::=
%   ViewName
%   |
%   ViewName(ColName,...,ColName)


% Parsing DRC statements
parse_drc_query(DRCst) -->
  {my_retract_all_facts(dictionary(_)),
   assertz(dictionary([]))}, % WARNING: ONLY FOR TEST CASE GENERATION
%  my_sql_blanks_star, 
  my_drc(DRCst,[],_Vo),
  my_sql_blanks_star, 
  my_optional(";").

% DRC Statement
%   DDL Statement
my_drc(DRCst,Vi,Vo) -->
  push_syntax_error(['Expected relation definition (view name or schema followed by '':='')'],Old),
  my_drc_DDL(DRCst,Vi,Vo),
  pop_syntax_error(Old).
%   DQL Statement
my_drc(DRCst,Vi,Vo) -->
  my_drc_DQL(DRCst,Vi,Vo).

% DRC DDL Statement
% Relation copying:
my_drc_DDL(assign(select(true,Relation),Schema),Vi,Vi) --> % Reuse RA assignment
  my_create_view_schema(Schema),
  my_sql_blanks_star, 
  is_valid_drc_schema(Schema),
  push_syntax_error(['Expected '':='''],Old1),
  ":=",
  pop_syntax_error(Old1),
  my_sql_blanks_star, 
  push_syntax_error(['Expected a relation name'],Old2),
  my_drc_relation_name(Relation),
  pop_syntax_error(Old2).
% New Relation from its DRC definition:
my_drc_DDL(assign(DRCst,Schema),Vi,Vo) -->
  my_create_view_schema(Schema),
  is_valid_drc_schema(Schema),
  my_sql_blanks_star, 
  push_syntax_error(['Expected '':='''],Old),
  ":=",
  pop_syntax_error(Old),
  my_sql_blanks_star, 
  my_drc_DQL(DRCst,Vi,Vo).

% DRC Relation: Table or View  
my_drc_relation_name(RelationName) -->
  my_symbol(RelationName),
  !,
  is_valid_drc_relation_name(RelationName).

is_valid_drc_schema(Schema) -->
  {functor(Schema,Name,_A)},
  is_valid_drc_relation_name(Name).
  
is_valid_drc_relation_name(Name) -->
  {to_lowercase(Name,DName),
   drc_keyword(DName)},
  push_syntax_error(['Keywords are not allowed as relation names'],_Old),
  {!,
   fail}.
is_valid_drc_relation_name(_Name) -->
  [].
  
drc_keyword(and).
drc_keyword(or).
drc_keyword(not).
drc_keyword(in).
drc_keyword(implies).
drc_keyword(exists).
drc_keyword(forall).

% DRC DQL Statement
my_drc_DQL(drc_query(VarsCtes,Vo,DRCst),Vi,Vo) -->
  push_syntax_error(['Expected start of DRC statement (left curly bracket: ''{'')'],Old1),
  "{",
  pop_syntax_error(Old1),
  my_sql_blanks_star, 
  push_syntax_error(['Expected comma-separated variables and/or constants.'],Old2),
  my_drc_var_cte_seq(VarsCtes,Vi,Vi1),
  pop_syntax_error(Old2),
  my_sql_blanks_star, 
  push_syntax_error(['Expected vertical bar (|)'],Old3),
  "|",
  pop_syntax_error(Old3),
  push_syntax_error(['Expected valid DRC formula'],Old4),
  my_sql_blanks_star, 
  my_drc_formula(DRCst,Vi1,Vo),
  pop_syntax_error(Old4),
  my_sql_blanks_star, 
  push_syntax_error(['Expected end of DRC statement (right curly bracket: ''}'')'],Old5),
  "}",
  pop_syntax_error(Old5),
  {!,
   is_drc_safe(drc_query(VarsCtes,Vo,DRCst))}.

my_drc_formula(F,Vi,Vo) -->
  my_drc_formula(1200,F,Vi,Vo).
   
my_drc_formula(PP,To,Vi,Vo) -->
  my_factor_drc_formula(L,Vi,Vo1), 
  my_r_drc_formula(PP,0,L/To,Vo1,Vo).
my_drc_formula(PP,To,Vi,Vo) -->
  "(", 
  my_sql_blanks_star, 
  push_syntax_error(['Expected valid DRC formula'],Old),
  my_drc_formula(1200,T,Vi,Vo1), 
  pop_syntax_error(Old),
  my_sql_blanks_star, 
  my_right_parenthesis,
  !,
  my_r_drc_formula(PP,0,T/To,Vo1,Vo).
my_drc_formula(PP,To,Vi,Vo) -->
  {my_drc_operator(P,FX,SOP,OP),
   prefix(P,FX,PR),
   P=<PP},
  my_kw(SOP),
  push_syntax_error(['Expected valid DRC formula'],Old),
  my_right_spaced(SOP),
  my_drc_formula(PR,T,Vi,Vo1), 
  pop_syntax_error(Old),
  {NT=..[OP,T]},
  my_r_drc_formula(PP,P,NT/To,Vo1,Vo).
  
  
% infix operator
%   DRCformula op DRCformula 
%   op ::= and | or | implies | -> 
my_r_drc_formula(PP,Pi,Ti/To,Vi,Vo) -->
  {my_drc_operator(P,YFX,SOP,OP),
   infix(P,YFX,PL,PR),
   P=<PP,
   Pi=<PL,
   NT=..[OP,Ti,T]},
  my_left_spaced(SOP), 
  push_syntax_error(['Expected infix DRC operator'],Old1),
  my_kw(SOP),
  pop_syntax_error(Old1),
  my_right_spaced(SOP), 
  push_syntax_error(['Expected valid DRC formula'],Old2),
  my_drc_formula(PR,T,Vi,Vo1), 
  pop_syntax_error(Old2),
  my_r_drc_formula(PP,P,NT/To,Vo1,Vo).
% my_r_drc_formula(PP,Pi,Ti/To,Vi,Vo) --> % No posfix operators in DRC
%   {my_drc_operator(P,FX,SOP,OP),
%    posfix(P,FX,PL),
%    P=<PP,
%    Pi=<PL,
%    NT=..[OP,Ti]},
%   my_sql_blanks_star, 
%   my_kw(SOP),
%   my_r_drc_formula(PP,P,NT/To,Vi,Vo).
my_r_drc_formula(_,_,Ti/Ti,Vi,Vi) -->
  [].
  
my_drc_operator(1100,xfy,     "OR",'or').
my_drc_operator(1050,xfy,     "->",'->').
my_drc_operator(1050,xfy,"IMPLIES",'->').
my_drc_operator(1000,xfy,   "AND",'and').
my_drc_operator( 900, fy,   "NOT",'not').
my_drc_operator( 900, fy,    "IN",'in').
   
 
% Parenthesed DRC DQL
my_factor_drc_formula(DRCst,Vi,Vo) -->
  my_b_drc_DQL(DRCst,Vi,Vo).
% Range restriction: Membership
my_factor_drc_formula(rel(Relation),Vi,Vo) -->
  my_drc_var_cte_seq(Arguments,Vi,Vo),
  my_sql_blanks,
  push_syntax_error(['Expected IN'],Old1),
  my_kw("IN"),
  pop_syntax_error(Old1),
  my_sql_blanks,
  push_syntax_error(['Expected valid relation name'],Old2),
  my_drc_relation_name(RelationName),
  pop_syntax_error(Old2),
  {length(Arguments,Arity),
   exist_relation(RelationName/Arity),
   Relation=..[RelationName|Arguments]}.
my_factor_drc_formula(not(rel(Relation)),Vi,Vo) -->
  my_drc_var_cte_seq(Arguments,Vi,Vo),
  my_sql_blanks,
  push_syntax_error(['Expected NOT'],Old1),
  my_kw("NOT"),
  pop_syntax_error(Old1),
  my_sql_blanks,
  push_syntax_error(['Expected IN'],Old2),
  my_kw("IN"),
  pop_syntax_error(Old2),
  my_sql_blanks,
  push_syntax_error(['Expected valid relation name'],Old3),
  my_drc_relation_name(RelationName),
  pop_syntax_error(Old3),
  {length(Arguments,Arity),
   exist_relation(RelationName/Arity),
   Relation=..[RelationName|Arguments]}.
% Relation
my_factor_drc_formula(rel(Relation),Vi,Vo) -->
  my_drc_relation_name(RelationName),
  my_sql_blanks_star,
  my_drc_var_cte_tuple(Arguments,Vi,Vo),
  {exist_relation(RelationName),
   Relation=..[RelationName|Arguments]}.
% Propositional relation
my_factor_drc_formula(rel(Relation),V,V) --> 
  my_drc_relation_name(Relation).
% % Negation
% my_factor_drc_formula(not(Formula),Vi,Vo) -->
%   my_kw("NOT"),
%   my_right_spaced("NOT"), 
%   push_syntax_error(['Expected valid DRC formula'],Old),
%   my_drc_arg(Formula,Vi,Vo),
%   pop_syntax_error(Old).
% Condition
my_factor_drc_formula(cond(Condition),Vi,Vo) -->
  my_drc_condition(Condition,Vi,Vo).
% Quantified Formula
my_factor_drc_formula(QuantifiedFormula,Vi,Vo) -->
  my_drc_quantified_formula(QuantifiedFormula,Vi,Vo).

my_drc_arg(B,Vi,Vo) -->
  "(",
  my_sql_blanks_star,
  my_drc_formula(B,Vi,Vo),
  my_sql_blanks_star,
  my_right_parenthesis.
my_drc_arg(B,Vi,Vo) -->
  my_factor_drc_formula(B,Vi,Vo).

my_drc_condition(Condition,Vi,Vo) -->
  my_drc_var_cte(L,Vi,Vi1),
  my_sql_blanks_star,
  push_syntax_error(['Expected valid comparison operator'],Old1),
  my_drc_comparison_operator(Op),
  pop_syntax_error(Old1),
  my_sql_blanks_star,
  push_syntax_error(['Expected a variable or constant'],Old2),
  my_drc_var_cte(R,Vi1,Vo),
  pop_syntax_error(Old2),
  {Condition=..[Op,L,R]}.

my_drc_comparison_operator('=') -->
  "=".
my_drc_comparison_operator('>') -->
  ">".
my_drc_comparison_operator('<') -->
  "<".
my_drc_comparison_operator('<>') -->
  "<>".
my_drc_comparison_operator('!=') -->
  "<>".
my_drc_comparison_operator('>=') -->
  ">=".
my_drc_comparison_operator('=<') -->
  "<=".

my_drc_quantified_formula(QuantifiedFormula,Vi,Vo) -->
  my_drc_quantification(Quantifier,Vars,Vi,Vo1),
  my_sql_blanks_star,
  push_syntax_error(['Expected valid DRC formula'],Old),
  my_drc_formula(Formula,Vo1,Vo),
  pop_syntax_error(Old),
  {QuantifiedFormula=..[Quantifier,Vars,Formula]}.
  
my_drc_quantification(Quantifier,Vars,Vi,Vo) -->
  "(",
  my_sql_blanks_star,
  my_drc_quantification(Quantifier,Vars,Vi,Vo),
  my_sql_blanks_star,
  my_right_parenthesis.
my_drc_quantification(Quantifier,Vars,Vi,Vo) -->
  my_trc_quantifier(Quantifier),
  my_sql_blanks,
  push_syntax_error(['Expected comma-separated variables'],Old),
  my_drc_var_seq(Vars,Vi,Vo),
  pop_syntax_error(Old).
  
my_trc_quantifier(exists) -->
  my_kw("EXISTS").
my_trc_quantifier(forall) -->
  my_kw("FORALL").
  
  
% Bracketed DRC DQL
my_b_drc_DQL(DRCst,Vi,Vo) -->
  "(",
  my_sql_blanks_star,
  my_drc_formula(DRCst,Vi,Vo),
  my_sql_blanks_star,
  my_right_parenthesis.
  
my_drc_var_cte_tuple(Arguments,Vi,Vo) -->
  "(",
  my_sql_blanks_star,
  my_drc_var_cte_seq(Arguments,Vi,Vo),
  my_sql_blanks_star,
  my_right_parenthesis.

my_drc_var_cte_seq([VarCte],Vi,Vo) -->
  push_syntax_error(['Expected a variable or constant'],Old),
  my_drc_var_cte(VarCte,Vi,Vo),
  pop_syntax_error(Old).
my_drc_var_cte_seq([VarCte|VarCtes],Vi,Vo) -->
  my_drc_var_cte(VarCte,Vi,Vo1),
  my_sql_blanks_star,
  ",",
  my_sql_blanks_star,
  my_drc_var_cte_seq(VarCtes,Vo1,Vo).

my_drc_var_cte(Cte,Vi,Vi) -->
  my_sql_constant(cte(Cte,_Type)).
my_drc_var_cte(Var,Vi,Vo) -->
  my_variable(Var,Vi,Vo).  
  
my_drc_var_seq([Var],Vi,Vo) -->
  my_variable(Var,Vi,Vo).
my_drc_var_seq([Var|Vars],Vi,Vo) -->
  my_drc_var_cte(Var,Vi,Vo1),
  my_sql_blanks_star,
  ",",
  my_sql_blanks_star,
  my_drc_var_seq(Vars,Vo1,Vo).


% Guess whether a string starts as a DRC statement  
my_guessed_drc_statement -->
  my_guessed_drc_ddl_statement.
my_guessed_drc_statement -->
  my_guessed_drc_dql_statement.
  
% Guess whether it is a DDL statement
my_guessed_drc_ddl_statement -->
  my_sql_blanks_star,
  my_create_view_schema(_),
  my_sql_blanks_star,
  ":=",
  my_sql_blanks_star,
  my_guessed_drc_dql_statement.
  
% Guess whether it is a DQL statement
my_guessed_drc_dql_statement -->
  my_sql_blanks_star,
  "{".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAFETY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
is_drc_safe(drc_query(PArgs,NVs,Formula)) :-
  drc_valid_target_list(PArgs,NVs),
  is_drc_formula_safe(Formula,NVs,[],FVars,[],QVars,[]),
  my_set_diff(FVars,QVars,UQVars),
  term_variables(PArgs,PVars),
  drc_safe_unquantified_free_vars(UQVars,PVars,NVs),
  drc_safe_unsafe_project_vars(PVars,FVars,QVars,NVs).
  
is_drc_formula_safe(rel(Relation),NVs,IVars,OVars,QVars,QVars,CQVars) :-
  term_variables(Relation,RVars),
  my_set_diff(RVars,CQVars,DVars),
  drc_safe_quantified_outside(DVars,QVars,NVs),
  append(RVars,IVars,OVars).
is_drc_formula_safe(and(L,R),NVs,IVars,OVars,IQVars,OQVars,CQVars) :-
  is_drc_formula_safe(L,NVs,IVars,I1Vars,IQVars,I1QVars,CQVars),
  is_drc_formula_safe(R,NVs,I1Vars,OVars,I1QVars,OQVars,CQVars).
is_drc_formula_safe(or(L,R),NVs,IVars,OVars,IQVars,OQVars,CQVars) :-
  is_drc_formula_safe(L,NVs,IVars,I1Vars,IQVars,I1QVars,CQVars),
  is_drc_formula_safe(R,NVs,I1Vars,OVars,I1QVars,OQVars,CQVars).
is_drc_formula_safe((L->R),NVs,IVars,OVars,IQVars,OQVars,CQVars) :-
  is_drc_formula_safe(L,NVs,IVars,I1Vars,IQVars,I1QVars,CQVars),
  is_drc_formula_safe(R,NVs,I1Vars,OVars,I1QVars,OQVars,CQVars).
is_drc_formula_safe(not(F),NVs,IVars,OVars,IQVars,OQVars,CQVars) :-
  is_drc_formula_safe(F,NVs,IVars,OVars,IQVars,OQVars,CQVars).
is_drc_formula_safe(cond(Condition),_NVs,IVars,OVars,QVars,QVars,_CQVars) :-
  term_variables(Condition,CVars),
  append(CVars,IVars,OVars).
is_drc_formula_safe(exists(EVars,F),NVs,IVars,OVars,IQVars,OQVars,CQVars) :-
  drc_safe_requantified(EVars,IQVars,NVs),
  drc_safe_quantified(EVars,IVars,NVs),
  append(IQVars,EVars,I1QVars),
  append(EVars,CQVars,NCQVars),
  is_drc_formula_safe(F,NVs,IVars,OVars,I1QVars,OQVars,NCQVars),
  drc_safe_quantified_unused(OQVars,OVars,NVs).
is_drc_formula_safe(forall(Vars,F),NVs,IVars,OVars,IQVars,OQVars,CQVars) :-
  is_drc_formula_safe(exists(Vars,F),NVs,IVars,OVars,IQVars,OQVars,CQVars).
  
drc_valid_target_list(Args,NVs) :-
  extract_duplicates_var(Args,Vs),
  (Vs==[]
   ->
    true
   ; 
    (Vs=[_] -> Nbr=' ' ; Nbr='s '),
    my_raise_exception(generic,syntax(['(DRC) Variable',Nbr,'$exec'(write_with_NVs(Vs,NVs)),' must not occur duplicated in the target list.']),[])).

drc_safe_quantified_outside(Vars,QVars,NVs) :-
  my_set_inter(Vars,QVars,RVars),
  my_reverse(RVars,DVars),
  remove_duplicates_var(DVars,IVars),
  (IVars==[]
   ->
    true
   ;
    (IVars=[_] -> Nbr=' ',Pr='its' ; Nbr='s ',Pr='their'),
    my_raise_exception(generic,syntax(['(DRC) Quantified variable',Nbr,'$exec'(write_with_NVs(IVars,NVs)),' cannot occur outside ',Pr,' scope.']),[])).
    
drc_safe_quantified(QVars,Vars,NVs) :-
  my_set_inter(QVars,Vars,IVars),
%   my_reverse(RVars,DVars),
%   remove_duplicates_var(DVars,IVars),
  (IVars==[]
   ->
    true
   ;
    (IVars=[_] -> Nbr=' ',Pr='its' ; Nbr='s ',Pr='their'),
    my_raise_exception(generic,syntax(['(DRC) Quantified variable',Nbr,'$exec'(write_with_NVs(IVars,NVs)),' cannot occur outside ',Pr,' scope.']),[])).

% A variable can be quantified only once and in only one quantifier 
drc_safe_requantified(QVars,OQVars,NVs) :-
  extract_duplicates_var(QVars,DQVars), % Only one occurrence in the quantifier's variable list
  (DQVars==[]
   ->
    true
   ;
    (DQVars=[_] -> Nbr=' '; Nbr='s '),
    my_raise_exception(generic,syntax(['(DRC) Quantified variable',Nbr,'$exec'(write_with_NVs(DQVars,NVs)),' can occur only once in the quantifier''s variable list.']),[])
  ),
  my_set_inter(QVars,OQVars,IVars),   % No occurrences in other quantifier
  (IVars==[]
   ->
    true
   ;
  (IVars=[_] -> Nbr=' '; Nbr='s '),
  my_raise_exception(generic,syntax(['(DRC) Quantified variable',Nbr,'$exec'(write_with_NVs(IVars,NVs)),' cannot occur in another quantifier.']),[])).

drc_safe_quantified_unused(QVars,PVars,NVs) :-
  my_set_diff(QVars,PVars,RVars),
  my_reverse(RVars,DVars),
  remove_duplicates_var(DVars,IVars),
  (IVars==[]
   ->
    true
   ;
    (IVars=[_] -> Nbr=' ',Pr='is' ; Nbr='s ',Pr='are'),
    my_raise_exception(generic,syntax(['(DRC) Quantified variable',Nbr,'$exec'(write_with_NVs(IVars,NVs)),' ',Pr,' not used.']),[])).

drc_safe_unquantified_free_vars(FVars,PVars,NVs) :-
  my_set_diff(FVars,PVars,ANQVars),
  remove_anonymous_vars(ANQVars,NVs,RVars),
  my_reverse(RVars,DVars),
  remove_duplicates_var(DVars,NQVars),
  (NQVars==[]
   ->
    true
   ;
    (NQVars=[_] -> Nbr=' ',Verb='is'; Nbr='s ',Verb='are'),
    my_raise_exception(generic,syntax(['(DRC) Variable',Nbr,'$exec'(write_with_NVs(NQVars,NVs)),' ',Verb,' not quantified and ',Verb,' not in the output list.']),[])).

drc_safe_unsafe_project_vars(PVars,FVars,QVars,NVs) :-
  my_set_diff(PVars,FVars,DUVars),
  remove_duplicates_var(DUVars,UVars),
  (UVars==[]
   ->
    true
   ;
    (UVars=[_] -> Nbr=' '; Nbr='s '),
    my_raise_exception(generic,syntax(['(DRC) Variable',Nbr,'$exec'(write_with_NVs(UVars,NVs)),' must be bound by a relation.']),[])),
  my_set_inter(PVars,QVars,PQVars),
  (PQVars==[]
   ->
    true
   ;
    (PQVars=[_] -> Nbr=' '; Nbr='s '),
    my_raise_exception(generic,syntax(['(DRC) Quantified variable',Nbr,'$exec'(write_with_NVs(PQVars,NVs)),' cannot be in the output list.']),[])).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMPILING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

drc_to_dl(drc_query(Arguments,NVs,Formula),(Head:-Body),RNVs) :-
  Head=..[answer|Arguments],
  drc_formula_to_cdrc(Formula,CFormula),
  cdrc_formula_to_dl(CFormula,Body,NVs,RNVs).
  
% DRC Formula to Canonical DRC Formula
drc_formula_to_cdrc(rel(Relation),rel(Relation)).  
drc_formula_to_cdrc(and(F1,F2),and(CF1,CF2)) :-
  drc_formula_to_cdrc(F1,CF1),
  drc_formula_to_cdrc(F2,CF2).
% drc_formula_to_cdrc(or(F1,F2),or(F1,F2)) :-
%   rel_or_cond(F1),
%   rel_or_cond(F2),
%   !.
% drc_formula_to_cdrc(or(F1,F2),CF) :-
%   drc_negate_formula(F1,NF1),
%   drc_negate_formula(F2,NF2),
%   drc_formula_to_cdrc(not(and(NF1,NF2)),CF).
drc_formula_to_cdrc(or(F1,F2),or(CF1,CF2)) :-
  drc_formula_to_cdrc(F1,CF1),
  drc_formula_to_cdrc(F2,CF2).
drc_formula_to_cdrc((F1->F2),Formula) :-
  drc_negate_formula(F1,NF1),
  drc_formula_to_cdrc(or(NF1,F2),Formula).
drc_formula_to_cdrc(not(F),CF) :-
  drc_negate_formula(F,CF).
drc_formula_to_cdrc(cond(C),cond(C)).
drc_formula_to_cdrc(exists(Vs,F),exists(Vs,CF)) :-
  drc_formula_to_cdrc(F,CF).
drc_formula_to_cdrc(forall(Vs,F),not(exists(Vs,CF))) :-
  drc_negate_formula(F,NF),
  drc_formula_to_cdrc(NF,CF).
  
rel_or_cond(rel(_)).
rel_or_cond(cond(_)).

drc_negate_formula(rel(F),not(rel(F))).
% drc_negate_formula(and(F1,F2),or(NF1,NF2)) :-
%   drc_negate_formula(F1,NF1),
%   drc_negate_formula(F2,NF2).
drc_negate_formula(and(F1,F2),not(and(CF1,CF2))) :-
  drc_formula_to_cdrc(F1,CF1),
  drc_formula_to_cdrc(F2,CF2).
drc_negate_formula(or(F1,F2),and(NF1,NF2)) :-
  drc_negate_formula(F1,NF1),
  drc_negate_formula(F2,NF2).
drc_negate_formula((F1->F2),and(F1,NF2)) :-
  drc_negate_formula(F2,NF2).
drc_negate_formula(not(F),CF) :-
  drc_formula_to_cdrc(F,CF).
drc_negate_formula(cond(C),cond(NC)) :-
  drc_negate_cond(C,NC).
drc_negate_formula(exists(Vs,F),not(exists(Vs,CF))) :-
  drc_formula_to_cdrc(F,CF).
drc_negate_formula(forall(Vs,F),exists(Vs,NF)) :-
  drc_negate_formula(F,NF).
  
:- op(400,xfy,[<>]).
drc_negate_cond((A=B),(A<>B)).
drc_negate_cond((A<>B),(A=B)).
drc_negate_cond((A>B),(A=<B)).
drc_negate_cond((A<B),(A>=B)).
drc_negate_cond((A>=B),(A<B)).
drc_negate_cond((A=<B),(A>B)).

% Canonical DRC Formula to Datalog Rule
cdrc_formula_to_dl(rel(Relation),Relation,NVs,NVs).  
cdrc_formula_to_dl(and(LFor,RFor),(LB,RB),INVs,ONVs) :-
  cdrc_formula_to_dl(LFor,LB,INVs,I1NVs),
  cdrc_formula_to_dl(RFor,RB,I1NVs,ONVs).
cdrc_formula_to_dl(or(LFor,RFor),(LB;RB),INVs,ONVs) :-
  cdrc_formula_to_dl(LFor,LB,INVs,I1NVs),
  cdrc_formula_to_dl(RFor,RB,I1NVs,ONVs).
cdrc_formula_to_dl((LFor -> RFor),Body,INVs,ONVs) :-
  cdrc_formula_to_dl(or(not(LFor),RFor),Body,INVs,ONVs).
cdrc_formula_to_dl(not(Formula),not Body,INVs,ONVs) :-
  cdrc_formula_to_dl(Formula,Body,INVs,ONVs).
cdrc_formula_to_dl(cond(DRCCondition),DLCondition,NVs,NVs) :-
  replace_functor('<>','\\=',DRCCondition,DLCondition).
cdrc_formula_to_dl(exists(Vars1,exists(Vars2,Formula)),Body,INVs,ONVs) :- % This clause is not needed, but eases further processing
  !,
  append(Vars1,Vars2,Vars),
  cdrc_formula_to_dl(exists(Vars,Formula),Body,INVs,ONVs).
cdrc_formula_to_dl(exists(Vars,Formula),exists(Vars,Body),INVs,ONVs) :-
  cdrc_formula_to_dl(Formula,Body,INVs,ONVs).
% cdrc has no forall:
% drc_formula_to_dl(forall(Vars1,forall(Vars2,Formula)),Body,INVs,ONVs) :- % This clause is not needed, but eases further processing
%   !,
%   append(Vars1,Vars2,Vars),
%   drc_formula_to_dl(forall(Vars,Formula),Body,INVs,ONVs).
% drc_formula_to_dl(forall(Vars,Formula),Body,INVs,ONVs) :-
%   drc_negate_formula(Formula,NegatedFormula),
%   drc_formula_to_dl(not(exists(Vars,NegatedFormula)),Body,INVs,ONVs).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOLVING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DDL Assignment: Formula to Relation
solve_drc_query(assign(drc_query(Vars,NVs,Formula),Schema)) :-
  !,
  functor(Schema,TableName,_),
  drop_view_k_if_exists(TableName),
  drc_schema(Vars,NVs,Schema,CompletedSchema),
  push_flag(safe,on,OldFlag),
  create_view_k(drc,(drc_query(Vars,NVs,Formula),_AS),CompletedSchema,_LVDs),
  pop_flag(safe,OldFlag),
  clear_et, 
  compute_stratification.
% DDL Assignment: Relation to Relation
solve_drc_query(assign(RAst,Schema)) :- % Reuse RA assignment
  solve_ra_query(assign(RAst,Schema)).
% DQL Query
solve_drc_query(Query) :-
  Query=drc_query(Vars,QNVs,_Formula),
  drc_to_dl(Query,Rule,NVs),
  push_flag(safe,on,OldFlag),
  save_et_st(ET,S),
  (solve_des_sql_query_k2(drc,[Rule],NVs,ColTypes,DLQuery,_DVs,CRNVss,Undefined,OrderBy)
   ->
    store_elapsed_time(computation),
    display_compiled_drc([(Rule,NVs)],CRNVss),
    drc_schema(Vars,QNVs,answer,Schema),
    Schema=..[_|ColnameTypes],
    internal_typename_to_user_typename_list(ColTypes,UserColTypes),
    my_zipWith(':',_Colnames,UserColTypes,ColnameTypes),
    display_rdb_answer_schema(Schema),
    display_solutions(DLQuery,Undefined,OrderBy),
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
  ),
  pop_flag(safe,OldFlag).

% If column names are not given, they receive the downcased variable names (as in WinRDBI)
drc_schema(_VarsCtes,_NVs,Schema,Schema) :- % With given column names
  \+ atom(Schema),
  !.
drc_schema(VarsCtes,NVs,RelationName,Schema) :-
  copy_term([VarsCtes,NVs],[CVarsCtes,CNVs]),
  call_nf_list(CNVs),
  drc_column_names(CVarsCtes,VarsCtes,ColumnNames),
  length(ColumnNames,L),
  length(Types,L),
  my_zipWith(':',ColumnNames,Types,ColumnTypes),
  Schema=..[RelationName|ColumnTypes].

drc_column_names([],[],[]).
drc_column_names([CVarCte|CVarsCtes],[VarCte|VarsCtes],[ColumnName|ColumnNames]) :-
  drc_column_name(CVarCte,VarCte,ColumnName),
  drc_column_names(CVarsCtes,VarsCtes,ColumnNames).
  
drc_column_name(CVarCte,VarCte,ColumnName) :-
  var(VarCte),
  !,
  first_char_to_lowercase(CVarCte,ColumnName).
%  to_lowercase(CVarCte,ColumnName).
drc_column_name(_CVarCte,_VarCte,ColumnName) :-
  argument_autorenaming(ColumnName).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DISPLAYING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
display_drc(drc_query(Vars,NVs,DRCFormula),I) :-
  push_flag(language,drc,L),
  (pretty_print(off) -> write_indent(I) ; true),
  pp_indent(I),
  write_log('{ '),
  write_csa_with_NVs(Vars,NVs),
  write_log(' |'),
  pp_nl_or_blank,
  I1 is I+2,
  pp_indent(I1),
  write_drc(DRCFormula,NVs,I1),
  write_log(' };'),
  pop_flag(language,L),
  nl_log.

write_drc(rel(Relation),NVs,_I) :-
  write_with_NVs(Relation,NVs).
write_drc(and(LFor,RFor),NVs,I) :-
  write_log('('),
  write_drc(LFor,NVs,I),
  write_log(' and '),
  write_drc(RFor,NVs,I),
  write_log(')').
write_drc(or(LFor,RFor),NVs,I) :-
  write_log('('),
  write_drc(LFor,NVs,I),
  write_log(' or '),
  write_drc(RFor,NVs,I),
  write_log(')').
write_drc((LFor->RFor),NVs,I) :-
  write_log('('),
  write_drc(LFor,NVs,I),
  write_log(' -> '),
  write_drc(RFor,NVs,I),
  write_log(')').
write_drc(not(For),NVs,I) :-
  write_log('not'),
  write_log('('),
  write_drc(For,NVs,I),
  write_log(')').
write_drc(cond(Condition),NVs,_I) :-
  write_drc_condition(Condition,NVs).
write_drc(exists(Vars,Formula),NVs,I) :-
  write_log('(exists '),
  write_csa_with_NVs(Vars,NVs),
  write_log(') '),
  write_drc(Formula,NVs,I).
write_drc(forall(Vars,Formula),NVs,I) :-
  write_log('(forall '),
  write_csa_with_NVs(Vars,NVs),
  write_log(') '),
  write_drc(Formula,NVs,I).
  
write_drc_condition(Condition,NVs) :-
  Condition=..[Op,L,R],
  write_drc_argument(L,NVs),
  write_log_list([' ',Op,' ']),
  write_drc_argument(R,NVs).
  
write_drc_argument(A,_NVs) :-
  number(A),
  !,
  write_log(A).
write_drc_argument(A,_NVs) :-
  atom(A),
  !,
  write_log_list(['''',A,'''']).
write_drc_argument(A,NVs) :-
  write_with_NVs(A,NVs).

