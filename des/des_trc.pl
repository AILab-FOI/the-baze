/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.4.1                 */
/*                                                       */
/*    Tuple Relational Calculus Processor                */
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

% Tuple Relational Calculus Grammar. 
% Builds on [Diet01]  Understanding Relational Database Query Languages, S. W. Dietrich, Prentice Hall, 2001.
% Adds:
% - Implication
% - Complete type checking (e.g., a numeric constant cannot be compared with a string field)
% - A comparison can occur before its data source
% - Syntax sugaring allowing constants in the result tuple
% - Avoid duplicate column names in schema-less relation definitions 
% - Better safety messages
% - Tuples can be built with constants, in addition to tuple variables and attributes
% - Support for propositional relations
% - Parentheses surrounding quantified formulas are not required 
% - Parentheses surrounding quantifiers are not required 
% - Variables starting with an underscore are existentially quantified by default (the same as for anonymous variables)
% Fixes:
% - Invalid uses of variables in the scope of a universal quantifier

% % Tuple Relational Calculus statement:
% TRCstmt ::= 
%   { VarsAttsCtes | Formula }
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
%   Relation (Var)
%   |
%   Var [NOT] IN Relation 
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
%   AttCte RelationalOp AttCte
%
% AttCte ::=
%   Variable.Attribute
%   |
%   Constant
%
% VarAttCte ::=
%   Variable
%   |
%   AttCte
%
% VarsAttsCtes ::=
%   VarAttCte
%   |
%   VarAttCte, VarsAttsCtes
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
% TRCview ::=
%   [Schema | ViewName] := [TRCstmt | ViewName]

% Schema ::=
%   ViewName
%   |
%   ViewName(ColName,...,ColName)


% Parsing TRC statements
parse_trc_query(TRCst) -->
  {my_retract_all_facts(dictionary(_)),
   assertz(dictionary([]))}, % WARNING: ONLY FOR TEST CASE GENERATION
  my_sql_blanks_star, 
  my_trc(TRCst,[],_Vo),
  my_sql_blanks_star, 
  my_optional(";").

% TRC Statement
%   DDL Statement
my_trc(TRCst,Vi,Vo) -->
  push_syntax_error(['Expected relation definition (view name or schema followed by '':='')'],Old),
  my_trc_DDL(TRCst,Vi,Vo),
  pop_syntax_error(Old).
%   DQL Statement
my_trc(TRCst,Vi,Vo) -->
  my_trc_DQL(TRCst,Vi,Vo).

% TRC DDL Statement
% Relation copying:
my_trc_DDL(assign(select(true,Relation),Schema),Vi,Vi) --> % Reuse RA assignment
  my_create_view_schema(Schema),
  my_sql_blanks_star, 
  is_valid_trc_schema(Schema),
  push_syntax_error(['Expected '':='''],Old1),
  ":=",
  pop_syntax_error(Old1),
  my_sql_blanks_star, 
  push_syntax_error(['Expected a relation name'],Old2),
  my_trc_relation_name(Relation),
  pop_syntax_error(Old2).
% New Relation from its TRC definition:
my_trc_DDL(assign(TRCst,Schema),Vi,Vo) -->
  my_create_view_schema(Schema),
  my_sql_blanks_star, 
  is_valid_trc_schema(Schema),
  push_syntax_error(['Expected '':='''],Old),
  ":=",
  pop_syntax_error(Old),
  my_sql_blanks_star, 
  my_trc_DQL(TRCst,Vi,Vo).

my_trc_relation_name(RelationName) -->
  my_symbol(RelationName),
  !,
  is_valid_trc_relation_name(RelationName).

is_valid_trc_schema(Schema) -->
  {functor(Schema,Name,_A)},
  is_valid_trc_relation_name(Name).
  
is_valid_trc_relation_name(Name) -->
  {to_lowercase(Name,DName),
   trc_keyword(DName)},
  push_syntax_error(['Keywords are not allowed as relation names'],_Old),
  {!,
   fail}.
is_valid_trc_relation_name(_Name) -->
  [].
  

trc_keyword(and).
trc_keyword(or).
trc_keyword(not).
trc_keyword(in).
trc_keyword(implies).
trc_keyword(exists).
trc_keyword(forall).

% TRC DQL Statement
my_trc_DQL(trc_query(VarsCtes,Vo,TRCst),Vi,Vo) -->
  push_syntax_error(['Expected start of TRC statement (left curly bracket: ''{'')'],Old1),
  "{",
  pop_syntax_error(Old1),
  my_sql_blanks_star, 
  push_syntax_error(['Expected comma-separated tuple variables and/or constants.'],Old2),
  my_trc_var_col_cte_seq(VarsCtes,Vi,Vi1),
  pop_syntax_error(Old2),
  my_sql_blanks_star, 
  push_syntax_error(['Expected vertical bar (|)'],Old3),
  "|",
  pop_syntax_error(Old3),
  push_syntax_error(['Expected valid TRC formula'],Old4),
  my_sql_blanks_star, 
  my_trc_formula(TRCst,Vi1,Vo),
  pop_syntax_error(Old4),
  my_sql_blanks_star, 
  push_syntax_error(['Expected end of TRC statement (right curly bracket: ''}'')'],Old5),
  "}",
  pop_syntax_error(Old5),
  {is_trc_safe(trc_query(VarsCtes,Vo,TRCst))}.
  
my_trc_formula(F,Vi,Vo) -->
  my_trc_formula(1200,F,Vi,Vo).
  
my_trc_formula(PP,To,Vi,Vo) -->
  my_factor_trc_formula(L,Vi,Vo1), 
  my_r_trc_formula(PP,0,L/To,Vo1,Vo).
my_trc_formula(PP,To,Vi,Vo) -->
  "(", 
  my_sql_blanks_star, 
  push_syntax_error(['Expected valid TRC formula'],Old),
  my_trc_formula(1200,T,Vi,Vo1), 
  pop_syntax_error(Old),
  my_sql_blanks_star, 
  my_right_parenthesis,
  !,
  my_r_trc_formula(PP,0,T/To,Vo1,Vo).
my_trc_formula(PP,To,Vi,Vo) -->
  {my_trc_operator(P,FX,SOP,OP),
   prefix(P,FX,PR),
   P=<PP},
  my_kw(SOP),
  push_syntax_error(['Expected valid TRC formula'],Old),
  my_right_spaced(SOP),
  my_trc_formula(PR,T,Vi,Vo1), 
  pop_syntax_error(Old),
  {NT=..[OP,T]},
  my_r_trc_formula(PP,P,NT/To,Vo1,Vo).
  
% infix operator
%   TRCformula op TRCformula 
%   op ::= and | or | implies | -> 
my_r_trc_formula(PP,Pi,Ti/To,Vi,Vo) -->
  {my_trc_operator(P,YFX,SOP,OP),
   infix(P,YFX,PL,PR),
   P=<PP,
   Pi=<PL,
   NT=..[OP,Ti,T]},
  push_syntax_error(['Expected infix TRC operator'],Old1),
  my_left_spaced(SOP), 
  my_kw(SOP),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected valid TRC formula'],Old2),
  my_right_spaced(SOP), 
  my_trc_formula(PR,T,Vi,Vo1), 
  pop_syntax_error(Old2),
  my_r_trc_formula(PP,P,NT/To,Vo1,Vo).
% my_r_trc_formula(PP,Pi,Ti/To,Vi,Vo) --> % No posfix operators in TRC
%   {my_trc_operator(P,FX,SOP,OP),
%    posfix(P,FX,PL),
%    P=<PP,
%    Pi=<PL,
%    NT=..[OP,Ti]},
%   my_sql_blanks_star, 
%   my_kw(SOP),
%   my_r_trc_formula(PP,P,NT/To,Vi,Vo).
my_r_trc_formula(_,_,Ti/Ti,Vi,Vi) -->
  [].
  
my_trc_operator(1100,xfy,     "OR",'or').
my_trc_operator(1050,xfy,     "->",'->').
my_trc_operator(1050,xfy,"IMPLIES",'->').
my_trc_operator(1000,xfy,   "AND",'and').
my_trc_operator( 900, fy,   "NOT",'not').
my_trc_operator( 900, fy,    "IN",'in').

  
% Parenthesed TRC DQL
my_factor_trc_formula(TRCst,Vi,Vo) -->
  my_b_trc_DQL(TRCst,Vi,Vo).
% Range restriction: Membership
my_factor_trc_formula(rel(RelationName,Variable),Vi,Vo) -->
  my_variable(Variable,Vi,Vo),
  my_sql_blanks,
  push_syntax_error(['Expected IN'],Old1),
  my_kw("IN"),
  pop_syntax_error(Old1),
  my_sql_blanks,
  push_syntax_error(['Expected valid relation name'],Old3),
  my_trc_relation_name(RelationName),
  pop_syntax_error(Old3),
  {exist_relation(RelationName)}.
my_factor_trc_formula(not(rel(RelationName,Variable)),Vi,Vo) -->
  my_variable(Variable,Vi,Vo),
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
  my_trc_relation_name(RelationName),
  pop_syntax_error(Old3),
  {exist_relation(RelationName)}.
% Relation
my_factor_trc_formula(rel(RelationName,Variable),Vi,Vo) -->
  my_trc_relation_name(RelationName),
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  push_syntax_error(['Expected a tuple variable'],Old1),
  my_variable(Variable,Vi,Vo),
  pop_syntax_error(Old1),
  my_sql_blanks_star,
  push_syntax_error(['Expected a right parenthesis'],Old2),
  my_right_parenthesis,
  pop_syntax_error(Old2),
  {exist_relation(RelationName)}.
% Propositional relation
my_factor_trc_formula(rel(RelationName),V,V) --> 
  my_trc_relation_name(RelationName).
% % Negation
% my_factor_trc_formula(not(Formula),Vi,Vo) -->
%   my_kw("NOT"),
%   my_right_spaced("NOT"), 
%   push_syntax_error(['Expected valid TRC formula'],Old),
%   my_trc_arg(Formula,Vi,Vo),
%   pop_syntax_error(Old).
% Condition
my_factor_trc_formula(cond(Condition),Vi,Vo) -->
  my_trc_condition(Condition,Vi,Vo).
% Quantified Formula
my_factor_trc_formula(QuantifiedFormula,Vi,Vo) -->
  my_trc_quantified_formula(QuantifiedFormula,Vi,Vo).

my_trc_arg(B,Vi,Vo) -->
  "(",
  my_sql_blanks_star,
  my_trc_formula(B,Vi,Vo),
  my_sql_blanks_star,
  my_right_parenthesis.
my_trc_arg(B,Vi,Vo) -->
  my_factor_trc_formula(B,Vi,Vo).

my_trc_condition(Condition,Vi,Vo) -->
  my_trc_var_col_cte(L,Vi,Vi1),
  my_sql_blanks_star,
  push_syntax_error(['Expected valid comparison operator'],Old1),
  my_trc_comparison_operator(Op),
  pop_syntax_error(Old1),
  my_sql_blanks_star,
  push_syntax_error(['Expected a variable, attribute or constant'],Old2),
  my_trc_var_col_cte(R,Vi1,Vo),
  pop_syntax_error(Old2),
  {Condition=..[Op,L,R]}.

my_trc_comparison_operator('=') -->
  "=".
my_trc_comparison_operator('>') -->
  ">".
my_trc_comparison_operator('<') -->
  "<".
my_trc_comparison_operator('<>') -->
  "<>".
my_trc_comparison_operator('<>') -->
  "!=".
my_trc_comparison_operator('>=') -->
  ">=".
my_trc_comparison_operator('=<') -->
  "<=".

my_trc_quantified_formula(QuantifiedFormula,Vi,Vo) -->
  my_trc_quantification(Quantifier,Vars,Vi,Vo1),
  my_sql_blanks_star,
  push_syntax_error(['Expected valid TRC formula'],Old),
  my_trc_formula(Formula,Vo1,Vo),
  pop_syntax_error(Old),
  {QuantifiedFormula=..[Quantifier,Vars,Formula]}.
  
my_trc_quantification(Quantifier,Vars,Vi,Vo) -->
  "(",
  my_sql_blanks_star,
  my_trc_quantification(Quantifier,Vars,Vi,Vo),
  my_sql_blanks_star,
  my_right_parenthesis.
my_trc_quantification(Quantifier,Vars,Vi,Vo) -->
  my_drc_quantifier(Quantifier),
  my_sql_blanks,
  push_syntax_error(['Expected comma-separated variables and/or attributes'],Old),
  my_trc_var_attr_seq(Vars,Vi,Vo),
  pop_syntax_error(Old).
  
my_drc_quantifier(exists) -->
  my_kw("EXISTS").
my_drc_quantifier(forall) -->
  my_kw("FORALL").
  
  
% Bracketed TRC DQL
my_b_trc_DQL(TRCst,Vi,Vo) -->
  "(",
  my_sql_blanks_star,
  my_trc_formula(TRCst,Vi,Vo),
  my_sql_blanks_star,
  my_right_parenthesis.
  
my_trc_var_col_cte_tuple(Arguments,Vi,Vo) -->
  "(",
  my_sql_blanks_star,
  my_trc_var_col_cte_seq(Arguments,Vi,Vo),
  my_sql_blanks_star,
  my_right_parenthesis.

my_trc_var_col_cte_seq([VarCte],Vi,Vo) -->
  push_syntax_error(['Expected a tuple variable, column or constant'],Old),
  my_trc_var_col_cte(VarCte,Vi,Vo),
  pop_syntax_error(Old).
my_trc_var_col_cte_seq([VarCte|VarCtes],Vi,Vo) -->
  my_trc_var_col_cte(VarCte,Vi,Vo1),
  my_sql_blanks_star,
  ",",
  my_sql_blanks_star,
  my_trc_var_col_cte_seq(VarCtes,Vo1,Vo).

my_trc_var_col_cte(Cte,Vi,Vi) -->
  my_sql_constant(cte(Cte,_Type)).
my_trc_var_col_cte(X,Vi,Vo) -->
  my_trc_var_attr(X,Vi,Vo).  
  
my_trc_var_attr(X,Vi,Vo) -->
  my_trc_attr(X,Vi,Vo).
my_trc_var_attr(X,Vi,Vo) -->
  my_variable(X,Vi,Vo).
  
my_trc_attr(attr(RelVar,Attr),Vi,Vo) -->
  my_variable(RelVar,Vi,Vo),
  ".",
  my_colname(Attr).  
  
my_trc_var_attr_seq([Var],Vi,Vo) -->
  my_variable(Var,Vi,Vo).
my_trc_var_attr_seq([Var|Vars],Vi,Vo) -->
  my_trc_var_attr(Var,Vi,Vo1),
  my_sql_blanks_star,
  ",",
  my_sql_blanks_star,
  my_trc_var_attr_seq(Vars,Vo1,Vo).

% Guess whether a string starts as a TRC statement  
my_guessed_trc_statement -->
  my_guessed_trc_ddl_statement.
my_guessed_trc_statement -->
  my_guessed_trc_dql_statement.
  
% Guess whether it is a DDL statement
my_guessed_trc_ddl_statement -->
  my_create_view_schema(_),
  my_sql_blanks_star,
  ":=",
  my_sql_blanks_star,
  my_guessed_trc_dql_statement.
  
% Guess whether it is a DQL statement
my_guessed_trc_dql_statement -->
  "{".
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAFETY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
is_trc_safe(trc_query(PArgs,NVs,Formula)) :-
  trc_valid_target_list(PArgs,NVs),
  term_variables(PArgs,PVars),
%  is_trc_formula_safe(Formula,NVs,PVars,FVars,[],QVars),
  is_trc_formula_safe(Formula,NVs,[],FVars,[],QVars,[]),
  my_set_diff(FVars,QVars,UQVars),
  trc_safe_unquantified_free_vars(UQVars,PVars,NVs),
  trc_safe_unsafe_project_vars(PVars,FVars,QVars,NVs).
  
is_trc_formula_safe(rel(_Relation),_NVs,Vars,Vars,QVars,QVars,_CQVars). % Propositional
is_trc_formula_safe(rel(_Relation,RelVar),NVs,IVars,[RelVar|IVars],QVars,QVars,CQVars) :-
  my_set_diff([RelVar],CQVars,DVars),
  trc_safe_quantified_outside(DVars,QVars,NVs).
is_trc_formula_safe(and(L,R),NVs,IVars,OVars,IQVars,OQVars,CQVars) :-
  is_trc_formula_safe(L,NVs,IVars,I1Vars,IQVars,I1QVars,CQVars),
  is_trc_formula_safe(R,NVs,I1Vars,OVars,I1QVars,OQVars,CQVars).
is_trc_formula_safe(or(L,R),NVs,IVars,OVars,IQVars,OQVars,CQVars) :-
  is_trc_formula_safe(L,NVs,IVars,I1Vars,IQVars,I1QVars,CQVars),
  is_trc_formula_safe(R,NVs,I1Vars,OVars,I1QVars,OQVars,CQVars).
is_trc_formula_safe((L->R),NVs,IVars,OVars,IQVars,OQVars,CQVars) :-
  is_trc_formula_safe(L,NVs,IVars,I1Vars,IQVars,I1QVars,CQVars),
  is_trc_formula_safe(R,NVs,I1Vars,OVars,I1QVars,OQVars,CQVars).
is_trc_formula_safe(not(F),NVs,IVars,OVars,IQVars,OQVars,CQVars) :-
  is_trc_formula_safe(F,NVs,IVars,OVars,IQVars,OQVars,CQVars).
is_trc_formula_safe(cond(Condition),_NVs,IVars,OVars,QVars,QVars,_CQVars) :-
  term_variables(Condition,CVars),
  append(CVars,IVars,OVars).
is_trc_formula_safe(exists(EVars,F),NVs,IVars,OVars,IQVars,OQVars,CQVars) :-
  trc_safe_requantified(EVars,IQVars,NVs),
  trc_safe_quantified(EVars,IVars,NVs),
  append(IQVars,EVars,I1QVars),
  append(EVars,CQVars,NCQVars),
  is_trc_formula_safe(F,NVs,IVars,OVars,I1QVars,OQVars,NCQVars),
  trc_safe_quantified_unused(OQVars,OVars,NVs).
is_trc_formula_safe(forall(Vars,F),NVs,IVars,OVars,IQVars,OQVars,CQVars) :-
  is_trc_formula_safe(exists(Vars,F),NVs,IVars,OVars,IQVars,OQVars,CQVars).
  
trc_valid_target_list(Args,NVs) :-
  extract_duplicates_var(Args,Vs),
  (Vs==[]
   ->
    true
   ; 
    (Vs=[_] -> Nbr=' ' ; Nbr='s '),
    my_raise_exception(generic,syntax(['(TRC) Variable',Nbr,'$exec'(write_with_NVs(Vs,NVs)),' must not occur duplicated in the target list.']),[])).
  
trc_safe_quantified(QVars,Vars,NVs) :-
  my_set_inter(QVars,Vars,IVars),
%   my_reverse(RVars,DVars),
%   remove_duplicates_var(DVars,IVars),
  (IVars==[]
   ->
    true
   ;
    (IVars=[_] -> Nbr=' ',Pr='its' ; Nbr='s ',Pr='their'),
    my_raise_exception(generic,syntax(['(TRC) Quantified variable',Nbr,'$exec'(write_with_NVs(IVars,NVs)),' cannot be used outside ',Pr,' scope.']),[])).
    
trc_safe_quantified_outside(Vars,QVars,NVs) :-
  my_set_inter(Vars,QVars,IVars),
  (IVars==[]
   ->
    true
   ;
    (IVars=[_] -> Nbr=' ',Pr='its' ; Nbr='s ',Pr='their'),
    my_raise_exception(generic,syntax(['(TRC) Quantified variable',Nbr,'$exec'(write_with_NVs(IVars,NVs)),' cannot occur outside ',Pr,' scope.']),[])).

% A variable can be quantified only once and in only one quantifier 
trc_safe_requantified(QVars,OQVars,NVs) :-
  extract_duplicates_var(QVars,DQVars), % Only one occurrence in the quantifier's variable list
  (DQVars==[]
   ->
    true
   ;
    (DQVars=[_] -> Nbr=' '; Nbr='s '),
    my_raise_exception(generic,syntax(['(TRC) Quantified variable',Nbr,'$exec'(write_with_NVs(DQVars,NVs)),' can occur only once in the quantifier''s variable list.']),[])
  ),
  my_set_inter(QVars,OQVars,IVars),   % No occurrences in other quantifier
  (IVars==[]
   ->
    true
   ;
  (IVars=[_] -> Nbr=' '; Nbr='s '),
  my_raise_exception(generic,syntax(['(TRC) Quantified variable',Nbr,'$exec'(write_with_NVs(IVars,NVs)),' cannot occur in another quantifier.']),[])).
  
% trc_safe_quantified_outside(Var,QVars,NVs) :-
%   (my_member_var(Var,QVars)
%    ->
%     my_raise_exception(generic,syntax(['(TRC) Quantified variable ','$exec'(write_with_NVs(Var,NVs)),' cannot be used outside its scope.']),[])
%    ;
%     true).

% trc_safe_quantified_reused(QVars,Vars,NVs) :-
%   my_set_inter(QVars,Vars,RVars),
%   my_reverse(RVars,DVars),
%   remove_duplicates_var(DVars,IVars),
%   (IVars==[]
%    ->
%     true
%    ;
%     (IVars=[_] -> Nbr=' '; Nbr='s '),
%     my_raise_exception(generic,syntax(['(TRC) Quantified variable',Nbr,'$exec'(write_with_NVs(IVars,NVs)),' cannot be reused in another quantifier.']),[])).

trc_safe_quantified_unused(QVars,PVars,NVs) :-
  my_set_diff(QVars,PVars,RVars),
  my_reverse(RVars,DVars),
  remove_duplicates_var(DVars,IVars),
  (IVars==[]
   ->
    true
   ;
    (IVars=[_] -> Nbr=' ',Pr='is' ; Nbr='s ',Pr='are'),
    my_raise_exception(generic,syntax(['(TRC) Quantified variable',Nbr,'$exec'(write_with_NVs(IVars,NVs)),' ',Pr,' not used.']),[])).

trc_safe_unquantified_free_vars(FVars,PVars,NVs) :-
  my_set_diff(FVars,PVars,ANQVars),
  remove_anonymous_vars(ANQVars,NVs,RVars),
  my_reverse(RVars,DVars),
  remove_duplicates_var(DVars,NQVars),
  (NQVars==[]
   ->
    true
   ;
    (NQVars=[_] -> Nbr=' ',Verb='is'; Nbr='s ',Verb='are'),
    my_raise_exception(generic,syntax(['(TRC) Variable',Nbr,'$exec'(write_with_NVs(NQVars,NVs)),' ',Verb,' not quantified and ',Verb,' not in the output list.']),[])).

trc_safe_unsafe_project_vars(PVars,FVars,QVars,NVs) :-
  my_set_diff(PVars,FVars,DUVars),
  remove_duplicates_var(DUVars,UVars),
  (UVars==[]
   ->
    true
   ;
    (UVars=[_] -> Nbr=' '; Nbr='s '),
    my_raise_exception(generic,syntax(['(TRC) Variable',Nbr,'$exec'(write_with_NVs(UVars,NVs)),' must be bound by a relation.']),[])),
  my_set_inter(PVars,QVars,PQVars),
  (PQVars==[]
   ->
    true
   ;
    (PQVars=[_] -> Nbr=' '; Nbr='s '),
    my_raise_exception(generic,syntax(['(TRC) Quantified variable',Nbr,'$exec'(write_with_NVs(PQVars,NVs)),' cannot be in the output list.']),[])).
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMPILING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

trc_to_drc(trc_query(Arguments,INVs,Formula),drc_query(DRCArguments,ONVs,DRCFormula)) :-
  my_all_members_term(rel(_Rel,_Var),Formula,Rels),
  trc_resolve_attrs(Arguments,Rels,INVs,I1NVs,DAttrNames,UAttrs),
  trc_result_tuple_to_drc(Arguments,I1NVs,I2NVs,DAttrNames,UAttrs,Rels,DRCArguments),
  trc_rel_aliases(Rels,ARels),
  trc_formula_to_drc(Formula,ARels,DAttrNames,UAttrs,I2NVs,I3NVs,Rels,DRCFormula),
  trc_apply_aliases(ARels,DRCFormula),
  my_reverse(I3NVs,I4NVs),
  trc_underscored_variables(DRCArguments,DRCFormula,I4NVs,ONVs).
  
trc_underscored_variables(Arguments,Formula,INVs,ONVs) :-
  term_variables(Formula,FVs),
  term_variables(Arguments,AVs),
  my_set_diff(FVs,AVs,NRVs),
  make_non_relevant_var_names(NRVs,INVs,ONVs).
  
trc_resolve_attrs(Arguments,Rels,INVs,ONVs,DAttrNames,UAttrs) :-
  trc_result_tuple_vars(Arguments,INVs,_I1NVs,Rels,[],ResultAttrs), 
  trc_duplicated_attr_names(ResultAttrs,DResultAttrNames),
  trc_remove_attrs(ResultAttrs,DResultAttrNames,UAttrs),
  term_variables(Rels,RelVars),
  trc_result_tuple_vars(RelVars,INVs,_ONVs,Rels,[],Attrs), % TRC1 has no tuples variables, only attributes and constants
  trc_duplicated_attr_names(Attrs,DAttrNames),
  trc_assign_attr_varnames(Attrs,DAttrNames,UAttrs,Rels,INVs,ONVs).
  
trc_assign_attr_varnames([],_DAttrNames,_UAttrs,_Rels,NVs,NVs).
trc_assign_attr_varnames([attr(RelVar,AttrName)|Attrs],DAttrNames,UAttrs,Rels,INVs,ONVs) :-
  trc_result_argument_to_drc(attr(RelVar,AttrName),INVs,I1NVs,Rels,DAttrNames,UAttrs,[],_As),
  trc_assign_attr_varnames(Attrs,DAttrNames,UAttrs,Rels,I1NVs,ONVs).

% Result tuple

trc_result_tuple_to_drc(TRC,INVs,ONVs,DAttrNames,UAttrs,Rels,DRC) :-
  trc_result_tuple_vars(TRC,INVs,I1NVs,Rels,[],TRC1), % TRC1 has no tuples variables, only attributes and constants
%  trc_duplicated_attr_names(TRC1,DAttrNames),
  trc_result_tuple_to_drc(TRC1,I1NVs,ONVs,Rels,DAttrNames,UAttrs,[],DRC).

% Deploy attributes of tuple variables
trc_result_tuple_vars([],NVs,NVs,_Rels,As,As).
trc_result_tuple_vars([RelVar|Args],INVs,ONVs,Rels,Asi,Aso) :-
  var(RelVar),  
  !,
  trc_var_to_attrs(RelVar,INVs,I1NVs,Rels,RelAttVars,[]),
  append(Asi,RelAttVars,Asi1),
  trc_result_tuple_vars(Args,I1NVs,ONVs,Rels,Asi1,Aso).
trc_result_tuple_vars([Arg|Args],INVs,ONVs,Rels,Asi,Aso) :-
  append(Asi,[Arg],Asi1),
  trc_result_tuple_vars(Args,INVs,ONVs,Rels,Asi1,Aso).
  
trc_var_to_attrs(RelVar,INVs,ONVs,Rels) -->
  {find_var_name(RelVar,RelVarName,INVs),
   trc_relname(RelVar,Rels,RelVarName,RelName),
   get_table_untyped_arguments(RelName,AttNames),
   build_trc_atts(AttNames,RelVar,RelVarName,RelAttrs,INVs,ONVs)},
  RelAttrs.
  
build_trc_atts([],_RelVar,_RelVarName,[],NVs,NVs).
build_trc_atts([AttrName|AttrNames],RelVar,RelVarName,[attr(RelVar,AttrName)|Attrs],INVs,ONVs) :-
  build_trc_atts(AttrNames,RelVar,RelVarName,Attrs,INVs,ONVs).
  
build_trc_att_vars([],_RelVar,_RelVarName,[],_DAttrNames,_UAttrs,NVs,NVs).
build_trc_att_vars([AttrName|AttrNames],RelVar,RelVarName,[RelAttVar|Attrs],DAttrNames,UAttrs,INVs,ONVs) :-
  trc_att(AttrName,RelVarName,RelAttVar,RelVar,DAttrNames,UAttrs,INVs,I1NVs),
  build_trc_att_vars(AttrNames,RelVar,RelVarName,Attrs,DAttrNames,UAttrs,I1NVs,ONVs).
  
trc_att(AttName,RelVarName,RelAttVar,RelVar,DAttrNames,UAttrs,NVs,NVs) :-
  trc_to_drc_attr_name(RelVar,RelVarName,AttName,DAttrNames,UAttrs,RelAttVarName),
  member((RelAttVarName=RelAttVar),NVs),
  !.
trc_att(AttName,RelVarName,RelAttVar,RelVar,DAttrNames,UAttrs,INVs,[RelAttVarName=RelAttVar|INVs]) :-
  trc_to_drc_attr_name(RelVar,RelVarName,AttName,DAttrNames,UAttrs,RelAttVarName).

trc_duplicated_attr_names([],[]).
trc_duplicated_attr_names([attr(_RelVar,AttrName)|Attrs],[AttrName|DAttrNames]) :-
  member(attr(_,AttrName),Attrs),
  !,
  trc_duplicated_attr_names(Attrs,DAttrNames).
trc_duplicated_attr_names([_|Attrs],DAttrNames) :-
  trc_duplicated_attr_names(Attrs,DAttrNames).

trc_remove_attrs([],_AttrNames,[]).
trc_remove_attrs([attr(_RelVar,AttrName)|Attrs],AttrNames,RAttrs) :-
  member(AttrName,AttrNames),
  !,
  trc_remove_attrs(Attrs,AttrNames,RAttrs).
trc_remove_attrs([Attr|Attrs],AttrNames,[Attr|RAttrs]) :-
  trc_remove_attrs(Attrs,AttrNames,RAttrs).

% Generate a DRC variable for each TRC argument
trc_result_tuple_to_drc([],NVs,NVs,_,_,_,As,As).
trc_result_tuple_to_drc([TRC|Arguments],INVs,ONVs,Rels,DAttrNames,UAttrs,Asi,Aso) :-
  trc_result_argument_to_drc(TRC,INVs,I1NVs,Rels,DAttrNames,UAttrs,Asi,Asi1),
  trc_result_tuple_to_drc(Arguments,I1NVs,ONVs,Rels,DAttrNames,UAttrs,Asi1,Aso).

% Number or literal
trc_result_argument_to_drc(Atom,NVs,NVs,_Rels,_DAttrNames,_UAttrs,Asi,Aso) :-
  atomic(Atom), 
  !,
  append(Asi,[Atom],Aso).
% Attribute
trc_result_argument_to_drc(attr(RelVar,AttName),INVs,ONVs,Rels,DAttrNames,UAttrs,Asi,Aso) :-
  find_var_name(RelVar,RelVarName,INVs),
  trc_relname(RelVar,Rels,RelVarName,RelName),
  exist_att(RelName,RelVarName,AttName),
  trc_to_drc_attr_name(RelVar,RelVarName,AttName,DAttrNames,UAttrs,VarAttName),
  (member((VarAttName=VarAtt),INVs)
   ->
    ONVs=INVs
   ;
    ONVs=[VarAttName=VarAtt|INVs]
  ),
  append(Asi,[VarAtt],Aso).
  
trc_relname(_,[],RelVarName,_) :- 
  my_raise_exception(generic,syntax(['Undefined tuple variable ''',RelVarName,'''.']),[]).
trc_relname(RelVar,[rel(RelName,Var)|_Rels],_RelVarName,RelName) :- 
  RelVar==Var,
  !.
trc_relname(RelVar,[_|Rels],RelVarName,RelName) :- 
  trc_relname(RelVar,Rels,RelVarName,RelName).

trc_to_drc_attr_name(RelVar,RelVarName,AttName,DAttrNames,UAttrs,VarAttName) :-
  memberchk(AttName,DAttrNames),
  \+ my_member_var(attr(RelVar,AttName),UAttrs),
  !,
  atom_concat_list([RelVarName,'_',AttName],VarAttName).
trc_to_drc_attr_name(_RelVar,_RelVarName,AttName,_DAttrNames,_UAttrs,UAttName) :-
  capitalize_first(AttName,UAttName).
  
capitalize_first(Name,UName) :-
  atom_codes(Name,[C|Cs]),
  to_uppercase_char(C,UC),
  atom_codes(UName,[UC|Cs]).

uncapitalize_first(Name,LName) :-
  atom_codes(Name,[C|Cs]),
  to_lowercase_char(C,UC),
  atom_codes(LName,[UC|Cs]).


% Formula

trc_formula_to_drc(rel(RelName),_ARels,_DAttrNames,_UAttrs,NVs,NVs,_Rels,rel(RelName)). % Propositional
trc_formula_to_drc(rel(RelName,RelVar),ARels,DAttrNames,UAttrs,INVs,ONVs,_Rels,rel(DRCRelation)) :-
  find_var_name(RelVar,RelVarName,INVs),
  trc_alias_relname(RelName,ARels,ARelName),
  get_table_untyped_arguments(ARelName,AttNames),
  build_trc_att_vars(AttNames,RelVar,RelVarName,RelAttVars,DAttrNames,UAttrs,INVs,ONVs),
  DRCRelation=..[RelName|RelAttVars].
trc_formula_to_drc(and(LTRC,RTRC),ARels,DAttrNames,UAttrs,INVs,ONVs,Rels,and(LDRC,RDRC)) :-
  trc_formula_to_drc(LTRC,ARels,DAttrNames,UAttrs,INVs,I1NVs,Rels,LDRC),
  trc_formula_to_drc(RTRC,ARels,DAttrNames,UAttrs,I1NVs,ONVs,Rels,RDRC).
trc_formula_to_drc(or(LTRC,RTRC),ARels,DAttrNames,UAttrs,INVs,ONVs,Rels,or(LDRC,RDRC)) :-
  trc_formula_to_drc(LTRC,ARels,DAttrNames,UAttrs,INVs,I1NVs,Rels,LDRC),
  trc_formula_to_drc(RTRC,ARels,DAttrNames,UAttrs,I1NVs,ONVs,Rels,RDRC).
trc_formula_to_drc((LTRC->RTRC),ARels,DAttrNames,UAttrs,INVs,ONVs,Rels,(LDRC->RDRC)) :-
  trc_formula_to_drc(LTRC,ARels,DAttrNames,UAttrs,INVs,I1NVs,Rels,LDRC),
  trc_formula_to_drc(RTRC,ARels,DAttrNames,UAttrs,I1NVs,ONVs,Rels,RDRC).
trc_formula_to_drc(not(TRC),ARels,DAttrNames,UAttrs,INVs,ONVs,Rels,not(DRC)) :-
  trc_formula_to_drc(TRC,ARels,DAttrNames,UAttrs,INVs,ONVs,Rels,DRC).  
trc_formula_to_drc(cond(TRCCondition),_ARels,DAttrNames,UAttrs,INVs,ONVs,Rels,cond(DRCCondition)) :-
  TRCCondition=..[Op,TOp1,TOp2],
  trc_operand_to_drc(TOp1,DAttrNames,UAttrs,INVs,I1NVs,Rels,DOp1),
  trc_operand_to_drc(TOp2,DAttrNames,UAttrs,I1NVs,ONVs,Rels,DOp2),
  DRCCondition=..[Op,DOp1,DOp2].
trc_formula_to_drc(exists(TRCVars,TRC),ARels,DAttrNames,UAttrs,INVs,ONVs,Rels,exists(DRCVars,DRC)) :-
  trc_var_att_to_drc_list(TRCVars,DAttrNames,UAttrs,INVs,I1NVs,Rels,DRCVars,[]),
  trc_formula_to_drc(TRC,ARels,DAttrNames,UAttrs,I1NVs,ONVs,Rels,DRC).
trc_formula_to_drc(forall(TRCVars,TRC),ARels,DAttrNames,UAttrs,INVs,ONVs,Rels,forall(DRCVars,DRC)) :-
  trc_var_att_to_drc_list(TRCVars,DAttrNames,UAttrs,INVs,I1NVs,Rels,DRCVars,[]),
  trc_formula_to_drc(TRC,ARels,DAttrNames,UAttrs,I1NVs,ONVs,Rels,DRC).
  
% Constant
trc_operand_to_drc(X,_DAttrNames,_UAttrs,NVs,NVs,_Rels,X) :-
  atomic(X), 
  !.
% Attribute
trc_operand_to_drc(TRCAtt,DAttrNames,UAttrs,INVs,ONVs,Rels,AttVar) :-
  trc_att_to_drc(TRCAtt,DAttrNames,UAttrs,INVs,ONVs,Rels,[AttVar],[]).
  
trc_var_att_to_drc_list([],_,_,NVs,NVs,_Rels) -->
  [].  
trc_var_att_to_drc_list([TRCVar|TRCVars],DAttrNames,UAttrs,INVs,ONVs,Rels) -->
  trc_var_att_to_drc(TRCVar,DAttrNames,UAttrs,INVs,I1NVs,Rels),
  trc_var_att_to_drc_list(TRCVars,DAttrNames,UAttrs,I1NVs,ONVs,Rels).

trc_var_att_to_drc(TRCVar,DAttrNames,UAttrs,INVs,ONVs,Rels) -->
  {var(TRCVar),
   !},
  trc_var_to_drc(TRCVar,DAttrNames,UAttrs,INVs,ONVs,Rels).
trc_var_att_to_drc(TRCVar,DAttrNames,UAttrs,INVs,ONVs,Rels) -->
  trc_att_to_drc(TRCVar,DAttrNames,UAttrs,INVs,ONVs,Rels).

trc_var_to_drc(RelVar,DAttrNames,UAttrs,INVs,ONVs,Rels) -->
  {find_var_name(RelVar,RelVarName,INVs),
   trc_relname(RelVar,Rels,RelVarName,RelName),
   get_table_untyped_arguments(RelName,AttNames),
   build_trc_att_vars(AttNames,RelVar,RelVarName,RelAttrs,DAttrNames,UAttrs,INVs,ONVs)},
  RelAttrs.

trc_att_to_drc(attr(RelVar,AttName),DAttrNames,UAttrs,INVs,ONVs,Rels) -->
  {find_var_name(RelVar,RelVarName,INVs),
   trc_relname(RelVar,Rels,RelVarName,RelName),
   exist_att(RelName,RelVarName,AttName),
   trc_to_drc_attr_name(RelVar,RelVarName,AttName,DAttrNames,UAttrs,AttVarName),
   (member((AttVarName=AttVar),INVs)
    ->
     ONVs=INVs
    ;
     ONVs=[(AttVarName=AttVar)|INVs]
   )
  },
  [AttVar].
  
% The first element in each list of aliases is the main relation to which attribute references are made
trc_alias_relname(RelVarName,[],RelVarName).
trc_alias_relname(RelVarName,[ARels|_ARelss],ARelVarName) :-
  member(rel(RelVarName,_),ARels),
  !,
  ARels=[rel(ARelVarName,_)|_].
trc_alias_relname(RelVarName,[_ARels|ARelss],ARelVarName) :-
  trc_alias_relname(RelVarName,ARelss,ARelVarName).
  
trc_rel_aliases(Rels,ARels) :-
  trc_rel_aliases(Rels,[],ARels).
  
trc_rel_aliases([],_,[]).
trc_rel_aliases([rel(RelName,Var1)|Rels],RelNames,[[rel(RelName,Var1)|ARels]|RARels]) :-
  \+ member(RelName,RelNames),
  trc_rel_aliases_for(Rels,Var1,ARels),
  ARels\==[],
  !,
  trc_rel_aliases(Rels,[RelName|RelNames],RARels).
trc_rel_aliases([_|Rels],RelNames,ARels) :-
  trc_rel_aliases(Rels,RelNames,ARels).

trc_rel_aliases_for([],_Var1,[]).
trc_rel_aliases_for([rel(RelName2,Var2)|Rels],Var1,[rel(RelName2,Var2)|ARels]) :-
  Var1==Var2,
  !,
  trc_rel_aliases_for(Rels,Var1,ARels).
trc_rel_aliases_for([_|Rels],Var1,ARels) :-
  trc_rel_aliases_for(Rels,Var1,ARels).

trc_apply_aliases([],_DRCFormula).
trc_apply_aliases([[rel(RelName,_Var)|ARels]|RARels],DRCFormula) :-
  my_table(RelName,Arity),
  length(VArgs,Arity),
  Rel=..[RelName|VArgs],
  my_member_term(rel(Rel),DRCFormula),
  trc_apply_aliases_vars(ARels,VArgs,DRCFormula),
  trc_apply_aliases(RARels,DRCFormula).

trc_apply_aliases_vars([],_VArgs,_DRCFormula).
trc_apply_aliases_vars([rel(RelName,_)|ARels],VArgs,DRCFormula) :-
  Rel=..[RelName|VArgs],
  my_member_term(rel(Rel),DRCFormula),
  !,
  trc_apply_aliases_vars(ARels,VArgs,DRCFormula).
trc_apply_aliases_vars([rel(RelName,_)|_ARels],_VArgs,_DRCFormula) :-
  my_raise_exception(generic,syntax(['Incorrect number of arguments in disjunction for relation ''',RelName,'''.']),[]).
  
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOLVING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DDL Assignment: Formula to Relation
solve_trc_query(assign(trc_query(Vars,NVs,Formula),Schema)) :-
  !,
  functor(Schema,TableName,_),
  drop_view_k_if_exists(TableName),
%  trc_schema(Vars,NVs,Schema,CompletedSchema),
%  CompletedSchema=Schema,
  push_flag(safe,on,OldFlag),
  create_view_k(trc,(trc_query(Vars,NVs,Formula),_AS),Schema,_LVDs),
  pop_flag(safe,OldFlag),
  clear_et, 
  compute_stratification.
% DDL Assignment: Relation to Relation
solve_trc_query(assign(RAst,Schema)) :- % Reuse RA assignment
  solve_ra_query(assign(RAst,Schema)).
% DQL Query
solve_trc_query(Query) :-
  trc_to_drc(Query,DRC),
  solve_drc_query(DRC).
  
% If column names are not given, they receive the downcased variable names (as in WinRDBI)
trc_schema(DLst,NVs,Schema) :-
  rule_head(DLst,Head),
  Head=..[RelationName|VarsCtes],
  copy_term([VarsCtes,NVs],[CVarsCtes,CNVs]),
  call_list(CNVs),
  trc_schema_attrs(CVarsCtes,VarsCtes,Attrs),
  Schema=[RelationName|Attrs].

trc_schema_attrs([],[],[]).
trc_schema_attrs([CVarCte|CVarsCtes],[VarCte|VarsCtes],[Attr|Attrs]) :-
  trc_schema_attr(CVarCte,VarCte,Attr),
  trc_schema_attrs(CVarsCtes,VarsCtes,Attrs).
  
trc_schema_attr(CVarCte,VarCte,attr(_,ColumnName,ColumnName)) :-
  var(VarCte),
  !,
  uncapitalize_first(CVarCte,ColumnName).
trc_schema_attr(_CVarCte,_VarCte,attr(_,ColumnName,ColumnName)) :-
  argument_autorenaming(ColumnName).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DISPLAYING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
display_trc(trc_query(Vars,NVs,TRCFormula),I) :-
  push_flag(language,trc,L),
  (pretty_print(off) -> write_indent(I) ; true),
  pp_indent(I),
  write_log('{ '),
  write_trc_argument_list(Vars,NVs),
  write_log(' |'),
  pp_nl_or_blank,
  I1 is I+2,
  pp_indent(I1),
  write_trc(TRCFormula,NVs,I1),
  write_log(' };'),
  pop_flag(language,L),
  nl_log.

write_trc(rel(Relation,Var),NVs,_I) :-
  write_with_NVs(Relation,NVs),
  write_log('('),
  write_with_NVs(Var,NVs),
  write_log(')').
write_trc(and(LFor,RFor),NVs,I) :-
  write_log('('),
  write_trc(LFor,NVs,I),
  write_log(' and '),
  write_trc(RFor,NVs,I),
  write_log(')').
write_trc(or(LFor,RFor),NVs,I) :-
  write_log('('),
  write_trc(LFor,NVs,I),
  write_log(' or '),
  write_trc(RFor,NVs,I),
  write_log(')').
write_trc((LFor->RFor),NVs,I) :-
  write_log('('),
  write_trc(LFor,NVs,I),
  write_log(' -> '),
  write_trc(RFor,NVs,I),
  write_log(')').
write_trc(not(For),NVs,I) :-
  write_log('not'),
  write_log('('),
  write_trc(For,NVs,I),
  write_log(')').
write_trc(cond(Condition),NVs,_I) :-
  write_trc_condition(Condition,NVs).
write_trc(exists(Vars,Formula),NVs,I) :-
  write_log('(exists '),
  write_csa_with_NVs(Vars,NVs),
  write_log(') '),
  write_trc(Formula,NVs,I).
write_trc(forall(Vars,Formula),NVs,I) :-
  write_log('(forall '),
  write_csa_with_NVs(Vars,NVs),
  write_log(') '),
  write_trc(Formula,NVs,I).
  
write_trc_condition(Condition,NVs) :-
  Condition=..[Op,L,R],
  write_trc_argument(L,NVs),
  write_log_list([' ',Op,' ']),
  write_trc_argument(R,NVs).
  
write_trc_argument(Arg,NVs) :-
  nonvar(Arg),
  Arg=attr(X,A),
  !,
  write_with_NVs(X,NVs),
  write_log('.'),
  write_log(A).
write_trc_argument(A,_NVs) :-
  number(A),
  !,
  write_log(A).
write_trc_argument(A,_NVs) :-
  atom(A),
  !,
  write_log_list(['''',A,'''']).
write_trc_argument(A,NVs) :-
  write_with_NVs(A,NVs).

write_trc_argument_list([],_NVs).
write_trc_argument_list([Arg|Args],NVs) :-
  write_trc_argument(Arg,NVs),
  write_trc_argument_list(Args,NVs).
