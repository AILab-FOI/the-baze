/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.4.1                 */
/*                                                       */
/*    Type Subsystem                                     */
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

%
% This file contains:
% - A description of types as documentation
% - Type checking system
% - Type inferencing system

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOCUMENTATION
%
% We distinguish 3 signatures for types:
% - DLType: Internal representations for types
% - DatalogType: User-provided type names for predicates
% - SQLType: A subset of SQL standard type names
%
% DLType is the internal representation for 
%  Datalog and SQL user types
%
% DLType ::= 
%           string(varchar)
%           | 
%           string(varchar(N))
%           | 
%           string(char(N)) 
%           | 
%           number(integer)
%           | 
%           number(float) 
%          
% Notes:
% - N is an integer number denoting the maximun length a string can take
% - string(varchar(N)) and string(char(N)) are considered equivalent
% - string(varchar) represents strings with unbounded length (maximum length is the minimum the maximum length of atoms supported by the underlying Prolog system)
%
% Currently, allowed Datalog types are:
%
% DatalogType ::=
%   string
%   |
%   varchar
%   |
%   varchar(N)
%   |
%   char(N)
%   |
%   char
%   |
%   int
%   |
%   integer
%   |
%   float
%   |
%   real
%
% Notes:
% - string and varchar are considered equivalent 
% - char is equivalent to char(1)
% - int and integer are considered equivalent 
% - float and real are considered equivalent
%
% Currently, allowed SQL types (see also SQL grammar at des_sql.pl) are:
% SQLType ::=
%   STRING      % As VARCHAR
%   |
%   VARCHAR     % Variable-length string of up to the maximum length of the underlying Prolog atom
%   |
%   VARCHAR(n)  % Variable-length string of up to n characters
%   |
%   VARCHAR2(n) % Oracle's variable-length string of up to n characters
%   |
%   CHAR(n)     % Fixed-length string of n characters
%   |
%   CHAR        % Fixed-length string of 1 character
%   |
%   INT         % Integer number
%   |
%   INTEGER     % Equivalent to the former
%   |
%   REAL        % Real number
% 
% The following table shows corresponding types,
% where each cell contain type aliases:
%
%  Internal Rep.      |  Datalog        SQL
%  ---------------------------------------------------------
%  string(varchar)    |  string      |  STRING
%                     |  varchar     |  VARCHAR
%  ---------------------------------------------------------
%  string(varchar(N)) |  varchar(N)  |  VARCHAR(n)
%  string(char(N))    |  char(N)     |  VARCHAR2(n)
%                     |              |  CHAR(n)
%  ---------------------------------------------------------
%  char(1)            |  char        |  CHAR
%  ---------------------------------------------------------
%  number(integer)    |  int         |  INT
%                     |  integer     |  INTEGER
%  ---------------------------------------------------------
%  number(float)      |  float       |  REAL
%                     |  real        |
%  ---------------------------------------------------------
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% type_equivalence(Connection,IR,DL,SQL) :-
%   my_odbc_get_dbms(Connection,DBMS),
%   (Connection=='$des' ; DBMS==access),
%   !,
%   type_equivalence(IR,DL,SQL).
type_equivalence(Connection,IR,DL,SQL) :-
  type_equivalence(IR,DL,CSQL),
  my_odbc_get_dbms(Connection,DBMS),
  convert_sql_type(DBMS,CSQL,SQL).

convert_sql_type('$des',T,T) :-
  !.
convert_sql_type(access,T,T) :-
  !.
convert_sql_type(_,string,varchar(255)) :-
  !.
convert_sql_type(_,T,T).

type_equivalence(string(varchar),string,STRING) :-
  downcase_sql_type(STRING,string).
type_equivalence(string(varchar),string,VARCHAR) :-
  downcase_sql_type(VARCHAR,varchar).
type_equivalence(string(varchar),varchar,STRING) :-
  downcase_sql_type(STRING,string).
type_equivalence(string(varchar),varchar,VARCHAR) :-
  downcase_sql_type(VARCHAR,varchar).
type_equivalence(IRvarcharN,DLvarcharN,SQLvarcharN) :-
  ir_type_varcharN(IRvarcharN,N),
  dl_type_varcharN(DLvarcharN,N),
  sql_type_varcharN(SQLvarcharN,N).
type_equivalence(char(1),char,CHAR) :-
  downcase_sql_type(CHAR,char).
type_equivalence(number(integer),DINT,SINT) :-
  integer_dl_type(DINT),
  integer_sql_type(SINT).
type_equivalence(number(float),DREAL,SREAL) :-
  real_dl_type(DREAL),
  real_sql_type(SREAL).
% type_equivalence(number(float),DREAL,REAL) :-
%   real_dl_type(DREAL),
%   downcase_sql_type(REAL,real).
% type_equivalence(number(float),DREAL,SQLrealN) :-
%   real_dl_type(DREAL),
%   sql_type_realN(SQLrealN,_N).
% type_equivalence(_,_,SQLType) :-
% %  write_error_log(['Unknown data type: ',SQLType]).
%   my_raise_exception(generic,syntax(['Unknown data type: ',SQLType]),[]).
  
integer_sql_type(INT) :-
  downcase_sql_type(INT,DINT),
  (DINT = int
   ;
   DINT = int(_)
   ;
   DINT = int4
   ;
   DINT = int4(_)
   ;
   DINT = integer
   ;
   DINT = integer(_)
   ;
   DINT = bigint
   ;
   DINT = bigint(_)
  ).
  
real_sql_type(REAL) :-
  downcase_sql_type(REAL,DREAL),
  (DREAL = real
   ;
   DREAL = real(_)
   ;
   DREAL = float4
   ;
   DREAL = float4(_)
   ;
   DREAL = double
   ;
   DREAL = double(_)
  ).

real_dl_type(real).
real_dl_type(float).

integer_dl_type(int).
integer_dl_type(integer).
  
downcase_sql_type(T1,T2) :-
  nonvar(T1),
  T1 =.. [Type,Ctr|Ctrs],
  !,
  to_lowercase(Type,LType),
  T2 =.. [LType,Ctr|Ctrs].
downcase_sql_type(T1,T2) :-
  nonvar(T1),
  !,
  to_lowercase(T1,T2).
downcase_sql_type(T,T).

type_equivalence_list([],[],[]).
type_equivalence_list([IR|IRs],[DL|DLs],[SQL|SQLs]) :-
  type_equivalence(IR,DL,SQL),
  type_equivalence_list(IRs,DLs,SQLs).

ir_type_varcharN(string(varchar(N)),N).
ir_type_varcharN(string(char(N)),N).

dl_type_varcharN(varchar(N),N).
dl_type_varcharN(char(N),N).

sql_type_varcharN(VARCHARN,N) :-
  nonvar(VARCHARN),
  !,
  VARCHARN =.. [Name,N],
  to_lowercase(Name,LName),
  LVARCHARN =.. [LName,N],
  sql_type_varcharN_g(LVARCHARN,N).
sql_type_varcharN(VARCHARN,N) :-
  sql_type_varcharN_g(VARCHARN,N).
  
sql_type_varcharN_g(varchar(N),N).
sql_type_varcharN_g(varchar2(N),N).
sql_type_varcharN_g(char(N),N).
  
sql_type_realN(REALN,N) :-
  nonvar(REALN),
  !,
  REALN =.. [Name,N],
  to_lowercase(Name,LName),
  LREALN =.. [LName,N],
  sql_type_realN_g(LREALN,N).
sql_type_realN(REALN,N) :-
  sql_type_realN_g(REALN,N).

sql_type_realN_g(real(N),N).
sql_type_realN_g(double(N),N).
  

% Internal representation of types. Type synonyms
ir_type_synonym(string(varchar(N)),string(char(N))).
ir_type_synonym(string(char(N)),string(varchar(N))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TYPE CHECKING
%

% check_pred_types(+Pred,+DeclaredTypes)
% Checks whether the rules of the given predicate
% match the given types.
% New types can be inferred along the way and 
% could be asserted (TODO).
check_pred_types(_Tablename/0,[]) :-
  !.
check_pred_types(Tablename/Arity,DeclaredTypes) :-
  infer_dl_types('$des',Tablename/Arity,InferredTypes,_ExtraTypes),
  (type_subsumed_list(InferredTypes,DeclaredTypes)
   ->
    true
    %assert_extra_types(Tablename,ExtraTypes)
   ;
    ISchema=..[Tablename|InferredTypes],
    write_error_log(['Type mismatch ',ISchema,' (inferred types).']),
    !,
    fail
  ).
check_pred_types(Tablename/Arity,_DeclaredTypes) :-
  write_error_log(['No type tuple covers all the loaded rules for ',Tablename/Arity,':',nl]),
  list_rules(Tablename,Arity,7),
  write_tapi_eot,
  !,
  fail.


% check_rule_types(+Rule)
% Checks whether the types of a defining Rule for a predicate P
% is subsumed by declared types for P
% Types for P are assumed to be declared
% Fails if types are not subsumed
% If succeeds, it is possible to infer new types, which could
% added to the metadata schema (TODO)

% Propositional rules
check_rule_types(Rule) :-
  get_rule_table_name_arity(Rule,_Tablename,0).
% Relational rules
check_rule_types(Rule) :-
  get_rule_table_name_arity(Rule,Tablename,_Arity),
  get_table_types('$des',Tablename,DeclaredTypes),
  infer_rule_types(Rule,OInferredTypes,_,_ExtraTypes),
  close_types(OInferredTypes,InferredTypes),
  (type_subsumed_list(InferredTypes,DeclaredTypes)
   ->
    true
   ;
    ISchema=..[Tablename|InferredTypes],
    write_error_log(['Type mismatch ',ISchema,' (inferred types).']),
    !,
    fail).

  % assert_extra_types(Tablename,ExtraTypes).

% assert_extra_types(+Tablename,+ExtraTypes)
% Tested and OK
% Assert the types ExtraTypes inferred in addition to a
% user-given type declaration for Tablename
% Inferred types for Tablename are not asserted
assert_extra_types(Tablename,ExtraTypes) :-
  my_remove(my_types('$des',Tablename,_ColnameTypes),ExtraTypes,RExtraTypes),
  assert_type_list(RExtraTypes),
  (RExtraTypes==[] ->
    true
   ;
    ctr_type_to_schema_list(RExtraTypes,Schemas),
    write_info_log(['Types inferred and asserted: ',Schemas])).

ctr_type_to_schema_list([],[]).
ctr_type_to_schema_list([Type|Types],[Schema|Schemas]) :-
  ctr_type_to_schema(Type,Schema),
  ctr_type_to_schema_list(Types,Schemas).
  
ctr_type_to_schema(my_types('$des',Tablename,ColnameTypes),Schema) :-
  Schema=..[Tablename|ColnameTypes].
  
assert_type_list([]).
assert_type_list([my_types('$des',Tablename,ColnameTypes)|Types]) :-
  % Exists already
  length(ColnameTypes,Arity),
  my_table('$des',Tablename,Arity),
  !,
  assert_type_list(Types).
assert_type_list([my_types('$des',Tablename,ColnameTypes)|Types]) :-
  assert_table_schema(Tablename,ColnameTypes),
  assert_type_list(Types).

%type_subsumed_list(+ConcreteTypes,+GeneralTypes)
% Succeed if each element of the list ConcreteTypes is 
% a type subsumed by the corresponding element in GeneralTypes
type_subsumed_list([],[]).
type_subsumed_list([CT|CTs],[GT|GTs]) :-
  type_subsumed(CT,GT),
  type_subsumed_list(CTs,GTs).

% type_subsumed(+ConcreteType,+GeneralType)
% Succeed if ConcreteType is a type subsumed by GeneralType
% ConcreteType can be nonground for string types
% Constraints over length of string types can be pending
type_subsumed(_T,V) :-
  var(V),
  !.
type_subsumed(string(_CT),string(_GT)) :-
  type_casting(on),
  !.
type_subsumed(string(CT),string(GT)) :-
  !,
  type_subsumed(CT,GT).
type_subsumed(T,varchar) :-
  !,
  (var(T)
  ;
   \+ T=number(_)).
type_subsumed(varchar(_N),varchar(_M)) :-
  type_casting(on),
  !.
type_subsumed(char(_N),varchar(_M)) :-
  type_casting(on),
  !.
type_subsumed(varchar(_N),char(_M)) :-
  type_casting(on),
  !.
type_subsumed(char(_N),char(_M)) :-
  type_casting(on),
  !.
type_subsumed(varchar(N),varchar(M)) :-
  !,
  N#=<M.
type_subsumed(char(N),varchar(M)) :-
  !,
  N#=<M.
type_subsumed(varchar(N),char(M)) :-
  !,
  N#=<M.
type_subsumed(char(N),char(M)) :-
  !,
  N#=<M.
type_subsumed(number(T),number(T)) :-
  !.
  
% check_rule_types_list(Tablename,Rules,DeclaredTypes)
% Checks whether the user-declared types for a predicate Tablename/Arity
% are consistent with the given rules (already loaded in the database)
% for such predicate.
% Fails if types are not subsumed
% If succeeds, it is possible to infer new types, which could be
% added to the metadata schema (TODO)
check_rule_types_list(_Tablename,[],_DeclaredTypes) :-
  !.
check_rule_types_list(Tablename,Rules,DeclaredTypes) :-
  infer_types_rule_list(Rules,InferredTypes,_TypedArgs,[],_ExtraTypes),
  (type_subsumed_list(InferredTypes,DeclaredTypes) ->
    %assert_extra_types(Tablename,ExtraTypes)
    true
   ;
    ISchema=..[Tablename|InferredTypes],
    write_error_log(['Type mismatch ',ISchema,' (inferred types).']),
    !,
    fail
  ).
check_rule_types_list(Tablename,_Rules,DeclaredTypes) :-
  write_error_log(['No type tuple covers all the loaded rules for ',Tablename,':',nl]),
  length(DeclaredTypes,Arity),
  list_rules(Tablename,Arity,7),
  write_tapi_eot,
  !,
  fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TYPE INFERENCE
% 
% Infer Datalog types as internal representations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% infer_rule_types(+Rule,-Types,-ColnameTypes,-ExtraTypes)
% Infer types (Types) for a given rule (Rule)
% Also returns a complete schema (ColnameTypes) as a list of colname:type
% Also returns extra inferred types in a list of my_types('$des',Pred,ColnameTypes)
infer_rule_types(Rule,Types,ColnameTypes,ExtraTypes) :-
  infer_types_rule_list([Rule],Types,ColnameTypes,ExtraTypes).  
  
infer_types_rule_list(Rules,Types,ColnameTypes,ExtraTypes) :-
  current_db(Connection),
  infer_types_rule_list(Connection,Rules,Types,ColnameTypes,ExtraTypes).
  
:- dynamic(last_type_error/1).            % Last type error from type annotation
infer_types_rule_list(Connection,Rules,Types,ColnameTypes,ExtraTypes) :-
  set_flag(last_type_error,_NoError),
  infer_types_rule_list(Connection,Rules,Types,ColnameTypes,[],ExtraTypes),
  !.
infer_types_rule_list(_Connection,_Rules,_Types,_ColnameTypes,_ExtraTypes) :-
  get_flag(last_type_error,message(Error)),
  assign_NVs(Error,NVs),
  call_list(NVs),
  write_error_log(Error),
  !,
  fail.
  
infer_types_rule_list(_Connection,[],_InferredTypes,_TypedArgs,Tis,Tis).
infer_types_rule_list(Connection,Rs,InferredTypes,TypedArgs,Tis,ExtraTypes) :-
  copy_term(Rs,CRs),
  annotate_types_term_list(Connection,CRs,TCRs,Tis,ExtraTypes),
  CRs=[CR|_],
  get_rule_table_name_arity(CR,Tablename,Arity),
%   my_nf_bagof(Types,
%              Body^Pattern^
%              (functor(Pattern,Tablename,Arity),
%               (member(':-'(Pattern,Body),TCRs)
%                ;
%                member(Pattern,TCRs)),
%               Pattern=..[Tablename|Types]),
%              TypesList),
  my_nf_bagof(Types,
             Body^PHead^Head^
          (functor(PHead,Tablename,Arity),
           (Head= PHead ; Head= -(PHead)),
           (member(':-'(Head,Body),TCRs)
            ;
            member(Head,TCRs)),
           PHead=..[Tablename|Types]),
          TypesList),
  most_concrete_types_list(TypesList,InferredTypes),
  type_to_coltype_list(InferredTypes,TypedArgs).
 
close_my_types([],[]).
close_my_types([my_types(Conn,Name,ColnameTypes)|MTs],[my_types(Conn,Name,CColnameTypes)|CMTs]) :-
  my_unzip(ColnameTypes,Cs,Ts),
  close_types(Ts,CTs),
  my_zipWith(':',Cs,CTs,CColnameTypes),
  close_my_types(MTs,CMTs).

%most_concrete_types_list([],[]).
most_concrete_types_list([Types],Types).
most_concrete_types_list([Types1,Types2|TypesLists],CTypes) :-
  most_concrete_type_list(Types1,Types2,Types3),
  most_concrete_types_list([Types3|TypesLists],CTypes).

most_concrete_type_list([],[],[]).
most_concrete_type_list([Type1|Types1],[Type2|Types2],[CType|CTypes]) :-
  most_concrete_type(Type1,Type2,CType),
  most_concrete_type_list(Types1,Types2,CTypes).

type_to_coltype_list([],[]).
type_to_coltype_list([Type|Types],[_C:Type|TypedArgs]) :-
  type_to_coltype_list(Types,TypedArgs).
  
  
annotate_types_term_list(Connection,Rs,TRs,Tis,ExtraTypes) :-
  annotate_types_term_list_aux(Connection,Rs,TRs,Tis,ETs1), 
%  my_remove_non_ground(ETs1,ETs2),
  my_remove_duplicates_sort(ETs1,ExtraTypes).
  
annotate_types_term_list_aux(_Connection,[],[],ETs,ETs).
annotate_types_term_list_aux(Connection,[T|Ts],[TT|TTs],ETis,ETos) :-
  annotate_types_term(Connection,T,TT,ETis,ETs), 
  annotate_types_term_list_aux(Connection,Ts,TTs,ETs,ETos).

% annotate_types_term(+T,-AT,+NewTypes,-ExtraTypes)
% Annotate term T with types either inferred or declared
% New inferred types (ExtraTypes) are added to NewTypes
% This predicate may fail if no type covers a given argument
% as, e.g., no built-in type covers the rules: p(1) and p(a)

% A variable, no info, no extra inferred types
annotate_types_term(_Connection,T,T,Ts,Ts) :- 
  var(T),
  !.
% A rule. ::WARNING: For avoiding name clashes between strings and identifiers, 
% this should be specialized to detect where identifiers can occur
annotate_types_term(Connection,':-'(H,B),':-'(RH,RB),Tis,Tos) :-
  !,
  annotate_types_term(Connection,H,RH,Tis,Ts),
  annotate_types_term(Connection,B,RB,Ts,Tos).
annotate_types_term(Connection,(A,B),(RA,RB),Tis,Tos) :-
  !,
  annotate_types_term(Connection,A,RA,Tis,Ts),
  annotate_types_term(Connection,B,RB,Ts,Tos).
annotate_types_term(Connection,'=>'(L,B),':-'(RL,RB),Tis,Tos) :-
  !,
  rules_from_hyp_program(L,Rs),
  annotate_types_term_list_aux(Connection,Rs,RRs,Tis,Tis1),
  rules_from_hyp_program(RL,RRs),
%  annotate_types_term(Connection,H,RH,Tis,Ts),
  annotate_types_term(Connection,B,RB,Tis1,Tos).
% An already typed variable
annotate_types_term(_Connection,T,T,Ts,Ts) :-
  nonvar(T), 
  is_type(T),
  !.
% Lists:
annotate_types_term(Connection,L,Ts,Tis,Tos) :- 
  my_is_list(L),
  !,
  annotate_types_term_list(Connection,L,Ts,Tis,Tos).
% NULL
annotate_types_term(_Connection,'$NULL'(_ID),_T,Ts,Ts) :- 
  !.
% Numbers:
annotate_types_term(_Connection,T,number(integer),Ts,Ts) :- 
  integer(T),
  type_casting(off),
  !.
annotate_types_term(_Connection,T,number(float),Ts,Ts) :- 
  float(T),
  type_casting(off),
  !.
annotate_types_term(_Connection,T,number(_IntOrFloat),Ts,Ts) :- 
  number(T),
  type_casting(on),
  !.
% Already declared propositional relation
annotate_types_term(_Connection,T,T,Ts,Ts) :- 
  T =.. [Tablename],
  my_table('$des',Tablename,0),
  !.
% Predefined relation identifiers
annotate_types_term(_Connection,T,T,Ts,Ts) :- 
  T =.. [answer],
  !.
% Atom (string)
annotate_types_term(_Connection,T,string(varchar(N)),Ts,Ts) :- 
  atom(T),
  !,
  atom_length(T,L),
  N #>= L.
% is/2 infix
annotate_types_term(_Connection,is(L,R),is(NT,NT),Ts,Ts) :- 
  !,
  NT=number(_T),
  (var(L) -> L=NT; true),
  get_expr_type(L,NT),
  (get_expr_type(R,NT),
   !
  ;
   get_expr_type(R,RT),
   close_types([NT],[CNT]),
   close_types([RT],[CRT]),
   set_flag(last_type_error,message(['Type mismatch ',CNT,' vs. ',CRT,'.'])),
	 !,
	 fail
  ).
% Infix comparison
annotate_types_term(_Connection,Comp,TComp,Ts,Ts) :- 
  my_infix_comparison(Op,_),
  Comp=..[Op,L,R],
  !,
  TComp=..[Op,TL,TR],
  (var(L) -> L=TR ; true),
  (var(R) -> R=TL ; true),
  get_expr_type(L,TL),
  (get_expr_type(R,TL),
   TL=TR,
   !
  ;
   get_expr_type(R,TR),
   close_types([TL],[CTL]),
   close_types([TR],[CTR]),
   set_flag(last_type_error,message(['Type mismatch ',CTL,' vs. ',CTR,'.'])),
	 !,
	 fail
  ).
% User-declared typed relation
annotate_types_term(Connection,C,RC,Ts,Ts) :- 
  C =.. [Tablename|As],
  length(As,Arity),
  user_declared_relation(Connection,Tablename,Arity),
  !, 
  annotate_types_arg_list(Connection,Tablename,1,As,TAs),
  RC =.. [Tablename|TAs].
% Aggregate relations:
annotate_types_term(Connection,C,RC,Tis,Tos) :- 
  C =.. [F|As],
  length(As,Arity),
  A is Arity-1, % Listed arities does not include the hidden group_by argument 
  my_builtin_relation(F,A,_M,aggregate),
  !, 
  annotate_types_aggregate_relation(Connection,C,RC,Tis,Tos).
% Built-in predicates:
annotate_types_term(Connection,C,RC,Tis,Tos) :- 
  C =.. [F|As],
  length(As,A),
  my_builtin_preds(BIPreds),
  member(F/A,[(',')/2|BIPreds]),
  !, 
  annotate_types_term_list(Connection,As,RAs,Tis,Tos),
  RC =.. [F|RAs].
% User-defined relations: Types already inferred in Tis
annotate_types_term(Connection,C,RC,Tis,Tos) :- 
  C =.. [F|As],
  length(As,A),
  length(ColnameTypes,A),
  member(my_types('$des',F,ColnameTypes),Tis),
  !,
  annotate_types_term_list(Connection,As,RAs,Tis,Tos),
  RC =.. [F|RAs],
  (types_colnametypes(RAs,ColnameTypes)
   ->
    true
   ;
    my_unzip(ColnameTypes,_,Types),
    set_flag(last_type_error,message(['Type mismatch ',RAs,' (inferred) vs. ',Types,' (declared).'])),
    !,
    fail
   ).
% User-defined relations: Types to be inferred in Tos
annotate_types_term(Connection,C,RC,Tis,[my_types('$des',F,ColnameTypes)|Ts]) :- 
  C =.. [F|As],
  annotate_types_term_list(Connection,As,RAs,Tis,Ts),
  RC =.. [F|RAs],
  build_default_attr_name_type_list(col,RAs,ColnameTypes).
    
% Annotate types for aggregate relations:
annotate_types_aggregate_relation(Connection,count(G,GBVs,O),count(RG,GBVs,O),Tis,Tos) :-
  % Count(*) 
  annotate_types_term(Connection,G,RG,Tis,Tos),
  get_aggregate_function_type(count,O).
annotate_types_aggregate_relation(Connection,Aggr,RAggr,Tis,Tos) :-
  % Other aggregates include an additional argument: the attribute w.r.t the aggregation is computed
  Aggr=..[AF,G,V,GBVs,O],
  my_aggregate_relation(AF,4),
  annotate_types_term(Connection,G,RG,Tis,Tos),
  F=..[AF,V],
  get_aggregate_function_type(F,O),
  RAggr=..[AF,RG,V,GBVs,O].

  
user_declared_relation(Connection,Tablename,Arity) :-
  (my_table('$des',Tablename,Arity),
   !
   ;
   my_table(Connection,Tablename,Arity)
  ),
  findall(Type,my_attribute(Connection,_,Tablename,_,Type),Types),
  my_ground(Types).

% is_type(+Term)
is_type(number(T)) :-
  var(T),
  !.
is_type(number(integer)) :-
  !.
is_type(number(float)) :-
  !.
is_type(string(varchar)) :-
  !.
is_type(string(varchar(N))) :-
  var(N),
  !.
is_type(string(varchar(N))) :-
  (integer(N), N>0 ->
    true
   ;
%3.0    my_raise_exception(string(varchar(N)),type,[])
   fail
  ).
is_type(string(char(N))) :-
  var(N),
  !.
is_type(string(char(N))) :-
  (integer(N), N>0 ->
    true
   ;
%3.0    my_raise_exception(string(varchar(N)),type,[])
    fail
  ).

is_string_type(string(_)).


annotate_types_arg_list(_Connection,_Tablename,_I,[],[]).
annotate_types_arg_list(Connection,Tablename,I,[A|As],[TA|TAs]) :-
  annotate_types_arg(Connection,Tablename,I,A,TA),
%WARNING: This cut could be replaced by an explicit Error variable  
  %!,
  I1 is I+1,
  annotate_types_arg_list(Connection,Tablename,I1,As,TAs).

% annotate_types_arg(Tablename,I,A,Type) :-
%   my_attribute('$des',I,Tablename,_Colname,Type),
%   (var(A) -> A=Type ; true).
annotate_types_arg(Connection,Tablename,I,A,TA) :-
  (my_attribute('$des',I,Tablename,Colname,CType)
   ;
   my_attribute(Connection,I,Tablename,Colname,RDBType),
   type_equivalence(CType,_,RDBType)
  ),
  open_type(CType,Type),
  (var(A) ->
    A=TA,
    get_internal_type(Type,TA)
   ;
    get_expr_type(A,T),
    (most_concrete_type(T,Type,TA)
     ->
      true
     ;
      close_types([T],[BT]),
      set_flag(last_type_error,message(['Type mismatch ',Tablename,'.',Colname,':',Type,' vs. ',BT,'.',nl])),
      !,
      fail
    )
  ).
  
open_type(T,T) :-
  type_casting(off),
  !.
open_type(number(float),number(Float)) :-
  !,
 my_put_type_att(Float,float).
%   des_atts:put_atts(Float,+float).
open_type(number(integer),number(Integer)) :-
  !,
  my_put_type_att(Integer,integer).
%  des_atts:put_atts(Integer,+integer).
open_type(T,T).

% All the internal representations coincide but non-limited strings
% get_internal_type(UserType,InternalType) :-
%   var(UserType),
%   !,
%   UserType=InternalType.
% get_internal_type(string(varchar),string(varchar(_N))) :-
%   !.
% get_internal_type(string(char),string(varchar(_N))) :-
%   !.
get_internal_type(string(varchar),string(varchar(N))) :-
  var(N).
% WARNING
%   ,
%   !.
get_internal_type(string(char),string(varchar(N))) :-
  var(N).
%   ,
%   !.
get_internal_type(string(varchar),string(char(N))) :-
  var(N).
%   ,
%   !.
get_internal_type(Type,Type) :-
  !.
get_internal_type(string(char(N)),string(varchar(N))) :-
  nonvar(N).
get_internal_type(string(varchar(N)),string(char(N))) :-
  nonvar(N).
get_internal_type(T,T) :-
  type_casting(on).
get_internal_type(_T1,_T2) :-
  type_casting(on).


get_internal_type_list([],[]).
get_internal_type_list([UT|UTs],[IT|ITs]) :-
  get_internal_type(UT,IT),
  get_internal_type_list(UTs,ITs).
    
get_internal_my_type_list([],[]).
get_internal_my_type_list([my_types(DB,P,UCTs)|UTs],[my_types(DB,P,ICTs)|ITs]) :-
  swap_ucnt_icnt(UCTs,ICTs),
  get_internal_my_type_list(UTs,ITs).

swap_ucnt_icnt([],[]).
swap_ucnt_icnt([C:UT|UCTs],[C:IT|ICTs]) :-
  get_internal_type(UT,IT),
  swap_ucnt_icnt(UCTs,ICTs).

swap_uct_ict([],[]).
swap_uct_ict([UT|UTs],[IT|ITs]) :-
  get_internal_type(UT,IT),
  swap_uct_ict(UTs,ITs).

  
% get_expr_type(+Expr,-Type)
% Get the built-in type Type of expression Expr
% Returned type can be either ground if a concrete 
% built-in type is found, or partially ground otherwise
% as, e.g., number(integer) for 1, and 
% string(varchar(_)) for 'a', resp.
get_expr_type(A,_T) :-
  var(A),
  !.
get_expr_type(true,boolean).
get_expr_type(false,boolean).
get_expr_type(C,TC) :- % Before string types, to avoid inferring an incorrect type
  C =.. [F|As],
  length(As,Arity),
  arithmetic_function(F,_,_,aggregate,_,Arity),
  !, 
  get_aggregate_function_type(C,TC).
get_expr_type(C,T) :- 
  C =.. [F|As],
  length(As,Arity),
  arithmetic_function(F,_,_,arithmetic,T,Arity),
  !.
get_expr_type(T,string(char(N))) :- 
  atom(T),
  atom_length(T,L),
  N#>=L.
get_expr_type(T,string(varchar(N))) :- 
  atom(T),
  !,
  atom_length(T,L),
  N#>=L.
get_expr_type(A,number(integer)) :-
  integer(A),
  type_casting(off),
  !.
get_expr_type(A,number(Number)) :-
  integer(A),
  type_casting(on),
  !,
  (nonvar(Number) -> (Number == integer ; Number == float) ; true).
get_expr_type(A,number(Number)) :-
  float(A),
  !,
  ((type_casting(off) ; nonvar(Number))
   ->
    Number = float
   ;
    my_put_type_att(Number,float)
%    true
  ).
get_expr_type('$NULL'(_ID),_T) :-
  !.
% A prefix operator
get_expr_type(C,T) :- 
  C =.. [F,A],
  unary_operator(F,_,_),
  !,
  get_expr_type(A,T).
% An infix with known type
get_expr_type(C,T) :- 
  C =.. [F,_L,_R],
  my_infix_arithmetic(F,_SOP,_PO,T,_D,_P,_A),
  my_ground(T),
  !.
% An infix with a type depending on its arguments
get_expr_type(C,T) :- 
  C =.. [F,L,R],
  my_infix_arithmetic(F,_SOP,_PO,T,_D,_P,_A),
  !,
  get_expr_type(L,TL),
  get_expr_type(R,TR),
  expr_number_type(TL,TR,T).
get_expr_type(C,boolean) :- 
  C =.. [F,_L,_R],
  (my_infix_comparison(F,_)
   ;
   F==(',')
   ;
   F==(';')
  ),
  !.
get_expr_type(A,A) :- % A type already substituted
  !.
  
% Type of an expression involving numbers
expr_number_type(TL,TR,number(integer)) :-
  type_casting(off),
  TL=number(integer),
  TR=number(integer),
  !.
expr_number_type(_TL,_TR,number(float)) :-
  type_casting(off),
  !.
expr_number_type(TL,TR,number(Integer)) :-
  type_casting(on),
  is_integer_type(TL),
  is_integer_type(TR),
%  (var(Integer) -> des_atts:put_atts(Integer,+integer) ; Integer==integer),
  (var(Integer) -> my_put_type_att(Integer,integer) ; Integer==integer),
  !.
expr_number_type(TL,TR,number(IntOrFloat)) :-
  type_casting(on),
  ((is_float_type(TL) ; is_float_type(TR))
   ->
%    des_atts:put_atts(IntOrFloat,+float)
    my_put_type_att(IntOrFloat,float)
   ;
    ((is_integer_type(TL) , is_integer_type(TR))
     ->
%      des_atts:put_atts(IntOrFloat,+integer))
     my_put_type_att(IntOrFloat,integer))
    ;
     true
  ).
 
is_integer_type(T) :-
  T==number(integer),
  !.
is_integer_type(T) :-
  T==number(float),
  !,
  fail.
is_integer_type(number(Integer)) :-
  var(Integer),
%  des_atts:get_atts(Integer,As),
  my_get_type_atts(Integer,As),
  memberchk(float,As),
  !,
  fail.
%is_integer_type(_).

is_float_type(T) :-
  T==number(float),
  !.
is_float_type(number(Float)) :-
  var(Float),
%  des_atts:get_atts(Float,As),
  my_get_type_atts(Float,As),
  memberchk(float,As).

% Get the type of aggregate functions:
get_aggregate_function_type(count,number(T)) :-
  !,
  (type_casting(off)
   ->
    T=integer
   ;
    true).
get_aggregate_function_type(count(_A),number(T)) :-
  !,
  (type_casting(off)
   ->
    T=integer
   ;
    true).
get_aggregate_function_type(count_distinct(_A),number(T)) :-
  !,
  (type_casting(off)
   ->
    T=integer
   ;
    true).
get_aggregate_function_type(SUM_TIMES,number(T)) :-
  (SUM_TIMES = sum(Var)   ; SUM_TIMES = sum_distinct(Var) ; 
   SUM_TIMES = times(Var) ; SUM_TIMES = times_distinct(Var)),
  !,
  get_expr_type(Var,number(T)).
%3.0   my_raise_exception(SUM_TIMES,type,[])).
get_aggregate_function_type(AVG,number(float)) :-
  (AVG = avg(Var) ; AVG = avg_distinct(Var)),
  !,
  get_expr_type(Var,number(_)).
%   (Var=number(_) ->
%     true
%    ;
%     fail).
%3.0   my_raise_exception(AVG,type,[])).
get_aggregate_function_type(A,T) :-
  A=..[F,Var],
  (F == min ; F == max),
  get_expr_type(Var,T).

most_concrete_type(T1,T2,T2) :-
  my_var_or_fd_var(T1),
  !,
%  \+ \+ T1#=T2. % Possibly pending constraints
  \+ \+ my_fd_eq(T1,T2). % Possibly pending constraints
most_concrete_type(T1,T2,T1) :-
  my_var_or_fd_var(T2),
  !,
%  \+ \+ T1#=T2. % Possibly pending constraints
  \+ \+ my_fd_eq(T1,T2). % Possibly pending constraints
most_concrete_type(T1,T2,T1) :-
  T1==T2,
  !.
% most_concrete_type(number(T1),number(T2),number(T)) :-
%   most_concrete_number_type(number(T1),number(T2),number(T)),
%   !.
most_concrete_type(number(T1),number(T2),number(T)) :-
  most_concrete_type(T1,T2,T),
  !.
most_concrete_type(string(T1),string(T2),string(T)) :-
  most_concrete_type(T1,T2,T),
  !.
most_concrete_type(char(T1),char(T2),char(T)) :-
  most_concrete_type(T1,T2,T),
  !.
most_concrete_type(char(T1),varchar(T2),varchar(T)) :-
  most_concrete_type(T1,T2,T),
  !.
most_concrete_type(char(T),varchar,char(T)) :-
  !.
most_concrete_type(varchar(T1),varchar(T2),varchar(T)) :-
  most_concrete_type(T1,T2,T),
  !.
most_concrete_type(varchar(T1),char(T2),varchar(T)) :-
  most_concrete_type(T1,T2,T),
  !.
most_concrete_type(varchar(T),varchar,varchar(T)) :-
  !.
most_concrete_type(varchar,varchar(T),varchar(T)) :-
  !.
most_concrete_type(varchar,char(T),char(T)) :-
  !.
  
% most_concrete_number_type(T1,T2,T) :-
%   type_casting(off),
%   !,
%   T1=number(N1),
%   T2=number(N2),
%   T =number(N),
%   most_concrete_type(N1,N2,N).
% most_concrete_number_type(T1,T2,number(integer)) :-
%   is_integer_type(T1),
%   is_integer_type(T2).
% most_concrete_number_type(T1,T2,number(float)) :-
%   is_float_type(T1),
%   is_float_type(T2).
    
% infer_dl_types(+Connection,+Pred/Arity,-InferredTypes,-ExtraTypes)
% Given a predicate Pred/Arity, infer its types InferredTypes and 
% types of all other predicates on which such predicate depends on.
% Types are stored in the schema database.
% First, find the sub pdg from Atom
% Second, find a topological order
% Finally, infer types in such an order
% If types has been previously declared by the user,
% these definitions are used instead of inferring
% Inferred types can contain string(char(N)) and string(varchar(N)),
% where N is an FD variable constrained by N#>=C, where C is a 
% constant of the longest string found during inferring the type 
% for a given argument

infer_dl_types(Pred/Arity,PredTypes,ExtraTypes) :-
  current_db(Connection),
  infer_dl_types(Connection,Pred/Arity,PredTypes,ExtraTypes).
  
infer_dl_types(Connection,Pred/Arity,PredTypes,ExtraTypes) :-
  pdg(G),
  sub_pdg(Pred/Arity,G,SG),
  topological_order(SG,Preds),
%   (member(Pred/Arity,Preds) -> TPreds=Preds ; TPreds=[Pred/Arity|Preds]),
%   infer_types_pred_list(Connection,TPreds,DExtraTypes),
  infer_types_pred_list(Connection,Preds,ExtraTypes),
%  my_remove_duplicates_sort(DExtraTypes,ExtraTypes),
  inferred_or_declared_types(Pred,ColnameTypes,ExtraTypes),
  types_colnametypes(UnboundTypes,ColnameTypes),
  close_types(UnboundTypes,PredTypes),
  length(PredTypes,Arity).
  
% Inferred
inferred_or_declared_types(TableName,ColnameTypes,ExtraTypes) :-
  member(my_types('$des',TableName,ColnameTypes),ExtraTypes),
  !.
% Declared already
inferred_or_declared_types(TableName,ColnameTypes,_ExtraTypes) :-
  get_table_typed_arguments('$des',TableName,ColnameTypes).
  
% close_types replaces nonground types 
% constrained with either FD or attributed vars
% string(char(N)) and string(varchar(N)) are replaced by string(varchar(Min)), 
% where Min is the minimum in the FD domain
% number(T) is replaced either by number(integer) or number(float),
% depending on the attributes on T
close_types([],[]).
close_types([T|Ts],[T|BTs]) :-
  var(T),
  !,
  close_types(Ts,BTs).
close_types([number(T)|Ts],[number(T)|BTs]) :-
  var(T),
%  des_atts:get_atts(T,Types),
  my_get_type_atts(T,Types),
  Types\==[],
  !,
  (member(float,Types)
   ->
    T=float
   ;
    T=integer
  ),
  close_types(Ts,BTs).
close_types([T|Ts],[CT|BTs]) :-
  (T=string(char(N))
   ;
   T=string(varchar(N))
  ),
  my_var_or_fd_var(N),
  !,
  my_fd_min(N,Min),
  (number(Min)  
   ->
    CT=string(varchar(Min)) % Constrained string
   ;
    CT=string(varchar)), % Unconstrained string
  !,
  close_types(Ts,BTs).
close_types([T|Ts],[T|BTs]) :-
  close_types(Ts,BTs).
  
types_colnametypes([],[]).
types_colnametypes([Type|Types],[_Colname:Type|ColnameTypes]) :-
  !,
  types_colnametypes(Types,ColnameTypes).
types_colnametypes([LType|Types],[_Colname:RType|ColnameTypes]) :-
  ir_type_synonym(LType,RType),
  types_colnametypes(Types,ColnameTypes).
  
% topological_order(+PDG,-Preds)
% Given a pdg, return a list of predicates in
% topological order. 
% Order is based on dependencies:
% First come predicates with no (other-predicate) incoming edges
% Mutually recursive predicates have no defined order among them
topological_order(([],[]),[]) :-
  !.
topological_order(PDG,OPreds) :-
  no_others_incoming_edges_preds(PDG,Preds),
  Preds\==[],
  !,
  remove_preds_from_pdg(Preds,PDG,NPDG),
  topological_order(NPDG,NOPreds),
  append(Preds,NOPreds,OPreds).
topological_order((NAs,_Es),NAs).

% Find those predicates with no (other-predicate) incoming edges
no_others_incoming_edges_preds((NAs,Es),Preds) :-
  findall(NA,
          (member(NA,NAs),
           no_others_incoming_edges(NA,Es)
          ),
          Preds).

% Find out whether a predicate has not (other-predicate) incoming edges
no_others_incoming_edges(N/A,Es) :-
  \+ (member(N/A+NO/_,Es),
      N\==NO
     ;
      member(N/A-NO/_,Es),
      N\==NO).

% Remove predicates (as well as their edges) from a pdg and return the reduced pdg
remove_preds_from_pdg(Preds,(NAs,Es),(RNAs,REs)) :-
  my_set_diff(NAs,Preds,RNAs),
%  remove_edges_from_node_list(RNAs,Es,REs).
  remove_edges_from_node_list(Preds,Es,REs).
  
remove_edges_from_node_list([],Es,Es).
remove_edges_from_node_list([NA|NAs],Es,REs) :-
  remove_edges_from_node(NA,Es,TREs),
  remove_edges_from_node_list(NAs,TREs,REs).
  
  
% Infer types of predicates in the given order
infer_types_pred_list(Connection,Preds,DLTypes) :-
  infer_types_pred_list(Connection,Preds,[],DDLTypes),
  remove_duplicates(DDLTypes,DLTypes).
  
infer_types_pred_list(_Connection,[],Ts,Ts).
infer_types_pred_list(Connection,[Pred|Preds],Tsi,Tso) :-
  infer_types_pred(Connection,Pred,Tsi,Ts),
  infer_types_pred_list(Connection,Preds,Ts,Tso).

% infer_types_pred(+Connection,+Pred/Arity,+TypesIn,+TypesOut)
% Infer predicate types
% If user-declared, then do nothing
% infer_types_pred(Tablename/Arity,Ts,Ts) :-
%   my_table('$des',Tablename,Arity),
%   !.
% If built-in, then do nothing
infer_types_pred(_Connection,Tablename/Arity,Ts,Ts) :-
  my_builtin_preds(BIPreds),
  member(Tablename/Arity,BIPreds),
  !.
% Predicates in external databases
% It may be the case that there is a new Datalog rule for a persistent relation 
%infer_types_pred(Connection,Tablename/Arity,_Tis,[my_types('$des',Tablename,ColnameTypes)|ExtraTypes]) :-
infer_types_pred(Connection,Tablename/Arity,Tis,ExtraTypes) :-
  Connection \== '$des',
  relation_exists(Connection,Tablename),
  get_table_typed_arguments(Connection,Tablename,RDBColnameTypes),
  %length(RDBColnameTypes,Arity),
  !,
  my_unzip(RDBColnameTypes,RDBColnames,RDBTypes),
  type_equivalence_list(Types,_,RDBTypes),
  !,
  my_zipWith(':',RDBColnames,Types,ColnameTypes),
  Tis1=[my_types(Connection,Tablename,ColnameTypes),my_types('$des',Tablename,ColnameTypes)|Tis],
  get_object_dlrules(namearity,Tablename/Arity,DLs),
  dlrule_to_rule_list(DLs,Rules),
  infer_types_rule_list(Connection,Rules,InferredTypes,_TypedArgs,Tis1,ExtraTypes),
  build_default_attr_name_type_list(col,InferredTypes,ColnameTypes).
% Predicates with metadata and possibly user-defined rules
%infer_types_pred(Connection,Tablename/Arity,_Tis,[my_types('$des',Tablename,ColnameTypes)|ExtraTypes]) :-
infer_types_pred(Connection,Tablename/Arity,Tis,ExtraTypes) :-
  my_table('$des',Tablename,Arity),
  get_table_typed_arguments('$des',Tablename,ColnameTypes),
  !,
  Tis1=[my_types('$des',Tablename,ColnameTypes)|Tis],
  get_object_dlrules(namearity,Tablename/Arity,DLs),
%  DLs\==[],
  !,
  dlrule_to_rule_list(DLs,Rules),
  infer_types_rule_list(Connection,Rules,_InferredTypes,_TypedArgs,Tis1,ExtraTypes),
%   build_default_attr_name_type_list(col,InferredTypes,ColnameTypes).
  % Stick to data type declaration:
  length(TopTypes,Arity),
  build_default_attr_name_type_list(col,TopTypes,ColnameTypes).
% Predicates without metadata and user-defined rules
infer_types_pred(Connection,Tablename/Arity,Tis,[my_types('$des',Tablename,ColnameTypes)|ExtraTypes]) :-
  get_object_dlrules(namearity,Tablename/Arity,DLs),
  DLs\==[],
  !,
  dlrule_to_rule_list(DLs,Rules),
  infer_types_rule_list(Connection,Rules,InferredTypes,_TypedArgs,Tis,ExtraTypes),
  build_default_attr_name_type_list(col,InferredTypes,ColnameTypes).
% Predicates without metadata and without user-defined rules
infer_types_pred(_Connection,Tablename/Arity,Tis,[my_types('$des',Tablename,ColnameTypes)|Tis]) :-
  length(TopTypes,Arity),
  build_default_attr_name_type_list(col,TopTypes,ColnameTypes).
  
% pop_type_declaration(+Tablename,+Arity,-DeclaredTypes,-DeclaredColnameTypes)
% Retract type declaration for Tablename/Arity, and returns its list of types and its list of colname:types
% If no types are declared already, return a var
pop_type_declaration(Tablename,Arity,DeclaredTypes,DeclaredColnameTypes) :-
  my_table('$des',Tablename,Arity),
  get_table_typed_arguments('$des',Tablename,DeclaredColnameTypes),
  get_table_types('$des',Tablename,DeclaredTypes),
  my_retract_all_facts(my_table('$des',Tablename,Arity)),
  my_retract_all_facts(my_attribute('$des',_I,Tablename,_C,_T)),
  !.
pop_type_declaration(_Tablename,_Arity,_DeclaredTypes,_DeclaredColnameTypes).
  
  
% push_type_declaration(+Tablename,+Arity,+ColnameTypes)
% Assert type declaration for Tablename/Arity
% Do nothing if no types are given
push_type_declaration(_Tablename,_Arity,ColnameTypes) :-
  var(ColnameTypes),
  !.
push_type_declaration(Tablename,Arity,ColnameTypes) :-
  assertz(my_table('$des',Tablename,Arity)),
  assert_attr_types(Tablename,ColnameTypes).

assert_my_types_list([]).
assert_my_types_list([Type|Types]) :- % Already asserted
  Type=my_types('$des',TableName,ColnameTypes),
  length(ColnameTypes,Arity),
  my_table('$des',TableName,Arity),
  !,
  assert_my_types_list(Types).
assert_my_types_list([Type|Types]) :-
  assert_my_types(Type),
  assert_my_types_list(Types).
  
assert_my_types(my_types('$des',TableName,ColnameTypes)) :-
  length(ColnameTypes,Arity),
  assertz(my_table('$des',TableName,Arity)),
  assert_attr_types(TableName,ColnameTypes).

retract_table_schema(Tablename,Arity) :-
	my_retract_all_facts(my_table('$des',Tablename,Arity)),
	my_retract_all_facts(my_attribute('$des',_Pos,Tablename,_Attr,_Type)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type casting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type_cast_fact(F,F) :-
  type_casting(off),
  !.
type_cast_fact(F,CF) :-
  functor(F,N,Ar),
  my_table('$des',N,Ar),
  !,
  get_table_types(N,Ts),
  F =..[N|Args],
  type_cast_arg_list(Args,Ts,CArgs),
  CF=..[N|CArgs].
type_cast_fact(F,F).

type_cast_arg_list([],[],[]).
type_cast_arg_list([Arg|Args],[T|Ts],[CArg|CArgs]) :-
  type_cast_arg(Arg,T,CArg),
  type_cast_arg_list(Args,Ts,CArgs).

type_cast_arg('$NULL'(Id),_,'$NULL'(Id)) :-
  !.
type_cast_arg(F,number(float),F) :-
  float(F),
  !.
type_cast_arg(I,number(float),F) :-
  integer(I),
  !,
  F is float(I).
type_cast_arg(I,number(integer),I) :-
  integer(I),
  !.
type_cast_arg(F,number(integer),I) :-
  float(F),
  !,
  I is round(F).
type_cast_arg(A,number(T),CN) :-
  atom(A),
  !,
  atom_codes(A,Cs),
  (my_number(N,Cs,[])
   ->
    type_cast_arg(N,number(T),CN)
   ;
    type_cast_error(A,number(T))).
type_cast_arg(A,string(T),S) :-
  atom(A),
  !,
  atom_codes(A,Cs),
  cast_string(Cs,string(T),CCs),
  atom_codes(S,CCs).
type_cast_arg(N,string(T),S) :-
  number(N),
  !,
  number_codes(N,Cs),
  cast_string(Cs,string(T),CCs),
  atom_codes(S,CCs).
type_cast_arg(A,T,_) :-
  type_cast_error(A,T).
  
type_cast_error(A,T) :-
  write_error_log(['Impossible conversion of ''',A,''' to ',T,'.']),
  fail.
  
cast_string(Cs,string(T),CCs) :-
  string_type_size(string(T),L),
  (nonvar(L)
   ->
    take_up_to_N(L,Cs,CCs)  % Truncate the string
   ;
    CCs=Cs).

string_type_size(string(char(N)),N) :-
  !.
string_type_size(string(varchar(N)),N) :-
  !.
string_type_size(_,_N).

% % Ancillary stuff for external predicates
% compatible_types_in_assignment_list(_TableName,[]) :-
%   !.
% compatible_types_in_assignment_list(TableName,[attr(_,A,_)=cte(_,Type)|Assignments]) :-
%   my_attribute('$des',_,TableName,A,AttType),
%   (AttType==Type
%    -> 
%     compatible_types_in_assignment_list(TableName,Assignments)
%    ;
%     write_error_log(['Incompatible types: ',AttType,' vs. ',Type])).	
	
%%%%%%%%%%%%%%%  END des_types.pl  %%%%%%%%%%%%%%%
