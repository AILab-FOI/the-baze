/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.4.1                 */
/*                                                       */
/*    DCG Subsystem                                      */
/*    Adapted and modified from                          */
/*     Ciao Prolog DCG translator                        */
/*     with permission from its authors                  */
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

:- op(1200, xfx, '-->').
%:- op(1100, xfy, ('|')). % This conflicts with SICStus CHR

my_phrase(P, L) :-
  var(P), 
  !,
  my_raise_exception(my_phrase(P, L),instantiation,unknown).
my_phrase(P, L) :-
  dcg_translate_dcg(P, P1, L, []), 
  !,
  call(P1).

my_phrase(P, L, R) :-
  var(P), 
  !,
  my_raise_exception(my_phrase(P, L, R),instantiation,unknown).
my_phrase(P, L, R) :- 
  dcg_translate_dcg(P, P1, L, R), 
  !,
  call(P1).

dcg_translation(T1, T3) :-
  dcg_expansion_internal(T1, T2),
  T2 = T3.

dcg_expansion_internal('-->'((H,List),B), (H1:-B2,B1)) :-
  !,
	dcg_translate_dcg_atom(H, H1, S0, S1),
	dcg_translate_dcg(List, B2, S1, S),
	dcg_translate_dcg(B, B1, S0, S).
dcg_expansion_internal('-->'(H,B), ':-'(H1,B1)) :-
  dcg_translate_dcg_atom(H, H1, S0, S),
  dcg_translate_dcg(B, B1, S0, S).

dcg_translate_dcg(X, Y, S0, S) :-
  dcg_translate_dcg(X, Y0, Tail, S0, S),
  (Tail\==S0 -> 
    S=Tail, dcg_connect(X, Y0, Y)
    ;   
    dcg_connect(X, Y0, S=Tail, Y)
  ).


dcg_connect((_->_), X, X) :-
  X = (_->_), 
  !.
dcg_connect(_, (P->Q), ((P->Q),true)) :-
  !.
dcg_connect(_, X, X).

dcg_connect((_->_), (P->Q0), Eq, (P->Q)) :-
  !, 
  dcg_and(Q0, Eq, Q).
dcg_connect(_, (P->Q), Eq, ((P->Q),Eq)) :-
  !.
dcg_connect(_, X0, Eq, X) :-
  dcg_and(X0, Eq, X).

dcg_translate_dcg(X, Y, S, S0, S) :-
  var(X), 
  !,
  Y=my_phrase(X,S0,S).
dcg_translate_dcg([], true, S0, S0, _) :-
  !.
dcg_translate_dcg([X|Y], S0=S1, Tail, S0, _) :-
  !,
  append([X|Y],Tail,S1).
dcg_translate_dcg(\+X, (X1 -> fail; S=S0), S, S0, S) :-
  !,
  dcg_translate_dcg(X, X1, S1, S0, S1).
dcg_translate_dcg((X,Y), XandY, Tail, S0, S) :-
  !,
  dcg_translate_dcg(X, X1, S1, S0, S1),
  dcg_translate_dcg(Y, Y1, Tail, S1, S),
  dcg_and(X1, Y1, XandY).
dcg_translate_dcg((X->Y), (X1->Y1), Tail, S0, S) :-
  !,
  dcg_translate_dcg(X, X1, S1, S0, S1),
  dcg_translate_dcg(Y, Y1, Tail, S1, S).
dcg_translate_dcg(if(X,Y,Z), if(X1,Y1,Z1), S, S0, S) :-
  !,
  dcg_translate_dcg(X, X1, S0, S1),
  dcg_translate_dcg(Y, Y1, S1, S),
  dcg_translate_dcg(Z, Z1, S1, S).
dcg_translate_dcg((X;Y), (X1;Y1), S, S0, S) :-
  !,
  dcg_translate_dcg(X, X1, S0, S),
  dcg_translate_dcg(Y, Y1, S0, S).
dcg_translate_dcg('|'(X,Y), (X1;Y1), S, S0, S) :-
  !,
  dcg_translate_dcg(X, X1, S0, S),
  dcg_translate_dcg(Y, Y1, S0, S).
dcg_translate_dcg(!, !, S0, S0, _) :-
  !.
dcg_translate_dcg({G}, call(G), S0, S0, _) :-
  var(G), 
  !.
dcg_translate_dcg({G}, G, S0, S0, _) :-
  !.
dcg_translate_dcg(X, X1, S, S0, S) :-
  dcg_translate_dcg_atom(X, X1, S0, S).

dcg_and(X, Y, Z) :-
  X==true, 
  !, 
  Z=Y.
dcg_and(X, Y, Z) :- 
  Y==true, 
  !, 
  Z=X.
dcg_and(X, Y, (X,Y)).

dcg_translate_dcg_atom(X, X1, S0, S) :-
  functor(X, F, A),
  A1 is A+1,
  A2 is A+2,
  functor(X1, F, A2),
  arg(A1, X1, S0),
  arg(A2, X1, S),
  X  =.. [F|As],
  X1 =.. [F|A1s],
  append(As,_,A1s).
  
%%%%%%%%%%%%%%%  END des_dcg.pl  %%%%%%%%%%%%%%%
  

% This file is appended to des_dcg.pl at start-up
% Distributable files contains this file appended already

% DCG expansion  
user:term_expansion('-->'(L, R), T) :-
  dcg_translation('-->'(L, R), T).

% SICStus operator defined with the same priority and association as SWI
% for allowing a goal expansion
%:- op(760, yfx, #<=>). 

% goal_expansion('#<=>'(A,B), '#<==>'(A,B)) :-
%   !,
%   nl,write('***********************************'),nl. 
