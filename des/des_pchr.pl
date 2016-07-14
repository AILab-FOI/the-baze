:- module(chr_mod, [ cts/0,u_valid/2, u_missing/2, u_wrong/2, 
                  valid/2,nonvalid/1, missing/2,wrong/2,
                  eq/3, buggy/3, in/3,getBuggy/1,getQuestions/1,subset/4 ]).
% :- use_module(library(chr)).
%:- dynamic(chr:'$chr_module'/1).
:- chr_constraint relation/2,subrel/2, bodyRel/2, 
                  checkWrong/5, subset/4,wrongQ/6,
                  collectQ/2, addCollect/3, includeQ/3,collectBuggy/1,
                  connectedSubgraph/2, cS/3, 
                  incompRel/2, subRel/2,  questions/2, get_cts/1, none/2,
                  u_valid/2, u_missing/2, u_wrong/2, 
                  valid/2,nonvalid/1, missing/2,wrong/2,
                  eq/3, buggy/3, in/3.


% constraints for deducing buggy relations
buggy1 @ connectedSubgraph(Circuit,Adyacent) ==> cS(Circuit,Circuit, Adyacent).
buggy2 @ cS(B,[],[]) <=> buggy(B,'buggy',[]).
buggy3 @ cS(B,[A|L],Adyacent), valid(A,no)  <=> cS(B,L,Adyacent).
buggy4 @ cS(B,Circuit,[A|L]), valid(A,yes) <=> cS(B,Circuit,L).

% missing atoms not matched by the heads
unmatchedAtom @ missing(X,R), incompRel(R,Q), eq(c(Q,X),empty,no) ==> buggy(R,'unmatched atom', [X]).

% wrong answer caused by body literals
wrong1  @ wrong(X,R), relation(R,N), subRel(Ri,R), bodyRel(Ri,L), in(X,c(Ri),yes)  ==> 
          checkWrong(X,R,N,Ri,L).
wrong2  @ checkWrong(X,R,_N,Ri,[]) ==> buggy(R,'erroneous program rule',[X,Ri]). 
wrong3  @ relation(S,M), subset(Sign,pi(N,M,c(WS,X)),i(S),yes)  \ checkWrong(X,R,N,Ri,[(S,Sign,WS,_MS)|L])  
             <=> 
          checkWrong(X,R,N,Ri,L).

% missing caused by body literals

missing1@ missing(X,R), relation(R,N), subRel(Ri,R), bodyRel(Ri,L), relation(S,M), 
          eq(c(MS,X),empty,no), eq(i(S,pi(N,M,c(MS,X))),empty,yes)
    ==>  member((S,pos,_WS,MS),L) |  buggy(R,'incomplete relation',[R,Ri,X]).
missing2@ missing(X,R), relation(R,N), subRel(Ri,R), bodyRel(Ri,L), relation(S,M), 
          eq(c(MS,X),empty,no), eq(i(S,pi(N,M,c(MS,X))),nonempty,yes)
    ==>  member((S,neg,_WS,MS),L) |  buggy(R,'incomplete relation',[R,Ri,X]).


collectIn   @  in(A,B,_V) \ collectQ(LI,LO)        <=> nonmember(in(A,B),LI)            | addCollect(in(A,B),LI,LO).
collectEq1  @  eq(c(A),i(A),_V), relation(A,N) \ collectQ(LI,LO)
                                                   <=> nonmember(eq(c(A/N),i(A)),LI)    | addCollect(eq(c(A/N),i(A)),LI,LO).
collectEq2  @  eq(c(A,P),B,_V) \ collectQ(LI,LO)   <=> nonmember(eq(c(A,P),B),LI)       | addCollect(eq(c(A,P),B),LI,LO).
%collectEq2  @  eq(c(A),B,_V) \ collectQ(LI,LO)     <=> nonmember(eq(c(A),B),LI)         | addCollect(eq(c(A),B),LI,LO).
collectEq3  @  eq(i(A,B),C,_V) \ collectQ(LI,LO)   <=> nonmember(eq(i(A,B),C),LI)       | addCollect(eq(i(A,B),C),LI,LO).
collectAll  @  subset(Q,A,B,_V) \ collectQ(LI,LO)  <=> nonmember(subset(Q,A,B),LI)      | addCollect(subset(Q,A,B),LI,LO).
collectEnd  @  collectQ(_LI,LO)                    <=> LO=[].
addCollect  @  addCollect(Q,LI,LO) <=>  LO= [Q|M], LI2 = [Q|LI], collectQ(LI2,M).

collectBuggy1@ buggy(A,B,C), collectBuggy(L) <=> L = [buggy(A,B,C)|M], collectBuggy(M).
collectBuggy1@ collectBuggy(L) <=> L = [].


q_buggy     @ relation(R,N) \ questions(LI,LO) <=>  nonmember(eq(c(R/N),i(R)),LI) | includeQ(eq(c(R/N),i(R)),LI,LO).
     
% q_unmatched@ missing(X,R), incompRel(R,Q)  \ questions(LI,LO)  <=> 
%     nonmember(eq(c((Q,X)),empty),LI)  | includeQ(eq(c((Q,X)),empty),LI,LO).
q_unmatched@ missing(X,R), incompRel(R,Q)  \ questions(LI,LO)  <=> 
    nonmember(eq(c(Q,X),empty),LI)  | includeQ(eq(c(Q,X),empty),LI,LO).

q_wrong1@ wrong(X,R), subRel(Ri,R) \ questions(LI,LO) <=>
    nonmember(in(X,c(Ri)),LI) | includeQ(in(X,c(Ri)),LI,LO).

% q_wrong2@ wrong(X,R), subRel(Ri,R),  bodyRel(Ri,L),subset(_Q,pi(_N,_M,c((WS,X))),i(S),no) \ questions(LI,LO)  <=>   
%                            member((S,_Sign,WS,_MS),L), 
%                            nonmember(nowrong(Ri,X),LI) |
%                            questions([nowrong(Ri,X)|LI],LO).
q_wrong2@ wrong(X,R), subRel(Ri,R),  bodyRel(Ri,L),subset(_Q,pi(_N,_M,c(WS,X)),i(S),no) \ questions(LI,LO)  <=>   
                           member((S,_Sign,WS,_MS),L), 
                           nonmember(nowrong(Ri,X),LI) |
                           questions([nowrong(Ri,X)|LI],LO).
 
q_wrong3@ wrong(X,R), relation(R,N),subRel(Ri,R), in(X,c(Ri),yes), bodyRel(Ri,L) \ questions(LI,LO) <=> 
    nonmember(nowrong(Ri,X),LI) |   wrongQ(LI,LO,X,Ri,L,N).
    

q_wrong4@ wrongQ(LI,LO,X,Ri,[],_N)  <=> questions([nowrong(Ri,X)|LI],LO).


% q_wrong5@ relation(S,M),subset(Sign,pi(N,M,c((WS,X))),i(S),yes) \ wrongQ(LI,LO,X,Ri,[(S,Sign,WS,_MS)|L],N)  <=> 
%                  wrongQ(LI,LO,X,Ri,L,N).
q_wrong5@ relation(S,M),subset(Sign,pi(N,M,c(WS,X)),i(S),yes) \ wrongQ(LI,LO,X,Ri,[(S,Sign,WS,_MS)|L],N)  <=> 
                 wrongQ(LI,LO,X,Ri,L,N).

% q_wrong6@ relation(S,M) \ wrongQ(LI,LO,X,Ri,[(S,Sign,WS,_MS)|L],N)  <=> 
%                  Q=subset(Sign,pi(N,M,c((WS,X))),i(S)),
%                  nonmember(Q,LI) |                  
%                  LO = [Q|LO2], LI2 = [Q|LI] , 
%                  wrongQ(LI2,LO2,X,Ri,L,N).
q_wrong6@ relation(S,M) \ wrongQ(LI,LO,X,Ri,[(S,Sign,WS,_MS)|L],N)  <=> 
                 Q=subset(Sign,pi(N,M,c(WS,X)),i(S)),
                 nonmember(Q,LI) |                  
                 LO = [Q|LO2], LI2 = [Q|LI] , 
                 wrongQ(LI2,LO2,X,Ri,L,N).



% q_missing1@ missing(X,R), subRel(Ri,R), bodyRel(Ri,L)  \ questions(LI,LO) <=> 
%              member((_S,_Sign,_WS,MS), L), Q = eq(c((MS,X)),empty), nonmember(Q,LI) |
%              includeQ(Q,LI,LO).        
% q_missing2@ missing(X,R), relation(R,N), relation(S,M), subRel(Ri,R), bodyRel(Ri,L), eq(c((MS,X)),empty,no) \ questions(LI,LO) <=>
%              member((S,pos,_WS,MS),L), Q=eq(i(S,pi(N,M,(c(MS,X)))),empty), nonmember(Q,LI)  |   
%              includeQ(Q,LI,LO).
% q_missing3@ missing(X,R), relation(R,N),  relation(S,M), subRel(Ri,R), bodyRel(Ri,L), eq(c((MS,X)),empty,no) \ questions(LI,LO) <=>
%              member((S,neg,_WS,MS),L), Q=eq(i(S,pi(N,M,(c(MS,X)))),nonempty), nonmember(Q,LI)  |   
%              includeQ(Q,LI,LO).

q_missing1@ missing(X,R), subRel(Ri,R), bodyRel(Ri,L)  \ questions(LI,LO) <=> 
             member((_S,_Sign,_WS,MS), L), Q = eq(c(MS,X),empty), nonmember(Q,LI) |
             includeQ(Q,LI,LO).        
q_missing2@ missing(X,R), relation(R,N), relation(S,M), subRel(Ri,R), bodyRel(Ri,L), eq(c(MS,X),empty,no) \ questions(LI,LO) <=>
             member((S,pos,_WS,MS),L), Q=eq(i(S,pi(N,M,(c(MS,X)))),empty), nonmember(Q,LI)  |   
             includeQ(Q,LI,LO).
q_missing3@ missing(X,R), relation(R,N),  relation(S,M), subRel(Ri,R), bodyRel(Ri,L), eq(c(MS,X),empty,no) \ questions(LI,LO) <=>
             member((S,neg,_WS,MS),L), Q=eq(i(S,pi(N,M,(c(MS,X)))),nonempty), nonmember(Q,LI)  |   
             includeQ(Q,LI,LO).

q_includeQ @ includeQ(Q,LI,LO) <=>  LO = [Q|M], LI2 = [Q|LI], questions(LI2,M).
q_end      @ questions(_LI,LO) <=> LO = [].           

% dealing with user answers
u_wrong @ u_wrong(X,A)   <=> in(X,c(A),yes), in(X,i(A),no).
u_missing @ u_missing(X,A) <=> in(X,c(A),no), in(X,i(A),yes) .
missing @ in(X,c(A),no), in(X,i(A),yes) ==> missing(X,A).
wrong   @ in(X,c(A),yes), in(X,i(A),no) ==> wrong(X,A).
u_valid @ u_valid(A,V)     <=> eq(c(A),i(A),V),eq(i(A),c(A),V).
valid   @ eq(c(A),i(A),V) ==> valid(A,V).


% set constraints
in1 @ in(X,A,yes), in(X,B,no)  ==> eq(A,B,no), eq(B,A,no).
eq1 @ eq(A,B,yes), in(X,A,yes) ==> in(X,B,yes).
eq2 @ eq(A,B,yes), in(X,A,no)  ==> in(X,B,no).

