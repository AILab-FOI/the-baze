/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.4.1                 */
/*                                                       */
/*    Help on commands and built-ins                     */
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
% HELP ON COMMANDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List help on commands

% display_help :-
%   system_mode(hrsql),
%   !,
%   display_hrsql_help.
display_help :-
  write_log_list(['|--COMMAND-------------|--SHORT HELP--------------------------------------|',nl]),
  setof(CategoryId-Category-Commands,
        Safe^(command_category(CategoryId,Category,Safe),
         setof(command(CategoryId,CommandId,Type,Command,Arguments,ShortDesc,ExtDesc), 
               C^A^(command(CategoryId,CommandId,Type,Command,Arguments,ShortDesc,ExtDesc),
                (Type=c ; Type=y(C,A))),
               Commands)),
        IdsCategoriesCommands),
  display_categories_commands(IdsCategoriesCommands),
  write_log_list(['Any other input is evaluated as either a Prolog goal or a SQL, RA or', nl,
                  'Datalog query, view or autoview, depending on the prompt.', nl,
                  'Type des. if you get out of DES from a Prolog interpreter.', nl,
                  'Type /help keyword for detailed help on <keyword>', nl,
                  'Type /builtins for help on built-ins', nl]).
  
display_categories_commands([]).
display_categories_commands([_CategoryId-Category-Commands|Tail]) :-
  write_log_list(['* ',Category,':',nl]),
  my_map(display_command_short,Commands),
  display_categories_commands(Tail).

display_command_short(command(_CategoryId,_CommandId,_CommandType,Command,Arguments,ShortDesc,_ExtDesc)) :-
  Width=75,
  Tab=24,
  atomic_concat_list(['   /',Command,' ',Arguments],ACommand),
  atom_length(ACommand,Length),
  (Length >= Tab -> write_log_list([ACommand,nl]), my_spaces(24,S), write_log_list([S]) ; write_unquoted_tab_log(ACommand,Tab)),
  display_width_restricted(Width,Tab,ShortDesc),
  display_command_shorthands(24,Command,Arguments),
  display_command_synonyms(24,Command,Arguments,Command).

display_command_shorthands(Tab,Cmd,CmdArgs) :-
  setof(BCommand,
        CategoryId^CommandId^Command^Arguments^ShortDesc^ExtDesc^(command(CategoryId,CommandId,s(Cmd,CmdArgs),Command,Arguments,ShortDesc,ExtDesc),atom_concat('/',Command,BCommand)),
        LShorthands),
  !,
  my_list_to_tuple(LShorthands,TShorthands),
  (LShorthands=[_] -> P='' ; P='s'),
  my_spaces(Tab,S),
  write_log_list([S,'Shorthand',P,': ',TShorthands,nl]).
display_command_shorthands(_Tab,_Cmd,_CmdArgs).
  
display_command_synonyms(Tab,Cmd,CmdArgs,OriCmd) :-
  setof(BCommand,
    CategoryId^CommandId^Command^Arguments^ShortDesc^ExtDesc^
    (command(CategoryId,CommandId,y(Cmd,CmdArgs),Command,Arguments,ShortDesc,ExtDesc),
     atom_concat('/',Command,BCommand),
     OriCmd\==Command),
    LSynonyms),
  !,
  my_list_to_tuple(LSynonyms,TSynonyms),
  (LSynonyms=[_] -> P='' ; P='s'),
  my_spaces(Tab,S),
  write_log_list([S,'Synonym',P,': ',TSynonyms,nl]).
display_command_synonyms(_Tab,_Cmd,_CmdArgs,_OriCmd).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List help on keyword. It can be either a command or built-in
  
display_help(KW) :-
  findall(KW,display_help_command(KW,_Args),CKWs),
  findall(KW,display_help_builtins(KW),BKWs),
  append(CKWs,BKWs,[_|_]).
display_help(KW) :-
  write_warning_log(['Unknown keyword ''',KW,'''']),
  display_object_alternatives(command,KW),
  display_object_alternatives(builtin,KW).
  
display_help_command(Command,Arguments) :-
  display_help_command(Command,Arguments,Command).
  
display_help_command(Command,Arguments,OriginCommand) :-
  setof(command(CategoryId,CommandId,Type,Command,Arguments,ShortDesc,ExtDesc), 
        command(CategoryId,CommandId,Type,Command,Arguments,ShortDesc,ExtDesc),
        Commands),
  !,
  display_command_extended_list(Commands,OriginCommand).
  
display_help_builtins(KW) :-
  list_builtins(KW),
  !.
  
  
display_command_extended(command(_CategoryId,_CommandId,CommandType,Command,Arguments,_ShortDesc,ExtDesc),OriginCommand) :-
  write_log_list(['/',Command,' ',Arguments,' : ',ExtDesc,nl]),
  display_command_shorthands(0,Command,Arguments),
  display_command_synonyms(0,Command,Arguments,OriginCommand),
  (CommandType=y(Cmd,CmdArgs) -> display_help_command(Cmd,CmdArgs,OriginCommand) ; true),
  (CommandType=s(Cmd,CmdArgs) -> display_help_command(Cmd,CmdArgs,OriginCommand) ; true).
  
display_command_extended_list([],_OriginCommand).
display_command_extended_list([Command|Commands],OriginCommand) :-
  display_command_extended(Command,OriginCommand),
  display_command_extended_list(Commands,OriginCommand).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DISPLAY help on builtins
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_builtins :-
  list_builtins(_B).
  
list_builtins(B) :-
  Width=75,
  Tab=9,
  (\+ \+ my_infix_comparison(B,_) -> 
    write_log_list(['* Comparison Operators:',nl]),
    Found=true
   ;
    true),
  (my_infix_comparison(B,M),
   atom_concat('   ',B,T),
   write_unquoted_tab_log(T,8), 
   write_log(' '), 
   display_width_restricted(Width,Tab,M),
   fail
   ; 
   true),
  (\+ \+ unary_operator(B,_,M) -> 
    write_log_list(['* Arithmetic Prefix Operators:',nl]),
    Found=true
   ;
    true),
  (unary_operator(B,_,M), 
   atom_concat('   ',B,T),
   write_unquoted_tab_log(T,8), 
   write_log(' '), 
   display_width_restricted(Width,Tab,M),
   fail
   ; 
   true),
  (\+ \+ my_infix_arithmetic(B,_,_,_,M,_,_) -> 
    write_log_list(['* Arithmetic Infix Operators:',nl]),
    Found=true
   ;
    true),
  (my_infix_arithmetic(B,_,_,_,M,_,_),
   atom_concat('   ',B,T),
   write_unquoted_tab_log(T,8), 
   write_log(' '), 
   display_width_restricted(Width,Tab,M),
   fail
   ;
   true),
  (\+ \+ arithmetic_function(B,_,M,arithmetic,_,_) ->
    write_log_list(['* Arithmetic Functions:',nl]),
    Found=true
   ;
    true),
  (arithmetic_function(B,_,M,arithmetic,_,_),
   atom_concat('   ',B,T),
   write_unquoted_tab_log(T,8), 
   write_log(' '), 
   display_width_restricted(Width,Tab,M),
   fail
   ; 
   true),
  (\+ \+ arithmetic_function(B,_,M,aggregate,_,_) ->
    write_log_list(['* Aggregate Functions:',nl]),
    Found=true
   ;
    true),
  (arithmetic_function(B,_,M,aggregate,_,Ar),
   atomic_concat_list(['   ',B,'/',Ar],T),
   write_unquoted_tab_log(T,8), 
   write_log(' '), 
   display_width_restricted(Width,Tab,M),
   fail
   ; 
   true),
  (\+ \+ arithmetic_constant(_,B,M) ->
    write_log_list(['* Arithmetic Constants:',nl]),
    Found=true
   ;
    true),
  (arithmetic_constant(_,B,M),
   atom_concat('   ',B,T),
   write_unquoted_tab_log(T,8), 
   write_log(' '), 
   display_width_restricted(Width,Tab,M),
   fail
   ; 
   true),
  (\+ \+ (my_infix_relation(B,M) ; my_builtin_relation(B,_,M,_)) ->
    write_log_list(['* Predicates:',nl]),
    Found=true
   ;
    true),
  (my_infix_relation(B,M),
   atomic_concat_list(['   ',B,'/','2'],T),
   write_unquoted_tab_log(T,8), 
   write_log(' '), 
   display_width_restricted(Width,Tab,M),
   fail
   ; 
   true),
  (my_builtin_relation(B,A,M,_),
   number_codes(A,As),
   atom_codes(Ar,As),
   atomic_concat_list(['   ',B,'/',Ar],T),
   write_unquoted_tab_log(T,8), 
   write_log(' '), 
   display_width_restricted(Width,Tab,M),
   fail
   ; 
   true),
  (\+ \+ B=not ->
    write_log_list(['   not/1 Stratified negation',nl]),
    Found=true
   ;
    true),
  (\+ \+ B=answer ->
    write_log_list(['   answer/N',nl,'$tab'(Tab)]),
    display_width_restricted(Width,Tab,'Reserved word for the outcome relation of an automatic temporary view'),
    Found=true
   ;
    true),
  !,
  nonvar(Found).


  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DISPLAY. Restricted to a page width in columns
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  
display_width_restricted(Width,Tab,Atom) :-
  atom_length(Atom,AtomLength), 
  CWidth is AtomLength+Tab,
  (CWidth > Width
   ->
    LineWidth is Width-Tab,
    split_line(Atom,LineWidth,A1,A2),
%    write_log_list([A1,nl,'$tab'(Tab),A2,nl])
    write_log_list([A1,nl,'$tab'(Tab)]),
    display_width_restricted(Width,Tab,A2)
   ; 
    write_log_list([Atom,nl])).
    
split_line(L,Width,L1,L2) :-
  atom_codes(L,Ls),
  setof((M,L1,L2),
    LL1^L1s^L2s^(
      concat_lists([L1s," ",L2s],Ls),
      length(L1s,LL1),
      M is Width-LL1, 
      M>=0, 
      atom_codes(L1,L1s), 
      atom_codes(L2,L2s)),
        [(M,L1,L2)|_]).
  
