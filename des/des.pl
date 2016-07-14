/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.4.1                 */
/*                                                       */
/*    MAIN PROGRAM                                       */
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

/*********************************************************/
/* Features:                                             */
/* - Multi-query language: Datalog, SQL, RA, Prolog      */
/* - Arithmetic expressions                              */
/* - Stratified negation                                 */
/* - Null value support                                  */
/* - Outer join builtin relations (lj,rj,fj)             */
/* - Aggregate builtin functions and predicates          */
/*   (count,sum,min,...)                                 */
/* - GROUP BY and HAVING support (see User Manual)       */
/* - Disjunctive bodies                                  */
/* - Type inferring/checking system                      */
/* - Datalog and SQL Declarative tracers and debuggers   */
/* - SQL test case generator                             */
/* - Memoization (tabling)                               */
/* - Terminating computations modulo built-ins (e.g., is/2) */
/* - ODBC connections to RDBMSs                          */
/* - Persistency                                         */
/* - Duplicates                                          */
/* - Strong constraints                                  */
/* - Textual API                                         */
/* - Hypothetical Datalog and SQL queries                */
/*********************************************************/


/*********************************************************************/
/* Notes about the implementation                                    */
/*********************************************************************/

% 1. Rule representation:
%    * Rule:    ':-'(Head,Body) or simply Head
%    * ruleNVs: (Rule,NVs), where NVs=['Varname1'=Var1,...]
%    * dlrule:  datalog(Rule,NVs,RuleId,CompId,Lines,FileId,source)
%               datalog(Rule,NVs,RuleId,CompId,Lines,FileId,compilation(SourceRule,SourceNVs,[RuleIds]))
%               datalog(Rule,NVs,RuleId,CompId,Lines,FileId,compiled)
%      'source' means that the rule has not been compiled
%      'compilation' means that the source rule SourceRule has been compiled into the rules identified by [RuleId|RuleIds]. 
%                    RuleId is arbitrarily chosen as a representative 
%      'compiled' means that the rule is the result of a compilation
%      RuleId is the rule identifier (an integer) used for managing duplicates
%      CompId is the computation identifier for hypothetical queries
%      Lines are the lines in the source text program where the rule occurs: (FromLine-ToLine)
%      FileID is the file identifier (an integer) where the rule is defined or otherwise the connection name of the external DBMS

% /* SWI-Prolog flag */
:- set_prolog_flag(double_quotes,codes).

/*********************************************************************/
/* Operators                                                         */
/*********************************************************************/

%:- op(1200,xfx,[<-]).      % LogiQL rules
:- op(1050,xfy,[=>]).      % Hypothetical implication
:- op(1020,yfx,[/\\]).     % Hypothetical conjunction
%:- op(300,yfx,[or]).       % Disjunction
:- op(400,xfy,[division]). % Relational division
%:- op(300,fy,[-]). 
:- op(900,fy,[not]).       % Stratified negation
%:- op(900,fy,[!]).         % LogiQL negation


/*********************************************************************/
/* Autorun information from Prolog interpreters (only as a reference)*/
/*********************************************************************/

%Autorun:
% Sicstus:
%  -l des.pl
% SWI-Prolog:
%  -g "ensure_loaded(des)"
% GNU-Prolog:
%  --entry-goal ['des.pl']
% CIAO Prolog:
%  -l ciaorc

% The following is needed since GNU 1.3.1 does not support ensure_loaded 
% ISO directive. A prelude for this system is prepended to this file, 
% containing a call to consult instead of to ensure_loaded.
my_ensure_loaded(X) :-
  ensure_loaded(X).

  
/*********************************************************************/
/* DES version number                                                */
/*********************************************************************/

des_version(V) :-
  SUV = "4.1        ",
  remove_ending_blanks(SUV,SV),
  atom_codes(V,SV).


/*********************************************************************/
/* Specific files                                                    */
/*********************************************************************/

:- include('des_dcg.pl').                                % DCG translator

:- initialization(my_ensure_loaded('des_atts.pl')).        % Attributed variables for type tracing
:- initialization(my_ensure_loaded('des_glue.pl')).        % Prolog system-dependent predicates
:- initialization(my_ensure_loaded('des_common.pl')).      % Common predicates
:- initialization(my_ensure_loaded('des_commands.pl')).    % Commands
:- initialization(my_ensure_loaded('des_help.pl')).        % Help system
:- initialization(my_ensure_loaded('des_sql_debug.pl')).   % SQL declarative debugger
:- initialization(my_ensure_loaded('des_dl_debug.pl')).    % Datalog declarative debugger
%:- initialization(my_ensure_loaded('des_logiql.pl')).      % LogiQL subsystem
:- initialization(my_ensure_loaded('des_dbigen.pl')).      % DB instance generation
:- initialization(my_ensure_loaded('des_modes.pl')).       % Mode assertions
:- initialization(my_ensure_loaded('des_persistence.pl')). % Persistence support for predicates
:- initialization(my_ensure_loaded('des_sql.pl')).         % SQL query processor
:- initialization(my_ensure_loaded('des_ra.pl')).          % AR query processor
:- initialization(my_ensure_loaded('des_drc.pl')).         % DRC query processor
:- initialization(my_ensure_loaded('des_trc.pl')).         % TRC query processor
%:- initialization(my_ensure_loaded('des_hrsql.pl')).       % HR-SQL query processor
:- initialization(my_ensure_loaded('des_trace.pl')).       % Datalog and SQL tracers
:- initialization(my_ensure_loaded('des_tc.pl')).          % Test case generator
:- initialization(my_ensure_loaded('des_types.pl')).       % Type system

/*********************************************************************/
/* Auto-start after loading                                          */
/*********************************************************************/

% process_out looks for des.out and processes its contents, if available
%:- initialization(call_cleanup(start,(process_out,halt))).
:- initialization(call_cleanup(start,process_out)).

/*********************************************************************/
/* Dynamic Predicates                                                */
/*********************************************************************/

:- dynamic(first_iter/2).     % Flag indicating whether it is the first iteration in the fixpoint computation: (CompId, Boolean)

:- dynamic(optimize_cf/1).    % Flag indicating whether complete flag optimization is enabled
:- dynamic(optimize_cc/1).    % Flag indicating whether complete computation optimization is enabled
:- dynamic(optimize_ep/1).    % Flag indicating whether extensional predicate optimization is enabled
%:- dynamic(optimize_edb/1).   % Flag indicating whether extensional database optimization is enabled
:- dynamic(optimize_nrp/1).   % Flag indicating whether non-recursive predicate optimization is enabled
:- dynamic(optimize_st/1).    % Flag indicating whether stratum optimization is enabled

:- dynamic(edb_retrievals/1). % Flag indicating the number of EDB retrievals during fixpoint computation
:- dynamic(idb_retrievals/1). % Flag indicating the number of IDB retrievals during fixpoint computation
:- dynamic(et_retrievals/1).  % Flag indicating the number of ET retrievals
:- dynamic(et_lookups/1).     % Flag indicating the number of ET lookups
:- dynamic(ct_lookups/1).     % Flag indicating the number of CT lookups
:- dynamic(cf_lookups/1).     % Flag indicating the number of CF lookups
:- dynamic(fp_iterations/1).  % Flag indicating the number of iterations during fixpoint computation

:- dynamic(my_log/1).         % Log file list information (filename and associated stream)
:- dynamic(verbose/1).        % Verbose mode flag 
:- dynamic(silent/1).         % Silent mode flag for batch scripts
:- dynamic(pretty_print/1).   % Pretty print for listings (takes more lines to print)
:- dynamic(batch/4).          % Batch processing info 
:- dynamic(batch/1).          % Batch mode flag: on (PDG is not updated)/off (PDB is updated)
:- dynamic(consult/2).        % Consult mode flag 
:- dynamic(file_table/2).     % File table (file_table/2): Holds a file name and a file identifier
:- dynamic(et/5).             % Extension Table. Dynamic predicate: 
                              % Hash, Fact (G), Source (IdG), Context (CId), Iteration (It)
                              % Views of the predicate:
                              % et/4: Fact (G), Source (IdG), Context (CId), Iteration (It)
                              % et/3: Fact (G), Source (IdG), Iteration (It)
:- dynamic(called/3).         % Call Patterns. Hash, Goal (G), Context (CId)
                              % Views of the predicate:
                              % called/2: Goal (G), Context (CId)
:- dynamic(et_flag/1).        % Extension Table flag 
:- dynamic(complete_flag/5).  % Complete computation flag 
:- dynamic(datalog/7).        % Datalog Rule Database 
:- dynamic(datalog_persistent/3). % Datalog Backup Rule Database of persistent predicates
:- dynamic(my_persistent/3).  % Predicate (schema) made persistent through an ODBC connection name (Connection,PredicateSchema,DependentNodes)
:- dynamic(my_modes/2).       % Modes of an unsafe predicate
:- dynamic(my_rule_modes/3).  % Modes of an unsafe rule 
:- dynamic(strata/1).         % Result from a stratification
:- dynamic(pdg/1).            % Predicate Dependency Graph
:- dynamic(user_predicates/1). % List of user predicates
:- dynamic(recursive_predicates/1). % List of recursive predicates
:- dynamic(extensional_predicates/1). % List of extensional predicates
:- dynamic(non_recursive_predicates/1). % List of non-recursive predicates
:- dynamic(nr_nd_predicates/1). % List of non-recursive predicates which do not depend on any recursive predicate
:- dynamic(non_completeable_predicates/1). % List of non-completeable predicates (e.g., those depending on top/2)
%:- dynamic(non_recursive_positive_predicates/1). % List of non-recursive predicates with positive path
:- dynamic(local_predicates/1). % List of predicates managed by DES (ddb predicates including those with mates in the external database)
:- dynamic(ddb_predicates/1). % List of predicates belonging only to DES (no mates in the external database)
:- dynamic(rdb_predicates/1). % List of external RDB predicates (maybe with a mate in the deductive database)
:- dynamic(mdb_predicates/1). % List of mixed predicates (both in the local deductive database and the external database)
:- dynamic(restricted_predicates/1). % List of restricted predicates (a predicate with at least a - head in one of its rules)
:- dynamic(dependent_restricted_predicates/1). % List of predicates that depend on restricted predicates 
:- dynamic(limited_domain_predicates/1). % List of limited domain predicates
:- dynamic(null_id/1).        % Integer identifier for nulls, represented as '$NULL'(i), where 'i' is the null identifier
:- dynamic(rule_id/1).        % Integer identifier for rules, represented as datalog(Rule,NVs,i,Lines,FileId,Kind), where 'i' is the rule identifier
:- dynamic(pred_id/2).        % Integer identifier for system predicates built along compilations: '$pi', where 'i' is the identifier
:- dynamic(duplicates/1).     % Flag indicating whether duplicates are enabled
:- dynamic(timing/1).         % Flag indicating elapsed time display: on, off or detailed
:- dynamic(format_timing/1).  % Flag indicating whether formatting of time is enabled or disabled: on or off
:- dynamic(elapsed_time/3).   % Elapsed times for parsing, computation and display
:- dynamic(last_elapsed_time/4). % Last elapsed times for parsing, computation and display
:- dynamic(error/0).          % Flag indicating whether there was an error
:- dynamic(safe/1).           % Flag indicating whether program transformation for safe rules is allowed
:- dynamic(simplification/1). % Flag indicating whether program simplification for performance is allowed
:- dynamic(reorder_goals/1).  % Flag indicating whether equality left-pushing for performance is allowed
:- dynamic(unfold/1).         % Flag indicating whether program unfolding is allowed
:- dynamic(language/1).       % Flag indicating the current default query language
%:- dynamic(system_mode/1).    % Flag indicating the current system mode (des or hrsql)
:- dynamic(start_path/1).     % Path on first initialization
:- dynamic(development/1).    % Flag indicating a development session. Listings and consultings show source and compiled rules
:- dynamic(safety_warnings/1).% Flag indicating whether safety warnings are enabled
:- dynamic(singleton_warnings/1).% Flag indicating whether singleton warnings are enabled
:- dynamic(undef_pred_warnings/1).% Flag indicating whether undefined predicate warnings are enabled
:- dynamic(last_autoview/1).  % Flag indicating the last autoview executed. This autoview should be retracted upon exceptions
:- dynamic(current_db/2).     % Flag indicating the current opened DB along with the DBMS
:- dynamic(opened_db/4).      % Facts indicating the open DBs: one fact for each DB including the connection name and handle, DBMS, and opening user options
:- dynamic(rdb_id/2).         % Identifier for RDB tuples (coming from tables or views)
:- dynamic(trusting/1).       % Flag indicating whether a trust file is being processed
:- dynamic(trusted_views/1).  % Predicate containing trusted view names for SQL debugging
:- dynamic(oracle_views/1).   % Predicate containing oracle view names for SQL debugging (don't miss with Oracle database system)
:- dynamic(output/1).         % Flag indicating whether output is enabled (on or off)
:- dynamic(check_ic/1).       % Flag indicating whether integrity constraint checking is enabled (on or off)
:- dynamic(my_odbc_query_handle/1). % Flag indicating the handle to the last ODBC query
:- dynamic(compact_listings/1).  % Flag indicating whether compact listings are enabled
:- dynamic(show_compilations/1). % Flag indicating whether SQL to DL compilations are displayed
:- dynamic(show_sql/1).       % Flag indicating whether externally-processed SQL statements and corresponding SQL statements to RA queries are displayed
:- dynamic(state/1).          % States for various flags to be restored upon exceptions
:- dynamic(running_info/1).   % Flag indicating whether running info is to be displayed (number of consulted rules)
:- dynamic(fp_info/1).        % Flag indicating whether fixpoint info is to be displayed (computed entries in ET)
:- dynamic(tapi/1).           % Flag indicating whether a tapi command is being processed
:- dynamic(type_casting/1).   % Flag indicating whether type casting for SQL is enabled (on or off)
:- dynamic(hypothetical/1).   % Flag indicating whether hypothetical queries are enabled (on or off)
:- dynamic(indexing/1).       % Flag indicating whether indexing on extension table is enabled (on or off)
:- dynamic(computed_tuples/1).% Flag with the number of computed tuples during fixpoint computation (for running info display)
:- dynamic(computed_tuples/6).% Flag with the number of computed tuples during fixpoint computation for a given goal (for top N queries)
:- dynamic(display_banner/1). % Flag indicating whether banner is displayed at startup (on or off)
:- dynamic(display_answer/1). % Flag indicating whether answers are to be displayed upon solving (on or off)
:- dynamic(display_nbr_of_tuples/1). % Flag indicating whether the number of tuples are to be displayed upon solving (on or off)
:- dynamic(order_answer/1).   % Flag indicating whether the answer is to be displayed upon solving (on or off)
:- dynamic(multiline/1).      % Flag indicating whether multiline input is enabled (on or off)
:- dynamic(my_statistics/1).  % Flag for statistics
:- dynamic(host_statistics/1).% Flag for host statistics
:- dynamic(stopwatch/3).      % Flag indicating stopwatch elapsed time
:- dynamic(des_sql_solving/1).% Flag indicating whether DES solving is forced for external DBMSs
:- dynamic(prompt/1).         % Flag indicating the prompt format
:- dynamic(editor/1).         % Flag indicating the current external editor, if defined already
:- dynamic(hyp_program_asserted/6). % Flag indicating the asserted rules for hypothetical computations
:- dynamic(nulls/1).          % Flag indicating whether nulls are allowed
:- dynamic(recompute_pdg/1).  % Flag indicating whether PDG must be recomputed after an exception
:- dynamic(shell_exit_code/1). % Flag indicating the last exit code returned by a shell invocation
:- dynamic(error/1).          % Flag with the last error number (only updated by dbigen)
:- dynamic(timeout/1).        % Flag indicating whether a timeout is being executed
%:- dynamic(logiql/1).         % Flag indicating whether LogiQL output is enabled
:- dynamic(last_syntax_error/5).  % Last syntax error from parsing: message, remaining chars, language, direction and context (sentence or column)
:- dynamic(host_safe/1).      % Flag indicating whether a safe mode for the host is enabled
%:- dynamic(last_semantic_error/2).% Last semantic error from parsing
% :- dynamic(logiql_cmd/1).     % Flag indicating whether LogiQL commands are included for trying output in REPL
% :- dynamic(logiql_log/2).     % Log file information (filename and associated stream) for LogiQL batch files



/*********************************************************************/
/* Initial Status                                                    */
/*********************************************************************/

set_initial_status :-
  % Status defined by user-configurable flags:
  set_flag(verbose(off)),        % Verbose output disabled
  set_flag(silent(off)),         % Silent batch output disabled
  set_flag(pretty_print(on)),    % Pretty print activated
  set_flag(duplicates(off)),     % Duplicates disabled
  set_flag(optimize_cc(on)),     % Complete flag optimization
%  set_flag(optimize_edb(off)),    % EDB optimization
  set_flag(optimize_ep(on)),     % Extensional predicate optimization
  set_flag(optimize_nrp(on)),    % Non-recursive predicate optimization
  set_flag(optimize_st(off)),    % Stratum optimization
  set_flag(timing(off)),         % Elapsed time display disabled
  set_flag(format_timing(on)),   % Formatting of timing enabled
  set_flag(safe(off)),           % Program transformation disabled
  set_flag(simplification(off)), % Program simplification disabled
  set_flag(reorder_goals(off)),  % Equality left-pushing disabled
  set_flag(unfold(off)),         % Program unfolding disabled
  set_flag(language(datalog)) ,  % Default interpreter language (possible values are: datalog, prolog, sql)
%  set_flag(system_mode(des)) ,   % Default system mode (possible values are: des, hrsql)
  set_flag(development(off)),    % Development session on/off
  set_flag(safety_warnings(on)), % Safety warnings on/off, no command for changing it
  set_flag(singleton_warnings(on)), % Singleton warnings on/off
  set_flag(undef_pred_warnings(on)), % Undefined predicate warnings on/off, no command for changing it
  set_flag(trusting(off)),       % No trust file processing
  set_flag(output(on)),          % Output enabled
  set_flag(my_log([])),          % No logging
  set_flag(display_banner(on)),  % Display of banner is enabled
  set_flag(display_answer(on)),  % Display of answers is enabled
  set_flag(display_nbr_of_tuples(on)),  % Display of number of computed tuples is enabled
  set_flag(order_answer(on)),    % Ordering answers is enabled
  set_flag(multiline(off)),      % Multiline input is disabled
  set_flag(check_ic(on)),        % Integrity constraint checking enabled
  set_flag(compact_listings(off)),  % Compact listings disabled
  set_flag(show_compilations(off)), % Compilation listings disabled
  set_flag(show_sql(off)),       % SQL external processing and RA compilations listings disabled
  set_flag(running_info(on)),    % Running info display enabled
  set_flag(fp_info(off)),        % Fixpoint info display disabled
  set_flag(tapi(off)),           % TAPI disabled
  set_flag(type_casting(off)),   % Type casting disabled
  set_flag(hypothetical(on)),    % Hypothetical queries enabled
  set_flag(indexing(on)),        % Hash indexing enabled
  set_flag(des_sql_solving(off)), % SQL queries are solved by external DBMS for an open ODBC connection
  set_flag(nulls(on)),           % Nulls are enabled by default
  set_flag(prompt(des)),         % Normal prompt: DES>
  set_flag(my_statistics(off)),  % Statistics disabled
  set_flag(shell_exit_code(0)),  % Exit code = 0
  set_flag(error(0)),            % Last error number = 0
  set_flag(timeout(off)),        % Last error number = 0
%  set_flag(logiql(off)),         % LogiQL output disabled
%   set_flag(logiql_cmd(off)),     % LogiQL commands disabled
  % Other flags
  set_flag(optimize_cf(on)),     % Complete flag optimization
  set_flag(limited_domain_predicates([])),
  set_flag(host_safe(off)),      % Host safe mode disabled
  reset_statistics,              % Reset statistics (fixpoint iterations, EDB retrievals, ...)
  reset_stopwatch,               % Stopwatch reset
  set_random_seed,               % Ste the seed of the randomizer to a random seed based on the system time
  % System initial status:
  set_start_path,
  set_initial_db,                % Set '$des' as the current database
  set_autosave_flag,
  retractall(state(_)),
  set_built_in_modes,
  reset_syntax_error,
  flag_et_no_change,             % Set 'change on et' to 'no'
  compute_stratification.
  
% Displaying the system status

display_status :-
  processC(version,[],_,_),
  processC(silent,[],_,_),
  processC(prolog_system,[],_,_),
  processC(verbose,[],_,_),
  processC(pretty_print,[],_,_),
  processC(duplicates,[],_,_),
  processC(optimize,[],_,_),
  processC(timing,[],_,_),
  processC(format_timing,[],_,_),
  processC(safe,[],_,_),
  processC(simplification,[],_,_),
  processC(reorder_goals,[],_,_),
  processC(unfold,[],_,_),
  processC(development,[],_,_),
  processC(safety_warnings,[],_,_),
  processC(singleton_warnings,[],_,_),
  processC(undef_pred_warnings,[],_,_),
  processC(output,[],_,_),
  processC(log,[],_,_),
  processC(display_banner,[],_,_),
  processC(display_answer,[],_,_),
  processC(display_nbr_of_tuples,[],_,_),
  processC(order_answer,[],_,_),
  processC(multiline,[],_,_),
  processC(check,[],_,_),
  processC(compact_listings,[],_,_),
  processC(show_compilations,[],_,_),
  processC(show_sql,[],_,_),
  processC(running_info,[],_,_),
  processC(fp_info,[],_,_),
  processC(type_casting,[],_,_),
  processC(host_safe,[],_,_),
  processC(hypothetical,[],_,_),
  processC(indexing,[],_,_),
  processC(des_sql_solving,[],_,_),
  processC(nulls,[],_,_),
  processC(prompt,[],_,_),
  processC(statistics,[],_,_),
  processC(log,[],_,_), 
  processC(tc_domain,[],_,_),
  processC(tc_size,[],_,_),
  processC(type_casting,[],_,_),
  processC(pwd,[],_,_),
  processC(current_db,[],_,_).

% Datalog DB ($des) is the default DB managed by the deductive engine '$des'. No connection data ($void)
set_initial_db :-
  close_dbs,
  set_default_db,
  set_flag(opened_db('$des','$void','$des',[])).
  
set_default_db :-
  set_flag(current_db('$des','$des')).
  
current_db(Connection) :-
  current_db(Connection,_DBMS).

opened_db(Connection) :-
  opened_db(Connection,_Handle,_DBMS,_UOptions).
  
opened_db(Connection,Handle) :-
  opened_db(Connection,Handle,_DBMS,_UOptions).
  
opened_db(Connection,Handle,DBMS) :-
  opened_db(Connection,Handle,DBMS,_UOptions).
  
close_dbs :-  
  opened_db(DB),
  (DB == '$des'
   ->
    true
   ;
    processC(close_db,[DB],_,_)    % Closing an opened RDB
  ),
  fail.
close_dbs.

    
% save_state_flags/0. Push current state of several flags to be restored upon exceptions
save_state_flags :-
  (state(_SavedState)
   ->
    true
   ; 
    State = [
      language(_),
      compact_listings(_),
      verbose(_),
      check_ic(_),
      optimize_cf(_),
      optimize_nrp(_),
      safety_warnings(_),
      simplification(_),
%      reorder_goals(_),
      pretty_print(_),
      output(_),
      timeout(_),
%       logiql(_),
      des_sql_solving(_)
%      last_syntax_error(_,_,_)
            ],
    call_list(State),
    assertz(state(State))
  ).
  
% restore_state_flags/0. Restore a saved state upon exception
restore_state_flags :-
  retract(state(State)),
  member(Flag,State),
  set_flag(Flag),
  fail.
restore_state_flags :-
  set_flag(timeout(off)).
  
set_state_flag(Flag) :-
  functor(Flag,F,A),
  functor(SFlag,F,A),
  state(State),
  member(SFlag,State),
  replace_list(SFlag,Flag,State,RState),
  set_flag(state(RState)).
  
% save_state/1. Save the Prolog dynamic database
save_state(File) :-
  current_stratification(St),
  persistent_assertions(Persistents),
  close_persistents(Persistents),
  opened_dbs(DBs,Options),
  current_db(CDB),
  close_dbs(DBs),
  my_datetime((Y,M,D,H,Mi,S)),
  pad_zeroes(H,2,PH),
  pad_zeroes(Mi,2,PMi),
  pad_zeroes(S,2,PS),
  my_absolute_filename(File,AFN),
  current_output(COS),
  open(AFN,write,OS),
  set_output(OS),
  write('%'),
  nl,
  write_list(['%% State saved at ',PH,':',PMi,':',PS,' on ',M,'-',D,'-',Y,'.']),
  nl,
  write('%'),
  nl,
  my_save_state(Persistents,DBs,Options,CDB),
  set_output(COS),
  close(OS),
  open_dbs(DBs,Options),
  open_persistents(Persistents),
  use_db(CDB),
  restore_stratification(St).

my_save_state_common(Persistents,DBs,Options,CDB) :-
  nl,
  write(':- open_dbs('),
  writeq(DBs),
  write(','),
  writeq(Options),
  write(').'),
  nl,
  write(':- use_db('),
  writeq(CDB),
  write(').'),
  nl,
  write_list([':- open_persistents(',Persistents,').']),
  nl.
  
% restore_state/1. Restore the Prolog dynamic database
restore_state(File) :-
  (my_file_exists(File)
   ->
    my_absolute_filename(File,AFN),
    my_restore_state(AFN)
   ;
    write_error_log(['File does not exist.'])
  ).
  
  
use_db(Connection) :-
  opened_db(Connection,_Handle,DBMS),
  set_flag(current_db(Connection,DBMS)).
  
open_dbs([],[]).
open_dbs([DB|DBs],[Opts|ROpts]) :-
  open_db(DB,Opts,_Error),
  open_dbs(DBs,ROpts).
  
opened_dbs(DBs,Options) :-
  findall((DB,Options),(opened_db(DB,_,_,Options),DB\=='$des'),DBsOptions),
  my_unzip(DBsOptions,DBs,Options).
  
close_dbs([]).
close_dbs([DB|DBs]) :-
  my_close_odbc(DB,'$des'),
  close_dbs(DBs).

persistent_assertions(Persistents) :-
  findall(persistent(PredSchema,Connection),my_persistent(Connection,PredSchema,_),Persistents).
  
close_persistents([]).
close_persistents([persistent(PredSchema,_Connection)|Persistents]) :-
  functor(PredSchema,Name,_Arity),
  close_persistent(Name),
  close_persistents(Persistents).

open_persistents([]).
open_persistents([Persistent|Persistents]) :-
  assert_assertion(Persistent),
  open_persistents(Persistents).

  
set_autosave_flag :-
  my_start_path_absolute_file_name('des.ini',Fi),
  my_start_path_absolute_file_name('des.out',Fo),
  is_line_in_file('/restore_state',Fi),
  is_line_in_file('/save_state',Fo),
  !,
  set_flag(autosave,on).
set_autosave_flag :-
  set_flag(autosave,off).
  
process_autosave(on) :-
  my_start_path_absolute_file_name('des.ini',Fi),
  my_start_path_absolute_file_name('des.out',Fo),
  include_line_in_file('/restore_state',Fi),
  include_line_in_file('/save_state',Fo).
process_autosave(off) :-
  my_start_path_absolute_file_name('des.ini',Fi),
  my_start_path_absolute_file_name('des.out',Fo),
  delete_line_from_file('/restore_state',Fi),
  delete_line_from_file('/save_state',Fo),
  delete_file_if_empty(Fi),
  delete_file_if_empty(Fo).
  
/*********************************************************************/
/* 'dual' Predefined Predicate                                       */
/*********************************************************************/

dual.
%'DUAL'.

/*********************************************************************/
/* Starting the System from scratch: start                           */
/*********************************************************************/

start :- 
  flush_output,
  init_des, 
  des.

% System initialization on start-up
init_des :- 
  retractall(batch(_,_,_,_)),
  retractall(consult(_,_)),
  retractall(rule_id(_)),
  processC(abolish,[],_NVs,_Continue),
  set_initial_status,
  process_conf,  % Process the contents of des.cnf with no output
  display_banner, 
  process_batch. % If des.ini exists, their entries are processed as command prompt inputs
  
set_start_path :-
  (start_path(D)
   -> 
    cd_path(D)
   ; 
    my_working_directory(D), 
    assertz(start_path(D))).
    
% Entry execution point
des :-
  repeat, 
    catch(exec_des(Continue), M, (my_exception_handling(M), complete_pending_tasks(M), nl_tapi_log)), 
    ((M == '$aborted'
      ;
      M == 'control_c' 
      ;
      Continue==no
     )
     ->
      true
     ;
      fail),
  !.

% Completing pending tasks upon exceptions
% WARNING: Other views/rules might be removed upon exceptions. Hint: Use answer as a view and follow the dependencies
complete_pending_tasks(des_exception(syntax(_Message))) :- % Nothing to do for syntax errors. WARNING: Maybe pending compilations in WITH SQL statements
  my_odbc_dangling_query_close,
  !,
%  (tapi(on) -> true ; nl_compact_log),
  retract_prototype_views,
  restore_state_flags,
  reset_syntax_error,
  !.
complete_pending_tasks(_Message) :-
  my_odbc_dangling_query_close,
  (retract(last_autoview(V))
   ->
    catch(
     (get_object_dlrules(rule,V,OVDLs),
      retract_dlrule_list(OVDLs,_Error2)),
     _M,
     true)
   ;
    true),
  retract_hyp_programs_k,
  retract_prototype_views,
  (recompute_pdg(no)
   ->
    true
   ;
    clear_et,
  % WARNING:
    catch(compute_stratification, NM, my_exception_message_display(error,NM)),
    set_flag(recompute_pdg(yes))
  ),
%  (tapi(on) -> true ; nl_compact_log),
  restore_state_flags,
  reset_syntax_error,
  !.
  
retract_prototype_views :-
  my_view('$des',Name,Arity,SQLst,_,_,_,_,_),
  var(SQLst),
  my_retract_all_facts(my_view('$des',Name,Arity,SQLst,_,_,_,_,_)),
  my_retract_all_facts(my_table('$des',Name,Arity)),
  my_retract_all_facts(my_attribute('$des',_,Name,_,_)),
  fail.
retract_prototype_views.

% Exception handling
my_exception_handling(M) :- 
   (consult(_,CSt)
    ->    % Closes the program file currently loading, if any
     retractall(consult(_,_)),
     try_close(CSt)
    ; 
     true), 
  (
   M = des_exception(DESM)  % If thrown by des, it is already displayed
   ->
    (DESM == user_break
     ->
      nl_compact_log,
      write_log('Info: User break. Exit to top-level? (y/n) [n]: '),
      flush_output,
      user_input_string(Str),
      ((Str=="y" ; Str=="Y") ->
        throw('$aborted')
       ;
        true
      )
     ;
      true
    )
   ;
    my_exception_message_display(error,M)
   ),
  repoint_batch_file.

% Display exception messages
my_exception_message_display(_,'$aborted') :- 
  !.
% If thrown by des, it is already displayed:
my_exception_message_display(_,des_exception(_)) :- 
  !.
% An ODBC error:
my_exception_message_display(_,M) :-
  my_display_odbc_error(M),
  !.
% Other:
my_exception_message_display(_,M) :-
  write_log_list(['Exception: ',M,nl]).

% Relocating the pointer to the batch file
repoint_batch_file :-
  current_batch_ID(ID),
  !, 
   batch(ID,L,F,S),
  (my_current_stream(S) -> try_close(S);true),
  open(F,read,S1),
  set_input(S1),
  retractall(batch(ID,_,_,_)),
  assertz(batch(ID,L,F,S1)), 
  my_skip_line(L).       % Skip the offending line
repoint_batch_file.

current_batch_ID(ID) :-
  current_batch_ID(-1,ID).
  
current_batch_ID(ID,CID) :-  
  ID1 is ID+1,
  batch(ID1,_,_,_),
  !,
  current_batch_ID(ID1,CID).
current_batch_ID(ID,ID) :-
  ID>=0.  
  
my_skip_line(0) :- 
  !.
my_skip_line(N) :- 
  readln(_,_),
  N1 is N-1,
  my_skip_line(N1).
  

% Processing the configuration file des.cnf at the system start path
% Display no output
process_conf :-
  my_start_path_absolute_file_name('des.cnf',F),
  my_file_exists(F),
  !,
  processC(output,[off],_,yes),
  process_batch_type(F,'configuration batch'),
  processC(output,[on],_,yes).
process_conf.

% Processing the exit file des.out at the distribution directory
process_out :-
  my_start_path_absolute_file_name('des.out',F),
  my_file_exists(F),
  !,
  process_batch_type(F,'exit batch').
process_out.

% Processing the batch file des.ini at the distribution directory
process_batch :-
  my_start_path_absolute_file_name('des.ini',F),
  my_file_exists(F),
  !,
  process_batch_type(F,'initialization batch').
process_batch.

process_batch(F,_Error) :-
  my_file_exists_with_default_extensions(F,['.sql','.ra','.drc','.trc','.ini'],FP),
  process_batch_type(FP,'file'),
  !.
process_batch(F,true) :-
  write_error_log(['When processing file ''',F,'''']).

my_file_exists_with_default_extensions(F,_Exts,CF) :-
  (my_file_exists(F)
   ->
    CF=F
   ;
    my_working_directory(D),
    atom_concat(D,F,CF),
    my_file_exists(CF)
  ),
  !.
my_file_exists_with_default_extensions(F,Exts,FP) :- 
  my_file_exists_with_default_extension_list(F,Exts,FP).

my_file_exists_with_default_extension_list(F,[],_FP) :-  
  write_error_log(['File ''',F,''' not found.']), 
  !,
  fail.
my_file_exists_with_default_extension_list(F,[Ext|_Exts],FP) :-  
  atom_concat(F,Ext,FExt),
  (my_file_exists(FExt)
   ->
    FP=FExt
   ;
    my_working_directory(D),
    atom_concat(D,FExt,FP),
    my_file_exists(FP)
  ),
  !.
my_file_exists_with_default_extension_list(F,[_Ext|Exts],FP) :-  
  my_file_exists_with_default_extension_list(F,Exts,FP).

    
process_batch_type(F,M) :-
  my_absolute_filename(F,CFN),
  current_input(OldInput),   % Current input stream
  open(CFN,read,BSt), % Open initialization batch
  set_input(BSt),
  new_batch_id(ID),
  assertz(batch(ID,0,CFN,BSt)),
  silent(SFlag),
  (SFlag==off
   ->
    write_info_log(['Processing ',M,' ''',F,''' ...']),
    nl_compact_log
   ;
    true
  ), 
  process_batch_file_lines(ID,OldInput).

process_batch_file_lines(ID,OldInput) :-
  repeat,
    read_input_str(S,E,Lines),
    inc_lines(ID,Lines),
    write_silent(S),
    catch(process_input(S,_), M, (my_exception_handling(M), complete_pending_tasks(M), nl_tapi_log)), 
    E == end_of_file,     % Repeat until end of file
  !,
  batch(ID,_,_,BSt),
  set_input(OldInput),    % Restore current input stream
  try_close(BSt),         % Close the input batch file
  retractall(batch(ID,_,_,_)),
  silent(SFlag),
  (SFlag==off
   ->
    write_info_log(['Batch file processed.']),
    nl_compact_log
   ;
    true
  ).
      
write_silent(S) :-
  my_kw("/SILENT ",S,_),
  !.
write_silent(S) :-
  write_prompt,
  write_string_log(S), 
  nl_log, 
  flush_output.
    
new_batch_id(ID) :-
  new_batch_id(0,ID).

new_batch_id(ID,NID) :-
  batch(ID,_,_,_),
  ID1 is ID+1,
  new_batch_id(ID1,NID).
new_batch_id(ID,ID).
  
inc_lines(ID,N) :-
  retract(batch(ID,L,F,S)),
  L1 is L+N,
  assertz(batch(ID,L1,F,S)).
  
/*********************************************************************/
/* Informative banner                                                */
/*********************************************************************/

display_banner :-
  display_banner(off),
  !.
display_banner :-
  write_log('*********************************************************'), nl_log,
  write_log('*                                                       *'), nl_log,
  write_log('*        DES: Datalog Educational System v.4.1          *'), nl_log,
  write_log('*                                                       *'), nl_log,
  write_log('* Type "/help" for help about commands                  *'), nl_log,
  write_log('*                                                       *'), nl_log,
  write_log('*                    Fernando Saenz-Perez (c) 2004-2016 *'), nl_log,
  write_log('*                                         DISIA GPD UCM *'), nl_log,
  write_log('*             Please send comments, questions, etc. to: *'), nl_log,
  write_log('*                                     fernan@sip.ucm.es *'), nl_log,
  write_log('*                                             Web site: *'), nl_log,
  write_log('*                           http://des.sourceforge.net/ *'), nl_log,
  write_log('*                                                       *'), nl_log,
  write_log('* This program comes with ABSOLUTELY NO WARRANTY, is    *'), nl_log,
  write_log('* free software, and you are welcome to redistribute it *'), nl_log,
  write_log('* under certain conditions. Type "/license" for details *'), nl_log,
  write_log('*********************************************************'), nl_log, nl_log.


/*********************************************************************/
/* Datalog Prompt                                                    */
/*********************************************************************/

exec_des(Continue) :-
  write_prompt,
  flush_output, 
  read_input_str(S,E,_L),
% The following is needed for SWI-Prolog in order to avoid failure
% when closing the application in Windows  
  (E == end_of_file 
   ->
    Continue = no
   ;
    write_only_to_log(S),
    nl_only_to_log,
    process_input(S,Continue)
  ),
  !.
% exec_des(yes) :-
%   write_error_log(['Input processing error.']),
%   nl_tapi_log.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prompt display
 
% Workaround to disable
% :-dynamic acide/1.
% acide(off).

write_prompt :- 
  tapi(on),
  !,
  set_flag(tapi,off).
% write_prompt :- 
%   tapi(on),
%   !,
%   set_flag(tapi,off),
%   set_flag(acide,on).
% write_prompt :- 
%   acide(on),
%   !,
%   write_log('DES> ').
write_prompt :-
  prompt(no),
  !. 
write_prompt :-
  prompt(plain),
  !,
  write_log('> '). 
write_prompt :-
  prompt(prolog),
  !,
  write_log('?- '). 
% write_prompt :-
%   system_mode(hrsql),
%   !,
%   (prompt(des) 
%    ->
%     DB=''
%    ;
%     hrsql_connection(Connection),
%     my_odbc_get_dbms(Connection,LRDBMS),
%     ldbms_dbms(LRDBMS,RDBMS),
%     (prompt(des_db),
%      current_db(CurrConnection),
%      CurrConnection\=='$des'
%      ->
%       atom_concat(':',CurrConnection,Conn)
%      ;
%       Conn=''
%     ),
%     atom_concat_list(['(',RDBMS,Conn,')'],DB)
%   ),
%   atom_concat_list(['HR-SQL',DB,'> '],P),
%   write_log(P).
write_prompt :-
  prompt(des),
  !,
  language_prompt(L),
  atom_concat_list(['DES',L,'> '],P),
  write_log(P).
write_prompt :-
  prompt(des_db),
  !,
  language_prompt(L),
  current_db(DB),
  atom_concat_list(['DES:',DB,L,'> '],P),
  write_log(P).

% language_prompt('-HR-SQL') :-  
%   system_mode(hrsql),
%   !.
language_prompt('') :-  
  language(datalog),
  !.
language_prompt(Prompt) :-
  language(L),
  language_acronym(L,Lang),
  atom_concat('-',Lang,Prompt).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parameter vector
:-dynamic('$parv$'/3).

:-dynamic(curr_parv/1).

reset_param_vector :-
  reset_param_vector_tail(1).

display_param_vector :-
  listing('$parv$').

current_param_vector(N) :-
  curr_parv(N),
  !.
current_param_vector(0) :-
  set_flag(curr_parv(0)).
  
param_vector_entry_value(Entry,Value) :-
  atomic_concat('$parv',R,Entry),
  atomic_concat(R,'$',AI),
  atom_number(AI,I),
  current_param_vector(N),
  '$parv$'(N,I,Value).
  
set_param_vector_entry_value(Entry,Value) :-
  atomic_concat('$parv',R,Entry),
  atomic_concat(R,'$',AI),
  atom_number(AI,I),
  set_param_vector_i(I,Value).
  
push_param_vector :-
  current_param_vector(N),
  !,
  N1 is N+1,
  set_flag(curr_parv,N1).

pop_param_vector :-
  current_param_vector(N),
  N>0,
  !,
  retractall('$parv$'(N,_,_)),
  N1 is N-1,
  set_flag(curr_parv,N1).
% pop_param_vector :-
%   current_param_vector(N),
%   write_warning_log(['Parameter ',N,' not defined.']).
pop_param_vector :-
  my_raise_exception(true,'Parameter vector is not defined in this context.',[]).

set_param_vector([]) :-
  !.
set_param_vector(Ps) :-
  set_param_vector(Ps,1).
  
set_param_vector([],_I).
set_param_vector([P|Ps],I) :-
  set_param_vector_i(I,P),
  I1 is I+1,
  set_param_vector(Ps,I1).

set_param_vector_i(I,P) :-
  current_param_vector(N),
  assertz('$parv$'(N,I,P)).

param_vector_i(I,Value) :-
  current_param_vector(N),
  '$parv$'(N,I,Value).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read_input_str(-String,-EOI,-Lines). 
% Read an input string from the current input stream
% (console, by default). Informs whether the line ends with an
% 'end of file' or 'end of line' character (end of input). 
% Return the number of lines in the input.
% Call to readln, if multiline is disabled
% Call to read_input/3, if multiline is enabled
% An input ends with:
% - EOL, if a command (starting with /)
% - A dot(.)+EOL, if Datalog
% - A semicolon(;)+EOL, if either SQL or RA
% Batch variables are substituted by batch parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_input_str(String,Error,1) :-
  multiline(off),
  !,
  readln(SString,Error),
  replace_system_variables(SString,String). 
read_input_str(String,Error,Lines) :-
  read_input(SString,Error,Lines),
  !,
  replace_system_variables(SString,String).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


replace_system_variables(Str,RStr) :-
  % Replace only batch parameters as system flags may change along iterations
  my_command_input(Str,repeat),
  !,
  replace_batch_parameters(RStr,Str,""),
  !.
replace_system_variables(Str,RStr) :-
  % Replace both batch parameters and system flags
  replace_system_variables(RStr,Str,""),
  !.
 
        
replace_batch_parameters(OutStr) -->
  parv(Str),
  !,
  replace_batch_parameters(TailStr),
  {append(Str,TailStr,OutStr)}.
replace_batch_parameters([C|OutStr]) -->
  [C],
  replace_batch_parameters(OutStr).
replace_batch_parameters([]) -->
  [].

replace_system_variables(OutStr) -->
  parv(Str),
  !,
  replace_system_variables(TailStr),
  {append(Str,TailStr,OutStr)}.
replace_system_variables(OutStr) -->
  system_var(V),
  {replace_system_flag(V,I),
   my_term_to_string(I,StrI)},
  !,
  replace_system_variables(TailStr),
  {append(StrI,TailStr,OutStr)}.
replace_system_variables([C|OutStr]) -->
  [C],
  replace_system_variables(OutStr).
replace_system_variables([]) -->
  [].

replace_system_flags(OutStr) -->
  system_var(V),
  {replace_system_flag(V,I),
   my_term_to_string(I,StrI)},
  !,
  replace_system_flags(TailStr),
  {append(StrI,TailStr,OutStr)}.
replace_system_flags([C|OutStr]) -->
  [C],
  replace_system_flags(OutStr).
replace_system_flags([]) -->
  [].

parv(Str) -->
  "$parv",
  my_positive_integer(I),
  "$",
  {
   (param_vector_i(I,Str)
    ->
     true
    ;
     write_warning_log(['Parameter $parv',I,'$ has not been passed to this script.']),
     Str=""
   )
  }.
%      my_raise_exception(generic,syntax(['Parameter $parv',I,'$ has not been passed to this script.']),[]))}.

%replace_system_flag('$$',0).
replace_system_flag('$stopwatch$',CT) :-
  !,
  current_stopwatch_time(CT).
replace_system_flag('$last_stopwatch$',CT) :-
  !,
  last_stopwatch_stop(CT).
replace_system_flag('$total_elapsed_time$',Total) :-
  !,
  last_elapsed_time(_Parsing,_Computation,_Display,Total).
replace_system_flag('$computation_time$',Computation) :-
  !,
  last_elapsed_time(_Parsing,Computation,_Display,_Total).
replace_system_flag('$parsing_time$',Parsing) :-
  !,
  last_elapsed_time(Parsing,_Computation,_Display,_Total).
replace_system_flag('$display_time$',Display) :-
  !,
  last_elapsed_time(_Parsing,_Computation,Display,_Total).
replace_system_flag(DVarD,Value) :-
  atom_concat('$',VarD,DVarD),
  atom_concat(Var,'$',VarD),
  Flag=..[Var,Value],
  clause(Flag,true).
%  (call(Flag) -> true ; Value='**ERROR**').
  
% system_var('$$') -->
%   "$$".
system_var('$stopwatch$') -->
  "$stopwatch$",
  !.  
system_var('$total_elapsed_time$') -->
  "$total_elapsed_time$",
  !.  
system_var(_C) -->
  "$parv",
  {!,
   fail}.
system_var(C) -->
  "$",
  my_chars_but_dollar(Cs),
  "$",
  {concat_lists(["$",Cs,"$"],CDs),
   atom_codes(C,CDs)}.  
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read_input(-String,-EOF,-Lines). Read an input from the current input stream
% (console, by default). Informs whether the line ends with an
% 'end of file' or 'end of line' character. Returns the number of lines in the input
% An input ends with:
% - EOL, if a command (starting with '/')
% - EOL, if a remark (starting with either '%' or '--', or '/*')
% - A dot(.)+EOL, if Datalog
% - A semicolon(;)+EOL, if SQL, TRC or DRC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_input(Str,E,L) :-
  read_input(Str,E,0,L).

read_input(Str,E,Li,Lo) :-
  current_input(HIn),
  read_skip_non_tokens(HIn,C,E,Li,Li1),
  ([C] == "%"
   ->
    readln(S,E),
    Lo is Li1+1,
    Str=[C|S]
   ;
    ([C] == "-"
     ->
      my_get0(HIn,C1),
      (C1 == C
       ->
        readln(S1,E),  % An SQL -- remark
        Lo is Li1+1,
        S=[C1|S1],
        Str=[C|S]
       ;
        "."=[EOI],     % A minus followed by something different from a minus (it should be a Datalog input)
        read_EOI_chars(HIn,EOI,C1,S1,E,Li1,Lo),
        S=[C1|S1],
        Str=[C|S]
      )
     ;
      ((end_of_file(C);C==end_of_file)
       ->     % End of file reached
        Str=[],
        Lo=Li1,
        E=C
       ;      % Else, either a Datalog, or RA or SQL input
       ([C] == "."
        ->    
         my_get0(HIn,C1),
         (end_of_line(C1)
          ->  % Empty input ended with a dot
           Str=[C],
           Lo=Li1
          ;
           read_up_to_EOI2(HIn,[C,C1],S,E,Li1,Lo),
           Str=[C,C1|S]
         )
        ;
         read_up_to_EOI2(HIn,[C],S,E,Li1,Lo),
         Str=[C|S]
       )
      )
    )
  ).

% Unknown terminator
read_up_to_EOI2(HIn,[Slash],[C|S],E,Li,Lo) :-
  [Slash]="/",
  my_get0(HIn,C),
  ([C]=="*"
   ->             % Multiline remark
     my_get0(HIn,C1),
     nbr_input_lines(C1,LC),
     Li1 is Li+LC,
     read_up_to_end_of_multiline_remark(HIn,C1,S,E,Li1,Lo,1,_R)
   ;
    readln(S,E),   % Command
    Lo is Li+1
  ).
read_up_to_EOI2(HIn,Cs,S,E,Li,Lo) :-
  my_get0(HIn,C),
  nbr_input_lines(C,LC),
  Li1 is Li+LC,
  append(Cs,[C],CCs),
  read_EOI_chars2(HIn,C,CCs,S,E,Li1,Lo).
  
%read_EOI_chars2(+HIn,+C,+Cs,-S,-E,+Li,-Lo)
read_EOI_chars2(_HIn,C,_Cs,[],end_of_file,L,L) :-
  end_of_file(C),
  !.
read_EOI_chars2(HIn,C,Cs,[C|S],E,Li,Lo) :-
  ";" == [C],
  my_guessed_sql_statement(Cs,_),
  !,
  read_EOI_chars(HIn,C,C,S,E,Li,Lo).
read_EOI_chars2(HIn,C,Cs,[C|S],E,Li,Lo) :-
  ";" == [C],
  my_guessed_ra_statement(Cs,_),
  !,
  read_EOI_chars(HIn,C,C,S,E,Li,Lo).
read_EOI_chars2(HIn,C,Cs,[C|S],E,Li,Lo) :-
  ";" == [C],
  my_guessed_drc_statement(Cs,_),
  !,
  read_EOI_chars(HIn,C,C,S,E,Li,Lo).
read_EOI_chars2(HIn,C,Cs,[C|S],E,Li,Lo) :-
  ";" == [C],
  my_guessed_trc_statement(Cs,_),
  !,
  read_EOI_chars(HIn,C,C,S,E,Li,Lo).
read_EOI_chars2(HIn,C,Cs,[C|S],E,Li,Lo) :-
  "." == [C],
  \+ my_guessed_sql_statement(Cs,_),
  \+ my_guessed_ra_statement(Cs,_),
  \+ my_guessed_drc_statement(Cs,_),
  \+ my_guessed_trc_statement(Cs,_),
  !,
  read_EOI_chars(HIn,C,C,S,E,Li,Lo).
read_EOI_chars2(HIn,C,Cs,[C|S],E,Li,Lo) :-
  read_up_to_EOI2(HIn,Cs,S,E,Li,Lo).


% Nested remarks
read_up_to_end_of_multiline_remark(_HIn,C,[],end_of_file,L,L,R,R) :-
  end_of_file(C),
  !.
read_up_to_end_of_multiline_remark(HIn,C,S,E,Li,Lo,Ri,Ro) :-
  [C]=="/",
  !,
  my_get0(HIn,C1),
  ([C1]=="*"
   ->
    Ri1 is Ri+1,
    my_get0(HIn,C2),
    nbr_input_lines(C2,LC),
    Li1 is Li+LC,
    read_up_to_end_of_multiline_remark(HIn,C2,S1,E,Li1,Lo,Ri1,Ro),
    S=[C,C1|S1]
   ;
    nbr_input_lines(C1,LC),
    Li1 is Li+LC,
    S=[C,C1|S1],
    read_up_to_end_of_multiline_remark(HIn,C1,S1,E,Li1,Lo,Ri,Ro)
  ).
% End of multiline remark
read_up_to_end_of_multiline_remark(HIn,C,S,E,Li,Lo,Ri,Ro) :-
  [C]=="*",
  !,
  my_get0(HIn,C1),
  ([C1]=="/"
   ->
    S=[C,C1|S1],
    Ri1 is Ri-1,
    (Ri1=<0
     ->  % All multiline remarks are closed
      read_after_EOR(HIn,S1,E,Li,Lo)
     ;   % There are pending multiline remarks to close
      my_get0(HIn,C2),
      nbr_input_lines(C2,LC),
      Li1 is Li+LC,
      read_up_to_end_of_multiline_remark(HIn,C2,S1,E,Li1,Lo,Ri1,Ro)
    )
   ;
    nbr_input_lines(C1,LC),
    Li1 is Li+LC,
    S=[C,C1|S1],
    read_up_to_end_of_multiline_remark(HIn,C1,S1,E,Li1,Lo,Ri,Ro)
  ).
% Read chars inside a multiline remark
read_up_to_end_of_multiline_remark(HIn,C,[C|S1],E,Li,Lo,Ri,Ro) :-
  my_get0(HIn,C1),
  nbr_input_lines(C1,LC),
  Li1 is Li+LC,
  read_up_to_end_of_multiline_remark(HIn,C1,S1,E,Li1,Lo,Ri,Ro).
  
% Read after an end of (multiline) remark    
read_after_EOR(_HIn,S,E,Li,Lo) :-
  readln(S,E),
  Lo is Li+1.

% Read up to end of input (a char code EOI).
% Return the input string
% Known terminator
read_up_to_EOI(HIn,EOI,Cs,E,Li,Lo) :-
  my_get0(HIn,C),
  read_EOI_chars(HIn,EOI,C,Cs,E,Li,Lo).
  
%read_EOI_chars(+HIn,+EOI,+C,-Cs,-E)
% Known terminator EOI
% First clause: 
% - EOI reached, skip blanks and get EOI
% - If EOI not found, continue reading
% Second clause: 
% read_EOI_chars(HIn,EOI,EOI,Cs,E,Li,Lo) :-
%   !,
%   read_skip_blanks(HIn,Bs,C),
%   (my_end_of_input(C,E)
%    ->
%     Lo is Li+1, 
%     Cs=[]
%    ;
%     read_up_to_EOI(HIn,EOI,TCs,E,Li,Lo),
%     append([C|Bs],TCs,Cs)
%   ).
read_EOI_chars(_HIn,_,C,[],end_of_file,L,L) :-
  end_of_file(C),
  !.
read_EOI_chars(HIn,EOI,EOI,Cs,E,Li,Lo) :-
  !,
  readln(S,E),
  (nonvar(E)
   ->
    Lo is Li+1, 
    Cs=S
   ;
    read_up_to_EOI(HIn,EOI,TCs,E,Li,Lo),
    append(S,TCs,Cs)
  ).
% read_EOI_chars(HIn,EOI,C,S,E,Li,Lo) :- % SQL
%   [EOI]==";",
%   !,
%   my_get0(HIn,C1),
%   ([C,C1]=="--"
%    ->
%     readln(S,E)
%    ;
%     true),
%   S=[C,C1|Cs],
%   read_up_to_EOI(HIn,EOI,Cs,E,Li,Lo).
read_EOI_chars(HIn,EOI,C,[C|Cs],E,Li,Lo) :- % Datalog
  read_up_to_EOI(HIn,EOI,Cs,E,Li,Lo).


% Return the first non-blank character read from input
% read_skip_blanks(HIn,C) :-
%   read_skip_blanks(HIn,_Bs,C).

% Return the first non-blank character read from input 
% and the blanks as a string
% read_skip_blanks(HIn,Bs,C) :- 
%   my_get0(HIn,GC),
%   ([GC]==" "
%    ->
%     Bs=[GC|TBs],
%     read_skip_blanks(HIn,TBs,C)
%    ;
%     C=GC,
%     Bs=[]).
  
% Return the first token character read from input 
% (skip blanks, tabs, line feeds and carriage returns)
read_skip_non_tokens(HIn,C,E,Li,Lo) :-
  my_get0(HIn,GC),
  (([GC]==" "
    ;
    [GC]=="\t")
   ->
    read_skip_non_tokens(HIn,C,E,Li,Lo)
   ;
    (my_end_of_input(GC,E1)
     ->
      (E1==end_of_line
       ->
        Li1 is Li+1,
        read_skip_non_tokens(HIn,C,E,Li1,Lo)
       ; % End of file
        Lo is Li+1,
        C=E1
      )
     ;
      C=GC,
      Lo=Li
    )
  ).

% Number of lines for a given character
% end_of_line and end_of_file amount 1 line
% Other characters amount 0 lines  
nbr_input_lines(C,1) :-
  end_of_line(C),
  !.
nbr_input_lines(C,1) :-
  end_of_file(C),
  !.
nbr_input_lines(_C,0).

% End of input: either end_of_line or end_of_file  
my_end_of_input(C,end_of_line) :-
  end_of_line(C),
  !. 
my_end_of_input(C,end_of_file) :-
  end_of_file(C),
  !. 
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% readln(-String,-EOF). Read a line from the current input stream
% (console, by default). Informs whether the line ends with an
% 'end of file' or 'end of line' character.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

readln(S,E) :-
  current_input(HIn),
  readln(HIn,S,E).

readln(HIn,Cs,E) :-
  my_get0(HIn,C),
  read_chars(HIn,C,Cs,E).

read_chars(_HIn,C,[],end_of_line) :-
  end_of_line(C), 
  !.
read_chars(_HIn,C,[],end_of_file) :-
  end_of_file(C), 
  !.
read_chars(HIn,C,[C|Cs],E) :-
  readln(HIn,Cs,E).

end_of_line(C) :-
  (C = 13
   ;
   C = 10),
  my_skip_line.

end_of_file(C) :-
  (C = 26
   ;
   C = -1).

/******************************************************************************/
/* Compiling to Datalog from other languages (SQL, RA, ...)                   */
/******************************************************************************/

compile_to_dl(Lang,ST,Schema,DLsts) :-
  compile_to_dl(Lang,ST,Schema,DLsts,_NVs).
  
compile_to_dl(hrsql,SQLst,Schema,DLsts,[]) :- % Support for HR-SQL syntax
  compile_to_dl(sql,SQLst,Schema,DLsts).
compile_to_dl(sql,SQLst,Schema,DLsts,[]) :-
  sql_to_dl(sql,(SQLst,_AS),Schema,_TableRen,DLsts).
compile_to_dl(ra,RAst,Schema,DLsts,[]) :-
  ra_to_sql(RAst,(SQLst,AS)),
  display_compiled_ra((SQLst,AS)),
  sql_to_dl(ra,(SQLst,AS),Schema,_TableRen,DLsts).
compile_to_dl(drc,DRCst,_Schema,[DLst],NVs) :-
  drc_to_dl(DRCst,DLst,NVs).
compile_to_dl(trc,TRCst,Schema,[DLst],NVs) :-
  trc_to_drc(TRCst,DRCst),
  drc_to_dl(DRCst,DLst,NVs),
  trc_schema(DLst,NVs,Schema).
  


/******************************************************************************/
/* Preprocessing                                                              */
/* preprocess(+(Rule,NVs),                                                    */
/*            -SimplifiedRuleNVsList,                                         */
/*            -SafedRuleNVsList,                                              */
/*            -ExplodedRuleNVsList,                                           */
/*            +InputArgs      % Arg. positions known to be ground at run-time */
/*            +InputArgsList  % List of arg. positions inferred to be ground  */
/*                              at run-time for transformed rules             */
/*            -Modes,         % Root rule modes (i,o) if unsafe               */
/*            +Action,        % 'exec','assert','consult'                     */
/*            +Origin,        % 'datalog','sql'                               */
/*            +Object,        % 'view','autoview','rule'                      */
/*            -Causes,        % 'transformed','simplified','safed','exploded' */
/*            -Tasks,         % 'simplify' (force simplification),            */
/*                            % 'no_safety' (disable safety transformations)  */
/*                            % 'safety' (force safety transformations)       */
/*                            % 'reorder' (force equality left-pushing)       */
/*            -Unsafe)        % unsafe(Classic,SetVar) or var is safe         */
/******************************************************************************/

% Preprocessing consists of source-to-source transformations for:
% - 'distinct' calls
% - not(Primitive) -> Negated Primitive, as: not(is_null(G)) -> is_not_null(G)
% - Negated compound goals, as not((a,b)) and not((a;b))
% - Grouped aggregates
% - Disjunctive bodies
% - Outer joins. May deliver conditions which can be further simplified
% - Simplification of equality (=), true, not(true) and not(false) goals
%

preprocess(RuleNVs,SiRuleNVsList,SfRuleNVsList,ExRuleNVsList,IArgs,IArgsList,Modes,Action,Origin,Object,Causes,Tasks,Unsafe) :-
  preprocess_task(Tasks,simplify,Simplify),
  preprocess_task(Tasks,safety,Safety),
  preprocess_task(Tasks,no_safety,Safety),
  preprocess_task(Tasks,reorder,Reorder),
  preprocess_task(Tasks,replace_eqs,ReplaceEqs),
  preprocess_task(Tasks,unfold,Unfold),
  copy_term(RuleNVs,CRuleNVs),
  remove_underscored_head_variables_from_rule(CRuleNVs,Action,RCRuleNVs,Transformed),
%  simplify_ruleNVs_list([RCRuleNVs],[SRCRuleNVs],Simplify,Simplified00), % WARNING: IArgsList should be passed to this
  RCRuleNVs=SRCRuleNVs,
  replace_not_primitive(SRCRuleNVs,RNRuleNVs,Simplified0),
  translate_division_calls_list([RNRuleNVs],DivRuleNVsList,[IArgs],DivArgsList,Compiled0),
  translate_top_calls_list(DivRuleNVsList,TRuleNVsList,DivArgsList,NArgsList,Exploded0),
  translate_distinct_calls_list(TRuleNVsList,DRuleNVsList,NArgsList,DArgsList,Exploded1),
  translate_order_by_calls_list(DRuleNVsList,OBRuleNVsList,DArgsList,OArgsList,Exploded2),
  translate_negated_compound_calls_list(OBRuleNVsList,OBRuleNVsList1,OArgsList,NCArgsList1,Safety,Exploded3),
  translate_or_calls_list(OBRuleNVsList1,NCRuleNVsList,NCArgsList1,NCArgsList,Exploded33),
  translate_aggregates_ruleNVs_list(NCRuleNVsList,TARuleNVsList,NCArgsList,TAArgsList,Simplified1,Exploded4,Unsafe),
  translate_hypo_calls_list(TARuleNVsList,HRuleNVsList,TAArgsList,HIArgsList,Exploded5),
  disjunctive_to_conjunctive_ruleNVs_list(HRuleNVsList,CRuleNVsList,HIArgsList,CIArgsList,Exploded6),
% display_ruleNVs_list(CRuleNVsList,2),
  translate_exists_list(CRuleNVsList,NERuleNVsList,CIArgsList,NEArgsList,Safety,Exploded7),
% display_ruleNVs_list(NERuleNVsList,2),
  simplify_ruleNVs_list(NERuleNVsList,SiRuleNVsList1,Simplify,Simplified2), % WARNING: IArgsList should be passed to this
  language(Lang),
  ((Lang==drc ; Lang==trc) -> Reorder=reorder ; true),
  reorder_goals_by_efficiency_ruleNVs_list(SiRuleNVsList1,Reorder,SiRuleNVsList2),
  (Exploded7==true -> Unfold=unfold,SafetyExists=safety ; true),
  unfold_RNVss(SiRuleNVsList2,Unfold,NEArgsList,UIArgsList1,SiRuleNVsList30),
  translate_safe_negated_calls_list(SiRuleNVsList30,SiRuleNVsList3,UIArgsList1,UIArgsList,SafetyExists,_E),
  (Exploded6==true -> Object1 = view ; Object1 = Object),
% display_ruleNVs_list(SiRuleNVsList3,2),
  make_safe_list(SiRuleNVsList3,SfRuleNVsList1,UIArgsList,Modes,Action,Origin,Object1,Safety,Safed,Unsafe),
  translate_outer_joins_list(SfRuleNVsList1,ORuleNVsList,UIArgsList,TIArgsList,Compiled1),
  disjunctive_to_conjunctive_ruleNVs_list(ORuleNVsList,CRuleNVsList2,TIArgsList,IArgsList,_Exploded8),
  simplify_ruleNVs_list(CRuleNVsList2,ExRuleNVsList1,Simplify,Simplified3), % WARNING: IArgsList should be passed to this
  (ReplaceEqs==replace_eqs
   ->
    replace_functor('$eq','=',[SiRuleNVsList2,SfRuleNVsList1,ExRuleNVsList1],[SiRuleNVsList,SfRuleNVsList,ExRuleNVsList]) 
   ;
    SiRuleNVsList2=SiRuleNVsList,
    SfRuleNVsList1=SfRuleNVsList,
    ExRuleNVsList1=ExRuleNVsList),
%  translate_division_calls_list(CRuleNVsList2,DivRuleNVsList,DivArgsList,IArgsList,Compiled1),
%  simplify_ruleNVs_list(DivRuleNVsList,ExRuleNVsList,Simplify,Simplified3), % WARNING: IArgsList should be passed to this
  % WARNING: Use only one Boolean var. Explodedi -> Exploded
  (Transformed==true -> TCauses=[transformed] ; TCauses=[]),
%  ((Simplified00==true ; Simplified0==true ; Simplified1==true ; Simplified2==true ; Simplified3==true) -> SCauses=[simplified|TCauses] ; SCauses=TCauses),
  ((Simplified0==true ; Simplified1==true ; Simplified2==true ; Simplified3==true) -> SCauses=[simplified|TCauses] ; SCauses=TCauses),
  (Safed==true -> SfCauses = [safed|SCauses] ; SfCauses=SCauses),
  ((Compiled0==true ; Compiled1==true) -> CCauses=[compiled|SfCauses] ; CCauses=SfCauses),
  ((Exploded0==true ; Exploded1==true ; Exploded2==true ; Exploded3==true ; Exploded33==true ; Exploded4==true ; Exploded5==true ; Exploded6==true ; Exploded7==true) -> Causes = [exploded|CCauses] ; Causes=CCauses).

dont_safe_negated_calls(NG,_Rule,_NVs,_Safety) :-
  my_ground(NG),
  !.
dont_safe_negated_calls(NG,_Rule,NVs,_Safety) :-
  underscored_variables(NG,NVs,[_|_]),
  !,
  fail.
dont_safe_negated_calls(_NG,_Rule,_NVs,Safety) :-
  (safe(off) ; Safety==no_safety),
  !.
dont_safe_negated_calls(NG,_Rule,_NVs,_Safety) :-
  my_ground(NG),
  !.
% dont_safe_negated_calls(NG,Rule,_NVs,_Safety) :-
%   rule_head(Rule,Head),
%   shared_variables(NG,Head,[_|_]),
%   !.
dont_safe_negated_calls(NG,Rule,_NVs,_Safety) :-
  replace_term(NG,true,Rule,RRule),
  shared_variables(NG,RRule,[_|_]),
  !.

shared_variables(X,Y,Vs) :-
  term_variables(X,VXs),
  term_variables(Y,VYs),
  my_set_inter(VXs,VYs,Vs).

% Remove underscored variables from queries
remove_underscored_head_variables_from_rule((H,NVs),assert,(H,NVs),_T) :- 
  !.
remove_underscored_head_variables_from_rule((':-'(H,B),NVs),_Action,(':-'(TH,B),NVs),T) :- 
  !,
  remove_underscored_head_variables_from_head(H,NVs,TH),
  (H==TH -> true ; T=true).
remove_underscored_head_variables_from_rule((H,NVs),_Action,(TH,NVs),T) :- 
  remove_underscored_head_variables_from_head(H,NVs,TH),
  (H==TH -> true ; T=true).

remove_underscored_head_variables_from_head(Head,NVs,NHead) :-
  Head=..[F|Args],
  remove_underscored_variables_list(Args,NVs,NArgs),
  NHead =.. [F|NArgs].
  
remove_underscored_variables_list([],_NVs,[]).
remove_underscored_variables_list([V|Args],NVs,NArgs) :-
  var(V),
  find_var_name(V,N,NVs),
  atom_concat('_',_,N),
  !,
  remove_underscored_variables_list(Args,NVs,NArgs).
remove_underscored_variables_list([Arg|Args],NVs,[Arg|NArgs]) :-
  remove_underscored_variables_list(Args,NVs,NArgs).

underscored_variables(T,NVs,Vs) :-
  term_variables(T,TVs),
  underscored_variables(TVs,NVs,Vs,[]).

underscored_variables([],_NVs) -->
  [].
underscored_variables([V|Vs],NVs) -->
  {var(V),
   find_var_name(V,N,NVs),
   atom_concat('_',_,N),
   !},
  [V],
  underscored_variables(Vs,NVs).
underscored_variables([_V|Vs],NVs) -->
  underscored_variables(Vs,NVs).

no_input_arguments_list(DLs,IArgsList) :-
  length(DLs,N),
  length(IArgsList,N),
  my_member_chk([],IArgsList).

preprocess_task(Tasks,Task,Task) :-
  member(Task,Tasks),
  !.
preprocess_task(_Tasks,_Task,_Var).

% Replaces all occurrences of not(is_null(S)) by is_not_null(S) in a term T
% not(null(T)) has to be avoided because of computation by strata
% Returns true on change
replace_not_primitive(T,T,_Tr) :- 
%  (number(T) ; var(T) ; atom(T)),
  (var(T) ; atomic(T)),
  !.
replace_not_primitive(not(not(T)),RT,true) :- 
  !,
  replace_not_primitive(T,RT,_).
replace_not_primitive(not(is_null(T)),is_not_null(T),true) :- 
  !.
replace_not_primitive(not(is_not_null(T)),is_null(T),true) :- 
  !.
replace_not_primitive(not(G),NotG,true) :-
  G=..[DLop,L,R],
  complement_DL_op(DLop,CDLop),
  !,
  NotG=..[CDLop,L,R].
replace_not_primitive(C,RC,Tr) :- 
  C =.. [F|As],
  !, 
  replace_not_primitive_list(As,RAs,Tr),
  RC =.. [F|RAs].

replace_not_primitive_list([],[],_Tr) :-
  !.
replace_not_primitive_list([T|Ts],[RT|RTs],Tr) :-
  !, 
  replace_not_primitive(T,RT,Tr), 
  replace_not_primitive_list(Ts,RTs,Tr).

% complemented DL operator
complement_DL_op('=<','>').
complement_DL_op('=','\\=').
complement_DL_op('\\=','='). 
complement_DL_op('>=','<').
complement_DL_op('>','=<').
complement_DL_op('<','>=').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translating division calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% translate_division_calls_list(+RuleNVsList,-DRuleNVsList,+IArgsList,-DArgsList,-Exploded)
translate_division_calls_list(RuleNVsList,DRuleNVsList,IArgsList,DArgsList,Exploded) :-
  translate_division_calls_list(RuleNVsList,IArgsList,[],[],DRuleNVsList,DArgsList,Exploded).
  
% translate_division_calls_list(+RuleNVsList,+IArgsList,+DRuleNVsList1,+DIArgsList1,-DRuleNVsList2,-DIArgsList2,-Exploded)
translate_division_calls_list([],[],DRuleNVsList,DIArgsList,DRuleNVsList,DIArgsList,_Exploded).
translate_division_calls_list([RuleNVs|RuleNVsList],[IArgs|IArgsList],DRuleNVsListi,DIArgsListi,DRuleNVsListo,DIArgsListo,Exploded) :-
  translate_division_calls_RNVs(RuleNVs,IArgs,DRuleNVsList1,DIArgsList1,Exploded),
  translate_division_calls_list(RuleNVsList,IArgsList,DRuleNVsList1,DIArgsList1,DRuleNVsList2,DIArgsList2,Exploded),
  append(DRuleNVsListi,DRuleNVsList2,DRuleNVsListo),
  append(DIArgsListi,DIArgsList2,DIArgsListo).

translate_division_calls_RNVs((Rule,NVs),IArgs,[(TRule,TNVs)|NCRuleNVsList],[IArgs|IArgsList],Exploded) :-
  Rule = ':-'(Head,Body),
  !,
  TRule = ':-'(THead,TBody),
  term_variables(Head,LVs),
  translate_division_calls_term(Body,TBody,NVs,LVs,[],IArgs,Exploded,[],NCRuleNVsList,[],IArgsList),
  Head=..[F|Args],
  (F==answer,
   Body=division(_,_) %, Exploded==true
   ->
    term_variables(TBody,TVs),
    my_set_inter(LVs,TVs,THVs),
    remove_vars_not_in(Args,THVs,TUArgs),
    remove_underscored_variables_list(TUArgs,NVs,TArgs),
    THead=..[F|TArgs],
%    append(LVs,TVs,RVs),
    my_union_var(TVs,LVs,RVs),
    my_var_name_list(RVs,NVs,TNVs)
   ;
    THead=Head,
    TNVs=NVs).
translate_division_calls_RNVs((Rule,NVs),IArgs,[(Rule,NVs)],[IArgs],_Exploded).

translate_division_calls_term(T,T,_NVs,_LVs,_RVs,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :- 
%  (number(T) ; var(T) ; atom(T)),
  (var(T) ; atomic(T)),
  !.
translate_division_calls_term(division(LG,RG),H,NVs,_LVs,_RVs,_IArgs,true,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
  !,
%  my_set_union(LVs,RVs,CVs),
%  my_set_inter(CoVs,CVs,Vs),
  translate_division_calls_term(LG,TLG,NVs,[],[],[],_,RuleNVsListi,RuleNVsListo1,IArgsListi,IArgsListo1),
  translate_division_calls_term(RG,TRG,NVs,[],[],[],_,RuleNVsListo1,RuleNVsListo2,IArgsListo1,IArgsListo2),
  relevant_vars(TLG,ALGVs),
  relevant_vars(TRG,ARGVs),
  remove_anonymous_vars(ALGVs,NVs,LGVs),
  remove_anonymous_vars(ARGVs,NVs,RGVs),
  division_compatible_vars(division(TLG,TRG),NVs,LGVs,RGVs,CoVs),
  build_head_from_body(CoVs,NVs,H),
  build_division_rules(H,TLG,TRG,LGVs,RGVs,CoVs,NVs,DivRules,IArgsDivRules),
  append(DivRules,RuleNVsListo2,RuleNVsListo),
  append(IArgsListo2,IArgsDivRules,IArgsListo).
translate_division_calls_term(C,RC,NVs,LVs,RVs,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
  C =.. [F|As],
  !, 
  translate_division_calls_term_list(As,RAs,NVs,LVs,RVs,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo),
  RC =.. [F|RAs].

translate_division_calls_term_list([],[],_NVs,_LVs,_RVs,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :-
  !.
translate_division_calls_term_list([T|Ts],[RT|RTs],NVs,LVs,RVs,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :-
  !, 
  term_variables(Ts,TsVs),
  my_set_union(TsVs,RVs,TRVs),
  translate_division_calls_term(T,RT,NVs,LVs,TRVs,IArgs,E,RuleNVsListi,RuleNVsList1,IArgsListi,IArgsList1), 
  term_variables(T,TVs),
  my_set_union(TVs,LVs,TLVs),
  translate_division_calls_term_list(Ts,RTs,NVs,TLVs,RVs,IArgs,E,RuleNVsList1,RuleNVsListo,IArgsList1,IArgsListo).

remove_anonymous_vars([],_NVs,[]).
remove_anonymous_vars([V|AVs],NVs,Vs) :-
  find_var_name(V,N,NVs),
  atom_concat('_',_,N),
  !,
  remove_anonymous_vars(AVs,NVs,Vs).
remove_anonymous_vars([V|AVs],NVs,[V|Vs]) :-
  remove_anonymous_vars(AVs,NVs,Vs).
  
division_compatible_vars(_Div,_NVs,LGVs,RGVs,CoVs) :-
  my_set_diff(LGVs,RGVs,CoVs),
  CoVs=[_|_],
  my_set_diff(RGVs,LGVs,[]),
  !.
division_compatible_vars(Div,NVs,LGVs,RGVs,_CoVs) :-
  (LGVs=[],RGVs=[],language(datalog) -> Msg='\n       Is this supposed to be a RA query? Try the command /ra your_query' ; Msg = ''),
  my_raise_exception(generic,syntax(['Incompatible schemas in division operation: ',Div,Msg]),NVs).
  
  
build_division_rules(H,L,R,LVs,RVs,DVs,NVs,DivRules,IArgs) :-
  build_head_from_body(DVs,NVs,P1),
  build_head_from_body(DVs,NVs,P2),
  append(DVs,RVs,P3Vs),
  build_head_from_body(P3Vs,NVs,P3),
  build_head_from_body(LVs,NVs,P4),
  length(RVs,Length),
  length(FRVs,Length),
  replace_term_list(RVs,FRVs,L,FL),
  R1=(H :-distinct(L), not(P1)),
  R2=(P1:-distinct(P2)),
  R3=(P2:-distinct(P3),not(P4)),
  R4=(P3:-distinct(FL),R),
  R5=(P4:-distinct(L)),
  term_variables(L,LLVs),
  term_variables([H,P1],HP1Vs),
  my_set_diff(LLVs,HP1Vs,A1Vs),
  my_set_union(LLVs,HP1Vs,R1Vs),
  my_var_name_list(R1Vs,NVs,RR1NVs),
  rename_anonymous_vars(RR1NVs,A1Vs,R1NVs),
  my_var_name_list(DVs,NVs,R2NVs),
  my_var_name_list(P3Vs,NVs,R3NVs),
  term_variables(FL,FLVs),
  term_variables([P3,R],P3RVs),
  my_set_diff(FLVs,P3RVs,A2Vs),
  my_set_union(FLVs,P3RVs,R4Vs),
  my_var_name_list(R4Vs,NVs,RR4NVs),
  rename_anonymous_vars(RR4NVs,A2Vs,R4NVs),
  copy_term((R1,R1NVs),CRNVs1),
  copy_term((R2,R2NVs),CRNVs2),
  copy_term((R3,R3NVs),CRNVs3),
  copy_term((R4,R4NVs),CRNVs4),
  copy_term((R5,RR1NVs),CRNVs5),
  DivRules=[CRNVs1,CRNVs2,CRNVs3,CRNVs4,CRNVs5],
  IArgs=[[],[],[],[],[]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translating top calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% translate_top_calls_list(+RuleNVsList,-DRuleNVsList,+IArgsList,-DArgsList,-Exploded)
translate_top_calls_list(RuleNVsList,DRuleNVsList,IArgsList,DArgsList,Exploded) :-
  translate_top_calls_list(RuleNVsList,IArgsList,[],[],DRuleNVsList,DArgsList,Exploded).
  
% translate_top_calls_list(+RuleNVsList,+IArgsList,+DRuleNVsList1,+DIArgsList1,-DRuleNVsList2,-DIArgsList2,-Exploded)
translate_top_calls_list([],[],DRuleNVsList,DIArgsList,DRuleNVsList,DIArgsList,_Exploded).
translate_top_calls_list([RuleNVs|RuleNVsList],[IArgs|IArgsList],DRuleNVsListi,DIArgsListi,DRuleNVsListo,DIArgsListo,Exploded) :-
  translate_top_calls(RuleNVs,IArgs,DRuleNVsList1,DIArgsList1,Exploded),
  translate_top_calls_list(RuleNVsList,IArgsList,DRuleNVsList1,DIArgsList1,DRuleNVsList2,DIArgsList2,Exploded),
  append(DRuleNVsListi,DRuleNVsList2,DRuleNVsListo),
  append(DIArgsListi,DIArgsList2,DIArgsListo).

translate_top_calls((Rule,NVs),IArgs,[(TRule,TNVs)|RNVsList],[IArgs|IArgsList],Exploded) :-
  Rule = ':-'(Head,Body),
  !,
  TRule = ':-'(Head,TBody),
  term_variables(Head,LVs),
  translate_top_calls(Body,TBody,NVs,LVs,[],IArgs,Exploded,[],RNVsList,[],IArgsList),
  term_variables((Head,TBody),TVs),
  my_var_name_list(TVs,NVs,TNVs).
translate_top_calls((Rule,NVs),IArgs,[(Rule,NVs)],[IArgs],_Exploded).

translate_top_calls(T,T,_NVs,_LVs,_RVs,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :- 
%  (number(T) ; var(T) ; atom(T)),
  (var(T) ; atomic(T)),
  !.
translate_top_calls(top(N,B),top(N,B),_NVs,_LVs,_RVs,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :- 
  my_atom(B),
  functor(B,F,A),
  \+ my_aggregate_relation(F,A),
  \+ my_outer_join_relation(F/A),
  B \= (_=>_),
  !.
translate_top_calls(top(N,B),top(N,H),NVs,LVs,RVs,_IArgs,true,RuleNVsListi,[(':-'(H,TB),TNVs)|RuleNVsListo],IArgsListi,[[]|IArgsListo]) :- 
  !,
  relevant_vars(B,BVs),
  my_set_union(LVs,RVs,CVs),
  my_set_inter(BVs,CVs,Vs),
  build_head_from_body(Vs,NVs,H),
  %WARNING: Build new IArgs appropriately
  translate_top_calls(B,TB,NVs,[],[],[],_,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo),
  term_variables([H,TB],TVs),
  my_var_name_list(TVs,NVs,TNVs).
translate_top_calls(C,RC,NVs,LVs,RVs,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
  C =.. [F|As],
  !, 
  translate_top_calls_list(As,RAs,NVs,LVs,RVs,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo),
  RC =.. [F|RAs].

translate_top_calls_list([],[],_NVs,_LVs,_RVs,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :-
  !.
translate_top_calls_list([T|Ts],[RT|RTs],NVs,LVs,RVs,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :-
  !, 
  term_variables(Ts,TsVs),
  my_set_union(TsVs,RVs,TRVs),
  translate_top_calls(T,RT,NVs,LVs,TRVs,IArgs,E,RuleNVsListi,RuleNVsList1,IArgsListi,IArgsList1), 
  term_variables(T,TVs),
  my_set_union(TVs,LVs,TLVs),
  translate_top_calls_list(Ts,RTs,NVs,TLVs,RVs,IArgs,E,RuleNVsList1,RuleNVsListo,IArgsList1,IArgsListo).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translating distinct calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% translate_distinct_calls_list(+RuleNVsList,-DRuleNVsList,+IArgsList,-DArgsList,-Exploded)
translate_distinct_calls_list(RuleNVsList,DRuleNVsList,IArgsList,DArgsList,Exploded) :-
  translate_distinct_calls_list(RuleNVsList,IArgsList,[],[],DRuleNVsList,DArgsList,Exploded).
  
% translate_distinct_calls_list(+RuleNVsList,+IArgsList,+DRuleNVsList1,+DIArgsList1,-DRuleNVsList2,-DIArgsList2,-Exploded)
translate_distinct_calls_list([],[],DRuleNVsList,DIArgsList,DRuleNVsList,DIArgsList,_Exploded).
translate_distinct_calls_list([RuleNVs|RuleNVsList],[IArgs|IArgsList],DRuleNVsListi,DIArgsListi,DRuleNVsListo,DIArgsListo,Exploded) :-
  translate_distinct_calls(RuleNVs,IArgs,DRuleNVsList1,DIArgsList1,Exploded),
  translate_distinct_calls_list(RuleNVsList,IArgsList,DRuleNVsList1,DIArgsList1,DRuleNVsList2,DIArgsList2,Exploded),
  append(DRuleNVsListi,DRuleNVsList2,DRuleNVsListo),
  append(DIArgsListi,DIArgsList2,DIArgsListo).

translate_distinct_calls((Rule,NVs),IArgs,[(TRule,TNVs)|NCRuleNVsList],[IArgs|IArgsList],Exploded) :-
  Rule = ':-'(Head,Body),
  !,
  TRule = ':-'(Head,TBody),
  term_variables(Head,LVs),
  translate_distinct_calls(Body,TBody,NVs,LVs,[],IArgs,Exploded,[],NCRuleNVsList,[],IArgsList),
  term_variables(TRule,TVs),
  my_var_name_list(TVs,NVs,TNVs).
translate_distinct_calls((Rule,NVs),IArgs,[(Rule,NVs)],[IArgs],_Exploded).

translate_distinct_calls(T,T,_NVs,_LVs,_RVs,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :- 
%  (number(T) ; var(T) ; atom(T)),
  (var(T) ; atomic(T)),
  !.
translate_distinct_calls(distinct(B),distinct(TB),NVs,LVs,RVs,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
  relevant_vars(B,Vs),
  %term_variables(B,Vs),
  translate_distinct_calls(distinct(Vs,B),distinct(Vs,TB),NVs,LVs,RVs,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo).
translate_distinct_calls(distinct(Vs,B),distinct(Vs,B),_NVs,_LVs,_RVs,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :- 
  my_atom(B),
  functor(B,F,A),
  \+ my_aggregate_relation(F,A),
  \+ my_outer_join_relation(F/A),
  B \= (_=>_),
  !.
translate_distinct_calls(distinct(DVs,B),distinct(DVs,H),NVs,LVs,RVs,_IArgs,true,RuleNVsListi,[(':-'(H,TB),TNVs)|RuleNVsListo],IArgsListi,[[]|IArgsListo]) :- 
  !,
  %WARNING: Build new IArgs appropriately
  relevant_vars(B,BVs),
  my_set_union(LVs,RVs,CVs),
  my_set_inter(BVs,CVs,Vs),
  build_head_from_body(Vs,NVs,H),
  translate_distinct_calls(B,TB,NVs,[],[],[],_,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo),
  term_variables([H,TB],TVs),
  my_var_name_list(TVs,NVs,TNVs).
translate_distinct_calls(C,RC,NVs,LVs,RVs,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
  C =.. [F|As],
  !, 
  translate_distinct_calls_list(As,RAs,NVs,LVs,RVs,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo),
  RC =.. [F|RAs].

translate_distinct_calls_list([],[],_NVs,_LVs,_RVs,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :-
  !.
translate_distinct_calls_list([T|Ts],[RT|RTs],NVs,LVs,RVs,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :-
  !, 
  term_variables(Ts,TsVs),
  my_set_union(TsVs,RVs,TRVs),
  translate_distinct_calls(T,RT,NVs,LVs,TRVs,IArgs,E,RuleNVsListi,RuleNVsList1,IArgsListi,IArgsList1), 
  term_variables(T,TVs),
  my_set_union(TVs,LVs,TLVs),
  translate_distinct_calls_list(Ts,RTs,NVs,TLVs,RVs,IArgs,E,RuleNVsList1,RuleNVsListo,IArgsList1,IArgsListo).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translating order_by calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% translate_order_by_calls_list(+RuleNVsList,-DRuleNVsList,+IArgsList,-DArgsList,-Exploded)
translate_order_by_calls_list(RuleNVsList,DRuleNVsList,IArgsList,DArgsList,Exploded) :-
  translate_order_by_calls_list(RuleNVsList,IArgsList,[],[],DRuleNVsList,DArgsList,Exploded).
  
% translate_order_by_calls_list(+RuleNVsList,+IArgsList,+DRuleNVsList1,+DIArgsList1,-DRuleNVsList2,-DIArgsList2,-Exploded)
translate_order_by_calls_list([],[],DRuleNVsList,DIArgsList,DRuleNVsList,DIArgsList,_Exploded).
translate_order_by_calls_list([RuleNVs|RuleNVsList],[IArgs|IArgsList],DRuleNVsListi,DIArgsListi,DRuleNVsListo,DIArgsListo,Exploded) :-
  translate_order_by_calls(RuleNVs,IArgs,DRuleNVsList1,DIArgsList1,Exploded),
  translate_order_by_calls_list(RuleNVsList,IArgsList,DRuleNVsList1,DIArgsList1,DRuleNVsList2,DIArgsList2,Exploded),
  append(DRuleNVsListi,DRuleNVsList2,DRuleNVsListo),
  append(DIArgsListi,DIArgsList2,DIArgsListo).

translate_order_by_calls((Rule,NVs),IArgs,[(TRule,TNVs)|NCRuleNVsList],[IArgs|IArgsList],Exploded) :-
  Rule = ':-'(Head,Body),
  !,
  TRule = ':-'(Head,TBody),
  term_variables(Head,LVs),
  translate_order_by_calls(Body,TBody,NVs,LVs,[],IArgs,Exploded,[],NCRuleNVsList,[],IArgsList),
  term_variables(TRule,TVs),
  my_var_name_list(TVs,NVs,TNVs).
translate_order_by_calls((Rule,NVs),IArgs,[(Rule,NVs)],[IArgs],_Exploded).

translate_order_by_calls(T,T,_NVs,_LVs,_RVs,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :- 
%  (number(T) ; var(T) ; atom(T)),
  (var(T) ; atomic(T)),
  !.
translate_order_by_calls(order_by(B,Es,Os),order_by(B,Es,Os),_NVs,_LVs,_RVs,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :- 
  my_atom(B),
  functor(B,F,A),
  \+ my_aggregate_relation(F,A),
  \+ my_outer_join_relation(F/A),
  B \= (_=>_),
  !.
translate_order_by_calls(order_by(B,Es,Os),order_by(H,Es,Os),NVs,LVs,RVs,_IArgs,true,RuleNVsListi,[(':-'(H,TB),TNVs)|RuleNVsListo],IArgsListi,[[]|IArgsListo]) :- 
  !,
  %WARNING: Build new IArgs appropriately
  relevant_vars(B,BVs),
  my_set_union(LVs,RVs,CVs),
  my_set_inter(BVs,CVs,Vs),
  build_head_from_body(Vs,NVs,H),
  translate_order_by_calls(B,TB,NVs,[],[],[],_,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo),
  term_variables([H,TB],TVs),
  my_var_name_list(TVs,NVs,TNVs).
translate_order_by_calls(C,RC,NVs,LVs,RVs,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
  C =.. [F|As],
  !, 
  translate_order_by_calls_list(As,RAs,NVs,LVs,RVs,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo),
  RC =.. [F|RAs].

translate_order_by_calls_list([],[],_NVs,_LVs,_RVs,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :-
  !.
translate_order_by_calls_list([T|Ts],[RT|RTs],NVs,LVs,RVs,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :-
  !, 
  term_variables(Ts,TsVs),
  my_set_union(TsVs,RVs,TRVs),
  translate_order_by_calls(T,RT,NVs,LVs,TRVs,IArgs,E,RuleNVsListi,RuleNVsList1,IArgsListi,IArgsList1), 
  term_variables(T,TVs),
  my_set_union(TVs,LVs,TLVs),
  translate_order_by_calls_list(Ts,RTs,NVs,TLVs,RVs,IArgs,E,RuleNVsList1,RuleNVsListo,IArgsList1,IArgsListo).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translating hypothetical calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% translate_hypo_calls_list(+RuleNVsList,-DRuleNVsList,+IArgsList,-DArgsList,-Exploded)
translate_hypo_calls_list(RuleNVsList,DRuleNVsList,IArgsList,DArgsList,Exploded) :-
  translate_hypo_calls_list(RuleNVsList,IArgsList,[],[],DRuleNVsList,DArgsList,Exploded).
  
% translate_hypo_calls_list(+RuleNVsList,+IArgsList,+DRuleNVsList1,+DIArgsList1,-DRuleNVsList2,-DIArgsList2,-Exploded)
translate_hypo_calls_list([],[],DRuleNVsList,DIArgsList,DRuleNVsList,DIArgsList,_Exploded).
translate_hypo_calls_list([RuleNVs|RuleNVsList],[IArgs|IArgsList],DRuleNVsListi,DIArgsListi,DRuleNVsListo,DIArgsListo,Exploded) :-
  translate_hypo_calls(RuleNVs,IArgs,DRuleNVsList1,DIArgsList1,Exploded),
  translate_hypo_calls_list(RuleNVsList,IArgsList,DRuleNVsList1,DIArgsList1,DRuleNVsList2,DIArgsList2,Exploded),
  append(DRuleNVsListi,DRuleNVsList2,DRuleNVsListo),
  append(DIArgsListi,DIArgsList2,DIArgsListo).

translate_hypo_calls((Rule,NVs),IArgs,[(TRule,TNVs)|NCRuleNVsList],[IArgs|IArgsList],Exploded) :-
  Rule = ':-'(Head,Body),
  !,
  TRule = ':-'(Head,TBody),
  translate_hypo_calls(Body,TBody,NVs,HNVs,IArgs,_First,Exploded,[],NCRuleNVsList,[],IArgsList),
  term_variables(TRule,TVs),
  my_var_name_list(TVs,HNVs,TNVs).
translate_hypo_calls((Rule,NVs),IArgs,[(Rule,NVs)],[IArgs],_Exploded).

translate_hypo_calls(T,T,NVs,NVs,_IArgs,_Fst,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :- 
%  (number(T) ; var(T) ; atom(T)),
  (var(T) ; atomic(T)),
  !.
translate_hypo_calls('=>'(L,R),'=>'(TL,TR),NVs,ONVs,_IArgs,Fst,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
  var(Fst), % First occurrence of => is not translated into a new rule
  !,
  %WARNING: Build new IArgs appropriately
%   copy_term((L,NVs),(CL,CNVs)),
%   translate_hypo_calls_arg(CL,TL,CNVs,_,[],Fst,E,RuleNVsListi,RuleNVsListo1,IArgsListi,IArgsListo1),
%   translate_hypo_calls_arg(R,TR,NVs,RNVs,[],Fst,E,RuleNVsListo1,RuleNVsListo,IArgsListo1,IArgsListo),
%   append(RNVs,CNVs,ONVs).
  translate_hypo_calls_arg(L,TL,NVs,_,[],Fst,E,RuleNVsListi,RuleNVsListo1,IArgsListi,IArgsListo1),
  translate_hypo_calls_arg(R,TR,NVs,RNVs,[],Fst,E,RuleNVsListo1,RuleNVsListo,IArgsListo1,IArgsListo),
  append(RNVs,NVs,ONVs).
translate_hypo_calls('=>'(L,R),H,NVs,ONVs,_IArgs,false,true,RuleNVsListi,[(':-'(H,'=>'(TL,TR)),TNVs)|RuleNVsListo],IArgsListi,[[]|IArgsListo]) :- 
  !,
  %WARNING: Build new IArgs appropriately
%   relevant_vars((L,R),BVs),
%   term_variables(NVs,VNVs),
%   my_set_inter(BVs,VNVs,Vs),
%   build_head_from_body(Vs,[],H),
  relevant_vars(R,BVs),
  term_variables(NVs,VNVs),
  my_set_inter(BVs,VNVs,Vs),
  build_head_from_body(Vs,NVs,H),
  copy_term((L,NVs),(CL,CNVs)),
  translate_hypo_calls_arg(CL,TL,CNVs,_,[],_,true,RuleNVsListi,RuleNVsListo1,IArgsListi,IArgsListo1),
  translate_hypo_calls_arg(R,TR,NVs,RNVs,[],_,true,RuleNVsListo1,RuleNVsListo,IArgsListo1,IArgsListo),
  term_variables((H,TL,TR),TVs),
  append(RNVs,CNVs,ONVs),
  my_var_name_list(TVs,ONVs,TNVs).
translate_hypo_calls(C,RC,NVs,NVs,IArgs,Fst,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
  C =.. [P|As],
  !, 
  translate_hypo_calls_list(As,RAs,NVs,IArgs,Fst,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo),
  RC =.. [P|RAs].

translate_hypo_calls_list([],[],_NVs,_IArgs,_Fst,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :-
  !.
translate_hypo_calls_list([T|Ts],[RT|RTs],NVs,IArgs,Fst,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :-
  !, 
  translate_hypo_calls(T,RT,NVs,_,IArgs,Fst,E,RuleNVsListi,RuleNVsList1,IArgsListi,IArgsList1), 
  translate_hypo_calls_list(Ts,RTs,NVs,IArgs,Fst,E,RuleNVsList1,RuleNVsListo,IArgsList1,IArgsListo).

translate_hypo_calls_arg(A,TA,NVs,ONVs,IArgs,Fst,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :-
  my_noncompound_term(A),
  !,
  translate_hypo_calls(A,TA,NVs,ONVs,IArgs,Fst,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo).
translate_hypo_calls_arg(B,H,NVs,ONVs,IArgs,Fst,true,RuleNVsListi,[(':-'(H,TB),TNVs)|RuleNVsListo],IArgsListi,[[]|IArgsListo]) :-
  functor(B,F,A),
  ( F/A = group_by/3
   ;
    F/A = group_by/4
   ;
    my_aggregate_relation(F,A)
   ;
    my_outer_join_relation(F/A)
   ;
    B = (_=>_)
   ;
    B = (_;_)
   ;
    B = (_,_) % Added to call solve_stratified in  (_ => B) so that RHS becomes a basic (non-compound) goal
  ),
  !,
  build_head_from_body(B,NVs,H),
  translate_hypo_calls(B,TB,NVs,ONVs,IArgs,Fst,true,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo),
  term_variables([H,TB],TVs),
  my_var_name_list(TVs,ONVs,TNVs).
translate_hypo_calls_arg(A,TA,NVs,ONVs,IArgs,Fst,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :-
  translate_hypo_calls(A,TA,NVs,ONVs,IArgs,Fst,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translating negated calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% translate_negated_compound_calls_list(+RuleNVsList,-DRuleNVsList,+IArgsList,-DArgsList,+Safety,-Exploded)
translate_negated_compound_calls_list(RuleNVsList,DRuleNVsList,IArgsList,DArgsList,Safety,Exploded) :-
  translate_negated_compound_calls_list(RuleNVsList,IArgsList,[],[],DRuleNVsList,DArgsList,Safety,Exploded).
  
% translate_negated_compound_calls_list(+RuleNVsList,+IArgsList,+DRuleNVsList1,+DIArgsList1,-DRuleNVsList2,-DIArgsList2,+Safety,-Exploded)
translate_negated_compound_calls_list([],[],DRuleNVsList,DIArgsList,DRuleNVsList,DIArgsList,_Safety,_Exploded).
translate_negated_compound_calls_list([RuleNVs|RuleNVsList],[IArgs|IArgsList],DRuleNVsListi,DIArgsListi,DRuleNVsListo,DIArgsListo,Safety,Exploded) :-
  translate_negated_compound_calls_ruleNVs(RuleNVs,IArgs,DRuleNVsList1,DIArgsList1,Safety,Exploded),
  translate_negated_compound_calls_list(RuleNVsList,IArgsList,DRuleNVsList1,DIArgsList1,DRuleNVsList2,DIArgsList2,Safety,Exploded),
  append(DRuleNVsListi,DRuleNVsList2,DRuleNVsListo),
  append(DIArgsListi,DIArgsList2,DIArgsListo).

% translate_negated_compound_calls_ruleNVs(+RuleNVs,+IArgs,-RuleNVsList,-IArgsList,+Safety,-Exploded)
translate_negated_compound_calls_ruleNVs((Rule,NVs),IArgs,[TRuleNVs|NCRuleNVsList],[IArgs|IArgsList],Safety,Exploded) :-
  translate_negated_compound_calls_rule(Rule,TRuleNVs,NVs,Rule,IArgs,Safety,Exploded,NCRuleNVsList,IArgsList).
  
translate_negated_compound_calls_rule(':-'(H,B),(TRule,TNVs),NVs,Rule,IArgs,Safety,Exploded,NCRuleNVsList,IArgsList) :-
  !,
  translate_negated_compound_calls(':-'(H,B),TRule,NVs,TNVs,Rule,IArgs,Safety,Exploded,[],NCRuleNVsList,[],IArgsList).
translate_negated_compound_calls_rule(H,(H,NVs),NVs,_Rule,_IArgs,_Safety,_Exploded,[],[]).
  
translate_negated_compound_calls(T,T,NVs,NVs,_Rule,_IArgs,_S,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :- 
  (var(T) ; atomic(T)),
  !.
% translate_negated_compound_calls(not(exists(Vs,B)),not(exists(RVs,TB)),NVsi,NVso,Rule,IArgs,S,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
%   !,
%   replace_existential_vars(Rule,exists(Vs,B),exists(RVs,RB),NVsi,NVsi1),
%   translate_negated_compound_calls(not(RB),not(TB),NVsi1,NVso,Rule,IArgs,S,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo).
% translate_negated_compound_calls(exists(Vs,B),exists(RVs,TB),NVsi,NVso,Rule,IArgs,S,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
%   !,
%   replace_existential_vars(Rule,exists(Vs,B),exists(RVs,RB),NVsi,NVsi1),
%   translate_negated_compound_calls(not(RB),not(TB),NVsi1,NVso,Rule,IArgs,S,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo).
translate_negated_compound_calls(not(exists(Vs,B)),not(exists(Vs,TB)),NVsi,NVso,Rule,IArgs,S,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
  !,
  check_existential_var_reusing(Vs,NVsi,exists(Vs,B),Rule),
  check_existential_var_duplicates(Vs,NVsi),
  make_non_relevant_var_names(Vs,NVsi,NVsi1),
  translate_negated_compound_calls(not(B),not(TB),NVsi1,NVso,Rule,IArgs,S,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo).
translate_negated_compound_calls(exists(Vs,B),exists(Vs,TB),NVsi,NVso,Rule,IArgs,S,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
  !,
  check_existential_var_reusing(Vs,NVsi,exists(Vs,B),Rule),
  check_existential_var_duplicates(Vs,NVsi),
  make_non_relevant_var_names(Vs,NVsi,NVsi1),
  translate_negated_compound_calls(not(B),not(TB),NVsi1,NVso,Rule,IArgs,S,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo).
translate_negated_compound_calls(not(B),not(B),NVs,NVs,Rule,_IArgs,S,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :- 
%  S\==safety,
%  safe(off),
  my_atom(B),
  functor(B,F,A),
  \+ my_aggregate_relation(F,A),
  \+ my_outer_join_relation(F/A),
  B \= (_=>_),
%  underscored_variables(B,NVs,[]),
  dont_safe_negated_calls(not(B),Rule,NVs,S),
  !.
translate_negated_compound_calls(not(B),not(H),NVsi,NVso,Rule,_IArgs,S,true,RuleNVsListi,[(':-'(H,TB),TNVs)|RuleNVsListo],IArgsListi,[[]|IArgsListo]) :- 
  !,
  %WARNING: Build new IArgs appropriately
  replace_term(not(B),true,Rule,RRule), % Extract this negated call to find out the needed variables in the call H
  term_variables(B,BVs),
  term_variables(RRule,RVs),
  my_set_diff(BVs,RVs,UVs),  % Variables occurring only in the negated call are not needed to build the new head
  build_head_from_body_acc(B,UVs,NVsi,H),
  translate_negated_compound_calls(B,TB,NVsi,NVso,Rule,[],S,_,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo),
  term_variables([H,TB],TVs),
  my_var_name_list(TVs,NVso,TNVs).
translate_negated_compound_calls(C,RC,NVsi,NVso,Rule,IArgs,S,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
  C =.. [F|As],
  !, 
  translate_negated_compound_calls_list(As,RAs,NVsi,NVso,Rule,IArgs,S,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo),
  RC =.. [F|RAs].

translate_negated_compound_calls_list([],[],NVs,NVs,_Rule,_IArgs,_S,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :-
  !.
translate_negated_compound_calls_list([T|Ts],[RT|RTs],NVsi,NVso,Rule,IArgs,S,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :-
  !, 
  translate_negated_compound_calls(T,RT,NVsi,NVsi1,Rule,IArgs,S,E,RuleNVsListi,RuleNVsList1,IArgsListi,IArgsList1), 
  translate_negated_compound_calls_list(Ts,RTs,NVsi1,NVso,Rule,IArgs,S,E,RuleNVsList1,RuleNVsListo,IArgsList1,IArgsListo).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translating existential quantifiers 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% translate_exists_list(+RuleNVsList,-DRuleNVsList,+IArgsList,-DIArgsList,+Safety,-Exploded)
translate_exists_list(RuleNVsList,DRuleNVsList,IArgsList,DIArgsList,_Safety,Exploded) :- % WARNING: Remove Safety
  translate_exists_list([],RuleNVsList,true,_Rels,[],IArgsList,DRuleNVsList,DIArgsList,Exploded).
  
% translate_exists_list(+HRuleNVsList,+RuleNVsList,+HIArgsList,+IArgsList,-DRuleNVsList,-DIArgsList,-Exploded)
translate_exists_list(DRuleNVsList,[],Rels,Rels,DIArgsList,[],DRuleNVsList,DIArgsList,_Exploded).
translate_exists_list(HRuleNVsList,[RuleNVs|RuleNVsList],IRels,ORels,HIArgsList,[IArgs|IArgsList],DRuleNVsList,DIArgsList,Exploded) :-
  translate_exists_ruleNVs(RuleNVs,IArgs,IRels,I1Rels,HRuleNVsList,RuleNVsList,CHRuleNVsList,CRuleNVsList,HIArgsList,IArgsList,CHIArgsList,CIArgsList,TRuleNVsList,TIArgsList,Exploded),
  append(CHRuleNVsList,TRuleNVsList,H1RuleNVsList),
  append(CHIArgsList,TIArgsList,H1IArgsList),
  translate_exists_list(H1RuleNVsList,CRuleNVsList,I1Rels,ORels,H1IArgsList,CIArgsList,DRuleNVsList,DIArgsList,Exploded).

% translate_exists_ruleNVs(+RuleNVs,+IArgs,+HRNVs,+TRNVs,-CHRNVs,-CTRNVs,-HIAss,-TIAss,-CHIAss,-CTIAss,-RuleNVsList,-IArgsList,-Exploded)
translate_exists_ruleNVs((Rule,NVs),IArgs,IRels,ORels,HRNVs,TRNVs,CHRNVs,CTRNVs,HIAss,TIAss,CHIAss,CTIAss,[TRuleNVs|NCRuleNVsList],[IArgs|IArgsList],Exploded) :-
  translate_exists_rule(Rule,TRuleNVs,NVs,IArgs,IRels,ORels,HRNVs,TRNVs,CHRNVs,CTRNVs,HIAss,TIAss,CHIAss,CTIAss,Exploded,NCRuleNVsList,IArgsList).
  
translate_exists_rule((H:-B),(TRule,TNVs),NVs,IArgs,IRels,ORels,HRNVs,TRNVs,CHRNVs,CTRNVs,HIAss,TIAss,CHIAss,CTIAss,Exploded,NCRuleNVsList,IArgsList) :-
  !,
%  translate_exists((H:-B),TRule,(H:-B),IRels,ORels,NVs,IArgs,HRNVs,TRNVs,CHRNVs,CTRNVs,HIAss,TIAss,CHIAss,CTIAss,Exploded,[],NCRuleNVsList,[],IArgsList),
  translate_exists(B,TB,(H:-B),IRels,ORels,NVs,IArgs,HRNVs,TRNVs,CHRNVs,CTRNVs,HIAss,TIAss,CHIAss,CTIAss,Exploded,[],NCRuleNVsList,[],IArgsList),
  TRule=(H:-TB),
  term_variables(TRule,TVs),
  my_var_name_list(TVs,NVs,TNVs).
translate_exists_rule(H,(H,NVs),NVs,_IArgs,Rels,Rels,HRNVs,TRNVs,HRNVs,TRNVs,HIAss,TIAss,HIAss,TIAss,_Exploded,[],[]).
  
translate_exists(T,T,_ORule,Rels,Rels,_NVs,_IArgs,HRNVs,TRNVs,HRNVs,TRNVs,HIAss,TIAss,HIAss,TIAss,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :- 
%  (number(T) ; var(T) ; atom(T)),
  (var(T) ; atomic(T)),
  !.
% translate_exists((H:-B),(H:-TB),ORule,IRels,ORels,NVs,IArgs,HRNVs,TRNVs,HRNVs,TRNVs,HIAss,TIAss,HIAss,TIAss,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
%   !,
%   translate_exists(B,TB,ORule,IRels,ORels,NVs,IArgs,HRNVs,TRNVs,HRNVs,TRNVs,HIAss,TIAss,HIAss,TIAss,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo).
translate_exists(not(exists(Vs,B)),not(H),(OH:-Gs),IRels,ORels,NVs,IArgs,HRNVs,TRNVs,CHRNVs,CTRNVs,HIAss,TIAss,HIAss,TIAss,true,RuleNVsListi,[((H:-CB),TNVs)|RuleNVsListi1],IArgsListi,[[]|IArgsListi1]) :- 
  !,
  translate_exists(B,TB,(OH:-Gs),IRels,ORels,NVs,IArgs,HRNVs,TRNVs,H1RNVs,T1RNVs,HIAss,TIAss,HIAss,TIAss,true,RuleNVsListi,RuleNVsListi1,IArgsListi,IArgsListi1),
  replace_term(not(exists(Vs,B)),true,Gs,RelGs),
  my_unzip(H1RNVs,HDLs,HNVsi),
  my_unzip(T1RNVs,TDLs,TNVsi),
  append(HDLs,TDLs,DLs),
  term_variables((OH,RelGs),RVs),
  term_variables(TB,BVs),
  my_set_diff(BVs,RVs,UVs),
  build_head_from_body_acc(TB,UVs,NVs,HB),
  my_set_diff(BVs,Vs,CVs),
%  add_correlated_goals(CVs,RelGs,[(HB:-TB)|DLs],[(H:-CB)|CDLs]),
  add_correlated_goals(CVs,IRels,[(HB:-TB)|DLs],[(H:-CB)|CDLs]),
  length(HDLs,N),
  split_list(N,CDLs,CHDLs,CTDLs),
  concat_lists(HNVsi,HNVsi1),
  concat_lists(TNVsi,TNVsi1),
  append(HNVsi1,TNVsi1,NVsi),
  rule_to_ruleNVs_list(CHDLs,NVsi,CHRNVs),
  rule_to_ruleNVs_list(CTDLs,NVsi,CTRNVs),
  term_variables([H,CB],TVs),
  my_var_name_list(TVs,NVs,TNVs).
translate_exists(exists(Vs,B),H,Gs,IRels,ORels,NVs,IArgs,HRNVs,TRNVs,CHRNVs,CTRNVs,HIAss,TIAss,CHIAss,CTIAss,true,RuleNVsListi,[((H:-TB),TNVs)|RuleNVsListi1],IArgsListi,[[]|IArgsListi1]) :- 
  !,
  translate_exists(B,TB,Gs,IRels,ORels,NVs,IArgs,HRNVs,TRNVs,CHRNVs,CTRNVs,HIAss,TIAss,CHIAss,CTIAss,_E,RuleNVsListi,RuleNVsListi1,IArgsListi,IArgsListi1),
  build_head_from_body_acc(TB,Vs,NVs,H),
  term_variables([H,TB],TVs),
  my_var_name_list(TVs,NVs,TNVs).
translate_exists(G,G,_ORule,IRels,ORels,_NVs,_IArgs,HRNVs,TRNVs,HRNVs,TRNVs,HIAss,TIAss,HIAss,TIAss,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :- 
  G=..[F|_],
  \+ is_system_pred(F/_),
  \+ my_builtin_pred(F/_),
  \+ my_infix(F),
  !,
  (my_member_var_term(G,IRels)
   ->
    ORels=IRels
   ;
    append_goals(IRels,G,ORels)).
translate_exists(C,RC,Gs,IRels,ORels,NVs,IArgs,HRNVs,TRNVs,CHRNVs,CTRNVs,HIAss,TIAss,CHIAss,CTIAss,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
  C =.. [F|As],
  !, 
  translate_exists_list(As,RAs,Gs,IRels,ORels,NVs,IArgs,HRNVs,TRNVs,CHRNVs,CTRNVs,HIAss,TIAss,CHIAss,CTIAss,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo),
  RC =.. [F|RAs].

translate_exists_list([],[],_Gs,Rels,Rels,_NVs,_IArgs,HRNVs,TRNVs,HRNVs,TRNVs,HIAss,TIAss,HIAss,TIAss,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :-
  !.
translate_exists_list([T|Ts],[RT|RTs],Gs,IRels,ORels,NVs,IArgs,HRNVs,TRNVs,CHRNVs,CTRNVs,HIAss,TIAss,CHIAss,CTIAss,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :-
  !, 
  translate_exists(T,RT,Gs,IRels,I1Rels,NVs,IArgs,HRNVs,TRNVs,C1HRNVs,C1TRNVs,HIAss,TIAss,C1HIAss,C1TIAss,E,RuleNVsListi,RuleNVsListi1,IArgsListi,IArgsListi1), 
  translate_exists_list(Ts,RTs,Gs,I1Rels,ORels,NVs,IArgs,C1HRNVs,C1TRNVs,CHRNVs,CTRNVs,C1HIAss,C1TIAss,CHIAss,CTIAss,E,RuleNVsListi1,RuleNVsListo,IArgsListi1,IArgsListo).

make_non_relevant_var_names([],NVs,NVs).
make_non_relevant_var_names([V|Vs],INVs,ONVs) :-
  find_var_name(V,N,INVs),
  atom_concat('_',_,N), % Non-relevant already 
  !,
  make_non_relevant_var_names(Vs,INVs,ONVs).
make_non_relevant_var_names([V|Vs],INVs,ONVs) :-
  find_var_name(V,N,INVs),
  !,
  atom_concat('_',N,NN),
  replace_list(N=V,NN=V,INVs,I1NVs),
  make_non_relevant_var_names(Vs,I1NVs,ONVs).
make_non_relevant_var_names([_V|Vs],INVs,ONVs) :- % V is not found
  make_non_relevant_var_names(Vs,INVs,ONVs).
  
make_non_relevant_var_names_list(_Vs,[],[]).
make_non_relevant_var_names_list(Vs,[X|Xs],[Y|Ys]) :-
  make_non_relevant_var_names(Vs,X,Y),
  make_non_relevant_var_names_list(Vs,Xs,Ys).

  
% replace_existential_vars(_Rule,exists(Vs,B),exists(RVs,RB),NVsi,NVso) :-
% %  replace_term(exists(Vs,B),true,Rule,RRule),
% %  term_variables(RRule,RRuleVs),
% %  my_set_inter(Vs,RRuleVs,SharedVs),
% %  length(SharedVs,L),
% %  length(RenamedVs,L),
%   length(Vs,L),
%   length(RVs,L),
% %  replace_list_term(SharedVs,RenamedVs,[Vs,B],[RVs,RB]),
%   replace_list_term(Vs,RVs,B,RB),
%   make_underscored_vars(RVs,NVsi,NVso).
% %  make_underscored_vars_from(SharedVs,RenamedVs,NVsi,NVso).

% % make_underscored_vars_from([],[],NVs,NVs).
% % make_underscored_vars_from([V|Vs],[RV|RVs],NVsi,NVso) :-
% %   find_var_name(V,N,NVsi),
% %   atom_concat('_',N,NN),
% %   make_underscored_vars_from(Vs,RVs,[NN=RV|NVsi],NVso).
% make_underscored_vars([],NVs,NVs).
% make_underscored_vars([V|Vs],NVsi,NVso) :-
%   name_underscored_var(NVsi,N),
%   make_underscored_vars(Vs,[N=V|NVsi],NVso).
  
check_existential_var_reusing(Vs,NVs,exists(Vs,B),Rule) :-
  replace_term(exists(Vs,B),true,Rule,RRule),
  term_variables(RRule,RRuleVs),
  my_set_inter(Vs,RRuleVs,IVars),
  (IVars==[]
   ->
    true
   ;
    (IVars=[_] -> Nbr=' ',Pr='its' ; Nbr='s ',Pr='their'),
    my_raise_exception(generic,syntax(['(DL) Quantified variable',Nbr,'$exec'(write_with_NVs(IVars,NVs)),' cannot occur outside ',Pr,' scope.']),[])
  ).
    
check_existential_var_duplicates(Vs,NVs) :-
  extract_duplicates_var(Vs,DQVars), % Only one occurrence in the quantifier's variable list
  (DQVars==[]
   ->
    true
   ;
    (DQVars=[_] -> Nbr=' '; Nbr='s '),
    my_raise_exception(generic,syntax(['(DL) Quantified variable',Nbr,'$exec'(write_with_NVs(DQVars,NVs)),' can occur only once in the quantifier''s variable list.']),[])
  ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translating safe negated calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% translate_safe_negated_calls_list(+RuleNVsList,-DRuleNVsList,+IArgsList,-DArgsList,+Safety,-Exploded)
translate_safe_negated_calls_list(RuleNVsList,RuleNVsList,IArgsList,IArgsList,Safety,_Exploded) :-
  Safety\==safety,
  safe(off),
  !.
translate_safe_negated_calls_list(RuleNVsList,DRuleNVsList,IArgsList,DArgsList,_Safety,Exploded) :-
  translate_safe_negated_calls_list(RuleNVsList,IArgsList,[],[],DRuleNVsList,DArgsList,Exploded).
  
% translate_safe_negated_calls_list(+RuleNVsList,+IArgsList,+DRuleNVsList1,+DIArgsList1,-DRuleNVsList2,-DIArgsList2,-Exploded)
translate_safe_negated_calls_list([],[],DRuleNVsList,DIArgsList,DRuleNVsList,DIArgsList,_Exploded).
translate_safe_negated_calls_list([RuleNVs|RuleNVsList],[IArgs|IArgsList],DRuleNVsListi,DIArgsListi,DRuleNVsListo,DIArgsListo,Exploded) :-
  translate_safe_negated_calls_ruleNVs(RuleNVs,IArgs,DRuleNVsList1,DIArgsList1,Exploded),
  translate_safe_negated_calls_list(RuleNVsList,IArgsList,DRuleNVsList1,DIArgsList1,DRuleNVsList2,DIArgsList2,Exploded),
  append(DRuleNVsListi,DRuleNVsList2,DRuleNVsListo),
  append(DIArgsListi,DIArgsList2,DIArgsListo).

% translate_safe_negated_calls_ruleNVs(+RuleNVs,+IArgs,-RuleNVsList,-IArgsList,-Exploded)
translate_safe_negated_calls_ruleNVs((Rule,NVs),IArgs,[TRuleNVs|NCRuleNVsList],[IArgs|IArgsList],Exploded) :-
  translate_safe_negated_calls_rule(Rule,TRuleNVs,NVs,[],_,Rule,IArgs,Exploded,NCRuleNVsList,IArgsList).
  
translate_safe_negated_calls_rule(':-'(H,B),(TRule,NVs),NVs,SVsi,SVso,Rule,IArgs,Exploded,NCRuleNVsList,IArgsList) :-
  !,
  translate_safe_negated_calls(':-'(H,B),TRule,NVs,SVsi,SVso,Rule,IArgs,Exploded,[],NCRuleNVsList,[],IArgsList).
translate_safe_negated_calls_rule(H,(H,NVs),NVs,SVs,SVs,_Rule,_IArgs,_Exploded,[],[]).
  
translate_safe_negated_calls(T,T,_NVs,SVs,SVs,_Rule,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :- 
%  (number(T) ; var(T) ; atom(T)),
  (var(T) ; atomic(T)),
  !.
translate_safe_negated_calls(not(B),not(B),_NVs,SVs,SVs,_Rule,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :- 
  my_atom(B),
  functor(B,F,A),
  \+ my_aggregate_relation(F,A),
  \+ my_outer_join_relation(F/A),
  B \= (_=>_),
  term_variables(B,BVs),
  my_set_diff(BVs,SVs,[]),
  !.
translate_safe_negated_calls(not(B),not(H),NVs,SVs,SVs,Rule,_IArgs,true,RuleNVsListi,[(':-'(H,B),TNVs)|RuleNVsListi],IArgsListi,[[]|IArgsListi]) :- 
  my_atom(B),
  underscored_variables(B,NVs,[_|_]),
  !,
  %WARNING: Build new IArgs appropriately
  replace_term(not(B),true,Rule,RRule), % Extract this negated call to find out the needed variables in the call H
  term_variables(B,BVs),
  term_variables(RRule,RVs),
  my_set_diff(BVs,RVs,UVs),  % Variables occurring only in the negated call are not needed to build the new head
  build_head_from_body_acc(B,UVs,NVs,H),
  term_variables([H,B],TVs),
  my_var_name_list(TVs,NVs,TNVs).
translate_safe_negated_calls(not(B),not(B),_NVs,SVs,SVs,_Rule,_IArgs,_E,RuleNVsListi,RuleNVsListi,IArgsListi,IArgsListi) :- 
  my_atom(B),
  !.
translate_safe_negated_calls(B,B,_NVs,SVsi,SVso,_Rule,_IArgs,_E,RuleNVsListi,RuleNVsListi,IArgsListi,IArgsListi) :- 
  my_atom(B),
  !,
  term_variables(B,BVs),
  append(SVsi,BVs,SVso).
translate_safe_negated_calls(C,RC,NVs,SVsi,SVso,Rule,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
  C =.. [F|As],
  !, 
  translate_safe_negated_calls_list(As,RAs,NVs,SVsi,SVso,Rule,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo),
  RC =.. [F|RAs].

translate_safe_negated_calls_list([],[],_NVs,SVs,SVs,_Rule,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :-
  !.
translate_safe_negated_calls_list([T|Ts],[RT|RTs],NVs,SVsi,SVso,Rule,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :-
  !, 
  translate_safe_negated_calls(T,RT,NVs,SVsi,SVsi1,Rule,IArgs,E,RuleNVsListi,RuleNVsList1,IArgsListi,IArgsList1), 
  translate_safe_negated_calls_list(Ts,RTs,NVs,SVsi1,SVso,Rule,IArgs,E,RuleNVsList1,RuleNVsListo,IArgsList1,IArgsListo).
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translating OR calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% translate_or_calls_list(+RuleNVsList,-DRuleNVsList,+IArgsList,-DArgsList,-Exploded)
translate_or_calls_list(RuleNVsList,DRuleNVsList,IArgsList,DArgsList,Exploded) :-
  translate_or_calls_list(RuleNVsList,IArgsList,[],[],DRuleNVsList,DArgsList,Exploded).
  
% translate_or_calls_list(+RuleNVsList,+IArgsList,+DRuleNVsList1,+DIArgsList1,-DRuleNVsList2,-DIArgsList2,-Exploded)
translate_or_calls_list([],[],DRuleNVsList,DIArgsList,DRuleNVsList,DIArgsList,_Exploded).
translate_or_calls_list([RuleNVs|RuleNVsList],[IArgs|IArgsList],DRuleNVsListi,DIArgsListi,DRuleNVsListo,DIArgsListo,Exploded) :-
  translate_or_calls_ruleNVs(RuleNVs,IArgs,DRuleNVsList1,DIArgsList1,Exploded),
  translate_or_calls_list(RuleNVsList,IArgsList,DRuleNVsList1,DIArgsList1,DRuleNVsList2,DIArgsList2,Exploded),
  append(DRuleNVsListi,DRuleNVsList2,DRuleNVsListo),
  append(DIArgsListi,DIArgsList2,DIArgsListo).
% translate_or_calls_list([RuleNVs|RuleNVsList],[IArgs|IArgsList],DRuleNVsListi,DIArgsListi,DRuleNVsListo,DIArgsListo,Exploded) :-
%   translate_or_calls_ruleNVs(RuleNVs,IArgs,DRuleNVsList1,DIArgsList1,Exploded),
%   translate_or_calls_list(RuleNVsList,IArgsList,DRuleNVsList1,DIArgsList1,DRuleNVsListo,DIArgsListo,Exploded).

% translate_or_calls_ruleNVs(+RuleNVs,+IArgs,-RuleNVsList,-IArgsList,-Exploded)
% translate_or_calls_ruleNVs((Rule,NVs),IArgs,[TRuleNVs|NCRuleNVsList],[IArgs|IArgsList],Exploded) :-
%   translate_or_calls_rule(Rule,TRuleNVs,NVs,IArgs,Exploded,NCRuleNVsList,IArgsList).
translate_or_calls_ruleNVs((Rule,NVs),IArgs,[TRuleNVs|NCRuleNVsList],[IArgs|IArgsList],Exploded) :-
  translate_or_calls_rule(Rule,TRuleNVs,NVs,IArgs,Exploded,NCRuleNVsList,IArgsList).
  
translate_or_calls_rule(':-'(H,B),(TRule,NVs),NVs,IArgs,Exploded,NCRuleNVsList,IArgsList) :-
  !,
  translate_or_calls(':-'(H,B),TRule,NVs,IArgs,Exploded,[],NCRuleNVsList,[],IArgsList).
translate_or_calls_rule(H,(H,NVs),NVs,_IArgs,_Exploded,[],[]).
  
translate_or_calls(T,T,_NVs,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :- 
%  (number(T) ; var(T) ; atom(T)),
  (var(T) ; atomic(T)),
  !.
translate_or_calls(or(L,R),or(TL,TR),NVs,_IArgs,Exploded,RNVsListi,RNVsListo,IArgsListi,IArgsListo) :- 
  !,
  translate_or_argument(or(L,R),or(TL,TR),NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo,Exploded).
translate_or_calls(C,RC,NVs,IArgs,Exploded,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :- 
  C =.. [F|As],
  !, 
  translate_or_calls_list(As,RAs,NVs,IArgs,Exploded,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo),
  RC =.. [F|RAs].

translate_or_calls_list([],[],_NVs,_IArgs,_E,RuleNVsList,RuleNVsList,IArgsList,IArgsList) :-
  !.
translate_or_calls_list([T|Ts],[RT|RTs],NVs,IArgs,E,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo) :-
  !, 
  translate_or_calls(T,RT,NVs,IArgs,E,RuleNVsListi,RuleNVsList1,IArgsListi,IArgsList1), 
  translate_or_calls_list(Ts,RTs,NVs,IArgs,E,RuleNVsList1,RuleNVsListo,IArgsList1,IArgsListo).

translate_or_argument(T,T,_NVs,RNVsList,RNVsList,IArgsList,IArgsList,_Exploded) :- 
%  (number(T) ; var(T) ; atom(T)),
  (var(T) ; atomic(T)),
  !.
translate_or_argument(L,L,_NVs,RNVsList,RNVsList,IArgsList,IArgsList,_Exploded) :-
  my_atom(L),
  !.
translate_or_argument(or(L,R),or(TL,TR),NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo,Exploded) :-
  !,
  translate_or_argument(L,TL,NVs,RNVsListi,RNVsListL,IArgsListi,IArgsListL,Exploded),
  translate_or_argument(R,TR,NVs,RNVsListL,RNVsListo,IArgsListL,IArgsListo,Exploded).
% translate_or_argument(B,H,NVs,RNVsListi,[(':-'(H,TB),TNVs)|RNVsListo],IArgsListi,[[]|IArgsListo],true) :-
%   !,
%   %WARNING: Build new IArgs appropriately
%   build_head_from_body(B,NVs,H),
%   translate_body_ors(B,TB,NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo,_),
%   term_variables([H,TB],TVs),
%   my_var_name_list(TVs,NVs,TNVs).
translate_or_argument(B,H,NVs,RNVsListi,RNVsListo,IArgsListi,[[]|IArgsListo],true) :-
  !,
  %WARNING: Build new IArgs appropriately
  build_head_from_body(B,NVs,H),
  translate_body_ors(B,TB,NVs,RNVsListi,RNVsListi1,IArgsListi,IArgsListo,_),
  term_variables([H,TB],TVs),
  my_var_name_list(TVs,NVs,TNVs),
  append(RNVsListi1,[(':-'(H,TB),TNVs)],RNVsListo).

translate_body_ors(or(L,R),TG,NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo,Exploded) :-
  !,
  translate_or_argument(or(L,R),TG,NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo,Exploded).
translate_body_ors((G,Gs),(TG,TGs),NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo,Exploded) :-
  !,
  translate_body_ors(G,TG,NVs,RNVsListi,RNVsList1,IArgsListi,IArgsList1,Exploded),
  translate_body_ors(Gs,TGs,NVs,RNVsList1,RNVsListo,IArgsList1,IArgsListo,Exploded).
translate_body_ors(G,G,_NVs,RNVsList,RNVsList,IArgsList,IArgsList,_Exploded).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translating aggregates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
% Translate aggregate predicates with compound goals, as in min((p(X,Y),X>Y),X,M) and group_by((p(X,Y),q(Y,Z)),[X],R=max(X))
translate_aggregates_ruleNVs_list([],[],[],[],_Simplified,_Exploded,_Unsafe).
translate_aggregates_ruleNVs_list([RuleNVs|RuleNVsList],RuleNVsListo,[IArgs|IArgsList],IArgsListo,Simplified,Exploded,Unsafe) :-
  translate_aggregates(RuleNVs,IArgs,RuleNVsList1,IArgsList1,Simplified,Exploded,Unsafe),
  translate_aggregates_ruleNVs_list(RuleNVsList,RuleNVsList2,IArgsList,IArgsList2,Simplified,Exploded,Unsafe),
  append(RuleNVsList1,RuleNVsList2,RuleNVsListo),
  append(IArgsList1,IArgsList2,IArgsListo).

translate_aggregates((':-'(H,B),NVs),IArgs,[(':-'(H,TB),RNVs)|RuleNVsList],[IArgs|IArgsList],Simplified,Exploded,Unsafe) :-
  !,
  term_variables(H,HVs),
  my_term_variables_bag([H,B],RVs),
  translate_body_aggregates(B,TB,HVs,RVs,NVs,NNVs,[],RuleNVsList,[],IArgsList,Simplified,Exploded,Unsafe),
  term_variables([H,TB],TVs),
  my_var_name_list(TVs,NNVs,RNVs).
translate_aggregates((H,NVs),IArgs,[(H,NVs)],[IArgs],_Simplified,_Exploded,_Unsafe).

translate_body_aggregates(T,T,_HVs,_RVs,NVs,NVs,RuleNVsList,RuleNVsList,IArgsList,IArgsList,_Simplified,_Exploded,_Unsafe) :-
  (atomic(T) ; var(T)),
  !.
translate_body_aggregates(group_by(P,GBVs,C),group_by(TP,GBPs,GBVs,TC),HVs,RVs,NVs,NNVs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,Simplified,Exploded,Unsafe) :-
  !,
  translate_aggregate_goal(P,TP,HVs,RVs,NVs,NNVs,RuleNVsListi,RuleNVsListi1,IArgsListi,IArgsListi1,Exploded,Unsafe),
  term_variables(P,GroundVars),
  input_args([],HVs,GroundVars,IArgs),
  group_by_positions(GBVs,TP,GBPs),
  translate_aggregate_cond(C,TC,HVs,NVs,RuleNVsListi1,RuleNVsListo,IArgs,IArgsListi1,IArgsListo,Simplified,Exploded,Unsafe).
translate_body_aggregates(G,TG,HVs,RVs,NVs,NNVs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,_Simplified,Exploded,Unsafe) :-
  G=..[AF,P|Args],
  length([P|Args],Arity),
%  (my_aggregate_relation(AF,Arity) ; (AF,Arity)==(group_by,3)),
  my_aggregate_relation(AF,Arity),
  valid_aggregate_argument_list(Args,NVs),
  !,
  translate_aggregate_goal(P,TP,HVs,RVs,NVs,NNVs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,Exploded,Unsafe),
  term_variables(P,PVs),
  my_intersect_var(HVs,PVs,GBVs),
  insert_into_last_but_one_pos(Args,GBVs,RArgs),
  TG=..[AF,TP|RArgs].
translate_body_aggregates(G,TG,HVs,RVs,NVs,NNVs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,Simplified,Exploded,Unsafe) :- 
  G =.. [F|As],
  !, 
  translate_body_aggregates_list(As,RAs,HVs,RVs,NVs,NNVs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,Simplified,Exploded,Unsafe),
  TG =.. [F|RAs].
      
translate_body_aggregates_list([],[],_HVs,_RVs,NVs,NVs,RuleNVsList,RuleNVsList,IArgsList,IArgsList,_Simplified,_Exploded,_Unsafe).
translate_body_aggregates_list([T|Ts],[TT|TTs],HVs,RVs,NVs,NNVs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,Simplified,Exploded,Unsafe) :-
  !, 
  translate_body_aggregates(T,TT,HVs,RVs,NVs,TNVs,RuleNVsListi,RuleNVsListi1,IArgsListi,IArgsList1,Simplified,Exploded,Unsafe), 
  translate_body_aggregates_list(Ts,TTs,HVs,RVs,TNVs,NNVs,RuleNVsListi1,RuleNVsListo,IArgsList1,IArgsListo,Simplified,Exploded,Unsafe).

translate_aggregate_goal(G,G,_HVs,_RVs,NVs,NVs,RuleNVsList,RuleNVsList,IArgsList,IArgsList,_Exploded,_Unsafe) :-
%  my_literal(G),
  my_atom(G),
  !.
translate_aggregate_goal(G,TP,HVs,RVs,NVs,NNVs,RuleNVsListi,[(':-'(CTP,CTG),CNVs)|RuleNVsListo],IArgsListi,[IArgs|IArgsListo],true,Unsafe) :-
  translate_body_aggregates(G,TG,HVs,RVs,NVs,_NNVs,RuleNVsListi,RuleNVsListo,IArgsListi,IArgsListo,_Simplified,_Exploded,Unsafe),
  my_term_variables_bag(G,GVs),
  my_bag_diff(RVs,GVs,BVs),
  my_set_inter(BVs,GVs,DVs),
  remove_duplicates_var(DVs,NAVs),
  remove_duplicates_var(GVs,Vs),
  my_set_diff(Vs,NAVs,AVs),
  set_anonymous_var_name_list(NVs,AVs,NNVs),
  'Vs2NVs'(Vs,NVs,RNVs),
  get_new_predicate_name(p,P),
  build_head(P,RNVs,TP),
  copy_term([TP,TG,NVs],[CTP,CTG,CCNVs]),
  term_variables([CTP,CTG],CVs),
  filter_NVs(CCNVs,CVs,CNVs),
  length(RNVs,L),
  from(1,L,IArgs).

set_anonymous_var_name_list([],_AVs,[]).
set_anonymous_var_name_list([N=V|NVs],AVs,[AN=V|NNVs]) :-
  once(my_member_var(V,AVs)),
  \+ atom_concat('_',_,N), % Is not anonymous already
  !,
  atom_concat('_',N,AN),
  set_anonymous_var_name_list(NVs,AVs,NNVs).
set_anonymous_var_name_list([NV|NVs],AVs,[NV|NNVs]) :-
  set_anonymous_var_name_list(NVs,AVs,NNVs).
  
translate_aggregate_cond(C,TC,HVs,NVs,RuleNVsListi,RuleNVsListo,IArgs,IArgsListi,IArgsListo,Simplified,Exploded,Unsafe) :-
  replace_and_get_aggregates_equalities(C,[],LEQs,NVs,NSBody),
  (LEQs == []
   ->
    EQs=true
   ;
    my_list_to_tuple(LEQs,EQs)),
  simplify_body(NSBody,NVs,Body,SNVs),
  build_head_from_body(Body,SNVs,Head),
  Rule = ':-'(Head,Body),
  term_variables(Head,RRHVs),
  get_arg_position_list(GHVs,HVs,IArgs),
  input_args([],RRHVs,GHVs,IArgs1),
  term_variables(LEQs,GAVs),
  input_args(IArgs1,RRHVs,GAVs,IRArgs),
  term_variables(Rule,RVs),
  my_var_name_list(RVs,SNVs,RNVs),
  preprocess((Rule,RNVs),SiRuleNVList,SfRuleNVList,ExRuleNVList,IRArgs,IArgsList1,_Modes,exec,datalog,view,Causes,[no_safety],Unsafe),
  (my_member_chk(safed,Causes)
   ->
    append_goals(EQs,Head,TC),
    append(RuleNVsListi,SfRuleNVList,RuleNVsListo),
    append(IArgsListi,IArgsList1,IArgsListo),
    Exploded=true
   ;
   (my_member_chk(exploded,Causes)
    ->
    append_goals(EQs,Head,TC),
    append(RuleNVsListi,ExRuleNVList,RuleNVsListo),
    append(IArgsListi,IArgsList1,IArgsListo),
    Exploded=true
   ;
    (my_member_chk(simplified,Causes)
     ->
     SiRuleNVList=[(':-'(Head,TB),_TNVs)],
     append_goals(EQs,TB,TC),
     RuleNVsListo=RuleNVsListi,
     IArgsListo=IArgsListi,
     Simplified=true
     ;
     append_goals(EQs,Body,TC),
     RuleNVsListo=RuleNVsListi,
     IArgsListo=IArgsListi))).

% input_args(+IArgsListi,+HeadVars,+GroundVars,-IArgsListo)
% Ex: input_args([1],[X,Y,Z],[Z,U],[1,3])
input_args(IArgsListi,Vs,GVs,IArgsListo) :-
  my_set_inter(Vs,GVs,GHVs),
  get_arg_position_list(GHVs,Vs,Ps),
  my_set_union(IArgsListi,Ps,IArgsListo).
     
  
project_input_args_rule_list(Ros,Ris,IArgsListi,IArgsListo) :-
  my_zipWith('+',Ris,IArgsListi,RisIArgsListi),
  project_input_args_rule_list(Ros,RisIArgsListi,IArgsListo).
  
project_input_args_rule_list([],_RisIArgsListi,[]).
project_input_args_rule_list([Ro|Ros],RisIArgsListi,[IArgs|IArgsListo]) :-
  pred_rule(P,Ro),
  member(Ri+IArgs,RisIArgsListi),
  pred_rule(P,Ri),
  !,
  project_input_args_rule_list(Ros,RisIArgsListi,IArgsListo).
  
rule_head(R,R) :-
  R\=':-'(_,_),
  !.
rule_head(':-'(H,_),H).

% All variables in the head are input arguments
build_head_from_body(Body,NVs,Head) :-
  build_head_from_body_acc(Body,[],NVs,Head).
  
build_head_from_body_headname(Body,HeadName,NVs,Head) :-  
  build_head_from_body_name_acc(Body,HeadName,[],NVs,Head).

build_head_from_body_acc(Body,UVs,NVs,Head) :-  
  get_new_predicate_name(p,HeadName),
  build_head_from_body_name_acc(Body,HeadName,UVs,NVs,Head).
  
build_head_from_body_name_acc(Body,HeadName,UVs,NVs,Head) :-  
  term_variables(Body,BVs),
%  my_reverse(BVs,RBVs),
  my_var_name_list(BVs,NVs,BNVs),
  relevant_NVs(Body,BNVs,RNVs), % Non-relevant vars are set vars in aggregates, as X in avg(p(X),X,Y)
  filter(BNVs,RNVs,OURNVs), % Keep the same order as NVs
  term_variables(OURNVs,OURVs),
%  remove_underscored_variables_list(OURVs,NVs,ORVs),
  remove_one_var_list(UVs,OURVs,RVs),
  my_var_name_list(RVs,NVs,ORNVs),
  build_head(HeadName,ORNVs,Head).
    
% Gets equalities aggregate(Variable)=Result from a term including aggregates. It also replaces aggregate(Variable) by Result in the term
replace_and_get_aggregates_equalities(T,Eqs,Eqs,_NVs,T) :- 
  var(T),
  !.
replace_and_get_aggregates_equalities('$NULL'(ID),Eqs,Eqs,_NVs,'$NULL'(ID)) :- 
  !.
replace_and_get_aggregates_equalities(R=C,Eqs,Eqs,_NVs,R=C) :- 
  var(R),
  var(C),
  !.
replace_and_get_aggregates_equalities(T,Eqs,Eqs,_NVs,T) :- 
  atomic(T),
  !.
replace_and_get_aggregates_equalities(C,Eqsi,Eqso,NVs,RC) :- 
  C =.. [Op,A1,A2],
  my_infix_comparison(Op,_),
  !,
  replace_and_get_aggregates_equalities_expr(A1,Eqsi,Eqsi1,NVs,RA1),
  replace_and_get_aggregates_equalities_expr(A2,Eqsi1,Eqso,NVs,RA2),
  RC =.. [Op,RA1,RA2].
replace_and_get_aggregates_equalities(C,Eqsi,Eqso,NVs,RC) :- 
  C =.. [F|As],
  replace_and_get_aggregates_equalities_list(As,Eqsi,Eqso,NVs,RAs),
  RC =.. [F|RAs].

replace_and_get_aggregates_equalities_list([],Eqs,Eqs,_NVs,[]) :-
  !.
replace_and_get_aggregates_equalities_list([T|Ts],Eqsi,Eqso,NVs,[RT|RTs]) :-
  replace_and_get_aggregates_equalities(T,Eqsi,Eqsi1,NVs,RT), 
  replace_and_get_aggregates_equalities_list(Ts,Eqsi1,Eqso,NVs,RTs).
  
replace_and_get_aggregates_equalities_expr(T,Eqs,Eqs,_NVs,T) :- 
  var(T),
  !.
replace_and_get_aggregates_equalities_expr('$NULL'(ID),Eqs,Eqs,_NVs,'$NULL'(ID)) :- 
  !.
replace_and_get_aggregates_equalities_expr(R=C,Eqs,Eqs,_NVs,R=C) :- 
  var(R),
  atom(C),
  arithmetic_function(C,_,_,aggregate,_,0),
  !.
replace_and_get_aggregates_equalities_expr(C=R,Eqs,Eqs,_NVs,C=R) :- 
  var(R),
  atom(C),
  arithmetic_function(C,_,_,aggregate,_,0),
  !.
replace_and_get_aggregates_equalities_expr(R=C,Eqsi,Eqso,NVs,NR=R) :- 
  var(R),
  C =.. [F,V],
  arithmetic_function(F,_,_,aggregate,_,1),
  valid_aggregate_argument(V,NVs),
  !,
  (my_member_var(C,R=C,Eqsi)
   ->
    Eqso=Eqsi
   ;
    Eqso=[NR=C|Eqsi]).
replace_and_get_aggregates_equalities_expr(C=R,Eqsi,Eqso,NVs,NR=R) :- 
  var(R),
  C =.. [F,V],
  arithmetic_function(F,_,_,aggregate,_,1),
  valid_aggregate_argument(V,NVs),
  !,
  (my_member_var(C,R=C,Eqsi)
   ->
    Eqso=Eqsi
   ;
    Eqso=[NR=C|Eqsi]).
replace_and_get_aggregates_equalities_expr(C,Eqsi,Eqso,NVs,R) :- 
  C =.. [F|Vs],
  length(Vs,Arity),
  arithmetic_function(F,_,_,aggregate,_,Arity),
  valid_aggregate_argument_list(Vs,NVs),
  !,
  (my_member_var(C,R=C,Eqsi)
   ->
    Eqso=Eqsi
   ;
    Eqso=[R=C|Eqsi]).
replace_and_get_aggregates_equalities_expr(T,Eqs,Eqs,_NVs,T) :- 
  atomic(T),
  !.
replace_and_get_aggregates_equalities_expr(C,Eqsi,Eqso,NVs,RC) :- 
  C =.. [F|As],
%  \+ arithmetic_function(F,_,_,aggregate,_,_Arity),
  replace_and_get_aggregates_equalities_expr_list(As,Eqsi,Eqso,NVs,RAs),
  RC =.. [F|RAs].
    
replace_and_get_aggregates_equalities_expr_list([],Eqs,Eqs,_NVs,[]) :-
  !.
replace_and_get_aggregates_equalities_expr_list([T|Ts],Eqsi,Eqso,NVs,[RT|RTs]) :-
  replace_and_get_aggregates_equalities_expr(T,Eqsi,Eqsi1,NVs,RT), 
  replace_and_get_aggregates_equalities_expr_list(Ts,Eqsi1,Eqso,NVs,RTs).

valid_aggregate_argument_list([],_NVs).
valid_aggregate_argument_list([A|As],NVs) :-
  valid_aggregate_argument(A,NVs),
  valid_aggregate_argument_list(As,NVs).
  
valid_aggregate_argument(A,_NVs) :-
  \+ include_aggregate(A),
  !.
valid_aggregate_argument(A,NVs) :-
  my_raise_exception(generic,syntax(['An aggregate cannot include another aggregate (',A,').']),NVs).

include_aggregate(A) :-
  arithmetic_function(F,_,_,aggregate,_,Arity),
  length(As,Arity),
  T=..[F|As],
  my_member_term(T,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translating outer joins
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% translate_outer_joins_list
translate_outer_joins_list(RNVsList,TRNVsList,IArgsListi,IArgsListo,Exploded) :-
  translate_outer_joins_rule_list(RNVsList,ORNVsList,IArgsListi,IArgsListo,Exploded),
  (Exploded==true
   ->
    ORNVsList=[RNVs|RNVss],
    copy_term_list(RNVss,CRNVss),
    TRNVsList=[RNVs|CRNVss]
   ;
    TRNVsList=ORNVsList).

translate_outer_joins_rule_list([],[],[],[],_Exploded).
translate_outer_joins_rule_list([RNVs|RNVsList],TRNVsList,[IArgs|IArgsListi],IArgsListo,Exploded) :-
  translate_outer_joins(RNVs,IArgs,TRNVsList1,IArgsList1,Exploded),
  translate_outer_joins_rule_list(RNVsList,TRNVsList2,IArgsListi,IArgsList2,Exploded),
  append(TRNVsList1,TRNVsList2,TRNVsList),
  append(IArgsList1,IArgsList2,IArgsListo).

translate_outer_joins((':-'(Head,Body),NVs),IArgs,[(':-'(Head,TBody),TNVs)|RNVsList],[IArgs|IArgsList],Exploded) :-
  !,
  %WARNING: IArgs should be adequately projected
  translate_body_outer_joins(Body,TBody,NVs,[],RNVsList,[],IArgsList,Exploded),
  term_variables([Head,TBody],RVs),
%  var_names(RVs,NVs,TNVs),
  my_var_name_list(RVs,NVs,TNVs).
translate_outer_joins(HNVs,IArgs,[HNVs],[IArgs],_Exploded).

translate_body_outer_joins(OJG,TG,NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo,Exploded) :-
  OJG =.. [OJ,_L,_R,_C],
  my_outer_join_relation(OJ/_Arity),
  !,
  translate_outer_join_argument(OJG,TG,NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo,Exploded).
translate_body_outer_joins((G,Gs),(TG,TGs),NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo,Exploded) :-
  !,
  translate_body_outer_joins(G,TG,NVs,RNVsListi,RNVsList1,IArgsListi,IArgsList1,Exploded),
  translate_body_outer_joins(Gs,TGs,NVs,RNVsList1,RNVsListo,IArgsList1,IArgsListo,Exploded).
translate_body_outer_joins(G,G,_NVs,RNVsList,RNVsList,IArgsList,IArgsList,_Exploded).
  
my_outer_join_relation(lj/3).
my_outer_join_relation(rj/3).
my_outer_join_relation(fj/3).

translate_outer_join_argument(T,T,_NVs,RNVsList,RNVsList,IArgsList,IArgsList,_Exploded) :- 
%  (number(T) ; var(T) ; atom(T)),
  (var(T) ; atomic(T)),
  !.
translate_outer_join_argument(L,L,_NVs,RNVsList,RNVsList,IArgsList,IArgsList,_Exploded) :-
  my_atom(L),
  !.
translate_outer_join_argument(OJG,TOJG,NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo,true) :-
  OJG =.. [OJ,L,R,C],
  my_outer_join_relation(OJ/3),
  OJ \== fj,
  !,
  translate_outer_join_argument(L,TL,NVs,RNVsListi,RNVsListL,IArgsListi,IArgsListL,_ExplodedL),
  translate_outer_join_argument(R,TR,NVs,RNVsListL,RNVsListR,IArgsListL,IArgsListR,_ExplodedR),
  get_new_predicate_name(p,P),
  TL =.. [LF|LFs],
  TR =.. [RF|RFs],
%   (my_outer_join_relation(LF/_LFAr) -> (LFs=[XL], XL=..[_XLF|LArgs]) ; TL =.. [_LFT|LArgs]),
%   (my_outer_join_relation(RF/_RFAr) -> (RFs=[XR], XR=..[_XRF|RArgs]) ; TR =.. [_RFT|RArgs]),
  (LF=st -> (LFs=[XL], XL=..[_XLF|LArgs]) ; TL =.. [_LFT|LArgs]),
  (RF=st -> (RFs=[XR], XR=..[_XRF|RArgs]) ; TR =.. [_RFT|RArgs]),
  append(LArgs,RArgs,Args),
  TG =.. [P|Args],
%  TOJG =.. [OJ,TG],
  TOJG =.. [st,TG],
  build_outer_join_tuple(OJ,LArgs,RArgs,NArgs),
  TNG =.. [P|NArgs],
  build_datalog_rules_outer_join(OJ,TG,TL,TR,C,TNG,NVs,RNVsListR,RNVsListo,IArgsListR,IArgsListo).
translate_outer_join_argument(fj(L,R,C),TOJG,NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo,true) :-
  !,
  translate_outer_join_argument(L,TL,NVs,RNVsListi,RNVsListL,IArgsListi,IArgsListL,_ExplodedL),
  translate_outer_join_argument(R,TR,NVs,RNVsListL,RNVsListR,IArgsListL,IArgsListR,_ExplodedR),
  get_new_predicate_name(p,P),
  TL =.. [LF|LFs],
  TR =.. [RF|RFs],
  (my_outer_join_relation(LF/_LFAr) -> (LFs=[XL], XL=..[_LFX|LArgs]) ; TL =.. [_LFT|LArgs]),
  (my_outer_join_relation(RF/_RFAr) -> (RFs=[XR], XR=..[_RFX|RArgs]) ; TR =.. [_RFT|RArgs]),
  append(LArgs,RArgs,Args),
  TG =.. [P|Args],
%  TOJG =.. [fj,TG],
  TOJG =.. [st,TG],
  build_outer_join_tuple(lj,LArgs,RArgs,LNArgs),
  build_outer_join_tuple(rj,LArgs,RArgs,RNArgs),
  LTNG =.. [P|LNArgs],
  RTNG =.. [P|RNArgs],
  build_datalog_rules_outer_join(fj,TG,TL,TR,C,LTNG,RTNG,NVs,RNVsListR,RNVsListo,IArgsListR,IArgsListo).
% translate_outer_join_argument(C,RC,NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo,Exploded) :- 
%   C =.. [F|As],
%   !, 
%   translate_outer_join_argument_list(As,RAs,NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo,Exploded),
%   RC =.. [F|RAs].
translate_outer_join_argument(B,H,NVs,RNVsListi,[(':-'(H,TB),TNVs)|RNVsListo],IArgsListi,[[]|IArgsListo],true) :-
  !,
  %WARNING: Build new IArgs appropriately
  build_head_from_body(B,NVs,H),
  translate_body_outer_joins(B,TB,NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo,_),
  term_variables([H,TB],TVs),
  my_var_name_list(TVs,NVs,TNVs).
% translate_outer_join_argument(L,L,_NVs,RNVsList,RNVsList,IArgsList,IArgsList,_Exploded) :-
%   my_raise_exception(L,basic_goal,[]).

% translate_outer_join_argument_list([],[],_NVs,RNVsList,RNVsList,IArgsList,IArgsList,_Exploded).
% translate_outer_join_argument_list([A|As],[TA|TAs],NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo,Exploded) :-
%   translate_outer_join_argument(B,TB,NVs,RNVsListi,RNVsList1,IArgsListi,IArgsList1,Exploded),
%   translate_outer_join_argument_list(As,TAs,NVs,RNVsList1,RNVsListo,IArgsList1,IArgsListo,Exploded).
  
build_datalog_rules_outer_join(lj,TG,TL,TR,C,TNG,NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo) :-
  TG =.. [_G|Args],
  get_new_predicate_name(p,P),
  TG1 =.. [P|Args],
  term_variables(TG1,TG1Vs),
  term_variables([TNG,TL],TVs),
  my_set_diff(TG1Vs,TVs,AVs),
  (C == true
   ->
    copy_term(([':-'(TG,TG1),':-'(TNG,(TL,not(TG1))),':-'(TG1,(TL,TR))],NVs,AVs),(Rs,CNVs,CAVs))
   ;
    copy_term(([':-'(TG,TG1),':-'(TNG,(TL,not(TG1))),':-'(TG1,(TL,TR,C))],NVs,AVs),(Rs,CNVs,CAVs))
  ),
  rule_to_ruleNVs_list(Rs,CNVs,[RNVs1,(R2,NVs2),RNVs3]),
  rename_anonymous_vars(NVs2,CAVs,TNVs2),
  append([RNVs1,(R2,TNVs2),RNVs3],RNVsListi,RNVsListo),
  no_input_arguments_list(Rs,IArgsList), % WARNING: IArgs should be projected.
  append(IArgsList,IArgsListi,IArgsListo).
build_datalog_rules_outer_join(rj,TG,TL,TR,C,TNG,NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo) :-
  build_datalog_rules_outer_join(lj,TG,TR,TL,C,TNG,NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo).
build_datalog_rules_outer_join(fj,TG,TL,TR,C,LTNG,RTNG,NVs,RNVsListi,RNVsListo,IArgsListi,IArgsListo) :-
  TG =.. [_G|Args],
  get_new_predicate_name(p,P1),
  get_new_predicate_name(p,P2),
  TG1 =.. [P1|Args],
  TG2 =.. [P2|Args],
  term_variables(TG1,TG1Vs),
  term_variables([LTNG,TL],LTVs),
  my_set_diff(TG1Vs,LTVs,LAVs),
  term_variables(TG2,TG2Vs),
  term_variables([RTNG,TR],RTVs),
  my_set_diff(TG2Vs,RTVs,RAVs),
  (C == true
   ->
    copy_term(([':-'(TG,TG1),':-'(LTNG,(TL,not(TG1))),':-'(RTNG,(TR,not(TG2))),':-'(TG1,(TL,TR)),':-'(TG2,TG1)],NVs,LAVs,RAVs),(Rs,CNVs,CLAVs,CRAVs))
   ;
    copy_term(([':-'(TG,TG1),':-'(LTNG,(TL,not(TG1))),':-'(RTNG,(TR,not(TG2))),':-'(TG1,(TL,TR,C)),':-'(TG2,TG1)],NVs,LAVs,RAVs),(Rs,CNVs,CLAVs,CRAVs))
  ),
  rule_to_ruleNVs_list(Rs,CNVs,[RNVs1,(R2,NVs2),(R3,NVs3),RNVs4,RNVs5]),
  rename_anonymous_vars(NVs2,CLAVs,TNVs2),
  rename_anonymous_vars(NVs3,CRAVs,TNVs3),
  append([RNVs1,(R2,TNVs2),(R3,TNVs3),RNVs4,RNVs5],RNVsListi,RNVsListo),
  no_input_arguments_list(Rs,IArgsList), % WARNING: IArgs should be projected.
  append(IArgsList,IArgsListi,IArgsListo).

build_outer_join_tuple(lj,LArgs,RArgs,Args) :-
  length(RArgs,N),
  build_null_list(N,NULLs),
  append(LArgs,NULLs,Args).
build_outer_join_tuple(rj,LArgs,RArgs,Args) :-
  length(LArgs,N),
  build_null_list(N,NULLs),
  append(NULLs,RArgs,Args).

build_null_list(0,[]) :-
  !.
build_null_list(N,['$NULL'(_ID)|NULLs]) :-
  N1 is N-1,
  build_null_list(N1,NULLs).

rename_anonymous_vars([],_,[]).
rename_anonymous_vars([N=V|NVs],Vs,[N=V|ANVs]) :-
  atom_concat('_',_,N),
  !,
  rename_anonymous_vars(NVs,Vs,ANVs).
rename_anonymous_vars([N=V|NVs],Vs,[AN=V|ANVs]) :-
  my_member_var(V,Vs),
  !,
  atom_concat('_',N,AN),
  rename_anonymous_vars(NVs,Vs,ANVs).
rename_anonymous_vars([NV|NVs],Vs,[NV|ANVs]) :-
  rename_anonymous_vars(NVs,Vs,ANVs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% simplify_rules(+DDLsts,-DLsts,?Simplify,-Simplified) 
% Simplifies a list of Datalog rules w.r.t.:
% - Expressions in  with equalities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For rules along with NVs
simplify_ruleNVs_list(RuleNVsList,SRuleNVsList,Simplify,Simplified) :-
  ((simplification(on) ; Simplify==simplify)
   ->
    force_simplify_ruleNVsList(RuleNVsList,SRuleNVsList,Simplified)
   ;
    SRuleNVsList=RuleNVsList).

force_simplify_ruleNVsList([],[],_S).
force_simplify_ruleNVsList([R|Rs],[SR|SRs],S) :-
  force_simplify_ruleNVs(R,SR,S),
  force_simplify_ruleNVsList(Rs,SRs,S).

force_simplify_ruleNVs((R,NVs),(SR,SNVs),S) :-
  R = ':-'(H,B),
  !,
  copy_term((':-'(H,B),NVs),(':-'(CH,CB),CNVs)),
  simplify_body(CB,CNVs,SB1,SCNVs1,S),
  simplify_body(SB1,SCNVs1,SB,SCNVs,S),
  (SB==true -> SR=CH ; SR = ':-'(CH,SB)),
  term_variables(SR,Vs),
  my_var_name_list(Vs,SCNVs,SNVs).
%  remove_NV_alias(SCNVs,SNVs).
force_simplify_ruleNVs(RNVs,RNVs,_S).
  
% % For rules 
% simplify_rules(Rs,SRs,Simplify,Simplified) :-
%   ((simplification(on) ; Simplify==simplify) ->
%     force_simplify_rules(Rs,SRs,Simplified)
%    ;
%     SRs=Rs).

force_simplify_rules([],[],_S).
force_simplify_rules([R|Rs],[SR|SRs],S) :-
  force_simplify_rule(R,SR,S),
  force_simplify_rules(Rs,SRs,S).

force_simplify_rule(R,SR,S) :-
  R = ':-'(H,B),
  !,
  copy_term(':-'(H,B),':-'(CH,CB)),
  simplify_body(CB,[],SB1,_,S),
  simplify_body(SB1,[],SB,_,S),
  (SB==true -> SR=CH ; SR = ':-'(CH,SB)).
force_simplify_rule(R,R,_S).
  
simplify_body(Body,NVs,SBody,SNVs,Simplified) :-
  simplify_body(Body,NVs,SBody,SNVs),
  (Body==SBody
   ->
    true
   ;
    Simplified=true).

% simplify_body(B,NVs,B,NVs) :- 
%   var(B), 
%   !.
simplify_body((true,Bs),NVs,SBs,SNVs) :-
  !,
  simplify_body(Bs,NVs,SBs,SNVs).
simplify_body((Bs,true),NVs,SBs,SNVs) :-
  !,
  simplify_body(Bs,NVs,SBs,SNVs).
simplify_body((false,_Bs),NVs,false,NVs) :-
  !.
simplify_body((Bs,false),NVs,false,NVs) :- 
  ground(Bs), %Bs cannot be removed if non-ground because type information can be lost
  !.
simplify_body((false;Bs),NVs,SBs,SNVs) :-
  !,
  simplify_body(Bs,NVs,SBs,SNVs).
simplify_body((Bs;false),NVs,SBs,SNVs) :-
  !,
  simplify_body(Bs,NVs,SBs,SNVs).
simplify_body((dual;Bs),NVs,SBs,SNVs) :-
  !,
  simplify_body(Bs,NVs,SBs,SNVs).
simplify_body((Bs;dual),NVs,SBs,SNVs) :-
  !,
  simplify_body(Bs,NVs,SBs,SNVs).
simplify_body((B1s;B2s),NVs,SBs,SNVs) :-
  term_variables(B1s,L1),
  term_variables(B1s,L2),
  my_set_inter(L1,L2,[]),
  simplify_body(B1s,NVs,SB1s,SNV1s),
  simplify_body(B2s,NVs,SB2s,SNV2s),
  (B1s\==SB1s ; B2s\==SB2s),
  !,
  (SB1s == false
   ->
    SBs = SB2s,
    SNVs = SNV2s
   ;
    (SB2s == false
     ->
      SBs = SB1s,
      SNVs = SNV1s
     ;
      simplify_body((SB1s;SB2s),NVs,SBs,SNVs))).
simplify_body((B,Bs),NVs,SBody,SNVs) :-
  !,
  simplify_goal(B,NVs,SB,SGNVs),
  simplify_body(Bs,NVs,SBs,SBNVs),
  append_goals(SB,SBs,SBody),
  my_set_union(SGNVs,SBNVs,SNVs).
simplify_body(B,NVs,SB,SNVs) :-
  !,
  simplify_goal(B,NVs,SB,SNVs).
    
% Simplify goal:
simplify_goal(distinct(true),NVs,true,NVs) :-
  !.
% - 'Var is CteExpr' is simplified to 'Var=CteExpr',
%   where CteExpr is ground
simplify_goal(A is B,NVs,G,SNVs) :-
  my_ground(B),
  !,
  eval_expr(B,EB,_),
  simplify_goal(A=EB,NVs,G,SNVs).
% - Expressions in equalities are evaluated
% - Equalities relating variables are unified
% - Equalities relating variables and constants 
%   are NOT unified in order to keep track of 
%   them for LogiQL translations
% simplify_goal(AOpB,NVs,G,NVs) :-
%   logiql(on),
%   AOpB=..[Op,A,B],
%   my_infix_comparison(Op,_),
%   !,
%   (my_ground(A) -> eval_expr(A,EA,_) ; EA = A),
%   (my_ground(B) -> eval_expr(B,EB,_) ; EB = B),
%   (Op == '=',
%    my_noncompound_term(EA),
%    my_noncompound_term(EB),
%    \+ (var(EA),
%        my_ground(EB)
%         ;
%        var(EB),
%        my_ground(EA))
%    ->
%     (EA=EB -> G=true ; G=false)
%    ;
%     G=..[Op,EA,EB]).
% - Equalities relating variables and constants 
%   ARE unified for better simplifications
simplify_goal(AOpB,NVs,G,NVs) :-
%  logiql(off),
  AOpB=..[Op,A,B],
  my_infix_comparison(Op,_),
  !,
  (my_ground(A) -> eval_expr(A,EA,_) ; EA = A),
  (my_ground(B) -> eval_expr(B,EB,_) ; EB = B),
  (Op == '=',         % Equality
%    my_noncompound_term(EA),
%    my_noncompound_term(EB)
   my_var_or_constant(EA),
   my_var_or_constant(EB)
   ->
    (EA=EB -> G=true ; G=false)
   ;
    (Op == '\\=',     % Disequality
     my_constant(EA),
     my_constant(EB)
%      my_noncompound_term(EA),
%      my_noncompound_term(EB)
     ->
      (compute_primitive(EA\=EB,_) -> G=true ; G=false)
     ;
      (my_ground(EA), % Other built-ins
       my_ground(EB)
       ->
      (P=..[Op,EA,EB],
       compute_primitive(P,_) -> G=true ; G=false)
      ;
       G=..[Op,EA,EB]))).
simplify_goal(not(true),NVs,false,NVs) :-
  !.
simplify_goal(not(dual),NVs,false,NVs) :-
  !.
simplify_goal(not(false),NVs,true,NVs) :-
  !.
simplify_goal(not(B),NVs,G,SNVs) :-
  simplify_body(B,NVs,S,SNVs),
  S\==B,
  !,
  simplify_goal(not(S),NVs,G,_).
simplify_goal(dual,NVs,true,NVs) :-
  !.
% simplify_goal('=>'('$void',R),NVs,SR,SNVs) :-
%   !,
%   simplify_body(R,NVs,SR,SNVs).
simplify_goal('=>'(L,R),NVs,'=>'(SL,SR),SNVs) :-
  !,
  simplify_hypo(L,NVs,SL,SLNVs),
  simplify_body(R,NVs,SR,SRNVs),
  my_set_union(SLNVs,SRNVs,SNVs).
% simplify_goal(not(G),NotG) :-
%   G=..[DLop,L,R],
%   complement_DL_op(DLop,CDLop),
%   !,
%   NotG=..[CDLop,L,R].
simplify_goal(G,NVs,G,NVs).
  
simplify_hypo('/\\'(Rs,R),NVs,'/\\'(SRs,SR),SNVs) :-
  !,
  simplify_hypo(Rs,NVs,SRs,SRsNVs),
  simplify_hypo(R,NVs,SR,SRNVs),
  my_set_union(SRsNVs,SRNVs,SNVs).
simplify_hypo(R,NVs,SR,SNVs) :-
  force_simplify_ruleNVs((R,NVs),(SR,SNVs),_S).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unfold_rules(+Rules,-URules) 
% Unfold a list of Datalog rules 
% A predicate p with only one rule and one goal so that no 
% negated call to p occurs in other rule is unfolded
% Adapted from 'The Art of Prolog', Sterling & Shapiro, 1986
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unfold_RNVss(RNVss,Unfold,IArgsList,UIArgsList,URNVss) :-
  ((unfold(on) ; Unfold==unfold)
   ->
%    my_unzip(RNVss,Rs,NVss),
%    concat_lists(NVss,NVs),
    unfold_RNVss(RNVss,URNVss),
    ruleNVs_to_rule_NVs_list(RNVss,Rs,_),
    ruleNVs_to_rule_NVs_list(URNVss,URs,_),
    project_input_args_rule_list(URs,Rs,IArgsList,UIArgsList)
  ;
    URNVss=RNVss,
    UIArgsList=IArgsList).
    
unfold_rules(Rs,URs) :-
  length(Rs,L),
  length(NVss,L),
  my_map_1('='([]),NVss),
  my_zipWith(',',Rs,NVss,RNVss),
  unfold_RNVss(RNVss,URNVss),
  my_unzip(URNVss,URs,_).

unfold_RNVss(Program,UProgram) :-
  my_unzip(Program,Rs,_NVss),
  build_pdg_from_rules(Rs,PDG),
  recursive_predicates(PDG,Preds),
  unfold_RNVss(Program,Preds,UProgram).
%   unfold_rules(Program,[],UProgram).

% unfold_RNVss(+Prog,+Preds,-UProg)
% Partial reduction applied to Prog, dealing to UProg.
% Calls to predicates in Preds must not be unfolded
unfold_RNVss(Prog,Ps,UProg) :-
  findall((PR,PNVs),(
     member((R,NVs),Prog), 
     (R=':-'(_,_) % Facts must not be tried to be unfolded
      ->
       partial_reduction(R,NVs,pred,[],Prog,Ps,PR,PNVs)
      ;
       PR=R,
       PNVs=NVs), 
     PR\==true
     ),
   PProg),
   remove_unused_rules(PProg,Ps,RProg),
   remove_duplicated_goals(RProg,UProg).  

remove_duplicated_goals([],[]).
remove_duplicated_goals([(R,NVs)|RNVss],[(RR,NVs)|RRNVss]) :-
  remove_duplicated_goals_rule(R,RR),
  remove_duplicated_goals(RNVss,RRNVss).
   
remove_duplicated_goals_rule((H:-B),(H:-RB)) :-
  !,
  my_list_to_tuple(Bs,B),
  remove_duplicates_var(Bs,RBs),
  my_list_to_tuple(RBs,RB).
remove_duplicated_goals_rule(R,R).
  
% partial_reduction(+Term,+Scope,+Heads,+Prog,+Preds,-ReducedTerm)
% Applies partial reduction to Term, dealing to ReducedTerm
% Heads is the list of heads that have been traversed already:
% If a rule with a unifiable head is tried again, it is discarded for further reduction.
% Prog is the program under which partial reduction is applied
% Preds are the predicates that must not be unfolded
% Scope is for what kind of predicate we are reducing:
% - A disjunction during reducing the argument of a metapredicate must not be unfolded
partial_reduction(true,NVs,_,_,_,_,true,NVs) :-
  !.
partial_reduction(-(H),NVs,Scope,Heads,Program,Preds,-(RH),PNVs) :-
  !,
  partial_reduction(H,NVs,Scope,Heads,Program,Preds,RH,PNVs).
partial_reduction((H:-B),NVs,Scope,Heads,Program,Preds,R,PNVs) :-
  !,
  partial_reduction(B,NVs,Scope,[H|Heads],Program,Preds,RB,PNVs),
  (RB==true -> R=H ; R=(H:-RB)).
% Particular cases for reduction:
partial_reduction((L=>distinct(R)),NVs,_Scope,Heads,Program,Preds,(UL=>distinct(RR)),PNVs) :-
  !,
  hypo_partial_reduction(L,NVs,Preds,UL,HNVs),
  metapredicate_partial_reduction_list([R],HNVs,Heads,Program,Preds,[RR],PNVs).
partial_reduction((L=>R),NVs,_Scope,Heads,Program,Preds,(UL=>RR),PNVs) :-
  !,
  hypo_partial_reduction(L,NVs,Preds,UL,HNVs),
  metapredicate_partial_reduction_list([R],HNVs,Heads,Program,Preds,[RR],PNVs).
partial_reduction((A,B),NVs,Scope,Heads,Program,Preds,PR,PNVs) :-
  !,
  partial_reduction(A,NVs, Scope,Heads,Program,Preds,PA,ANVs),
  partial_reduction(B,ANVs,Scope,Heads,Program,Preds,PB,PNVs),
  combine_reductions(PA,PB,PR).
% The following is not needed up to now:
% partial_reduction(A,_Heads,_Program,_Preds,B) :-
%   should_fold(A,B),
%   !.
% partial_reduction(A,_Heads,_Program,_Preds,A) :-
%   % Tables and views are not tried to be unfolded
%   % They can get names as primitive predicates (e.g. count)
%   functor(A,N,Ar),
%   my_table('$des',N,Ar),
%   !.
partial_reduction(not A,NVs,Scope,Heads,Program,Preds,not PR,PNVs) :-
  !,
  term_variables(A,Vs),
  length(Vs,L),
  (partial_reduction(A,NVs,Scope,Heads,Program,Preds,PR1,P1NVs),
   no_compound_unfolded_body(PR1),
   term_variables(PR1,PVs),
   length(PVs,L)
   ->
    PR=PR1,
    PNVs=P1NVs % Bindings cannot be reduced as in p(X,Y):-q(X,Y), not r(X,Y). r(X,1) which would be incorrectly reduced to p(X,1):-q(X,1), not r(X,1).
   ;
    PR=A,
    PNVs=NVs
  ).
partial_reduction(A,NVs,_Scope,Heads,Program,Preds,PR,PNVs) :-
  my_metapredicate_term_idxs_goals(A,Idxs,As),
%  allowed_reduction_metapredicate_goal(A,Program),
  metapredicate_partial_reduction_list(As,NVs,Heads,Program,Preds,RAs,PNVs),
  !,
  replace_ith_args_term(A,Idxs,RAs,RA),
  replace_metapredicate_partial_reduction(RA,A,PR).
partial_reduction(A,NVs,Scope,Heads,Program,Preds,Residue,PNVs) :-
  should_unfold(A,Scope,Heads,Program,Preds),
  !,
  copy_term(Program,CProgram),
  (member(((A:-B),BNVs),CProgram),
   partial_reduction(B,BNVs,Scope,[A|Heads],Program,Preds,Residue,P1NVs)
  ;
   member((A,ANVs),CProgram),
   (my_ground(A) -> Residue=true ; Residue=A),
   P1NVs=ANVs
  ),
  append(NVs,P1NVs,PNVs).
partial_reduction(A,NVs,_Scope,_Heads,_Program,_Preds,A,NVs).

% Reduction of the antecedent in an implication
hypo_partial_reduction(L,NVs,Ps,UL,ULNVs) :-
  !,
  rules_from_hyp_program(L,Rs),
  rule_pred_list(Rs,RPs),
  % User predicates defined in the antecedent must not be removed as 
  % others in the program might depend on them.
  user_predicate_list(RPs,UPs),
  append(Ps,UPs,NPs),
  length(Rs,N),
  length(NVss,N),
  my_map_1('='(NVs),NVss),
  my_zipWith(',',Rs,NVss,RNVss),
  unfold_RNVss(RNVss,NPs,URNVs),
  my_unzip(URNVs,URs,UNVs),
  rules_from_hyp_program(UL,URs),
  concat_lists(UNVs,CUNVs),
  term_variables(UL,UVs),
  my_var_name_list(UVs,CUNVs,ULNVs).

replace_metapredicate_partial_reduction(distinct(G),A,distinct(AVars,G)) :-
  term_variables(A,AVars),
  term_variables(G,GVars),
  my_set_diff(GVars,AVars,[_|_]),
  !.
replace_metapredicate_partial_reduction(G,_A,G).

metapredicate_partial_reduction_list([A],NVs,Heads,Program,Preds,[RA],RNVs):-
%   % Check there is only one clause matching the goal:
%   findall((A:-B),(member(A,Program), B=true ; member(':-'(A,B),Program)),[(A:-B)]),
  should_unfold(A,metapred,Heads,Program,Preds),
  partial_reduction(A,NVs,metapred,Heads,Program,Preds,RA,RNVs),
  no_compound_unfolded_body(RA),
  !.
metapredicate_partial_reduction_list([A],NVs,_Heads,_Program,_Preds,[A],NVs).
metapredicate_partial_reduction_list([A1,A2|As],NVs,Heads,Program,Preds,[RA1,RA2|RAs],RNVs) :-
  metapredicate_partial_reduction_list([A1],NVs,Heads,Program,Preds,[RA1],R1NVs),
  metapredicate_partial_reduction_list([A2|As],R1NVs,Heads,Program,Preds,[RA2|RAs],RNVs).

combine_reductions(true,B,B) :-
  !.
combine_reductions(A,true,A) :-
  !.
combine_reductions((A,B),C,(A,D)) :-
  !,
  combine_reductions(B,C,D).
combine_reductions(A,B,(A,B)).
  
% should_fold(_,_) :- fail.

% % should_unfold(+Atom,+Scope,+Heads,+Prog,+Preds)
% Root must not be unfolded
should_unfold(Goal,_Scope,_Heads,[(R,_)|_RNVss],_Preds) :- 
  functor(Goal,F,Ar),
  functor(R,F,Ar),
  !,
  fail.
% Recursive calls must not be unfolded.
% Heads seem to be not needed from now on
% should_unfold(Goal,_Scope,Heads,_Program,_Preds) :-
%   member(Goal,Heads),
%   !,
%   fail.
should_unfold(Goal,_Scope,_Heads,_Program,Preds) :-
  functor(Goal,N,A),
  member(N/A,Preds),
  !,
  fail.
should_unfold(A,metapred,_Heads,Program,_Preds) :-
  % Check there is only one clause for the predicate:
  findall((A:-B),member(((A:-B),_),Program),[_,_|_]),
  !,
  fail.
% should_unfold(Goal,metapred,Heads,Program,Preds) :-
%   % Variable aliasings must not be transferred to the call 'not Goal'
%   copy_term(Goal,A),
%   partial_reduction(A,pred,Heads,Program,Preds,_RA),
%   term_variables(Goal,GVs),
%   term_variables(A,AVs),
%   length(GVs,GL),
%   length(AVs,AL),
%   AL<GL,
%   !,
%   fail.
should_unfold(Goal,metapred,Heads,Program,Preds) :-
  % Unfolding not Goal -> not RGoal must not allowed when RGoal contains more variables than Goal
  language(Lang),
  Lang\==sql,
  Lang\==ra,
  copy_term(Goal,A),
  partial_reduction(A,[],pred,Heads,Program,Preds,RA,_),
  term_variables(A,AVs),
  term_variables(RA,RAVs),
  my_set_diff(RAVs,AVs,[_|_]),
  !,
  fail.
% Goals for which there are no matching rules must not be unfolded.
% (These goals can be solved with the asserted or external ODBC database)
% % Straight calls to "distinct" must not be unfolded
should_unfold(Goal,_Scope,_Heads,Program,_Preds) :-
  \+ \+ ((
   member((Goal,_),Program)
  ;
   member(((Goal:-_B),_),Program)
  )),
  !.

% Don't reduce metapredicates as they rely on strata to correct computation
no_compound_unfolded_body(H) :-
  functor(H,F,A),
  my_metapredicate(F,A),
  !,
  fail.
no_compound_unfolded_body((_B,_Bs)) :-
  !,
  fail.
no_compound_unfolded_body(_H).

%remove_unused_rules([],_Ps,[]).
remove_unused_rules([(R,NVs)|RNVss],Ps,[(R,NVs)|RRNVss]) :-
  rule_pred(R,P),
  my_unzip([(R,NVs)|RNVss],Rs,_),
  pred_calls_rule_list(Rs,[P],CPs),
  append(Ps,CPs,NPs),
  remove_pred_rules(RNVss,NPs,RRNVss).

pred_calls_rule_list([],Ps,Ps).
pred_calls_rule_list([R|Rs],IPs,OPs) :-
  pred_calls_rule(R,IPs,TPs),
  pred_calls_rule_list(Rs,TPs,OPs).

% A call to itself is not counted
pred_calls_rule((H:-B),IPs,OPs) :-
  !,
  rule_pred(H,P),
  pred_calls(B,[],TPs),
  my_remove(P,TPs,RPs),
  append(IPs,RPs,OPs).
pred_calls_rule(_H,Ps,Ps).

pred_calls((A,B),IPs,OPs) :-
  !,
  pred_calls(A,IPs,TPs),
  pred_calls(B,TPs,OPs).
% pred_calls(A,Ps,Ps) :-
%   functor(A,N,Ar),
%   my_table('$des',N,Ar),
%   !.
pred_calls(A,IPs,OPs) :-
  my_metapredicate_term_goals(A,Gs),
  !,
  pred_calls_list(Gs,IPs,OPs).
pred_calls(A,Ps,Ps) :-
  functor(A,N,Ar),
  my_builtin_pred(N/Ar),
  !.
pred_calls(A,Ps,[N/Ar|Ps]) :-
  functor(A,N,Ar).

pred_calls_list([],Ps,Ps).
pred_calls_list([G|Gs],IPs,OPs) :-
  pred_calls(G,IPs,TPs),
  pred_calls_list(Gs,TPs,OPs).

remove_pred_rules([],_Ps,[]).
% remove_pred_rules([R|Rs],Ps,[R|RRs]) :-
%   R\= (_:-_),
%   !,
%   remove_pred_rules(Rs,Ps,RRs).
remove_pred_rules([(R,NVs)|Rs],Ps,[(R,NVs)|RRs]) :-
  rule_pred(R,P),
  member(P,Ps),
  !,
  remove_pred_rules(Rs,Ps,RRs).
remove_pred_rules([_R|Rs],Ps,RRs) :-
  remove_pred_rules(Rs,Ps,RRs).
  
my_metapredicate(F,A) :-
  (my_builtin_relation(F,A,_,_) ; my_infix_relation(F,_) ; (F=not, A=1)),
  !.
  
my_metapredicate_term_goals(T,Gs) :-
  my_metapredicate_term_idxs_goals(T,_,TGs),
  my_metapredicate_term_goals_list(TGs,[],Gs).
  
my_metapredicate_term_goals_list([],Gs,Gs).
my_metapredicate_term_goals_list([TG|TGs],IGs,OGs) :-
  my_metapredicate_term_idxs_goals(TG,_,Gs),
  !,
  append(Gs,IGs,NGs),
  my_metapredicate_term_goals_list(TGs,NGs,OGs).
my_metapredicate_term_goals_list([TG|TGs],IGs,OGs) :-
  my_metapredicate_term_goals_list(TGs,[TG|IGs],OGs).

% my_metapredicate_term_idxs_goals(T,[1],[G]) :-
%   T=..[F,G],
%   my_metapredicate(F,1),
%   !.
%my_metapredicate_term_idxs_goals(st(G),[1],[G]).
my_metapredicate_term_idxs_goals(not(G),[1],[G]). 
my_metapredicate_term_idxs_goals('=>'(_L,R),[2],[R]).
my_metapredicate_term_idxs_goals(or(L,R),[1,2],[L,R]).
my_metapredicate_term_idxs_goals(lj(L,R,C),[1,2,3],[L,R,C]).
my_metapredicate_term_idxs_goals(rj(L,R,C),[1,2,3],[L,R,C]).
my_metapredicate_term_idxs_goals(fj(L,R,C),[1,2,3],[L,R,C]).
my_metapredicate_term_idxs_goals(top(_,G),[2],[G]).
my_metapredicate_term_idxs_goals(distinct(G),[1],[G]).
my_metapredicate_term_idxs_goals(distinct(_,G),[2],[G]).
my_metapredicate_term_idxs_goals(exists(_,G),[2],[G]).
my_metapredicate_term_idxs_goals(order_by(G,_,_),[1],[G]).
my_metapredicate_term_idxs_goals(group_by(G,_,C),[1,3],[G,C]).
%my_metapredicate_term_idxs_goals(group_by(G,_,_,C),[1,4],[G,C]).
my_metapredicate_term_idxs_goals(avg(G,_,_),[1],[G]).
my_metapredicate_term_idxs_goals(avg_distinct(G,_,_),[1],[G]).
% my_metapredicate_term_idxs_goals(count(G,_,_,_),[1],[G]).
% my_metapredicate_term_idxs_goals(count_distinct(G,_,_,_),[1],[G]).
my_metapredicate_term_idxs_goals(count(G,_),[1],[G]).
my_metapredicate_term_idxs_goals(count_distinct(G,_),[1],[G]).
my_metapredicate_term_idxs_goals(count(G,_,_),[1],[G]).
my_metapredicate_term_idxs_goals(count_distinct(G,_,_),[1],[G]).
my_metapredicate_term_idxs_goals(max(G,_,_),[1],[G]).
my_metapredicate_term_idxs_goals(min(G,_,_),[1],[G]).
my_metapredicate_term_idxs_goals(sum(G,_,_),[1],[G]).
my_metapredicate_term_idxs_goals(sum_distinct(G,_,_),[1],[G]).
my_metapredicate_term_idxs_goals(times(G,_,_),[1],[G]).
my_metapredicate_term_idxs_goals(times_distinct(G,_,_),[1],[G]).

datalog_metapredicate(F,N) :-
  my_metapredicate_term_idxs_goals(P,_,_),
  functor(P,F,N).


/************************************************************************/
/* Input Processing                                                     */
/* process_input(+String,-ContinueInputProcessing,+SurroundingNewLines) */
/************************************************************************/

process_input(SInput,Continue) :-
%(state(X) -> write_info_log(['Before: ',state(X)]) ; true),
  process_input(SInput,Continue,nl),
%(state(X) -> (atom_codes(I,SInput),write_info_log(['After : ',state(X),nl,I])) ; true),
  !.
process_input(_,yes) :-
  write_error_log(['Input processing error.']),
  nl_tapi_log.
  
process_input(end_of_file,yes,_NL) :-
  !.
process_input(SInput,Continue,NL) :-
  reset_elapsed_time,
  remove_initial_blanks(SInput,RInput),
  remove_ending_blanks(RInput,Input),
  % tapi, cd, ls and dir commands can end with a mandatory dot (e.g., "cd ." or "cd ..") 
% PERS
  ((
    my_command_input(Input,tapi)
    ;
    (((Command=cd ; Command=ls ; Command=dir), my_command_input(Input,Command))
     ->
     (parse_command(Command,[..],_NVsT,Input,[]); 
      parse_command(Command,[.],_NVsF,Input,[]))
    )
   ) 
   -> 
    CInput=Input, 
    !
   ;
   (append(CInput,".",Input), % Inputs are allowed to end with an optional dot
    !
    ; 
    CInput=Input)
  ),
% PERS
%CInput=Input,  
   ( 
     is_command(CInput),
     !,
     process_command(CInput,Continue)
    ; 
     (
      (NL==nl -> nl_compact_log ; true),
      (
       blank_input(CInput),
       store_elapsed_time(parsing),
       store_elapsed_time(computation),
       !
      ; 
       process_single_line_remark(CInput), 
       store_elapsed_time(parsing),
       store_elapsed_time(computation),
       !
      ; 
       process_multi_line_remark(CInput), 
       store_elapsed_time(parsing),
       store_elapsed_time(computation),
       !
      ;
       save_state_flags,
       language(datalog), 
       (process_datalog(CInput)
        ->
         true
        ;
         try_to_process_sql(CInput)
        ;
         try_to_process_ra(CInput)
        ;
         try_to_process_trc(CInput)
        ;
         try_to_process_drc(CInput)
       ),
       !
      ; 
       language(prolog),  
       process_prolog(CInput), 
       !
      ; 
       language(sql),     
       process_sql(CInput), 
       !
      ; 
       language(ra),
       process_ra(CInput),
       !
      ;
       language(drc),
       process_drc(CInput),
       !
      ;
       language(trc),
       process_trc(CInput),
       !
      ;
       process_error(CInput),
       !
      ;
       invalid_input_message
      ),
      (NL==nl -> nl_tapi_log ; true)
     )
   ),
   restore_state_flags,
   retract_hyp_programs,
%   retract_hyp_programs_k,
   !.

process_error(StrInput) :- % Fail if no syntax error. Succeed if unbalanced brackets. Raise an exception with the syntax error message otherwise
  \+ balanced_brackets(StrInput),
  reset_syntax_error,
  !.
process_error(StrInput) :-
  syntax_error_detected(StrInput,ErrorMessage),
  reset_syntax_error,
  my_raise_exception(generic,syntax(ErrorMessage),[]).
  
syntax_error_detected(StrInput,ErrorMessage) :-
  bagof(last_syntax_error(M,R,L,D,C),last_syntax_error(M,R,L,D,C),Errors),
  compose_syntax_error_message(StrInput,Errors,ErrorMessage).
  
compose_syntax_error_message(_StrInput,[Error],ErrorMessage) :-
  Error=last_syntax_error(_Message,R,_Language,Direction,sentence),
  !,
  compose_syntax_error_message_list([Error],R,Direction,ErrorMessageList),
  append(ErrorMessageList,['.'],ErrorMessage).
compose_syntax_error_message(StrInput,Errors,ErrorMessage) :-
  compose_syntax_error_message_list(Errors,R,Direction,ErrorMessageList),
  length(StrTail,R),
  append(StrHead,StrTail,StrInput),
  atom_codes(Head,StrHead),
  (StrHead==[]
   ->
    StrTailErrorMessage=['.']
   ;
    StrTailErrorMessage=[' ',Direction,' ''',Head,''''] % Do not end with a dot
  ),
  concat_lists([ErrorMessageList,StrTailErrorMessage],ErrorMessage).

compose_syntax_error_message_list([last_syntax_error(Message,R,Language,Direction,_Context)],R,Direction,ErrorMessage) :-
  nonvar(Message),
  language_acronym(Language,Acronym),
  concat_lists([['(',Acronym,') '],Message],ErrorMessage).
compose_syntax_error_message_list([Error1,Error2|Errors],R,near,ErrorMessage) :-
  compose_syntax_error_message_list([Error1],R,_Direction1,ErrorMessage1),
  compose_syntax_error_message_list([Error2|Errors],R,_Direction2,ErrorMessage2),
  concat_lists([ErrorMessage1,[' or '],ErrorMessage2],ErrorMessage).

% last_syntax_error includes the number of characters left to be parsed (0 means the end of the input)
reset_syntax_error :-
  set_flag(last_syntax_error(_NoError,1000000,datalog,_Direction,_Context)).
  
set_syntax_error(M,R,C) :-
  set_syntax_error(M,R,C,_).

set_syntax_error(M,R,C,OldErrors) :-
  language(L),
  set_syntax_error(M,R,L,C,after,OldErrors).

set_syntax_error(M,R,L,C,D,OldErrors) :-
  findall(last_syntax_error(X,Y,Z,U,V),last_syntax_error(X,Y,Z,U,V),OldErrors),
  OldErrors=[last_syntax_error(_,OR,_,_,_)|_],
  (R<OR
   ->
    set_flag(last_syntax_error(M,R,L,D,C))
   ;
    (R==OR
     ->
      (memberchk(last_syntax_error(M,R,L,_D,C),OldErrors)
       ->
        true
       ;
        assertz(last_syntax_error(M,R,L,D,C))
      )
     ;
      true
    )
  ).
    
push_syntax_error(M) -->
  push_syntax_error(M,statement,_OldErrors).
  
push_syntax_error(M,OldErrors) -->
  push_syntax_error(M,column,OldErrors).
  
push_syntax_error(M,C,OldErrors) -->
  remaining_chars(R),
  {set_syntax_error(M,R,C,OldErrors)}.
   
push_syntax_error_before(M,R,OldErrors) -->
  {language(L),
   set_syntax_error(M,R,L,column,before,OldErrors)}.
  
pop_syntax_error(OldErrors) -->
  {set_syntax_error_list(OldErrors)}.
  
set_syntax_error_list([]).
set_syntax_error_list([last_syntax_error(M,R,L,D,C)|Ls]) :-
  set_syntax_error(M,R,L,C,D,_),
  set_syntax_error_list(Ls).
  
remaining_chars(L,I,I) :-
  length(I,L).

% set_semantic_error(M,L) :-
%   (last_semantic_error(_M,LL),
%    LL>L
%    ->
%     set_flag(last_semantic_error(M,L))
%    ;
%     true).
%    
% semantic_error :-
%   last_semantic_error(M,_L),
%   nonvar(M),
%   my_raise_exception(generic,syntax(M),[]).

language_acronym(prolog,'Prolog').
language_acronym(datalog,'DL').
language_acronym(sql,'SQL').
language_acronym(ra,'RA').
language_acronym(drc,'DRC').
language_acronym(trc,'TRC').

% remove_ending_blanks   
remove_ending_blanks(L1,L2):-
  remove_ending_blanks(L2,_,L1).

remove_ending_blanks([],X,X) :-
  my_blanks_star(X,[]),
  !.
remove_ending_blanks([X|Xs],Y,[X|Zs]) :-
  remove_ending_blanks(Xs,Y,Zs).
   
remove_initial_blanks([B|Xs],Ys) :-
  [B]=" ",
  !,
  remove_initial_blanks(Xs,Ys).
remove_initial_blanks(Xs,Xs).
  
% When Datalog prompt is enabled, maybe the user types an SQL statement
% Then, try to process it
try_to_process_sql(CInput) :-
  my_guessed_sql_statement(CInput,_),
  !,
  processC(sql,[],_,_),
  (process_sql(CInput)
   ->
    processC(datalog,[],_,_)
   ;
    processC(datalog,[],_,_),
    fail
  ).
  
% When Datalog prompt is enabled, maybe the user types an RA expression
% Then, try to process it
try_to_process_ra(CInput) :-
  my_guessed_ra_statement(CInput,_),
  !,
  processC(ra,[],_,_),
  (process_ra(CInput)
   ->
    processC(datalog,[],_,_)
   ;
    processC(datalog,[],_,_),
    fail
  ).
  
% The same for DRC ...
try_to_process_drc(CInput) :-
  my_guessed_drc_statement(CInput,_),
  !,
  processC(drc,[],_,_),
  (process_drc(CInput)
   ->
    processC(datalog,[],_,_)
   ;
    processC(datalog,[],_,_),
    fail
  ).
  
% ... and TRC
try_to_process_trc(CInput) :-
  my_guessed_trc_statement(CInput,_),
  !,
  processC(trc,[],_,_),
  (process_trc(CInput)
   ->
    processC(datalog,[],_,_)
   ;
    processC(datalog,[],_,_),
    fail
  ).
  
process_datalog(CInput) :-
  reset_pred_id,
  get_flag(null_id,NId),
  (my_blanks_star(CInput,[])
   ->  % Switch to Datalog command prompt, empty argument
    true
    ;
    write_info_verb_log(['Parsing query...'])),
  (process_datalog_constraint(CInput),
   !
  ;
   set_flag(null_id,NId),
   process_datalog_assertion(CInput), 
   !
  ; 
   set_flag(null_id,NId),
   process_datalog_query(CInput),
   !
  ; 
   set_flag(null_id,NId),
   process_view(CInput), 
   set_flag(null_id,NId),
   !
  ; 
   set_flag(null_id,NId),
   process_autoview(CInput),
   set_flag(null_id,NId),
   !
  ;
   set_flag(null_id,NId),
   !,
   fail
  ).
  
successful_parsing(Object) :-
  language(L),
  language_acronym(L,UL),
  write_info_verb_log([UL,' ',Object,' successfully parsed.']),
%   (true
%    ;
%    my_raise_exception(generic,syntax(['Invalid ',UL,' input.']),[])),
  reset_syntax_error.

% Processing a Prolog goal  
process_prolog(CInput) :-
  write_info_verb_log(['Parsing goal...']),
  parse_body(Goal,[],NVs,CInput,[]), 
  !,
  successful_parsing('goal'),
  store_elapsed_time(parsing),
  solve_prolog(Goal,NVs).

% Processing an SQL query
process_sql(QueryStr) :-
  current_db(Connection),
  Connection\=='$des',
  des_sql_solving(off),
  !,
  (parse_sql_query(Query,QueryStr,"")
   ->
    true
   ;
    write_info_verb_log(['Giving up parsing to ODBC controller.']),
    Query=unknown),
  store_elapsed_time(parsing),
  solve_sql_query(QueryStr,Query).
process_sql(QueryStr) :-
  write_info_verb_log(['Parsing query...']),
  parse_sql_query(Query,QueryStr,""), 
  !,
  successful_parsing('query'),
  store_elapsed_time(parsing),
  reset_pred_id,
  solve_sql_query(QueryStr,Query).

% Processing a RA query
process_ra(QueryStr) :-
  write_info_verb_log(['Parsing query...']),
  parse_ra_query(Query,QueryStr,[]), 
  !,
  successful_parsing('query'),
  store_elapsed_time(parsing),
  reset_pred_id,
  solve_ra_query(Query).
  
% Processing a DRC query
process_drc(QueryStr) :-
  write_info_verb_log(['Parsing query...']),
  parse_drc_query(Query,QueryStr,[]), 
  !,
  successful_parsing('query'),
  store_elapsed_time(parsing),
  reset_pred_id,
  solve_drc_query(Query).
  
% Processing a TRC query
process_trc(QueryStr) :-
  write_info_verb_log(['Parsing query...']),
  parse_trc_query(Query,QueryStr,[]), 
  !,
  successful_parsing('query'),
  store_elapsed_time(parsing),
  reset_pred_id,
  solve_trc_query(Query).
  
% Processing a Datalog query
process_datalog_query(SBody) :- 
  parse_datalog_query(Body,NVs,SBody,[]),
  !,
  successful_parsing('query'),
  store_elapsed_time(parsing),
  compute_datalog_query(Body,NVs,[],user).  % In response to a user input, output enabled
  
% Metapredicates at the system prompt
compute_datalog_query(Body,NVs,CId,Origin) :-
  reset_statistics,
  functor(Body,AF,Arity),
  (my_non_system_aggregate_relation(AF,Arity)
   ;
   (AF,Arity)==(group_by,3)
   ;
   (AF,Arity)==(top,2)
   ;
   (AF,Arity)==(distinct,1)
   ;
   (AF,Arity)==(distinct,2)
   ;
   (AF,Arity)==(exists,2)
   ;
   (AF,Arity)==(order_by,3)
   ;
   (AF,Arity)==('=>',2)
   ;
   (AF,Arity)==(or,2)
%    ;
%    (AF,Arity)==('-',1)
  ),
  !,
  relevant_NVs(Body,NVs,URNVs), % Non-relevant vars are set vars in aggregate predicates, as X in avg(p(X),X,Y)
  filter(NVs,URNVs,RNVs),
  build_head(answer,RNVs,Head),
  Rule = ':-'(Head,(Body,0=0)),
  retract(simplification(S)),
  assertz(simplification(on)),
  retractall(last_autoview(_)),
  assertz(last_autoview(Rule)),
  process_rule((Rule,NVs),CId,Origin,autoview),
  retract(simplification(on)), 
  assertz(simplification(S)).
% Primitives at the system prompt: is/2 and comparison operators (=, <, >, ...)
%   Do not follow et mechanism
compute_datalog_query(Query,NVs,CId,Origin) :-
  is_primitive(Query),
  !,
  build_head(answer,NVs,Head),
  preprocess((':-'(Head,Query),NVs),_SiRNVsList,_SfRNVsList,_ExRNVsList,[],_IArgsList,_Modes,exec,datalog,query,_Causes,[],Unsafe),
  (nonvar(Unsafe)
   ->
    true
   ;
    (compute_primitive(Query,Query)
     ->
      my_idx_assertz(et(Query,[(-1,[])],CId,1)),
      ground_nulls, % Needed for, e.g.,  X is null in development mode 
      Success=true
     ;
      Success=false),
    (Origin==user
     ->
      store_elapsed_time(computation),
      display_solutions(Query,[],on),
      display_statistics,
      display_elapsed_time,
      write_tapi_eot,
      (Success==true
       -> 
        my_idx_retract(et(Query,[(-1,[])],CId,1)) 
       ; 
        true
      )
     ;
      true  % If Origin is system, the answer does persist
    )
  ).
% An invalid use of an aggregate
compute_datalog_query(Body,_NVs,_CId,_Origin) :-
  functor(Body,AF,Arity),
  my_aggregate_relation(AF,Arity),
  !,
  write_error_log(['Non valid use of system aggregate.']).
% A non-aggregate, non-primitive, basic query
compute_datalog_query(Body,NVs,CId,Origin) :-
  relevant_NVs(Body,NVs,URNVs), % Non-relevant vars are set vars in aggregates, as X in avg(p(X),X,Y)
  filter(NVs,URNVs,RNVs),
  build_head(answer,RNVs,Head),
  Rule = ':-'(Head,Body),
  retractall(last_autoview(_)),
  assertz(last_autoview(Rule)),
  process_rule((Rule,NVs),CId,Origin,query).

% Compute the meaning of a given query. No output to the user
compute_datalog(Rule) :-
  compute_datalog(Rule,[]).

compute_datalog(Rule,CId) :-
  compute_datalog_rule_body(Rule,CId),
  set_complete_flags.

compute_datalog_rule_body(Rule,CId) :-
  Rule=':-'(_H,_B),
  !,
  assign_NVs(Rule,NVs),
  compute_datalog_view((Rule,NVs),CId,system,view).
compute_datalog_rule_body(Body,CId) :-
  assign_NVs(Body,NVs),
  compute_datalog_query(Body,NVs,CId,system).

% Test whether two predicates P1 and P2 of arity A have the same meaning
same_meaning(P1,P2,A) :-
  et_entries_by_name_arity(P1,A,P1Facts),
  et_entries_by_name_arity(P2,A,P2Facts),
  length(P1Facts,L),
  length(P2Facts,L),
  (L==0
   ->
    true
   ;
    P1Facts=[Fact|_Facts],
    Fact=..[F|_Args],
    replace_functor_term_list(P2Facts,F,RP2Facts),
    my_sort(P1Facts,OFacts),
    my_sort(RP2Facts,OFacts)
  ).

fact_to_tuple_list([],[]).
fact_to_tuple_list([F|Fs],[T|Ts]) :-
  F=..[_|Xs],
  my_list_to_tuple(Xs,T),
  fact_to_tuple_list(Fs,Ts).
  
% Processing a Datalog constraint
process_datalog_constraint(SConstraint) :-  
  parse_datalog_constraint(Constraint,NVs,SConstraint,[]),
  !,
  successful_parsing('constraint'),
  store_elapsed_time(parsing),
  process_datalog_constraints(Constraint,NVs,[],0,exec,_Error).

% Processing a Datalog assertion
process_datalog_assertion(SConstraint) :-  
  parse_datalog_assertion(Constraint,NVs,SConstraint,[]),
  !,
  successful_parsing('assertion'),
  store_elapsed_time(parsing),
  process_datalog_assertion(Constraint,NVs,0,exec,_Error).

% Processing a Datalog view
process_view(SRule) :-
  parse_rule((Head:-Body),[],NVs,SRule,[]),
  check_redef(Head),
  !,
  successful_parsing('view'),
  store_elapsed_time(parsing),
  compute_datalog_view(((Head:-Body),NVs),[],user,view).
  
compute_datalog_view((Rule,NVs),CId,Origin,QueryType):-
  retractall(last_autoview(_)),
  assertz(last_autoview(Rule)),
  process_rule((Rule,NVs),CId,Origin,QueryType).

% Processing a Datalog autoview
process_autoview(SBody) :-
  parse_body(Body,[],NVs,SBody,[]),
%   relevant_NVs(Body,NVs,URNVs), 
%   term_variables(URNVs,UVs)
%   remove_anonymous_NVs(URNVs,ARVs),
%   filter(NVs,URNVs,RNVs),
%   build_head(answer,RNVs,Head),
  build_head_from_body_headname(Body,answer,NVs,Head),
  !,
  successful_parsing('query'),
  store_elapsed_time(parsing),
  Rule = ':-'(Head,Body),
  compute_datalog_view((Rule,NVs),[],user,autoview).
  
% Processing a Datalog rule (either returning error condition or not)
process_rule(RuleNVs,CId,Origin,QueryType) :-
  process_rule(RuleNVs,CId,Origin,QueryType,_Unsafe).
  
process_rule(RuleNVs,CId,Origin,QueryType,Unsafe) :-
  preprocess(RuleNVs,SiRuleNVsList,SfRuleNVsList,ExRuleNVsList,[],_IArgsList,_Modes,exec,datalog,QueryType,Causes,[],Unsafe),
  singleton_warning_list(RuleNVs,ExRuleNVsList,QueryType,exec,Origin),
  (nonvar(Unsafe)
   ->
    true
   ;
    process_compiled_rule(RuleNVs,SiRuleNVsList,SfRuleNVsList,ExRuleNVsList,CId,Origin,QueryType,Causes)
  ).

process_compiled_rule((ORule,NVs),_SiRuleNVsList,SfRuleNVsList,ExRuleNVsList,CId,Origin,QueryType,Causes) :-
  ORule =.. [':-',OHead,OBody],
  ExRuleNVsList = [_TRuleNVs|RTRuleNVsList],
  SfRuleNVsList = [(SRule,SNVs)|_RSRuleNVsList],
  (SRule =.. [':-',Head,SBody] ; SRule=Head, SBody=true),
  !,
  (basic_query(QueryType,RTRuleNVsList)
   ->      
    (member(transformed,Causes)
     ->                                    % Basic query with removed anonymous variables from the head
      my_datetime(X),
      build_datalog_rules((':-'(Head,OBody),NVs),[],ExRuleNVsList,[],asserted(X),DLs),
      my_assertz_DL_list(DLs,CId,Error)
     ;
      true                                 % Basic query: No rules have been added
    )
   ;
   (strata(CurrentStrata),                 % Increased program, compute stratification
    pdg(CurrentPDG),
    current_tags(CurrentTags),
    my_datetime(X),
    build_datalog_rules((ORule,NVs),[],ExRuleNVsList,[],asserted(X),DLs),
    my_assertz_DL_list(DLs,CId,Error),
    functor(OHead,N,A),
    pdg((Ps,_)),
    ((N==answer ; \+ member(N/A,Ps))
     ->
      true % Already completed results can be reused
     ;
      my_idx_retractall(complete_flag(_P,_G,_CF,_CId)))
   )
  ),
  (basic_query(QueryType,RTRuleNVsList),
   \+ member(transformed,Causes)
    ->       % Basic query: No rules have been added/transformed
    (SBody\==true
     -> 
      Query = SBody 
     ;
      Query = OBody
    ),
    display_undefined_predicates(Query)
   ;
    (build_query_from_head(Head,Query),
     ((Origin==system
       ;
       SBody==true)
      ->                             % Only to compute the meaning, no output to the user
       true
      ;
       write_info_log(['Processing:']),    % A transformed query and/or conjunctive query
       (member(exploded,Causes)
        ->
         write_query_to_process(Query,SNVs,exploded),
         (development(on) -> DRuleNVsList=ExRuleNVsList ; DRuleNVsList=SfRuleNVsList)
        ;
         (member(compiled,Causes)
          ->
           (development(on) -> DRuleNVsList=ExRuleNVsList ; append(NVs,SNVs,SSNVs),DRuleNVsList=[(':-'(Head,OBody),SSNVs)]),
           (development(on) -> write_query_to_process(Query,SNVs,compiled) ; true)
          ;
           (development(on) -> DRuleNVsList=ExRuleNVsList ; DRuleNVsList=SfRuleNVsList)
         )
       ),
       display_ruleNVs_list(DRuleNVsList,2)
     ),
     compute_stratification
    )
  ),
%  display_undefined_predicates(OBody,QueryType),
  % Simplification may end up with a primitive:
  (is_primitive(Query)
   ->
    compute_datalog_query(Query,SNVs,CId,Origin)
   ;    
    (
     (Origin==system
      ->                               % Only to compute the meaning, no output
       solve_datalog_query(Query,SNVs,CId,_Undefined)
      ;
       order_by_query(Query,OrderBy),
       solve_datalog_query(Query,SNVs,CId,Undefined),
       store_elapsed_time(computation),
       write_info_verb_log(['Displaying query answer...']),
       display_solutions(Query,Undefined,OrderBy),
       display_statistics,
       display_elapsed_time,
       write_tapi_eot
     ),
     (basic_query(QueryType,RTRuleNVsList)
      ->
       (member(transformed,Causes) ->                  % Basic query with removed anonymous variables from the head
        my_retract_DL_list(DLs,Error),
        reset_et(Origin,CurrentStrata,CurrentPDG,CurrentTags)
        ;
        true                                           % Basic query: No rules have been added
       )
      ;
       my_retract_DL_list(DLs,Error),                 % Transformed query: Added rules have to be removed
       reset_et(Origin,CurrentStrata,CurrentPDG,CurrentTags)
     )
    )
  ).

write_query_to_process(Query,SNVs,Cause) :-
  write_log('  '),
  write_with_NVs(Query,SNVs),
  write_log_list([nl,'in the program context of the ',Cause,' query:',nl]).
  
reset_et(QueryOrigin,CurrentStrata,CurrentPDG,CurrentTags) :-
	(QueryOrigin==user
	 ->
	  clear_et,                                     % Decrease program, compute stratification
	  load_stratification(CurrentStrata,CurrentPDG,CurrentTags)  % TODO: Restore previous state
	 ;
	  true                                         % ET is kept for system 
	).

basic_query(QueryType,TRules) :-
  QueryType==query, 
  TRules==[].
  
  
build_head(Functor,NVs,Head) :-
  'NVs2Vs'(NVs,Vs),
  Head =.. [Functor|Vs].
  
build_query_from_head(H,Q) :-
  H=..[F|Args],
  query_args_from_head_args(Args,QArgs),
  Q=..[F|QArgs].
  
query_args_from_head_args([],[]).
query_args_from_head_args([Arg|Args],[Arg|QArgs]) :-
  var(Arg),
  !,
  query_args_from_head_args(Args,QArgs).
query_args_from_head_args([_|Args],[_|QArgs]) :-
  query_args_from_head_args(Args,QArgs).


% build_open_fact_list([],[]).
% build_open_fact_list([N/A|Ps],[F|Fs]) :-
%   length(L,A),
%   F=..[N|L],
%   build_open_fact_list(Ps,Fs).

% Non-relevant vars are set vars in aggregates, as X and Y in avg(p(X,Y),X,Z)
% Consider also: X in count((e(X,Y),avg(f(U,Y),U,A)),X,C)
relevant_NVs(Body,NVs,RelNVs) :-
  find_non_relevant_vars(Body,NVs,[],RelNVs,[],_NRNVs).

% Relevant vars
relevant_vars(Body,RelVs) :-
  find_non_relevant_vars(Body,[],[],RelNVs,[],_NRNVs),
  term_variables(RelNVs,RelVs).

  
find_non_relevant_vars(T,NVs,IRNVs,ORNVs,NRNVs,NRNVs) :-
  var(T),    % Variable
  !,
  my_var_name_list([T],NVs,RNVs),
  my_union_var(IRNVs,RNVs,ORNVs).
find_non_relevant_vars(T,_NVs,RNVs,RNVs,NRNVs,NRNVs) :-
  atomic(T), % Constant
  !. 
find_non_relevant_vars(T,NVs,IRNVs,ORNVs,INRNVs,ONRNVs) :- 
  T=..[Count,_Rel,P,R], % count(Relation,GroupBy,Result)
  atom_concat(count,_Distinct,Count),
  var(P),
  !,                                % Example:     count(e(X,Y,Z),Y,A) : COUNT(y)   - Pivot, no group by
  term_variables(T,AllVs),          % All          [X,Y,Z,A]
  term_variables([R],RelVs),        % Relevant     [A]
  my_subtract_var(AllVs,RelVs,Vs),  % Non-relevant [X,Y,Z] = All - Relevant
  my_var_name_list(Vs,NVs,NRNVs),
  my_union_var(INRNVs,NRNVs,ONRNVs),
  my_var_name_list(RelVs,NVs,RNVs),
  my_union_var(IRNVs,RNVs,ORNVs).
find_non_relevant_vars(T,NVs,IRNVs,ORNVs,INRNVs,ONRNVs) :- 
  T=..[Count,_Rel,GB,R], % count(Relation,GroupBy,Result)
  atom_concat(count,_Distinct,Count),
  my_is_list(GB),
  !,                                % Example:     count(e(X,Y,Z),[Z],A) : COUNT(*) - No pivot, group by
  term_variables(T,AllVs),          % All          [X,Y,Z,A]
  term_variables([GB,R],RelVs),     % Relevant     [A,Z]
  my_subtract_var(AllVs,RelVs,Vs),  % Non-relevant [X,Y] = All - Relevant
  my_var_name_list(Vs,NVs,NRNVs),
  my_union_var(INRNVs,NRNVs,ONRNVs),
  my_var_name_list(RelVs,NVs,RNVs),
  my_union_var(IRNVs,RNVs,ORNVs).
find_non_relevant_vars(T,NVs,IRNVs,ORNVs,INRNVs,ONRNVs) :- 
  T=..[Count,_Rel,R], % count(Relation,Result)
  atom_concat(count,_Distinct,Count),
  !,                                % Example:     count(e(X,Y,Z),A) : COUNT(*)     - No pivot, No group by
  term_variables(T,AllVs),          % All          [X,Y,Z,A]
  term_variables([R],RelVs),        % Relevant     [A]
  my_subtract_var(AllVs,RelVs,Vs),  % Non-relevant [X,Y,Z] = All - Relevant
  my_var_name_list(Vs,NVs,NRNVs),
  my_union_var(INRNVs,NRNVs,ONRNVs),
  my_var_name_list(RelVs,NVs,RNVs),
  my_union_var(IRNVs,RNVs,ORNVs).
find_non_relevant_vars(T,NVs,IRNVs,ORNVs,INRNVs,ONRNVs) :- 
  T=..[AF,_Rel,_P,GB,R], % avg(Relation,Pivot,GroupBy,Result)
  my_aggregate_relation(AF,4),
  !,                                % Example:     avg(e(X,Y,Z),X,[Z],A)            - Pivot, group by (applies also to count)
  term_variables(T,AllVs),          % All          [X,Y,Z,A]
  term_variables([GB,R],RelVs),     % Relevant     [A,Z]
  my_subtract_var(AllVs,RelVs,Vs),  % Non-relevant [X,Y] = All - Relevant
  my_var_name_list(Vs,NVs,NRNVs),
  my_union_var(INRNVs,NRNVs,ONRNVs),
  my_var_name_list(RelVs,NVs,RNVs),
  my_union_var(IRNVs,RNVs,ORNVs).
find_non_relevant_vars(T,NVs,IRNVs,ORNVs,INRNVs,ONRNVs) :- 
  T=..[AF,_Rel,_P,R], % avg(Relation,Pivot,Result)
  my_aggregate_relation(AF,3),
  !,                                % Example:     avg(e(X,Y,Z),X,A)                - Pivot, no group by
  term_variables(T,AllVs),          % All          [X,Y,Z,A]
  term_variables([R],RelVs),        % Relevant     [A]
  my_subtract_var(AllVs,RelVs,Vs),  % Non-relevant [X,Y,Z] = All - Relevant
  my_var_name_list(Vs,NVs,NRNVs),
  my_union_var(INRNVs,NRNVs,ONRNVs),
  my_var_name_list(RelVs,NVs,RNVs),
  my_union_var(IRNVs,RNVs,ORNVs).
find_non_relevant_vars(T,NVs,IRNVs,ORNVs,INRNVs,ONRNVs) :- 
  (T=..[group_by,R,GBVs,_]
   ;
   T=..[group_by,R,_Ps,GBVs,_]),
  !,                                 % Example:     group_by(employee(N,D,S), [D], R=count(S))
  term_variables(T,AllVs),           % All          [N,D,S,R]
  term_variables(R,RVs),             % Relevant     [D,R] = All-Non-relevant(Rel)+GB=[N,D,S,R]-[N,D,S]+[D]
  my_subtract_var(AllVs,RVs,SVs),    % Non-relevant [N,S] = All-Relevant
  my_union_var(SVs,GBVs,RelVs),
  my_subtract_var(AllVs,RelVs,NRelVs),
  my_var_name_list(NRelVs,NVs,NRNVs),
  my_union_var(INRNVs,NRNVs,ONRNVs),
  my_var_name_list(RelVs,NVs,RNVs),
  my_union_var(IRNVs,RNVs,ORNVs).
find_non_relevant_vars(T,NVs,IRNVs,ORNVs,INRNVs,ONRNVs) :- 
  T=..[distinct,GBVs,R],
  !,                                 % Example:     distinct([D],employee(N,D,S))
  term_variables(T,AllVs),           % All          [N,D,S]
  term_variables(R,RVs),             % Relevant     [D] = All-Non-relevant(Rel)+GB=[N,D,S,R]-[N,D,S]+[D]
  my_subtract_var(AllVs,RVs,SVs),    % Non-relevant [N,S] = All-Relevant
  my_union_var(SVs,GBVs,RelVs),
  my_subtract_var(AllVs,RelVs,NRelVs),
  my_var_name_list(NRelVs,NVs,NRNVs),
  my_union_var(INRNVs,NRNVs,ONRNVs),
  my_var_name_list(RelVs,NVs,RNVs),
  my_union_var(IRNVs,RNVs,ORNVs).
find_non_relevant_vars(T,NVs,IRNVs,ORNVs,INRNVs,ONRNVs) :- 
  T=..[exists,NRelVs,R],
  !,                                 % Example:     exists([D],employee(N,D,S))
  find_non_relevant_vars(R,NVs,IRNVs,I1RNVs,INRNVs,I1NRNVs),
  my_var_name_list(NRelVs,NVs,NRNVs),
  my_subtract_var(I1RNVs,NRNVs,ORNVs),
  my_union_var(I1NRNVs,NRNVs,ONRNVs).
find_non_relevant_vars(T,NVs,IRNVs,ORNVs,INRNVs,ONRNVs) :- 
  T=..['=>',_A,C],
  !,                                 % Example:     p(X,Y)/\a:-q(Z) => b(X,U)
                                     % All          [X,Y,Z,U]
                                     % Relevant     [X,U] = relevant (b(X,U))
                                     % Non-relevant [] = non-relevant (b(X,U))
  find_non_relevant_vars(C,NVs,IRNVs,ORNVs,INRNVs,ONRNVs).
find_non_relevant_vars(T,NVs,IRNVs,ORNVs,INRNVs,ONRNVs) :- 
  T =.. [_F|As],
  !, 
  find_non_relevant_vars_list(As,NVs,IRNVs,ORNVs,INRNVs,ONRNVs).

find_non_relevant_vars_list([],_NVs,RNVs,RNVs,NRNVs,NRNVs) :-
  !.
find_non_relevant_vars_list([T|Ts],NVs,IRNVs,ORNVs,INRNVs,ONRNVs) :-
  !, 
  find_non_relevant_vars(T,NVs,IRNVs,TRNVs,INRNVs,TNRNVs), 
  find_non_relevant_vars_list(Ts,NVs,TRNVs,ORNVs,TNRNVs,ONRNVs).

% find_non_relevant_vars(e(X,Y),['X'=X,'Y'=Y],[],RVs,[],Vs).      
% Vs = [],
% RVs = ['X'=X,'Y'=Y] ? 

% find_non_relevant_vars((e(X,Y),X>Y),['X'=X,'Y'=Y],[],RVs,[],Vs).
% Vs = [],
% RVs = ['X'=X,'Y'=Y,'X'=X,'Y'=Y] ? 

% find_non_relevant_vars(avg(e(X,Y),X,A),['X'=X,'Y'=Y,'A'=A],[],RVs,[],Vs).
% Vs = ['X'=X,'Y'=Y],
% RVs = ['A'=A] ? 

% find_non_relevant_vars(avg((e(X,Y),X>Y),X,A),['X'=X,'Y'=Y,'A'=A],[],RVs,[],Vs).
% Vs = ['X'=X,'Y'=Y],
% RVs = ['A'=A] ? 

% find_non_relevant_vars(count((f(U,X),avg((e(X,Y),X>Y),X,A)),U,C),['X'=X,'Y'=Y,'A'=A,'U'=U,'C'=C],[],RVs,[],Vs).
% Vs = ['U'=U,'X'=X,'Y'=Y,'A'=A],
% RVs = ['C'=C] ? 

% find_non_relevant_vars(count((f(U,X),avg((e(X,U),X>U),U,A)),U,C),['X'=X,'A'=A,'U'=U,'C'=C],[],RVs,[],Vs).
% Vs = ['U'=U,'X'=X,'A'=A],
% RVs = ['C'=C] ? 

process_single_line_remark(SRemark) :-
  parse_single_line_remark(SRemark,[]), 
  !.

process_multi_line_remark(SRemark) :-
  parse_multi_line_remark(SRemark,[]), 
  !.

blank_input(SBlanks) :-
  my_blanks_star(SBlanks,[]).  

  
balanced_brackets(StrInput) :-
  findall(last_syntax_error(A,B,C,D,E),last_syntax_error(A,B,C,D,E),Ls),
  reset_syntax_error,
  balanced_brackets(StrInput,[]),
  retractall(last_syntax_error(_,_,_,_,_)),
  assertz_list(Ls),
  !.
balanced_brackets(StrInput) :-
  syntax_error_detected(StrInput,ErrorMessage),
  write_error_log(ErrorMessage),
%  nl_compact_log,
  !,
  fail. 

% balanced_brackets -->
%   my_chars_but_brackets.
% balanced_brackets -->
%   my_chars_but_brackets,
%   push_syntax_error(['Unbalanced bracket'],Old1),
%   my_left_bracket(StrLB),
%   {brackets(StrLB,StrRB)},
%   pop_syntax_error(Old1),
%   balanced_brackets,
%   push_syntax_error(['Unbalanced bracket'],Old2),
%   my_right_bracket(StrRB),
%   pop_syntax_error(Old2),
%   balanced_brackets. 
balanced_brackets -->
  my_chars_but_brackets.
balanced_brackets -->
  my_chars_but_brackets,
  my_left_bracket(StrLB),
  {right_bracket(StrLB,RB)},
  push_syntax_error(['Right bracket ''',RB,''' not found'],Old1),
  {brackets(StrLB,StrRB)},
  balanced_brackets,
  my_right_bracket(StrRB),
  pop_syntax_error(Old1),
  {left_bracket(StrRB,LB)},
  push_syntax_error(['Left bracket ''',LB,''' not found'],Old2),
  balanced_brackets,
  pop_syntax_error(Old2).
balanced_brackets -->
  my_chars_but_brackets,
  remaining_chars(R),
  my_right_bracket(StrRB),
  {left_bracket(StrRB,LB)},
  push_syntax_error_before(['Left bracket ''',LB,''' not found'],R,_Old),
  !,
  {fail}.
  
left_bracket(StrRB,LB) :-
  brackets(StrLB,StrRB),
  atom_codes(LB,StrLB).

right_bracket(StrLB,RB) :-
  brackets(StrLB,StrRB),
  atom_codes(RB,StrRB).

my_chars_but_brackets -->
  my_char_but_parentheses,
  my_chars_but_brackets,
  !.
my_chars_but_brackets -->
  [].
  
my_char_but_parentheses -->
  [C],
  { [C] \== "(" ,
    [C] \== ")" ,
    [C] \== "{" ,
    [C] \== "}" ,
    [C] \== "[" ,
    [C] \== "]" }.
    
brackets("(",")").
brackets("{","}").
brackets("[","]").

my_left_bracket(L) --> 
  {brackets(L,_R)},
  L.

my_right_bracket(R) -->
  {brackets(_L,R)},
  R.

my_chars_but_blank([C|Cs]) -->
  [C],
  { [C] \== " " },
  my_chars_but_blank(Cs).
my_chars_but_blank([]) -->
  [],
  {!}.
  
my_chars_but_dollar([C|Cs]) -->
  [C],
  { [C] \== "$" },
  my_chars_but_dollar(Cs).
my_chars_but_dollar([]) -->
  [],
  {!}.
  
invalid_input_message :-
  write_error_log(['Unrecognized start of input.']).

  
% instance_system_vars([]) -->
%   [].
% instance_system_vars(OutStr) -->
%   system_var(V),
%   !,
%   {instance_system_var(V,I),
%    my_term_to_string(I,StrI)},
%   instance_system_vars(TailStr),
%   {append(StrI,TailStr,OutStr)}.
% instance_system_vars([C|OutStr]) -->
%   [C],
%   instance_system_vars(OutStr).
%  
%     
% %instance_system_var('$$',0).
% instance_system_var('$stopwatch$',CT) :-
%   !,
%   current_stopwatch_time(CT).
% instance_system_var('$last_stopwatch$',CT) :-
%   !,
%   last_stopwatch_stop(CT).
% instance_system_var('$total_elapsed_time$',Total) :-
%   !,
%   last_elapsed_time(_Parsing,_Computation,_Display,Total).
% instance_system_var('$computation_time$',Computation) :-
%   !,
%   last_elapsed_time(_Parsing,Computation,_Display,_Total).
% instance_system_var('$parsing_time$',Parsing) :-
%   !,
%   last_elapsed_time(Parsing,_Computation,_Display,_Total).
% instance_system_var('$display_time$',Display) :-
%   !,
%   last_elapsed_time(_Parsing,_Computation,Display,_Total).
% instance_system_var(DVarD,Value) :-
%   atom_concat('$',VarD,DVarD),
%   atom_concat(Var,'$',VarD),
%   Flag=..[Var,Value],
%   (call(Flag) -> true ; Value='**ERROR**').
%   
% % system_var('$$') -->
% %   "$$".
% system_var('$stopwatch$') -->
%   "$stopwatch$",
%   !.  
% system_var('$total_elapsed_time$') -->
%   "$total_elapsed_time$",
%   !.  
% system_var(C) -->
%   "$",
%   my_chars_but_dollar(Cs),
%   "$",
%   {concat_lists(["$",Cs,"$"],CDs),
%    atom_codes(C,CDs)}.  
  
%%%%%%%%%%%%%%%%%%
% Host Statistics
%%%%%%%%%%%%%%%%%%

display_host_statistics(Kw) :-
  (Kw=runtime ; Kw=total_runtime),
  statistics(Kw,[CT|_]),
  (host_statistics(LT)
   ->
    DT is CT-LT
   ;
    DT is CT),
  set_flag(host_statistics(CT)),
  display_stopwatch(DT).
display_host_statistics(Kw) :-
  write_error_log(['Unsupported keyword: ',Kw,'. Alternatives are ''runtime'' and ''total_runtime''.']).
  
%%%%%%%%%%%%%%%%%%
% Stopwatch
%%%%%%%%%%%%%%%%%%
  
% stopwatch(LastStartTime,CummulatedTime,State)

start_stopwatch :-
  stopwatch(_LST,_CT,start),
  !.
start_stopwatch :-
  my_get_time(CurrentTime),
  retract(stopwatch(_LST,CT,_State)),
  assertz(stopwatch(CurrentTime,CT,start)).

stop_stopwatch :-
  stopwatch(_LST,_CT,stop),
  !.
stop_stopwatch :-
  retract(stopwatch(LST,CT,_State)),
  my_get_time(CurrentTime),
  NCT is CurrentTime-LST+CT,
  assertz(stopwatch(LST,NCT,stop)).

reset_stopwatch :-
  retractall(stopwatch(_,_,_)),
  assertz(stopwatch(0,0,stop)).

display_stopwatch :-
  current_stopwatch_time(CT),
  display_stopwatch(CT).

display_stopwatch(CT) :-
  format_timing(CT,FCT),
  write_info_log([FCT]).

last_stopwatch_stop(T) :-
  stopwatch(_LST,T,_Status).
  
current_stopwatch_time(T) :-
  stopwatch(_LST,T,stop),
  !.
current_stopwatch_time(T) :-
  stopwatch(LST,CT,_),
  my_get_time(CurrentTime),
  T is CurrentTime-LST+CT.


verb_display_stopwatch :-
  verbose(off),
  !.
verb_display_stopwatch :-
  display_stopwatch.
    
/*********************************************************************/
/* Parsing                                                           */
/*********************************************************************/

my_arguments([]) -->
  [].
my_arguments([A|As]) -->
  my_charsbutcomma(Cs),
  my_blanks_star,
  ",",
  my_blanks_star,
  {name(A,Cs)},
  my_arguments(As).
my_arguments([A]) -->
  my_charsbutcomma(Cs),
  {name(A,Cs)}.

my_charsbutcomma([C|Cs]) -->
  my_charsbut(",",[C|Cs]).
  
my_chars_but_blanks([C|Cs]) -->
  my_charsbut(" ",[C|Cs]).
  
my_charsbut(B,[C|Cs]) -->
  my_charbut(B,C),
  my_charsbut(B,Cs).
my_charsbut(B,[C]) -->
  my_charbut(B,C).

my_charbut(B,C) -->
  [C],
  {[C] \= B}.

my_pattern(N/A) -->
  my_blanks_star,
%  my_sql_user_identifier(N),
  my_user_identifier(N),
  my_blanks_star,
  "/",
  my_blanks_star,
  my_positive_integer(A),
  my_blanks_star.
my_pattern(-(N)/A) -->
  my_blanks_star,
  "-",
  my_blanks_star,
%  my_sql_user_identifier(N),
  my_user_identifier(N),
  my_blanks_star,
  "/",
  my_blanks_star,
  my_positive_integer(A),
  my_blanks_star.

% User identifiers for both SQL and Datalog
my_user_identifier(I) -->
  my_sql_user_identifier(I).
my_user_identifier(I) -->
  my_symbol(I).
  
  
%
% Parsing queries
%

parse_datalog_query(Term,Vo) -->
  my_blanks_star,
  my_literal(NTerm,[],Vo),
  my_blanks_star,
  {!,
   abstract_nulls(NTerm,ATerm),
   concrete_nulls(ATerm,Term,_Grounded)}.
% GNU Prolog:
% parse_datalog_query(H, B, A, E) :-
%         my_blanks_star(A, C),
%         my_literals(F, [], B, C, D),
%         my_blanks_star(D, E), !,
%         abstract_nulls(F, G),
%         concrete_nulls(G, H, _).
  
%
% Parsing rules: Nulls are uniquelly grounded
%

parse_rule(Rule,Vi,Vo) -->
  my_blanks_star,
  my_head(NHead,Vi,Vo1),
  my_blanks_star,
  ":-",
  my_blanks_star,
  !,
  my_body(NBody,Vo1,Vo),
  my_blanks_star,
  {NRule=':-'(NHead,NBody),
   abstract_nulls(NRule,ARule),
   concrete_nulls(ARule,Rule,_Grounded)}.  
parse_rule(Rule,Vi,Vo) -->
  my_blanks_star,
  my_head(NHead,Vi,Vo),
  my_blanks_star,
  {!,
   abstract_nulls(NHead,AHead),
   concrete_nulls(AHead,Rule,_Grounded)}.

   
%
% Parsing rule heads: Nulls are uniquelly grounded
%

parse_head(Head,Vi,Vo) -->
  my_head(NHead,Vi,Vo),
  my_blanks_star,
  {!,
   abstract_nulls(NHead,AHead),
   concrete_nulls(AHead,Head,_Grounded)}.
%  !.  % This cut should be before abstract_nulls. 
  
check_redef(Rule) :-
   rule_pred(Rule,F/A),
%    Term =.. [F|As],
%    length(As,A),
   (datalog_keyword(F,A) -> redefinition_error(F,A); true).
   
datalog_keyword(F,A) :-
   my_infix_relation(F,_), A=2   ;
   my_builtin_relation(F,A,_,_)  ;
   my_infix_comparison(F,_), A=2 ; % Infix comparison operators are rejected when parsing
   my_assertion_predicate(F,A)   ; % Built-in predicates in assertion (persistent, modes, ...)
   my_infix_arithmetic(F), A=2   ; % Names of arithmetic Built-ins can be reused because of their scope
   F = (','), A=2                ;
   F = 'not', A=1                ;
   F = 'answer'.
   
datalog_keyword(F) :-
  datalog_keyword(F,_A).

redef_rule_error_list([]).  
redef_rule_error_list([R|Rs]) :-
%  (R=':-'(T,_) ; R=T),  
%  !,
  check_redef(R),
  redef_rule_error_list(Rs).
   
   
%
% Parsing rule bodies: Nulls are uniquelly grounded
%

parse_body(Body,Vi,Vo) -->
  my_blanks_star,
  my_body(NBody,Vi,Vo),
  my_blanks_star,
  {!,
   abstract_nulls(NBody,ABody),
   concrete_nulls(ABody,Body,_Grounded)}.


%
% Parsing single line remarks: lines starting with % or -- are interpreted as single line remarks
%

parse_single_line_remark -->
  parse_datalog_single_line_remark.
parse_single_line_remark -->
  parse_sql_single_line_remark.
  
%  Datalog
parse_datalog_single_line_remark -->
  my_blanks_star,
  "%",
  my_chars_star_up_to_EOL(_).
  
%  SQL
parse_sql_single_line_remark -->
  my_blanks_star,
  "--",
  my_chars_star_up_to_EOL(_).

my_chars_star_up_to_EOL([]) -->
  [EOL],
  {my_end_of_input(EOL,_),
   !}.
my_chars_star_up_to_EOL([C|Cs]) -->
  [C],
  my_chars_star_up_to_EOL(Cs).
my_chars_star_up_to_EOL([]) -->
  [].

%
% Parsing multi-line remarks: lines starting with /* and ending with */ are interpreted as multi-line remarks
%

parse_multi_line_remark -->
  my_blanks_star,
  my_multi_line_remark,
  my_blanks_star.
  
my_multi_line_remark -->
  "/*",
  my_remark_body,
  "*/".

my_remark_body -->
  my_chars_star_but_multi_line_remark_delimiters,
  my_multi_line_remark,
  my_chars_star_but_multi_line_remark_delimiters.
my_remark_body -->
  my_chars_star_but_multi_line_remark_delimiters.
      
my_chars_star_but_multi_line_remark_delimiters -->
  [].
my_chars_star_but_multi_line_remark_delimiters -->
  "/*",
  {!,fail}.
my_chars_star_but_multi_line_remark_delimiters -->
  "*/",
  {!,fail}.
my_chars_star_but_multi_line_remark_delimiters -->
  [_C],
  my_chars_star_but_multi_line_remark_delimiters.

%
% Rules, heads, bodies and queries
%

my_rule(':-'(H,B),Vi,Vo) -->
  my_blanks_star,
  my_head(H,Vi,Vo1),
  my_blanks_star,
  ":-",
  my_blanks_star,
  my_body(B,Vo1,Vo),
  my_blanks_star.
my_rule(H,Vi,Vo) -->
  my_blanks_star,
  my_head(H,Vi,Vo),
  my_blanks_star.

my_head(H,Vi,Vo) -->
  my_atom(H,Vi,Vo).
my_head(-(H),Vi,Vo) -->
  "-",
  my_blanks_star,
  my_opening_parentheses_star(N),
  my_blanks_star,
  my_atom(H,Vi,Vo),
  my_blanks_star,
  my_closing_parentheses_star(N).

my_body(B,Vi,Vo) -->
  my_literals(B,Vi,Vo).

% my_body_arg(B,Vi,Vo) -->
%   "(",
%   my_blanks_star,
%   my_literals(B,Vi,Vo),
%   my_blanks_star,
%   my_right_parenthesis.
% my_body_arg(B,Vi,Vo) -->
%   my_literal(B,Vi,Vo).
  
%my_query(Q,Vi,Vo) -->
%  my_literals(Q,Vi,Vo).


%
% Constants, Variables, Terms, Atoms, Literals
%

my_constant(A) -->
  my_number(A).
my_constant(A) -->
  my_symbol(A),
  {A\=null}.

my_variable(V,Vi,Vo) -->
  my_uppercase(C),
  my_identifier_chars(Cs),
  {name(N,[C|Cs]),
   append_NV(N,V,Vi,Vo)}.
my_variable(V,Vi,Vo) -->
  my_uppercase(C),
  {name(N,[C]),
   append_NV(N,V,Vi,Vo)}.
my_variable(V,Vi,Vo) -->
  "_",
  my_underscored_variable_chars(Cs),
  {[US] = "_",
   atom_codes(N,[US|Cs]),
   append_NV(N,V,Vi,Vo)}.
my_variable(V,Vi,Vo) -->
  "_",
  {append_NV('_',V,Vi,Vo)}.
  
my_variable_or_number(V,Vi,Vo) -->
  my_variable(V,Vi,Vo).
my_variable_or_number(N,Vi,Vi) -->
  my_number(N).

my_variable_or_integer(V,Vi,Vo) -->
  my_variable(V,Vi,Vo).
my_variable_or_integer(N,Vi,Vi) -->
  my_integer(N).

%my_variable_or_star_in_aggr(V,_AF,Vi,Vo) -->
%  my_variable(V,Vi,Vo).
%my_variable_or_star_in_aggr(*,count,Vi,Vi) -->
%  "*".

my_noncompound_terms([T|Ts],Vi,Vo) -->
  my_noncompound_term(T,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_noncompound_terms(Ts,Vo1,Vo).
my_noncompound_terms([T],Vi,Vo) -->
  my_noncompound_term(T,Vi,Vo).
  
my_noncompound_term(T,Vi,Vo) -->
  my_noncompound_term_ne(T,Vi,Vo).
% my_noncompound_term(T,Vi,Vo) -->
%   push_syntax_error(['Invalid non-compound term'],Old),
%   my_noncompound_term_ne(T,Vi,Vo),
%   pop_syntax_error(Old).
  
my_noncompound_term_ne(Null,V,V) -->
  "null",
  {nulls(on) -> Null='$NULL'(_ID) ; Null=null}.
my_noncompound_term_ne(A,V,V) -->
  my_constant(A).
my_noncompound_term_ne(A,Vi,Vo) -->
  my_variable(A,Vi,Vo).
my_noncompound_term_ne('$NULL'(ID),V,V) -->
  "'$NULL'(",
  {nulls(on)},
  my_integer(ID),
  my_right_parenthesis.
my_noncompound_term_ne('$NULL'(ID),Vi,Vo) -->
  "'$NULL'(",
  {nulls(on)},
  my_variable(ID,Vi,Vo),
  my_right_parenthesis.

my_atom(-(A),Vi,Vo) -->
  "-",
  my_blanks_star,
  my_positive_atom(A,Vi,Vo).
my_atom(A,Vi,Vo) -->
  my_positive_atom(A,Vi,Vo).

my_positive_atom(A,Vi,Vo) -->
  my_symbol(F),
  my_blanks_star,
  push_syntax_error(['Invalid atom'],Old1),
  "(",
  pop_syntax_error(Old1),
  push_syntax_error(['Expected sequence of non-compound terms'],Old2),
  my_blanks_star,
  my_noncompound_terms(Ts,Vi,Vo),
  my_blanks_star,
  my_right_parenthesis,
  pop_syntax_error(Old2),
  {A =.. [F|Ts]}.
%    length(Ts,L),
%    not_builtin(F/L)}.
% my_positive_atom(A,Vi,Vo) -->
%   my_noncompound_term(L,Vi,Vo1),
%   my_blanks_star,
%   my_infix_comparison(P),
%   my_blanks_star,
%   push_syntax_error(['Invalid right-hand-side term'],Old),
%   my_noncompound_term(R,Vo1,Vo), 
%   my_blanks_star,
%   pop_syntax_error(Old),
%   {A =.. [P,L,R]}.
my_positive_atom(A,Vi,Vo) -->
  my_variable_or_number(L,Vi,Vo1),
  my_blanks,
  "is",
  my_blanks,
  push_syntax_error(['Arithmetic expression expected'],Old),
  my_arithmeticexp(R,Vo1,Vo),
  my_blanks_star,
  {A =.. [is,L,R]},
  pop_syntax_error(Old).
my_positive_atom(A,Vi,Vo) -->
  my_expression(L,Vi,Vo1),
  my_blanks_star,
  my_infix_comparison(Op),
  my_blanks_star,
  push_syntax_error(['Invalid right-hand-side term'],Old),
  my_expression(R,Vo1,Vo),
  my_blanks_star,
  {A =.. [Op,L,R]},
  pop_syntax_error(Old).
my_positive_atom(A,V,V) -->
  my_symbol(A).
  
my_atoms_star([A,B|As],Vi,Vo) -->
  my_atom(A,Vi,Vo1),
  my_blanks,
  my_atoms_star([B|As],Vo1,Vo).
my_atoms_star([A],Vi,Vo) -->
  my_atom(A,Vi,Vo),
  my_blanks_star.
my_atoms_star([],V,V) -->
  [].  

prefix(P,fx,P-1).
prefix(P,fy,P).

infix(P,xfx,P-1,P-1).
infix(P,xfy,P-1,P).
infix(P,yfx,P,P-1).

posfix(P,xf,P-1).
posfix(P,yf,P).

my_literal(L,Vi,Vo) -->
  my_basic_literal(L,Vi,Vo).

my_literals(L,Vi,Vo) -->
  my_literals(1200,L,Vi,Vo).

my_literals(PP,To,Vi,Vo) -->
  my_basic_literal(L,Vi,Vo1), 
  my_r_literals(PP,0,L/To,Vo1,Vo).
my_literals(PP,To,Vi,Vo) -->
  "(", 
  my_blanks_star, 
  my_literals(1200,T,Vi,Vo1), 
  my_blanks_star, 
  my_right_parenthesis,
  !,
  my_r_literals(PP,0,(T)/To,Vo1,Vo).
my_literals(PP,To,Vi,Vo) -->
  {my_literal_operator(P,FX,SOP,OP),
   prefix(P,FX,PR),
   P=<PP},
  my_string(SOP),
  my_right_spaced(SOP),
  my_literals(PR,T,Vi,Vo1), 
  {NT=..[OP,T]},
  my_r_literals(PP,P,NT/To,Vo1,Vo).
  
my_r_literals(PP,Pi,Ti/To,Vi,Vo) -->
  {my_literal_operator(P,YFX,SOP,OP),
   infix(P,YFX,PL,PR),
   P=<PP,
   Pi=<PL},
  my_left_spaced(SOP), 
  my_string(SOP),
  my_right_spaced(SOP), 
  my_literals(PR,T,Vi,Vo1), 
  {NT=..[OP,Ti,T],
   (OP='=>'
    ->
     rules_from_hyp_program(Ti,Rs),
     redef_rule_error_list(Rs) 
    ;
     true)
  }, 
  my_r_literals(PP,P,NT/To,Vo1,Vo).
% my_r_literals(PP,Pi,Ti/To,Vi,Vo) --> % No posfix operators yet
%   {my_literal_operator(P,FX,SOP,OP),
%    posfix(P,FX,PL),
%    P=<PP,
%    Pi=<PL,
%    NT=..[OP,Ti]},
%   my_blanks_star, 
%   my_string(SOP),
%   my_r_literals(PP,P,NT/To,Vi,Vo).
my_r_literals(_,_,Ti/Ti,Vi,Vi) -->
  [].
  
my_literal_operator(1200,xfx,":-", ':-').
my_literal_operator(1100,xfy,";",  ';').
my_literal_operator(1050,xfy,"=>", '=>').
my_literal_operator(1020,yfx,"/\\",'/\\').
my_literal_operator(1000,xfy,",",  ',').
my_literal_operator(500,yfx,"or",  'or').
my_literal_operator(400,xfy,"division",'division').
%my_literal_operator(300, fy, "-",'-').
%my_literal_operator(900, fy, "\\+",'\\+').
my_literal_operator(900,fy,"not",'not').

  
my_basic_literal(E,Vi,Vo) -->
  "(",
  my_blanks_star,
  my_literals(E,Vi,Vo),
  my_blanks_star,
  my_right_parenthesis,
  !.
% my_basic_literal('$diff'(A),Vi,Vo) -->
%   "'$diff'",
%   my_blanks_star,
%   "(",
%   my_blanks_star,
%   my_atom(A,Vi,Vo),
%   my_blanks_star,
%   ")",
%   !.
my_basic_literal(not(Body),Vi,Vo) -->
  "not",
  my_blanks_star,
  "(",
  my_blanks_star,
  push_syntax_error(['Single operand of not/1 must be a relation'],Old),
  my_body(Body,Vi,Vo),
  my_blanks_star,
  my_right_parenthesis,
  pop_syntax_error(Old),
  !.
my_basic_literal(top(N,Body),Vi,Vo) -->
  "top",
  my_blanks_star,
  "(",
  my_blanks_star,
  push_syntax_error(['First argument of top/2 must be either a variable or a number'],Old1),
  my_variable_or_integer(N,Vi,Vo1),
  my_blanks_star,
  ",",
  pop_syntax_error(Old1),
  my_blanks_star,
  push_syntax_error(['Second argument of top/2 must be a relation'],Old2),
  my_body(Body,Vo1,Vo),
  my_blanks_star,
  my_right_parenthesis,
  pop_syntax_error(Old2),
  !,
  {((var(N);N>0)
    ->
     true
    ;
     my_raise_exception(top(N,Body),syntax('First argument of top must be greater than 0:'),Vo)
   )}.
my_basic_literal(distinct(Body),Vi,Vo) -->
  "distinct",
  my_blanks_star,
  "(",
  my_blanks_star,
  push_syntax_error(['Single argument of distinct/1 must be a relation'],Old1),
  my_body(Body,Vi,Vo),
  my_blanks_star,
  my_right_parenthesis,
  pop_syntax_error(Old1),
  !.
my_basic_literal(distinct(Vars,Body),Vi,Vo) -->
  "distinct",
  my_blanks_star,
  "(",
  my_blanks_star,
  push_syntax_error(['First argument of distinct/2 must be a list of variables'],Old1),
  my_var_list(Vars,Vi,Vo1),
  my_blanks_star,
  ",",
  pop_syntax_error(Old1),
  my_blanks_star,
  push_syntax_error(['Second argument of distinct/2 must be a relation'],Old2),
  my_body(Body,Vo1,Vo),
  my_blanks_star,
  my_right_parenthesis,
  pop_syntax_error(Old2),
  !,
  {term_variables(Body,BodyVars),
   (my_intersect_var(Vars,BodyVars,Vars)
    ->
     true
    ;
     my_raise_exception(distinct(Vars,Body),syntax('Variables in the first argument of ''distinct'' must occur in its second argument:'),Vo)
   )}.
my_basic_literal(exists(Vars,Body),Vi,Vo) -->
  "exists",
  my_blanks_star,
  "(",
  my_blanks_star,
  push_syntax_error(['First argument of exists/2 must be a list of variables'],Old1),
  my_var_list(Vars,Vi,Vo1),
  my_blanks_star,
  ",",
  pop_syntax_error(Old1),
  my_blanks_star,
  push_syntax_error(['Second argument of exists/2 must be a relation'],Old2),
  my_body(Body,Vo1,Vo),
  my_blanks_star,
  my_right_parenthesis,
  pop_syntax_error(Old2),
  !,
  {term_variables(Body,BodyVars),
   (my_intersect_var(Vars,BodyVars,Vars)
    ->
     true
    ;
     my_raise_exception(exists(Vars,Body),syntax('Variables in the first argument of ''exists'' must occur in its second argument:'),Vo)
   )}.
my_basic_literal(order_by(Body,Exprs,OrdSpecs),Vi,Vo) -->
  "order_by",
  my_blanks_star,
  "(",
  my_blanks_star,
  push_syntax_error(['First argument of order_by must be a relation'],Old1),
  my_body(Body,Vi,Vo1),
  my_blanks_star,
  ",",
  pop_syntax_error(Old1),
  my_blanks_star,
  push_syntax_error(['Second argument of order_by must be a list of expressions'],Old2),
  my_expression_list(Exprs,Vo1,Vo),
  my_blanks_star,
  pop_syntax_error(Old2),
  my_optional_ord_specs(Exprs,OrdSpecs),
  my_blanks_star,
  my_right_parenthesis,
  !,
  {term_variables(Body,BodyVars),
   term_variables(Exprs,ExprsVars),
   (my_intersect_var(ExprsVars,BodyVars,ExprsVars)
    ->
     true
    ;
     my_raise_exception(order_by(Body,Exprs,OrdSpecs),syntax('Variables in the second argument of ''order_by'' must occur in its first argument:'),Vo)
   )}.
my_basic_literal(group_by(B,Vs,C),Vi,Vo) -->
  "group_by",
  my_blanks_star,
  "(",
  my_blanks_star,
  push_syntax_error(['First argument of group_by/3 must be a relation'],Old1),
  my_body(B,Vi,Vo1),
  my_blanks_star,
  ",",
  pop_syntax_error(Old1),
  my_blanks_star,
  push_syntax_error(['Second argument of group_by/3 must be a list of variables'],Old2),
  my_var_list(Vs,Vo1,Vo2),
  my_blanks_star,
  ",",
  pop_syntax_error(Old2),
  my_blanks_star,
  push_syntax_error(['Last argument of group_by/3 must be a relation'],Old3),
  my_body(C,Vo2,Vo),
  my_blanks_star,
  my_right_parenthesis,
  pop_syntax_error(Old3),
  !,
  {valid_grouping(B,Vs,Vo),
   valid_group_by_body(B,Vs,C,Vo)}.
my_basic_literal(call(A),Vi,Vo) -->
  {nulls(on)},
  "call",
  my_blanks_star,
  "(",
  my_blanks_star,
  push_syntax_error(['Single argument of call/1 must be an atom'],Old1),
  my_atom(A,Vi,Vo),
  my_blanks_star,
  my_right_parenthesis,
  pop_syntax_error(Old1),
  !.
my_basic_literal(st(A),Vi,Vo) -->
  {nulls(on)},
  "st",
  my_blanks_star,
  "(",
  my_blanks_star,
  push_syntax_error(['Single argument of st/1 must be an atom'],Old1),
  my_atom(A,Vi,Vo),
  my_blanks_star,
  my_right_parenthesis,
  pop_syntax_error(Old1),
  !.
my_basic_literal(lj(A,B,C),Vi,Vo) -->  
  {nulls(on)},
  "lj",
  my_blanks_star,
  "(",
  my_blanks_star,
  push_syntax_error(['First argument of lj/3 must be a relation'],Old1),
  my_basic_literal(A,Vi,Vo1),
  my_blanks_star,
  ",",
  pop_syntax_error(Old1),
  my_blanks_star,
  push_syntax_error(['Second argument of lj/3 must be a relation'],Old2),
  my_basic_literal(B,Vo1,Vo2),
  my_blanks_star,
  ",",
  pop_syntax_error(Old2),
  my_blanks_star,
  push_syntax_error(['Third argument of lj/3 must be a valid query'],Old3),
  my_body(C,Vo2,Vo),
  my_blanks_star,
  my_right_parenthesis,
  pop_syntax_error(Old3),
  !.
my_basic_literal(rj(A,B,C),Vi,Vo) -->  
  {nulls(on)},
  "rj",
  my_blanks_star,
  "(",
  my_blanks_star,
  push_syntax_error(['First argument of rj/3 must be a relation'],Old1),
  my_basic_literal(A,Vi,Vo1),
  my_blanks_star,
  ",",
  pop_syntax_error(Old1),
  my_blanks_star,
  push_syntax_error(['Second argument of rj/3 must be a relation'],Old2),
  my_basic_literal(B,Vo1,Vo2),
  my_blanks_star,
  ",",
  pop_syntax_error(Old2),
  my_blanks_star,
  push_syntax_error(['Third argument of rj/3 must be a valid query'],Old3),
  my_body(C,Vo2,Vo),
  my_blanks_star,
  my_right_parenthesis,
  pop_syntax_error(Old3),
  !.
my_basic_literal(fj(A,B,C),Vi,Vo) -->  
  {nulls(on)},
  "fj",
  my_blanks_star,
  "(",
  my_blanks_star,
  push_syntax_error(['First argument of fj/3 must be a relation'],Old1),
  my_basic_literal(A,Vi,Vo1),
  my_blanks_star,
  ",",
  pop_syntax_error(Old1),
  my_blanks_star,
  push_syntax_error(['Second argument of fj/3 must be a relation'],Old2),
  my_basic_literal(B,Vo1,Vo2),
  my_blanks_star,
  ",",
  pop_syntax_error(Old2),
  my_blanks_star,
  push_syntax_error(['Third argument of fj/3 must be a valid query'],Old3),
  my_body(C,Vo2,Vo),
  my_blanks_star,
  my_right_parenthesis,
  pop_syntax_error(Old3),
  !.
my_basic_literal(Aggr,Vi,Vo) -->
  my_aggregate_relation(AF,3),
  my_blanks_star,
  "(",
  my_blanks_star,
  push_syntax_error(['First argument of aggregate ',AF/3,' must be a valid relation'],Old1),
  my_body(P,Vi,Vo1),
  my_blanks_star,
  ",",
  pop_syntax_error(Old1),
  my_blanks_star,
%  my_variable(V,Vo1,Vo2),
  my_arithmeticexp(E,Vo1,Vo2),
  my_blanks_star,
  ",",
  my_blanks_star,
  push_syntax_error(['Last argument of aggregate ',AF/3,' must be either a variable or number'],Old3),
  my_variable_or_number(T,Vo2,Vo),
  my_blanks_star,
  my_right_parenthesis,
  pop_syntax_error(Old3),
  !,
  {Aggr =.. [AF,P,E,T],
   term_variables(E,Vs),
   (my_member_vars_term(Vs,P)
    ->
     true
    ;
     my_raise_exception(Aggr,syntax(['Pivot variables ',Vs,' must occur in the first argument ',P,' of aggregate']),Vo)
   )
  }.
my_basic_literal(Aggr,Vi,Vo) -->
  my_aggregate_relation(AF,2),
  my_blanks_star,
  "(",
  my_blanks_star,
  push_syntax_error(['First argument of aggregate ',AF/2,' must be a valid relation'],Old1),
  my_body(P,Vi,Vo1),
  my_blanks_star,
  ",",
  pop_syntax_error(Old1),
  my_blanks_star,
  push_syntax_error(['Last argument of aggregate ',AF/2,' must be either a variable or number'],Old2),
  my_variable_or_number(T,Vo1,Vo),
  my_blanks_star,
  my_right_parenthesis,
  pop_syntax_error(Old2),
  !,
  {Aggr =.. [AF,P,T]}.
my_basic_literal(L,Vi,Vo) -->
  my_head(L,Vi,Vo),
  {pred_head(F/A,L),
   not_builtin(F/A)}.
%   push_syntax_error(['Invalid literal'],Old),
%   my_head(L,Vi,Vo),
%   pop_syntax_error(Old).


%
% Parsing Datalog constraints
%

parse_datalog_constraint(Constraint,Vo) -->
  my_blanks_star,
  ":-",
  my_blanks_star,
  my_datalog_constraint(Constraint,[],Vo),
  my_blanks_star,
  {!}.

my_datalog_constraint(Constraint,Vi,Vo) -->
  my_predef_datalog_constraints(Constraint,Vi,Vo),
  {!}.
my_datalog_constraint(my_integrity_constraint(Preds,Constraint),Vi,Vo) -->
  my_body(Constraint,Vi,Vo),
  {reachable_user_predicates_rule(':-'(Constraint),Preds),
   \+ (member(Name/Arity,Preds),
       assertion(Name,Arity))}.

my_predef_datalog_constraints((Ctr,Ctrs),Vi,Vo) -->
  my_predef_datalog_constraint(Ctr,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_predef_datalog_constraints(Ctrs,Vo1,Vo).
my_predef_datalog_constraints(Ctr,Vi,Vo) -->
  my_predef_datalog_constraint(Ctr,Vi,Vo),
  my_blanks_star.

% Type declaration: 
%   type(pred,[colname:type]) % colname and type
%   type(pred,[type])         % only type, colnames are assigned as $1, $2, ...
% A propositional relation can also be declared as
%   type(pred,[])
my_predef_datalog_constraint(type(Pred,Types),V,V) -->
  my_kw("TYPE"),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_symbol(Pred),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_list_of_ctr_types(Types),
  my_blanks_star,
  ")".
% Alternative type declaration
%   type(pred(colname:type, ...)) % colname and type
%   type(pred(type,...))          % only type, colnames are assigned as $1, $2, ...
% A propositional relation can also be declared as
%   type(pred)
my_predef_datalog_constraint(type(Pred,Types),V,V) -->
  my_kw("TYPE"),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_typed_predicate_schema(Pred,Types),
  my_blanks_star,
  ")".
my_predef_datalog_constraint(type(Pred,[]),V,V) -->
  my_kw("TYPE"),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_symbol(Pred),
  my_blanks_star,
  ")".
my_predef_datalog_constraint(nn(Pred,Columns),V,V) -->
  my_kw("NN"),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_symbol(Pred),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_list_of_symbols(Columns),
  my_blanks_star,
  ")".
my_predef_datalog_constraint(pk(Pred,Columns),V,V) -->
  my_kw("PK"),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_symbol(Pred),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_list_of_symbols(Columns),
  my_blanks_star,
  ")".
my_predef_datalog_constraint(ck(Pred,Columns),V,V) -->
  my_kw("CK"),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_symbol(Pred),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_list_of_symbols(Columns),
  my_blanks_star,
  ")".
my_predef_datalog_constraint(fk(FKPred,FKColumns,PKPred,PKColumns),V,V) -->
  my_kw("FK"),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_symbol(FKPred),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_list_of_symbols(FKColumns),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_symbol(PKPred),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_list_of_symbols(PKColumns),
  my_blanks_star,
  ")".
my_predef_datalog_constraint(fd(Pred,Columns,DepColumns),V,V) -->
  my_kw("FD"),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_symbol(Pred),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_list_of_symbols(Columns),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_list_of_symbols(DepColumns),
  my_blanks_star,
  ")".
  
my_list_of_symbols(Symbols) -->
  "[",
  my_blanks_star,
  my_symbol_sequence(Symbols),  
  my_blanks_star,
  "]".
 
my_symbol_sequence([Symbol|Symbols]) -->
  my_symbol(Symbol),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_symbol_sequence(Symbols).
my_symbol_sequence([Symbol]) -->
  my_symbol(Symbol).
  
% Declaration of Datalog type constraints (type(Pred,[Type]) or type(Pred,[Colname:Type])
my_list_of_ctr_types(Types) -->
  "[",
  my_blanks_star,
  my_sequence_of_ctr_types(Types),  
  my_blanks_star,
  "]".
my_list_of_ctr_types([]) -->
  "[",
  my_blanks_star,
  "]".
 
my_sequence_of_ctr_types(S) -->
  my_type_sequence(S).
my_sequence_of_ctr_types(S) -->
  my_colnametype_sequence(S).

my_type_sequence([Type|Types]) -->
  my_DLtype(Type),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_type_sequence(Types).
my_type_sequence([Type]) -->
  my_DLtype(Type).
  
my_DLtype(Type) -->
  my_atom(DLType,[],_),
  {dltype_type(DLType,Type)}.
  
my_colnametype_sequence([Type|Types]) -->
  my_colnametype(Type),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_colnametype_sequence(Types).
my_colnametype_sequence([Type]) -->
  my_colnametype(Type).
  
% Datalog columns cannot get the same name as an SQL identifier 
% since they can be referenced from an SQL statement  
my_colnametype(Colname:Type) -->
  my_non_sql_symbol(Colname),
  my_blanks_star,
  ":",
  my_blanks_star,
  my_DLtype(Type).
  
my_aggregate_relation(count_distinct,4) -->
  "count_distinct".  
my_aggregate_relation(count_distinct,3) -->
  "count_distinct".  
my_aggregate_relation(count_distinct,2) -->
  "count_distinct".  
my_aggregate_relation(count,4) -->
  "count".  
my_aggregate_relation(count,3) -->
  "count".  
my_aggregate_relation(count,2) -->  
  "count".  
my_aggregate_relation(sum_distinct,4) -->
  "sum_distinct".  
my_aggregate_relation(sum_distinct,3) -->
  "sum_distinct".  
my_aggregate_relation(sum,4) -->
  "sum".  
my_aggregate_relation(sum,3) -->
  "sum".  
my_aggregate_relation(times_distinct,4) -->
  "times_distinct".  
my_aggregate_relation(times_distinct,3) -->
  "times_distinct".  
my_aggregate_relation(times,4) -->
  "times".  
my_aggregate_relation(times,3) -->
  "times".  
my_aggregate_relation(avg_distinct,4) -->
  "avg_distinct".  
my_aggregate_relation(avg_distinct,3) -->
  "avg_distinct".  
my_aggregate_relation(avg,4) -->
  "avg".  
my_aggregate_relation(avg,3) -->
  "avg".  
% my_aggregate_relation(min,4) --> % As allowed by SQL2, min_distinct behaves as min
%   "min_distinct".  
% my_aggregate_relation(min,3) -->
%   "min_distinct".  
my_aggregate_relation(min,4) -->
  "min".  
my_aggregate_relation(min,3) -->
  "min".  
% my_aggregate_relation(max,4) --> % As allowed by SQL2, max_distinct behaves as max
%   "max_distinct".
% my_aggregate_relation(max,3) -->
%   "max_distinct".
my_aggregate_relation(max,4) -->
  "max".
my_aggregate_relation(max,3) -->
  "max".
 
my_non_system_aggregate_relation(AF,Arity) :-
  my_aggregate_relation(AF,Arity),
  Arity<4.

my_aggregate_relation(AF,Arity) :-
  my_aggregate_relation(AF,Arity,_H,_T).  
  
my_optional_ord_specs(Exprs,OrdSpecs) -->
  ",",
  my_blanks_star,
  push_syntax_error(['List of order expressions expected'],Old1),
  "[",
  my_blanks_star,
  my_ord_specs(Exprs,OrdSpecs),
  my_blanks_star,
  "]",
  pop_syntax_error(Old1),
  my_blanks_star,
  !.
my_optional_ord_specs(Exprs,OrdSpecs) -->
  [],
  {length(Exprs,L),
   length(OrdSpecs,L),
   my_map_1('='(a),OrdSpecs)}.
   
my_ord_specs([_E],[a]) -->
  "a".
my_ord_specs([_E],[d]) -->
  "d".
my_ord_specs([E1,E2|Es],[O1,O2|Os]) -->
  my_blanks_star,
  my_ord_specs([E1],[O1]),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_ord_specs([E2|Es],[O2|Os]).
  
%
% Ancillary Stuff
%
    
lang_interpreter_cmd(datalog) --> 
  command_begin,
  my_blanks_star,
  my_kw("DATALOG"),
  my_blanks.
lang_interpreter_cmd(prolog) --> 
  command_begin,
  my_blanks_star,
  my_kw("PROLOG"),
  my_blanks.
lang_interpreter_cmd(sql) --> 
  command_begin,
  my_blanks_star,
  my_kw("SQL"),
  my_blanks.
lang_interpreter_cmd(ra) --> 
  command_begin,
  my_blanks_star,
  my_kw("RA"),
  my_blanks.
lang_interpreter_cmd(drc) --> 
  command_begin,
  my_blanks_star,
  my_kw("DRC"),
  my_blanks.
lang_interpreter_cmd(trc) --> 
  command_begin,
  my_blanks_star,
  my_kw("TRC"),
  my_blanks.
% % lang_interpreter_cmd(hrsql) --> 
% %   command_begin,
% %   my_blanks_star,
% %   my_kw("HRSQL"),
% %   my_blanks.

my_var_list([],V,V) -->  
  "[",
  my_blanks_star,
  "]".
my_var_list(Vs,Vi,Vo) -->  
  "[",
  my_blanks_star,
  my_comma_separated_vars(Vs,Vi,Vo),
  my_blanks_star,
  "]".
  
my_comma_separated_vars([V|Vs],Vi,Vo) -->
  my_variable(V,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_comma_separated_vars(Vs,Vo1,Vo).
my_comma_separated_vars([V],Vi,Vo) -->
  my_variable(V,Vi,Vo).
  
my_blank -->         % One blank
  " ".
my_blank -->         % Tab
  "\t".
my_blank -->         % End of line
  [EOL],
  {end_of_line(EOL)}.
  
my_blanks_star -->   % Zero or more blanks. Eagerly consumes blanks
  my_blanks.         
  %{!}.               % leaving no choicepoints
my_blanks_star -->
  [],
  !. % :::WARNING

% my_blanks_star -->   % Zero or more blanks. Eagerly consumes blanks
%   my_blanks_star,         
%   {!}.               % leaving no choicepoints
my_blanks_star(A, B) :-
  my_blanks_star(A, B), 
  !.
        
my_blanks -->        % One or more blanks
  my_blank,
  my_blanks_star,
  !. % :::WARNING


my_right_parenthesis," " --> % Consume the parenthesis but adding a blank for textual operators, as "(a)or(b)"
  ")".
  
  
% my_letter(C) --> 
%   my_lowercase(C).
% my_letter(C) --> 
%   my_uppercase(C).

my_lowercase(C) --> 
  [C],
  {my_lowercase(C)}.
  
my_uppercase(C) -->
  [C],
  {my_uppercase(C)}.
   
my_digit(C) -->
  [C],
  {[C] >= "0",
   [C] =< "9"}.

my_lowercase(C) :-
  [C] >= "a",
  [C] =< "z".
my_lowercase(C) :-
  [C] = "á".
my_lowercase(C) :-
  [C] = "é".
my_lowercase(C) :-
  [C] = "í".
my_lowercase(C) :-
  [C] = "ó".
my_lowercase(C) :-
  [C] = "ú".
my_lowercase(C) :-
  [C] = "à".
my_lowercase(C) :-
  [C] = "è".
my_lowercase(C) :-
  [C] = "ì".
my_lowercase(C) :-
  [C] = "ò".
my_lowercase(C) :-
  [C] = "ù".
my_lowercase(C) :-
  [C] = "ä".
my_lowercase(C) :-
  [C] = "ë".
my_lowercase(C) :-
  [C] = "ï".
my_lowercase(C) :-
  [C] = "ö".
my_lowercase(C) :-
  [C] = "ü".
my_lowercase(C) :-
  [C] = "ñ".
my_lowercase(C) :-
  [C] = "ç".

my_uppercase(C) :-
  [C] >= "A",
  [C] =< "Z".
my_uppercase(C) :-
  [C] = "Á".
my_uppercase(C) :-
  [C] = "É".
my_uppercase(C) :-
  [C] = "Í".
my_uppercase(C) :-
  [C] = "Ó".
my_uppercase(C) :-
  [C] = "Ú".
my_uppercase(C) :-
  [C] = "À".
my_uppercase(C) :-
  [C] = "È".
my_uppercase(C) :-
  [C] = "Ì".
my_uppercase(C) :-
  [C] = "Ò".
my_uppercase(C) :-
  [C] = "Ù".
my_uppercase(C) :-
  [C] = "Ä".
my_uppercase(C) :-
  [C] = "Ë".
my_uppercase(C) :-
  [C] = "Ï".
my_uppercase(C) :-
  [C] = "Ö".
my_uppercase(C) :-
  [C] = "Ü".
my_uppercase(C) :-
  [C] = "Ñ".
my_uppercase(C) :-
  [C] = "Ç".
  
  
% my_identifier_chars: 
% one or more digits, letters, underscores or dollars
my_identifier_chars([C|Cs]) -->
  my_identifier_char(C),
  my_identifier_chars(Cs).
my_identifier_chars([C]) -->
  my_identifier_char(C),
  !. % :::WARNING

% my_underscored_variable_chars: 
% one or more letters, digits or underscores
my_underscored_variable_chars([C|Cs]) -->
  my_underscored_variable_char(C),
  my_underscored_variable_chars(Cs).
my_underscored_variable_chars([C]) -->
  my_underscored_variable_char(C),
  !. % :::WARNING

my_underscored_variable_char(C) --> 
  my_alfa(C).
my_underscored_variable_char(C) -->
  my_digit(C).
my_underscored_variable_char(C) -->
  "_",
  {[C] = "_"}.

% my_identifier_chars_star:
% zero or more digits, letters, underscores or dollars
my_identifier_chars_star(Cs) -->
  my_identifier_chars(Cs).
my_identifier_chars_star([]) -->
  [].

% my_identifier_char(C) --> 
%   my_char(C).
my_identifier_char(C) --> 
  my_lowercase(C).
my_identifier_char(C) -->
  my_uppercase(C).
my_identifier_char(C) -->
  my_digit(C).
my_identifier_char(C) -->
  "_",
  {[C] = "_"}.
my_identifier_char(C) -->
  "$",
  {[C] = "$"}.

% my_chars: one or more characters
my_chars([C]) -->
  my_char(C).
my_chars([C|Cs]) -->
  my_char(C),
  my_chars(Cs).

my_char(C) -->
  [C].

% my_chars_star: zero or more characters
% my_chars_star([]) -->
%   [].
% my_chars_star(Cs) -->
%   my_chars(Cs). 
   
% my_quote_enclosed_chars: one or more characters enclosed between quotes
my_quote_enclosed_chars([C|Cs]) -->
  my_non_quote_char(C),
  my_quote_enclosed_chars(Cs).
my_quote_enclosed_chars([C]) -->
  my_non_quote_char(C),
  !. % :::WARNING

% my_non_quote_char: one character inside a quoted atom
my_non_quote_char(C2) --> % An escaped quote inside a quoted atom denoting a single quote
  [C1,C2],
  {[C1,C2] = "\\'"}.
my_non_quote_char(C) --> % A pair of quotes inside a quoted atom denoting a single quote
  [C,C],
  {[C] = "'"}.
my_non_quote_char(C) --> % A single quote is not allowed inside a quoted atom
  [C],
  {[C] \== "'"}.

% my_symbol is intended to parse Datalog constants, function (functor), and relation (predicate) symbols
my_symbol(A) -->
  my_lowercase(C),
  my_identifier_chars_star(Cs),
  push_syntax_error(['Invalid symbol'],Old),
 {name(A,[C|Cs])},
  pop_syntax_error(Old).
my_symbol('') -->
  "''".
my_symbol(A) -->
  "'",
  push_syntax_error(['Invalid symbol'],Old1),
  my_quote_enclosed_chars(Cs),
  pop_syntax_error(Old1),
  push_syntax_error(['Expected single quote ('')'],Old2),
  "'",
  {atom_codes(A,Cs)},
  pop_syntax_error(Old2).
% my_symbol(_A) -->
%   my_uppercase(_C),
%   push_syntax_error(['An identifier cannot start in uppercase unless single quoted.'],_Old),
%   {fail}.
  
% my_non_sql_symbol is intended to identify user identifiers which are not SQL reserved words
my_non_sql_symbol(A) -->
  my_symbol(A),
  {my_non_sql_symbol(A)}.
  
my_non_sql_symbol(A) :-
  atom(A),
  \+ sql_identifier(A).
  
% Type conversion: Datalog types -> Internal types
dltype_type(string,string(varchar)).
dltype_type(varchar,string(varchar)).
dltype_type(char,string(char(1))).
dltype_type(char(N),string(char(N))) :-
  (integer(N)
   ->
    true
   ;
    write_error_log(['Invalid type: ',char(N)]),
    fail
  ).
dltype_type(varchar(N),string(varchar(N))) :-
  (integer(N)
   ->
    true
   ;
    write_error_log(['Invalid type: ',varchar(N)]),
    fail
  ).
dltype_type(integer,number(integer)).
dltype_type(int,number(integer)).
dltype_type(float,number(float)).
dltype_type(real,number(float)).
% dltype_type(Type,_) :-
%   write_error_log(['Invalid type: ',Type]),
%   fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Validate syntax
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% valid_grouping(+Relation,+GroupByVars,+NVs)
% Check that group-by-variables are in the relation to be grouped
% NVs are used to display the relation with source var names with their textual names in case of failure
valid_grouping(B,GBVs,_NVs) :-
  term_variables(B,BVs),
  my_set_diff(GBVs,BVs,[]),
  !.
valid_grouping(B,_GBVs,NVs) :-
  my_raise_exception(generic,syntax(['Grouping variables are not in source relation: ',B]),NVs).

% valid_group_by_body(+Relation,+GroupingVariables,+Body (including aggregate functions),+NVs)
% - Check that variables as arguments of aggregate functions in Body are in the Relation
% - Check that variables in Relation which are not in the grouping set do not occur in Body but as arguments of aggregates
% - Check type of Body
valid_group_by_body(R,GBVs,B,NVs) :-
  valid_group_by_aggr_arg(R,B,NVs),
  valid_group_by_set_var(R,GBVs,B,NVs).
%  valid_group_by_body_type(R,B,NVs).

valid_group_by_aggr_arg(R,B,NVs) :-
  term_variables(R,RVs),
  my_aggregate_function(F,A),
  functor(T,F,A),
  my_member_var_term(T,B),
  term_variables(T,TVs),
  (my_set_diff(TVs,RVs,[])
   ->
    fail
   ;
    my_raise_exception(generic,syntax(['Variable in aggregate function ',T,' must be in ',R]),NVs)).
valid_group_by_aggr_arg(_B,_Vs,_NVs).

valid_group_by_set_var(R,GBVs,B,NVs) :-
  term_variables(R,RVs),
  my_set_diff(RVs,GBVs,SetVs),
  get_vars_not_in_aggr(B,[],BVs),
  my_set_inter(SetVs,BVs,DVs),
  (DVs==[]
   ->
    true
   ;
    (DVs=[_] -> MV=variable ; MV=variables),
    my_raise_exception(generic,syntax(['Ungrouped ',MV,' ',DVs,' cannot occur in ',B,' out of aggregate functions.']),NVs)).

get_vars_not_in_aggr(V,AVs,[V|AVs]) :- 
  var(V),
  !.
get_vars_not_in_aggr('$NULL'(_ID),AVs,AVs) :- 
  !.
get_vars_not_in_aggr(T,AVs,AVs) :- 
  atomic(T),
  !.
get_vars_not_in_aggr(T,AVs,AVs) :- 
  functor(T,F,1),
  arithmetic_function(F,_,_,aggregate,_,1),
  !.
get_vars_not_in_aggr(T,AVsi,AVso) :- 
  T =.. [_F|As],
  get_vars_not_in_aggr_list(As,AVsi,AVso).

get_vars_not_in_aggr_list([],AVs,AVs) :-
  !.
get_vars_not_in_aggr_list([T|Ts],AVsi,AVso) :-
  get_vars_not_in_aggr(T,AVsi,AVso1), 
  get_vars_not_in_aggr_list(Ts,AVso1,AVso).

% valid_group_by_body_type(_R,_B,_NVs) :- !.
% valid_group_by_body_type(_R,B,NVs) :-
%   copy_term(B,CB),
%   (get_expr_type(CB,boolean)
%    ->
%    true
%    ;
%    my_raise_exception(generic,syntax(['Incorrect type of ''',B,''' or non-valid expression.']),NVs)).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
% my_expression(E,Vi,Vo) --> 
%   push_syntax_error(['Expression expected'],Old),
%   my_expression_ne(E,Vi,Vo),
%   pop_syntax_error(Old).
%   
% my_expression_ne(E,Vi,Vo) --> 
%   my_noncompound_term(E,Vi,Vo).
% my_expression_ne(E,Vi,Vo) --> 
%   my_arithmeticexp(E,Vi,Vo).
my_expression(E,Vi,Vo) --> 
  my_noncompound_term(E,Vi,Vo).
my_expression(E,Vi,Vo) --> 
  my_arithmeticexp(E,Vi,Vo).
  
my_expression_list(Es,Vi,Vo) -->
  "[",
  my_blanks_star,
  my_expression_sequence(Es,Vi,Vo),  
  my_blanks_star,
  "]".
 
my_expression_sequence([E|Es],Vi,Vo) -->
  my_expression(E,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_expression_sequence(Es,Vo1,Vo).
my_expression_sequence([E],Vi,Vo) -->
  my_expression(E,Vi,Vo).
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing arithmetic expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

my_arithmeticexp(L,Vi,Vo) -->
  my_arithmetic_expression(1200,L,Vi,Vo).

my_arithmetic_expression(PP,To,Vi,Vo) -->
  my_factor(L,Vi,Vo1), 
  my_r_arithmetic_expression(PP,0,L/To,Vo1,Vo).
my_arithmetic_expression(PP,To,Vi,Vo) -->
  "(", 
  my_blanks_star, 
  my_arithmetic_expression(1200,T,Vi,Vo1), 
  my_blanks_star, 
  my_right_parenthesis,
  !,
  my_r_arithmetic_expression(PP,0,(T)/To,Vo1,Vo).
my_arithmetic_expression(PP,To,Vi,Vo) -->
  {my_arithmetic_operator(P,FX,SOP,OP),
   prefix(P,FX,PR),
   P=<PP},
  my_string(SOP),
  my_blanks_star, 
  my_arithmetic_expression(PR,T,Vi,Vo1), 
  {NT=..[OP,T]},
  my_r_arithmetic_expression(PP,P,NT/To,Vo1,Vo).
  
my_r_arithmetic_expression(PP,Pi,Ti/To,Vi,Vo) -->
  {my_arithmetic_operator(P,YFX,SOP,OP),
   infix(P,YFX,PL,PR),
   P=<PP,
   Pi=<PL},
  my_blanks_star, 
  my_string(SOP),
  my_blanks_star, 
%  push_syntax_error(['Incorrect expression'],Old),
  my_arithmetic_expression(PR,T,Vi,Vo1), 
  {NT=..[OP,Ti,T]}, 
  my_r_arithmetic_expression(PP,P,NT/To,Vo1,Vo).
%  pop_syntax_error(Old).
my_r_arithmetic_expression(PP,Pi,Ti/To,Vi,Vo) -->
  {my_arithmetic_operator(P,FX,SOP,OP),
   posfix(P,FX,PL),
   P=<PP,
   Pi=<PL,
   NT=..[OP,Ti]},
  my_blanks_star, 
  my_string(SOP),
  my_r_arithmetic_expression(PP,P,NT/To,Vi,Vo).
my_r_arithmetic_expression(_,_,Ti/Ti,Vi,Vi) -->
  [].
  
%my_priority_operator(Priority,Associativity,StringOperator,Operator)
% Infix:
my_arithmetic_operator(P,A,SOP,POP) :-
  my_infix_arithmetic(_,SOP,POP,_,_,P,A).
% Prefix:
my_arithmetic_operator(200,fy,"+",'+').
my_arithmetic_operator(200,fy,"-",'-').
my_arithmetic_operator(200,fy,"\\",'\\').

my_factor(T,Vi,Vo) -->
  "(",
  my_blanks_star,
  my_arithmetic_expression(1200,T,Vi,Vo),
  my_blanks_star,
  my_right_parenthesis,
  {!}. % WARNING: This whole clause is only for improving parsing performance
my_factor('$NULL'(ID),Vi,Vi) -->
  my_kw("NULL"),
  {nulls(on),
   get_null_id(ID)}.
my_factor(N,Vi,Vi) -->
  my_number(N),
  !.
my_factor(V,Vi,Vo) -->
  my_variable(V,Vi,Vo).
my_factor(C,Vi,Vi) -->
  my_arithmetic_constant(C).
my_factor(T,Vi,Vo) --> 
  {my_arithmetic_function(SF,F,A),
   A>0},
  my_string(SF), 
  my_blanks_star,
  "(",
  my_blanks_star,
  my_function_arguments(A,As,Vi,Vo),
  my_blanks_star,
  my_right_parenthesis,
  {T=..[F|As]}.
my_factor(F,Vi,Vi) --> 
  {my_arithmetic_function(SF,F,0)},
  my_string(SF).


my_function_arguments(1,[E],Vi,Vo) -->
  !,
  my_arithmeticexp(E,Vi,Vo).
my_function_arguments(A,[E|Es],Vi,Vo) -->
  {A>1},
  my_arithmeticexp(E,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  {A1 is A-1},
  my_function_arguments(A1,Es,Vo1,Vo).

  
%
% Numbers
%

% my_number(N) -->
%   push_syntax_error(['Incorrect number'],Old),
%   my_positive_or_negative_number(N),
%   pop_syntax_error(Old).
%   
% my_positive_or_negative_number(N) -->
%   my_negative_number(N).
% my_positive_or_negative_number(N) -->
%   my_positive_number(N).

my_number(N) -->
  my_negative_number(N).
my_number(N) -->
  my_positive_number(N).

my_negative_number(N) -->
  "-",
  push_syntax_error(['Invalid negative number'],Old),
  my_positive_number(P),
  {N is -(P)},
  pop_syntax_error(Old).

my_positive_number(N) -->
  my_fractional_positive_number(M),
  "E+",
  my_integer(E),
  {N is M*(10**E)}.
my_positive_number(N) -->
  my_fractional_positive_number(M),
  "E",
  my_integer(E),
  {N is M*(10**E)}.
my_positive_number(N) -->
  my_fractional_positive_number(N).
my_positive_number(N) -->
  my_positive_integer(N).

my_fractional_positive_number(N) -->
  my_digits(Is),
  ".",
  push_syntax_error(['Invalid fractional number'],Old),
  my_digits(Ds),
  {concat_lists([Is,".",Ds],Ns),
   number_codes(N,Ns)},
  pop_syntax_error(Old).

my_integer(N) -->
  my_negative_integer(N).
my_integer(N) -->
  my_positive_integer(N).

my_negative_integer(N) -->
  "-",
  my_positive_integer(P),
  {N is -(P)}.

my_positive_integer(N) -->
  my_digits(Ds),
  {name(N,Ds)}.

my_digits([D|Ds]) -->
  my_digit(D),
  push_syntax_error(['Invalid number'],Old),
  my_remaining_digits(Ds),
  pop_syntax_error(Old),
  !. % :::WARNING

my_remaining_digits([D|Ds]) -->
  my_digit(D),
  my_remaining_digits(Ds).
my_remaining_digits([]) -->
  [].
% my_digits([D|Ds]) -->
%   my_digit(D),
%  my_digits(Ds).
% my_digits([D]) -->
%   my_digit(D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Built-ins
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Built-in Arithmetic Constants

my_arithmetic_constant(Value) --> 
  {arithmetic_constant(Value,Constant,_), 
   atom_codes(Constant, SConstant)}, 
  my_string(SConstant).

arithmetic_constant(4*atan(1),'pi','Archimedes'' constant').
arithmetic_constant(exp(1),'e','Euler''s number').

% Built-in Arithmetic Functions

my_arithmetic_function(SF,PF,A) :- 
  arithmetic_function(F,PF,_,_,number(_N),A),
  atom_codes(F,SF).

my_aggregate_function(SF,PF,A) :- 
  arithmetic_function(F,PF,_,aggregate,_,A),
  atom_concat(_,distinct,PF),
  atom_codes(F,SF).
  
my_aggregate_function(F,A) :-
  arithmetic_function(F,_,_,aggregate,_,A).

% arithmetic_function(Name, PrologPredefined, Description, Kind(arithmetic or aggregate), Type, Arity)
arithmetic_function('sqrt','sqrt','Square root',arithmetic,number(float),1).
arithmetic_function('ln','log','Neperian logarithm',arithmetic,number(float),1).
arithmetic_function('log','log','Neperian logarithm',arithmetic,number(float),1).
arithmetic_function('log','log','Logarithm of the second argument in the base of the first one',arithmetic,number(float),2).
arithmetic_function('sin','sin','Sine',arithmetic,number(float),1).
arithmetic_function('cos','cos','Cosine',arithmetic,number(float),1).
arithmetic_function('tan','tan','Tangent',arithmetic,number(float),1).
arithmetic_function('cot','cot','Cotangent',arithmetic,number(float),1).
arithmetic_function('asin','asin','Arc sine',arithmetic,number(float),1).
arithmetic_function('acos','acos','Arc cosine',arithmetic,number(float),1).
arithmetic_function('atan','atan','Arc tangent',arithmetic,number(float),1).
arithmetic_function('acot','acot','Arc cotangent',arithmetic,number(float),1).
arithmetic_function('abs','abs','Absolute value',arithmetic,number(float),1).
arithmetic_function('float','float','Float value of its argument',arithmetic,number(float),1).
arithmetic_function('integer','integer','Closest integer between 0 and its argument',arithmetic,number(integer),1).
arithmetic_function('sign','sign','Returns -1 if its argument is negative, 0 otherwise',arithmetic,number(integer),1).
arithmetic_function('gcd','gcd','Greatest common divisor between two numbers',arithmetic,number(integer),2).
arithmetic_function('min','min','Least of two numbers',arithmetic,number(_Type),2).
arithmetic_function('max','max','Greatest of two numbers',arithmetic,number(_Type),2).
arithmetic_function('truncate','truncate','Closest integer between 0 and its argument',arithmetic,number(integer),1).
arithmetic_function('float_integer_part','float_integer_part','Integer part as a float',arithmetic,number(float),1).
arithmetic_function('float_fractional_part','float_fractional_part','Fractional part as a float',arithmetic,number(float),1).
arithmetic_function('round','round','Closest integer',arithmetic,number(integer),1).
arithmetic_function('floor','floor','Greatest integer less or equal to its argument',arithmetic,number(integer),1).
arithmetic_function('ceiling','ceiling','Least integer greater or equal to its argument',arithmetic,number(integer),1).
% aggregate functions
arithmetic_function('avg','avg','Average. Returns a float',aggregate,number(float),1).
arithmetic_function('avg_distinct','avg_distinct','Average of distinct values but nulls. Returns a float',aggregate,number(float),1).
arithmetic_function('count','count','Count all (with no argument). Returns an integer',aggregate,number(integer),0).
arithmetic_function('count','count','Count but nulls wrt. its argument. Returns an integer',aggregate,number(integer),1).
arithmetic_function('count_distinct','count_distinct','Count all distincts (with no argument) but nulls wrt. its argument. Returns an integer',aggregate,number(integer),0).
arithmetic_function('count_distinct','count_distinct','Count distincts but nulls wrt. its argument. Returns an integer',aggregate,number(integer),1).
arithmetic_function('max','max','Maximum. Returns a value with the same type as its argument',aggregate,_Type,1).
arithmetic_function('min','min','Minimum. Returns a value with the same type as its argument',aggregate,_Type,1).
arithmetic_function('sum','sum','Cumulative sum of values but nulls. Returns a value with the same type as its argument',aggregate,number(_N),1).
arithmetic_function('sum_distinct','sum_distinct','Cumulative sum of distinct values but nulls. Returns a value with the same type as its argument',aggregate,number(_N),1).
arithmetic_function('times','times','Cumulative product of values but nulls. Returns a value with the same type as its argument',aggregate,number(_N),1).
arithmetic_function('times_distinct','times_distinct','Cumulative product of distinct values. Returns a value with the same type as its argument',aggregate,number(_N),1).


% Built-in Infinite Predicates

my_infinite_builtin_pred(is/2).
my_infinite_builtin_pred(N/2) :-
  my_infix_comparison(N,_).

% Built-in Predicates

% my_builtin_pred(X) :- 
%   my_infix_relation(X,_)
%   ;
%   my_infix_comparison(X,_)
%   ;
%   my_outer_join_relation(X/_)
%   ;
%   my_aggregate_relation(X,_),
%   !
%   ;
%   X=top
%   ;
%   X=distinct
%   ;
%   X=group_by
%   ;
%   X=order_by.

% Built-in Functions

my_builtin_function((not)/1).
my_builtin_function(group_by/4).
my_builtin_function(count/3).
my_builtin_function(count/2).
my_builtin_function(sum/3).
my_builtin_function(avg/3).
my_builtin_function(min/3).
my_builtin_function(max/3).
%my_builtin_function(is_null/1).
%my_builtin_function(is_not_null/1).
%my_builtin_function(st/1).
% my_builtin_function(lj/1).
% my_builtin_function(rj/1).
% my_builtin_function(fj/1).
%my_builtin_function('$diff'/1).

% Built-in Operators

my_infix(X) :- 
  X==(':-'); 
  X==(':'); 
  X==(','); 
  X==(';'); 
  my_infix_relation(X,_); 
  my_infix_comparison(X,_); 
  my_infix_arithmetic(X,_,_,_,_,_,_).

% my_prefix(X) :- 
%   X==(':-').
  

% Built-in Unary Operators
% my_unary_operator(SOP,POP) :-
%   unary_operator(OP,POP,_),
%   atom_codes(OP,SOP).
  
unary_operator('\\','\\','Bitwise negation').
unary_operator('-','-','Negative value of its single argument').
unary_operator('+','+','Value of its single argument').

% % Built-in Binary Operators

% binary_operator('or','or','Inclusive disjunction').

% Built-in arithmetic expression evaluation operator
my_infix_relation('=>','Implication for hypothetical queries. Infix operator').
my_infix_relation('is','Evaluation of arithmetic expressions. Infix operator').
my_infix_relation('or','Metapredicate for disjunction tests. Succeeds only once. Infix operator').

% Built-in relations
% my_builtin_relation(Name, Arity, Description, Kind(null, outer_join, aggregate, or misc))
my_builtin_relation('is_null',1,'Determining whether its single argument is null', null).
my_builtin_relation('is_not_null',1,'Determining whether its single argument is not null', null).
my_builtin_relation('lj',3,'Left outer join: first relation, second relation, and join condition', outer_join).
my_builtin_relation('rj',3,'Right outer join: first relation, second relation, and join condition', outer_join).
my_builtin_relation('fj',3,'Full outer join: first relation, second relation, and join condition', outer_join).
my_builtin_relation('top',2,'Allows N solution tuples at most', misc).
my_builtin_relation('distinct',1,'Discard duplicates', misc).
my_builtin_relation('distinct',2,'Discard duplicates w.r.t. given variables', misc).
my_builtin_relation('exists',2,'Existential quantifier', misc).
my_builtin_relation('order_by',2,'Order answer w.r.t. given expressions', misc).
my_builtin_relation('group_by',3,'Create groups from a relation wrt. a list of variables, possibly applying aggregated conditions', aggregate).
my_builtin_relation('avg',3,'Aggregate returning the average of values for an argument in a relation, ignoring nulls', aggregate).
my_builtin_relation('avg_distinct',3,'Aggregate returning the average of different values for an argument in a relation, ignoring nulls', aggregate).
my_builtin_relation('count',3,'Aggregate returning the number of the tuples in a relation wrt. an argument, ignoring nulls', aggregate).
my_builtin_relation('count',2,'Aggregate returning the number of the tuples in a relation (cf. SQL''s COUNT(*))', aggregate).
my_builtin_relation('count_distinct',3,'Aggregate returning the number of tuples in a relation with different values for a given argument, ignoring nulls', aggregate).
my_builtin_relation('count_distinct',2,'Aggregate returning the number of different tuples in a relation', aggregate).
my_builtin_relation('max',3,'Aggregate returning the maximum of values for an argument in a relation, ignoring nulls', aggregate).
my_builtin_relation('min',3,'Aggregate returning the minimum of values for an argument in a relation, ignoring nulls', aggregate).
my_builtin_relation('sum',3,'Aggregate returning the sum of values for an argument in a relation, ignoring nulls', aggregate).
my_builtin_relation('sum_distinct',3,'Aggregate returning the sum of different values for an argument in a relation, ignoring nulls', aggregate).
my_builtin_relation('times',3,'Aggregate returning the product of values for an argument in a relation, ignoring nulls', aggregate).
my_builtin_relation('times_distinct',3,'Aggregate returning the product of different values for an argument in a relation, ignoring nulls', aggregate).
my_builtin_relation('dual',0,'Oracle''s dual table for building SELECT statements with no data source ', misc).
my_builtin_relation('select_not_null',3,'Select in the third argument the non-null value in the first and second arguments. mode(select_not_null(i,i,o))', misc).

% Built-in Binary Comparison Operators
% my_infix_comparison(Name, Description)
my_infix_comparison('=','Syntactic equality').
my_infix_comparison('\\=','Syntactic disequality').
my_infix_comparison('>','Greater than').
my_infix_comparison('>=','Greater or equal than').
my_infix_comparison('<','Less than').
my_infix_comparison('=<','Less or equal than').

my_infix_comparison(Op) -->
  {my_infix_comparison(Op,_), 
  atom_codes(Op,SOp)}, 
  my_string(SOp).


% Built-in Binary Arithmetic Operators
% my_infix_arithmetic(Name, StrName, PrologBuiltin, ReturnType, Description, Priority, Associativity)
% The priority of an operator in each priority group follows textual order of clauses (the first one has the higher priority, the last one has the lower priority)
my_infix_arithmetic('^',"^",'**',number(_),'Power',200,xfx).
my_infix_arithmetic('**',"**",'**',number(_),'Power',200,xfx).
my_infix_arithmetic('/\\',"/\\",'/\\',number(integer),'Bitwise conjuntion between integers',500,yfx).
my_infix_arithmetic('\\/',"\\/",'\\/',number(integer),'Bitwise disjunction between integers',500,yfx).
my_infix_arithmetic('*',"*",'*',number(_),'Multiplication',400,yfx).
my_infix_arithmetic('/',"/",'/',number(float),'Real division',400,yfx).
my_infix_arithmetic('//',"//",'//',number(integer),'Integer quotient',400,yfx).
my_infix_arithmetic('rem',"rem",'rem',number(integer),'Integer remainder',400,yfx).
my_infix_arithmetic('mod',"mod",'mod',number(integer),'Modulo',400,yfx).
my_infix_arithmetic('xor',"xor",'xor',number(integer),'Bitwise exclusive or between integers',500,yfx).
my_infix_arithmetic('+',"+",'+',number(_),'Addition',500,yfx).
my_infix_arithmetic('-',"-",'-',number(_),'Difference between its arguments',500,yfx).
my_infix_arithmetic('<<',"<<",'<<',number(integer),'Shift left the first argument the number of places indicated by the second one',400,yfx).
my_infix_arithmetic('>>',">>",'>>',number(integer),'Shift right the first argument the number of places indicated by the second one',400,yfx).

my_infix_arithmetic(F) :-
  my_infix_arithmetic(F,_,_,_,_,_,_).
  
% Built-in Predicates used for assertions
my_assertion_predicate(type,2).
my_assertion_predicate(nn,2).
my_assertion_predicate(pk,2).
my_assertion_predicate(ck,2).
my_assertion_predicate(fk,3).
my_assertion_predicate(fd,3).
my_assertion_predicate(persistent,2).
my_assertion_predicate(mode,2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Built-in Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Modes are set at the end of des_modes.pl

% :-mode(is_null(i))
is_null(T) :-
  \+ \+ T='$NULL'(_ID).

% :-mode(is_not_null(i))
is_not_null(T) :-
  T\='$NULL'(_ID).

% :-mode(select_not_null(i,i,o))
select_not_null(A1,A2,A1) :-
  is_not_null(A1),
  is_null(A2).
select_not_null(A1,A2,A2) :-
  is_null(A1), 
  is_not_null(A2).
select_not_null(A1,A2,A1) :-
  is_not_null(A1), 
  is_not_null(A2).


/*********************************************************************/
/* Solving Prolog Goals                                              */
/*********************************************************************/

solve_prolog(Goal,NVs) :- 
  solve_prolog_body(Goal,NVs), 
  write_with_NVs(Goal,NVs), 
  nl_log, 
  write_log('? (type ; for more solutions, <Intro> to continue) '), 
  user_input_string(Str),
  Str=="",
  !, 
  write_log_list([yes,nl]).
solve_prolog(_Goal,_R) :- 
  write_log_list([no,nl]).

solve_prolog_body(true,_) :- 
  !.
solve_prolog_body((LGs,RGs),R) :- 
  !, 
  solve_prolog_body(LGs,R), 
  solve_prolog_body(RGs,R).
solve_prolog_body((LGs;RGs),R) :- 
  !, 
  solve_prolog_body(LGs,R), 
  solve_prolog_body(RGs,R).
solve_prolog_body(G,R) :- 
  solve_prolog_goal(G,R).

solve_prolog_goal(not(G),R) :- % Negation
  !, 
  \+ (solve_prolog_body(G,R)).
solve_prolog_goal(group_by(A,Vs,C),R) :- % Group by
  !, 
  my_raise_exception(group_by(A,Vs,C),unsupported_in_Prolog,R).
solve_prolog_goal(top(A,B),R) :- % Top
  !, 
  my_raise_exception(top(A,B),unsupported_in_Prolog,R).
solve_prolog_goal(distinct(A),R) :- % Distinct
  !, 
  my_raise_exception(distinct(A),unsupported_in_Prolog,R).
solve_prolog_goal(distinct(A,B),R) :- % Distinct
  !, 
  my_raise_exception(distinct(A,B),unsupported_in_Prolog,R).
solve_prolog_goal(exists(A,B),R) :- % Distinct
  !, 
  my_raise_exception(exists(A,B),unsupported_in_Prolog,R).
solve_prolog_goal(order_by(A,B,C),R) :- % Order By
  !, 
  my_raise_exception(order_by(A,B,C),unsupported_in_Prolog,R).
solve_prolog_goal(Aggr,R) :- % Aggregates
  my_aggregate_relation(AF,Arity),
  functor(Aggr,AF,Arity),
  !, 
  my_raise_exception(Aggr,unsupported_in_Prolog,R).
solve_prolog_goal(G,_) :-      % Solves a goal using all of its matching rules
  (datalog((G:-B),NVs,RId,CId,Ls,FId,Rs); (datalog(G,NVs,RId,CId,Ls,FId,Rs),B=true)),
  R=datalog((G:-B),NVs,RId,CId,Ls,FId,Rs),
  solve_prolog_body(B,R).
solve_prolog_goal(G,_R) :-      % Prolog Built-ins
  G =.. [P|As],
  length(As,Ar),
  (my_builtin_pred(P/Ar)
   ->
   call(G)
   ;
   (user_predicates(Ps),
    (memberchk(P/Ar,Ps)
     ->
      fail
     ;
      my_raise_exception(P/Ar,undefined,[])
    )
   )
  ).

  
/*********************************************************************/
/* Computing answers to a query                                      */
/*********************************************************************/

% No compound goals: instead, use a view
get_answer(Query,Facts) :-
  get_answer(Query,[],Facts).
  
get_answer(Query,CId,Facts) :-
  save_et_st(ET,ST),
  check_ic(CheckStatus),
  set_flag(check,off),
% (Query='$p22'(_)-> deb ; true),
% WARNING: clear_et should not be needed
% clear_et,
  compute_datalog(Query,CId),
  set_flag(check,CheckStatus),
  (Query=':-'(H,_B)
   ->
    Fact=H
   ;
    Fact=Query
  ),
  findall(Fact,et(Fact,_,CId,_),Facts),
  restore_et_st(ET,ST).
%   restore_stratification(ST),
%   restore_et(ET).
%   clear_et,
%   compute_stratification.
  

/*********************************************************************/
/* Solving Datalog Queries                                           */
/*********************************************************************/

% Solve a Datalog query ensuring that the extension table will be  
% completelly filled BY considering all its dependent nodes in the PDG
% (non-recursive predicate optimization avoids some filling)
solve_datalog_query_complete_fill(Goal,NVs) :-
  push_flag(optimize_nrp,off,OldValue), % Disable optimization to completely fill extension table
  (OldValue==on
   ->
    query_predicate(Goal,N/A),
    pdg(G),
    sub_pdg(N/A,G,(NAs,_)),
    !,
    (member(NA,NAs),
     my_idx_retractall(complete_flag(NA,_,_,_)),
     fail
     ;
     true)
   ;
    true),
  solve_datalog_query(Goal,NVs,[],_Undefined),
  pop_flag(optimize_nrp,OldValue).
 
% Decide whether to apply the ordering specified in the predicate solving the query
%   off: let the predicate apply its ordering
%   on : apply the default ordering (this depends on /order_answer)
order_by_query(Query,off) :-
  %datalog((Query:-B),_,_,_,_,_,_),
  single_rule_datalog(Query,B),
  (B=order_by(_,_,_)
   ->
     true % For order_by in the answer rule
   ; 
     (B=top(_,G) ; B=distinct(G) ; B=G), % For selecting from ordered views
     single_rule_datalog(G,B2),
     B2=order_by(_,_,_)
   ),
  !.
order_by_query(_Query,Value) :-
  order_answer(Value).

single_rule_datalog(Query,B) :-
  datalog((Query:-B),_,RId1,_,_,_,_), 
  \+ (datalog((Query:-_),_,RId2,_,_,_,_),
      RId1\==RId2).
  
  
% Solve a Datalog query 
% % Disable order answer if the query contains an order_by predicate
% solve_datalog_query_at_system_prompt(Query,NVs,[],Undefined,off) :-
%   Query=..[answer|_],
%   datalog((Query:-B),_NVs,_RId,_CId,_Ls,_FId,_Rs),
%   B=order_by(_,_,_),
%   !,
% %   push_flag(order_answer,off,OldValue),
% %   catch(solve_datalog_query(Query,NVs,[],Undefined),M,(pop_flag(order_answer,OldValue),throw(M))),
% %   pop_flag(order_answer,OldValue).
%   solve_datalog_query(Query,NVs,[],Undefined).
% solve_datalog_query_at_system_prompt(Query,NVs,CId,Undefined,OrderBy) :-
%   order_answer(OrderBy),
%   solve_datalog_query(Query,NVs,CId,Undefined).
%   
solve_datalog_query(Query,NVs,CId,Undefined) :-
  write_info_verb_log(['Solving query ','$NVs'(Query,NVs),'...']),
  set_flag(computed_tuples,0),
  reset_statistics,
  strata(S),
  (S==[]
   ->
    solve_datalog_stratum(Query,1,CId,Undefined)  % No program was loaded; so, no strata computed
    ;
    (S==[non-stratifiable]
     -> 
      try_solve_stratified(Query,CId,Undefined) % Although in a non-stratifiable program, try to solve for the given query, hopefully finding a stratifiable subprogram
     ;
      solve_stratified(Query,CId,Undefined))),  % Stratifiable program: stratum solving
  (running_info(on),
   output(on)
   ->
    write('                                            \r') % Clear the running message about computed tuples (no dump to log, if enabled)
   ;
    true).

solve_datalog_stratum(not(Q),Stratum,CId,Undefined) :-
  solve_datalog_stratum(Q,Stratum,CId,_Undefined),
  !,
  solve_positive_datalog_stratum(not(Q),Stratum,CId,Undefined).
solve_datalog_stratum(Q,Stratum,CId,Undefined) :-
  solve_pos_res_datalog_stratum(Q,Stratum,CId,Undefined).
  
solve_pos_res_datalog_stratum(Query,Stratum,CId,Undefined) :-
%  query_goal(Query,Q),
  Query=Q,
  solve_positive_datalog_stratum(Q,Stratum,CId,Undefined),
  functor(Q,N,A),
  (restricted_predicate(N/A)
   ->
    solve_positive_datalog_stratum(-(Q),Stratum,CId,_Undefined2),
    remove_restricted_tuples(Q,CId)
   ;
    true).  
    
remove_restricted_tuples(Query,CId) :-
  et(Query,Ids,CId,It),
  et(-(Query),_,CId,_),
  my_idx_retract(et(Query,Ids,CId,It)),
  fail.
remove_restricted_tuples(_Query,_CId).

solve_positive_datalog_stratum(Query,Stratum,CId,_Undefined) :- 
  solve_star(Query,Stratum,CId). % This call is always made to fail
solve_positive_datalog_stratum(_Query,_Stratum,_CId,Undefined) :-
  remove_undefined(Undefined),
  ground_nulls,
  set_complete_flags.
  
set_complete_flags :-
  optimize_cc(off),
  !.
% set_complete_flags :-
%   optimize_cf(off),
%   called(_Hash,G,CId),
%   functor(G,N,A),
%   completeable_predicate(N/A),
% %  my_idx_retractall(complete_flag(N/A,G,no,CId)),
%   my_idx_retract(complete_flag(N/A,G,no,CId)),
%   my_idx_assertz(complete_flag(N/A,G,yes,CId)),
%   fail.
set_complete_flags :-
  optimize_cf(off),
  called(_,G,CId),
%  G\='-'(_),
  \+ \+ et(_Hash,G,_,CId,_),
  functor(G,N,A),
  completeable_predicate(N/A),
  (my_idx_retract(complete_flag(N/A,G,_,CId)) -> true ; true),
  my_idx_assertz(complete_flag(N/A,G,yes,CId)),
  fail.
set_complete_flags :-
  optimize_cf(on),
  my_idx_retract(complete_flag(P,G,no,CId)),
  my_idx_assertz(complete_flag(P,G,yes,CId)),
  fail.
set_complete_flags.

try_solve_stratified(Query,CId,Undefined) :-
  query_predicate(Query,N/A),
  pdg(G),
  current_tags(T),
  sub_pdg(N/A,G,SG),
  stratify(SG,NS,B), 
  !,
  (B==false
   -> 
   (write_notapi_warning_log(['Unable to ensure correctness/completeness for this query.']),
    solve_datalog_stratum(Query,1,CId,Undefined)) 
   ;
   (write_notapi_info_log(['Stratifiable subprogram found for the given query.']),
    strata(S),
    load_stratification(NS,SG,T),
    solve_stratified(Query,CId,Undefined),
    load_stratification(S,G,T))),
  set_flag(pdg,G).

% solve_stratified(+Query,CId,-Undefined)
% Solve a given query and return undefined facts (only present in a non-stratifiable database)
% et_not algorithm in [SD91] does not resort to strata computations
% solve_stratified(Query,CId,Undefined) :- 
%   neg(et_not),
%   !,
%   solve_datalog_stratum(Query,1,CId,Undefined).
% Solving by strata
solve_stratified(Query,CId,Undefined) :- 
  query_goal(Query,Q),
%   (Query=not(Q)
%   ;
%    Query=(_L=>Q)
%   ;
%    Query=Q
%   ),
  !,
  pdg(G),
  functor(Q,N,A),
  sub_pdg(N/A,G,(_Nodes,Arcs)),
  neg_dependencies(Arcs,ND),
  (ND=[]
   ->
    push_flag(optimize_cf,off,CF),
    solve_datalog_stratum(Query,1,CId,Undefined),
    pop_flag(optimize_cf,CF)
   ;
    strata(S),
    sort_by_strata(S,ND,SR),
    build_queries(SR,Queries,NVs),
    exec_if_verbose_on(
      write_log('Info: Computing by stratum: ['),
      write_csa_with_NVs(Queries,NVs),
      write_log_list(['].',nl])),
    solve_datalog_stratum_list(Query,CId,Undefined,Queries)
  ).

% solve stratified when solving implication
% top-N info must be passed
solve_stratified(G,NCId,T,Undefined) :-
  var(T),
  !,
  solve_stratified(G,NCId,Undefined).
solve_stratified(top(T,G),NCId,_T,Undefined) :-
  solve_stratified(top(T,G),NCId,Undefined),
  !.
solve_stratified(G,NCId,T,Undefined) :-
  solve_stratified(top(T,G),NCId,Undefined).


  
% query_goal(not(Q),Q) :-
%   !.
% query_goal('=>'(_L,Q),Q) :-
%   !.
query_goal(Q,G) :-
  my_metapredicate_term_goals(Q,[G]),
  !.
query_goal(Q,Q).


solve_datalog_stratum_list(Query,CId,Undefined,[Q|Qs]) :-
  get_stratum(Q,Stratum),
  solve_datalog_stratum(Q,Stratum,CId,_U),
  solve_datalog_stratum_list(Query,CId,Undefined,Qs).
solve_datalog_stratum_list(Query,CId,Undefined,[]) :-
  get_stratum(Query,Stratum),
  solve_datalog_stratum(Query,Stratum,CId,Undefined).

get_stratum(G,St) :-
  G =.. [F,SG],
  my_builtin_function(F/1),
  !,
  get_atom_stratum(SG,St).
get_stratum(G,St) :-
  get_atom_stratum(G,St).

get_atom_stratum(G,St) :-
  G =.. [P|Args],
  length(Args,A),
  strata(S),
  member((P/A,St),S),
  !.
  
neg_dependencies([],[]).
neg_dependencies([_T-F|As],[F|Fs]) :-
  neg_dependencies(As,Fs).
neg_dependencies([_T+_F|As],Fs) :-
  neg_dependencies(As,Fs).

sort_by_strata(S,R,SR) :-
  flip_pairs(S,FS),
  my_sort(FS,OFS),
  filterdrop(OFS,R,SR).

flip_pairs([],[]).
flip_pairs([(P,S)|Xs],[(S,P)|Ys]) :-
  flip_pairs(Xs,Ys).

filterdrop([],_,[]).
filterdrop([(_S,P)|Xs],R,[P|Ps]) :-
  member(P,R),
  !,
  filterdrop(Xs,R,Ps).
filterdrop([(_S,_P)|Xs],R,Ps) :-
  filterdrop(Xs,R,Ps).

build_queries([],[],[]).
build_queries([N/A|Ps],[Q|Qs],NVs) :-
  length(L,A),
  Q =.. [N|L],
  assign_variable_names(L,[],NV1s),
  build_queries(Ps,Qs,NV2s),
  append(NV1s,NV2s,NVs).
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fixpoint Computation: solve_star
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A call to solve_star is always made to fail
% First clause deals with completed computations: Do nothing
solve_star(G,_St,CId) :-
  optimize_cc(on),
  functor(G,N,A),
  functor(CG,N,A),
  complete_flag(N/A,CG,yes,CId),
  my_subsumes_chk(CG,G),
  !,
  fail.
% Second clause deals with extensional predicates: Only one iteration for linear fetching
solve_star(G,St,CId) :-
  optimize_ep(on),
  functor(G,N,A),
  extensional_predicate(N/A),
  !,
  set_p_flag(first_iter(CId),true),
  set_flag(fp_iterations,1),
  copy_term(G,CG),
  void_dlrule(R),
  solve_all_goal(G,CG,St,CId,1,R,_IdG),
  my_idx_retractall(complete_flag(N/A,G,_,CId)),
  my_idx_assertz(complete_flag(N/A,G,yes,CId)),
  !,
  fail.
% Third clause for non-recursive completeable predicates
solve_star(G,St,CId) :-
  optimize_nrp(on),
%   optimize_st(StSwitch),
%   StSwitch\==off,
  functor(G,N,A),
  nr_nd_predicate(N/A),
  % completeable_predicate(N/A),
  \+strata([non-stratifiable]),
  !,
  set_p_flag(first_iter(CId),true),
  set_flag(fp_iterations,1),
  copy_term(G,CG),
  void_dlrule(R),
  solve_all_goal(G,CG,St,CId,1,R,_IdG),
  my_idx_retractall(complete_flag(N/A,G,_,CId)),
  my_idx_assertz(complete_flag(N/A,G,yes,CId)),
  !,
  fail.
% Fourth clause deals with the general case: Fixpoint computation
solve_star(Q,St,CId) :-
  set_p_flag(first_iter(CId),true),
  set_flag(fp_iterations,0),
  repeat,
  (
   remove_calls(CId),
   inc_flag(fp_iterations),
   flag_et_no_change, % Set a flag indicating that the extension table has not changed 
   retractall(computed_tuples(_,_,_,_,_,_)), % Reset number of computed tuples for each call
   solve(Q,St,CId),   % Solve the call to Q using memoization at stratum St
   fail               % Request all alternatives
  ;              
   set_p_flag(first_iter(CId),false),
   display_fp_info,
   et_not_changed,    % When no more alternatives, restart the computation if the extension table has changed,
   !,                 % otherwise, 
   fail               % fail and exit
  ).             
                      
display_fp_info :-
  fp_info(off),
  !.
display_fp_info :-
  fp_iterations(It),
  write_info_log(['Fixpoint iteration ',It,':']),
  findall(et(G,Ids,CId,It),et(G,Ids,CId,It),Es),
  display_bag(Es),
  display_nbr_of_tuples(Es,computed,_Error1),
  findall(computed_tuples(G,GId,RId,CId,T,N),computed_tuples(G,GId,RId,CId,T,N),Cs),
  display_bag(Cs),
  display_nbr_of_tuples(Cs,computed,_Error2).
  
% Building a Predicate with Fresh Variables and Universal Nulls
build(Q,G) :-
  nulls(off),
  !,
  copy_term(Q,G).
build(Q,G) :-
  copy_term(Q,FQ),
  abstract_nulls(FQ,G).
  
% Get the variables of a term
% my_term_variables(T,Vs):-
%   term_variables(T,Vs).

/*********************************************************************/
/* Removing previous calls on a new run of fixpoint computation      */
/*********************************************************************/

% remove_calls :-
%   my_idx_retractall(called(_X,_CId)).
remove_calls(CId) :-
  my_idx_retractall(called(_X,CId)).

/*********************************************************************/
/* Testing whether the extension table has not changed               */
/*********************************************************************/

et_not_changed :-
  et_flag(no).

/*********************************************************************/
% Setting Extension Table Flag to 'changed'
/*********************************************************************/

flag_et_change(C) :-
  C==no,
  !.
flag_et_change(_C) :-
  set_flag(et_flag,yes).

/*********************************************************************/
% Setting Extension Table Flag to 'not changed'
/*********************************************************************/

flag_et_no_change :-
  set_flag(et_flag,no).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Solving with Extension Table: solve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% solve(+Goal,+Stratum,+CompId,+GId,-GIdo,+Rule)
%
% Solve Goal, which corresponds to Stratum in the context of Rule,
% either keeping duplicates (Distinct=all) or not (Distinct=distinct).

solve(G,St,CId) :-
  solve(G,St,CId,datalog((answer:-G),[],-1,[],[],-1,[])). % Solving a top-level query

solve(G,St,CId,R) :-
  solve(G,St,CId,1,_GId,R,_N,r,_Ids).

% solve(+Goal,+Stratum,+CompId,+GoalId,GoalIdo,+Rule,+Rec/NonRec,-Ids)
%
% As solve/3,
% where Ids is the chain of identifiers used to solve Goal
%

solve(true,_St,_CId,GId,GIdo,_R,_N,_NR,[]) :-
  !,
  GIdo is GId+1.
% solve((G1,(G2,Gs)),St,CId,R,[]) :-
%   % Duplicates OFF
%   % Do not store Id chain in et
%   duplicates(off),
%   !, 
%   solve(G1,St,CId,R,_Ids1), 
%   solve(G2,St,CId,R,_Ids3), 
% %  ((G1=city(amsterdam_centraal), G2=city(foo)) -> trace ; true),
% %  ((G1=city(amsterdam_centraal)) -> trace ; true),
%   ((G2=city(foo),G1\=city(foo)) -> trace ; true),
%   solve(Gs,St,CId,R,_Ids2).
solve((G,Gs),St,CId,GId,GIdo,R,N,NR,[]) :-
  % Duplicates OFF
  % Do not store Id chain in et
  duplicates(off),
  !, 
  solve(G,St,CId,GId,GId1,R,N,NR,_Ids1), 
  solve(Gs,St,CId,GId1,GIdo,R,N,NR,_Ids2).
solve((G,Gs),St,CId,GId,GIdo,R,N,NR,Ids) :-
  % Duplicates ON
  % Store Id chain in et
%  duplicates(on),
  !, 
  solve(G,St,CId,GId,GId1,R,N,NR,Ids1), 
  solve(Gs,St,CId,GId1,GIdo,R,N,NR,Ids2),
  append(Ids1,Ids2,Ids).
solve(st(G),St,CId,GId,GIdo,R,N,NR,Ids) :-  % Stratum increase metapredicate for outer joins
  !,
  nulls(on),
  solve(G,St,CId,GId,GIdo,R,N,NR,Ids).
solve(or(LA,RA),St,CId,GId,GIdo,R,N,NR,Ids) :-  % Stratum increase metapredicate for outer joins
  !,
  once((solve(LA,St,CId,GId,GIdo,R,N,NR,Ids);solve(RA,St,CId,GId,GIdo,R,N,NR,Ids))).
solve(call(G),St,CId,GId,GIdo,R,N,NR,Ids) :-  % Metapredicate 'call'
  !,
  solve(G,St,CId,GId,GIdo,R,N,NR,Ids).
% solve(rj(G),St,CId,GId,GIdo,R,N,NR,Ids) :-  % Right outer join; simply execute the goal
%   !,
%   nulls(on),
%   solve(G,St,CId,GId,GIdo,R,N,NR,Ids).
% solve(fj(G),St,CId,GId,GIdo,R,N,NR,Ids) :-  % Full outer join; simply execute the goal
%   !,
%   nulls(on),
%   solve(G,St,CId,GId,GIdo,R,N,NR,Ids).
% solve(A is B,_St,_R,(-1,[])) :- % Provide an unique id for the evaluation of a given expression containing concrete nulls
%   contain_null(B),
%   my_ground(B),
%   !,
%   A='$NULL'(Id),
%   get_null_id(Id).
solve(top(N,G),St,CId,GId,GIdo,R,_N,NR,[Id]) :-  % Meta predicate top/2
  !,
  (var(N) -> my_raise_exception(top(N,G),instantiation,'First argument must be ground') ; true),
  (N<1 -> my_raise_exception(generic,syntax(['First argument of top must be greater than 0: ',top(N,G)]),[]) ; true),
  dlrule_id(R,RId),
  retractall(computed_tuples(_,GId,RId,CId,_,_)), % Reset number of computed tuples for each call
  memo(G,St,CId,GId,R,all,N,NR,Id),
  computed_tuples(_,GId,RId,CId,T,N1),
  N1>0,
  (N1>T
   ->
    (computed_tuples(G,_GId2,_RId2,CId,T2,N2),
%      GId2\==GId, 
%      RId2\==RId, 
     (var(T2) ; N2<T2)
     ->
      fail  % This top(N,G) is completely computed, but there is another call to G that not
     ;
      !,
      fail) % This top(N,G) is completely computed, and there is no other call to G that not
   ;
    true),
  GIdo is GId+1.
solve(distinct(G),St,CId,GId,GIdo,R,N,NR,[Id]) :-  % Meta predicate distinct/1
  !,
  memo(G,St,CId,GId,R,distinct,N,NR,Id),
  GIdo is GId+1.
solve(distinct(Vs,G),St,CId,GId,GIdo,R,N,NR,[Id]) :-  % Meta predicate distinct/2
  !,
  G=..[_|Args],
  get_arg_position_list(Vs,Args,Ps),
  copy_term(G,FG),               % Free existential variables, i.e., 
  get_ith_arg_list(Ps,G,DArgs),  % not in Vs
  get_ith_arg_list(Ps,FG,DArgs), % not in Vs
  memo(FG,St,CId,GId,R,distinct((Vs,Ps)),N,NR,Id),
  GIdo is GId+1.
solve((A => C),St,CId,GId,GIdo,R,N,NR,IdG) :-
  !,
  solve_implication((A => C),St,CId,GId,GIdo,R,N,NR,IdG).
solve(G,_St,_CId,GId,GIdo,R,_N,_NR,[]) :-
  is_primitive(G),
  \+ ((G=is(_A,B), contain_null(B), my_ground(B))),
  !,
  compute_primitive(G,R),
  GIdo is GId+1.
solve(G,St,CId,GId,GIdo,R,N,NR,[Id]) :-           
  memo(G,St,CId,GId,R,all,N,NR,Id),
  GIdo is GId+1.
% solve(G,St,CId,GId,GIdo,R,N,NR,Ids) :-  
%   G=..[Name|_],
%   my_foreign_key('$des',Name,_,_,_),
%   !,
%   findall(my_foreign_key('$des',Name,FK_AttNames,ForeignTablename,PK_AttNames),my_foreign_key('$des',Name,FK_AttNames,ForeignTablename,PK_AttNames),FKs),
%   build_FK_goal_list(FKs,Name,G,GL),
%   my_list_to_tuple(GL,Gs),
%   solve(Gs,St,CId,GId,GId1,R,N,NR,Ids1),
%   solve_memo(G,St,CId,GId1,GIdo,R,N,NR,Ids2), 
%   append(Ids1,Ids2,Ids).
% solve(G,St,CId,GId,GIdo,R,N,NR,Ids) :-  % Stratum increase metapredicate for outer joins
%   solve_memo(G,St,CId,GId,GIdo,R,N,NR,Ids).
%  
% solve_memo(G,St,CId,GId,GIdo,R,N,NR,[Id]) :-           
%   memo(G,St,CId,GId,R,all,N,NR,Id),
%   GIdo is GId+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Memoization: memo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Already called. Extension table with an entry for the current call
% memo(+Goal,+Stratum,+CompId,+GoalId,+Rule,+Distinct,+TopN,+r/nrd/d,-Id)
memo(G,_St,CId,GId,R,D,T,r,IdG) :-
  copy_term(G,CG),
  build(G,Q),           % Build in Q the same call with fresh variables
  called(Q,CId),  % Try to find a unifiable call in the extension table for the current call
  my_subsumes(Q,G),     % Test whether the extension table call subsumes the current call
  dlrule_id(R,RId),
%  complete_comp(Q,GId,RId,CId,T),
  !,                    % If so,
  et_lookup(GId,RId,CId,G,CG,D,T,IdG),   % use the result in the extension table; otherwise, process the new call with the next memo clause
  inc_goal_computed_tuples(CG,GId,RId,CId,T).
% New call. Extension table without an entry for the current call
memo(G,St,CId,GId,R,D,T,r,IdG) :-
  my_idx_assertz(called(G,CId)),   % Assert the current call in the extension table (because: (1) there is no previous call to G, or (2) G is not subsumed by a previous call to G
  copy_term(G,CG),
  set_complete_flag(G,D,T,CF,CId),
  dlrule_id(R,RId),
  (
   et_lookup(GId,RId,CId,G,CG,D,T,IdG), % The first call have to return all the possible answers computed in a previous pass of the fixpoint computation
% nl,fp_iterations(I),write(I),write(':'),write(G),nl,
   inc_goal_computed_tuples(CG,GId,RId,CId,T) 
  ;
   CF == yes,          % Don't try to recompute a completed computation (in a previous stratum)
   !,
   fail
  ;
   solve_goal(G,St,CId,GId,R,T,r,IdG,C), % Solve the current call using its matching rules
   build(G,Q),         % Build in Q the same call with fresh variables
   no_subsumed_by_et(GId,RId,CId,Q,D,(G,IdG)), % Test whether there is no entry in the extension table subsuming the current result
   \+ restricted_in_et(CId,Q),
%   dlrule_id(R,RId),
   et_assert(GId,RId,CId,G,CG,T,IdG,C)  % Assert the new result
  ).
memo(G,_St,CId,GId,R,D,T,nrd,IdG) :- % This clause (and the next one) is only used by solve_all_goal for non-recursive computations, duplicates and no nulls
  completed_goal(G,CId),
  copy_term(G,CG),
  dlrule_id(R,RId),
  !,
  et_lookup(GId,RId,CId,G,CG,D,T,IdG),
  inc_goal_computed_tuples(CG,GId,RId,CId,T).
memo(G,St,CId,GId,R,_D,T,nrd,IdG) :- % This clause is only used by solve_all_goal for non-recursive computations, duplicates and no nulls
  solve_goal(G,St,CId,GId,R,T,nrd,IdG,_C). % Solve the current call using its matching rules

% % Classical behaviour (non-top computation):
% complete_comp(_G,_GId,_RId,_CId,T) :-
%   var(T),
%   !.
% % complete_comp(top(T,G),GId,RId,CId,_N) :-
% %   computed_tuples(G,GId,RId,CId,M),
% %   M>=T,
% %   !.
% complete_comp(G,GId,RId,CId,T) :-
%   computed_tuples(G,GId,RId,CId,M),
%   M>=T,
%   !.
% complete_comp(G,_GId,_RId,CId,_N) :-
%   my_idx_retractall(called(G,CId)),
%   !,
%   fail.
  
et_assert_if_not_subsumed(GId,RId,CId,G,IdG,CG,D,T,C) :-
  build(G,Q),         % Build in Q the same call with fresh variables
  (no_subsumed_by_et(GId,RId,CId,Q,D,(G,IdG)), % Test whether there is no entry in the extension table subsuming the current result
   \+ restricted_in_et(CId,Q)
   ->
    et_assert(GId,RId,CId,G,CG,T,IdG,C)  % Assert the new result
   ;
    true).

my_nf_retract(X) :-
  (retract(X) -> true ; true).

my_idx_nf_retract(X) :-
  (my_idx_retract(X) -> true ; true).

my_idx_retract(complete_flag(P,F,IdF,FCId)) :-
  my_term_hash(P,Hash),
  retract(complete_flag(Hash,P,F,IdF,FCId)).  
my_idx_retract(et(F,IdF,ECId,It)) :-
  my_term_hash(F,Hash),
  retract(et(Hash,F,IdF,ECId,It)).  
my_idx_retract(called(F,CCId)) :-
  my_term_hash(F,Hash),
  retract(called(Hash,F,CCId)).  
  
my_idx_retractall(complete_flag(P,F,CF,FCId)) :-
  retractall(complete_flag(_Hash,P,F,CF,FCId)).  
my_idx_retractall(et(F,IdF,ECId,It)) :-
  retractall(et(_Hash,F,IdF,ECId,It)).  
my_idx_retractall(called(F,CCId)) :-
  retractall(called(_Hash,F,CCId)).  
  
my_idx_assertz(complete_flag(P,F,CF,CId)) :-
  (indexing(on) -> my_term_hash(P,Hash) ; true),
  assertz(complete_flag(Hash,P,F,CF,CId)).  
my_idx_assertz(et(F,IdF,CId,It)) :-
  (indexing(on) -> my_term_hash(F,Hash) ; true),
  assertz(et(Hash,F,IdF,CId,It)).  
my_idx_assertz(called(F,CId)) :-
  (indexing(on) -> my_term_hash(F,Hash) ; true),
  assertz(called(Hash,F,CId)).  
  
complete_flag(P,F,CF,CId) :-
  (indexing(on) -> my_term_hash(P,Hash) ; true),
  inc_statistics_flag(cf_lookups),
  complete_flag(Hash,P,F,CF,CId).

% Extension table predicates
  
et(F,IdF) :-
  et(F,IdF,_It).

et(F,IdF,It) :-
  et(F,IdF,[],It). % First computation level

et(F,IdF,CId,It) :-
  (indexing(on) -> my_term_hash(F,Hash) ; true),
  inc_statistics_flag(et_lookups),
  et(Hash,F,IdF,CId,It),
  inc_statistics_flag(et_retrievals).
  
% et_all_levels(F,IdF,CId) :-
%   et_one_level(F,IdF,CId).

% et_all_levels(F,IdF,Level) :-
%   et_one_level(F,IdF,Level)
%   ;
%   Level=[_Id|CIds],
%   nonvar(CIds),
%   CIds\==[],
%   et_all_levels(F,IdF,CIds).
%   
% et_one_level(F,IdF,CId) :-
%   et(F,IdF,CId,_It).

% called(F) :-
%   called_one_level(F,[]). % First computation level

% called(F,[Id|CIds]) :-
%   called_one_level(F,[Id|CIds])
%   ;
%   nonvar(CIds),
%   CIds\==[],
%   called(F,CIds).
  
%called_one_level(F,CId) :-
called(F,CId) :-
  (indexing(on) -> my_term_hash(F,Hash) ; true),
  inc_statistics_flag(ct_lookups),
  called(Hash,F,CId).
  
restricted_in_et(CId,Q) :-
  functor(Q,N,A),
  restricted_predicate(N/A),
  et(-(Q),_IdG,CId,_It).
  
  
% Tests whether there is not an entry in the extension table subsuming the current result
% If duplicates are enabled, identifier chains has to be used to distinguish data sources
% no_subsumed_by_et(+CompId,+Query,+Distinct,+(Goal,IdGoal))
no_subsumed_by_et(GId,RId,CId,Q,D,(G,_IdG)) :-
  % Duplicates OFF or DISTINCT
  (duplicates(off) ; D=distinct),
  !,
  \+ ((et_lookup(GId,RId,CId,Q,_CQ,all,_N,_IdQ),
       abstract_nulls(Q,AQ),
       my_subsumes(AQ,G))).
no_subsumed_by_et(GId,RId,CId,Q,all,(_G,_IdG)) :-
  % Duplicates ON
%  duplicates(on),
  % No entry matching Q; so, not subsumed
  \+ (et_lookup(GId,RId,CId,Q,_CQ,all,_N,_IdQ)),
  !.
no_subsumed_by_et(GId,RId,CId,Q,all,(G,IdG)) :-
%  duplicates(on),
  !,
  % If there are existing entries matching Q, they do not subsume G
  % G must have a non recursive chain of ids
  nr_id(IdG),
  \+ ((et_lookup(GId,RId,CId,Q,_CQ,all,_N,IdQ),
          my_subsumes((Q,IdQ),(G,IdG)))).
no_subsumed_by_et(_GId,_RId,CId,Q,distinct((_Vs,Ps)),(G,_IdG)) :-
  % Duplicates ON and DISTINCT/2
%  duplicates(on),
  !,
  \+ ((
       functor(Q,F,A),
       functor(FQ,F,A),
       get_ith_arg_list(Ps,Q,QAs),
       get_ith_arg_list(Ps,FQ,QAs),
       et(FQ,_Id,CId,_It), 
       get_ith_arg_list(Ps,G,GAs),
       abstract_nulls(QAs,AQAs),
       my_subsumes(AQAs,GAs)
     )).

% et_lookup(+GoalId,+RuleId,+CompId,+G,+UnsolvedG,+Distinct,?T,-Id)
et_lookup(GId,RId,CId,G,CG,D,T,IdG) :-
  et_lookup(GId,RId,CId,_It,G,CG,D,T,IdG).

% et_lookup(+CompId,?It,+G,+UnsolvedG,+Distinct,?T,-Id)
% et_lookup(_GId,_RId,CId,_It,_G,CG,_D,T,_IdG) :-
%   nonvar(T),
%   \+ (computed_tuples(CG,_,_,CId,T,N1),
%       T=<N1),
%   !,
%   fail.
et_lookup(GId,RId,CId,It,G,CG,all,T,IdG) :-
  et(G,IdG,CId,It),
  more_tuples_needed(CG,GId,RId,CId,T).
et_lookup(GId,RId,CId,It,G,CG,distinct,T,IdG) :-
  more_tuples_needed(CG,GId,RId,CId,T),
  findall((AG,IdG),(et(G,IdG,CId,It),abstract_nulls(G,AG)),GIdGs),
  setof(G,IdG^member((G,IdG),GIdGs),Gs),
  member(G,Gs),
  once((member((G,IdG),GIdGs),et(G,IdG,CId,It))). % Retrieve only one representative for nulls
et_lookup(GId,RId,CId,It,G,CG,distinct((Vs,_SG)),T,IdG) :-
  more_tuples_needed(CG,GId,RId,CId,T),
%  findall((G,IdG),et(G,IdG,CId,It),GIdGs),
  findall((AG,IdG),(et(G,IdG,CId,It),abstract_nulls(G,AG)),GIdGs),
  term_variables(G,GVs),
  my_set_diff(GVs,Vs,EVs),
  build_ex_quantifier(EVs,member((G,IdG),GIdGs),QG),
  QQG=IdG^QG, % Ciao needs this!
  setof(Vs,QQG,Vss),
  member(Vs,Vss),
  once((member((G,IdG),GIdGs),et(G,IdG,CId,It))).  % Retrieve only one representative for nulls

more_tuples_needed(_G,_GId,_RId,_CId,T) :-
  var(T), % No limits
  !.
more_tuples_needed(G,_GId,_RId,_CId,_T) :-
  (computed_tuples(G,_,_,_,T1,N),
   N>0,
   N<T1,
   !
  ;
   true
  ). % Limit is not yet reached
%  !.
%   (computed_tuples(_,GId,RId,CId,T,N)
%    ->
%     true
%    ;
%     N=0),
%   N<T. % Limit is not yet reached

% Asserts the input fact whenever it does not contain any variable;
% though, it may contain null
% et_assert(G) :-
%   ((my_no_contains_vars(G)
%     ;
%     functor(G,group_by,3)
%     ;
%     (functor(G,F,A), my_aggregate_relation(F,A))
%     ;
%     % Negated, non-ground facts are allowed to be asserted for outer join computations
%    (G=not(NG), functor(NG,NGF,_), [Dolar]="$", name(NGF,[Dolar|_])))
%    ->
%     assertz(et(G))
%    ;
%     my_raise_exception(G,instantiation,'Extension table')).   % Asserts the new result
% If nonground facts are not allowed, some sql translations cannot be computed as:
% select a from s where b not in ((select a from t where t.a=s.a) union (select a from t where b=1))
% in des.ini

% ::: WARNING: REMOVE CG
% et_assert(+GoalId,+RuleId,+CompId,+Fact,+OriginalCall,?TupleLimit,+FactId,+ETChange)
% et_assert(GId,RId,CId,_G,_CG,N,_IdG,_C) :-
%   nonvar(N),
%   computed_tuples(_,GId,RId,CId,T),
%   T>=N,
%   !,
%   fail.
et_assert(GId,RId,CId,G,CG,T,IdG,C) :-
  fp_iterations(It),
  my_idx_assertz(et(G,IdG,CId,It)),
  flag_et_change(C),      % Set a flag indicating that the extension table has changed
  inc_goal_computed_tuples(CG,GId,RId,CId,T),
  display_running_nbr_computed_tuples.
  
inc_goal_computed_tuples(G,GId,RId,CId,T) :-
  var(T),
  !,
  (computed_tuples(G,GId,RId,CId,_,0)
   ->
    true
   ;
    assert(computed_tuples(G,GId,RId,CId,_,0))).
inc_goal_computed_tuples(G,GId,RId,CId,T) :-
   (retract(computed_tuples(G,GId,RId,CId,T,N))
    ->
     true
    ;
     N=0),
  N1 is N+1,
  assert(computed_tuples(G,GId,RId,CId,T,N1)).

display_running_nbr_computed_tuples :-
  (
   running_info(off)
   ;
   \+ output(on)
   ;
   batch(_,_,_,_)
   ;
   tapi(on)
  ),
  !.  
display_running_nbr_computed_tuples :-
  inc_computed_tuples(N),
  (N == 1 -> R = tuple ; R = tuples),
%  store_elapsed_time(computation,Time),
%  format_timing(Time,FTime),
%  write_info_log(['',N,' ',R,' computed. Elapsed time: ',FTime,'\r','$tbc']),
%  write_info_log(['',N,' ',R,' computed.','\r','$tbc']),
  write_list(['Info: ',N,' ',R,' computed.','\r']),
  flush_output.

inc_computed_tuples(T1) :-
  computed_tuples(T),
  T1 is T+1,
  set_flag(computed_tuples,T1).

% Set complete flag. Distinct and top_n computations are never assumed to be 
% completed since they 
% only ought to compute a subset of the meaning of the involved relation
% set_complete_flag(G,_D,_N,CF,CId) :-
%   functor(G,Name,Arity),
%   (complete_flag(Name/Arity,CF,CId) -> true ; CF=no, my_idx_assertz(complete_flag(Name/Arity,CF,CId))),
%   !.

set_complete_flag(_G,_D,_N,_CF,_CId) :-
  optimize_cc(off),
  !.
set_complete_flag(G,D,N,CF,CId) :-
  optimize_cf(off),
  !,
  (complete_flag(_,_,_,_,_)  % If there is a single entry in complete_flag, look for a match of a previous top-level query
   ->
    (completed_goal(G,CId)
     ->
      CF=yes
     ;
      functor(G,Name,Arity),
      % A computation under a distinct operator is not completely computed
      (((D=distinct;D=distinct(_);nonvar(N))
       ;
      % Infinite built-in predicates can not be completely computed 
       (my_infinite_builtin_pred(Name/Arity)))
        -> 
         true
        ;
        (CF=no,
%          (complete_flag(Name/Arity,G,CF,CId) % WARNING: Implicit cut here avoids scanning of multiple alternatives, posibly pruning solutions
%           ->
%            true
%           ;
%            my_idx_assertz(complete_flag(Name/Arity,G,CF,CId)))
         (complete_flag(Name/Arity,G,CF,CId) 
          ;
          my_idx_assertz(complete_flag(Name/Arity,G,CF,CId)))
        )
      )
    )
   ;
    true).
set_complete_flag(G,D,N,CF,CId) :-
  functor(G,Name,Arity),
  (
    functor(SG,Name,Arity),
    complete_flag(Name/Arity,SG,CF,CId), 
    my_subsumes(SG,G)
   -> 
    true 
   ;
    % A computation under a distinct operator is not completely computed
    (((D=distinct;D=distinct(_);nonvar(N))
     ;
    % Infinite built-in predicates can not be completely computed 
     (my_infinite_builtin_pred(Name/Arity)))
      -> 
       true
      ;
       (CF=no,
        my_idx_assertz(complete_flag(Name/Arity,G,CF,CId))))
  ).

completed_goal(G,CId) :-
  functor(G,Name,Arity),
  copy_term(G,CG),
  complete_flag(Name/Arity,CG,yes,CId), 
  my_subsumes(CG,G).
  
% Non recursive chain of ids
% nr_id((ID,T)) :-
%   \+ (my_member_var_term(ID,T)),
%   !.
nr_id((ID,PairList)) :-
  id_not_in(ID,PairList).
  
id_not_in(_ID,[]) :-
  !.
id_not_in(ID,[(ID,_PairList1)|_PairList]) :-
  !,
  fail.
id_not_in(ID,[(_ID1,PairList1)|PairList]) :-
  id_not_in(ID,PairList1),
  id_not_in(ID,PairList).

% solve_all_goal(G,CG,St,CId,R,IdG) :-
%   solve_goal(G,St,CId,R,nr,IdG,_),
%   inc_goal_computed_tuples(N,CG,CId), 
% %  et_assert(CId,G,CG,N,IdG,no),
%   et_assert_if_not_subsumed(CId,G,IdG,CG,all,N,no),
%   remove_calls(CId),
%   fail.
solve_all_goal(G,CG,St,CId,GId,R,IdG) :-
  duplicates(on),
  nulls(off),
  !,
  fast_solve_all_goal(G,CG,St,CId,GId,R,IdG).
solve_all_goal(G,CG,St,CId,GId,R,IdG) :-
  % duplicates off or nulls on
  solve_goal(G,St,CId,GId,R,_N,nrd,IdG,_),
  dlrule_id(R,RId),
  inc_goal_computed_tuples(CG,GId,RId,CId,N), 
%  et_assert(CId,G,CG,N,IdG,no),
  et_assert_if_not_subsumed(GId,RId,CId,G,IdG,CG,all,N,no),
  remove_calls(CId),
  fail.
solve_all_goal(_G,_CG,_St,_CId,_GId,_R,_IdG).

fast_solve_all_goal(G,CG,St,CId,GId,R,IdG) :-
  solve_goal(G,St,CId,GId,R,_N,nrd,IdG,_),
  dlrule_id(R,RId),
  inc_goal_computed_tuples(CG,GId,RId,CId,N),
  et_assert(GId,RId,CId,G,CG,N,IdG,no),
  fail.
fast_solve_all_goal(_G,_CG,_St,_CId,_GId,_R,_IdG).
  
% Solving a Goal: solve_goal. Goals are cached
% C: ET changed (last argument) set to 'n' implies do not reiterate fp because of this goal
solve_goal(not(G),St,CId,GId,R,N,NR,(-1,[]),_C) :- % Negation; follows the et mechanism
  !, 
  solve_not(G,St,CId,GId,R,N,NR).
% solve_goal(top(N,G),St,CId,GId,R,_N,NR,(-1,[]),_C) :- % Negation; follows the et mechanism
%   !, 
%   (var(N) -> my_raise_exception(top(N,G),instantiation,'First argument must be ground') ; true),
%   (N<1 -> my_raise_exception(generic,syntax(['First argument of top must be greater than 0: ',top(N,G)]),[]) ; true),
%   dlrule_id(R,RId),
%   retractall(computed_tuples(_,GId,RId,CId,_,_)), % Reset number of computed tuples for each call
%   solve(G,St,CId,GId,_,R,N,NR,_Ids),
%   computed_tuples(_,GId,RId,CId,T,N1),
%   (N1>T -> !, fail ; true).
% solve_goal(G,_St,R,(-1,[])) :-     
%   compute_primitive(G,R).
% The only one primitive to be computed here with memoization is 'A is B' 
% where B is ground and contains at least a null value
% ::: WARNING. Memorize 'is'?
solve_goal(A is B,_St,_CId,GId,N,R,_NR,IdG,_C) :-
  my_ground(B), 
  contain_null(B),
  !,
  (dlrule_id(R,RId),
   et_lookup(GId,RId,[],A is B,_CG,all,N,IdG),
   !
  ;
   A='$NULL'(Id),
   get_null_id(Id),
   IdG=(-1,[])
  ).
solve_goal(group_by(A,Ps,Vs,C),St,CId,_GId,R,_N,_NR,IdG,_C) :-     
  !, 
  compute_group_by(group_by(A,Ps,Vs,C),St,CId,R,IdG).
solve_goal(order_by(G,Es,Os),St,CId,_GId,R,_N,_NR,IdG,_C) :-     
  !, 
  compute_order_by(order_by(G,Es,Os),St,CId,R,IdG).
solve_goal('$eq'(X,X),_St,_CId,_GId,_R,_N,_NR,(-1,[]),_C) :- % Equality for top-n SQL translations
  !.
solve_goal(G,St,_CId,_GId,R,_N,_NR,(-1,[]),_C) :-  % Deciding whether a term is null
  compute_builtin_relation(G,St,R), % All built-ins are deterministic
  !.
solve_goal(G,_St,CId,_GId,R,_N,_NR,(-1,[]),_C) :- 
  functor(G,AF,Arity),
  my_aggregate_relation(AF,Arity),
  !, 
  compute_aggregate_pred(G,CId,R).
solve_goal(G,_St,CId,_GId,_R,_N,_NR,(RId,[]),_C) :-      % Solves a goal using all of its matching facts. 'no change' annotated in ET
%  ((first_iter(CId,true) ; optimize_edb(off)) -> true ; fail),
  datalog_all_levels(G,_NVs,RId,CId,_Ls,_FId,_Rs),
  inc_statistics_flag(edb_retrievals).
solve_goal(G,St,CId,_GId,_,N,NR,(RId,AIds),_C) :-      % Solves a goal using all of its matching rules
  (datalog_all_levels((G:-B),NVs,RId,CId,Ls,FId,Rs)),
  R=datalog((G:-B),NVs,RId,CId,Ls,FId,Rs),
  inc_statistics_flag(idb_retrievals),
  solve(B,St,CId,1,_GIdo,R,N,NR,AIds).

datalog_all_levels(Rule,NVs,RId,CId,Ls,FId,Kind) :-
  datalog(Rule,NVs,RId,CId,Ls,FId,Kind)
  ;
  nonvar(CId),
  CId=[_|CIds],
  datalog_all_levels(Rule,NVs,RId,CIds,Ls,FId,Kind).

% Solving Negation
% :::WARNING: GId is not updated
solve_not(G,St,CId,GId,R,N,NR) :-
  solve(G,St,CId,GId,_,R,N,NR,_Ids), 
%  !, 
  fail.
solve_not(G,_St,CId,_GId,_R,_N,_NR) :-
 %(G=numbers(_) -> deb ; true), 
  goal_instance(G,CId),
  \+ et(G,_,CId,_).
%  ((\+ et(G,_,CId,_)) ; et(-(G),_,CId,_)).
% % Solving Negation: solve_et_not for et_not [SD91] and solve_strata (optimized)
% solve_not(G,St,CId,R) :-
%   neg(A),
%   (A==strata
%    -> 
%     solve_strata(G,St,CId,R)
%    ;
%     solve_et_not(G,St,CId)).

% solve_strata(G,St,CId,R) :-
%   solve(G,St,CId,R), 
%   !, 
%   fail.
% solve_strata(_G,_St,_CId,_R).

% solve_et_not(G,St,CId) :-
%   (solve_star(G,St,CId), fail; true),  % ET* evaluation
%   (et(G,_,CId), !, fail; true).        % ET lookup

%rule_instance_list(_,_CId) :- !.
rule_instance_list([],_CId).
rule_instance_list([R|Rs],CId) :-
  rule_instance(R,CId),
  rule_instance_list(Rs,CId).

rule_instance(':-'(_H,_B),_CId) :-
  !.
rule_instance(G,CId) :-
  goal_instance(G,CId).

goal_instance(G,_CId) :-
  ground(G),
  !.
goal_instance(G,CId) :-
  G=..[F|_],
  findall(my_foreign_key('$des',F,FK_AttNames,ForeignTablename,PK_AttNames,RIds),my_foreign_key('$des',F,FK_AttNames,ForeignTablename,PK_AttNames,RIds),FKs),
  build_FK_goal_list(FKs,F,G,FKGoals),
  build_et_goal_list(FKGoals,CId,Goals),
  (my_list_to_tuple(Goals,GT)
   ->
    call(GT)
   ;
    true).
    
is_limited_domain_predicate(P) :-
  limited_domain_predicates(Ps),
  memberchk(P,Ps).
  
become_limited_domain_predicate(N/A) :-
  get_table_untyped_arguments(N,OAtts),
  length(OAtts,A),
  findall(FK_AttNames,my_foreign_key('$des',N,FK_AttNames,_ForeignTablename,_PK_AttNames,_RIds),FK_AttNames_list),
  concat_lists(FK_AttNames_list,UAtts),
  my_mergesort(UAtts,OAtts).
  
build_FK_goal_list([],_F,_G,[]).
build_FK_goal_list([my_foreign_key('$des',F,FK_AttNames,ForeignTablename,PK_AttNames,_RIds)|FKs],F,G,[Goal|Goals]) :-
  build_FK_goal(G,FK_AttNames,ForeignTablename,PK_AttNames,_FK_Vars,Goal),
  build_FK_goal_list(FKs,F,G,Goals).

build_et_goal_list([],_CId,[]).
build_et_goal_list([Goal|Goals],CId,[et(Goal,_,CId,_)|ETGoals]) :-
  build_et_goal_list(Goals,CId,ETGoals).

%([my_foreign_key('$des',F,FK_AttNames,ForeignTablename,PK_AttNames)|FKs],F,G,CId,[et(Goal,_,CId,_)|Goals]) :-

%   findall(et(Goal,_,CId,_),
%           (my_foreign_key('$des',F,FK_AttNames,ForeignTablename,PK_AttNames),
%            build_FK_goal(G,FK_AttNames,ForeignTablename,PK_AttNames,_FK_Vars,Goal)
%           ),
%           Goals),
  
% Computing Primitives: compute_primitive
compute_primitive(A is B,_R) :-
  my_ground(B), 
  contain_null(B),
  !,
  (et_lookup(_,_,[],A is B,_CG,all,_,_IdG),
   !
  ;
   A='$NULL'(Id),
   get_null_id(Id),
   IdG=(-1,[]),
   et_assert(_,_,[],A is B,A is B,_,IdG,no)
  ).
compute_primitive(A is B,R) :- 
  \+ (contain_null(B)),
  (my_ground(B)
   ->
    A is B
   ;
    my_raise_exception(A is B,instantiation,R)).
% compute_primitive(A is B,R) :- 
%   ((my_ground(B), \+ (contain_null(B))) ->
%      A is B
%     ;
%      (contain_null(B) ->
%        A='$NULL'(_Id)
%        %my_raise_exception(A is B,instantiation,R)
%       ;
%        my_raise_exception(A is B,instantiation,R))).
compute_primitive(A=B,_R) :- 
  eval_expr(A,EA,R),
  eval_expr(B,EB,R),
  EA=EB.
compute_primitive(A\=B,R) :- 
  ((A='$NULL'(_IDA) ; B='$NULL'(_IDB)) -> fail ; true),
  ((var(A); var(B))
   ->
    my_raise_exception(A\=B,instantiation,R)
   ;
    eval_expr(A,EA,R),
    eval_expr(B,EB,R),
    EA\=EB
  ).
compute_primitive(A>B,R)  :- 
  ((var(A); var(B))
   ->
    my_raise_exception(A>B,instantiation,R)
   ;
    eval_expr(A,EA,R),
    eval_expr(B,EB,R),
    (number(EA),number(EB) -> 
     EA>EB
     ; 
     ((EA='$NULL'(_IDA) ; EB='$NULL'(_IDB)) ->
      fail 
      ; 
      EA@>EB)
    )
  ).
compute_primitive(A>=B,R) :- 
  ((var(A); var(B))
   ->
    my_raise_exception(A>=B,instantiation,R)
   ;
    eval_expr(A,EA,R),
    eval_expr(B,EB,R),
    (number(EA),number(EB) -> 
     EA>=EB
     ; 
%    ((EA='$NULL'(_IDA) ; EB='$NULL'(_IDB)) ->
     ((EA='$NULL'(IdA), var(IdA) 
       ;
       EB='$NULL'(IdB), var(IdB)) ->
      fail 
      ; 
      EA@>=EB)
   )
  ).
compute_primitive(A<B,R)  :- 
  ((var(A); var(B))
   ->
    my_raise_exception(A<B,instantiation,R)
   ;
    eval_expr(A,EA,R),
    eval_expr(B,EB,R),
    (number(EA),number(EB) -> 
     EA<EB
    ; 
     ((EA='$NULL'(_IDA) ; EB='$NULL'(_IDB)) ->
      fail 
      ; 
      EA@<EB)
    )
  ).
compute_primitive(A=<B,R) :- 
  ((var(A); var(B))
   ->
    my_raise_exception(A=<B,instantiation,R)
   ;
    eval_expr(A,EA,R),
    eval_expr(B,EB,R),
    (number(EA),number(EB)
     -> 
      EA=<EB
     ; 
%    ((EA='$NULL'(_IDA) ; EB='$NULL'(_IDB)) ->
      ((EA='$NULL'(IdA), var(IdA) 
       ;
        EB='$NULL'(IdB), var(IdB))
         ->
          fail 
         ; 
          EA@=<EB)
    )
  ).

is_primitive(_A is _B) :- !. 
%is_primitive(or(_A, _B)) :- !. 
is_primitive(_A = _B)  :- !.
is_primitive(_A \= _B) :- !.
is_primitive(_A > _B)  :- !.
is_primitive(_A >= _B) :- !.
is_primitive(_A < _B)  :- !.
is_primitive(_A =< _B) :- !.


eval_expr(E,EE,R) :-  
  (my_noncompound_term(E)
   -> 
    EE=E
   ;
    compute_primitive(EE is E,R)).
  
eval_exprs([],[],_R).
eval_exprs([E|Es],[EE|EEs],R) :-
  eval_expr(E,EE,R),
  eval_exprs(Es,EEs,R).

% compute_order_by(order_by(G,_Es,_Os),_St,CId,_R,IdG) :-
%   order_answer(on),
%   !,
%   et(G,IdG,CId,_It).
compute_order_by(order_by(G,Es,Os),_St,CId,R,IdG) :-
  bagof((EEs,G,IdG),
         It^
         (et(G,IdG,CId,It),
          eval_exprs(Es,EEs,R)),
         EEsGIds),
  my_mergesort(EEsGIds,my_multi_key_compare(Os,n1_of_3_tuple_arg),OEEsGIds),
  member((_,G,IdG),OEEsGIds).

    
solve_implication('=>'(L,R),St,CId,GId,GIdo,Rule,N,NR,Ids) :-
  dlrule_id(Rule,RId),
  dlrule_NVs(Rule,NVs),
  assert_hyp_program(RId,L,R,_G,NVs,CId,NCId,_Ps),
  functor(R,Name,Arity),
  (pred_stratum(Name/Arity,1),
   nr_nd_predicate(Name/Arity), % Non-recursive and does not depend on any recursive predicate
   \+ dependent_restricted_predicate(Name/Arity)
    ->
    set_flag(first_iter(NCId,true))
   ;
    solve_stratified(R,NCId,N,_Undefined)
  ),
  !,
  solve(R,St,NCId,GId,GIdo,Rule,N,NR,Ids).
solve_implication('=>'(_L,R),St,CId,GId,GIdo,Rule,N,NR,Ids) :-
  dlrule_id(Rule,RId),
  is_hyp_program_asserted(RId,G,CId,NCId,_ODLIds,Ps),
  (my_subsumes(G,R)
  ;
   (G==R -> true ; assertz(hyp_program_asserted(RId,R,CId,NCId,[],Ps))),
   solve_stratified(R,NCId,N,_Undefined)
  ),
  !,
  solve(R,St,NCId,GId,GIdo,Rule,N,NR,Ids).
  
% BEGIN OLD IMPLICATION
assert_hyp_program(RId,_L,_R,G,_NVs,CId,NCId,Ps) :-
  is_hyp_program_asserted(RId,G,CId,NCId,_ODLIds,Ps),
  !,
  fail.
assert_hyp_program(RId,L,R,R,NVs,CId,NCId,_Ps) :-
  rules_from_hyp_program(L,Rs),
% For optimization: 
%   reachable_user_predicates_rule_list(Rs,Preds),
%   reachable_user_predicates_body(R,DepPreds),
%   neg_dependent_predicates(Preds,DepPreds,Ps),
  rule_to_ruleNVs_list(Rs,NVs,RNVs),
  new_comp_id(RId,CId,NCId),
  write_info_verb_log(['Building hypothetical computation context ',NCId,' for:']),
  exec_if_verbose_on(display_ruleNVs_list(RNVs,0)),
  language(Lang),
  assert_rules(RNVs,NCId,Lang,[no_safety],_CRNVs,ODLIds,_Unsafe,Error),
  (var(Error)
   ->
    assertz(hyp_program_asserted(RId,R,CId,NCId,ODLIds,_)),
    OkRNVs=RNVs
   ;
    my_remove_non_ground(ODLIds,NFODLIds),
    assertz(hyp_program_asserted(RId,R,CId,NCId,NFODLIds,_)),
    rules_with_errors(RNVs,ODLIds,ErrRNVs,OkRNVs),
    (length(ErrRNVs,1) -> RulesTxt=rule ; RulesTxt=rules),
    write_info_log(['The following ',RulesTxt,' cannot be assumed:']),
    display_ruleNVs_list(ErrRNVs,2)
  ),
  ruleNVs_to_rule_list(OkRNVs,OkRs),
  update_stratification_add_rules(OkRs),
  write_info_verb_log(['PDG:']),
  exec_if_verbose_on(processC(pdg,[],_,yes)),
  write_info_verb_log(['Strata:']),
  exec_if_verbose_on(processC(strata,[],_,yes)).
   
% The same call
is_hyp_program_asserted(RId,R,CId,NCId,ODLIds,Ps) :-
  hyp_program_asserted(RId,R,CId,NCId,ODLIds,Ps),
  !.
% A previous call in the path
is_hyp_program_asserted(RId,R,CId,NCId,ODLIds,Ps) :-
  duplicates(off),
  hyp_program_asserted(_RId,R,_LCId,NCId,ODLIds,Ps),
  my_append(NCId,_,CId),
  member(RId,NCId).
% END OLD IMPLICATION

% BEGIN NEW IMPLICATION
% solve_implication('=>'(L,R),St,CId,GId,GIdo,Rule,N,NR,Ids) :-
%   dlrule_id(Rule,RId),
%   dlrule_NVs(Rule,NVs),
%   assert_hyp_program(RId,L,R,_G,NVs,CId,NCId,_Ps),
%   functor(R,Name,Arity),
%   (pred_stratum(Name/Arity,1),
%    nr_nd_predicate(Name/Arity), % Non-recursive and does not depend on any recursive predicate
%    \+ dependent_restricted_predicate(Name/Arity)
%     ->
%     set_flag(first_iter(NCId,true))
%    ;
%     solve_stratified(R,NCId,N,_Undefined)
%   ),
% %  !,
%   solve(R,St,NCId,GId,GIdo,Rule,N,NR,Ids).
% solve_implication('=>'(_L,R),St,CId,GId,GIdo,Rule,N,NR,Ids) :-
%   dlrule_id(Rule,RId),
%   is_hyp_program_asserted(RId,G,CId,NCId,_ODLIds,Ps),
%   (my_subsumes(G,R)
%   ;
%    (G==R
%     ->
%      true
%     ;
% %      retractall(called(_,_,CId)),            % Remove ct entries
% %      retractall(complete_flag(_,_,_,_,CId)), % Remove complete-flag entries
% %      retractall(called(_,_,NCId)),            % Remove ct entries
% %      retractall(complete_flag(_,_,_,_,NCId)), % Remove complete-flag entries
%      assertz(hyp_program_asserted(RId,R,CId,NCId,[],Ps))),
%    solve_stratified(R,NCId,N,_Undefined)
%   ),
%   !,
%   solve(R,St,NCId,GId,GIdo,Rule,N,NR,Ids).
%   
% assert_hyp_program(RId,_L,_R,G,_NVs,CId,NCId,Ps) :-
%   is_hyp_program_asserted(RId,G,CId,NCId,_ODLIds,Ps),
%   !,
%   fail.
% assert_hyp_program(RId,L,R,R,NVs,CId,NCId,_Ps) :-
%   rules_from_hyp_program(L,Rs),
%   rule_instance_list(Rs,CId),
% % For optimization: 
% %   reachable_user_predicates_rule_list(Rs,Preds),
% %   reachable_user_predicates_body(R,DepPreds),
% %   neg_dependent_predicates(Preds,DepPreds,Ps),
%   rule_to_ruleNVs_list(Rs,NVs,RNVs),
%   new_comp_id(RId,CId,NCId),
%   write_info_verb_log(['Building hypothetical computation context ',NCId,' for:']),
%   exec_if_verbose_on(display_ruleNVs_list(RNVs,0)),
%   language(Lang),
%   retract_hyp_instances(RId,CId,NCId),
%   assert_rules(RNVs,NCId,Lang,[no_safety],_CRNVs,ODLIds,_Unsafe,Error),
%   (var(Error)
%    ->
%     assertz(hyp_program_asserted(RId,R,CId,NCId,ODLIds,_)),
%     OkRNVs=RNVs
%    ;
%     my_remove_non_ground(ODLIds,NFODLIds),
%     assertz(hyp_program_asserted(RId,R,CId,NCId,NFODLIds,_)),
%     rules_with_errors(RNVs,ODLIds,ErrRNVs,OkRNVs),
%     (length(ErrRNVs,1) -> RulesTxt=rule ; RulesTxt=rules),
%     write_info_log(['The following ',RulesTxt,' cannot be assumed:']),
%     display_ruleNVs_list(ErrRNVs,2)
%   ),
%   ruleNVs_to_rule_list(OkRNVs,OkRs),
%   update_stratification_add_rules(OkRs),
% %  exec_if_verbose_on((listing(et),listing(complete_flag))),
%   write_info_verb_log(['PDG:']),
%   exec_if_verbose_on(processC(pdg,[],_,yes)),
%   write_info_verb_log(['Strata:']),
%   exec_if_verbose_on(processC(strata,[],_,yes)).
%    
%   
% retract_hyp_instances(RId,CId,NCId) :-
%   hyp_program_asserted(RId,R,CId,NCId,ODLIds,Ps),
%   !,
%   retract_et_by_limited_domain_predicate_id_list(ODLIds,NCId),
% %   retractall(called(_,_,CId)),            % Remove ct entries
% %   retractall(complete_flag(_,_,_,_,CId)), % Remove complete-flag entries
%   retractall(called(_,_,NCId)),            % Remove ct entries
%   retractall(complete_flag(_,_,_,_,NCId)), % Remove complete-flag entries
%   retract_rule_by_id_list(ODLIds,_Error),
%   retract(hyp_program_asserted(RId,R,CId,NCId,ODLIds,Ps)).
% retract_hyp_instances(_RId,_CId,_NCId).

% retract_et_by_limited_domain_predicate_id_list([],_CId).
% retract_et_by_limited_domain_predicate_id_list([Id|Ids],CId) :-
%   retract_et_by_limited_domain_predicate_id(Id,CId),
%   retract_et_by_limited_domain_predicate_id_list(Ids,CId).
%   
% retract_et_by_limited_domain_predicate_id(Id,CId) :-
%   (datalog(F,_,Id,_,_,_,_),
%    rule_is_fact(F),
%    functor(F,N,A),
%    is_limited_domain_predicate(N/A)
%    ->
%     my_nf_retract(et(_,F,_,CId,_))
%    ;
%     true).

% % The same call
% is_hyp_program_asserted(RId,R,CId,NCId,ODLIds,Ps) :-
%   hyp_program_asserted(RId,R,CId,NCId,ODLIds,Ps),
%   !.
% % A previous call in the path
% is_hyp_program_asserted(RId,R,CId,NCId,ODLIds,Ps) :-
%   duplicates(off),
%   hyp_program_asserted(_RId,R,_LCId,NCId,ODLIds,Ps),
%   my_append(NCId,_,CId),
%   member(RId,NCId).
% END OLD IMPLICATION
  
my_append(A,B,C):-append(A,B,C).
  
neg_dependent_predicates(Preds,DepPreds,Ps) :-
  pdg(PDG),
  neg_dep_nodes_list(Preds,PDG,Ns),
  reachable_list(DepPreds,PDG,Ds),
  my_set_inter(Ns,Ds,Ps).

reachable_list(Ns,PDG,Rs) :-  
  my_nf_setof(D,N^RNs^(member(N,Ns), reachable(N,PDG,RNs), member(D,RNs)),Rs).
  
reachable(N,PDG,SNodes) :-
  sub_pdg(N,PDG,(SNodes,_)). 
  
neg_dep_nodes_list(Ns,PDG,Rs) :-
  my_nf_setof(D,N^NNs^(member(N,Ns), neg_dep_nodes(N,PDG,NNs), member(D,NNs)),Rs).

neg_dep_nodes(N,(Nodes,Arcs),Rs) :-
  flip_arcs(Arcs,FArcs),
  sub_pdg(N,(Nodes,FArcs),(SNodes,SArcs)),
  my_nf_setof(D,(member(B-H,SArcs), sub_pdg(H,(SNodes,SArcs),(Ds,_)), member(D,[B|Ds])),Rs).
  
flip_arcs([],[]).
flip_arcs([A+B|Arcs],[B+A|FArcs]) :-
  flip_arcs(Arcs,FArcs).
flip_arcs([A-B|Arcs],[B-A|FArcs]) :-
  flip_arcs(Arcs,FArcs).
  
rules_with_errors([],[],[],[]).
rules_with_errors([RNVs|RNVss],[Id|Ids],[RNVs|ErrRNVss],OkRNVss) :-
  var(Id),
  !,
  rules_with_errors(RNVss,Ids,ErrRNVss,OkRNVss).
rules_with_errors([RNVs|RNVss],[_Id|Ids],ErrRNVss,[RNVs|OkRNVss]) :-
  rules_with_errors(RNVss,Ids,ErrRNVss,OkRNVss).
  

% Retract hypothetical rules and compute stratification if needed
retract_hyp_programs :-
  retract_hyp_programs_k(Retracted),
  (Retracted==true
   ->
    push_flag(undef_pred_warnings,off,IC),
    compute_stratification,
    pop_flag(undef_pred_warnings,IC)
   ;
    true).
  
% Retract hypothetical rules keeping current stratification
retract_hyp_programs_k :-
  retract_hyp_programs_k(_).
  
retract_hyp_programs_k(Retracted) :-
  my_nf_setof((RId,NCId,ODLIds), G^CId^Ps^hyp_program_asserted(RId,G,CId,NCId,ODLIds,Ps), Xs),
  (Xs==[]
   ->
    Retracted=false
   ;
    Retracted=true,
    retract_hyp_programs(Xs),
    retractall(first_iter(_,_))
  ).
  
retract_hyp_programs([]).
retract_hyp_programs([(RId,CId,ODLIds)|Xs]) :-
  retractall(hyp_program_asserted(RId,_,_,CId,ODLIds,_)),
  retractall(et(_,_,_,CId,_)),            % Remove et entries
  retractall(called(_,_,CId)),            % Remove ct entries
  retractall(complete_flag(_,_,_,_,CId)), % Remove complete-flag entries
  retract_rule_by_id_list(ODLIds,no_check,_Error2),
  retract_hyp_programs(Xs).

new_comp_id(RId,CId,[RId|CId]).

% new_comp_id(CId,NCId) :-
%   new_comp_id(CId,1,NCId).

% new_comp_id(CId,Id,NCId) :-
%   (datalog(_,_,_,[Id|CId],_,_,_)
%    ->
%     Id1 is Id+1,
%     new_comp_id(CId,Id1,NCId)
%    ;
%     NCId=[Id|CId]
%   ).

rules_from_hyp_program(Rs,ORs) :-
  var(ORs),
  !,
  rules_from_hyp_program_io(Rs,[],ORs).
rules_from_hyp_program(R,Rs) :-
  my_reverse(Rs,RRs),
  rules_from_hyp_program_oi(R,RRs).

rules_from_hyp_program_io('/\\'(Rs,R),IRs,ORs) :-
  !,
  rules_from_hyp_program_io(Rs,[R|IRs],ORs).
rules_from_hyp_program_io(R,IRs,[R|IRs]).

% rules_from_hyp_program_oi('$void',[]) :-
%   !.
rules_from_hyp_program_oi(R,[R]) :-
  !.
rules_from_hyp_program_oi('/\\'(CRs,R),[R|Rs]) :-
  !,
  rules_from_hyp_program_oi(CRs,Rs).

% res_rules_from_hyp_program(Rs,NRs) :-
%   pos_res_rules_from_hyp_program(Rs,[],_PRs,[],NRs).

% pos_res_rules_from_hyp_program(Rs,PRs,NRs) :-
%   pos_res_rules_from_hyp_program(Rs,[],PRs,[],NRs).

% pos_res_rules_from_hyp_program('/\\'(Rs,-(R)),IPRs,OPRs,INRs,ONRs) :-
%   !,
%   pos_res_rules_from_hyp_program(Rs,IPRs,OPRs,[R|INRs],ONRs).
% pos_res_rules_from_hyp_program('/\\'(Rs,R),IPRs,OPRs,INRs,ONRs) :-
%   !,
%   pos_res_rules_from_hyp_program(Rs,[R|IPRs],OPRs,INRs,ONRs).
% pos_res_rules_from_hyp_program(-(R),PRs,PRs,NRs,[R|NRs]) :-
%   !.
% pos_res_rules_from_hyp_program(R,PRs,[R|PRs],NRs,NRs).


compute_group_by(group_by(G,_Ps,GBVs,C),St,CId,R,Ids) :-
  !,
  replace_and_get_aggregates_pairs(C,G,[],AVs,RC),
  build_groups(G,GBVs,CId,Ids),
%  findall(G, et_all_levels(G,_EIds,CId), CGs),
  findall(G, et(G,_EIds,CId,_It), CGs),
  solve_aggregates_list(AVs,G,CGs),
  solve(RC,St,CId,R).
  
% Gets pairs (aggregate(Position),Result) from a term including aggregates. 
% It also replaces aggregate(Variable) by Result in the term. 
% Result is a varible which will be unified with the result of the aggregate afterwards
replace_and_get_aggregates_pairs(T,_G,AVs,AVs,T) :- 
  var(T),
  !.
replace_and_get_aggregates_pairs('$NULL'(ID),_G,AVs,AVs,'$NULL'(ID)) :- 
  !.
replace_and_get_aggregates_pairs(C,_G,AVs,[(C,R)|AVs],R) :- 
  arithmetic_function(C,_,_,aggregate,_,0),
  !.
replace_and_get_aggregates_pairs(T,_G,AVs,AVs,T) :- 
  atomic(T),
  !.
replace_and_get_aggregates_pairs(C,_G,AVs,[(C,R)|AVs],R) :- 
  C =.. [F,_E],
  arithmetic_function(F,_,_,aggregate,_,1),
  !.
replace_and_get_aggregates_pairs(C,G,AVsi,AVso,RC) :- 
  C =.. [F|As],
  replace_and_get_aggregates_pairs_list(As,G,AVsi,AVso,RAs),
  RC =.. [F|RAs].

replace_and_get_aggregates_pairs_list([],_G,AVs,AVs,[]) :-
  !.
replace_and_get_aggregates_pairs_list([T|Ts],G,AVsi,AVso,[RT|RTs]) :-
  replace_and_get_aggregates_pairs(T,G,AVsi,AVso1,RT), 
  replace_and_get_aggregates_pairs_list(Ts,G,AVso1,AVso,RTs).
  
% Solves each aggregate(Position,Result) in a list, holding the result in Result, wrt. the list of a computed set CGs
solve_aggregates_list([],_G,_CGs).
solve_aggregates_list([(AF,V)|AVs],G,CGs) :-
  AF=..[F,E],
  !,
  G=..[_|Args],
  term_variables(E,EVs),
  get_arg_position_list(EVs,Args,Is),
  findall(N,
           (member(G,CGs),
            not_null_ith_arg_list(Is,G),
            get_arg_position_list(EVs,Args,Is),
            eval_expr(E,N,void)),
          DNs),
  (atom_concat(_,'_distinct',F) -> my_remove_duplicates_sort(DNs,Ns) ; Ns=DNs),
  compute_aggr_from_group(F,Ns,V),
  solve_aggregates_list(AVs,G,CGs).
solve_aggregates_list([(F,V)|AVs],G,CGs) :-
  compute_aggr_from_group(F,CGs,V),
  solve_aggregates_list(AVs,G,CGs).

% Computing Aggregate Predicates: compute_aggregate_pred

compute_aggregate_pred(count(G,GBVs,O),CId,_R) :-
  % Count(*) counts all rows, even when some might contain nulls
  % Other aggregates include an additional argument: the attribute w.r.t the aggregation is computed
  !,
  build_groups(G,GBVs,CId),
%  findall(G, et_all_levels(G,_Ids,CId), CGs), % et(CG,Ids,CId,It)
  findall(G, et(G,_Ids,CId,_It), CGs),
  length(CGs,O).
compute_aggregate_pred(count_distinct(G,GBVs,O),CId,_R) :-
  % Count(*) counts all rows, even when some might contain nulls
  % Other aggregates include an additional argument: the attribute w.r.t the aggregation is computed
  !,
  build_groups(G,GBVs,CId),
%  findall(G, et_all_levels(G,_Ids,CId), DCGs),
  findall(AG, (et(G,_Ids,CId,_It),abstract_nulls(G,AG),make_ground(AG)), DCGs),
  my_remove_duplicates_sort(DCGs,CGs),
  length(CGs,O).
compute_aggregate_pred(Aggr,CId,Rule) :-
  % A tuple with a null in any pivot variable is omitted in the aggregate computation
  Aggr=..[AF,AR,E,GBVs,O],
  my_aggregate_relation(AF,4),
  ((atom_concat(F,'_distinct',AF),
    R=AR 
     ;
    AR = distinct(R), 
    AF = F)
   -> 
    Pred = my_remove_duplicates_sort 
   ;
    R = AR,
    F = AF,
    Pred = (=)),
  term_variables(E,EVs),
  R=..[_|Args],
  get_arg_position_list(EVs,Args,Is),
  build_groups(R,GBVs,CId),
  findall(N,
%           (et_all_levels(R,_Ids,CId),
           (et(R,_Ids,CId,_It),
            not_null_ith_arg_list(Is,R),
            get_arg_position_list(EVs,Args,Is),
            eval_expr(E,N,Rule)),
          DNs),
  Goal =.. [Pred,DNs,Ns],
  call(Goal),
  compute_aggr_from_group(F,Ns,O).

not_null_ith_arg_list([],_T).
not_null_ith_arg_list([I|Is],T) :-
  not_null_ith_arg(I,T),
  not_null_ith_arg_list(Is,T).

not_null_ith_arg(I,T) :-
  arg(I,T,N),
  N\='$NULL'(_Id).
    
%compute_aggr_from_group(count,Ns,O) :- 
%  length(Ns,O).
compute_aggr_from_group(count,Ns,O) :-
  compute_count(Ns,O).
compute_aggr_from_group(count_distinct,DNs,O) :-
  my_remove_duplicates_sort(DNs,Ns),
  compute_count(Ns,O).
compute_aggr_from_group(sum,Ns,O) :- 
  compute_sum(Ns,O).
compute_aggr_from_group(sum_distinct,DNs,O) :- 
  my_remove_duplicates_sort(DNs,Ns),
  compute_sum(Ns,O).
compute_aggr_from_group(times,Ns,O) :- 
  compute_times(Ns,O).
compute_aggr_from_group(times_distinct,DNs,O) :- 
  my_remove_duplicates_sort(DNs,Ns),
  compute_times(Ns,O).
compute_aggr_from_group(avg,Ns,O) :- 
  compute_avg(Ns,O).
compute_aggr_from_group(avg_distinct,DNs,O) :- 
  my_remove_duplicates_sort(DNs,Ns),
  compute_avg(Ns,O).
compute_aggr_from_group(min,Ns,O) :- 
  compute_min(Ns,O).
compute_aggr_from_group(max,Ns,O) :- 
  compute_max(Ns,O).

build_groups(G,GBVs,CId) :-
  build_groups(G,GBVs,CId,_Ids).
  
build_groups(G,GBVs,CId,(N,[])) :-
  G=..[_|Args],
  get_arg_position_list(GBVs,Args,GBVPoss),
  copy_term(G,FG),
  get_ith_arg_list(GBVPoss,FG,FGBVs),
  term_variables(FG,FGVs),
  copy_term(GBVs,NVs),
  build_ex_quantifier(FGVs,(et(FG,Ids,CId,_It),abstract_unify_nulls_varlist(FGBVs,NVs,GBVs)),QFG),
  QQFG=Ids^QFG, % Ciao needs this! Instead, it should be written as the remarked line below
  setof(GBVs,QQFG,GBVals),
%  setof(GBVs,Ids^QFG,GBVals),
  !,
%  member(GBVs,GBVals). % Leave choicepoint to build groups
  my_nth1_member(GBVs,PN,GBVals), % Leave choicepoint to build groups
  N is -PN.
build_groups(G,_GBVs,CId,Ids) :-
%  et_all_levels(G,Ids,CId),
  et(G,Ids,CId,_It),
  !.
% The next clause applies when no group can be found but there are tuples in the input relation.
build_groups(G,_GBVs,CId,Ids) :-
%  et_all_levels(G,Ids,CId),
  et(G,Ids,CId,_It),
  !.
% The next clause applies when no group can be found.
% For instance, counting wrt. to no groups returns 0 
build_groups(_G,[],_CId,(-1,[])) :-
  !.

build_ex_quantifier([],G,G).
build_ex_quantifier([V|Vs],G,QG) :-
  build_ex_quantifier(Vs,V^G,QG).

arg_list(_I,[],[]) :-
  !.
arg_list(I,[CG|CGs],Ns) :-
  arg(I,CG,N),
  nonvar(N),
  N='$NULL'(_ID),
  !,
  arg_list(I,CGs,Ns).
arg_list(I,[CG|CGs],[N|Ns]) :-
  arg(I,CG,N),
  arg_list(I,CGs,Ns).

  
get_ith_arg_list([],_T,[]).
get_ith_arg_list([P|Ps],T,[A|As]) :-
  arg(P,T,A),
  get_ith_arg_list(Ps,T,As).
 
get_ith_member(1,[X|_Xs],X) :-
  !. % Ensure choice point is discarded
get_ith_member(I,[_X|Xs],Y) :-
  I > 1,
  I1 is I-1,
  get_ith_member(I1,Xs,Y).
  
get_ith_member_list([],_Xs,[]).
get_ith_member_list([I|Is],Xs,[Y|Ys]) :-
  get_ith_member(I,Xs,Y),
  get_ith_member_list(Is,Xs,Ys).

term_args_positions(_T,[],[]).
term_args_positions(T,Vs,Ps) :-
  T=..[_|Args],
  setof(P,V^(member(V,Vs),get_arg_position(V,Args,P)),Ps).

get_arg_position_list(Xs,Ys,Ps) :-
  var(Xs),
  !,
  get_ith_member_list(Ps,Ys,Xs).
get_arg_position_list([],_L,[]).
get_arg_position_list([V|Vs],L,[P|Ps]) :-
  once(get_arg_position(V,L,P)),
  get_arg_position_list(Vs,L,Ps).

get_arg_position(V,L,P) :-
  nonvar(P),
  !,
  my_nth1_member(V,P,L).
get_arg_position(V,L,P) :-
  get_arg_position_from(V,L,1,P).

get_arg_position_from(V,[A|_As],I,I) :-
  V==A.
%   V==A,
%   !.
get_arg_position_from(V,[_A|As],I,NI) :-
  I1 is I+1,
  get_arg_position_from(V,As,I1,NI).
  
compute_count(Ns,C) :-
  compute_count_acc(Ns,0,C).

compute_count_acc([],C,C) :-
  !. 
compute_count_acc([X|Xs],Ci,Co) :-
  nonvar(X),
  X='$NULL'(_ID),
  !,
  compute_count_acc(Xs,Ci,Co).
compute_count_acc([_X|Xs],Ci,Co) :-
  !,
  C1 is Ci+1,
  compute_count_acc(Xs,C1,Co).

compute_avg([],'$NULL'(_Id)) :-
  !.
compute_avg(Ns,O) :-
  compute_sum(Ns,S),
  length(Ns,L),
  O is S/L.

compute_sum([],'$NULL'(_Id)).
compute_sum([N|Ns],S) :-
  compute_sum_acc(Ns,N,S).

compute_sum_acc([],N,N).
compute_sum_acc([N|Ns],TS,S) :-
  NN is N+TS,
  compute_sum_acc(Ns,NN,S).

compute_times([],'$NULL'(_Id)).
compute_times([N|Ns],S) :-
  compute_times_acc(Ns,N,S).

compute_times_acc([],N,N).
compute_times_acc([N|Ns],TS,S) :-
  NN is N*TS,
  compute_times_acc(Ns,NN,S).

compute_min([],'$NULL'(_Id)).
compute_min([N|Ns],M) :-
  compute_min_acc(Ns,N,M).

compute_min_acc([],M,M).
compute_min_acc([N|Ns],TM,M) :-
  compute_primitive(N<TM,_R),
  !,
  compute_min_acc(Ns,N,M).
compute_min_acc([_N|Ns],TM,M) :-
  compute_min_acc(Ns,TM,M).

compute_max([],'$NULL'(_Id)).
compute_max([N|Ns],M) :-
  compute_max_acc(Ns,N,M).

compute_max_acc([],M,M).
compute_max_acc([N|Ns],TM,M) :-
  compute_primitive(N>TM,_R),
  !,
  compute_max_acc(Ns,N,M).
compute_max_acc([_N|Ns],TM,M) :-
  compute_max_acc(Ns,TM,M).

compute_builtin_relation(G,_St,R) :-  
  functor(G,F,A),
  my_builtin_relation(F,A,_M,_K),
  \+ my_aggregate_relation(F,A),
  \+ (F,A)==(group_by,4),
  !,
  check_modes(G,R),
  call(G).


my_raise_exception(G,Mid,R_V) :-
  (seen;true),
  (Mid = instantiation -> 
   Message = exception('Non ground argument(s) found in goal')
   ;
   (Mid = undefined ->
    Message = exception('Undefined predicate')
    ;
    (Mid = basic_goal ->
     Message = exception('The following is not a valid goal:')
     ;
     (Mid = exec ->
      Message = exception('Executing goal:')
      ;
      (Mid = unsupported_in_Prolog ->
       Message = exception('Aggregates are not supported in Prolog mode')
       ;
       (Mid = non_number ->
        Message = exception('Non-numbers found in result set of')
        ;
        (Mid = type ->
         Message = exception('Type error')
         ;
         (Mid = bounds ->
          Message = exception('Bounds error')
          ;
          (Mid = fd_unsupported ->
           Message = exception('FD constraint solving unsupported by underlying Prolog system. ')
           ;
           (Mid = odbc_unsupported ->
            Message = exception('ODBC connections unsupported by underlying Prolog system. Use either binaries or SWI-Prolog or SICStus Prolog sources. ')
            ;
            Message = Mid
           )
           )
         )
        )
       )
      )
     )
    )
   )
  ),
  write_exception_message(G,Message,R_V),
  throw(des_exception(Message)).

write_exception_message(generic,syntax(Message),NVs) :-
  !,
  write_error_log(['$tbc']),
  write_cond_unquoted_with_NVs_list(Message,NVs),
  nl_log,
  write_tapi_eot.
write_exception_message(unallowed_identifier(O,I,A),syntax(_Message),_R_V) :-
  !,
  write_error_log(['Built-in identifier ''',I,''' with ',A,' arguments is not allowed as a ',O,'.']).
write_exception_message(invalid_use(I),syntax(_Message),_R_V) :-
  !,
  write_error_log(['Invalid use of ''',I,''' in context.']).
write_exception_message(unknown_column(T,C),syntax(Message),R_V) :-
  !,
  write_exception_message(unknown_column(T,C,statement),syntax(Message),R_V).
write_exception_message(unknown_column(T,C,S),syntax(_Message),_R_V) :-
  !,
  scope_error_tail_message(S,M),
  ((is_system_identifier(T) ; var(T))
   ->
    write_error_log(['Unknown column ''',C,'''',M,'.'])
   ;
    write_error_log(['Unknown column ''',T,'.',C,'''',M,'.'])
  ),
  display_column_alternatives(T,C).
write_exception_message(unknown_relation(R),syntax(_Message),_R_V) :-
  !,
  write_error_log(['Unknown table or view ''',R,'''.']),
  display_relation_alternatives(R).
write_exception_message(unknown_view(V),syntax(_Message),_R_V) :-
  !,
  write_error_log(['Unknown view ''',V,'''.']),
  display_view_alternatives(V).
write_exception_message(unknown_table(T),syntax(_Message),_R_V) :-
  !,
  write_error_log(['Unknown table ''',T,'''.']),
  display_table_alternatives(T).
write_exception_message(unknown_user_predicate(F/A),syntax(_Message),_R_V) :-
  !,
  write_error_log(['Unknown user predicate ',F/A,'.']),
  display_user_predicate_alternatives(F).
write_exception_message(G,M,R_V) :-
  (M=exception(Message), 
   I='Exception'
  ;
   M=syntax(Message), 
   I='Error'
  ;
   M=Message, 
   I='Exception'
  ),
  !,
  (my_is_list(Message)
   ->
    DisplayMessage = Message
   ;
    DisplayMessage = [Message]
  ),
  write_log_list([I,': ']),
  write_unquoted_with_NVs_list(DisplayMessage,R_V),
  write_log_list([' ']),
  ((nonvar(R_V),R_V=datalog(R,NVs,_Rid,_CId,Ls,FId,_Rs))
   ->
    write_with_NVs(G,NVs),
    write_log_list([' in the instanced rule:',nl]),
    display_ruleNVs_list([(R,NVs)],11),
    display_rule_info(Ls,FId)
   ;
    (nonvar(R_V),my_is_list(R_V)
     ->
     write_with_NVs(G,R_V)
    ;
     write_with_NVs(G,[])
    )
  ),
  nl_log.

scope_error_tail_message(Scope,Message) :-
  language(Lang),
  scope_error_tail_message(Lang,Scope,Message).
  
scope_error_tail_message(_Lang,Var,'') :- var(Var), !.
scope_error_tail_message(sql,project,' in ''select'' list') :- !.
scope_error_tail_message(sql,sigma,' in ''where'' condition') :- !.
scope_error_tail_message(sql,having,' in ''having'' condition') :- !.
scope_error_tail_message(sql,on,' in ''on'' condition') :- !.
scope_error_tail_message(sql,using,' in ''using'' clause') :- !.
scope_error_tail_message(sql,group,' in ''group by'' clause') :- !.
scope_error_tail_message(sql,order,' in ''order by'' clause') :- !.
scope_error_tail_message(ra,project,' in projection') :- !.
scope_error_tail_message(ra,sigma,' in select condition') :- !.
scope_error_tail_message(ra,having,' in group_by condition') :- !.
scope_error_tail_message(ra,on,' in join condition') :- !.
scope_error_tail_message(ra,group,' in grouping criteria') :- !.
scope_error_tail_message(ra,order,' in order_by criteria') :- !.
scope_error_tail_message(_,Scope,Message) :- atom_concat(' in ',Scope,Message).

% Testing whether a term T1 subsumes a term T2
% i.e., T1 is 'more general' than T2

% Aggregates:
% min(p(X,Y),X,[],1) does not subsume min(p(X,2),X,[],1)
% min(p(X,2),X,[],1) does not subsume min(p(X,Y),X,[],1)
% min(p(X,Y),X,[Y],1) does not subsume min(p(X,Y),X,[a],1)
% It suffices to test whether they are the same term up to variable renaming
my_subsumes(General,Specific) :-
  functor(General,AF,Arity),
  (my_aggregate_relation(AF,Arity)
   ;
   (AF,Arity)=(group_by,4)
  ),
  !,
  functor(Specific,AF,Arity),
  my_equal_up_to_renaming(General,Specific).
% p(X,Y) does subsume p(X,2)
% p(X,2) does not subsume p(X,Y)
% p(X,X) does not subsume p(X,Y)
% p(X,Y) does subsume p(X,X)
% p('$NULL'(0)) does not subsume p('$NULL'(1)) 
% p('$NULL'(_Id1)) does subsume p('$NULL'(_Id2)) 
my_subsumes(General,Specific) :-
  my_subsumes_chk(General,Specific).
% my_subsumes(General,Specific) :-
%   \+ \+ (make_ground(Specific),
%          General=Specific).

my_equal_up_to_renaming(General,Specific) :-
  \+ \+ (make_ground(General),
         make_ground(Specific),
         General==Specific).

% remove_GBArg(A,RA) :-
%   A =.. [F,P,V,_GB,_O],
%   !,
%   RA =.. [F,P,V].
% remove_GBArg(A,RA) :-
%   A =.. [F,P,_GB,_O],
%   RA =.. [F,P].
     
% Replaces all occurrences of '$NULL'(CteOrVar) by '$NULL'(FreshVar) in a term T
abstract_nulls(T,T) :- 
  nulls(off),
  !.
abstract_nulls(T,AT) :- 
  my_abstract_nulls(T,AT).

my_abstract_nulls(T,T) :- 
  (var(T)),
  !.
% my_abstract_nulls('$NULL'(_CteOrVar),'$NULL'(_FreshVar)) :- 
%   !.
my_abstract_nulls('$NULL'(Cte),'$NULL'(_FreshVar)) :-
  nonvar(Cte), 
  !.
my_abstract_nulls('$NULL'(Var),'$NULL'(Var)) :-
  !.
my_abstract_nulls(T,T) :- 
%  (number(T) ; atom(T)),
  atomic(T),
  !.
my_abstract_nulls(C,RC) :- 
  C =.. [F|As],
  !, 
  my_abstract_nulls_list(As,RAs),
  RC =.. [F|RAs].

my_abstract_nulls_list([],[]) :-
  !.
my_abstract_nulls_list([T|Ts],[RT|RTs]) :-
  my_abstract_nulls(T,RT), 
  my_abstract_nulls_list(Ts,RTs).

% Replaces all occurrences of '$NULL'(CteOrVar) by '$NULL'(Var) in a term T
my_abstract_unify_nulls(T,_V,T) :- 
  (var(T)),
  !.
my_abstract_unify_nulls('$NULL'(_CteOrVar),V,'$NULL'(V)) :- 
  !.
my_abstract_unify_nulls(T,_V,T) :- 
%  (number(T) ; atom(T)),
  atomic(T),
  !.
my_abstract_unify_nulls(C,V,RC) :- 
  C =.. [F|As],
  !, 
  my_abstract_unify_nulls_list(As,V,RAs),
  RC =.. [F|RAs].

my_abstract_unify_nulls_list([],_V,[]) :-
  !.
my_abstract_unify_nulls_list([T|Ts],V,[RT|RTs]) :-
  my_abstract_unify_nulls(T,V,RT), 
  my_abstract_unify_nulls_list(Ts,V,RTs).

my_abstract_unify_nulls_varlist([],[],[]) :-
  !.
my_abstract_unify_nulls_varlist([T|Ts],[V|Vs],[RT|RTs]) :-
  my_abstract_unify_nulls(T,V,RT), 
  my_abstract_unify_nulls_varlist(Ts,Vs,RTs).

abstract_unify_nulls_varlist(T,_V,T) :-
  nulls(off),
  !.
abstract_unify_nulls_varlist(T,V,RT) :-
  my_abstract_unify_nulls_varlist(T,V,RT).
  
% Instantiates all variables in Term to fresh constants.
make_ground(Term) :-
  numbervars(Term, 0, _).

make_ground_args(T,Ps):-
  positive_atom(T,PT),
  make_ground_args_atom(PT,Ps).

make_ground_args_atom(_T,[]).
make_ground_args_atom(T,[P|Ps]) :-
  functor(T,_F,A),
  (A>=P
   ->
    arg(P,T,Arg),
    make_ground(Arg)
   ;
    true),
  make_ground_args_atom(T,Ps).

%% Tests whether a term does not contain variables
%my_no_contains_vars(T) :-
%  var(T),
%  !,
%  fail.
%my_no_contains_vars('$NULL'(_ID)) :-
%  !.
%my_no_contains_vars(C) :- 
%  C =.. [_F|As],
%  !, 
%  my_no_contains_vars_list(As).
%
%my_no_contains_vars_list([]).
%my_no_contains_vars_list([T|Ts]) :-
%  my_no_contains_vars(T),
%  my_no_contains_vars_list(Ts).
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% remove_undefined
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_undefined(Undefined) :-
  strata([non-stratifiable]), % Remove undefined only if non-stratifiable query
% Warning:
  setof(Fact, 
%  findall(Fact, 
        (et(not(Fact),NIds,CId,It),
         et(Fact,Ids,CId,It), 
         my_idx_retract(et(not(Fact),NIds,CId,It)), 
         my_idx_retract(et(Fact,Ids,CId,It))), 
        Undefined).
remove_undefined([]).

ground_nulls :-
  nulls(on),
  et(Fact,Ids,CId,It),
  concrete_nulls(Fact,GFact,Grounded),
  (Grounded == grounded
   ->
   my_idx_retract(et(Fact,Ids,CId,It)),
   my_idx_assertz(et(GFact,Ids,CId,It))),
  fail.
ground_nulls.
   
%% Assigns a unique ID to each occurrence of '$NULL'(Var) in a term T
concrete_nulls(_T) :-
  nulls(off),
  !.
concrete_nulls(T) :-
  concrete_nulls(T,T,_Grounded).

%::WARNING: the second argument is the very same as the first, why is it used?
concrete_nulls(T,T,_Grounded) :- 
  nulls(off),
  !.
concrete_nulls(T,CT,Grounded) :- 
  my_concrete_nulls(T,CT,Grounded).
  
my_concrete_nulls(Var,Var,_Grounded) :- 
  var(Var),
  !.
my_concrete_nulls('$NULL'(Var),'$NULL'(Var),grounded) :- 
  var(Var),
  !,
  get_null_id(Var).
my_concrete_nulls(T,T,_) :- 
%  (number(T) ; atom(T)),
  atomic(T),
  !.
my_concrete_nulls(C,RC,Grounded) :- 
  C =.. [F|As],
  !, 
  my_concrete_nulls_list(As,RAs,Grounded),
  RC =.. [F|RAs].

my_concrete_nulls_list([],[],_Grounded) :-
  !.
my_concrete_nulls_list([T|Ts],[RT|RTs],Grounded) :-
  !, 
  my_concrete_nulls(T,RT,Grounded), 
  my_concrete_nulls_list(Ts,RTs,Grounded).

% Tests whether a term contains a null
contain_null(_T) :-
  nulls(off),
  !,
  fail.
contain_null(T) :-
  var(T),
  !,
  fail.
contain_null('$NULL'(_ID)) :-
  !.
contain_null(C) :- 
  C =.. [_F|As],
  !, 
  contain_null_list(As).

contain_null_list([T|_Ts]) :-
  contain_null(T).
contain_null_list([_T|Ts]) :-
  contain_null_list(Ts).

% contain_null(T) :-
%   (nulls(on) -> my_contain_null(T)).
%   
% my_contain_null(T) :-
%   var(T),
%   !,
%   fail.
% my_contain_null('$NULL'(_ID)) :-
%   !.
% my_contain_null(C) :- 
%   C =.. [_F|As],
%   !, 
%   my_contain_null_list(As).

% my_contain_null_list([T|_Ts]) :-
%   my_contain_null(T).
% my_contain_null_list([_T|Ts]) :-
%   my_contain_null_list(Ts).
%   
/*********************************************************************/
/* Building a predicate dependency graph: build_pdg/1                */
/*********************************************************************/
% A predicate dependency graph is a pair of a list of predicate nodes 
% (name/arity), and a list of arcs (nto/ato + nfrom/afrom, meaning that
% the predicate nto/ato depends on -has in the rhs of any of its defining
% rules- the predicate nfrom/afrom; alternatively, nto/ato - nfrom/afrom 
% is used when the predicate nfrom/afrom appears negated)
% Follows [ZCF+97]

% Complete building of PDG for External databases
build_pdg(_PDG) :-
  batch(on),
  !.
build_pdg(PDG) :-
  write_info_verb_log(['Computing predicate dependency graph...']),
  current_db(Connection),
  Connection \== '$des',
  !,
  catch(build_rdb_pdg(PDG),M,(my_exception_message_display(error,M), fail)). % If an exception occurs, try to build the local PDG instead
% Complete building of PDG for local database
build_pdg(PDG) :-
  get_persistent_preds(Nodes),
  disable_rdb_datasources,
  build_des_pdg(Nodes,PDG,LocalNodes,REPs,Ps),
%  write_info_verb_log(['Tagging predicates...']),
  tag_predicates(PDG,LocalNodes,[],REPs,Ps),
  enable_rdb_datasources.

% Complete building of PDG for External databases
build_rdb_pdg(PDG) :-
  write_info_verb_log(['- Reading external metadata...']),
  current_db(Connection),
  my_odbc_get_table_arity_list(Connection,RDBNodes),
  write_info_verb_log(['- Building graph...']),
  get_persistent_preds(PersistentNodes),
  append(RDBNodes,PersistentNodes,DupAllExternalNodes),
  my_remove_duplicates_sort(DupAllExternalNodes,AllExternalNodes),
  disable_rdb_datasources,
  build_des_pdg_for_rdb(AllExternalNodes,PDG,LocalNodes,REPs,Ps),
  tag_predicates(PDG,LocalNodes,RDBNodes,REPs,Ps),
  enable_rdb_datasources.

build_des_pdg_for_rdb(RDBNodes,PDG,LocalNodes,REPs,NCPs) :-
  get_rules_from_external_views(Rs),
  build_des_pdg_from_rules(Rs,[],warning,SPDG,RDBNodes,LocalNodes1,REPs1,NCPs1),
  remove_nonrel_preds_from_pdg(SPDG,PDG1),
%  remove_nonuser_preds_from_pdg(SPDG,PDG1),
  build_des_pdg(RDBNodes,PDG2,LocalNodes2,REPs2,NCPs2),
  merge_pdgs(PDG1,PDG2,PDG),
  my_set_union(LocalNodes1,LocalNodes2,LocalNodes),
  my_set_union(REPs1,REPs2,REPs),
  my_set_union(NCPs1,NCPs2,NCPs).
  
build_des_pdg_from_rules([],_PrevNodes,_Warning,(RDBNodes,[]),RDBNodes,[],[],[]) :-
  !.
build_des_pdg_from_rules(Program,PrevNodes,Warning,(Nodes,Arcs),RDBNodes,LocalNodes,REPs,NCPs) :-
  build_pdg_from_rules(Program,PrevNodes,Warning,(Nodes,Arcs),RDBNodes,LocalNodes,REPs,NCPs).
  
% Complete building of PDG for local database
build_des_pdg(RDBNodes,PDG,LocalNodes,REPs,NCPs) :-
  get_persistent_dlrules(PDLs),
  assertz_list(PDLs),
  build_pdg_from_rules([],[],warning,PDG,RDBNodes,LocalNodes,REPs,NCPs), % Don't use the list of rules ([]) and use the DB instead, and display warnings if there are undefined predicates
  retract_list(PDLs),
  !.
build_des_pdg(_,([],[]),[],[],[]).

% Incremental building of the PDG for local database
build_pdg_from_rules(Program,PDG) :-
  build_pdg_from_rules(Program,[],no_warning,PDG,[],_LocalNodes,_REPs,_NCPs).
  
build_pdg_from_rules(Program,PrevNodes,Warning,(Nodes,Arcs),RDBNodes,LocalNodes,REPs,NCPs) :-
  my_nf_setof(Arc,find_pdg_arcs(Arc,Program),AArcs),
  pdg_nodes_arcs_from_arcs(AArcs,PrevNodes,Warning,RDBNodes,TNodes,Arcs,TLocalNodes,REPs,NCPs),
  add_ddb_table_nodes(Program,TNodes,TLocalNodes,Nodes,LocalNodes).
  
% Incremental building of the PDG by providing some local rules
% (+Program,+PrevNodes,+Warning,-(Nodes,Arcs),-RDBNodes,-LocalNodes,-REPs,-NCPs) :-
build_pdg_from_local_rules(Program,PrevNodes,Warning,(Nodes,Arcs),RDBNodes,LocalNodes,REPs,NCPs) :-
  build_pdg_from_rules(Program,PrevNodes,Warning,(Nodes,Arcs),RDBNodes,TLocalNodes,REPs,NCPs),
  pred_rule_list(Ps,Program),
  my_set_union(TLocalNodes,Ps,LocalNodes).

remove_nonuser_preds_from_strata(S,S) :-
  development(on),
  !.
remove_nonuser_preds_from_strata([],[]).
remove_nonuser_preds_from_strata([(P,_S)|Ss],RSs) :-
  is_system_pred(P),
  !,
  remove_nonuser_preds_from_strata(Ss,RSs).
remove_nonuser_preds_from_strata([S|Ss],[S|RSs]) :-
  remove_nonuser_preds_from_strata(Ss,RSs).

  
remove_nonuser_preds_from_pdg((Nodes,Arcs),(RNodes,RArcs)) :-
  remove_nonuser_nodes(Nodes,RNodes),
  remove_nonuser_arcs(Arcs,DRArcs),
  remove_duplicates(DRArcs,RArcs).
  
remove_nonuser_nodes([],[]).
remove_nonuser_nodes([Node|Nodes],RNodes) :-
  is_system_pred(Node),
  !,
  remove_nonuser_nodes(Nodes,RNodes).
remove_nonuser_nodes([Node|Nodes],[Node|RNodes]) :-
  remove_nonuser_nodes(Nodes,RNodes).
  
from_to_arc(To+From,(+),To,From).  
from_to_arc(To-From,(-),To,From).  
  
remove_nonuser_arcs(Arcs,RArcs) :-
  remove_nonuser_arcs(Arcs,Arcs,RArcs).

remove_nonuser_arcs([],_AllArcs,[]).
% An incoming arc to a system predicate is removed
remove_nonuser_arcs([Arc|Arcs],AllArcs,RArcs) :-
  from_to_arc(Arc,_Type,To,_From),
  is_system_pred(To),
  !,
  remove_nonuser_arcs(Arcs,AllArcs,RArcs).
% An outcoming arc from a system predicate must be replaced by all arcs with incoming user predicates
remove_nonuser_arcs([Arc|Arcs],AllArcs,RArcs) :-
  from_to_arc(Arc,Type,To,From),
  is_system_pred(From),
  !,
  (Type == (-) -> Neg = Type ; true),
  get_user_arcs_from_node(From,To,AllArcs,Neg,UserArcs),
  remove_nonuser_arcs(Arcs,AllArcs,RemArcs),
  append(UserArcs,RemArcs,RArcs).
% An arc relating no system predicates is kept
remove_nonuser_arcs([Arc|Arcs],AllArcs,[Arc|RArcs]) :-
  remove_nonuser_arcs(Arcs,AllArcs,RArcs).

remove_nonrel_preds_from_strata([],[]).
remove_nonrel_preds_from_strata([(N/A,_)|Nodes],RNodes) :-
  current_db(Connection),
  \+ my_table(Connection,N,A),
  !,
  remove_nonrel_preds_from_strata(Nodes,RNodes).
remove_nonrel_preds_from_strata([Node|Nodes],[Node|RNodes]) :-
  remove_nonrel_preds_from_strata(Nodes,RNodes).

remove_nonrel_preds_from_pdg((Nodes,Arcs),(RNodes,RArcs)) :-
  remove_nonrel_nodes(Nodes,RNodes),
  remove_nonrel_arcs(Arcs,DRArcs),
  remove_duplicates(DRArcs,RArcs).
  
remove_nonrel_nodes([],[]).
remove_nonrel_nodes([N/A|Nodes],RNodes) :-
  current_db(Connection),
  \+ my_table(Connection,N,A),
  !,
  remove_nonrel_nodes(Nodes,RNodes).
remove_nonrel_nodes([Node|Nodes],[Node|RNodes]) :-
  remove_nonrel_nodes(Nodes,RNodes).
  
remove_nonrel_arcs(Arcs,RArcs) :-
  remove_nonrel_arcs(Arcs,Arcs,RArcs).

remove_nonrel_arcs([],_AllArcs,[]).
% An incoming arc to a system predicate is removed
remove_nonrel_arcs([Arc|Arcs],AllArcs,RArcs) :-
  To=N/A,
  from_to_arc(Arc,_Type,To,_From),
  current_db(Connection),
  \+ my_table(Connection,N,A),
  !,
  remove_nonrel_arcs(Arcs,AllArcs,RArcs).
% An outcoming arc from a system predicate must be replaced by all arcs with incoming user predicates
remove_nonrel_arcs([Arc|Arcs],AllArcs,RArcs) :-
  From=N/A,
  from_to_arc(Arc,Type,To,From),
  current_db(Connection),
  \+ my_table(Connection,N,A),
  !,
  (Type == (-) -> Neg = Type ; true),
  get_nonrel_arcs_from_node(From,To,AllArcs,Neg,UserArcs),
  remove_nonrel_arcs(Arcs,AllArcs,RemArcs),
  append(UserArcs,RemArcs,RArcs).
% An arc relating no system predicates is kept
remove_nonrel_arcs([Arc|Arcs],AllArcs,[Arc|RArcs]) :-
  remove_nonrel_arcs(Arcs,AllArcs,RArcs).
  
is_system_pred(N/_A) :-
  is_system_identifier(N).

get_nonrel_arcs_from_node(From,To,AllArcs,Neg,Arcs) :-
  findall(Arc,(
               reach_rel_node(From,[From],AllArcs,Neg,UFrom),
               (Neg == (-) -> Type = (-) ; Type = (+)),
               from_to_arc(Arc,Type,To,UFrom)
              ),
          Arcs).

get_user_arcs_from_node(From,To,AllArcs,Neg,Arcs) :-
  findall(Arc,(
               reach_user_node(From,[From],AllArcs,Neg,UFrom),
               (Neg == (-) -> Type = (-) ; Type = (+)),
               from_to_arc(Arc,Type,To,UFrom)
              ),
          Arcs).

reach_rel_node(From,Traversed,AllArcs,Neg,UFrom) :-
  member(Arc,AllArcs),
  from_to_arc(Arc,Type,From,To),
  (Type == (-) -> Neg = Type ; true),
  current_db(Connection),
  To=N/A,
  (\+ my_table(Connection,N,A)
   ->
    (\+ member(To,Traversed)
     ->
      reach_rel_node(To,[To|Traversed],AllArcs,Neg,UFrom)
     ;
      fail)
   ;
    UFrom=To).
  
reach_user_node(From,Traversed,AllArcs,Neg,UFrom) :-
  member(Arc,AllArcs),
  from_to_arc(Arc,Type,From,To),
  (Type == (-) -> Neg = Type ; true),
  (is_system_pred(To)
   ->
    (\+ member(To,Traversed)
     ->
      reach_user_node(To,[To|Traversed],AllArcs,Neg,UFrom)
     ;
      fail)
   ;
    UFrom=To).
  

%tag_predicates(+PDG,+LocalNodes,+RDBNodes,+REPs,+NCPs) :-
tag_predicates(PDG,LocalNodes,RDBNodes,REPs,NCPs) :-
% WARNING: Include in tags/1 (below) all new tags to delete
  my_set_diff(LocalNodes,RDBNodes,DDBNodes),
  my_set_inter(DDBNodes,RDBNodes,MDBNodes),
  set_flag(local_predicates(LocalNodes)),
  set_flag(ddb_predicates(DDBNodes)),
  set_flag(mdb_predicates(MDBNodes)),
  set_flag(rdb_predicates(RDBNodes)),
  set_flag(restricted_predicates(REPs)),
  set_flag(non_completeable_predicates(NCPs)),
  dependent_restricted_predicates(REPs,PDG,DREPs),
  set_flag(dependent_restricted_predicates(DREPs)),
  user_predicates(PDG,UNAs),
  set_flag(user_predicates(UNAs)),
  recursive_predicates(PDG,RNAs),
  set_flag(recursive_predicates(RNAs)),
  PDG=(Ps,Arcs),
  my_set_diff(Ps,RNAs,NRNAs),
  my_set_diff(Ps,LocalNodes,RDBPs),
  set_flag(non_recursive_predicates(NRNAs)),
  findall(ENA,(member(ENA,NRNAs),extensional(ENA,Arcs)),ENAs),
  my_set_union(ENAs,RDBPs,EPs),
  set_flag(extensional_predicates(EPs)),
  my_set_union(RNAs,NCPs,RNCs),
  nr_nd_predicates(NRNAs,RNCs,PDG,NRNDs),
%   nr_nd_predicates(NRNAs,RNAs,PDG,NRNDs),
  set_flag(nr_nd_predicates(NRNDs)).

% tags([local_predicates(_),
%       ddb_predicates(_),
%       mdb_predicates(_),
%       rdb_predicates(_),
%       restricted_predicates(_),
%       dependent_restricted_predicates(_),
%       user_predicates(_),
%       recursive_predicates(_),
%       non_recursive_predicates(_),
%       extensional_predicates(_),
%       non_completeable_predicates(_),
%       nr_nd_predicates(_)]).

tags([local_predicates(_),
      ddb_predicates(_),
      mdb_predicates(_),
      rdb_predicates(_),
      restricted_predicates(_),
      non_completeable_predicates(_),
      dependent_restricted_predicates(_),
      user_predicates(_),
      recursive_predicates(_),
      non_recursive_predicates(_),
      extensional_predicates(_),
      nr_nd_predicates(_)]).

% update_tag_predicates(+PDG,+LocalNodes1,+RDBNodes1,+REPs1,+NCPs1)
update_tag_predicates(PDG,LocalNodes1,RDBNodes1,REPs1,NCPs1) :-
% WARNING: Include in tags/1 (below) all new tags to delete
  (local_predicates(LocalNodes2) -> true ; LocalNodes2=[]),
  (rdb_predicates(RDBNodes2) -> true ; RDBNodes2=[]),
  (restricted_predicates(REPs2) -> true ; REPs2=[]),
  (non_completeable_predicates(NCPs2) -> true ; NCPs2=[]),
  my_set_union(LocalNodes1,LocalNodes2,LocalNodes),
  my_set_union(RDBNodes1,RDBNodes2,RDBNodes),
  my_set_union(REPs1,REPs2,REPs),
  my_set_union(NCPs1,NCPs2,NCPs),
  tag_predicates(PDG,LocalNodes,RDBNodes,REPs,NCPs).

remove_rdb_pred_from_tag(Pred) :-
  rdb_predicates(Preds),
  my_rdb_pred_remove(Pred,Preds,NPreds),
  set_flag(rdb_predicates(NPreds)).
  
assertz_tags(Ts) :-
  tags(Ts),
  my_map(assertz,Ts).

retractall_tags :-
  tags(Ts),
  my_map(retractall,Ts).
  
current_tags(Ts) :-
  tags(Ts),
  call_list(Ts).
  
restore_tags(Tags) :-
  tags(L),
  my_map_1(retractall,L),
  my_map_1(assertz,Tags).
  
  
empty_tags(Ts) :-
  tags(Ts),
  empty_tag_list(Ts).

empty_tag_list([]).
empty_tag_list([T|Ts]) :-
  T=..[_|[[]]],
  empty_tag_list(Ts).

  
dependent_restricted_predicates(REPs,(Ns,As),DREPs) :-
  my_set_diff(Ns,REPs,FNs),
  findall(Q,(member(P,REPs), member(Q,FNs), once(pdg_path(P,Q,As))),Qs),
  my_set_union(REPs,Qs,DREPs).

dependent_restricted_predicate(N/A) :-
  dependent_restricted_predicates(NAs),
  my_member_chk(N/A,NAs).
  
user_predicate_list([],[]).
user_predicate_list([P|Ps],UPs) :-
  my_builtin_pred(P),
  !,
  user_predicate_list(Ps,UPs).
user_predicate_list([N/_A|Ps],UPs) :-
  is_system_identifier(N),
  !,
  user_predicate_list(Ps,UPs).
user_predicate_list([P|Ps],[P|UPs]) :-
  user_predicate_list(Ps,UPs).

user_predicates((Ns,_As),Ps) :-
%  findall(P,(member(P,Ns),not_builtin(P)),Ps). 
  findall(P,(member(P,Ns),\+ my_builtin_pred(P)),Ps). 
  
user_predicate(N/A) :- 
  user_predicates(NAs),
  my_member_chk(N/A,NAs).
  
recursive_predicate(N/A) :- 
  recursive_predicates(RNAs),
  my_member_chk(N/A,RNAs).
  
extensional_predicate(N/A) :- 
  extensional_predicates(NAs),
  my_member_chk(N/A,NAs).

non_completeable_predicate(N/A) :- 
  non_completeable_predicates(NAs),
  my_member_chk(N/A,NAs).
  
completeable_predicate(N/A) :- 
  \+ non_completeable_predicate(N/A).
   
nr_nd_predicate(N/A) :- 
  nr_nd_predicates(NAs),
  my_member_chk(N/A,NAs).
  
% non_recursive_predicate(N/A) :- 
%   non_recursive_predicates(NAs),
%   my_member_chk(N/A,NAs).
  
% non_recursive_positive_predicate(N/A) :- 
%   non_recursive_positive_predicates(NAs),
%   my_member_chk(N/A,NAs).
  
recursive_predicate(N/A,NAs) :- 
  my_member_chk(N/A,NAs).
  
restricted_predicate(N/A) :- 
  restricted_predicates(Ps),
  my_member_chk(N/A,Ps).
  

% Non-recursive predicate that do not depend on any other recursive predicate
% +NonRecursivePreds,+RecursivePreds,+PDG,-Preds)
% nr_nd_predicates(NRs,Rs,(_Ns,As),NRNDs) :- % Ok, but bad performance
%   findall(P,(member(P,NRs), member(Q,Rs), once(pdg_path(Q,P,As))),NRDs),
%   my_set_diff(NRs,NRDs,NRNDs).
nr_nd_predicates(NRs,Rs,PDG,NRNDs) :-
  findall(P,(
    member(P,NRs), 
    sub_pdg(P,PDG,(Ns,_)),
    my_set_inter(Ns,Rs,[_|_]) 
             ),
          NRDs),
  my_set_diff(NRs,NRDs,NRNDs).
  
recursive_predicates((Ns,As),RNAs) :-
  recursive_predicate_list(Ns,As,[],RNAs).
  
recursive_predicate_list([],_,RNAs,RNAs).
recursive_predicate_list([N|Ns],As,IRNAs,ORNAs) :-
  member(N,IRNAs),
  !,
  recursive_predicate_list(Ns,As,IRNAs,ORNAs).
recursive_predicate_list([N|Ns],As,IRNAs,ORNAs) :-
  recursive_predicate(N,As,[],Visited),
  !,
  my_set_union(Visited,IRNAs,I1RNAs),
  recursive_predicate_list(Ns,As,I1RNAs,ORNAs).
recursive_predicate_list([_N|Ns],As,IRNAs,ORNAs) :-
  recursive_predicate_list(Ns,As,IRNAs,ORNAs).

recursive_predicate(N,As,IVs,OVs) :-
  pdg_path(N,N,As,[],_,IVs,Vs),
  my_set_union(IVs,Vs,OVs).

pdg_path(N1,N2) :-
  pdg((_,As)),
  pdg_path(N1,N2,As).
  
pdg_path(N1,N2,As) :-  
  pdg_path(N1,N2,As,[],_,[],_).
  
pdg_path(F,T,As,IVAs,OVAs,IVNs,OVNs) :-
  pdg_arc(F,T,As,IVAs,OVAs,IVNs,OVNs).
pdg_path(F,T,As,IVAs,OVAs,IVNs,OVNs) :-
  pdg_arc(F,T1,As,IVAs,I1VAs,IVNs,I1VNs),
  pdg_path(T1,T,As,I1VAs,OVAs,I1VNs,OVNs).

pdg_arc(F,T,As,IVAs,[Arc|IVAs],IVNs,[F,T|IVNs]) :-
%  (Arc=T+F ; Arc=T-F),
  from_to_arc(Arc,F,T),
  \+ member(Arc,IVAs),
  member(Arc,As).

% % Given a predicate P and a set of arcs, succeed if there is no negative arc in the sub-pdg for P
% positive_path(N/A,Arcs) :-
%   member(N/A-_,Arcs),
%   !,
%   fail.
% positive_path(N/A,Arcs) :-
%   member(N/A+N1/A1,Arcs),
%   positive_path(N1/A1,Arcs),
%   fail.
% positive_path(_P,_Arcs).

% Given a predicate P and a set of arcs, succeed if it is extensional
extensional(N/A,Arcs) :-
  (member(N/A+_,Arcs)
   ;
   member(N/A-_,Arcs)),
  !,
  fail.
extensional(_P,_Arcs).
   
% get_pdg: Return PDG; build it if not available
get_pdg(PDG) :-
  pdg(PDG),
  !.
get_pdg(PDG) :-
  build_pdg(PDG).

% Return rules for external views 
get_rules_from_external_views(Rules) :-
%%   only on supported DBMS's:
%   current_db(Conn,DBMS),
   current_db(Conn),
%   member(DBMS,[db2,mysql,oracle,postgresql]),
%   !,
  get_viewnames(EVs),
  get_persistent_preds(Nodes),
  my_unzip(Nodes,PVs,_),
  my_set_diff(EVs,PVs,Vs),
  findall(Rs,
    (member(V,Vs),
     my_view(Conn,V,A,SQLst,_,_,_,_,_),
     my_timeout(rules_from_SQL_view(V,A,SQLst,Rs),2,_)
    ),
     Rss),
  concat_lists(Rss,Rules),
  !.
get_rules_from_external_views([]).

rules_from_SQL_view(V,A,SQLst,Rs) :-
  write_info_verb_log(['Processing view ''',V,'''.']),
  catch(sql_to_dl(sql,(SQLst,_),_Schema,_TableRen,DLsts),_M,fail),
  once((DLsts=[':-'(Head,_)|_] ; DLsts=[Head|_])),
  functor(Head,Pred,A),
  replace_predicate_names(Pred,A,V,DLsts,CRs,_),  
%   once((CRs=[':-'(H,_)|_] ; CRs=[H|_])),
%   functor(H,P,A), 
%   replace_functor(P,V,CRs,RCRs),
  preprocess_rdb_view_rules(CRs,V,Rs,RNVss),
  display_compiled_language(sql,RNVss,RNVss),
  !.
rules_from_SQL_view(_V,_A,_SQLst,[]).
  
preprocess_rdb_view_rules([],_,[],[]).
preprocess_rdb_view_rules([R|Rs],V,PRs,RNVss) :-
  preprocess((R,[]),_,_,PRNVss,[],_,_,assert,sql(V),rule,_,[no_safety,replace_eqs],_),
  ruleNVs_to_rule_list(PRNVss,PPRs),
  preprocess_rdb_view_rules(Rs,V,RPRs,RRNVss),
  append(PPRs,RPRs,PRs),
  append(PRNVss,RRNVss,RNVss).

add_ddb_table_nodes([],TNodes,TLocalNodes,Nodes,LocalNodes) :-
  !,
  findall(Relation/Arity,
          (get_relation_arity('$des',Relation,Arity),
           \+ my_builtin_relation(Relation,Arity,_, misc)),
          Preds),
  my_set_union(Preds,TNodes,Nodes),
  my_set_union(Preds,TLocalNodes,LocalNodes).
add_ddb_table_nodes(_Program,Nodes,LocalNodes,Nodes,LocalNodes). 
  
  
% (+AArcs,+PrevNodes,+Warning,+RDBNodes,-Nodes,-Arcs,-LocalNodes,-REPs,-NCPs) :-
pdg_nodes_arcs_from_arcs(AArcs,PrevNodes,Warning,RDBNodes,Nodes,Arcs,LocalNodes,REPs,NCPs) :-
  get_tagged_predicates(AArcs,NArcs,NCPs,REPs,NLocalNodes),
  remove_from_list('$true',NLocalNodes,LocalNodes),
  remove_from_list((_Fact+'$true'),NArcs,RArcs),
  to_neg_arcs(REPs,PrevNodes,RArcs,NegArcs),
  rhs_nodes(NegArcs,RHSNodes),
  lhs_nodes(NArcs,LHSNodes),
  concat_lists([RDBNodes,LHSNodes,RHSNodes],UNodes),
  my_remove_duplicates_sort(UNodes,Nodes),
  remove_neg_dep_rdb(NegArcs,LocalNodes,RDBNodes,Arcs),
  (Warning==no_warning
   ->
    true
   ;
    display_undefined(RDBNodes,PrevNodes,RHSNodes,LHSNodes)).

remove_neg_dep_rdb(Arcs,LNodes,RDBNodes,RArcs) :-
  my_set_diff(RDBNodes,LNodes,Nodes),
  remove_neg_dep_rdb(Arcs,Nodes,DRArcs),
  my_remove_duplicates_sort(DRArcs,RArcs).

remove_neg_dep_rdb([],_Nodes,[]).
remove_neg_dep_rdb([Arc|Arcs],Nodes,[T+F|RArcs]) :-
  from_to_arc(Arc,F,T),
  (memberchk(F,Nodes)
   ;
   memberchk(T,Nodes)),
  !,
  remove_neg_dep_rdb(Arcs,Nodes,RArcs).
remove_neg_dep_rdb([Arc|Arcs],Nodes,[Arc|RArcs]) :-
  remove_neg_dep_rdb(Arcs,Nodes,RArcs).

% Get tagged predicates:
%   - non completeable predicates (top, distinct, ...)
%   - reduced predicates (-).
get_tagged_predicates([],[],[],[],[]).
get_tagged_predicates([local(Arc)|Arcs],NArcs,Ps,REPs,[P1,P2|LNodes]) :-
  tagged_arc_predicates(Arc,P1,P2),
  get_tagged_predicates([Arc|Arcs],NArcs,Ps,REPs,LNodes).
get_tagged_predicates([(nc(P1)+P2)|Arcs],[(P1+P2)|NArcs],[P1,P2|Ps],REPs,LNodes) :-
  !,
  get_tagged_predicates(Arcs,NArcs,Ps,REPs,LNodes).
get_tagged_predicates([(nc(P1)-P2)|Arcs],[(P1-P2)|NArcs],[P1,P2|Ps],REPs,LNodes) :-
  !,
  get_tagged_predicates(Arcs,NArcs,Ps,REPs,LNodes).
get_tagged_predicates([re(REP)+'$true'|Arcs],[REP+'$true'|NArcs],Ps,[REP|REPs],LNodes) :-
  !,
  get_tagged_predicates(Arcs,NArcs,Ps,REPs,LNodes).
get_tagged_predicates([Arc|Arcs],[Arc|NArcs],Ps,REPs,LNodes) :-
  get_tagged_predicates(Arcs,NArcs,Ps,REPs,LNodes).
  
tagged_arc_predicates((nc(P1)+P2),P1,P2) :- !.
tagged_arc_predicates((nc(P1)-P2),P1,P2) :- !.
tagged_arc_predicates((re(P)+'$true'),P,P) :- !.
tagged_arc_predicates(Arc,P1,P2) :- 
  from_to_arc(Arc,P1,P2).

from_to_arc(T+F,F,T).
from_to_arc(T-F,F,T).


% Change to negative labeled arcs those positive arcs with a
% restricted predicate to its right for des mode, and the 
% other way round for hrsql mode
% to_neg_arcs(_,_,Arcs,Arcs) :-
%   system_mode(hrsql),
%   !.
to_neg_arcs(REPs,[],Arcs,NArcs) :-
  !,
  to_neg_arcs(REPs,Arcs,NArcs). 
% Take into account existing known REPs if we are incrementally building the PDG
to_neg_arcs(REPs,_,Arcs,NArcs) :-
  !,
  restricted_predicates(OREPs),
  append(REPs,OREPs,NREPs),
  to_neg_arcs(NREPs,Arcs,NArcs). 
  
to_neg_arcs(_REPs,[],[]).
to_neg_arcs(REPs,[T+F|Arcs],[T-F|NArcs]) :- 
  T\==F,
%   system_mode(des),
  member(F,REPs),
  !,
  to_neg_arcs(REPs,Arcs,NArcs). 
% to_neg_arcs(REPs,[T+F|Arcs],[T-F|NArcs]) :- 
%   T\==F,
%   system_mode(hrsql),
%   member(T,REPs),
%   !,
%   to_neg_arcs(REPs,Arcs,NArcs). 
to_neg_arcs(REPs,[Arc|Arcs],[Arc|NArcs]) :- 
  to_neg_arcs(REPs,Arcs,NArcs). 
  
to_neg_arc(T+F,T-F) :-
  !.
to_neg_arc(Arc,Arc).

regular_nc_arc((P1+P2),(nc(P1)+P2)) :-
  P1\=nc(_),
  !.
regular_nc_arc((P1-P2),(nc(P1)-P2)) :-
  P1\=nc(_),
  !.
regular_nc_arc(Arc,Arc).

get_persistent_dlrules(DLs) :-
  findall(PDLs,datalog_persistent(_,PDLs,_),PDLss),
  concat_lists(PDLss,DLs).

% user_predicates(Preds) :-
%   my_nf_setof(N/A, 
%         H^B^Ls^NVs^RId^CId^FId^Rs^
%           ((datalog(':-'(H,B),NVs,RId,CId,Ls,FId,Rs)
%            ;
%             datalog(H,NVs,RId,CId,Ls,FId,Rs)),
%           functor(H,N,A),
%           N\==(':-')), 
%         Preds).
        
% find_pdg_arcs(Arcs) :-
%   datalog(R,_,_,_,_,_,_), 
%   find_pdg_arcs_from_rule(R,Arcs).
%find_pdg_arcs(local(Arcs),_Rules) :-
find_pdg_arcs(local(Arcs),[]) :-
  datalog(R,_,_,_,_,_,_), 
  find_pdg_arcs_from_rule(R,Arcs).
find_pdg_arcs(Arcs,[Rule|Rules]) :-
  member(R,[Rule|Rules]),
  find_pdg_arcs_from_rule(R,Arcs).
  
find_pdg_arcs_from_rule(R,Arcs) :-
  (R =  ':-'(H,B),
   H \= '-'(_),
   functor(H,N,A), 
   pdg_arcs_from_to(B,N/A,Arcs)
  ;
%   system_mode(des),
   restricted_to_normal_rule(R,NR),
   (find_pdg_arcs_from_rule(NR,Arcs)
    ;
    pred_rule(P,NR),
    Arcs = re(P)+'$true')
  ;
%    system_mode(hrsql),
%    restricted_to_normal_rule(R,':-'(_,B)),
%    pred_rule(P,R),
%    (pdg_arcs_from_to(B,P,PArcs),
%     to_neg_arc(PArcs,Arcs)
%     ;
%     Arcs = re(P)+'$true')
%   ;
   R \= ':-'(_,_),
   R \= '-'(_),
   functor(R,N,A), 
   Arcs = N/A+'$true'
  ).

% restricted_rule(':-'('-'(_H),_B)) :-
%   !.
% restricted_rule('-'(_H).

restricted_to_normal_rule(':-'('-'(H),B),(':-'(H,B))) :-
  !.
restricted_to_normal_rule('-'(H),H) :-
  H \= ':-'(_).
  
to_normal_rule(RR,R) :-
  restricted_to_normal_rule(RR,R),
  !.
to_normal_rule(R,R).

to_normal_rule_list([],[]).
to_normal_rule_list([RR|RRs],[R|Rs]) :-
  to_normal_rule(RR,R),
  to_normal_rule_list(RRs,Rs).
  
% find_pdg_arcs_from_rules(Rules,Arcs) :-
%   member(':-'(H,B),Rules),
%   functor(H,N,A), 
%   pdg_arcs_from_to(B,N/A,Arcs).
find_pdg_arcs_from_rules(Rules,Arcs) :-
  member(Rule,Rules),
  find_pdg_arcs_from_rule(Rule,Arcs).

pdg_arcs_from_to((B,_Bs),P,Arc) :-
  pdg_arc(B,P,Arc).
pdg_arcs_from_to((_B,Bs),P,Arc) :-
  !,
  pdg_arcs_from_to(Bs,P,Arc).
pdg_arcs_from_to(B,P,Arc) :-
  pdg_arc(B,P,Arc).

  
pdg_arc(-(T),P,Arcs) :- 
  !,
  pdg_arc(T,P,Arcs).
pdg_arc(not(T),P,P-N/A) :- 
  functor(T,N,A),
  !.
% pdg_arc(not(T),P,P-N/A) :- 
%   functor(T,N,A).
% pdg_arc(not(T),_P,Arcs) :-
%   !, 
%   functor(T,N,A),
%   pdg_arcs_limited_domain_predicate(N/A,Arcs).
pdg_arc(group_by(T,_Ps,_Vs,_C),P,P-N/A) :- 
  functor(T,N,A).
pdg_arc(group_by(_T,_Ps,_Vs,C),P,Arcs) :- 
  !,
  pdg_arcs_from_to(C,P,Arcs).
% pdg_arc(group_by(_T,_Ps,_Vs,_C),_,_) :- 
%   !,
%   fail.
pdg_arc(group_by(T,Vs,C),P,Arcs) :- 
  !,
  pdg_arc(group_by(T,_Ps,Vs,C),P,Arcs).
% pdg_arc(group_by(_T,_Vs,C),P,P+N/A) :- 
%  user_predicate_in_term(C,N/A).
pdg_arc(st(T),P,nc(P)-N/A) :- % st/1 replaces lj/1, rj/1 and fj/1
  !,
  functor(T,N,A).
pdg_arc(or(L,_R),P,Arcs) :- 
  pdg_arcs_from_to(L,P,Arcs).
pdg_arc(or(_L,R),P,Arcs) :- 
  !,
  pdg_arcs_from_to(R,P,Arcs).
pdg_arc('$diff'(T),P,P+N/A) :- 
  !,
  functor(T,N,A).
pdg_arc(order_by(T,_,_),P,P-N/A) :-      
  !,
  functor(T,N,A).
pdg_arc('=>'(_L,R),P,Arcs) :-  
  pdg_arcs_from_to(R,P,RArcs),
  regular_nc_arc(RArcs,Arcs). 
% pdg_arc('=>'(L,_R),_P,Arcs) :-
%   system_mode(hrsql),
%   rules_from_hyp_program(L,Rules),
%   find_pdg_arcs_from_rules(Rules,Arcs).
pdg_arc('=>'(L,_R),_P,(P+'$true')) :- % Only to avoid undefined predicate warning
  !, 
  rules_from_hyp_program(L,Rules),
  to_normal_rule_list(Rules,NRules),
  pred_rule_list(DPs,NRules),
  remove_duplicates(DPs,Ps),
  member(P,Ps).
% pdg_arc('=>'(L,_R),P,P-NP) :-
%   !, 
%   neg_rules_from_hyp_program(L,NRules),
%   member(NRule,NRules),
%   pred_rule(NP,NRule).
pdg_arc(Aggr,P,P-N/A) :- 
  my_aggregate_relation(AF,Arity),
  Aggr =.. [AF,T|Args],
  length([T|Args],Arity),
  functor(T,N,A), 
  !.
% Positive dependency
pdg_arc(top(_,T),P,nc(P)+N/A) :-      
  !,
  functor(T,N,A).
pdg_arc(distinct(T),P,nc(P)+N/A) :-      
  !,
  functor(T,N,A).
pdg_arc(distinct(_,T),P,nc(P)+N/A) :-      
  !,
  functor(T,N,A).
pdg_arc(T,P,P+N/A) :-      
  functor(T,N,A),
  \+ my_infix_relation(N,_),
  \+ my_infix_comparison(N,_A),
  \+ my_builtin_relation(N,A,_,null),
  \+ my_builtin_relation(N,A,_,misc),
  !.
pdg_arc(_T,P,(P+'$true')). % Only to avoid undefined predicate warning

% Arcs from negated predicate rules in L (L=>R) to rule predicate
% find_pdg_arcs_from_neg_rules_from_hyp_program(L,R,BP-NP) :-
%   neg_rules_from_hyp_program(L,NRules),
%   member(NRule,NRules),
%   pred_rule(NP,NRule),
%   user_predicates_body(R,BPs),
%   member(BP,BPs).
  
% pdg_arcs_limited_domain_predicate(Pred,_Arcs) :-
%   \+ is_limited_domain_predicate(Pred),
%   !,
%   fail.
% pdg_arcs_limited_domain_predicate(N/A,N/A-FN/FA) :-
%   my_foreign_key('$des',N,_FK_AttNames,FN,_PK_AttNames),
%   my_table('$des',FN,FA).
  

rhs_nodes([],[]).
rhs_nodes([Arc|Arcs],[N/A|Nodes]) :-
  (Arc = _P1-N/A; Arc = _P2+N/A),
  rhs_nodes(Arcs,Nodes).

lhs_nodes([],[]).
lhs_nodes([Arc|Arcs],[N/A|Nodes]) :-
  (Arc = N/A-_P1; Arc = N/A+_P2),
  lhs_nodes(Arcs,Nodes).

display_undefined(_RDBNodes,_PrevNodes,_RHSNodes,_LHSNodes) :-
  (undef_pred_warnings(off)
   ;
   tapi(on)),
  !.
display_undefined(RDBNodes,PrevNodes,RHSNodes,LHSNodes) :-
  my_builtin_preds(BIPreds),
  concat_lists([PrevNodes,LHSNodes,BIPreds],LBIPreds),
  findall(Tablename/Arity,my_table('$des',Tablename,Arity),TAs),
  append(LBIPreds,TAs,TPreds),
  append(RDBNodes,TPreds,Preds),
  my_set_diff(RHSNodes,Preds,UndefPred),
  (UndefPred == []
   -> 
    true
   ;
    remove_duplicates_var(UndefPred,RUndefPred),
    my_sort(RUndefPred,SUndefPred),
    (SUndefPred=[_] -> M=predicate ; M=predicates),
    write_warning_log(['Undefined ',M,': ',SUndefPred])).
   
   
% display_undefined_predicates(Query,Type) :-
%   undef_pred_warnings(on),
%   user_predicates_body(Query,Ps),
%   setof(P,(member(P,Ps), \+ declared_predicate(P)),[UP|UPs]),
%   !,
%   (Type==query
%    ->
%     (UPs==[] -> M=predicate ; M=predicates),
%     write_warning_log(['Undefined ',M,': ',[UP|UPs]])
%    ;
%     true % Undefined warnings are already displayed when computing the stratification
%   ),
%   my_unzip([UP|UPs],Names,_),
%   my_map(display_predicate_alternatives,Names).
display_undefined_predicates(Query) :-
  undef_pred_warnings(on),
  user_predicates_body(Query,Ps),
  setof(P,(member(P,Ps), \+ declared_predicate(P)),[UP|UPs]),
  !,
  (UPs==[] -> M=predicate ; M=predicates),
  write_warning_log(['Undefined ',M,': ',[UP|UPs]]),
  my_unzip([UP|UPs],Names,_),
  my_map(display_predicate_alternatives,Names).
display_undefined_predicates(_).
  
declared_predicate(F/Arity) :-
  length(Args,Arity),
  P=..[F|Args],
  current_db(Connection),
  (my_table(Connection,F,Arity)
   ;
   (my_builtin_preds(BIs),
    member(F/Arity,BIs)
    ;
    (datalog(H,_,_,_,_,_,_)
     ; 
     datalog(':-'(H,_),_,_,_,_,_,_)
    ),
    (H=(-P) ; H=P)
   )
  ),
  !.  
  
% display_strata :-
%   system_mode(hrsql),
%   !,
%   display_rel_strata.
display_strata :-
  (strata(S) 
   -> 
    my_mergesort(S,stratum_compare,OS),
    remove_nonuser_preds_from_strata(OS,DS),
    write_log_list([DS,nl])
   ; 
    write_warning_log(['Strata not yet computed.'])
  ).

display_rel_strata :-
  (rel_strata(S)
   ->
    my_mergesort(S,stratum_compare,OS),
    write_log_list([OS,nl])
   ;
    write_warning_log(['Strata not yet computed.'])
  ).
  
rel_strata(S) :-
  strata(CS),
  (CS==[non-stratifiable]
   ->
    S=CS
   ;
    remove_nonrel_preds_from_strata(CS,RS),
    S=RS
%     (system_mode(hrsql)
%      ->
%       my_mergesort(RS,stratum_compare,OS),
%       remove_strata_holes(OS,S)
%      ;
%       S=RS)
  ).
  
remove_strata_holes(RS,S) :-
  remove_strata_holes(RS,0,0,S).
  
remove_strata_holes([],_I0,_J,[]).
remove_strata_holes([(N,I)|Ps],I,J,[(N,J)|RPs]) :-
  !,
  remove_strata_holes(Ps,I,J,RPs).
remove_strata_holes([(N,I)|Ps],_I,J,[(N,J1)|RPs]) :-
  J1 is J+1,
  remove_strata_holes(Ps,I,J1,RPs).
  
% display_strata_for(N/A) :-
%   system_mode(hrsql),
%   !,
%   display_rel_strata_for(N/A).
display_strata_for(N/A) :-
  (strata(S) 
   -> 
    pdg((Ns,As)),
    (member(N/A,Ns)
    ->
      sub_pdg(N/A,(Ns,As),(SNs,_SAs)),
      length(SNs,L),
      length(Sts,L),
      my_zipWith(',',SNs,Sts,RS),
      my_member_list(RS,S),
      my_set_inter(S,RS,IS),
      my_mergesort(IS,stratum_compare,OS),
      remove_nonuser_preds_from_strata(OS,DS),
      write_log_list([DS,nl])
    ;
     write_error_log(['Undeclared predicate']),
     display_object_alternatives(user_predicate,N)
    )
   ; 
    write_warning_log(['Strata not yet computed.'])
  ).

display_rel_strata_for(N/A) :-
  (rel_strata(S) 
   -> 
    rdg((Ns,As)),
    (member(N/A,Ns)
    ->
      sub_pdg(N/A,(Ns,As),(SNs,_SAs)),
      length(SNs,L),
      length(Sts,L),
      my_zipWith(',',SNs,Sts,RS),
      my_member_list(RS,S),
      my_set_inter(S,RS,IS),
      my_mergesort(IS,stratum_compare,OS),
      remove_nonuser_preds_from_strata(OS,DS),
      write_log_list([DS,nl])
    ;
     write_error_log(['Undeclared predicate']),
     display_object_alternatives(user_predicate,N)
    )
   ; 
    write_warning_log(['Strata not yet computed.'])
  ).

% display_pdg :-
%   system_mode(hrsql),
%   !,
%   display_rdg.
display_pdg :-
  (pdg(PDG)
   -> 
    (development(on)
     ->
      RPDG=PDG
     ;
      remove_nonuser_preds_from_pdg(PDG,RPDG)
    ),
    display_pdg(RPDG)
   ; 
    write_info_log(['Predicate dependency graph not yet computed.'])
  ).
  
rdg(RDG) :-
  pdg(PDG),
  remove_nonrel_preds_from_pdg(PDG,RDG).
  
sub_rdg(N/A,SRDG) :-
  rdg(RDG),
  sub_pdg(N/A,RDG,SRDG).
  
display_rdg :-
  (rdg(RDG)
   -> 
    display_pdg(RDG)
   ; 
    write_info_log(['Relation dependency graph not yet computed.'])
  ).
  
% display_sub_pdg_for(N/A) :-
%   system_mode(hrsql),
%   !,
%   display_sub_rdg_for(N/A).
display_sub_pdg_for(N/A) :-
  (pdg((Ns,As))
   -> 
    (member(N/A,Ns)
    ->
     sub_pdg(N/A,(Ns,As),SPDG),
     (development(on)
      ->
       DPDG=SPDG
      ;
       remove_nonuser_preds_from_pdg(SPDG,DPDG)
     ),
     display_pdg(DPDG)
    ;
     write_error_log(['Undeclared predicate']),
     display_object_alternatives(user_predicate,N)
    )
   ; 
    write_info_log(['Predicate dependency graph not yet computed.'])
  ).

display_sub_rdg_for(N/A) :-
  (rdg((Ns,As))
   -> 
    (member(N/A,Ns)
     ->
      sub_pdg(N/A,(Ns,As),SPDG),
      display_pdg(SPDG)
     ;
      write_error_log(['Undeclared relation']),
      display_object_alternatives(relation,N)
    )
   ; 
    write_info_log(['Relation dependency graph not yet computed.'])
  ).

display_pdg((Ns,As)) :-
  tapi(off),
  !,
  write_log_list(['Nodes: ',Ns,nl,'Arcs : ',As,nl]).
display_pdg((Ns,As)) :-
  % TAPI on
  member(N,Ns),
  write_log_list([N,nl]),
  fail
  ;
  write_tapi_delimiter,
  member(A,As),
  A=..[F,K,T],
  write_log_list([F,nl,K,nl,T,nl]),
  fail
  ;
  write_tapi_eot.
  
my_builtin_preds(Ps) :-
  findall(P,my_builtin_pred(P),Ps).
  
my_builtin_pred(N/2) :-
  my_infix_relation(N,_).
my_builtin_pred(N/2) :-
  my_infix_comparison(N,_).
my_builtin_pred(N/A) :-
  my_outer_join_relation(N/A).
% my_builtin_pred(N/1) :- % Outer joins for stratification
%   my_outer_join_relation(N/_A).
my_builtin_pred(N/A) :-
  my_other_builtin_predicate(N/A).

my_other_builtin_predicate(true/0).
my_other_builtin_predicate(false/0).
my_other_builtin_predicate(dual/0).
my_other_builtin_predicate((not)/1).
my_other_builtin_predicate('-'/1).
my_other_builtin_predicate(is_null/1).
my_other_builtin_predicate(is_not_null/1).
my_other_builtin_predicate(select_not_null/3).
my_other_builtin_predicate(top/2).
my_other_builtin_predicate(distinct/1).
my_other_builtin_predicate(distinct/2).
my_other_builtin_predicate(exists/2).
my_other_builtin_predicate(order_by/3).
my_other_builtin_predicate(group_by/3).
my_other_builtin_predicate(group_by/4).
my_other_builtin_predicate(st/1).
my_other_builtin_predicate(call/1).
my_other_builtin_predicate(P/A) :-
  my_aggregate_relation(P,A).

% pdg_arc(ON,DN,[ON+DN|_],ON+DN).
% pdg_arc(ON,DN,[ON-DN|_],ON-DN).
% pdg_arc(ON,DN,[_|Arcs],Arc) :-
%   pdg_arc(ON,DN,Arcs,Arc).
    
  
/*********************************************************************/
/* Building a predicate dependency subgraph: sub_pdg/2, sub_pdg/3    */
/*********************************************************************/

% sub_pdg(+Node,-SubPDG)
% Given a starting node N and the current pdg G, build the subgraph of nodes reachable from N in G
sub_pdg(Node,SubPDG) :-
  pdg(PDG),
  sub_pdg(Node,PDG,SubPDG).

% sub_pdg_list(Nodes,PDGs) :-
%   pdg(PDG),
%   sub_pdg_list(Nodes,PDG,PDGs).
  
sub_pdg_list([],_PDG,[]).
sub_pdg_list([Node|Nodes],PDG,[SubPDG|SubPDGs]) :-
  sub_pdg(Node,PDG,SubPDG),
  sub_pdg_list(Nodes,PDG,SubPDGs).

% Given a list of nodes Ns, return the nodes that can be reached from any node in Ns
reachable_node_list([],[]) :-
  !.
reachable_node_list(Ns,RNs) :-
  pdg(PDG),
  sub_pdg_list(Ns,PDG,PDGs),
  merge_pdg_list(PDGs,(MNs,_As)),
  my_remove_duplicates_sort(MNs,SNs),
  filter_relation_nodes(SNs,RNs).

% Given a starting node N and a pdg G, build the subgraph of nodes reachable from N in G

% sub_pdg(+Node,+PDG,-SubPDG)
% An empty sub-pdg from an empty pdg
%sub_pdg(_N,[],([],[])).
sub_pdg(N,(_Nodes,Arcs),(SNodes,SArcs)) :-
  arcs_from_node(N,Arcs,USArcs),
  my_mergesort(USArcs,SArcs),
  nodes_in(SArcs,DSNodes),
  my_remove_duplicates_sort([N|DSNodes],SNodes).
  
arcs_from_node(N,Arcs,ArcsFromArcs) :-
  arcs_from_node(N,Arcs,ArcsFromNode,RemainingArcs1),
  arcs_from_arcs(ArcsFromNode,RemainingArcs1,SArcs,_RemainingArcs2),
  append(ArcsFromNode,SArcs,ArcsFromArcs),
  !.

% N,Arcs,ArcsFromNode,RemainingArcs
arcs_from_node(_N,[],[],[]).
arcs_from_node(N,[N+DN|Arcs],[N+DN|ArcsFromNode],RemainingArcs) :-
  !,
  arcs_from_node(N,Arcs,ArcsFromNode,RemainingArcs).
arcs_from_node(N,[N-DN|Arcs],[N-DN|ArcsFromNode],RemainingArcs) :-
  !,
  arcs_from_node(N,Arcs,ArcsFromNode,RemainingArcs).
arcs_from_node(N,[Arc|Arcs],ArcsFromNode,[Arc|RemainingArcs]) :-
  arcs_from_node(N,Arcs,ArcsFromNode,RemainingArcs).
  
% ArcsFromNode,Arcs,ArcsFromArcs,RemainingArcs
arcs_from_arcs([],Arcs,[],Arcs).
%arcs_from_arcs([Arc|ArcsNs],Arcs,[Arc|ArcsFromArcs],RemainingArcs) :-
arcs_from_arcs([Arc|ArcsNs],Arcs,ArcsFromArcs,RemainingArcs) :-
  (Arc=_+DN ; Arc=_-DN),
  arcs_from_node(DN,Arcs,NArcs,RemainingArcs1),
  arcs_from_arcs(NArcs,RemainingArcs1,AArcs,RemainingArcs2),
  arcs_from_arcs(ArcsNs,RemainingArcs2,AAArcs,RemainingArcs),
  concat_lists([NArcs,AArcs,AAArcs],ArcsFromArcs).

% Nodes in arcs
nodes_in([],[]).
nodes_in([Arc|Arcs],[F,T|Nodes]) :-
  (Arc=F+T
    ;
   Arc=F-T),
  nodes_in(Arcs,Nodes).


/*********************************************************************/
/* Finding a stratification: stratify/3                              */
/*********************************************************************/
% Follows Ullman, but modified for pdg
% Finds a stratification from a given predicate dependency graph, 
% and returns whether it was successful

stratify((Ns,As),S,Success) :-
  write_info_verb_log(['Computing strata...']),
  (optimize_st(off)
   ->
    lfp_stratify((Ns,As),S,Success),
    set_flag(pdg,(Ns,As))               % Update current PDG
   ;
    (optimize_st(on)
     -> % Optimize strata rec. vs. non-rec. predicates
      change_pos_dep_nr_r_preds(As,MAs),
      lfp_stratify((Ns,MAs),S,Success),
      set_flag(pdg,(Ns,MAs))            % Modify current PDG
     ;  % Maximize strata
      max_stratify((Ns,As),S,Success),
%       (system_mode(hrsql)
%        ->
%         set_flag(pdg,(Ns,As))             % Keep current PDG
%        ;
        change_pos_dep_by_strata(As,S,MAs),
        set_flag(pdg,(Ns,MAs))            % Modify current PDG
%       )
    )
  ),
  !.
  
% Turn each positive dependency (non_recursive_predicate + recursive predicate) into a negative one
change_pos_dep_nr_r_preds([],[]).  
change_pos_dep_nr_r_preds([NR+R|As],[NR-R|MAs]) :-
  recursive_predicates(Rs),
  memberchk(R,Rs),
  \+ member(NR,Rs),
  !,
  change_pos_dep_nr_r_preds(As,MAs).
change_pos_dep_nr_r_preds([A|As],[A|MAs]) :-
  change_pos_dep_nr_r_preds(As,MAs).

change_pos_dep_by_strata([],_S,[]).  
change_pos_dep_by_strata([T+F|As],S,[T-F|MAs]) :-
  pred_stratum(T,S,StT),
  pred_stratum(F,S,StF),
  StT\==StF,
  !,
  change_pos_dep_by_strata(As,S,MAs).
change_pos_dep_by_strata([A|As],S,[A|MAs]) :-
  change_pos_dep_by_strata(As,S,MAs).
  
pred_stratum(P,S,St) :-
  memberchk((P,St),S).
  
pred_stratum(P,St) :-
  strata(S),
  memberchk((P,St),S).
  
lfp_stratify((N,A),S,Success) :-
  assign_1st_stratum(N,FS),
  length(N,Max),
  lfp_recompute_strata(A,FS,S,Max,Success).

assign_1st_stratum([],[]).
assign_1st_stratum([N/A|Ns],[(N/A,1)|SNs]) :-
  assign_1st_stratum(Ns,SNs).

lfp_recompute_strata(A,Si,Sj,Max,Success) :-
  recompute_strata(A,Si,So,Max,false,Change,SSuccess),
  (SSuccess=false -> fail ; true),
  (Change=false
   -> 
    So=Sj, 
    Success=true, 
    !
   ;
    lfp_recompute_strata(A,So,Sj,Max,Success)).
lfp_recompute_strata(_A,S,S,_Max,false).

recompute_strata([],S,S,_Max,C,C,true) :- !. 
recompute_strata([A|As],Si,Sj,Max,Ci,Co,B) :-
  (A = Nt/At-Nf/Af, Add=1 ; A = Nt/At+Nf/Af, Add=0),
  recompute_stratum(Nt/At,Nf/Af,Add,Si,So,Stratum,C1),
  my_or(Ci,C1,C2),
  (Stratum > Max -> fail ; 
   recompute_strata(As,So,Sj,Max,C2,Co,B)).
% recompute_strata(_A,S,S,_Max,false). % Non-stratifiable program

recompute_stratum(Nt/At,Nf/Af,Add,Si,Sj,Stratum,Change) :-
  find_stratum(Nt/At,Si,Stt),
  find_stratum(Nf/Af,Si,Stf),
  IStf is Stf+Add,
  my_max(Stt,IStf,Stratum),
  (Stt=Stratum -> Change=false, Si=Sj; Change=true, reassign_stratum(Nt/At,Stratum,Si,Sj)).

find_stratum(N/A,[(N/A,Stratum)|_Ps],Stratum) :- !.
find_stratum(N/A,[_P|Ps],Stratum) :-
  find_stratum(N/A,Ps,Stratum).

reassign_stratum(N/A,Stratum,[(N/A,_St)|Ps],[(N/A,Stratum)|Ps]) :- !.
reassign_stratum(N/A,Stratum,[P|Ps],[P|NPs]) :-
  reassign_stratum(N/A,Stratum,Ps,NPs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% max_stratify
  
% % Version based on FD constraints
% % max_stratify(+PDG,-Strata,-Success) :-
% max_stratify(([],[]),[],true) :-
%   !.
% max_stratify((Ns,As),S,true) :-
%   length(Ns,N),
%   length(VNs,N),
%   my_fd_domain(VNs,1,N),
%   my_zipWith('=',Ns,VNs,NVs),
%   post_arcs_inequalities(As,As,NVs),
%   post_nodes_disequalities(Ns,As,NVs),
%   yfx_connect_with(VNs,'+',SVNs),
%   Sum is integer((N+N*N)/2),
%   call('#=<'(SVNs,Sum)),
%   triangular_reify(VNs,RVs),
%   (RVs==[]
%    ->
%     true
%    ;
%     !,
%     my_fd_labeling([ff],VNs)
%   ),
%   my_zipWith(',',Ns,VNs,S),
%   !.
% max_stratify(_,_,false).
%   
% post_nodes_disequalities([],_Arcs,_NVs).
% post_nodes_disequalities([_N],_Arcs,_NVs).
% post_nodes_disequalities([N1,N2|Ns],Arcs,NVs) :-
%   distinct_strata_node(N1,[N2|Ns],Arcs,NVs),
%   post_nodes_disequalities([N2|Ns],Arcs,NVs).

% distinct_strata_node(_N,[],_Arcs,_NVs).
% distinct_strata_node(F,[T|Ns],Arcs,NVs) :-
%   \+ (pdg_path(F,T,Arcs),
%       pdg_path(T,F,Arcs)),
%   !,
%   find_name_var(VF,F,NVs),
%   find_name_var(VT,T,NVs),
%   '#\\='(VF,VT),
%   distinct_strata_node(F,Ns,Arcs,NVs).
% distinct_strata_node(F,[_T|Ns],Arcs,NVs) :-
%   distinct_strata_node(F,Ns,Arcs,NVs).

% post_arcs_inequalities([],_Arcs,_NVs).
% post_arcs_inequalities([A|As],Arcs,NVs) :-
%   (
%    (A=T+F, \+ pdg_path(F,T,Arcs), !, I='#<'(VF,VT))
%   ;
%    (A=T+F, I='#=<'(VF,VT)) 
%   ;
%    (A=T-F, I='#<'(VF,VT))
%   ),
%   find_name_var(VF,F,NVs),
%   find_name_var(VT,T,NVs),
%   call(I),
%   post_arcs_inequalities(As,Arcs,NVs).

% triangular_reify([_],[]).
% triangular_reify([V1,V2|Vs],Bs) :-
%   distinct_reify(V1,[V2|Vs],B1s),
%   triangular_reify([V2|Vs],B2s),
%   append(B1s,B2s,Bs).

% distinct_reify(_V,[],[]).
% distinct_reify(V1,[V2|Vs],[B|Bs]) :-
%   '#<=>'('#\\='(V1,V2),B),
%   distinct_reify(V1,Vs,Bs).

% Version adapted from HR-SQL. See credits in des_hrsql.pl
% max_stratify(+PDG,-Strata,-Success) :-
max_stratify(([],[]),[],true) :-
  !.
max_stratify((Ns,As),S,true) :-
  max_stratify_aux(Ns,As,S),
  !.
max_stratify(_,_,false).

% max_stratify_aux([fib1,fib2,fib],[fib+fib1,fib+fib2,fib1+fib,fib2+fib],Strata).
% max_stratify_aux([a,b,c,d,e],[a+b,b+c,c+b,c+d,c+e],Strata).
%         stratify([a,b,c,d,e,f,g],[a=<b,b=<c,c=<b,c=<d,c=<e,a<f,f=<g],St).
% max_stratify_aux([a,b,c,d,e,f,g],[b+a, c+b, b+c, d+c, e+c, f-a,g+f],Strata).

% max_stratify_aux(+Nodes,+Arcs,-Strata) :-
max_stratify_aux(Nodes,[],Strata) :-
  assign_strata_nbr(Nodes,1,Strata).
max_stratify_aux(Nodes,Arcs,Strata):-
  % Strongly connectect components
  my_scc(Nodes,Arcs,Scc),
  % Check for negatively labelled dependency in a SCC = non-stratifiable graph
  findall(X,(member(C,Scc),member(X,C),member(Y,C),(member(X-Y,Arcs);member(Y-X,Arcs))),[]),
  % Collapse each SCC into a single relation (the first one) to avoid cycles
  % and perform the topological sorting.
  collapse_graph(Arcs,Scc,Edges1),
  % Set of vertices of the collapsed graph
  findall(X,member([X|_],Scc),Verts1),
  % Obtain the representation for the library ugraphs
  vertices_edges_to_ugraph(Verts1,Edges1,CollapsedGraph),
  % Topological sort
  top_sort(CollapsedGraph,Sorted),
  % Recover the relations erased in the collapse
  uncollapse_graph(Sorted,Scc,Graph1),
  % Assign strata from 1 to n in the ordering returned by to-sort
  assign_strata_graph(Graph1,1,Strata).

collapse_graph([],_,[]).
collapse_graph([Arc|Rs],Scc,Rs1) :- 
  from_to_arc(Arc,A,B),
  repr(A,Scc,A1), 
  repr(B,Scc,A1),
  !,
  collapse_graph(Rs,Scc,Rs1).
collapse_graph([Arc|Rs],Scc,[A1-B1|Rs1]) :- 
  from_to_arc(Arc,A,B),
	repr(A,Scc,A1), 
	repr(B,Scc,B1),
	collapse_graph(Rs,Scc,Rs1).

repr(A,[[N|Rs]|_],N) :-
  member(A,[N|Rs]),
  !.
repr(A,[_|Rs1],N) :-
  repr(A,Rs1,N).

uncollapse_graph([],_Scc,[]).
uncollapse_graph([N|Rs],Scc,[[N|Ns]|Rs1]) :- 
  member([N|Ns],Scc),
  uncollapse_graph(Rs,Scc,Rs1).

assign_strata_graph(Cs,N,Strata) :-
  assign_strata_graph(Cs,N,[],Strata).

assign_strata_graph([],_N,Strata,Strata).
assign_strata_graph([C|Cs],N,StI,StO) :-
  assign_strata_nbr(C,N,St),
  append(StI,St,StI1),
  N1 is N+1, 
  assign_strata_graph(Cs,N1,StI1,StO).
  
assign_strata_nbr(Nodes,Nbr,Strata) :-
  length(Nodes,L),
  length(Vars,L),
  my_map_1('='(Nbr),Vars),
  my_zipWith(',',Nodes,Vars,Strata).

  
/*********************************************************************/
/* Computing Stratification: compute_stratification/0                */
/*********************************************************************/

compute_stratification :-
  batch(on),
  !.
compute_stratification :-
  clear_stratification,
  build_pdg(G),
  stratify(G,S,Success),
  assert_stratification(S,Success).
  
compute_stratification_silent :-
  push_flag(verbose,off,CurrentValue),
  compute_stratification,
  pop_flag(verbose,CurrentValue).

compute_stratification_verbose :-
  push_flag(verbose,on,CurrentValue),
  compute_stratification,
  pop_flag(verbose,CurrentValue).

update_stratification(G) :-
%  clear_stratification,
  stratify(G,S,Success),
  assert_stratification(S,Success).

assert_stratification(S,Success) :-
  (Success==true
   -> 
    set_flag(strata(S)),
    !
   ; 
    set_flag(strata([non-stratifiable])), 
    !,
    write_warning_log(['Non stratifiable program.'])
  ).

% compute_stratification_if_not_yet_computed :-
%   pdg(([],[])),
%   !,
%   compute_stratification.
% compute_stratification_if_not_yet_computed.

compute_stratification_add_fact(Name/Arity) :-
%  functor(Fact,Name,Arity),
  pdg((Nodes,_Arcs)),
  (member(Name/Arity,Nodes)
   ->
    true
   ;
    compute_stratification
  ).

% compute_stratification_remove_fact(Name/Arity) :-
% %  functor(Fact,Name,Arity),
%   pdg((Nodes,_Arcs)),
%   (member(Name/Arity,Nodes)
%    ->
%     true
%    ;
%     compute_stratification
%   ).

  
/*********************************************************************/
/* Loading Stratification: load_stratification/2                     */
/*********************************************************************/

load_stratification(S,G,T) :-
  clear_stratification, 
  assertz(strata(S)),
  assertz(pdg(G)),
  assertz_tags(T).


/*********************************************************************/
/* Drilling Down Stratification: update_stratification_add_rules/1   */
/*********************************************************************/

% Adding new rules to the database means in general to change PDG
% However, some computations can be saved in building this PDG if only
% new facts are added. 
% Saving is important when many predicates are defined (typically,
% ODBC connections to DB2, Oracle or SQL Server empty databases
% already has defined a lot of views).

update_stratification_add_rules(_) :-
  batch(on),
  !.
update_stratification_add_rules([Fact]) :-
  rule_is_fact(Fact),
  !,
  pred_rule(Node,Fact),
  pdg((Nodes,Arcs)),
  (memberchk(Node,Nodes)
   ->
    % The predicate is already known
    rdb_predicates(RDBNodes),
    (memberchk(Node,RDBNodes)
     ->
      % There was a mate RDB predicate
      remove_from_list(Node,RDBNodes,RRDBNodes),
      set_flag(rdb_predicates(RRDBNodes)),
      mdb_predicates(MDBNodes),
      set_flag(mdb_predicates([Node|MDBNodes])),
      local_predicates(LocalNodes),
      set_flag(local_predicates([Node|LocalNodes]))
     ;
      % There was a DDB fact already for the same predicate
      true
    ),
    (Fact='-'(_),
     pred_rule(P,Fact),
     restricted_predicates(REPs),
     \+ memberchk(P,REPs)
     ->
      to_neg_arcs([P],Arcs,NArcs),
      update_flags_for_restricted_pred(Fact),
      update_stratification((Nodes,NArcs))
     ;
      true
    )
   ; 
    % The fact is added for the first time:
    ddb_predicates(DDBNodes),
    set_flag(ddb_predicates([Node|DDBNodes])),
    local_predicates(LocalNodes),
    set_flag(local_predicates([Node|LocalNodes])),
    user_predicates(UserNodes),
    set_flag(user_predicates([Node|UserNodes])),
    non_recursive_predicates(RecNodes),
    set_flag(non_recursive_predicates([Node|RecNodes])),
    extensional_predicates(ExtNodes),
    set_flag(extensional_predicates([Node|ExtNodes])),
    nr_nd_predicates(NRNDNodes),
    set_flag(nr_nd_predicates([Node|NRNDNodes])),
    update_flags_for_restricted_pred(Fact),
    my_set_union([Node],Nodes,NNodes),
    pred_rule(P,Fact),
    (Fact='-'(_)
     ->
      to_neg_arcs([P],Arcs,NArcs)
     ;
      NArcs=Arcs),
    set_flag(pdg((NNodes,NArcs))),
    strata(S),
    set_flag(strata([(P,1)|S]))
  ).
update_stratification_add_rules(Rules) :-
  update_stratification_add_rules(local,Rules).
  
update_stratification_add_rdb_rules(Rules) :-
  update_stratification_add_rules(rdb,Rules).

update_stratification_add_rules(Source,Rules) :-
  pdg((PrevNodes,PrevArcs)),
  rdb_predicates(RDBNodes),
  (Source==rdb
   ->
    pred_rule_list(PredRules,Rules),
    my_set_union(PredRules,RDBNodes,NRDBNodes),
    build_pdg_from_rules(Rules,PrevNodes,warning,SPDG,NRDBNodes,LocalNodes,REPs,NCPs),
    remove_nonuser_preds_from_pdg(SPDG,PDG1)
%    PDG1=SPDG
%    remove_neg_dep_rdb(NUArcs,LocalNodes,RDBNodes,P1Arcs),
%    PDG1 = (Nodes,P1Arcs)
   ;
    build_pdg_from_local_rules(Rules,PrevNodes,warning,PDG1,RDBNodes,LocalNodes,REPs,NCPs)
   ),
  to_neg_arcs(REPs,PrevArcs,Arcs),
  merge_pdgs(PDG1,(PrevNodes,Arcs),PDG),
  update_tag_predicates(PDG,LocalNodes,RDBNodes,REPs,NCPs),
  update_stratification(PDG).
  

update_flags_for_restricted_pred(-(Fact)) :-
  !,
  pred_rule(Node,Fact),
  restricted_predicates(Ps),
  my_set_union([Node],Ps,REPs),
  set_flag(restricted_predicates(REPs)),
  pdg(PDG),
  dependent_restricted_predicates(REPs,PDG,DREPs),
  set_flag(dependent_restricted_predicates(DREPs)).
update_flags_for_restricted_pred(_Fact).

update_stratification_add_ruleNVs(RNVs) :-
  ruleNVs_to_rule_list(RNVs,Rs),
  update_stratification_add_rules(Rs).
  
  
% update_rdb_pdg_object_action_query(+Object,+Action,+Existed,+SQLst)
% Add a node with create table
update_rdb_pdg_object_action_query(_,_,_,_) :-
  batch(on),
  !.
update_rdb_pdg_object_action_query(table(T),create,_,_Query) :-
  object_name(table(T),TableName),
  (my_odbc_exists_table(TableName) % If the table was probably created:
   ->
    table_arity(TableName,Arity),
    my_odbc_identifier_name(TableName,ODBCTableName),
    add_node_to_pdg(ODBCTableName/Arity), % If the node exists already, this does nothing
    pdg(PDG),
    update_tag_predicates(PDG,[],[ODBCTableName/Arity],[],[])
   ;
    true),
  !.
% Remove a node with drop table
update_rdb_pdg_object_action_query(table(T),drop,_,_Query) :-
%   (my_table('$des',N,A)
%    ->
%     true
%    ;
  object_name_arity(table(T),TableName,Arity),
  (relation_exists(TableName) % If the table was not dropped, do nothing
   ->
    true
   ;
    remove_rdb_node_from_pdg(TableName/Arity),
    remove_rdb_pred_from_tag(TableName/Arity)),
  !.
% Add a node with create view
update_rdb_pdg_object_action_query(view(V),create,_,_SQLst) :-
  object_name(view(V),ViewName),
%   pdg(PDG2),
%   PDG2=(Nodes,_Arcs),
  view_arity(ViewName,Arity),
  my_odbc_identifier_name(ViewName,ODBCViewName),
  (relation_exists(ODBCViewName)
%   \+ rdb_pred_memberchk(ODBCViewName/Arity,Nodes) % If the view was really created with this statement, then:
   -> 
    current_db(Connection),
    get_sql_view_text_from_connection(Connection,ODBCViewName,ODBCSQLstr),
    parse_sql_query((ODBCSQLst,_),ODBCSQLstr,""),
    rules_from_SQL_view(ODBCViewName,Arity,ODBCSQLst,Rs),
    update_stratification_add_rdb_rules(Rs)
   ;
    true),
  !.
% Add or replace a node with create or replace view
update_rdb_pdg_object_action_query(view(V),create_or_replace,false,SQLst) :-
  update_rdb_pdg_object_action_query(view(V),create,false,SQLst), % It didn't exist, so use the create view case
  !.
update_rdb_pdg_object_action_query(view(V),create_or_replace,true,_SQLst) :-
%  fail. % Use the default case
  update_rdb_pdg_object_action_query(view(V),drop,_,_DSQLst),
  update_rdb_pdg_object_action_query(view(V),create,_,_CSQLst).
% Drop a node with drop view (predicates only in DDB or in RDB)
update_rdb_pdg_object_action_query(view(V),drop,_,_Query) :-
  object_name(view(V),ViewName),
%   rdb_predicates(Ps),
%   memberchk(ViewName/Arity,Ps),
  remove_rdb_node_from_pdg(ViewName/Arity),
  remove_rdb_pred_from_tag(ViewName/Arity),
  !.
% Default case for unsupported DB modifications: Do nothing as we don't know what to do
update_rdb_pdg_object_action_query(_,_,_,_) :-
  compute_stratification.
  
merge_pdgs((N1s,A1s),(N2s,A2s),(Ns,As)) :-
  my_set_union(N1s,N2s,Ns),
  my_set_union(A1s,A2s,UAs),
  my_mergesort(UAs,arc_compare,As).

merge_pdg_list([PDG],PDG).
merge_pdg_list([PDG1,PDG2|PDGs],MPDG) :-
  merge_pdgs(PDG1,PDG2,MPDG12),
  merge_pdg_list([MPDG12|PDGs],MPDG).
    
table_pred(T,N/A) :-
  functor(T,N,A).
  
add_node_to_pdg(Node) :-
  pdg((Nodes,_Arcs)),
  memberchk(Node,Nodes),
  !.
add_node_to_pdg(Node) :-
  pdg((Nodes,Arcs)),
  my_set_union([Node],Nodes,NNodes),
  update_stratification((NNodes,Arcs)).
  
remove_rdb_node_from_pdg(Node) :-
  pdg((Nodes,_Arcs)),
  \+ rdb_pred_memberchk(Node,Nodes),
  !.
remove_rdb_node_from_pdg(Node) :-
  pdg((Nodes,Arcs)),
  remove_arcs_for_rdb_node(Node,Arcs,RArcs),
  to_uppercase_arc_list(RArcs,UArcs),
  to_uppercase_pred(Node,UNode),
  ((Arc=_ + UNode ; Arc=_ - UNode),
   memberchk(Arc,UArcs)
   ->
    RNodes=Nodes
   ;
    my_rdb_pred_remove(Node,Nodes,RNodes)
  ),
  RPDG=(RNodes,RArcs),
  update_stratification(RPDG).
  
% remove_arcs_from(_Node,[],[]).
% remove_arcs_from(Node,[Arc|Arcs],RArcs) :-
%   from_to_arc(Arc,_Type,_To,Node),
%   !,
%   remove_arcs_from(Node,Arcs,RArcs).
% remove_arcs_from(Node,[Arc|Arcs],[Arc|RArcs]) :-
%   remove_arcs_from(Node,Arcs,RArcs).

remove_arcs_for_rdb_node(N/A,Arcs,RArcs) :-
  to_uppercase(N,UN),
  remove_arcs_for_rdb_node_aux(UN/A,Arcs,RArcs).
  
remove_arcs_for_rdb_node_aux(_Node,[],[]).
remove_arcs_for_rdb_node_aux(Node,[Arc|Arcs],RArcs) :-
%   from_to_arc(Arc,_Type,To,From),
%   to_uppercase_pred(To,UTo),
%   to_uppercase_pred(From,UFrom),
%   (Node=UTo ; Node=UFrom),
  from_to_arc(Arc,_Type,To,_From),
  to_uppercase_pred(To,Node),
  !,
  remove_arcs_for_rdb_node_aux(Node,Arcs,RArcs).
remove_arcs_for_rdb_node_aux(Node,[Arc|Arcs],[Arc|RArcs]) :-
  remove_arcs_for_rdb_node_aux(Node,Arcs,RArcs).


/*********************************************************************/
/* Rolling Up Stratification: update_stratification_remove_rules/1                */
/*********************************************************************/

update_stratification_remove_rules(_L) :-
  batch(on),
  !.
update_stratification_remove_rules(_L) :- %TODO
  compute_stratification.


/*********************************************************************/
/* Reset Stratification: reset_stratification/0                   */
/*********************************************************************/

reset_stratification :- 
  clear_stratification,
  assertz(strata([])),
  assertz(pdg(([],[]))),
  empty_tags(Ts),
  assertz_tags(Ts).


/*********************************************************************/
/* Clearing Stratification: clear_stratification/0                   */
/*********************************************************************/

clear_stratification :- 
  retractall(strata(_)),
  retractall(pdg(_)),
  retractall_tags.
  
  
/*********************************************************************/
/* Current stratification: current_stratification/1                  */
/*********************************************************************/

current_stratification((Strata,PDG,Tags)) :- 
  strata(Strata),
  pdg(PDG),
  current_tags(Tags).

  
/*********************************************************************/
/* Restore stratification: restore_stratification/1                  */
/*********************************************************************/

restore_stratification((Strata,PDG,Tags)) :- 
  retractall(strata(_)),
  retractall(pdg(_)),
  assertz(strata(Strata)),
  assertz(pdg(PDG)),
  restore_tags(Tags).


/*********************************************************************/
/* Save extension table: save_et/1                                   */
/*********************************************************************/

save_et((ESet,CSet,FSet)) :- 
  findall(et(EFact,EIds,ECId,EIt), et(EFact,EIds,ECId,EIt), ESet),
  findall(called(CFact,CCId), called(CFact,CCId), CSet),
  findall(complete_flag(P,FFact,CF,FCId), complete_flag(P,FFact,CF,FCId), FSet).


/***************************************************************************/
/* Remove extension table restricted to predicates up to CId: remove_et/2  */
/***************************************************************************/

% remove_et(Ps,CId,(ESet,CSet,FSet)) :- 
%   findall(et(EFact,EIds,ECId,EIt), (et(EFact,EIds,ECId,EIt), append(_,ECId,CId), contain_preds(EFact,Ps), my_idx_retract(et(EFact,EIds,ECId,EIt))), ESet),
%   findall(called(CFact,CCId), (called(CFact,CCId), append(_,CCId,CId), contain_preds(CFact,Ps), my_idx_retract(called(CFact,CCId))), CSet),
%   findall(complete_flag(P,FFact,CF,FCId), (complete_flag(P,FFact,CF,FCId), append(_,FCId,CId), contain_preds(FFact,Ps), my_idx_retract(complete_flag(P,FFact,CF,FCId))), FSet).

% contain_preds(F,Ps) :-
%   user_predicates_literal(F,FPs),
%   my_set_inter(Ps,FPs,I),
%   I\==[]. 

/*********************************************************************/
/* Restore extension table: restore_et/1                             */
/*********************************************************************/

restore_et((ESet,CSet,FSet)) :- 
  my_idx_retractall(et(_EFact,_EIds,_ECId,_EIt)),
  my_idx_retractall(called(_CFact,_CCId)),
  my_idx_retractall(complete_flag(_P,_FFact,_CF,_FCId)),
  my_map(my_idx_assertz,ESet),
  my_map(my_idx_assertz,CSet),
  my_map(my_idx_assertz,FSet).


/*********************************************************************/
/* Saving and restoring computation state: restore_et_st/1           */
/*********************************************************************/
 
save_et_st(ET,ST) :-
  save_et(ET),
  current_stratification(ST).

restore_et_st(ET,ST) :-
  retract_hyp_programs_k,
  restore_et(ET),
  restore_stratification(ST).


/*********************************************************************/
/* Add to extension table: add_et/1                                  */
/*********************************************************************/

% add_et((ESet,CSet,FSet)) :- 
%   my_map(my_idx_nf_retract,ESet),
%   my_map(my_idx_nf_retract,CSet),
%   my_map(my_idx_nf_retract,FSet),
%   my_map(my_idx_assertz,ESet),
%   my_map(my_idx_assertz,CSet),
%   my_map(my_idx_assertz,FSet).


/*********************************************************************/
/* Clear extension table: clear_et                                   */
/*********************************************************************/

clear_et :-
  my_idx_retractall(et(_E,_I,_ECId,_EIt)),
  my_idx_retractall(called(_C,_CCId)), 
  my_idx_retractall(complete_flag(_P,_G,_CF,_FCId)).

% Verbosely clearing ET
verb_clear_et :-
  clear_et,
  write_info_verb_log(['Extension table cleared.']).
  
/*********************************************************************/
/* Listing Datalog Rules (Command): list_rules/2                     */
/*********************************************************************/
% Indent (number), Delimiter==delim
% list_rules(I) :-
%   list_rules(I,_D).
  
list_rules(I,D) :-
  list_filtered_rules(I,D,[]).
  
% Indent, Delimiter (delim), Filters (asserted)
list_filtered_rules(I,D,Fs) :-
  list_filtered_rules_wo_number(I,D,Fs,ODLs),
  display_nbr_rules(ODLs).
  
list_rules_wo_number(I,ODLs) :-
  list_filtered_rules_wo_number(I,_D,[],ODLs).

list_filtered_rules_wo_number(I,D,Fs,ODLs) :-
  (development(off)
   ->
    get_filtered_source_dlrules(Fs,DLs)
   ;
    get_filtered_object_dlrules(Fs,DLs)),
  store_elapsed_time(computation),
  my_mergesort(DLs,dlrule_compare_asc,ODLs),
  (development(off)
   ->
    display_source_dlrule_list(ODLs,I,D)
   ;
    display_dlrule_list(ODLs,I,D)).


/*********************************************************************/
/* Listing Datalog Rules matching name and arity (Command): list_rules/4  */
/*********************************************************************/

list_rules(N,A,I,D) :-
  list_filtered_rules(N,A,I,D,[]).
  
list_filtered_rules(N,A,I,D,Fs) :-
  list_filtered_rules_wo_number(N,A,I,D,Fs,ODLs),
  display_nbr_rules(ODLs).
  
list_filtered_rules_wo_number(N,A,I,D,Fs,ODLs) :-
  (development(off)
   ->
    get_filtered_source_dlrules(namearity,N/A,Fs,DLs)
   ;
    get_filtered_object_dlrules(namearity,N/A,Fs,DLs)),
  store_elapsed_time(computation),
  my_mergesort(DLs,dlrule_compare_asc,ODLs),
  (development(off)
   ->
    display_source_dlrule_list(ODLs,I,D)
   ;
    display_dlrule_list(ODLs,I,D)).


/*********************************************************************/
/* Listing Datalog Rules matching a name (Command): list_rules/2     */
/*********************************************************************/

list_rules(N,I,D) :-
  list_filtered_rules(N,I,D,[]).
  
list_filtered_rules(N,I,D,Fs) :-
  list_filtered_rules_wo_number(N,I,D,Fs,DLs),
  display_nbr_rules(DLs).
  
list_filtered_rules_wo_number(N,I,D,Fs,ODLs) :-
  (development(off)
   ->
    get_filtered_source_dlrules(name,N,Fs,DLs)
   ;
    get_filtered_object_dlrules(name,N,Fs,DLs)),
  store_elapsed_time(computation),
  my_mergesort(DLs,dlrule_compare_asc,ODLs),
  (development(off)
   ->
    display_source_dlrule_list(ODLs,I,D)
   ;
    display_dlrule_list(ODLs,I,D)).


/*********************************************************************/
/* Listing Datalog Rules matching a head (Command): list_rules_from_head/3     */
/*********************************************************************/

list_rules_from_head(H,I,D) :-
  list_filtered_rules_from_head(H,I,D,[]).
  
list_filtered_rules_from_head(H,I,D,Fs) :-
  list_filtered_rules_from_head_wo_number(H,I,D,Fs,DLs),
  display_nbr_rules(DLs).
  
list_filtered_rules_from_head_wo_number(H,I,D,Fs) :-
  list_filtered_rules_from_head_wo_number(H,I,D,Fs,_DLs).  
  
list_filtered_rules_from_head_wo_number(H,I,D,Fs,ODLs) :-
  (development(off)
   ->
    get_filtered_source_dlrules(head,H,Fs,DLs)
   ;
    get_filtered_object_dlrules(head,H,Fs,DLs)),
  store_elapsed_time(computation),
  my_mergesort(DLs,dlrule_compare_asc,ODLs),
  (development(off)
   ->
    display_source_dlrule_list(ODLs,I,D)
   ;
    display_dlrule_list(ODLs,I,D)).


/*********************************************************************/
/* Listing Datalog Rules matching a rule (Command): list_rules_from_rule/3     */
/*********************************************************************/

list_rules_from_rule(R,I,D) :-
  list_filtered_rules_from_rule(R,I,D,[]).

list_filtered_rules_from_rule(R,I,D,Fs) :-
  (development(off)
   ->
    get_filtered_source_dlrules(rule,R,Fs,DLs)
   ;
    get_filtered_object_dlrules(rule,R,Fs,DLs)),
  store_elapsed_time(computation),
  my_mergesort(DLs,dlrule_compare_asc,ODLs),
  (development(off)
   ->
    display_source_dlrule_list(ODLs,I,D)
   ;
    display_dlrule_list(ODLs,I,D)),
  display_nbr_rules(ODLs).


/*********************************************************************/
/* Listing Datalog Rule sources matching name and arity (Command): list_sources/2  */
/*********************************************************************/

list_sources(N,A) :-
  (development(off)
   ->
    get_filtered_source_dlrules(namearity,N/A,[],DLs)
   ;
    get_filtered_object_dlrules(namearity,N/A,[],DLs)),
  dlrule_fid_list(DLs,DFIds),
  store_elapsed_time(computation),
  my_remove_duplicates_sort(DFIds,FIds),
  display_set(FIds,yes,no),
  write_tapi_eot.

dlrule_fid_list([],[]).
dlrule_fid_list([datalog(_Rule,_NVs,_RId,_CId,(B,E),FId,_Kind)|DLs],['$file'(F,B,E)|DFIds]) :-
  file_table(F,FId),
  dlrule_fid_list(DLs,DFIds).
dlrule_fid_list([datalog(_Rule,_NVs,_RId,_CId,[],asserted(T),_Kind)|DLs],['$asserted'(T)|DFIds]) :-
  dlrule_fid_list(DLs,DFIds).

/*********************************************************************/
/* Listing Extension Table Contents (Command): list_et               */
/*********************************************************************/

list_et :- 
  (tapi(off) -> [AnsM,CalM]=['Answers:','Calls:'] ; [AnsM,CalM]=['$answers','$calls']),
  write_log_list([AnsM, nl]), 
  list_et_answers,
  write_log_list([CalM, nl]), 
  list_ct_calls,
  nl_tapi_log,
  write_tapi_eot.

list_et_answers :-
  et_entries_by_name_arity(_,_,Bag),
  !,
  display_entries(Bag,OBag,on,yes,yes),
  (tapi(off) -> display_nbr_of_tuples(OBag,'in the answer table',_Error) ; true).
list_et_answers :-
  display_entries([]).

list_ct_calls :-
  ct_entries_by_name_arity(_,_,Bag),
  !,
  display_entries(Bag,OBag,on,yes,yes),
  (tapi(off) -> display_nbr_of_tuples(OBag,'in the call table',_Error) ; true).
list_ct_calls :- 
  display_entries([]).


/*********************************************************************/
/* Listing Extension Table Contents matching a Pattern (Command): list_et/1 */
/*********************************************************************/

list_et(N/A) :- 
  !, 
  (tapi(off) -> [AnsM,CalM]=['Answers:','Calls:'] ; [AnsM,CalM]=['$answers','$calls']),
  write_log_list([AnsM, nl]), 
  list_et_answers(N/A),
  write_log_list([CalM, nl]), 
  list_ct_calls(N/A),
  nl_tapi_log,
  write_tapi_eot.
list_et(N) :- 
  list_et(N/_A).

list_et_answers(N/A) :- 
  et_entries_by_name_arity(N,A,Bag),
  !,
  display_entries(Bag,OBag,on,yes,yes),
  (tapi(off) -> display_nbr_of_tuples(OBag,'in the answer table',_Error) ; true).

list_ct_calls(N/A) :- 
  ct_entries_by_name_arity(N,A,Bag),
  display_entries(Bag,OBag,on,yes,yes),
  (tapi(off) -> display_nbr_of_tuples(OBag,'in the call table',_Error) ; true).

% Get solutions already stored in the extension table for a given Goal
get_solutions(Query,Solutions) :-
  (Query = ':-'(Goal,_) -> true ; Goal = Query),
  et_entries_by_goal(Goal,Solutions).

get_ordered_solutions(Query,Solutions) :-
  get_solutions(Query,USolutions),
  my_sort(USolutions,Solutions).

  
% et_entries_pred gets entries in et for a predicate specified by either
% +Name/?Arity or simply +Name
% Both Pred and not(Pred) are looked for
% Targeted to list_et
et_entries_by_name_arity(N,A,L) :-
  duplicates(off),
  !,
  findall(S,et_entry_by_name_arity(N,A,S),DL),
  my_remove_duplicates_sort(DL,L).
et_entries_by_name_arity(N,A,L) :-
  findall(S,et_entry_by_name_arity(N,A,S),L).

% Looks for a given entry in et for a given predicate name (including not(Pred))
% Upon backtracking, all matching entries are returned
et_entry_by_name_arity(N,A,S) :-
  nonvar(N),
  nonvar(A), % Look for a concrete pattern
  !,
  functor(E,N,A),
  (
   G=E
   ;
   G=not(E)
  ),
  et_entry_by_goal(G,S).
et_entry_by_name_arity(N,A,S) :-
  et_entry_by_goal(E,S), % No specific pattern is given
  (E=not(NS)
   ->
    true
   ;
    NS=E
  ),
  functor(NS,N,A).
  
% ct_entries_by_goal get entries in et for the given goal
% (not(Goal) is not looked for)
% Targeted to get_answers
et_entries_by_goal(Goal,L) :-
  duplicates(off),
  !,
  findall(S,et_entry_by_goal(Goal,S),DL),
  remove_duplicates(DL,L).
et_entries_by_goal(Goal,L) :-
  findall(S,et_entry_by_goal(Goal,S),L).

% Get entry in et matching its first argument, which can be a variable 
et_entry_by_goal(G,S) :-
  et(G,_Ids,[],_It), % Only first computation level
%  et(G,_Ids,_CId), % All computation levels
%  et(G,_Ids),     
  ((development(on)
   ;
    tapi(on))
   ->
    S=G
   ;
    hide_nulls(G,S)
  ).
         
% ct_entries_pred gets entries in et for a predicate specified by either
% +Name/?Arity or simply +Name
% Both Pred and not(Pred) are looked for
% Targeted to list_ct
ct_entries_by_name_arity(N,A,L) :-
  duplicates(off),
  !,
  findall(S,ct_entry_by_name_arity(N,A,S),DL),
  my_remove_duplicates_sort(DL,L).
ct_entries_by_name_arity(N,A,L) :-
  findall(S,ct_entry_by_name_arity(N,A,S),L).

% Looks for a given entry in et for a given predicate name (including not(Pred))
% Upon backtracking, all matching entries are returned
ct_entry_by_name_arity(N,A,S) :-
  nonvar(N),
  nonvar(A), % Look for a concrete pattern
  !,
  functor(E,N,A),
  (
   G=E
   ;
   G=not(E)
  ),
  ct_entry_by_goal(G,S).
ct_entry_by_name_arity(N,A,S) :-
  ct_entry_by_goal(E,S), % No specific pattern is given
  (E=not(NS)
   ->
    true
   ;
    NS=E
  ),
  functor(NS,N,A).
  
% et_entries_by_goal get entries in et for the given goal
% (not(Goal) is not looked for)
% Targeted to get_answers
% ct_entries_by_goal(Goal,L) :-
%   duplicates(off),
%   !,
%   findall(S,ct_entry_by_goal(Goal,S),DL),
%   my_remove_duplicates_sort(DL,L).
% ct_entries_by_goal(Goal,L) :-
%   findall(S,ct_entry_by_goal(Goal,S),L).

% Get entry in et matching its first argument, which can be a variable 
ct_entry_by_goal(G,S) :-
  called(G,_CIds),   % All computation levels
%  called(G),        % Only first computation level
  ((development(on)
   ;
    tapi(on))
   ->
    S=G
   ;
    hide_nulls(G,S)
  ).

  
display_entries(L) :-
  order_answer(O),
  display_entries(L,_OL,O).

display_entries(L,OL) :-
  order_answer(O),
  display_entries(L,OL,O,_R,_D).

% display_entries(L,OL) :-
%   (duplicates(off)
%    ->
%    OL=L % Already sorted
%    ;
%    my_sort(L,OL)
%   ),
%   display_set(OL).
display_entries(L,OL,O) :-
  display_entries(L,OL,O,_R,_D).
  
display_entries(L,OL,O,R,D) :-
  (O==off % No ordering
   ->
   OL=L 
   ;
   my_sort(L,OL)
  ),
  display_set(OL,R,D).
        
/*********************************************************************/
/* Asserting Datalog Rules: assert_rules                             */
/*********************************************************************/
% Assert rules (R,NVs) w.r.t. the tasks Tasks
% Return rule identifiers for object rules resulting from compiling each R
% Return Error=true on error

assert_rules(RNVs,CId,Origin,Tasks,CRNVs,ODLIds,Unsafe,Error) :-
  assert_rules(RNVs,CId,Origin,Tasks,[],CRNVs,[],ODLIds,Unsafe,Error).

assert_rules([],_CId,_Origin,_Tasks,CRNVs,CRNVs,ODLids,ODLids,_Unsafe,_Error).
assert_rules([RNVs|RNVss],CId,Origin,Tasks,ICRNVs,OCRNVs,IODLids,OODLids,Unsafe,Error) :-
  assert_rule(RNVs,CId,Origin,Tasks,TCRNVs,TODLids,Unsafe,Error),
  append(IODLids,TODLids,T2ODLids),
  append(ICRNVs,TCRNVs,T2CRNVs),
  assert_rules(RNVss,CId,Origin,Tasks,T2CRNVs,OCRNVs,T2ODLids,OODLids,Unsafe,Error).

% Assert a rule which may be needed to be transformed
% For /assert command and SQL CREATE VIEW: Without information regarding lines or file ids
assert_rule((Rule,NVs),CId,Origin,Tasks,CRNVs,ODLids,Unsafe,Error) :-
  my_datetime(DT),
  assert_rule((Rule,NVs),CId,CRNVs,[],asserted(DT),assert,Origin,Tasks,ODLids,Unsafe,Error).

% For both asserting/consulting rules(with information regarding lines and file ids)
assert_rule((Rule,NVs),CId,ExRuleNVsList,Ls,FId,_Action,_Origin,_Tasks,ODLids,_Unsafe,Error) :-
% assert_rule((Rule,NVs),CId,ExRuleNVsList,Ls,FId,_Action,_Origin,[],ODLids,_Unsafe,Error) :-
  % Ground facts: No need for asserting modes
  rule_is_fact(Rule),
  my_ground(Rule),
  !,
  (type_cast_fact(Rule,CRule)
   ->
    ExRuleNVsList=[(CRule,NVs)],
    build_datalog_rules((CRule,NVs),CId,ExRuleNVsList,Ls,FId,DLs),
    get_RuleId_list(DLs,ODLids),
    my_assertz_DL_list(DLs,CId,Error)
   ;
    Error=true).
assert_rule(RuleNVs,CId,ExRuleNVsList,Ls,FId,Action,Origin,Tasks,ODLids,Unsafe,Error) :-
  build_input_args(Origin,RuleNVs,IArgs),
  preprocess(RuleNVs,SiRuleNVsList,SfRuleNVsList,ExRuleNVsList,IArgs,_IArgsList,Modes,Action,Origin,rule,Causes,Tasks,Unsafe),
  singleton_warning_list(RuleNVs,ExRuleNVsList,rule,Action,Origin),
  preprocess_task(Tasks,replace_eqs,ReplaceEqs),
  (ReplaceEqs==replace_eqs -> replace_functor('$eq','=',RuleNVs,RRuleNVs); RRuleNVs=RuleNVs),
  assert_compiled_dl_rules(RRuleNVs,CId,SiRuleNVsList,SfRuleNVsList,ExRuleNVsList,Ls,FId,Modes,Causes,ODLids,Unsafe,Error).
  
assert_compiled_dl_rules(RuleNVs,CId,SiRuleNVsList,SfRuleNVsList,ExRuleNVsList,Ls,FId,Modes,Causes,ODLids,Unsafe,Error) :-
  build_datalog_rules(RuleNVs,CId,ExRuleNVsList,Ls,FId,DLs),
  get_RuleId_list(DLs,ODLids),
  language(Lang),
  ((member(safed,Causes)
   ;
    Lang==sql
   ;
    Lang==ra
   )
   ->
    true
   ;
    (show_compilations(on)
     -> 
      (member(exploded,Causes)
       ->
        DRuleNVsList=ExRuleNVsList
        ;
        (member(safed,Causes)
         ->
          DRuleNVsList=SfRuleNVsList
         ;
          (member(simplified,Causes)
           ->
            DRuleNVsList=SiRuleNVsList
           ;
            DRuleNVsList=false
          )
        )
      ),
     ((DRuleNVsList==false ; Lang==drc ; Lang==trc)
      ->
       true
      ;
       write_info_log(['This rule has been translated into:']),
       display_ruleNVs_list(DRuleNVsList,2)
     )
    ;
     true
    )
  ),
  my_assertz_DL_list(DLs,CId,Error),
  % Modes:
  (nonvar(Unsafe),
%   safety_warnings(on)
   language(datalog) % Modes are updated for Datalog rules generated by the user, not by compilations from other languages
   ->
    ODLids=[RId|_], 
    RuleNVs=(Rule,_),
    rule_pred(Rule,Pred),
    set_rule_modes_assertion(RId,Pred,Modes)
   ;
    true).

build_input_args(sql(Name),(Rule,_NVs),IArgs) :-
  pred_rule(N/A,Rule),
  N\==Name,
  !,
  from(1,A,IArgs).
build_input_args(_Origin,_RuleNVs,[]).

% build_datalog_rules(+RuleNVs,+CompId,+RuleNVsList,+Ls,+FId,-DLs)
build_datalog_rules(RuleNVs,CId,[],Ls,FId,[DL]) :-
  !,
  build_datalog_rules(RuleNVs,CId,[RuleNVs],Ls,FId,[DL]).
build_datalog_rules(RuleNVs,CId,[TRuleNVs],Ls,FId,[DL]) :-
  my_equal_up_to_renaming(RuleNVs,TRuleNVs),
  !,
  build_datalog_rule(RuleNVs,CId,Ls,FId,source,DL).
build_datalog_rules(RuleNVs,CId,[(TRule,TNVs)|TRuleNVsList],Ls,FId,[datalog(TRule,TNVs,_Rid,CId,Ls,FId,compilation(Rule,NVs,RuleIds))|DLs]) :-
  (RuleNVs = (':-'(H,B),NVs),
   !,
   Rule = ':-'(H,B)
  ;
   RuleNVs = H,
   Rule = H),
  build_datalog_rule_list(TRuleNVsList,CId,Ls,FId,DLs),
  get_RuleId_list(DLs,RuleIds).
  
build_datalog_rule_list([],_CId,_Ls,_Fid,[]).
build_datalog_rule_list([RuleNVs|RuleNVsList],CId,Ls,FId,[DL|DLs]) :-
  build_datalog_rule(RuleNVs,CId,Ls,FId,compiled,DL),
  build_datalog_rule_list(RuleNVsList,CId,Ls,FId,DLs).

build_datalog_rule((Rule,NVs),CId,Ls,FId,Kind,datalog(Rule,NVs,_RId,CId,Ls,FId,Kind)).
%   term_variables(Rule,TVs),
%   relevant_var_names(NVs,TVs,RNVs),
%   assign_new_var_names(TVs,RNVs,NNVs),
%   append(RNVs,NNVs,DLNVs).

get_RuleNVsId_list([],[]).
get_RuleNVsId_list([datalog(_Rule,_NVs,RuleId,_CId,_Ls,_Fid,_Kind)|DLs],[RuleId|RuleIds]) :-
  get_RuleNVsId_list(DLs,RuleIds).
get_RuleId_list([],[]).
get_RuleId_list([datalog(_Rule,_NVs,RuleId,_CId,_Ls,_Fid,_Kind)|DLs],[RuleId|RuleIds]) :-
  get_RuleId_list(DLs,RuleIds).
  
singleton_warning_list((Rule,NVs),RuleNVss,QueryType,Action,Origin) :-
  singleton_warnings(on),
  member(Origin,[user,datalog]),
  !,
  ruleNVs_to_rule_list(RuleNVss,Rules),
  \+ \+
  (
  make_ground_vars_in_compiled_goals(Rules),
  singleton_var_list(RuleNVss,[],Vs),
  (Vs==[]
   ->
    true
   ;
    (Action == consult
     -> 
      M='Next '
     ;
      M='This '
    ),
    my_unzip(RuleNVss,_,NVss),
    concat_lists(NVss,DANVs),
    my_var_name_list(Vs,DANVs,VsNVs),
    call_list(VsNVs),
    remove_duplicates(Vs,Ns),
    (Ns=[_] -> MV='a singleton variable' ; MV='singleton variables'),
    (QueryType==autoview -> Object = view ; Object = QueryType),
    write_warning_log([M,Object,' has ',MV,': ', Ns]),
    (Action == consult, 
     verbose(off)
     ->
      display_ruleNVs_list([(Rule,NVs)],0)
     ;
      true)
  )
  ).
singleton_warning_list(_,_,_,_,_). % Compilations from SQL and RA are assumed to be correct

singleton_var_list([],Vi,Vi).
singleton_var_list([(Rule,NVs)|RuleNVss],Vi,Vo) :-
  rule_pred(Rule,N/_A),
  \+ is_system_identifier(N),
  !,
  singletons(Rule,SVs),
  remove_anonymous_vars(SVs,NVs,Vs),
  append_new_vars(Vi,Vs,Vo1),
  singleton_var_list(RuleNVss,Vo1,Vo).
% Compiled rules ('$pi') are not checked
singleton_var_list([_|RuleNVss],Vi,Vo) :-
  singleton_var_list(RuleNVss,Vi,Vo).

% The following grounds variables in goals with the form st('$name'(args)) 
% This avoids detection of singleton variables as in the translation of 
% p(X):-lj(t(X),s(Y),X=Y) -> [p(X) :- st('$p0'(X,Y)), ...].

make_ground_vars_in_compiled_goals(T) :- 
  var(T),
  !.
make_ground_vars_in_compiled_goals(st(T)) :-
  T=..[N|_],
  is_system_identifier(N),
  !,
  make_ground(T).
make_ground_vars_in_compiled_goals(T) :-
  T =.. [_F|As],
  make_ground_vars_in_compiled_goals_list(As).

make_ground_vars_in_compiled_goals_list([]).
make_ground_vars_in_compiled_goals_list([T|Ts]) :-
  make_ground_vars_in_compiled_goals(T),
  make_ground_vars_in_compiled_goals_list(Ts).
  

% /*********************************************************************/
% /* Replacing RNVs rules: replace_ruleNVs_list                        */
% /*********************************************************************/

% replace_ruleNVs_list(RNVss,RRNVss) :-
%    ruleNVs_to_rule_list(RNVss,Rs),
%    retract_rule_list(Rs,_Error),
%    assert_rules(RRNVss,[],sql(_Q),[],_,_,_,_).


/*********************************************************************/
/* Retracting Datalog Source Rules: retract_source_dlrule            */
/* Deletes the given source dl rule                                  */ 
/*********************************************************************/

retract_source_dlrule_list([],[],[],_Error).
retract_source_dlrule_list([DL|DLs],[DL|RDLs],RODLs,Error) :-
  retract_source_dlrule(DL,ODLs,RError),
  var(RError),
  !,
  retract_source_dlrule_list(DLs,RDLs,RODLsT,Error),
  append(ODLs,RODLsT,RODLs).
retract_source_dlrule_list([DL|DLs],RDLs,RODLs,true) :-
  write_warning_log(['Rule not retracted:']),
  (development(off) ->
    display_source_dlrule_list([DL],2)
    ;
    display_dlrule_list([DL],2)),
  retract_source_dlrule_list(DLs,RDLs,RODLs,true).

retract_source_dlrule(DL,ODLs,Error) :-
  get_object_dlrules(DL,ODLs),
  retract_dlrule_list(ODLs,Error),
  (Error==true -> 
    reassert_dlrule_list(ODLs)
   ;
    true
   ).
   
reassert_dlrule_list([]).
reassert_dlrule_list([DL|DLs]) :-
  (call(DL) -> 
    true
   ;
    dlrule_cid(DL,CId),
    my_nocheck_assertz_list([DL],CId)
%    assertz(DL)
  ),
  reassert_dlrule_list(DLs).

  
/*********************************************************************/
/* Retracting Datalog Source Rules: retract_source_rule              */
/* Deletes the given source rule                                     */ 
/*********************************************************************/

retract_source_rule(R,Error) :-
  get_filtered_source_dlrules(rule,R,[],SDLs),
  (SDLs==[] -> 
    write_warning_log(['Nothing retracted.'])
    ;
    SDLs = [SDL|_], % Take only the first rule matching R
    retract_source_dlrule(SDL,ODLs,Error),
    (Error== true ->
      true
     ;
      exec_if_verbose_on(
        (development(on) -> 
          length(ODLs,Nbr),
          (Nbr==1 -> S =' ' ; S='s '),
          (Nbr==0 -> D = '.' ; D = (':')),
          write_info_log(['',Nbr,' rule',S,'retracted',D]), 
          display_dlrule_list(ODLs,2)
          ; 
          write_info_log(['Rule retracted.']))),
      clear_et, 
      compute_stratification
    )
  ).
   
/*********************************************************************/
/* Retracting Datalog Rules: retract_rule_list                       */
/* Deletes the given list of object rules                            */ 
/*********************************************************************/

retract_rule_list(Rs,Error) :-
  (nonvar(Rs) -> retract_g_rule_list(Rs,Error) ; true).

retract_g_rule_list([],_Error).
retract_g_rule_list([R|Rs],Error) :-
  retract_rule(R,Error),
  retract_g_rule_list(Rs,Error).

retract_rule((R,_FId),Error) :-
  !,
  retract_rule(R,Error).
retract_rule(R,Error) :-
  (datalog(R,_,_,_,_,_,_), getFileIdsRule(R,FIds) -> retract_fileids(FIds) ; true),
   my_retract(datalog(R,_,_,_,_,_,_),Error),
  !.
  
/*********************************************************************/
/* Retracting Datalog Rules by Identifier:                           */
/* Deletes the dlrules corresponding to given identifiers            */ 
/*********************************************************************/

retract_rule_by_id_list(Ids,no_check,Error) :-
  push_flag(check_ic,off,IC),
  retract_rule_by_id_list(Ids,Error),
  pop_flag(check_ic,IC).

retract_rule_by_id_list([],_Error).
retract_rule_by_id_list([Id|Ids],Error) :-
  retract_rule_by_id(Id,Error),
  retract_rule_by_id_list(Ids,Error).
  
retract_rule_by_id(Id,Error) :-
  findall(DL,(datalog_persistent(_,DLs,_),member(DL,DLs),dlrule_id(DL,Id)),[DL]),
  !,
  my_retract(DL,Error).
retract_rule_by_id(Id,Error) :-
  (datalog(R,NVs,Id,CId,Ls,FId,C)
   ->
%   (getFileIdsRule(R,FIds) -> retract_fileids(FIds) ; true),
   my_retract(datalog(R,NVs,Id,CId,Ls,FId,C),Error),
   update_modes_fileids_retract_DL(datalog(R,NVs,Id,CId,Ls,FId,C))
  ;
   (write_warning_log(['Cannot retract.']),
    Error=true)
  ),
  !.


/*********************************************************************/
/* Retracting Datalog Rules: retract_dlrules                         */
/* Deletes the given list of object dlrules                          */ 
/*********************************************************************/

retract_dlrule_list([],_Error).
retract_dlrule_list([DL|DLs],Error) :-
  retract_dlrule(DL,Error),
  retract_dlrule_list(DLs,Error).

retract_dlrule(datalog(R,NVs,RId,CId,Ls,FId,C),Error) :-
  my_retract(datalog(R,NVs,RId,CId,Ls,FId,C),Error), 
  % If there are no other rules/facts for the same predicate:
  % - retract file identifiers
  % - retract modes
  update_modes_fileids_retract_DL(datalog(R,NVs,RId,CId,Ls,FId,C)).
  
update_modes_fileids_retract_DL(datalog(R,_NVs,RId,_CId,_Ls,FId,_C)) :-
  (R=':-'(H,_B) -> true ; R=H),
  functor(H,F,A),
  functor(OR,F,A),
%  (datalog(OR,_,_,_,_,_,_)
  (once_datalog(OR,_,_,_,_,_,_)
   ->
    % There are other rules/facts. 
    retract_rule_modes_update(RId)
   ;
    % Retract fileids
    (integer(FId)
     ->
      retract_fileids([FId])
     ;
      true),
    drop_modes_assertion(modes(F/A,_))
  ).


/*********************************************************************/
/* Abolishing: abolishDL and friends                                 */
/*********************************************************************/

abolishDL :- 
  abolish_persistent_predicates,
  retractall(datalog(_,_,_,_,_,_,_)), 
  retractall(rule_id(_)),
  enable_rdb_datasources.
  
abolish_persistent_predicates :-
  retractall(datalog_persistent(_,_,_)),
  retract(my_persistent(Connection,PredSchema,_)),
  functor(PredSchema,Name,_Arity),
  drop_persistent_relations(Connection,Name),
  fail.
abolish_persistent_predicates.

drop_persistent_flags(Name/Arity) :-
  !,
  functor(PredSchema,Name,Arity),
  retractall(datalog_persistent(Name/Arity,_PDLs,_UPDLs)),
  retractall(my_persistent(_Connection,PredSchema,_DepNodes)),
  retract((my_view('$des',Name,Arity,_,_,_,_,_,_):-true)).
  
drop_persistent_flags(Name) :-
  datalog_persistent(Name/Arity,_PDLs,_UPDLs),
  drop_persistent_flags(Name/Arity),
  fail.
drop_persistent_flags(_Name).
  
% Abolishing a relation given its name and arity
% abolish_relation(+Name,?Arity)
abolish_relation(N,A) :-
  my_view('$des',N,A,_SQLst,_Lang,_RNVss,_ODLIds,_LVDs,_SCs),
  !,
%  drop_view_k(N).
  drop_view(N).
abolish_relation(N,A) :-
  my_table('$des',N,A),
  !,
%  drop_table_k(N).
  drop_table(N).
abolish_relation(N,A) :-
  (nonvar(A)
   ->
    get_object_dlrules(namearity,N/A,ODLs)
   ;
    get_object_dlrules(name,N,ODLs)
  ),
  (ODLs==[] -> 
    write_warning_log(['Nothing abolished.'])
   ;
    (is_persistent_predicate(N/A)
      ->
       drop_persistent_relations(N),
       drop_persistent_flags(N/A)
      ;
       true
    ),
    (nonvar(A)
     ->
      get_source_dlrules(namearity,N/A,SDLs)
     ;  
      get_source_dlrules(name,N,SDLs))
    ,
    retract_dlrule_list(ODLs,_Error),
    clear_et, 
    update_stratification_remove_rules(ODLs),
    display_tuples_and_nbr_info(SDLs,ODLs)
   ).

/*********************************************************************/
/* Abolishing Modes: abolishModes                                    */
/*********************************************************************/

abolishModes :- 
  retractall(my_modes(_,_)),
  retractall(my_rule_modes(_,_,_)).


/*********************************************************************/
/* Abolishing File Table: abolishFT                                  */
/*********************************************************************/

abolishFT :- 
  retractall(file_table(_F,_Fid)).


/*********************************************************************/
/* Abolishing Integrity Constraints: abolishIC                       */
/*********************************************************************/

abolishIC :- 
  retractall(my_integrity_constraint(_,_,_,_,_,_,_,_)),
  retractall(my_not_nullables(_,_,_)),
  retractall(my_primary_key(_,_,_)),
  retractall(my_candidate_key(_,_,_)),
  retractall(my_foreign_key(_,_,_,_,_,_)),
  retractall(my_functional_dependency(_,_,_,_)).


/*********************************************************************/
/* Clear database: reset_database/0                                  */
/*********************************************************************/
% Remove:
% - Integrity constraints
% - File table
% - Datalog rules
% - Modes
% - Extension table
% - Table definitions
% - View definitions
% - Hypothetical programs
% Reset:
% - Rule identifier seed
% - Null identifier seed
% - Stratification

reset_database :-
  retract_hyp_programs_k,
  abolishIC,
  abolishFT,
  abolishDL,
  abolishModes, 
  clear_et, 
  resetDB,
  reset_ids,
  reset_stratification.
  
resetDB :-
  retractall(my_view(_,_,_,_,_,_,_,_,_)),
  retractall(my_table(_,_,_)),
%  retractall(my_persistent(_,_)), % Already retracted in abolishDL
  retractall(my_attribute(_,_,_,_,_)),
  set_startup_schema.
%   ,
%   assertz(':-'(
% datalog(Rule,NVs,RId,[_Id1,Id2|CId],Ls,FId,Kind),
%   datalog(Rule,NVs,RId,[Id2|CId],Ls,FId,Kind)
%   )).
  
set_startup_schema :-
  assert_system_tables,
  assertz(':-'(
% Tables
my_table(Connection,TableName,Arity),
  (
  opened_db(Connection),
  Connection \== '$des',
  my_odbc_get_table_arity(Connection,TableName,Arity)
  )
  )),
  assertz(':-'(
% Attributes
my_attribute(Connection,Position,RelationName,AttributeName,DESDataType),
  (
  opened_db(Connection),
  Connection \== '$des',
  my_odbc_get_colnames(Connection,RelationName,ColNames),
  my_nth1_member(AttributeName,Position,ColNames),
  my_odbc_get_type(Connection,RelationName,AttributeName,DESDataType)
  )
  )),
  assertz(':-'(
% Views
my_view(ConnectionName,ViewName,Arity,SQLst,sql,[],[],[],[]),
  (
  ConnectionName \== '$des',
  my_table(ConnectionName,ViewName,Arity),
  get_sql_view_text_from_connection(ConnectionName,ViewName,SQLstr),
  parse_sql_query((SQLst,_),SQLstr,"")
  )
  )).

 
assert_system_tables :-
  assert_table_schema(dual,[]),
%  assert_table_schema('DUAL',[]),
  assert_table_schema(select_not_null,[a:_,b:_,c:_]).


/*********************************************************************/
/* Get File Ids for Rule Pattern: getFileIdsRule/2                   */
/*********************************************************************/

getFileIdsRule(R,FIds) :- 
  copy_term(R,CR),
  setof(FId,NVs^RId^CId^Ls^Rs^(datalog(CR,NVs,RId,CId,Ls,FId,Rs)),FIds).


/*********************************************************************/
/* Get File Ids for Head Pattern: getFileIdsHead/2                   */
/*********************************************************************/

%getFileIdsHead(H,FIds) :- 
%  copy_term(H,CH),
%  setof(FId,B^NVs^RId^Ls^Rs^
%          ((datalog(':-'(CH,B),NVs,RId,Ls,FId,Rs); 
%            datalog(CH,NVs,RId,Ls,FId,Rs))),
%        FIds).


/*********************************************************************/
/* Get File Ids: getFileIds/2                                        */
/*********************************************************************/

%getFileIds(N/A,FIds) :- 
%  !,
%  setof(FId,H^B^NVs^RId^Ls^Rs^
%          ((datalog(':-'(H,B),NVs,RId,Ls,FId,Rs); 
%            datalog(H,NVs,RId,Ls,FId,Rs)),functor(H,N,A)),
%        FIds).

/*********************************************************************/
/* Get File Ids: getFileIds/2                                        */
/*********************************************************************/

%getFileIds(N,FIds) :- 
%  setof(FId,H^B^NVs^RId^Ls^A^Rs^
%          ((datalog(':-'(H,B),NVs,RId,Ls,FId,Rs); 
%            datalog(H,NVs,RId,Ls,FId,Rs)), functor(H,N,A)),
%        FIds).

  
/*********************************************************************/
/* Retracting File Table Entries: retract_fileids/2                  */
/*********************************************************************/

retract_fileids([]).
retract_fileids([FId|FIds]) :-
  (datalog(_R,_Vs,_Rid,_CId,_Ls,FId,_Rs)
   ->
    true
   ;
    retractall(file_table(_,FId))),
  retract_fileids(FIds).

  
/*********************************************************************/
/* Displaying                                                        */
/*********************************************************************/

% For ODBC RDB connection:
display_solutions(NRows) :-
  display_answer(on),
%  logiql(off),
  !,
  ((development(on)
   ;
   tapi(on))
    ->
    Rows=NRows
    ;
    hide_nulls(NRows,Rows)),
  display_bag(Rows),
  (display_nbr_of_tuples(on) -> display_nbr_of_tuples(Rows,computed,_Error) ; true).
display_solutions(_) :-
  display_nbr_of_tuples(off),
%   (display_nbr_of_tuples(off)
%    ;
%    logiql(on)),
  !.
display_solutions(Rows) :-
  display_nbr_of_tuples(Rows,computed,_Error).

% For Datalog DDB
display_solutions(G,U,O) :-
  display_answer(on),
%  logiql(off),
  !,
  write_info_verb_log(['Sorting answer...']),
  et_entries_by_goal(G,L),
  display_entries(L,OL,O),
  (display_nbr_of_tuples(on) -> display_notapi_nbr_of_tuples(OL,computed,_Error1) ; true),
  display_undefined_solutions(U),
  (display_nbr_of_tuples(on) -> display_notapi_nbr_of_tuples(U,undefined,_Error2) ; true).
display_solutions(_G,[],_O) :-
  display_nbr_of_tuples(off),
%   (display_nbr_of_tuples(off)
%    ;
%    logiql(on)),
  !.
display_solutions(G,U,_O) :-
  et_entries_by_goal(G,L),
  display_notapi_nbr_of_tuples(L,computed,_Error1),
  display_notapi_nbr_of_tuples(U,undefined,_Error2).

display_notapi_nbr_of_tuples(_N,undefined,_Error) :-
  \+ strata([non-stratifiable]),
  !.
display_notapi_nbr_of_tuples(_N,_M,_Error) :-
  tapi(on),
  !.
display_notapi_nbr_of_tuples(N,M,Error) :-
  display_nbr_of_tuples(N,M,Error).
  
display_nbr_of_tuples(T,M,Error) :-
  (integer(T) -> N=T ; length(T,N)),
  display_tapi_nbr_of_tuples(N,M,Error).
  
display_tapi_nbr_of_tuples(0,undefined,_Error) :-
  !.
display_tapi_nbr_of_tuples(N,_M,Error) :-
  tapi(on),
  !,
  (var(Error) 
   ->
   write_log_list([N,nl])
   ;
   true).
display_tapi_nbr_of_tuples(N,M,_Error) :-
  (N == 1 -> R = tuple ; R = tuples),
  my_spaces(10,S),
  write_info_log([N,' ',R,' ',M,'.',S]).

display_undefined_solutions([]) :-
  !.
display_undefined_solutions(_) :-
  \+ strata([non-stratifiable]),
  !.
display_undefined_solutions(U) :-
  (tapi(off)
   ->
   write_log_list(['Undefined:',nl])
   ;
   write_log_list(['$undefined',nl])
  ),
  (development(on) ->
    HU=U
    ;
    hide_nulls(U,HU)),
  display_entries(HU,_OHU,on).
%  display_entries(HU).
  
display_bag(Ls) :-
  tapi(on),
  !,
  display_tuples(Ls).
display_bag(L) :-
  write_log('{'),
  my_display_list(L),
  write_log_list([nl,'}',nl]).

display_tuples(Ls) :-
  display_tuples(Ls,_,_).

% Lines, Write relname (yes), Write delimiter (no)
display_tuples(Ls,R,D) :-
  member(L,Ls),
  (D==no -> true ; write_tapi_delimiter),
  display_tuple(L,R),
  fail.
display_tuples(_LsR,_R,_D).

display_lines(Ls) :-
  member(L,Ls),
  write_log_list([L,nl]),
  fail.
display_lines(_Ls).

display_tuple(L,R) :-
  L=..[F|As],
  (R==yes -> write_log_list([F,nl]) ; true),
  member(A,As),
  (number(A)
   ->
   write_log_list([A,nl])
   ;
   (var(A)
    ->
     write_log_list([A,nl])
    ;
     (A='$NULL'(_Id)
     ->
     write_log_list([null,nl])
     ;
     write_log_list(['''',A,'''',nl])
     )
   )
  ),
  fail.
display_tuple(_L,_R).

% display_ordered_set(L,KeepDuplicates,OL) :-
%   (KeepDuplicates == keep_duplicates -> 
%     my_sort(L,OL)
%    ;
%     my_remove_duplicates_sort(L,OL)
%   ),
%   display_set(OL).
  
display_set(L) :-
  display_set(L,_,_).
  
display_set(L,R,D) :-
  tapi(on),
  !,
  display_tuples(L,R,D).
display_set(L,_,_) :-
  write_log('{'),
  my_display_list(L),
  write_log_list([nl,'}',nl]).

my_display_list([]).
my_display_list([X]) :-
  write_log_list([nl,'  ']),
  write_log_fresh_NVs(X). 
my_display_list([X,Y|Xs]) :-
  write_log_list([nl,'  ']),
  write_log_fresh_NVs(X),
  write_log(','), 
  my_display_list([Y|Xs]).

display_rule_info(Ls,FId) :-
  format_lines(Ls,FLs),
  (file_table(F,FId)->
    write_log_list(['$tab'(11),'File : ',F,nl,
                    '$tab'(11),'Lines: ',FLs])
   ;
    FId=asserted((Y,M,D,H,Mi,S)),
    write_log_list(['$tab'(11),'Asserted at ',H,':',Mi,':',S,' on ',M,'-',D,'-',Y,'.'])
  ).

format_lines(','(L,L),L) :- !.
format_lines(','(F,T),F-T) :- !.
format_lines(Ls,Ls) :- !.

% Displaying number of rules
display_nbr_rules(_L) :-
  tapi(on),
  !,
  write_tapi_eot.
display_nbr_rules(L) :-
  length(L,N),
  (N==1 -> S = ' listed.' ; S = 's listed.'),
  TInfoList = ['Info: ',N,' rule',S,nl],
  (compact_listings(on) -> InfoList=TInfoList ; InfoList=[nl|TInfoList]),
  write_log_list(InfoList).
  

% Displaying Datalog rules
%display_dlrule_list(DLs) :-
%  display_dlrule_list(DLs,0).

display_source_dlrule_list(DLs,I) :-
  display_source_dlrule_list(DLs,I,_D).

display_source_dlrule_list(DLs,I,D) :-
  source_dlrule_to_ruleNVs_list(DLs,RNVss),
  display_ruleNVs_list(RNVss,I,D).

display_dlrule_list(DLs,I) :-
  display_dlrule_list(DLs,I,_D).
  
display_dlrule_list(DLs,I,D) :-
  dlrule_to_ruleNVs_list(DLs,RNVss),
  display_ruleNVs_list(RNVss,I,D).

display_dlrules_by_ids(Ids,I) :-
  findall(datalog(R,NVs,RId,CId,Ls,FId,C),(member(RId,Ids),datalog(R,NVs,RId,CId,Ls,FId,C)),DLs),
  display_dlrule_list(DLs,I).
  
% display_ruleNVs_list(+Rs,+I,+D)
% Display rules in the format (Head :- Body,NVs), with indentation I, and delimiter for tapi
display_ruleNVs_list(RNVss,I) :-
  display_ruleNVs_list(RNVss,I,_D).
  
display_ruleNVs_list(RNVss,I,D) :-
  (development(on) ->
    DRNVss=RNVss
    ;
    hide_nulls(RNVss,DRNVss)),
  write_rulesNVs_list(DRNVss,I,D).
  
% display_rules_list(+Rs,+I)
% Display rules in the format Head :- Body, with indentation I
display_rules_list(Rs,I) :-
  rule_to_ruleNVs_list(Rs,[],DisplayRules),
  display_ruleNVs_list(DisplayRules,I).
  
% Replaces all occurrences of '$NULL'(Cte) by the constant null in a term T
% only in non-development mode   
development_hide_nulls(T,T) :-
  development(on),
  !.
development_hide_nulls(T,HT) :-
  hide_nulls(T,HT).

 

% Replaces all occurrences of '$NULL'(Cte) by the constant null in a term T
hide_nulls(T,T) :- 
   nulls(off),
   !.
hide_nulls(T,HT) :- 
  my_hide_nulls(T,HT).
   
my_hide_nulls(V,V) :- 
   var(V),
   !.
my_hide_nulls('$NULL'(_ID),null) :- 
  !.
my_hide_nulls(T,T) :- 
  atomic(T),
%  (number(T) ; atom(T)),
  !.
my_hide_nulls(C,RC) :- 
  C =.. [F|As],
  !, 
  my_hide_nulls_list(As,RAs),
  RC =.. [F|RAs].

my_hide_nulls_list([],[]) :-
  !.
my_hide_nulls_list([T|Ts],[RT|RTs]) :-
  !, 
  my_hide_nulls(T,RT), 
  my_hide_nulls_list(Ts,RTs).  
  
write_rulesNVs_list([],_I,_D).
write_rulesNVs_list([RNVs|RNVss],I,D) :-
  write_datalog_rule(RNVs,I),
  nl_log,
  (RNVss\==[] -> write_tapi_delimiter(D) ; true),
  write_rulesNVs_list(RNVss,I,D).

% Rules, no pretty print
write_datalog_rule((':-'(Head,Body),NVs),I) :-
  pretty_print(off),
  !,
  write_indent(I),
  write_with_NVs(Head,NVs), 
  write_log(' :- '),
  write_with_NVs(Body,NVs),
  write_log('.').
% Rules, pretty-print
write_datalog_rule((':-'(Head,Body),NVs),I) :-
  !,
  write_indent(I),
  write_with_NVs(Head,NVs), 
  write_log(' :-'),
  nl_log,
  I1 is I+2,
  write_goals_with_NVs(Body,NVs,I1),
  write_log('.').
% Integrity constraints, no pretty-print
write_datalog_rule((':-'(Body),NVs),I) :-
  pretty_print(off),
  !,
  write_indent(I),
  write_log(':- '),
  write_with_NVs(Body,NVs),
  write_log('.').
% Integrity constraints, pretty-print
write_datalog_rule((':-'(Body),NVs),I) :-
  !,
  write_indent(I),
  write_log(':-'),
  nl_log,
  I1 is I+2,
  write_goals_with_NVs(Body,NVs,I1),
  write_log('.').
% Facts, pretty-print
write_datalog_rule((F,NVs),I) :-
  write_indent(I),
  write_with_NVs(F,NVs),
  write_log('.').

write_rules_with_NVs([Rule|Rules],NVs,I) :-
  write_rules_with_NVs([Rule|Rules],NVs,I),
  write_rules_with_NVs([Rule|Rules],NVs,I).

write_goals_with_NVs(Goals,NVs,I) :-
  write_goals_with_NVs(Goals,NVs,_OP,0,I).

%write_goals_with_NVs(+Goals,+NVs,?OpenParenthesis,+Depth,+Indent).
write_goals_with_NVs((Goal,Goals),NVs,OP,D,I) :-
  !,
%  write_indent(I),
  write_goals_with_NVs(Goal,NVs,OP,D,I),
%  write_with_NVs(Goal,NVs),
  write_log(','),
  nl_log,
  D1 is D+1,
  write_goals_with_NVs(Goals,NVs,_,D1,I).
write_goals_with_NVs(':-'(H,B),NVs,_OP,_D,I) :-
  write_datalog_rule((':-'(H,B),NVs),I).
write_goals_with_NVs('=>'(L,R),NVs,OP,D,I) :-
  !,
  write_goals_with_NVs(L,NVs,OP,D,I),
  nl_log,
  write_indent(I),
  write_log('=>'),
  nl_log,
  write_goals_with_NVs(R,NVs,OP,D,I).
write_goals_with_NVs('/\\'(Rules,Rule),NVs,OP,D,I) :-
  !,
  write_goals_with_NVs(Rules,NVs,OP,D,I),
  nl_log,
  write_indent(I),
  write_log('/\\'),
  nl_log,
  write_goals_with_NVs(Rule,NVs,OP,D,I).
write_goals_with_NVs((Goal;Goals),NVs,OP,D,I) :-
  !,
%  write_indent(I),
  (D>0 -> OP=true ; true),
  write_goals_with_NVs(Goal,NVs,OP,D,I),
  nl_log,
  write_indent(I),
  write_log(';'),
  nl_log,
  D1 is D+1,
  (D>0 -> I1 is I+1 ; I1=I),
  write_goals_with_NVs(Goals,NVs,_OP,D1,I1),
  (D>0 -> write_log(')') ; true).
write_goals_with_NVs(Goal,NVs,OP,_D,I) :-
  write_indent(I),
  (nonvar(OP) -> write_log('(') ; true),
  write_with_NVs(Goal,NVs).

% write_op_goals_with_NVs(Goal,NVs,D,I),
%   (D>0 -> write_log('(') ; true),
%   write_goals_with_NVs(Goal,NVs,D,I).

/*********************************************************************/
/* Consulting a list of Datalog programs (Command): consult_DL_list  */
/*********************************************************************/

consult_DL_list(Files,Success) :-
  consult_DL_list(Files,0,0,false,Success).

consult_DL_list([],NbrRules,NbrCtrs,Success,Success) :-
  display_nbr_consulted_rules(no_log,NbrRules),
  nl_log,
  display_nbr_consulted_ctrs(NbrCtrs).
consult_DL_list([File|Files],NbrRulesi,NbrCtrsi,Si,So) :-
  consult_DL(File,NbrRules,NbrCtrs,S),
  my_or(S,Si,S1),
  NbrRuleso is NbrRulesi+NbrRules,
  NbrCtrso is NbrCtrsi+NbrCtrs,
  consult_DL_list(Files,NbrRuleso,NbrCtrso,S1,So).

display_nbr_running_consulted_rules(Log,NbrRules) :-
  running_info(on),
  output(on),
  verbose(off),
  tapi(off),
  !,
  display_nbr_consulted_rules(Log,NbrRules),
%  write_log_list(['\r']).
  write('\r').
%  write_log_list([nl]).
display_nbr_running_consulted_rules(_Log,_NbrRules).
  
display_nbr_consulted_rules(log,NbrRules) :-
  (output(on)
   ->
    (NbrRules == 1 -> RulesTxt = rule ; RulesTxt = rules),
    write('Info: '),
    write(NbrRules),
    write(' '),
    write(RulesTxt),
    write(' consulted.'),
    flush_output
   ;
    true).
display_nbr_consulted_rules(no_log,NbrRules) :-
  (NbrRules == 1 -> RulesTxt = rule ; RulesTxt = rules),
  write_info_log([NbrRules,' ',RulesTxt,' consulted.','$tbc']),
  flush_output.
  
display_nbr_consulted_ctrs(0) :-
  !.
display_nbr_consulted_ctrs(NbrCtrs) :-
  (NbrCtrs == 1 -> CtrsTxt = constraint ; CtrsTxt = constraints),
  write_info_log([NbrCtrs,' ',CtrsTxt,' consulted.']).
  
display_nbr_running_read_relations(Log,NbrRelations) :-
  running_info(on),
  verbose(on),
  output(on),
  tapi(off),
  !,
  display_nbr_read_relations(Log,NbrRelations),
  write('\r').
display_nbr_running_read_relations(_Log,_NbrRules).

display_nbr_read_relations(log,NbrRelations) :-
  (NbrRelations == 1 -> RelationsTxt = relation ; RelationsTxt = relations),
  write('Info: '),
  write(NbrRelations),
  write(' '),
  write(RelationsTxt),
  write(' read.'),
  flush_output.
display_nbr_read_relations(no_log,NbrRelations) :-
  (NbrRelations == 1 -> RelationsTxt = relation ; RelationsTxt = relations),
  write_info_log([NbrRelations,' ',RelationsTxt,' read.','$tbc']),
  flush_output.

  
/*********************************************************************/
/* Consulting a Datalog Program (Command): consult_DL                */
/*********************************************************************/

consult_DL(F,NbrRules,NbrCtrs,true) :-
  current_input(OldInput),
  try_open(F,CFN,St),       % Try to open file F, which has the complete file name CFN
  assertz(consult(CFN,St)),
  my_new_file_id(CFN,FId),
  write_info_verb_log(['Consulting ',F,'...']),
  consult_DL_rules(FId,0,NbrRules,0,NbrCtrs),
  try_close(St),                % Close the file
  retractall(consult(_,_)),
  set_input(OldInput).
consult_DL(F,0,0,false) :-
  write_log_list(['Reading file ''',F,'''.',nl]),
%  (consult(_,St) -> try_close(St), retractall(consult(_,_)) ; true).
  (consult(_,St) -> try_close(St) ; true).

consult_DL_rules(FId,NbrRulesi,NbrRuleso,NbrCtrsi,NbrCtrso) :-
  % Read a term, along with its variable names and line numbers
  catch(my_read(T,NVs,Ls),Msg,my_read_message(FId,Msg)),
  var(Msg),
  !,
  process_term(T,RNVss,NVs,Ls,FId,consult,TermType,_Unsafe,Error), % Process it, i.e., parse and assert
  incr_nbr_rules_ctrs(Error,TermType,NbrRulesi,NbrRules1,NbrCtrsi,NbrCtrs1),
  ((verbose(on) ; Error == true) -> 
   (development(on) ->
     display_ruleNVs_list(RNVss,2)  % Lists the compiled rules
     ;
     display_ruleNVs_list([(T,NVs)],2)) % Lists the source rule
   ; 
   true), 
  (Error == true ->
    display_rule_info(Ls,FId),
    nl_log
   ;
    true),
%    display_nbr_running_consulted_rules(NbrRules1)),
  (T == end_of_file
   ->
   NbrRuleso = NbrRulesi,
   NbrCtrso = NbrCtrsi
   ;
   (TermType==rule
    ->
    (my_log([])
     ->
      display_nbr_running_consulted_rules(no_log,NbrRules1)
     ;
      display_nbr_running_consulted_rules(log,NbrRules1)
    )
    ;
    true
   ),
    consult_DL_rules(FId,NbrRules1,NbrRuleso,NbrCtrs1,NbrCtrso)
  ).
consult_DL_rules(_Fid,NbrRules,NbrRules,NbrCtrs,NbrCtrs).

incr_nbr_rules_ctrs(Error,_TermType,NbrRules,NbrRules,NbrCtrs,NbrCtrs) :-
  Error==true,
  !.
incr_nbr_rules_ctrs(_Error,TermType,NbrRulesi,NbrRuleso,NbrCtrsi,NbrCtrso) :-
  incr_nbr_rules(TermType,NbrRulesi,NbrRuleso),
  incr_nbr_constraints(TermType,NbrCtrsi,NbrCtrso).
  
incr_nbr_rules(rule,NbrRulesi,NbrRuleso) :-
  !,
  NbrRuleso is NbrRulesi+1.
incr_nbr_rules(_No_rule,NbrRules,NbrRules).
  
incr_nbr_constraints(constraint,NbrCtrsi,NbrCtrso) :-
  !,
  NbrCtrso is NbrCtrsi+1.
incr_nbr_constraints(_No_constraint,NbrCtrs,NbrCtrs).
  
%write_term_list([],_I,_NVs).
%write_term_list([T|Ts],I,NVs) :-
%  my_spaces(I,S),
%  write_log(S),
%  write_with_NVs(T,NVs), 
%  write_log('.'), 
%  nl_log,
%  write_term_list(Ts,I,NVs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Assertions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

my_assertion(Assertion) :-
  Assertion =.. [Name|Args],
  length(Args,Arity),
  assertion(Name,Arity).
  
% :- persistent(PredSchema)
assertion(persistent,1).
% :- persistent(PredSchema,Connection)
assertion(persistent,2).
  
%
% Parsing Datalog assertions
%

parse_datalog_assertion(persistent(PredSchema,Connection),[]) -->
  my_blanks_star,
  ":-",
  my_blanks_star,
  my_kw("PERSISTENT"),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_persistent_assertion_schema(PredSchema),
  my_blanks_star,
  my_optional_connection(Connection),
  ")",
  my_blanks_star.
  
my_persistent_assertion_schema(PredSchema) -->
  my_complete_untyped_schema(PredSchema).
my_persistent_assertion_schema(PredSchema) -->
  my_typed_predicate_schema(PredSchema).
my_persistent_assertion_schema(PredSchema) -->
  my_pattern(Name/Arity),
  {(relation_exists('$des',Name)
    ->
     get_table_typed_arguments('$des',Name,ColnameTypes),
     PredSchema =.. [Name|ColnameTypes],
     (functor(PredSchema,Name,Arity)
      ->
       true
      ;
       my_raise_exception(persistent(Name/Arity),syntax('Persistent assertion arity does not match with existing table declaration:'),[])  
     )
    ;
     my_raise_exception(persistent(Name/Arity),syntax('Cannot add persistent assertion without known schema:'),[])
   )
  }.
  
my_typed_predicate_schema(PredSchema) -->
  my_typed_predicate_schema(Pred,Types),
  {PredSchema=..[Pred|Types]}.
  
my_typed_predicate_schema(Pred,Types) -->
  my_symbol(Pred),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_sequence_of_ctr_types(Types),
  my_blanks_star,
  ")".

my_optional_connection(Connection) -->
  ",",
  !,
  my_blanks_star,
  my_symbol(Connection),
  my_blanks_star.
my_optional_connection(Connection) -->
  [],
  {current_db(Connection)}.

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DATALOG ASSERTIONS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constraints are handled apart
  
assert_assertion(Assertion) :-
  process_datalog_assertion(Assertion,_Ls,_FId,_Action,_Error).

process_datalog_assertion(persistent(PredSchema,Connection),Ls,FId,Action,Error) :-
  process_persistent_datalog_assertion(persistent(PredSchema,Connection),Ls,FId,Action,Error).
process_datalog_assertion(modes(Pred,Modes),Ls,FId,Action,Error) :-
  process_modes_datalog_assertion(modes(Pred,Modes),Ls,FId,Action,Error).
    
% END DATALOG ASSERTIONS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exec_and_show_rdb_sql_list(_Connection,[]).
exec_and_show_rdb_sql_list(Connection,[SQLstr|SQLstrs]) :-
  display_string_list_sql_on([SQLstr]),
  my_odbc_ddl_query(Connection,SQLstr),
  exec_and_show_rdb_sql_list(Connection,SQLstrs).
  
process_term_syntax_error(_Msg) :-
  syntax_error_detected(_,Error),
  !,
  write_error_log(Error),
  reset_syntax_error.
process_term_syntax_error(Msg) :-
  write_error_log(Msg).

%  
% process_term(+Term,-RNVss,+NVs,+Lines,+File_Id,+Action,-TermType,-Unsafe,-Error)
% TermType = end_of_file | assertion | constraint | rule
% Action = consult | assert
%  
process_term(end_of_file,[(end_of_file,[])],_NVs,_Ls,_Fid,_Action,end_of_file,_Unsafe,_Error) :-
  !.
process_term(':-'(Assertion),[],NVs,Ls,FId,Action,assertion,_Unsafe,Error) :-
  my_assertion(Assertion),
  !,
  my_term_to_string(':-'(Assertion),SAssertion,NVs),
  (parse_datalog_assertion(PAssertion,NVs,SAssertion,[])
   ->
    process_datalog_assertion(PAssertion,Ls,FId,Action,Error)
   ;
    process_term_syntax_error(['Syntax error in assertion.']) 
  ).
process_term(':-'(Constraint),[],NVs,Ls,FId,Action,constraint,_Unsafe,Error) :-
  !,
  my_term_to_string(':-'(Constraint),SConstraint,NVs),
  (parse_datalog_constraint(PConstraint,PNVs,SConstraint,[])
   ->
    process_datalog_constraints(PConstraint,PNVs,Ls,FId,Action,Error)
   ;
    process_term_syntax_error(['Syntax error in constraint ',':-'(Constraint)]) 
  ).
process_term(NT,RNVss,NVs,Ls,FId,Action,rule,Unsafe,Error) :-
  (NT=':-'(NHead,NBody),
   T=':-'(Head,Body), !
   ;
   NT=NHead,
   T=Head), 
  my_term_to_string(NHead,SHead,NVs),
  (parse_head(Head,[],HNVs,SHead,[])
   -> 
    check_redef(Head)
   ; 
    process_term_syntax_error(['Syntax error in rule head.']), 
    Error=true
  ),
  (nonvar(NBody)
   ->
    ((my_term_to_string(NBody,SBody,NVs),
      parse_body(Body,HNVs,BNVs,SBody,[]) ->
     true
     ; 
     process_term_syntax_error(['Syntax error in rule body.']), 
     Error=true))
    ; 
    BNVs=HNVs
  ),
%  The following yields to non-termination when safety translations are enabled and syntax error is detected
%  ((var(Error);safe(on)) -> 
  (var(Error) -> 
% nulls were not translated to '$NULL' with the following:
%     assert_rule((NT,BNVs),RNVss,Ls,FId,Action,_Tasks,_ODLIds,_Unsafe,Error)
     append(NVs,BNVs,NNVs),
     assert_rule((T,NNVs),[],RNVss,Ls,FId,Action,datalog,[],_ODLIds,Unsafe,Error)
    ; 
     RNVss=[(NT,NVs)]
  ), 
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Integrity constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Processing an integrity constraint entails:
%  1- Check whether it is correctly declared
%  2- Check whether it holds in the current database
%  3- If so, assert it
  
% Predefined integrity constraints
process_datalog_constraints(Ctr,NVs,Ls,FId,_Action,Error) :-
  my_list_to_tuple(CtrList,Ctr),
  my_map(predef_constraint,CtrList),  % Check that every element in CtrLis is a predefined constraint
  !,
  process_predef_constraints_list(CtrList,NVs,Ls,FId,Error).
% User-defined integrity constraints
process_datalog_constraints(my_integrity_constraint(Preds,Constraint),NVs,Ls,FId,_Action,Error) :-
  process_userdef_constraint(my_integrity_constraint(Preds,Constraint),NVs,Ls,FId,Error).
  
% Process user-defined constraints  
process_userdef_constraint(my_integrity_constraint(Preds,Constraint),NVs,_Ls,_Fid,Error) :-
  build_head_from_body(Constraint,NVs,Head),
  post_table_constraint(_Tablename,my_integrity_constraint('$des',Preds,Constraint,NVs,Head,_Ids,no_sql,'$no_table$'),check,Error).
  
% Supported predefined contraints:  
% Type integrity constraint  
predef_constraint(type(_,_)).
% Not nullable integrity constraint  
predef_constraint(nn(_,_)).
% Primary key integrity constraint  
predef_constraint(pk(_,_)).
% Candidate key integrity constraint  
predef_constraint(ck(_,_)).
% Foreign key integrity constraint  
predef_constraint(fk(_,_,_,_)).
% Functional dependency integrity constraint  
predef_constraint(fd(_,_,_)).

% Process a list of predefined constraints (type, nn, pk, ck, fk, fd)

process_predef_constraints_list([],_NVs,_Ls,_Fid,_Error) :-
  !.
process_predef_constraints_list([Ctr|Ctrs],NVs,Ls,FId,Error) :-
  process_predef_constraint(Ctr,NVs,Ls,FId,Error),
%  Error \== true,
  !,
  process_predef_constraints_list(Ctrs,NVs,Ls,FId,Error).
%process_predef_constraints_list(_,_NVs,_Ls,_Fid,true).


% Process a predefined constraint

%    
% Type integrity constraint (type)
%    
process_predef_constraint(type(Pred,ColnameTypes),_NVs,_Ls,_Fid,_Error) :-
  ColnameTypes=[_C:_T|_CTs],
  !,
  assert_type_ctr(Pred,ColnameTypes),
  write_tapi_success.
process_predef_constraint(type(Pred,Types),_NVs,_Ls,_Fid,_Error) :-
  !,
  build_default_attr_name_type_list(Types,ColnameTypes),
  assert_type_ctr(Pred,ColnameTypes),
  write_tapi_success.
process_predef_constraint(type(Pred,DLTypes),NVs,Ls,FId,true) :-
  !,
  write_error_log(['Invalid type declaration:']),
  display_ruleNVs_list([(type(Pred,DLTypes),NVs)],8),
  display_rule_info(Ls,FId).
%    
% Not nullable integrity constraint (nn)
%    
process_predef_constraint(nn(Pred,Columns),_NVs,_Ls,_Fid,Error) :-
  (my_table('$des',Pred,_Arity)
   ->
    (retract(my_not_nullables('$des',Pred,NAtts)) -> true ; true),
    post_table_constraint(Pred,not_nullables(Columns),check,Error),
    (var(Error)
     ->
      write_info_verb_log(['Not nullable integrity constraint successfully imposed.',nl,'      Resulting schema: ',nl,'$exec'((list_schema(Pred),nl_compact_log))]),
      write_tapi_success
     ;
      (nonvar(Error), nonvar(NAtts) -> assert(my_not_nullables('$des',Pred,NAtts)) ; true)
    )
   ;
    write_error_log(['Relation ',Pred,' has not been typed yet.'])
  ).
%    
% Primary key integrity constraint (pk)
%    
process_predef_constraint(pk(Pred,Columns),_NVs,_Ls,_Fid,Error) :-
  (my_table('$des',Pred,_Arity)
   ->
    post_table_constraint(Pred,primary_key(Columns),check,Error),
    (var(Error)
     ->
      write_info_verb_log(['Primary key integrity constraint successfully imposed.',nl,'      Resulting schema: ',nl,'$exec'((list_schema(Pred),nl_compact_log))]),
      write_tapi_success
     ;
      true
    )
   ;
    write_error_log(['Relation ',Pred,' has not been typed yet.'])
  ).
%    
% Candidate key integrity constraint (ck)
%    
process_predef_constraint(ck(Pred,Columns),_NVs,_Ls,_Fid,Error) :-
  (my_table('$des',Pred,_Arity) ->
    post_table_constraint(Pred,candidate_key(Columns),check,Error),
    (var(Error) ->
      write_info_verb_log(['Candidate key integrity constraint successfully imposed.',nl,'      Resulting schema: ',nl,'$exec'((list_schema(Pred),nl_compact_log))]),
      write_tapi_success
     ;
      true
    )
   ;
    write_error_log(['Relation ',Pred,' has not been typed yet.'])
  ).
%    
% Foreign key integrity constraint (fk)
%    
process_predef_constraint(fk(FKPred,FKColumns,PKPred,PKColumns),_NVs,_Ls,_Fid,Error) :-
  (my_table('$des',FKPred,_FKArity)
   ->
    post_table_constraint(FKPred,foreign_key(FKColumns,PKPred,PKColumns),check,Error),
    (var(Error)
     ->
      write_info_verb_log(['Foreign key integrity constraint successfully imposed.',nl,'      Resulting schema: ',nl,'$exec'((list_schema(FKPred),nl_compact_log))]),
      write_tapi_success
     ;
      true
    )
   ;
    write_error_log(['Relation ',FKPred,' has not been typed yet.'])
  ).
%    
% Functional dependency integrity constraint (fd)
%    
process_predef_constraint(fd(Pred,Columns,DepColumns),_NVs,_Ls,_Fid,Error) :-
  post_table_constraint(Pred,fd(Columns,DepColumns),check,Error),
  (var(Error) ->
    write_info_verb_log(['Functional dependency integrity constraint successfully imposed.',nl,'      Resulting schema: ',nl,'$exec'((list_schema(Pred),nl_compact_log))]),
    write_tapi_success
   ;
    true
  ).

build_default_attr_name_type_list(Types,ColnameTypes) :-
  build_default_attr_name_type_list('$',Types,ColnameTypes).
  
build_default_attr_name_type_list(Symbol,Types,ColnameTypes) :-
  build_default_attr_name_type_list(Symbol,1,Types,ColnameTypes).  
  
build_default_attr_name_type_list(_Symbol,_ColNbr,[],[]).
% Already named:
build_default_attr_name_type_list(Symbol,ColNbr,[Type|Types],[Colname:Type|ColnameTypes]) :-
  nonvar(Colname),
  !,
  ColNbr1 is ColNbr+1,
  build_default_attr_name_type_list(Symbol,ColNbr1,Types,ColnameTypes).
build_default_attr_name_type_list(Symbol,ColNbr,[Type|Types],[Colname:Type|ColnameTypes]) :-
  ensure_atom(ColNbr,AColNbr),
  atom_concat(Symbol,AColNbr,Colname),
  ColNbr1 is ColNbr+1,
  build_default_attr_name_type_list(Symbol,ColNbr1,Types,ColnameTypes).
  
assert_type_ctr(Pred,_ColnameTypes) :-
  (my_not_nullables('$des',Pred,_)
   ;
   my_primary_key('$des',Pred,_)
   ;
   my_candidate_key('$des',Pred,_)
   ;
   my_foreign_key('$des',Pred,_,_,_,_)
   ;
   my_functional_dependency('$des',Pred,_,_)
  ),
  !,
  write_error_log(['Cannot change type assertion while other constraints remain.']).
assert_type_ctr(Pred,ColnameTypes) :-
  length(ColnameTypes,Arity),
  my_table('$des',Pred,Arity1),
  Arity\==Arity1,
  !,
  write_error_log(['Cannot add types to a relation with several arities.']),
  write_log_list(['       Relation: ',Pred,nl]).
assert_type_ctr(Tablename,NewColnameTypes) :-
  length(NewColnameTypes,Arity),
  % Remove the old type schema, if any:
  pop_type_declaration(Tablename,Arity,_DeclaredTypes,DeclaredColnameTypes),
  % Check whether the new type is consistent with the loaded database
  (check_ctr(my_types('$des',Tablename,NewColnameTypes)) ->
    % If consistent, assert new type declaration
    push_type_declaration(Tablename,Arity,NewColnameTypes), 
    Schema=..[Tablename|NewColnameTypes],
    write_info_verb_log(['Types successfully imposed.',nl,'      Resulting schema: ',Schema])
   ;
    % Otherwise, recover the old type schema
    push_type_declaration(Tablename,Arity,DeclaredColnameTypes) % Type error: Restore the old type, if any
  ).
  
% swap_table_definition(Pred,OColnameTypes,ColnameTypes) :-
%   var(ColnameTypes),
%   !,
%   length(OColnameTypes,Arity),
%   my_retract_all_facts(my_table('$des',Pred,Arity)),
%   my_retract_all_facts(my_attribute('$des',_I,Pred,_C,_T)).
% swap_table_definition(Pred,OColnameTypes,ColnameTypes) :-
%   length(ColnameTypes,Arity),
%   (my_table('$des',Pred,Arity) -> 
%     my_retract_all_facts(my_table('$des',Pred,Arity)),
%     get_table_typed_arguments(Pred,OColnameTypes),
%     my_retract_all_facts(my_attribute('$des',_I,Pred,_C,_T))
%    ;
%     true
%   ),
%   assert_table_schema(Pred,ColnameTypes).

assert_limited_domain_rulesNVs(my_foreign_key('$des',TableName,Atts,FTableName,FAtts,RIds)) :-
  my_table('$des',TableName,Arity1),
  length(FAtts,Arity2),
  length(Args,Arity1),
  H=..[TableName|Args],
  build_FK_goal(H,Atts,FTableName,FAtts,_FKVars,G),
  (functor(G,_,Arity2)
   ->
    term_variables(G,GVs),
    my_add_tup_arg_list(is_not_null,GVs,NNGs),
    my_list_to_tuple(NNGs,NNG),
    append_goals_list([H,NNG,not G],Body),
    rule_to_ruleNVs_list([(-H :- Body)],RNVss),
    RNVss=[SRuleNVs]
   ;
    get_new_predicate_name(TableName,N),
    project_tuple(G,FAtts,Vs),
    NG=..[N|Vs],
    my_add_tup_arg_list(is_not_null,Vs,NNGs),
    my_list_to_tuple(NNGs,NNG),
    append_goals_list([H,NNG,not NG],Body),
    append_goals_list([H,NNG,not G],SBody),
    rule_to_ruleNVs_list([(-H :- SBody)],[SRuleNVs]),
    rule_to_ruleNVs_list([(-H :- Body),(NG :- G)],RNVss)
  ),
%  RNVss=[RuleNVs|_],
  my_datetime(DT),
  assert_compiled_dl_rules(SRuleNVs,[],[],RNVss,RNVss,[],asserted(DT),[],[],RIds,_Unsafe,_Error).
%  assert_rules(RNVss,[],datalog,[no_safety],_CRNVs,RIds,_Unsafe,_Error).
  
 
%%%%%%%%%%%%%%%%%%%% 
% DROP ASSERTIONS
%%%%%%%%%%%%%%%%%%%% 

drop_assertion(modes(Pred,Modes)) :-
  drop_modes_assertion(modes(Pred,Modes)).
drop_assertion(persistent(PredSchema,Connection)) :-
  drop_persistent_assertion(persistent(PredSchema,Connection)).

%%%%%%%%%%%%%%%%%%%% 
% DROP CONSTRAINTS
%%%%%%%%%%%%%%%%%%%% 

drop_ic(type(Tablename,ColumnsTypes),_NVs,Error) :-
  length(ColumnsTypes,Arity),
  (my_view(_DB,Tablename,Arity,_SQLst,_Lang,_RNVss,_ODLIds,_LVDs,_SCs)
   ->
    write_warning_log(['Views cannot be untyped.']),
    Error=true
   ;
    (my_table('$des',Tablename,Arity),
     get_table_typed_schema(Tablename,Table),
     Table=..[Tablename|ColumnsTypes]
     ->
     (bagof(Ctr,
      (Ctr=my_not_nullables('$des',Tablename,_NNAtts), call(Ctr)
       ;
       Ctr=my_primary_key('$des',Tablename,_PKAtts), call(Ctr)
       ;
       Ctr=my_candidate_key('$des',Tablename,_CKAtts), call(Ctr)
       ;
       Ctr=my_foreign_key('$des',Tablename,_PAtts,_FKTablename,_FKAtts,_RIds), call(Ctr)
       ;
       Ctr=my_functional_dependency('$des',Tablename,_FDAtts,_FDDetAtts), call(Ctr)
      ), Ctrs)
      ->
       write_error_log(['Cannot retract type declaration.',nl,'Other constraints remain:',nl,Ctrs])
      ;
       retract_table_schema(Tablename,Arity),
       display_ic_dropped
     )
    ;
     display_ic_does_not_exist,
     Error=true
    )
  ).
drop_ic(nn(Tablename,Columns),_NVs,Error) :-
  my_mergesort(Columns,MColumns),
  sort_columns_by_relation_def(Tablename,MColumns,OColumns),
  (my_not_nullables('$des',Tablename,OColumns)
   ->
    my_retract_all_facts(my_not_nullables('$des',Tablename,OColumns)),
    display_ic_dropped
   ;
    display_ic_does_not_exist,
    Error=true).
drop_ic(pk(Tablename,Columns),_NVs,Error) :-
  my_mergesort(Columns,MColumns),
  sort_columns_by_relation_def(Tablename,MColumns,OColumns),
  (my_primary_key('$des',Tablename,OColumns)
   ->
    (bagof([':-'(fk(T,Cs,Tablename,OColumns)),'.',nl],RIds^my_foreign_key('$des',T,Cs,Tablename,OColumns,RIds),FKs)
     ->
      display_ic_not_dropped,
      write_log_list(['Other FK constraints depend on this: ',nl]),
      my_map_1(write_quoted_log_list,FKs),
%      nl_log,
      write_tapi_eot
     ;
      my_retract_all_facts(my_primary_key('$des',Tablename,OColumns)),
      display_ic_dropped)
   ;
    display_ic_does_not_exist,
    Error=true).
drop_ic(ck(Tablename,Columns),_NVs,Error) :-
  my_mergesort(Columns,MColumns),
  sort_columns_by_relation_def(Tablename,MColumns,OColumns),
  (my_candidate_key('$des',Tablename,OColumns)
   ->
    my_retract_all_facts(my_candidate_key('$des',Tablename,OColumns)),
    display_ic_dropped
   ;
    display_ic_does_not_exist,
    Error=true).
drop_ic(fk(Tablename,Columns,RTablename,RColumns),_NVs,Error) :-
  % Attributes must not be sorted, they are left "as is"
  % For other predefined constraints, sorting helps in removing duplicates
  (my_foreign_key('$des',Tablename,Columns,RTablename,RColumns,RIds)
   ->
    my_table('$des',Tablename,Arity),
    (is_limited_domain_predicate(Tablename/Arity)
     ->
      my_retract_all_facts(my_foreign_key('$des',Tablename,Columns,RTablename,RColumns,RIds)),
      retract_rule_by_id_list(RIds,_Error),
      compute_stratification, 
      clear_et,
      limited_domain_predicates(Ps),
      remove_from_list(Tablename/Arity,Ps,RPs),
      set_flag(limited_domain_predicates(RPs)),
      write_info_verb_log(['Predicate ',Tablename/Arity,' is not limited domain anymore.'])
     ;
      my_retract_all_facts(my_foreign_key('$des',Tablename,Columns,RTablename,RColumns,RIds))
    ),
    display_ic_dropped
   ;
    display_ic_does_not_exist,
    Error=true).
drop_ic(fd(Tablename,Columns,DepColumns),_NVs,Error) :-
  my_mergesort(Columns,MColumns),
  my_mergesort(DepColumns,MDepColumns),
  sort_columns_by_relation_def(Tablename,MColumns,OColumns),
  sort_columns_by_relation_def(Tablename,MDepColumns,ODepColumns),
  (my_functional_dependency('$des',Tablename,OColumns,ODepColumns)
   ->
    my_retract_all_facts(my_functional_dependency('$des',Tablename,OColumns,ODepColumns)),
    display_ic_dropped
   ;
    display_ic_does_not_exist,
    Error=true).
% From assertion:
drop_ic(my_integrity_constraint(Preds,Constraint),NVs,Error) :-
  drop_ic(my_integrity_constraint(_DB,Preds,Constraint,NVs,_Head,_Ids,_SQL,_TableName),NVs,Error).
% From SQL drop constraint:
drop_ic(my_integrity_constraint(DB,Preds,Constraint,NVs,Head,Ids,SQL,TableName),NVs,Error) :-
  (my_integrity_constraint(DB,Preds,Constraint,NVs,Head,Ids,SQL,TableName)
   ->
    retract(my_integrity_constraint(DB,Preds,Constraint,NVs,Head,Ids,SQL,TableName)),
    retract_rule_by_id_list(Ids,_Error2),
    compute_stratification,
    % No need to clear_et
    display_ic_dropped
   ;
    display_ic_does_not_exist,
    Error=true).
    
drop_constraints(_TableName,[]).
drop_constraints(TableName,[Ctr|Ctrs]) :-
  drop_constraint(TableName,Ctr),
  !,
  drop_constraints(TableName,Ctrs).
drop_constraints(TableName,[_Ctr|Ctrs]) :-
  drop_constraints(TableName,Ctrs).
  
drop_constraint(TableName,not_nullables(Columns)) :-
  my_retract_all_facts(my_not_nullables('$des',TableName,Columns)).
drop_constraint(TableName,primary_key(Columns)) :-
  my_retract_all_facts(my_primary_key('$des',TableName,Columns)).
drop_constraint(TableName,candidate_key(Columns)) :-
  my_retract_all_facts(my_candidate_key('$des',TableName,Columns)).
drop_constraint(TableName,foreign_key(Columns,RTableName,RColumns)) :-
  my_retract_all_facts(my_foreign_key('$des',TableName,Columns,RTableName,RColumns,_RIds)).
drop_constraint(TableName,fd(Columns,DepColumns)) :-
  my_retract_all_facts(my_functional_dependency('$des',TableName,Columns,DepColumns)).
drop_constraint(TableName,my_sql_check_constraint(SQL)) :-
 (retract(my_integrity_constraint('$des',_Preds,_Constraint,_NVs,_Head,Ids,SQL,TableName))
  ->
   retract_rule_by_id_list(Ids,_Error2)
  ;
   true).

drop_all_ic(TableName/_Arity) :-
  findall(_,
    (my_integrity_constraint(_DB,Preds,Constraint,NVs,_Head,_Ids,_SQL,TableName),
     drop_ic(my_integrity_constraint(Preds,Constraint),NVs,_Error)),
    _).
    
display_ic_dropped :-
  write_info_verb_log(['Constraint dropped.']),
  write_tapi_success.
  
display_ic_not_dropped :-
  write_error_log(['Constraint has not been dropped.',nl]).
  
display_ic_does_not_exist :-
  write_error_log(['Constraint does not exist.']).
  
/*******************************************************************/
/* File ids                                                        */
/*******************************************************************/
 
my_new_file_id(F,FId) :-
  file_table(F,FId), 
  !,
  write_verb(['Warning: Reloading an already loaded program.',nl,
              '         References to source program may have changed.',nl]).
my_new_file_id(F,FId) :-
  my_max_fid(MaxFid),
  FId is MaxFid+1,
  assertz(file_table(F,FId)).

my_max_fid(MaxFid) :-
  setof(FId,F^file_table(F,FId),FIds),
  my_list_max(FIds,1,MaxFid), 
  !.
my_max_fid(0).

  
/*********************************************************************/
/* Trying to safe a rule/goal/view/autoview: make_safe/4             */
/*********************************************************************/

ensure_safe_ruleNVs(RuleNVs) :-
  is_safe_rule(RuleNVs),
  !.
ensure_safe_ruleNVs(_RuleNVs) :-
  write_warning_log(['This rule has not been transferred to the external database.']),
  !,
  fail.
% ensure_safe_ruleNVs((Rule,NVs)) :-
%   write_info_log(['Unsafe rule: ', Rule],NVs),
%   !,
%   fail.

is_safe_rule(RuleNVs) :-
  make_safe(RuleNVs,[],_Modes,consult,datalog,rule,_Safe,_TRNVs,_Transformed,Error),
  !,
  var(Error).

make_safe_list(SiRuleNVsList,SfRuleNVsList,CIArgsList,Modes,Action,Origin,Object1,Safety,Safed,Unsafe) :-
  SiRuleNVsList=[(R,_)|_],
  rule_pred(R,N/A),
  make_safe_list(SiRuleNVsList,SfRuleNVsList,CIArgsList,[],PredModesList,Action,Origin,Object1,Safety,Safed,Unsafe),
  join_pred_modes_list(N/A,PredModesList,Modes).

% make_safe_list(RNVsList,RNVsList,_IArgsList,ModesList,ModesList,_Action,_Object,Safe,_Transformed,_Unsafe) :-
%   Safe==no_safety, % Neither safety checks nor transformations (for SQL to Datalog compilations, which it is assumed safe transformations for allowed input modes)
%   !.
make_safe_list([],[],_,ModesList,ModesList,_,_,_,_,_,_Unsafe).
make_safe_list([RNVs|RNVsList],[SRNVs|SRNVsList],[IArgs|IArgsList],IModesList,OModesList,Action,Origin,Object,Safe,Transformed,Unsafe) :-
  make_safe(RNVs,IArgs,RModes,Action,Origin,Object,Safe,SRNVs,Transformed,Unsafe),
  ruleNVs_pred(RNVs,Pred),
  make_safe_list(RNVsList,SRNVsList,IArgsList,[(Pred,RModes)|IModesList],OModesList,Action,Origin,Object,Safe,Transformed,Unsafe).
      
make_safe_hypo((R,NVs),(SR,NVs),Action,Object,Safe,Transformed,Unsafe) :-
  build_incomplete_hypo_term(R,SR,HRVarList),
  make_safe_hypo_list(HRVarList,NVs,Action,Object,Safe,Transformed,Unsafe).
  
make_safe_hypo_list([],_NVs,_Action,_Object,_Safe,_Transformed,_Unsafe).
make_safe_hypo_list([(HR,Var)|HRVarList],NVs,Action,Object,Safe,Transformed,Unsafe) :-
  hypo_input_args(HR,IArgs),
  make_safe((HR,NVs),IArgs,_Modes,Action,datalog,Object,Safe,(Var,_NVs),Transformed,Unsafe),
  make_safe_hypo_list(HRVarList,NVs,Action,Object,Safe,Transformed,Unsafe).
  
hypo_input_args(F,IArgs) :-
  functor(F,N,A),
  is_limited_domain_predicate(N/A),
  !,
  list_between(1,A,IArgs).
hypo_input_args(_F,[]).
  
build_incomplete_hypo_term(R,SR,HRVarList) :-
  build_incomplete_hypo_term(R,SR,[],HRVarList).

% Replaces all occurrences of functor O by N in a term T
build_incomplete_hypo_term(T,T,RVs,RVs) :- 
%  (number(T) ; var(T) ; atom(T)),
  (var(T) ; atomic(T)),
  !.
build_incomplete_hypo_term((L=>R),(NL=>NR),IRVs,ORVs) :- 
  !,
  rules_from_hyp_program(L,HRs),
  length(HRs,Nbr),
  length(Vs,Nbr),
  yfx_connect_with(Vs,'/\\',NL),
  my_zipWith(',',HRs,Vs,HRVs),
  append(IRVs,HRVs,RVs1),
  build_incomplete_hypo_term(R,NR,RVs1,ORVs).
build_incomplete_hypo_term(C,RC,IRVs,ORVs) :- 
  C =.. [F|As],
  !, 
  build_incomplete_hypo_term_list(As,RAs,IRVs,ORVs),
  RC =.. [F|RAs].

build_incomplete_hypo_term_list([],[],RVs,RVs) :-
  !.
build_incomplete_hypo_term_list([T|Ts],[RT|RTs],IRVs,ORVs) :-
  !, 
  build_incomplete_hypo_term(T,RT,IRVs,RVs1), 
  build_incomplete_hypo_term_list(Ts,RTs,RVs1,ORVs).

%make_safe(+RuleNVs,+IArgs,-Modes,+Action,+Origin,+Object,+Safe,-TRuleNVs,-Transformed,?Unsafe)
% Safe=safe force safety transformations
make_safe(RuleNVs,IArgs,Modes,Action,Origin,Object,Safe,TRuleNVs,Transformed,Unsafe) :-
  Safe==no_safety,
  !,
  make_setvar_safe(RuleNVs,IArgs,Modes,Action,Origin,Object,TRuleNVs,_Positions,Transformed,SetVarUnsafe),
  tag_unsafe(Unsafe,_,SetVarUnsafe).
make_safe(RuleNVs,IArgs,Modes,Action,Origin,Object,Safe,TRuleNVs,Transformed,Unsafe) :-
  push_flag_if(safe,on,Safe,OldValue),
  make_safe_hypo(RuleNVs,HRuleNVs,Action,Object,Safe,Transformed,ClassicUnsafe),
  make_setvar_safe(HRuleNVs,IArgs,SModes,Action,Origin,Object,ARuleNVs,Positions,Transformed,SetVarUnsafe),
  make_classic_safe(ARuleNVs,IArgs,SModes,Modes,Action,Object,TRuleNVs,Positions,Transformed,ClassicUnsafe),
  tag_unsafe(Unsafe,ClassicUnsafe,SetVarUnsafe),
  pop_flag_if(safe,OldValue,Safe).
  
tag_unsafe(_,ClassicUnsafe,SetVarUnsafe) :-
  var(ClassicUnsafe),
  var(SetVarUnsafe),
  !.
tag_unsafe(unsafe(ClassicUnsafe,SetVarUnsafe),ClassicUnsafe,SetVarUnsafe).

is_setvar_safe(Unsafe) :-
  var(Unsafe),
  !.
is_setvar_safe(unsafe(_ClassicUnsafe,SetVarUnsafe)) :-
  var(SetVarUnsafe).

% Set variables safety: set variables in a metapredicate (aggregates and distinct/2) 
% can only be used as input in other occurrences out of the metapredicate
% e.g. group_by(t(X,Y),[X],C=sum(X)). distinct([X],t(X,Y))
% Set variables: [Y] 
% These variables are not allowed to be bound by a subsequent goal, but they
% can be bound before the metapredicate, therefore filtering elements in groups
% A set variable of a metapredicate can also occur in:
%   - Non-demanded argument positions of predicates (as, e.g., user-defined predicates)
%     Because of metapredicate implementation, data providers must be in a previous 
%     position w.r.t to the metapredicate
%     e.g.: p(Y),group_by(t(X,Y),[X],C=sum(Y))
%     e.g.: p(Y),distinct([X],t(X,Y))
%     An automatic reordering can be applied to get a safe rule should p(Y) occurs after the metapredicate
%   - Rule head: 
%      * Maybe an unsafe rule if called with a non ground argument
%      * Surely an unsafe rule if it is a temporary view 
%        e.g.: v(C,X,Y):-group_by(t(X,Y),[X],C=sum(Y))
%        e.g.: v(C,X,Y):-distinct([X],t(X,Y))
% Set variables are not allowed to occur at other places
%
% If there are data providers for a given set variable, at least one must be before the metapredicate
%
% make_setvar_safe(+RuleNVs,+IArgs,-Modes,+Action,+Origin,+Object,-RuleNVs,-ConstrainedPositions,-Transformed,?Unsafe).
% Ciao does neither support FD reification nor optimization
make_setvar_safe((':-'(HN,BN),NVs),IArgs,Modes,Action,Origin,Object,(':-'(H,OB),NVs),ConstrainedPositions,Transformed,Unsafe) :-
  !,
  concrete_nulls((HN,BN),(H,B),_),
  copy_term((H,B,NVs),(CH,CB,CNVs)),
  make_ground_args(CH,IArgs),
  my_list_to_tuple(CBs,CB),
  length(CBs,L),
  length(ConstrainedPositions,L),
  set_safe_domains(ConstrainedPositions),
  % WARNING: Remove when Ciao supports FD reification and optimization
  (prolog_system(ciao,_Version)
   ->
    H=HN,
    OB=B
   ;
    set_safe_preference_ctrs(ConstrainedPositions,Cost),
    set_safe_hard_ctrs(CBs,ConstrainedPositions,Positions,CNVs,_Ctrs,SetVars,Cost,SetvarError),
    (SetvarError==true,SetVars\==[]
     ->
      Unsafe=true,
      OB=B,
      display_setvar_unsafe(CNVs,SetVars,Action)
     ;
      safe_setvar_head(CH,CB,SetVars,CNVs,Modes,Action,Origin,Object,SetvarError),
      (SetvarError==true,SetVars\==[]
       ->
        OB=B,
        Unsafe=true
       ;
        (SetVars\==[]
         ->
          reorder_goals_by_positions(B,Positions,OB,Transformed)
         ;
          OB=B
        )
      )
    )
  ).
make_setvar_safe(HNVs,_IArgs,_Modes,_Action,_Origin,_Object,HNVs,_Positions,_Transformed,_Unsafe).

display_setvar_unsafe(NVs,SetVars,Action) :-
  (SetVars=[_] -> M=variable ; M=variables),
%  my_raise_exception(generic,syntax(['Incorrect use of shared set ',M,' in metapredicate: ',SetVars]),NVs).
  WE=['Invalid use of shared set ',M,' in metapredicate: ','$NVs'(SetVars,NVs)],
  (Action==exec
    -> 
     write_error_log(WE)
    ;
     (safety_warnings(on)
      ->
       write_warning_log(WE)
      ;
       true
     )
  ).
  
% safe_setvar_head(+Head,+SetVars,+NVs,-Modes,+Action,+Origin,+Object,-Unsafe) :-
safe_setvar_head(_Head,_Body,_SetVars,_NVs,_Modes,_Action,_Origin,autoview,_Unsafe) :- % The head of an automatic temporary view is safely built
  !.
safe_setvar_head(Head,Body,SetVars,NVs,Modes,Action,Origin,Object,Unsafe) :-
  relevant_NVs(Body,NVs,RelNVs),
  my_unzip(RelNVs,_Names,RelVars),
  term_variables(Head,HeadVs),
  my_set_diff(HeadVs,RelVars,HSVars),
  my_set_inter(HSVars,SetVars,Vs),
  (Vs==[] ->
    true
   ;
    (Action == consult -> 
     M='Next '
     ;
     M='This '
    ),
    (Vs=[_] -> MV=variable ; MV=variables),
    (Object==view ->
      write_error_log([M,'view is unsafe because of set ',MV,' in head: ','$NVs'(Vs,NVs)]),
      Unsafe=true
     ;
     (Object==autoview ->
       write_error_log([M,'autoview is unsafe because of set ',MV,' in metapredicate: ','$NVs'(Vs,NVs)]),
       Unsafe=true
      ;
      (Origin = sql(_)
       ->
         write_error_log(['This statement has non-grouped attributes in the projection list.']),
         Unsafe=true
       ;
         write_warning_log([M,'rule is unsafe if called with nonground ',MV,': ','$NVs'(Vs,NVs)]),
         Unsafe=true
      )
     )
    ),
    (Action == consult, verbose(off)
     ->
      display_ruleNVs_list([(':-'(Head,Body),NVs)],0)
     ;
      true
    ),
    term_args_positions(Head,Vs,Ps),
    input_positions_modes(Ps,Modes)
  ).

reorder_goals_by_positions(B,Positions,OB,Transformed) :-
  my_list_to_tuple(Bs,B),
  my_zipWith(',',Positions,Bs,LPsBs),
  my_mergesort(LPsBs,OLPsBs),
  my_unzip(OLPsBs,_OPs,OBs),
  my_list_to_tuple(OBs,OB),
  (B\==OB -> Transformed=true ; true).

% Set domain of FD varibles and constrain them to be all different
% set_safe_domains(+Positions)
set_safe_domains(Positions) :-
  length(Positions,L),
  my_fd_domain(Positions,1,L),
  my_fd_all_different(Positions).

% Set preferences about the original ordering of clauses
% Return the cost function to be maximized, i.e., the sum of entailed preferences
% set_safe_preference_ctrs(+Positions,-Cost)
set_safe_preference_ctrs(Positions,Cost) :-
  set_safe_preference_ctrs(Positions,0,Cost).

set_safe_preference_ctrs([],C,C).
set_safe_preference_ctrs([_P],C,C).
set_safe_preference_ctrs([P1,P2|Ps],Ci,Co) :-
  ctr_lt_list(P1,[P2|Ps],Ci,Co1),
  set_safe_preference_ctrs([P2|Ps],Co1,Co).

% ctr_lt_list(+X,+List,0,-Cost)
% Cost=sum(X#<Xi):Xi in List
ctr_lt_list(_X,[],C,C).
ctr_lt_list(X,[Y|Ys],Ci,Co) :-
  '#<=>'(B,'#<'(X,Y)),
  '#='(Co1, Ci + B),
  ctr_lt_list(X,Ys,Co1,Co).

% Get the (hard) constraints to commit to
% Return set variables 
% Find the best solution w.r.t. preference constraints
% If no solution: Unsafe=true
% set_safe_hard_ctrs(+Goals,+ConstrainedPositions,+Positions,+NVs,-Ctrs,-SetVars,-Cost,-Unsafe)
:- dynamic(positions/1).
set_safe_hard_ctrs(Goals,ConstrainedPositions,Positions,NVs,Ctrs,SetVars,Cost,Unsafe) :-
  safe_hard_ctrs(Goals,ConstrainedPositions,NVs,Ctrs,SetVars),
  ((post_safe_hard_ctrs(Ctrs,SetVars),
    \+ \+ (my_fd_maximize(my_fd_labeling([],ConstrainedPositions),Cost), 
           retractall(positions(_)),
           assertz(positions(ConstrainedPositions)))) ->
    positions(Positions),
    (my_mergesort(Positions,Positions) -> % If no reordering is needed, do nothing
      true
     ;
     (safe(on) ->
       true         % Unsafe body, safety transformation enabled, and reordering is possible
      ;
       Unsafe=true   % Unsafe body, safety transformation disabled, so display error
     )
    )
  ;
    Unsafe=true). % No way to reorder goals to get a safe body
    
post_safe_hard_ctrs([],_SetVars).
post_safe_hard_ctrs([Ctr|Ctrs],SetVars) :-
  call(Ctr),
  !,
  post_safe_hard_ctrs(Ctrs,SetVars).


% Build hard constraints for the goals which use set variables
% safe_hard_ctrs(+Goals,+Positions,+NVs,-Ctrs,-SetVars)
safe_hard_ctrs(Goals,Positions,NVs,Ctrs,SetVars) :-
  safe_hard_ctrs([],[],Goals,Positions,NVs,[],Ctrs,[],SetVars).

% safe_hard_ctrs(+LeftGoals,+LeftPositions,+Goals,+Positions,+NVs,+Ctrsi,-Ctrso,+SetVarsi,-SetVarso)
safe_hard_ctrs(_LeftGoals,_LeftPositions,[],[],_NVs,Ctrs,Ctrs,SetVars,SetVars).
safe_hard_ctrs(LeftGoals,LeftPositions,[Goal|RightGoals],[Position|RightPositions],NVs,Ctrsi,Ctrso,SetVarsi,SetVarso) :-
  get_set_vars(Goal,SetVars),
  SetVars\==[],
  !,
  append(LeftGoals,RightGoals,Goals),
  append(LeftPositions,RightPositions,Positions),
  build_hard_ctrs(SetVars,set_var,Goal,Position,Goals,Positions,NVs,Ctrsi,Ctrso2),
  append(SetVars,SetVarsi,SetVarso1),
  safe_hard_ctrs([Goal|LeftGoals],[Position|LeftPositions],RightGoals,RightPositions,NVs,Ctrso2,Ctrso,SetVarso1,SetVarso).
safe_hard_ctrs(LeftGoals,LeftPositions,[Goal|RightGoals],[Position|RightPositions],NVs,Ctrsi,Ctrso,SetVarsi,SetVarso) :-
  get_demanded_vars(Goal,DemVars),
  DemVars\==[],
  !,
  append(LeftGoals,RightGoals,Goals),
  append(LeftPositions,RightPositions,Positions),
  build_hard_ctrs(DemVars,dem_var,Goal,Position,Goals,Positions,NVs,Ctrsi,Ctrso2),
%  append(DemVars,DemVarsi,DemVarso1),
  safe_hard_ctrs([Goal|LeftGoals],[Position|LeftPositions],RightGoals,RightPositions,NVs,Ctrso2,Ctrso,SetVarsi,SetVarso).
safe_hard_ctrs(LeftGoals,LeftPositions,[Goal|RightGoals],[Position|RightPositions],NVs,Ctrsi,Ctrso,SetVarsi,SetVarso) :-
  safe_hard_ctrs([Goal|LeftGoals],[Position|LeftPositions],RightGoals,RightPositions,NVs,Ctrsi,Ctrso,SetVarsi,SetVarso).

% Get set variables (only in aggregates and distinct/2)
% distinct(Vs,Rel)
get_set_vars(distinct(Vs,Rel),SetVars) :-  
  !,
  term_variables(Rel,RVs),
  my_set_diff(RVs,Vs,SetVars).
% count(Rel,GBVs,O), count_distinct(Rel,GBVs,O)
% group_by(Rel,GBVs,Cond)
get_set_vars(group_by(Rel,_Ps,GBVs,_C),SetVars) :-  
  !,
  term_variables(Rel,RVs),
  my_set_diff(RVs,GBVs,SetVars).
% count(Rel,GBVs,O), count_distinct(Rel,GBVs,O)
get_set_vars(Goal,SetVars) :-  
  (Goal = count(Rel,GBVs,_OC)
  ;
   Goal = count_distinct(Rel,GBVs,_OCD)),
  !,
  term_variables(Rel,RVs),
  my_set_diff(RVs,GBVs,SetVars).
% count, sum, ... aggr(Rel,Var,GBVars,Result)
get_set_vars(Goal,SetVars) :-  
  Goal=..[Aggr,Rel,_Var,GBVs,_Res],
  my_aggregate_relation(Aggr,4),
  !,
  term_variables(Rel,RVs),
  my_set_diff(RVs,GBVs,SetVars).
get_set_vars(_Goal,[]).  

% get_non_set_vars(Goal,NonSetVars) :-
%   term_variables(Goal,Vars),
%   get_set_vars(Goal,SetVars),
%   my_set_diff(Vars,SetVars,NonSetVars).
  

% Demanded variables for built-ins
get_demanded_vars(_A is _B,[]) :-
  !.
get_demanded_vars(_A = _B,[]) :-
  !.
get_demanded_vars(Goal,[]) :-  
  functor(Goal,P,A),
  is_non_demanded_predicate(P/A),
  !.
get_demanded_vars(Goal,SetVars) :-
  term_variables(Goal,SetVars).

% build_hard_ctrs(+SetVars,+MetaGoal,+MetaPosition,+Goals,+Positions,+NVs,+Ctrsi,-Ctrso1)
build_hard_ctrs([],_Kind,_MetaGoal,_MetaPosition,_Goals,_Positions,_NVs,Ctrs,Ctrs).
build_hard_ctrs([V|Vs],Kind,MetaGoal,MetaPosition,Goals,Positions,NVs,Ctrsi,Ctrso) :-
  build_var_hard_ctrs(V,Kind,MetaGoal,MetaPosition,Goals,Positions,NVs,0,Sum,Ctrsi,Ctrso1),
  (Sum\==0 -> 
    Ctrso2=['#>'(Sum,0)|Ctrso1]
   ;
    Ctrso2=Ctrso1),
  build_hard_ctrs(Vs,Kind,MetaGoal,MetaPosition,Goals,Positions,NVs,Ctrso2,Ctrso).

% build_var_hard_ctrs(+SetVar,+MetaGoal,+MetaPosition,+Goals,+Positions,+NVs,+Sumi,-Sumo,+Ctrsi,-Ctrso1)
build_var_hard_ctrs(_SetVar,_Kind,_MetaGoal,_MetaPosition,[],[],_NVs,S,S,Ctrs,Ctrs).
build_var_hard_ctrs(SetVar,set_var,_MetaGoal,_MetaPosition,[Goal|_Goals],_Positions,NVs,_Si,_So,_Ctrsi,_Ctrso) :-
  get_set_vars(Goal,GoalSetVars),
  my_member_var(SetVar,GoalSetVars),
  !,
  my_raise_exception(generic,syntax(['Set variable ',[SetVar],' is not allowed to occur in different metapredicates.']),NVs).
build_var_hard_ctrs(SetVar,Kind,MetaGoal,MetaPosition,[Goal|Goals],[Position|Positions],NVs,Si,So,Ctrsi,Ctrso) :-
  term_variables(Goal,GVs),
  my_member_var(SetVar,GVs),
  !,
  FDCtr='#<=>'('#<'(Position,MetaPosition),B),
  build_var_hard_ctrs(SetVar,Kind,MetaGoal,MetaPosition,Goals,Positions,NVs,'+'(Si,B),So,[FDCtr|Ctrsi],Ctrso).
build_var_hard_ctrs(SetVar,Kind,MetaGoal,MetaPosition,[_Goal|Goals],[_Position|Positions],NVs,Si,So,Ctrsi,Ctrso) :-
  !,
  build_var_hard_ctrs(SetVar,Kind,MetaGoal,MetaPosition,Goals,Positions,NVs,Si,So,Ctrsi,Ctrso).

% Classical safety: finite domains and negation
% Rules/Views/Autoviews:
make_classic_safe((':-'(IHN,BN),NVs),IArgs,IModes,OModes,Action,Object,(':-'(OH,OB),NVs),Positions,Transformed,Unsafe) :-
  !,
  language(Lang),
  (Object==autoview -> (HN=autoview , OH=IHN)
   ;
   (Object==query -> (HN=query , OH=IHN)
    ;
    (HN=IHN, OH=H))),
  concrete_nulls((HN,BN),(H,B),_),
  copy_term((IHN,H,B),(CIHN,CH,CB)),
  make_ground_args(CIHN,IArgs),
  mark_safe_NVs(CB,CH,H,B,NVs,[],_,[],IPs,Object,Action,Err1),
  (Object==query -> Unsafe=Err1 ; true),
  % Try to reorder when:
  ((my_ground((CH,CB));           % Either all variables are safe,
   (my_ground(CB),Object\=view);  % or body variables are safe,
   (term_variables(CB,V1s),       % or only head variables are unsafe. May be safe at run-time
    term_variables(CH,V2s),
    same_variables(V1s,V2s)))
    ->
    (safe(on) -> 
      reorder_goals(H,B,IArgs,OB,Ps), 
       % Goals can actually be reordered to ensure that, upon execution, 
       % demanded arguments become ground before the call.
       % But this modifies performance for some cases:
       % p(X) :- X>1,q(X) could be translated into p(X) :- q(X),X>1
       % In the first case, q(X) is not actually computed for X=<1
       % Input arguments are taken into account for the reordering
      (B\==OB, \+ Ps=Positions ->
        Unsafe=true % No way to satisfy hard constraints for metapredicates
       ;
        true
      )
     ; 
      OB=B),
    ((safe(on), B \= OB, Unsafe\==true)
     ->
     Transformed = true,
     ((safety_warnings(on), \+ (Lang==drc ; Lang==trc))
      ->
      write_info_log(['For allowing a (possible) safe computation, the ',Object,':']),
      ((Object=rule;Object=view) ->
        display_ruleNVs_list([(':-'(H,B),NVs)],2)
        ;
        display_ruleNVs_list([(B,NVs)],2)),
      write_log_list(['has been translated into:',nl]),
      ((Object=rule;Object=view) ->
        display_ruleNVs_list([(':-'(H,OB),NVs)],2)
        ;
        display_ruleNVs_list([(OB,NVs)],2))
      ;
       true
      )
    ;
    (safe(off) -> Unsafe=Err1; true)
    )
   ;
   Unsafe=true,
%   (Object==query ->
   ((Object==query ; Object==autoview)
    ->
     head_modes(IHN,CModes),
     display_unsafe(IHN,B,IHN,B,NVs,Action,Object)
    ; 
     Unsafe=true),
   OB=B
   ),
   (my_ground(CH) ->
    true
    ;
    (var(Unsafe) -> Unsafe=true; true),
     head_modes(CH,CModes),
    (%(H,B)=(CH,CB),
     display_unsafe(H,B,CH,CB,NVs,Action,Object),
     fail
     ;
     true
    )
   ),
   (IPs==[]
    ->
     join_modes(IModes,CModes,OModes)
    ;
     functor(IHN,_,Arity),
     length(HModes,Arity),
     input_positions_modes(IPs,HModes),
     join_modes(IModes,CModes,TModes),
     join_modes(TModes,HModes,OModes)
   ).
% Goals:   
make_classic_safe((HN,NVs),IArgs,IModes,OModes,Action,Object,(H,NVs),_Positions,_,Unsafe) :-
  concrete_nulls(HN,H,_),
  copy_term(H,CH),
  make_ground_args(CH,IArgs),
  (my_ground(CH)
   ->
    OModes=IModes                                     % Contains no variables
   ;
    Unsafe=true,
    head_modes(H,HModes),
    join_modes(IModes,HModes,OModes),
    display_unsafe(H,true,H,true,NVs,Action,Object)). % Unsafe: nonground fact

same_variables(V1s,V2s) :-
  length(V1s,L),
  length(V2s,L),
  my_set_diff(V1s,V2s,[]).
  
% (Goals (marked), Head, Body, Variables in negated atoms, Names of variables,Pending variables,Input variables)
% A set of pending variables represents variables aliased to a given one such that grounding this one, will also ground pending variables
% Input variables are those required to be ground to avoid instantiation errors
mark_safe_NVs((G,Gs),CH,H,(B,Bs),NVs,PVi,PVo,IPi,IPo,Object,Action,Unsafe) :-
  !,
  mark_safe_NVs(G,CH,H,B,NVs,PVi,PVo1,IPi,IPo1,Object,Action,E1),
  mark_safe_NVs(Gs,CH,H,Bs,NVs,PVo1,PVo,IPo1,IPo,Object,Action,E2),
  my_u_or(E1,E2,Unsafe).
mark_safe_NVs(G,_,_,_,_,PV,PV,IP,IP,_,_,_Unsafe) :- 
  my_ground(G),
  !.
mark_safe_NVs('='(L,R),_,_,_,_,PVi,PVo,IP,IP,_,_,_Unsafe) :- 
  my_noncompound_term(L),
  my_noncompound_term(R),
  !,
  L=R,
  (my_ground(L) ->
   make_ground_pending(PVi,PVo)
   ;
   PVo=PVi).
mark_safe_NVs(L=R,CH,H,CL=CR,NVs,PVi,PVo,IPi,IPo,Object,Action,Unsafe) :- 
  my_noncompound_term(L),
  !,
  mark_safe_NVs(is(L,R),CH,H,is(CL,CR),NVs,PVi,PVo,IPi,IPo,Object,Action,Unsafe).
mark_safe_NVs(L=R,CH,H,CL=CR,NVs,PVi,PVo,IPi,IPo,Object,Action,Unsafe) :- 
  my_noncompound_term(R),
  !,
  mark_safe_NVs(is(R,L),CH,H,is(CR,CL),NVs,PVi,PVo,IPi,IPo,Object,Action,Unsafe).
% mark_safe_NVs('='(L,R),H,_B,NVs,PVi,PVo,Object,Action,Unsafe) :- 
%   my_noncompound_term(R),
%   !,
%   mark_safe_NVs(is(R,L),H,is(L,R),NVs,PVi,PVo,Object,Action,Unsafe).
mark_safe_NVs(is(X,Y),CH,H,B,NVs,PVi,PVo,IPi,IPo,Object,_,Unsafe) :- 
  !,
  (my_ground(Y)
   -> 
    make_ground(X), % X gets bound when Y does
    make_ground_pending(PVi,PVo),
    IPo=IPi
    %,Unsafe=false
   ; 
    make_ground(X), % X gets bound when Y does
    make_ground_pending(PVi,PVo1),
    B=is(_,YB),     % If Y is not safe, 'is' may or will raise an exception
    term_variables(Y,YVs),
    PVo=[(X,YVs)|PVo1],
    term_variables(CH,HVs),
    my_set_inter(HVs,YVs,Vs),
    positive_atom(CH,PCH),
    term_args_positions(PCH,Vs,Ps),
    my_set_union(IPi,Ps,IPo),
    display_computation_warning(B,Y,H,YB,NVs,Object,Unsafe)
  ).
mark_safe_NVs(group_by(R,_,_,C),CH,H,group_by(_CR,_,_,CC),NVs,PVi,PVo,IPi,IPo,Object,Action,Unsafe) :-
  !,
  make_ground(R),   
  make_ground_pending(PVi,PVo1),
%  term_variables(R,RVs),
  mark_safe_NVs(C,CH,H,CC,NVs,PVo1,PVo,IPi,IPo,Object,Action,Unsafe).
% mark_safe_NVs(distinct(Vs,_R),_H,_B,_NVs,PVi,PVo,_Object,_Action,_Unsafe) :-
%   !,
%   make_ground(Vs),   
%   make_ground_pending(PVi,PVo).
mark_safe_NVs(top(_N,G),CH,H,B,NVs,PVi,PVo,IPi,IPo,Object,Action,Unsafe) :-
  !,
  mark_safe_NVs(G,CH,H,B,NVs,PVi,PVo,IPi,IPo,Object,Action,Unsafe).
mark_safe_NVs(-(G),CH,H,B,NVs,PVi,PVo,IPi,IPo,Object,Action,Unsafe) :-
  !,
  mark_safe_NVs(G,CH,H,B,NVs,PVi,PVo,IPi,IPo,Object,Action,Unsafe).
mark_safe_NVs('=>'(L,G),CH,H,'=>'(_,B),NVs,PVi,PVo,IPi,IPo,Object,Action,Unsafe) :-
  !,
  make_ground(L),   
  make_ground_pending(PVi,PVo1),
  mark_safe_NVs(G,CH,H,B,NVs,PVo1,PVo,IPi,IPo,Object,Action,Unsafe).
mark_safe_NVs(not(G),_CH,_H,not(_G),_NVs,PVi,PVo,IPi,IPi,_Object,_Action,_Unsafe) :-
  functor(G,N,A),
  is_limited_domain_predicate(N/A),
  !,
  make_ground(G),   
  make_ground_pending(PVi,PVo).
mark_safe_NVs(G,CH,H,B,NVs,PVi,PVo,IPi,IPo,Object,Action,Unsafe) :-
  functor(G,P,A),
  (is_non_demanded_predicate(P/A)
   ->
%   get_non_set_vars(G,Vs),
%   make_ground(Vs),   % Defined predicate or set built-in (distinct,count,...)
   make_ground(G),   % Defined predicate or set built-in (distinct,count,...)
   make_ground_pending(PVi,PVo),
   IPo=IPi
                     % (assumed safe, but actually it may not;
                     %  a global analysis should be performed),
                     % all variables are marked as safe
   ;
   (my_ground(G) ->  % Built-in (including negation)
     IPo=IPi         % Nothing to do if all variables are safe (ground goal)
    ;
    (P/A == (not)/1 -> % Deciding whether not/1 or other built-in
                     % If negation, the rule cannot be computed
      add_term_variables(G,CH,IPi,IPo),
      display_neg_error(G,H,B,NVs,Action),
      Unsafe=true
     ;               % If built-in, display computation warning
      add_term_variables(G,CH,IPi,IPo),
      display_computation_warning(B,G,H,B,NVs,Object,Unsafe)
    )
   ),
   PVo=PVi
  ).

add_term_variables(T,H,IPi,IPo) :-
  term_variables(H,HVs),
  term_variables(T,TVs),
  my_set_inter(HVs,TVs,Vs),
  positive_atom(H,PH),
  term_args_positions(PH,Vs,Ps),
  my_set_union(IPi,Ps,IPo).
  
  
make_ground_pending([],[]).
make_ground_pending([(X,Vs)|PVi],PVo) :-
  my_ground(Vs),
  !,
  make_ground(X),
  make_ground_pending(PVi,PVo).
make_ground_pending([(X,Vs)|PVi],[(X,Vs)|PVo]) :-
  make_ground_pending(PVi,PVo).

% reorder_goals(+Body,-OrderedBody)
reorder_goals(B,OB) :-
  reorder_goals(true,B,[],OB,_Ps).
% reorder_goals(+Head,+Body,+InputArguments,-OrderedBody,-Positions)  
reorder_goals(H,B,IArgs,OB,Ps) :-
  my_list_to_tuple(Bs,B),
  copy_term((H,Bs,IArgs),(CH,CBs,CIArgs)),
  concrete_nulls(CBs,GBs,_Grounded),
  make_ground_args(CH,CIArgs),
  reorder_goals_by_safety(Bs,GBs,[],[],EOBs),
%   (false ->
%     reorder_goals_by_efficiency(SOBs,EOBs)
%    ;
%     EOBs=SOBs
%   ),
  my_list_to_tuple(EOBs,OB),
  get_goal_positions(Bs,EOBs,Ps).
  
% get_goal_positions(+PBs,+Bs,-Ps)
% Get goal positions Ps of PBs w.r.t. Bs
% e.g. PBs=[p(X),q(X),p(X)] Bs=[p(X),p(X),q(X)] Ps=[1,3,2]
get_goal_positions(PBs,Bs,Ps) :-
  get_goal_positions(PBs,Bs,[],Ps).
  
% get_goal_positions(+PBs,+Bs,+UsedPs,-Ps)
get_goal_positions([],_Bs,_UPs,[]).
get_goal_positions([PB|PBs],Bs,UPs,[P|Ps]) :-
  my_nth1_member_var(PB,P,Bs),
  \+ member(P,UPs),
  !,
  get_goal_positions(PBs,Bs,[P|UPs],Ps).
get_goal_positions([group_by(R,_,Vs,_C)|PBs],Bs,UPs,[P|Ps]) :-
  my_nth1_member(group_by(R,_,Vs,_),P,Bs),
  \+ member(P,UPs),
  get_goal_positions(PBs,Bs,[P|UPs],Ps).
  
% Push equalities to the left
% For Rules along with NVs
reorder_goals_by_efficiency_ruleNVs_list(RuleNVsList,Reorder,RRuleNVsList) :-
  ((reorder_goals(on) ; Reorder==reorder)
   ->
    force_reorder_goals_by_efficiency_ruleNVs_list(RuleNVsList,RRuleNVsList)
   ;
    RRuleNVsList=RuleNVsList).
    
force_reorder_goals_by_efficiency_ruleNVs_list([],[]).
force_reorder_goals_by_efficiency_ruleNVs_list([(R,NVs)|Rs],[(OR,NVs)|ORs]) :-
  reorder_goals_by_efficiency_rule(R,OR),
  force_reorder_goals_by_efficiency_ruleNVs_list(Rs,ORs).

% % For Rules
reorder_goals_by_efficiency_rule_list([],[]).
reorder_goals_by_efficiency_rule_list([R|Rs],[OR|ORs]) :-
  reorder_goals_by_efficiency_rule(R,OR),
  reorder_goals_by_efficiency_rule_list(Rs,ORs).
  
% reorder_goals_by_efficiency_rule(R,R) :-
%   reorder_goals(off),
%   !.
reorder_goals_by_efficiency_rule(':-'(H,B),OR) :-
  !,
  OR=':-'(H,OB),
  reorder_goals_by_efficiency_body(B,OB).
reorder_goals_by_efficiency_rule(R,R).
  
reorder_goals_by_efficiency_body(B,OB) :-
  my_list_to_tuple(Bs,B),
  reorder_goals_by_efficiency(Bs,OBs),
  my_list_to_tuple(OBs,OB).

% Push equalities to the left
reorder_goals_by_efficiency(Gs,OGs) :-
  reorder_goals_by_efficiency(Gs,FGs,[],PGs),
  append(PGs,FGs,OGs).

reorder_goals_by_efficiency([],[],Gs,Gs).
reorder_goals_by_efficiency([L=R|As],Bs,IGs,OGs) :-
%  G=..['=',L,R],
  my_noncompound_term(L),
  my_noncompound_term(R),
  !,
  reorder_goals_by_efficiency(As,Bs,[L=R|IGs],OGs).
reorder_goals_by_efficiency(['=>'(L,R)|As],['=>'(RL,RR)|Bs],IGs,OGs) :-
  !,
  rules_from_hyp_program(L,Ls),
  reorder_goals_by_efficiency_rule_list(Ls,OLs),
  yfx_connect_with(OLs,'/\\',RL),
  reorder_goals_by_efficiency_body(R,RR),
  reorder_goals_by_efficiency(As,Bs,IGs,OGs).
reorder_goals_by_efficiency([G|As],[G|Bs],IGs,OGs) :-
  reorder_goals_by_efficiency(As,Bs,IGs,OGs).

reorder_goals_by_safety([],[],[],[],[]) :-
  !.
reorder_goals_by_safety([group_by(R,Ps,Vs,C)],[group_by(CR,_CPs,_CVs,CC)],[],[],[group_by(R,Ps,Vs,OC)]) :-
  !,
  make_ground(CR),
  my_list_to_tuple(Cs,C),
  my_list_to_tuple(CCs,CC),
  reorder_goals_by_safety(Cs,CCs,[],[],OCs),
  my_list_to_tuple(OCs,OC).
reorder_goals_by_safety([B],[_CB],[],[],[B]) :-
  !.
reorder_goals_by_safety([],[],PGs,CPGs,PGs) :- % Give up: No way to find a safe order
  get_safe_goals(PGs,CPGs,_PG1s,_CPG1s,SGs),
  SGs==[],
  !.
reorder_goals_by_safety([],[],PGs,CPGs,OBs) :-
  !,
  reorder_goals_by_safety(PGs,CPGs,[],[],OBs).
reorder_goals_by_safety(Bs,CBs,PGs,CPGs,OBs) :-
  get_safe_goals(PGs,CPGs,PG1s,CPG1s,SGs),
  SGs\==[],
  !,
  append(SGs,VGs,OBs),
  reorder_goals_by_safety(Bs,CBs,PG1s,CPG1s,VGs).
reorder_goals_by_safety([BI|Bs],[CBI|CBs],PGs,CPGs,OBs) :-
  CBI=(L=R),
  (my_compound_term(L) % compound terms in equalities are evaluated
   ;
   my_compound_term(R)),
  !,
  ((my_ground(L)
    ;
    my_ground(R)) ->
    make_ground(L),
    make_ground(R),
    OBs=[BI|OB1s],
    reorder_goals_by_safety(Bs,CBs,PGs,CPGs,OB1s)
   ;
    reorder_goals_by_safety(Bs,CBs,[BI|PGs],[CBI|CPGs],OBs)
  ). 
reorder_goals_by_safety([B|Bs],[CB|CBs],PGs,CPGs,[B|OBs]) :-
  functor(B,P,A),
  (is_non_demanded_predicate(P/A),
   make_ground(CB)
   ; 
   P/A = ('=')/2,
   (my_ground(CB) -> true ; call(CB))),
  !,
  reorder_goals_by_safety(Bs,CBs,PGs,CPGs,OBs).    
reorder_goals_by_safety([BI|Bs],[CBI|CBs],PGs,CPGs,OBs) :-
  !,
  (CBI=not(A) -> 
   BG=A
   ;
   (CBI=is(X,Y) ->
    BG=Y
    ;
    BG=BI)),
  (my_ground(BG) ->
   (CBI=is(X,Y) -> 
    make_ground(X)
    ;
    true),
   OBs=[BI|OB1s],
   reorder_goals_by_safety(Bs,CBs,PGs,CPGs,OB1s)
   ;
   reorder_goals_by_safety(Bs,CBs,[BI|PGs],[CBI|CPGs],OBs)).
reorder_goals_by_safety(Bs,_CBs,[],[],Bs) :-
  !.

get_safe_goals([],[],[],[],[]).
get_safe_goals([G|Gs],[CG|CGs],PGs,CPGs,[G|SGs]) :-
  my_ground(CG),
  !,
  get_safe_goals(Gs,CGs,PGs,CPGs,SGs).
get_safe_goals([G|Gs],[CG|CGs],[G|PGs],[CG|CPGs],SGs) :-
  get_safe_goals(Gs,CGs,PGs,CPGs,SGs).

display_neg_error(G,H,B,NVs,Action) :-
%  safety_warnings(on),
  should_display_warning(_ErrorType),
  !,
  (write_log('Error: '),
   write_with_NVs(B,NVs),
   B=G,
   term_variables(B,Vs),
   (Vs=[_] -> M=variable ; M=variables),
   write_log_list([' might not be correctly computed because of the unrestricted ',M,': ','$NVs'(Vs,NVs)]),
   fail
  ;
   ((Action==consult,verbose(off)) -> 
    write_log(' in rule: '), 
    write_with_NVs(':-'(H,B),NVs), 
    nl_log
   ;
    nl_log)
  ).
display_neg_error(_G,_H,_B,_NVs,_Action).

%is_non_demanded_predicate(top/2) :- % WARNING
%  !.
is_non_demanded_predicate(is_null/1) :-
  !,
  fail.
is_non_demanded_predicate(is_not_null/1) :-
  !,
  fail.
is_non_demanded_predicate(distinct/1) :-
  !.
is_non_demanded_predicate(distinct/2) :-
  !.
is_non_demanded_predicate(exists/2) :-
  !.
is_non_demanded_predicate(order_by/3) :-
  !.
is_non_demanded_predicate(group_by/4) :-
  !.
is_non_demanded_predicate(st/1) :-
  !.
is_non_demanded_predicate(call/1) :-
  !.
% is_non_demanded_predicate(lj/1) :-
%   !.
is_non_demanded_predicate('=>'/2) :-
 !.
is_non_demanded_predicate('/\\'/2) :-
  !.
is_non_demanded_predicate(AF/Arity) :-
  my_aggregate_relation(AF,Arity),
  !.
is_non_demanded_predicate(AF/Arity) :-
  my_builtin_relation(AF,Arity,_M,_K),
  !.
is_non_demanded_predicate(Pred/Arity) :-
  my_outer_join_relation(Pred/Arity),
  !.
is_non_demanded_predicate(Pred/Arity) :-
  Pred/Arity \== (not)/1,
  my_builtin_preds(BIPreds),
  \+ (member(Pred/Arity,BIPreds)).

is_demanded_predicate(Pred/Arity) :-
  \+ is_non_demanded_predicate(Pred/Arity).

display_unsafe(_H,_B,_CH,_CB,_NVs,_Action,_Object) :-
  safety_warnings(off),
  !.
display_unsafe(H,B,CH,CB,NVs,Action,Object) :-
  term_variables(CH,HVs),
  collect_neg_NVs(CB,BVs),
  append(HVs,BVs,TVs),
  remove_duplicates_var(TVs,NNVs),
  (NVs==[] ->
   true
  ;
   language(Lang),
   ((Lang==drc ; Lang==trc) -> ErrType='Error' ; ErrType='Warning'),
   (Action == consult -> 
    M='Next '
    ;
    M='This '
   ),
   (NNVs=[_] -> MV=variable ; MV=variables),
   write_log_list([ErrType,': ',M,Object,' is unsafe because of ',MV,': ']),
   (CH=H,
    CB=B,
    write_with_NVs_delimited_list(NNVs,NVs),
    fail
    ;
    true),
   write_log_list([nl])
  ),
  (Action == consult, verbose(off) ->
   display_ruleNVs_list([(':-'(H,B),NVs)],0)
   ;
   true).

collect_neg_NVs((G,Gs),Vs) :-
  !,
  collect_neg_NVs(G,GVs),
  collect_neg_NVs(Gs,GsVs),
  append(GVs,GsVs,Vs).
collect_neg_NVs(not(P),Vs) :-
  !, 
  term_variables(P,Vs).
collect_neg_NVs(_,[]).
   
% display_computation_warning(Reference literal, copied part of the reference literal, head, original part of the reference literal, variable names)
% Ex: (X is Y, _Y, fib(N,M), Y, ['Y'=Y|...])
% Ex: (X < Y, _X < _Y, fib(N,M), X < Y, ['Y'=Y|...])
display_computation_warning(B,SG,H,SB,NVs,Object,_Error) :-
  my_term_to_string(B,S,NVs),
  SB=SG,
  term_variables(H,HVs),
  term_variables(SB,BVs),
  ((my_set_diff(BVs,HVs,[]),Object\=view)
   -> 
    I='Warning: ', M=' may raise a computing exception if non-ground at run-time.',
    ErrorType=warning,
    assertz(error)
   ; 
    I='Error: ', M=' will raise a computing exception at run-time.',
    ErrorType=error,
    assertz(error)
  ),
  (should_display_warning(ErrorType)
   ->
    write_log(I),
    write_string_log(S),
    write_log_list([M,nl])
   ;
    true),
  fail.
display_computation_warning(_,_,_,_,_,_,Error) :-
  (error
   -> 
    Error=true,
    retractall(error)
   ;
    true). 
  
should_display_warning(_ErrorType) :-
  safety_warnings(on),
  language(Lang),
%  ((Lang\==drc,Lang\==trc) ; ErrorType==error),
  (Lang\==drc,Lang\==trc),
  !.
   

/*********************************************************************/
/* Auxiliary Predicates                                              */
/*********************************************************************/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% set_flag(+Flag). Retracts the flag and asserts the input fact
set_flag(Flag) :-
  functor(Flag,F,A),
  functor(FFlag,F,A),
  retractall(FFlag),
  assertz(Flag).

% set_flag(+FlagName,+Value). Retracts the flag and asserts the input flag_name/value
set_flag(Flag,Values) :-
  nonvar(Values),
%  functor(Values,'.',2),
  my_is_list(Values),
  !,
  length(Values,L),
  length(Args,L),
  Fact =.. [Flag|Args],
  retractall(Fact),
  NewFact =.. [Flag|Values],
  assertz(NewFact).
set_flag(Flag,Value) :-
  Fact =.. [Flag,_OldValue],
  retractall(Fact),
  NewFact =.. [Flag,Value],
  assertz(NewFact).

% set_flag(+FlagName,+Value1,+Value2). Retracts the flag and asserts the input fact/value1/value2
set_flag(Flag,Value1,Value2) :-
  Fact =.. [Flag,_OldValue1,_OldValue2],
  retractall(Fact),
  NewFact =.. [Flag,Value1,Value2],
  assertz(NewFact).

% set_p_flag(+UFlag,+Value). Retracts the flag and asserts the input uncomplete_fact+value
set_p_flag(UFact,Value) :-
  UFact =.. [Name|UArgs],
  append(UArgs,[Arg],Args),
  Fact =.. [Name|Args],
  (\+ \+ retract(Fact) -> true ; true),
  Arg=Value,
  NewFact =.. [Name|Args],
  assertz(NewFact).

% push_flag
push_flag(FlagName,NewValues,CurrentValues) :-
  my_is_list(NewValues),
  !,
  length(NewValues,Arity),
  functor(CFlag,FlagName,Arity),
  CFlag=..[FlagName|CurrentValues],
  retract(CFlag),
  NFlag=..[FlagName|NewValues],
  assertz(NFlag).
push_flag(FlagName,NewValue,CurrentValue) :-
  CFlag=..[FlagName,CurrentValue],
  retract(CFlag),
  NFlag=..[FlagName,NewValue],
  assertz(NFlag).

push_flag_if(FlagName,_NewValue,Force,OldValue) :-
  var(Force),
  !,
  get_flag(FlagName,OldValue).
push_flag_if(FlagName,NewValue,_Force,OldValue) :-
  push_flag(FlagName,NewValue,OldValue).
  
% pop_flag
pop_flag(FlagName,OldValue) :-
  set_flag(FlagName,OldValue).
  
pop_flag_if(_FlagName,_OldValue,Force) :-
  var(Force),
  !.
pop_flag_if(FlagName,OldValue,_Force) :-
  pop_flag(FlagName,OldValue).

% get_flag
get_flag(FlagName,Value) :-
  CFlag=..[FlagName,Value],
  call(CFlag),
  !.
get_flag(null_id,-1).
  
% drop_flag
drop_flag(Flag) :-
  retract(Flag),
  !.
drop_flag(_Flag).
  
% inc_flag
inc_flag(FlagName) :-
  inc_flag(FlagName,_NewValue).

inc_flag(FlagName,NewValue) :-
  CFlag=..[FlagName,Value],
  retract(CFlag),
  !,
  NewValue is Value+1,
  NCFlag=..[FlagName,NewValue],
  assertz(NCFlag).
% inc_flag(FlagName,0) :-
%   NCFlag=..[FlagName,0],
%   assertz(NCFlag).

add_to_flag(FlagName,Value) :-
  CFlag=..[FlagName,OldValue],
  retract(CFlag),
  !,
  NewValue is OldValue+Value,
  NCFlag=..[FlagName,NewValue],
  assertz(NCFlag).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Program Identifiers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Null identifiers
get_null_id(Id) :-
  (null_id(CId) -> 
   Id is CId+1,
   retract(null_id(CId))
   ; 
   Id is 0),
  assertz(null_id(Id)).

reset_null_id :-
  retractall(null_id(_Id)).

% Rule identifiers 
get_rule_id(Id) :-
  var(Id),
  !,  
  (rule_id(CId)
   ->
    Id is CId+1,
    retract(rule_id(CId))
   ;
    Id=0),
  assertz(rule_id(Id)).
get_rule_id(_Id).

reset_rule_id :-
  retractall(rule_id(_Id)).

pop_rule_id :-  
  (retract(rule_id(CId))
   ->
    Id is CId-1,
    assertz(rule_id(Id))
   ;
    true).
    
% Predicate identifiers 
get_pred_id(Id) :-
  get_pred_id(_O,Id).
  
get_pred_id(O,OId) :-
  (pred_id(O,IId) -> 
   OId is IId+1,
   retract(pred_id(O,IId))
   ; 
   OId is 0),
  assertz(pred_id(O,OId)).

% current_pred_id(O,OId) :-
%   get_new_predicate_name(O,P),
%   atom_concat('$',O,DP),
%   atom_concat(DP,AN,P),
%   atom_codes(AN,As),
%   number_codes(OId,As).
  
% set_pred_id(O,Id) :-
%   (retract(pred_id(O,_IId)) ; true), 
%   !,
%   assertz(pred_id(O,Id)).
  
reset_pred_id :-
  retractall(pred_id(_O,_I)).

% Reset all system identifiers
reset_ids :-  
  reset_null_id,
  reset_rule_id,
  reset_pred_id.
    
% % Maximum predicate identifier
% max_pred_id(T,P,I) :- 
%   max_pred_id(T,P,-1,I). 

% max_pred_id(T,_P,I,I) :- 
%   (number(T) ; var(T)),
%   !.
% max_pred_id(T,P,I,MI) :- 
%   atom(T),
%   !,
%   (atom_concat('$',P,DP),
%    atom_concat(DP,AN,T),
%    atom_codes(AN,As),
%    number_codes(N,As)
%    ->
%     (N>I -> MI = N ; MI=I)
%    ;
%     MI=I).
% max_pred_id(T,P,I,MI) :- 
%   T =.. [F|Ts],
%   max_pred_id(F,P,I,I1),
%   !, 
%   max_pred_id_list(Ts,P,I1,MI).

% max_pred_id_list([],_P,I,I).
% max_pred_id_list([T|Ts],P,I,MI) :-
%   !, 
%   max_pred_id(T,P,I,I1), 
%   max_pred_id_list(Ts,P,I1,MI).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Program Variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
% filter_NVs(+NVs,Vs,+FNVs)
% Filter pairs N=V in NVs for variables V occurring in Vs
filter_NVs(_NVs,[],[]).
filter_NVs(NVs,[V|Vs],[N=V|FNVs]) :-
  find_var_name(V,N,NVs),
  filter_NVs(NVs,Vs,FNVs).

% Filter
filter([],_FNVs,[]).
filter([NV|INVs],FNVs,[NV|ONVs]) :-
  my_member_var(NV,FNVs),
  !,
  filter(INVs,FNVs,ONVs).
filter([_INV|INVs],FNVs,ONVs) :-
  !,
  filter(INVs,FNVs,ONVs).

% remove_NV_alias(NVs,RNVs) :-
%   remove_NV_alias(NVs,[],RNVs).

% remove_NV_alias([],_Vs,[]).
% remove_NV_alias([_N=V|NVs],Vs,RNVs) :-
%   my_member_var(V,Vs),
%   !,
%   remove_NV_alias(NVs,Vs,RNVs).
% remove_NV_alias([N=V|NVs],Vs,[N=V|RNVs]) :-
%   remove_NV_alias(NVs,[V|Vs],RNVs).
  
% Conversion
'NVs2Vs'([],[]).
'NVs2Vs'([_N=V|NVs],[V|Vs]) :-
  'NVs2Vs'(NVs,Vs).
% 'NVs2Vs'([_N=V|NVs],IV) :-
%   'NVs2Vs'(NVs,Vs),
%   append(Vs,[V],IV).

'Vs2NVs'([],_NVs,[]).
'Vs2NVs'([V|Vs],NVs,[N=V|TNVs]) :-
  find_var_name(V,N,NVs),
%  my_var_name(V,N,NVs),
  'Vs2NVs'(Vs,NVs,TNVs).


% Get the program variable name, if not found, assign it a new name
my_var_name(V,N,NVs) :-
  find_var_name(V,N,NVs),
  !.
my_var_name(_V,N,NVs) :-
  name_var(NVs,N).
  
replace_var_by_name_list([],_NVs).
replace_var_by_name_list([V|Vs],NVs) :-
  my_var_name(V,N,NVs),
  V=N,
  replace_var_by_name_list(Vs,NVs).
  
% Get the program variable name for a list of variables, if not found, assign it a new name
my_var_name_list([],_INVs,[]).
my_var_name_list([V|Vs],INVs,[N=V|ONVs]) :-
  my_var_name(V,N,INVs),
  my_var_name_list(Vs,[N=V|INVs],ONVs).
  
% Find a program variable name from the variable
find_var_name(V,N,[N=V1|_NVs]) :- 
  V == V1,
  !.
find_var_name(V,N,[_NV|NVs]) :- 
  find_var_name(V,N,NVs).

% Find a program variable from its name
find_name_var(X,X,[]).
find_name_var(V,N,[N=V|_NVs]) :- 
  !.
find_name_var(V,N,[_NV|NVs]) :- 
  find_name_var(V,N,NVs).

% Find a list of program variable names
% find_var_name_list([],_NVs,[]).
% find_var_name_list([V|Vs],NVs,[N=V|ONVs]) :- 
%   find_var_name(V,N,NVs),
%   find_var_name_list(Vs,NVs,ONVs).

assign_new_var_names([],_NVs,[]).
assign_new_var_names([V|Vs],NVs,NNVs) :-
  find_var_name(V,_N,NVs),
  !,
  assign_new_var_names(Vs,NVs,NNVs).  
assign_new_var_names([V|Vs],NVs,[N=V|NNVs]) :-
  name_var(NVs,V,N),
  assign_new_var_names(Vs,[N=V|NVs],NNVs).  

% var_names(+Vs,+NVs,-FNVs)
% Returns FNVs from NVs only for Vs
% var_names([],_NVs,[]).
% var_names([V|Vs],NVs,[N=V|FNVs]) :-
%   my_var_name(V,N,NVs),
%   var_names(Vs,NVs,FNVs).

% Assign names to variables in a term
assign_NVs(T,NVs) :-
  term_variables(T,Vs),
  my_var_name_list(Vs,[],NVs).
  
assign_variable_names_list([],_NVs,[]).
assign_variable_names_list([T|Ts],NVs,[(T,Vs)|RTVs]) :-
  assign_variable_names(T,NVs,Vs),
  assign_variable_names_list(Ts,NVs,RTVs).

assign_variable_names_list([],[]).
assign_variable_names_list([T|Ts],[(T,Vs)|RTVs]) :-
  assign_variable_names(T,Vs),
  assign_variable_names_list(Ts,RTVs).

assign_variable_names(T,Vs) :-
  assign_variable_names(T,[],Vs).
  
assign_variable_names(T,JNVs,NVs) :-
  term_variables(T,Vs),
  name_NVs(Vs,JNVs,_,NVs).

% (NVs,AlreadyNamed by other procedure, Last name, Name vars)
name_NVs([],NVs,_,NVs).
name_NVs([V|Vs],JNVs,LN,NVs) :-  
  find_var_name(V,_N,JNVs),
  !,
  name_NVs(Vs,JNVs,LN,NVs).
name_NVs([V|Vs],JNVs,LN,NVs) :-
  name_var(JNVs,LN,N),
  name_NVs(Vs,[N=V|JNVs],N,NVs).

name_var(NVs,LN,N) :-
  (var(LN)
   -> 
    first(SN),
    atom_codes(TN,SN)
   ;
    next(LN,TN)),
  (member(TN=_,NVs)
   ->
    name_var(NVs,TN,N)
   ;
    TN=N).

name_var(NVs,N) :-
  name_var(NVs,_LN,N).
  
% name_underscored_var(NVs,N) :-
%   name_underscored_var(NVs,_LN,N).
%   
% name_underscored_var(NVs,LN,N) :-
%   [US]="_",
%   (var(LN)
%    -> 
%     first(SN),
%     atom_codes(TN,[US|SN])
%    ;
%     next(LN,UN),
%     atom_concat('_',UN,TN)
%   ),
%   (member(TN=_,NVs)
%    ->
%     name_var(NVs,TN,N)
%    ;
%     TN=N).

next(X,Y) :-
  atom_codes(X,SX),
  nextS(SX,SY),
  atom_codes(Y,SY).

nextS("",SF) :- 
  !, 
  first(SF).
nextS(SX,SY) :-
  append(SFX,[LX],SX),
  last([CZ]),
  (LX<CZ ->
   LY is LX+1,
   append(SFX,[LY],SY)
   ;
   nextS(SFX,SFY),
   first(SF),
   append(SFY,SF,SY)
  ).

first(SF) :-
  atom_codes('A',SF).
last(SL) :-
  atom_codes('Z',SL).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Database predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%
% ASSERT
%%%%%%%%%%%%%%%%%%%%

% Asserting a list of datalog rules
% Each rule is checked for constraint consistency
% before assert.
% If ok, then each rule is given a new id, asserted, 
% and its userdef constraints updated
my_assertz_DL_list(DLs,CId,Error) :-
  check_DL_constraints_list(DLs),
  !,
  (my_nocheck_assertz_list(DLs,CId)
   ->
    true
   ;
    my_retract_DL_list(DLs,Error),
    write_error_log(['Asserting rules.'])
  ).
my_assertz_DL_list(_DLs,_CId,true) :-
  true.
  % WARNING:
  %write_error_log(['Asserting rules due to integrity constraint violation.']).

my_nocheck_assertz_list(DLs,CId) :-
  assertz_dlrules(DLs),
  update_persistent_preds_from_dlrules(DLs,CId),
  update_userdef_constraints_list(DLs).
  
check_DL_constraints_list([]).
check_DL_constraints_list([DL|DLs]) :-
  check_DL_constraints(DL),
  check_DL_constraints_list(DLs).

% Assert in reverse order to get rule ids for compiled rules first, then for compilation roots
assertz_dlrules([]).
assertz_dlrules([DL]) :-
  DL=datalog(_R,_NVs,RId,_CId,_Ls,_Fid,_C),
  get_rule_id(RId),
  assertz(DL).
  % Update dependencies (predicates) of existing user-defined constraints
%  update_userdef_constraints(DL).
assertz_dlrules([DL,DL2|DLs]) :-
  assertz_dlrules([DL2|DLs]),
  assertz_dlrules([DL]).

% assertz_list Assert each element in the list
assertz_list([]).
assertz_list([X|Xs]) :-
  assertz(X),
  assertz_list(Xs).

% retract_list Retract each element in the list
retract_list([]).
retract_list([X|Xs]) :-
  retract(X),
  retract_list(Xs).

% retractall_list Retractall each element in the list
% retractall_list([]).
% retractall_list([X|Xs]) :-
%   retractall(X),
%   retractall_list(Xs).

% Get predicate (name/arity) from a rule
% get_predicate_from_rule(':-'(Head,_Body),Name/Arity) :-
%   functor(Head,Name,Arity).
% get_predicate_from_rule(Head,Name/Arity) :-
%   functor(Head,Name,Arity).


%%%%%%%%%%%%%%%%%%%%
% RETRACT
%%%%%%%%%%%%%%%%%%%%

% Retracting a list of rules of the form datalog/6
my_retract_DL_list([],_Error).
my_retract_DL_list([DL|DLs],Error) :-
  (my_retract(DL,Error) -> true ; Error=true),
  my_retract_DL_list(DLs,Error).

my_retract(DL,_Error) :-
  DL=datalog(R,_NVs,_RId,CId,_Ls,_Fid,_C),
  my_nocheck_retract(DL),
  (check_removing_DL_constraint(DL)
   -> 
    true
   ;
%    assertz(DL), 
    my_nocheck_assertz_list([DL],CId),
    fail),
  (R=':-'(H,_B) -> true ; R=H),
  functor(H,P,A),
  functor(G,P,A),
  ((datalog(G,_,_,_,_,_,_) ; datalog(':-'(G,_),_,_,_,_,_,_))
   ->
    % If there are other facts/rules for the predicate the rule belongs to
    % there is no need for updating either dependencies, user defined constraints or modes
    true
   ;
    % Update dependencies (predicates) of existing user defined constraints
    update_userdef_constraints(DL)
  ).
my_retract(_DL,true).

% my_retract_list([],_Error).
% my_retract_list([DL|DLs],Error) :-
%   my_retract(DL,Error),
%   my_retract_list(DLs,Error).

my_nocheck_retract(DL) :-
% Try to retract first from the in-memory database (either persistent predicate or not)
  (retract(DL)
   ->
    Retracted = true
   ;
    Retracted = false),
  % If persistent, try to retract from the external database
  dlrule_to_ruleNVs_list([DL],[(R,NVs)]),
  pred_rule(Name/Arity,R),
  dlrule_cid(DL,CId),
  (CId==[],
   is_persistent_predicate(Name/Arity)
   ->
    (rule_is_positive_fact(R)
     ->
      true % Positive facts are not stored in datalog_persistent
     ;
      retract(datalog_persistent(Name/Arity,DLs,UPDLs)),
      %DL = datalog(R,_,_,_,_,_),
      %retractall(DL), % It might be not transferred to the external DB because of unsafety or unsupported feature
      remove_one_element_if_exists_from_list(datalog(R,_,_,_,_,_,_),DLs,RDLs),
      remove_one_element_if_exists_from_list(datalog(R,_,_,_,_,_,_),UPDLs,RUPDLs),
      assertz(datalog_persistent(Name/Arity,RDLs,RUPDLs))
    ),
    !,
    functor(PredSchema,Name,Arity),
    my_persistent(Connection,PredSchema),
    drop_persistent_rule(Connection,R,NVs)
   ;
    Retracted == true % Nothing else to do with non-persistent predicates, but ensuring that the rule was indeed retracted
  ).

check_removing_DL_constraint(DL) :-
  check_removing_DL_FK_constraint(DL),
  check_removing_DL_userdef_constraint(DL).
  
check_removing_DL_FK_constraint(datalog(R,_,_,_,_,_,_)) :-
  (R=':-'(H,_B) -> true ; R=H),
  functor(H,FTablename,_A),
  FK=my_foreign_key('$des',_Tablename,_FK_AttNames,FTablename,_PK_AttNames,_RIds),
  (call(FK)
   ->
    (check_ctr(FK)
     ->
      fail % Looks for more foreign keys 
     ;
      !,
      fail % Constraint is violated. Fail 
    )
   ;
    true
  ).
check_removing_DL_FK_constraint(_DL).
  
check_removing_DL_userdef_constraint(datalog(R,_,_,_,_,_,_)) :-
  (R=':-'(H,_B) -> true ; R=H),
  functor(H,Pred,Arity),
  IC=my_integrity_constraint('$des',Preds,_Constraint,_NVs,_Head,_Ids,_SQL,_TableName),
  call(IC),
  (member(Pred/Arity, Preds) ->
    (check_ctr(IC) ->
      fail
     ;
      !,
      fail
    )
   ;
    true
  ).
check_removing_DL_userdef_constraint(_DL).

% Retracting all facts and only facts (not rules)
my_retract_all_facts(Head) :-
  retract(':-'(Head,true)),
  fail.
my_retract_all_facts(_Head).

my_retract_all_facts_list([]).
my_retract_all_facts_list([H|Hs]) :-
  my_retract_all_facts(H),
  my_retract_all_facts_list(Hs).

my_retract_fact(Head) :-
  retract(':-'(Head,true)).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constraint checking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 
% Check whether the provided Datalog fact is consistent w.r.t. integrity constraints
%
check_DL_constraints(DL) :-
  (check_ic(on)
%    current_db('$des'),
%    functor(DL,Name,Arity),
%    \+ is_persistent_predicate(Name/Arity)
   ->
    check_DL_type_ctr(DL),
    check_DL_NN_ctr(DL),
    check_DL_PK_ctr(DL),
    check_DL_CK_ctrs(DL),
    check_DL_FK_ctrs(DL),
    check_DL_FD_ctrs(DL),
    check_DL_user_ctrs(DL)
   ;
    true
  ).
  
%
% Check types of a Datalog rule
%
check_DL_type_ctr(DL) :-
  DL=datalog(R,_,_,_,_,_,_),
  (R=':-'(H,_B) -> true ; R=H),
  functor(H,T,A),
  (my_table('$des',T,A)
   ->
    (check_rule_types(R)
     ->
      true
     ;
%      get_table_types(T,DeclTypes),
      get_table_typed_schema(T,Schema),
%      write_error_log(['Type mismatch ',DeclTypes,' (table declaration)']),
%     A former Error info
      write_log_list(['$tab'(7),Schema,' (declared types).',nl]),
      write_tapi_eot,
      !,
      fail
    )
   ;
    true
  ).

%
% Check not nullables constraints of a Datalog fact
%  
check_DL_NN_ctr(DL) :-
  DL=datalog(Fact,_,_,_,_,_,_),
  (Fact\=':-'(_Head,_Body)
   ->
   (functor(Fact,Tablename,Arity),
    my_table('$des',Tablename,Arity)
    ->
     check_tuple_NN_ctr(Tablename,Fact) 
    ;
     true
   )
   ;
    true
  ).

%
% Check not nullables constraint of a Datalog fact
%  
check_tuple_NN_ctr(Tablename,Fact) :-
  my_not_nullables('$des',Tablename,NN_AttNames),
  build_PK_goal(Fact,Tablename,NN_AttNames,NN_Vars,_Goal),
  (\+ member('$NULL'(_Id),NN_Vars)
   ->
   !
  ;
   development_hide_nulls(Fact,HFact),
   write_error_log(['Not null violation ',Tablename,'.',NN_AttNames,nl,'       when trying to insert: ','$quoted'(HFact)]),
   !,
   fail
  ).
check_tuple_NN_ctr(_Tablename,_Fact).
   
%
% Check primary key constraint of a Datalog fact
%  
check_DL_PK_ctr(DL) :-
  check_DL_UN_ctrs(DL,pk).
 
%
% Check candidate key constraints of a Datalog fact
%  
check_DL_CK_ctrs(DL) :-
  check_DL_UN_ctrs(DL,ck).

%
% Check unique constraint of a Datalog fact (called from PK and CK)
%  
check_DL_UN_ctrs(DL,Kind) :-
  DL=datalog(Fact,_,_,CId,_,_,_),
  (Fact\=':-'(_Head,_Body) ->
   functor(Fact,Tablename,Arity),
   (my_table('$des',Tablename,Arity) ->
     check_tuple_UN_ctr(Tablename,Fact,CId,Kind)
    ;
     true)
   ;
    true).
    
%
% Check unique constraint of a Datalog fact
%  
check_tuple_UN_ctr(Tablename,Fact,CId,Kind) :-
  (Kind == pk -> 
    Message='Primary key',
    Declaration=my_primary_key('$des',Tablename,PK_AttNames)
   ;
    Message='Candidate key',
    Declaration=my_candidate_key('$des',Tablename,PK_AttNames)),
  call(Declaration),
  build_PK_goal(Fact,Tablename,PK_AttNames,PK_Vars,Goal),
  (\+ member('$NULL'(_Id),PK_Vars),
   get_answer(Goal,CId,[])
  ->
   true
  ;
   write_error_log(['',Message,' violation ',Tablename,'.',PK_AttNames,nl,'       when trying to insert: ',Fact]),
   !,
   fail
  ),
  fail  % Looks for all constraint declarations
  .
check_tuple_UN_ctr(_Tablename,_Fact,_CId,_Kind).   
    
    
build_PK_goal(Fact,Tablename,PK_AttNames,PK_Vars,Goal) :-
  functor(Fact,Tablename,Arity),
  functor(Goal,Tablename,Arity),
  project_tuple(Goal,PK_AttNames,PK_Vars),
  project_tuple(Fact,PK_AttNames,PK_Vars).
  
% build_PK_goal_arguments(Args,[],_Pos,RArgs) :-
%   !,
%   length(Args,L),
%   length(RArgs,L).
% build_PK_goal_arguments([Arg|Args],[Pos|Poss],Pos,[Arg|RArgs]) :-
%   !,
%   Pos1 is Pos+1,
%   build_PK_goal_arguments(Args,Poss,Pos1,RArgs).
% build_PK_goal_arguments([_Arg|Args],Poss,Pos,[_RArg|RArgs]) :-
%   Pos1 is Pos+1,
%   build_PK_goal_arguments(Args,Poss,Pos1,RArgs).
  
check_DL_FK_ctrs(DL) :-
  DL=datalog(Fact,_,_,_,_,_,_),
  (Fact\=':-'(_Head,_Body) ->
   functor(Fact,Tablename,Arity),
   (my_table('$des',Tablename,Arity) ->
     check_tuple_FK_ctr(Tablename,Fact,[]) 
    ;
     true)
   ;
    true).

%
% Check foreign key constraint of a Datalog fact
%  
check_tuple_FK_ctr(Tablename,Fact,CId) :-
  my_foreign_key('$des',Tablename,FK_AttNames,ForeignTablename,PK_AttNames,_RIds),
  \+ my_member_term('$NULL'(_),Fact), % Nulls are not checked
  build_FK_goal(Fact,FK_AttNames,ForeignTablename,PK_AttNames,_FK_Vars,Goal),
  (get_answer(Goal,CId,[])
   ->
    write_error_log(['Foreign key violation ',Tablename,'.',FK_AttNames,'->',ForeignTablename,'.',PK_AttNames,nl,'       when trying to insert: ',Fact]),
    !,
    fail
   ;
    true
  ),
  fail  % Looks for all foreign key declarations
  .
check_tuple_FK_ctr(_Tablename,_Fact,_CId).

    
% Build a goal that looks an entry in the foreign table with the same values in the corresponding positions
build_FK_goal(Fact,FK_AttNames,ForeignTablename,PK_AttNames,FK_Vars,Goal) :-
  project_tuple(Fact,FK_AttNames,FK_Vars),
  my_table('$des',ForeignTablename,Arity),
  functor(Goal,ForeignTablename,Arity),
  project_tuple(Goal,PK_AttNames,FK_Vars).

project_tuple(Tuple,AttNames,ValueList) :-
%  Tuple=..[Tablename|TableArgs],
  functor(Tuple,Tablename,_),
  get_att_positions(Tablename,AttNames,Positions),
  get_ith_arg_list(Positions,Tuple,ValueList).
%  filter_positions(TableArgs,Positions,ValueList).

filter_positions(TableArgs,Positions,ValueList) :-
  filter_positions(TableArgs,Positions,1,ValueList).
  
filter_positions(_Args,[],_I,[]) :-
  !.
filter_positions([Arg|Args],[I|Is],I,[Arg|RArgs]) :-
  !,
  I1 is I+1,
  filter_positions(Args,Is,I1,RArgs).
filter_positions([_Arg|Args],Is,I,RArgs) :-
  I1 is I+1,
  filter_positions(Args,Is,I1,RArgs).

get_att_positions(Tablename,AttNames,AttPositions) :-
  bagof(AttPosition,
        AttName^DataType^
        (member(AttName,AttNames),
         my_attribute('$des',AttPosition,Tablename,AttName,DataType)
        ),
        AttPositions).
  
% get_att_positions(Tablename,AttNames,AttPositions) :-
%   setof(AttPosition,
%         AttName^DataType^
%         (my_attribute('$des',AttPosition,Tablename,AttName,DataType),
%          member(AttName,AttNames)
%         ),
%         AttPositions).
  
build_FK_goal_arguments([],[],Arity,I,Args) :-
  !,
  TL is Arity-I+1,
  length(Args,TL).
build_FK_goal_arguments([Value|Values],[I|Is],Arity,I,[Value|Args]) :-
  !,
  I1 is I+1,
  build_FK_goal_arguments(Values,Is,Arity,I1,Args).
build_FK_goal_arguments(Values,Is,Arity,I,[_Var|Args]) :-
  !,
  I1 is I+1,
  build_FK_goal_arguments(Values,Is,Arity,I1,Args).
  
  
%
% Check functional dependency constraint of a Datalog fact
%  
check_DL_FD_ctrs(DL) :-
  DL=datalog(Fact,_,_,_,_,_,_),
  (Fact\=':-'(_Head,_Body) ->
   functor(Fact,Tablename,Arity),
   (my_table('$des',Tablename,Arity) ->
     check_tuple_FD_ctr(Tablename,Fact,[]) 
    ;
     true)
   ;
    true).

check_tuple_FD_ctr(Tablename,Fact,CId) :-
  my_functional_dependency('$des',Tablename,AttNames,DepAttNames),
  build_FD_goal(Fact,AttNames,DepAttNames,Goal),
  (get_answer(Goal,CId,[Witness|_]) ->
    get_table_untyped_schema('$des',Tablename,Table),
    Witness=..[_|WArs],
    WTuple=..[Tablename|WArs],
    write_error_log(['Functional dependency violation ',Tablename,'.',AttNames,'->',Tablename,'.',DepAttNames,nl,'       in table ',Table,nl,'       when trying to insert: ',Fact,nl,'       Witness tuple        : ',WTuple]),
    !,
    fail
   ;
    true
  ),
  fail  % Looks for all functional dependency constraint declarations
  .
check_tuple_FD_ctr(_Tablename,_Fact,_CId).
  
build_FD_goal(Fact,AttNames,DepAttNames,Goal) :-
  functor(Fact,Tablename,Arity),
  project_tuple(Fact,AttNames,ValueList),
  get_att_positions(Tablename,AttNames,AttPositions),
  my_table('$des',Tablename,Arity),
  build_FK_goal_arguments(ValueList,AttPositions,Arity,1,Args),
  Goal1=..[Tablename|Args],
  get_att_positions(Tablename,DepAttNames,DepAttPositions),
  filter_positions(Args,DepAttPositions,DepVars),
  project_tuple(Fact,DepAttNames,DepValues),
  my_zipWith('\\=',DepVars,DepValues,DisjList),
  my_list_to_disjunction(DisjList,Goal2),
  FD=..[fd|Args],
  Goal=':-'(FD,(Goal1,Goal2)).
  
%
% Check user-defined integrity constraint of a rule
%  
check_DL_user_ctrs(DL) :-
  copy_term(DL,CDL),
  CDL=datalog(Rule,_,RId,CId,_,_,_),
  (Rule=':-'(H,_B),
   !
   ;
   Rule=H
  ),
  functor(H,F,A),
  get_userdef_integrity_ctrs(F/A,Ctrs),
  % Temporarily use a new Rule Id., which is not wasted as the rule is retracted afterwards
  get_rule_id(RId),
  assertz(CDL),
  (check_ctr_list(Ctrs,CId) 
   ->
    retract(CDL),
    pop_rule_id
   ;
    retract(CDL),
    pop_rule_id,
    !,
    fail
  ).

  
% Update dependencies of user-defined integrity constraints related to a Datalog rule  
update_userdef_constraints_list([]).
update_userdef_constraints_list([DL|DLs]) :-
  update_userdef_constraints(DL),
  update_userdef_constraints_list(DLs).

update_userdef_constraints(DL) :-
  DL=datalog(Rule,_,_,_,_,_,_),
  (Rule=':-'(H,_B),
   !
   ;
   Rule=H
  ),
  functor(H,F,A),
  get_userdef_integrity_ctrs(F/A,Ctrs),
  update_userdef_constraint_list(Ctrs).
  
update_userdef_constraint_list([]).
update_userdef_constraint_list([Ctr|Ctrs]) :-
  update_userdef_constraint(Ctr),
  update_userdef_constraint_list(Ctrs).
  
update_userdef_constraint(my_integrity_constraint(_DB,_Preds,_Constraint,_NVs,_Head,_Ids,SQL,_TableName)) :-
  SQL\==no_sql,
  !.
update_userdef_constraint(my_integrity_constraint(DB,OldPreds,Constraint,NVs,Head,Ids,SQL,TableName)) :-
  reachable_user_predicates_rule(':-'(Constraint),Preds),
  (OldPreds\==Preds ->
    my_retract_all_facts(my_integrity_constraint(DB,_OldPreds,Constraint,NVs,Head,Ids,SQL,TableName)),
    assertz(my_integrity_constraint(DB,Preds,Constraint,NVs,Head,Ids,SQL,TableName))
   ;
    true
  ).
  
    
% Get all the user-defined integrity constraints involving a predicate F/A
get_userdef_integrity_ctrs(F/A,Ctrs) :-
  findall(Ctr,
          (Ctr=my_integrity_constraint('$des',Preds,_B,_NVs,_H,_Ids,_SQL,_TableName),
           call(Ctr),
           member(F/A,Preds)
           ),
          Ctrs).
          
check_ctr_list([],_).
check_ctr_list([Ctr|Ctrs],CId) :-
  check_ctr(Ctr,CId),
  check_ctr_list(Ctrs,CId).
  
check_ctr_failing_list([],_).
check_ctr_failing_list([Ctr|Ctrs],CId) :-
  check_ctr(Ctr,CId),
  !,
  check_ctr_failing_list(Ctrs,CId).
check_ctr_failing_list([Ctr|Ctrs],CId) :-
%  internal_ctr_program_ctr(Ctr,ProgramCtr,CId),
  internal_ctr_program_ctr(Ctr,ProgramCtr),
  write_error_log(['In constraint: ',ProgramCtr,nl]),
  check_ctr_failing_list(Ctrs,CId).
  
% Representation conversion for constraints:

internal_ctr_program_ctr(my_not_nullables('$des',Tablename,Colnames),':-'(nn(Tablename,Colnames))).
internal_ctr_program_ctr(my_primary_key('$des',Tablename,Colnames),':-'(pk(Tablename,Colnames))).
internal_ctr_program_ctr(my_candidate_key('$des',Tablename,Colnames),':-'(ck(Tablename,Colnames))).
internal_ctr_program_ctr(my_foreign_key('$des',Tablename,Colnames,FTableName,FColnames,_RIds),':-'(fk(Tablename,Colnames,FTableName,FColnames))).
internal_ctr_program_ctr(my_functional_dependency('$des',Tablename,Colnames,DColnames),':-'(fd(Tablename,Colnames,DColnames))).
internal_ctr_program_ctr(my_integrity_constraint('$des',_Preds,Ctr,NVs,_Head,_Ids,_SQL,_TableName),':-'(CtrNVs)) :-
  term_to_term_NVs(Ctr,NVs,CtrNVs).

%constraint_ic(type(ColumnsTypes),TableName,type(TableName,ColumnsTypes)).
constraint_ic(not_nullables(Columns),TableName,nn(TableName,Columns)).
constraint_ic(primary_key(Columns),TableName,pk(TableName,Columns)).
constraint_ic(candidate_key(Columns),TableName,ck(TableName,Columns)).
constraint_ic(foreign_key(Columns,RTablename,RColumns),TableName,fk(TableName,Columns,RTablename,RColumns)).
constraint_ic(fd(Columns,DepColumns),TableName,fd(TableName,Columns,DepColumns)).
constraint_ic(my_sql_check_constraint(SQLCondition),TableName,my_integrity_constraint('$des',Preds,Body,NVs,Head,Ids,SQLCondition,TableName)) :-
  my_integrity_constraint('$des',Preds,Body,NVs,Head,Ids,SQLCondition,TableName).
%constraint_ic(my_sql_check_constraint(SQLCondition),TableName,my_integrity_constraint(Preds,Constraint)).

/*********************************************************************/
/* Finding Datalog source Rules matching a pattern: get_source_dlrules */
/*********************************************************************/

% Get source rules from a list of rules
% get_source_dlrules_list([],[]).
% get_source_dlrules_list([(Rule,_NVs)|Rules],CRules) :-
%   get_source_dlrules(rule,Rule,CRules1),
%   get_source_dlrules_list(Rules,CRules2),
%   append(CRules1,CRules2,CDRules),
%   remove_duplicates(CDRules,CRules).

% Get source rules as they were originally typed. Optionally filtered by name and arity
get_source_dlrules(DLs) :-
  get_filtered_source_dlrules([],DLs).
  
get_filtered_source_dlrules(Fs,DLs) :-
   rule_filtering(Fs,FId,RId,GFs),
   findall(datalog(R,NVs,RId,CId,Ls,FId,C),
        ((C=source, 
          get_datalog(R,NVs,RId,CId,Ls,FId,C),
          call(GFs)
         ;
          C=compilation(_SR,_SNVs,_CDLs),
          get_datalog(R,NVs,RId,CId,Ls,FId,C),
          call(GFs)
         ),
         (R=':-'(SH,_SB) -> functor(SH,F,_A), \+ (is_system_identifier(F)) ; true)
        ),
        DLs).

rule_filtering(Fs,FId,RId,GFs) :-
   (member(asserted,Fs) -> FId = asserted(_Time) ; true),
   (member(filter_rids(RIds),Fs) -> GFs = (\+ member(RId,RIds)) ; GFs=true).

get_source_dlrules(name,N,DLs) :-
  get_filtered_source_dlrules(name,N,[],DLs).
get_source_dlrules(head,H,DLs) :-
  get_filtered_source_dlrules(head,H,[],DLs).
get_source_dlrules(namearity,N/A,DLs) :-
  get_filtered_source_dlrules(namearity,N/A,[],DLs).

get_filtered_source_dlrules(name,N,Fs,DLs) :-
  rule_filtering(Fs,FId,RId,GFs),
  findall(datalog(R,NVs,RId,CId,Ls,FId,C),
        ((C=source,
          get_datalog(R,NVs,RId,CId,Ls,FId,C),
          call(GFs),
          (R=':-'(SH,_) -> true ; R=SH)
         ;
          C=compilation(':-'(SH,_),_SNVs,_CRIds), 
          get_datalog(R,NVs,RId,CId,Ls,FId,C),
          call(GFs)
         ),
         positive_name(N,PN),
         (is_negative_name(N) -> is_negative_rule(SH) ; true), % If positive, return both. If negative, return only negative rules
         rule_pred(SH,PN/_A)
        ),
        DLs).

get_filtered_source_dlrules(namearity,N/A,Fs,DLs) :-
  rule_filtering(Fs,FId,RId,GFs),
  positive_name(N,PN),
  findall(datalog(R,NVs,RId,CId,Ls,FId,C),
        (
         (C=source,
          (functor(PR,PN,A),
           (R= -(PR) ; R=PR)
           ; 
           functor(PH,PN,A),
           R = ':-'(H,_),
           (H= -(PH) ; H=PH)
          ),
          get_datalog(R,NVs,RId,CId,Ls,FId,C),
          call(GFs),
          (R=':-'(SH,_) -> true ; R=SH)
         ;
          C=compilation(CR,_SNVs,_CRIds), 
          functor(PH,PN,A),
          CR = ':-'(H,_),
          (H= -(PH) ; H=PH),
          get_datalog(R,NVs,RId,CId,Ls,FId,C),
          call(GFs),
          (R=':-'(SH,_) -> true ; R=SH)
         ),
         positive_name(N,PN),
         (is_negative_name(N) -> is_negative_rule(SH) ; true), % If positive, return both. If negative, return only negative rules
         rule_pred(SH,PN/_A)
        ),
        DLs).

get_filtered_source_dlrules(head,H,Fs,DLs) :-
  rule_filtering(Fs,FId,RId,GFs),
  findall(datalog(CR,NVs,RId,CId,Ls,FId,C),
        (
         rule_pred(H,N/A),
         functor(PH,N,A),
         (is_negative_rule(H) -> SH= -(PH) ; SH=PH),
         (C=source,
          (R = SH
          ; 
           R = ':-'(SH,_) 
          ),
          get_datalog(R,NVs,RId,CId,Ls,FId,C),
          call(GFs),
          CR=R
         ;
          R=':-'(SH,_),
          C=compilation(R,_SNVs,_CRIds), 
          get_datalog(CR,NVs,RId,CId,Ls,FId,C),
          call(GFs)
         ),
         my_subsumes(H,SH)
        ),
        DLs).

% % Persistent rules:
% get_source_dlrules(rule,PR,DLs) :-
%   PR = ':-'(PH,_PB),
%   functor(PH,N,A),
%   datalog_persistent(N/A,PDLs),
%   !,
%   findall(DL,
%           ((C=source,
%             DL=datalog(R,NVs,RId,Ls,FId,C),
%             member(DL,PDLs), 
%             R=':-'(SH,SB), 
%             SR=R
%            ;
%             functor(SH,N,A),
%             C=compilation(':-'(SH,SB),_SNVs,_CRIds), 
%             DL=datalog(R,NVs,RId,Ls,FId,C),
%             member(DL,PDLs), 
%             SR=':-'(SH,SB)
%            ),
%            my_subsumes(PR,SR)
%           ),
%           DLs).
% Rules:
get_filtered_source_dlrules(rule,PR,Fs,DLs) :-
  ((PR = ':-'(-(_),_), 
    SH = -(_))
   ;
   (PR = ':-'(_,_))
  ),
  !,
   rule_filtering(Fs,FId,RId,GFs),
%  (my_ground(PR) -> R=PR ; true),
%  functor(PH,N,A),
%  functor(H,N,A),
  findall(datalog(R,NVs,RId,CId,Ls,FId,C),
        ((C=source,
          get_datalog(R,NVs,RId,CId,Ls,FId,C),
          call(GFs), 
          R=':-'(SH,SB), 
          SR=R
         ;
          C=compilation(SR,_SNVs,_CRIds), 
          get_datalog(R,NVs,RId,CId,Ls,FId,C),
          call(GFs),
          SR=':-'(SH,SB)
         ),
         my_subsumes(PR,SR)
        ),
        DLs).
% Facts:
get_filtered_source_dlrules(rule,PR,Fs,DLs) :-
   rule_filtering(Fs,FId,RId,GFs),
%  (my_ground(PR) -> R=PR ; true),
  functor(PR,N,A),
  functor(R,N,A),
%   my_nf_bagof(datalog(R,NVs,RId,CId,Ls,FId,C),
%         SNVs^SH^SB^SR^CRIds^
  findall(datalog(R,NVs,RId,CId,Ls,FId,C),
        ((C=source,
          datalog(R,NVs,RId,CId,Ls,FId,C),  % get_datalog?
          call(GFs), 
          (R=':-'(SH,_) -> true ; R=SH), 
          SR=R
         ;
          C=compilation(':-'(SH,SB),_SNVs,_CRIds), 
          datalog(R,NVs,RId,CId,Ls,FId,C),  % get_datalog?
          call(GFs),
          SR=':-'(SH,SB)
         ),
         my_subsumes(PR,SR)
        ),
        DLs).

% functor_pred_rule(':-'(H,_B),N,A) :-
%   !,
%   functor(H,N,A).
% functor_pred_rule(H,N,A) :-
%   !,
%   functor(H,N,A).

% get_source_dlrules(namearity,N/A,FId,DLs) :-
%   functor(R,N,A),
%   my_nf_bagof(datalog(R,NVs,RId,Ls,FId,C),
%         SNVs^SH^SB^CRIds^
%         ((C=source,
%           datalog(R,NVs,RId,Ls,FId,C), 
%           (R=':-'(SH,SB) -> true ; R=SH)
%          ;
%           C=compilation(':-'(SH,SB),SNVs,CRIds), 
%           datalog(R,NVs,RId,Ls,FId,C)
%          ),
%          functor(SH,N,A)
%         ),
%         DLs).

% Get source local rules.  Filtered by name and arity
% Discarding those in external DBs
get_des_source_dlrules(namearity,N/A,DLs) :-
  Head = datalog(Rule,[],RuleId,_CId,[],rdb(Connection),source),
  Body = datalog_rdb(Rule,[],RuleId,[],rdb(Connection),source),
  findall(':-'(Head,Body),clause(Head,Body),RDBDSs),
  findall(datalog_persistent(Pred,DLs,UPDLs),clause(datalog_persistent(Pred,DLs,UPDLs),true),DPs),
  retract_list(RDBDSs),
  retract_list(DPs),
  get_source_dlrules(namearity,N/A,DLs),
  assertz_list(RDBDSs),
  assertz_list(DPs).

% Get object local rules. Filtered by name and arity
% Discarding those in external DBs,
get_des_object_dlrules(namearity,N/A,DLs) :-
  Head = datalog(Rule,[],RuleId,_CId,[],rdb(Connection),source),
  Body = datalog_rdb(Rule,[],RuleId,[],rdb(Connection),source),
  findall(':-'(Head,Body),clause(Head,Body),RDBDSs),
  findall(datalog_persistent(Pred,DLs,UPDLs),clause(datalog_persistent(Pred,DLs,UPDLs),true),DPs),
  retract_list(RDBDSs),
  retract_list(DPs),
  get_object_dlrules(namearity,N/A,DLs),
  assertz_list(RDBDSs),
  assertz_list(DPs).

% Get datalog local rules. Filtered by name and arity
% Discarding those in external DBs,
get_des_dlrules(namearity,N/A,DLs) :-
  Head = datalog(Rule,[],RuleId,_CId,[],rdb(Connection),source),
  Body = datalog_rdb(Rule,[],RuleId,[],rdb(Connection),source),
  findall(':-'(Head,Body),clause(Head,Body),RDBDSs),
  findall(datalog_persistent(Pred,DLs,UPDLs),clause(datalog_persistent(Pred,DLs,UPDLs),true),DPs),
  retract_list(RDBDSs),
  retract_list(DPs),
  get_dlrules(namearity,N/A,DLs),
  assertz_list(RDBDSs),
  assertz_list(DPs).

% get_local_and_persistent_dlrules(namearity,+Name/Arity,-DLs,-PDLs)
% Get local and persistent object datalog rules in DLs
% Get also only persistent object datalog rules in PDLs
get_local_and_persistent_dlrules(namearity,Name/Arity,DLs,PDLs) :-
%   get_des_object_dlrules(namearity,Name/Arity,LDLs),
  get_des_dlrules(namearity,Name/Arity,LDLs),
  (datalog_persistent(Name/Arity,PDLs,_UPDLs)
   ->
    append(PDLs,LDLs,DLs)
   ;
    DLs=LDLs,
    PDLs=[]).
    
get_dlrules(namearity,Name/Arity,DLs) :-
  findall(datalog(R,NVs,RId,CId,Ls,FId,C),
         (
          (functor(PH,Name,Arity),
           R = H
           ; 
           functor(PH,Name,Arity),
           R = ':-'(H,_B) 
          ),
          (H= PH ; H= -PH),
          datalog(R,NVs,RId,CId,Ls,FId,C)
         ),
         DLs).

% get_local_and_persistent_source_dlrules(namearity,Name/Arity,DLs,PDLs) :-
%   get_des_source_dlrules(namearity,Name/Arity,LDLs),
%   (datalog_persistent(Name/Arity,PDLs,_UPDLs)
%    ->
%     append(PDLs,LDLs,DLs)
%    ;
%     DLs=LDLs,
%     PDLs=[]).

/*********************************************************************/
/* Finding Datalog Object Rules: get_object_dlrules                  */
/*********************************************************************/
% Get the rules that are actually used in a computation. 
% They include rules as typed by the programmer (source) and compiled rules  
% from others which cannot be directly computed (compilation)

% Types of rules:
% - Uncompiled: 
%    * Tagged as 'source'
% - Compiled: 
%    * The root of a compilation tagged as 'compilation'(SourceHead,SourceBody,ListOfCompiledRules)
%      The list of compiled rules may contain additional compilations.
%    * A leaf of a compilation is tagged as 'compiled'.

% Get object rules from a list of source rules
get_object_dlrules_list([],[]).
get_object_dlrules_list([(Rule,_NVs)|Rules],CRules) :-
  get_object_dlrules(rule,Rule,CRules1),
  get_object_dlrules_list(Rules,CRules2),
  append(CRules1,CRules2,CRules).


% Get object rules. 
get_object_dlrules(DLs) :-
  get_filtered_object_dlrules([],DLs).
  
get_filtered_object_dlrules(Fs,DLs) :-
   rule_filtering(Fs,FId,RId,GFs),
%  my_nf_bagof(datalog(R,NVs,RId,CId,Ls,FId,C),
  findall(datalog(R,NVs,RId,CId,Ls,FId,C),
          (get_datalog(R,NVs,RId,CId,Ls,FId,C),call(GFs)),
          DLs).

% Get object rules. Filtered by a given source rule
get_object_dlrules(datalog(R,NVs,RId,CId,Ls,FId,source),[datalog(R,NVs,RId,CId,Ls,FId,source)]) :-
  !.
get_object_dlrules(datalog(R,NVs,RId,CId,Ls,FId,compilation(':-'(H,B),SNVs,RuleIds)),
                  [datalog(R,NVs,RId,CId,Ls,FId,compilation(':-'(H,B),SNVs,RuleIds))|ODLs]) :-
  !,
  get_dependent_dlrules([datalog(R,NVs,RId,CId,Ls,FId,compilation(':-'(H,B),SNVs,RuleIds))],ODLs).

% Get object rules. Filtered by name
get_object_dlrules(name,N,DLs) :-
  get_filtered_object_dlrules(name,N,[],DLs).
  
% Get object rules. Filtered by name and arity
get_object_dlrules(namearity,N/A,DLs) :-
  get_filtered_object_dlrules(namearity,N/A,[],DLs).
  
% Get object rules. Filtered by head
get_object_dlrules(head,H,DLs) :-
  get_filtered_object_dlrules(head,H,[],DLs).
  
% Get object rules. Filtered by rule. Only rules, not facts
get_object_dlrules(rule,(H:-B),DLs) :-
  !,
  get_filtered_object_dlrules(rule,(H:-B),[],DLs).
get_object_dlrules(rule,_,[]).
  
listing_source(N,source) :-
  \+ is_system_identifier(N),
  !.
listing_source(_,compiled).

get_filtered_object_dlrules(rule,R,Fs,DLs) :-
  pred_rule(N/_A,R),
  listing_source(N,source),
  !,
  get_filtered_nonroot_dlrules_from_rule(R,source,Fs,UDLs),
  get_filtered_root_compiled_dlrules_from_rule(R,Fs,RDLs),
  get_dependent_dlrules(RDLs,DDLs),
  concat_lists([UDLs,RDLs,DDLs],DLs).
get_filtered_object_dlrules(rule,R,Fs,DLs) :-
  get_filtered_nonroot_dlrules_from_rule(R,_Source,Fs,DLs).

get_filtered_object_dlrules(name,N,Fs,DLs) :-
  listing_source(N,source),
  !,
  get_filtered_nonroot_dlrules(N,source,Fs,UDLs),
  get_filtered_root_compiled_dlrules(N,Fs,RDLs),
  get_dependent_dlrules(RDLs,DDLs),
  concat_lists([UDLs,RDLs,DDLs],DLs).
get_filtered_object_dlrules(name,N,Fs,DLs) :-
  get_filtered_nonroot_dlrules(N,_Source,Fs,DLs).

get_filtered_object_dlrules(namearity,N/A,Fs,DLs) :-
  listing_source(N,source),
  !,
  get_filtered_nonroot_dlrules(N,A,source,Fs,UDLs),
  get_filtered_root_compiled_dlrules(N,A,Fs,RDLs),
  get_dependent_dlrules(RDLs,DDLs),
  concat_lists([UDLs,RDLs,DDLs],DLs).
get_filtered_object_dlrules(namearity,N/A,Fs,DLs) :-
  get_filtered_nonroot_dlrules(N,A,_Source,Fs,DLs).

get_filtered_object_dlrules(head,H,Fs,DLs) :-
  pred_rule(N/_A,H),
  listing_source(N,source),
  !,
  get_filtered_nonroot_dlrules_from_head(H,source,Fs,UDLs),
  get_filtered_root_compiled_dlrules_from_head(H,Fs,RDLs),
  get_dependent_dlrules(RDLs,DDLs),
  concat_lists([UDLs,RDLs,DDLs],DLs).
get_filtered_object_dlrules(head,H,Fs,DLs) :-
  get_filtered_nonroot_dlrules_from_head(H,_Source,Fs,DLs).

% Get uncompiled rules. Filtered by name
get_filtered_nonroot_dlrules(N,Source,Fs,DLs) :-
  rule_filtering(Fs,FId,RId,GFs),
  findall(datalog(R,NVs,RId,CId,Ls,FId,Source),
          (
           get_datalog(R,NVs,RId,CId,Ls,FId,Source),
           call(GFs),
          (R=':-'(SH,_) -> true ; R=SH),
           positive_name(N,PN),
           (is_negative_name(N) -> is_negative_rule(SH) ; true), % If positive, return both. If negative, return only negative rules
           rule_pred(SH,PN/_A)
          ),
          DLs).

% Get uncompiled rules. Filtered by name and arity
get_filtered_nonroot_dlrules(N,A,Source,Fs,DLs) :-
  rule_filtering(Fs,FId,RId,GFs),
  positive_name(N,PN),
  findall(datalog(R,NVs,RId,CId,Ls,FId,Source),
         ((functor(PR,PN,A),
           (R= -(PR) ; R=PR)
          ; 
           functor(PH,PN,A),
           R = ':-'(H,_),
           (H= -(PH) ; H=PH)
          ),
          get_datalog(R,NVs,RId,CId,Ls,FId,Source),
          call(GFs),
          (R=':-'(SH,_) -> true ; R=SH),
          positive_name(N,PN),
          (is_negative_name(N) -> is_negative_rule(SH) ; true), % If positive, return both. If negative, return only negative rules
          rule_pred(SH,PN/_A)
         ),
        DLs).

% Get uncompiled rules. Filtered by head
get_filtered_nonroot_dlrules_from_head(H,Source,Fs,DLs) :-
  rule_filtering(Fs,FId,RId,GFs),
  findall(datalog(R,NVs,RId,CId,Ls,FId,Source),
%   my_nf_bagof(datalog(R,NVs,RId,CId,Ls,FId,source),
%         H^B^
        (
         rule_pred(H,N/A),
         functor(PH,N,A),
         (is_negative_rule(H) -> SH= -(PH) ; SH=PH),
         (R = SH
         ; 
          R = ':-'(SH,_) 
         ),
         get_datalog(R,NVs,RId,CId,Ls,FId,Source),
         call(GFs),
         my_subsumes(H,SH)
        ),
        DLs).

% Get uncompiled rules. Filtered by rule
get_filtered_nonroot_dlrules_from_rule(PR,Source,Fs,DLs) :-
  ((PR = ':-'(-(_),_), 
    SH = -(_))
   ;
   (PR = ':-'(_,_))
  ),
  rule_filtering(Fs,FId,RId,GFs),
%  (my_ground(PR) -> R=PR ; true),
%  functor(PH,N,A),
%  functor(H,N,A),
  findall(datalog(R,NVs,RId,CId,Ls,FId,Source),
%   my_nf_bagof(datalog(R,NVs,RId,CId,Ls,FId,source),
        (get_datalog(R,NVs,RId,CId,Ls,FId,Source),
         call(GFs), 
         R=':-'(SH,_), 
         my_subsumes(PR,R)
         ),
        DLs).


% get_root_compiled_dlrules(RNVss) :-
%   my_nf_bagof((R,NVs),
%         RId^Ls^FId^Rs^
%         (get_datalog(R,NVs,RId,Ls,FId,Rs)),
%         RNVss).

% Get rules that are compilation roots. Filtered by source rule name
get_filtered_root_compiled_dlrules(N,Fs,DLs) :-
  rule_filtering(Fs,FId,RId,GFs),
  findall(datalog(R,NVs,RId,CId,Ls,FId,compilation(':-'(SH,SB),SNVs,RuleIds)),
        (get_datalog(R,NVs,RId,CId,Ls,FId,compilation(':-'(SH,SB),SNVs,RuleIds)),
         call(GFs),
         (R=':-'(H,_B) -> true ; H=R), 
         functor(SH,N,_A)
        ),
        DLs).

% Get rules that are compilation roots. Filtered by source rule name and arity
get_filtered_root_compiled_dlrules(N,A,Fs,DLs) :-
  rule_filtering(Fs,FId,RId,GFs),
  positive_name(N,PN),
  findall(datalog(R,NVs,RId,CId,Ls,FId,compilation(':-'(SH,SB),SNVs,RuleIds)),
       (
        functor(PH,PN,A),
        (SH= -(PH) ; SH=PH),
        get_datalog(R,NVs,RId,CId,Ls,FId,compilation(':-'(SH,SB),SNVs,RuleIds)),
        call(GFs),
        (R=':-'(SH,_) -> true ; R=SH),
        (is_negative_name(N) -> is_negative_rule(SH) ; true), % If positive, return both. If negative, return only negative rules
        rule_pred(SH,PN/_A)
       ),
       DLs).

% Get rules that are compilation roots. Filtered by source rule head
get_filtered_root_compiled_dlrules_from_head(PH,Fs,DLs) :-
  rule_filtering(Fs,FId,RId,GFs),
  findall(datalog(R,NVs,RId,CId,Ls,FId,compilation(':-'(SH,SB),SNVs,RuleIds)),
        (get_datalog(R,NVs,RId,CId,Ls,FId,compilation(':-'(SH,SB),SNVs,RuleIds)),
         call(GFs),
         my_subsumes(PH,SH)),
        DLs).

% Get rules that are compilation roots. Filtered by source rule 
get_filtered_root_compiled_dlrules_from_rule(PR,Fs,DLs) :-
  ((PR = ':-'(-(_),_), 
    SH = -(_))
   ;
   (PR = ':-'(_,_))
  ),
  rule_filtering(Fs,FId,RId,GFs),
%  (my_ground(PR) -> R=PR ; true),
%  functor(PH,N,A),
%  functor(H,N,A),
  findall(datalog(R,NVs,RId,CId,Ls,FId,compilation(':-'(SH,SB),SNVs,RuleIds)),
        (get_datalog(R,NVs,RId,CId,Ls,FId,compilation(':-'(SH,SB),SNVs,RuleIds)),
         call(GFs),
         my_subsumes(PR,':-'(SH,SB))),
        DLs).

% Get rules which are the result from a compilation. There may be further compilations from a given root
get_dependent_dlrules([],[]).
get_dependent_dlrules([datalog(R,NVs,RId,CId,Ls,FId,compilation(':-'(H,B),SNVs,RIds))|DLs],DDLs) :-
  !,
  findall(CDLs,
        (get_datalog(R,NVs,RId,CId,Ls,FId,compilation(':-'(H,B),SNVs,RIds)),
         findall(datalog(DR,DNVs,CRId,DCId,D_Ls,DFId,DC),
                (member(CRId,RIds),
                 get_datalog(DR,DNVs,CRId,DCId,D_Ls,DFId,DC)
                ),
                CDLs)
        ),
        CDLss),
  concat_lists(CDLss,CCDLs),
  get_dependent_dlrules(CCDLs,HDLs),
  get_dependent_dlrules(DLs,TDLs),
  concat_lists([CCDLs,HDLs,TDLs],DDLs).
get_dependent_dlrules([_|DLs],DDLs) :-
  get_dependent_dlrules(DLs,DDLs).
  
  
% Get non-persistent rule by Id
get_datalog(R,NVs,RId,CId,Ls,FId,Source) :-
  var(R), 
  nonvar(RId),
  datalog(R,NVs,RId,CId,Ls,FId,Source).
% Get persistent rule by Id
get_datalog(R,NVs,RId,CId,Ls,FId,Source) :-
  var(R), 
  nonvar(RId),
  datalog_persistent(_Pred,DLs,_UPDLs),
  member(datalog(R,NVs,RId,CId,Ls,FId,Source),DLs).
% Get persistent rules and facts for a given predicate
get_datalog(R,NVs,RId,CId,Ls,FId,Source) :-
  nonvar(R),
  pred_rule(Name/Arity,R),
  datalog_persistent(Name/Arity,DLs,UPDLs),
  get_datalog_persistent_rule_or_fact(Name/Arity,DLs,UPDLs,R,NVs,RId,CId,Ls,FId,Source).
% Get non-persistent rules and facts for a given predicate which is not persistent
get_datalog(R,NVs,RId,CId,Ls,FId,Source) :-
  nonvar(R),
  pred_rule(Name/Arity,R),
  \+ datalog_persistent(Name/Arity,_DLs,_UPDLs),
  datalog(R,NVs,RId,CId,Ls,FId,Source).
% General case:
%   - Get all persistent rules and facts 
%     (includes local rules and facts of persistent predicates that have not been transferred 
%      to the external database)
%   - Get all non-persistent rules and facts from external RDB
%   - Get local rules and facts which are not persistent
get_datalog(R,NVs,RId,CId,Ls,FId,Source) :-
%  var(R),
  var(R), 
  var(RId),
  datalog_persistent(Name/Arity,_DLs,_UPDLs),
  functor(H,Name,Arity),
  (R=H ; R=':-'(H,_B)),
  get_datalog(R,NVs,RId,CId,Ls,FId,Source).
get_datalog(R,NVs,RId,[],Ls,FId,Source) :-
%  var(R),
  var(R), 
  var(RId),
  datalog_rdb_all_np(R,NVs,RId,Ls,FId,Source).
get_datalog(R,NVs,RId,CId,Ls,FId,Source) :-
%  var(R),
  var(R), 
  var(RId),
  my_fact(datalog(R,NVs,RId,CId,Ls,FId,Source)),
  (R = ':-'(H,_B) -> true ; R = H),
  functor(H,N,A),
  \+ datalog_persistent(N/A,_DLs,_UPDLs).

  
get_datalog_persistent_rule_or_fact(Name/Arity,DLs,_UPDLs,R,NVs,RId,CId,Ls,FId,Source) :-
  % Rules already projected (in datalog_persistent):
  (member(datalog(R,NVs,RId,CId,Ls,FId,Source),DLs)
   ;
%   % Rules in datalog_persistent which have been rejected for persistence:
%    member(datalog(R,NVs,RId,CId,Ls,FId,Source),UPDLs)
%    ;
  % Facts in RDB. Ask only for persistent table name (TableName_des):
   (nonvar(R) -> rule_is_positive_fact(R) ; true),
   functor(R,Name,Arity),
   functor(PredSchema,Name,Arity),
   my_persistent(Connection,PredSchema),
   FId=rdb(Connection),
   get_datalog_persistent_fact(datalog(R,NVs,RId,Ls,FId,Source)),
   CId=[]
   ;
  % Rules and facts which are locally stored, waiting to be transferred to the external RDB
   get_des_object_dlrules(namearity,Name/Arity,LDLs),
   member(datalog(R,NVs,RId,CId,Ls,FId,Source),LDLs)
  ).

  
get_datalog_persistent_fact(datalog(R,[],RuleId,[],rdb(Connection),source)) :-
  R =.. [Name|Columns],
  length(Columns,Arity),
  my_odbc_get_table_arity(Connection,Name,Arity),
  prepare_rdb_ruleid(Name),
  persistent_table_name(Name,TableName),
  build_sql_rdb_datasource(Connection,TableName,Columns,SQLstr),
  display_string_list_sql_on([SQLstr]),
  my_odbc_dql_query_fetch_row(Connection,SQLstr,Row),
  Row=..[_AnswerRel|Columns],
  get_rdb_ruleid(Name,RuleId).
  
  
%%%%%%%%%%%%%%%  END des.pl  %%%%%%%%%%%%%%%
