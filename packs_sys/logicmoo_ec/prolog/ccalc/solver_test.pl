
:- ensure_loaded(ccalc).

solver_test :-
   initialize,
   format("Testing all SAT solvers and utility programs.~n~n",[]),
   value(ccalc_dir,CCDir),
   determine_os(OS),

% There's a problem with checking the usage of pipes so the section below
% has been commented out.  (Pipes are not used by default, anyway.)
%
% Actually, it seems that this will work correctly in swipl, if we replace
%    make_pipe(P1),make_pipe(P2),format_to_atom(...),system(...),
% by
%    pipe(P2,P1)
% and remove
%    common_file_delete(P1),common_file_delete(P2)
% later on.
%
% However, I didn't do this because 
% a) we should also fix all places in ccalc.pl where pipes are used,
% b) sicstus (version 3.11.2, in the department) doesn't have pipe/2.
%
% -- Selim T. Erdogan, 23 May 2012
/*
   format("Testing whether CCalc can create and use pipes...",[]),
   flush_output,
   ( make_pipe(P1),
     make_pipe(P2),
     format_to_atom(Call1,"cat ~w > ~w &",[P1,P2]),
     system(Call1),
     see(P2),
     tell(P1),
     write('This is a test'), nl,
     told,
     read_line("This is a test"),
     seen,
     common_file_delete(P1),
     common_file_delete(P2),
     set(file_io,false),
     format(" successful.~n~n",[])
   ; format("Error using pipes for I/O.  The system utilities called by CCalc might not be~n",[]),
     format("portable.  Please contact ccalc-help@cs.utexas.edu for assistance.  In the~n",[]),
     format("meantime, please use the command 'set(file_io,true)' to use files instead of~n",[]),
     format("pipes for I/O.  You may add this line to the options.std file to make this the~n",[]),
     format("default.",[]) ),
   flush_output,
   !,
*/

   format("Checking for existence of solvers directory...",[]),
   flush_output,
   format_to_atom(D,"~wsolvers/~s",[CCDir,OS]),
   ( common_file_exists(D)
     ->  format(" successful.~n~n",[]),
         format("SAT solvers executables for this operating system are stored in:~n",[]),
         format("~w~n~n",[D])
/*
   ; format("Directory does not exist.~nCreating ~w.~n~n ",[D]),
     flush_output,
     format_to_atom(Call2,"mkdir ~w",[D]),
     system(Call2)
*/
   ; format("Error: cannot create solvers directory.~n~n",[]),
     write('All of the SAT solvers and utility programs for CCalc must reside in'),nl,
     write('the directory <CCalc base dir>/solvers/<OS>, where <CCalc base dir> is the'),nl,
     write('directory in which ccalc.pl is located and <OS> is the name of the operating'),nl,
     write('system being used, as returned by the Unix system function \'uname\'.  (There'),nl,
     write('may be multiple subdirectories of the solvers directory if your CCalc'),nl,
     write('installation is shared by multiple operating systems.)  This directory does'),nl,
     write('not currently exist, and an attempt to create it has failed.  Please ask'),nl,
     write('your system administrator to create this directory and ensure that the'),nl,
     write('SAT solver executables are located therein.'),nl,nl,
     fail ),
   flush_output,
   !,

   /* Test all the SAT solvers */

   iset(compact,false),
   iset(num,1),
   iset(mode,basic),
   iset(atom_count,2),
   iset(clause_count,2),
   iset(query_clause_count,0),
   iset(timed,false),
   iset(verbose,false),
   iset(file_io,true),
   iset(noabort,true),
   assertz(clause([1,2])),
   assertz(clause([-2])),

% SAT solver "sato_old" only exists as a binary for SunOS.  So no need
% to test it when we're running on Linux.
% -- Selim T. Erdogan, 22 May 2012
%
%   ( member(S,[mchaff,relsat,relsat_old,grasp,sato,sato_old,satz,satz-rand,
%	       walksat,zchaff]),
   ( ( OS == "Linux" 
       -> member(S,[mchaff,relsat,relsat_old,grasp,sato,satz,satz-rand,
	          walksat,zchaff])
     ; member(S,[mchaff,relsat,relsat_old,grasp,sato,sato_old,satz,satz-rand,
                 walksat,zchaff])), 

     format("~nTesting solver ~w...",[S]),
     flush_output,
     system_value('sh rm ccsat.* mchaff-opts.smj 2> /dev/null'),
     format_to_atom(FullName,"~wsolvers/~s/~w",[CCDir,OS,S]),
     ( \+ common_file_exists(FullName)
       -> format("~nExecutable ~w is missing.~n",[FullName]),
          display_note(S), nl, fail
     ; ( iset(solver,S),
         call_sat(no_notify,Out1,Out2,VT,_),
         extract_info(Out1,Out2,VT,A,M,_,_),
         A == "SAT",
         M == [[1]] )
       -> format(" successful.~n",[]),
          fail
     ; format("~n",[]), display_note(S), nl, fail )
   ; format("~nIf you have any problems running CCalc, please contact the development~n",[]),
     format("team at ccalc-help@cs.utexas.edu.  Thanks for using CCalc!~n~n",[]),
     iset(noabort,false) ).


display_note(grasp) :- 
   format("There was an error invoking GRASP.  You may need to obtain the correct~n",[]),
   format("executable or recompile this solver for your system.  Source code and~n",[]),
   format("executables for the Feb/2000 version of GRASP are available at:~n",[]),
   format("http://sat.inesc.pt/grasp/~n",[]),
   format("Please rename the executable from 'sat-grasp' to 'grasp' and copy it to the SAT~n",[]),
   format("solver directory listed above.~n",[]).

display_note(mchaff) :-
   format("There was an error invoking mChaff.  You may need to obtain the correct~n",[]),
   format("executable or recompile this solver for your system.  Source code and~n",[]),
   format("executables for the spelt3 distribution of mChaff used to be available from~n",[]),
   format("http://www.princeton.edu/~~chaff/ but they are not supported anymore.~n",[]),
   format("Contact us by e-mailing ccalc-help@cs.utexas.edu for a copy of the code.~n",[]),
   format("After you get mchaff's code, please rename the executable from 'Chaff2'~n",[]),
   format("to 'mchaff' and copy it to the SAT solver directory listed above.~n",[]).

display_note(relsat) :-
   format("There was an error invoking relsat version 2.02.  You may need to obtain the~n",[]),
   format("correct executable and recompile this solver for your system.  Source code for~n",[]),
   format("relsat version 2.02 is available at http://www.bayardo.org/resources.html~n",[]),
   format("Please copy the executable 'relsat' to the SAT solver directory listed above.~n",[]).

display_note(relsat_old) :-
   format("There was an error invoking relsat version 1.1.2 (called 'relsat_old' within~n",[]),
   format("CCalc).  You may need to recompile this solver for your system.~n",[]),
%   format("Source code for relsat version 1.1.2 is be available at:~n",[]),
%   format("http://www.almaden.ibm.com/cs/people/bayardo/resources.html~n",[]),
   format("Contact us by e-mailing ccalc-help@cs.utexas.edu for a copy of the code.~n",[]),
   format("After you get relsat_old's code, please rename the executable from 'relsat'~n",[]),
   format("to 'relsat_old' and copy it to the SAT solver directory listed above.~n",[]).

display_note(sato) :-
   format("There was an error invoking SATO version 3.2.1.  You may need to recompile this~n",[]),
   format("solver for your system.  Source code for SATO version 3.2.1 is available at:~n",[]),
   format("http://www.cs.uiowa.edu/~~hzhang/sato.html~n",[]),
   format("Please copy the executable 'sato' to the SAT solver directory listed above.~n",[]).

display_note(sato_old) :-
   format("There was an error invoking SATO version 3.1.2 (called 'sato_old' within~n",[]),
   format("CCalc).  Source code for this version is no longer available, though some~n",[]),
   format("executables (for SunOS) are included with CCalc for backwards compatibility~n",[]),
   format("with previous versions of CCalc.  Please use another solver (such as the more~n",[]),
   format("recent SATO version 3.2.1).~n",[]).

display_note(satz) :-
   format("There was an error invoking Satz.  You may need to recompile this solver for~n",[]),
   format("your system.  Source code for Satz215.2 is available at:~n",[]),
   format("http://www.laria.u-picardie.fr/~~cli/EnglishPage.html~n",[]),
   format("Please name the executable 'satz' and copy it to the SAT solver directory~n",[]),
   format("listed above.~n",[]).

display_note(satz-rand) :-
   format("There was an error invoking Satz-rand.  You may need to recompile this solver~n",[]),
   format("for your system.  Source code for Satz-rand version 4.9 is available at:~n",[]),
   format("http://www.cs.washington.edu/homes/kautz/satz-rand/~n",[]),
   format("Please copy the executable 'satz-rand' to the SAT solver directory listed~n",[]),
   format("above.~n",[]).

display_note(walksat) :-
   format("There was an error invoking Walksat.  You may need to obtain the correct~n",[]),
   format("executables or recompile this solver for your system.  Executables and source~n",[]),
   format("code for Walksat version 41 are available at:~n",[]),
   format("http://www.cs.washington.edu/homes/kautz/walksat/~n",[]),
   format("Please copy the executable 'walksat' to the SAT solver directory listed above.~n",[]).

display_note(zchaff) :-
   format("There was an error invoking zChaff.  You may need to obtain the correct~n",[]),
   format("executable or recompile this solver for your system.  Executables and source~n",[]),
   format("code for zChaff are available at:~n",[]),
   format("http://www.ee.princeton.edu/~~chaff/zchaff.php~n",[]),
   format("Please rename the executable to 'zchaff' and copy it to the SAT solver~n",[]),
   format("directory listed above.~n",[]).

% For some odd reason, when we load/consult solver_test.pl, calling
% solver_test at the very end leads to failure (i.e. "false").  
% So, to avoid confusion, I'm commenting it out.
%
% -- Selim T. Erdogan, 28 May 2012
%:- solver_test.
