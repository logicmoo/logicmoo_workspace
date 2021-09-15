/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module( ctc_util_files, [
     consult_if_not_yet/2,            % (File, Head)
     export_goal_output/2,            % (File,Goal)
                                        % <-- OBSOLETE, use with_output_to_file/2 instead
     with_output_to_file/2,           % (File,Goal)       Mode = write
     with_output_to_file/3,           % (File,Mode,Goal)  Encoding = utf8
     with_output_to_file/4,           % (File,Mode,Goal,Encoding)
     with_output_to_folder/2,         % (+Folder,+Call)           FileName=Functor+Time
     with_output_to_folder/3,         % (+Folder,-FileName,+Call) FileName=Functor+Time
     report_to_file_ctc/1,            % (CallLiteral) As above but in CTC_HOME directory 
                                        % <-- OBSOLETE, use with_output_to_file/2 instead
     report_to_file_in_ctchome/1,     % (CallLiteral) As above but in CTC_HOME directory
     report_to_file_in_ctchome/2,     % (CallLiteral,File) As above but tell which file
     create_timestamped_file_path/4,  % (Folder,Prefix,Suffix,File)
     create_timestamped_folder_path/2 % (Folder,Timestamped)
] ).

% Conditional loading:

:- use_module(library(memfile)).
:- use_module(logging).
:- use_module(count).

:- module_transparent consult_if_not_yet/2.

consult_if_not_yet(File,Head) :-
    clause(Head,_),
    predicate_loaded_from(Head,F),
    !,
    (   current_prolog_flag(verbose_consult, false)
        ->  true
         ;  report_already_loaded(Head,F,File)
    ).
consult_if_not_yet(File,_) :-
    consult(File),
    !.
consult_if_not_yet(File,_) :-
    format(' *** ERROR loading ~k. ~n', [File]).   


/*
 * predicate_loaded_from(+Head,?File)
 *   Return in Arg2 
 *    - the File from which the predicate with head Arg1 was loaded 
 *      (if the predicate is not 'multifile') or
 *    - 'multifile(F)' if the predicate is multifile and F is a file
 *       from which a definition was loaded or
 *    - 'unknown' (if the predicate was created dynamically).
 */ 
predicate_loaded_from(Head,File) :- predicate_property(Head,file(F)),
                                    ( predicate_property(Head,multifile)
                                      -> File = multifile(F)
                                       ; File = F
                                    ).
predicate_loaded_from(Head,File) :- predicate_property(Head,_),
                                    File = unknown.

/*
 * Report that predicate Arg1 has already been loaded from 
 * file Arg2 instead of file Arg3.
 */
report_already_loaded(Head,F1,F2) :-
    functor(Head,Fkt,N),
    format(' *** Definition of ~a/~a  *not* loaded from ~k~n        because it is already loaded from ~k~n', [Fkt,N,F2,F1]).

/*
 * ctc_home_dir(CTCHomeOrPWD)
 *   Get the value of CTC home directory in an 
 *   operating system independent syntax. If it
 *   is undefined, return the current working directory. 
 */
ctc_home_dir(Dir) :- 
    (  file_search_path(ctcore,HomeOS)
    -> prolog_to_os_filename(Dir, HomeOS)
    ;  pwd(Dir)
    ).

% Strange but works. Copied from SWI-Prolog library predicate shell:pwd/0.
pwd(Path) :-  
	absolute_file_name('', Path).
	
/*
 * report_to_file_in_ctchome(+Call : a single literal)
 * report_to_file_in_ctchome(+Call : a single literal, -File)
 *
 *   The same as with_output_to_folder but with Folder
 *   parameter set to CTC home direcory.
 */
 :- module_transparent report_to_file_in_ctchome/1,
                       report_to_file_in_ctchome/2,
                       report_to_file_ctc/1.
 
report_to_file_in_ctchome(Call) :- 
   report_to_file_in_ctchome(Call,_).
   
report_to_file_in_ctchome(Call,File) :-
   ctc_home_dir(CTCHome),
   with_output_to_folder(CTCHome,File,Call).

% OBSOLETE, use the above instead.
report_to_file_ctc(Call) :- report_to_file_in_ctchome(Call).


%% with_input_from( +Source, :Goal) is semidet.
%
%  Temporarily switch current input to object specified by Source while calling Goal as in once/1.
%  Source is a term like that supplied to with_output_to/2 and can be any of:
%     * A stream handle or alias.
%     * atom(+Atom) 
%     * codes(+Codes)
%     * chars(+Chars)
%     * string(+String)
%  Author: Samer Abdallah (published on SWI-Prolog mailing list, 10 Aug 2010).


with_input_from(atom(A),Goal) :- !,
   setup_call_cleanup(
      atom_to_memory_file(A,MF),
      setup_call_cleanup(
         open_memory_file(MF,read,S),
         with_input_from(S,Goal),
         close(S)
      ),
      free_memory_file(MF)    
   ).  
       
with_input_from(codes(Codes),G) :- !, atom_codes(A,Codes), with_input_from(atom(A),G).
with_input_from(chars(Chars),G) :- !, atom_chars(A,Chars), with_input_from(atom(A),G).
with_input_from(string(Str),G)  :- !, string_to_atom(Str,A), with_input_from(atom(A),G).
       
with_input_from(S,G) :- is_stream(S), !,
   current_input(S0),
   setup_call_cleanup( set_input(S), once(G), set_input(S0)).


/*
 * with_output_to_folder(+Directory: Path,        +Call : a single literal)
 * with_output_to_folder(+Directory: Path, -File, +Call : a single literal)
 *
 *   Find all solutions for Call, write them  followed by the number of 
 *   solutions to a file whose name is prefixed by the functor of the 
 *   call. Create the file in the directory Directory.
 *   
 *   Report the file name on stdout. In the 3-argument version
 *   return the file name in the second argument. 
 */ 
 :- module_transparent with_output_to_folder/2,
                       with_output_to_folder/3,
                       report_to_file/2.
                       
with_output_to_folder(Folder,Call) :-
   with_output_to_folder(Folder,_,Call).
  
with_output_to_folder(Folder,File,Call) :-
   functor(Call,Functor,_),
   atomic_list_concat([Functor,'-'],Prefix),
   create_timestamped_file_path(Folder,Prefix,'.txt',File),
   export_all_results(File, Call),            
   log_on_stdout('Report written to file~n~a~n',[File]).

% OBSOLETE, use the above instead.
report_to_file(Folder,Call) :- with_output_to_folder(Folder,Call).


/*
 * export_all_results(+File, +Goal)
 *   Open an output stream for File and redirect the  output
 *   of the goal to that stream. Find all solution for Goal,
 *   report the number of solutions and print each solution on 
 *   a separate line. Close the stream afterwards.
 *
 *   +File is a file in Unix notation (with slashes "/" as 
 *   separators). On Windows, the path may start with the 
 *   typical single letter followed by a colon, e.g. 
 *   'C:/folder/file.pl'
 */
:- module_transparent export_all_results/2.
                       
export_all_results(File, Goal) :-
   with_output_to_file(File, 
       ( count(Goal,N),  % enforces calculating all results
         format('Found ~a results.~n',[N]),
         forall(Goal, writeln(Goal)),
         nl
       )
    ).
 
%% with_output_to_file(+File, +Mode, +Goal)
%    Open an output stream for File in mode Mode 
%    (Mode = write | append), and redirect all output
%    of the goal to that stream. Close it afterwards
%    even if the Goal exited with an exception.
%    CAUTION: If you choose Mode = write, the previous 
%    contents of the file will be overwritten!
%
:- module_transparent with_output_to_file/3.

with_output_to_file(File,Mode,Goal) :- 
   with_output_to_file(File,Mode,Goal,utf8).

:- module_transparent with_output_to_file/4.

with_output_to_file(File,Mode,Goal,Encoding) :- 
   setup_call_cleanup( (open(File, Mode, Stream),     
   					   	set_stream(Stream, encoding(Encoding))
   					    ), % setup
                       with_output_to(Stream,Goal),  % call
                       close(Stream)                 % cleanup
   ).

% -- OBSOLETE, use with_output_to_file/3 instead:
:- module_transparent with_output_to_file/2, export_goal_output/2.
with_output_to_file(File,Goal) :-  with_output_to_file(File,write,Goal).
export_goal_output(File,Goal)  :-  with_output_to_file(File,write,Goal).

   
% Portable implementation of the above predicate
% (without SWI-specific "with_otput_to"):
%
%with_output_to_file(File,Goal) :- 
%    telling(CurrentOutput),
%    tell(File),
%      once(Goal),
%    told,
%    tell(CurrentOutput).
        
/*
 * Return in Arg4 the path to a unique file in folder Arg1.
 * The file name is created from the prefix passed
 * in Arg2 by appending the time of the invocation of this 
 * predicate and the sufix from Arg3.  
 */
create_timestamped_file_path(Directory,Prefix,Suffix,FilePath) :-
    create_timestamp(Timestamp),
    atomic_list_concat([Directory,'/',Prefix,Timestamp,Suffix],FilePath).

create_timestamped_folder_path(Directory,Timestamped) :-
    create_timestamp(Timestamp),
    atomic_list_concat([Directory,Timestamp],Timestamped).    
/*
 * create_timestamp(?TimeStampAtom)
 *   Return in Arg1 an atom representing the time of the 
 *   invocation of this predicate in the form 
 *   Year-Month-Day-Hour-Minute-Second.Milliseconds  
 */
create_timestamp(TimeStampAtom) :-
    get_time(TimeStamp),
    stamp_date_time(TimeStamp, DateTimeTerm1, local),
    DateTimeTerm1 = date(Y,M,D,H,Mn,S,_,_,_),
    DateTimeTerm2 = Y-M-D-H-Mn-S,
    term_to_atom(DateTimeTerm2,TimeStampAtom).

create_timestamp_2(TimeStampAtom) :-
    get_time(TimeStamp),
    stamp_date_time(TimeStamp, DateTimeTerm1, local),
    DateTimeTerm1 = date(Y,M,D,H,Mn,S,_,_,_),
    Seconds is truncate(S), 
    format(atom(TimeStampAtom), '~a.~a.~a ~a:~a:~d', [D,M,Y,H,Mn,Seconds]).
    
/*
 * Determine the absolute path to the root directory of the current 
 * workspace ASSUNIMG that this file is three levels deeper:
 *  --> WorkspaceDirPath/ProjectDir/FileDir/thisfile
 */                                    
workspace_root(WorkspaceDirPath) :-
    file_search_path('worspace_root', WorkspaceDirPath),
    !.
workspace_root(WorkspaceDirPath) :-
    source_file(workspace_root(_), CurrentFile),    % get absolute path of current file
    file_directory_name(CurrentFile, FileDirPath),  % get path to its containing directory
    file_base_name(FileDirPath, FileDir),           % get name of containing directory 
    concat(ProjectDirPath, FileDir, FileDirPath),   % get path to its containing project
    file_base_name(ProjectDirPath, ProjectDir),     % get name of containing project
    concat(WorkspaceDirPath, ProjectDir, ProjectDirPath), % get path of its containing worksapace
    assert(file_search_path('worspace_root', WorkspaceDirPath)).


