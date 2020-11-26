/** Swicli.Library - Two Way Interface for .NET and MONO to/from SWI-Prolog
*  Author:        Douglas R. Miles
*  E-mail:        logicmoo@gmail.com
*  WWW:           http://www.logicmoo.com
*  Copyright (C):  2010-2012 LogicMOO Developement
*
*  This library is free software; you can redistribute it and/or
*  modify it under the terms of the GNU Lesser General Public
*  License as published by the Free Software Foundation; either
*  version 2.1 of the License, or (at your option) any later version.
*
*  This library is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  Lesser General Public License for more details.
*
*  You should have received a copy of the GNU Lesser General Public
*  License along with this library; if not, write to the Free Software
*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*/
:- module(swicli,
          [
            module_functor/4,
            to_string/2,
            member_elipse/2,
            %'$dict_dot'/3,
            %'$dict_dot'/4,
            op(600,fx,'@'),
					  cli_init/0
          ]).

/** <module>  Swicli.Library - Two Way Interface for .NET and MONO to/from SWI-Prolog

The easiest way to install on SWI is via the package manager. 
Simply do:
==
     ?- pack_install( swicli ).

     ?- use_module(library(swicli )).
==
And you are good to go.
*********************************************************/


cli_api:- !.
  
 
:- op(600,fx,'@').
:- meta_predicate(cli_add_event_handler(+,+,0)).
:- meta_predicate(cli_new_delegate(+,0,+)).
:- meta_predicate(cli_new_delegate_term(+,0,+,-)).
:- meta_predicate(cli_no_repeats(0,*)).
:- meta_predicate(cli_transitive_except(*,2,?,?)).
:- meta_predicate(with_env_vars(2,?)).
:- meta_predicate(cli_must(0)).
:- meta_predicate(cli_transitive_lc(2,?,?)).
:- meta_predicate(cli_no_repeats(0)).
:- meta_predicate(cli_no_repeats(+,0)).
:- meta_predicate(cli_with_lock(*,0)).
:- meta_predicate(cli_with_gc(0)).
:- meta_predicate(cli_preserve(*,0)).
:- meta_predicate(cli_trace_call(0)).
:- meta_predicate(cli_eval_hook(*,0,0)).
:- meta_predicate(cli_eval(*,0,0)).

:- use_module(library(lists)).
:- use_module(library(shlib)).
:- use_module(library(system)).

is_swi:- current_prolog_flag(version_data,DATA),DATA=swi(_,_,_,_).

%:- push_operators([op(600, fx, ('*'))]).
%:- push_operators([op(600, fx, ('@'))]). 
:- set_prolog_flag(double_quotes,string).

cli_must(Call):- (Call *-> true; throw(failed_cli_must(Call))).

cli_debug:- debug(swicli), set_prolog_flag(verbose_file_search,true), set_prolog_flag(swicli_debug,true).
cli_nodebug:- nodebug(swicli), set_prolog_flag(verbose_file_search,false), set_prolog_flag(swicli_debug,false).


memberchk_same(X, [Y|Ys]) :- (   X =@= Y ->  (var(X) -> X==Y ; true) ;   memberchk_same(X, Ys) ).
cli_no_repeats(Call):- term_variables(Call,Vs),cli_no_repeats(Call,Vs).
cli_no_repeats(Call,Vs):- CONS = [_],!, Call, (( \+ memberchk_same(Vs,CONS), copy_term(Vs,CVs), CONS=[_|T], nb_setarg(2, CONS, [CVs|T]))).

cli_trace_call(Call):- catch((Call,debug(swicli,'SUCCEED: ~q.~n',[Call])),E,(debug(swicli), debug(swicli,'ERROR: ~q.~n',[E=Call]))) *-> true; debug(swicli,'FAILED: ~q.~n',[Call]) .

cli_tests:- debugging(swicli),!,forall(clause(swicli_test,Call),Call),!.
cli_tests:- cli_debug,forall(clause(swicli_test,Call),cli_trace_call(Call)),cli_nodebug.


:- discontiguous(swicli_test/0).

swicli_test :- cli_debug.


:- discontiguous(cli_init0/0).


		 /*******************************
		 *             PATHS            *
		 *******************************/

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- if(current_prolog_flag(version_data,yap(_,_,_,_))).

user:file_search_path(jar, library('.')).
:- else.
user:file_search_path(jar, swi(lib)).
:- endif.

%% @pred 	add_search_path(+Var, +Value) is det.
%
%	Add value to the end of  search-path   Var.  Value is normally a
%	directory. Does not change the environment  if Dir is already in
%	Var.
%
%	@param Value	Path to add in OS notation.

add_search_path(Path, Dir) :- 
	(   getenv(Path, Old)
	->  (   current_prolog_flag(windows, true)
	    ->	Sep = (;)
	    ;	Sep = (:)
	    ),
	    (	atomic_list_concat(Current, Sep, Old),
		memberchk(Dir, Current)
	    ->	true			% already present
	    ;	atomic_list_concat([Old, Sep, Dir], New),
		setenv(Path, New)
	    )
	;   setenv(Path, Dir)
	).

%% @pred 	path_sep(-Sep:atom)
%
%	Separator  used  the  the  OS    in  =PATH=,  =LD_LIBRARY_PATH=,
%	=ASSEMBLYPATH=, etc.

path_sep((;)) :- 
	current_prolog_flag(windows, true), !.
path_sep(:).

		 /*******************************
		 *         LOAD THE RUNTIME         *
		 *******************************/

%% @pred       check_framework_environment
%
%       Verify the Framework environment.  Preferably   we  would create, but
%       most Unix systems do not   allow putenv("LD_LIBRARY_PATH=..." in
%       the current process. A suggesting found on  the net is to modify
%       LD_LIBRARY_PATH right at startup and  next execv() yourself, but
%       this doesn't work if we want to load Framework on demand or if Prolog
%       itself is embedded in another application.
%
%       So, after reading lots of pages on   the web, I decided checking
%       the environment and producing a sensible   error  message is the
%       best we can do.
%
%       Please not that Framework2 doesn't require   $ASSEMBLYPATH to be set, so
%       we do not check for that.

check_framework_libs(RUNTIME, Framework) :- 
    location( framework_root, '/' , Root),
    libfile( runtime, Root, RUNTIME),
    libfile( framework, Root, Framework), !.

% try FRAMEWORK_HOME, registry, etc..
location( framework_root, _, Home) :- 
    getenv( 'FRAMEWORK_HOME', Home ).
location(framework_root, _, MONO) :- 
    % OS well-known
    member(Root, [ '/usr/lib',
		   '/usr/local/lib',
                   '/opt/lib',
  '/Library/Framework/FrameworkVirtualMachines',
  '/System/Library/Frameworks'
		 ]),
    exists_directory(Root),
    dontnet_mono( Root, MONO).

dontnet_mono( Home, J ) :- 
    member(Extension, [framework, runtime, 'runtime/*framework*', 'runtime/*dontnet*', 'runtime/*sun*', 'dontnet*/Contents/Home', 'FrameworkVM.framework/Home'] ),
    absolute_file_name( Extension, [expand(true), relative_to(Home), access(exists), file_type( directory ), file_errors(fail), solutions(all) ], J0 ),
    pick_dontnet_mono(J0, J).

  
pick_dontnet_mono(J, J).
pick_dontnet_mono(J0, J) :- 
    absolute_file_name( 'mono*', [expand(true), relative_to(J0), access(exists), file_type( directory ), file_errors(fail), solutions(all) ], J ).
pick_dontnet_mono(J0, J) :- 
    absolute_file_name( 'dontnet*', [expand(true), relative_to(J0), access(exists), file_type( directory ), file_errors(fail), solutions(all) ], J ).
    

libfile(Base, HomeLib, File) :- 
  framework_arch( Arch ),
  monovm(Base, LBase),
  atomic_list_concat(['lib/',Arch,LBase], Lib),
  absolute_file_name( Lib, [relative_to(HomeLib), access(read), file_type( executable),  expand(true), file_errors(fail), solutions(all)], File ).
libfile(Base, HomeLib, File) :- 
  monovm(Base, LBase),
  atomic_list_concat(['lib',LBase], Lib),
  absolute_file_name( Lib, [relative_to(HomeLib), access(read), file_type( executable),  expand(true), file_errors(fail), solutions(all)], File ).
  
monovm( runtime, '/server/libruntime' ).
monovm( runtime, '/client/libruntime' ).
monovm( framework, '/libframework' ).

framework_arch( amd64 ) :- 
    current_prolog_flag( arch, x86_64 ).

/*
%% @pred 	library_search_path(-Dirs:list, -EnvVar) is det.
%
%	Dirs  is  the  list   of    directories   searched   for  shared
%	objects/DLLs. EnvVar is the variable in which the search path os
%	stored.

library_search_path(Path, EnvVar) :- 
	current_prolog_flag(shared_object_search_path, EnvVar),
	path_sep(Sep),
	phrase(framework_dirs, _Extra),
	(   getenv(EnvVar, Env),
	    atomic_list_concat(Path, Sep, Env)
	->  true
	;   Path = []
	).
*/


%% @pred       add_swicli_to_assemblypath
%
%       Add swicli.jar to =ASSEMBLYPATH= to facilitate callbacks

add_swicli_to_assemblypath :- 
	absolute_file_name(jar('swicli.jar'),
			   [ access(read)
			   ], SwicliLibraryDLL), !,
	(   getenv('MONO_PATH', Old)
	->  true
	;   Old = '.'
	),
	(       current_prolog_flag(windows, true)
	->      Separator = ';'
	;       Separator = ':'
	),
	atomic_list_concat([SwicliLibraryDLL, Old], Separator, New),
	setenv('MONO_PATH', New).


%% @pred 	add_swicli_to_ldpath(+SWICLI) is det.
%
%	Add the directory holding swicli.so  to   search  path  for dynamic
%	libraries. This is needed for callback   from Framework. Framework appears
%	to use its own search  and  the   new  value  of the variable is
%	picked up correctly.

add_swicli_to_ldpath(SWICLI, File) :- 
	absolute_file_name(SWICLI, File,
			   [ file_type(executable),
			     access(read),
			     file_errors(fail)
			   ]),
	file_directory_name(File, Dir),
	prolog_to_os_filename(Dir, OsDir),
	current_prolog_flag(shared_object_search_path, PathVar),
	add_search_path(PathVar, OsDir).

%% @pred 	add_framework_to_ldpath is det.
%
%	Adds the directories holding runtime.dll and framework.dll to the %PATH%.
%	This appears to work on Windows. Unfortunately most Unix systems
%	appear to inspect the content of LD_LIBRARY_PATH only once.

add_framework_to_ldpath(_LIBFRAMEWORK, LIBRUNTIME) :- 
    add_lib_to_ldpath(LIBRUNTIME),
    fail.
add_framework_to_ldpath(LIBFRAMEWORK, _LIBRUNTIME) :- 
    add_lib_to_ldpath(LIBFRAMEWORK),
    fail.
add_framework_to_ldpath(_,_).

%=========================================
% Load C++ DLL
%=========================================

:- dynamic(scc:swicli_so_loaded/1).

cli_is_windows:- current_prolog_flag(unix,true),!,fail.
cli_is_windows:- current_prolog_flag(windows, true),!.
cli_is_windows:- current_prolog_flag(shared_object_extension,dll),!.
cli_is_windows:- current_prolog_flag(arch,ARCH),atomic_list_concat([_,_],'win',ARCH),!.

%% @pred       libswicli(-Spec) is det.
%
%       Return the spec for loading the   SWICLI shared object. This shared
%       object must be called libswicliYap.so as the Framework System.LoadLibrary()
%       call used by swicli.jar adds the lib* prefix.
libswicli(swicli):- is_swi,!.
libswicli(X):- 
  (current_prolog_flag(unix,true)->Lib='lib';Lib=''),
    current_prolog_flag(address_bits,Bits),
    atomic_list_concat([Lib,swicli,'Yap',Bits],X).

% swicli_foreign_name('/usr/local/lib/Yap/libswicliYap64.so').
swicli_foreign_name(foreign(X)):- libswicli(X).
swicli_foreign_name(ext(X)):- libswicli(X).
swicli_foreign_name(lib(X)):- libswicli(X).
swicli_foreign_name(bin(X)):- libswicli(X).
swicli_foreign_name(jar(X)):- libswicli(X).
swicli_foreign_name(X):- libswicli(X).


cli_ensure_so_loaded:- scc:swicli_so_loaded(_),!.
cli_ensure_so_loaded:- swicli_foreign_name(FO), catch(load_foreign_library(FO,install),_,fail),assert(scc:swicli_so_loaded(FO)),!.
cli_ensure_so_loaded:- swicli_foreign_name(FO), catch(load_foreign_library(FO),_,fail),assert(scc:swicli_so_loaded(FO)),!.
cli_ensure_so_loaded:- swicli_foreign_name(FO), catch(load_foreign_library(FO,install),_,fail),assert(scc:swicli_so_loaded(FO)),!.
:- if(current_predicate(load_absolute_foreign_files/3)).
cli_ensure_so_loaded:- swicli_foreign_name(FO),
     catch(load_absolute_foreign_files([FO], [],install),E,(writeln(E),fail)), assert(scc:swicli_so_loaded(FO)),!.
cli_ensure_so_loaded:- FO= '/usr/local/lib/Yap/libswicliYap64.so',
     catch(load_absolute_foreign_files([FO], [],install),E,(writeln(E),fail)), assert(scc:swicli_so_loaded(FO)),!.
cli_ensure_so_loaded:- FO= '/usr/local/lib/Yap/libswicliYap64.so',
     catch(load_absolute_foreign_files([FO], ['/usr/lib/libmonoboehm-2.0.so.1', '/usr/local/lib/libYap.so.6.3'],
    install),E,(writeln(E),fail)), assert(scc:swicli_so_loaded(FO)),!.
:-endif.
cli_ensure_so_loaded:- swicli_foreign_name(FO), throw(missing_dll(FO)).




%=========================================
% Assembly Searchpath
%=========================================

%% cli_add_swicli_assembly_search_path(+Path).
%% cli_remove_swicli_assembly_search_path(+Path).
%  Add or remove directories to the search path
% ==
% ?- cli_add_swicli_assembly_search_path('c:/myproj/bin').
%
% ?- cli_remove_swicli_assembly_search_path('c:/myproj/bin').
% ==
%
% This now makes the System assembly resolver see Assemblies in that directory
%
%  Simular to
%  _Windows_: adding to %PATH%
%  _Linux_:  adding to $MONO_PATH
%

cli_path(ASSEMBLY,PATHO):- absolute_file_name(ASSEMBLY,PATH),exists_file(PATH),!,prolog_to_os_filename(PATH,PATHO).
cli_path(ASSEMBLY,PATHO):- cli_path(ASSEMBLY,['.exe','.dll',''],PATHO).
cli_path(ASSEMBLY,ExtList,PATHO):- cli_os_dir(DIR),member(Ext,ExtList),atomic_list_concat([ASSEMBLY,Ext],'',ADLL),  
      absolute_file_name(ADLL,PATH,[relative_to(DIR)]),exists_file(PATH),!,prolog_to_os_filename(PATH,PATHO).

cli_os_dir(OS):- cli_search(gac,DIR),absolute_file_name(DIR,ABS),prolog_to_os_filename(ABS,OS).



cli_search(VAR,DIR):- cli_no_repeats((user:file_search_path(VAR, FROM), expand_file_search_path(FROM,DIR))).


		 /*******************************
		 *	 FILE_SEARCH_PATH	*
		 *******************************/

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

user:file_search_path(gac, DIR):- cli_search_path(DIR).

cli_search_path(DIR):- cli_no_repeats(gac_search_path(DIR)).
gac_search_path(DIR):- gac_search_path0(DIR0),fix_pathname(DIR0,DIR).
gac_search_path0(DIR):- cli_search(lib,DIR),exists_directory(DIR).
gac_search_path0(DIR):- is_swi,call( '$pack':pack_dir(swicli, _, DIR)).
gac_search_path0(DIR):- expand_file_search_path(pack(swicli/lib),DIR),exists_directory(DIR).
gac_search_path0(DIR):- expand_file_search_path(pack(swicli/bin),DIR),exists_directory(DIR).
gac_search_path0(DIR):- env_path_elements('MONO_PATH', DIR).
gac_search_path0(DIR):- env_path_elements('PATH', DIR).
gac_search_path0(DIR):- env_path_elements('LD_LIBRARY_PATH', DIR).

/*
user:(file_search_path(library, Dir) :- 
	library_directory(Dir)).
user:file_search_path(swi, Home) :- 
	current_prolog_flag(home, Home).
user:file_search_path(foreign, swi(ArchLib)) :- 
	current_prolog_flag(arch, Arch),
	atom_concat('lib/', Arch, ArchLib).
user:file_search_path(foreign, swi(SoLib)) :- 
	(   current_prolog_flag(windows, true)
	->  SoLib = lib
	;   SoLib = lib
	).
user:file_search_path(path, Dir) :- 
	getenv('PATH', Path),
	(   current_prolog_flag(windows, true)
	->  atomic_list_concat(Dirs, (;), Path)
	;   atomic_list_concat(Dirs, :, Path)
	),
	'member'(Dir, Dirs),
	'$no-null-bytes'(Dir).
*/

'$no-null-bytes'(Dir) :- 
	sub_atom(Dir, _, _, _, '\u0000'), !,
	print_message(warning, null_byte_in_path(Dir)),
	fail.
'$no-null-bytes'(_).

%%	expand_file_search_path(+Spec, -Expanded) is nondet.
%
%	Expand a search path.  The system uses depth-first search upto a
%	specified depth.  If this depth is exceeded an exception is raised.
%	TBD: bread-first search?

user_expand_file_search_path(Spec, Expanded) :- 
	catch('$expand_file_search_path'(Spec, Expanded, 0, []),
	      loop(Used),
	      throw(error(loop_error(Spec), file_search(Used)))).

'$expand_file_search_path'(Spec, Expanded, N, Used) :- 
	functor(Spec, Alias, 1), !,
	user:file_search_path(Alias, Exp0),
	NN is N + 1,
	(   NN > 16
	->  throw(loop(Used))
	;   true
	),
	'$expand_file_search_path'(Exp0, Exp1, NN, [Alias=Exp0|Used]),
	arg(1, Spec, Segments),
	'$segments_to_atom'(Segments, File),
	'$make_path'(Exp1, File, Expanded).
'$expand_file_search_path'(Spec, Path, _, _) :- 
	'$segments_to_atom'(Spec, Path).

'$make_path'(Dir, File, Path) :- 
	atom_concat(_, /, Dir), !,
	atom_concat(Dir, File, Path).
'$make_path'(Dir, File, Path) :- 
	atomic_list_concat([Dir, /, File], Path).

'$segments_to_atom'(Atom, Atom) :- 
	atomic(Atom), !.
'$segments_to_atom'(Segments, Atom) :- 
	'$segments_to_list'(Segments, List, []), !,
	atomic_list_concat(List, /, Atom).

'$segments_to_list'(A/B, H, T) :- 
	'$segments_to_list'(A, H, T0),
	'$segments_to_list'(B, T0, T).
'$segments_to_list'(A, [A|T], T) :- 
	atomic(A).



%= 	 	 

%% cli_transitive_lc( :PRED2X, +A, -B) is semidet.
%
% Transitive Not Loop Checked.
%
cli_transitive_lc(X,A,B):-cli_transitive_except([],X,A,B).


%= 	 	 

%% cli_transitive_except( +NotIn, :PRED2X, +A, -B) is semidet.
%
% Transitive Except.
%
cli_transitive_except(NotIn,X,A,B):- memberchk_same_two(A,NotIn)-> (B=A,!) ;((once((call(X,A,R)) -> ( R\=@=A -> cli_transitive_except([A|NotIn],X,R,B) ; B=R); B=A))),!.


%= 	 	 

%% memberchk_same_two( ?X, :TermY0) is semidet.
%
% Memberchk Same Two.
%
memberchk_same_two(X, [Y0|Ys]) :- is_list(Ys),!,C=..[v,Y0|Ys],!, arg(_,C,Y), ( X =@= Y ->  (var(X) -> X==Y ; true)),!.
memberchk_same_two(X, [Y|Ys]) :- (   X =@= Y ->  (var(X) -> X==Y ; true) ;   (nonvar(Ys),memberchk_same_two(X, Ys) )).

fix_pathname(Path,PathFixed):-absolute_file_name(Path,PathFixed0),prolog_to_os_filename(PathFixed0,PathFixed),!.
fix_pathname(Path,PathFixed):- cli_transitive_lc(fix_pathname0,Path,PathFixed).

fix_pathname0(Path,PathFixed):-absolute_file_name(Path,PathFixed)-> PathFixed\==Path,!.
fix_pathname0(Path,PathFixed):-prolog_to_os_filename(Path,PathFixed)-> PathFixed\==Path,!.
fix_pathname0(Path,PathFixed):-atom_concat(PathFixed,'\\\\',Path),!.
fix_pathname0(Path,PathFixed):-atom_concat(PathFixed,'/',Path),!.
fix_pathname0(Path,Path).

env_path_elements(VAR,DIR0):- getenv(VAR,VAL),path_sep(Sep),atomic_list_concat(DIRS, Sep, VAL),!, cli_no_repeats('member'(DIR,DIRS)),fix_pathname(DIR,DIR0).


get_path_elements(VAL,NEWDIRS):- path_sep(Sep),atomic_list_concat(DIRS, Sep, VAL),!,maplist(fix_pathname,DIRS,NEWDIRS).

remove_zero_codes(WAZ,WAS):- member(M,['a\000\n\000\/.','\a;','\\\\000','\\000',';;']), % '\\\\C','\\C',
  atomic_list_concat([W,A|ZL],M,WAZ),atomic_list_concat([W,A|ZL],';',WAZ0),!,remove_zero_codes(WAZ0,WAS),!.
remove_zero_codes(WAS,WAS).



% sometimes usefull
swicli_test :- getenv('PATH',WAZ),remove_zero_codes(WAZ,WAS),setenv('PATH',WAS).

prepend_env_var(Var,PathF):-
   fix_pathname(PathF,Path),
   getenv(Var,WAZ),
   remove_zero_codes(WAZ,WAS),   
   get_path_elements(WAS,PathS),
   subtract(PathS,[Path],PathSN),
   path_sep(Sep),
   atomic_list_concat([Path|PathSN],Sep,NEWPATH),
   setenv(Var,NEWPATH).
prepend_env_var(Var,PathF):-
   fix_pathname(PathF,Path),
   setenv(Var,Path).


% so we dont have to export MONO_PATH=/usr/lib/swi-prolog/lib/amd64

find_swicli_libdir(JARLIB,DIR):-call( '$pack':pack_dir(swicli, _, DIR))->file_directory_name(DIR,JARLIB),!.


cli_update_paths:- 
  forall(expand_file_search_path(foreign('.'),D),add_lib_to_ldpath(D)),
  find_swicli_libdir(JARLIB,DIR),
  add_lib_to_ldpath(JARLIB),
  add_lib_to_ldpath(DIR),!.


add_lib_to_ldpath(DIR):-with_env_vars(prepend_env_var,DIR).

with_env_vars(Call,D):- call(Call,'PATH',D),call(Call,'MONO_PATH',D),call(Call,'LD_LIBRARY_PATH',D),call(Call,'CLASSPATH',D).

% sometimes usefull
swicli_test :- cli_update_paths.

getenv_safe(N,V,ELSE):- getenv(N,V)->true;V=ELSE.

cli_env(N,V):- getenv_safe(N,WV,'(missing)'),WV=='(missing)',!,setenv(N,V),format('~NSetting: ~q.~n',[N=V]).
cli_env(N,_):- getenv_safe(N,V,'(missing)'),format('~N~q.~n',[N=V]).

cli_env(N):- getenv_safe(N,V,'(missing)'),format('~N~q.~n',[N=V]).

cli_env:-    
   add_lib_to_ldpath('C:/pf/Mono/bin'),
   cli_env('MONO_PATH','/usr/lib/mono/4.5'),
   cli_env('LD_LIBRARY_PATH','/usr/local/lib/Yap:/usr/lib/mono/4.5:.'),
   cli_env('PATH').

% sometimes usefull
swicli_test :- cli_env.



swicli_test :- cli_trace_call(scc:swicli_so_loaded(_)).

:- cli_update_paths, cli_env.
cli_init0:- cli_ensure_so_loaded.

%=========================================
% Library Loading
%=========================================

%% cli_load_lib(+AppDomainName, +AssemblyPartialName_Or_FullPath, +FullClassName, +StaticMethodName).
%  Loads an assembly into AppDomainName
%
% :- cli_load_lib('Example4SWICLIClass','Example4SWICLI','Example4SWICLI.Example4SWICLIClass','install'),!.
%
%  cli_load_lib/4 is what was used to bootstrap SWICLI 
%  (it defined the next stage where cli_load_assembly/1) became present
%
%  remember to: export LD_LIBRARY_PATH=/development/opensim4opencog/bin:$LD_LIBRARY_PATH
%
% in swicli.pl we called:
% ==
% :- cli_load_lib_safe('SWIProlog','Swicli.Library','Swicli.Library.Embedded','install').
% ==

swicli_cs_assembly('Swicli.Library').

cli_load_lib_safe(DOMAIN,ASSEMBLY,CLASS,METHOD):- cli_path(ASSEMBLY,PATH),cli_load_lib(DOMAIN,PATH,CLASS,METHOD).



cli_init0:- swicli_cs_assembly(ASSEMBLY),cli_load_lib_safe('SWIProlog',ASSEMBLY,'Swicli.Library.Embedded','install').



%% cli_lib_type(-LibTypeName).
% LibTypeName is an atom that denotes the implementation class SWICLI uses
cli_lib_type('Swicli.Library.PrologCLR').

%% link_swiplcs(+PathName).
%  TODO

%=========================================
% Assembly Loading
%=========================================

%% cli_load_assembly(+AssemblyPartialNameOrPath).
%% cli_load_assembly_uncaught(+AssemblyPartialNameOrPath).
% the cli_<Predicates> came because we had:
% ==
% ?- cli_load_assembly('Swicli.Library').
% ==
% The uncaught version allows exception to come from .NET
% (We use the caugth version)
cli_init0:- swicli_cs_assembly(SWICLI_DOT_LIBRARY),cli_load_assembly(SWICLI_DOT_LIBRARY).

swicli_test:- cli_load_assembly('Example4SWICLI').

%% cli_load_assembly_methods(+AssemblyPartialNameOrPath, +OnlyPrologVisible, +StringPrefixOrNull).
% Loads foreign predicates from Assembly 
% ==
% ?- cli_load_assembly_methods('Swicli.Library', @false, "cli_").
% ==

cli_load_assembly_methods_safe(A,B,C):- cli_path(A,AP),cli_load_assembly_methods(AP,B,C).



% A test
swicli_test:- cli_load_assembly_methods_safe('Example4SWICLI',@false, "excli_").
swicli_test:- listing(excli_install).

%% cli_add_foreign_methods(+Type, +OnlyPrologVisible, +StringPrefixOrNull).
% Loads foreign predicates from Type 

% A test
swicli_test:- cli_add_foreign_methods('Example4SWICLI.Example4SWICLIClass',@false,'foo_').
% swicli_test:- listing(foo_main/1).


swicli_test :- cli_trace_call((
 cli_new('java.lang.String',["a"],X),cli_get_type(X,C),cli_type_to_classname(C,_N))).

swicli_test :- cli_trace_call((
 cli_new('java.lang.String',["b"],X),cli_get_type(X,C),cli_type_to_classname(C,_N))).


% Install our .NET GC Hook
cli_init0:- initialization(cli_lib_call('InstallAtomGCHook',_), restore).

cli_init0:- export_prefixed(cli).

%=========================================
% Term/Reference Inspection
%=========================================

%% cli_non_obj(+Obj) 
% is null or void or var
cli_non_obj(Obj):- (var(Obj) ; Obj= @(null) ; Obj= @(void)),!.

%% cli_non_null(+Obj)
% is not null or void
cli_non_null(Obj):- \+(cli_is_null(Obj)).


%% cli_is_null(+Obj)
% equiv to  Obj == @(null)
cli_is_null(Obj):- Obj == @(null).
%% cli_null(+Obj)
% construct a null
cli_null(@(null)).


%% cli_is_true(+Obj).
%  equiv to  Obj == @(true)
cli_is_true(Obj):- Obj == @(true).
%% cli_true(+Obj)
% construct a @(true)
cli_true(@(true)).


%% cli_is_false(+Obj).
%  equiv to  Obj == @(false)
cli_is_false(Obj):- Obj== @(false).
%% cli_false(+Obj)
% construct a @(false)
cli_false(@(false)).

%% cli_is_void(+Obj).
%  equiv to  Obj == @(void)
cli_is_void(Obj):- Obj== @(void).
%% cli_void(+Obj)
% construct a @(void)
cli_void(@(void)).


%% cli_is_type(+Obj).
%  equiv to cli_is_type(Obj,'System.Type')
cli_is_type(Obj):- nonvar(Obj),cli_is_type(Obj,'System.Type').


%% cli_is_object(+Obj).
%
% is Object a CLR object and not null or void (includes struct,enum,object,event)

cli_is_object(Var):- \+ compound(Var),!,var(Var),!,get_attr(Var,cli,_),!.
cli_is_object('@'(O)):- !,O\=void,O\=null.
cli_is_object(O):- functor(O,CLRF,_),hcli_clr_functor(CLRF).

hcli_clr_functor(F):- memberchk(F,[struct,enum,object,event,'{}']).

%% cli_is_prolog(+Obj).
%
% is Object a CLR ValueType and not null or void (includes struct,enums)
cli_is_prolog(O):- \+ cli_is_object(O).

%% cli_is_tagged_object(+Obj)
% is Object a ref object (maybe null or void) (excludes struct,enum,object/N,event refernces)

%% cli_is_value(+Obj).
%
% is a CLR ValueType and not null or void (includes struct,enums)
cli_is_value(O):- cli_is_type(O,'System.ValueType').

%% cli_is_enum(+Obj).
%
% is Enum
cli_is_enum(O):- cli_is_type(O,'System.Enum').

%% cli_is_struct(+Obj).
%
% is Struct
cli_is_struct(O):- cli_is_type(O,'System.Struct').


%% cli_is_ref(+Obj) 
% is Object a ref object and not null or void (excludes struct,enum,object/N,event refernces)

% cli_is_ref([_|_]):- !,fail.
cli_is_ref('@'(O)):- \+ h_cli_simple_at(O).

h_cli_simple_at(void).
h_cli_simple_at(null).
h_cli_simple_at(true).
h_cli_simple_at(false).

%=========================================
% Type Inspection
%=========================================

%% cli_member_doc(+Memb,+Doc,+Xml).
%% cli_members(+ClazzOrInstance,-Members).
%% cli_memb(O,X).
%% cli_memb(O,F,X).
% ==
% cli_memb(O,X):- cli_members(O,Y),member(X,Y).
% cli_memb(O,F,X):- cli_memb(O,X),member(F,[f,p, c,m ,e]),functor(X,F,_).
% ==
%
% Object to the member infos of it
% ==
%    3 ?- cli_new('System.Collections.Generic.List'(string),[int],[10],O),cli_members(O,M),!,member(E,M),writeq(E),nl,fail.
%    f(0,'_items'(arrayOf('String')))
%    f(1,'_size'('Int32'))
%    f(2,'_version'('Int32'))
%    f(3,'_syncRoot'('Object'))
%    f(4,'_emptyArray'(arrayOf('String')))
%    f(5,'_defaultCapacity'('Int32'))
%    p(0,'Capacity'('Int32'))
%    p(1,'Count'('Int32'))
%    p(2,'System.Collections.IList.IsFixedSize'('Boolean'))
%    p(3,'System.Collections.Generic.ICollection<T>.IsReadOnly'('Boolean'))
%    p(4,'System.Collections.IList.IsReadOnly'('Boolean'))
%    p(5,'System.Collections.ICollection.IsSynchronized'('Boolean'))
%    p(6,'System.Collections.ICollection.SyncRoot'('Object'))
%    p(7,'Item'('String'))
%    p(8,'System.Collections.IList.Item'('Object'))
%    m(0,'ConvertAll'('Converter'('String',<)))
%    m(1,get_Capacity)
%    m(2,set_Capacity('Int32'))
%    m(3,get_Count)
%    m(4,'System.Collections.IList.get_is_FixedSize')
%    m(5,'System.Collections.Generic.ICollection<T>.get_is_ReadOnly')
%    m(6,'System.Collections.IList.get_is_ReadOnly')
%    m(7,'System.Collections.ICollection.get_is_Synchronized')
%    m(8,'System.Collections.ICollection.get_SyncRoot')
%    m(9,get_item('Int32'))
%    m(10,set_item('Int32','String'))
%    m(11,'IsCompatibleObject'('Object'))
%    m(12,'VerifyValueType'('Object'))
%    m(13,'System.Collections.IList.get_item'('Int32'))
%    m(14,'System.Collections.IList.set_item'('Int32','Object'))
%    m(15,'Add'('String'))
%    m(16,'System.Collections.IList.Add'('Object'))
%    m(17,'AddRange'('System.Collections.Generic.IEnumerable'('String')))
%    m(18,'AsReadOnly')
%    m(19,'BinarySearch'('Int32','Int32','String','System.Collections.Generic.IComparer'('String')))
%    m(20,'BinarySearch'('String'))
%    m(21,'BinarySearch'('String','System.Collections.Generic.IComparer'('String')))
%    m(22,'Clear')
%    m(23,'Contains'('String'))
%    m(24,'System.Collections.IList.Contains'('Object'))
%    m(25,'CopyTo'(arrayOf('String')))
%    m(26,'System.Collections.ICollection.CopyTo'('Array','Int32'))
%    m(27,'CopyTo'('Int32',arrayOf('String'),'Int32','Int32'))
%    m(28,'CopyTo'(arrayOf('String'),'Int32'))
%    m(29,'EnsureCapacity'('Int32'))
%    m(30,'Exists'('System.Predicate'('String')))
%    m(31,'Find'('System.Predicate'('String')))
%    m(32,'FindAll'('System.Predicate'('String')))
%    m(33,'FindIndex'('System.Predicate'('String')))
%    m(34,'FindIndex'('Int32','System.Predicate'('String')))
%    m(35,'FindIndex'('Int32','Int32','System.Predicate'('String')))
%    m(36,'FindLast'('System.Predicate'('String')))
%    m(37,'FindLastIndex'('System.Predicate'('String')))
%    m(38,'FindLastIndex'('Int32','System.Predicate'('String')))
%    m(39,'FindLastIndex'('Int32','Int32','System.Predicate'('String')))
%    m(40,'ForEach'('System.Action'('String')))
%    m(41,'GetEnumerator')
%    m(42,'System.Collections.Generic.IEnumerable<T>.GetEnumerator')
%    m(43,'System.Collections.IEnumerable.GetEnumerator')
%    m(44,'GetRange'('Int32','Int32'))
%    m(45,'IndexOf'('String'))
%    m(46,'System.Collections.IList.IndexOf'('Object'))
%    m(47,'IndexOf'('String','Int32'))
%    m(48,'IndexOf'('String','Int32','Int32'))
%    m(49,'Insert'('Int32','String'))
%    m(50,'System.Collections.IList.Insert'('Int32','Object'))
%    m(51,'InsertRange'('Int32','System.Collections.Generic.IEnumerable'('String')))
%    m(52,'LastIndexOf'('String'))
%    m(53,'LastIndexOf'('String','Int32'))
%    m(54,'LastIndexOf'('String','Int32','Int32'))
%    m(55,'Remove'('String'))
%    m(56,'System.Collections.IList.Remove'('Object'))
%    m(57,'RemoveAll'('System.Predicate'('String')))
%    m(58,'RemoveAt'('Int32'))
%    m(59,'RemoveRange'('Int32','Int32'))
%    m(60,'Reverse')
%    m(61,'Reverse'('Int32','Int32'))
%    m(62,'Sort')
%    m(63,'Sort'('System.Collections.Generic.IComparer'('String')))
%    m(64,'Sort'('Int32','Int32','System.Collections.Generic.IComparer'('String')))
%    m(65,'Sort'('System.Comparison'('String')))
%    m(66,'ToArray')
%    m(67,'TrimExcess')
%    m(68,'TrueForAll'('System.Predicate'('String')))
%    m(69,'ToString')
%    m(70,'Equals'('Object'))
%    m(71,'GetHashCode')
%    m(72,'GetType')
%    m(73,'Finalize')
%    m(74,'MemberwiseClone')
%    c(0,'List`1')
%    c(1,'List`1'('Int32'))
%    c(2,'List`1'('System.Collections.Generic.IEnumerable'('String')))
%    c(3,'List`1')
% ==
cli_memb(O,X):- cli_members(O,Y),cli_col(Y,X).
cli_memb(O,F,X):- cli_memb(O,X),member(F,[f,p, c,m ,e]),functor(X,F,_).


:- dynamic(cli_subproperty/2).
:- module_transparent(cli_subproperty/2).
:- multifile(cli_subproperty/2).


%%  cli_is_type(+Impl,?Type).
%
% tests to see if the Impl Object is assignable to Type
%
cli_is_type(Impl,Type):- not(ground(Impl)),nonvar(Type),!,cli_find_type(Type,RealType),cli_call(RealType,'IsInstanceOfType'(object),[Impl],'@'(true)).
cli_is_type(Impl,Type):- nonvar(Type),cli_find_type(Type,RealType),!,cli_call(RealType,'IsInstanceOfType'(object),[Impl],'@'(true)).
cli_is_type(Impl,Type):- cli_get_type(Impl,Type).

%=========================================
% Type Inspection
%=========================================

%% cli_subclass(+Subclass,+Superclass) 
% tests to see if the Subclass is assignable to Superclass

cli_subclass(Sub,Sup):- cli_find_type(Sub,RealSub),cli_find_type(Sup,RealSup),cli_call(RealSup,'IsAssignableFrom'('System.Type'),[RealSub],'@'(true)).

%% cli_get_typespec(+Obj,?TypeSpec)
% gets or checks the TypeSpec
cli_get_typespec(Obj,TypeSpec):- cli_get_type(Obj,Type), cli_type_to_typespec(Type,TypeSpec).

%% cli_get_typeref(+Obj,?TypeRef)
% gets or checks the TypeRef
cli_get_typeref(Obj,TypeRef):- cli_get_type(Obj,Type), cli_to_ref(Type,TypeRef).

%% cli_object_is_typename(+Obj,?TypeName)
% gets or checks the TypeName
cli_object_is_typename(Obj,TypeName):- cli_get_type(Obj,Type), cli_type_to_fullname(Type,TypeName).
% gets or checks the TypeName
cli_object_is_classname(Obj,TypeName):- cli_get_type(Obj,Type), cli_type_to_classname(Type,TypeName).

%% cli_type_to_typespec(+ClazzSpec,-Value).
% coerces a ClazzSpec to a Value representing a TypeSpec term

%% cli_add_tag(+RefObj,+TagString).
%  lowlevel access to create a tag name 
% ==
% ?- cli_new(array(string),[int],[32],O),cli_add_tag(O,'string32').
%
% ?- cli_get_type(@(string32),T),cli_writeln(T).
%
% ==

%% cli_remove_tag(+TagString).
%  lowlevel access to remove a tag name

%% cli_to_ref(+Obj,+Ref).
%  return a @(Ref) version of the object (even if a enum) 
%     ==
%     15 ?- cli_to_ref(sbyte(127),O),cli_get_type(O,T),cli_writeln(O is T).
%     "127"is"System.SByte"
%     O = @'C#283319280',
%     T = @'C#283324332'.
%
%     16 ?- cli_to_ref(long(127),O),cli_get_type(O,T),cli_writeln(O is T).
%     "127"is"System.Int64"
%     O = @'C#283345876',
%     T = @'C#283345868'.
%
%     17 ?- cli_to_ref(ulong(127),O),cli_get_type(O,T),cli_writeln(O is T).
%     "127"is"System.UInt64"
%     O = @'C#283346772',
%     T = @'C#283346760'.
%
%     15 ?- cli_to_ref(sbyte(127),O),cli_get_type(O,T),cli_writeln(O is T).
%     "127"is"System.SByte"
%     O = @'C#283319280',
%     T = @'C#283324332'.
%
%     16 ?- cli_to_ref(long(127),O),cli_get_type(O,T),cli_writeln(O is T).
%     "127"is"System.Int64"
%     O = @'C#283345876',
%     T = @'C#283345868'.
%
%     18 ?- cli_to_ref(343434127,O),cli_get_type(O,T),cli_writeln(O is T).
%     "343434127"is"System.Int32"
%     O = @'C#281925284',
%     T = @'C#281925280'.
%
%     19 ?- cli_to_ref(3434341271,O),cli_get_type(O,T),cli_writeln(O is T).
%     "3434341271"is"System.UInt64"
%     O = @'C#281926616',
%     T = @'C#283346760'.
%
%     21 ?- cli_to_ref(343434127111,O),cli_get_type(O,T),cli_writeln(O is T).
%     "343434127111"is"System.UInt64"
%     O = @'C#281930092',
%     T = @'C#283346760'.
%
%     28 ?- cli_to_ref(34343412711111111111111111111111111111,O),cli_get_type(O,T),cli_writeln(O is T).
%     "34343412711111111111111111111111111111"is"java.math.BigInteger"
%     O = @'C#281813796',
%     T = @'C#281810860'.
%     ==


%% cli_to_immediate(+Ref,-Immediate).
%  return an Immediate value of Ref to just REf if no immediate type exists

%% cli_cast(+Value,+ClazzSpec,-Ref).
%% cli_cast_immediate(+Value,+ClazzSpec,-Immediate).
% Convert the type of Value to ClazzSpec returning eigther a Ref or Immediate value.
% ==
% ?- cli_cast(1,'double',X).
% X = @'C#568261440'.
%
% ?- cli_cast(1,'System.DayOfWeek',X).
% X = @'C#568269000'.
%
% ?- cli_cast_immediate(1,'System.DayOfWeek',X).
% X = enum('DayOfWeek', 'Monday').
%
% ?- cli_cast_immediate(1.0,'System.DayOfWeek',X).
% X = enum('DayOfWeek', 'Monday').
%
% ?- cli_cast_immediate(1.01,'System.DayOfWeek',X).
% ERROR: Having time of it convcerting 1.01 to System.DayOfWeek why System.ArgumentException: Requested value '1.01' was not found.
% ==

/*

% ?- cli_cast_immediate(0,'System.Drawing.Color',X).

*/
%=========================================
% Object Tracker
%=========================================

%% cli_tracker_begin(-Tracker).
%  Return a Tracker ref and all objects created from this point can be released via cli_tracker_free/1

%% cli_tracker_free(+Tracker).
%  @see cli_tracker_begin/1

%% cli_free(+RefObject).
%  remove a RefObject from the heap

%% cli_heap(+RefObject).
%  Pin a RefObject onto the heap

%% cli_with_gc(+Call)
% as ref objects are created they are tracked .. when the call is complete any new object tags are released
% uses Forienly defined cli_tracker_begin/1 and cli_tracker_free/1
cli_with_gc(Call):- setup_call_cleanup(cli_tracker_begin(Mark),Call,cli_tracker_free(Mark)).


%=========================================
% Object Locking
%=========================================

%% cli_with_lock(+Lock,+Call)
% 
%  Lock the first arg while calling Call
cli_with_lock(Lock,Call):- setup_call_cleanup(cli_lock_enter(Lock),Call,cli_lock_exit(Lock)).

%% cli_lock_enter(+LockObj).
% Does a Monitor.Enter on LockObj

%% cli_lock_exit(+LockObj).
% Does a Monitor.Exit on LockObj



%=========================================
% Formating and writing
%=========================================

%% cli_write(+Obj)
%  writes an object out
cli_write(S):- cli_to_str(S,W),writeq(W).

%% cli_writeln(+Obj)
%  writes an object out with a new line
cli_writeln(S):- cli_write(S),nl.

%% cli_fmt(+String,+Args).
%% cli_fmt(+Obj,+String,+Args).
% use .NET system string.Format(String,Args)
% Obj is WriteLineDelegate
cli_fmt(WID,String,Args):- cli_fmt(String,Args),cli_free(WID). % WID will be made again each call
cli_fmt(String,Args):- cli_call('System.String','Format'('string','object[]'),[String,Args],Result),cli_writeln(Result).

%% cwl(+StringValue).
% allas for System.Console.WriteLine(+String)   (not user_output but what .NET thinks its System.Console.Out)

%=========================================
% Object string
%=========================================

%% to_string(+Obj,-String).
%% cli_to_str(+Obj,-String).
% Resolves inner @(Obj)s to strings

%% cli_to_str_raw(+Obj,-String).
%% cli_java_to_string(+Obj,-Value).
% Resolves @(Obj) to string

cli_to_str(Term,String):- catch(ignore(hcli_to_str_0(Term,String0)),_,true),copy_term(String0,String),numbervars(String,666,_).
hcli_to_str_0(Term,Term):- not(compound(Term)),!.
hcli_to_str_0(Term,String):- Term='@'(_),cli_is_object(Term),catch(cli_to_str_raw(Term,String),_,Term==String),!.
hcli_to_str_0([A|B],[AS|BS]):- !,hcli_to_str_0(A,AS),hcli_to_str_0(B,BS).
hcli_to_str_0(eval(Call),String):- nonvar(Call),!,call(Call,Result),hcli_to_str_0(Result,String).
hcli_to_str_0(Term,String):- Term=..[F|A],hcli_to_str_0(A,AS),String=..[F|AS],!.
hcli_to_str_0(Term,Term).

%%to_string(Object,String):- jpl_is_ref(Object),!,jpl_call(Object,toString,[],String).
to_string(Object,String):- cli_to_str(Object,String).


%=========================================
% Exceptions and exiting
%=========================================

%% cli_halt.
%% cli_halt(+Obj).
% 
cli_halt:- cli_halt(0).
cli_halt(_Status):- cli_lib_type(LibType),cli_call(LibType,'ManagedHalt',_).


%% cli_throw(+Ex).
% throw an exception to .NET

%% cli_break(+Ex).
%

%% cli_debug(+Obj).
%% cli_debug(+Fmt,Args).
% writes to user_error
cli_debug(format(Format,Args)):- atom(Format),sformat(S,Format,Args),!,cli_debug(S).
cli_debug(Data):- format(user_error,'~n %% cli_-DEBUG: ~q~n',[Data]),flush_output(user_error).

%%cli_debug(Engine,Data):- format(user_error,'~n %% ENGINE-DEBUG: ~q',[Engine]),cli_debug(Data).


%=========================================
% Collections
%=========================================

cli_iterator_element(I, E) :- cli_is_type(I,'java.util.Iterator'),!,
	(   cli_call(I, hasNext, [], @(true))
	->  (   cli_call(I, next, [], E)        % surely it's steadfast...
	;   cli_iterator_element(I, E)
	)
	).

cli_enumerator_element(I, _E) :- cli_call_raw(I, 'MoveNext', [], @(false)),!,fail.
cli_enumerator_element(I, E) :- cli_get(I, 'Current', E).
cli_enumerator_element(I, E) :- cli_enumerator_element(I, E).


%% cli_col(+Col,-Elem).
%% cli_enumerator_element(+Enumer,-Elem).
%% cli_iterator_element(+Iter,-Elem).
% Iterates out Elem for Col/Iter/Enumer
% ==
%
%    ?- cli_new('System.Collections.Generic.List'('System.String'),[int],[10],Obj).
%    Obj = @'C#516939544'.
%
%
%    ?- cli_get($Obj,'Count',Out).
%    Out = 0.
%
%
%    ?- cli_call($Obj,'Add'("foo"),Out).
%    Out = @void.
%
%
%    ?- cli_call($Obj,'Add'("bar"),Out).
%    Out = @void.
%
%
%    ?- cli_get($Out,'Count',Out).
%    Out = 2.
%
%
%    ?- cli_col($Obj,E).
%    E = "foo" ;
%    E = "bar" ;
%    false.
% ==
cli_col(X,Y):- hcli_col(X,Y).

% old version:s hcli_col(Obj,Ele):- cli_call(Obj,'ToArray',[],Array),cli_array_to_term_args(Array,Vect),!,arg(_,Vect,Ele).
hcli_col(Error,_Ele):- cli_is_null(Error),!,fail.
hcli_col([S|Obj],Ele):- !,member(Ele,[S|Obj]).
hcli_col('[]',_Ele):- !,fail.
hcli_col(C,Ele):- functor(C,'[]',_),!,arg(_,C,Ele).
hcli_col(Obj,Ele):- 
      cli_memb(Obj,m(_, 'GetEnumerator', _, [], [], _, _)),!,
      cli_call(Obj,'GetEnumerator',[],Enum),!,
      call_cleanup(cli_enumerator_element(Enum,Ele),cli_free(Enum)).
hcli_col(Obj,Ele):- cli_array_to_term_args(Obj,Vect),!,arg(_,Vect,Ele).
hcli_col(Obj,Ele):- cli_memb(Obj,m(_, 'ToArray', _, [], [], _, _)),cli_call(Obj,'ToArray',[],Array),cli_array_to_term_args(Array,Vect),!,arg(_,Vect,Ele).
hcli_col(Obj,Ele):- cli_array_to_termlist(Obj,Vect),!,member(Ele,Vect).

%% cli_col_add(+Col,+Item)
% add an Item to Col
cli_col_add(Col,Value):- cli_call(Col,'Add'(Value),_).

%% cli_col_contains(+Col,+Item)
% Test an Item in Col
cli_col_contains(Col,Value):- cli_call(Col,'Contains'(Value),_).

%% cli_col_remove(+Col,+Item)
% Remove an Item in Col
cli_col_remove(Col,Value):- cli_call(Col,'Remove'(Value),_).

%% cli_col_removeall(+Col)
% Clears a Col
cli_col_removeall(Col):- cli_call(Col,'Clear',_).

%% cli_col_size(+Col,?Count)
% Returns the Count
cli_col_size(Col,Count):- cli_call(Col,'Count',Count).

%% cli_set_element(+Obj,+IndexParams,+Item).
%% cli_add_element(+Obj,+Item).
%  todo

%% cli_make_list(+Obj,+Arg2,+Arg3).
% @see  cli_new_list_1/2

%% cli_new_list_1(+Obj,+Arg2,+Arg3).
% @see cli_make_list/2

cli_new_list_1(Item,Type,List):- cli_new('System.Collections.Generic.List'(Type),[],[],List),cli_call(List,add(Item),_).
cli_make_list(Items,Type,List):- cli_new('System.Collections.Generic.List'(Type),[],[],List),forall(member(Item,Items),cli_call(List,add(Item),_)).

%% cli_sublist(+Mask,+List)
%  Test to see if Mask appears in List

cli_sublist(What,What):- !.
cli_sublist(Mask,What):- append(Pre,_,What),append(_,Mask,Pre).


%=========================================
% Arrays
%=========================================

%% cli_new_array(+ClazzSpec,+Rank,-Value).
%% cli_array_fill(+Obj, Arg2).
%% cli_array_fill_values(+Obj, Arg2).
%% cli_array_to_length(+Obj, Arg2).
%% cli_array_to_list(+Obj,+Arg2).
%% cli_array_to_term(+ArrayValue,-Value).
%% cli_array_to_termlist(+ArrayValue,-Value).
%% cli_term_to_array(+ArrayValue,-Value).
%% cli_array_to_term_args(+Array,-Term).
%  todo
cli_array_to_list(Array,List):- cli_array_to_term(Array,array(_,Term)),Term=..[_|List].
cli_array_to_term_args(Array,Term):- cli_array_to_term(Array,array(_,Term)).
cli_array_to_length(Array,Length):- cli_get(Array,'Length',Length).

/*

?- cli_new(array(string),[int],[32],O),cli_array_to_length(O,L),cli_array_to_term(O,T).
O = @'C#861856064',
L = 32,
T = array('String', values(@null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null, @null)).
*/

%=========================================
% .NET Backed Dictionaries/Maps
%=========================================

%% cli_map(Map,?Key,?Value).
%% cli_map_add(+Map,+Key,+Value).
%% cli_map_set(+Map,+Key,+Value).
%% cli_map_remove(+Map,+Key).
%% cli_map_remove(+Map,?Key,?Value).
%% cli_map_removeall(+Map).
%% cli_map_size(+Map,-Count).
% Map calls

cli_map(Map,Key,Value):- nonvar(Key),!,cli_call(Map,'TryGetValue',[Key,Value],@(true)).
cli_map(Map,Key,Value):- cli_col(Map,Ele),cli_get(Ele,'Key',Key),cli_get(Ele,'Value',Value).
cli_map_set(Map,Key,Value):- cli_call(Map,'[]'(type(Key)),[Key,Value],_).
cli_map_add(Map,Key,Value):- cli_call(Map,'Add'(Key,Value),_).
cli_map_remove(Map,Key):- cli_call(Map,'Remove'(Key),_).
cli_map_remove(Map,Key,Value):- cli_map(Map,Key,Value),!,cli_call(Map,'Remove'(Key),_).
cli_map_removeall(Map):- cli_call(Map,'Clear',_).
cli_map_size(Map,Count):- cli_call(Map,'Count',Count).


%=========================================
% Object Expansion
%=========================================

%% cli_preserve(TF,:Call)
% make Call with PreserveObjectType set to TF
cli_preserve(TF,Calls):- 
   cli_lib_type(LibType),
   cli_get(LibType,'PreserveObjectType',O),
   call_cleanup(
     (cli_set(LibType,'PreserveObjectType',TF),Calls),
         cli_set(LibType,'PreserveObjectType',O)).

%% member_elipse(Ele,Elipse).
% ==
% ?- member_elipse(E,{a,b,c}).
% E = a ;
% E = b ;
% E = c.
% ==
member_elipse(NV,{NVs}):- !,nonvar(NVs),member_elipse(NV,NVs).
member_elipse(NV,(A,B)):- !,(member_elipse(NV,A);member_elipse(NV,B)).
member_elipse(NV,NV).

cli_expanded(In,Out):- cli_expand(In,Out),!,In\==Out,!.

cli_expand(Obj,RObj):- var(Obj),once(get_attr(Obj,oo,binding(_Var,RObj));Obj=RObj),!.
cli_expand(Value,Value):- (atomic(Value);cli_is_ref(Value)),!.
cli_expand(eval(Call),Result):- nonvar(Call),!,call(Call,Result).
%%cli_expand([A|B],Result):- cli_get(A,B,Result),!.
%%cli_expand(Call,Result):- call(Call,Result),!.
cli_expand(Value,Value).




%% cli_to_data(+Ref,-Term).
%% cli_to_data(+ValueCol,+Ref,-Term).
%% cli_getterm(+ValueCol,+Ref,-Term).
%
% converts a Ref to prolog Term
% ValCol is a .NET List used to break cyclic loops
% ==
% ?- cli_cast("Yellow",'System.Drawing.Color',C),cli_to_data(C,D),writeq(D).
% ["R"=255,"G"=255,"B"=0,"A"=255,"IsKnownColor"= @true,"IsEmpty"= @false,"IsNamedColor"= @true,"IsSystemColor"= @false,"Name"="Yellow"]
% C = @'C#802963000',
% D = ["R"=255, "G"=255, "B"=0, "A"=255, "IsKnownColor"= @true, "IsEmpty"= @false, "IsNamedColor"= @true, "IsSystemColor"= @ ..., ... = ...].
% ==

cli_to_data(Term,String):- cli_new('System.Collections.Generic.List'(object),[],[],Objs),cli_to_data(Objs,Term,String).
cli_to_data(_,Term,Term):- not(compound(Term)),!.
%cli_to_data(_Objs,[A|B],[A|B]):- !.
cli_to_data(_Objs,[A|B],[A|B]):- \+( \+(A=[_=_])),!.
cli_to_data(Objs,[A|B],[AS|BS]):- !,cli_to_data(Objs,A,AS),cli_to_data(Objs,B,BS).
cli_to_data(Objs,Term,String):- cli_is_ref(Term),!,hcli_get_termdata(Objs,Term,Mid),(Term==Mid-> true; cli_to_data(Objs,Mid,String)).
cli_to_data(Objs,Term,FAS):- Term=..[F|A],hcli_to_data_1(Objs,F,A,Term,FAS).

hcli_to_data_1(_Objs,CLRFunctor,_A,Term,Term):- hcli_clr_functor(CLRFunctor),!.
hcli_to_data_1(Objs,F,A,_Term,String):- cli_to_data(Objs,A,AS),!,String=..[F|AS].

%% hcli_get_termdata(+Obj,+Arg2,+Arg3).
%
hcli_get_termdata(Done,Term,String):- cli_get_type(Term,Type),cli_props_for_type(Type,Props),Props\=[],
   hcli_getmap(Done,Term,Props,Name,Value,Name=Value,Mid),!,cli_to_data(Done,Mid,String).
%%hcli_get_termdata(Done,Term,String):- cli_is_ref(Term),!,cli_getterm(Done,Term,String),!.
hcli_get_termdata(_Done,Term,Mid):- Term=Mid.


hcli_getmap(Done,Term,_,_,_,_,ListO):- cli_is_type(Term,'System.Collections.IEnumerable'),findall(ED,(cli_col(Term,E),cli_to_data(Done,E,ED)),ListO),!.
hcli_getmap(Done,Term,Props,Name,Value,NameValue,List):- hcli_getmap_1(Done,Term,Props,Name,Value,NameValue,List).

hcli_getmap_1(Objs,Term,Props,Name,Value,NameValue,List):- findall(NameValue,(member(Name,Props),cli_get_raw(Term,Name,ValueM),cli_to_data(Objs,ValueM,Value)),List).


%=========================================
% Object Comparison and Unification
%=========================================

%% cli_unify(OE,PE)

cli_unify(OE,PE):- OE=PE,!.
cli_unify(enum(_,O1),O2):- !,cli_unify(O1,O2).
cli_unify(O2,enum(_,O1)):- !,cli_unify(O1,O2).
cli_unify(eval(O1),O2):- cli_expand(O1,O11),!,cli_unify(O11,O2).
cli_unify(O2,eval(O1)):- cli_expand(O1,O11),!,cli_unify(O11,O2).
cli_unify(O1,O2):- atomic(O1),atomic(O2),string_to_atom(S1,O1),string_to_atom(S2,O2),!,S1==S2.
cli_unify([O1|ARGS1],[O2|ARGS2]):- !,cli_unify(O1,O2),cli_unify(ARGS1,ARGS2).
cli_unify(O1,O2):- cli_is_ref(O1),cli_to_str(O1,S1),!,cli_unify(O2,S1).
cli_unify(O1,O2):- O1=..[F|[A1|RGS1]],!,O2=..[F|[A2|RGS2]],cli_unify([A1|RGS1],[A2|RGS2]).


%=========================================
% MUSHDLR223 Dictionary
%=========================================

% cli_intern/3
:- dynamic(cli_interned/3).
:- multifile(cli_interned/3).
:- module_transparent(cli_interned/3).
cli_intern(Engine,Name,Value):- retractall(cli_interned(Engine,Name,_)),assert(cli_interned(Engine,Name,Value)),cli_debug(cli_interned(Name,Value)),!.


% cli_eval/3
:- dynamic(cli_eval_hook/3).
:- multifile(cli_eval_hook/3).
:- module_transparent(cli_eval_hook/3).

cli_eval(Engine,Name,Value):- cli_eval_hook(Engine,Name,Value),!,cli_debug(cli_eval(Engine,Name,Value)),!.
cli_eval(Engine,Name,Value):- Value=cli_eval(Engine,Name),cli_debug(cli_eval(Name,Value)),!.
cli_eval_hook(Engine,In,Out):- catch(call((In,Out=In)),E,Out= foobar(Engine,In,E)).
cli_is_defined(_Engine,Name):- cli_debug(cli_not_is_defined(Name)),!,fail.
cli_get_symbol(Engine,Name,Value):- (cli_interned(Engine,Name,Value);Value=cli_UnDefined(Name)),!,cli_debug(cli_get_symbol(Name,Value)),!.



%=========================================
% Object NEW
%=========================================

%% cli_make_default(+ClazzSpec, -Result).
%% cli_new(+ClassNameWithParams,-Result).
%% cli_new(+ClazzSpec, +Params, -Result).
%% cli_new(+ClazzSpec,+MemberSpec,+Params,-Result).
% ==
% ?- cli_load_assembly('IKVM.OpenJDK.Core')
% ?- cli_new('java.lang.Long'(long),[44],Out),cli_to_str(Out,Str).
% ==
% same as..
% ==
% ?- cli_new('java.lang.Long',[long],[44],Out),cli_to_str(Out,Str).
% ==
% arity 4 exists to specify generic types
% ==
% ?- cli_new('System.Int64',[int],[44],Out),cli_to_str(Out,Str).
% ?- cli_new('System.Text.StringBuilder',[string],["hi there"],Out),cli_to_str(Out,Str).
% ?- cli_new('System.Int32'(int),[44],Out),cli_to_str(Out,Str).
% ==
%
%   ClazzSpec can be:
%    * an atomic classname
%       e.g. 'java.lang.String'
%    * an atomic descriptor
%       e.g. '[I' or 'Ljava.lang.String;'
%    * a suitable type
%       i.e. any class(_,_) or array(_)
%
%   if ClazzSpec is an object (non-array)  type   or  descriptor and Params is a
%   list of values or references, then Result is the result of an invocation
%   of  that  type's  most  specifically-typed    constructor  to  whose
%   respective formal parameters the actual   Params are assignable (and
%   assigned)
%
%   if ClazzSpec is an array type or descriptor   and Params is a list of values
%   or references, each of which is   (independently)  assignable to the
%   array element type, then Result is a  new   array  of as many elements as
%   Params has members,  initialised  with   the  respective  members of
%   Params;
%
%   if ClazzSpec is an array type  or   descriptor  and Params is a non-negative
%   integer N, then Result is a new array of that type, with N elements, each
%   initialised to CLR's appropriate default value for the type;
%
%   If Result is {Term} then we attempt to convert a new PlTerm instance to
%   a corresponding term; this is of  little   obvious  use here, but is
%   consistent with cli_call/4 and cli_get/3
%
% Make a "new string[32]" and get it's length.
% ==
%  ?- cli_new(array(string),[int],[32],O),cli_get(O,'Length',L).
% ==

cli_new(ClazzConstArgs,Out):- ClazzConstArgs=..[BasicType|ConstArgs],cli_new(BasicType,ConstArgs,Out).
cli_new(Clazz,ConstArgs,Out):- Clazz=..[BasicType|ParmSpc],cli_new(BasicType,ParmSpc,ConstArgs,Out).


%=========================================
% Object CALL
%=========================================

%% cli_call(+ClazzOrInstance,+CallTerm,-Result).
%% cli_call(+ClazzOrInstance,+MethodSpec,+Params,-Result).
%% cli_call_raw(+ClazzOrInstance, +MethodSpec, +Params, -Result).
%% cli_raise_event_handler(+ClazzOrInstance,+MemberSpec, +Params, -Result).
%
%   ClazzOrInstance should be:
%     * an object reference
%       (for static or instance methods)
%     * a classname, descriptor or type
%       (for static methods of the denoted class)
%
%   MethodSpec should be:
%     * a method name (as an atom)
%       (may involve dynamic overload resolution based on inferred types of params)
%
%   Params should be:
%     * a proper list (perhaps empty) of suitable actual parameters for the named method
%
%   CallTerm should be:
%     * a method name with parameters
%       (may involve dynamic overload resolution based on inferred types of params)
%
%   finally, an attempt will be made to unify Result with the returned result

cli_call(Obj,[Prop|CallTerm],Out):- cli_get(Obj,Prop,Mid),!,cli_call(Mid,CallTerm,Out).
cli_call(Obj,CallTerm,Out):- CallTerm=..[MethodName|Args],cli_call(Obj,MethodName,Args,Out).

% arity 4
cli_call(Obj,[Prop|CallTerm],Params,Out):- cli_get(Obj,Prop,Mid),!,cli_call(Mid,CallTerm,Params,Out).

% UNUSED: cli_call(Obj,MethodSpec,Params,Out):- cli_expand(Obj,ObjO),cli_call_raw(ObjO,MethodSpec,Params,Out_raw),!,cli_unify(Out,Out_raw).

cli_call(Obj,MethodSpec,Params,Out):- cli_expand(Obj,ObjO),
   cli_call_raw(ObjO,MethodSpec,Params,Out).


%=========================================
% Library Call
%=========================================

%% cli_lib_call(+CallTerm, -Result).
%
%   CallTerm should be:
%     * a method name with parameters
%       (may involve dynamic overload resolution based on inferred types of params)
%
%   finally, an attempt will be made to unify Result with the returned result
cli_lib_call(CallTerm,Out):- cli_lib_type(LibType),cli_call(LibType,CallTerm,Out).

%=========================================
% Object GET
%=========================================
:- dynamic(cli_get_hook/3).
:- multifile(cli_get_hook/3).

%% cli_set(+Obj,+NameValueParis:list).
%% cli_get(+Obj,+NameValueParis:list).
%  gets or set multiple values


%% cli_get(+ClazzOrInstance, +MemberSpec, -Value).
%% cli_set(+ClazzOrInstance, +MemberSpec, +Value).
%% cli_get_raw(+ClazzOrInstance,+MemberSpec,-Value).
%% cli_set_raw(+ClazzOrInstance,+MemberSpec,+Value).
%% cli_get_field(+ClazzOrInstance,+MemberSpec,-Value).
%% cli_set_field(+ClazzOrInstance,+MemberSpec,+Value).
%% cli_set_property(+ClazzOrInstance,+MemberSpec,+IndexValues,+Value).
%% cli_get_property(+ClazzOrInstance,+MemberSpec,+IndexValues,-Value).
%
%   _get/_set (the first two) 
%    Attempts to find the "best" member 
%     * Public properties, fields and bean-ifications (happy, is_happy, GetHappy, get_Happy, etc)
%     * Nonpublic properties, fields and bean-ifications (is_happy, GetHappy, get_Happy, etc)
%     * Case insensive public and non-public
%   _raw is the foreing impls of the first two (Actually the above search impl is done from this _raw)
%   _field will only try to set fields
%   _property will only try to set fields
%
%   ClazzOrInstance can be:
%     * a classname, a descriptor, or an (object or array) type
%       (for static fields);
%     * a non-array object
%       (for static and non-static fields)
%     * an array
%       (for 'length' pseudo field, or indexed element retrieval),
%   but not:
%     * a String
%       (clashes with class name; anyway, String has no fields to retrieve)
%
%   MemberSpec can be:
%       * an atomic field name,
%       * or an integral array index (to get an element from an array,
%	* or a pair I-J of integers (to get a subrange (slice?) of an
%	  array)
%       * A list of  [a,b(1),c] to denoate cli getting X.a.b(1).c
%       * [#f(fieldname),#p(propertyname),#p(propertyname,indexer)] when you want to avoid the search
%
%   IndexValues can be:
%	* Property index params ["foo",1] or []
%
%   Value:
%       * Getting, an attempt will be made to unify Value with the retrieved value
%       * Setting, put Value

cli_get(Obj,NVs):- forall(member_elipse(N=V,NVs),cli_get(Obj,N,V)).

cli_get(Obj,_,_):- cli_non_obj(Obj),!,fail.
cli_get(Expand,Prop,Value):- cli_expanded(Expand,ExpandO),!,cli_get(ExpandO,Prop,Value).
cli_get(Obj,[P],Value):- !,cli_get(Obj,P,Value).
cli_get(Obj,[P|N],Value):- !,cli_get(Obj,P,M),cli_get(M,N,Value),!.
cli_get(Obj,P,ValueOut):- hcli_get_overloaded(Obj,P,Value),!,cli_unify(Value,ValueOut).

hcli_get_overloaded(Obj,_,_):- cli_non_obj(Obj),!,fail,throw(cli_non_obj(Obj)).
hcli_get_overloaded(Obj,P,Value):- cli_get_hook(Obj,P,Value),!.
hcli_get_overloaded(Obj,P,Value):- compound(P),!,cli_call(Obj,P,Value),!.
hcli_get_overloaded(Obj,P,Value):- cli_get_raw(Obj,P,Value),!.
hcli_get_overloaded(Obj,P,Value):- not(atom(Obj)),cli_get_type(Obj,CType),!,hcli_get_type_subprops(CType,Sub),hcli_get_raw_0(Obj,Sub,SubValue),hcli_get_overloaded(SubValue,P,Value),!.

hcli_get_raw_0(Obj,[P],Value):- !,hcli_get_raw_0(Obj,P,Value).
hcli_get_raw_0(Obj,[P|N],Value):- !,hcli_get_raw_0(Obj,P,M),hcli_get_raw_0(M,N,Value),!.
hcli_get_raw_0(Obj,P,Value):- cli_get_raw(Obj,P,Value),!.

%%hcli_get_type_subprops(CType,Sub):- cli_ProppedType(
hcli_get_type_subprops(CType,Sub):- cli_subproperty(Type,Sub),cli_subclass(CType,Type).


%=========================================
% Object SET
%=========================================
:- dynamic(cli_set_hook/3).
:- multifile(cli_set_hook/3).

cli_set(Obj,NVs):- forall(member_elipse(N=V,NVs),cli_set(Obj,N,V)).
cli_set(Obj,_,_):- cli_non_obj(Obj),!,fail.
cli_set(Expand,Prop,Value):- cli_expanded(Expand,ExpandO),!,cli_set(ExpandO,Prop,Value).
cli_set(Obj,[P],Value):- !,cli_set(Obj,P,Value).
cli_set(Obj,[P|N],Value):- !,cli_get(Obj,P,M),cli_set(M,N,Value),!.
cli_set(Obj,P,Value):- hcli_set_overloaded(Obj,P,Value).

hcli_set_overloaded(Obj,_,_):- cli_non_obj(Obj),!,fail.
hcli_set_overloaded(Obj,P,ValueI):- cli_expanded(ValueI,Value),!,hcli_set_overloaded(Obj,P,Value).
hcli_set_overloaded(Obj,P,Value):- cli_set_hook(Obj,P,Value),!.
hcli_set_overloaded(Obj,P,Value):- cli_subproperty(Type,Sub),cli_is_type(Obj,Type),hcli_get_raw_0(Obj,Sub,SubValue),hcli_set_overloaded(SubValue,P,Value),!.
hcli_set_overloaded(Obj,P,Value):- cli_set_raw(Obj,P,Value),!.


%=========================================
% Object EVENT
%=========================================

%% cli_new_event_waiter(+ClazzOrInstance,+MemberSpec,-WaitOn).
% Creates a new ManualResetEvent (WaitOn) that when an Event is called WaitOn in pulsed so that cli_block_until_event/3 will unblock 

%% cli_add_event_waiter(+WaitOn,+ClazzOrInstance,+MemberSpec,-NewWaitOn).
% Adds a new Event to the ManualResetEvent (WaitOn) created by cli_new_event_waiter/3

%% cli_block_until_event(+WaitOn,+Time,+Lambda).
% Calls (foreignly defined) cli_block_until_event/4 and then cleansup the .NET objects.
cli_block_until_event(WaitOn,Time,Lambda):- setup_call_cleanup(true,cli_block_until_event(WaitOn,Time,Lambda,_),cli_call(WaitOn,'Dispose',_)).

%% cli_block_until_event(+WaitOn,+MaxTime,+TestVarsCode,-ExitCode).
% foreignly defined tododocs


%% cli_new_delegate(+DelegateClass,+PrologPred,-Value).
%% cli_new_delegate_term(+TypeFi,+PrologPred,+BooleanSaveKey,-Delegate).
% todo

%% cli_add_event_handler(+Term1,+Arity,+IntPtrControl,Pred).
% @see cli_add_event_handler/4

%% cli_add_event_handler(+ClazzOrInstance,+MemberSpec,+PrologPred).
% Create a .NET Delegate that calls PrologPred when MemberSpec is called

%% cli_remove_event_handler(+ClazzOrInstance,+MemberSpec,+PrologPred).
%

/*

ADDING A NEW EVENT HOOK

We already at least know that the object we want to hook is found via our call to

?- botget(['Self'],AM).

So we ask for the e/7 (event handlers of the members)

?- botget(['Self'],AM),cli_memb(AM,e(A,B,C,D,E,F,G)). 

 Press ;;;; a few times until you find the event Name you need (in the B var)

A = 6,                                          % index number
B = 'IM',                                       % event name
C = 'System.EventHandler'('InstantMessageEventArgs'),   % the delegation type
D = ['Object', 'InstantMessageEventArgs'],      % the parameter types (2)
E = [],                                         % the generic paramters
F = decl(static(false), 'AgentManager'),        % the static/non staticness.. the declaring class
G = access_pafv(true, false, false, false)      % the PAFV bits

So reading the parameter types  "['Object', 'InstantMessageEventArgs']" lets you know the pred needs at least two arguments
And "F = decl(static(false), 'AgentManager')" says add on extra argument at from for Origin

So registering the event is done:

?- botget(['Self'],AM), cli_add_event_handler(AM,'IM',handle_im(_Origin,_Object,_InstantMessageEventArgs))

To target a predicate like 

handle_im(Origin,Obj,IM):- writeq(handle_im(Origin,Obj,IM)),nl.



*/


%=========================================
% Prolog Backed Collection
%=========================================

%% cli_new_prolog_collection(+PredImpl,+ElementType,-PBD)
% Prolog Backed Collection

cli_new_prolog_collection(PredImpl,TypeSpec,PBC):- 
   module_functor(PredImpl,Module,Pred,_),
   atom_concat(Pred,'_get',GET),atom_concat(Pred,'_add',ADD),atom_concat(Pred,'_remove',REM),atom_concat(Pred,'_clear',CLR),
   PANON =..[Pred,_],PGET =..[GET,Val],PADD =..[ADD,Val],PREM =..[REM,Val],PDYN =..[Pred,Val],
   asserta(( PGET :- PDYN )),
   asserta(( PADD :- assert(PDYN) )),
   asserta(( PREM :- retract(PDYN) )),
   asserta(( CLR :- retractall(PANON) )),
   cli_new('Swicli.Library.PrologBackedCollection'(TypeSpec),0,
      [Module,GET,ADD,REM,CLR],PBC).

%=========================================
% Prolog Backed Dictionaries
%=========================================

%% cli_new_prolog_dictionary(+PredImpl,+KeyType,+ValueType,-PBD)
% Prolog Backed Dictionaries

cli_new_prolog_dictionary(PredImpl,KeyType,ValueType,PBD):- 
   cli_new_prolog_collection(PredImpl,KeyType,PBC),
   module_functor(PredImpl,Module,Pred,_),
   atom_concat(Pred,'_get',GET),atom_concat(Pred,'_set',SET),atom_concat(Pred,'_remove',REM),atom_concat(Pred,'_clear',CLR),
   PANON =..[Pred,_,_],PGET =..[GET,Key,Val], PSET =..[SET,Key,Val],PREM =..[REM,Val],PDYN =..[Pred,Key,Val],
   asserta(( PGET :- PDYN )),
   asserta(( PSET :- assert(PDYN) )),
   asserta(( PREM :- retract(PDYN) )),
   asserta(( CLR :- retractall(PANON) )),
   cli_new('Swicli.Library.PrologBackedDictionary'(KeyType,ValueType),0,
      [Module,GET,PBC,SET,REM,CLR],PBD).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* EXAMPLE: How to turn current_prolog_flag/2 into a PrologBacked dictionary
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Here is the webdocs:

create_prolog_flag(+Key, +Value, +Options)                         [YAP]
    Create  a  new Prolog  flag.    The ISO  standard does  not  foresee
    creation  of  new flags,  but many  libraries  introduce new  flags.

current_prolog_flag(?Key, -Value)    
    Get system configuration parameters

set_prolog_flag(:Key, +Value)                                      [ISO]
    Define  a new  Prolog flag or  change its value.   


It has most of the makings of a "PrologBackedDictionary"  but first we need a 
PrologBackedCollection to produce keys

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% First we'll need a conveinence predicate add_new_flag/1  for adding new flags for the collection
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

?- asserta(( add_new_flag(Flag):- create_prolog_flag(Flag,_,[access(read_write),type(term)])   )).

?- asserta(( current_pl_flag(Flag):- current_prolog_flag(Flag,_)   )).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Next we'll use the add_new_flag/1 in our PrologBackedCollection
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
?- context_module(Module),cli_new('Swicli.Library.PrologBackedCollection'(string),0,[Module,current_pl_flag,add_new_flag,@(null),@(null)],PBC).

% meaning:
       %% 'Swicli.Library.PrologBackedCollection'(string) ==> Type of object it returs to .NET is System.String
       %% 0 ==> First (only) constructor
       %% Module ==> user
       %% current_pl_flag ==> use current_pl_flag/1 for our GETTER of Items
       %% add_new_flag ==> Our Adder(Item) (defined in previous section)
       %% @(null) ==> No Remover(Item) 
       %% @(null) ==> No clearer
       %% PBC ==> Our newly created .NET ICollection<string>

% by nulls in the last two we've created a partially ReadOnly ICollection wexcept we can add keys


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Now we have a Keys collection let us declare the Dictionary (our intial objective)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
?- context_module(Module), cli_new('Swicli.Library.PrologBackedDictionary'(string,string),0,
           [Module,current_prolog_flag,$PBC,set_prolog_flag,@(null),@(null)],PBD).

       %% 'Swicli.Library.PrologBackedDictionary'(string) ==> Type of Key,Value it returns to .NET are System.Strings
       %% 0 ==> First (only) constructor
       %% Module ==> user
       %% current_prolog_flag ==> use current_prolog_flag/2 is a GETTER.
       %% $PBC ==> Our Key Maker from above
       %% set_prolog_flag/2 ==> our SETTER(Key,ITem)
       %% @(null) ==> No Remover(Key,Value) 
       %% @(null) ==> No clearer
       %% PBD ==> Our newly created .NET IDictionary<string,string>

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Now we have a have a PrologBackedDictionary in $PBD
% so let us play with it
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% is there a key named foo?

?- current_pl_flag(foo).
No.

%% Add a value to the Dictionanry
?- cli_map_add($PBD,foo,bar).
Yes.

%% set if there is a proper side effect
?- current_pl_flag(foo).
Yes.

?- current_prolog_flag(foo,X).
X = bar.
Yes.

?- cli_map($PBD,foo,X).
X = bar.
Yes.

?- cli_call($PBD,'ContainsKey'(foo),X).
X = @true.

%% iterate the Dictionary
?- cli_map($PBD,K,V).

*/

cli_demo(PBC,PBD):- asserta(( add_new_flag(Flag) :- create_prolog_flag(Flag,_,[access(read_write),type(term)])   )),
   asserta(( current_pl_flag(Flag):- current_prolog_flag(Flag,_)   )),
   context_module(Module),cli_new('Swicli.Library.PrologBackedCollection'(string),0,[Module,current_pl_flag,add_new_flag,@(null),@(null)],PBC),
   cli_new('Swicli.Library.PrologBackedDictionary'(string,string),0,[Module,current_prolog_flag,PBC,set_prolog_flag,@(null),@(null)],PBD).



%=========================================
% Module Utils
%=========================================

%% module_functor(+Obj, Arg2, Arg3, Arg4).

module_functor(PredImpl,Module,Pred,Arity):- strip_module(PredImpl,Module,NewPredImpl),strip_arity(NewPredImpl,Pred,Arity).
strip_arity(Pred/Arity,Pred,Arity).
strip_arity(PredImpl,Pred,Arity):- functor(PredImpl,Pred,Arity).


%:- use_module(library(jpl)).
%:- use_module(library(pce)).

%:- interactor.

%% cli_hide(+Pred).
% hide Pred from tracing

to_pi(M:F/A,M:PI):- functor(PI,F,A),!.
to_pi(F/A,M:PI):- context_module(M),functor(PI,F,A),!.
to_pi(M:PI,M:PI):- !.
to_pi(PI,M:PI):- context_module(M).
cli_hide(PIn):- to_pi(PIn,Pred),
  ignore(( '$set_predicate_attribute'(Pred, trace, 1),
   '$set_predicate_attribute'(Pred, noprofile, 1),
   '$set_predicate_attribute'(Pred, hide_childs, 1))).

:- meta_predicate(cli_notrace(0)).

%% cli_notrace(+Call) is nondet.
%  use call/1 with trace turned off
cli_notrace(Call):- tracing,notrace,!,call_cleanup(call(Call),trace).
cli_notrace(Call):- call(Call).

%% cli_class_from_type(+Type,-JClass).
%% cli_type_from_class(+JClass,-Type).
%% cli_find_class(+ClazzName,-ClazzObject).
%% cli_find_type(+ClazzSpec,+ClassRef).
%% cli_get_type(+Value,-Value).
%% cli_get_class(+Value,-Value).
%% cli_type_to_classname(+Value,-Value).
%% cli_type_to_fullname(+Value,-Value).
% todo

% cli_new('System.Drawing.Color',['Red'],C),cli_get_class(C,T),cli_class_from_type(T,CN).


%% cli_is_layout(+MemberSpec).
%% cli_add_layout(+ClazzSpec,+MemberSpec).
%% cli_add_layout(+ClazzSpec,+MemberSpec,+ToSpec).
%% cli_add_recomposer(+ClazzSpec,+MemberSpec,+Obj2r,+R2obj).
% need doc!

%% cli_find_constructor(+ClazzSpec,+MemberSpec,-Method).
%% cli_find_method(+ClazzOrInstance,+MemberSpec,-Method).
%% cli_add_shorttype(+Short,+Long).
%% cli_props_for_type(+ClazzSpec,+MemberSpecs).
% need doc

%% cli_special_unify(+Obj, Arg2).
%% cli_expand(+Obj, Arg2).
%% cli_expanded(+Obj, Arg2).
%% cli_eval(+Obj, Arg2, Arg3).
%% cli_eval_hook(+Obj, Arg2, Arg3).
%% cli_set_hook(+Obj, Arg2, Arg3).
%% cli_get_hook(+Obj, Arg2, Arg3).
%% cli_subproperty(+Obj, Arg2).
%% cli_link_swiplcs(+Obj).

%% cli_demo(+Obj, Arg2).
%% cli_is_defined(+Obj, Arg2).
%% cli_interned(+Obj, Arg2, Arg3).
%% cli_intern(+Obj, Arg2, Arg3).
%% cli_get_symbol(+Obj, Arg2, Arg3).
% need docs!

% ===================================================
% test preds
% ===================================================

%% cli_test_array_to_term1(-Value).
%% cli_test_array_to_term2(-Value).
%% cli_test_opt(+Incoming,?REFInt32Outbound).
%% cli_test_opt(+Incoming,+StringOptionalstr,?REFInt32Outbound).
%% cli_test_out(+Incoming,?REFInt32Outbound).
%% cli_test_pbc(+Pred,+Counted).
%% cli_test_pbct(+Pred,+Counted).
%% cli_test_pbd(+Pred,+Counted).
%% cli_test_pbdt(+Pred,+Counted).
%% cli_test_ref(+Incoming,?REFInt32Outbound).
%% cli_test_ref(+Incoming,?REFStringOptionalstr,?REFInt32Outbound).
%% cli_test_var_arg(?REFInt32Outbound,+ArrayOfInt32Incoming).
% Assembly definition test preds for Examples



cap_word(In,Out):- atom_codes(In,[L|Rest]),code_type(U,to_upper(L)),atom_codes(Out,[U|Rest]).

ppList2Args(PP,Args):- ppList2Args0(PP,Args).

ppList2Args0([],[]):- !.
ppList2Args0([P|PP],[A|Args]):- 
   ppList2Arg(P,A),
   ppList2Args0(PP,Args).

ppList2Arg('PlTerm':A,AA):- !,ppList2Arg(A,AA).
ppList2Arg('Int32':A,AA):- !,ppList2Arg(A,AA).
ppList2Arg(A:B,AA):- ppList2Arg(A,A1),ppList2Arg(B,B1),atom_concat(A1,B1,AB),!,ppList2Arg(AB,AA).
ppList2Arg(F,B):- compound(F),F=..List,atomic_list_concat(List,'',A),!,ppList2Arg(A,B).
ppList2Arg(A,BB):- atomic_list_concat([B,''],"Out",A),!,cap_word(B,BB1),atomic_list_concat([-,BB1],'',BB).
ppList2Arg(A,BB):- atomic_list_concat([B,''],"In",A),!,cap_word(B,BB1),atomic_list_concat([+,BB1],'',BB).
ppList2Arg(A,BB):- atomic_list_concat([_,_|_],"Byref",A),!,A=B,cap_word(B,BB1),atomic_list_concat([?,BB1],'',BB).
ppList2Arg(A,BB):- atomic_list_concat([_,_|_],"Out",A),!,A=B,cap_word(B,BB1),atomic_list_concat([-,BB1],'',BB).
ppList2Arg(A,BB):- atomic_list_concat([_,_|_],"In",A),A=B,!,cap_word(B,BB1),atomic_list_concat([+,BB1],'',BB).
ppList2Arg(A,BB):- atomic_list_concat([A],'',B),cap_word(B,BB).


bot_params_to_list(PPs,PNs):- findall(T:N,(cli_col(PPs,PI),bot_param(PI,T,N)),PNs).

bot_param(PI,T,N):- cli_get(PI,'ParameterType',TR),cli_type_to_typespec(TR,T),cli_get(PI,'Name',N).


% cli_docs:- predicate_property(swicli:P,file(_)),P=P,!.
cli_docs:- cli_find_type('Swicli.Library.PrologCLR',T),
   cli_get(static(T),'AutoDocInfos',SRF),cli_map(SRF,K,V),P=V,cli_get(P,'GetParameters',PPs),
   bot_params_to_list(PPs,PP),
   cli_member_doc(P,_Doc,_XML),
   atomic_list_concat([FC,AC],"/",K),atom_number(AC,A),string_to_atom(FC,F),
    ppList2Args(PP,Args),PRED=..[F|Args],A=A,
    cli_to_str(V,VS),
   %% cli_to_str(F/A=eval(cli_get(V,name)):PRED:Doc,TSTR),
    %%term_to_atom(TSTR,ASTR),string_to_atom(STR,ASTR),
    'format'('~n%% ~w',[PRED]),
    %%'format'('% ~w~n',[Doc]),
    VS==VS, %%'format'('%       Foreign call to ~w~n',[VS]),
    fail.

cli_start_pldoc_server:- use_module(library(pldoc)), doc_server(57007,[workers(5)]) , portray_text(true). 

/** <module> SWI-Prolog 2-Way interface to .NET/Mono

*Introduction*

This is an overview of an interface which allows SWI-Prolog programs to dynamically create and manipulate .NET objects. 

Here are some significant features of the interface and its implementation: 

* API is similar to that of XPCE: the four main interface calls are cli_new, cli_call, cli_set and cli_get (there is a single cli_free, though .NET's garbage collection is extended transparently into Prolog) 
* Uses @/1 to construct representations of certain .NET values; if  @/1 is defined as a prefix operator (as used by XPCE), then you can write @false, @true, @null etc. in your source code; otherwise (and for portability) you'll have to write e.g. @(true) etc. 
* cli_call/4 (modeled from JPL's jpl_call/4) resolves overloaded methods automatically and dynamically, inferring the types of the call's actual parameters, and identifying the most specific of the applicable method implementations (similarly, cli_new resolves overloaded constructors)
* Completely dynamic: no precompilation is required to manipulate any .NET classes which can be found at run time, and any objects which can be instantiated from them 
* Interoperable with SwiPlCS's .NET API (which has evolved from Uwe Lesta's SwiPlCS) 
* Exploits the Invocation API of the .NET P/Invoke Interface: this is a mandatory feature of any compliant .NET 
* Implemented with a fair amount of C# code and Prolog code in one module (swicli.pl)  (which I believe to be ISO Standard Prolog compliant and portable) and a SWI-Prolog-specific foreign library (swicli[32].dll for Windows and swicli[32].so *nix), implemented in ANSI C but making a lot of use of the SWI-Prolog Foreign Language Interface Then uses Swicli.Library.dll (Managed binary) that runs on both Mono and .NET runtimes. 
* the Prolog-calls-CLI (mine) and CLI-calls-Prolog (Ewe's) parts of SWICLI are largely independent; mine concentrates on representing all .NET data values and objects within Prolog, and supporting manipulation of objects; Ewe's concentrates on representing any Prolog term within .NET, and supporting the calling of goals within Prolog and the retrieving of results back into .NET 
* @(terms) are canonical (two references are ==/2 equal if-and-only-if they refer to the same object within the .NET) 
* are represented as structures containing a distinctive atom so as to exploit SWI-Prolog's atom garbage collection: when an object reference is garbage-collected in Prolog, the .NET garbage collector is informed, so there is sound and complete overall garbage collection of .NET objects within the combined Prolog+.NET system 
* .NET class methods can be called by name: SWICLI invisibly fetches (and caches) essential details of method invocation, exploiting .NET Reflection facilities 
* Reason about the types of .NET data values, object references, fields and methods: SWICLI supports a canonical representation of all .NET types as structured terms (e.g. array(array(byte))) and also as atomic .NET signatures 
* when called from Prolog, void methods return a @(void) value (which is distinct from all other SWICLI values and references) 
* Tested on Windows XP, Windows7 and Fedora Linux, but is believed to be readily portable to SWI-Prolog on other platforms as far as is feasible, .NET data values and object references are represented within Prolog canonically and without loss of information (minor exceptions: .NET float and double values are both converted to Prolog float values; .NET byte, char, short, int and long values are all converted to Prolog integer values; the type distinctions which are lost are normally of no significance) 
* Requires .NET 2.0 and class libraries (although it doesn't depend on any .NET 2-specific facilities, and originally was developed for use with both 1.0 thru 4.0 .NETs, I haven't tested it with 1.0 recently, and don't support this) 

==

?- use_module(library(swicli)).

?- cli_call('System.Threading.ThreadPool','GetAvailableThreads'(X,Y),_).

X=499, Y=1000

==

?- cli_call('System.Environment','Version',X),cli_writeln(X).
"2.0.50727.5448"
X = @'C#499252128'.

==

Doc root and Download will be findable from http://code.google.com/p/opensim4opencog/wiki/SwiCLI


@see	CSharp.txt
	
@author	Douglas Miles

*/

% :- cli_ensure_so_loaded.

export_prefixed(Cli):- 
 user:forall((current_predicate(swicli:F/A),atom_concat(Cli,_,F)),
  catch(
    (swicli:export(F/A),
     % writeln(':-'(export(F/A))),
     functor(P,F,A),
     swicli:cli_hide(P)),_,true)).


cli_init:- user:forall(clause(swicli:cli_init0,B),swicli:cli_must(once(cli_trace_call(B)))).

:- debug(swicli).
:- cli_init.
:- cli_trace_call((cli_call('System.Threading.ThreadPool','GetAvailableThreads'(_X,_Y),_))).
:- cli_trace_call((cli_call('System.Environment','Version',X),cli_writeln(X))).

end_of_file.




:- cli_load_lib_safe('SWIProlog','Swicli.Library.dll','Swicli.Library.Embedded','install').




end_of_file.

%
%    ?- cli_get_type($Obj,Type),cli_object_get_typename(Type,Name).
%    Type = @'C#516939520',
%    Name = 'System.Collections.Generic.List`1[[System.String, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]]'.
%
%
%    ?- cli_get_type($Obj,Type), cli_type_to_typespec(Type,Name).
%    Type = @'C#516939520',
%    Name = 'System.Collections.Generic.List'('String').
%
%
%    ?- cli_add_shorttype(stringl,'System.Collections.Generic.List'('String')).
%    true.
%
%
%    ?- cli_new(stringl,[],O).
%    O = @'C#516939472'.
%
%
%    ?- cli_get_type($O,Type),cli_type_to_typespec(Type,Name).
%    Type = @'C#516939520',
%    Name = 'System.Collections.Generic.List'('String').
%
%    
%    
%    [debug] 3 ?- cli_new('System.Collections.Generic.List'(string),[int],[10],O),cli_members(O,M),!,member(E,M),writeq(E),nl,fail.
%    f(0,'_items'(arrayOf('String')))
%    f(1,'_size'('Int32'))
%    f(2,'_version'('Int32'))
%    f(3,'_syncRoot'('Object'))
%    f(4,'_emptyArray'(arrayOf('String')))
%    f(5,'_defaultCapacity'('Int32'))
%    p(0,'Capacity'('Int32'))
%    p(1,'Count'('Int32'))
%    p(2,'System.Collections.IList.IsFixedSize'('Boolean'))
%    p(3,'System.Collections.Generic.ICollection<T>.IsReadOnly'('Boolean'))
%    p(4,'System.Collections.IList.IsReadOnly'('Boolean'))
%    p(5,'System.Collections.ICollection.IsSynchronized'('Boolean'))
%    p(6,'System.Collections.ICollection.SyncRoot'('Object'))
%    p(7,'Item'('String'))
%    p(8,'System.Collections.IList.Item'('Object'))
%    m(0,'ConvertAll'('Converter'('String',<)))
%    m(1,get_Capacity)
%    m(2,set_Capacity('Int32'))
%    m(3,get_Count)
%    m(4,'System.Collections.IList.get_is_FixedSize')
%    m(5,'System.Collections.Generic.ICollection<T>.get_is_ReadOnly')
%    m(6,'System.Collections.IList.get_is_ReadOnly')
%    m(7,'System.Collections.ICollection.get_is_Synchronized')
%    m(8,'System.Collections.ICollection.get_SyncRoot')
%    m(9,get_item('Int32'))
%    m(10,set_item('Int32','String'))
%    m(11,'IsCompatibleObject'('Object'))
%    m(12,'VerifyValueType'('Object'))
%    m(13,'System.Collections.IList.get_item'('Int32'))
%    m(14,'System.Collections.IList.set_item'('Int32','Object'))
%    m(15,'Add'('String'))
%    m(16,'System.Collections.IList.Add'('Object'))
%    m(17,'AddRange'('System.Collections.Generic.IEnumerable'('String')))
%    m(18,'AsReadOnly')
%    m(19,'BinarySearch'('Int32','Int32','String','System.Collections.Generic.IComparer'('String')))
%    m(20,'BinarySearch'('String'))
%    m(21,'BinarySearch'('String','System.Collections.Generic.IComparer'('String')))
%    m(22,'Clear')
%    m(23,'Contains'('String'))
%    m(24,'System.Collections.IList.Contains'('Object'))
%    m(25,'CopyTo'(arrayOf('String')))
%    m(26,'System.Collections.ICollection.CopyTo'('Array','Int32'))
%    m(27,'CopyTo'('Int32',arrayOf('String'),'Int32','Int32'))
%    m(28,'CopyTo'(arrayOf('String'),'Int32'))
%    m(29,'EnsureCapacity'('Int32'))
%    m(30,'Exists'('System.Predicate'('String')))
%    m(31,'Find'('System.Predicate'('String')))
%    m(32,'FindAll'('System.Predicate'('String')))
%    m(33,'FindIndex'('System.Predicate'('String')))
%    m(34,'FindIndex'('Int32','System.Predicate'('String')))
%    m(35,'FindIndex'('Int32','Int32','System.Predicate'('String')))
%    m(36,'FindLast'('System.Predicate'('String')))
%    m(37,'FindLastIndex'('System.Predicate'('String')))
%    m(38,'FindLastIndex'('Int32','System.Predicate'('String')))
%    m(39,'FindLastIndex'('Int32','Int32','System.Predicate'('String')))
%    m(40,'ForEach'('System.Action'('String')))
%    m(41,'GetEnumerator')
%    m(42,'System.Collections.Generic.IEnumerable<T>.GetEnumerator')
%    m(43,'System.Collections.IEnumerable.GetEnumerator')
%    m(44,'GetRange'('Int32','Int32'))
%    m(45,'IndexOf'('String'))
%    m(46,'System.Collections.IList.IndexOf'('Object'))
%    m(47,'IndexOf'('String','Int32'))
%    m(48,'IndexOf'('String','Int32','Int32'))
%    m(49,'Insert'('Int32','String'))
%    m(50,'System.Collections.IList.Insert'('Int32','Object'))
%    m(51,'InsertRange'('Int32','System.Collections.Generic.IEnumerable'('String')))
%    m(52,'LastIndexOf'('String'))
%    m(53,'LastIndexOf'('String','Int32'))
%    m(54,'LastIndexOf'('String','Int32','Int32'))
%    m(55,'Remove'('String'))
%    m(56,'System.Collections.IList.Remove'('Object'))
%    m(57,'RemoveAll'('System.Predicate'('String')))
%    m(58,'RemoveAt'('Int32'))
%    m(59,'RemoveRange'('Int32','Int32'))
%    m(60,'Reverse')
%    m(61,'Reverse'('Int32','Int32'))
%    m(62,'Sort')
%    m(63,'Sort'('System.Collections.Generic.IComparer'('String')))
%    m(64,'Sort'('Int32','Int32','System.Collections.Generic.IComparer'('String')))
%    m(65,'Sort'('System.Comparison'('String')))
%    m(66,'ToArray')
%    m(67,'TrimExcess')
%    m(68,'TrueForAll'('System.Predicate'('String')))
%    m(69,'ToString')
%    m(70,'Equals'('Object'))
%    m(71,'GetHashCode')
%    m(72,'GetType')
%    m(73,'Finalize')
%    m(74,'MemberwiseClone')
%    c(0,'List`1')
%    c(1,'List`1'('Int32'))
%    c(2,'List`1'('System.Collections.Generic.IEnumerable'('String')))
%    c(3,'List`1')
%    
%    2 ?- botget([self,simposition],X).
%    X = struct('Vector3', 153.20449829101562, 44.02702713012695, 63.06859588623047).
%    
%    cli_get_type(struct('Vector3', 153.20449829101562, 44.02702713012695, 63.06859588623047),T),cli_writeln(T).
%    
%    cli_writeln(struct('Vector3', 153.20449829101562, 44.02702713012695, 63.06859588623047)).
%    
%    cli_typeToSpec/2
%    
%    cli_SpecToType/2
%    
%     Just file storage now...
%    
%     [debug] 8 ?- cli_new('System.Collections.Generic.Dictionary'(string,string),[],[],O),cli_get(O,count,C)
%    
%     cli_new('System.Collections.Generic.List'(string),[int],[10],O),cli_get_type(O,T),cli_writeln(T).
%    
%     cli_new('System.Collections.Generic.List'(string),[int],[10],O),cli_members(O,M)
%    
%     ERROR: findField IsVar _g929 on type System.Collections.Generic.Dictionary`2[System.String,System.String]
%    ERROR: findProperty IsVar _g929 on type System.Collections.Generic.Dictionary`2[System.String,System.String]
%       Call: (9) message_to_string('Only possible for compound or atoms', _g1079) ? leap
%    
%    cli_add_shorttype(dict,'System.Collections.Generic.Dictionary`2').
%    
%    cli_find_type(dict(string,string),Found).
%    
%    12 ?- cli_find_class('System.Collections.Generic.Dictionary'('int','string'),X),cli_to_str(X,Y).
%    X = @'C#592691552',
%    Y = "class cli.System.Collections.Generic.Dictionary$$00602_$$$_i_$$_Ljava_lang_String_$$$$_".
%    
%    13 ?- cli_find_type('System.Collections.Generic.Dictionary'('int','string'),X),cli_to_str(X,Y).
%    X = @'C#592687600',
%    Y = "System.Collections.Generic.Dictionary`2[System.Int32,System.String]".
%    
%    
%    ?- cli_find_type('System.Collections.Generic.Dictionary'(string,string),NewObj).
%    
%    ?- cli_new('System.Collections.Generic.Dictionary'(string,string),NewObj).
%    %=========================================
%    %=========================================
%    5 ?- grid_object(O),cli_get(O,simregion,X),cli_to_str(X,S).
%    O = @'C#701938432',
%    X = @'C#702256808',
%    S = "Belphegor (216.82.46.79:13005)" .
%    
%    4 ?- grid_object(O),cli_get(O,name,X),cli_to_str(X,S).
%    O = @'C#701938432',
%    X = S, S = "BinaBot Daxeline" .
%    
%    5 ?- grid_object(O),cli_get(O,simulator,X),cli_to_str(X,S).
%    O = @'C#701938432',
%    X = @'C#701938424',
%    S = "Belphegor (216.82.46.79:13005)" .
%    
%    1 ?- grid_object(O),cli_get(O,'Type',X),cli_to_str(X,S).
%    O = @'C#679175768',
%    X = @'C#679175760',
%    S = "cogbot.TheOpenSims.SimAvatarImpl" .
%    
%    7 ?- cli_get('cogbot.Listeners.WorldObjects','SimObjects',Objs),cli_get(Objs,'count',X).
%    Objs = @'C#585755456',
%    X = 5905.
%    
%    
%    22 ?- current_bot(X),cli_members(X,M),cli_to_str(M,S).
%    X = @'C#585755312',
%    M = [m('GetFolderItems'('String')), m('GetFolderItems'('UUID')), m('SetRadegastLoginOptions'), m('GetGridIndex'('String', 'Int32&')), m('SetRadegastLoginForm'('LoginConsole', 'LoginOptions')), m('GetLoginOptionsFromRadegast'), m('ShowTab'('String')), m('AddTab'(..., ..., ..., ...)), m(...)|...],
%    S = "[m(GetFolderItems(String)),m(GetFolderItems(UUID)),m(SetRadegastLoginOptions),m(GetGridIndex(String,Int32&)),m(SetRadegastLoginForm(LoginConsole,LoginOptions)),m(GetLoginOptionsFromRadegast),m(ShowTab(String)),m(AddTab(String,String,UserControl,EventHandler)),m(InvokeThread(String,ThreadStart)),m(InvokeGUI(Control,ThreadStart)),m(InvokeGUI(ThreadStart)),m(GetSecurityLevel(UUID)),m(ExecuteTask(String,TextReader,OutputDelegate)),m(DoHttpGet(String)),m(DoHttpPost(arrayOf(Object))),m(ExecuteXmlCommand(String,OutputDelegate)),m(XmlTalk(String,OutputDelegate)),m(DoAnimation(String)),m(GetAnimationOrGesture(String)),m(Talk(String)),m(Talk(String,Int32,ChatType)),m(InstantMessage(UUID,String,UUID)),m(NameKey),m(get_EventsEnabled),m(set_EventsEnabled(Boolean)),m(<.ctor>b_0),m(<.ctor>b_1),m(<.ctor>b_2),m(<.ctor>b_3),m(<.ctor>b_4),m(<DoAnimation>b_34),m(op_implicit(current_bot)),m(get_network),m(get_Settings),m(get_Parcels),m(get_Self),m(get_Avatars),m(get_Friends),m(get_grid),m(get_Objects),m(get_groups),m(get_Assets),m(get_Estate),m(get_Appearance),m(get_inventory),m(get_directory),m(get_terrain),m(get_Sound),m(get_throttle),m(OnEachSimEvent(SimObjectEvent)),m(add_EachSimEvent(EventHandler`1)),m(remove_EachSimEvent(EventHandler`1)),m(get_is_LoggedInAndReady),m(Login),m(Login(Boolean)),m(LogException(String,Exception)),m(get_BotLoginParams),m(GetBotCommandThreads),m(AddThread(Thread)),m(RemoveThread(Thread)),m(getPosterBoard(Object)),m(get_masterName),m(set_masterName(String)),m(get_masterKey),m(set_masterKey(UUID)),m(get_AllowObjectMaster),m(get_is_RegionMaster),m(get_theRadegastInstance),m(set_theRadegastInstance(RadegastInstance)),m(IMSent(Object,InstantMessageSentEventArgs)),m(add_OnInstantMessageSent(InstantMessageSentArgs)),m(remove_OnInstantMessageSent(InstantMessageSentArgs)),m(SetLoginName(String,String)),m(SetLoginAcct(LoginDetails)),m(LoadTaskInterpreter),m(StartupClientLisp),m(RunOnLogin),m(SendResponseIM(GridClient,UUID,OutputDelegate,String)),m(updateTimer_Elapsed(Object,ElapsedEventArgs)),m(AgentDataUpdateHandler(Object,PacketReceivedEventArgs)),m(GroupMembersHandler(Object,GroupMembersReplyEventArgs)),m(AvatarAppearanceHandler(Object,PacketReceivedEventArgs)),m(AlertMessageHandler(Object,PacketReceivedEventArgs)),m(ReloadGroupsCache),m(GroupName2UUID(String)),m(Groups_OnCurrentGroups(Object,CurrentGroupsEventArgs)),m(Self_OnTeleport(Object,TeleportEventArgs)),m(Self_OnChat(Object,ChatEventArgs)),m(Self_OnInstantMessage(Object,InstantMessageEventArgs)),m(DisplayNotificationInChat(String)),m(Inventory_OnInventoryObjectReceived(Object,InventoryObjectOfferedEventArgs)),m(Network_OnDisconnected(Object,DisconnectedEventArgs)),m(EnsureConnectedCheck(DisconnectType)),m(Network_OnConnected(Object)),m(Network_OnSimDisconnected(Object,SimDisconnectedEventArgs)),m(Client_OnLogMessage(Object,LogLevel)),m(Network_OnEventQueueRunning(Object,EventQueueRunningEventArgs)),m(Network_OnSimConnected(Object,SimConnectedEventArgs)),m(Network_OnSimConnecting(Simulator)),m(Network_OnLogoutReply(Object,LoggedOutEventArgs)),m(UseInventoryItem(String,String)),m(ListObjectsFolder),m(wearFolder(String)),m(PrintInventoryAll),m(findInventoryItem(String)),m(logout),m(WriteLine(String)),m(DebugWriteLine(String,arrayOf(Object))),m(WriteLine(String,arrayOf(Object))),m(output(String)),m(describeAll(Boolean,OutputDelegate)),m(describeSituation(OutputDelegate)),m(describeLocation(Boolean,OutputDelegate)),m(describePeople(Boolean,OutputDelegate)),m(describeObjects(Boolean,OutputDelegate)),m(describeBuildings(Boolean,OutputDelegate)),m(get_LispTaskInterperter),m(enqueueLispTask(Object)),m(evalLispReader(TextReader)),m(evalLispReaderString(TextReader)),m(evalXMLString(TextReader)),m(XML2Lisp2(String,String)),m(XML2Lisp(String)),m(evalLispString(String)),m(evalLispCode(Object)),m(ToString),m(Network_OnLogin(Object,LoginProgressEventArgs)),m(InvokeAssembly(Assembly,String,OutputDelegate)),m(ConstructType(Assembly,Type,String,System.Predicate`1[[System.Type, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]],System.Action`1[[System.Type, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]])),m(LoadAssembly(Assembly)),m(RegisterCommand(String,Command)),m(RegisterCommand(Command)),m(DoCommandAll(String,UUID,OutputDelegate)),m(GetVoiceManager),m(Dispose),m(AddBotMessageSubscriber(SimEventSubscriber)),m(RemoveBotMessageSubscriber(SimEventSubscriber)),m(SendNetworkEvent(String,arrayOf(Object))),m(SendPersonalEvent(SimEventType,String,arrayOf(Object))),m(SendPipelineEvent(SimObjectEvent)),m(argsListString(IEnumerable)),m(argString(Object)),m(ExecuteCommand(String)),m(InvokeJoin(String)),m(InvokeJoin(String,Int32)),m(InvokeJoin(String,Int32,ThreadStart,ThreadStart)),m(InvokeNext(String,ThreadStart)),m(ExecuteCommand(String,OutputDelegate)),m(ExecuteBotCommand(String,OutputDelegate)),m(DoCmdAct(Command,String,String,OutputDelegate)),m(GetName),m(cogbot.Listeners.SimEventSubscriber.OnEvent(SimObjectEvent)),m(cogbot.Listeners.SimEventSubscriber.Dispose),m(TalkExact(String)),m(Intern(String,Object)),m(InternType(Type)),m(RegisterListener(Listener)),m(RegisterType(Type)),m(GetAvatar),m(FakeEvent(Object,String,arrayOf(Object))),m(Equals(Object)),m(GetHashCode),m(GetType),m(Finalize),m(MemberwiseClone),c(current_bot),c(current_bot(ClientManager,GridClient)),p(Network(NetworkManager)),p(Settings(Settings)),p(Parcels(ParcelManager)),p(Self(AgentManager)),p(Avatars(AvatarManager)),p(Friends(FriendsManager)),p(Grid(GridManager)),p(Objects(ObjectManager)),p(Groups(GroupManager)),p(Assets(AssetManager)),p(Estate(EstateTools)),p(Appearance(AppearanceManager)),p(Inventory(InventoryManager)),p(Directory(DirectoryManager)),p(Terrain(TerrainManager)),p(Sound(SoundManager)),p(Throttle(AgentThrottle)),p(IsLoggedInAndReady(Boolean)),p(BotLoginParams(LoginDetails)),p(MasterName(String)),p(MasterKey(UUID)),p(AllowObjectMaster(Boolean)),p(IsRegionMaster(Boolean)),p(TheRadegastInstance(RadegastInstance)),p(LispTaskInterperter(ScriptInterpreter)),p(EventsEnabled(Boolean)),e(EachSimEvent(Object,SimObjectEvent)),e(OnInstantMessageSent(Object,IMessageSentEventArgs)),f(m_EachSimEvent(EventHandler`1)),f(m_EachSimEventLock(Object)),f(OneAtATimeQueue(TaskQueueHandler)),f(gridclient_ref(GridClient)),f(LoginRetriesFresh(Int32)),f(LoginRetries(Int32)),f(ExpectConnected(Boolean)),f(thisTcpPort(Int32)),f(_BotLoginParams(LoginDetails)),f(botPipeline(SimEventPublisher)),f(botCommandThreads(IList`1)),f(XmlInterp(XmlScriptInterpreter)),f(GroupID(UUID)),f(GroupMembers(System.Collections.Generic.Dictionary`2[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null],[OpenMetaverse.GroupMember, OpenMetaverse, Version=0.0.0.26031, Culture=neutral, PublicKeyToken=null]])),f(Appearances(System.Collections.Generic.Dictionary`2[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null],[OpenMetaverse.Packets.AvatarAppearancePacket, OpenMetaverse, Version=0.0.0.26031, Culture=neutral, PublicKeyToken=null]])),f(Running(Boolean)),f(GroupCommands(Boolean)),f(_masterName(String)),f(PosterBoard(Hashtable)),f(SecurityLevels(System.Collections.Generic.Dictionary`2[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null],[cogbot.BotPermissions, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(_masterKey(UUID)),f(_theRadegastInstance(RadegastInstance)),f(OnInstantMessageSent(InstantMessageSentArgs)),f(VoiceManager(VoiceManager)),f(CurrentDirectory(InventoryFolder)),f(bodyRotation(Quaternion)),f(forward(Vector3)),f(left(Vector3)),f(up(Vector3)),f(updateTimer(System.Timers.Timer)),f(WorldSystem(WorldObjects)),f(GetTextures(Boolean)),f(describers(System.Collections.Generic.Dictionary`2[[System.String, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[cogbot.DescribeDelegate, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(listeners(System.Collections.Generic.Dictionary`2[[System.String, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[cogbot.Listeners.Listener, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(Commands(SortedDictionary`2)),f(tutorials(System.Collections.Generic.Dictionary`2[[System.String, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[cogbot.Tutorials.Tutorial, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(describeNext(Boolean)),f(describePos(Int32)),f(currTutorial(String)),f(BoringNamesCount(Int32)),f(GoodNamesCount(Int32)),f(RunningMode(Int32)),f(AnimationFolder(UUID)),f(searcher(BotInventoryEval)),f(taskInterperterType(String)),f(scriptEventListener(ScriptEventListener)),f(ClientManager(ClientManager)),f(muteList(System.Collections.Generic.List`1[[System.String, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]])),f(muted(Boolean)),f(GroupMembersRequestID(UUID)),f(GroupsCache(System.Collections.Generic.Dictionary`2[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null],[OpenMetaverse.Group, OpenMetaverse, Version=0.0.0.26031, Culture=neutral, PublicKeyToken=null]])),f(GroupsEvent(ManualResetEvent)),f(CatchUpInterns(MethodInvoker)),f(useLispEventProducer(Boolean)),f(lispEventProducer(LispEventProducer)),f(RunStartupClientLisp(Boolean)),f(RunStartupClientLisplock(Object)),f(_LispTaskInterperter(ScriptInterpreter)),f(LispTaskInterperterLock(Object)),f(registeredTypes(System.Collections.Generic.List`1[[System.Type, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]])),f(KnownAssembies(System.Collections.Generic.Dictionary`2[[System.Reflection.Assembly, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[System.Collections.Generic.List`1[[cogbot.Listeners.Listener, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]], mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]])),f(AssemblyListeners(System.Collections.Generic.Dictionary`2[[System.Reflection.Assembly, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[System.Collections.Generic.List`1[[cogbot.Listeners.Listener, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]], mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]])),f(TalkingAllowed(Boolean)),f(IsEnsuredRunning(Boolean)),f(EnsuredRadegastRunning(Boolean)),f(InvokedMakeRunning(Boolean)),f(AddingTypesToBotclientNow(Boolean)),f(NeedRunOnLogin(Boolean)),f(debugLevel(Int32)),f(CS$<>9_CachedAnonymousMethodDelegate5(MethodInvoker)),f(CS$<>9_CachedAnonymousMethodDelegate35(ThreadStart))]".
%    
%    24 ?- world_avatar(X),cli_members(X,M),cli_to_str(M,S).
%    X = @'C#585739064',
%    S = "[m(get_SelectedBeam),m(set_SelectedBeam(Boolean)),m(cogbot.TheOpenSims.SimActor.GetSelectedObjects),m(SelectedRemove(SimPosition)),m(SelectedAdd(SimPosition)),m(get_ProfileProperties),m(set_ProfileProperties(AvatarProperties)),m(get_AvatarInterests),m(set_AvatarInterests(Interests)),m(get_AvatarGroups),m(set_AvatarGroups(System.Collections.Generic.List`1[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null]])),m(set_is_Killed(Boolean)),m(ThreadJump),m(WithAnim(UUID,ThreadStart)),m(OpenNearbyClosedPassages),m(OnMoverStateChange(SimMoverState)),m(DebugInfo),m(LogEvent(SimObjectEvent)),m(AddCanBeTargetOf(Int32,SimObjectEvent)),m(get_theAvatar),m(get_SightRange),m(set_SightRange(Double)),m(GetKnownObjects),m(GetNearByObjects(Double,Boolean)),m(get_LastAction),m(set_LastAction(BotAction)),m(get_CurrentAction),m(set_CurrentAction(BotAction)),m(makeActionThread(BotAction)),m(MakeEnterable(SimMover)),m(RestoreEnterable(SimMover)),m(get_is_Root),m(get_is_Sitting),m(set_is_Sitting(Boolean)),m(get_HasPrim),m(get_is_Controllable),m(GetSimulator),m(get_globalPosition),m(GetSimRegion),m(get_SimRotation),m(Do(SimTypeUsage,SimObject)),m(TakeObject(SimObject)),m(AttachToSelf(SimObject)),m(WearItem(InventoryItem)),m(ScanNewObjects(Int32,Double,Boolean)),m(AddKnowns(System.Collections.Generic.IEnumerable`1[[cogbot.TheOpenSims.SimObject, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),m(ResetRegion(UInt64)),m(GetSizeDistance),m(GetGridClient),m(AddGoupRoles(System.Collections.Generic.List`1[[OpenMetaverse.AvatarGroup, OpenMetaverse, Version=0.0.0.26031, Culture=neutral, PublicKeyToken=null]])),m(TalkTo(SimAvatar,String)),m(TalkTo(SimAvatar,BotMentalAspect)),m(Debug(String,arrayOf(Object))),m(Eat(SimObject)),m(WithSitOn(SimObject,ThreadStart)),m(StopAllAnimations),m(WithGrabAt(SimObject,ThreadStart)),m(WithAnim(SimAsset,ThreadStart)),m(ExecuteLisp(SimObjectUsage,Object)),m(get_Flying),m(set_Flying(Boolean)),m(KilledPrim(Primitive,Simulator)),m(ResetPrim(Primitive,current_bot,Simulator)),m(SetFirstPrim(Primitive)),m(GetName),m(ToString),m(SetClient(current_bot)),m(FindSimObject(SimObjectType,Double,Double)),m(Matches(String)),m(StandUp),m(UpdateObject(ObjectMovementUpdate,ObjectMovementUpdate)),m(Touch(SimObject)),m(RemoveObject(SimObject)),m(StopMoving),m(Approach(SimObject,Double)),m(TrackerLoop),m(MoveTo(Vector3d,Double,Single)),m(Write(String)),m(GotoTarget(SimPosition)),m(SendUpdate(Int32)),m(TeleportTo(SimRegion,Vector3)),m(SetMoveTarget(SimPosition,Double)),m(OnlyMoveOnThisThread),m(SetMoveTarget(Vector3d)),m(EnsureTrackerRunning),m(get_ApproachPosition),m(set_ApproachPosition(SimPosition)),m(get_ApproachVector3D),m(set_ApproachVector3D(Vector3d)),m(get_KnownTypeUsages),m(SitOn(SimObject)),m(SitOnGround),m(SetObjectRotation(Quaternion)),m(TurnToward(Vector3)),m(TurnToward0(Vector3)),m(get_is_drivingVehical),m(UpdateOccupied),m(get_is_Walking),m(get_is_Flying),m(get_is_Standing),m(get_is_Sleeping),m(get_debugLevel),m(set_debugLevel(Int32)),m(GetCurrentAnims),m(GetAnimUUIDs(List`1)),m(GetBeforeUUIDs(List`1,Int32)),m(GetAfterUUIDs(List`1,Int32)),m(GetDurringUUIDs(List`1,Int32)),m(GetCurrentAnimDict),m(OnAvatarAnimations(List`1)),m(AnimEvent(UUID,SimEventStatus,Int32)),m(get_groupRoles),m(set_groupRoles(Dictionary`2)),m(SetPosture(SimObjectEvent)),m(GetSequenceNumbers(System.Collections.Generic.IEnumerable`1[[System.Collections.Generic.KeyValuePair`2[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null],[System.Int32, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]], mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]],Int32&,Int32&)),m(StartOrStopAnimEvent(IDictionary`2,IDictionary`2,String,System.Collections.Generic.IList`1[[cogbot.TheOpenSims.SimObjectEvent, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),m(Overlaps(System.Collections.Generic.IEnumerable`1[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null]],System.Collections.Generic.IEnumerable`1[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null]])),m(GetLastEvent(String,Int32)),m(<ThreadJump>b_13),m(get_ConfirmedObject),m(set_ConfirmedObject(Boolean)),m(get_UsePosition),m(get_ZHeading),m(GetHeading),m(CanShoot(SimPosition)),m(AddInfoMap(Object,String)),m(get_propertiesCache),m(set_propertiesCache(ObjectProperties)),m(GetObject(String)),m(get_RegionHandle),m(set_RegionHandle(UInt64)),m(get_iD),m(set_iD(UUID)),m(get_Properties),m(set_Properties(ObjectProperties)),m(GetCubicMeters),m(GetGroupLeader),m(GetTerm),m(get_PathStore),m(TurnToward(SimPosition)),m(IndicateTarget(SimPosition,Boolean)),m(FollowPathTo(SimPosition,Double)),m(TeleportTo(SimPosition)),m(SetObjectPosition(Vector3d)),m(SetObjectPosition(Vector3)),m(TurnToward(Vector3d)),m(get_OuterBox),m(get_LocalID),m(get_ParentID),m(GetInfoMap),m(SetInfoMap(String,MemberInfo,Object)),m(AddInfoMapItem(NamedParam)),m(PollForPrim(WorldObjects,Simulator)),m(get_is_touchDefined),m(get_is_SitDefined),m(get_is_Sculpted),m(get_is_Passable),m(set_is_Passable(Boolean)),m(get_is_Phantom),m(set_is_Phantom(Boolean)),m(get_is_Physical),m(set_is_Physical(Boolean)),m(get_inventoryEmpty),m(get_Sandbox),m(get_temporary),m(get_AnimSource),m(get_AllowInventoryDrop),m(get_is_Avatar),m(Distance(SimPosition)),m(get_Prim),m(get_ObjectType),m(set_ObjectType(SimObjectType)),m(get_needsUpdate),m(get_is_Killed),m(RemoveCollisions),m(IsTypeOf(SimObjectType)),m(get_Children),m(get_HasChildren),m(get_Parent),m(set_Parent(SimObject)),m(AddChild(SimObject)),m(get_is_typed),m(RateIt(BotNeeds)),m(GetTypeUsages),m(GetUsages),m(GetMenu(SimAvatar)),m(IsParentAccruate(Primitive)),m(UpdateOccupied0),m(UpdateOccupied1),m(UpdateOccupied2),m(AddSuperTypes(System.Collections.Generic.IList`1[[cogbot.TheOpenSims.SimObjectType, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),m(SuperTypeString),m(get_is_RegionAttached),m(GetSimScale),m(TryGetGlobalPosition(Vector3d&)),m(UpdatePosition(UInt64,Vector3)),m(TryGetGlobalPosition(Vector3d&,OutputDelegate)),m(TryGetSimPosition(Vector3&)),m(TryGetSimPosition(Vector3&,OutputDelegate)),m(get_SimPosition),m(set_SimPosition(Vector3)),m(GetParentPrim(Primitive,OutputDelegate)),m(GetParentPrim0(Primitive,OutputDelegate)),m(EnsureParentRequested(Simulator)),m(get_ParentGrabber),m(BadLocation(Vector3)),m(GetActualUpdate(String)),m(GetBestUse(BotNeeds)),m(GetProposedUpdate(String)),m(ToGlobal(UInt64,Vector3)),m(HasFlag(Object)),m(Error(String,arrayOf(Object))),m(SortByDistance(System.Collections.Generic.List`1[[cogbot.TheOpenSims.SimObject, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),m(CompareDistance(SimObject,SimObject)),m(CompareDistance(Vector3d,Vector3d)),m(DistanceVectorString(SimPosition)),m(DistanceVectorString(Vector3d)),m(DistanceVectorString(Vector3)),m(get_mesh),m(BottemArea),m(GetGlobalLeftPos(Int32,Double)),m(IsInside(Vector3)),m(get_ActionEventQueue),m(set_ActionEventQueue(Queue`1)),m(get_ShouldEventSource),m(GetSimVerb),m(get_SitName),m(get_touchName),m(get_is_Attachment),m(get_AttachPoint),m(get_is_Attachable),m(get_is_Child),m(set_is_Child(Boolean)),m(OnSound(UUID,Single)),m(OnEffect(String,Object,Object,Single,UUID)),m(get_is_Solid),m(set_is_Solid(Boolean)),m(get_is_Useable),m(set_is_Useable(Boolean)),m(DebugColor),m(get_is_debugging),m(set_is_debugging(Boolean)),m(get_is_meshed),m(set_is_meshed(Boolean)),m(get_item(String)),m(set_item(String,Object)),m(Equals(Object)),m(GetHashCode),m(GetType),m(Finalize),m(MemberwiseClone),c(SimAvatarImpl(UUID,WorldObjects,Simulator)),c(SimAvatarImpl),p(SelectedBeam(Boolean)),p(ProfileProperties(AvatarProperties)),p(AvatarInterests(Interests)),p(AvatarGroups(System.Collections.Generic.List`1[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null]])),p(IsKilled(Boolean)),p(theAvatar(Avatar)),p(SightRange(Double)),p(LastAction(BotAction)),p(CurrentAction(BotAction)),p(IsRoot(Boolean)),p(IsSitting(Boolean)),p(HasPrim(Boolean)),p(IsControllable(Boolean)),p(GlobalPosition(Vector3d)),p(SimRotation(Quaternion)),p(Flying(Boolean)),p(ApproachPosition(SimPosition)),p(ApproachVector3D(Vector3d)),p(KnownTypeUsages(IEnumerable`1)),p(IsDrivingVehical(Boolean)),p(IsWalking(Boolean)),p(IsFlying(Boolean)),p(IsStanding(Boolean)),p(IsSleeping(Boolean)),p(DebugLevel(Int32)),p(GroupRoles(Dictionary`2)),p(ConfirmedObject(Boolean)),p(UsePosition(SimPosition)),p(ZHeading(Single)),p(_propertiesCache(ObjectProperties)),p(RegionHandle(UInt64)),p(ID(UUID)),p(Properties(ObjectProperties)),p(PathStore(SimPathStore)),p(OuterBox(Box3Fill)),p(LocalID(UInt32)),p(ParentID(UInt32)),p(IsTouchDefined(Boolean)),p(IsSitDefined(Boolean)),p(IsSculpted(Boolean)),p(IsPassable(Boolean)),p(IsPhantom(Boolean)),p(IsPhysical(Boolean)),p(InventoryEmpty(Boolean)),p(Sandbox(Boolean)),p(Temporary(Boolean)),p(AnimSource(Boolean)),p(AllowInventoryDrop(Boolean)),p(IsAvatar(Boolean)),p(Prim(Primitive)),p(ObjectType(SimObjectType)),p(NeedsUpdate(Boolean)),p(Children(ListAsSet`1)),p(HasChildren(Boolean)),p(Parent(SimObject)),p(IsTyped(Boolean)),p(IsRegionAttached(Boolean)),p(SimPosition(Vector3)),p(ParentGrabber(TaskQueueHandler)),p(Mesh(SimMesh)),p(ActionEventQueue(Queue`1)),p(ShouldEventSource(Boolean)),p(SitName(String)),p(TouchName(String)),p(IsAttachment(Boolean)),p(AttachPoint(AttachmentPoint)),p(IsAttachable(Boolean)),p(IsChild(Boolean)),p(IsSolid(Boolean)),p(IsUseable(Boolean)),p(IsDebugging(Boolean)),p(IsMeshed(Boolean)),p(Item(Object)),f(BeamInfos(MushDLR223.Utilities.ListAsSet`1[[cogbot.TheOpenSims.EffectBeamInfo, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(SelectedObjects(MushDLR223.Utilities.ListAsSet`1[[PathSystem3D.Navigation.SimPosition, PathSystem3D, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(_debugLevel(Int32)),f(_SelectedBeam(Boolean)),f(_profileProperties(AvatarProperties)),f(_AvatarInterests(Interests)),f(_AvatarGroups(System.Collections.Generic.List`1[[OpenMetaverse.UUID, OpenMetaverseTypes, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(old(SimMoverState)),f(_SightRange(Double)),f(KnownSimObjects(ListAsSet`1)),f(_knownTypeUsages(MushDLR223.Utilities.ListAsSet`1[[cogbot.TheOpenSims.SimTypeUsage, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(_currentAction(BotAction)),f(actionThread(Thread)),f(actionLock(Object)),f(AspectName(String)),f(Client(current_bot)),f(TrackerLoopLock(Object)),f(IsBlocked(Boolean)),f(lastDistance(Double)),f(MoveToMovementProceedure(MovementProceedure)),f(GotoMovementProceedure(MovementProceedure)),f(MovementConsumer(Thread)),f(ApproachDistance(Double)),f(ApproachThread(Thread)),f(ExpectedCurrentAnims(InternalDictionary`2)),f(CurrentAnimSequenceNumber(Int32)),f(PostureType(String)),f(LastPostureEvent(SimObjectEvent)),f(postureLock(Object)),f(IsProfile(Boolean)),f(<LastAction>k_BackingField(BotAction)),f(<ApproachPosition>k_BackingField(SimPosition)),f(<ApproachVector3D>k_BackingField(Vector3d)),f(<GroupRoles>k_BackingField(Dictionary`2)),f(InTurn(Int32)),f(mergeEvents(Boolean)),f(UseTeleportFallback(Boolean)),f(ObjectMovementUpdateValue(ObjectMovementUpdate)),f(_Prim0(Primitive)),f(WorldSystem(WorldObjects)),f(WasKilled(Boolean)),f(_children(ListAsSet`1)),f(scaleOnNeeds(Single)),f(_Parent(SimObject)),f(RequestedParent(Boolean)),f(LastKnownSimPos(Vector3)),f(lastEvent(SimObjectEvent)),f(LastEventByName(System.Collections.Generic.Dictionary`2[[System.String, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[cogbot.TheOpenSims.SimObjectEvent, Cogbot.Library, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null]])),f(HasPrimLock(Object)),f(toStringNeedsUpdate(Boolean))]"
%    
%    
%    6 ?- cli_find_type('ulong&',X),cli_to_str(X,S).
%    X = @'C#40644616',
%    S = "System.UInt64&".
%    
%    6 ?- cli_find_type('ulong',X),cli_to_str(X,S).
%    X = @'C#40644616',
%    S = "System.UInt64".
%    
%    
%    3 ?- cli_find_type('int',X),cli_to_str(X,S).
%    X = @'C#40644880',
%    S = "System.Int32".
%    
%    4 ?- cli_find_class('int',X),cli_to_str(X,S).
%    X = @'C#40644792',
%    S = "int".
%    
%    5 ?- cli_find_class('ulong',X),cli_to_str(X,S).
%    X = @'C#40644704',
%    S = "class cli.System.UInt64".
%    
%    6 ?- cli_find_type('ulong',X),cli_to_str(X,S).
%    X = @'C#40644616',
%    S = "System.UInt64".
%    
%    7 ?- cli_find_type('java.lang.String',X),cli_to_str(X,S).
%    X = @'C#40644608',
%    S = "System.String".
%    
%    8 ?- cli_find_class('java.lang.String',X),cli_to_str(X,S).
%    X = @'C#40643064',
%    S = "class java.lang.String".
%    
%    9 ?- cli_find_type('System.String',X),cli_to_str(X,S).
%    X = @'C#40644608',
%    S = "System.String".
%    
%    10 ?- cli_find_class('System.String',X),cli_to_str(X,S).
%    X = @'C#40643064',
%    S = "class java.lang.String".
%    
%    11 ?- cli_find_class('cli.System.String',X),cli_to_str(X,S).
%    X = @'C#40643064',
%    S = "class java.lang.String".
%    
%    cli_find_class('Dictionary'('int','string'),X)
%    
%    8 ?- cli_get('System.UInt64','MaxValue',X),cli_to_str(X,S).
%    X = @'C#33826112',
%    S = "18446744073709551615".
%    
%    cli_to_str(18446744073709551615,S).
%    cli_to_str(18446744073709551616,S).
%    cli_to_str(18446744073709551617,S).
%    
%    1 ?- cli_get_type(X,Y).
%    Y = @'C#8398216'.
%    
%    2 ?- cli_get_type(X,Y),cli_to_str(Y,W).
%    Y = @'C#8398216',
%    W = "SbsSW.SwiPlCs.PlTerm".
%    
%    3 ?- cli_get_type(1,Y),cli_to_str(Y,W).
%    Y = @'C#8398208',
%    W = "System.Int32".
%    
%    4 ?- cli_get_type(1.1,Y),cli_to_str(Y,W).
%    Y = @'C#8398200',
%    W = "System.Double".
%    
%    5 ?- cli_get_type(1.1,Y),cli_to_str(Y,W).
%    Y = @'C#8398200',
%    W = "System.Double".
%    
%    6 ?- cli_getClass(1.1,Y),cli_to_str(Y,W).
%    Y = @'C#8398192',
%    W = "class cli.System.Double".
%    
%    7 ?- cli_getClass(f,Y),cli_to_str(Y,W).
%    Y = @'C#8398184',
%    W = "class java.lang.String".
%    
%    8 ?- cli_getClass('f',Y),cli_to_str(Y,W).
%    Y = @'C#8398184',
%    W = "class java.lang.String".
%    
%    
%    9 ?- cli_get_type(1,Y),cli_to_str(Y,W).
%    Y = @'C#8398208',
%    W = "System.Int32".
%    
%    
%    9 ?- cli_get_type(c(a),Y),cli_to_str(Y,W).
%    
%    cli_get_type('ABuildStartup.Program',Y),cli_to_str(Y,W).
%    
%    cli_load_assembly('Cogbot.exe'),cli_call('ABuildStartup.Program','Main',[],Y),cli_to_str(Y,W).
%    cli_find_type('ABuildStartup.Program',Y),cli_to_str(Y,W).
%    
%    



end_of_file.


