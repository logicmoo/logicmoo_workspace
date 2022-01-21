%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% shells.pl
%%% Useful routines for interacting with unix via prolog.

:- ensure_loaded(library(aux)).


% SHELL(+Tree)
% Extremely useful command for interfacing with unix system.
% Tree is a list of lists.
% Flattens this tree into a list, treats each element of this list
% (each atom) as a word in the command, which is then sent to unix.  
%
shell(Tree) :-
	command_from_args(Tree,Command),
	unix(shell(Command)).

% SHELL(+Tree,-Value)
% Calls the command, and reads in the first term output
% by that command  as Value.
shell(Command,Value) :- 
	shell(Command,'/tmp/shelltmp',Value).

% SHELL(+Tree,+TmpFile,-Value)
% Calls the command, outputs the result to a temp file TmpFile,
% and reads in the first term from that file as Value,
% then deletes TmpFile.
% It would be much faster if we could use environmnet variables
% to pass data back, but neither Quintus nor Sicstus let us
% change the variables of the shell in use.  
shell(Command,TmpFile,Value) :- 
	shell([Command,'> ',TmpFile,' ; echo '' . '' >> ',TmpFile]),
	see(TmpFile), read(Value), seen,
	shell([rm,TmpFile]).

% SHELL_OUT(+Tree,+File)
% Calls the command, outputs the result to File.
shell_out(Command,File) :- 
	shell([Command,'> ',File]).

% WRITEP(+Command,+File)
% Echos the command to File.
% Works with pipes. 
writep(Command,File) :- 
	shell_out([echo,Command],File).


command_from_args(Tree,Command) :-
	command_from_args(Tree,' ',Command).

command_from_args(Tree,Space,Command) :-
	flatten(Tree,List),
	interleave_list(List,Space,SpacedList),
	concat_list(SpacedList,Command).


interleave_list([],_,[]) :- !.
interleave_list([H],_,[H]) :- !.
interleave_list([H|T],Sym,[H,Sym|TT]) :- interleave_list(T,Sym,TT).


spacify_list([],[]).
spacify_list([H|T],[H,' '|TT]) :- spacify_list(T,TT).




% UNLOADED_HOST(Addr)
% Calls a special command to find the name of an unloaded host
% to use.  If your system does not have such a program,
% you must either write one or give specific host
% names. 
unloaded_host(Addr) :-
	shell([rsh,ely,'/usr/etc/resman',dbank],Addr).
	

% SHELL_RSH(+Program,+Args),
% Runs Program with its Args as a shell on the current host.
shell_rsh(Program,Args) :-
	current_host(Addr),
	shell_rsh(Addr,Program,Args).


% SHELL_RSH(+Addr,+Program,+Args)
% Runs Program with its Args as a remote shell on host Addr,
% defaulting the title as the name of the program.
%
% If Addr=any, finds the least used host.

shell_rsh(any,Program,Args) :- !,
	unloaded_host(Addr),
	shell_rsh(Addr,Program,Args).
shell_rsh(Addr,Program,Args) :-
	shell_rsh(Addr,Program,Args,Addr).


% SHELL_RSH(+Addr,+Program,+Args,+Title)
% Runs Program with its Args as a remote shell on host Addr.
% As we use XRSH, we give the window a Title.
%
shell_rsh(any,Program,Args,Title) :- !,
	unloaded_host(Addr),
	shell_rsh(Addr,Program,Args,Title).
shell_rsh(Addr,Program,Args,Title) :-
	shell([ '$HOME/Bin/xrsh',
			    Addr,
			    'xterm ',
			    '-sb',
			    '-name',
			    Title,
			    '-title',
			    Title,
			    '-e',
			    Program,
			    Args, '&'
			  ]).




% START_SICSTUS_SHELL(+Host,+ArgsList)
% Runs a sicstus shell on Host (could be ANY), 
% calling it with its list of args.

start_sicstus_shell(Host,Args) :-
	shell_rsh(Host,sicstus,Args).

start_sicstus_shell(Args) :-
	shell_rsh(sicstus,Args).




% FIND_ARCHITECTURE(-Arch)
% enables the command:  current_architecture(-Arch),
% like current_host(-Host).

find_architecture(Arch) :- 
	shell(arch,Arch),
	assert(found_current_architecture(Arch)).

current_architecture(Arch) :-
	get_current_architecture(Arch1),
	Arch=Arch1.

get_current_architecture(Arch) :-
	environment_variable('$ARCH',Arch), !.
get_current_architecture(Arch) :-
	current_predicate(found_current_architecture,_) 
	-> found_current_architecture(Arch)
        ;  find_architecture(Arch).

	
bin_directory(D) :-
	current_architecture(Arch),
	concat_list(['~/prolog/bin/',Arch,'/'],D).


% Arch is a placeholder, to be inserted in Path, to make Name.
% arch_path_name(Arch,['prolog/',Arch,'/newrandoms'],Dir).
% Arch = sun4,
% Name = 'prolog/sun4/newrandoms'
%
arch_path_name(Arch,Path,Name) :-
	current_architecture(Arch),
	concat_list(Path,Name).
	

add_bin_library :-
	bin_directory(D),
	assert(library_directory(D)).

current_directory(X) :- absolute_file_name(., X).

file_exists(X) :- unix(access(X,0)).


% ENVIRONMENT_VARIABLE(+Name,?Value) 
% Name should be a unix environment variable, like: '$ARCH'.
% Value will be its value, like:  sun4.
environment_variable(Name,Value) :- 
	shell([echo,Name],Value).


% WITH_OUTPUT_FILE(+File,+Mode,+Goal)
% Opens File for writing in Mode (write or append),
% executes Goal with this output stream current,
% then closes the stream and reverts to previous output. 
with_output_file(File,Mode,Goal) :- 
	switch_output_to_file(File,Mode,Old,New),
	call(Goal),
	close(New),
	set_output(Old).
	
% WITH_INPUT_FILE(+File,+Goal)
% Opens File for reading, 
% executes Goal with this input stream current,
% then closes the stream and reverts to previous input. 
with_input_file(File,Goal) :- 
	switch_input_to_file(File,Old,New),
	call(Goal),
	close(New),
	set_input(Old).
	

% SWITCH_OUTPUT_TO_FILE(+File,+Mode,-Old,-New)
% Opens File for output in MODE (write or append).
% NEW is returned as the new stream, and becomes the current output. 
% OLD is the previous output stream. 
switch_output_to_file(File,Mode,Old,New) :-
	current_output(Old),
	open(File,Mode,New),
	set_output(New).

% SWITCH_INPUT_TO_FILE(+File,-Old,-New)
% Opens File for input, with NEW as the stream,
% and sets it to current input.  OLD is the previous input
% stream. 
switch_input_to_file(File,Old,New) :-
	current_input(Old),
	open(File,read,New),
	set_input(New).


% WITH_TEMP_OUTFILE(+Template,-File,+Goal)
% Gensyms as File a new filename in /tmp/, based on Template.
% Then calls Goal, which should create File.
% At the end, this deletes File 
% unless 'trace savetables' is on. 
%
% mktemp only really works in sicstus prolog. 
% The file quintus-version defines it for quintus to 
% just use that filename, while sicstus generates a 
% guaranteed unique version of that template. 
with_temp_file(Template,File,Goal) :- 
	gensym(Template,P), 
	concat_list(['/tmp/',P,'XXXXXX'],FullTemplate),
        mktemp(FullTemplate,File), 
	call(Goal),
	( tracing(savetables) -> 
	  true 
	; shell([rm,File])
	).

%mktemp(File,File).



% Some abbreviations
cd :- unix(cd).
cd(X) :- unix(cd(X)).
ls :- unix(shell(ls)).
lsa :- unix(shell('ls -Al')).
pwd :- unix(shell('pwd')).
