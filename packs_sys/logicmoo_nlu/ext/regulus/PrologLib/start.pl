
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(start,
	  [start_processes/2,
	   test_start/1]
	 ).

%------------------------------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(process)).
:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(xml)).
:- use_module(library(file_systems)).

/*

   start_processes(+XMLFile, +MachineID).

Tool for starting multiple processes, each of which may need to wait for
trace input from earlier processes. Execution is controlled by an XML config file.

One call is made on each machine. The different instantiations of the process
communicate by means of touchfiles, which must be placed in an area
accessible to all the machines.

Example of call:

start_processes('$REGULUS/tmp/test_start.xml', machine1).

Example of XML config file:

<?xml version="1.0"?>
<start>

<!-- Where to put things -->
<declarations>
   <touchfiledir>$REGULUS/tmp/starttouchfiles</touchfiledir>             <!-- Tmp directory for touchfiles and scripts -->
   <tracefiledir>$REGULUS/tmp/starttracefiles</tracefiledir>             <!-- Tmp directory for tracefiles -->
   <webpage>$REGULUS/tmp/startstatus.html</webpage>                      <!-- Generated eb page with pointers to tracefiles -->
</declarations>

<!-- Process1 -->
<process> 
   <id>process1</id>                                                     <!-- Process ID -->
   <machine>machine1</machine>                                           <!-- ID for machine this runs on -->   
   <command>sicstus -l $REGULUS/PrologLib/test_start_a.pl</command>      <!-- Command to run -->   
   <tracefile>process1.txt</tracefile>                                   <!-- Where to write output -->   
</process> 

<!-- Process2, waits for Process1 to reach "a: 5" -->
<process> 
   <id>process2</id>                                                     <!-- Process ID -->
   <machine>machine1</machine>                                           <!-- ID for machine this runs on -->   
   <command>sicstus -l $REGULUS/PrologLib/test_start_b.pl</command>      <!-- Command to run -->   
   <condition process="process1">a: 5</condition>                        <!-- Search for string "a: 5" in trace of "process1" -->   
   <tracefile>process2.txt</tracefile>                                   <!-- Where to write output -->   
</process> 

</start>

*/

%------------------------------------------------------------------------------------

test_start(1) :-
	start_processes('$REGULUS/tmp/test_start.xml', machine1).

%----------------------------------------------------

start_processes(ConfigFile, MachineId) :-
	read_config_file(ConfigFile, Config),
	init_start_processes(Config, MachineId, Plan),
	execute_start_processes(Plan, ProcessIds),
	watcher_loop(ProcessIds).

%----------------------------------------------------

/*
% Internalize the XML file

Typical input:

<?xml version="1.0"?>
<start>

<!-- Where to put things -->
<declarations>
   <touchfiledir>$REGULUS/tmp/starttouchfiles</touchfiledir>
   <tracefiledir>$REGULUS/tmp/starttracefiles</tracefiledir>
   <webpage>$REGULUS/tmp/startstatus.html</webpage> 
</declarations>

<!-- Process1 -->
<process> 
   <id>process1</id>
   <machine>machine1</machine>
   <command>sicstus -l $REGULUS/PrologLib/test_start_a.pl</command>
   <tracefile>process1.txt</tracefile>
</process> 

<!-- Process2, waits for Process1 to reach "a: 5" -->
<process> 
   <id>process2</id>
   <machine>machine1</machine>
   <command>sicstus -l $REGULUS/PrologLib/test_start_b.pl</command>
   <condition process="process1">a: 5</condition>
   <tracefile>process2.txt</tracefile>
</process> 

</start>

Output:

[declarations([(touchfiledir='$REGULUS/tmp/starttouchfiles'), 
               (tracefiledir='$REGULUS/tmp/starttracefiles'), 
               (webpage='$REGULUS/tmp/startstatus.html')]),
 process([(id=process1), (machine=machine1), 
          (command='sicstus -l $REGULUS/PrologLib/test_start_a.pl'), 
          (tracefile='process1.txt')]),
 process([(id=process2), (machine=machine1), 
          (command='sicstus -l $REGULUS/PrologLib/test_start_b.pl'), 
          (condition=[process=process1,string='a: 5']), 
          (tracefile='process2.txt')])]

*/

read_config_file(File, Config) :-
	safe_absolute_file_name(File, AbsFile),
	read_file_to_string(File, String),
	(   xml_parse(String, Config0) ->
	    replace_strings_with_atoms_in_xml(Config0, Config1),
	    remove_comments_in_xml(Config1, Config2),
	    read_config_file1(Config2, Config),
	    format('~N--- Read config file: ~w~n', [AbsFile])
	;
	    otherwise ->
	    format('~N*** Error: unable to parse XML~n', [])
	),
	!.
read_config_file(File, _Config) :-
	format('~N*** Error: unable to read config file: ~w~n', [File]),
	fail.

read_config_file1(XML, Config) :-
	(   XML =  xml(_Version, [element(start, _, Body)]) ->
	    read_config_file2(Body, Config)
	;
	    otherwise ->
	    format('~N*** Error: outermost tag in file must be <start>~n', []),
	    fail
	).

read_config_file2([], []).
read_config_file2([F | R], [F1 | R1]) :-
	read_config_file_element(F, F1),
	!,
	read_config_file2(R, R1).

read_config_file_element(element(declarations, _, Body), declarations(Declarations)) :-
	!,
	read_declarations(Body, Declarations).
read_config_file_element(element(process, _, Body), process(Process)) :-
	!,
	read_process(Body, Process).
read_config_file_element(element(Tag, _, _), _) :-
	format('~N*** Error: unknown tag <~w>~n', [Tag]),
	fail.

read_declarations(Body, Declarations) :-
	read_declarations1(Body, Declarations).

read_declarations1([], []).
read_declarations1([F | R], [F1 | R1]) :-
	read_declaration_element(F, F1),
	!,
	read_declarations1(R, R1).

read_declaration_element(element(Tag, _, Body), Tag=Value) :-
	(   (  member(Tag, [touchfiledir, tracefiledir, webpage]), Body = [Value] ) ->
	    true
	;
	    otherwise ->
	    format('~N*** Error: bad tag <~w> in declarations~n', [Tag]),
	    fail
	).

read_process(Body, Process) :-
	read_process1(Body, Process).

read_process1([], []).
read_process1([F | R], [F1 | R1]) :-
	read_process_element(F, F1),
	!,
	read_process1(R, R1).

read_process_element(element(Tag, Attr, Body), Result) :-
	(   (  member(Tag, [id, machine, command, tracefile]), Body = [Value], Attr = [] ) ->
	    Result = ( Tag=Value )
	;
	    (  Tag = condition, Body = [String], Attr = [process = Process] ) ->
	    Result = ( condition = [ process = Process, string = String ] )
	;
	    otherwise ->
	    format('~N*** Error: bad tag <~w> in process~n', [Tag]),
	    fail
	).

%----------------------------------------------------

/*
Set things up: initialize the directories to use, create the plan, the Bash scripts and web page.

Typical input:

[declarations([(touchfiledir='$REGULUS/tmp/starttouchfiles'), 
               (tracefiledir='$REGULUS/tmp/starttracefiles'), 
               (webpage='$REGULUS/tmp/startstatus.html')]),
 process([(id=process1), (machine=machine1), 
          (command='sicstus -l $REGULUS/PrologLib/test_start_a.pl'), 
          (tracefile='process1.txt')]),
 process([(id=process2), (machine=machine1), 
          (command='sicstus -l $REGULUS/PrologLib/test_start_b.pl'), 
          (condition=[process=process1,string='a: 5']), 
          (tracefile='process2.txt')])]

Typical output plan:

[process([(id=process1),
	  (machine=machine1), 
          (command='sicstus -l $REGULUS/PrologLib/test_start_a.pl'), 
          (tracefile = 'c:/cygwin64/home/speech/regulus-code/trunk/regulus/tmp/starttracefiles/process1.txt'),
          (string_conditions = [('a: 5'-'$REGULUS/tmp/starttouchfiles/process1_found_a__5.txt')]),
          (script = 'c:/cygwin64/home/speech/regulus-code/trunk/regulus/tmp/starttouchfiles/run_process1')]),
 process([(id=process2),
	  (machine=machine1), 
          (command='sicstus -l $REGULUS/PrologLib/test_start_b.pl'), 
          (touchfile_condition = '$REGULUS/tmp/starttouchfiles/process1_found_a__5.txt'),
          (tracefile = 'c:/cygwin64/home/speech/regulus-code/trunk/regulus/tmp/starttracefiles/process2.txt'),
          (string_conditions=[]), 
          (script = 'c:/cygwin64/home/speech/regulus-code/trunk/regulus/tmp/starttouchfiles/run_process2')])]

Typical Bash file:

#!/bin/bash
sicstus -l $REGULUS/PrologLib/test_start_a.pl |& sicstus -l $REGULUS/PrologLib/search_for_patterns.pl --goal "search_for_patterns(['a: 5'-'$REGULUS/tmp/starttouchfiles/process1_found_a__5.txt'])." > c:/cygwin64/home/speech/regulus-code/trunk/regulus/tmp/starttracefiles/process1.txt

Typical web page:

<html>
<body>

<p><a href="file:///c:/cygwin64/home/speech/regulus-code/trunk/regulus/tmp/starttracefiles/process1.txt">Trace for process1</a></p>

<p><a href="file:///c:/cygwin64/home/speech/regulus-code/trunk/regulus/tmp/starttracefiles/process2.txt">Trace for process2</a></p>

</body>
</html>
*/

init_start_processes(Config, MachineId, Plan1) :-
	format('~N--- Initialising plan~n', []),
	get_declarations_from_config(Config, Declarations, Config1),
	init_directories(Declarations),
	config_to_plan(Config1, MachineId, Declarations, Plan),
	create_bash_scripts(Plan, Declarations, Plan1),
	create_web_page(Plan1, Declarations),
	init_touchfiles_and_tracefiles(Plan1),
	format('~N--- Plan initialized~n', []),
	!.
init_start_processes(_Config, _MachineId, _Plan) :-
	format('~N*** Error: unable to initialise start script~n', []),
	fail.

get_declarations_from_config(Config, Declarations, Config1) :-
	get_declarations_from_config1(Config, DeclarationsList, Config1),
	(   DeclarationsList = [Declarations] ->
	    check_declarations(Declarations)
	;
	    DeclarationsList = [] ->
	    format('~N*** Error: no <declarations> tag in start script~n', []),
	    fail
	;
	    otherwise ->
	    format('~N*** Error: multiple <declarations> tags in start script~n', []),
	    fail
	).

get_declarations_from_config1([], [], []).
get_declarations_from_config1([declarations(List) | R], [declarations(List) | R1], R2) :-
	!,
	get_declarations_from_config1(R, R1, R2).
get_declarations_from_config1([Other | R], R1, [Other | R2]) :-
	!,
	get_declarations_from_config1(R, R1, R2).

check_declarations(declarations(List)) :-
	complain_if_missing_tag(touchfiledir, List),
	complain_if_missing_tag(tracefiledir, List),
	complain_if_missing_tag(webpage, List),
	!.
check_declarations(_Other) :-
	format('~N*** Error: bad <declarations> tag in start script~n', []),
	fail.

% Create any directories that may be needed 

init_directories(declarations(List)) :-
	member(touchfiledir=Touchfiledir, List),
	member(tracefiledir=Tracefiledir, List),
	create_directories_as_required([Touchfiledir, Tracefiledir]),
	!.
init_directories(_Declarations) :-
	format('~N*** Error: unable to initialise directories~n', []),
	fail.

create_directories_as_required([]).
create_directories_as_required([F | R]) :-
	create_directory_if_necessary(F),
	create_directory_above_if_necessary(F),
	!,
	create_directories_as_required(R).

% Reformulate conditions in terms of creating and waiting for touchfiles.

config_to_plan(Processes, MachineId, Declarations, Plan) :-
	config_to_plan1(Processes, Declarations, Processes1, []-Conditions),
	config_to_plan2(Processes1, MachineId, Declarations, Conditions, Plan),
	!.
config_to_plan(_Processes, _MachineId, _Declarations, _Plan) :-
	format('~N*** Error: unable to convert to plan~n', []),
	fail.

config_to_plan1([], _Declarations, [], CondsIn-CondsIn).
config_to_plan1([F | R], Declarations, [F1 | R1], CondsIn-CondsOut) :-
	config_item_to_plan1(F, Declarations, F1, CondsIn-CondsNext),
	!,
	config_to_plan1(R, Declarations, R1, CondsNext-CondsOut).

config_item_to_plan1(process(List), Declarations, process(List1), CondsIn-CondsOut) :-
	complain_if_missing_tag(id, List),
	complain_if_missing_tag(machine, List),
	complain_if_missing_tag(command, List),
	complain_if_missing_tag(tracefile, List),
	config_item_to_plan1_sub(List, Declarations, List1, CondsIn-CondsOut).

config_item_to_plan1_sub([], _Declarations, [], CondsIn-CondsIn).
config_item_to_plan1_sub([F | R], Declarations, [F1 | R1], CondsIn-CondsOut) :-
	config_item_to_plan1_sub1(F, Declarations, F1, CondsIn-CondsNext),
	config_item_to_plan1_sub(R, Declarations, R1, CondsNext-CondsOut).

config_item_to_plan1_sub1(condition=[process=PreId, string=Str], Declarations, F1, CondsIn-[Cond | CondsIn]) :-
	get_touchfile_name(PreId, Str, Declarations, Touchfile),
	F1 = ( touchfile_condition = Touchfile ),
	Cond = condition([process=PreId, string=Str, touchfile=Touchfile]),
	!.
config_item_to_plan1_sub1(tracefile=File, Declarations, tracefile=AbsFullFile, CondsIn-CondsIn) :-
	get_tracefile_dir(Declarations, Dir),
	format_to_atom('~w/~w', [Dir, File], FullFile),
	safe_absolute_file_name(FullFile, AbsFullFile),
	!.
config_item_to_plan1_sub1(Other, _Declarations, Other, CondsIn-CondsIn).

get_touchfile_name(ProcessId, Str, Declarations, Touchfile) :-
	get_touchfile_dir(Declarations, Dir),
	clean_atom_for_use_in_filename(Str, Str1),
	format_to_atom('~w/~w_found_~w.txt', [Dir, ProcessId, Str1], Touchfile),
	!.

config_to_plan2([], _MachineId, _Declarations, _Conditions, []).
config_to_plan2([F | R], MachineId, Declarations, Conditions, [F1 | R1]) :-
	config_item_to_plan2(F, MachineId, Declarations, Conditions, F1),
	!,
	config_to_plan2(R, MachineId, Declarations, Conditions, R1).
config_to_plan2([_F | R], MachineId, Declarations, Conditions, R1) :-
	!,
	config_to_plan2(R, MachineId, Declarations, Conditions, R1).

config_item_to_plan2(process(List), MachineId, _Declarations, _Conditions, _Output) :-
	\+ member(machine=MachineId, List),
	!,
	fail.
config_item_to_plan2(process(List), _MachineId, _Declarations, Conditions, process(List1)) :-
	member(id=ProcessId, List),
	findall(StrAtom-Touchfile,
		(   member(condition(CondList), Conditions),
		    member(process=ProcessId, CondList),
		    member(string=StrAtom, CondList),
		    member(touchfile=Touchfile, CondList)
		    %atom_codes(StrAtom, Str)
		),
		StringConditions),
	append(List, [string_conditions=StringConditions], List1),
	!.
config_item_to_plan2(F, MachineId, Declarations, Conditions, F1) :-
	format('~N*** Error: bad call: ~w~n',
	       [config_item_to_plan2(F, MachineId, Declarations, Conditions, F1)]),
	fail.

% Create a Bash script for each process.

create_bash_scripts(Plan, Declarations, Plan1) :-
	create_bash_scripts1(Plan, Declarations, Plan1),
	format('~N--- Created bash scripts~n', []).

create_bash_scripts1([], _Declarations, []).
create_bash_scripts1([F | R], Declarations, [F1 | R1]) :-
	create_bash_script_for_plan_element(F, Declarations, F1),
	!,
	create_bash_scripts1(R, Declarations, R1).

create_bash_script_for_plan_element(process(List), Declarations, process(List1)) :-
	member(id=ProcessId, List),
	member(command=Command, List),
	member(tracefile=Tracefile, List),
	member(string_conditions=Alist, List),
	get_scriptfile_name(ProcessId, Declarations, ScriptFile),
	open(ScriptFile, write, S),
	format(S, '#!/bin/bash~n', []),
	format(S,
	       '~w |& sicstus -l $REGULUS/PrologLib/search_for_patterns.pl --goal "~q." > ~w',
	       [Command, search_for_patterns(Alist), Tracefile]),
	close(S),
	format('~NWritten script file: ~w~n', [ScriptFile]),
	append(List, [script=ScriptFile], List1),
	!.
create_bash_script_for_plan_element(F, Declarations, F1) :-
	format('~N*** Error: bad call: ~w~n',
	       [create_bash_script_for_plan_element(F, Declarations, F1)]),
	fail.

get_scriptfile_name(ProcessId, Declarations, AbsScriptFile) :-
	get_touchfile_dir(Declarations, Dir),
	format_to_atom('~w/run_~w', [Dir, ProcessId], ScriptFile),
	safe_absolute_file_name(ScriptFile, AbsScriptFile),
	!.
get_scriptfile_name(ProcessId, Declarations, AbsScriptFile) :-
	format('~N*** Error: bad call: ~w~n',
	       [get_scriptfile_name(ProcessId, Declarations, AbsScriptFile)]),
	fail.

% Create a web page that we can use to track output

create_web_page(Plan, Declarations) :-
	get_web_page(Declarations, File),
	safe_absolute_file_name(File, AbsFile),
	open(AbsFile, write, S),
	write_web_page_intro(S),
	write_web_page_body(Plan, S),
	write_web_page_coda(S),
	close(S),
	format('~N--- Written out web page ~w~n', [AbsFile]),
	!.
create_web_page(Plan, Declarations) :-
	format('~N*** Error: bad call: ~w~n',
	       [create_web_page(Plan, Declarations)]),
	fail.

write_web_page_intro(S) :-
	format(S, '~N<html>~n', []),
	format(S, '~N<body>~n~n', []),
	!.

write_web_page_body([], _S).
write_web_page_body([F | R], S) :-
	write_web_page_body_element(F, S),
	!,
	write_web_page_body(R, S).

write_web_page_body_element(process(List), S) :-
	member(tracefile=File, List),
	member(id=Id, List),
	safe_absolute_file_name(File, AbsFile),
	format(S, '~N<p><a href="file:///~w">Trace for ~w</a></p>~n~n', [AbsFile, Id]),
	!.

write_web_page_coda(S) :-
	format(S, '~N</body>~n', []),
	format(S, '~N</html>~n', []),
	!.

% Delete old touchfiles, overwrite tracefiles with null content

init_touchfiles_and_tracefiles(Plan) :-
	init_touchfiles_and_tracefiles1(Plan),
	format('~N--- Removed old touchfiles~n', []).

init_touchfiles_and_tracefiles1([]).
init_touchfiles_and_tracefiles1([F | R]) :-
	init_touchfiles_and_tracefiles_for_plan_element(F),
	!,
	init_touchfiles_and_tracefiles1(R).

init_touchfiles_and_tracefiles_for_plan_element(process(List)) :-
	init_touchfile_for_plan_element(List),
	init_tracefile_for_plan_element(List),
	!.
init_touchfiles_and_tracefiles_for_plan_element(_Other).

init_touchfile_for_plan_element(List) :-
	member(touchfile_condition=Touchfile, List),
	safe_absolute_file_name(Touchfile, AbsTouchfile),
	safe_file_exists(AbsTouchfile),
	delete_file(AbsTouchfile),
	!.
init_touchfile_for_plan_element(_List).

init_tracefile_for_plan_element(List) :-
	member(tracefile=Tracefile, List),
	write_atom_list_to_file(['Waiting for trace output'], Tracefile),
	!.
init_tracefile_for_plan_element(_List).

%====================================================

% Run-time 

execute_start_processes(Plan, ProcessIds) :-
	execute_start_processes1(Plan, ProcessIds),
	format('~N--- All processes started~n', []),
	!.
execute_start_processes(Plan, _ProcessIds) :-
	format('~N*** Error: bad attempt to run plan: ~w~n', []),
	prettyprint(Plan),
	fail.

execute_start_processes1([], []).
execute_start_processes1([F | R], [F1 | R1]) :-
	execute_start_process_element(F, F1),
	!,
	execute_start_processes1(R, R1).

execute_start_process_element(process(List), ProcessId) :-
	member(id=Id, List),
	member(script=Script, List),
	format('~N--- Process: ~w~n', [Id]),
	wait_for_touchfile_conditions(List),
	absolute_file_name(path('bash.exe'), Bash),
	process_create(Bash, [file(Script)], [process(ProcessId)]),
	format('~N--- Process started~n', []),
	!.

wait_for_touchfile_conditions([]).
wait_for_touchfile_conditions([F | R]) :-
	wait_for_touchfile_condition(F),
	!,
	wait_for_touchfile_conditions(R).

wait_for_touchfile_condition(touchfile_condition=Touchfile) :-
	keep_checking_for_touchfile(Touchfile),
	!.
wait_for_touchfile_condition(_Other).

keep_checking_for_touchfile(File) :-
	safe_absolute_file_name(File, AbsFile),
	format('~NWaiting for file ~w~n', [AbsFile]),
	keep_checking_for_touchfile1(AbsFile).

keep_checking_for_touchfile1(File) :-
	(   safe_file_exists(File) ->
	    format('~NFound file ~w~n', [File])
	;
	    otherwise ->
	    %format('~NUnable to find file ~w, waiting...~n', [File]),
	    sleep(1),
	    !,
	    keep_checking_for_touchfile1(File)
	).

%----------------------------------------------------

% This will later contain more commands.

watcher_loop(ProcessIds) :-
	format('~N==========================================================~n~n', []),
	show_process_ids(ProcessIds),
	get_loop_prompt(PromptAtom),
	format(PromptAtom, []),
	read_line(Line),
	split_string_into_words(Line, Words0),
	lowercase_atom_list(Words0, Words),
	(   member(Words, [[kill]]) ->
	    handle_top_loop_command(Line, Words, ProcessIds)
	;
	    otherwise ->
	    handle_top_loop_command(Line, Words),
	    !,
	    watcher_loop(ProcessIds)
	).

show_process_ids(ProcessIds) :-
	findall(PID,
		(   member(ProcessId, ProcessIds),
		    process_id(ProcessId, PID)
		),
		PIDs),
	format('~NPIDS: ~w~n~n', [PIDs]).

get_loop_prompt(PromptAtom) :-
	PromptAtom = '~NType "kill" to kill all processes >> ',
	!.

handle_top_loop_command(Line, Words, ProcessIds) :-
	(   member(Words, [[kill]]) ->
	    kill_process_list(ProcessIds)
	;
	    otherwise ->
	    format('~NUnknown command: "~s"~n', [Line])
	).	

kill_process_list([]).
kill_process_list([F | R]) :-
	try_to_kill_process(F),
	!,
	kill_process_list(R).

%try_to_kill_process(Process) :-
%	process_kill(Process),
%	!.
try_to_kill_process(Process) :-
	process_id(Process, PID),
	%system_on_list(['kill -f', PID]),
	%system_on_list(['kill -f -t', PID]),
	system_on_list(['taskkill -f -t -pid', PID]),
	!.
try_to_kill_process(Process) :-
	format('~N*** Error: bad call: ~w~n', [try_to_kill_process(Process)]),
	fail.

%====================================================

% Utilities

get_touchfile_dir(declarations(List), Dir) :-
	member(touchfiledir=Dir, List),
	!.
get_touchfile_dir(Other, Dir) :-
	format('~N*** Error: bad call: ~w~n',
	       [get_touchfile_dir(Other, Dir)]),
	fail.

get_tracefile_dir(declarations(List), Dir) :-
	member(tracefiledir=Dir, List),
	!.
get_tracefile_dir(Other, Dir) :-
	format('~N*** Error: bad call: ~w~n',
	       [get_tracefile_dir(Other, Dir)]),
	fail.

get_web_page(declarations(List), File) :-
	member(webpage=File, List),
	!.
get_web_page(Other, File) :-
	format('~N*** Error: bad call: ~w~n',
	       [get_web_page(Other, File)]),
	fail.

complain_if_missing_tag(Tag, List) :-
	(   member(Tag=_, List) ->
	    true
	;
	    otherwise ->
	    format('~N*** Error: tag ~w not found~n', [Tag]),
	    fail
	).

clean_atom_for_use_in_filename(Atom, Atom1) :-
	atomic(Atom),
	atom_codes(Atom, Str),
	clean_string_for_use_in_filename(Str, Str1),
	atom_codes(Atom1, Str1),
	!.
clean_atom_for_use_in_filename(Atom, Atom1) :-
	format('~N*** Error: bad call: ~w~n', [clean_atom_for_use_in_filename(Atom, Atom1)]),
	fail.

clean_string_for_use_in_filename([], []).
clean_string_for_use_in_filename([F | R], [F1 | R1]) :-
	(   ok_char_for_filename(F) ->
	    F1 = F
	;
	    otherwise ->
	    F1 = 0'_
	),
	clean_string_for_use_in_filename(R, R1).

ok_char_for_filename(Char) :-
	lowercase_char(Char),
	!.
ok_char_for_filename(Char) :-
	uppercase_char(Char),
	!.
ok_char_for_filename(Char) :-
	digit_char(Char),
	!.
ok_char_for_filename(Char) :-
	member(Char, "_-"),
	!.
