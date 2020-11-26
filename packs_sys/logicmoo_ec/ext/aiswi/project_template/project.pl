:- dynamic ics_file/1, sokb_file/1, history_file/1, required_option/2.

history_file('trace.txt').
ics_file('regole.txt').

sokb_file('kb.pl').




%%%%%%%%%%%%%%%%%%%%%%% Constant Part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_prj(Path):-
    findall(F,ics_file(F),ICS_files), append_path(Path,ICS_files,IcsPathFiles),
    translate_ics_files(IcsPathFiles,'./ics.pl'),
    findall(F,history_file(F),Hist_files),  append_path(Path,Hist_files,HistPathFiles),
    translate_histories(HistPathFiles,'./history.pl'),
    findall(F,sokb_file(F),Sokb_files),     append_path(Path,Sokb_files,SokbPathFiles),
    convert_sokb(SokbPathFiles,'./sokb.pl'),
    compile(sokb), compile(history), compile(ics),
    findall([O,V],required_option(O,V),LOptions),
    set_options(LOptions).

% Default:
run(_):- run.
run_open(_):- run_no_close.
run_closed(_):- run.
