:- use_module(library(logtalk)).

:- dynamic file_search_path/2.
:- multifile file_search_path/2.

:- prolog_load_context(directory,Dir),
   asserta(user:file_search_path(pong_dir,Dir)).


load_pong :- 
  user:file_search_path(pong_dir,Dir),
  working_directory(CWD,CWD),
  cd(Dir/'PongTalk'),
  logtalk_load(loader),
  cd(CWD),!.

play_pong :- game::play.
% play:- '$game#0.play#0'(c(user, user, r(user, _20890, [], []))).
