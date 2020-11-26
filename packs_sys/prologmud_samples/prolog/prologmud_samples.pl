:- module(prologmud_sample,[]).
:- consult(prologmud_sample_games/run_mud_server).
:- current_prolog_flag(os_argv,[Was]),
  (app_argv('--noworld')->true;
    set_prolog_flag(os_argv,[Was,'--world','--telnet'])).
:- user:ensure_loaded(prologmud_sample_games/run_mud_server).
:- baseKB:lar-> true ; wdmsg("To begin again type ?- baseKB:lar. ").

