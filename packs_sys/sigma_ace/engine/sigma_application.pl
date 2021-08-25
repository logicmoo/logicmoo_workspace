:-asserta(user:prolog_file_type('P', prolog)),asserta(user:prolog_file_type('p', prolog)).
:-include('sigma_header.pl').

:-initializeSigmaServerData.

:- ignore_catch((current_prolog_flag(arch,'i386-win32') -> win32 ; cs)).
 


