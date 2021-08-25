%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% utility preds for prospec
%% -------------------------------------

%% Author: bernd thomas 
%% E-mail: bthomas@informatik.uni-koblenz.de
%% --------------------------------------------------
%% $Id: utils.pl,v 1.2 1998/04/06 09:50:48 bthomas Exp $
%% $Log: utils.pl,v $
%% Revision 1.2  1998/04/06 09:50:48  bthomas
%% no more make_exec.
%%
%% Revision 1.1  1998/02/10 18:01:01  bthomas
%% Initial revision
%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close_list([V1],[V1]) :- var(V1), !.

close_list(OpenList,ClosedList) :-
	length(OpenList,L),
	!,
	length(ClosedList,L),
	ClosedList = OpenList, !.

term_list(U,[U]) :- var(U), !.
term_list((A,B),[A|More]) :-
	term_list(B,More), !.
term_list(A,[A]) :- !.

stop :-
	clean_up,
	halt.

msg([]) :- nl,!.
msg([E|ME]) :-
	numbervars(E,0,_),
	write(E),
	msg(ME), !.

msg(S,[]) :- nl(S),!.
msg(S,[E|ME]) :-
	numbervars(E,0,_),
	write(S,E),
	msg(S,ME), !.


error(Code) :-
	error_code(Code,Message),
	msg(Message),
	halt.

logo :-
	msg([""]),
	msg(["\t\t     o-o         "]),
	msg(["\t\t     |P|rolog &  "]),
        msg(["\t\t     |R|         "]),
	msg(["\t\t   1.|O|rder     "]),
	msg(["\t\t     |S|ort      "]),
	msg(["\t\t     |P|         "]),
	msg(["\t\t     |E|xtension "]),
	msg(["\t\t     |C|         "]),
	msg(["\t\t     o-o         "]),
	version_number(VersionNumber),
	msg(["\n\t\t   B.Thomas"]),
	msg(["\t\t P.Baumgartner"]),
        msg(["\n\t\t",VersionNumber,"\n\n"]).


file_header(OUT,INPUT) :-
	msg(OUT,['%% O--------------------------------------O']),
        msg(OUT,['%% | ProSpec                              |']),
        msg(OUT,['%% |   bthomas@informatik.uni-koblenz.de  |']),
        msg(OUT,['%% |--------------------------------------|']),
        msg(OUT,['%% | Normal Form Transformator (pl2tme):  |']),
        msg(OUT,['%% |   peter@informatik.uni-koblenz.de    |']),
        msg(OUT,['%% O--------------------------------------O']),
        msg(OUT,['%% ========================================']),
        msg(OUT,['%%  input file: ',INPUT]),
        msg(OUT,['%% ========================================\n']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fun stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_blinky :-
	setval(blinky,99), flush(stdout), !.

stop_blinky :-
	clean,
	write(' '),
	flush(stdout), !.

blinky :-
	flush(stdout),
	getval(blinky,V),
	( V=99, write('-'), setval(blinky,0) ;
	  V=0 , clean, write('\\'), setval(blinky,1) ;
	  V=1, clean, write('|'), setval(blinky,2) ;
	  V=2, clean, write('/'), setval(blinky,3) ;
	  V=3, clean, write('-'), setval(blinky,0) ), 
	 flush(stdout), !.
  
clean :- write('\b'), !.
 

usage :-
	nl,
	version_number(VersionNumber),
	write('ProSpec Version '),
	writeln(VersionNumber),
	nl,
	writeln('usage: prospec XXX.sort'),
	writeln('omit extender .sort'),
	halt.
