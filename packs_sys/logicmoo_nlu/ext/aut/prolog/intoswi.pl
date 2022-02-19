:- module(ari_swi,[op(1000,fy,extrn),op(1000,fy,visible),op(1000,fy,mode),op(900,fy,not)]).
:- use_module(library(logicmoo_common)).
:- set_prolog_flag(back_quotes,string).

pc(Term,[]):- Term == end_of_file,!.
%pc(:-(OP),C):- writeln(':- '),!,pc(OP,C).
pc(:- op(X,Y,[Z]),C):- !, pc(:- op(X,Y,Z),C).
pc(:- op(X,Y,Z),_):- ari_swi:op(X,Y,Z),fail.
pc(Term,VNs):-  portray_clause(current_output,Term,[variable_names(VNs)]),!.
pc(Term,VNs):-  print_tree(Term,[variable_names(VNs)]),!.

into_swi(F):-
  atom_concat(F,'.pl',O),
  setup_call_cleanup( 
  open(O,write,OS),
  %OS = current_output,
  with_output_to(OS,
  (writeln(''),
  setup_call_cleanup(open(F,read,R),
   (repeat,once((read_term(R,Term,[module(ari_swi),variable_names(VNs)]),
      pc(Term,VNs),nl)),
      Term==end_of_file),close(R)))),
  close(OS)).


into_swi:- 
  \+ \+ (expand_file_name('*.ari',Files),maplist(into_swi,Files)),
    \+ \+ (expand_file_name('*.ARI',Files),maplist(into_swi,Files)),
      \+ \+ (expand_file_name('*.BAK',Files),maplist(into_swi,Files)),!.


:- into_swi.
