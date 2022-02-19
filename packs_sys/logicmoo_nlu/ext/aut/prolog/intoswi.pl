:- module(ari_swi,[op(1000,fy,extrn),op(1000,fy,visible),op(1000,fx,not)]).


pc(Term,[]):- Term == end_of_file,!.
pc(Term,VNs):-  portray_clause(user_output,Term,[variable_names(VNs)]),!.

into_swi(F):-
  atom_concat(F,'.pl',O),
  setup_call_cleanup(open(O,write,OS),
  with_output_to(OS,
  (writeln(''),
  setup_call_cleanup(open(F,read,R),
   (repeat,once((read_term(R,Term,[module(ari_swi),variable_names(VNs)]),
      pc(Term,VNs),nl)),
      Term==end_of_file),close(R)))),
  close(OS)).


into_swi:- expand_file_name(*,Files),maplist(into_swi,Files).
