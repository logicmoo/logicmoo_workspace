:- module(interface, [implements/1,
		      bind_interface/2,
		      end_interface/0]).

:- use_module(library(apply)).

:- meta_predicate implements(:).
implements(Context:Alias) :-
    Context:use_module(Alias, []), % Ensure that the module is loaded
    absolute_file_name(Alias, File,
		       [file_type(prolog), access(read)]),
    current_module(Module, File),
    module_property(Module, exports(PIL)),
    maplist(Context:export, PIL).

:- module_transparent end_interface/0.
end_interface :-
    context_module(M),
    module_property(M, exports(PIL)),
    forall(( member(F/A, PIL),
	     \+ current_predicate(M:F/A)
	   ),
	   multifile(F/A)). % Just to avoid warnings, will be abolished

bind_interface(Interface, Implementation) :-
    module_property(Interface, exports(PIL)),
    atom_concat(Interface, '$impl', II),
    current_module(Implementation, File),
    partition(direct_interface(Interface), PIL, DIL, IIL),
    maplist(Interface:abolish, DIL),
    Interface:use_module(File, DIL),
    maplist(II:abolish, IIL),
    II:use_module(File, IIL).

direct_interface(Interface, F/A) :-
    current_predicate(Interface:F/A),
    functor(H, F, A),
    \+ ( predicate_property(Interface:H, number_of_clauses(N)),
	 N>0
       ).
