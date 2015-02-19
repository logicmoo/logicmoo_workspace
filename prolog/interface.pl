:- module(interface, [implements/1,
		      bind_interface/2,
		      end_interface/0]).

:- use_module(library(apply)).

:- multifile
    '$interface'/3.

:- meta_predicate implements(:).
implements(Context:Alias) :-
    Context:use_module(Alias, []), % Ensure that the module is loaded
    absolute_file_name(Alias, File,
		       [file_type(prolog), access(read)]),
    current_module(Module, File),
    module_property(Module, exports(PIL)),
    maplist(Context:export, PIL).

direct_interface(M, F/A) :-
    \+ current_predicate(M:F/A).
		 
:- module_transparent end_interface/0.
end_interface :-
    context_module(Context),
    module_property(Context, exports(PIL)),
    partition(interface:direct_interface(Context), PIL, DIL, IIL),
    compile_aux_clauses(interface:'$interface'(Context, DIL, IIL)),
    maplist(Context:multifile, DIL). % Just to avoid warnings, will be abolished

bind_interface(Interface, Implementation) :-
    atom_concat(Interface, '$impl', II),
    current_module(Implementation, File),
    '$interface'(Interface, DIL, IIL),
    maplist(Interface:abolish, DIL),
    Interface:use_module(File, DIL),
    maplist(II:abolish, IIL),
    II:use_module(File, IIL).
