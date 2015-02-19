:- module(interface, [implements/1,
		      bind_interface/2,
		      end_interface/0]).

:- use_module(library(apply)).
:- use_module(library(error)).

:- multifile
    '$interface'/3,
    '$implementation'/2.

:- meta_predicate implements(:).
implements(Implementation:Alias) :-
    Implementation:use_module(Alias, []), % Ensure that the module is loaded
    absolute_file_name(Alias, File,
		       [file_type(prolog), access(read)]),
    module_property(Interface, file(File)),
    module_property(Interface, exports(PIL)),
    compile_aux_clauses(interface:'$implementation'(Implementation, Interface)),
    maplist(Implementation:export, PIL).

direct_interface(M, F/A) :-
    \+ current_predicate(M:F/A).
		 
:- module_transparent end_interface/0.
end_interface :-
    context_module(Interface),
    module_property(Interface, exports(PIL)),
    partition(interface:direct_interface(Interface), PIL, DIL, IIL),
    compile_aux_clauses(interface:'$interface'(Interface, DIL, IIL)),
    maplist(Interface:multifile, DIL). % Just to avoid warnings, will be abolished

bind_interface(Interface, Implementation) :-
    ( '$interface'(Interface, DIL, IIL)
    ->true
    ; existence_error(interface, Interface)
    ),
    ( '$implementation'(Implementation, Interface)
    ->true
    ; ( '$implementation'(Implementation, _)
      ->existence_error(implementation, Implementation)
      ; existence_error(binding, Interface->Implementation)
      )
    ),
    module_property(Implementation, file(File)),
    maplist(Interface:abolish, DIL),
    Interface:use_module(File, DIL),
    atom_concat(Interface, '$impl', II),
    maplist(II:abolish, IIL),
    II:use_module(File, IIL).
