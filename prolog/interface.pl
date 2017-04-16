/*  Part of Extended libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

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
    \+ ( current_predicate(M:F/A),
         functor(H, F, A),
         predicate_property(M:H, defined),
         \+ predicate_property(M:H, imported_from(_))
       ).

:- module_transparent end_interface/0.
end_interface :-
    context_module(Interface),
    end_interface(Interface).

end_interface(Interface) :-
    module_property(Interface, exports(PIL)),
    partition(interface:direct_interface(Interface), PIL, DIL, IIL),
    compile_aux_clauses(interface:'$interface'(Interface, DIL, IIL)),
    forall(( member(F/A, DIL),
             functor(H, F, A)),
           compile_aux_clauses([(Interface:H
                                :- existence_error(binding, Interface:F/A))])).

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
    maplist(Interface:abolish, DIL),
    '$import_from_loaded_module'(Implementation, Interface, [imports(DIL)]),
    atom_concat(Interface, '$impl', II),
    forall(member(P, IIL), abolish(II:P)),
    '$import_from_loaded_module'(Implementation, II, [imports(IIL)]).
