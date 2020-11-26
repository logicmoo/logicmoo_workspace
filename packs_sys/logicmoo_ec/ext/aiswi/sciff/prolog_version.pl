% dal manuale di SWI: file://localhost/home/marco/lib/pl-5.7.12/doc/Manual/flags.html#current_prolog_flag/2
%The code below is a reliable and portable way to detect SWI-Prolog. 
%is_dialect(swi) :-
%        catch(current_prolog_flag(dialect, swi), _, fail).

:- module(prolog_version,
    [is_dialect/1, is_unix/1]).

is_dialect(X) :-
        catch(current_prolog_flag(dialect, X), _, fail),
        !.
is_dialect(sicstus) :-
        catch(( current_prolog_flag(version, X),
                 atom_concat('SICStus',_,X)
              )
            , _, fail).


is_unix(X):-
    is_dialect(swi),!, current_prolog_flag(unix,X).
is_unix(X):-
    current_prolog_flag(host_type,HostType),
    % In SICStus, I suppose the host is Unix if the returned atom begins with 'x86'
    % but I am not sure this is the right way to do this check
    ((atom_concat(x86,_,HostType) ;
    % SWI recognizes Darwin (Mac) as Unix, so I will impose that also in SICStus
    % On Mac OSX 10.5.8 (Leopard) on MacBook (Intel), SICStus returns i38664-darwin-8.9.1
      atom_concat(_,B,HostType), atom_concat(darwin,_,B)
     )
    ->  X = true
    ;   X = false
    ).

