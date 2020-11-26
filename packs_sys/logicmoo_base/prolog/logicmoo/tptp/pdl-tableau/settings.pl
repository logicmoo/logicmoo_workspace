%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright 2005, Renate Schmidt, University of Manchester
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    The key calculus determines which set of rules are used.
%    Possible settings:

set_calculus(Flag) :-
    flag(calculus, _, Flag).

get_calculus(Flag) :-
    flag(calculus, Flag, Flag).

load_calculus(Calculus) :-
    current_module(Calculus).

load_calculus(Calculus) :-
    get_calculus(OldCalculus),
    atom(OldCalculus),
    abolish(eventualities/2),
    abolish(simplify_X/2),
    abolish(reduce_local/7),
    set_calculus(Calculus),
    use_module(Calculus).

load_calculus(Calculus) :-
    set_calculus(Calculus),
    use_module(Calculus).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    The key negate_first can be either yes or no
%    If set to yes, the problem is negated first 
%    (theory remains unchanged)
%    Default: unset == no

set_negate_first(Flag) :-
    flag(negate_first, _, Flag).

get_negate_first(Flag) :-
    flag(negate_first, Flag, Flag).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    The key search_output_filename represents the file to which the
%    derivation steps are output

create_search_output_file :- !,
    get_search_output_filename(Filename),
    open(Filename, write, _).

set_search_output_filename(Filename) :- !,
    write('set search_output_filename to '), write(Filename), nl,
    flag(search_output_filename, _, Filename).

get_search_output_filename(Filename) :- !,
    flag(search_output_filename, Filename, Filename).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    The key test_output_filename represents the file to which the
%    output of tests is written

create_test_output_file :- !,
    get_test_output_filename(Filename),
    open(Filename, write, _, [alias(test_output)]).

set_test_output_filename(Filename) :- !,
    write('set test_output_filename to '), print(Filename), nl,
    flag(test_output_filename, _, Filename).

get_test_output_filename(Filename) :- !,
    flag(test_output_filename, Filename, Filename).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    The key output_format determines the format of the output
%    Either: latex or txt

set_output_format(Format) :-
    flag(output_format, _, Format).

get_output_format(Format) :-
    flag(output_format, Format, Format).

