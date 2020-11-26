:-module(sciff_options,
     [get_option/2,
      set_option/2,
      sciff_option/2,
      set_options/1,
      show_options/0,
      available_option/1]).


:- dynamic(sciff_option/2).




%----------------------------------------------------------
% ALL OPTIONS
%----------------------------------------------------------
sciff_option(fulfiller,off).

sciff_option(fdet,off).
%sciff_option(fdet,on).

sciff_option(seq_act,off).

sciff_option(factoring,off).

sciff_option(sciff_debug, on).

sciff_option(violation_causes_failure, yes).

sciff_option(graphviz, off).

sciff_option(allow_events_not_expected, yes).

sciff_option(portray_ic,on).

sciff_option(print_quant,off).

available_option(fulfiller).
available_option(fdet).
available_option(seq_act).
available_option(factoring).
available_option(sciff_debug).
available_option(violation_causes_failure).
available_option(graphviz).
available_option(allow_events_not_expected).
available_option(portray_ic).
available_option(print_quant).
available_option(coloring).


get_option(O,V):-
    sciff_option(O,V).

set_option(Option,Value):-
    (sciff_option(Option,OldVal)
     -> retract(sciff_option(Option,OldVal)),
        assert(sciff_option(Option,Value))
     ;  (available_option(Option)
            ->  assert(sciff_option(Option,Value))
            ;   (is_dialect(swi)
                ->  atom_concat('SCIFF Unknown option ',Option,Message),
                    throw(error(_,context(set_option/2,Message)))
                ;   throw(type_error(set_option(Option,Value), 1, available_option, Option))
                )
        )
    ).

show_options :-
	findall(sciff_option(Option, Value), sciff_option(Option, Value), ListOption),
	print_options(ListOption).
print_options([]) :- nl, nl.
print_options([sciff_option(Option, Value)| T]) :-
	write(Option),
	write(' is '),
	write(Value),
	write('.'), nl,
	print_options(T).

set_options([]).
set_options([[O,V]|T]):-
    set_option(O,V),
    set_options(T).
	

