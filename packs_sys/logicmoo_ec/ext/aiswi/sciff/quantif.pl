% SWI version.
% Since in SWI the name of the attribute is also the name of the module,
% we need 2 modules to define the 2 required attributes

:- module(quantif,     
    [% From module quant:
     quant/2,
     set_term_quantification/2,
     get_quant/2,
     set_quant/2,
     is_term_quantified/2,
     is_unquantified/1,
     forall/1, forallf/1, exists/1, existsf/1,
     is_universal/1,
     is_existential/1,
     print_quant/0, print_quant/1,          %FOR THE DEBUGGER

    % From module restrictions:
     set_restriction/1,
     set_restriction/2,
     get_restrictions/2,
     set_restriction_list/1,
     set_restriction_list/2,
     st/1, st/2
    ]).

%:- reexport([quant,restrictions]). In SICStus there is no reexport, so I do it by hand
:- use_module(quant).
:- use_module(restrictions).

