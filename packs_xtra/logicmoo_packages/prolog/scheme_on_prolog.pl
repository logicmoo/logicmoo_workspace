% -------------------------------BEGIN PROLOG CODE----------------------------

/* The 2 implicit arguments added by '-->' are the input and output
versions of the Machine.
interpret/4 is called by:
interpret(Expression, Value, MachineIn, MachineOut)
where Expression and MachineIn are not variables (generally should be
ground), and Value and MachineOut are generally unbound variables.
[define, [factorial, n],
  [if, [=, n, 1],
        1,
        [*, n, [factorial, [-, n, 1]]]].

*/

interpret(Expression,Value) -->
          interpret_case(Expression, Value),
          interpret_message(Expression, Value),
          !.

interpret(Expression, _) -->
          print_representation(Expression, PrintExpression),
          {proscheme_error_message(interpret, ['Unable to interpret',
PrintExpression])},
          !,
          {fail}.


interpret_case(atom(X), Value) -->
          interpret_case_atom(X, Value).

interpret_case(ptr(ID), Value) -->
          dereferenced_value(ptr(ID), cons(Car, Cdr)),
          interpret_case_cons(Car, Cdr, ptr(ID), Value).


interpret_case_atom(Symbol, atom(Symbol)) -->
          {number(Symbol)},
          !.

interpret_case_atom(Symbol, Value) -->
          choose_binding(atom(Symbol), Value).


interpret_case_cons(atom(X), Cdr, Ptr, Value) -->
          interpret_case_cons_atom(X, Cdr, Ptr, Value).

interpret_case_cons(ptr(ID), Cdr, _, Value) -->
          interpret(ptr(ID), Procedure),
          interpret_operand_expressions(Cdr, RestValues),
          interpret_procedure(Procedure, RestValues, Value).


interpret_case_cons_atom(load, atom(nil), _, Value) -->
          !,
          {old('TEXT', File, Vol, 'ProScheme source file:'),
           scheme_parse(File, Vol, Source)},
          allocate_list([sequence|Source], AllocatedSource),
          interpret(AllocatedSource, Value).
          
interpret_case_cons_atom(load, Cdr, _, Value) -->
          !,
          dereferenced_value(Cdr, cons(atom(File), atom(nil))),
          {dvol(Vol),
           scheme_parse(File, Vol, Source)},
          allocate_list([sequence|Source], AllocatedSource),
          interpret(AllocatedSource, Value).
          
interpret_case_cons_atom(primitive, Cdr, _, Name) -->
          !,
           dereferenced_value(Cdr, cons(Name, Rest)),
           interpret_primitive_functor(Name, Rest, Functor),
          {procedure_parts(Procedure, primitive, ID, [], Functor)},
          current_environment_id(ID),
          bind(Name, Procedure).

interpret_case_cons_atom(lambda, Cdr, _, Procedure) -->
          !,
          interpret_lambda(Cdr, Procedure).

interpret_case_cons_atom(define, Cdr, _, Symbol) -->
          match_cons_ptr_to_list(Cdr, [[Symbol|Parameters]|Expressions]),
          !,
          cons_ptr(atom(sequence), Expressions, Body),
          interpret_procedure_definition(Parameters, Body, Procedure),
          bind(Symbol, Procedure).

interpret_case_cons_atom(define, Cdr, _, Symbol) -->
          match_cons_ptr_to_list(Cdr, [Symbol, Expression|atom(nil)]),
          !,
          interpret(Expression, ExpressionValue),
          bind(Symbol, ExpressionValue).

interpret_case_cons_atom(sequence, Cdr, _, Value) -->
          !,
          interpret_sequence(Cdr, Value).

interpret_case_cons_atom(quote, Cdr, _, Symbol) -->
          !,
          dereferenced_value(Cdr, cons(Symbol, atom(nil))).

interpret_case_cons_atom('set!', Cdr, _, Symbol) -->
          !,
          match_cons_ptr_to_list(Cdr, [Symbol, Expression|atom(nil)]),
          interpret(Expression, SetValue),
          rebind(Symbol, SetValue).

interpret_case_cons_atom('set-car!', Cdr, _, ConsValue) -->
          !,
          match_cons_ptr_to_list(Cdr, [Cons, NewCar|atom(nil)]),
          interpret(Cons, ConsValue),
          interpret(NewCar, NewCarValue),
          set_car(ConsValue, NewCarValue).

interpret_case_cons_atom('set-cdr!', Cdr, _, ConsValue) -->
          !,
          match_cons_ptr_to_list(Cdr, [Cons, NewCdr|atom(nil)]),
          interpret(Cons, ConsValue),
          interpret(NewCdr, NewCdrValue),
          set_cdr(ConsValue, NewCdrValue).

interpret_case_cons_atom(cons, SExpressionCdr, _, Value) -->
          !,
          match_cons_ptr_to_list(SExpressionCdr, [Car, Cdr|atom(nil)]),
          interpret(Car, CarValue),
          interpret(Cdr, CdrValue),
          cons_ptr(CarValue, CdrValue, Value).

interpret_case_cons_atom(car, SExpressionCdr, _, Value) -->
          !,
          dereferenced_value(SExpressionCdr, cons(Cons, atom(nil))),
          interpret(Cons, ConsValue),
          car_ptr(ConsValue, Value).

interpret_case_cons_atom(cdr, SExpressionCdr, _, Value) -->
          !,
          dereferenced_value(SExpressionCdr, cons(Cons, atom(nil))),
          interpret(Cons, ConsValue),
          cdr_ptr(ConsValue, Value).

interpret_case_cons_atom(list, SExpressionCdr, _, Value) -->
          !,
          interpret_operand_expressions(SExpressionCdr, Value).

interpret_case_cons_atom(let, Cdr, _, Value) -->
          !,
          interpret_let(Cdr, Value).

interpret_case_cons_atom(cond, Cdr, _, Value) -->
          !,
          interpret_conditional_expressions(Cdr, Value).
          
interpret_case_cons_atom(if, Cdr, _, Value) -->
          !,
          match_cons_ptr_to_list(Cdr, [Test, Then, Else|atom(nil)]),
          interpret(Test, TestValue),
          interpret_if(TestValue, Then, Else, Value).

interpret_case_cons_atom(error, Cdr, _, atom(nil)) -->
          !,
          interpret_operand_expressions(Cdr, MessageValues),
          print_representation(MessageValues, MessageData),
          proscheme_error_message(program, MessageData).

interpret_case_cons_atom('eq?', Cdr, _, Value) -->
          !,
          interpret_operand_expressions(Cdr, ArgValues),
          dereferenced_value(ArgValues, cons(Arg1, Arg2)),
          interpret_eq(Arg1, Arg2, Value).

interpret_case_cons_atom('cons-stream', SExpressionCdr, _, Value) -->
          !,
          match_cons_ptr_to_list(SExpressionCdr, [Car, Cdr|atom(nil)]),
          interpret(Car, CarValue),
          interpret_procedure_definition(atom(nil), Cdr, DelayedCdr),
          cons_ptr(atom('memo-proc'), DelayedCdr, MemoDelayedCdr),
          cons_ptr(CarValue, MemoDelayedCdr, Value).

interpret_case_cons_atom(force, SExpressionCdr, _, Value) -->
          !,
          interpret(SExpressionCdr, Value).

interpret_case_cons_atom(AtomName, Cdr, _, Value) -->
          !,
          interpret(atom(AtomName), Procedure),
          interpret_operand_expressions(Cdr, RestValues),
          interpret_procedure(Procedure, RestValues, Value).

interpret_procedure_definition(Parameters, Expression, Procedure) -->
          {procedure_parts(Procedure, defined, ID, Parameters,
Expression)},
          current_environment_id(ID).


interpret_primitive_functor(Name, atom(nil), Functor) -->
           !,
          {Name = atom(Functor)}.

interpret_primitive_functor(_, Rest, Functor) -->
            dereferenced_value(Rest, cons(atom(Functor), atom(nil))).


interpret_lambda(Cdr, Procedure) -->
          match_cons_ptr_to_list(Cdr, [Parameters, Expression|atom(nil)]),
          interpret_procedure_definition(Parameters, Expression,
Procedure).


interpret_sequence(Expressions, Value) -->
          interpret_sequence(Expressions, atom(nil), Value).


interpret_sequence(atom(nil), Value, Value) --> [].

interpret_sequence(Cons, _, Value) -->
          dereferenced_value(Cons, cons(Expression, OtherExpressions)),
          interpret(Expression, TempValue),
          interpret_sequence(OtherExpressions, TempValue, Value).


interpret_operand_expressions(atom(nil), atom(nil)) --> [].

interpret_operand_expressions(Cons, Values) -->
          dereferenced_value(Cons, cons(Expression, OtherExpressions)),
          cons_ptr(Value, OtherValues, Values),
          interpret(Expression, Value),
          interpret_operand_expressions(OtherExpressions, OtherValues).


interpret_if(atom(nil), _, Else, Value) -->
          !,
          interpret(Else, Value).

interpret_if(_, Then, _, Value) -->
          interpret(Then, Value).


interpret_let(Cdr, Value) -->
          match_cons_ptr_to_list(Cdr, [VariableDefinitions,
Expression|atom(nil)]),
          split_variable_definitions(VariableDefinitions, Parameters,
VariableExpressions),
          interpret_procedure_definition(Parameters, Expression,
Procedure),
          interpret_operand_expressions(VariableExpressions,
VariableValues),
          interpret_procedure(Procedure, VariableValues, Value).
         

interpret_eq(Arg, Arg, atom(t)) --> !.

interpret_eq(_, _, atom(nil)) --> [].


split_variable_definitions(atom(nil), atom(nil), atom(nil)) --> [].

split_variable_definitions(Definitions,
                                         Parameters,
                                         Expressions) -->
          dereferenced_value(Definitions, cons(Definition,
OtherDefinitions)),
          match_cons_ptr_to_list(Definition, [Parameter,
Expression|atom(nil)]),
          cons_ptr(Parameter, OtherParameters, Parameters),
          cons_ptr(Expression, OtherExpressions, Expressions),
          split_variable_definitions(OtherDefinitions, OtherParameters,
OtherExpressions).


interpret_message(Expression, Value, Machine, Machine) :-
          flags_in_machine(Flags, Machine),
          choose(Flags, verbose),
          !,
          print_representation(Expression, PrintExpression, Machine,
Machine),
          print_representation(Value, PrintValue, Machine, Machine),
          nl,
          writeseqnl(['Interpreted:', PrintExpression, '; with value (',
Value, '):', PrintValue]),
          display_machine(Machine).

interpret_message(_, _, Machine, Machine).


proscheme_error_message(ProcedureName, Message) :-
          writeseqnl([ProcedureName, ':'|Message]),
          Depth = 100,
          Width = 300,
          ButtonWidth = 110,
          ContinueTop is Depth - 25,
          ContinueLeft is Width - 2 * (ButtonWidth + 5),
          HaltTop is ContinueTop,
          HaltLeft is Width - (ButtonWidth + 5),
          TextTop is 10,
          TextLeft is 10,
          TextDepth is ContinueTop - (TextTop + 5),
          TextWidth is Width - (TextLeft - 10),
          dialog('PROSCHEME ERROR', 50, 50, Depth, Width,
                     [button(ContinueTop, ContinueLeft, 20, ButtonWidth,
'Continue'),
                       button(HaltTop, HaltLeft, 20, ButtonWidth, 'Halt
ProScheme'),
                       text(TextTop, TextLeft, TextDepth, TextWidth,
                              wseq([ProcedureName, ':'|Message]))],
                     _)
           -> true
          ; abort.

% -------END PROLOG CODE-----------------
