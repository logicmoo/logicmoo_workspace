%==============================================================================
%----This outputs TPTP Problem Set clauses in a format acceptable to
%----the SMT standard format
%==============================================================================
%==============================================================================
%----Generic output
%==============================================================================
%------------------------------------------------------------------------------
%----Output an atom with variables in SMT format
smt_output_term(_,Variable):-
    looks_like_a_variable(Variable),
    !,
    write('?'),
    write(Variable).

smt_output_term(Format,[OneTerm]):-
    !,
    smt_output_term(Format,OneTerm).

smt_output_term(Format,[FirstTerm|RestOfTerms]):-
    !,
    smt_output_term(Format,FirstTerm),
    write(' '),
    smt_output_term(Format,RestOfTerms).

smt_output_term(smt2,Numerator / Denominator):-
    looks_like_a_number(Numerator),
    Numerator < 0, 
    !,
    write('(- '),
    PositiveNumerator is -Numerator,
    smt_output_term(smt2,PositiveNumerator / Denominator),
    write(' )').

%----This doesn't work in SMT2
smt_output_term(smt2,Numerator / Denominator):-
    fail,
    !,
    smt_output_term(smt2,Numerator),
    write('.0'),
    write('/'),
    smt_output_term(smt2,Denominator),
    write('.0').
   
smt_output_term(_,Numerator / Denominator):-
    !,
    write('(/ '),
    smt_output_term(smt,Numerator),
    write('.0'),
    write(' '),
    smt_output_term(smt,Denominator),
    write('.0'),
    write(')').
%    Real is Numerator / Denominator,
%    write('approximately('),
%    smt_output_term(smt,Real),
%    write(')').
   
%----Negative numbers
smt_output_term(_,NegativeNumber):-
    looks_like_a_number(NegativeNumber),
    NegativeNumber < 0,
    !,
    PositiveNumber is -NegativeNumber,
    write('(- '),
    write(PositiveNumber),
    write(')').

%----Numbers
smt_output_term(_,Number):-
    looks_like_a_number(Number),
    !,
    write(Number).

%----Die for things SMT does not support
smt_output_term(_,Symbol):-
    atomic(Symbol),
    tptp2X_member(Symbol,['$evaleq','$to_rat','$is_rat']),
    !,
    % write('ERROR: SMT does not know what to do with '),
    write(Symbol).

%----Translate atomic symbols
smt_output_term(smt,Symbol):-
    atomic(Symbol),
    tptp2X_member((Symbol)-(SMTSymbol),
[
('$uminus')-('~'),
('$is_int')-('IsInt')
]),
    !,
    write(SMTSymbol).

%----Translate atomic symbols
smt_output_term(_,Symbol):-
    atomic(Symbol),
    tptp2X_member((Symbol)-(SMTSymbol),
[
%----Functions
('$sum')-('+'),
('$difference')-('-'),
('$product')-('*'),
('$quotient')-(div),
% ('$remainder_int')-(''),
% ('$power_int')-(''),
('$uminus')-('-'),
% ('$abs_int')-(''),
% ('$min_int')-(''),
% ('$max_int')-(''),
('$to_int')-(to_int),
('$to_real')-(to_real),
%----Predicates
('$less')-('<'),
('$lesseq')-('<='),
('$greater')-('>'),
('$greatereq')-('>='),
('$tptp_equal')-('='),
('$is_int')-(is_int),
('$true')-(true),
('$false')-(false),
('$distinct')-(distinct),
%----Types
('$int')-('Int'),
('$rat')-('Real'),
('$real')-('Real'),
('$i')-(smt_individual),
('$iType')-(smt_individual),
('$o')-('Bool'),
('$oType')-('Bool'),
%----Clashes with SMT keywords
('abs')-(smt_abs),
('and')-(smt_and),
('Array')-(smt_Array),
('as')-(smt_as),
('assert')-(smt_assert),
('BitVec')-(smt_BitVec),
('Bool')-(smt_Bool),
('bvadd')-(smt_bvadd),
('bvand')-(smt_bvand),
('bvashr')-(smt_bvashr),
('bvcomp')-(smt_bvcomp),
('bvlshr')-(smt_bvlshr),
('bvmul')-(smt_bvmul),
('bvnand')-(smt_bvnand),
('bvneg')-(smt_bvneg),
('bvnor')-(smt_bvnor),
('bvnot')-(smt_bvnot),
('bvor')-(smt_bvor),
('bvsdiv')-(smt_bvsdiv),
('bvsge')-(smt_bvsge),
('bvsgt')-(smt_bvsgt),
('bvshl')-(smt_bvshl),
('bvsle')-(smt_bvsle),
('bvslt')-(smt_bvslt),
('bvsmod')-(smt_bvsmod),
('bvsrem')-(smt_bvsrem),
('bvsub')-(smt_bvsub),
('bvudiv')-(smt_bvudiv),
('bvuge')-(smt_bvuge),
('bvugt')-(smt_bvugt),
('bvule')-(smt_bvule),
('bvult')-(smt_bvult),
('bvurem')-(smt_bvurem),
('bvxnor')-(smt_bvxnor),
('bvxor')-(smt_bvxor),
('check-sat')-(smt_check-sat),
('concat')-(smt_concat),
('continued-execution')-(smt_continued-execution),
('declare-fun')-(smt_declare-fun),
('declare-sort')-(smt_declare-sort),
('define-fun')-(smt_define-fun),
('define-sort')-(smt_define-sort),
('distinct')-(smt_distinct),
('div')-(smt_div),
('error')-(smt_error),
('exists')-(smt_exists),
('exit')-(smt_exit),
('extract')-(smt_extract),
('false')-(smt_false),
('forall')-(smt_forall),
('get-assertions')-(smt_get-assertions),
('get-assignment')-(smt_get-assignment),
('get-info')-(smt_get-info),
('get-model')-(smt_get-model),
('get-option')-(smt_get-option),
('get-proof')-(smt_get-proof),
('get-unsat-core')-(smt_get-unsat-core),
('get-value')-(smt_get-value),
('immediate-exit')-(smt_immediate-exit),
('incomplete')-(smt_incomplete),
('Int')-(smt_Int),
('ite')-(smt_ite),
('let')-(smt_let),
('logic')-(smt_logic),
('memout')-(smt_memout),
('mod')-(smt_mod),
('none')-(smt_none),
('not')-(smt_not),
('or')-(smt_or),
('par')-(smt_par),
('pop')-(smt_pop),
('push')-(smt_push),
('Real')-(smt_Real),
('repeat')-(smt_repeat),
('rotate_left')-(smt_rotate_left),
('rotate_right')-(smt_rotate_right),
('sat')-(smt_sat),
('select')-(smt_select),
('set-info')-(smt_set-info),
('set-logic')-(smt_set-logic),
('set-option')-(smt_set-option),
('sign_extend')-(smt_sign_extend),
('success')-(smt_success),
('store')-(smt_store),
('theory')-(smt_theory),
('true')-(smt_true),
('unknown')-(smt_unknown),
('unsat')-(smt_unsat),
('unsupported')-(smt_unsupported),
('xor')-(smt_xor),
('zero_extend')-(smt_zero_extend),
('benchmark')-(smt_benchmark),
('declare-datatypes')-(smt_declare-datatypes),
('declare-const')-(smt_declare-const),
('declare-funs')-(smt_declare-funs),
('declare-sorts')-(smt_declare-sorts),
('define-sorts')-(smt_define-sorts),
('echo')-(smt_echo),
('eval')-(smt_eval),
('flet')-(smt_flet),
('iff')-(smt_iff),
('implies')-(smt_implies)
]),
    !,
    write(SMTSymbol).

%----Normal atomic things
smt_output_term(_,Atomic):-
    atomic(Atomic),
    !,
    writeq(Atomic).

smt_output_term(Format,T1 > T2):-
    !,
    output_prefix_formula(Format,T1,0,0,outermost),
    write(' '),
    output_prefix_formula(Format,T2,0,0,outermost).

smt_output_term(Format,T1*T2):-
    !,
    output_prefix_formula(Format,T1,0,0,outermost),
    write(' '),
    output_prefix_formula(Format,T2,0,0,outermost).

smt_output_term(_Format,_Name : _Type):-
    !,
    write('SHOULD NEVER DO THIS').

%----Complex terms
smt_output_term(Format,Complex):-
    Complex =.. [Symbol|Arguments],
    write('('),
    smt_output_term(Format,Symbol),
    write(' '),
    smt_output_term(Format,Arguments),
    write(')').

smt_output_term(Term):-
    smt_output_term(smt,Term).

smt2_output_term(Term):-
    smt_output_term(smt2,Term).

%------------------------------------------------------------------------------
%----Output a quantified variable
smt_output_variable(Variable:Thing):-
    looks_like_a_variable(Variable),
    write('('),
    smt_output_term(Variable),
    (   tptp_atomic_formula(Thing)
    ->  (   write(' '),
            NewIndent = 0
        )
    ;   (   nl,
            NewIndent = 6
        )
    ),
    output_prefix_formula(smt,Thing,NewIndent,0,outermost),
    write(')').

smt_output_variable(Variable:=Thing):-
    looks_like_a_variable(Variable),
    smt_output_variable(Variable:Thing).

smt2_output_variable(Variable):-
    smt_output_variable(Variable).

%------------------------------------------------------------------------------
%----Output an atom
smt_output_atom(_,Atom):-
    smt_output_term(smt,Atom).

smt2_output_atom(_,Atom):-
    smt_output_term(smt2,Atom).
%------------------------------------------------------------------------------
%==============================================================================
%----FOF output
%==============================================================================
%------------------------------------------------------------------------------
%----Equality must be a formula
smt_let_type([_:='$tptp_equal'(_,_)],_,flet):-
    !.

%----Something that looks like a formula is one
smt_let_type([_:=Value|_],_,flet):-
    tptp_non_atomic_formula(Value),
    !.

%----If it's not obvious, see if the Variable is used as an atom in Scope
smt_let_type([Variable:=_],Scope,flet):-
    findall(PredicateSymbol,
        (   extract_atom_from_formula(Scope,no,Atom),
            functor(Atom,PredicateSymbol,_)),
        PredicateSymbols),
    tptp2X_exact_member(Variable,PredicateSymbols),
    !.

%----This assumes all things that look like terms really are. But they can
%----be predicates, so I need to dig into the formula - TO DO
smt_let_type([_:=_],_,let):-
    !.

%----Otherwise something is wrong
smt_let_type([Variable:=_],_,_):-
    write('ERROR: In := definitions --->'),
    write(Variable),
    nl,
    fail.
%------------------------------------------------------------------------------
%----Recognise and split up quantified formulae
smt_quantified_formula(QuantifiedFormula,SMTQuantifier,Variables,
Formula):-
    QuantifiedFormula =.. [:,Quantification,Formula],
    !,
    Quantification =.. [Quantifier,Variables],
    tptp2X_member(ct(Quantifier,SMTQuantifier),[ct('!',forall),ct('?',exists),
ct(':=',SMTLet)]),
    ( Quantifier == ':=' ->
        smt_let_type(Variables,Formula,SMTLet)
    ; true).

smt2_quantified_formula(QuantifiedFormula,SMTQuantifier,Variables,
Formula):-
    smt_quantified_formula(QuantifiedFormula,SMTQuantifier,Variables,
Formula).
%------------------------------------------------------------------------------
%----Recognise and split up binary formulae
smt_binary_formula(BinaryFormula,SMTConnective,LHS,RHS):-
    BinaryFormula =.. [Connective,LHS,RHS],
    tptp2X_member((Connective)-(SMTConnective),
[('<=>')-('iff'),('<~>')-('xor'),('=>')-('implies'),('&')-('and'),
(';')-('or'),('|')-('or'),('>')-(''),('*')-('')]).
%----Both needed after shorten which sets '|' somewhere - need to check
%----|;BUG

smt2_binary_formula(BinaryFormula,SMTConnective,LHS,RHS):-
    BinaryFormula =.. [Connective,LHS,RHS],
    tptp2X_member((Connective)-(SMTConnective),
[('<=>')-('='),('<~>')-('xor'),('=>')-('=>'),('&')-('and'),
(';')-('or'),('|')-('or'),('>')-(''),('*')-('')]).
%------------------------------------------------------------------------------
%----Recognise and split up unary formulae
smt_unary_formula(UnaryFormula,SMTConnective,Formula):-
    UnaryFormula =.. [Connective,Formula],
%----Had to use ()s due to some Prolog confusion about -
    tptp2X_member((Connective)-(SMTConnective),[('~')-('not')]).

smt2_unary_formula(UnaryFormula,SMTConnective,Formula):-
    smt_unary_formula(UnaryFormula,SMTConnective,Formula).
%------------------------------------------------------------------------------
%----The format for outputing quantified formulae in oscar format
%----FormulaPrefix,VariablesPrefix,VariablesSeparator,VariablesSuffix,
%----FormulaSuffix
smt_quantified_format('(',' ',' ',' ',' )').

%----PrefixBracket,ConnectivePrefix,ConnectiveSuffix,SuffixBracket
smt_binary_format('(','','',' )').

%----FormulaPrefix,ConnectiveSuffix,FormulaSuffix
smt_unary_format('(',' ',' )').
%------------------------------------------------------------------------------
%----The format for outputing quantified formulae in oscar format
%----FormulaPrefix,VariablesPrefix,VariablesSeparator,VariablesSuffix,
%----FormulaSuffix
smt2_quantified_format('(',' (',' ',') ',' )').

%----PrefixBracket,ConnectivePrefix,ConnectiveSuffix,SuffixBracket
smt2_binary_format('(','','',' )').

%----FormulaPrefix,ConnectiveSuffix,FormulaSuffix
smt2_unary_format('(',' ',' )').
%------------------------------------------------------------------------------
%----Write extrasorts-declarations for the sort symbols occuring in a type;
output_smt_extrasorts([]):-
    !.

output_smt_extrasorts([Type|Types]):-
    !,
    write(':extrasorts ('),
    output_prefix_formula(smt,Type,0,0,outermost),
    write(')'),nl,
    output_smt_extrasorts(Types).

%------------------------------------------------------------------------------
%----Write extrasorts-declarations for the sort symbols occuring in a type;
output_smt2_extrasorts([]):-
    !.

output_smt2_extrasorts([Type|Types]):-
    !,
    write('(declare-sort '),
    output_prefix_formula(smt,Type,0,0,outermost),
    write(' 0)'),nl,
    output_smt2_extrasorts(Types).

%------------------------------------------------------------------------------
%----If the formulae has an acceptable status then output
output_smt_formulae([]).

%----Ignore type signature for sorts
output_smt_formulae([tff(_,type,_:'$tType')|RestOfFormulae]):-
    !,
    output_smt_formulae(RestOfFormulae).

%----Type signature for proposition
output_smt_formulae([tff(Name,type,F:Boolean)|RestOfFormulae]):-
    tptp2X_member(Boolean,['$o','$oType']),
    !,
    write('; '),
    write(Name),nl,
    write(':extrapreds(('),
    smt_output_term(smt,F),
    write('))'),nl,
    nl,
    output_smt_formulae(RestOfFormulae).

%----Type signature for predicate
output_smt_formulae([tff(Name,type,F:(Type>Boolean))|RestOfFormulae]):-
    tptp2X_member(Boolean,['$o','$oType']),
    !,
    write('; '),
    write(Name),nl,
    write(':extrapreds (('),
    smt_output_term(smt,F),
    write(' '),
    output_prefix_formula(smt,Type,0,0,outermost),
    write('))'),nl,
    nl,
    output_smt_formulae(RestOfFormulae).

%----Type signature for functions
output_smt_formulae([tff(Name,type,F:Type)|RestOfFormulae]):-
    !,
    write('; '),
    write(Name),nl,
    write(':extrafuns (('),
    smt_output_term(smt,F),
    write(' '),
    output_prefix_formula(smt,Type,0,0,outermost),
    write('))'),nl,
    nl,
    output_smt_formulae(RestOfFormulae).

%----Conjecture
output_smt_formulae([tff(Name,conjecture,Formula)|RestOfFormulae]):-
    !,
    write(';----This is the conjecture with negated conjecture'),
    write('; '),
    write(Name),nl,
    write(':formula '),nl,
    output_generic_prefix_formula(smt,~ (Formula)),nl,
    nl,
    output_smt_formulae(RestOfFormulae).

output_smt_formulae([fof(Name,conjecture,Formula)|RestOfFormulae]):-
    !,
    write(';----This is the conjecture with negated conjecture'),
    write('; '),
    write(Name),nl,
    write(':formula '),nl,
    output_generic_prefix_formula(smt,~ (Formula)),nl,
    nl,
    output_smt_formulae(RestOfFormulae).

%----Everything else assumed to be axiom-like
output_smt_formulae([tff(Name,_,Formula)|RestOfFormulae]):-
    write('; '),
    write(Name),nl,
    write(':assumption '),nl,
    output_generic_prefix_formula(smt,Formula),nl,
    nl,
    output_smt_formulae(RestOfFormulae).

output_smt_formulae([fof(Name,_,Formula)|RestOfFormulae]):-
    write('; '),
    write(Name),nl,
    write(':assumption '),nl,
    output_generic_prefix_formula(smt,Formula),nl,
    nl,
    output_smt_formulae(RestOfFormulae).

%------------------------------------------------------------------------------
%----If the formulae has an acceptable status then output
output_smt2_formulae([]).

%----Ignore type signature for sorts
output_smt2_formulae([tff(_,type,_:'$tType')|RestOfFormulae]):-
    !,
    output_smt2_formulae(RestOfFormulae).

%----Type signature for predicate
output_smt2_formulae([tff(Name,type,F:(Arguments>Result))|RestOfFormulae]):-
    !,
    write('; '),
    write(Name),nl,
    write('(declare-fun '),
    smt_output_term(smt2,F),
    write(' ('),
    output_prefix_formula(smt2,Arguments,0,0,outermost),
    write(') '),
    output_prefix_formula(smt2,Result,0,0,outermost),
    write(')'),nl,
    nl,
    output_smt2_formulae(RestOfFormulae).

%----Type signature for functions
output_smt2_formulae([tff(Name,type,F:Type)|RestOfFormulae]):-
    !,
    write('; '),
    write(Name),nl,
    write('(declare-fun '),
    smt_output_term(smt2,F),
    write(' () '),
    output_prefix_formula(smt2,Type,0,0,outermost),
    write(')'),nl,
    nl,
    output_smt2_formulae(RestOfFormulae).

%----Conjecture
output_smt2_formulae([Conjecture|RestOfFormulae]):-
    Conjecture =.. [Form,Name,conjecture,Formula|_],
    tptp2X_member(Form,[tff,fof]),
    !,
    write(';----This is the conjecture with negated conjecture'),
    write('; '),
    write(Name),nl,
    write('(assert '),nl,
    output_generic_prefix_formula(smt2,~ (Formula)),nl,
    write(')'),nl,
    nl,
    output_smt2_formulae(RestOfFormulae).

%----Everything else assumed to be axiom-like
output_smt2_formulae([AxiomLike|RestOfFormulae]):-
    AxiomLike =.. [Form,Name,_,Formula|_],
    tptp2X_member(Form,[tff,fof]),
    write('; '),
    write(Name),nl,
    write('(assert '),nl,
    output_generic_prefix_formula(smt2,Formula),nl,
    write(')'),nl,
    nl,
    output_smt2_formulae(RestOfFormulae).

%------------------------------------------------------------------------------
%----This is done in the context of a setof, so allow backtracking
smt_extract_sort_from_declaration(Type: '$tType',Type):-
    !.

smt_extract_sort_from_declaration(_: Signature,Type):-
    smt_extract_sort_from_declaration(Signature,Type).

smt_extract_sort_from_declaration(DefinedType,DefinedType):-
    atomic(DefinedType),
    \+ tptp2X_member(DefinedType,['$int','$rat','$real','$o','$oType']).

smt_extract_sort_from_declaration(Complex,Type):-
    \+ atomic(Complex),
    Complex =.. [F | ComponentTypes],
    !,
    tptp2X_member(F, [*, >]),
    tptp2X_member(Component,ComponentTypes),
    smt_extract_sort_from_declaration(Component,Type).

%------------------------------------------------------------------------------
add_individuals_if_missing(Types,Types):-
    tptp2X_member('$i',Types),
    !.

add_individuals_if_missing(Types,['$i'|Types]).
%------------------------------------------------------------------------------
smt_extract_extrasorts(TypeDeclarations,FinalExtraTypes):-
    tptp2X_findall_setof1(ExtraType,
        (   tptp2X_member(tff(_,type,Declaration),TypeDeclarations),
            smt_extract_sort_from_declaration(Declaration,ExtraType)
        ),
    ExtraTypes),
%----smt_individual is used for $i, and can be implicit if equality is used.
    add_individuals_if_missing(ExtraTypes,FinalExtraTypes).
%------------------------------------------------------------------------------
%----Output all the formulae in smt format
smt(smt,Formulae,_):-
    tptp_formulae_language(Formulae,[tff,fof]),
    !,
    write('(benchmark name'),nl,
    write(':logic UFNIRA'),nl,
    nl,
    tptp_complete_types(Formulae,CompletedTypeDeclarations,
CompletedLogicalFormulae),
%DEBUG write('-- CTDs '),write(CompletedTypeDeclarations),nl,
    smt_extract_extrasorts(CompletedTypeDeclarations,ExtraTypes),
%DEBUG write('-- ExSs '),write(ExtraTypes),nl,
    output_smt_extrasorts(ExtraTypes),
    nl,
    output_smt_formulae(CompletedTypeDeclarations),
    nl,
    output_smt_formulae(CompletedLogicalFormulae),
    write(')'),nl.

smt(Format,_,_):-
    write('ERROR: Cannot format that type of problem for '),
    write(Format),nl.

smt2(smt2,Formulae,_):-
    tptp_formulae_language(Formulae,[tff,fof]),
    !,
    write('(set-logic UFNIRA)'),nl,
    nl,
    tptp_complete_types(Formulae,CompletedTypeDeclarations,
CompletedLogicalFormulae),
%DEBUG write('-- CTDs '),display(CompletedTypeDeclarations),nl,
%DEBUG write('-- CLFs '),display(CompletedLogicalFormulae),nl,
    smt_extract_extrasorts(CompletedTypeDeclarations,ExtraTypes),
%DEBUG write('-- ExSs '),write(ExtraTypes),nl,
    output_smt2_extrasorts(ExtraTypes),
    nl,
    output_smt2_formulae(CompletedTypeDeclarations),
    nl,
    output_smt2_formulae(CompletedLogicalFormulae),
    write('(check-sat)'),nl.

smt2(Format,_,_):-
    write('ERROR: Cannot format that type of problem for '),
    write(Format),nl.

%------------------------------------------------------------------------------
%----Provide information about the smt format
smt_format_information(';','.smt').
smt2_format_information(';','.smt2').
%------------------------------------------------------------------------------
%----Provide information about the smt file
smt_file_information(format,smt,'SMT format').
%------------------------------------------------------------------------------
