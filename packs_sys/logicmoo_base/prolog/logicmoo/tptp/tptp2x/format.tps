%============================================================================
%----This outputs TPTP FOF in TPS format
%----
%----Written by Geoff Sutcliffe, August 2003
%============================================================================
%----------------------------------------------------------------------------
translate_to_tps_symbol(Symbol,_,TPSSymbol):-
%----Need the ()s to allow translation of ! and ? - weird Prolog bug
    tptp2X_member((Symbol)-TPSSymbol,[
('~')-not,('NOT')-or,('|')-'OR',('&')-'AND',('=>')-'IMPLIES',('<=>')-'EQUIV',
('!')-'FORALL',('?')-'EXISTS',('^')-'LAMBDA',
%% NOT in TPS ('!!')-'forall-op',('??')-'exists-op',(':=')-letrec,
('$true')-'TRUTH',('$false')-'FALSEHOOD',
('$i')-'I',('$o')-'O',
('=')-('='),('!=')-('!=')
%% NOT in TPS ('$tType')-type,
%% NOT in TPS ('*')-tupletype,('+')-cotupletype
]),
    !.

%----Translation of types to uppercase letters
translate_to_tps_symbol(Type,TypeMap,TranslatedType):-
    tptp2X_member(Type-TranslatedType,TypeMap),
    !.

translate_to_tps_symbol(Symbol,_,Symbol).
%DEBUG write('translating '),write(Symbol),nl,
%    name(Symbol,SymbolASCII),
%    tptp2X_append(SymbolASCII,[124],SymbolASCIIBar),
%    name(TranslatedSymbol,[124|SymbolASCIIBar]).
%----------------------------------------------------------------------------
find_tps_variable_mapping(VariableMap,Variable,MappedTo):-
    tptp2X_member(MappedVariable-MappedTo,VariableMap),
    MappedVariable == Variable.
%----------------------------------------------------------------------------
%----Extend the map if there is a name clash
extend_tps_variable_map(VariableMap,Variable:_,ExtendedVariableMap):-
    !,
    extend_tps_variable_map(VariableMap,Variable,ExtendedVariableMap).

extend_tps_variable_map(VariableMap,Variable,[Variable-MappedTo|VariableMap]):-
    find_tps_variable_mapping(VariableMap,Variable,_),
    !,
    retract(tps_variable_index(Index)),
    NewIndex is Index+1,
    assert(tps_variable_index(NewIndex)),
    concatenate_atoms([Variable,'_',NewIndex],MappedTo).
    
%----Otherwise keep original name
extend_tps_variable_map(VariableMap,Variable,[Variable-Variable|VariableMap]).
%----------------------------------------------------------------------------
%----Variables with types or as definitions
output_tps_variable(Language,TypeOrDefnForm,TypeMap,VariableMap):-
    TypeOrDefnForm =.. [Separator,Variable,TypeOrDefn],
    tptp2X_member(Separator,[':']),
    !,
    output_tps_variable(Language,Variable,TypeMap,VariableMap),
    write('('),
    output_a_tps_formula(Language,TypeOrDefn,TypeMap,VariableMap,0,0),
    write(')').

output_tps_variable(_,Variable,_,VariableMap):-
    find_tps_variable_mapping(VariableMap,Variable,MappedTo),
    !,
    write(MappedTo).

output_tps_variable(_,Variable,_,_):-
    write(Variable).
%----------------------------------------------------------------------------
output_tps_symbol(_,Symbol,TypeMap):-
    translate_to_tps_symbol(Symbol,TypeMap,TPSSymbol),
    write(TPSSymbol).
%----------------------------------------------------------------------------
output_tps_formula_list(_,[],_,_,_,_).

output_tps_formula_list(Language,[Formula|RestOfFormulae],TypeMap,
VariableMap,Indent,AlreadyIndented):-
    output_a_tps_formula(Language,Formula,TypeMap,VariableMap,Indent,
AlreadyIndented),
    output_tps_formula_list(Language,RestOfFormulae,TypeMap,VariableMap,Indent,
AlreadyIndented).
%----------------------------------------------------------------------------
%DEBUG output_a_tps_formula(_,Formula,TM,VM,_,_):-nl,write('OATF--- '),write(Formula),write(' TM--- '),write(TM),write(' VM--- '),write(VM),nl,fail.

%----Variables done first to avoid being instantiated
output_a_tps_formula(Language,Variable,TypeMap,VariableMap,Indent,
AlreadyIndented):-
    looks_like_a_variable(Variable),
    !,
    output_indent(Indent,AlreadyIndented),
    output_tps_variable(Language,Variable,TypeMap,VariableMap).

%----Quantified. Break up multiple variables
output_a_tps_formula(Language,QuantifiedFormula,TypeMap,VariableMap,Indent,
AlreadyIndented):-
    tptp_quantified_formula(QuantifiedFormula,Quantifier,[FirstVariable,
SecondVariable|RestOfVariables],Formula),
    !,
    tptp_quantified_formula(InnerQuantifiedFormula,Quantifier,[SecondVariable|
RestOfVariables],Formula),
    tptp_quantified_formula(OuterQuantifiedFormula,Quantifier,[FirstVariable],
InnerQuantifiedFormula),
    output_a_tps_formula(Language,OuterQuantifiedFormula,TypeMap,VariableMap,
Indent,AlreadyIndented).

output_a_tps_formula(Language,QuantifiedFormula,TypeMap,VariableMap,Indent,
AlreadyIndented):-
    tptp_quantified_formula(QuantifiedFormula,Quantifier,[Variable],Formula),
    !,
    extend_tps_variable_map(VariableMap,Variable,NewVariableMap),
    output_indent(Indent,AlreadyIndented),
    write('[ '),
    output_tps_symbol(Language,Quantifier,TypeMap),
    write(' '),
    output_tps_variable(Language,Variable,TypeMap,NewVariableMap),
    nl,
    Indent2 is Indent + 2,
    output_a_tps_formula(Language,Formula,TypeMap,NewVariableMap,Indent2,0),
    write(' ]').

%----Application
output_a_tps_formula(Language,BinaryFormula,TypeMap,VariableMap,Indent,
AlreadyIndented):-
    tptp_binary_formula(BinaryFormula,'@',LHS,RHS),
    !,
    output_indent(Indent,AlreadyIndented),
    write('[ '),
    Indent2 is Indent + 2,
    output_a_tps_formula(Language,LHS,TypeMap,VariableMap,Indent2,Indent2),
    nl,
    output_a_tps_formula(Language,RHS,TypeMap,VariableMap,Indent2,0),
    write(' ]').
    
%----Mapping
output_a_tps_formula(Language,BinaryFormula,TypeMap,VariableMap,Indent,
AlreadyIndented):-
    tptp_binary_formula(BinaryFormula,'>',LHS,RHS),
    !,
    output_indent(Indent,AlreadyIndented),
%----Chad says "You can't put whitespace in the types"
    write('('),
    Indent2 is Indent + 2,
    output_a_tps_formula(Language,RHS,TypeMap,VariableMap,Indent2,Indent2),
    write(''),
    output_a_tps_formula(Language,LHS,TypeMap,VariableMap,Indent2,Indent2),
    write(')').

%----Binary formulae that TPS can't do ...
output_a_tps_formula(Language,BinaryFormula,TypeMap,VariableMap,Indent,
AlreadyIndented):-
    tptp_binary_formula(BinaryFormula,'<~>',LHS,RHS),
    !,
    output_a_tps_formula(Language,~(LHS <=> RHS),TypeMap,VariableMap,Indent,
AlreadyIndented).

output_a_tps_formula(Language,BinaryFormula,TypeMap,VariableMap,Indent,
AlreadyIndented):-
    tptp_binary_formula(BinaryFormula,'<=',LHS,RHS),
    !,
    output_a_tps_formula(Language,(RHS => LHS),TypeMap,VariableMap,Indent,
AlreadyIndented).

%----Binary Formula
output_a_tps_formula(Language,BinaryFormula,TypeMap,VariableMap,Indent,
AlreadyIndented):-
    tptp_binary_formula(BinaryFormula,BinaryConnective,LHS,RHS),
    !,
    output_indent(Indent,AlreadyIndented),
    write('[ '),
    Indent2 is Indent + 2,
    output_a_tps_formula(Language,LHS,TypeMap,VariableMap,Indent2,Indent2),
    nl,
    output_indent(Indent,0),
    output_tps_symbol(Language,BinaryConnective,TypeMap),
    nl,
    output_a_tps_formula(Language,RHS,TypeMap,VariableMap,Indent2,0),
    write(' ]').

%----Unary Formula
output_a_tps_formula(Language,UnaryFormula,TypeMap,VariableMap,Indent,
AlreadyIndented):-
    tptp_unary_formula(UnaryFormula,UnaryConnective,Formula),
    !,
    output_indent(Indent,AlreadyIndented),
    write('[ '),
    output_tps_symbol(Language,UnaryConnective,TypeMap),
    nl,
    Indent2 is Indent + 2,
    output_a_tps_formula(Language,Formula,TypeMap,VariableMap,Indent2,0),
    write(' ]').

%----Tuple

%----Equality
output_a_tps_formula(Language,'$tptp_equal'(LHS,RHS),TypeMap,VariableMap,Indent,
AlreadyIndented):-
    !,
    output_indent(Indent,AlreadyIndented),
    write('[ '),
    Indent2 is Indent + 2,
    output_a_tps_formula(Language,LHS,TypeMap,VariableMap,Indent2,Indent2),
    nl,
    output_indent(Indent,0),
    output_tps_symbol(Language,'=',TypeMap),
    write(' '),
    output_a_tps_formula(Language,RHS,TypeMap,VariableMap,Indent2,Indent2),
    write(' ]').

output_a_tps_formula(Language,'$tptp_not_equal'(LHS,RHS),TypeMap,VariableMap,
Indent,AlreadyIndented):-
    !,
    output_a_tps_formula(Language,~ $tptp_equal(LHS,RHS),TypeMap,VariableMap,
Indent,AlreadyIndented).

%----Atomic formulae
output_a_tps_formula(Language,Atom,TypeMap,_VariableMap,Indent,
AlreadyIndented):-
    atomic(Atom),
    !,
    output_indent(Indent,AlreadyIndented),
    output_tps_symbol(Language,Atom,TypeMap).

output_a_tps_formula(_,NonAtom,_,_,_,_):-
    nl,
    write('ERROR: Cannot output '),
    write(NonAtom),
    write(' in TPS format'),
    nl.
%----------------------------------------------------------------------------
%DEBUG output_tps_formulae([F|_],TM):-write('OTF--- '),write(F),nl,fail.

output_tps_formulae([],_).

%----Type assertions - declarations of types are replaced by TPS type names
output_tps_formulae([TypeFormula|RestOfFormulae],TypeMap):-
    TypeFormula =.. [Language,Name,type,Symbol: '$tType'|_],
    !,
    write(';----The type declaration '),
    write(Name),
    write(' has been removed'),
    nl,
    write(';----The type '),
    write(Symbol),
    write(' has been renamed to '),
    output_tps_symbol(Language,Symbol,TypeMap),
    nl,
    nl,
    output_tps_formulae(RestOfFormulae,TypeMap).

output_tps_formulae([TypeFormula|RestOfFormulae],TypeMap):-
    TypeFormula =.. [Language,Name,type,Symbol:Type|_],
    !,
    write(';----This is the type declaration '),
    write(Name),
    nl,
    write('(const "'),
    translate_to_tps_symbol(Symbol,TypeMap,TranslatedSymbol),
    write(TranslatedSymbol),
    write('" "'),
    output_a_tps_formula(Language,Type,TypeMap,[],4,4),
    write('" )'),
    nl,
    nl,
    output_tps_formulae(RestOfFormulae,TypeMap).

%----Top level definitions
output_tps_formulae([DefnFormula|RestOfFormulae],TypeMap):-
    DefnFormula =.. [Language,Name,definition,FormalOrEqualDefn|_],
    FormalOrEqualDefn =.. [Connective,Symbol,Definition],
    tptp2X_member(Connective,['$tptp_equal',':=']),
    !,
    write(';----This is the definition '),
    write(Name),
    nl,
    write('(def "'),
    translate_to_tps_symbol(Symbol,TypeMap,TranslatedSymbol),
    write(TranslatedSymbol),
    write('" "'),
    nl,
    output_a_tps_formula(Language,Definition,TypeMap,[],4,0),
    write('" )'),
    nl,
    nl,
    output_tps_formulae(RestOfFormulae,TypeMap).

output_tps_formulae([Conjecture|RestOfFormulae],TypeMap):-
    Conjecture =.. [Language,Name,conjecture,Formula|_],
    !,
    write(';----This is the '),
%----Hack to hide "This is the conjecture from SystemOnTPTP when $false
    (   Formula == '$false'
    ->  write('dummy ')
    ;   true
    ),
    write('conjecture '),
    write(Name),
    nl,
    write('(thm "'),
    write(Name),
    write('" "'),
    nl,
    output_a_tps_formula(Language,Formula,TypeMap,[],4,0),
    nl,
    write(' " )'),
    nl,
    nl,
    output_tps_formulae(RestOfFormulae,TypeMap).

%----Regular formulae
output_tps_formulae([NonConjecture|RestOfFormulae],TypeMap):-
    NonConjecture =.. [Language,Name,Role,Formula|_],
    !,
    write(';----This is the '),
    write(Role),
    write(' '),
    write(Name),
    nl,
    write('(axiom "'),
    nl,
    output_a_tps_formula(Language,Formula,TypeMap,[],4,0),
    nl,
    write(' " )'),
    nl,
    nl,
    output_tps_formulae(RestOfFormulae,TypeMap).
%----------------------------------------------------------------------------
next_tps_type(TPSType,NextTPSType):-
    name(TPSType,[TPSTypeASCII]),
    NextTPSTypeASCII is TPSTypeASCII + 1,
    name(PossibleNextTPSType,[NextTPSTypeASCII]),
    (   tptp2X_member(PossibleNextTPSType,['I','O','S'])
    ->  next_tps_type(PossibleNextTPSType,NextTPSType)
    ;   NextTPSType = PossibleNextTPSType
    ).
%----------------------------------------------------------------------------
make_tps_type_map([],_,[]).

make_tps_type_map([thf(_Name,type,Type: '$tType')|RestOfFormulae],TPSType,
[Type-TPSType|RestOfTypeMap]):-
    !,
    next_tps_type(TPSType,NextTPSType),
    make_tps_type_map(RestOfFormulae,NextTPSType,RestOfTypeMap).

make_tps_type_map([_|RestOfFormulae],TPSType,TypeMap):-
    make_tps_type_map(RestOfFormulae,TPSType,TypeMap).
%----------------------------------------------------------------------------
ensure_tps_conjecture(Formulae,FormulaeWithConjecture):-
    tptp2X_select(Conjecture,Formulae,NotConjectures),
    Conjecture =.. [_,_,conjecture,_|_],
    !,
    tptp2X_append(NotConjectures,[Conjecture],FormulaeWithConjecture).

ensure_tps_conjecture(Formulae,FormulaeWithConjecture):-
    tptp2X_append(Formulae,[thf(prove_false,conjecture,'$false')],
FormulaeWithConjecture).
%----------------------------------------------------------------------------
%----Output clause-tps-syntax
tps(tps,Clauses,_):-
    tptp_clauses(Clauses),
    !,
    write('ERROR: No CNF format available in TPS'),
    nl.

%----Output FOF-tps-syntax
tps(tps,Formulae,_):-
    tptp_formulae(Formulae),
    !,
    make_tps_type_map(Formulae,'A',TypeMap),
    ensure_tps_conjecture(Formulae,FormulaeWithConjecture),
    assert(tps_variable_index(0)),
    output_tps_formulae(FormulaeWithConjecture,TypeMap).
%----------------------------------------------------------------------------
%----Provide information about the TPS format
%----Have to suppress comments because the <?xml must come first
tps_format_information(';','.tps').
%----------------------------------------------------------------------------
%----Provide information about the TPS file
tps_file_information(format,tps,'TPS format').
%----------------------------------------------------------------------------
