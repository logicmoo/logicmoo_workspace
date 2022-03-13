
:- module(unity_prolog_prims,[]).

:- use_module(library(logicmoo_utils)).



%=autodoc
%% def_prolog_prim_te( ?X, ?A4) is semidet.
%
% Def Prolog Prim Te.
%
def_prolog_prim_te(X,A4):- compound(X),
  compound_name_arguments(X,def_prolog_prim,[A,B,C|Rest]), 
  prolog_load_context(variable_names,G),maplist(call,G),
  compound_name_arguments(A4,def_prolog_prim,[A,B,C,Rest]).

:- style_check(- singleton).

:- set_prolog_flag_until_eof(allow_variable_name_as_functor,true).
:- multifile(term_expansion/2).
:- dynamic(term_expansion/2).
:- asserta_until_eof((term_expansion(X,A4):- def_prolog_prim_te(X,A4))).



%=autodoc
%% def_prolog_prim( ?Symbol, ?AndImplementation, ?Flow control, ?True if both goals are true., ?:goal1, ?:goal2) is semidet.
%
% Def Prolog Prim.
%
def_prolog_prim(Symbol.Comma, AndImplementation, "flow control", "True if both goals are true.", ":goal1",
                ":goal2").
def_prolog_prim(".", OrImplementation, "flow control", "True if both goals are true.", ":goal1", ":goal2").
def_prolog_prim("->", IfThenImplementation, "flow control", "Proves CONSEQUENT if TEST is true.", ":test",
                ":consequent").


%=autodoc
%% def_prolog_prim( ?Not, ?NotImplementation, ?Flow control, ?True if GOAL is unprovable.  GOAL must be ground., ?*goal) is semidet.
%
% Def Prolog Prim.
%
def_prolog_prim("not", NotImplementation, "flow control", "True if GOAL is unprovable.  GOAL must be ground.", "*goal").
def_prolog_prim("\\+", NotPlusImplementation, "flow control", "True if GOAL is unprovable", ":goal").
def_prolog_prim("once", OnceImplementation, "flow control,meta-logical predicates",
                "Attempts to prove GOAL, but suppresses backtracking for a second solution.", ":goal").
def_prolog_prim("ignore", IgnoreImplementation, "flow control,meta-logical predicates",
                "Attempts to prove GOAL, but suppresses backtracking for a second solution.",
                ":goals", "...").


%=autodoc
%% def_prolog_prim( ?Symbol, :CallCallImplementation, ?Flow control,meta-logical predicates, ?Attempts to prove the specified GOAL, adding any additional arguments, if specified., ?:goal, ??optionalArguments, ?...) is semidet.
%
% Def Prolog Prim.
%
def_prolog_prim(Symbol.Call, CallImplementation, "flow control,meta-logical predicates",
                "Attempts to prove the specified GOAL, adding any additional arguments, if specified.",
                ":goal", "?optionalArguments", "...").
def_prolog_prim("apply", ApplyImplementation, "flow control,meta-logical predicates",
                "Adds arguments in ARGLIST to end of GOAL and attempts to prove the resulting goal.", ":goal",
                "+arglist").
def_prolog_prim("randomize", RandomizeImplementation, "meta-logical predicates",
                "Proves GOAL, while randomizing clause order for clauses declared randomizable.", ":goal").
def_prolog_prim("begin", BeginImplementation, "flow control,meta-logical predicates",
    "Runs each goal in sequence, throwing an exception if any goal fails.  Cannot be backtracked.",
    ":goal", "...").
def_prolog_prim(Symbol.Dot, MethodCallImplementation, "flow control",
    "Calls the specified method of the specified object.",
    "*object", "method(*arguments...)").
def_prolog_prim("::", ModuleCallImplementation, "flow control",
    "Attempts to prove the specified goal in the specified module.",
    "*module", ":goal").
def_prolog_prim("freeze", FreezeImplementation, "flow control,constraint programming",
                "Runs GOAL when VAR becomes bound; unification will fail if GOAL fails.", "?var", ":goal").
def_prolog_prim("frozen_u", FrozenImplementation, "flow control,constraint programming",
                "Unifies GOAL with the goal frozen_u on TERM, if TERM is an unbound variable with a frozen_u goal; otherwise unifies GOAL with true.", "?term", "-goal").
def_prolog_prim("dif", DifImplementation, "comparisons,constraint programming",
                "Requires that TERM1 and TERM2 never be equal.  If they are, the predicate fails.  If they are not, it forces any future unifications that would make them equal to fail.", "?term1", "?term2").
def_prolog_prim("maplist", MapListImplementation, "list predicates,meta-logical predicates",
                "True if PREDICATE is true of all successive pairs of elements from LIST1 and LIST2.",
                ":predicate", "?list1", "?list2").
def_prolog_prim("bind",
                BindImplementation,
                "declarations",
                "Dynamically binds IndexicalName to Value until this goal is backtracked.",
                "*IndexicalName", "=Value").
def_prolog_prim("indexical",
                IndexicalImplementation,
                "declarations",
                "Declares that the specified name is an indexical name that can be bound using bind.",
                "*Name=*DefaultValue").
def_prolog_prim("indexical_named",
                IndexicalNamedImplementation,
                "term manipulation",
                "The INDEXICAL is an indexical object with name NAME.",
                "?name","?indexical").
def_prolog_prim("randomizable",
                MakeDeclarationPredicate((p, context) => context.KnowledgeBase.DeclareRandomizable(p)),
                "flow control,declarations",
                "Declares that the specified predicate is allowed to have its clauses explored in random order, when clause randomization is enabled.",
                ":predicateIndicator", "...").
def_prolog_prim("shadow",
                MakeDeclarationPredicate((p, context) => context.KnowledgeBase.DeclareShadow(p)),
                "flow control,declarations",
                "Declares that declarations for the specified predicate are allowed in this knowledgebase and override any declarations in the parent.",
                ":predicateIndicator", "...").
def_prolog_prim("external",
                MakeDeclarationPredicate((p, context) => context.KnowledgeBase.DeclareExternal(p)),
                "flow control,declarations",
                "Declares that the specified predicate is optional to define and/or defined elsewhere; thus, it should not generate undefined predicate warnings.",
                ":predicateIndicator", "...").
def_prolog_prim("public",
                MakeDeclarationPredicate((p, context) => context.KnowledgeBase.DeclarePublic(p)),
                "flow control,declarations",
                "Declares that the specified predicate is expected to be called from elsewhere.  It should not generate unreferenced predicate warnings.",
                ":predicateIndicator", "...").
def_prolog_prim("higher_order",
                DeclareHigherOrderImplementation,
                "flow control,declarations",
                "Declares that the specified predicate is may call its arguments as subgoals.  For example, higher_order(find_all(0,1,0)) means find_all/3 calls its second argument.  Used by the static checker to weed out unreferenced predicates.",
                ":predicateIndicator", "...").
def_prolog_prim("disassemble",
    MakeDeclarationPredicate((p, context) => context.KnowledgeBase.Disassemble(p)),
    "flow control",
    "Prints bytecode for a compiled predicate.",
    ":predicateIndicator", "...").
def_prolog_prim("compile",
    MakeDeclarationPredicate((p, context) => context.KnowledgeBase.Compile(p)),
    "flow control",
    "Declares that the predicate should be byte compiled rather than interpreted.",
    ":predicateIndicator", "...").
def_prolog_prim("predicate_property", PredicatePropertyImplementation, "declarations",
                "True if PROPERTY holds of the predicate of GOAL.",
                ":goal", "+property").
def_prolog_prim("forall", ForAllImplementation, "all solutions predicates",
                "True if GOAL is true for all bindings of all solutions of GENERATOR.",
                ":generator", ":goal").
def_prolog_prim("for_all_unique", ForAllUniqueImplementation, "all solutions predicates",
                "True if GOAL is true given variable bindings of each unique value of TEMPLATE produced by GENERATOR.",
                "-Template", ":generator", ":goal").
def_prolog_prim("generate_unique", GenerateUniqueImplementation, "all solutions predicates",
                "Succeeds once for each unique value of TEMPLATE produced by GENERATOR.",
                "-Template", ":generator").
def_prolog_prim("findall", FindallImplementation, "all solutions predicates",
                "Unifies SOLUTIONS with a list of every value of TEMPLATE for every possible solution of GOAL.",
                "=template", ":goal", "-solutions").


%=autodoc
%% def_prolog_prim( ?Findnsols, +FindNSolsImplementation, ?All solutions predicates, ?Unifies SOLUTIONS with a list of every value of TEMPLATE for every possible solution of GOAL.  Finds at most N solutiosn., ?*n, ?=template, ?:goal, ?-solutions) is semidet.
%
% Def Prolog Prim.
%
def_prolog_prim("findnsols", FindNSolsImplementation, "all solutions predicates",
                "Unifies SOLUTIONS with a list of every value of TEMPLATE for every possible solution of GOAL.  Finds at most N solutiosn.",
                "*n", "=template", ":goal", "-solutions").
def_prolog_prim("all", AllImplementation, "all solutions predicates",
                "Unifies SOLUTIONS with a list of every unique value of TEMPLATE for every possible solution of GOAL.",
                "=template", ":goal", "-solutions").
def_prolog_prim("sumall", SumallImplementation, "all solutions predicates",
                "Unifies SUM with sum of the values of NUMBERVAR in every possible solution of GOAL.",
                "-numberVar", ":goal", "-sum").
def_prolog_prim("arg_min", ArgMinImplementation, "all solutions predicates",
                "Find the value of TEMPLATE that gives the lowest SCORE among all solutions to GOAL.",
                ">template", "-score", "+goal").
def_prolog_prim("arg_max", ArgMaxImplementation, "all solutions predicates",
                "Find the value of TEMPLATE that gives the highest SCORE among all solutions to GOAL.",
                ">template", "-score", "+goal").
def_prolog_prim("property", PropertyImplementation, ".net interoperation",
                "Unifies VALUE with the value of OBJECT's property named PROPERTY_NAME.Always succeeds exactly once (unless it throws an exception).",
                "*object", "*property_name", ">value").
def_prolog_prim("set_property", SetPropertyImplementation, ".net interoperation",
                "Sets OBJECT's property named PROPERTY_NAME to NEW_VALUE.  Always succeeds exactly once (unless it throws an exception).",
                "*object", "*property_name", "*new_value").
def_prolog_prim("call_method", CallMethodImplementation, ".net interoperation",
                "Calls the specified method on OBJECT with the specified arguments and unifies RESULT with its return value.  Always succeeds exactly once (unless it throws an exception).",
                "*object", "*method_and_args", ">result").
def_prolog_prim("is_class", IsClassImplementation, ".net interoperation",
                "True if OBJECT is of the specified CLASS.  If CLASS is a subclass of TwigMetaverseComponent and OBJECT is uninstantiated, then it will enumerate objects if the specified type.",
                "?object", "?class").
def_prolog_prim("component_of_metaverse_object_with_type", ComponentOfMetaverseObjectWithTypeImplementation, ".net interoperation",
                "True if component is a component of metaverseobject with type class.",
                "?component", "?metaverseobject", "+class").
def_prolog_prim("parent_of_metaverse_object", ParentOfMetaverseObjectImplementation, ".net interoperation",
                "True if CHILD is a child of PARENT in the metaverse's rendering hierarchy.",
                "?child", "?parent").
def_prolog_prim("discontiguous", (args1, context1) => CutStateSequencer.Succeed(), "declarations",
                "Declares that the specified predicate is allowed to be scattered through a file.  Currently unused but provided for compatibility with other Prolog implementation.",
                ":predicateIndicator", "..."). %// noop
def_prolog_prim("multifile", (args2, context2) => CutStateSequencer.Succeed(), "declarations",
                "Declares that the specified predicate is allowed to be scattered through multiple files.  Currently unused but provided for compatibility with other Prolog implementation.",
                ":predicateIndicator", "..."). %// noop
def_prolog_prim("dynamic", MakeDeclarationPredicate( (p, context) => context.KnowledgeBase.DeclareExternal(p)),
                "declarations",
                "Declares that the specified predicate is allowed be dynamically modified using assert.  Currently unused but provided for compatibility with other Prolog implementation.",
                ":predicateIndicator", "..."). %// noop
def_prolog_prim("trace", TraceImplementation,
                "flow control,declarations",
                "Declares that the specified predicate should be traced when executing.",
                ":predicateIndicator", "...").
def_prolog_prim("notrace", NoTraceImplementation,
                "flow control,declarations",
                "Declares that the specified predicate should be traced when executing.",
                ":predicateIndicator", "...").
def_prolog_prim("pause_metaverse", PauseImplementation,
                "flow control",
                "Pauses the metaverse, leaving GUI, etc. running.",
                ":predicateIndicator", "...").
def_prolog_prim("unpause_metaverse",
                UnpauseImplementation,
                "flow control",
                "Restores normal flow of time in metaverse.",
                ":predicateIndicator", "...").
def_prolog_prim("set_prolog_flag", SetPrologFlagImplementation, "declarations", "Sets/gets value of the specified control parameter for the prolog system.",
                "*flag", "?value").
def_prolog_prim("check", CheckImplementation, "other predicates",
                "Checks that Goal is true, and throws an exception if it fails.  Only succeeds once, so similar to once/1.",
                ":goal").


%=autodoc
%% def_prolog_prim( ?Symbol, ?CutImplementation, ?Flow control,meta-logical predicates, ?Prohibits backtracking past this point for the current goal.) is semidet.
%
% Def Prolog Prim.
%
def_prolog_prim(Symbol.Cut, CutImplementation, "flow control,meta-logical predicates",
                "Prohibits backtracking past this point for the current goal.").
def_prolog_prim(Symbol.Fail, ((args, context) => FailImplementation), "flow control",
                "Forces failure of the current goal.").
def_prolog_prim("true", (args3, context3) => CutStateSequencer.Succeed(), "flow control", "Always succeeds.").
def_prolog_prim("repeat", RepeatImplementation, "flow control", "Always succeeds, and allows infinite backtracking.").
def_prolog_prim("throw", ThrowImplementation, "flow control,meta-logical predicates",
                "Throws the specified exception.",
                "+exception"). 
def_prolog_prim("catch", CatchImplementation, "flow control,meta-logical predicates",
                "Attempts to prove the specified GOAL, catching exceptions.  If an exception is thrown, it is unified with EXCEPTION and RECOVER is run.",
                ":goal", "=exception", ":recover").
def_prolog_prim("is", IsImplementation, "arithmetic",
                "Computes the value of FUNCTIONAL_EXPRESSION and unifies it with VARIABLE.  Expression must be fully instantiated, i.e. all variables in it must already have values.",
                ">variable", "*functional_expression").
def_prolog_prim("=", EqualsImplementation, "comparisons", "Succeeds if the two terms are unifiable.", "?x",
                "?y").
def_prolog_prim("unifiable", UnifiableImplementation, "comparisons",
                "True if X and Y can be unified, but does not unify them.  Instead returns the most general unifier in UNIFIER.",
                "?x", "?y", "-unifier").
def_prolog_prim("\\=", NotEqualsImplementation, "comparisons",
                "Succeeds if the two terms are not unifiable.", "?x", "?y").
def_prolog_prim("==", EquivalentImplementation, "comparisons", "Succeeds if the two terms are already identical, as opposed to =, which tries to make them identical through unification.", "?x",
                "?y").
def_prolog_prim("\\==", NotEquivalentImplementation, "comparisons", "Succeeds if the two terms are not identical, as opposed to \\= which tests if it's possible to make them identical through unification.", "?x",
                "?y").
def_prolog_prim("copy_term", CopyTermImplementation, "term manipulation",
                "Makes a new copy of ORIGINAL with fresh variables, and unifies it with COPY.", "=original", "-copy").
def_prolog_prim("=..", UnivImplementation, "term manipulation,list predicates",
                "If TERM is instantiated, explodes it into a list: [Functor | Arguments] and unifies it with LIST; if TERM uninstantiated, converts LIST to a term and unifies it with TERM.",
                "?term", "?list").
def_prolog_prim("functor", FunctorImplementation, "term manipulation",
                "True if TERM has the specified FUNCTOR and ARITY.",
                "?term", "?functor", "?arity").
def_prolog_prim("arg", ArgImplementation, "term manipulation", "True if argument number ARG (counting from 1, not zero) of STRUCTURE is TERM.",
                "*arg", "+structure", "?argumentValue").
def_prolog_prim("list", ListImplementation, "list predicates", "True if X is a list.", "?x").
def_prolog_prim("length", LengthImplementation, "list predicates",
                "Unifies LENGTH with the length of LIST.  This is a true relation, so if LIST is uninstantiated, it will create lists with specified lengths.",
                "?list", "?length").
def_prolog_prim("member", MemberImplementation, "list predicates",
                "True if ELEMENT is an element of LIST.  This is a true relation, so if necessary, it will create new LISTS.",
                "?element", "?list").
def_prolog_prim("memberchk", MemberChkImplementation, "list predicates",
                "True if ELEMENT is an element of LIST, but will not backtrack different choices of the element.  This is a true relation, so if necessary, it will create new LISTS.",
                "?element", "?list").
def_prolog_prim("random_member", RandomMemberImplementation, "list predicates",
                "Unifies ELEMENT with elements of LIST in random order.",
                "?element", "+list").
def_prolog_prim("append", AppendImplementation, "list predicates",
                "True if JOINED is a list that starts with the elements of START and is followed by the elements of END.  This is a true relation, so it can be used to compute any argument from the others.",
                "?start", "?end", "?joined").
def_prolog_prim("reverse", ReverseImplementation, "list predicates",
                "True if the lists FORWARD and BACKWARD are reversed versions of one another.",
                "?forward", "?backward").
def_prolog_prim("flatten", FlattenImplementation, "list predicates",
                "True if FLATLIST contains all the atoms of LISTOFLISTS, in order.",
                "+listoflists", "?flatlist").
def_prolog_prim("prefix", PrefixImplementation, "list predicates", "True if LIST starts with PREFIX.",
                "?prefix", "?list").
def_prolog_prim("suffix", SuffixImplementation, "list predicates", "True if LIST ends with SUFFIX.",
                "?suffix", "?list").
def_prolog_prim("select", SelectImplementation, "list predicates",
                "True if X is an element of LIST_WITH and LIST_WITHOUT is LIST_WITH minus an occurance of X.",
                "?x", "?list_with", "?list_without").
def_prolog_prim("delete", DeleteImplementation, "list predicates", "True if HasNoXs is LIST without X.",
                "?list", "?x", "?HasNoXs").
def_prolog_prim("msort", MSortImplementation, "list predicates",
    "True if RESULT is unifiable with a sorted version of LIST.  Does not remove duplicates.",
    "+list", "-result").
def_prolog_prim("sort", SortImplementation, "list predicates",
    "True if RESULT is unifiable with a sorted version of LIST.  Removes duplicates.",
    "+list", "-result").
def_prolog_prim("keysort", KeySortImplementation, "list predicates",
    "LIST should be of the format [KEY-VALUE, ...].  True if RESULT is unifiable with a version of LIST sorted by its KEYs.  Does not remove duplicates.",
    "+list", "-result").
def_prolog_prim("<", MakeComparisonPredicate("<", (a, b) => a < b), "comparisons",
                "True if number X is less than Y.  Both must be ground.", "*x", "*y").
def_prolog_prim(">", MakeComparisonPredicate(">", (a, b) => a > b), "comparisons",
                "True if number X is greater than Y.  Both must be ground.", "*x", "*y").
def_prolog_prim("=<", MakeComparisonPredicate("<=", (a, b) => (a =< b)), "comparisons",
                "True if number X is less than or equal to Y.  Both must be ground.", "*x", "*y").
def_prolog_prim(">=", MakeComparisonPredicate(">=", (a, b) => a >= b), "comparisons",
                "True if number X is greater than or equal to Y.  Both must be ground.", "*x", "*y").
def_prolog_prim("@<", MakeTermComparisonPredicate("@<", a => a < 0), "comparisons",
    "True if term X is less than Y given Prolog's ordering on terms.  X and Y need not be numbers.", "?x", "?y").
def_prolog_prim("@>", MakeTermComparisonPredicate("@>", a => a > 0), "comparisons",
                "True if term X is greater than Y given Prolog's ordering on terms.  X and Y need not be numbers.", "?x", "?y").
def_prolog_prim("@=<", MakeTermComparisonPredicate("@<=", a => a @=< 0), "comparisons",
                "True if term X is less than or equal to Y given Prolog's ordering on terms.  X and Y need not be numbers.", "?x", "?y").
def_prolog_prim("@>=", MakeTermComparisonPredicate("@>=", a => a @>= 0), "comparisons",
                "True if term X is greater than or equal to Y given Prolog's ordering on terms.  X and Y need not be numbers.", "?x", "?y").

%// ReSharper disable CompareOfFloatsByEqualityOperator
def_prolog_prim("=\\=", MakeComparisonPredicate("=\\=", (a, b) => a \== b), "comparisons",
                "True if X and Y are different numbers.  Both must be ground.", "*x", "*y").
%// ReSharper restore CompareOfFloatsByEqualityOperator
%// ReSharper disable CompareOfFloatsByEqualityOperator
def_prolog_prim("=:=", MakeComparisonPredicate("=:=", (a, b) => a == b), "comparisons",
                "True if functional expressions X and Y have the same values.  Both must be ground.",
                "*x", "*y").
%// ReSharper restore CompareOfFloatsByEqualityOperator
def_prolog_prim("C", CPrimitiveImplementation, "definite clause grammars",
                "Used in implementation of DGCs.  True if LIST starts with WORD and continues with TAIL.",
                "?list", "?word", "?tail").
def_prolog_prim("var", MakeNullFailingTypePredicate("var", (x => (x is LogicVariable))),
                "meta-logical predicates", "True if X is an uninstantiated variable.", "?x").

:- op(1000,xfx,'||').

/*
%    // ReSharper disable once RedundantComparisonWithNull
def_prolog_prim("nonvar", MakeNullTestingTypePredicate("nonvar", (x => x==null || !(x is LogicVariable))),
                "meta-logical predicates",
                "True if X isn't an uninstantiated variable, that is, if it's instantiated to some term.",
                "?x").
def_prolog_prim("ground", MakeNullTestingTypePredicate("var", (Term.IsGround)),
                "meta-logical predicates", "True if X is a ground term, i.e. contains no unbound variables.", "?x").
def_prolog_prim("number", MakeNullFailingTypePredicate("number", (x => (x is float) || (x is int))), "type predicates",
                "True if X is a number.", "?x").
def_prolog_prim("integer", MakeNullFailingTypePredicate("integer", (x => (x is int))), "type predicates",
                "True if X is an integer.", "?x").
def_prolog_prim("float", MakeNullFailingTypePredicate("float", (x => ((x is float) || (x is double)))),
                "type predicates", "True if X is a floating-point number.", "?x").
%    // ReSharper disable once RedundantComparisonWithNull
def_prolog_prim("atomic", MakeNullTestingTypePredicate("atomic", (x => x == null || !(x is Structure))),
                "type predicates", "True if X is not a structured term, i.e. it's a number, symbol, etc..",
                "?x").
def_prolog_prim("string", MakeNullFailingTypePredicate("string", (x => (x is string))), "type predicates",
                "True if X is a string.", "?x").
def_prolog_prim("atom", MakeNullTestingTypePredicate("atom", (x => (x == null) || (x is Symbol))), "type predicates",
                "True if X is a symbol.", "?x").

*/

def_prolog_prim("symbol", MakeNullFailingTypePredicate("symbol", (x => (x is Symbol))), "type predicates",
                "True if X is a symbol.", "?x").
def_prolog_prim("compound", MakeNullFailingTypePredicate("compound", (x => (x is Structure))),
                "type predicates", "True if X is a structured term or list.", "?x").
def_prolog_prim("consult", ConsultImplementation, "loading code",
                "Reads the clauses in FILE and addds them to the database.", "*file", "[kb]").
def_prolog_prim("reconsult", ReconsultImplementation, "loading code",
                "Removes all clauses previously loaded from FILE, then reads the clauses in FILE and addds them to the database.",
                "*file").
def_prolog_prim("listing", ListingImplementation, "loading code,database manipulation", "Prints a listing of PREDICATE", "*predicate").
def_prolog_prim("asserta", AssertaImplementation, "database manipulation", "Adds TERM (a rule or fact) to the database as the first clause for the predicate.", "+term").
def_prolog_prim("assertz", AssertzImplementation, "database manipulation",
                "Adds TERM (a rule or fact) to the database as the last clause for the predicate.", "+term").
def_prolog_prim("assert", AssertzImplementation, "database manipulation",
                "Adds TERM (a rule or fact) to the database as the last clause for the predicate.  Same as assertz.", "+term").
def_prolog_prim("retractall", RetractAllImplementation, "database manipulation", "Removes all database entries whose heads unify with HEAD.", "+head").
def_prolog_prim("retract", RetractImplementation, "database manipulation", "Removes first database entry that unifies with TERM.", "+term").
def_prolog_prim("clause", ClauseImplementation, "database manipulation", "Unifies HEAD and BODY with entries in the database.", "+head", "?body").
def_prolog_prim("step_limit", StepLimitImplementation, "other predicates",
                "Gets/sets the maximum number of inference steps allowed.", "*maximum_steps").
def_prolog_prim("call_with_step_limit", CallWithStepLimitImplementation, "other predicates",
                "Runs GOAL, using at most MAXIMUM_STEPS.", "*maximum_steps", ":Goal").
def_prolog_prim("benchmark", BenchmarkImplementation, "other predicates",
                "Runs GOAL repeatedly, COUNT times.", "+goal", "*count").
def_prolog_prim("word_list", WordListImplementation, "definite clause grammars",
                "Parses/unparses STRING into a LIST of word.", "?string", "?list").
def_prolog_prim("register_lexical_item", RegisterLexicalItemImplementation, "definite clause grammars",
                "Adds WORD to list of words recognized by word_list.", "+word").
def_prolog_prim("string_representation", StringRepresentationImplementation, "other predicates",
                "Parses/unparses between TERM and STRING.", "?term", "?string").
def_prolog_prim("starts_with", StartsWithImplementation, "other predicates",
                "The string or symbol's name begins with the specified substring.", "*substring", "*string_or_symbol").
def_prolog_prim("ends_with", EndsWithImplementation, "other predicates",
                "The string or symbol's name end with the specified substring.", "*substring", "*string_or_symbol").
def_prolog_prim("starts_with_one_of", StartsWithOneOfImplementation, "other predicates",
                "The string or symbol's name begins with one of the characters in the specified string.", "*possible_first_chars_string", "*string_or_symbol").
def_prolog_prim("contains_substring", ContainsSubstringImplementation, "other predicates",
                "The string or symbol's name contains the specified string.", "*substring", "*string_or_symbol").
def_prolog_prim("plural_form", PluralFormImplementation, "other predicates",
                "String plural is the plural form of string singular, using the default rules for English plurals.", "*singular", "?plural").
def_prolog_prim("atom_string", AtomStringImplementation, "other predicates",
                "Atom has the same print name as string.", "?atom", "?string").
def_prolog_prim("metaverse_object_name", MetaverseObjectNameImplementation, "other predicates",
                "True when name_symbol is the name of metaverse_object.", "?metaverse_object", "?name_symbol").
def_prolog_prim("set", KnowledgeBaseVariable.SetImplementation, "other predicates,meta-logical predicates",
                "Forcibly asserts PREDICATE(VALUE) and retracts all other clauses for PREDICATE.",
                "*predicate", "*value").
def_prolog_prim("display", DisplayImplementation, "other predicates",
                "Prints arguments to the console; string arguments are not quoted.", "?argument", "...").
def_prolog_prim("displayln", DisplayLnImplementation, "other predicates",
                "Prints arguments to the console; string arguments are not quoted.", "?argument", "...").
def_prolog_prim("write", WriteImplementation, "other predicates",
                "Prints the value of OBJECT to the console.", "?objectOrStream", "[+Object]").
def_prolog_prim("writeln", WritelnImplementation, "other predicates",
                "Prints the value of OBJECT to the console, along with a newline.", "?objectOrStream", "[+Object]").
def_prolog_prim("nl", NLImplementation, "other predicates", "Prints a newline to the system console.").
def_prolog_prim("log", LogImplementation, "other predicates", "Prints TERMS as a line in the Unity console.",
    "?Term", "...").
def_prolog_prim("log_warning", LogWarningImplementation, "other predicates", "Prints TERMS as a line in the Unity console.",
    "?Term", "...").
def_prolog_prim("log_error", LogErrorImplementation, "other predicates", "Prints TERMS as a line in the Unity console.",
    "?Term", "...").
def_prolog_prim("break", BreakImplementation, "flow control", "Pauses metaverse within the Unity editor and prints TERMS as a line in the unity console.",
    "?Term", "...").
def_prolog_prim("break_cs", BreakCSharpImplementation, "flow control", "Breakpoints the Prolog interpreter itself.").
def_prolog_prim("op", DeclareOperator, "declarations",
                "Declares the type and priority of an infix, prefix, or postfix operator.",
                "*priority", "*type", "*operator").
def_prolog_prim("open", OpenImplementation, "other predicates", "Opens a file for input or output.",
                "*path", "*mode", "-stream").
def_prolog_prim("close", CloseImplementation, "other predicates", "Closes an open file.", "*stream").
def_prolog_prim("read", ReadImplementation, "other predicates", "Reads an expression from an open stream.",
                "*stream", "-term").
def_prolog_prim("shell", ShellImplementation, "other predicates",
                "Runs a shell command; disabled outside of editor builds.",
                "command", "arg_string").
def_prolog_prim(ELProlog.NonExclusiveOperator, ELNonExclusiveQueryImplementation, "eremic logic",
                "Succeeds if expression can be matched against the EL knowledgebase.",
                "parent", "key").
def_prolog_prim(ELProlog.ExclusiveOperator, ELExclusiveQueryImplementation, "eremic logic",
                "Succeeds if expression can be matched against the EL knowledgebase.",
                "parent", "key").
def_prolog_prim(">>", ELNodeQueryImplementation, "eremic logic",
                "Binds VARIABLE to the subtree of the EL KB matching EXPRESSION.",
                "*expression", "-variable").


