:- module(foreign_generator, [generate_foreign_interface/2,
			      generate_library/3,
			      gen_foreign_library/2,
			      read_foreign_properties/8]).

:- use_module(library(swi/assertions)).
:- use_module(library(maplist_dcg)).
:- use_module(library(foreign/foreign_props)).

:- multifile
    gen_foreign_library/2,
    use_foreign_source/2,
    use_foreign_header/2,
    include_foreign_dir/2,
    extra_compiler_opts/2,
    link_foreign_library/2,
    pkg_foreign_config/2.

command_to_atom(Command, Args, Atom) :-
    process_create(path(Command), Args, [stdout(pipe(Out))]),
    read_stream_to_codes(Out, String),
    string_to_atom(String, Atom).

fortran_command(Command) :-
    command_to_atom(swipl, ['--dump-runtime-variables'], Atom),
    atomic_list_concat(AtomL, ';\n', Atom),
    findall(Value, ( member(NameValue, AtomL),
		     atomic_list_concat([Name|ValueL], '=', NameValue),
		     memberchk(Name, ['PLCFLAGS', 'PLLDFLAGS']),
		     atomic_list_concat(ValueL, '=', CValueC),
		     atom_concat('"', ValueC, CValueC),
		     atom_concat(Value, '"', ValueC)		     
		   ),
	    ValueL),
    atomic_list_concat([gfortran|ValueL], ' ', Command).

intermediate_obj(DirSO, Source, Object) -->
    {file_name_extension(Base, for, Source)},
    !,
    { file_base_name(Base, Name),
      file_name_extension(Name, o, NameO),
      directory_file_path(DirSO, NameO, Object),
      fortran_command(Fortran)
    },
    {atomic_list_concat([Fortran, '-c', Source, '-o', Object], ' ', Command)},
    [Command].
intermediate_obj(_, Source, Source) --> [].

is_newer(File1, File2) :-
    exists_file(File1),
    exists_file(File2),
    time_file(File1, Time1),
    time_file(File2, Time2),
    Time1 > Time2.

generate_library(M, AliasSO, File) :-
    absolute_file_name(AliasSO, FileSO),
    findall(FSource, ( use_foreign_source(M, FAlias),
		       absolute_file_name(FAlias, FSource)
		     ), FSourceL),
    ( forall(( member(Dep, [File|FSourceL])
	     ; use_foreign_header(M, HAlias),
	       absolute_file_name(HAlias, Dep)
	     ),
	     is_newer(FileSO, Dep))
    ->print_message(informational,
		    format('Skipping build of ~w: is up to date', [FileSO]))
    ; do_generate_library(M, FileSO, FSourceL)
    ).

do_generate_library(M, FileSO, FSourceL) :-
    file_name_extension(BaseFile, so, FileSO),
    generate_foreign_interface(M, BaseFile),
    absolute_file_name(library(foreign/foreign_interface), IntfPl,
		       [file_type(prolog), access(read)]),
    directory_file_path(DirIntf, _, IntfPl),
    directory_file_path(DirSO,   _, FileSO),
    maplist_dcg(intermediate_obj(DirSO), FSourceL, FTargetL, Commands, CommandsT),
    atomic_list_concat(FTargetL, ' ', FSources),
    findall(CLib, ( link_foreign_library(M, Lib),
		    atom_concat('-l', Lib, CLib)
		  ; pkg_foreign_config(M, Package),
		    command_to_atom('pkg-config', ['--libs', Package], CLib0),
		    atom_concat(CLib, '\n', CLib0)
		  ), CLibL),
    atomic_list_concat(CLibL, ' ', CLibs),
    findall(COpt, ( extra_compiler_opts(M, COpt)
		  ; pkg_foreign_config(M, Package),
		    command_to_atom('pkg-config', ['--cflags', Package], COpt0),
		    atom_concat(COpt, '\n', COpt0)
		  ), COptL),
    ( COptL = []
    ->COpts = ''
    ; atomic_list_concat(COptL, ' ', COpts)
    ),
    findall(IDir, ( ( Dir = DirSO
		    ; Dir = DirIntf
		    ; include_foreign_dir(M, DAlias),
		      absolute_file_name(DAlias, Dir, [file_type(directory)])
		    ),
		    atom_concat('-I', Dir, IDir)
		  ),
	    IDirL),
    atomic_list_concat(IDirL, ' ', IDirs),
    atomic_list_concat(['swipl-ld -shared ', COpts, IDirs, ' ', FSources,
			' ', BaseFile, '.c ', CLibs, ' -o ', FileSO],
		       CommandT),
    CommandsT = [CommandT],
    forall(member(Command, Commands),
	   ( shell(Command, Status),
	     ( Status==0
	     ->MessageType=informational
	     ; MessageType=error
	     ),
	     print_message(MessageType, format('`~w\' exited with status ~w',
					       [Command, Status]))
	   )).

generate_foreign_interface(Module, BaseFile) :-
    file_name_extension(BaseFile, h, File_h),
    file_name_extension(BaseFile, c, File_c),
    directory_file_path(_, Base, BaseFile),
    generate_foreign_h(Module, File_h),
    generate_foreign_c(Module, Base, File_c, File_h).

c_var_name(Arg, CArg) :-
    format(atom(CArg), '_c_~w', [Arg]).

generate_foreign_h(Module, File) :-
    setup_call_cleanup(tell(File), generate_foreign_h(Module), told).

generate_foreign_c(Module, Base, File_c, File_h) :-
    setup_call_cleanup(tell(File_c),
		       generate_foreign_c(Module, Base, File_h),
		       told).

generate_foreign_h(Module) :-
    write_header(Module),
    forall(current_type_props(Module, Type, _),
	   declare_type_converters(Module, Type)),
    collect_bind_head(Module, BindHeadL),
    maplist(declare_wrapper_bind, BindHeadL),
    nl,
    maplist(declare_foreign_bind(Module), BindHeadL),
    nl,
    format('#endif /* __~w_H */~n', [Module]).

add_autogen_note(Module) :-
    format('/* NOTE: File generated automatically from ~w */~n~n', [Module]).

generate_foreign_c(Module, Base, File_h) :-
    add_autogen_note(Module),
    format('#include "~w"\n\n', [File_h]),
    implement_type_converters(Module),
    generate_foreign_register(Module, Base),
    generate_foreign_wrapper(Module).

write_header(Module) :-
    add_autogen_note(Module),
    format('#ifndef __~w_H~n#define __~w_H~n~n', [Module, Module]),
    write('#include <SWI-Prolog.h>\n'),
    write('#include <foreign_interface.h>\n\n').

apply_name(Name=Value) :- ignore(Value='$VAR'(Name)).

apply_dict(Head, Dict) :-
    maplist(apply_name, Dict),
    numbervars(Head, 0, _, [singletons(false)]).

current_type_props(M, Type, ADict) :-
    assrt_lib:assertion_db(Type, M, _, prop, _, _, _, Glob, _, ADict),
    once(( member(TType, [type, regtype]),
	   memberchk(TType, Glob)
	 )).

declare_typedef_struct(func(Term), CSpecL, Name) :-
    format('typedef struct {~n', []),
    declare_type_members_each(1, CSpecL, Term),
    format('} ~w;~n', [Name]).
declare_typedef_struct(atom(_Term), [Spec], Name) :-
    format('typedef ', []),
    c_get_ctype_decl(Spec),
    format(' ~w;~n', [Name]).

declare_type_converters(M, Type) :-
    get_type_component_props(M, Type, Name, CSpecL, Result),
    declare_typedef_struct(Result, CSpecL, Name),
    format('int PL_get_~w(void**__root, term_t, ~w*);~n', [Name, Name]),
    format('int PL_unify_~w(term_t, ~w* const);~n~n', [Name, Name]).

declare_type_members_each(N, [Spec|CSpecL], Term) :-
    arg(N, Term, Arg),
    !,
    write('    '),
    c_get_ctype_decl(Spec),
    ( Spec = type(_) -> write('*') ; true),
    format(' ~w;~n', [Arg]),
    N1 is N + 1,
    declare_type_members_each(N1, CSpecL, Term).
declare_type_members_each(_, _, _).

implement_type_converters(M) :-
    forall(current_type_props(M, Type, ADict),
	   implement_type_converter(M, Type, ADict)).

implement_type_converter(M, Type, ADict) :-
    get_type_component_props(M, Type, Name, CSpecL, Result),
    apply_dict(Type, ADict),
    arg(1, Type, Arg),
    c_var_name(Arg, CArg),
    implement_type_getter( Name, CSpecL, Result, CArg, Arg),
    implement_type_unifier(Name, CSpecL, Result, CArg, Arg).

get_type_component_props(M, Type, Name, CSpecL, Result) :-
    Type =.. [Name, _   |Params],
    Head =.. [Name, Term|Params],
    once(clause(M:Head, Body, Ref)),
    ( clause_property(Ref, file(File)),
      clause_property(Ref, line_count(Line)),
      get_dictionary(Head :- Body, File, Line, M, Dict) -> true
    ; Dict = []
    ),
    ( nonvar(Term) ->
      Term =.. [_F|ArgL],
      Result = func(Term)
    ; ArgL = [Term],
      Result = atom(Term)
    ),
    sequence_list(Body, BodyL, []),
    apply_dict(Head, Dict),
    maplist(match_known_type(M), BodyL, CSpecL, ArgL).

implement_type_getter(Name, CSpecL, Term, CArg, Arg) :-
    format('int PL_get_~w(void** __root, term_t ~w, ~w *~w) {~n',
	   [Name, Arg, Name, CArg]),
    implement_type_getter_struct(Term, CArg, Arg, CSpecL),
    format('    return TRUE;~n}~n~n', []).

implement_type_getter_struct(func(Term), CArg, Arg, CSpecL) :-
    implement_type_getter_member(1, CArg, Arg, CSpecL, Term).
implement_type_getter_struct(atom(_Var), CArg, Arg, [Spec]) :-
    format('    ', []),
    c_get_argument(Spec, inout, deref, CArg, Arg),
    format(';~n', []).

implement_type_getter_member(N, CArg0, Arg0, [Spec|CSpecL], Term) :-
    arg(N, Term, MArg),
    !,
    format(atom(CArg), '~w->~w', [CArg0, MArg]),
    format(atom( Arg), '~w_~w', [ Arg0, MArg]),
    format('    term_t ~w=PL_new_term_ref();~n', [Arg]),
    format('    __rtcheck(PL_get_arg(~w,~w,~w));~n', [N, Arg0, Arg]),
    format('    ', []),
    (Spec = type(_) -> Mode = inout ; Mode = in),
    c_get_argument(Spec, Mode, deref, CArg, Arg),
    format(';~n', []),
    N1 is N + 1,
    implement_type_getter_member(N1, CArg0, Arg0, CSpecL, Term).
implement_type_getter_member(_, _, _, _, _).

implement_type_unifier(Name, CSpecL, Term, CArg, Arg) :-
    format('int PL_unify_~w(term_t ~w, ~w * const ~w) {~n', [Name, Arg, Name, CArg]),
    implement_type_unifier_(Term, CSpecL, CArg, Arg),
    format('    return TRUE;~n}~n~n', []).

implement_type_unifier_(func(Term), CSpecL, CArg, Arg) :-
    functor(Term, F, A),
    format('    __rtcheck(PL_unify_functor(~w, PL_new_functor(PL_new_atom("~w"), ~d)));~n',
	   [Arg, F, A]),
    implement_type_unifier_member(1, CArg, Arg, CSpecL, Term).
implement_type_unifier_(atom(_Var), [Spec], CArg, Arg) :-
    format('    ', []),
    c_set_argument(Spec, inout, CArg, Arg),
    format(';~n', []).    

implement_type_unifier_member(N, CArg0, Arg0, [Spec|CSpecL], Term) :-
    arg(N, Term, MArg),
    !,
    format(atom(CArg), '~w->~w', [CArg0, MArg]),
    format(atom( Arg), '~w_~w', [ Arg0, MArg]),
    format('    term_t ~w=PL_new_term_ref();~n', [Arg]),
    format('    __rtcheck(PL_get_arg(~w,~w,~w));~n', [N, Arg0, Arg]),
    format('    ', []),
    (Spec = type(_) -> Mode = inout ; Mode = out),
    c_set_argument(Spec, Mode, CArg, Arg),
    format(';~n', []),
    N1 is N + 1,
    implement_type_unifier_member(N1, CArg0, Arg0, CSpecL, Term).
implement_type_unifier_member(_, _, _, _, _).

collect_bind_head(Module, BindHeadL) :-
    findall(Bind-Head,
	    ( read_foreign_properties(Head, Module, _, _, _, _, Dict, Bind),
	      apply_dict(Head, Dict)
	    ), BindHeadUL),
    sort(BindHeadUL, BindHeadL).

declare_wrapper_bind(BindHead) :-
    declare_wrapper_head(BindHead),
    format(';~n', []).
    
declare_wrapper_head((CN/_A as _PN)-Head) :-
    format('foreign_t pl_~w(', [CN]),
    ( compound(Head)
    ->declare_wrapper_bind_(1, Head)
    ; true
    ),
    format(')', []).

declare_wrapper_bind_arg(Arg) :-
    format('term_t ~w', [Arg]).

declare_wrapper_bind_(N, Head) :-
    arg(N, Head, Arg),
    (N \= 1 -> write(', ') ; true),
    declare_wrapper_bind_arg(Arg),
    N1 is N + 1,
    !,
    declare_wrapper_bind_(N1, Head).
declare_wrapper_bind_(_, _).

declare_foreign_bind(M, Bind-Head) :-
    read_foreign_properties(Head, M, Comp, Call, Succ, Glob, _, Bind),
    ( memberchk(returns_state, Glob) ->
      format('int '), % int to avoid SWI-Prolog.h dependency at this level
      CHead = Head
    ; member(returns(Var), Glob) ->
      bind_argument(M, Comp, Call, Succ, Var, Spec, Mode),
      c_get_ctype_decl(Spec, Mode),
      Head =.. Args,
      once(select(Var, Args, CArgs)),
      CHead =.. CArgs
    ; format('void '),
      CHead = Head
    ),
    declare_foreign_head(Bind, M, CHead, Comp, Call, Succ, Glob),
    format(';~n', []).

declare_foreign_head((CN/_A as _PN), M, Head, Comp, Call, Succ, Glob) :-
    format(' ~w(', [CN]),
    (memberchk(memory_root, Glob) -> format('void** __root, ', []) ; true),
    ( compound(Head) ->
      declare_foreign_bind_(1, M, Head, Comp, Call, Succ)
    ; true
    ),
    format(')', []).

declare_foreign_bind_(N, M, Head, Comp, Call, Succ) :-
    arg(N, Head, Arg),
    ( N \= 1 -> write(', ') ; true ),
    declare_foreign_bind_arg(M, Comp, Call, Succ, Arg),
    format(' ~w', [Arg]),
    N1 is N + 1,
    !,
    declare_foreign_bind_(N1, M, Head, Comp, Call, Succ).
declare_foreign_bind_(_, _, _, _, _, _).

declare_foreign_bind_arg(M, Comp, Call, Succ, Arg) :-
    bind_argument(M, Comp, Call, Succ, Arg, Spec, Mode),
    c_get_ctype_decl(Spec, Mode),
    (Mode = in, Spec \= type(_) ->true ; write('*')),
    (Mode = in -> write(' const') ; true). % Ensure const correctness

c_get_ctype_decl(Spec, Mode) :-
    c_get_ctype_decl(Spec),
    c_get_ref_level(Mode, Spec).

c_get_ref_level(_,     term)    :- !.
c_get_ref_level(_,     list(_)) :- !. % Always ref
c_get_ref_level(in,    _).
c_get_ref_level(out,   _).
c_get_ref_level(inout, _) :- format('*', []).
				% Allow pointer to NULL,
				% the equivalent to free
				% variables in imperative
				% languages --EMM

c_get_ctype_decl(list(Spec)) :-
    c_get_ctype_decl(Spec),
    format('*', []).
c_get_ctype_decl(type(Name)) :-
    format('~w', Name).
c_get_ctype_decl(term) :-
    format('term_t', []).
c_get_ctype_decl(_-CType) :-
    format('~w', CType).

generate_foreign_register(Module, Base) :-
    format('install_t install_~w() {~n', [Base]),
    findall(Bind, read_foreign_properties(_, Module, _, _, _, _, _, Bind), BindUL),
    sort(BindUL, BindL),
    maplist(register_foreign_bind, BindL),
    format('} /* install_~w */~n~n', [Base]).

register_foreign_bind(CN/A as PN) :-
    format('    PL_register_foreign(\"~w\", ~w, pl_~w, 0);~n', [PN, A, CN]).

read_foreign_properties(Head, Module, Comp, Call, Succ, Glob, Dict, CN/A as PN) :-
    assrt_lib:assertion_db(Head, Module, check, pred, Comp, Call, Succ, Glob,
			   _Comm, Dict),
    functor(Head, PN, A),
    ( memberchk(foreign,     Glob) -> CN = PN
    ; memberchk(foreign(CN), Glob) -> true
    ).

generate_foreign_wrapper(Module) :-
    collect_bind_head(Module, BindHeadL),
    maplist(declare_wrapper_impl(Module), BindHeadL).

declare_wrapper_impl(Module, BindHead) :-
    declare_wrapper_head(BindHead),
    format(' {~n', []),
    format('    __mkroot(__root);~n'),
    bind_arguments(Module, BindHead, Return),
    BindHead = (PI as _)-_,
    format('    __delroot(__root);~n'),
    format('    return ~w;~n', [Return]),
    format('} /* ~w */~n~n', [PI]).

c_set_argument(list(Spec), _,    CArg, Arg) :- c_set_argument_list(Spec, CArg, Arg).
c_set_argument(type(Type), Mode, CArg, Arg) :- c_set_argument_type(Mode, Type, CArg, Arg).
c_set_argument(Type-_,     Mode, CArg, Arg) :- c_set_argument_one( Mode, Type, CArg, Arg).
c_set_argument(term,       _,    CArg, Arg) :- format('~w=~w', [Arg, CArg]).

c_set_argument_type(out,   Type, CArg, Arg) :-
    format('__rtc_PL_unify(~w, ~w, &~w)', [Type, Arg, CArg]).
c_set_argument_type(inout, Type, CArg, Arg) :-
    format('PL_unify_inout(~w, ~w, &~w)', [Type, Arg, CArg]).

c_set_argument_one(out,   Trans, CArg, Arg) :-
    format('__rtc_PL_unify(~w, ~w, ~w)', [Trans, Arg, CArg]).
c_set_argument_one(inout, Trans, CArg, Arg) :-
    format('PL_unify_inout(~w, ~w, ~w)', [Trans, Arg, CArg]).

c_set_argument_list(Spec, CArg, Arg) :-
    format('PL_unify_array(', []),
    format(atom(Arg_), '~w_', [Arg]),
    c_var_name(Arg_, CArg_),
    c_set_argument(Spec, out, CArg_, Arg_),
    format(', ~w, ~w)', [Arg, CArg]).

c_get_argument(list(Spec), _, Deref, CArg, Arg) :-
    !,
    c_get_argument_list(Deref, Spec, CArg, Arg).
c_get_argument(term, _, _, CArg, Arg) :-
    format('~w=PL_copy_term_ref(~w)', [CArg, Arg]).
c_get_argument(Trans-_, Mode, Deref, CArg, Arg) :-
    c_get_argument_one(Deref, Mode, Trans, CArg, Arg).
c_get_argument(type(Name), Mode, Deref, CArg, Arg) :-
    c_get_argument_type(Deref, Mode, Name, CArg, Arg).

c_get_argument_one(deref, Mode, Type, CArg, Arg) :-
    c_get_argument_one_deref(Mode, Type, CArg, Arg).
c_get_argument_one(noder,  _, Type, CArg, Arg) :-
    format('__rtc_PL_get(~w, ~w, ~w)', [Type, Arg, CArg]).

c_get_argument_one_deref(in, Type, CArg, Arg) :-
    format('__rtc_PL_get(~w, ~w, &~w)', [Type, Arg, CArg]).
c_get_argument_one_deref(inout, Type, CArg, Arg) :-
    format('PL_get_inout(~w, ~w, ~w)', [Type, Arg, CArg]).

c_get_argument_type(deref, Mode, Type, CArg, Arg) :-
    c_get_argument_type_deref(Mode, Type, CArg, Arg).
c_get_argument_type(noder,  _, Type, CArg, Arg) :-
    format('__rtc_PL_get_t(~w, ~w, ~w)', [Type, Arg, CArg]).

c_get_argument_type_deref(in, Type, CArg, Arg) :-
    format('__rtc_PL_get_t(~w, ~w, &~w)', [Type, Arg, CArg]).
c_get_argument_type_deref(inout, Type, CArg, Arg) :-
    format('PL_get_inout_t(~w, ~w, ~w)', [Type, Arg, CArg]).

c_get_argument_list(deref, Spec, CArg, Arg) :-
    format('PL_get_array(',[]),
    format(atom(Arg_),   '~w_', [Arg]),
    c_var_name(Arg_, CArg_),
    c_get_argument(Spec, in, noder, CArg_, Arg_),
    format(', ~w, &~w)', [Arg, CArg]).
c_get_argument_list(noder, Spec, CArg, Arg) :-
    c_get_argument_list_noder(Spec, CArg, Arg).

c_get_argument_list_noder(Spec, CArg, Arg) :-
    format('PL_get_array(',[]),
    format(atom(Arg_), '~w_',   [Arg]),
    c_var_name(Arg_, CArg_),
    c_get_argument(Spec, in, noder, CArg_, Arg_),
    format(', ~w, ~w)', [Arg, CArg]).

bind_arguments(M, Bind-Head, Return) :-
    read_foreign_properties(Head, M, Comp, Call, Succ, Glob, _, Bind),
    ( compound(Head) ->
      forall(( arg(_, Head, Arg),
	       bind_argument(M, Comp, Call, Succ, Arg, Spec, Mode)),
	     ( write('    '),
	       c_get_ctype_decl(Spec, Mode),
	       c_var_name(Arg, CArg),
	       format(' ~w;~n', [CArg])
	     )),
      forall(( arg(_, Head, Arg),
	       bind_argument(M, Comp, Call, Succ, Arg, Spec, Mode),
	       memberchk(Mode, [in, inout])
	     ),
	     ( c_var_name(Arg, CArg),
	       write('    '),
	       c_get_argument(Spec, Mode, deref, CArg, Arg),
	       write(';\n')
	     ))
    ; true
    ),
    generate_foreign_call(Bind-Head, M, Comp, Call, Succ, Glob, Return),
    ( compound(Head) ->
      forall(( arg(_, Head, Arg),
	       bind_argument(M, Comp, Call, Succ, Arg, Spec, Mode),
	       memberchk(Mode, [out, inout])
	     ),
	     ( write('    '),
	       c_var_name(Arg, CArg),
	       c_set_argument(Spec, Mode, CArg, Arg),
	       write(';\n')
	     ))
    ; true
    ).

generate_foreign_call((CN/_A as _PN)-Head, M, Comp, Call, Succ, Glob, Return) :-
    format('    ', []),
    ( member(returns_state, Glob) ->
      format('foreign_t __result='),
      CHead = Head,
      Return = '__result'
    ; member(returns(Var), Glob) ->
      c_var_name(Var, CVar),
      format('~w=', [CVar]),
      Head =.. Args,
      once(select(Var, Args, CArgs)),
      CHead =.. CArgs,
      Return = 'TRUE'
    ; CHead = Head,
      Return = 'TRUE'
    ),
    format('~w(', [CN]),
    (memberchk(memory_root, Glob) -> format('__root, ') ; true),
    ( compound(CHead) ->
      generate_foreign_call_(1, M, CHead, Comp, Call, Succ)
    ; true
    ),
    format(');~n', []).

generate_foreign_call_(N, M, Head, Comp, Call, Succ) :-
    arg(N, Head, Arg),
    ( N \= 1 -> write(', ') ; true ),
    bind_argument(M, Comp, Call, Succ, Arg, Spec, Mode),
    c_var_name(Arg, CArg),
    (Mode = in, Spec \= type(_)->true ; write('&')),
    format('~w', [CArg]),
    N1 is N + 1,
    !,
    generate_foreign_call_(N1, M, Head, Comp, Call, Succ).
generate_foreign_call_(_, _, _, _, _, _).

:- use_module(library(sequence_list)).
:- use_module(library(prolog_clause)).

get_dictionary(Term, File, Line, M, Dict) :-
    ( prolog_clause:read_term_at_line(File, Line, M, RawTerm0, _TermPos, Dict),
      ( RawTerm0 \= (_ :- _)
      ->RawTerm = (RawTerm0 :- true)
      ; RawTerm0 = RawTerm
      ),
      subsumes(RawTerm, Term) -> true
    ; Dict = []
    ).


match_known_type(M, Prop, Spec, Arg) :-
    match_known_type_(Prop, M, Spec, Arg).

match_known_type_(atm(A),           _, atom_chars-'char*', A) :- !.
match_known_type_(atom(A),          _, atom_chars-'char*', A) :- !.
match_known_type_(pointer(A),       _, pointer   -'void*', A) :- !.
% match_known_type_(pointer(A, Type), _, pointer   -PType,   A) :- !,
%     atom_concat(Type, '*', PType).
match_known_type_(int(A),           _, integer   -int,     A) :- !.
match_known_type_(integer(A),       _, integer   -int,     A) :- !.
match_known_type_(num(A),           _, float     -double,  A) :- !.
match_known_type_(number(A),        _, float     -double,  A) :- !.
match_known_type_(term(A),          _, term,               A) :- !.
match_known_type_(list(A, Type), M, list(Spec),         A) :- !,
    nonvar(Type),
    Type =.. [F|Args],
    Prop =.. [F, E|Args],
    match_known_type_(Prop, M, Spec, E).
match_known_type_(Type, M, type(Name), A) :-
    current_type_props(M, Type, _),
    !,
    functor(Type, Name, _),
    arg(1, Type, A).

bind_argument(M, CompL, CallL, SuccL, Arg, Spec, Mode) :-
    ( member(Comp, CompL),
      match_known_type(M, Comp, Spec, Arg0),
      Arg0 == Arg -> true
    ; true
    ),
    ( var(Spec) ->
      ( member(Call, CallL),
	match_known_type(M, Call, Spec, Arg0),
	Arg0 == Arg -> Mode = in
      ; true
      )
    ; ( member(Call, CallL),
	match_known_type(M, Call, Spec, Arg0),
	Arg0 == Arg -> Mode = in
      ; true
      )
    ),
    ( var(Spec) ->
      ( member(Succ, SuccL),
	match_known_type(M, Succ, Spec, Arg0),
	Arg0 == Arg -> Mode = out
      ; true
      )
    ; ( member(Succ, SuccL),
	match_known_type(M, Succ, Spec, Arg0),
	Arg0 == Arg -> Mode = out
      ; true
      )
    ),
    ignore(Mode = inout),
    ignore(Spec = term).
