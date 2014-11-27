:- module(foreign_generator, [generate_library/4,
			      gen_foreign_library/2,
			      current_foreign_prop/11]).

:- use_module(library(swi/assertions)).
:- use_module(library(assertions/assrt_lib)).
:- use_module(library(maplist_dcg)).
:- use_module(library(foreign/foreign_props)).
:- use_module(library(camel_snake)).

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
    {intermediate_obj(DirSO, Source, Object, Command)}, !,
    [Command].
intermediate_obj(_, Source, Source) --> [].

intermediate_obj(DirSO, Source, Object, Command) :- 
    file_name_extension(Base, for, Source),
    file_base_name(Base, Name),
    file_name_extension(Name, o, NameO),
    directory_file_path(DirSO, NameO, Object),
    fortran_command(Fortran),
    atomic_list_concat([Fortran, '-c', Source, '-o', Object], ' ', Command).

is_newer(File1, File2) :-
    exists_file(File1),
    exists_file(File2),
    time_file(File1, Time1),
    time_file(File2, Time2),
    Time1 > Time2.

generate_library(M, AliasSO, AliasSOPl, File) :-
    absolute_file_name(AliasSO, FileSO, [file_type(executable),
					 relative_to(File)]),
    findall(FSource, ( ( use_foreign_source(M, FAlias)
		       ; FAlias = library('foreign/foreign_interface.c')
		       ),
		       absolute_file_name(FAlias, FSource,
					  [extensions(['.c', '']),
					   access(read),
					   relative_to(File)])
		     ), FSourceL),
    ( forall(( member(Dep, [File|FSourceL])
	     ; ( use_foreign_header(M, HAlias)
	       ; HAlias = library('foreign/foreign_interface.h')
	       ; HAlias = library('foreign/foreign_swipl.h')
	       ),
	       absolute_file_name(HAlias, Dep,
				  [extensions(['.h','']),
				   access(read),
				   relative_to(File)])
	     ; member(Alias, [library(foreign/foreign_generator),
			      library(foreign/foreign_props),
			      library(foreign/foreign_interface)
			     ]),
	       absolute_file_name(Alias, Dep, [file_type(prolog),
					       access(read),
					       relative_to(File)])
	     ),
	     is_newer(FileSO, Dep))
    ->print_message(informational,
		    format('Skipping build of ~w: is up to date', [FileSO]))
    ; do_generate_library(M, FileSO, File, FSourceL),
      do_generate_wrapper(M, AliasSO, AliasSOPl, File)
    ).

do_generate_wrapper(M, AliasSO, AliasSOPl, File) :-
    findall(F/A, ( current_foreign_prop(Head, M, _, _, _, _, _, _, _, _, _),
		   \+ ( predicate_property(M:Head, number_of_clauses(X)),
			X>0
		      ),
		   functor(Head, F, A)
		 ), IntfPIU),
    sort(IntfPIU, IntfPIL),
    atom_concat(M, '$impl', IModule),
    absolute_file_name(AliasSOPl, FileSOPl, [file_type(prolog),
					     relative_to(File)]),
    with_output_to_file(FileSOPl,
			( add_autogen_note(M),
			  portray_clause((:- module(IModule, IntfPIL))),
			  generate_aux_clauses(M),
			  nl,
			  portray_clause((:- use_foreign_library(AliasSO)))
			)).

do_generate_library(M, FileSO, File, FSourceL) :-
    file_name_extension(BaseFile, _, FileSO),
    generate_foreign_interface(M, File, BaseFile),
    absolute_file_name(library(foreign/foreign_interface),
		       IntfPl,
		       [file_type(prolog), access(read), relative_to(File)]),
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
		      absolute_file_name(DAlias, Dir, [file_type(directory),
						       relative_to(File)])
		    ),
		    atom_concat('-I', Dir, IDir)
		  ),
	    IDirL),
    atomic_list_concat(IDirL, ' ', IDirs),
    format(atom(CommandT), "swipl-ld -shared ~w ~w ~w ~w_intf.c ~w -o ~w",
	   [COpts, IDirs, FSources, BaseFile, CLibs, FileSO]),
    CommandsT = [CommandT],
    forall(member(Command, Commands),
	   ( shell(Command, Status),
	     print_message(informational, format('~w', [Command])),
	     assertion(Status==0)
	   )).

:- meta_predicate with_output_to_file(+,0).

with_output_to_file(File, Goal) :- setup_call_cleanup(tell(File), Goal, told).

generate_foreign_interface(Module, FilePl, BaseFile) :-
    atom_concat(BaseFile, '_impl', BaseFileImpl),
    file_name_extension(BaseFileImpl, h, FileImpl_h),
    atom_concat(BaseFile, '_intf', BaseFileIntf),
    file_name_extension(BaseFileIntf, h, FileIntf_h),
    file_name_extension(BaseFileIntf, c, FileIntf_c),
    directory_file_path(_, Base, BaseFile),
    with_output_to_file(FileImpl_h, generate_foreign_impl_h(Module)),
    with_output_to_file(FileIntf_h, generate_foreign_intf_h(Module, FileImpl_h)),
    with_output_to_file(FileIntf_c, generate_foreign_c(Module, Base, FilePl, FileIntf_h)).

c_var_name(Arg, CArg) :-
    format(atom(CArg), '_c_~w', [Arg]).

generate_foreign_intf_h(Module, FileImpl_h) :-
    add_autogen_note(Module),
    format('#ifndef __~w_INTF_H~n#define __~w_INTF_H~n~n', [Module, Module]),
    format('#include <foreign_swipl.h>~n', []),
    format('#include "~w"~n~n', [FileImpl_h]),
    forall_tp(Module, type_props_nf, declare_typeconv),
    forall(current_foreign_prop(Head, Module, _, _, _, _, Dict, _, _, BindName, _),
	   ( apply_dict(Head, Dict),
	     format('extern ', []),
	     declare_intf_head(BindName, Head),
	     format(';~n', []))),
    format('~n#endif /* __~w_INTF_H */~n', [Module]).

generate_foreign_impl_h(Module) :-
    add_autogen_note(Module),
    format('#ifndef __~w_IMPL_H~n#define __~w_IMPL_H~n~n', [Module, Module]),
    forall_tp(Module, type_props, declare_typedef),
    forall_tp(Module, type_props, declare_struct),
    declare_foreign_bind(Module),
    format('~n#endif /* __~w_IMPL_H */~n', [Module]).

add_autogen_note(Module) :-
    format('/* NOTE: File generated automatically from ~w */~n~n', [Module]).

generate_foreign_c(Module, Base, FilePl, FileIntf_h) :-
    add_autogen_note(Module),
    forall(use_foreign_header(Module, HAlias),
	   ( absolute_file_name(HAlias, File_h, [extensions(['.h', '']),
						 access(read),
						 relative_to(FilePl)]),
	     format('#include "~w"~n', [File_h])
	   )),
    format('#include "~w"~n~n', [FileIntf_h]),
    forall_tp(Module, type_props_nf, implement_type_getter),
    forall_tp(Module, type_props_nf, implement_type_unifier),
    generate_foreign_register(Module, Base),
    generate_foreign_intf(Module).

:- meta_predicate forall_tp(+,5,3).

forall_tp(Module, TypeProps, Call) :-
    forall(call(TypeProps, Module, Type, PropL, Dict, Pos),
	   ( apply_dict(Type-PropL, Dict),
	     type_components(Module, Type, PropL, Call, Pos)
	   )).

type_props(M, Type, PropL, Dict, Pos) :-
    type_props(M, Type, PropL, _, Dict, Pos).

type_props(M, Type, PropL, GlobL, Dict, Pos) :-
    type_props_(M, Type, TPropL, GlobL, TDict, Pos),
    ( TPropL \= []
    ->PropL = TPropL,
      Dict = TDict
    ; bind_type_names(M, Type, PropL, Dict)
    ).

type_props_(M, Type, PropL, GlobL, Dict, Pos) :-
    assertion_db(Type, M, check, prop, PropL, _, _, GlobL, _, Dict, Pos),
    once(( member(TType, [type, regtype]),
	   memberchk(TType, GlobL)
	 )).

type_props(M, Type) :-
    type_props_(M, Type, _, _, _, _).

type_props_nf(Module, Type, PropL, Dict, Pos) :-
    type_props(Module, Type, PropL, GlobL, Dict, Pos),
    \+ memberchk(foreign(_), GlobL).
    
implement_type_getter_ini(PName, CName, Arg) :-
    format('int FL_get_~w(void** __root, term_t ~w, ~w *~w) {~n',
	   [Arg, PName, Arg, CName]).

implement_type_getter(func_ini, Term, Arg) :-
    term_pcname(Term, PName, CName),
    implement_type_getter_ini(PName, CName, Arg).
implement_type_getter(func_rec(N, Term), Spec, Arg) :-
    term_pcname(Term, PName, CName),
    format(atom(CNameArg), '~w->~w', [CName, Arg]),
    camel_snake(PArg, Arg),
    format(atom(PNameArg), '~w_~w', [PName, PArg]),
    format('    term_t ~w=PL_new_term_ref();~n', [PNameArg]),
    format('    __rtcheck(PL_get_arg(~w,~w,~w));~n', [N, PName, PNameArg]),
    format('    ', []),
    c_get_argument(Spec, in, deref, CNameArg, PNameArg),
    format(';~n', []).
implement_type_getter(func_end, _, _) :-
    implement_type_end.
implement_type_getter(atom(Name), Spec, Term) :-
    term_pcname(Term, PName, CName),
    implement_type_getter_ini(PName, CName, Name),
    format('    ', []),
    c_get_argument(Spec, inout, deref, CName, PName),
    format(';~n', []),
    implement_type_end.
implement_type_getter(dict_ini(M, _, Name), _, Arg) :-
    term_pcname(Name, PName, CName),
    implement_type_getter_dict_ini(M, PName, CName, Arg).
implement_type_getter(dict_key_value(Dict, _, _, N), Key, Value) :-
    key_value_from_dict(Dict, N, Key, Value).
implement_type_getter(dict_rec(_, _, N, Name), Spec, Arg) :-
    term_pcname(Name, _, CName),
    format(atom(CNameArg), '~w->~w', [CName, Arg]),
    format('        case ~w: ', [N]),
    c_get_argument(Spec, in, deref, CNameArg, '__value'),
    format('; break;~n', []).
implement_type_getter(dict_end(_, _), _, _) :-
    format('        }~n', []),
    implement_type_end.
implement_type_getter(atom(Name), Spec, Term) :-
    term_pcname(Term, PName, CName),
    implement_type_getter_ini(PName, CName, Name),
    format('    ', []),
    c_set_argument(Spec, inout, CName, PName),
    format(';~n', []),
    implement_type_end.
				    
implement_type_getter_dict_ini(Module, PName, CName, Arg) :-
    format('static int~n', []),
    format('get_pair_~w(void **__root, term_t __keyid, term_t __value, ~w *);~n',
	   [Arg, Arg]),
    implement_type_getter_ini(PName, CName, Arg),
    format('    memset(~w, 0, sizeof(~w));~n', [CName, Arg]),
    format('    FL_get_dict_t(~w, ~w, ~w);~n', [Arg, PName, CName]),
    implement_type_end,
    format('static int~n', []),
    format('get_pair_~w(void **__root, term_t __keyid, term_t __value, ~w *~w){~n',
	   [Arg, Arg, CName]),
    format('    int __index;~n', []),
    format('    FL_get_keyid_index("~w$impl", ~w, __keyid, __index);~n',
	   [Module, Arg]),
    format('    switch (__index) {~n', []).

implement_type_end :-
    format('    return TRUE;~n}~n~n', []).

term_pcname(Term, PName, CName) :-
    compound(Term), !,
    functor(Term, Func, _),
    func_pcname(Func, PName, CName).
term_pcname(Func, PName, CName) :-
    func_pcname(Func, PName, CName).

func_pcname(Func, PName, CName) :-
    camel_snake(PName, Func),
    c_var_name(Func, CName).

implement_type_unifier(atom(Name), Spec, Term) :-
    term_pcname(Term, PName, CName),
    implement_type_unifier_ini(PName, CName, Name),
    format('    ', []),
    c_set_argument(Spec, inout, CName, PName),
    format(';~n', []),
    implement_type_end.
implement_type_unifier(func_ini, Term, Arg) :-
    term_pcname(Term, PName, CName),
    functor(Term, Func, Arity),
    implement_type_unifier_ini(PName, CName, Arg),
    format('    __rtcheck(PL_unify_functor(~w, PL_new_functor(PL_new_atom("~w"), ~d)));~n',
	   [PName, Func, Arity]).
implement_type_unifier(func_rec(N, Term), Spec, Arg) :-
    term_pcname(Term, PName, CName),
    format(atom(CNameArg), '~w->~w', [CName, Arg]),
    camel_snake(PArg, Arg),
    format(atom(PNameArg), '~w_~w', [PName, PArg]),
    format('    term_t ~w=PL_new_term_ref();~n', [PNameArg]),
    format('    __rtcheck(PL_get_arg(~w,~w,~w));~n', [N, PName, PNameArg]),
    format('    ', []),
    c_set_argument(Spec, out, CNameArg, PNameArg),
    format(';~n', []).
implement_type_unifier(func_end, _, _) :-
    implement_type_end.
implement_type_unifier(dict_ini(_M, Dict, Name), _, Arg) :-
    func_pcname(Name, PName, CName),
    implement_type_unifier_ini(PName, CName, Arg),
    functor(Dict, _, Arity),
    N is Arity//2,
    format('    term_t dict_args=PL_new_term_refs(~w);~n', [N]),
    format('    int index=0, indexes[~w];~n', [N]).
implement_type_unifier(dict_key_value(Dict, _, _, N), Key, Value) :-
    key_value_from_dict(Dict, N, Key, Value). % Placed in 'dict' order
implement_type_unifier(dict_rec(_, _, N, Name), Spec, Arg) :-
    func_pcname(Name, PName, CName),
    format(atom(CNameArg), '~w->~w', [CName, Arg]),
    camel_snake(PArg, Arg),
    format(atom(PNameArg), '~w_~w', [PName, PArg]),
    (spec_pointer(Spec)->format('    if(~w) {~n', [CNameArg]);true),
    format('        term_t ~w=dict_args+index;~n        ', [PNameArg]),
    c_set_argument(Spec, out, CNameArg, PNameArg),
    format(';~n', []),
    format('        indexes[index++]=~w;~n', [N]),
    (spec_pointer(Spec)->format('    }~n', []);true).
implement_type_unifier(dict_end(M, Name), _, Arg) :-
    func_pcname(Name, PName, _),
    format('    FL_unify_dict_t("~w$impl", ~w, ~w, "~w");~n',
	   [M, Arg, PName, Name]),
    implement_type_end.

spec_pointer(chrs(_)).
spec_pointer(ptr(_)).
spec_pointer(pointer-_).
spec_pointer(list(_)).
% spec_pointer(type(_)).

implement_type_unifier_ini(PName, CName, Arg) :-
    format('int FL_unify_~w(term_t ~w, ~w* const ~w) {~n',
	   [Arg, PName, Arg, CName]).
    
apply_name(Name=Value) :-
    camel_snake(Name, Arg),
    ignore(Value=Arg).

apply_dict(Head, Dict) :-
    maplist(apply_name, Dict),
    term_variables(Head, Vars),
    fg_numbervars(Vars, 1, Dict).

fg_numbervars([], _, _).
fg_numbervars([V|Vs], N, Dict) :-
    format(atom(T), 'var_~d', [N]),
    succ(N, N1),
    ( memberchk(_=T, Dict)
    ->fg_numbervars([V|Vs], N1, Dict)
    ; V=T,
      fg_numbervars(Vs, N1, Dict)
    ).

bind_type_names(M, Type, PropL, Dict) :-
    once(catch(clause(M:Type, Body, Ref),_,fail)),
    ( clause_property(Ref, file(File)),
      clause_property(Ref, line_count(Line)),
      get_dictionary(Type :- Body, File, Line, M, Dict)
    ->true
    ; Dict = []
    ),
    sequence_list(Body, PropL, []).

declare_struct(atom(_),  _,    _).
declare_struct(func_ini, Term, _) :-
    functor(Term, Name, _),
    format('struct ~w {~n', [Name]).
declare_struct(func_end, _, _) :- format('};~n', []).
declare_struct(func_rec(_, _), Spec, Name) :-
    write('    '),
    c_get_ctype_decl(Spec),
    format(' ~w;~n', [Name]).
%%
declare_struct(dict_ini(_, _, Spec), _, _) :-
    format('~nstruct ~w {~n', [Spec]).
declare_struct(dict_key_value(Dict, Desc, Tag, N), Key, Value) :-
    key_value_from_desc(Dict, Desc, Tag, N, Key, Value).
declare_struct(dict_rec(_, _, _, _), Spec, Name) :-
    write('    '),
    c_get_ctype_decl(Spec),
    format(' ~w;~n', [Name]).
declare_struct(dict_end(_, _), _, _) :- format('};~n', []).

declare_typedef(atom(Name), Spec, _) :-
    format('typedef ', []),
    c_get_ctype_decl(Spec),
    format(' ~w;~n', [Name]).
declare_typedef(func_ini, Term, Name) :-
    functor(Term, Spec, _),
    format('typedef struct ~w ~w;~n', [Spec, Name]).
declare_typedef(func_end, _, _).
declare_typedef(func_rec(_, _), _, _).
declare_typedef(dict_ini(_, _, Spec), _, Name) :-
    format('typedef struct ~w ~w;~n', [Spec, Name]).
declare_typedef(dict_end(_, _), _, _).
declare_typedef(dict_rec(_, _, _, _), _, _).

declare_typeconv(atom(Name), _, _) :-
    declare_typeconv(Name).
declare_typeconv(func_ini, _, Name) :-
    declare_typeconv(Name).
declare_typeconv(func_end, _, _).
declare_typeconv(func_rec(_, _), _, _).
declare_typeconv(dict_ini(_, _, _), _, Name) :-
    declare_typeconv(Name).
declare_typeconv(dict_end(_, _), _, _).
declare_typeconv(dict_rec(_, _, _, _), _, _).

declare_typeconv(Name) :-
    format('int FL_get_~w(void** __root, term_t, ~w*);~n', [Name, Name]),
    format('int FL_unify_~w(term_t, ~w* const);~n~n', [Name, Name]).

generate_aux_clauses(Module) :-
    forall_tp(Module, type_props, generate_aux_clauses).

% This will create an efficient method to convert keys to indexes in the C side,
% avoiding string comparisons.
generate_aux_clauses(dict_key_value(Dict, _, _, N), Key, Value) :- !,
    key_value_from_dict(Dict, N, Key, Value).
generate_aux_clauses(dict_rec(_, Name, N, _), _, Key) :- !,
    atom_concat('__aux_keyid_index_', Name, F),
    Pred =.. [F, Key, N],
    portray_clause(Pred).
generate_aux_clauses(_, _, _).

:- multifile
    prolog:message//1,
    prolog:message_location//1.

prolog:message(ignored_type(Loc, Name, Arg)) -->
    prolog:message_location(Loc),
    ['~w->~w ignored'-[Name, Arg]].

prolog:message(failed_binding(Loc, TypeComponents)) -->
    prolog:message_location(Loc),
    ['~w failed'-[TypeComponents]].

:- meta_predicate type_components(?,+,+,3,+).
type_components(M, Type, PropL, Call, Loc) :-
    functor(Type, Name, _),
    arg(1, Type, Term),
    ( compound(Term)
    ->call(Call, func_ini, Term, Name),
      forall(arg(N, Term, Arg),
	     ( member(Prop, PropL),
	       match_known_type_(Prop, M, Spec, Arg),
	       call(Call, func_rec(N, Term), Spec, Arg)
	     ->true
	     ; print_message(warning, ignored_type(Loc, Name, Arg))
	     )
	    ),
      call(Call, func_end, Term, Name)
    ; ( select(dict_t(Term, Desc), PropL, PropL1)
      ; select(dict_t(Term, Tag, Desc), PropL, PropL1)
      )
    ->( is_dict(Desc, Tag)
      ->Dict=Desc
      ; dict_create(Dict, Tag, Desc)
      ),
      ignore(Tag = Name),
      call(Call, dict_ini(M, Dict, Tag), Term, Name),
      forall(call(Call, dict_key_value(Dict, Desc, Tag, N), Arg, Value),
	     ( fetch_kv_prop_arg(Arg, Value, PropL1, Prop),
	       match_known_type_(Prop, M, Spec, Arg),
	       call(Call, dict_rec(M, Name, N, Tag), Spec, Arg)
	     )
	    ),
      call(Call, dict_end(M, Tag), Term, Name)
    ; member(Prop, PropL),
      match_known_type_(Prop, M, Spec, Term),
      call(Call, atom(Name), Spec, Term)
    ), !.
type_components(M, Type, PropL, Call, Loc) :-
    print_message(error, failed_binding(Loc, type_components(M, Type, PropL, Call, '_'))).

key_value_from_dict(Dict, N, Key, Value) :-
    S = s(0),
    Value=Dict.Key,
    S = s(N),
    succ(N, N2),
    nb_setarg(1, S, N2).

key_value_from_list(Desc, N, Key, Value) :-
    nth0(N, Desc, KeyValue),
    key_value(KeyValue, Key, Value).

key_value_from_desc(_, Desc, _, N, Key, Value) :-
    is_list(Desc), !,
    key_value_from_list(Desc, N, Key, Value).
key_value_from_desc(Dict, _, _, N, Key, Value) :-
    key_value_from_dict(Dict, N, Key, Value).

key_value(KV,  K, V) :- functor(KV, K, 1), arg(1, KV, V), !.
key_value(K:V, K, V).
key_value(K-V, K, V).
key_value(K=V, K, V).

fetch_kv_prop_arg(Key, Value, PropL, Prop) :-
    ( member(Prop, PropL),
      arg(1, Prop, Key)
    ; Value =.. [A|AL],
      Prop  =.. [A, Key|AL]
    ).

collect_bind_head(Module, BindHeadL) :-
    findall(Bind-Head,
	    ( read_foreign_properties(Head, Module, _, _, _, _, Dict, Bind),
	      apply_dict(Head, Dict)
	    ), BindHeadUL),
    sort(BindHeadUL, BindHeadL).

declare_intf_head((CN/_A as _PN + _)-Head) :-
    atom_concat('pl_', CN, PCN),
    declare_intf_head(PCN, Head).

declare_intf_head(PCN, Head) :-
    format('foreign_t ~w(', [PCN]),
    ( compound(Head)
    ->findall(Txt, ( arg(_, Head, Arg),
		     format(atom(Txt), 'term_t ~w', [Arg])
		   ), TxtL),
      atomic_list_concat(TxtL, ', ', ArgS),
      format('~a', [ArgS])
    ; true
    ),
    format(')', []).

declare_foreign_bind(M) :-
    ( read_foreign_properties(Head, M, Comp, Call, Succ, Glob, Dict, Bind),
      apply_dict(Head, Dict),
      ( member(RS, [returns_state, type]),
	memberchk(RS, Glob) ->
	format('int '),	    % int to avoid SWI-Prolog.h dependency at this level
	CHead = Head
      ; member(returns(Var), Glob) ->
	bind_argument(Head, M, Comp, Call, Succ, Glob, Var, Spec, Mode),
	c_get_ctype_decl(Spec, Mode),
	Head =.. Args,
	once(select(Var, Args, CArgs)),
	CHead =.. CArgs
      ; format('void '),
	CHead = Head
      ),
      declare_foreign_head(Bind, M, CHead, Comp, Call, Succ, Glob),
      format(';~n', []),
      fail
    ; true
    ).

declare_foreign_head((CN/_A as _PN + _), M, Head, Comp, Call, Succ, Glob) :-
    format(' ~w(', [CN]),
    (memberchk(memory_root, Glob) -> format('void** __root, ', []) ; true),
    ( compound(Head) ->
      declare_foreign_bind_(1, M, Head, Comp, Call, Succ, Glob)
    ; true
    ),
    format(')', []).

declare_foreign_bind_(N, M, Head, Comp, Call, Succ, Glob) :-
    arg(N, Head, Arg),
    ( N \= 1 -> write(', ') ; true ),
    declare_foreign_bind_arg(Head, M, Comp, Call, Succ, Glob, Arg),
    format(' ~w', [Arg]),
    N1 is N + 1,
    !,
    declare_foreign_bind_(N1, M, Head, Comp, Call, Succ, Glob).
declare_foreign_bind_(_, _, _, _, _, _, _).

declare_foreign_bind_arg(Head, M, Comp, Call, Succ, Glob, Arg) :-
    bind_argument(Head, M, Comp, Call, Succ, Glob, Arg, Spec, Mode),
    c_get_ctype_decl(Spec, Mode),
    (Mode = in, Spec \= type(_) ->true ; write('*')),
    (Mode = in -> write(' const') ; true). % Ensure const correctness

c_get_ctype_decl(Spec, Mode) :-
    c_get_ctype_decl(Spec),
    (is_ref(Mode, Spec) -> true ; write('*')).

is_ref(_,   term)    :- !.
is_ref(_,   list(_)) :- !.	% Always ref
is_ref(_,   ptr(_))  :- !.	% Always ref
is_ref(_,   chrs(_)) :- !.
is_ref(in,  _).
is_ref(out, _).
				% is_ref(inout, _) :- fail.
				% Allow pointer to NULL,
				% the equivalent to free
				% variables in imperative
				% languages --EMM

c_get_ctype_decl(list(Spec)) :-
    c_get_ctype_decl(Spec),
    format('*', []).
c_get_ctype_decl(ptr(Spec)) :-
    c_get_ctype_decl(Spec),
    format('*', []).
c_get_ctype_decl(chrs(Name)) :-
    format('~w', Name).
c_get_ctype_decl(type(Name)) :-
    format('~w', [Name]).
c_get_ctype_decl(term) :-
    format('term_t', []).
c_get_ctype_decl(_-CType) :-
    format('~w', [CType]).

generate_foreign_register(Module, Base) :-
    format('install_t install_~w() {~n', [Base]),
    forall(current_foreign_prop(_, Module, _, _, _, _, _, _, PredName, BindName, Arity),
	   format('    PL_register_foreign(\"~w\", ~w, ~w, 0);~n',
		  [PredName, Arity, BindName])),
    format('} /* install_~w */~n~n', [Base]).

current_foreign_prop(Head, Module, Comp, Call, Succ, Glob, Dict,
		     FuncName, PredName, BindName, Arity) :-
    assertion_db(Head, Module, check, Type, Comp, Call, Succ, Glob, _, Dict, _),
    memberchk(Type, [pred, prop]),
    functor(Head, PredName, Arity),
    ( memberchk(foreign,     Glob)
    ->FuncName = PredName
    ; memberchk(foreign(FuncName), Glob)
    ->true
    ; true
    ),
    ( memberchk(native, Glob)
    ->BindName = PredName
    ; memberchk(native(BindName), Glob)
    ->true
    ; nonvar(FuncName)
    ->atom_concat('pl_', FuncName, BindName)
    ).

read_foreign_properties(Head, Module, Comp, Call, Succ, Glob, Dict, CN/A as PN + CheckMode) :-
    current_foreign_prop(Head, Module, Comp, Call, Succ, Glob, Dict, CN, PN, _, A),
    nonvar(CN),
    ( memberchk(type, Glob)
    ->CheckMode=type
    ; CheckMode=pred
    ).

generate_foreign_intf(Module) :-
    collect_bind_head(Module, BindHeadL),
    maplist(declare_intf_impl(Module), BindHeadL).

declare_intf_impl(Module, BindHead) :-
    declare_intf_head(BindHead),
    format(' {~n', []),
    BindHead = (PI as _ + CheckMode)-Head,
    ( CheckMode==type	   % If is variable then succeed (because is compatible)
    ->forall(arg(_, Head, Arg),
	     format('    if(PL_is_variable(~w)) return TRUE;~n', [Arg]))
    ; true
    ),
    format('    __mkroot(__root);~n'),
    bind_arguments(Module, BindHead, Return),
    format('    __delroot(__root);~n'),
    format('    return ~w;~n', [Return]),
    format('} /* ~w */~n~n', [PI]).

c_set_argument(list(S), _, C, A) :- c_set_argument_rec(list, S, C, A).
c_set_argument(ptr( S), _, C, A) :- c_set_argument_rec(ptr,  S, C, A).
c_set_argument(type(T), M, C, A) :- c_set_argument_type(M, T, C, A).
c_set_argument(T-_,     M, C, A) :- c_set_argument_one( M, T, C, A).
c_set_argument(chrs(_), M, C, A) :- c_set_argument_chrs(M, C, A).
c_set_argument(term,    _, C, A) :- format('~w=~w', [A, C]).

c_set_argument_one(out,   Type, CArg, Arg) :-
    format('__rtc_FL_unify(~w, ~w, ~w)', [Type, Arg, CArg]).
c_set_argument_one(inout, Type, CArg, Arg) :-
    format('FL_unify_inout(~w, ~w, ~w)', [Type, Arg, CArg]).

c_set_argument_type(out,   Type, CArg, Arg) :-
    format('__rtc_FL_unify(~w, ~w, &~w)', [Type, Arg, CArg]).
c_set_argument_type(inout, Type, CArg, Arg) :-
    format('FL_unify_inout(~w, ~w, &~w)', [Type, Arg, CArg]).

c_set_argument_chrs(out,   CArg, Arg) :-
    format('__rtc_FL_unify(chrs, ~w, ~w)', [Arg, CArg]).
c_set_argument_chrs(inout, CArg, Arg) :-
    format('FL_unify_inout_chrs(~w, ~w)', [Arg, CArg]).

c_set_argument_rec(Type, Spec, CArg, Arg) :-
    format('FL_unify_~w(', [Type]),
    format(atom(Arg_), '~w_', [Arg]),
    c_var_name(Arg_, CArg_),
    c_set_argument(Spec, out, CArg_, Arg_),
    format(', ~w, ~w)', [Arg, CArg]).

c_get_argument(list(Spec), _, Deref, CArg, Arg) :-
    c_get_argument_rec(Deref, list, Spec, CArg, Arg).
c_get_argument(ptr(Spec), _, Deref, CArg, Arg) :-
    c_get_argument_rec(Deref, ptr,  Spec, CArg, Arg).
c_get_argument(type(Name), Mode, Deref, CArg, Arg) :-
    c_get_argument_type(Deref, Mode, Name, CArg, Arg).
c_get_argument(chrs(_), Mode, Deref, CArg, Arg) :-
    c_get_argument_chrs(Deref, Mode, CArg, Arg).
c_get_argument(term, _, _, CArg, Arg) :-
    format('~w=PL_copy_term_ref(~w)', [CArg, Arg]).
c_get_argument(Type-_, Mode, Deref, CArg, Arg) :-
    c_get_argument_one(Deref, Mode, Type, CArg, Arg).

c_get_argument_one(deref, Mode, Type, CArg, Arg) :-
    c_get_argument_one_deref(Mode, Type, CArg, Arg).
c_get_argument_one(noder,  _, Type, CArg, Arg) :-
    format('__rtc_FL_get(~w, ~w, ~w)', [Type, Arg, CArg]).

c_get_argument_one_deref(in, Type, CArg, Arg) :-
    format('__rtc_FL_get(~w, ~w, &~w)', [Type, Arg, CArg]).
c_get_argument_one_deref(inout, Type, CArg, Arg) :-
    format('FL_get_inout(~w, ~w, ~w)', [Type, Arg, CArg]).

c_get_argument_chrs(deref, Mode, CArg, Arg) :-
    c_get_argument_chrs_deref(Mode, CArg, Arg).
c_get_argument_chrs(noder,  _, CArg, Arg) :-
    format('__rtc_FL_get(~w, ~w, ~w)', [chrs, Arg, CArg]).

c_get_argument_chrs_deref(in, CArg, Arg) :-
    format('__rtc_FL_get(~w, ~w, &~w)', [chrs, Arg, CArg]).
c_get_argument_chrs_deref(inout, CArg, Arg) :-
    format('FL_get_inout_chrs(~w, &~w)', [Arg, CArg]).

c_get_argument_type(deref, inout, Type, CArg, Arg) :- !,
    format('FL_get_inout(~w, ~w, ~w)', [Type, Arg, CArg]).
c_get_argument_type(Deref, _, Type, CArg, Arg) :-
    deref_sym(Deref, DSym),
    format('__rtc_FL_get(~w, ~w, ~w~w)', [Type, Arg, DSym, CArg]).

c_get_argument_rec(Deref, Type, Spec, CArg, Arg) :-
    format('FL_get_~w(',[Type]),
    format(atom(Arg_), '~w_',   [Arg]),
    c_var_name(Arg_, CArg_),
    deref_sym(Deref, DSym),
    c_get_argument(Spec, in, noder, CArg_, Arg_),
    format(', ~w, ~w~w)', [Arg, DSym, CArg]).

deref_sym(deref, '&').
deref_sym(noder, '').

bind_arguments(M, Bind-Head, Return) :-
    read_foreign_properties(Head, M, Comp, Call, Succ, Glob, _, Bind),
    ( compound(Head)
    ->forall(( arg(_, Head, Arg),
	       bind_argument(Head, M, Comp, Call, Succ, Glob, Arg, Spec, Mode)),
	     ( write('    '),
	       c_get_ctype_decl(Spec, Mode),
	       c_var_name(Arg, CArg),
	       format(' ~w;~n', [CArg])
	     )),
      forall(( arg(_, Head, Arg),
	       bind_argument(Head, M, Comp, Call, Succ, Glob, Arg, Spec, Mode),
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
	       bind_argument(Head, M, Comp, Call, Succ, Glob, Arg, Spec, Mode),
	       memberchk(Mode, [out, inout])
	     ),
	     ( write('    '),
	       c_var_name(Arg, CArg),
	       c_set_argument(Spec, Mode, CArg, Arg),
	       write(';\n')
	     ))
    ; true
    ).

generate_foreign_call((CN/_A as _PN + _)-Head, M, Comp, Call, Succ, Glob, Return) :-
    format('    ', []),
    ( member(RS, [returns_state, type]),
      memberchk(RS, Glob)
    ->format('foreign_t __result='),
      CHead = Head,
      Return = '__result'
    ; ( member(returns(Var), Glob)
      ->c_var_name(Var, CVar),
	format('~w=', [CVar]),
	Head =.. Args,
	once(select(Var, Args, CArgs)),
	CHead =.. CArgs
      ; CHead = Head
      ),
      ( member(no_exception, Glob)
      ->Return = 'TRUE'
      ; Return = '!PL_exception(0)'
      )
    ),
    format('~w(', [CN]),
    ( memberchk(memory_root, Glob)
    ->format('__root, ')
    ; true
    ),
    ( compound(CHead)
    ->generate_foreign_call_(1, M, CHead, Comp, Call, Succ, Glob)
    ; true
    ),
    format(');~n', []).

generate_foreign_call_(N, M, Head, Comp, Call, Succ, Glob) :-
    arg(N, Head, Arg),
    ( N \= 1 -> write(', ') ; true ),
    bind_argument(Head, M, Comp, Call, Succ, Glob, Arg, Spec, Mode),
    c_var_name(Arg, CArg),
    ( Mode = in, Spec \= type(_)
    ->true
    ; write('&')
    ),
    format('~w', [CArg]),
    N1 is N + 1,
    !,
    generate_foreign_call_(N1, M, Head, Comp, Call, Succ, Glob).
generate_foreign_call_(_, _, _, _, _, _, _).

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
    match_known_type_(Prop, M, Spec, Arg), !.

match_known_type_(atm(A),       _, chrs('char*'), A).
match_known_type_(atom(A),      _, chrs('char*'), A).
match_known_type_(ptr(A, Type), M, ptr(Spec), A) :-
    nonvar(Type),
    Type =.. [F|Args],
    Prop =.. [F, E|Args],
    match_known_type_(Prop, M, Spec, E).
% match_known_type_(string(A),        _, string_chars-'char*', A).
match_known_type_(ptr(A),            _, pointer-'void*', A).
match_known_type_(int(A),            _, integer-int,     A).
match_known_type_(integer(A),        _, integer-int,     A).
match_known_type_(character_code(A), _, char-char,       A).
match_known_type_(num(A),            _, float-double,    A).
match_known_type_(number(A),         _, float-double,    A).
match_known_type_(term(A),           _, term,            A).
match_known_type_(list(A, Type),     M, list(Spec),      A) :-
    nonvar(Type),
    Type =.. [F|Args],
    Prop =.. [F, E|Args],
    match_known_type_(Prop, M, Spec, E).
match_known_type_(Type, M, type(Name), A) :-
    type_props(M, Type),
    functor(Type, Name, _),
    arg(1, Type, A).

bind_argument(Head, M, CompL, CallL, SuccL, GlobL, Arg, Spec, Mode) :-
    ( member(Comp, CompL),
      match_known_type(M, Comp, Spec, Arg0),
      Arg0 == Arg
    ->true
    ; true
    ),
    ( member(Call, CallL),
      match_known_type(M, Call, Spec, Arg0),
      Arg0 == Arg
    ->Mode = in
    ; true
    ),
    ( member(Succ, SuccL),
      match_known_type(M, Succ, Spec, Arg0),
      Arg0 == Arg
    ->Mode = out
    ; true
    ),
    ( memberchk(type, GlobL)
    ->Mode = in,
      functor(Head, Name, _),
      ignore(Spec = type(Name))
    ; true
    ),
    ignore(Mode = inout),
    ignore(Spec = term).
