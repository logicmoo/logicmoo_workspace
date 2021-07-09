
:- module('pfc_iri',
          [ % add_pfc_to_module/1,
            include_pfc_res/2,
            include_module_file/2,
            open_pfc_resource/2,            % +Name, -Stream
            open_pfc_resource/3,            % +Name, +RW, -Stream
            current_pfc_resource/2          % :Name, ?File            
          ]).

:- meta_predicate
    open_pfc_resource(:, -),
    open_pfc_resource(:, -, +),
    current_pfc_resource(:, ?).

:- dynamic
    user:pfc_resource/2,
    user:pfc_resource/3.
:- volatile
    user:pfc_resource/2,
    user:pfc_resource/3.
:- multifile
    user:pfc_resource/2,
    user:pfc_resource/3.

%!  open_pfc_resource(:Name, -Stream) is det.
%!  open_pfc_resource(:Name, -Stream, +Options) is det.
%!  open_pfc_resource(:Name, ?Class, -Stream) is det.
%
%   Open pfc_resource with given Name, returning a Stream.

open_pfc_resource(Name, Handle) :-
    open_pfc_resource(Name, Handle, []).

open_pfc_resource(Module:RcName, Stream, Options) :-
    is_list(Options),
    !,
    (   default_module(Module, RModule),
        current_pfc_resource(RModule:RcName, FileSpec)
    ->  absolute_file_name(FileSpec, File),
        open(File, read, Stream, Options)
    ;   '$rc_handle'(Zipper),
        tag_pc_name(Module, RcName, TaggedName),
        zipper_goto(Zipper, file(TaggedName))
    ->  zipper_open_current(Zipper, Stream,
                            [ release(true)
                            | Options
                            ])
    ;   '$existence_error'(pfc_resource, Module:RcName)
    ).
open_pfc_resource(Name, _Class, Stream) :-
    open_pfc_resource(Name, Stream).

tag_pc_name(user, RcName, RcName) :- !.
tag_pc_name(Module, RcName, TaggedName) :-
    atomic_list_concat([Module, ':', RcName], TaggedName).
tag_pc_name(_, RcName, RcName).

%!  current_pfc_resource(:Name, ?File) is nondet.
%
%   List all currently declared pfc_resources.   Should  eventually deal
%   with pfc_resources that are already part of the state.

current_pfc_resource(M:Name, File) :-
    current_module(M),
    (   current_predicate(M:pfc_resource/2),
        M:pfc_resource(Name, File)
    ;   current_predicate(M:pfc_resource/3),
        M:pfc_resource(Name, _Class, File)
    ).

%!  c_open_pfc_resource(:Name, +Mode, -Stream)
%
%   Callback for PL_open_pfc_resource().

:- public c_open_pfc_resource/3.
:- meta_predicate c_open_pfc_resource(:, +, -).

c_open_pfc_resource(Name, Mode, Stream) :-
    atom_chars(Mode, Chars),
    (   Chars = [r|MChars]
    ->  pfc_mode_options(MChars, Options),
        open_pfc_resource(Name, Stream, Options)
    ;   '$domain_error'(open_pfc_resource_mode, Mode)
    ).

pfc_mode_options([], []).
pfc_mode_options([t|Chars], [type(text)|T]) :-
    !,
    pfc_mode_options(Chars, T).
pfc_mode_options([b|Chars], [type(binary)|T]) :-
    !,
    pfc_mode_options(Chars, T).
pfc_mode_options([_|Chars], T) :-
    pfc_mode_options(Chars, T).


		 /*******************************
		 *      RESOURCES AS FILES	*
		 *******************************/

:- register_iri_scheme(pfc, pfc_iri_hook, []).

%!  pfc_iri_hook(+Action, +IRI, -Stream) is det.
%
%   Define the =|pfc://|= IRI scheme, binding   to  the central pfc_resource
%   DB. For speed, the  first  call   calls  index_pc/0  that  creates a
%   predicate associating IRIs to offsets in the central pfc_resource ZIP.

pfc_iri_hook(open(Mode,Options), IRI, Stream) :-
    (   Mode == read
    ->  setup_call_cleanup(
            pfc_iri_pfc_zipper(IRI, Zipper),
            zipper_open_current(Zipper, Stream, Options),
            zip_close_(Zipper, _))
    ;   '$permission_error'(open, source_sink, IRI)
    ).
pfc_iri_hook(access(Mode), IRI0, True) :-
    (   pfc_read_mode(Mode),
        '$absolute_file_name'(IRI0, Canonical0),
        pfc_entry_name(Canonical0, Canonical),
        pfc_iri_offset(Canonical, _Offset)
    ->  pfc_access_ok(Mode, Canonical, True)
    ;   True = false
    ).
pfc_iri_hook(time, IRI, Time) :-
    setup_call_cleanup(
            iri_pfc_zipper_ex(IRI, Zipper),
            pfc_pfc_zipper_file_property(Zipper, _, time, Time),
            zip_close_(Zipper, _)).
pfc_iri_hook(size, IRI, Size) :-
    setup_call_cleanup(
            iri_pfc_zipper_ex(IRI, Zipper),
            pfc_pfc_zipper_file_property(Zipper, _, size, Size),
            zip_close_(Zipper, _)).

pfc_read_mode(read).
pfc_read_mode(exists).
pfc_read_mode(file).
pfc_read_mode(directory).

pfc_entry_name(Entry, Entry).
pfc_entry_name(Entry0, Entry) :-
    \+ sub_atom(Entry0, _, _, 0, /),
    atom_concat(Entry0, /, Entry).


%!  pfc_access_ok(+Access, +Entry, -Ok) is det.
%
%   This assumes directories are added with a trailing /

pfc_access_ok(directory, Entry, True) :-
    !,
    (   sub_atom(Entry, _, _, 0, /)
    ->  True = true
    ;   True = false
    ).
pfc_access_ok(file, Entry, True) :-
    !,
    (   sub_atom(Entry, _, _, 0, /)
    ->  True = false
    ;   True = true
    ).
pfc_access_ok(_, _, true).

%!  pfc_iri_pfc_zipper(+IRI, -Zipper) is semidet.
%!  iri_pfc_zipper_ex(+IRI, -Zipper) is det.
%
%   Find and position a pfc_zipper for IRI. Fails if this cannot be found.

pfc_iri_pfc_zipper(IRI, Clone) :-
    '$absolute_file_name'(IRI, Canonical),
    pfc_iri_offset(Canonical, Offset),
    '$rc_handle'(Zipper),
    zip_clone(Zipper, Clone),
    zipper_goto(Clone, offset(Offset)).

iri_pfc_zipper_ex(IRI, Zipper) :-
    pfc_iri_pfc_zipper(IRI, Zipper),
    !.
iri_pfc_zipper_ex(IRI, _Zipper) :-
    '$existence_error'(source_sink, IRI).

%!  pfc_iri_offset(+IRI, -Offset) is semidet.
%
%   True when Offset is the pfc_zipper offset where we can find IRI.

:- dynamic pc_index_db/2, pc_index_done/0.
:- volatile pc_index_db/2, pc_index_done/0.

pfc_iri_offset(Entry, Offset) :-
    pc_index_done,
    !,
    pc_index_db(Entry, Offset).
pfc_iri_offset(Entry, Offset) :-
    with_mutex('$rc', index_pc),
    !,
    pc_index_db(Entry, Offset).

index_pc :-
    pc_index_done,
    !.
index_pc :-
    '$rc_handle'(Zipper),
    setup_call_cleanup(
        zip_clone(Zipper, Clone),
        ( zipper_goto(Clone, first),
          index_pc(Clone)
        ),
        zip_close_(Clone, _)),
    asserta(pc_index_done).

index_pc(Zipper) :-
    pfc_pfc_zipper_file_property(Zipper, Name, offset, Offset),
    atom_concat('pfc://', Name, IRI),
    assertz(pc_index_db(IRI, Offset)),
    (   zipper_goto(Zipper, next)
    ->  index_pc(Zipper)
    ;   true
    ).


%!  pfc_pfc_zipper_file_property(+Zipper, -Name, +Prop, -Value)

pfc_pfc_zipper_file_property(Zipper, Name, Prop, Value) :-
    zip_file_info_(Zipper, Name, Info),
    pfc_zip_prop_arg(Prop, Arg),
    arg(Arg, Info, Value).

pfc_zip_prop_arg(size,   2).
pfc_zip_prop_arg(time,   5).
pfc_zip_prop_arg(offset, 6).

:- volatile(user:resource/2).
:- asserta(user:resource(app/pfc_res, pfc_res(.))).
:- asserta(user:file_search_path(pfc_res, 'res://app/pfc_res')).


%%      prolog_clause:open_source(+File, -Stream) is semidet.
%
%       Open PFC non-file sources.

:- if(current_prolog_flag(pfc_version,3.0)).
pfc_lib_name(library('pfc3.0/pfc_3_0_full')).
:- else.
pfc_lib_name(library('pfc2.0/pfc_2_0_includes')).
:- endif.

:- multifile prolog_clause:open_source/2.

prolog_clause:open_source(pfc_res(Module), Stream) :-
        pfc_incl_module_source(Module, Source),
        open_string(Source, Stream).

prolog_clause:open_source(PfcInclFile, Stream) :-
        atom_concat('pfc://', Module, PfcInclFile), !,
        pfc_incl_module_source(Module, Source),
        open_string(Source, Stream).

prolog_clause:open_source(PfcInclFile, Stream) :-
        atom_concat('res://app/pfc_res/', Module, PfcInclFile), !,
        pfc_incl_module_source(Module, Source),
        open_string(Source, Stream).

pfc_incl_module_source(Module, Source):-
  pfc_lib_name(LibName),
  format(string(Source),":- module(~q,[]). :- include(~q).",[Module, LibName]).

:- meta_predicate(include_module_file(:,+)).

include_module_file(M:PfcInclFile,Module):- 
   absolute_file_name(PfcInclFile,FullName,[access(read),file_type(prolog)]), 
   %time_file(FullName,Time),
   open(FullName, read, Stream),
   M:load_files(FullName,[
      module(Module),
      if(always),
      stream(Stream), 
      register(false),
      must_be_module(false),
      reexport(true),
      silent(false)]),!,
   close(Stream).


include_pfc_res(Module,PfcInclFile):- 
   % Version 3.0
   % atom_concat('pfc://', Module, PfcInclFile),
   absolute_file_name(pfc_res(Module),PfcInclFile),
   
   prolog_clause:open_source(PfcInclFile, Stream),    
   pfc_lib_name(LibName),
   absolute_file_name(LibName,FullName,[access(read),file_type(prolog)]), time_file(FullName,Time),
   Module:load_files(PfcInclFile,[module(Module),
     if(always),
     modified(Time),
     stream(Stream), % register(false),
     must_be_module(false),
     reexport(true),
     silent(false)]),!.          

% add_pfc_to_module(Module):- module_property(Module,class(library)),!.


