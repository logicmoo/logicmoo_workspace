/* -*- Mode: Prolog -*- */

:- module(cl_io,
          [load_cltext/1,
           load_cltext/2,
           load_cltext/3,

           parse_cltext/2,
           parse_cltext/3,
           parse_cltext/4,
           
           store_cltext/2,
           store_cltext/3,
           
           serialize_cltext/3,
           serialize_cltext/4,

           convert_cltext/2,
           convert_cltext/5
          ]).

:- multifile parse_cltext_hook/4.
:- multifile serialize_cltext_hook/4.

:- multifile cl:cltext/1.
:- dynamic cl:cltext/1.

%% load_cltext(+File)
% populates cl cltext from File. Attempts to guess format from extension
load_cltext(File) :-
        load_cltext(File,_).

%% load_cltext(+File,+Fmt)
% populates cl cltext from File.
% Fmt = clif | prover9 | prolog | ...
% (for non-standard fmts you may have to ensure the required io model is loaded
%  so the hooks are visible)
load_cltext(File,Fmt) :-
        load_cltext(File,Fmt,[]).

%% load_cltext(+File,+Fmt,+Opts)
% as load_cltext/2 with options
% Opts are Fmt specific - see individual modules for details
load_cltext(File,Fmt,Opts) :-
        parse_cltext(File,Fmt,Text,Opts),
        assert(cl:cltext(Text)).

%% parse_cltext(+File,?Text)
% parses Text from File. Attempts to guess format from extension
parse_cltext(File,Text) :-
        parse_cltext(File,_,Text).

%% parse_cltext(+File,+Fmt,?Text)
% populates cl cltext from File.
% Fmt = clif | prover9 | prolog | ...
% (for non-standard fmts you may have to ensure the required io model is parseed
%  so the hooks are visible)
parse_cltext(File,Fmt,Text) :-
        parse_cltext(File,Fmt,Text,[]).

%% parse_cltext(+File,+Fmt,Text,+Opts)
% as parse_cltext/3 with options
% Opts are Fmt specific - see individual modules for details
parse_cltext(File,Fmt,Text,Opts) :-
        var(Fmt),
        guess_format(File,Fmt,Opts),
        nonvar(Fmt), % should be satisfied, but just to be safe..
        !,
        parse_cltext(File,Fmt,Text,Opts).
parse_cltext(File,Fmt,Text,_Opts) :-
        nonvar(Fmt),
        (   Fmt=prolog
        ;   Fmt=pl),
        !,
        consult_cltext(File,Text).
parse_cltext(File,Fmt,Text,Opts) :-
        io_handler(read,Fmt),
        parse_cltext_hook(File,Fmt,Text,Opts),
        !.
parse_cltext(File,Fmt,Text,Opts) :-
        throw(cl_io('cannot parse fmt for',File,Fmt,Text,Opts)).

consult_cltext(File,Text) :-
        open(File,read,S,[]),
        read(S,cltext(Text)),
        close(S).


%% store_cltext(+File,+Fmt)
% stores cl cltext to File.
% Fmt = rdf | owlx | prolog | ...
% (for non-standard fmts you may have to ensure the required io model is loaded
%  so the hooks are visible)
store_cltext(File,Fmt) :-
        store_cltext(File,Fmt,[]).

%% store_cltext(+File,+Fmt,+Opts)
% as store_cltext/2 with options
% Opts are Fmt specific - see individual modules for details
store_cltext(File,Fmt,Opts) :-
        cl:cltext(Text),
        !,
        serialize_cltext(File,Fmt,Text,Opts).


%% serialize_cltext(+File,+Fmt,?Text)
% stores cl cltext to File.
% Fmt = rdf | owlx | prolog | ...
% (for non-standard fmts you may have to ensure the required io model is loaded
%  so the hooks are visible)
serialize_cltext(File,Fmt,Text) :-
        serialize_cltext(File,Fmt,Text,[]).

%% serialize_cltext(+File,+Fmt,?Text,+Opts)
% as serialize_cltext/2 with options
% Opts are Fmt specific - see individual modules for details
serialize_cltext(File,Fmt,Text,_Opts) :-
        nonvar(Fmt),
        (   Fmt=prolog
        ;   Fmt=pl),
        !,
        (   nonvar(File)
        ->  tell(File)
        ;   true),
        format('~q.~n',[Text]).
serialize_cltext(File,Fmt,Text,Opts) :-
        io_handler(write,Fmt),
        serialize_cltext_hook(File,Fmt,Text,Opts),
        !.
serialize_cltext(File,Fmt,Text,Opts) :-
        throw(cl_io('cannot store fmt for',File,Fmt,Text,Opts)).


convert_cltext(FileIn,FmtOut) :-
        convert_cltext(FileIn,_,_,FmtOut,[]).

convert_cltext(FileIn,FmtIn,FileOut,FmtOut,Opts) :-
        parse_cltext(FileIn,FmtIn,Text,Opts),
        serialize_cltext(FileOut,FmtOut,Text,Opts).

io_handler(Dir,Fmt) :-
        forall(format_module(Dir,Fmt,Mod),
               (   atom_concat('cltools/',Mod,TMod), % TODO: check for more elegant way to do this..
                   ensure_loaded(library(TMod)))).

guess_format(File,Fmt,_Opts) :-
        concat_atom(Toks,'.',File),
        reverse(Toks,[Suffix,_|_]),
        suffix_format(Suffix,Fmt).

suffix_format(clif,clif).
suffix_format(clf,clif).
suffix_format(pro,prolog).
suffix_format(prolog,prolog).
suffix_format(pl,prolog).
suffix_format(owlpl,prolog).
suffix_format(plsyn,plsyn).
suffix_format(owl,owl).
suffix_format(ttl,owl).
suffix_format(owlx,owlx).


:- multifile format_module/3.
format_module(read,clif,clif_parser).
format_module(read,owl,cl_owl2).

format_module(write,clif,clif_writer).
format_module(write,prover9,p9_writer).
format_module(write,owl,cl_owl2).
format_module(write,lp,lp_writer).


/** <module> 

  ---+ Synopsis

==
:- use_module(library('thea2/cl_io')).
:- use_module(library('thea2/cl')).

% reads in RDF/OWL and serializes to other formats
test :-
        load_cltext('testfiles/wine.owl'), % auto-detects RDF serialization
        store_cltext('testfiles/wine.owlpl',prolog),
        store_cltext('testfiles/wine.pl',plsyn),
        store_cltext('testfiles/wine.owlx',owlx),
        store_cltext('testfiles/wine.dlp',dlp),
        store_cltext('testfiles/wine.owlms',manchester).

==

---+ Details

Extensible: format-specific modules can define hooks

*/
