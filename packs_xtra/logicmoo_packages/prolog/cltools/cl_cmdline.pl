/* -*- Mode: Prolog -*- */

:- use_module(library('cltools/cl')).
:- use_module(library('cltools/cl_io')).

main :-
        current_prolog_flag(argv, Arguments),
        append(_SytemArgs, [--|Args], Arguments), !,
        parse_args(Args,Opts),
        (   member(infmt(InFmt),Opts)
        ->  true
        ;   InFmt=clif),
        (   member(outfmt(OutFmt),Opts)
        ->  true
        ;   OutFmt=clif),
        forall(member(rest(Arg),Opts),
               load_cltext(Arg,InFmt)),
        % -- MACRO EXPANSION --
        forall(member(macro(F),Opts),
               macro_expand_using_file(F,Opts)),
        store_cltext(_,OutFmt).

macro_expand_using_file(F,_Opts) :-
        parse_cltext(F,MT),
        text_sentence(MT,MacroText),
        cl:cltext(Text),
        macro_expand(Text,cltext([MacroText]),Text2),
        clear_cltext,
        assert_cltext(Text2).
        
parse_args([],[]).
parse_args(Args,[Opt|Opts]) :-
        parse_arg(Args,Rest,Opt),
        !,
        parse_args(Rest,Opts).
parse_args([A|Args],[rest(A)|Opts]) :-
        parse_args(Args,Opts).

parse_arg(['--from',Fmt|L],L,infmt(Fmt)).
parse_arg(['--to',Fmt|L],L,outfmt(Fmt)).
parse_arg(['--macro-file',MacroFile|L],L,macro(MacroFile)).

