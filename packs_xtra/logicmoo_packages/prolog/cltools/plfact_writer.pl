/* -*- Mode: Prolog -*- */

:- module(plfact_writer,
          [
           export_plfact/2
           ]).

export_plfact(Text,Opts) :-
        writeq(Text),
        nl.
