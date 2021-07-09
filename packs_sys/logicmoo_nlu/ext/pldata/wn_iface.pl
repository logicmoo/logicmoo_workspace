:- module(wn_iface, [load_wordnet/0]).

:- if(\+ (exists_file('wn_frames.qlf'))).
:- format(user_error,'~NQuickload Compiling (qcompile/1) wn_frames.qlf (this may take 60-120 seconds the very first time) ... ~n',[]),
   prolog_statistics:time(load_files(wn_frames,[qcompile(auto)])),
   % prolog_statistics:time(qcompile(wn_frames)),
   format(user_error,'~NMade wn_frames.qlf ~n',[]).
:- endif.


:- if((exists_file('wn_frames.qlf'))).

:- format(user_error,'~NLoading wn_frames.qlf  ... ~n',[]).
:- reexport('wn_frames.qlf').

:- else.
:- format(user_error,'~NMissing wn_frames.qlf  ... ~n',[]).

:- if(exists_source(wn_frames)).
:- format(user_error,'~NLoading wn_frames instead  ... ~n',[]).
:- reexport(wn_frames).
:- endif.

:- endif.
/*
:- format(user_error,'~NMissing wn_frames.qlf  ... ~n',[]).

:- if(exists_source(wn_frames)).
:- format(user_error,'~NLoading wn_frames.pl instead  ... ~n',[]).
:- re export(wn_frames).
:- format(user_error,'~NLoaded wn_frames. ~n',[]).
:- endif.
*/

load_wordnet:- use_module(wn_frames).


