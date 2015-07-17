:- module(rtchecks_disc, [disc1/1],
	  [assertions, nativeprops, rtchecks]).

:- discontiguous(disc1/1).

:- discontiguous('disc1/1$rtc2'/1). % TODO: fix this in the transformation --EMM

:- pred disc1(A) : (A=a).
disc1(a).
:- pred disc1(A) : (A=b).
disc1(b).
:- pred disc1(A) : (A=c).
disc1(c).
