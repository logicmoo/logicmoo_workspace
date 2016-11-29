/* cplint_r.pl
 *
 * Copyright (c) 2016 Franco Masotti (franco.masotti@student.unife.it)
 *
 * This is free software: you can redistribute it and/or modify it under the 
 * terms of the Artistic License 2.0 as published by The Perl Foundation.
 * 
 * This source is distributed in the hope that it will be useful, but WITHOUT 
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE. See the Artistic License 2.0 for more 
 * details.
 *
 * You should have received a copy of the Artistic License 2.0 along the source 
 * as a COPYING file. If not, obtain it from 
 * http://www.perlfoundation.org/artistic_license_2_0.
 */

:- module(cplint_r,
          [ histogram_r/2,
            density_r/4
          ]).

:- use_module(library(r/r_call)).
:- use_module(library(r/r_data)).
:- use_module(swish(r_swish)).
:- use_module(library(lists)).

/*********** 
 * Helpers *
 ***********/


/* 
 * Lists 
 */

to_pair([E]-W,E-W):- !.
to_pair(E-W,E-W).


key(K-_,K).


y(_ - Y,Y).


bin(0,_L,_Min,_BW,[]):-!.

bin(N,L,Lower,BW,[V-Freq|T]):-
  V is Lower+BW/2,
  Upper is Lower+BW,
  count_bin(L,Lower,Upper,0,Freq,L1),
  N1 is N-1,
  bin(N1,L1,Upper,BW,T).

count_bin([],_L,_U,F,F,[]).

count_bin([H-_W|T0],L,U,F0,F,T):-
  H<L,!,
  count_bin(T0,L,U,F0,F,T).

count_bin([H-W|T0],L,U,F0,F,T):-
  (H>=U->
    F=F0,
    T=[H-W|T0]
  ;
    F1 is F0+W,
    count_bin(T0,L,U,F1,F,T)
  ).


bin_width(Min,Max,NBins,Width) :-
  D is Max-Min,
  Width is D/NBins.


/* 
 * R
 */

load_r_libraries :-
    <- library("ggplot2").
    /* Debug purposes */
    use_rendering(table).

r_row(X,Y,r(X,Y)).

xy_from_list(L,R) :-
    maplist(key,L,X),
    maplist(y,L,Y),
    maplist(r_row,X,Y,R).

/* ggplot2 geom wrappers. */

/*
 * FIXME: Change naming of Rserve.tmp.* variables,
 * this was discovered using:
 * <- df.
 */
geom_histogram(L,NBins,BinWidth) :-
    nbinS <- NBins,
    binwidtH <- BinWidth,
    xy_from_list(L,R),
    r_data_frame_from_rows(df, R),
    <- ggplot(data=df,aes(x='Rserve.tmp.0')) + geom_histogram(binwidth=binwidtH,bins=nbinS).


/* FIXME:
 * Add use of Nbins and BinWidth, compare output of R and c3js version to see 
 * what I mean.
 * 
 */
geom_density(L) :-
    xy_from_list(L,R),
    r_data_frame_from_rows(df, R),
    <- ggplot(data=df,aes(x='Rserve.tmp.0',y='Rserve.tmp.1',group=1)) + geom_line() + geom_point().

/*    <- df.
 */

/***************************************** 
 * Plot predicates ***********************
 *****************************************
 * grep -l histogram *.pl | less *********
 * grep -l density *.pl | less ***********
 *****************************************/


/**
 * histogram_r(+List:list,+NBins:int) is det
 *
 * Draws a histogram of the samples in List dividing the domain in
 * NBins bins. List must be a list of couples of the form [V]-W or V-W
 * where V is a sampled value and W is its weight.
 */
/* Input controls. Don't change. */
histogram_r(L0,NBins) :-
    load_r_libraries,
    maplist(to_pair,L0,L1),
    maplist(key,L1,L2),
    max_list(L2,Max),
    min_list(L2,Min),
    histogram_r(L0,NBins,Min,Max),
    r_download.

/**
 * histogram_r(+List:list,+NBins:int,+Min:float,+Max:float) is det
 *
 * Draws a histogram of the samples in List dividing the domain in
 * NBins bins. List must be a list of couples of the form [V]-W or V-W
 * where V is a sampled value and W is its weight. The minimum and maximum
 * values of the domains must be provided.
 */
histogram_r(L0,NBins,Min,Max) :-
    maplist(to_pair,L0,L1),
    bin_width(Min,Max,NBins,BinWidth),
    keysort(L1,L),
    geom_histogram(L,NBins,BinWidth).


/**
 * density_r(+List:list,+NBins:int,+Min:float,+Max:float) is det
 *
 * Draws a line chart of the density of a sets of samples.
 * The samples are in List
 * as couples [V]-W or V-W where V is a value and W its weigth.
 * The lines are drawn dividing the domain in
 * NBins bins.
 */
density_r(Post0,NBins,Min,Max) :-
    load_r_libraries,
    maplist(to_pair,Post0,Post),
    bin_width(Min,Max,NBins,BinWidth),
    keysort(Post,Po),
    bin(NBins,Po,Min,BinWidth,LPo),
    geom_density(LPo),
    r_download.





