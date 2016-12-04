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
            density_r/4,
            densities_r/3
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
    /*
        use_rendering(table).
        <- df. % name of a data frame.
    */

r_row(X,Y,r(X,Y)).

get_set_from_list(L,R) :-
    maplist(key,L,X),
    maplist(y,L,Y),
    maplist(r_row,X,Y,R).


/* ggplot2 geom wrappers. */

/*
 * BinWidth was added because it was recomended on the R documentation,
 * see docs.ggplot2.org/current/geom_histogram.html
 */
geom_histogram(L,NBins,BinWidth) :-
    nbinS <- NBins,
    binwidtH <- BinWidth,
    nbinS <- NBins,
    get_set_from_list(L,R),
    r_data_frame_from_rows(df, R),
    colnames(df) <- c("x", "y"),
    <- ggplot(data=df,aes_string(x="x")) + geom_histogram(bins=nbinS,binwidth=binwidtH).


/*
 *   + scale_y_log10() 
 */
geom_density(L) :-
    get_set_from_list(L,R),
    r_data_frame_from_rows(df, R),
    colnames(df) <- c("x", "y"),
    <- ggplot(data=df,aes_string(x="x",y="y",group=1)) + geom_line() + geom_point().


/*
 * The numbers seem correct, although the representation slightly differs 
 * from the one made with c3js.
 *
 * Is there a more efficient way of handling the data frame(s)?
 *
 * df <- merge(df1,df2,by.x="x"),
 * <- ggplot(data=df,aes_string(x="x",y="y1",group=1)) + geom_line() + geom_point().
 */
geom_densities(LPr,LPo) :-
    get_set_from_list(LPr,R1),
    get_set_from_list(LPo,R2),
    r_data_frame_from_rows(df1, R1),
    r_data_frame_from_rows(df2, R2),
    colnames(df1) <- c("x", "y1"),
    colnames(df2) <- c("x", "y2"),
    df <- data.frame(x=df1$x,y=c(df1$y1,df2$y2),group=rep(c("pre","post"))),
    <- ggplot(data=df,aes(x=x,y=y,group=group)) + geom_line(aes(color=group)) + geom_point(aes(color=group)).
    


/***************************************** 
 * Plot predicates ***********************
 *****************************************
 * cd swish/examples/inference ***********
 * grep -l histogram *.pl | less *********
 * grep -l density *.pl | less ***********
 * grep -l densities *.pl | less *********
 *****************************************/


/**
 * histogram_r(+List:list,+NBins:int) is det
 *
 * Draws a histogram of the samples in List dividing the domain in
 * NBins bins. List must be a list of couples of the form [V]-W or V-W
 * where V is a sampled value and W is its weight.
 */
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
    keysort(L1,L),
    bin_width(Min,Max,NBins,BinWidth),
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

/**
 * densities_r(+PriorList:list,+PostList:list,+NBins:int) is det
 *
 * Draws a line chart of the density of two sets of samples, usually
 * prior and post observations. The samples from the prior are in PriorList
 * while the samples from the posterior are in PostList
 * as couples [V]-W or V-W where V is a value and W its weigth.
 * The lines are drawn dividing the domain in
 * NBins bins.
 */
densities_r(Pri0,Post0,NBins):-
  load_r_libraries,
  maplist(to_pair,Pri0,Pri1),
  maplist(to_pair,Post0,Post1),
  maplist(key,Pri1,Pri),
  maplist(key,Post1,Post),
  append(Pri,Post,All),
  max_list(All,Max),
  min_list(All,Min),
  bin_width(Min,Max,NBins,BinWidth),
  keysort(Pri1,Pr),
  keysort(Post0,Po),
  bin(NBins,Pr,Min,BinWidth,LPr),
  bin(NBins,Po,Min,BinWidth,LPo),
  geom_densities(LPr,LPo),
  r_download.

