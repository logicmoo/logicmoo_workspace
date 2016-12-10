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
 * as a LICENSE file. If not, obtain it from 
 * http://www.perlfoundation.org/artistic_license_2_0.
 */

/* Interface */

:- module(cplint_r,
          [ prob_bar_r/1,
            mc_prob_bar_r/1,
            mc_sample_bar_r/2,
            mc_sample_arg_bar_r/3,
            mc_sample_arg_first_bar_r/3,
            mc_rejection_sample_arg_bar_r/4,
            mc_mh_sample_arg_bar_r/5,
            histogram_r/2,
            density_r/4,
            densities_r/3
          ]).


/* 
 * Reexport library(mcintyre) and library(pita)
 * once I figure out how to do it.
 */


/* Dependencies */

:- use_module(library(r/r_call)).
:- use_module(library(r/r_data)).
:- use_module(swish(r_swish)).
:- use_module(library(lists)).
:- use_module(library(pita)).
:- use_module(library(mcintyre)).


/* Meta predicate definitions. */

:-meta_predicate prob_bar_r(:).
:-meta_predicate mc_prob_bar_r(:).
:-meta_predicate mc_sample_bar_r(:,+).
:-meta_predicate mc_sample_arg_bar_r(:,+,+).
:-meta_predicate mc_sample_arg_first_bar_r(:,+,+).
:-meta_predicate mc_rejection_sample_arg_bar_r(:,:,+,+).
:-meta_predicate mc_mh_sample_arg_bar_r(:,:,+,+,+).

/*********** 
 * Helpers *
 ***********/

/* Debug purposes
 *
 *  use_rendering(table).
 *  <- df. % name of a data frame.
 */

bin_width(Min,Max,NBins,Width) :-
  D is Max-Min,
  Width is D/NBins.

load_r_libraries :-
    <- library("ggplot2").

finalize_r_graph :-
	r_download.


r_row(X,Y,r(X,Y)).

get_set_from_list(L,R) :-
    maplist(key,L,X),
    maplist(y,L,Y),
    maplist(r_row,X,Y,R).


/*******************************************
 * Plot predicates *************************
 *******************************************
 * cd swish/examples/inference *************
 * grep -l "<predicate_name>(" *.pl | less *
 *******************************************/

/* pita */


/* Scale between 0 and 1 with 10 ticks (0.1,0.2,...,1) */
geom_prob_bar(PTrue,PFalse) :-
    L=[PTrue-1,PFalse-1],
    get_set_from_list(L,R),
    r_data_frame_from_rows(df1, R),
    colnames(df1) <- c("prob", "tf"),
    df <- data.frame(
        ids=factor(
            c("T","F")
        ),
        probabilities=c(df1$prob)
    ),
    <- ggplot(
        data=df,
        aes(
            x=ids,
            y=probabilities,
            fill=ids
        )
    ) + geom_bar(
        stat="identity",
        width=0.05,
        position=position_dodge()
    )
    + scale_y_continuous(
        breaks=seq(0,1,0.1)
    )
    + coord_flip(ylim=c(0,1)).
    

/**
 * prob_bar_r(:Query:atom) is nondet
 *
 * The predicate computes and plots the probability of Query
 * as a bar chart with a bar for the probability of Query true and
 * a bar for the probability of Query false.
 * If Query is not ground, it returns in backtracking all ground
 * instantiations of
 * Query together with their probabilities
 *
 * PF is 1.0-PT i.e:
 * Probability False = 1 - Probability True. 
 */
prob_bar_r(M:Goal) :-
    load_r_libraries,
    s(M:Goal,PT),
    PF is 1.0-PT,
    geom_prob_bar(PT,PF),
    finalize_r_graph.


geom_mc_prob_bar(PTrue,PFalse) :-
    writeln("TODO").    

/**
 * mc_prob_bar(:Query:atom) is det
 *
 * The predicate computes the probability of the ground query Query
 * and returns it as a dict for rendering with c3 as a bar chart with
 * a bar for the probability of Query true and a bar for the probability of
 * Query false.
 * If Query is not ground, it considers it as an existential query
 * and returns the probability that there is a satisfying assignment to
 * the query.
 */
mc_prob_bar_r(M:Goal):-
    writeln("TODO").



/* mcintyre */

geom_sample_bar(PTrue,PFalse) :-
    L=[PTrue-1,PFalse-1],
    get_set_from_list(L,R),
    r_data_frame_from_rows(df1, R),
    colnames(df1) <- c("prob", "tf"),
    df <- data.frame(
        ids=factor(
            c("T","F")
        ),
        probabilities=c(df1$prob)
    ),
    <- ggplot(
        data=df,
        aes(
            x=ids,
            y=probabilities,
            fill=ids
        )
    ) + geom_bar(
        stat="identity",
        width=0.05,
        position=position_dodge()
    )
    + coord_flip().


/*
    df <- data.frame(ids=factor(c("T","F"),levels=c("T","F")),probabilities=c(df1$prob)),
    <- ggplot(data=df,aes(x=ids,y=probabilities)) + geom_bar(stat="identity",width=0.05,position="dodge") + coord_flip().
*/

/**
 * mc_sample_bar_r(:Query:atom,+Samples:int) is det
 *
 * The predicate samples Query a number of Samples times and returns
 * a dict for rendering with c3 as a bar chart with
 * a bar for the number of successes and a bar for the number
 * of failures.
 * If Query is not ground, it considers it as an existential query.
 */
mc_sample_bar_r(M:Goal,S):-
    load_r_libraries,
    mc_sample(M:Goal,S,T,F,_P),
    geom_sample_bar(T,F),
    finalize_r_graph.


mc_sample_arg_bar_r(A,B,C).


mc_sample_arg_first_bar_r(A,B,C).


mc_rejection_sample_arg_bar_r(A,B,C,D).


mc_mh_sample_arg_bar_r(A,B,C,D,E).


/*
 * BinWidth was added because it was recomended on the R documentation,
 * see docs.ggplot2.org/current/geom_histogram.html
 */
geom_histogram(L,NBins,BinWidth) :-
    nbinS <- NBins,
    binwidtH <- BinWidth,
    get_set_from_list(L,R),
    r_data_frame_from_rows(df, R),
    colnames(df) <- c("x", "y"),
    <- ggplot(
        data=df,
    	aes_string(x="x")
    ) + geom_histogram(
        bins=nbinS,
        binwidth=binwidtH
	).

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
    finalize_r_graph.

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


/*
 *   + scale_y_log10() 
 */
geom_density(L) :-
    get_set_from_list(L,R),
    r_data_frame_from_rows(df, R),
    colnames(df) <- c("x", "y"),
    <- ggplot(data=df,aes_string(x="x",y="y",group=1)) + geom_line() + geom_point().

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
    finalize_r_graph.


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
    df <- data.frame(
        x=df1$x,
        y=c(df1$y1,df2$y2),
        group=rep(c("pre","post"))
    ),
    <- ggplot(
    	data=df,
    	aes(
	    	x=x,
		    y=y,
    		group=group
	    )
	) + geom_line(
		aes(
			color=group
		)
	) + geom_point(
		aes(
			color=group
		)
	).

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
densities_r(Pri0,Post0,NBins) :-
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
    finalize_r_graph.

