/* cplint_r.pl
 *
 * Copyright (c) 2016-2017 Franco Masotti (franco.masotti@student.unife.it)
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
          [ build_xy_list/3,
            r_row/3,
            get_set_from_xy_list/2,
            prob_bar_r/1,
            prob_bar_r/2,
            mc_prob_bar_r/1,
            mc_sample_bar_r/2,
            mc_sample_arg_bar_r/3,
            mc_sample_arg_first_bar_r/3,
            mc_rejection_sample_arg_bar_r/4,
            mc_mh_sample_arg_bar_r/5,
            mc_mh_sample_arg_bar_r/6,
            histogram_r/2,
            density_r/1,
            densities_r/2,
            compute_areas_diagrams_r/3
          ]).


/* Dependencies */

:- use_module(library(r/r_call)).
:- use_module(library(r/r_data)).
:- use_module(library(lists)).
:- use_module(library(pita)).
:- use_module(library(mcintyre)).
:- use_module(library(auc)).

/* Optional module */
:- use_module(swish(lib/r_swish)).

/* Meta predicate definitions. */

:-meta_predicate prob_bar_r(:).
:-meta_predicate prob_bar_r(:,:).
:-meta_predicate mc_prob_bar_r(:).
:-meta_predicate mc_sample_bar_r(:,+).
:-meta_predicate mc_sample_arg_bar_r(:,+,+).
:-meta_predicate mc_sample_arg_first_bar_r(:,+,+).
:-meta_predicate mc_rejection_sample_arg_bar_r(:,:,+,+).
:-meta_predicate mc_mh_sample_arg_bar_r(:,:,+,+,+).
:-meta_predicate mc_mh_sample_arg_bar_r(:,:,+,+,+,+).

:- multifile sandbox:safe_primitive/1.
sandbox:safe_primitive(current_predicate(_)).

/*********** 
 * Helpers *
 ***********/

check_modules :-
    current_module(r_call),
    current_module(r_data),
    current_module(lists),
    current_module(pita),
    current_module(mcintyre),
    current_module(auc).

check_modules :-
    writeln("ERROR: Library/ies missing"),
    abort.

/* Debug purposes
 *
 *  use_rendering(table). % should be available by default.
 *  <- df. % name of a data frame.
 */

load_r_libraries :-
    /* To enable pdf output instead of using the default plotting window, add 
     * the following command
     */
    /* <- pdf("plot.pdf"), */
    check_modules,
    !,
    <- library("ggplot2").

/* Only do if library(r_swish) exists, otherwise return true. */
finalize_r_graph :-
    current_predicate(r_download/0),
    !,
    r_download.

finalize_r_graph :-
    true.

bin_width(Min,Max,NBins,Width) :-
    D is Max-Min,
    Width is D/NBins.

/**
 * build_xy_list(X:list,Y:list,Out:list) is det
 *
 * Given to lists X and Y build an output list Out
 * in the form [X1-Y1,X2-Y2,...,XN-YN].
 */
build_xy_list([], [], []).

build_xy_list([XH|XT], [YH|YT], [XH-YH|Out]) :-
        build_xy_list(XT, YT, Out).

/**
 * r_row(X:atom,Y:atom,Out:atom) is det
 *
 * Given two atoms X and Y, build the term r(X,Y) in Out.
 */
r_row(X,Y,r(X,Y)).

/**
 * get_set_from_xy_list(L:list,R:list) is det
 *
 * Given an input list L in the form [X1-Y1,X2-Y2,...,XN-YN], transform it in 
 * an output list R in the form [r(X1,Y1),r(X2,Y2),...,r(XN,YN)]. This means 
 * that R will contain an X-Y relationship which can be then passed to an R 
 * data frame. 
 */
get_set_from_xy_list(L,R) :-
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


/* Scale between 0 and 1 with 10 ticks (0.1,0.2,...,1)
 * This represents a probability between 0 and 1.
 */
geom_prob_bar(PTrue,PFalse) :-
    X=['T','F'],
    Y=[PTrue,PFalse],
    build_xy_list(X,Y,L),
    get_set_from_xy_list(L,R),
    r_data_frame_from_rows(df1, R),
    colnames(df1) <- c("names", "prob"),
    df <- data.frame(
        ids=as.character(df1$names),
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
        width=0.5
    )
    + scale_y_continuous(
        breaks=seq(0,1,0.1)
    )
    + coord_flip(ylim=c(0,1))
    + theme(aspect.ratio=1/2).

/**
 * prob_bar_r(:Query:atom) is nondet
 *
 * The predicate computes and plots the probability of Query
 * as a bar chart with a bar for the probability of Query true and
 * a bar for the probability of Query false.
 * If Query is not ground, it returns in backtracking all ground
 * instantiations of Query together with their probabilities
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


/**
 * prob_bar_r(:Query:atom,:Evidence:atom) is nondet
 *
 * The predicate computes the probability of the Query given Evidence
 * as a bar chart with
 * a bar for the probability of Query true and a bar for the probability of
 * Query false given Evidence.
 * If Query /Evidence are not ground, it returns in backtracking all
 * ground instantiations of
 * Query/Evidence together with their probabilities
 */
prob_bar_r(M:Goal,M:Evidence):-
    load_r_libraries,
    prob(M:Goal,M:Evidence,PT),
    PF is 1.0-PT,
    geom_prob_bar(PT,PF),
    finalize_r_graph.

/* mcintyre */


/**
 * mc_prob_bar_r(:Query:atom) is det
 *
 * See prob_bar.
 */
mc_prob_bar_r(M:Goal):-
    load_r_libraries,
    mc_prob(M:Goal,PT),
    PF is 1.0-PT,
    geom_prob_bar(PT,PF),
    finalize_r_graph.


geom_mc_sample_bar(PTrue,PFalse) :-
    X=['T','F'],
    Y=[PTrue,PFalse],
    build_xy_list(X,Y,L),
    get_set_from_xy_list(L,R),
    r_data_frame_from_rows(df1, R),
    colnames(df1) <- c("names", "prob"),
    df <- data.frame(
        ids=as.character(df1$names),
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
        width=0.5
    )
    + coord_flip()
    + theme(aspect.ratio=1/2).

/**
 * mc_sample_bar_r(:Query:atom,+Samples:int) is det
 *
 * The predicate samples Query a number of Samples times and
 * plots a bar chart with a bar for the number of successes and a bar for the 
 * number of failures.
 * If Query is not ground, it considers it as an existential query.
 */
mc_sample_bar_r(M:Goal,S):-
    load_r_libraries,
    mc_sample(M:Goal,S,T,F,_P),
    geom_mc_sample_bar(T,F),
    finalize_r_graph.


/* Diffs from the previous predicates:
 * ===================================
 *
 * Transform names column into a string column. 
 *
 * The use of max/1 instead of 'NA'/0
 * is a hack (because NA does not work).
 *
 * Reorder by decreasing frequency.
 */
geom_mc_sample_arg_bar(L) :-
    get_set_from_xy_list(L,R),
    r_data_frame_from_rows(df1, R),
    colnames(df1) <- c("names", "prob"),
    df <- data.frame(
        ids=as.character(df1$names),
        probabilities=c(df1$prob)
    ),
    <- ggplot(
        data=df,
        aes(
            x=reorder(
                ids,
                probabilities
            ),
            y=probabilities
        )
    ) + geom_bar(
        stat="identity",
        width=0.5
    )
    + scale_y_continuous(
        breaks=seq(
            0,max(df$probabilities),1
        )
    )
    + coord_flip(
        ylim=c(0,max(df$probabilities))
    ).

/**
 * mc_sample_arg_bar_r(:Query:atom,+Samples:int,?Arg:var) is det
 *
 * The predicate samples Query Samples times. Arg should be a variable
 * in Query.
 * The predicate plots a bar chart
 * with a bar for each possible value of L,
 * the list of values of Arg for which Query succeeds in
 * a world sampled at random.
 * The size of the bar is the number of samples
 * returning that list of values.
 */
mc_sample_arg_bar_r(M:Goal,S,Arg):-
    load_r_libraries,
    mc_sample_arg(M:Goal,S,Arg,ValList0),
    maplist(to_atom,ValList0,ValList),
    geom_mc_sample_arg_bar(ValList),
    finalize_r_graph.


geom_mc_sample_arg_first_bar(L) :-
    get_set_from_xy_list(L,R),
    r_data_frame_from_rows(df1, R),
    colnames(df1) <- c("names", "prob"),
    df <- data.frame(
        ids=as.character(df1$names),
        probabilities=c(df1$prob)
    ),
    <- ggplot(
        data=df,
        aes(
            x=reorder(
                ids,
                probabilities
            ),
            y=probabilities
        )
    ) + geom_bar(
        stat="identity",
        width=0.5
    )
    + coord_flip().

/**
 * mc_sample_arg_first_bar_r(:Query:atom,+Samples:int,?Arg:var) is det
 *
 * The predicate samples Query Samples times. Arg should be a variable
 * in Query.
 * The predicate plots a bar chart
 * with a bar for each value of Arg returned as a first answer by Query in
 * a world sampled at random.
 * The size of the bar is the number of samples that returned that value.
 * The value is failure if the query fails.
 */
mc_sample_arg_first_bar_r(M:Goal,S,Arg):-
    load_r_libraries,
    mc_sample_arg_first(M:Goal,S,Arg,ValList0),
    maplist(to_atom,ValList0,ValList),
    geom_mc_sample_arg_first_bar(ValList),
    finalize_r_graph.
  

geom_mc_rejection_sample_arg_bar(L) :-
    geom_mc_sample_arg_first_bar(L).

/**
 * mc_rejection_sample_arg_bar_r(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var) is det
 *
 * The predicate calls mc_rejection_sample_arg/5 and builds an R graph
 * of the results.
 * It plots a bar chart with a bar for each possible value of L,
 * the list of values of Arg for which Query succeeds
 * given that Evidence is true
 * The size of the bar is the number of samples
 * returning that list of values.
 */
mc_rejection_sample_arg_bar_r(M:Goal,M:Ev,S,Arg):-
    load_r_libraries,
    mc_rejection_sample_arg(M:Goal,M:Ev,S,Arg,ValList0),
    maplist(to_atom,ValList0,ValList),
    geom_mc_rejection_sample_arg_bar(ValList),
    finalize_r_graph.


geom_mc_mh_sample_arg_bar(L) :-
    geom_mc_sample_arg_first_bar(L).

/**
 * mc_mh_sample_arg_bar_r(:Query:atom,:Evidence:atom,+Samples:int,+Mix:int,+Lag:int,?Arg:var) is det
 *
 * The predicate calls mc_mh_sample_arg/7 and builds an R graph
 * of the results.
 * The predicate plots a bar chart
 * with a bar for each possible value of L,
 * the list of values of Arg for which Query succeeds in
 * a world sampled at random.
 * The size of the bar is the number of samples
 * returning that list of values.
 */
mc_mh_sample_arg_bar_r(M:Goal,M:Ev,S,Mix,L,Arg):-
    load_r_libraries,
    mc_mh_sample_arg(M:Goal,M:Ev,S,Mix,L,Arg,ValList0),
    maplist(to_atom,ValList0,ValList),
    geom_mc_mh_sample_arg_bar(ValList),
    finalize_r_graph.


/**
 * mc_mh_sample_arg_bar_r(:Query:atom,:Evidence:atom,+Samples:int,+Lag:int,?Arg:var) is det
 *
 * The predicate call mc_mh_sample_arg/6 and builds a R graph
 * of the results.
 * The predicate plots a bar chart
 * with a bar for each possible value of L,
 * the list of values of Arg for which Query succeeds in
 * a world sampled at random.
 * The size of the bar is the number of samples
 * returning that list of values.
 */
mc_mh_sample_arg_bar_r(M:Goal,M:Ev,S,L,Arg):-
    load_r_libraries,
    mc_mh_sample_arg(M:Goal,M:Ev,S,L,Arg,ValList0),
    maplist(to_atom,ValList0,ValList),
    geom_mc_mh_sample_arg_bar(ValList),
    finalize_r_graph.


geom_histogram(L,Min,Max,BinWidth) :-
    binwidtH <- BinWidth,
    get_set_from_xy_list(L,R),
    r_data_frame_from_rows(df, R),
    colnames(df) <- c("x", "y"),
    miN <- Min,
    maX <- Max,
    <- ggplot(
        data=df,
        aes_string(
            x="x"
        )
    ) + geom_histogram(
        weight="y",
        binwidth=binwidtH
    ) + xlim(
        miN,
        maX
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
    geom_histogram(L,Min,Max,BinWidth).


geom_density(L) :-
    get_set_from_xy_list(L,R),
    r_data_frame_from_rows(df, R),
    colnames(df) <- c("x", "y"),
    <- ggplot(
        data=df,
        aes(x)
    ) + geom_density(
            aes(
                fill="density",
                weights=y
            ),
            alpha=0.5
    ).

/**
 * density_r(+List:list) is det
 *
 * COMMENTS MUST BE CORRECTED
 * Draws a line chart of the density of a sets of samples.
 * The samples are in List
 * as couples [V]-W or V-W where V is a value and W its weigth.
 */
density_r(Post0) :-
     load_r_libraries,
     maplist(to_pair,Post0,Post),
     geom_density(Post),
     finalize_r_graph.

geom_densities(LPr,LPo) :-
    get_set_from_xy_list(LPr,R1),
    get_set_from_xy_list(LPo,R2),
    r_data_frame_from_rows(df1, R1),
    r_data_frame_from_rows(df2, R2),
    colnames(df1) <- c("x1", "y1"),
    colnames(df2) <- c("x2", "y2"),
    df <- data.frame(
        x1=df1$x1,
        x2=df2$x2,
        y1=df1$y1,
        y2=df2$y2
    ),
    alphA <- 0.5,
    <- ggplot(
        data=df
    ) + geom_density(
            aes(
                x=x1,
                fill="pre",
                weights=y1
            ),
            alpha=alphA
    ) + geom_density(
            aes(
                x=x2,
                fill="post",
                weights=y2
            ),
            alpha=alphA
    ) + xlab("x").

/**
 * densities_r(+PriorList:list,+PostList:list) is det
 *
 * COMMENT MUST BE CORRECTED
 * Draws a line chart of the density of two sets of samples, usually
 * prior and post observations. The samples from the prior are in PriorList
 * while the samples from the posterior are in PostList
 * as couples [V]-W or V-W where V is a value and W its weigth.
 */
densities_r(Pri0,Post0) :-
    load_r_libraries,
    maplist(to_pair,Pri0,Pri1),
    maplist(to_pair,Post0,Post1),
    geom_densities(Pri1,Post1),
    finalize_r_graph.

/* auc */

/* geom_line + geom_point + y 0,1,0.1 + x 0,1,0.1 + title */
/* Fix scale: breaks(0,1,0.1) does not seem to work here. */
geom_compute_areas_diagram(L,Title,XName,YName) :-
    get_set_from_xy_list(L,R),
    r_data_frame_from_rows(df, R),
    titlE <- as.character(Title),
    labelS <- labs(title = titlE, x=XName, y=YName),
    colnames(df) <- c("x", "y"),
    <- ggplot(
        data=df,
        aes_string(
            x="x",
            y="y",
            group=1
        )
    ) + geom_line()
    + geom_point()
    + scale_x_continuous(
        limits=c(0,1),
        breaks=seq(0,1,0.1)
    )
    + scale_y_continuous(
        limits=c(0,1),
        breaks=seq(0,1,0.1)
    )
    + labelS
    + theme(
        plot.title = element_text(
            size = rel(2)
        )
    ).


/**
 * compute_areas_diagrams_r(+LG:list,-AUCROC:float,-AUCPR:float) is det
 *
 * The predicate takes as input
 * a list LG of pairs probability-literal in asceding order on probability
 * where the literal can be an Atom (incading a positive example) or \+ Atom,
 * indicating a negative example while the probability is the probability of
 * Atom of being true.
 * The predicate returns
 * AUCROC: the size of the area under the ROC curve
 * AUCPR: the size of the area under the PR curve
 * PR and ROC diagrams are plotted.
 * See http://cplint.lamping.unife.it/example/exauc.pl for an example
 */
compute_areas_diagrams_r(LG,AUCROC,AUCPR) :-
    load_r_libraries,
    compute_areas(LG,AUCROC,ROC0,AUCPR,PR0),
    geom_compute_areas_diagram(ROC0,"ROC","FPR","TPR"),
    finalize_r_graph,
    load_r_libraries,
    geom_compute_areas_diagram(PR0,"PR","Precision","Recall"),
    finalize_r_graph.

sandbox:safe_primitive(cplint_r:build_xy_list(_,_,_)).
sandbox:safe_primitive(cplint_r:r_row(_,_,_)).
sandbox:safe_primitive(cplint_r:get_set_from_xy_list(_,_)).
sandbox:safe_primitive(cplint_r:histogram_r(_,_)).
sandbox:safe_primitive(cplint_r:density_r(_)).
sandbox:safe_primitive(cplint_r:densities_r(_,_)).
sandbox:safe_primitive(cplint_r:compute_areas_diagrams_r(_,_,_)).

sandbox:safe_meta(cplint_r:prob_bar_r(_),[]).
sandbox:safe_meta(cplint_r:prob_bar_r(_,_),[]).
sandbox:safe_meta(cplint_r:mc_prob_bar_r(_),[]).
sandbox:safe_meta(cplint_r:mc_sample_bar_r(_,_),[]).
sandbox:safe_meta(cplint_r:mc_sample_arg_bar_r(_,_,_),[]).
sandbox:safe_meta(cplint_r:mc_sample_arg_first_bar_r(_,_,_),[]).
sandbox:safe_meta(cplint_r:mc_rejection_sample_arg_bar_r(_,_,_,_),[]).
sandbox:safe_meta(cplint_r:mc_mh_sample_arg_bar_r(_,_,_,_,_),[]).
sandbox:safe_meta(cplint_r:mc_mh_sample_arg_bar_r(_,_,_,_,_,_),[]).

