/** <module> cplint_r

This module contains predicates for drawing graphs with R in cplint on SWISH.

@author Franco Masotti, Fabrizio Riguzzi
@license Artistic License 2.0 https://opensource.org/licenses/Artistic-2.0
@copyright Franco Masotti, Fabrizio Riguzzi
*/


:- module(cplint_r,
          [ build_xy_list/3,
            r_row/3,
            get_set_from_xy_list/2,
            bar_r/1,
            bar_r/2,
            argbar_r/1,
            histogram_r/2,
            density_r/1,
            densities_r/2,
            compute_areas_diagrams_r/3,
            test_r/5
          ]).


/* Dependencies */

:- use_module(library(r/r_call)).
:- use_module(library(r/r_data)).
:- use_module(library(lists)).
:- use_module(library(pita)).
:- use_module(library(mcintyre)).
:- use_module(library(auc)).
:- use_module(library(slipcover)).
:- use_module(library(cplint_util)).
/* Optional module */
:- use_module(swish(lib/r_swish)).

/* Meta predicate definitions. */

:-meta_predicate test_r(:,+,-,-,-).

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
    current_module(auc),
    current_module(slipcover).

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
 * build_xy_list(+X:list,+Y:list,-Out:list) is det
 *
 * Given to lists X and Y build an output list Out
 * in the form [X1-Y1,X2-Y2,...,XN-YN].
 */
build_xy_list([], [], []).

build_xy_list([XH|XT], [YH|YT], [XH-YH|Out]) :-
        build_xy_list(XT, YT, Out).

/**
 * r_row(+X:atom,+Y:atom,-Out:atom) is det
 *
 * Given two atoms X and Y, build the term r(X,Y) in Out.
 */
r_row(X,Y,r(X,Y)).

/**
 * get_set_from_xy_list(+L:list,-R:list) is det
 *
 * Given an input list L in the form [X1-Y1,X2-Y2,...,XN-YN], transform it in 
 * an output list R in the form [r(X1,Y1),r(X2,Y2),...,r(XN,YN)]. This means 
 * that R will contain an X-Y relationship which can be then passed to an R 
 * data frame. 
 */
get_set_from_xy_list(L,R) :-
    maplist(key,L,X),
    maplist(value,L,Y),
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
    + coord_flip(
        ylim=c(0,1)
    )
    + theme(
        aspect.ratio=1/2
    ).

/**
 * bar_r(+Probability:float) is nondet
 *
 * The predicate plots the probability 
 * as a bar chart with a bar for Probability and
 * a bar for 1-Probability.
 */
bar_r(PT) :-
    load_r_libraries,
    PF is 1.0-PT,
    geom_prob_bar(PT,PF),
    finalize_r_graph.


/**
 * bar_r(+Successes:int,+Failures:int) is nondet
 *
 * The predicate plots a  chart with a bar for the number
 * of Successes and
 * a bar for the number of Failures.
 */
bar_r(S,F) :-
    load_r_libraries,
    geom_mc_sample_bar(S,F),
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


/* Differences from the previous predicates:
 * =========================================
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
    scalingthresholD <- 20,
    {|r||xbreakS <- if(max(df$probabilities) > scalingthresholD) element_blank() else scale_y_continuous(breaks=seq(0,max(df$probabilities),1))|},
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
    ) + xbreakS + coord_flip(
        ylim=c(0,max(df$probabilities))
    ) + theme(
        axis.title.y=element_blank()
    ).

/**
 * argbar_r(+Values:list) is det
 *
 * Values is a list of couples V-N where
 * V is the value and N is the number of samples
 * returning that value.
 * The predicate plots a bar chart
 * with a bar for each possible value V.
 * The size of the bar is given by N.
 */
argbar_r(ValList0):-
    load_r_libraries,
    maplist(to_atom,ValList0,ValList),
    geom_mc_sample_arg_bar(ValList),
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
    ) + theme(
        axis.title.x=element_blank()
    ).

/**
 * histogram_r(+List:list,+Options:list) is det
 *
 * Draws a histogram of the samples in List.
 * List must be a list of couples of the form [V]-W or V-W
 * where V is a sampled value and W is its weight.
 * Options is a list of options, the following are recognised by histogram/3:
 * * min(+Min:float)
 *   the minimum value of domain, default value the minimum in List
 * * max(+Max:float)
 *   the maximum value of domain, default value the maximum in List
 * * nbins(+NBins:int)
 *   the number of bins for dividing the domain, default value 40
*/
histogram_r(L0,Options):-
    load_r_libraries,
    maplist(to_pair,L0,L1),
    maplist(key,L1,L2),
    max_list(L2,DMax),
    min_list(L2,DMin),
    option(max(Max),Options,DMax),
    option(min(Min),Options,DMin),
    option(nbins(NBins),Options,40),
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
    ) + theme(
        legend.title = element_blank(),
        axis.title.x=element_blank()
    ).

/**
 * density_r(+List:list) is det
 *
 * Display a smooth density estimate of a sets of samples.
 * The samples are in List
 * as couples V-W where V is a value and W its weigth.
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
    ) + theme(
        legend.title = element_blank(),
        axis.title.x=element_blank()
    ).

/**
 * densities_r(+PriorList:list,+PostList:list) is det
 *
 * Display a smooth density estimate of two sets of samples, usually
 * prior and post observations. The samples from the prior are in PriorList
 * while the samples from the posterior are in PostList
 * as couples V-W where V is a value and W its weigth.
 */
densities_r(Pri0,Post0) :-
    load_r_libraries,
    maplist(to_pair,Pri0,Pri1),
    maplist(to_pair,Post0,Post1),
    geom_densities(Pri1,Post1),
    finalize_r_graph.

/* auc */

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
        legend.title = element_blank(),
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

/**
 * test_r(+P:probabilistic_program,+TestFolds:list_of_atoms,-LL:float,-AUCROC:float,-AUCPR:float) is det
 *
 * The predicate takes as input in P a probabilistic program,
 * tests P on the folds indicated in TestFolds and returns the
 * log likelihood of the test examples in LL, the area under the Receiver
 * Operating Characteristic curve in AUCROC, the area under the Precision Recall
 * curve in AUCPR and draws R diagrams of the curves.
 */
test_r(P,TestFolds,LL,AUCROC,AUCPR):-
  test_prob(P,TestFolds,_NPos,_NNeg,LL,LG),
  compute_areas_diagrams_r(LG,AUCROC,AUCPR).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(cplint_r:build_xy_list(_,_,_)).
sandbox:safe_primitive(cplint_r:r_row(_,_,_)).
sandbox:safe_primitive(cplint_r:get_set_from_xy_list(_,_)).
sandbox:safe_primitive(cplint_r:histogram_r(_,_)).
sandbox:safe_primitive(cplint_r:density_r(_)).
sandbox:safe_primitive(cplint_r:densities_r(_,_)).
sandbox:safe_primitive(cplint_r:compute_areas_diagrams_r(_,_,_)).
sandbox:safe_primitive(cplint_r:bar_r(_),[]).
sandbox:safe_primitive(cplint_r:argbar_r(_),[]).

:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(cplint_r:test_r(_,_,_,_,_), []).

