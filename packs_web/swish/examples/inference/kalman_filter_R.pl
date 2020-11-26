/*
One-dimensional  Kalman filter. Hidden Markov model with a real
value as state and a real value as output. The next state is given by
the current state plus Gaussian noise (mean 0 and variance 2 in this example)
and the output is given by the current state plus Gaussian noise (mean
0 and variance 1 in this example). 
This example can be considered as modeling a random walk of a single continuous 
state variable with noisy observations. 
Given that at time 0 the value 2.5 was
observed, what is the distribution of the state at time 1 (filtering problem)?
The distribution of the state is plotted in the case of having (posterior) or 
not having the observation (prior).
Likelihood weighting is used to condition the distribution on evidence on
a continuous random variable (evidence with probability 0).
CLP(R) constraints allow both sampling and weighing samples with the same
program.
Filtering can be used to estimate the sequence of state variables
given a sequence of observations. Either likelihood weighting or particle 
filtering can be used for this purpose.
From
Islam, Muhammad Asiful, C. R. Ramakrishnan, and I. V. Ramakrishnan. 
"Inference in probabilistic logic programs with continuous random variables." 
Theory and Practice of Logic Programming 12.4-5 (2012): 505-523.
http://arxiv.org/pdf/1112.2681v3.pdf
Russell, S. and Norvig, P. 2010. Artificial Intelligence: A Modern Approach. 
Third Edition, Prentice Hall, Figure 15.10 page 587

*/

/** <examples>
?- filter_sampled_par(100).
?- filter_par(100).
?- filter_sampled(1000).
?- filter(1000).
?- dens_par(1000).
?- dens_lw(1000).
% plot the density of the state at time 1 in case of no observation (prior)
% and in case of observing 2.5 by taking 1000 samples.
?- hist(1000,40).
% plot the density of the state at time 1 in case of no observation
% by taking 1000 samples and dividing the domain
% in 40 bins

*/
:- use_module(library(mcintyre)).
:- use_module(library(cplint_r)).
:- mc.
:- begin_lpad.

kf_fin(N, T) :-
  kf_fin(N,_O,T).

kf_fin(N,O, T) :-
  init(S),
  kf_part(0, N, S,O,_LS,T).


kf(N,O,LS) :-
  init(S),
  kf_part(0, N, S,O,LS,_T).

kf_o(N,ON):-
  init(S),
  N1 is N-1,
  kf_part(0,N1,S,_O,_LS,T),
  emit(T,N,ON).

kf_part(I, N, S, [V|RO], [S|LS], T) :-
  I < N, 
  NextI is I+1,
  trans(S,I,NextS),
  emit(NextS,I,V),
  kf_part(NextI, N, NextS, RO, LS, T).

kf_part(N, N, S, [], [], S).

trans(S,I,NextS) :-
  {NextS =:= E + S},
  trans_err(I,E).

emit(NextS,I,V) :-
  {V =:= NextS +X},
  obs_err(I,X).

init(S):gaussian(S,0,1).
% prior as in Russel and Norvig 2010, Fig 15.10
trans_err(_,E):gaussian(E,0,2).
% transition noise as in Russel and Norvig 2010, Fig 15.10
obs_err(_,E):gaussian(E,0,1).
% observation noise as in Russel and Norvig 2010, Fig 15.10

:- end_lpad.

%! hist(+S:int,+Bins:int,-C:dict) is det
% Plots a histogram of the density of the state at time 1 in case of 
% no observation
hist(Samples,NBins):-
  mc_sample_arg(kf_fin(1,_O1,Y),Samples,Y,L0),
  histogram_r(L0,[nbins(NBins)]).

%! dens_lw(+S:int) is det
% Plots the density of the state at time 1 in case of no observation (prior)
% and in case of observing 2.5.
% Observation as in Russel and Norvig 2010, Fig 15.10
dens_lw(Samples):-
  mc_sample_arg(kf_fin(1,_O1,Y),Samples,Y,L0),
  mc_lw_sample_arg(kf_fin(1,_O2,T),kf_fin(1,[2.5],_T),Samples,T,L),
  densities_r(L0,L).

dens_par(Samples):-
  mc_sample_arg(kf_fin(1,_O1,Y),Samples,Y,L0),
  mc_particle_sample_arg(kf_fin(1,_O2,T),[kf_fin(1,[2.5],_T)],Samples,T,L),
  densities_r(L0,L).


%! filter_par(+S:int) is det
% Draws a sample trajectory for 4 time points and performs particle filtering
filter_par(Samples):-
  sample_trajectory(4,O,St),
  filter_par(Samples,O,St).

%! filter_sampled_par(+S:int) is det
% Considers a sampled trajectory for 4 time points and performs particle filtering
filter_sampled_par(Samples):-
  o(O),
  st(St),
  filter_par(Samples,O,St).

%! filter(+S:int) is det
% Draws a sample trajectory for 4 time points and performs filtering with
% likelihood weighting
filter(Samples):-
  sample_trajectory(4,O,St),
  filter(Samples,O,St).

%! filter_sampled(+S:int) is det
% Considers a sampled trajectory for 4 time points and performs filtering
% with likelihood weighting
filter_sampled(Samples):-
  o(O),
  st(St),
  filter(Samples,O,St).

o([-0.13382010096024688, -1.1832019975321675, -3.2127809027386567, -4.586259511038596]).
st([-0.18721387460211258, -2.187978176930458, -1.5472275345566668, -2.9840114021132713]).

geom_densities(L1,L2,L3,L4,O,St,Y):-
    <- library("ggplot2"),
    <- library("gridExtra"),
    <- library("grid"),

    build_xy_list(O,Y,Obs),
    build_xy_list(St,Y,State),

    get_set_from_xy_list(L1,R1),
    get_set_from_xy_list(L2,R2),
    get_set_from_xy_list(L3,R3),
    get_set_from_xy_list(L4,R4),
    get_set_from_xy_list(Obs,R5),
    get_set_from_xy_list(State,R6),
    
    r_data_frame_from_rows(df1, R1),
    r_data_frame_from_rows(df2, R2),
    r_data_frame_from_rows(df3, R3),
    r_data_frame_from_rows(df4, R4),
    r_data_frame_from_rows(df5, R5),
    r_data_frame_from_rows(df6, R6),

    colnames(df1) <- c("x1", "y1"),
    colnames(df2) <- c("x2", "y2"),
    colnames(df3) <- c("x3", "y3"),
    colnames(df4) <- c("x4", "y4"),
    colnames(df5) <- c("x5", "y5"),
    colnames(df6) <- c("x6", "y6"),
    df <- data.frame(
        x1=df1$x1,
        x2=df2$x2,
        x3=df3$x3,
        x4=df4$x4,
        y1=df1$y1,
        y2=df2$y2,
        y3=df3$y3,
        y4=df4$y4
    ),
    dfp <- data.frame(
        x5=df5$x5,
        x6=df6$x6,
        y5=df5$y5,
        y6=df6$y6
    ),
    alphA <- 0.5,
    xmiN <- -8,
    xmaX <- 8,

    g.top <- ggplot(
        data=dfp
    ) + geom_point(
        aes(
            x=x5,
            y=y5,
            color="Obs"
        )
    ) + geom_point(
        aes(
            x=x6,
            y=y6,
            color="True State"
        )
    ) + scale_x_continuous(
        limits=c(
            xmiN,
            xmaX
        ),
        breaks=seq(
            xmiN,
            xmaX,
            2
        )
    ) + theme(
        legend.position = "top",
        legend.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(
            margin = margin(
                r=8
            )
        )
    ) + ylab("time"),

    g.bottom <- ggplot(
        data=df
    ) + geom_density(
        aes(
            x=x1,
            fill="S1",
            weights=y1
        ),
        alpha=alphA
    ) + geom_density(
        aes(
            x=x2,
            fill="S2",
            weights=y2
        ),
        alpha=alphA
    ) + geom_density(
        aes(
            x=x3,
            fill="S3",
            weights=y3
        ),
        alpha=alphA
    ) + geom_density(
        aes(
            x=x4,
            fill="S4",
            weights=y4
        ),
        alpha=alphA
    ) + scale_x_continuous(
        limits=c(
            xmiN,
            xmaX
        ),
        breaks=seq(
            xmiN,
            xmaX,
            2
        )
    ) + theme(
        legend.position="bottom",
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y = element_text(
            margin = margin(b=10)
        )
    ),
    <- grid.arrange(g.top,g.bottom, heights = c(1/5, 4/5)),
    r_download.


%! filter(+S:int,+O:list,+St:list) is det
% Takes observations O and true states St for 4 time points 
% and performs filtering on the trajectory: given O, computes the
% distribution of the state for each time point by taking S samples
% using likelihood weighting.
% Returns a graph C with the distributions of the state variable 
% at time 1, 2, 3 and 4 
% (S1, S2, S3, S4, density on the left y axis) 
% and with O and St (time on the right y axis).
filter(Samples,O,St):-
    mc_lw_sample_arg(kf(4,_O,T),kf_fin(4,O,_T),Samples,T,L),
    maplist(separate,L,T1,T2,T3,T4),
    Y=[1,2,3,4],
    geom_densities(T1,T2,T3,T4,O,St,Y).

%! filter_par(+S:int,+O:list,+St:list) is det
% Takes observations O and true states St for 4 time points 
% and performs filtering on the trajectory: given O, computes the
% distribution of the state for each time point.
% It uses particle filtering with S particles.
% Returns a graph C with the distributions of the state variable 
% at time 1, 2, 3 and 4 
% (S1, S2, S3, S4, density on the left y axis) 
% and with O and St (time on the right y axis).
filter_par(Samples,O,St):-
  O=[O1,O2,O3,O4],
  mc_particle_sample_arg([kf_fin(1,T1),kf_fin(2,T2),kf_fin(3,T3),kf_fin(4,T4)],
  [kf_o(1,O1),kf_o(2,O2),kf_o(3,O3),kf_o(4,O4)],Samples,[T1,T2,T3,T4],[F1,F2,F3,F4]),
  Y=[1,2,3,4],
  geom_densities(F1,F2,F3,F4,O,St,Y).

separate([S1,S2,S3,S4]-W,S1-W,S2-W,S3-W,S4-W).

sample_trajectory(N,Ob,St):-
  mc_sample_arg(kf(N,O,T),1,(O,T),L),
  L=[[(Ob,St)]-_].

