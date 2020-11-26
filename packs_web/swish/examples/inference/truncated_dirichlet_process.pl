/*
Comparison of the Dirichlet distribution with a truncated 
Dirichlet process.
We compare the index values sampled from a Dirichlet process 
truncated at K with those sampled from a discrete distribution 
over 1..K sampled from a symmetric Dirichlet distribution.
The truncated Dirichlet process samples an integer between 
1 and K as follows:

pmass=0;
for(i=0;i<K-1;i++)
{
theta_0= sample from Beta(1,alpha);
theta[i]=theta_0*(1-pmass);
pmass=pmass+theta[i];
}
theta[K-1]=1-pmass;
sample index from Discrete([1,...,K],theta)

Or, equivalently

pmass=1;
for(i=0;i<K-1;i++)
{
theta_0= sample from Beta(1,alpha);
theta[i]=theta_0*pmass;
pmass=pmass*(1-theta_0);
}
theta[K-1]=pmass;

See https://en.wikipedia.org/wiki/Dirichlet_process#The_stick-breaking_process

sample index from Discrete([1,...,K],theta)

For alpha=1, Beta(1,alpha) is the uniform distribution in [0,1]

Sampling an integer from a discrete distribution over 1..K 
sampled from a symmetric Dirichlet distribution with parameter 
alpha instead is

sample theta from Dirichlet([alpha,...,alpha])
sample index from Discrete([1,...,K],theta)

Sampling from Dirichlet([alpha,...,alpha]) can be implemented as
pmass=0;
for(i=0;i<K-1;i++)
{
theta_0= sample from Beta(1,(K-i-1)*alpha);
theta[i]=theta_0*(1-pmass);
pmass=pmass+theta[i];
}
theta[K-1]=1-pmass;

See https://en.wikipedia.org/wiki/Dirichlet_distribution#Marginal_beta_distributions

This example compares the two sampling processes, showing 
that they produce different distribution, even for alpha=1 
(flat Dirichlet distribution).
*/
/** <examples>
?- truncated(10000,1,3,G).
shows the distribution of integers from the truncated 
Dirichlet process with K=3 and alpha=1. Takes 10000 samples

?- dir(10000,1,3,G).
shows the distribution of integers from the discrete distribution
sampled from a Dirichlet distribution with K=3 and alpha=1.
Takes 10000 samples

?- comparison(10000,1,3,G).
compares the samples from the two processes

?- dir(10000,10,3,G).
shows the distribution of integers from the discrete distribution
sampled from a Dirichlet distribution with K=3 and alpha=10.
The distribution is still uniform in 1..3 because the Dirichlet
distribution is symmetric.
*/
 :- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

dir_index(K,Alpha,I):-
  findall(Alpha,between(1,K,_),AlphaVec),
  theta(AlphaVec,Theta),
  index(Theta,K,I).

theta(AlphaVec,Theta):dirichlet(Theta,AlphaVec).

index(Theta,K,I):discrete(I,Dist):-
  numlist(1,K,Indexes),
  maplist(pair,Indexes,Theta,Dist).

pair(V,P,V:P).

% trunc_dp_stick_index(K,Alpha,I)
% returns in I the index of the NVth sample from the DP
trunc_dp_stick_index(K,Alpha,I):-
  tdp_stick_index(1,K,Alpha,I).
 
tdp_stick_index(K,K,_Alpha,K):-!.

tdp_stick_index(N,K,Alpha,V):-
  stick_proportion(N,Alpha,P),
  choose_prop(N,K,Alpha,P,V).
  
% choose_prop(N,NV,Alpha,P,V)
% returns in V the index of the end of the stick breaking process starting
% from index N for the NVth value to be sampled from the DP
choose_prop(N,_K,_Alpha,P,N):-
  pick_portion(N,P).

choose_prop(N,K,Alpha,P,V):-
  neg_pick_portion(N,P),
  N1 is N+1,
  tdp_stick_index(N1,K,Alpha,V).
 
% sample of the beta_i parameters
stick_proportion(_,Alpha,P):beta(P,1,Alpha).

% flip of the coin for the portion of the stick of size P
pick_portion(N,P):P;neg_pick_portion(N,P):1-P.

:- end_lpad.

truncated(Samples,Alpha,K,Chart):-
  mc_sample_arg(trunc_dp_stick_index(K,Alpha,V),Samples,V,L),
  histogram(L,Chart).

dir(Samples,Alpha,K,Chart):-
  mc_sample_arg(dir_index(K,Alpha,V),Samples,V,L),
  histogram(L,Chart).

comparison(Samples,Alpha,K,Chart):-
  mc_sample_arg(trunc_dp_stick_index(K,Alpha,V),Samples,V,LP),
  mc_sample_arg(dir_index(K,Alpha,V),Samples,V,LD),
  densities(LD,LP,Chart).


