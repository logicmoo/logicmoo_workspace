:- begin_tests(cr_charts).
:- use_module(prolog/cplint_r).

test(histogram_r) :-
    Data=[[1]-2,[3]-5,[8]-9],
    NBins is 2,
    histogram_r(Data,NBins).

test(density_r) :-
    Data=[[1]-2,[3]-5,[8]-9],
    NBins is 2,
    Min is 1.0,
    Max is 9.0,
    density_r(Data,NBins,Min,Max).

test(densities_r) :-
    Prior=[[1]-2,[3]-5,[8]-9],
    Post=[[5]-6,[7]-10,[12]-15],
    NBins is 2,
    densities_r(Prior,Post,NBins).

:- end_tests(cr_charts).
