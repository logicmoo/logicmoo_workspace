:- module(static_strip_module, [static_strip_module/4]).

%% static_strip_module(+Call, -Head, -Module, +ContextModule) is det.
%
% Like strip_module/4, but assume as Module the ContextModule if Call is
% uninstantiated
%
static_strip_module(T, T, M, M) :-
    var(T), !.
static_strip_module(Module:RT, T, M, _) :- !,
    static_strip_module(RT, T, M, Module).
static_strip_module(T, T, M, M).
