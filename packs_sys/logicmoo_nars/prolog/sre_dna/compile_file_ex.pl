?- use_module(library(lists)).

guardedexpr_a(node(guardedexpr_a,[[a]],8), A, B) :-
        c(A, a, B).
guardedexpr_a(node(guardedexpr_a,[[a],A],9), B, C) :-
        c(B, a, D),
        expr(A, D, C).

char(A, B) :-
        integer(A),
        A<256, !,
        name(B, [A]).

c([A|B], A, B).

dctg_rule_info(guardedexpr_b, 11, guardedexpr_b(node(_,_,11),_,_), 3, nonterminal).
dctg_rule_info(guardedexpr_a, 9, guardedexpr_a(node(_,_,9),_,_), 3, nonterminal).
dctg_rule_info(noniter_expr, 5, noniter_expr(node(_,_,5),_,_), 3, nonterminal).
dctg_rule_info(expr, 0, expr(node(_,_,0),_,_), 3, nonterminal).
dctg_rule_info(expr, 1, expr(node(_,_,1),_,_), 2, nonterminal).
dctg_rule_info(noniter_expr, 4, noniter_expr(node(_,_,4),_,_), 2, nonterminal).
dctg_rule_info(iter_expr, 6, iter_expr(node(_,_,6),_,_), 2, nonterminal).
dctg_rule_info(iter_expr, 7, iter_expr(node(_,_,7),_,_), 2, nonterminal).
dctg_rule_info(probval, 13, probval(node(_,_,13),_,_), 1, terminal).
dctg_rule_info(intval, 12, intval(node(_,_,12),_,_), 1, terminal).
dctg_rule_info(guardedexpr_b, 10, guardedexpr_b(node(_,_,10),_,_), 1, terminal).
dctg_rule_info(guardedexpr_a, 8, guardedexpr_a(node(_,_,8),_,_), 1, terminal).
dctg_rule_info(noniter_expr, 3, noniter_expr(node(_,_,3),_,_), 1, terminal).
dctg_rule_info(noniter_expr, 2, noniter_expr(node(_,_,2),_,_), 1, terminal).

library_directory('c:/program files/sicstus prolog/library').

sre_pp_l([A]) :-
        sre_pp(A), !.
sre_pp_l([A|B]) :-
        write('['),
        sre_pp(A),
        write(+),
        sre_pp_l(B),
        write(']'), !.

sre2b(A) :-
        generate_tree(expr, grow, 8, _, B, _),
        B^^construct(C),
        B^^recognize(A,D,1.0,E),
        nl,
        sre_pp(C),
        nl,
        write('Prob = '),
        write(E),
        nl,
        write('Leftover = '),
        write(D),
        nl.

sre2c(A, B, C) :-
        repeat,
        (   A=full
        ;   A=grow
        ),
        generate_tree(expr, A, 12, _, D, _),
        D^^construct(B),
        nl,
        write(A),
        nl,
        sre_pp(B),
        nl,
        bagof(E, user:(D^^recognize(C,[],1.0,E)), F),
        write('Pr list: '),
        nl,
        writelist(F),
        nl.

sre2(A, B, C) :-
        repeat,
        (   A=full
        ;   A=grow
        ),
        generate_tree(expr, A, 12, _, D, _),
        D^^construct(B),
        nl,
        write(A),
        nl,
        sre_pp(B),
        nl,
        bagof((E,F), user:(D^^recognize(C,E,1.0,F)), G),
        write('Recog list: '),
        nl,
        writelist(G),
        nl.

sre_pp(A*B) :-
        write('('),
        sre_pp(A),
        write(')*'),
        write(B), !.
sre_pp(A+B) :-
        write('('),
        sre_pp(A),
        write(')+'),
        write(B), !.
sre_pp(A:B) :-
        sre_pp(A),
        write(:),
        sre_pp(B), !.
sre_pp([A|B]) :-
        sre_pp_l([A|B]), !.
sre_pp((A,B)) :-
        write('('),
        sre_pp(A),
        write(','),
        write(B),
        write(')'), !.
sre_pp(A) :-
        write(A).

select_kth_term([A], _, B, B, A) :- !.
select_kth_term([A|_], B, C, C, A) :-
        A>=B, !.
select_kth_term([_|A], B, C, D, E) :-
        F is C+1,
        select_kth_term(A, B, F, D, E).

sumlist([], [], A, A).
sumlist([A|B], [C|D], E, F) :-
        C is E+A,
        sumlist(B, D, C, F).

int_range(0, 1000).

is_a_probability(A) :-
        float(A), !.
is_a_probability(A) :-
        random:random(B),
        A is truncate(B*100)/100.

is_an_integer(A) :-
        integer(A), !.
is_an_integer(A) :-
        int_range(B, C),
        random:random(B, C, A).

recognize_loop(_, A, [], [], B, C) :- !,
        C is B*(1.0-A),
        check_prob(C).
recognize_loop(_, A, B, B, C, D) :-
        D is C*(1.0-A),
        check_prob(D).
recognize_loop(A, B, C, D, E, F) :-
        G is E*B,
        check_prob(G),
        A^^recognize(C,H,G,I),
        \+C=H,
        check_prob(I),
        recognize_loop(A, B, H, D, I, F).

raw_gen_loop(A, B, C, D, E, F) :-
        E<C,
        maybe(B),
        A^^raw_generate(G,E,H),
        raw_gen_loop(A, B, C, I, H, F),
        lists:append(G, I, D), !.
raw_gen_loop(_, _, _, [], A, A) :- !.

probval(node(probval,[[A]],13), B, C) :-
        c(B, A, C),
        is_a_probability(A).

raw_select_term(A, B) :-
        sumlist(A, C, 0, D),
        random:random(0, D, E),
        select_kth_term(C, E, 1, B, _), !.

guardedexpr_b(node(guardedexpr_b,[[b]],10), A, B) :-
        c(A, b, B).
guardedexpr_b(node(guardedexpr_b,[[b],A],11), B, C) :-
        c(B, b, D),
        expr(A, D, C).

intval(node(intval,[[A]],12), B, C) :-
        c(B, A, C),
        is_an_integer(A).

noniter_expr(node(noniter_expr,[[a]],2), A, B) :-
        c(A, a, B).
noniter_expr(node(noniter_expr,[[b]],3), A, B) :-
        c(A, b, B).
noniter_expr(node(noniter_expr,[A,B,C,D],4), E, F) :-
        guardedexpr_a(A, E, G),
        intval(B, G, H),
        guardedexpr_b(C, H, I),
        intval(D, I, F).
noniter_expr(node(noniter_expr,[A,B],5), C, D) :-
        expr(A, C, E),
        expr(B, E, D).

check_prob(A) :-
        min_grammar_prob_P(B),
        A>B, !.

iter_expr(node(iter_expr,[A,B],6), C, D) :-
        noniter_expr(A, C, E),
        probval(B, E, D).
iter_expr(node(iter_expr,[A,B],7), C, D) :-
        noniter_expr(A, C, E),
        probval(B, E, D).

identify_type([], [], []).
identify_type([A|B], [A|C], D) :-
        dctg_rule_info(_, A, _, _, terminal), !,
        identify_type(B, C, D).
identify_type([A|B], C, [A|D]) :-
        identify_type(B, C, D).

get_rule_stuff(A, B) :-
        clause(user:semantic_rule(B,_,C,_), _),
        C=..[A|_].

make_id_entries([]) :- !.
make_id_entries([(A,B)|C]) :-
        assert(user:dctg_id_table(A,B,_,_)),
        make_id_entries(C), !.

make_rule_id_list2(A, B) :-
        bagof(C, user:get_rule_stuff(A,C), D),
        rem_dups(D, B).

same_goal(A, B) :-
        A=..[C|_],
        B=..[C|_], !.

abstract_member2(A, [B|_]) :-
        same_goal(A, B).
abstract_member2(A, [_|B]) :-
        abstract_member2(A, B).

goal_type(A, B, _, C, D, E, C, D, [A|E]) :-
        (   B=(F,_) ->
            true
        ;   B=F
        ),
        (   abstract_member2(F, E)
        ;   same_goal(A, F)
        ), !.
goal_type(A, B, C, D, E, F, [A|D], E, F) :-
        (   B=(G,_) ->
            true
        ;   B=G
        ),
        (   abstract_member2(G, D)
        ;   abstract_member2(G, C)
        ), !.
goal_type(A, (_,B), C, D, E, F, G, H, I) :- !,
        goal_type(A, B, C, D, E, F, G, H, I).
goal_type(A, _, _, B, C, D, B, [A|C], D).

user_override(A, B, C, [A|B], C) :-
        A=..[D|_],
        dctg_override_P(E, _),
        lists:member(D, E), !.
user_override(A, B, C, B, [A|C]) :-
        A=..[D|_],
        dctg_override_P(_, E),
        lists:member(D, E), !.

grammar_type_loop([], A, B, C, A, B, C) :- !.
grammar_type_loop([A|B], C, D, E, F, G, H) :-
        user_override(A, D, E, I, J),
        grammar_type_loop(B, C, I, J, F, G, H).
grammar_type_loop([A|B], C, D, E, F, G, H) :-
        copy_term(A, I),
        clause(user:I, J),
        goal_type(A, J, B, C, D, E, K, L, M),
        grammar_type_loop(B, K, L, M, F, G, H).

find_minimum_depth(_, [], A, A).
find_minimum_depth(A, [(B,C)|D], E, F) :-
        B=..[A|_],
        G is min(C,E),
        find_minimum_depth(A, D, G, F), !.
find_minimum_depth(A, [_|B], C, D) :-
        find_minimum_depth(A, B, C, D), !.

abstract_member(A, [(B,_)|_]) :-
        B=..[A|_].
abstract_member(A, [_|B]) :-
        abstract_member(A, B).

find_min_depth(A, [(B,C)|_], C) :-
        A=..[B|_], !.
find_min_depth(A, [_|B], C) :-
        find_min_depth(A, B, C), !.

is_a_rule_call(A) :-
        A=..[B|_],
        dctg_id_table(B, _, _, _), !.

find_min_depth_body((A,B), C, D, E) :-
        is_a_rule_call(A), !,
        find_min_depth(A, C, F),
        G is max(F,D),
        find_min_depth_body(B, C, G, E).
find_min_depth_body((_,A), B, C, D) :- !,
        find_min_depth_body(A, B, C, D).
find_min_depth_body(A, B, C, D) :-
        is_a_rule_call(A), !,
        find_min_depth(A, B, E),
        D is max(E,C).
find_min_depth_body(_, _, A, A) :- !.

find_rule_mins([], A, A) :- !.
find_rule_mins([(A,B)|C], D, E) :-
        A=..[F|_],
        \+member((F,_),D), !,
        find_rule_mins(C, [(F,B)|D], E).
find_rule_mins([_|A], B, C) :-
        find_rule_mins(A, B, C).

process_rules([], A, _, B, A, B) :- !.
process_rules([A|B], C, D, E, F, G) :-
        copy_term(A, H),
        clause(user:H, I),
        find_min_depth_body(I, D, 0, J), !,
        K is J+1,
        process_rules(B, [(A,K)|C], D, E, F, G).
process_rules([A|B], C, D, E, F, G) :- !,
        process_rules(B, C, D, [A|E], F, G).

set_rule_data([], _) :- !.
set_rule_data([(A,B)|C], D) :-
        A=..[E|F],
        lists:append(_, [node(_,_,G),_,_], F),
        (   lists:member(A, D) ->
            H=terminal
        ;   H=nonterminal
        ),
        assert(user:dctg_rule_info(E,G,A,B,H)),
        set_rule_data(C, D), !.

grammar_type_top_loop(A, B, C, D) :-
        grammar_type_loop(A, [], B, C, E, F, G),
        (   length(A, H),
            length(E, H) ->
            F=D
        ;   grammar_type_top_loop(E, F, G, D)
        ), !.

grammar_depth_top_loop([], A, _, A) :- !.
grammar_depth_top_loop(A, B, C, D) :-
        process_rules(A, B, C, [], E, F),
        find_rule_mins(E, C, G),
        (   length(A, H),
            length(F, H) ->
            write('Problem - '),
            write(H),
            write(' rules cannot terminate:'),
            nl,
            writelist(F),
            nl,
            write('these terminated - '),
            nl,
            writelist(E),
            nl,
            write('These are mincalls - '),
            nl,
            writelist(G),
            nl,
            fail
        ;   grammar_depth_top_loop(F, E, G, D)
        ), !.

clone_list([], []) :- !.
clone_list([_|A], [_|B]) :-
        clone_list(A, B), !.

get_rule_name(A) :-
        clause(user:semantic_rule(B,_,C,_), _),
        C=..[D|E],
        clone_list(E, F),
        lists:append(F, [node(_,_,B),_,_], G),
        A=..[D|G].

dctg_id_table(expr, [0,1], [], [0,1]).
dctg_id_table(guardedexpr_a, [8,9], [8], [9]).
dctg_id_table(guardedexpr_b, [10,11], [10], [11]).
dctg_id_table(intval, [12], [12], []).
dctg_id_table(iter_expr, [6,7], [], [6,7]).
dctg_id_table(noniter_expr, [2,3,4,5], [2,3], [4,5]).
dctg_id_table(probval, [13], [13], []).

enhance_rule_id_list :-
        retract(user:dctg_id_table(A,B,_,_)),
        identify_type(B, C, D),
        assert(user:dctg_id_table(A,B,C,D)),
        fail.
enhance_rule_id_list.

generate_rule_data :-
        findall(A, user:get_rule_name(A), B),
        rem_dups(B, C),
        grammar_depth_top_loop(C, [], [], D),
        grammar_type_top_loop(C, [], [], E),
        set_rule_data(D, E), !.

make_rule_id_list :-
        findall((A,B), user:make_rule_id_list2(A,B), C),
        make_id_entries(C), !.

cleanup_grammar_data :-
        retractall(user:dctg_rule_info(_,_,_,_)),
        retractall(user:dctg_id_table(_,_,_,_)), !.

make_grammar_table :-
        cleanup_grammar_data,
        make_rule_id_list,
        generate_rule_data,
        enhance_rule_id_list, !.

file_search_path(library, A) :-
        library_directory(A).
file_search_path(system, A) :-
        prolog_flag(host_type, A).

eval_with_ID_P(no).

negsetsize_P(30).

elite_migrate_P(0, no).

unique_guards_P(no).

min_skip_prob_P(1.0e-004).

min_grammar_prob_P(1.0e-004).

gen_set_size_P(1000).

sre_mintestcnt_P(2).

mutation_range_P(0.1).

dctg_override_P([], []).

expr(node(expr,[A],0), B, C) :-
        iter_expr(A, B, C).
expr(node(expr,[A],1), B, C) :-
        noniter_expr(A, B, C).

dctg_root_P(expr).

user_args_P([]).

reprod_verif_P(no).

evaluator_reset_P(generate_testset, 100).

gen_type_P(steadystate).

popn_dump_P(no).

max_string_length_P(20).

rep_limit_P(2).

trace_limit_P(0, 0).

unique_population_P(yes).

lamarckian_P(0.0, 10, best, 0.1).

tournament_size_P(4, 4).

error_tolerance_P(0).

max_depth_P(10, 17).

prob_terminal_mutation_P(0.75).

prob_internal_crossover_P(0.9).

reprod_P(3).

prob_crossover_P(0.9).

prob_grow_P(0.5).

max_runs_P(1, solution, 3).

cull_method_P(elite).

population_size_P(75, 50).

dctg_file_P('sre3.pl').

fitness_func_P('reg_gram_1').

wd_P('c:/research/sre_dna_fastX').

seed_P(random, (_,_,_)).

rule_number(14).

semantic_rule(0, construct(A), expr, [B]) :- !,
        B^^construct(A).
semantic_rule(0, raw_generate(A,B,C), expr, [D]) :- !,
        D^^raw_generate(A,B,C).
semantic_rule(0, recognize(A,B,C,D), expr, [E]) :- !,
        check_prob(C),
        E^^recognize(A,B,C,D).
semantic_rule(1, construct(A), expr, [B]) :- !,
        B^^construct(A).
semantic_rule(1, raw_generate(A,B,C), expr, [D]) :- !,
        D^^raw_generate(A,B,C).
semantic_rule(1, recognize(A,B,C,D), expr, [E]) :- !,
        check_prob(C),
        E^^recognize(A,B,C,D).
semantic_rule(2, construct(a), noniter_expr, [[a]]) :- !,
        true.
semantic_rule(2, raw_generate([a],A,B), noniter_expr, [[a]]) :- !,
        B is A+1.
semantic_rule(2, recognize([a|A],A,B,B), noniter_expr, [[a]]) :- !,
        check_prob(B).
semantic_rule(3, construct(b), noniter_expr, [[b]]) :- !,
        true.
semantic_rule(3, raw_generate([b],A,B), noniter_expr, [[b]]) :- !,
        B is A+1.
semantic_rule(3, recognize([b|A],A,B,B), noniter_expr, [[b]]) :- !,
        check_prob(B).
semantic_rule(4, construct([(A,B),(C,D)]), noniter_expr, [E,F,G,H]) :- !,
        E^^construct(A),
        F^^construct(B),
        G^^construct(C),
        H^^construct(D).
semantic_rule(4, raw_generate(A,B,C), noniter_expr, [D,E,F,G]) :- !,
        E^^construct(H),
        G^^construct(I),
        (   raw_select_term([H,I], 1) ->
            D^^raw_generate(A,B,C)
        ;   F^^raw_generate(A,B,C)
        ).
semantic_rule(4, recognize(A,B,C,D), noniter_expr, [E,F,_,G]) :- !,
        F^^construct(H),
        G^^construct(I),
        J is C*H/(H+I),
        check_prob(J),
        E^^recognize(A,B,J,D).
semantic_rule(4, recognize(A,B,C,D), noniter_expr, [_,E,F,G]) :- !,
        E^^construct(H),
        G^^construct(I),
        J is C*I/(H+I),
        check_prob(J),
        F^^recognize(A,B,J,D).
semantic_rule(5, construct(A:B), noniter_expr, [C,D]) :- !,
        C^^construct(A),
        D^^construct(B).
semantic_rule(5, raw_generate(A,B,C), noniter_expr, [D,E]) :- !,
        D^^raw_generate(F,B,G),
        E^^raw_generate(H,G,C),
        lists:append(F, H, A).
semantic_rule(5, recognize(A,B,C,D), noniter_expr, [E,F]) :- !,
        check_prob(C),
        E^^recognize(A,G,C,H),
        check_prob(H),
        F^^recognize(G,B,H,D).
semantic_rule(6, construct(A*B), iter_expr, [C,D]) :- !,
        C^^construct(A),
        D^^construct(B).
semantic_rule(6, raw_generate(A,B,C), iter_expr, [D,E]) :- !,
        E^^construct(F),
        max_string_length_P(G),
        raw_gen_loop(D, F, G, A, B, C).
semantic_rule(6, recognize(A,B,C,D), iter_expr, [E,F]) :- !,
        check_prob(C),
        F^^construct(G),
        recognize_loop(E, G, A, B, C, D).
semantic_rule(7, construct(A+B), iter_expr, [C,D]) :- !,
        C^^construct(A),
        D^^construct(B).
semantic_rule(7, raw_generate(A,B,C), iter_expr, [D,E]) :- !,
        D^^raw_generate(F,B,G),
        E^^construct(H),
        max_string_length_P(I),
        raw_gen_loop(D, H, I, J, G, C),
        lists:append(F, J, A), !.
semantic_rule(7, recognize(A,B,C,D), iter_expr, [E,F]) :- !,
        check_prob(C),
        E^^recognize(A,G,C,H),
        \+A=G,
        check_prob(H),
        F^^construct(I),
        recognize_loop(E, I, G, B, H, D).
semantic_rule(8, construct(a), guardedexpr_a, [[a]]) :- !,
        true.
semantic_rule(8, raw_generate([a],A,B), guardedexpr_a, [[a]]) :- !,
        B is A+1.
semantic_rule(8, recognize([a|A],A,B,B), guardedexpr_a, [[a]]) :- !,
        check_prob(B).
semantic_rule(9, construct(a:A), guardedexpr_a, [[a],B]) :- !,
        B^^construct(A).
semantic_rule(9, raw_generate([a|A],B,C), guardedexpr_a, [[a],D]) :- !,
        D^^raw_generate(A,B,E),
        C is E+1.
semantic_rule(9, recognize([a|A],B,C,D), guardedexpr_a, [[a],E]) :- !,
        check_prob(C),
        E^^recognize(A,B,C,D).
semantic_rule(10, construct(b), guardedexpr_b, [[b]]) :- !,
        true.
semantic_rule(10, raw_generate([b],A,B), guardedexpr_b, [[b]]) :- !,
        B is A+1.
semantic_rule(10, recognize([b|A],A,B,B), guardedexpr_b, [[b]]) :- !,
        check_prob(B).
semantic_rule(11, construct(b:A), guardedexpr_b, [[b],B]) :- !,
        B^^construct(A).
semantic_rule(11, raw_generate([b|A],B,C), guardedexpr_b, [[b],D]) :- !,
        D^^raw_generate(A,B,E),
        C is E+1.
semantic_rule(11, recognize([b|A],B,C,D), guardedexpr_b, [[b],E]) :- !,
        check_prob(C),
        E^^recognize(A,B,C,D).
semantic_rule(12, construct(A), intval, [[A]]) :- !,
        true.
semantic_rule(13, construct(A), probval, [[A]]) :- !,
        true.

process(A) :-
        (   A=(B<:>C)
        ;   A=(B::=C)
        ), !,
        translate_rule(A, D),
        assertz(user:D), !.
process((:-A)) :- !,
        call(user:A).
process((A:-B)) :- !,
        assertz(user:(A:-B)).
process(A) :-
        assertz(user:A).

check_it(A) :-
        A=end_of_file, !.
check_it(A) :-
        process(A),
        fail.

consume :-
        repeat,
        read(A),
        check_it(A).

grammar(A) :-
        seeing(B),
        see(A),
        consume,
        seen,
        see(B).

add_extra_args(A, B, C) :-
        B=..D,
        lists:append(D, A, E),
        C=..E.

assert_semantic_rule(A, B, C, (D,E)) :- !,
        (   D=(F::-G)
        ;   F=D,
            G=true
        ),
        assert(user:(semantic_rule(A,F,B,C):-!,G)),
        assert_semantic_rule(A, B, C, E).
assert_semantic_rule(A, B, C, D) :-
        (   D=(E::-F)
        ;   E=D,
            F=true
        ),
        assert(user:(semantic_rule(A,E,B,C):-!,F)).

prod_number(A) :-
        retract(user:rule_number(A)),
        B is A+1,
        assert(user:rule_number(B)).

tidy(((A,B),C), D) :-
        tidy((A,B,C), D).
tidy((A,B), (C,D)) :- !,
        tidy(A, C),
        tidy(B, D).
tidy(A, A) :- !.

t_rp(!, A, A, B, B, !) :- !.
t_rp([], A, [[]|A], B, C, B=C) :- !.
t_rp([A], B, [[C]|B], D, E, c(D,A,E)) :-
        char(A, C).
t_rp([A], B, [[A]|B], C, D, c(C,A,D)) :- !.
t_rp([A|B], C, [[D|E]|C], F, G, (c(F,A,H),I)) :-
        char(A, D),
        t_rp(B, C, [E|C], H, G, I).
t_rp([A|B], C, [[A|B]|C], D, E, (c(D,A,F),G)) :- !,
        t_rp(B, C, [B|C], F, E, G).
t_rp({A}, B, B, C, C, A) :- !.
t_rp((A,B), C, D, E, F, (G,H)) :- !,
        t_rp(A, C, I, E, J, G),
        t_rp(B, I, D, J, F, H).
t_rp(A^^B, C, [B|C], D, E, F) :-
        add_extra_args([B,D,E], A, F).
t_rp(A, B, [C|B], D, E, F) :-
        add_extra_args([C,D,E], A, F).

t_lp((A,B), C, D, E, F, G) :-
        lists:append(B, E, H),
        prod_number(I),
        assert_semantic_rule(I, A, C, F),
        add_extra_args([node(A,C,I),D,H], A, G).
t_lp(A, B, C, D, E, F) :-
        prod_number(G),
        assert_semantic_rule(G, A, B, E),
        add_extra_args([node(A,B,G),C,D], A, F).

translate_rule((A::=[]<:>B), C) :- !,
        t_lp(A, [], D, D, B, C).
translate_rule((A::=[]), B) :- !,
        t_lp(A, [], C, C, [], B).
translate_rule((A::=B<:>C), (D:-E)) :- !,
        t_rp(B, [], F, G, H, I),
        lists:reverse(F, J),
        t_lp(A, J, G, H, C, D),
        tidy(I, E).
translate_rule((A::=B), (C:-D)) :-
        translate_rule((A::=B<:>[]), (C:-D)).

node(A,B,C)^^D :-
        semantic_rule(C, D, A, B).

sre(A, B, C, D) :-
        repeat,
        (   A=full
        ;   A=grow
        ),
        generate_tree(expr, A, 12, _, E, _),
        E^^construct(B),
        E^^raw_generate(C,0,D),
        nl,
        sre_pp(B),
        nl,
        write('tree '),
        write(E),
        nl,
        tree_depth(E, F),
        write('Depth = '),
        write(F),
        nl.

