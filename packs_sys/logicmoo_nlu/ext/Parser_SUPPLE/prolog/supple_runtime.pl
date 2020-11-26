:- assert('$package_name'('shef.nlp.supple.prolog.cafe')).


:- dynamic verbose/0, chart/3, grammar_file/1, filter_grammar/1,
	output_file/1, chart_file/1, markup_file/1, 
	best_parse_file/1, bracketed_parses/0.

:- op(700, xfx, \=).
X \= Y :- \+(X=Y).
not(Goal) :- \+(Goal).
 
:- op(900, fy, once).
once(Goal) :- call(Goal), !.
 
:- op(505,xfx,:).       % feature:value separator
:- op(10,xfy,^).        % pseudo-lambda operator for extraposing

% top-level call
parse :-
    prolog_flag(argv,Args,Args),
    parse(Args).
parse.

