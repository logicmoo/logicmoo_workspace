:- expects_dialect(lps).

%  Graphviz graph for https://swish.swi-prolog.org or other SWISH server
%  Documentation in https://swish.swi-prolog.org/example/render_graphviz.swinb

% :- use_module('../../swish/lib/render').
:- use_rendering(graphviz).

lf_graph(LogicalForm,dot(digraph(Edges))) :- % show only binary relations
    findall( edge(From->To,[label=Relation]), (
    	member(P,LogicalForm),P=..[Relation,From_,To_],
        % Nodes are WordIndex:Type; stringify them so Graphviz does NOT take them as node ports:
        term_string(From_,From), term_string(To_,To)
        ), Edges).


