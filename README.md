SLDNF Draw is a Prolog program that draws SLDNF Trees in LaTeX
==============================================================



See:

Marco Gavanelli SLDNF-Draw: Visualization of Prolog operational semantics in LaTeX, Intelligenza Artificiale, vol. 11, no. 1, pp. 81-92, 2017. DOI: 10.3233/IA-170108.
http://content.iospress.com/articles/intelligenza-artificiale/ia108

Post-print available at: http://endif.unife.it/it/ricerca-1/aree-di-ricerca/informazione/ingegneria-informatica/software/sldnf-draw/sldnf-draw_paper_ia.pdf

Installation
------------
This is a SWI-Prolog (http://www.swi-prolog.org/) pack.

It can be installed with `pack_install/1`

    $ swipl
    ?- pack_install(sldnfdraw).

Example of use
---------------

    $ cd <pack>/sldnfdraw/prolog/examples
    $ swipl
    ?- [member].
    ?- draw_goal(T).

Usage
-----

Prepare an input file that first loads the module `sldnfdraw`

    :- use_module(library(sldnfdraw)).


Then you need to initialize the library with

    :- sldnf.

You can now write the program you want to query by including it within
the directives `:- begin_program.` and `:- end_program.` as in
```
:- begin_program.

member(X ,[X|_T]).
member(X ,[_H|T]):-
  member(X,T).

:-end_program.
```
You can now write the query by including it within
the directives `:- begin_query.` and `:- end_query.` as in
```
:-begin_query.

member(X,[1,2]), \+ member(X,[1,3]).

:-end_query.
```
You can now build the SLDNF tree with the predicate `draw_goal/1`.
If you call it with a variable as in
```
?- draw_goal(T).
```
it will return in `T` a string with the Latex code for drawing the tree, that you
can then include in a LaTeX document.

If you call `draw_goal/1` with a string as in
```
?- draw_goal("tree.tex").
```
it will write the LaTeX code in file `tree.tex` in the current folder.
You can then include it in a LaTeX document. The minimal LaTeX file you could use
is
```
\documentclass{article}
\usepackage{epic}
\usepackage{ecltree}
\begin{document}
\input{tree}
\end{document}
```

If you are using your example in SWISH and want the output to be shown
in SVG add the following code after laoding the library

    :- if(current_predicate(use_rendering/1)).
    :- use_rendering(sldnf).
    :- endif.

and you need to have the following programs on the server
 - LaTeX
 - pdfcrop
 - pdf2svg

In Ubuntu you would need the packages
```
texlive
texlive-extra-utils  
pdf2svg
texlive-humanities
texlive-pictures
```

Full Manual
-----------
http://endif.unife.it/it/ricerca-1/aree-di-ricerca/informazione/ingegneria-informatica/software/sldnf-draw/sldnf-draw
