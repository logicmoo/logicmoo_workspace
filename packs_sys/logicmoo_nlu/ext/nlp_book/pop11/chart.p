;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; chart.p [Chapter  6] Simple chart parser

;;; This file contains code for both top-down and bottom-up
;;; but the former is commented out

vars agenda tree agenda_add inactive_edge_procedure active_edge_procedure;

;;; add an edge to the chart, recording any new edges that may need
;;; to be added as a consequence

define add_edge(edge);
   vars substart subfinish subgoal subfound
        start finish label found rest;
   add(edge);
   if edge matches [?substart ?subfinish ?subgoal ?subfound []] then
      ;;; inactive edge
      foreach
         [?start ^substart ?label ?found [^subgoal ??rest]] do
         agenda_add([^start ^subfinish ^label
                     [^^found ^(tree(subgoal, subfound))] ^rest])
      endforeach;
      inactive_edge_procedure(edge)
   elseif edge matches [?start ?finish ?label ?found [?subgoal ??rest]] then
      ;;; active edge
      foreach [^finish ?subfinish ^subgoal ?subfound []] do
         agenda_add([^start ^subfinish ^label
                     [^^found ^(tree(subgoal, subfound))] ^rest])
      endforeach;
      active_edge_procedure(edge)
   else
      ;;; malformed edge
      mishap('adding malformed edge', [^edge])
   endif
enddefine;

;;; initialize the chart (bottom-up version)

define initialize_chart(goal, string);
   vars pos strlen;
   length(string) -> strlen;
   for pos from 1 to strlen do
      agenda_add([^(pos-1) ^pos ^(string(pos)) [] []])
   endfor;
enddefine;

;;; top level procedure

define chart_parse(goal, string);
   vars edge strlen found;
   [] -> agenda;
   [] -> database;
   length(string) -> strlen;
   initialize_chart(goal, string);
   until agenda = [] do
      agenda --> [?edge ??agenda];
      add_edge(edge)
   enduntil;
   [% foreach [0 ^strlen ^goal ?found []] do
      tree(goal, found)
   endforeach %]
enddefine;

;;; other bottom up parsing procedures

define inactive_edge_procedure(edge);
   vars start label head goals r;
   edge --> [?start = ?label = []];
   for r in rules do
      if r matches [?head ^label ??goals] then
         agenda_add([^start ^start ^head [] [^label ^^goals]])
      endif
   endfor
enddefine;

define active_edge_procedure(x); enddefine;

;;; "depth first" search

vars already_in;

define agenda_add(edge);
   unless already_in(edge, agenda) or
          already_in(edge, database) then
      [^edge ^^agenda] -> agenda
   endunless
enddefine;

member -> already_in;

;;; building parse trees

define tree(cat, subtrees);
   vars sym;
   if cat matches [?sym] then
      [^sym ^^subtrees]
   else
      cat
   endif
enddefine;

;;; top down procedures
/*

define initialize_chart(goal, string);
   vars pos strlen;
   length(string) -> strlen;
   for pos from 1 to strlen do
      agenda_add([^(pos-1) ^pos ^(string(pos)) [] []])
   endfor;
   agenda_add([0 0 dummy [] [^goal]])
enddefine;

define inactive_edge_procedure(x); enddefine;

define active_edge_procedure(edge);
   vars finish subgoal r subsubgoals;
   edge --> [= ?finish = = [?subgoal ==]];
   for r in rules do
      if r matches [^subgoal ??subsubgoals] then
         agenda_add([^finish ^finish ^subgoal [] ^subsubgoals])
      endif
   endfor
enddefine;

*/
vars chart; true -> chart;
