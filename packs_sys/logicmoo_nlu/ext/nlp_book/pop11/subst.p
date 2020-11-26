;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; subst.p [Chapter  7] Substitution utilities
;;;
;;; Procedures and constants provided:
;;;
;;; EMPTY_SUBST (a substitution)
;;; LOOKUP_SUBST(ITEM,SUBSTITUTION) -> ITEM
;;; ADD_SUBST(VARIABLE,ITEM,SUBSTITUTION) -> SUBSTITUTION
;;; APPLY_SUBST(SUBSTITUTION,STRUCTURE) -> NEWSTRUCTURE
;;; COMPOSE_SUBSTS(SUBSTITUTION,SUBSTITUTION) -> SUBSTITUTION
;;;
;;; ISVAR(ITEM) -> TRUE/FALSE
;;; RENAME(ITEM) -> NEWITEM
;;; NEWVAR() -> VARIABLE
;;; GROUND(ITEM) -> TRUE/FALSE
;;;
;;; OPERATIONS ON VARIABLE SYMBOLS

define isvar(x);
   isword(x) and subscrw(1,x) = `_`
enddefine;

define ground(term);
   vars x;
   if isvar(term) then false
   elseif islist(term) then
      for x in term do
         unless ground(x) then return(false) endunless
      endfor;
      true
   else true
   endif
enddefine;

;;; renames an arbitrary list structure
;;; sublist which satisfies ISVAR is assumed to denote a variable

vars ren;
vars rename_assoc;

define rename(list);
   newproperty([],20,false,false) -> rename_assoc;
   ren(list)
enddefine;

vars rename_var newvar;

define ren(list);
   if isvar(list) then
      rename_var(list)
   elseif list.islist then
      maplist(list,ren)
   else
      list
   endif
enddefine;

define rename_var(v);
   unless rename_assoc(v) then
      newvar() -> rename_assoc(v)
   endunless;
   rename_assoc(v)
enddefine;

;;; generate a new variable

define newvar();
   gensym("_")
enddefine;

;;; OPERATIONS ON SUBSTITUTIONS

vars empty_subst; [] -> empty_subst;

define lookup_subst(value,substitution);
   vars val;
   if isvar(value) and substitution matches
            [== [^value ?val] ==] then
      lookup_subst(val,substitution)
   else
      value
   endif
enddefine;

define add_subst(var,value,substitution);
   [[^var ^value] ^^substitution]
enddefine;

define compose_substs(s1,s2);
   s1 <> s2
enddefine;

;;; apply a substitution to an arbitrary list structure

define apply_subst(subst,item);
   vars g predicate args varname a val;
   lookup_subst(item,subst) -> item;
   if islist(item) then
      [%
         for g in item do
             apply_subst(subst,g)
         endfor
      %]
   else
      item
   endif
enddefine;

vars subst; true -> subst;
