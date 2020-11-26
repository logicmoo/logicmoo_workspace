;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; featexp.p [Chapter  4] Expanding a feature-based grammar into a CF-PSG

;;; Assume that all the possible features, together with all
;;; their possible values, are declared in the global variable
;;; FEATURES:
;;;
;;; vars features;
;;; [
;;;   [cat   S NP VP PP AP 0 P N V]
;;;   [slash S NP VP PP AP 0]
;;;   [arg1  S NP VP PP AP 0]
;;;   [empty yes no]
;;; ] -> features;
;;;

vars features;
vars fillout_rule subst_values subst_value get_feature_val;

;;; expand a rule into all possible instances

define expand_rule(r);
   vars variable_list newrule;
   fillout_rule(r) -> newrule -> variable_list;
   subst_values(variable_list, newrule)
enddefine;

;;; augment a rule with new variables for all unmentioned
;;; features, and return also a list of variables
;;; together with possible values they can have

define fillout_rule(rule) -> newrule -> variable_list;
   vars c fs name;
   [] -> variable_list;
   [% for c in rule do
         [% for fs in features do
               fs --> [?name ==];
               get_feature_val(name, c, variable_list) -> variable_list
               ;;; leave actual value on the stack
            endfor
         %]
      endfor
   %] -> newrule
enddefine;

;;; for all possible name/value possibilities in the VARIABLE_LIST,
;;; print out the appropriate version of the rule

define subst_values(variable_list, rule);
   vars name values rest v;
   if variable_list = [] then
      rule =>
   elseif variable_list matches [[?name ??values] ??rest] then
      for v in values do
         subst_values(rest, subst_value(name, v, rule))
      endfor
   endif
enddefine;

;;; produce a new version of a thing (e.g. rule) which has VALUE
;;; substituted for each occurrence of NAME

define subst_value(name, value, thing);
   vars x;
   if thing = name then value
   elseif islist(thing) then
      [% for x in thing do subst_value(name, value, x) endfor %]
   else thing
   endif
enddefine;

;;; find the entry in a category for a given feature name.
;;; if the entry is a variable, make sure it is in the VARIABLE_LIST.
;;; if there is no entry, introduce a new variable for it

define get_feature_val(featname, cat, variable_list) -> variable_list -> val;
   vars vals;
   if c matches [== [^featname ?val] ==] then
      if isword(val) and subscrw(1, val) = `_` then
         unless variable_list matches [== [^val ==] ==] then
            features --> [== [^featname ??vals] ==];
            [[^val ^^vals] ^^variable_list] -> variable_list
         endunless
      endif
   else
      gensym("_var") -> val;
      features --> [== [^featname ??vals] ==];
      [[^val ^^vals] ^^variable_list] -> variable_list
   endif
enddefine;

vars featexp; true -> featexp;
