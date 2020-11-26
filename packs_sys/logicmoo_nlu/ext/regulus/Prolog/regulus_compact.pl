
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(regulus_compact,
	  [compact_rules/2,print_rules/1,print_rules/2]).

'LOAD_DYNAMIC_LEXICON_SUPPORT_IF_AVAILABLE'.

:- use_module('$REGULUS/Prolog/regulus_declarations').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(terms)).


% compact_rules(+RulesIn,-RulesOut)
%
% Compact RulesIn to RulesOut. Each should satisfy grouped_cfg_rules/1,
% i.e. be a legitimate Regulus rule set. RulesIn may either have
% variables in it, or (for standard Regulus use) be a ground term
% with '$VAR'(N) terms it, i.e. have undergone numbervars/3.
%
% Compaction consists of three transformations, applied repeatedly
% until no more are possible:
%
% A) "Absorbing": if a category occurs as a mother in only one rule
%    in the whole grammar, and that rule has only terminals on
%    the RHS, then the RHS is substituted for the mother category
%    wherever else it occurs in the grammar, and the rule itself is
%    deleted.
% B) Elimination of duplicate rules: if exactly the same rule occurs
%    more than once, all but one copy are discarded.
% C) Elimination of duplicate rule groups (a group is the set of
%    all rules with a given mother category): if two groups are
%    the same except for having different mother category names,
%    one is discarded, and its mother category is replaced by that
%    of the other group wherever it occurs in the grammar.
% D) Elimination of empty categories. If X -> empty as well as to
%    some non-empty stuff, delete the X -> empty rule, and for
%    all RHSs in the grammar that mention X, create a variant that doesn't
%    mention X (e.g. A -> B X C gets a variant A -> B C).
%    We assume empty RHSs are represented by the pseudo-terminal
%    *empty*, since the formalism requires something of this sort.
%    Note that *empty* can ONLY occur at the top level of the RHS!
%
% The algorithm used is:
%   1. Eliminate duplicate rules on entry (B).
%   2. Remove empty productions (D), and again eliminate duplicate rules.
%   3. Find all categories to be absorbed, and absorb them (A), all at once.
%      Eliminate duplicate rules in each group (B), since absorbing can
%      create duplicates where there were none before, and repeat
%      until no more to be absorbed.
%   4. Eliminate duplicate rule groups (C), and again, eliminate
%      duplicate rules (B), and repeat until no more to be done.
% 
% There is no need to return to steps 2 or 3 after 4, or 3 after 2, because
% later steps cannot create things that earlier steps would change.

% Arg should be yes to have statistics printed, no to be silent.

verbose_compaction(yes).

may_format(Pat,Args) :-
  (verbose_compaction(yes) ->
     format(Pat,Args);
   true).

may_flush :-
  (verbose_compaction(yes) ->
     flush_output(user_output);
   true).

% Following clauses can be useful for debugging...
%compact_rules(Rules,_) :-
%  abolish(user:rules_to_compact/1),
%  assert(user:rules_to_compact(Rules)),
%  fail.
%compact_rules(Rules,_) :-
%  open('C:/tmp_compact_in.txt',write,S),
%  print_rules(Rules,S),
%  close(S),
%  list_to_prolog_file(Rules, 'C:/tmp_compact_in.pl'),
%  fail.
compact_rules(RulesIn,RulesIn) :-
  regulus_switch(compaction, off),
  format('~NCompaction is switched off - not calling.~n~n', []),
  !.
compact_rules(RulesIn,RulesOut) :-
  format('~N -- Performing compaction~n', []),	
  timed_call(compact_rules_main(RulesIn,RulesOut),TimeTaken),
  format('~N -- Compaction done, ~1f secs~n~n', [TimeTaken]),	
  !.

compact_rules_main(RulesIn,RulesOut) :-
  load_intermediate_dynamic_lex_associations_file_if_defined(_Loaded),	
  grouped_cfg_rules(RulesIn),
  report_rule_set_size('Before compaction: ',RulesIn,NCIn,NRIn,NDIn),
  numbervars_if_necessary(RulesIn,RulesMid,_UnnumberLater),
  compact_rules1(RulesMid,RulesOut),
  %unnumbervars_if_necessary(UnnumberLater,RulesOut1,RulesOut),
  grouped_cfg_rules(RulesOut),
  report_percentage_reductions(RulesOut,NCIn,NRIn,NDIn).
  % This can be useful for debugging too:
  %open('C:/tmp_compact_out.txt',write,S),
  %print_rules(RulesOut,S),
  %close(S).

% To do the work in the main part of the algorithm, the grammar needs
% to be in numbervars-ed form, i.e. ground.

numbervars_if_necessary(Rules,Rules,no) :-
  ground(Rules),
  !.
numbervars_if_necessary(RulesIn,RulesOut,yes) :-
  unnumbervars(RulesIn,RulesOut), % in case partially numbervars'd
  numbervars_rule_groups(RulesOut).

numbervars_rule_groups([]).
numbervars_rule_groups([Cat-GroupedRules|Rules]) :-
  numbervars(Cat,0,_),
  numbervars_rule_list(GroupedRules),
  numbervars_rule_groups(Rules).

numbervars_rule_list([]).
numbervars_rule_list([H|T]) :-
  numbervars(H,0,_),
  numbervars_rule_list(T).

unnumbervars_if_necessary(yes,RulesIn,RulesOut) :-
  !,
  unnumbervars(RulesIn,RulesOut).
unnumbervars_if_necessary(no,RulesIn,RulesOut) :-
  ground(RulesIn),
  !,
  RulesOut=RulesIn.
unnumbervars_if_necessary(_,_,_) :-
  may_format('WARNING: regulus_compact:compact_rules: variables out when no variables in!!~n',[]),
  fail.

% --------------------------------------------------------------------

% If we're being verbose, report on how much compaction reduces
% the grammar by.

report_rule_set_size(Msg,Rules,NC,NR,ND) :-
  verbose_compaction(yes),
  !,
  find_rule_set_size(Rules,0,0,0,NC,NR,ND),
  format('~w~d categories, ~d rules, ~d daughters~n',[Msg,NC,NR,ND]).
report_rule_set_size(_,_,_,_,_).

find_rule_set_size([],NC,NR,ND,NC,NR,ND).
find_rule_set_size([_-Group|Rules],NCIn,NRIn,NDIn,NCOut,NROut,NDOut) :-
  NCMid is NCIn+1,
  length(Group,LG),
  NRMid is NRIn+LG,
  count_group_daughters(Group,NDIn,NDMid),
  find_rule_set_size(Rules,NCMid,NRMid,NDMid,NCOut,NROut,NDOut).

count_group_daughters([],N,N).
count_group_daughters([rule((_-->Body),_)|Rules],NIn,NOut) :-
  count_rule_daughters(Body,NIn,NMid),
  count_group_daughters(Rules,NMid,NOut).

count_rule_daughters((L,R),NIn,NOut) :-
  !,
  count_rule_daughters(L,NIn,NMid),
  count_rule_daughters(R,NMid,NOut).
count_rule_daughters(_,NIn,NOut) :-
  NOut is NIn+1.

report_percentage_reductions(RulesOut,NCIn,NRIn,NDIn) :-
  verbose_compaction(yes),
  !,
  report_rule_set_size('After  compaction: ',RulesOut,NCOut,NROut,NDOut),
  percent_reduction(NCIn,NCOut,NCRed),
  percent_reduction(NRIn,NROut,NRRed),
  percent_reduction(NDIn,NDOut,NDRed),
  format('Percent reduction: ~d% categories, ~d% rules, ~d% daughters~n',
	 [NCRed,NRRed,NDRed]).
report_percentage_reductions(_,_,_,_).

percent_reduction(Big,Small,Perc) :-
  Perc is floor(0.5+100*(1-Small/Big)).

% --------------------------------------------------------------------

% Test for well-formedness of the rule set...

grouped_cfg_rules([]).
grouped_cfg_rules([F|R]) :-
  cfg_rule_group(F),
  grouped_cfg_rules(R).

cfg_rule_group(MotherCat-Rules) :-
  cat_name(MotherCat),
  cfg_rules(Rules),
  !.
cfg_rule_group(X) :-
  format('Not a CFG rule group: ~q~n',[X]),
  fail.

cfg_rules([]).
cfg_rules([F|R]) :-
  cfg_rule(F),
  cfg_rules(R).

cfg_rule(rule(MainRule, LineInfo)) :-
  cfg_rule_main(MainRule),
  line_info(LineInfo),
  !.
cfg_rule(X) :-
  format('Not a CFG rule: ~q~n',[X]),
  fail.

cfg_rule_main(( Head --> Body )) :-
  cfg_rule_cat(Head),
  (Body == '*empty*'; % DMC's addition
  cfg_rule_body(Body)).

cfg_rule_body(X) :-
  (cfg_terminal(X) ;
   cfg_rule_cat(X) ;
   cfg_rule_conjunction(X)),
  !.
cfg_rule_body(X) :-
  format('Not a CFG rule body: ~q~n',[X]),
  fail.

cfg_terminal(X) :-
  atom(X).

cfg_rule_conjunction((L, R)) :-
  cfg_rule_body(L),
  cfg_rule_body(R).

cfg_rule_cat(cat(CatName, [], SemValue)) :-
  cat_name(CatName),
  sem_value(SemValue).

cat_name(X) :-
  atomic(X).

sem_value(value(GlobalOrReturn, MainValue)) :-
  ( GlobalOrReturn = global ; GlobalOrReturn = return ),
  main_value(MainValue).
% Added by DMC:
sem_value(no_value).

main_value(_).

line_info(line_info(Id, From-To, File)) :-
  ( number(Id) ; ( Id = frequency(Freq), number(Freq) ) ),
  number(From),
  number(To),
  atom(File).

% -----------------------------------------------------------

% First, sort the rules within each group; this is step (B) of
% compaction because Prolog sort eliminates duplicates. The
% sorting is also useful because it allows us to recognize
% duplicate rule sets.
%
% In each Name-Group rule group, replace Name by * wherever it occurs
% in Group. This also helps in recognizing duplicate groups because of
% the possibility of recursion.
%
% After doing the main compaction operation (compact_rules2),
% apply the "star" operation in the reverse direction.

% There used to be a compact_rules3, but it got moved to regulus_eliminate_empties

compact_rules1(RulesIn,RulesOut) :-
  sort_rules_in_groups(RulesIn,RulesMid),
  star_recursive_names(RulesMid,RulesMid2),
  compact_rules3(RulesMid2,RulesOut1),      
  star_recursive_names(RulesOut,RulesOut1).

sort_rules_in_groups([],[]).
sort_rules_in_groups([Cat-In|RulesIn],[Cat-Out|RulesOut]) :-
  sort_rule_group(In,Out),
  sort_rules_in_groups(RulesIn,RulesOut).

% First, sort normally; Main comes before Info so rules with same Main
% should then be adjacent. Then, prune out any such.
sort_rule_group(In,Out) :-
  sort(In,Mid),
  remove_duplicate_rules(Mid,Out).

remove_duplicate_rules([],[]).
remove_duplicate_rules([rule(Main,_)|In],Out) :-
  In = [rule(Main,_)|_],
  !,
  remove_duplicate_rules(In,Out).
remove_duplicate_rules([R|In],[R|Out]) :-
  remove_duplicate_rules(In,Out).

% This predicate works both ways round...

star_recursive_names([],[]).
star_recursive_names([Name-GroupIn|RulesIn],[Name-GroupOut|RulesOut]) :-
  star_recursive_names1(GroupIn,Name,GroupOut),
  star_recursive_names(RulesIn,RulesOut).

star_recursive_names1([],_,[]).
star_recursive_names1([rule((HeadIn-->BodyIn),Info)|GroupIn],Name,
		      [rule((HeadOut-->BodyOut),Info)|GroupOut]) :-
  star_recursive_names2(HeadIn,Name,HeadOut),
  star_recursive_names2(BodyIn,Name,BodyOut),
  star_recursive_names1(GroupIn,Name,GroupOut).

star_recursive_names2(cat(Name,F,S),Name,cat('*',F,S)) :-
  !.
star_recursive_names2(cat(Name,F,S),_,cat(Name,F,S)) :-
  !.
star_recursive_names2((L1,R1),Name,(L2,R2)) :-
  !,
  star_recursive_names2(L1,Name,L2),
  star_recursive_names2(R1,Name,R2).
star_recursive_names2(X,_,X).

% -----------------------------------------------------------

% The core of the compaction algorithm: first repeatedly look for
% categories to "absorb" (step A of compaction), then repeatedly look for
% duplicate categories (groups). Interleaved with both of these is
% duplicate-rule elimination within the same group.

compact_rules3(RulesIn,RulesOut) :-
  compact_rules_absorb(RulesIn,RulesMid),
  !,
  compact_rules3(RulesMid,RulesOut).
compact_rules3(RulesIn,RulesOut) :-
  compact_rules4(RulesIn,RulesOut).

compact_rules4(RulesIn,RulesOut) :-
  compact_rules_uniquify(RulesIn,RulesMid),
  !,
  compact_rules4(RulesMid,RulesOut).
compact_rules4(Rules,Rules).

% ---------------------------------------------------
%
% The absorbing code: step A of compaction. Look for all the absorbpable
% rule groups in the grammar, and absorb them all. Succeed iff any
% such groups were found.
%
% We don't absorb the first (assumed top) rule in the grammar...
  
compact_rules_absorb(RulesIn,RulesOut) :-
  RulesIn=[_TopRule|RestRulesIn],
  findall(RG,(member(RG,RestRulesIn),absorbable_rule_group(RG)),AbsList),
  AbsList=[_|_],
  !,
  (verbose_compaction(yes) ->
     length(AbsList,ZLen),
     format('  Absorbing ~w preterminal categories ... ',[ZLen]),
     flush_output(user_output),
     absorb_rule_groups(RulesIn,AbsList,RulesOut),
     length(RulesOut,L),
     format('we now have ~w categories~n',[L]);
   absorb_rule_groups(RulesIn,AbsList,RulesOut)).

% Only single-rule groups are absorbable.
% We don't want to absorb dynamic rules, since this makes things much more complicated and isn't even clearly useful.

absorbable_rule_group(_Cat-[rule((Head --> Body),_Info)]) :-
  \+ is_dynamic_nuance_rule((Head --> Body)),
  absorbable_rule_body(Body).

% And a rule body is absorbable if it contains only terminals.

absorbable_rule_body(X) :-
  cfg_terminal(X).
absorbable_rule_body((H,T)) :-
  absorbable_rule_body(H),
  absorbable_rule_body(T).
 
% For efficiency, set ZNames to the names of all the absorbable
% categories. Then get going...

absorb_rule_groups(RulesIn,AbsList,RulesOut) :-
  findall(Name,member(Name-_,AbsList),ZNames),
  absorb_rule_groups(RulesIn,ZNames,AbsList,RulesOut).

% For each group, if it is itself absorbable, then absorb it, i.e.
% leave it out of RulesOut. Otherwise, substitute for absorbable
% categories in the daughter part of the group.

absorb_rule_groups([],_,_,[]).
absorb_rule_groups([Name-_|RulesIn],ZNames,AbsList,RulesOut) :-
  member(Name,ZNames),
  !,
  absorb_rule_groups(RulesIn,AbsList,RulesOut).
absorb_rule_groups([Name-GroupIn|RulesIn],
		ZNames,AbsList,
		[Name-GroupOut|RulesOut]) :-
  !,
  substitute_for_absorbed(GroupIn,ZNames,AbsList,GroupMid),
  % This sorting also removes duplicates...
  sort_rule_group(GroupMid,GroupOut),
  absorb_rule_groups(RulesIn,ZNames,AbsList,RulesOut).

% Substitute for absorbed categories in each rule in the group...

substitute_for_absorbed([],_,_,[]).
substitute_for_absorbed([rule(MainIn,Info)|In],ZNames,AbsList,
		     [rule(MainOut,Info)|Out]) :-
  % An efficiency check to see if there is anything to do,
  % since the actual substituion needs to be done with the
  % rule body unnumbervars-ed.
  MainIn = (_-->BodyIn),
  contains_cat_with_name(BodyIn,ZNames),
  !,
  unnumbervars(MainIn,MainInUNV),
  substitute_for_absorbed1(MainInUNV,AbsList,MainOut),
  numbervars(MainOut,0,_),
  substitute_for_absorbed(In,ZNames,AbsList,Out).
% If the rule doesn't contain any categories to be absorbed,
% we leave it alone.
substitute_for_absorbed([Rule|In],ZNames,AbsList,[Rule|Out]) :-
  substitute_for_absorbed(In,ZNames,AbsList,Out).

contains_cat_with_name(cat(Name,_,_),Lst) :-
  !,
  memberchk(Name,Lst).
contains_cat_with_name((L,_),Lst) :-
  contains_cat_with_name(L,Lst),
  !.
contains_cat_with_name((_,R),Lst) :-
  contains_cat_with_name(R,Lst),
  !.

substitute_for_absorbed1((Head --> BodyIn),AbsList,(Head --> BodyOut)) :-
  substitute_for_absorbed2(BodyIn,AbsList,BodyOut).

% If we have a cat that is to be absorbed, unify the features (should
% be [] anyway) and the body, and return the result. We do this
% with a copy of the absorbed rule to prevent it being further
% specialized (though it ought to be ground anyway).

substitute_for_absorbed2(cat(Name,Feats1,Sem1),AbsList,Body) :-
  member(Name-[rule((cat('*',Feats2,Sem2) --> ZBody),_)],AbsList),
  copy_term((Feats2,Sem2,ZBody),Copy),
  (Feats1,Sem1,Body)=Copy,
  !.
substitute_for_absorbed2((LIn,RIn),AbsList,(LOut,ROut)) :-
  !,
  substitute_for_absorbed2(LIn,AbsList,LOut),
  substitute_for_absorbed2(RIn,AbsList,ROut).
% If we're at either a terminal or a cat/3 that isn't to be absorbed,
% leave it alone.
substitute_for_absorbed2(Body,_,Body).

% -----------------------------------------------------

% Step (C) of compaction. The basic idea is simple, but make it
% efficient is less so. Here's how we do it:
%
% * Restructure the rule group so that the name is first, then the
%   rules themselves without the info, then the info. This means
%   we can sort the groups using sort/2 and have equivalent groups
%   next to each other, making the search more efficient.
% * Find bindings of Name1 to Name2 where the groups for Name1 and
%   Name2 are equivalent, and Name1 will be replaced by Name2.
% * Resolve the bindings so bdg(Name1,Name2),bdg(Name2,Name3)
%   because bdg(Name1,Name3),bdg(Name2,Name3).
% * Apply the bindings - change the names and eliminate the groups
%   to be eliminated. This is the actual compacting bit.
% * Convert back to standard rule group format (though the order
%   of the rule groups is not preserved).
% * Look for and eliminate duplicate rules in each group.
%   Duplications may have been introduced by category
%   renaming in a rule body.

compact_rules_uniquify(RulesIn,RulesOut) :-
  reorder_rules_for_sorting(RulesIn,Groups1),
  sort(Groups1,Groups2),
  find_equivalent_names(Groups2,Bdgs,Groups3),
  \+(Bdgs=[]), % fail if there's nothing to do
  !,
  length(Bdgs,N),
  may_format('  Deleting ~w redundant categories ... ',[N]),
  resolve_bindings(Bdgs,ResBdgs),
  apply_bindings(Groups3,ResBdgs,Groups4),
  reorder_rules_for_sorting(RulesOut1,Groups4),
  sort_rules_in_groups(RulesOut1,RulesOut), % step B
  (verbose_compaction(yes) ->
     length(RulesOut,N2),
     format('we now have ~w categories~n',[N2]);
   true).

reorder_rules_for_sorting([],[]).
reorder_rules_for_sorting([HIn|TIn],[HOut|TOut]) :-
  reorder_group_for_sorting(HIn,HOut),
  reorder_rules_for_sorting(TIn,TOut).

reorder_group_for_sorting(Name-RulesIn,group(RulesOut,Name,InfoList)) :-
  reorder_rules_for_sorting1(RulesIn,RulesOut,InfoList).

% Separate out the info, and replace Head-->Body by prod(Head,Body)
% to avoid any confusion about which format we're working with.

reorder_rules_for_sorting1([],[],[]).
reorder_rules_for_sorting1([rule((Head --> Body),Info)|RulesIn],
			   [prod(Head,Body)|RulesOut],
			   [Info|InfoList]) :-
  reorder_rules_for_sorting1(RulesIn,RulesOut,InfoList).

% We've sorted the groups before coming here, so equivalent groups
% should always be neighbours in the order. Because we've separated
% out the info, and starred any recursivelly called names, equivalence
% is just a matter of equality of the Rules fields.
%
% As well as building up the Bdgs list, this predicate also discards
% rule groups (in its final arg).

find_equivalent_names([],[],[]).
% Same Rules value: create a binding, drop the first group
find_equivalent_names([group(Rules,Name1,_)|GroupsIn],
		      [bdg(Name1,Name2)|Bdgs],GroupsOut) :-
  GroupsIn = [group(Rules,Name2,_)|_], % same rules!
  !,
  find_equivalent_names(GroupsIn,Bdgs,GroupsOut).
% Different Rules values: keep the bindings, keep the group.
find_equivalent_names([G|GroupsIn],Bdgs,[G|GroupsOut]) :-
  find_equivalent_names(GroupsIn,Bdgs,GroupsOut).

% Ensure any chains of bindings are resolved so that each non-final
% item is bound to the final one, not to the next in the chain.
% Then the bindings can be used for straight substitution.

resolve_bindings(In,Out) :-
  reverse(In,RIn),
  resolve_bindings(RIn,[],Out).

resolve_bindings([],Ans,Ans).
resolve_bindings([bdg(A,B)|BdgsIn],Done,Ans) :-
  (member(bdg(B,C),Done) -> true; C=B),
  resolve_bindings(BdgsIn,[bdg(A,C)|Done],Ans).

% Adjust each (surviving) group so that when bdg(X,Y) exists,
% calls to X are replaced by calls to Y. We pass the MotherName
% down to ensure that recursive calls are shown by * not the
% name itself.

apply_bindings([],_,[]).
apply_bindings([group(RulesIn,MotherName,Info)|GroupsIn],Bdgs,
	       [group(RulesOut,MotherName,Info)|GroupsOut]) :-
  apply_bindings1(RulesIn,MotherName,Bdgs,RulesOut),
  apply_bindings(GroupsIn,Bdgs,GroupsOut).

apply_bindings1([],_,_,[]).
apply_bindings1([prod(Head,BodyIn)|RulesIn],MotherName,Bdgs,
		[prod(Head,BodyOut)|RulesOut]) :-
  apply_bindings2(BodyIn,MotherName,Bdgs,BodyOut),
  apply_bindings1(RulesIn,MotherName,Bdgs,RulesOut).

apply_bindings2(cat(CName,F,S),MotherName,Bdgs,BodyOut) :-
  !,
  find_new_name(CName,Bdgs,MotherName,NewName),
  BodyOut = cat(NewName,F,S).
apply_bindings2((LIn,RIn),MotherName,Bdgs,(LOut,ROut)) :-
  apply_bindings2(LIn,MotherName,Bdgs,LOut),
  apply_bindings2(RIn,MotherName,Bdgs,ROut).
apply_bindings2(X,_,_,X).	

find_new_name(NameIn,Bdgs,MotherName,NameOut) :-
  member(bdg(NameIn,Bound),Bdgs),
  !,
  (Bound = MotherName -> NameOut = '*'; % recursive call
   NameOut = Bound).
find_new_name(Name,_,_,Name).

% -------------------------------------------------------------------
% Useful rule-prettyprinting code, for debugging...

print_rules(Rules) :-
  print_rules(Rules,user_output).

print_rules(Rules,S) :-
  print_rules(Rules,S,1).

print_rules([],_,_).
print_rules([Sym-Group|Rules],S,N) :-
  format(S,'~d: ~w~n',[N,Sym]),
  copy_term(Group,Copy),
  numbervars(Copy,0,_),
  print_rule_group(Copy,Sym,S),
  nl(S),
  N1 is N+1,
  print_rules(Rules,S,N1).

print_rule_group([],_,_) :-
  !.
print_rule_group([rule((cat(Name,Feats,Sem) --> Body),_)|Rules],Sym,S) :-
  (Name=Sym -> Name2='*';Name2=Name),
  format(S,'   ~q -->~n      ',[cat(Name2,Feats,Sem)]),
  print_rule_body(Body,S),
  nl(S),
  print_rule_group(Rules,Sym,S).

print_rule_body((L,R),S) :-
  !,
  print_rule_body(L,S),
  format(S,'~n      ',[]),
  print_rule_body(R,S).
print_rule_body(X,S) :-
  format(S,'~q',[X]).

% --------------------------------------------------------------
%
% This should go in a more general place...

unnumbervars(X,Y) :-
  unnumbervars(X,Y,_).

unnumbervars(V,W,_) :-
  var(V),
  !,
  W=V.
unnumbervars('$VAR'(N),V,Lst) :-
  !,
  nth_member_from_zero(N,Lst,V).
unnumbervars(X,Y,Lst) :-
  functor(X,F,N),
  functor(Y,F,N),
  unnumbervars_args(N,X,Y,Lst),
  !.
unnumbervars(X,Y,Lst) :-
  format('UNNUMBERVARS FAILURE:~n   ~q~n   ~q~n   ~q~n~n',[X,Y,Lst]),
  abort.

unnumbervars_args(0,_,_,_).
unnumbervars_args(N,X,Y,Lst) :-
  arg(N,X,XA),
  arg(N,Y,YA),
  unnumbervars(XA,YA,Lst),
  N1 is N-1,
  unnumbervars_args(N1,X,Y,Lst).

nth_member_from_zero(0,[V|_],V) :-
  !.
nth_member_from_zero(N,[_|T],X) :-
  N1 is N-1,
  nth_member_from_zero(N1,T,X).

test :-
  rules_to_compact(X),
  open('C:/tmpin.txt',write,S1),
  print_rules(X,S1),
  close(S1),
  compact_rules(X,Y),
  abolish(compacted_rules/1),
  assert(compacted_rules(Y)),
  open('C:/tmpout.txt',write,S2),
  !,
  print_rules(Y,S2),
  close(S2).

