% ===================================================================
% File 'logicmoo_module_aiml_loader.pl'
% Purpose: An Implementation in SWI-Prolog of AIML
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% See https://nlp.stanford.edu/~wcmac/downloads/fracas.xml
% ===================================================================

:- module(fracas_iface, []).

:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo/xml_reader)).
%:- use_module(library(logicmoo_nlu/e2c/e2c_fracas)).
%:- use_module(library(nomic_mu)).

:- set_prolog_flag(sgml_parser_defaults, [defaults(false), qualify_attributes(false), dialect(xml)]).
:- set_prolog_flag(sgml_parser_callbacks, [max_errors(0), call(begin, on_begin), call(end, on_end)]).



pp_fracas:- cls, make  , pp_fracas(pack('logicmoo_nlu/test/fracas.xml')).

pp_fracas(FNI):- absolute_file_name(FNI, FN), expand_file_name(FN, Exp), member(F, Exp), pp_fracas1(F), fail.
pp_fracas(_):-!.

pp_fracas1(File):-
    fileToLineInfoElements(_Ctx, File, Z),
     writeEachTo([], [], Z).

split_atom(A, B, C, E):- split_string(A, B, C, E0), maplist(any_to_atom, E0, E), !.

splt_g(Atom, G):- atom_contains(Atom, ', '), split_atom(Atom, ", ", " ", G).
splt_g(Atom, G):- split_atom(Atom, " ", " ", G).
splt_g('', []).

pairs_to_values(SO, Vs):- maplist(arg2of, SO, Vs).
arg2of(_=V, V).
arg2of(V, V).

crrect_value_arg(E, [V], VV):-!, crrect_value_arg(E, V, VV).
crrect_value_arg(_, V, VV):- compound(V), !, VV=V.
crrect_value_arg(_, V, VV):- \+ atom(V), !, VV=V.
crrect_value_arg(_, V, V):- atom_number(V, _), !.
crrect_value_arg(_, V, VV):- split_string(V, "", " \n\t\r", [VV]).

append_terms([H|List], O):- append_terms(H, List, O).

append_terms(P, L, O):- L==[], P = O, !.
append_terms(P, L, O):- P==[], L = O, !.
append_terms([F|P], L, O):- atom(F), is_list(P), !, PP=..[F|P], append_terms(PP, L, O).
append_terms(P, L, O):- is_list(P), PP=..[lst|P], append_terms(PP, L, O).
append_terms(P, L, O):- is_list(L)->append_termlist(P, L, O);append_term(P, L, O).


writeEachTo(_ID, _Ctx, []):-!.
writeEachTo(_ID, _Ctx, ''):-!.
writeEachTo(ID, Ctx, List):- is_list(List), !, must_maplist(writeEachTo(ID, Ctx), List).
writeEachTo(ID, Ctx, N=V):- crrect_value_arg(N, V, VV), !, append_terms([Ctx, ID, N, VV], O), !, do_test(O).

writeEachTo(ID, Ctx, element(Tag, [], STUFF)):- member(Tag, ['fracas', 'problems']), !, writeEachTo(ID, Ctx, STUFF).

writeEachTo(ID, Ctx, element(Tag, _, STUFF)):- member(Tag, ['q', 'a', 'p', 'h', 'why']), !, writeEachTo(ID, Ctx, Tag=STUFF).

writeEachTo(ParentID, Ctx, element(CLASS, S, STUFF)):- % member(CLASS, ['VNCLASS', 'VNSUBCLASS']), !,
   member('id'=ID, S), !,
   flag(frame_num, _, 0),
   nl, nl,
   do_test(switch_test(ID, CLASS, ParentID, Ctx)),
   writeEachTo(ID, [CLASS], S),
   writeEachTo(ID, [CLASS], STUFF), !.


writeEachTo(ID, Ctx, element('role', [], STUFF)):- !,
  flag(frame_num, N, N+1),
  (N==0->atomic_list_concat([ID, '_f', N], NewID);atomic_list_concat([ID, '_f', N], NewID)),
  writeEachTo(NewID, Ctx, STUFF).

writeEachTo(_D, _Tx, element('comment', _, S)):-!, nl, writeEachTo(problem_header, problem_header, S), !.

writeEachTo(ID, Ctx, element(Tag, [], STUFF)):- !, writeEachTo(ID, [Tag|Ctx], STUFF).

writeEachTo(_, Ctx, Atom):-append_terms(Ctx, Atom, O), !, do_test(O).
writeEachTo(_, Ctx, S):-length(Ctx, L), tab(L), append_terms(S, Ctx, O), do_test(O), !.

do_test(problem_header(O)):- write('\n/*\n'), fmt(O), write('\n\n*/\n').

do_test(O):- format('~N~q.~n', [:-O]), call(O).

switch_test(N, _, _, _):- dmsg(switch_test(N)), nb_setval('$fracas_test',N).

note(_, Note):- nb_current('$fracas_test',N),problem(N, note, Note),!.

:- abolish(fracas_test_problem/3).

%problem(N, _Else, Text):- N == Text.
problem(N, Else, Text):- part_of_test(Else,New),!,problem(N, New, Text).
%problem(N, Else, Text):- \+ is_list(Else),flatten([fracas(N),Else],NewElse),!,problem(N, NewElse, Text).
problem(N, Else, Text):- dmsg(fracas_test_problem(N, Else, Text)), fail.
% problem(_N, _Else, _Text):-!.
problem(N, Else, Text):- assertz_if_new(fracas_test_problem(N, Else, Text)),!.
problem(_N, p, Text):- !, e2c(Text).
problem(_N, q, Text):- !, e2c(Text).
problem(_N, _, Text):- !, e2c(Text).

:- multifile(parser_e2c:test_e2c/2).
:- parser_e2c:export(parser_e2c:test_e2c/2).
:- import(parser_e2c:test_e2c/2).
:- dynamic(parser_e2c:test_e2c/2).

parser_e2c:e2c_test(T,W):- e2c_fracas(T,W).

e2c_fracas(NewText,[fracas_test_problem(N),oper(Why)]):- 
  fracas_test_problem(N,zid,N),
  once((findall(Why+Text,(fracas_iface:fracas_test_problem(N,Why,Text)),Items),
  sort(Items,ItemsS),reverse(ItemsS,ItemsR))),
  member(Why+Text,ItemsR),
  once((reframe_text(N,Text,Why,NewText),string(NewText),
  atomic_list_concat(AtLeastThreeWords," ",NewText),
  AtLeastThreeWords = [_,_,_|_])).

reframe_text(_,Text,zid,Write):- sformat(Write,'Your next test is named "~w".',[Text]).
reframe_text(_,Text,ask,Write):- sformat(Write,'~w',[Text]).
reframe_text(_,Text,tell,Write):- sformat(Write,'~w',[Text]).
reframe_text(N,Text,hypothesis,Write):- fracas_iface:fracas_test_problem(N, fracas_answer, "no"),!, lower_first_word(Text,LText), sformat(Write,'You should not be able to infer that ~w',[LText]).
reframe_text(_,Text,hypothesis,Write):- lower_first_word(Text,LText), sformat(Write,'You should be able to infer that ~w',[LText]).
reframe_text(_,Text,'1stexpected_answer',Write):- lower_first_word(Text,LText), sformat(Write,'Your expected answer was ~w.',[LText]).
reframe_text(_,Text,why,Write):-  lower_first_word(Text,LText), sformat(Write,'Your test is to seek ~w',[LText]).
reframe_text(_,Text,_,Text). 

lower_first_word(Text,LText):- name(Text,[C|Rest]),maybe_change_case(Text,C,L),text_to_string([L|Rest],LText).

maybe_change_case(Text,C,L):-code_type(C,to_upper(L)), into_text80(Text,[W|_]),l_word(W),!.
maybe_change_case(_,C,C).


atom_starts_with(S,A):- atom_concat(S,_,A).

fracas(LText):- e2c_fracas(X,_Y), \+ atom_starts_with("You",X), lower_first_word(X,LText).
%e2c_fracas:- forall(e2c_fracas(X,_Y),run_pipeline(X)).
:- add_history(forall(e2c_fracas(X,_Y),fmt('?- run_pipeline( ~p ). ',[X]))).

l_word(F):- atom_length(F,1).
l_word(F):- upcase_atom(F,U),U==F,!,fail.
l_word(F):- l_human_name(H),atom_starts_with(H,F), !,fail.
l_word(_).
l_human_name('Mar'). l_human_name('Mik'). 
l_human_name('Fido').
l_human_name('Mic').
l_human_name('Bill'). l_human_name('Jo'). l_human_name('Smith').
l_human_name('Dumbo'). l_human_name('Kim'). l_human_name('Pav').
%l_word(F):- to functionword(F).
%l_word(most). l_word(most). l_word(both). l_word(a).

part_of_test(p,tell).
part_of_test(id,zid).
part_of_test(q,ask).
part_of_test(a,'1stexpected_answer').
part_of_test(h,hypothesis).

pi(_).

:- fixup_exports.


/*


=================================================================================
THE FRACAS TEXTUAL INFERENCE PROBLEM SET
=================================================================================

The textual inference problems contained in this file are derived from the
FraCaS project undertaken in the mid-1990s.  The source document is:

    "Using the Framework"
    The FraCaS Consortium
    Cooper et al.
    January 1996
    ftp://ftp.cogsci.ed.ac.uk/pub/FRACAS/del16.ps.gz

There are 346 problems.  Each problem contains one or more premises and one
question.  There are a total of 536 premises, or an average of 1.55 premises per
problem.  The premise counts are distributed as follows:

            # premises      # problems      % problems
            ----------      ----------      ----------
                1              192            55.5 %
                2              122            35.3 %
                3               29             8.4 %
                4                2             0.6 %
                5                1             0.3 %

---------------------------------------------------------------------------------

Conversion to XML by Bill MacCartney (wcmac@cs.stanford.edu).  If you benefit
from my work, please give me appropriate credit.

Some notes on the XML representation:

The 'p' and 'q' elements:

    These contain the premises and the question from the original source
    problem.  Note that four problems are missing questions altogether.

    I have made some very minor corrections here and there, such as adding
    missing sentence-final periods.  But I've generally been reluctant to make
    other edits, even when the original is not perfectly grammatical.

The 'h' element:

    The original problems included questions, not hypotheses.  But in my work, I
    prefer to work with declarative hypotheses.  I've therefore introduced the
    'h' element, containing a sentence which is, as nearly as possible, the
    declarative equivalent to the question posed in the 'q' element.  The
    transformation from question form to declarative form was initially
    performed automatically, but all the results were manually reviewed and
    edited for correctness.  Note that in a few questions (specifically, those
    containing "any", "anyone", or "either"), it was necessary to change some
    key words to achieve grammaticality.  "Any" and "anyone" were replaced by
    "some" and "someone", while "either" was replaced by "one of the".  Problems
    38, 46, 54, 55, 62, 70, 71, 78, 107, 108, 141, 344 were affected in this
    way.

The 'a' element:

    Most problems in the source document included an answer, marked in square
    brackets.  The 'a' element shows that answer.  Most answers are "Yes",
    "Don't know", or "No", but some are longer phrases containing qualifications
    or elaborations such as "Yes, on one reading".  The answer distribution is
    roughly as follows:

        180    52%     Yes
         94    27%     Don't know
         31     9%     No
         41    12%     [other / complex]

The 'why' element:

    These elements contain text from the source document intended to explain or
    justify the answer.

The 'note' element:

    I've added notes to a number of problems in order to explain issues which
    arose during translation to XML.

The 'fracas_answer' attribute:

    To remedy the fact that the answers given in the source document are not
    restricted to a set of canonical values, most problems include a
    'fracas_answer' attribute, having one of the values {'yes', 'no', 'unknown',
    'undef'}.  In most cases, this attribute value was generated by the obvious
    mapping from the 'a' element, but for complex 'a' elements I used my own
    judgment in generating the 'answer' value.

The 'fracas_nonstandard' attribute:

    If the answer given in the source document is not one of {'Yes', 'No', or
    'Don't know'}, then the problem will include the 'fracas_nonstandard'
    attribute with value 'true'.  41 problems have this annotation.


---------------------------------------------------------------------------------

Here's the outline of the problem set:

    1 GENERALIZED QUANTIFIERS
        1.1 Conservativity
        1.2 Monotonicity (upwards on second argument)
        1.3 Monotonicity (downwards on second argument)
        1.4 Monotonicity (upwards on first argument)
        1.5 Monotonicity (downwards on first argument)

    2 PLURALS
        2.1 Conjoined Noun Phrases
        2.2 Definite Plurals
        2.3 Bare Plurals
        2.4 Dependent Plurals
        2.5 Negated Plurals
        2.6 Collective and Distributive Plurals

    3 (NOMINAL) ANAPHORA
        3.1 Intra-Sentential
        3.2 Inter-Sentential
        3.3 Plural Anaphora
        3.4 E-type and Donkey Pronouns
        3.5 Functional and Subsectional
        3.6 Simple Reflexives

    4 ELLIPSIS
        4.1 VP Ellipsis
        4.2 Gapping
        4.3 One Anaphora
        4.4 Sluicing
        4.5 Phrasal Ellipsis
        4.6 Antecedent Contained Deletion
        4.7 Configurational Effects
        4.8 Ellipsis and Anaphora
        4.9 Ellipsis and Quantification

    5 ADJECTIVES
        5.1 Affirmative and Non-Affirmative
        5.2 No Comparison Class
        5.3 Opposites
        5.4 Extensional Comparison Classes
        5.5 Extensional and Intensional Comparison Classes
        5.6 Default Comparison Classes

    6 COMPARATIVES
        6.1 Phrasal Comparatives
        6.2 Clausal Complement
        6.3 Measure Phrases
        6.4 Differential Comparatives
        6.5 Attributive Comparatives
        6.6 Comparatives and Quantifiers

    7 TEMPORAL REFERENCE
        7.1 Standard Use of Tenses
        7.2 Temporal Adverbials
            7.2.1 Indexicals
            7.2.2 "Before", "After" (Temporal Subordinate Clauses)
            7.2.3 "In", "For" and "On" Temporal Adverbials
            7.2.4 Quantificational Adverbials
        7.3 Anaphoric Dimension
        7.4 Adverbs of Quantification
        7.5 Some more Complex Examples

    8 VERBS
        8.1 Aspectual Classes
        8.2 Distributive and Collective Predication

    9 ATTITUDES
        9.1 Epistemic, Intentional and Reportive Attitudes
        9.2 Preceptive Attitudes: "See" with Bare Infinitive Complements
            9.2.1 Inferences we do not get
            9.2.2 Veridicality
            9.2.3 Substitution
            9.2.4 Existential instantiation
            9.2.5 Conjunction distribution
            9.2.6 Disjunction distribution

---------------------------------------------------------------------------------

Here's the breakdown by topic:

                                                   single-
    sec   topic            start   count     %     premise
    ---   -----------      -----   -----   -----   -------
     1    Quantifiers         1      80     23 %      50
     2    Plurals            81      33     10 %      24
     3    Anaphora          114      28      8 %       6
     4    Ellipsis          142      55     16 %      25
     5    Adjectives        197      23      7 %      15
     6    Comparatives      220      31      9 %      16
     7    Temporal          251      75     22 %      39
     8    Verbs             326       8      2 %       8
     9    Attitudes         334      13      4 %       9



*/


/*
 1 GENERALIZED QUANTIFIERS

*/


/*
 1.1 Conservativity

*/


/*
 Q As are Bs == Q As are As who are Bs

*/


:-switch_test('001', problem, [], ['fracas-problems']).
:-problem('001', id, '001').
:-problem('001', fracas_answer, "yes").
:-problem('001', p, "An Italian became the world's greatest tenor.").
:-problem('001', q, "Was there an Italian who became the world's greatest tenor?").
:-problem('001', h, "There was an Italian who became the world's greatest tenor.").
:-problem('001', a, "Yes").


:-switch_test('002', problem, [], ['fracas-problems']).
:-problem('002', id, '002').
:-problem('002', fracas_answer, "yes").
:-problem('002', p, "Every Italian man wants to be a great tenor.").
:-problem('002', p, "Some Italian men are great tenors.").
:-problem('002', q, "Are there Italian men who want to be a great tenor?").
:-problem('002', h, "There are Italian men who want to be a great tenor.").
:-problem('002', a, "Yes").
:-note(problem, ' Note that second premise is unnecessary and irrelevant. ').


:-switch_test('003', problem, [], ['fracas-problems']).
:-problem('003', id, '003').
:-problem('003', fracas_answer, "yes").
:-problem('003', p, "All Italian men want to be a great tenor.").
:-problem('003', p, "Some Italian men are great tenors.").
:-problem('003', q, "Are there Italian men who want to be a great tenor?").
:-problem('003', h, "There are Italian men who want to be a great tenor.").
:-problem('003', a, "Yes").
:-note(problem, ' Note that second premise is unnecessary and irrelevant. ').


:-switch_test('004', problem, [], ['fracas-problems']).
:-problem('004', id, '004').
:-problem('004', fracas_answer, "yes").
:-problem('004', p, "Each Italian tenor wants to be great.").
:-problem('004', p, "Some Italian tenors are great.").
:-problem('004', q, "Are there Italian tenors who want to be great?").
:-problem('004', h, "There are Italian tenors who want to be great.").
:-problem('004', a, "Yes").
:-note(problem, ' Note that second premise is unnecessary and irrelevant. ').


:-switch_test('005', problem, [], ['fracas-problems']).
:-problem('005', id, '005').
:-problem('005', fracas_answer, "yes").
:-problem('005', p, "The really ambitious tenors are Italian.").
:-problem('005', q, "Are there really ambitious tenors who are Italian?").
:-problem('005', h, "There are really ambitious tenors who are Italian.").
:-problem('005', a, "Yes").


:-switch_test('006', problem, [], ['fracas-problems']).
:-problem('006', id, '006').
:-problem('006', fracas_answer, "no").
:-problem('006', p, "No really great tenors are modest.").
:-problem('006', q, "Are there really great tenors who are modest?").
:-problem('006', h, "There are really great tenors who are modest.").
:-problem('006', a, "No").


:-switch_test('007', problem, [], ['fracas-problems']).
:-problem('007', id, '007').
:-problem('007', fracas_answer, "yes").
:-problem('007', p, "Some great tenors are Swedish.").
:-problem('007', q, "Are there great tenors who are Swedish?").
:-problem('007', h, "There are great tenors who are Swedish.").
:-problem('007', a, "Yes").


:-switch_test('008', problem, [], ['fracas-problems']).
:-problem('008', id, '008').
:-problem('008', fracas_answer, "yes").
:-problem('008', p, "Many great tenors are German.").
:-problem('008', q, "Are there great tenors who are German?").
:-problem('008', h, "There are great tenors who are German.").
:-problem('008', a, "Yes").


:-switch_test('009', problem, [], ['fracas-problems']).
:-problem('009', id, '009').
:-problem('009', fracas_answer, "yes").
:-problem('009', p, "Several great tenors are British.").
:-problem('009', q, "Are there great tenors who are British?").
:-problem('009', h, "There are great tenors who are British.").
:-problem('009', a, "Yes").


:-switch_test('010', problem, [], ['fracas-problems']).
:-problem('010', id, '010').
:-problem('010', fracas_answer, "yes").
:-problem('010', p, "Most great tenors are Italian.").
:-problem('010', q, "Are there great tenors who are Italian?").
:-problem('010', h, "There are great tenors who are Italian.").
:-problem('010', a, "Yes").


:-switch_test('011', problem, [], ['fracas-problems']).
:-problem('011', id, '011').
:-problem('011', fracas_answer, "yes").
:-problem('011', p, "A few great tenors sing popular music.").
:-problem('011', p, "Some great tenors like popular music.").
:-problem('011', q, "Are there great tenors who sing popular music?").
:-problem('011', h, "There are great tenors who sing popular music.").
:-problem('011', a, "Yes").
:-note(problem, ' Note that second premise is unnecessary and irrelevant. ').


:-switch_test('012', problem, [], ['fracas-problems']).
:-problem('012', id, '012').
:-problem('012', fracas_answer, "undef").
:-problem('012', fracas_nonstandard, "true").
:-problem('012', p, "Few great tenors are poor.").
:-problem('012', q, "Are there great tenors who are poor?").
:-problem('012', h, "There are great tenors who are poor.").
:-problem('012', a, "Not many").


:-switch_test('013', problem, [], ['fracas-problems']).
:-problem('013', id, '013').
:-problem('013', fracas_answer, "yes").
:-problem('013', p, "Both leading tenors are excellent.").
:-problem('013', p, "Leading tenors who are excellent are indispensable.").
:-problem('013', q, "Are both leading tenors indispensable?").
:-problem('013', h, "Both leading tenors are indispensable.").
:-problem('013', a, "Yes").


:-switch_test('014', problem, [], ['fracas-problems']).
:-problem('014', id, '014').
:-problem('014', fracas_answer, "no").
:-problem('014', p, "Neither leading tenor comes cheap.").
:-problem('014', p, "One of the leading tenors is Pavarotti.").
:-problem('014', q, "Is Pavarotti a leading tenor who comes cheap?").
:-problem('014', h, "Pavarotti is a leading tenor who comes cheap.").
:-problem('014', a, "No").


:-switch_test('015', problem, [], ['fracas-problems']).
:-problem('015', id, '015').
:-problem('015', fracas_answer, "yes").
:-problem('015', p, "At least three tenors will take part in the concert.").
:-problem('015', q, "Are there tenors who will take part in the concert?").
:-problem('015', h, "There are tenors who will take part in the concert.").
:-problem('015', a, "Yes").


:-switch_test('016', problem, [], ['fracas-problems']).
:-problem('016', id, '016').
:-problem('016', fracas_answer, "undef").
:-problem('016', fracas_nonstandard, "true").
:-problem('016', p, "At most two tenors will contribute their fees to charity.").
:-problem('016', q, "Are there tenors who will contribute their fees to charity?").
:-problem('016', h, "There are tenors who will contribute their fees to charity.").
:-problem('016', a, "At most two").


/*
 1.2 Monotonicity (upwards on second argument)

*/


/*
 Q As are Bs and all Bs are Cs < Q As are Cs

*/


:-switch_test('017', problem, [], ['fracas-problems']).
:-problem('017', id, '017').
:-problem('017', fracas_answer, "yes").
:-problem('017', p, "An Irishman won the Nobel prize for literature.").
:-problem('017', q, "Did an Irishman win a Nobel prize?").
:-problem('017', h, "An Irishman won a Nobel prize.").
:-problem('017', a, "Yes").


:-switch_test('018', problem, [], ['fracas-problems']).
:-problem('018', id, '018').
:-problem('018', fracas_answer, "yes").
:-problem('018', p, "Every European has the right to live in Europe.").
:-problem('018', p, "Every European is a person.").
:-problem('018', p, "Every person who has the right to live in Europe can travel freely within Europe.").
:-problem('018', q, "Can every European travel freely within Europe?").
:-problem('018', h, "Every European can travel freely within Europe.").
:-problem('018', a, "Yes").


:-switch_test('019', problem, [], ['fracas-problems']).
:-problem('019', id, '019').
:-problem('019', fracas_answer, "yes").
:-problem('019', p, "All Europeans have the right to live in Europe.").
:-problem('019', p, "Every European is a person.").
:-problem('019', p, "Every person who has the right to live in Europe can travel freely within Europe.").
:-problem('019', q, "Can all Europeans travel freely within Europe?").
:-problem('019', h, "All Europeans can travel freely within Europe.").
:-problem('019', a, "Yes").


:-switch_test('020', problem, [], ['fracas-problems']).
:-problem('020', id, '020').
:-problem('020', fracas_answer, "yes").
:-problem('020', p, "Each European has the right to live in Europe.").
:-problem('020', p, "Every European is a person.").
:-problem('020', p, "Every person who has the right to live in Europe can travel freely within Europe.").
:-problem('020', q, "Can each European travel freely within Europe?").
:-problem('020', h, "Each European can travel freely within Europe.").
:-problem('020', a, "Yes").


:-switch_test('021', problem, [], ['fracas-problems']).
:-problem('021', id, '021').
:-problem('021', fracas_answer, "yes").
:-problem('021', p, "The residents of member states have the right to live in Europe.").
:-problem('021', p, "All residents of member states are individuals.").
:-problem('021', p, "Every individual who has the right to live in Europe can travel freely within Europe.").
:-problem('021', q, "Can the residents of member states travel freely within Europe?").
:-problem('021', h, "The residents of member states can travel freely within Europe.").
:-problem('021', a, "Yes").


:-switch_test('022', problem, [], ['fracas-problems']).
:-problem('022', id, '022').
:-problem('022', fracas_answer, "unknown").
:-problem('022', p, "No delegate finished the report on time.").
:-problem('022', q, "Did no delegate finish the report?").
:-problem('022', h, "No delegate finished the report.").
:-problem('022', a, "Don't know").
:-problem('022', why, "can't drop adjunct in negative context").


:-switch_test('023', problem, [], ['fracas-problems']).
:-problem('023', id, '023').
:-problem('023', fracas_answer, "yes").
:-problem('023', p, "Some delegates finished the survey on time.").
:-problem('023', q, "Did some delegates finish the survey?").
:-problem('023', h, "Some delegates finished the survey.").
:-problem('023', a, "Yes").
:-problem('023', why, "OK to drop adjunct in positive context").


:-switch_test('024', problem, [], ['fracas-problems']).
:-problem('024', id, '024').
:-problem('024', fracas_answer, "yes").
:-problem('024', p, "Many delegates obtained interesting results from the survey.").
:-problem('024', q, "Did many delegates obtain results from the survey?").
:-problem('024', h, "Many delegates obtained results from the survey.").
:-problem('024', a, "Yes").
:-problem('024', why, "OK to drop adjunct in positive context").


:-switch_test('025', problem, [], ['fracas-problems']).
:-problem('025', id, '025').
:-problem('025', fracas_answer, "yes").
:-problem('025', p, "Several delegates got the results published in major national newspapers.").
:-problem('025', q, "Did several delegates get the results published?").
:-problem('025', h, "Several delegates got the results published.").
:-problem('025', a, "Yes").
:-problem('025', why, "OK to drop adjunct in positive context").


:-switch_test('026', problem, [], ['fracas-problems']).
:-problem('026', id, '026').
:-problem('026', fracas_answer, "yes").
:-problem('026', p, "Most Europeans are resident in Europe.").
:-problem('026', p, "All Europeans are people.").
:-problem('026', p, "All people who are resident in Europe can travel freely within Europe.").
:-problem('026', q, "Can most Europeans travel freely within Europe?").
:-problem('026', h, "Most Europeans can travel freely within Europe.").
:-problem('026', a, "Yes").


:-switch_test('027', problem, [], ['fracas-problems']).
:-problem('027', id, '027').
:-problem('027', fracas_answer, "yes").
:-problem('027', p, "A few committee members are from Sweden.").
:-problem('027', p, "All committee members are people.").
:-problem('027', p, "All people who are from Sweden are from Scandinavia.").
:-problem('027', q, "Are at least a few committee members from Scandinavia?").
:-problem('027', h, "At least a few committee members are from Scandinavia.").
:-problem('027', a, "Yes").


:-switch_test('028', problem, [], ['fracas-problems']).
:-problem('028', id, '028').
:-problem('028', fracas_answer, "unknown").
:-problem('028', p, "Few committee members are from Portugal.").
:-problem('028', p, "All committee members are people.").
:-problem('028', p, "All people who are from Portugal are from southern Europe.").
:-problem('028', q, "Are there few committee members from southern Europe?").
:-problem('028', h, "There are few committee members from southern Europe.").
:-problem('028', a, "Don't know").


:-switch_test('029', problem, [], ['fracas-problems']).
:-problem('029', id, '029').
:-problem('029', fracas_answer, "yes").
:-problem('029', p, "Both commissioners used to be leading businessmen.").
:-problem('029', q, "Did both commissioners used to be businessmen?").
:-problem('029', h, "Both commissioners used to be businessmen.").
:-problem('029', a, "Yes").


:-switch_test('030', problem, [], ['fracas-problems']).
:-problem('030', id, '030').
:-problem('030', fracas_answer, "unknown").
:-problem('030', p, "Neither commissioner spends a lot of time at home.").
:-problem('030', q, "Does neither commissioner spend time at home?").
:-problem('030', h, "Neither commissioner spends time at home.").
:-problem('030', a, "Don't know").
:-problem('030', why, "Not OK to drop [qualifier] in negative context").


:-switch_test('031', problem, [], ['fracas-problems']).
:-problem('031', id, '031').
:-problem('031', fracas_answer, "yes").
:-problem('031', p, "At least three commissioners spend a lot of time at home.").
:-problem('031', q, "Do at least three commissioners spend time at home?").
:-problem('031', h, "At least three commissioners spend time at home.").
:-problem('031', a, "Yes").
:-note(problem, 'There was a typographical error in original problem').
:-note(problem, 'The premise was "A least three..." ').


:-switch_test('032', problem, [], ['fracas-problems']).
:-problem('032', id, '032').
:-problem('032', fracas_answer, "unknown").
:-problem('032', p, "At most ten commissioners spend a lot of time at home.").
:-problem('032', q, "Do at most ten commissioners spend time at home?").
:-problem('032', h, "At most ten commissioners spend time at home.").
:-problem('032', a, "Don't know").


/*
 1.3 Monotonicity (downwards on second argument)

*/


/*
 Q As are Bs and all Cs are Bs < Q As are Cs

*/


:-switch_test('033', problem, [], ['fracas-problems']).
:-problem('033', id, '033').
:-problem('033', fracas_answer, "unknown").
:-problem('033', p, "An Irishman won a Nobel prize.").
:-problem('033', q, "Did an Irishman win the Nobel prize for literature?").
:-problem('033', h, "An Irishman won the Nobel prize for literature.").
:-problem('033', a, "Don't know").


:-switch_test('034', problem, [], ['fracas-problems']).
:-problem('034', id, '034').
:-problem('034', fracas_answer, "unknown").
:-problem('034', p, "Every European can travel freely within Europe.").
:-problem('034', p, "Every European is a person.").
:-problem('034', p, "Every person who has the right to live in Europe can travel freely within Europe.").
:-problem('034', q, "Does every European have the right to live in Europe?").
:-problem('034', h, "Every European has the right to live in Europe.").
:-problem('034', a, "Don't know").


:-switch_test('035', problem, [], ['fracas-problems']).
:-problem('035', id, '035').
:-problem('035', fracas_answer, "unknown").
:-problem('035', p, "All Europeans can travel freely within Europe.").
:-problem('035', p, "Every European is a person.").
:-problem('035', p, "Every person who has the right to live in Europe can travel freely within Europe.").
:-problem('035', q, "Do all Europeans have the right to live in Europe?").
:-problem('035', h, "All Europeans have the right to live in Europe.").
:-problem('035', a, "Don't know").


:-switch_test('036', problem, [], ['fracas-problems']).
:-problem('036', id, '036').
:-problem('036', fracas_answer, "unknown").
:-problem('036', p, "Each European can travel freely within Europe.").
:-problem('036', p, "Every European is a person.").
:-problem('036', p, "Every person who has the right to live in Europe can travel freely within Europe.").
:-problem('036', q, "Does each European have the right to live in Europe?").
:-problem('036', h, "Each European has the right to live in Europe.").
:-problem('036', a, "Don't know").


:-switch_test('037', problem, [], ['fracas-problems']).
:-problem('037', id, '037').
:-problem('037', fracas_answer, "unknown").
:-problem('037', p, "The residents of member states can travel freely within Europe.").
:-problem('037', p, "All residents of member states are individuals.").
:-problem('037', p, "Every individual who has the right to live anywhere in Europe can travel freely within Europe.").
:-problem('037', q, "Do the residents of member states have the right to live anywhere in Europe?").
:-problem('037', h, "The residents of member states have the right to live anywhere in Europe.").
:-problem('037', a, "Don't know").


:-switch_test('038', problem, [], ['fracas-problems']).
:-problem('038', id, '038').
:-problem('038', fracas_answer, "no").
:-problem('038', p, "No delegate finished the report.").
:-problem('038', q, "Did any delegate finish the report on time?").
:-problem('038', h, "Some delegate finished the report on time.").
:-problem('038', a, "No").


:-switch_test('039', problem, [], ['fracas-problems']).
:-problem('039', id, '039').
:-problem('039', fracas_answer, "unknown").
:-problem('039', p, "Some delegates finished the survey.").
:-problem('039', q, "Did some delegates finish the survey on time?").
:-problem('039', h, "Some delegates finished the survey on time.").
:-problem('039', a, "Don't know").


:-switch_test('040', problem, [], ['fracas-problems']).
:-problem('040', id, '040').
:-problem('040', fracas_answer, "unknown").
:-problem('040', p, "Many delegates obtained results from the survey.").
:-problem('040', q, "Did many delegates obtain interesting results from the survey?").
:-problem('040', h, "Many delegates obtained interesting results from the survey.").
:-problem('040', a, "Don't know").


:-switch_test('041', problem, [], ['fracas-problems']).
:-problem('041', id, '041').
:-problem('041', fracas_answer, "unknown").
:-problem('041', p, "Several delegates got the results published.").
:-problem('041', q, "Did several delegates get the results published in major national newspapers?").
:-problem('041', h, "Several delegates got the results published in major national newspapers.").
:-problem('041', a, "Don't know").


:-switch_test('042', problem, [], ['fracas-problems']).
:-problem('042', id, '042').
:-problem('042', fracas_answer, "unknown").
:-problem('042', p, "Most Europeans can travel freely within Europe.").
:-problem('042', p, "All Europeans are people.").
:-problem('042', p, "All people who are resident in Europe can travel freely within Europe.").
:-problem('042', q, "Are most Europeans resident in Europe?").
:-problem('042', h, "Most Europeans are resident in Europe.").
:-problem('042', a, "Don't know").


:-switch_test('043', problem, [], ['fracas-problems']).
:-problem('043', id, '043').
:-problem('043', fracas_answer, "unknown").
:-problem('043', p, "A few committee members are from Scandinavia.").
:-problem('043', p, "All committee members are people.").
:-problem('043', p, "All people who are from Sweden are from Scandinavia.").
:-problem('043', q, "Are at least a few committee members from Sweden?").
:-problem('043', h, "At least a few committee members are from Sweden.").
:-problem('043', a, "Don't know").


:-switch_test('044', problem, [], ['fracas-problems']).
:-problem('044', id, '044').
:-problem('044', fracas_answer, "yes").
:-problem('044', p, "Few committee members are from southern Europe.").
:-problem('044', p, "All committee members are people.").
:-problem('044', p, "All people who are from Portugal are from southern Europe.").
:-problem('044', q, "Are there few committee members from Portugal?").
:-problem('044', h, "There are few committee members from Portugal.").
:-problem('044', a, "Yes").


:-switch_test('045', problem, [], ['fracas-problems']).
:-problem('045', id, '045').
:-problem('045', fracas_answer, "unknown").
:-problem('045', p, "Both commissioners used to be businessmen.").
:-problem('045', q, "Did both commissioners used to be leading businessmen?").
:-problem('045', h, "Both commissioners used to be leading businessmen.").
:-problem('045', a, "Don't know").


:-switch_test('046', problem, [], ['fracas-problems']).
:-problem('046', id, '046').
:-problem('046', fracas_answer, "no").
:-problem('046', p, "Neither commissioner spends time at home.").
:-problem('046', q, "Does either commissioner spend a lot of time at home?").
:-problem('046', h, "One of the commissioners spends a lot of time at home.").
:-problem('046', a, "No").


:-switch_test('047', problem, [], ['fracas-problems']).
:-problem('047', id, '047').
:-problem('047', fracas_answer, "unknown").
:-problem('047', p, "At least three commissioners spend time at home.").
:-problem('047', q, "Do at least three commissioners spend a lot of time at home?").
:-problem('047', h, "At least three commissioners spend a lot of time at home.").
:-problem('047', a, "Don't know").
:-note(problem, 'Was a typo in original problem').
:-note(problem, 'The premise said "A least three..." ').


:-switch_test('048', problem, [], ['fracas-problems']).
:-problem('048', id, '048').
:-problem('048', fracas_answer, "yes").
:-problem('048', p, "At most ten commissioners spend time at home.").
:-problem('048', q, "Do at most ten commissioners spend a lot of time at home?").
:-problem('048', h, "At most ten commissioners spend a lot of time at home.").
:-problem('048', a, "Yes").


/*
 1.4 Monotonicity (upwards on first argument)

*/


/*
 Q As are Bs and all As are Cs < Q Cs are Bs

*/


:-switch_test('049', problem, [], ['fracas-problems']).
:-problem('049', id, '049').
:-problem('049', fracas_answer, "yes").
:-problem('049', p, "A Swede won a Nobel prize.").
:-problem('049', p, "Every Swede is a Scandinavian.").
:-problem('049', q, "Did a Scandinavian win a Nobel prize?").
:-problem('049', h, "A Scandinavian won a Nobel prize.").
:-problem('049', a, "Yes").


:-switch_test('050', problem, [], ['fracas-problems']).
:-problem('050', id, '050').
:-problem('050', fracas_answer, "unknown").
:-problem('050', p, "Every Canadian resident can travel freely within Europe.").
:-problem('050', p, "Every Canadian resident is a resident of the North American continent.").
:-problem('050', q, "Can every resident of the North American continent travel freely within Europe?").
:-problem('050', h, "Every resident of the North American continent can travel freely within Europe.").
:-problem('050', a, "Don't know").


:-switch_test('051', problem, [], ['fracas-problems']).
:-problem('051', id, '051').
:-problem('051', fracas_answer, "unknown").
:-problem('051', p, "All Canadian residents can travel freely within Europe.").
:-problem('051', p, "Every Canadian resident is a resident of the North American continent.").
:-problem('051', q, "Can all residents of the North American continent travel freely within Europe?").
:-problem('051', h, "All residents of the North American continent can travel freely within Europe.").
:-problem('051', a, "Don't know").


:-switch_test('052', problem, [], ['fracas-problems']).
:-problem('052', id, '052').
:-problem('052', fracas_answer, "unknown").
:-problem('052', p, "Each Canadian resident can travel freely within Europe.").
:-problem('052', p, "Every Canadian resident is a resident of the North American continent.").
:-problem('052', q, "Can each resident of the North American continent travel freely within Europe?").
:-problem('052', h, "Each resident of the North American continent can travel freely within Europe.").
:-problem('052', a, "Don't know").


:-switch_test('053', problem, [], ['fracas-problems']).
:-problem('053', id, '053').
:-problem('053', fracas_answer, "unknown").
:-problem('053', p, "The residents of major western countries can travel freely within Europe.").
:-problem('053', p, "All residents of major western countries are residents of western countries.").
:-problem('053', q, "Do the residents of western countries have the right to live in Europe?").
:-problem('053', h, "The residents of western countries have the right to live in Europe.").
:-problem('053', a, "Don't know").


:-switch_test('054', problem, [], ['fracas-problems']).
:-problem('054', id, '054').
:-problem('054', fracas_answer, "unknown").
:-problem('054', p, "No Scandinavian delegate finished the report on time.").
:-problem('054', q, "Did any delegate finish the report on time?").
:-problem('054', h, "Some delegate finished the report on time.").
:-problem('054', a, "Don't know").


:-switch_test('055', problem, [], ['fracas-problems']).
:-problem('055', id, '055').
:-problem('055', fracas_answer, "yes").
:-problem('055', p, "Some Irish delegates finished the survey on time.").
:-problem('055', q, "Did any delegates finish the survey on time?").
:-problem('055', h, "Some delegates finished the survey on time.").
:-problem('055', a, "Yes").


:-switch_test('056', problem, [], ['fracas-problems']).
:-problem('056', id, '056').
:-problem('056', fracas_answer, "unknown").
:-problem('056', p, "Many British delegates obtained interesting results from the survey.").
:-problem('056', q, "Did many delegates obtain interesting results from the survey?").
:-problem('056', h, "Many delegates obtained interesting results from the survey.").
:-problem('056', a, "Don't know").
:-note(problem, 'This answer seems dubious to me.').  
:-note(problem, 'Apparently the FraCaS people interpret "many" as denoting a large proportion, whereas I interpret it as denoting a large absolute number.').
:-note(problem, 'That is, if "many" is regarded as a binary generalized quantifier, they want to say it\'s non-monotone in the first argument (and upward-monotone in the second), whereas I would say it\'s upward-monotone in both arguments.').
:-note(problem, 'Note that this problem is exactly the inverse of problem fracas-72.\n  ').


:-switch_test('057', problem, [], ['fracas-problems']).
:-problem('057', id, '057').
:-problem('057', fracas_answer, "yes").
:-problem('057', p, "Several Portuguese delegates got the results published in major national newspapers.").
:-problem('057', q, "Did several delegates get the results published in major national newspapers?").
:-problem('057', h, "Several delegates got the results published in major national newspapers.").
:-problem('057', a, "Yes").


:-switch_test('058', problem, [], ['fracas-problems']).
:-problem('058', id, '058').
:-problem('058', fracas_answer, "unknown").
:-problem('058', p, "Most Europeans who are resident in Europe can travel freely within Europe.").
:-problem('058', q, "Can most Europeans travel freely within Europe?").
:-problem('058', h, "Most Europeans can travel freely within Europe.").
:-problem('058', a, "Don't know").


:-switch_test('059', problem, [], ['fracas-problems']).
:-problem('059', id, '059').
:-problem('059', fracas_answer, "yes").
:-problem('059', p, "A few female committee members are from Scandinavia.").
:-problem('059', q, "Are at least a few committee members from Scandinavia?").
:-problem('059', h, "At least a few committee members are from Scandinavia.").
:-problem('059', a, "Yes").


:-switch_test('060', problem, [], ['fracas-problems']).
:-problem('060', id, '060').
:-problem('060', fracas_answer, "unknown").
:-problem('060', p, "Few female committee members are from southern Europe.").
:-problem('060', q, "Are few committee members from southern Europe?").
:-problem('060', h, "Few committee members are from southern Europe.").
:-problem('060', a, "Don't know").
:-note(problem, ' Note that this problem is exactly the inverse of problem fracas-76.\n  ').


:-switch_test('061', problem, [], ['fracas-problems']).
:-problem('061', id, '061').
:-problem('061', fracas_answer, "undef").
:-problem('061', fracas_nonstandard, "true").
:-problem('061', p, "Both female commissioners used to be in business.").
:-problem('061', q, "Did both commissioners used to be in business?").
:-problem('061', h, "Both commissioners used to be in business.").
:-problem('061', a, "Yes, if both commissioners are female; otherwise there are more than two commissioners.").


:-switch_test('062', problem, [], ['fracas-problems']).
:-problem('062', id, '062').
:-problem('062', fracas_answer, "undef").
:-problem('062', fracas_nonstandard, "true").
:-problem('062', p, "Neither female commissioner spends a lot of time at home.").
:-problem('062', q, "Does either commissioner spend a lot of time at home?").
:-problem('062', h, "One of the commissioners spends a lot of time at home.").
:-problem('062', a, "No, if both commissioners are female; otherwise there are more than two commissioners.").


:-switch_test('063', problem, [], ['fracas-problems']).
:-problem('063', id, '063').
:-problem('063', fracas_answer, "yes").
:-problem('063', p, "At least three female commissioners spend time at home.").
:-problem('063', q, "Do at least three commissioners spend time at home?").
:-problem('063', h, "At least three commissioners spend time at home.").
:-problem('063', a, "Yes").
:-note(problem, ' Typo in original problem: premise was "A least three..." ').


:-switch_test('064', problem, [], ['fracas-problems']).
:-problem('064', id, '064').
:-problem('064', fracas_answer, "unknown").
:-problem('064', p, "At most ten female commissioners spend time at home.").
:-problem('064', q, "Do at most ten commissioners spend time at home?").
:-problem('064', h, "At most ten commissioners spend time at home.").
:-problem('064', a, "Don't know").


/*
 1.5 Monotonicity (downwards on first argument)

*/


/*
 Q As are Bs and all Cs are As < Q Cs are Bs

*/


:-switch_test('065', problem, [], ['fracas-problems']).
:-problem('065', id, '065').
:-problem('065', fracas_answer, "unknown").
:-problem('065', p, "A Scandinavian won a Nobel prize.").
:-problem('065', p, "Every Swede is a Scandinavian.").
:-problem('065', q, "Did a Swede win a Nobel prize?").
:-problem('065', h, "A Swede won a Nobel prize.").
:-problem('065', a, "Don't know").


:-switch_test('066', problem, [], ['fracas-problems']).
:-problem('066', id, '066').
:-problem('066', fracas_answer, "yes").
:-problem('066', p, "Every resident of the North American continent can travel freely within Europe.").
:-problem('066', p, "Every Canadian resident is a resident of the North American continent.").
:-problem('066', q, "Can every Canadian resident travel freely within Europe?").
:-problem('066', h, "Every Canadian resident can travel freely within Europe.").
:-problem('066', a, "Yes").
:-note(problem, ' NB: in the original, "travel" was missing from the question. ').


:-switch_test('067', problem, [], ['fracas-problems']).
:-problem('067', id, '067').
:-problem('067', fracas_answer, "yes").
:-problem('067', p, "All residents of the North American continent can travel freely within Europe.").
:-problem('067', p, "Every Canadian resident is a resident of the North American continent.").
:-problem('067', q, "Can all Canadian residents travel freely within Europe?").
:-problem('067', h, "All Canadian residents can travel freely within Europe.").
:-problem('067', a, "Yes").


:-switch_test('068', problem, [], ['fracas-problems']).
:-problem('068', id, '068').
:-problem('068', fracas_answer, "yes").
:-problem('068', p, "Each resident of the North American continent can travel freely within Europe.").
:-problem('068', p, "Every Canadian resident is a resident of the North American continent.").
:-problem('068', q, "Can each Canadian resident travel freely within Europe?").
:-problem('068', h, "Each Canadian resident can travel freely within Europe.").
:-problem('068', a, "Yes").


:-switch_test('069', problem, [], ['fracas-problems']).
:-problem('069', id, '069').
:-problem('069', fracas_answer, "yes").
:-problem('069', p, "The residents of western countries can travel freely within Europe.").
:-problem('069', p, "All residents of major western countries are residents of western countries.").
:-problem('069', q, "Do the residents of major western countries have the right to live in Europe?").
:-problem('069', h, "The residents of major western countries have the right to live in Europe.").
:-problem('069', a, "Yes").


:-switch_test('070', problem, [], ['fracas-problems']).
:-problem('070', id, '070').
:-problem('070', fracas_answer, "no").
:-problem('070', p, "No delegate finished the report on time.").
:-problem('070', q, "Did any Scandinavian delegate finish the report on time?").
:-problem('070', h, "Some Scandinavian delegate finished the report on time.").
:-problem('070', a, "No").


:-switch_test('071', problem, [], ['fracas-problems']).
:-problem('071', id, '071').
:-problem('071', fracas_answer, "unknown").
:-problem('071', p, "Some delegates finished the survey on time.").
:-problem('071', q, "Did any Irish delegates finish the survey on time?").
:-problem('071', h, "Some Irish delegates finished the survey on time.").
:-problem('071', a, "Don't know").


:-switch_test('072', problem, [], ['fracas-problems']).
:-problem('072', id, '072').
:-problem('072', fracas_answer, "unknown").
:-problem('072', p, "Many delegates obtained interesting results from the survey.").
:-problem('072', q, "Did many British delegates obtain interesting results from the survey?").
:-problem('072', h, "Many British delegates obtained interesting results from the survey.").
:-problem('072', a, "Don't know").
:-note(problem, ' Note that this problem is exactly the inverse of problem fracas-56.\n  ').


:-switch_test('073', problem, [], ['fracas-problems']).
:-problem('073', id, '073').
:-problem('073', fracas_answer, "unknown").
:-problem('073', p, "Several delegates got the results published in major national newspapers.").
:-problem('073', q, "Did several Portuguese delegates get the results published in major national newspapers?").
:-problem('073', h, "Several Portuguese delegates got the results published in major national newspapers.").
:-problem('073', a, "Don't know").


:-switch_test('074', problem, [], ['fracas-problems']).
:-problem('074', id, '074').
:-problem('074', fracas_answer, "unknown").
:-problem('074', p, "Most Europeans can travel freely within Europe.").
:-problem('074', q, "Can most Europeans who are resident outside Europe travel freely within Europe?").
:-problem('074', h, "Most Europeans who are resident outside Europe can travel freely within Europe.").
:-problem('074', a, "Don't know").


:-switch_test('075', problem, [], ['fracas-problems']).
:-problem('075', id, '075').
:-problem('075', fracas_answer, "unknown").
:-problem('075', p, "A few committee members are from Scandinavia.").
:-problem('075', q, "Are at least a few female committee members from Scandinavia?").
:-problem('075', h, "At least a few female committee members are from Scandinavia.").
:-problem('075', a, "Don't know").


:-switch_test('076', problem, [], ['fracas-problems']).
:-problem('076', id, '076').
:-problem('076', fracas_answer, "yes").
:-problem('076', p, "Few committee members are from southern Europe.").
:-problem('076', q, "Are few female committee members from southern Europe?").
:-problem('076', h, "Few female committee members are from southern Europe.").
:-problem('076', a, "Yes").
:-note(problem, ' Note that this problem is exactly the inverse of problem fracas-60.  If the answer is "yes", then apparently the FraCaS people interpret "few" as denoting a small absolute number, rather than a small proportion.  That is,  if "few" is regarded as a binary generalized quantifier, they want to say it\'s downward-monotone in the first argument, rather than non-monotone. Contrast this with the treatment of "many" in fracas-56 and fracas-72, which is apparently interpreted to be non-monotone in its first argument.\n  ').


:-switch_test('077', problem, [], ['fracas-problems']).
:-problem('077', id, '077').
:-problem('077', fracas_answer, "undef").
:-problem('077', fracas_nonstandard, "true").
:-problem('077', p, "Both commissioners used to be in business.").
:-problem('077', q, "Did both female commissioners used to be in business?").
:-problem('077', h, "Both female commissioners used to be in business.").
:-problem('077', a, "Yes, if both commissioners are female; otherwise there are more than two commissioners.").


:-switch_test('078', problem, [], ['fracas-problems']).
:-problem('078', id, '078').
:-problem('078', fracas_answer, "undef").
:-problem('078', fracas_nonstandard, "true").
:-problem('078', p, "Neither commissioner spends a lot of time at home.").
:-problem('078', q, "Does either female commissioner spend a lot of time at home?").
:-problem('078', h, "One of the female commissioners spends a lot of time at home.").
:-problem('078', a, "No, if both commissioners are female; otherwise there are more than two commissioners.").


:-switch_test('079', problem, [], ['fracas-problems']).
:-problem('079', id, '079').
:-problem('079', fracas_answer, "unknown").
:-problem('079', p, "At least three commissioners spend time at home.").
:-problem('079', q, "Do at least three male commissioners spend time at home?").
:-problem('079', h, "At least three male commissioners spend time at home.").
:-problem('079', a, "Don't know").
:-note(problem, 'The premise said "A least three..." in the original problem').


:-switch_test('080', problem, [], ['fracas-problems']).
:-problem('080', id, '080').
:-problem('080', fracas_answer, "yes").
:-problem('080', p, "At most ten commissioners spend time at home.").
:-problem('080', q, "Do at most ten female commissioners spend time at home?").
:-problem('080', h, "At most ten female commissioners spend time at home.").
:-problem('080', a, "Yes").


/*
 2 PLURALS

*/


/*

  A number of inferences pertaining to plurals are covered under the headings of
  generalized quantifiers and elsewhere. Here we concentrate on conjoined NPs;
  bare, existential and definite plurals; dependent plurals; and collective and
  distributive readings and scope ambiguity.


*/


/*
 2.1 Conjoined Noun Phrases

*/


:-switch_test('081', problem, [], ['fracas-problems']).
:-problem('081', id, '081').
:-problem('081', fracas_answer, "yes").
:-problem('081', p, "Smith, Jones and Anderson signed the contract.").
:-problem('081', q, "Did Jones sign the contract?").
:-problem('081', h, "Jones signed the contract.").
:-problem('081', a, "Yes").


:-switch_test('082', problem, [], ['fracas-problems']).
:-problem('082', id, '082').
:-problem('082', fracas_answer, "yes").
:-problem('082', p, "Smith, Jones and several lawyers signed the contract.").
:-problem('082', q, "Did Jones sign the contract?").
:-problem('082', h, "Jones signed the contract.").
:-problem('082', a, "Yes").


:-switch_test('083', problem, [], ['fracas-problems']).
:-problem('083', id, '083').
:-problem('083', fracas_answer, "unknown").
:-problem('083', p, "Either Smith, Jones or Anderson signed the contract.").
:-problem('083', q, "Did Jones sign the contract?").
:-problem('083', h, "Jones signed the contract.").
:-problem('083', a, "Don't know").


:-switch_test('084', problem, [], ['fracas-problems']).
:-problem('084', id, '084').
:-problem('084', fracas_answer, "yes").
:-problem('084', p, "Either Smith, Jones or Anderson signed the contract.").
:-problem('084', q, "If Smith and Anderson did not sign the contract, did Jones sign the contract?").
:-problem('084', h, "If Smith and Anderson did not sign the contract, Jones signed the contract.").
:-problem('084', a, "Yes").


:-switch_test('085', problem, [], ['fracas-problems']).
:-problem('085', id, '085').
:-problem('085', fracas_answer, "no").
:-problem('085', p, "Exactly two lawyers and three accountants signed the contract.").
:-problem('085', q, "Did six lawyers sign the contract?").
:-problem('085', h, "Six lawyers signed the contract.").
:-problem('085', a, "No").
:-problem('085', why, "No scope relations between the two conjoined NPs.").


:-switch_test('086', problem, [], ['fracas-problems']).
:-problem('086', id, '086').
:-problem('086', fracas_answer, "no").
:-problem('086', p, "Exactly two lawyers and three accountants signed the contract.").
:-problem('086', q, "Did six accountants sign the contract?").
:-problem('086', h, "Six accountants signed the contract.").
:-problem('086', a, "No").
:-problem('086', why, "No scope relations between the two conjoined NPs.").


/*
 Conjoined Nbars

*/


/*

  Nbar conjunction tends to be quite ambiguous. This may be the result of a
  syntactic ambiguity between (i) genuine Nbar conjunction, and (ii) NP
  conjunction where the determiner of one of the NPs is ellided.


*/


:-switch_test('087', problem, [], ['fracas-problems']).
:-problem('087', id, '087').
:-problem('087', fracas_answer, "yes").
:-problem('087', fracas_nonstandard, "true").
:-problem('087', p, "Every representative and client was at the meeting.").
:-problem('087', q, "Was every representative at the meeting?").
:-problem('087', h, "Every representative was at the meeting.").
:-problem('087', a, "Yes, on one reading").
:-problem('087', why, "Arguably NP conjunction: every representative and every client.").


:-switch_test('088', problem, [], ['fracas-problems']).
:-problem('088', id, '088').
:-problem('088', fracas_answer, "unknown").
:-problem('088', fracas_nonstandard, "true").
:-problem('088', p, "Every representative and client was at the meeting.").
:-problem('088', q, "Was every representative at the meeting?").
:-problem('088', h, "Every representative was at the meeting.").
:-problem('088', a, "Don't know, on one reading").
:-problem('088', why, "NBar conjunction: everyone who is both a representative and a client.").
:-note(problem, ' Note that this is formally identical with preceding. ').


:-switch_test('089', problem, [], ['fracas-problems']).
:-problem('089', id, '089').
:-problem('089', fracas_answer, "yes").
:-problem('089', p, "Every representative or client was at the meeting.").
:-problem('089', q, "Was every representative and every client at the meeting?").
:-problem('089', h, "Every representative and every client was at the meeting.").
:-problem('089', a, "Yes").
:-problem('089', why, "With disjunction, NP conjunction seems unavailable.").


/*
 2.2 Definite Plurals

*/


/*

  Definite plurals can often be non-anaphoric and behave like universally
  quantified noun phrases (90). However, as with (generic) bare plurals, the
  force of the quantification can also be less than universal (91). Whether this
  lessening of quantificational force is due to the noun phrase or to the
  predicate of which the NP is an argument is unclear (92, 93).


*/


:-switch_test('090', problem, [], ['fracas-problems']).
:-problem('090', id, '090').
:-problem('090', fracas_answer, "yes").
:-problem('090', p, "The chairman read out the items on the agenda.").
:-problem('090', q, "Did the chairman read out every item on the agenda?").
:-problem('090', h, "The chairman read out every item on the agenda.").
:-problem('090', a, "Yes").


/*
 Non-anaphoric, universal plural definite

*/


:-switch_test('091', problem, [], ['fracas-problems']).
:-problem('091', id, '091').
:-problem('091', fracas_answer, "unknown").
:-problem('091', p, "The people who were at the meeting voted for a new chairman.").
:-problem('091', q, "Did everyone at the meeting vote for a new chairman?").
:-problem('091', h, "Everyone at the meeting voted for a new chairman.").
:-problem('091', a, "Don't know").
:-problem('091', why, "Some people may have abstained from the vote").


:-switch_test('092', problem, [], ['fracas-problems']).
:-problem('092', id, '092').
:-problem('092', fracas_answer, "yes").
:-problem('092', p, "All the people who were at the meeting voted for a new chairman.").
:-problem('092', q, "Did everyone at the meeting vote for a new chairman?").
:-problem('092', h, "Everyone at the meeting voted for a new chairman.").
:-problem('092', a, "Yes").


:-switch_test('093', problem, [], ['fracas-problems']).
:-problem('093', id, '093').
:-problem('093', fracas_answer, "yes").
:-problem('093', p, "The people who were at the meeting all voted for a new chairman.").
:-problem('093', q, "Did everyone at the meeting vote for a new chairman?").
:-problem('093', h, "Everyone at the meeting voted for a new chairman.").
:-problem('093', a, "Yes").


/*
 Closely related to this, plural definites can have a
collective/institutional or even generic interpretation:

*/


:-switch_test('094', problem, [], ['fracas-problems']).
:-problem('094', id, '094').
:-problem('094', fracas_answer, "unknown").
:-problem('094', p, "The inhabitants of Cambridge voted for a Labour MP.").
:-problem('094', q, "Did every inhabitant of Cambridge vote for a Labour MP?").
:-problem('094', h, "Every inhabitant of Cambridge voted for a Labour MP.").
:-problem('094', a, "Don't know").


:-switch_test('095', problem, [], ['fracas-problems']).
:-problem('095', id, '095').
:-problem('095', fracas_answer, "unknown").
:-problem('095', p, "The Ancient Greeks were noted philosophers.").
:-problem('095', q, "Was every Ancient Greek a noted philosopher?").
:-problem('095', h, "Every Ancient Greek was a noted philosopher.").
:-problem('095', a, "Don't know").


:-switch_test('096', problem, [], ['fracas-problems']).
:-problem('096', id, '096').
:-problem('096', fracas_answer, "yes").
:-problem('096', p, "The Ancient Greeks were all noted philosophers.").
:-problem('096', q, "Was every Ancient Greek a noted philosopher?").
:-problem('096', h, "Every Ancient Greek was a noted philosopher.").
:-problem('096', a, "Yes").


/*
 2.3 Bare Plurals

*/


/*
 Bare plurals can exhibit existential, (quasi) universal, generic or
dependent plural behaviour.

*/


/*
 The circumstances giving rise to these different behaviours a poorly
understood, so we only give a few illustrative examples.

*/


:-switch_test('097', problem, [], ['fracas-problems']).
:-problem('097', id, '097').
:-problem('097', fracas_answer, "yes").
:-problem('097', p, "Software faults were blamed for the system failure.").
:-problem('097', q, "Was the system failure blamed on one or more software faults?").
:-problem('097', h, "The system failure was blamed on one or more software faults.").
:-problem('097', a, "Yes").
:-problem('097', why, "Existential bare plural").


:-switch_test('098', problem, [], ['fracas-problems']).
:-problem('098', id, '098').
:-problem('098', fracas_answer, "unknown").
:-problem('098', p, "Software faults were blamed for the system failure.").
:-problem('098', p, "Bug # 32-985 is a known software fault.").
:-problem('098', q, "Was Bug # 32-985 blamed for the system failure?").
:-problem('098', h, "Bug # 32-985 was blamed for the system failure.").
:-problem('098', a, "Don't know").
:-problem('098', why, "Existential interpretation: not every software fault contributed to the failure.").


:-switch_test('099', problem, [], ['fracas-problems']).
:-problem('099', id, '099').
:-problem('099', fracas_answer, "yes").
:-problem('099', p, "Clients at the demonstration were all impressed by the system's performance.").
:-problem('099', p, "Smith was a client at the demonstration.").
:-problem('099', q, "Was Smith impressed by the system's performance?").
:-problem('099', h, "Smith was impressed by the system's performance.").
:-problem('099', a, "Yes").
:-problem('099', why, "(Quasi) universal bare plural").


:-switch_test('100', problem, [], ['fracas-problems']).
:-problem('100', id, '100').
:-problem('100', fracas_answer, "yes").
:-problem('100', p, "Clients at the demonstration were impressed by the system's performance.").
:-problem('100', q, "Were most clients at the demonstration impressed by the system's performance?").
:-problem('100', h, "Most clients at the demonstration were impressed by the system's performance.").
:-problem('100', a, "Yes").
:-problem('100', why, "(Quasi) universal bare plural").


:-switch_test('101', problem, [], ['fracas-problems']).
:-problem('101', id, '101').
:-problem('101', fracas_answer, "yes").
:-problem('101', p, "University graduates make poor stock-market traders.").
:-problem('101', p, "Smith is a university graduate.").
:-problem('101', q, "Is Smith likely to make a poor stock market trader?").
:-problem('101', h, "Smith is likely to make a poor stock market trader.").
:-problem('101', a, "Yes").
:-problem('101', why, "Generic interpretation").


:-switch_test('102', problem, [], ['fracas-problems']).
:-problem('102', id, '102').
:-problem('102', fracas_answer, "unknown").
:-problem('102', p, "University graduates make poor stock-market traders.").
:-problem('102', p, "Smith is a university graduate.").
:-problem('102', q, "Will Smith make a poor stock market trader?").
:-problem('102', h, "Smith will make a poor stock market trader.").
:-problem('102', a, "Don't know").
:-problem('102', why, "Generic interpretation").


/*
 2.4 Dependent Plurals

*/


:-switch_test('103', problem, [], ['fracas-problems']).
:-problem('103', id, '103').
:-problem('103', fracas_answer, "yes").
:-problem('103', p, "All APCOM managers have company cars.").
:-problem('103', p, "Jones is an APCOM manager.").
:-problem('103', q, "Does Jones have a company car?").
:-problem('103', h, "Jones has a company car.").
:-problem('103', a, "Yes").


:-switch_test('104', problem, [], ['fracas-problems']).
:-problem('104', id, '104').
:-problem('104', fracas_answer, "unknown").
:-problem('104', p, "All APCOM managers have company cars.").
:-problem('104', p, "Jones is an APCOM manager.").
:-problem('104', q, "Does Jones have more than one company car?").
:-problem('104', h, "Jones has more than one company car.").
:-problem('104', a, "Don't know").


/*
 2.5 Negated Plurals

*/


:-switch_test('105', problem, [], ['fracas-problems']).
:-problem('105', id, '105').
:-problem('105', fracas_answer, "no").
:-problem('105', p, "Just one accountant attended the meeting.").
:-problem('105', q, "Did no accountants attend the meeting?").
:-problem('105', h, "No accountants attended the meeting.").
:-problem('105', a, "No").


:-switch_test('106', problem, [], ['fracas-problems']).
:-problem('106', id, '106').
:-problem('106', fracas_answer, "no").
:-problem('106', p, "Just one accountant attended the meeting.").
:-problem('106', q, "Did no accountant attend the meeting?").
:-problem('106', h, "No accountant attended the meeting.").
:-problem('106', a, "No").


:-switch_test('107', problem, [], ['fracas-problems']).
:-problem('107', id, '107').
:-problem('107', fracas_answer, "yes").
:-problem('107', p, "Just one accountant attended the meeting.").
:-problem('107', q, "Did any accountants attend the meeting?").
:-problem('107', h, "Some accountants attended the meeting.").
:-problem('107', a, "Yes").


:-switch_test('108', problem, [], ['fracas-problems']).
:-problem('108', id, '108').
:-problem('108', fracas_answer, "yes").
:-problem('108', p, "Just one accountant attended the meeting.").
:-problem('108', q, "Did any accountant attend the meeting?").
:-problem('108', h, "Some accountant attended the meeting.").
:-problem('108', a, "Yes").


:-switch_test('109', problem, [], ['fracas-problems']).
:-problem('109', id, '109').
:-problem('109', fracas_answer, "no").
:-problem('109', fracas_nonstandard, "true").
:-problem('109', p, "Just one accountant attended the meeting.").
:-problem('109', q, "Did some accountants attend the meeting?").
:-problem('109', h, "Some accountants attended the meeting.").
:-problem('109', a, "No, just one").


:-switch_test('110', problem, [], ['fracas-problems']).
:-problem('110', id, '110').
:-problem('110', fracas_answer, "yes").
:-problem('110', p, "Just one accountant attended the meeting.").
:-problem('110', q, "Did some accountant attend the meeting?").
:-problem('110', h, "Some accountant attended the meeting.").
:-problem('110', a, "Yes").


/*
 2.6 Collective and Distributive Plurals

*/


:-switch_test('111', problem, [], ['fracas-problems']).
:-problem('111', id, '111').
:-problem('111', fracas_answer, "yes").
:-problem('111', fracas_nonstandard, "true").
:-problem('111', p, "Smith signed one contract.").
:-problem('111', p, "Jones signed another contract.").
:-problem('111', q, "Did Smith and Jones sign two contracts?").
:-problem('111', h, "Smith and Jones signed two contracts.").
:-problem('111', a, "Yes, on a collective/cumulative reading of the conclusion.").


:-switch_test('112', problem, [], ['fracas-problems']).
:-problem('112', id, '112').
:-problem('112', fracas_answer, "yes").
:-problem('112', fracas_nonstandard, "true").
:-problem('112', p, "Smith signed two contracts.").
:-problem('112', p, "Jones signed two contracts.").
:-problem('112', q, "Did Smith and Jones sign two contracts?").
:-problem('112', h, "Smith and Jones signed two contracts.").
:-problem('112', a, "Yes, on a distributive reading of \"Smith and Jones\".").


:-switch_test('113', problem, [], ['fracas-problems']).
:-problem('113', id, '113').
:-problem('113', fracas_answer, "yes").
:-problem('113', p, "Smith signed two contracts.").
:-problem('113', p, "Jones also signed them.").
:-problem('113', q, "Did Smith and Jones sign two contracts?").
:-problem('113', h, "Smith and Jones signed two contracts.").
:-problem('113', a, "Yes").


/*
 3 (NOMINAL) ANAPHORA

*/


/*
 In the examples below we make the assumption (unless otherwise
indicated) that there is no context beyond that provided by the
mini-discourse. This is so that we can do away with explicit co-indexing
of pronouns with their antecedents, on the grounds that context will
provide only (or sometimes a strictly limited number) of possible
antecedents.

*/


/*
 3.1 Intra-Sentential

*/


:-switch_test('114', problem, [], ['fracas-problems']).
:-problem('114', id, '114').
:-problem('114', fracas_answer, "yes").
:-problem('114', p, "Mary used her workstation.").
:-problem('114', q, "Was Mary's workstation used?").
:-problem('114', h, "Mary's workstation was used.").
:-problem('114', a, "Yes").


:-switch_test('115', problem, [], ['fracas-problems']).
:-problem('115', id, '115').
:-problem('115', fracas_answer, "yes").
:-problem('115', p, "Mary used her workstation.").
:-problem('115', q, "Does Mary have a workstation?").
:-problem('115', h, "Mary has a workstation.").
:-problem('115', a, "Yes").


:-switch_test('116', problem, [], ['fracas-problems']).
:-problem('116', id, '116').
:-problem('116', fracas_answer, "yes").
:-problem('116', p, "Mary used her workstation.").
:-problem('116', q, "Is Mary female?").
:-problem('116', h, "Mary is female.").
:-problem('116', a, "Yes").


:-switch_test('117', problem, [], ['fracas-problems']).
:-problem('117', id, '117').
:-problem('117', fracas_answer, "yes").
:-problem('117', p, "Every student used her workstation.").
:-problem('117', p, "Mary is a student.").
:-problem('117', q, "Did Mary use her workstation?").
:-problem('117', h, "Mary used her workstation.").
:-problem('117', a, "Yes").


:-switch_test('118', problem, [], ['fracas-problems']).
:-problem('118', id, '118').
:-problem('118', fracas_answer, "yes").
:-problem('118', p, "Every student used her workstation.").
:-problem('118', p, "Mary is a student.").
:-problem('118', q, "Does Mary have a workstation?").
:-problem('118', h, "Mary has a workstation.").
:-problem('118', a, "Yes").


:-switch_test('119', problem, [], ['fracas-problems']).
:-problem('119', id, '119').
:-problem('119', fracas_answer, "no").
:-problem('119', p, "No student used her workstation.").
:-problem('119', p, "Mary is a student.").
:-problem('119', q, "Did Mary use a workstation?").
:-problem('119', h, "Mary used a workstation.").
:-problem('119', a, "No").


/*
 3.2 Inter-Sentential

*/


:-switch_test('120', problem, [], ['fracas-problems']).
:-problem('120', id, '120').
:-problem('120', fracas_answer, "yes").
:-problem('120', p, "Smith attended a meeting.").
:-problem('120', p, "She chaired it.").
:-problem('120', q, "Did Smith chair a meeting?").
:-problem('120', h, "Smith chaired a meeting.").
:-problem('120', a, "Yes").


:-switch_test('121', problem, [], ['fracas-problems']).
:-problem('121', id, '121').
:-problem('121', fracas_answer, "yes").
:-problem('121', p, "Smith delivered a report to ITEL.").
:-problem('121', p, "She also delivered them an invoice.").
:-problem('121', p, "And she delivered them a project proposal.").
:-problem('121', q, "Did Smith deliver a report, an invoice and a project proposal to ITEL?").
:-problem('121', h, "Smith delivered a report, an invoice and a project proposal to ITEL.").
:-problem('121', a, "Yes").
:-problem('121', why, "Keeping track of same entities across more than two sentences.").


:-switch_test('122', problem, [], ['fracas-problems']).
:-problem('122', id, '122').
:-problem('122', fracas_answer, "yes").
:-problem('122', p, "Every committee has a chairman.").
:-problem('122', p, "He is appointed its members.").
:-problem('122', q, "Does every committee have a chairman appointed by members of the committee?").
:-problem('122', h, "Every committee has a chairman appointed by members of the committee.").
:-problem('122', a, "Yes").
:-problem('122', why, "Modal subordination.").


/*
 3.3 Plural Anaphora

*/


:-switch_test('123', problem, [], ['fracas-problems']).
:-problem('123', id, '123').
:-problem('123', fracas_answer, "yes").
:-problem('123', p, "ITEL has sent most of the reports Smith needs.").
:-problem('123', p, "They are on her desk.").
:-problem('123', q, "Are there some reports from ITEL on Smith's desk?").
:-problem('123', h, "There are some reports from ITEL on Smith's desk.").
:-problem('123', a, "Yes").


:-switch_test('124', problem, [], ['fracas-problems']).
:-problem('124', id, '124').
:-problem('124', fracas_answer, "yes").
:-problem('124', p, "Two out of ten machines are missing.").
:-problem('124', p, "They have been removed.").
:-problem('124', q, "Have two machines been removed?").
:-problem('124', h, "Two machines have been removed.").
:-problem('124', a, "Yes").


:-switch_test('125', problem, [], ['fracas-problems']).
:-problem('125', id, '125').
:-problem('125', fracas_answer, "unknown").
:-problem('125', p, "Two out of ten machines are missing.").
:-problem('125', p, "They have been removed.").
:-problem('125', q, "Have eight machines been removed?").
:-problem('125', h, "Eight machines have been removed.").
:-problem('125', a, "Don't know").
:-problem('125', why, "Set difference can't be used to construct plural antecedents").


:-switch_test('126', problem, [], ['fracas-problems']).
:-problem('126', id, '126').
:-problem('126', fracas_answer, "yes").
:-problem('126', p, "Two out of ten machines are missing.").
:-problem('126', p, "They were all here yesterday.").
:-problem('126', q, "Were ten machines here yesterday?").
:-problem('126', h, "Ten machines were here yesterday.").
:-problem('126', a, "Yes").


:-switch_test('127', problem, [], ['fracas-problems']).
:-problem('127', id, '127').
:-problem('127', fracas_answer, "yes").
:-problem('127', fracas_nonstandard, "true").
:-problem('127', p, "Smith took a machine on Tuesday, and Jones took a machine on Wednesday.").
:-problem('127', p, "They put them in the lobby.").
:-problem('127', q, "Did Smith and Jones put two machines in the lobby?").
:-problem('127', h, "Smith and Jones put two machines in the lobby.").
:-problem('127', a, "Yes, on a distributive reading of the question.").
:-problem('127', why, "Construction of plural antecedents from separate constituents.").


:-switch_test('128', problem, [], ['fracas-problems']).
:-problem('128', id, '128').
:-problem('128', fracas_answer, "yes").
:-problem('128', p, "John and his colleagues went to a meeting.").
:-problem('128', p, "They hated it.").
:-problem('128', q, "Did John's colleagues hate the meeting?").
:-problem('128', h, "John's colleagues hated the meeting.").
:-problem('128', a, "Yes").


:-switch_test('129', problem, [], ['fracas-problems']).
:-problem('129', id, '129').
:-problem('129', fracas_answer, "yes").
:-problem('129', fracas_nonstandard, "true").
:-problem('129', p, "John and his colleagues went to a meeting.").
:-problem('129', p, "They hated it.").
:-problem('129', q, "Did John hate the meeting?").
:-problem('129', h, "John hated the meeting.").
:-problem('129', a, "Yes, on one possible reading").
:-problem('129', why, "\"They\" can be resolved to John and his colleagues.").


:-switch_test('130', problem, [], ['fracas-problems']).
:-problem('130', id, '130').
:-problem('130', fracas_answer, "unknown").
:-problem('130', fracas_nonstandard, "true").
:-problem('130', p, "John and his colleagues went to a meeting.").
:-problem('130', p, "They hated it.").
:-problem('130', q, "Did John hate the meeting?").
:-problem('130', h, "John hated the meeting.").
:-problem('130', a, "Don't know, on one possible reading").
:-problem('130', why, "\"They\" can also be resolved to John's colleagues but not John.").
:-note(problem, ' Note that this is formally identical with preceding. ').


:-switch_test('131', problem, [], ['fracas-problems']).
:-problem('131', id, '131').
:-problem('131', fracas_answer, "yes").
:-problem('131', p, "Each department has a dedicated line.").
:-problem('131', p, "They rent them from BT.").
:-problem('131', q, "Does every department rent a line from BT?").
:-problem('131', h, "Every department rents a line from BT.").
:-problem('131', a, "Yes").
:-problem('131', why, "Dependent plural anaphora").


:-switch_test('132', problem, [], ['fracas-problems']).
:-problem('132', id, '132').
:-problem('132', fracas_answer, "yes").
:-problem('132', p, "Each department has a dedicated line.").
:-problem('132', p, "The sales department rents it from BT.").
:-problem('132', q, "Does the sales department rent a line from BT?").
:-problem('132', h, "The sales department rents a line from BT.").
:-problem('132', a, "Yes").
:-problem('132', why, "Paycheque pronoun").


/*
 3.4 E-type and Donkey Pronouns

*/


:-switch_test('133', problem, [], ['fracas-problems']).
:-problem('133', id, '133').
:-problem('133', fracas_answer, "yes").
:-problem('133', p, "GFI owns several computers.").
:-problem('133', p, "ITEL maintains them.").
:-problem('133', q, "Does ITEL maintain all the computers that GFI owns?").
:-problem('133', h, "ITEL maintains all the computers that GFI owns.").
:-problem('133', a, "Yes").
:-problem('133', why, "E-type anaphora").


:-switch_test('134', problem, [], ['fracas-problems']).
:-problem('134', id, '134').
:-problem('134', fracas_answer, "yes").
:-problem('134', p, "Every customer who owns a computer has a service contract for it.").
:-problem('134', p, "MFI is a customer that owns exactly one computer.").
:-problem('134', q, "Does MFI have a service contract for all its computers?").
:-problem('134', h, "MFI has a service contract for all its computers.").
:-problem('134', a, "Yes").
:-problem('134', why, "Donkey sentence").


:-switch_test('135', problem, [], ['fracas-problems']).
:-problem('135', id, '135').
:-problem('135', fracas_answer, "yes").
:-problem('135', p, "Every customer who owns a computer has a service contract for it.").
:-problem('135', p, "MFI is a customer that owns several computers.").
:-problem('135', q, "Does MFI have a service contract for all its computers?").
:-problem('135', h, "MFI has a service contract for all its computers.").
:-problem('135', a, "Yes").
:-problem('135', why, "This pattern of inference, unlike (134), tends to some theory dependence. Although the inference seems correct in this example, compare with (136)").


:-switch_test('136', problem, [], ['fracas-problems']).
:-problem('136', id, '136').
:-problem('136', fracas_answer, "unknown").
:-problem('136', p, "Every executive who had a laptop computer brought it to take notes at the meeting.").
:-problem('136', p, "Smith is a executive who owns five different laptop computers.").
:-problem('136', q, "Did Smith take five laptop computers to the meeting?").
:-problem('136', h, "Smith took five laptop computers to the meeting.").
:-problem('136', a, "Don't know").
:-problem('136', why, "Similar to (135), except for tense and pragmatic plausibility.").


:-switch_test('137', problem, [], ['fracas-problems']).
:-problem('137', id, '137').
:-problem('137', fracas_answer, "yes").
:-problem('137', p, "There are 100 companies.").
:-problem('137', p, "ICM is one of the companies and owns 150 computers.").
:-problem('137', p, "It does not have service contracts for any of its computers.").
:-problem('137', p, "Each of the other 99 companies owns one computer.").
:-problem('137', p, "They have service contracts for them.").
:-problem('137', q, "Do most companies that own a computer have a service contract for it?").
:-problem('137', h, "Most companies that own a computer have a service contract for it.").
:-problem('137', a, "Yes").
:-problem('137', why, "Proportion problem").


/*
 3.5 Functional and Subsectional

*/


/*
 Due to the heavy domain dependence of functional (or better perhaps,
relational) anaphora, it is hard to state general inferences that don't assume
considerable background knowledge unless this is given explicitly.

*/


:-switch_test('138', problem, [], ['fracas-problems']).
:-problem('138', id, '138').
:-problem('138', fracas_answer, "yes").
:-problem('138', p, "Every report has a cover page.").
:-problem('138', p, "R-95-103 is a report.").
:-problem('138', p, "Smith signed the cover page.").
:-problem('138', q, "Did Smith sign the cover page of R-95-103?").
:-problem('138', h, "Smith signed the cover page of R-95-103.").
:-problem('138', a, "Yes").


/*
 3.6 Simple Reflexives

*/


:-switch_test('139', problem, [], ['fracas-problems']).
:-problem('139', id, '139').
:-problem('139', fracas_answer, "yes").
:-problem('139', p, "A company director awarded himself a large payrise.").
:-problem('139', q, "Has a company director awarded and been awarded a payrise?").
:-problem('139', h, "A company director has awarded and been awarded a payrise.").
:-problem('139', a, "Yes").


:-switch_test('140', problem, [], ['fracas-problems']).
:-problem('140', id, '140').
:-problem('140', fracas_answer, "yes").
:-problem('140', p, "John said Bill had hurt himself.").
:-problem('140', q, "Did John say Bill had been hurt?").
:-problem('140', h, "John said Bill had been hurt.").
:-problem('140', a, "Yes").


:-switch_test('141', problem, [], ['fracas-problems']).
:-problem('141', id, '141').
:-problem('141', fracas_answer, "unknown").
:-problem('141', p, "John said Bill had hurt himself.").
:-problem('141', q, "Did anyone say John had been hurt?").
:-problem('141', h, "Someone said John had been hurt.").
:-problem('141', a, "Don't know").


/*
 4 ELLIPSIS

*/


/*

  In nearly all cases the inferences presented here have conclusions that are
  simply reconstructions of the ellided constituent. Unfortunately, an inference
  test suite is not well suited to illustrating prohibitions on ellipsis
  resolution. For example, an ill-formed discourse like

  John was in Paris yesterday. *So did Bill.

  doesn't even get as far as supporting any inferences.


*/


/*
 4.1 VP Ellipsis

*/


:-switch_test('142', problem, [], ['fracas-problems']).
:-problem('142', id, '142').
:-problem('142', fracas_answer, "yes").
:-problem('142', p, "John spoke to Mary.").
:-problem('142', p, "So did Bill.").
:-problem('142', q, "Did Bill speak to Mary?").
:-problem('142', h, "Bill spoke to Mary.").
:-problem('142', a, "Yes").
:-problem('142', why, "Basic example.").


:-switch_test('143', problem, [], ['fracas-problems']).
:-problem('143', id, '143').
:-problem('143', fracas_answer, "unknown").
:-problem('143', p, "John spoke to Mary.").
:-problem('143', p, "So did Bill.").
:-problem('143', p, "John spoke to Mary at four o'clock.").
:-problem('143', q, "Did Bill speak to Mary at four o'clock?").
:-problem('143', h, "Bill spoke to Mary at four o'clock.").
:-problem('143', a, "Don't know").
:-problem('143', why, "Temporal resolution of tense in antecedent is not carried across to ellipsis.").


:-switch_test('144', problem, [], ['fracas-problems']).
:-problem('144', id, '144').
:-problem('144', fracas_answer, "yes").
:-problem('144', p, "John spoke to Mary at four o'clock.").
:-problem('144', p, "So did Bill.").
:-problem('144', q, "Did Bill speak to Mary at four o'clock?").
:-problem('144', h, "Bill spoke to Mary at four o'clock.").
:-problem('144', a, "Yes").
:-problem('144', why, "Explicit temporal adverbials are carried across to ellipsis.").


:-switch_test('145', problem, [], ['fracas-problems']).
:-problem('145', id, '145').
:-problem('145', fracas_answer, "yes").
:-problem('145', p, "John spoke to Mary at four o'clock.").
:-problem('145', p, "And Bill did at five o'clock.").
:-problem('145', q, "Did Bill speak to Mary at five o'clock?").
:-problem('145', h, "Bill spoke to Mary at five o'clock.").
:-problem('145', a, "Yes").
:-problem('145', why, "Explicit temporal adverbials are not carried across if overridden.").


:-switch_test('146', problem, [], ['fracas-problems']).
:-problem('146', id, '146').
:-problem('146', fracas_answer, "yes").
:-problem('146', p, "John has spoken to Mary.").
:-problem('146', p, "Bill is going to.").
:-problem('146', q, "Will Bill speak to Mary?").
:-problem('146', h, "Bill will speak to Mary.").
:-problem('146', a, "Yes").
:-problem('146', why, "Tense agreement not necessary between ellipsis and antecedent.").


:-switch_test('147', problem, [], ['fracas-problems']).
:-problem('147', id, '147').
:-problem('147', fracas_answer, "no").
:-problem('147', p, "John spoke to Mary on Monday.").
:-problem('147', p, "Bill didn't.").
:-problem('147', q, "Did Bill speak to Mary on Monday?").
:-problem('147', h, "Bill spoke to Mary on Monday.").
:-problem('147', a, "No").
:-problem('147', why, "Polarity agreement not necessary between ellipsis and antecedent.").


:-switch_test('148', problem, [], ['fracas-problems']).
:-problem('148', id, '148').
:-problem('148', fracas_answer, "yes").
:-problem('148', p, "Has John spoken to Mary?").
:-problem('148', p, "Bill has.").
:-problem('148', q, "Has Bill spoken to Mary?").
:-problem('148', h, "Bill has spoken to Mary.").
:-problem('148', a, "Yes").
:-problem('148', why, "Mood agreement not necessary between ellipsis and antecedent.").
:-note(problem, ' Note unusual use of question as premise. ').


:-switch_test('149', problem, [], ['fracas-problems']).
:-problem('149', id, '149').
:-problem('149', fracas_answer, "yes").
:-problem('149', p, "John has spoken to Mary.").
:-problem('149', p, "The students have too.").
:-problem('149', q, "Have the students spoken to Mary?").
:-problem('149', h, "The students have spoken to Mary.").
:-problem('149', a, "Yes").
:-problem('149', why, "Number agreement not necessary.").


/*
 4.2 Gapping

*/


:-switch_test('150', problem, [], ['fracas-problems']).
:-problem('150', id, '150').
:-problem('150', fracas_answer, "yes").
:-problem('150', p, "John went to Paris by car, and Bill by train.").
:-problem('150', q, "Did Bill go to Paris by train?").
:-problem('150', h, "Bill went to Paris by train.").
:-problem('150', a, "Yes").
:-problem('150', why, "Basic example").


:-switch_test('151', problem, [], ['fracas-problems']).
:-problem('151', id, '151').
:-problem('151', fracas_answer, "yes").
:-problem('151', p, "John went to Paris by car, and Bill by train to Berlin.").
:-problem('151', q, "Did Bill go to Berlin by train?").
:-problem('151', h, "Bill went to Berlin by train.").
:-problem('151', a, "Yes").
:-problem('151', why, "Another basic example").


:-switch_test('152', problem, [], ['fracas-problems']).
:-problem('152', id, '152').
:-problem('152', fracas_answer, "yes").
:-problem('152', p, "John went to Paris by car, and Bill to Berlin.").
:-problem('152', q, "Did Bill go to Berlin by car?").
:-problem('152', h, "Bill went to Berlin by car.").
:-problem('152', a, "Yes").
:-problem('152', why, "Another basic example").


:-switch_test('153', problem, [], ['fracas-problems']).
:-problem('153', id, '153').
:-problem('153', fracas_answer, "yes").
:-problem('153', p, "John is going to Paris by car, and the students by train.").
:-problem('153', q, "Are the students going to Paris by train?").
:-problem('153', h, "The students are going to Paris by train.").
:-problem('153', a, "Yes").
:-problem('153', why, "Subject-verb agreement not necessary").


:-switch_test('154', problem, [], ['fracas-problems']).
:-problem('154', id, '154').
:-problem('154', fracas_answer, "yes").
:-problem('154', p, "John went to Paris by car.").
:-problem('154', p, "Bill by train.").
:-problem('154', q, "Did Bill go to Paris by train?").
:-problem('154', h, "Bill went to Paris by train.").
:-problem('154', a, "Yes").
:-problem('154', why, "Cross-sentential gapping").


/*
 4.3 One Anaphora

*/


:-switch_test('155', problem, [], ['fracas-problems']).
:-problem('155', id, '155').
:-problem('155', fracas_answer, "yes").
:-problem('155', p, "John owns a car.").
:-problem('155', p, "Bill owns one too.").
:-problem('155', q, "Does Bill own a car?").
:-problem('155', h, "Bill owns a car.").
:-problem('155', a, "Yes").
:-problem('155', why, "Basic example").


:-switch_test('156', problem, [], ['fracas-problems']).
:-problem('156', id, '156').
:-problem('156', fracas_answer, "unknown").
:-problem('156', p, "John owns a car.").
:-problem('156', p, "Bill owns one too.").
:-problem('156', q, "Is there a car that John and Bill own?").
:-problem('156', h, "There is a car that John and Bill own.").
:-problem('156', a, "Don't know").
:-problem('156', why, "It needn't be the same car that John and Bill own.").


:-switch_test('157', problem, [], ['fracas-problems']).
:-problem('157', id, '157').
:-problem('157', fracas_answer, "yes").
:-problem('157', p, "John owns a red car.").
:-problem('157', p, "Bill owns a blue one.").
:-problem('157', q, "Does Bill own a blue car?").
:-problem('157', h, "Bill owns a blue car.").
:-problem('157', a, "Yes").


:-switch_test('158', problem, [], ['fracas-problems']).
:-problem('158', id, '158').
:-problem('158', fracas_answer, "unknown").
:-problem('158', p, "John owns a red car.").
:-problem('158', p, "Bill owns a blue one.").
:-problem('158', q, "Does Bill own a red car?").
:-problem('158', h, "Bill owns a red car.").
:-problem('158', a, "Don't know").


:-switch_test('159', problem, [], ['fracas-problems']).
:-problem('159', id, '159').
:-problem('159', fracas_answer, "yes").
:-problem('159', p, "John owns a red car.").
:-problem('159', p, "Bill owns a fast one.").
:-problem('159', q, "Does Bill own a fast car?").
:-problem('159', h, "Bill owns a fast car.").
:-problem('159', a, "Yes").


:-switch_test('160', problem, [], ['fracas-problems']).
:-problem('160', id, '160').
:-problem('160', fracas_answer, "yes").
:-problem('160', fracas_nonstandard, "true").
:-problem('160', p, "John owns a red car.").
:-problem('160', p, "Bill owns a fast one.").
:-problem('160', q, "Does Bill own a fast red car?").
:-problem('160', h, "Bill owns a fast red car.").
:-problem('160', a, "Yes, on one possible reading").
:-problem('160', why, "The \"one\" anaphor may be resolved via the property of being a red car.").


:-switch_test('161', problem, [], ['fracas-problems']).
:-problem('161', id, '161').
:-problem('161', fracas_answer, "unknown").
:-problem('161', fracas_nonstandard, "true").
:-problem('161', p, "John owns a red car.").
:-problem('161', p, "Bill owns a fast one.").
:-problem('161', q, "Does Bill own a fast red car?").
:-problem('161', h, "Bill owns a fast red car.").
:-problem('161', a, "Don't know, on one possible reading").
:-problem('161', why, "Or the \"one\" anaphor may just be resolved via the property of being a car.").
:-note(problem, ' Note that this is formally identical with preceding. ').


:-switch_test('162', problem, [], ['fracas-problems']).
:-problem('162', id, '162').
:-problem('162', fracas_answer, "yes").
:-problem('162', p, "John owns a fast red car.").
:-problem('162', p, "Bill owns a slow one.").
:-problem('162', q, "Does Bill own a slow red car?").
:-problem('162', h, "Bill owns a slow red car.").
:-problem('162', a, "Yes").
:-problem('162', why, "When semantically parallel (e.g. fast/slow) modifiers appear on the antecedent and one-anaphor, it appears that all non-parallel modifiers must form part of the resolution.").


/*
 4.4 Sluicing

*/


:-switch_test('163', problem, [], ['fracas-problems']).
:-problem('163', id, '163').
:-problem('163', fracas_answer, "no").
:-problem('163', p, "John had his paper accepted.").
:-problem('163', p, "Bill doesn't know why.").
:-problem('163', q, "Does Bill know why John had his paper accepted?").
:-problem('163', h, "Bill knows why John had his paper accepted.").
:-problem('163', a, "No").


/*
 4.5 Phrasal Ellipsis

*/


:-switch_test('164', problem, [], ['fracas-problems']).
:-problem('164', id, '164').
:-problem('164', fracas_answer, "yes").
:-problem('164', p, "John spoke to Mary.").
:-problem('164', p, "And to Sue.").
:-problem('164', q, "Did John speak to Sue?").
:-problem('164', h, "John spoke to Sue.").
:-problem('164', a, "Yes").
:-problem('164', why, "PP ellipsis (subcategorized)").


:-switch_test('165', problem, [], ['fracas-problems']).
:-problem('165', id, '165').
:-problem('165', fracas_answer, "yes").
:-problem('165', p, "John spoke to Mary.").
:-problem('165', p, "On Friday.").
:-problem('165', q, "Did John speak to Mary on Friday?").
:-problem('165', h, "John spoke to Mary on Friday.").
:-problem('165', a, "Yes").
:-problem('165', why, "PP ellipsis: adds PP to antecedent").


:-switch_test('166', problem, [], ['fracas-problems']).
:-problem('166', id, '166').
:-problem('166', fracas_answer, "yes").
:-problem('166', p, "John spoke to Mary on Thursday.").
:-problem('166', p, "And on Friday.").
:-problem('166', q, "Did John speak to Mary on Friday?").
:-problem('166', h, "John spoke to Mary on Friday.").
:-problem('166', a, "Yes").
:-problem('166', why, "PP ellipsis: replaces PP in antecedent").


:-switch_test('167', problem, [], ['fracas-problems']).
:-problem('167', id, '167').
:-problem('167', fracas_answer, "no").
:-problem('167', p, "Twenty men work in the Sales Department.").
:-problem('167', p, "But only one woman.").
:-problem('167', q, "Do two women work in the Sales Department?").
:-problem('167', h, "Two women work in the Sales Department.").
:-problem('167', a, "No").
:-problem('167', why, "NP ellipsis").


:-switch_test('168', problem, [], ['fracas-problems']).
:-problem('168', id, '168').
:-problem('168', fracas_answer, "yes").
:-problem('168', p, "Five men work part time.").
:-problem('168', p, "And forty five women.").
:-problem('168', q, "Do forty five women work part time?").
:-problem('168', h, "Forty five women work part time.").
:-problem('168', a, "Yes").
:-problem('168', why, "NP ellipsis").


:-switch_test('169', problem, [], ['fracas-problems']).
:-problem('169', id, '169').
:-problem('169', fracas_answer, "yes").
:-problem('169', fracas_nonstandard, "true").
:-problem('169', p, "John found Mary before Bill.").
:-problem('169', q, "Did John find Mary before Bill found Mary?").
:-problem('169', h, "John found Mary before Bill found Mary.").
:-problem('169', a, "Yes, on one possible reading").
:-problem('169', why, "NP ellipsis").


:-switch_test('170', problem, [], ['fracas-problems']).
:-problem('170', id, '170').
:-problem('170', fracas_answer, "yes").
:-problem('170', fracas_nonstandard, "true").
:-problem('170', p, "John found Mary before Bill.").
:-problem('170', q, "Did John find Mary before John found Bill?").
:-problem('170', h, "John found Mary before John found Bill.").
:-problem('170', a, "Yes, on one possible reading").
:-problem('170', why, "NP ellipsis").


:-switch_test('171', problem, [], ['fracas-problems']).
:-problem('171', id, '171').
:-problem('171', fracas_answer, "yes").
:-problem('171', p, "John wants to know how many men work part time.").
:-problem('171', p, "And women.").
:-problem('171', q, "Does John want to know how many women work part time?").
:-problem('171', h, "John wants to know how many women work part time.").
:-problem('171', a, "Yes").
:-problem('171', why, "Nbar ellipsis").


:-switch_test('172', problem, [], ['fracas-problems']).
:-problem('172', id, '172').
:-problem('172', fracas_answer, "yes").
:-problem('172', p, "John wants to know how many men work part time, and which.").
:-problem('172', q, "Does John want to know which men work part time?").
:-problem('172', h, "John wants to know which men work part time.").
:-problem('172', a, "Yes").
:-problem('172', why, "Determiner ellipsis").


/*
 4.6 Antecedent Contained Deletion

*/


/*
 Antecedent contained deletion is a notorious problem for copying
approaches to ellipsis, since the antecedent clause contains the ellipsis and
some way must be found of removing it from the copy.

*/


:-switch_test('173', problem, [], ['fracas-problems']).
:-problem('173', id, '173').
:-problem('173', fracas_answer, "yes").
:-problem('173', p, "Bill spoke to everyone that John did.").
:-problem('173', p, "John spoke to Mary.").
:-problem('173', q, "Did Bill speak to Mary?").
:-problem('173', h, "Bill spoke to Mary.").
:-problem('173', a, "Yes").


:-switch_test('174', problem, [], ['fracas-problems']).
:-problem('174', id, '174').
:-problem('174', fracas_answer, "unknown").
:-problem('174', p, "Bill spoke to everyone that John did.").
:-problem('174', p, "Bill spoke to Mary.").
:-problem('174', q, "Did John speak to Mary?").
:-problem('174', h, "John spoke to Mary.").
:-problem('174', a, "Don't know").


/*
 4.7 Configurational Effects

*/


/*

  There are a number of syntactic and other configurational constraints on what
  can constitute the antecedent to an ellipsis. These constraints varying
  depending on the type of ellipsis (VP, phrasal, gapping, etc).


*/


:-switch_test('175', problem, [], ['fracas-problems']).
:-problem('175', id, '175').
:-problem('175', fracas_answer, "yes").
:-problem('175', fracas_nonstandard, "true").
:-problem('175', p, "John said Mary wrote a report, and Bill did too.").
:-problem('175', q, "Did Bill say Mary wrote a report?").
:-problem('175', h, "Bill said Mary wrote a report.").
:-problem('175', a, "Yes, on one possible reading/parse").


:-switch_test('176', problem, [], ['fracas-problems']).
:-problem('176', id, '176').
:-problem('176', fracas_answer, "yes").
:-problem('176', fracas_nonstandard, "true").
:-problem('176', p, "John said Mary wrote a report, and Bill did too.").
:-problem('176', q, "Did John say Bill wrote a report?").
:-problem('176', h, "John said Bill wrote a report.").
:-problem('176', a, "Yes, on one possible reading/parse").


:-switch_test('177', problem, [], ['fracas-problems']).
:-problem('177', id, '177').
:-problem('177', fracas_answer, "unknown").
:-problem('177', p, "John said that Mary wrote a report, and that Bill did too.").
:-problem('177', q, "Did Bill say Mary wrote a report?").
:-problem('177', h, "Bill said Mary wrote a report.").
:-problem('177', a, "Don't know").


/*
 Note that the first sentence in (175) and (176) is syntactically
ambiguous, depending on whether the conjunctive clause conjoins with the main
or subordinate clause of "John said Mary wrote a report". In (177) the
conjunctive clause unambiguously conjoins with the subordinate clause, and
only one interpretation of the ellipsis is possible. This appears to indicate
that the antecedent clause to a VP ellipsis must be adjacent to the elliptical
clause. However, as the examples below show, this is not correct.

*/


:-switch_test('178', problem, [], ['fracas-problems']).
:-problem('178', id, '178').
:-problem('178', fracas_answer, "yes").
:-problem('178', p, "John wrote a report, and Bill said Peter did too.").
:-problem('178', q, "Did Bill say Peter wrote a report?").
:-problem('178', h, "Bill said Peter wrote a report.").
:-problem('178', a, "Yes").
:-problem('178', why, "Embedded elliptical clause").


:-switch_test('179', problem, [], ['fracas-problems']).
:-problem('179', id, '179').
:-problem('179', fracas_answer, "yes").
:-problem('179', p, "If John wrote a report, then Bill did too.").
:-problem('179', p, "John wrote a report.").
:-problem('179', q, "Did Bill write a report?").
:-problem('179', h, "Bill wrote a report.").
:-problem('179', a, "Yes").
:-problem('179', why, "Elliptical and antecedent clause embedded (in parallel)").


:-switch_test('180', problem, [], ['fracas-problems']).
:-problem('180', id, '180').
:-problem('180', fracas_answer, "yes").
:-problem('180', p, "John wanted to buy a car, and he did.").
:-problem('180', q, "Did John buy a car?").
:-problem('180', h, "John bought a car.").
:-problem('180', a, "Yes").
:-problem('180', why, "Embedded antecedent clause").


:-switch_test('181', problem, [], ['fracas-problems']).
:-problem('181', id, '181').
:-problem('181', fracas_answer, "unknown").
:-problem('181', p, "John needed to buy a car, and Bill did.").
:-problem('181', q, "Did Bill buy a car?").
:-problem('181', h, "Bill bought a car.").
:-problem('181', a, "Don't know").


/*

  Other configurational effects of the kinds illustrated in Deliverable 7 are
  hard to exemplify using inference suites.


*/


/*
 4.8 Ellipsis and Anaphora

*/


/*

  The following inferences illustrate differences between strict and sloppy
  interpretations of anaphors in elliptical clauses.


*/


:-switch_test('182', problem, [], ['fracas-problems']).
:-problem('182', id, '182').
:-problem('182', fracas_answer, "yes").
:-problem('182', fracas_nonstandard, "true").
:-problem('182', p, "Smith represents his company and so does Jones.").
:-problem('182', q, "Does Jones represent Jones' company?").
:-problem('182', h, "Jones represents Jones' company.").
:-problem('182', a, "Yes, on one reading").
:-problem('182', why, "Sloppy identity").


:-switch_test('183', problem, [], ['fracas-problems']).
:-problem('183', id, '183').
:-problem('183', fracas_answer, "yes").
:-problem('183', fracas_nonstandard, "true").
:-problem('183', p, "Smith represents his company and so does Jones.").
:-problem('183', q, "Does Jones represent Smith's company?").
:-problem('183', h, "Jones represents Smith's company.").
:-problem('183', a, "Yes, on one reading").
:-problem('183', why, "Strict identity").


:-switch_test('184', problem, [], ['fracas-problems']).
:-problem('184', id, '184').
:-problem('184', fracas_answer, "unknown").
:-problem('184', p, "Smith represents his company and so does Jones.").
:-problem('184', q, "Does Smith represent Jones' company?").
:-problem('184', h, "Smith represents Jones' company.").
:-problem('184', a, "Don't know").


:-switch_test('185', problem, [], ['fracas-problems']).
:-problem('185', id, '185').
:-problem('185', fracas_answer, "yes").
:-problem('185', fracas_nonstandard, "true").
:-problem('185', p, "Smith claimed he had costed his proposal and so did Jones.").
:-problem('185', q, "Did Jones claim he had costed his own proposal?").
:-problem('185', h, "Jones claimed he had costed his own proposal.").
:-problem('185', a, "Yes, on one reading").
:-problem('185', why, "Sloppy identity on both pronouns").


:-switch_test('186', problem, [], ['fracas-problems']).
:-problem('186', id, '186').
:-problem('186', fracas_answer, "yes").
:-problem('186', fracas_nonstandard, "true").
:-problem('186', p, "Smith claimed he had costed his proposal and so did Jones.").
:-problem('186', q, "Did Jones claim he had costed Smith's proposal?").
:-problem('186', h, "Jones claimed he had costed Smith's proposal.").
:-problem('186', a, "Yes, on one reading").
:-problem('186', why, "Sloppy identity \"he\", strict on \"his\"").


:-switch_test('187', problem, [], ['fracas-problems']).
:-problem('187', id, '187').
:-problem('187', fracas_answer, "yes").
:-problem('187', fracas_nonstandard, "true").
:-problem('187', p, "Smith claimed he had costed his proposal and so did Jones.").
:-problem('187', q, "Did Jones claim Smith had costed Smith's proposal?").
:-problem('187', h, "Jones claimed Smith had costed Smith's proposal.").
:-problem('187', a, "Yes, on one reading").
:-problem('187', why, "Strict identity on both pronouns").


:-switch_test('188', problem, [], ['fracas-problems']).
:-problem('188', id, '188').
:-problem('188', fracas_answer, "unknown").
:-problem('188', p, "Smith claimed he had costed his proposal and so did Jones.").
:-problem('188', q, "Did Jones claim Smith had costed Jones' proposal?").
:-problem('188', h, "Jones claimed Smith had costed Jones' proposal.").
:-problem('188', a, "Don't know").
:-problem('188', why, "Can't have strict identity on \"he\" and sloppy identity on \"his\"").


:-switch_test('189', problem, [], ['fracas-problems']).
:-problem('189', id, '189').
:-problem('189', fracas_answer, "yes").
:-problem('189', fracas_nonstandard, "true").
:-problem('189', p, "John is a man and Mary is a woman.").
:-problem('189', p, "John represents his company and so does Mary.").
:-problem('189', q, "Does Mary represent her own company?").
:-problem('189', h, "Mary represents her own company.").
:-problem('189', a, "Yes, on one reading").
:-problem('189', why, "Sloppy identity, gender agreement not necessary").


:-switch_test('190', problem, [], ['fracas-problems']).
:-problem('190', id, '190').
:-problem('190', fracas_answer, "yes").
:-problem('190', fracas_nonstandard, "true").
:-problem('190', p, "John is a man and Mary is a woman.").
:-problem('190', p, "John represents his company and so does Mary.").
:-problem('190', q, "Does Mary represent John's company?").
:-problem('190', h, "Mary represents John's company.").
:-problem('190', a, "Yes, on one reading").
:-problem('190', why, "Strict identity, gender agreement not necessary").


:-switch_test('191', problem, [], ['fracas-problems']).
:-problem('191', id, '191').
:-problem('191', fracas_answer, "yes").
:-problem('191', p, "Bill suggested to Frank's boss that they should go to the meeting together, and Carl to Alan's wife.").
:-problem('191', q, "If it was suggested that Bill and Frank should go together, was it suggested that Carl and Alan should go together?").
:-problem('191', h, "If it was suggested that Bill and Frank should go together, it was suggested that Carl and Alan should go together.").
:-problem('191', a, "Yes").
:-problem('191', why, "Plural pronouns resolved in parallel").


:-switch_test('192', problem, [], ['fracas-problems']).
:-problem('192', id, '192').
:-problem('192', fracas_answer, "unknown").
:-problem('192', p, "Bill suggested to Frank's boss that they should go to the meeting together, and Carl to Alan's wife.").
:-problem('192', q, "If it was suggested that Bill and Frank should go together, was it suggested that Carl and Alan's wife should go together?").
:-problem('192', h, "If it was suggested that Bill and Frank should go together, it was suggested that Carl and Alan's wife should go together.").
:-problem('192', a, "Don't know").
:-problem('192', why, "Plural pronouns resolved in parallel").


:-switch_test('193', problem, [], ['fracas-problems']).
:-problem('193', id, '193').
:-problem('193', fracas_answer, "yes").
:-problem('193', p, "Bill suggested to Frank's boss that they should go to the meeting together, and Carl to Alan's wife.").
:-problem('193', q, "If it was suggested that Bill and Frank's boss should go together, was it suggested that Carl and Alan's wife should go together?").
:-problem('193', h, "If it was suggested that Bill and Frank's boss should go together, it was suggested that Carl and Alan's wife should go together.").
:-problem('193', a, "Yes").
:-problem('193', why, "Plural pronouns resolved in parallel").


:-switch_test('194', problem, [], ['fracas-problems']).
:-problem('194', id, '194').
:-problem('194', fracas_answer, "unknown").
:-problem('194', p, "Bill suggested to Frank's boss that they should go to the meeting together, and Carl to Alan's wife.").
:-problem('194', q, "If it was suggested that Bill and Frank's boss should go together, was it suggested that Carl and Alan should go together?").
:-problem('194', h, "If it was suggested that Bill and Frank's boss should go together, it was suggested that Carl and Alan should go together.").
:-problem('194', a, "Don't know").
:-problem('194', why, "Plural pronouns resolved in parallel").


:-switch_test('195', problem, [], ['fracas-problems']).
:-problem('195', id, '195').
:-problem('195', fracas_answer, "yes").
:-problem('195', p, "Bill suggested to Frank's boss that they should go to the meeting together, and Carl to Alan's wife.").
:-problem('195', q, "If it was suggested that Bill, Frank and Frank's boss should go together, was it suggested that Carl, Alan and Alan's wife should go together?").
:-problem('195', h, "If it was suggested that Bill, Frank and Frank's boss should go together, it was suggested that Carl, Alan and Alan's wife should go together.").
:-problem('195', a, "Yes").
:-problem('195', why, "Plural pronouns resolved in parallel").


/*
 4.9 Ellipsis and Quantification

*/


/*
 Scope parallelism turns out to be rather tricky to illustrate
through inference suites. This is because of the entailment relation: 98
j= 89.

*/


:-switch_test('196', problem, [], ['fracas-problems']).
:-problem('196', id, '196').
:-problem('196', fracas_answer, "yes").
:-problem('196', p, "A lawyer signed every report, and so did an auditor.").
:-problem('196', p, "That is, there was one lawyer who signed all the reports.").
:-problem('196', q, "Was there one auditor who signed all the reports?").
:-problem('196', h, "There was one auditor who signed all the reports.").
:-problem('196', a, "Yes").


/*
 5 ADJECTIVES

*/


/*

  The inferences below carve up adjectives into (a by no means exhaustive)
  cross-cutting set of dimensions. Typical inferences are given for example
  adjectives.


*/


/*
 5.1 Affirmative and Non-Affirmative

*/


/*

  Affirmative adjectives map the denotation of the predicate they modify onto a
  subset of the denotation. So for example, an old man is a man. Most adjectives
  are affirmative, but a few like former and fake are not. Given that someone is
  a former student, one cannot conclude that they are now a student. But it is
  not entirely clear whether one can conclude that they are not now a student
  ... they may have become one again.


*/


:-switch_test('197', problem, [], ['fracas-problems']).
:-problem('197', id, '197').
:-problem('197', fracas_answer, "yes").
:-problem('197', p, "John has a genuine diamond.").
:-problem('197', q, "Does John have a diamond?").
:-problem('197', h, "John has a diamond.").
:-problem('197', a, "Yes").
:-problem('197', why, "Affirmative adjectives: Adj N entails N").


:-switch_test('198', problem, [], ['fracas-problems']).
:-problem('198', id, '198').
:-problem('198', fracas_answer, "no").
:-problem('198', fracas_nonstandard, "true").
:-problem('198', p, "John is a former university student.").
:-problem('198', q, "Is John a university student?").
:-problem('198', h, "John is a university student.").
:-problem('198', a, "No / don't know").
:-problem('198', why, "Non-affirmative: Adj N =/=> N (Opinions differ about whether \"Adj N entails !N\")").


:-switch_test('199', problem, [], ['fracas-problems']).
:-problem('199', id, '199').
:-problem('199', fracas_answer, "yes").
:-problem('199', fracas_nonstandard, "true").
:-problem('199', p, "John is a successful former university student.").
:-problem('199', q, "Is John successful?").
:-problem('199', h, "John is successful.").
:-problem('199', a, "Yes (for a former university student)").
:-problem('199', why, "Ordering between affirmative and non-affirmative adjectives affects which adjectival predications are and aren't affirmed").


:-switch_test('200', problem, [], ['fracas-problems']).
:-problem('200', id, '200').
:-problem('200', fracas_answer, "unknown").
:-problem('200', p, "John is a former successful university student.").
:-problem('200', q, "Is John successful?").
:-problem('200', h, "John is successful.").
:-problem('200', a, "Don't know").


:-switch_test('201', problem, [], ['fracas-problems']).
:-problem('201', id, '201').
:-problem('201', fracas_answer, "unknown").
:-problem('201', p, "John is a former successful university student.").
:-problem('201', q, "Is John a university student?").
:-problem('201', h, "John is a university student.").
:-problem('201', a, "Don't know").
:-problem('201', why, "John may currently be an unsuccessful university student").


/*
 5.2 No Comparison Class

*/


/*

  Gradable adjectives (e.g. big, small) usually assume some form of comparison
  class (i.e. "big for an N"). But some others do not e.g. four-legged, or the
  adjectival phrase ten foot long. Adjectives not requiring a comparison class
  permit straightforward predication without reference to a nominal property
  providing a comparison class: a ten foot long alligator is ten foot long.


*/


:-switch_test('202', problem, [], ['fracas-problems']).
:-problem('202', id, '202').
:-problem('202', fracas_answer, "yes").
:-problem('202', p, "Every mammal is an animal.").
:-problem('202', q, "Is every four-legged mammal a four-legged animal?").
:-problem('202', h, "Every four-legged mammal is a four-legged animal.").
:-problem('202', a, "Yes").
:-problem('202', why, "[N1 entails N2] entails [Adj(N1) entails Adj(N2)]").


:-switch_test('203', problem, [], ['fracas-problems']).
:-problem('203', id, '203').
:-problem('203', fracas_answer, "yes").
:-problem('203', p, "Dumbo is a four-legged animal.").
:-problem('203', q, "Is Dumbo four-legged?").
:-problem('203', h, "Dumbo is four-legged.").
:-problem('203', a, "Yes").
:-problem('203', why, "Adj(N)(x) entails Adj(x)").


/*
 5.3 Opposites

*/


/*
 Large and small (applied to the same comparison class) are opposites. If
something is a small N it cannot be a large N, and vice versa. Some things can
be neither large nor small Ns.

*/


:-switch_test('204', problem, [], ['fracas-problems']).
:-problem('204', id, '204').
:-problem('204', fracas_answer, "no").
:-problem('204', p, "Mickey is a small animal.").
:-problem('204', q, "Is Mickey a large animal?").
:-problem('204', h, "Mickey is a large animal.").
:-problem('204', a, "No").
:-problem('204', why, "Small(N) entails !Large(N)").


:-switch_test('205', problem, [], ['fracas-problems']).
:-problem('205', id, '205').
:-problem('205', fracas_answer, "no").
:-problem('205', p, "Dumbo is a large animal.").
:-problem('205', q, "Is Dumbo a small animal?").
:-problem('205', h, "Dumbo is a small animal.").
:-problem('205', a, "No").
:-problem('205', why, "Large(N) entails !Small(N)").


:-switch_test('206', problem, [], ['fracas-problems']).
:-problem('206', id, '206').
:-problem('206', fracas_answer, "unknown").
:-problem('206', p, "Fido is not a small animal.").
:-problem('206', q, "Is Fido a large animal?").
:-problem('206', h, "Fido is a large animal.").
:-problem('206', a, "Don't know").
:-problem('206', why, "!Small(N) =/=> Large(N)").


:-switch_test('207', problem, [], ['fracas-problems']).
:-problem('207', id, '207').
:-problem('207', fracas_answer, "unknown").
:-problem('207', p, "Fido is not a large animal.").
:-problem('207', q, "Is Fido a small animal?").
:-problem('207', h, "Fido is a small animal.").
:-problem('207', a, "Don't know").
:-problem('207', why, "!Large(N) =/=> Small(N)").


:-switch_test('208', problem, [], ['fracas-problems']).
:-problem('208', id, '208').
:-problem('208', fracas_answer, "yes").
:-problem('208', p, "Mickey is a small animal.").
:-problem('208', p, "Dumbo is a large animal.").
:-problem('208', q, "Is Mickey smaller than Dumbo?").
:-problem('208', h, "Mickey is smaller than Dumbo.").
:-problem('208', a, "Yes").
:-problem('208', why, "\"Small\" and \"large\" are related via the comparative \"smaller\"").


:-switch_test('209', problem, [], ['fracas-problems']).
:-problem('209', id, '209').
:-problem('209', fracas_answer, "no").
:-problem('209', p, "Mickey is a small animal.").
:-problem('209', p, "Dumbo is a large animal.").
:-problem('209', q, "Is Mickey larger than Dumbo?").
:-problem('209', h, "Mickey is larger than Dumbo.").
:-problem('209', a, "No").
:-problem('209', why, "\"Small\" and \"large\" are related via the comparative \"larger\"").


/*
 5.4 Extensional Comparison Classes

*/


/*
 Adjectives like large and small depend only on the extension of the
comparison class they depend on.

*/


:-switch_test('210', problem, [], ['fracas-problems']).
:-problem('210', id, '210').
:-problem('210', fracas_answer, "no").
:-problem('210', p, "All mice are small animals.").
:-problem('210', p, "Mickey is a large mouse.").
:-problem('210', q, "Is Mickey a large animal?").
:-problem('210', h, "Mickey is a large animal.").
:-problem('210', a, "No").


:-switch_test('211', problem, [], ['fracas-problems']).
:-problem('211', id, '211').
:-problem('211', fracas_answer, "no").
:-problem('211', p, "All elephants are large animals.").
:-problem('211', p, "Dumbo is a small elephant.").
:-problem('211', q, "Is Dumbo a small animal?").
:-problem('211', h, "Dumbo is a small animal.").
:-problem('211', a, "No").


:-switch_test('212', problem, [], ['fracas-problems']).
:-problem('212', id, '212').
:-problem('212', fracas_answer, "yes").
:-problem('212', p, "All mice are small animals.").
:-problem('212', p, "All elephants are large animals.").
:-problem('212', p, "Mickey is a large mouse.").
:-problem('212', p, "Dumbo is a small elephant.").
:-problem('212', q, "Is Dumbo larger than Mickey?").
:-problem('212', h, "Dumbo is larger than Mickey.").
:-problem('212', a, "Yes").
:-problem('212', why, "Assume comparative relations exemplified in (208) and (209)").


:-switch_test('213', problem, [], ['fracas-problems']).
:-problem('213', id, '213').
:-problem('213', fracas_answer, "undef").
:-problem('213', fracas_nonstandard, "true").
:-problem('213', p, "All mice are small animals.").
:-problem('213', p, "Mickey is a large mouse.").
:-problem('213', q, "Is Mickey small?").
:-problem('213', h, "Mickey is small.").
:-problem('213', a, "??: Yes for a mouse; ?? No for an animal").
:-problem('213', why, "Adjectives requiring a comparison class cannot usually be predicated in the absence of a common noun, unless some comparison class is clear from the wider context.").


/*
 5.5 Extensional and Intensional Comparison Classes

*/


/*

  Some adjectives require an "intensional" comparison class: different
  inferences may follow when two distinct but co-extensive predicates provide
  the comparison class.


*/


:-switch_test('214', problem, [], ['fracas-problems']).
:-problem('214', id, '214').
:-problem('214', fracas_answer, "yes").
:-problem('214', p, "All legal authorities are law lecturers.").
:-problem('214', p, "All law lecturers are legal authorities.").
:-problem('214', q, "Are all fat legal authorities fat law lecturers?").
:-problem('214', h, "All fat legal authorities are fat law lecturers.").
:-problem('214', a, "Yes").
:-problem('214', why, "Extensional comparison class").


:-switch_test('215', problem, [], ['fracas-problems']).
:-problem('215', id, '215').
:-problem('215', fracas_answer, "unknown").
:-problem('215', p, "All legal authorities are law lecturers.").
:-problem('215', p, "All law lecturers are legal authorities.").
:-problem('215', q, "Are all competent legal authorities competent law lecturers?").
:-problem('215', h, "All competent legal authorities are competent law lecturers.").
:-problem('215', a, "Don't know").
:-problem('215', why, "Intensional comparison class").


:-switch_test('216', problem, [], ['fracas-problems']).
:-problem('216', id, '216').
:-problem('216', fracas_answer, "yes").
:-problem('216', p, "John is a fatter politician than Bill.").
:-problem('216', q, "Is John fatter than Bill?").
:-problem('216', h, "John is fatter than Bill.").
:-problem('216', a, "Yes").
:-problem('216', why, "Extensional").


:-switch_test('217', problem, [], ['fracas-problems']).
:-problem('217', id, '217').
:-problem('217', fracas_answer, "unknown").
:-problem('217', p, "John is a cleverer politician than Bill.").
:-problem('217', q, "Is John cleverer than Bill?").
:-problem('217', h, "John is cleverer than Bill.").
:-problem('217', a, "Don't know").
:-problem('217', why, "Intensional").


/*

  Note that both intensional and extensional comparison class adjectives support
  comparatives.


*/


/*
 5.6 Default Comparison Classes

*/


/*
 Comparison class adjectives can sometimes pick up a default comparison
class from the subject NP. For example, knowing that Kim is a person provides
a default scale for assessing cleverness in people. If Kim were known to be a
dog, the assessment scale would be different.

*/


:-switch_test('218', problem, [], ['fracas-problems']).
:-problem('218', id, '218').
:-problem('218', fracas_answer, "yes").
:-problem('218', p, "Kim is a clever person.").
:-problem('218', q, "Is Kim clever?").
:-problem('218', h, "Kim is clever.").
:-problem('218', a, "Yes").


:-switch_test('219', problem, [], ['fracas-problems']).
:-problem('219', id, '219').
:-problem('219', fracas_answer, "unknown").
:-problem('219', p, "Kim is a clever politician.").
:-problem('219', q, "Is Kim clever?").
:-problem('219', h, "Kim is clever.").
:-problem('219', a, "Don't know").


/*
 6 COMPARATIVES

*/


/*
 6.1 Phrasal Comparatives

*/


:-switch_test('220', problem, [], ['fracas-problems']).
:-problem('220', id, '220').
:-problem('220', fracas_answer, "yes").
:-problem('220', p, "The PC-6082 is faster than the ITEL-XZ.").
:-problem('220', p, "The ITEL-XZ is fast.").
:-problem('220', q, "Is the PC-6082 fast?").
:-problem('220', h, "The PC-6082 is fast.").
:-problem('220', a, "Yes").


:-switch_test('221', problem, [], ['fracas-problems']).
:-problem('221', id, '221').
:-problem('221', fracas_answer, "unknown").
:-problem('221', p, "The PC-6082 is faster than the ITEL-XZ.").
:-problem('221', q, "Is the PC-6082 fast?").
:-problem('221', h, "The PC-6082 is fast.").
:-problem('221', a, "Don't know").


:-switch_test('222', problem, [], ['fracas-problems']).
:-problem('222', id, '222').
:-problem('222', fracas_answer, "unknown").
:-problem('222', p, "The PC-6082 is faster than the ITEL-XZ.").
:-problem('222', p, "The PC-6082 is fast.").
:-problem('222', q, "Is the ITEL-XZ fast?").
:-problem('222', h, "The ITEL-XZ is fast.").
:-problem('222', a, "Don't know").


:-switch_test('223', problem, [], ['fracas-problems']).
:-problem('223', id, '223').
:-problem('223', fracas_answer, "no").
:-problem('223', p, "The PC-6082 is faster than the ITEL-XZ.").
:-problem('223', p, "The PC-6082 is slow.").
:-problem('223', q, "Is the ITEL-XZ fast?").
:-problem('223', h, "The ITEL-XZ is fast.").
:-problem('223', a, "No").


:-switch_test('224', problem, [], ['fracas-problems']).
:-problem('224', id, '224').
:-problem('224', fracas_answer, "yes").
:-problem('224', p, "The PC-6082 is as fast as the ITEL-XZ.").
:-problem('224', p, "The ITEL-XZ is fast.").
:-problem('224', q, "Is the PC-6082 fast?").
:-problem('224', h, "The PC-6082 is fast.").
:-problem('224', a, "Yes").


:-switch_test('225', problem, [], ['fracas-problems']).
:-problem('225', id, '225').
:-problem('225', fracas_answer, "unknown").
:-problem('225', p, "The PC-6082 is as fast as the ITEL-XZ.").
:-problem('225', q, "Is the PC-6082 fast?").
:-problem('225', h, "The PC-6082 is fast.").
:-problem('225', a, "Don't know").


:-switch_test('226', problem, [], ['fracas-problems']).
:-problem('226', id, '226').
:-problem('226', fracas_answer, "unknown").
:-problem('226', p, "The PC-6082 is as fast as the ITEL-XZ.").
:-problem('226', p, "The PC-6082 is fast.").
:-problem('226', q, "Is the ITEL-XZ fast?").
:-problem('226', h, "The ITEL-XZ is fast.").
:-problem('226', a, "Don't know").


:-switch_test('227', problem, [], ['fracas-problems']).
:-problem('227', id, '227').
:-problem('227', fracas_answer, "no").
:-problem('227', p, "The PC-6082 is as fast as the ITEL-XZ.").
:-problem('227', p, "The PC-6082 is slow.").
:-problem('227', q, "Is the ITEL-XZ fast?").
:-problem('227', h, "The ITEL-XZ is fast.").
:-problem('227', a, "No").


:-switch_test('228', problem, [], ['fracas-problems']).
:-problem('228', id, '228').
:-problem('228', fracas_answer, "unknown").
:-problem('228', p, "The PC-6082 is as fast as the ITEL-XZ.").
:-problem('228', q, "Is the PC-6082 faster than the ITEL-XZ?").
:-problem('228', h, "The PC-6082 is faster than the ITEL-XZ.").
:-problem('228', a, "Don't know").


:-switch_test('229', problem, [], ['fracas-problems']).
:-problem('229', id, '229').
:-problem('229', fracas_answer, "no").
:-problem('229', p, "The PC-6082 is as fast as the ITEL-XZ.").
:-problem('229', q, "Is the PC-6082 slower than the ITEL-XZ?").
:-problem('229', h, "The PC-6082 is slower than the ITEL-XZ.").
:-problem('229', a, "No").


:-switch_test('230', problem, [], ['fracas-problems']).
:-problem('230', id, '230').
:-problem('230', fracas_answer, "yes").
:-problem('230', p, "ITEL won more orders than APCOM did.").
:-problem('230', q, "Did ITEL win some orders?").
:-problem('230', h, "ITEL won some orders.").
:-problem('230', a, "Yes").


:-switch_test('231', problem, [], ['fracas-problems']).
:-problem('231', id, '231').
:-problem('231', fracas_answer, "unknown").
:-problem('231', p, "ITEL won more orders than APCOM did.").
:-problem('231', q, "Did APCOM win some orders?").
:-problem('231', h, "APCOM won some orders.").
:-problem('231', a, "Don't know").


:-switch_test('232', problem, [], ['fracas-problems']).
:-problem('232', id, '232').
:-problem('232', fracas_answer, "yes").
:-problem('232', p, "ITEL won more orders than APCOM did.").
:-problem('232', p, "APCOM won ten orders.").
:-problem('232', q, "Did ITEL win at least eleven orders?").
:-problem('232', h, "ITEL won at least eleven orders.").
:-problem('232', a, "Yes").


/*

  Inferences (233)-(235) are similar to (230)-(232). Note however, that if
  "APCOM" can be interpreted as referring to a particular order (e.g. "the APCOM
  contract"), as it can in (233), the sentence ITEL won more orders than APCOM
  is ambiguous between a reading like that in (230)-(232), and one where ITEL
  won more than just the APCOM order ... see (236)


*/


:-switch_test('233', problem, [], ['fracas-problems']).
:-problem('233', id, '233').
:-problem('233', fracas_answer, "yes").
:-problem('233', p, "ITEL won more orders than APCOM.").
:-problem('233', q, "Did ITEL win some orders?").
:-problem('233', h, "ITEL won some orders.").
:-problem('233', a, "Yes").


:-switch_test('234', problem, [], ['fracas-problems']).
:-problem('234', id, '234').
:-problem('234', fracas_answer, "unknown").
:-problem('234', p, "ITEL won more orders than APCOM.").
:-problem('234', q, "Did APCOM win some orders?").
:-problem('234', h, "APCOM won some orders.").
:-problem('234', a, "Don't know").


:-switch_test('235', problem, [], ['fracas-problems']).
:-problem('235', id, '235').
:-problem('235', fracas_answer, "yes").
:-problem('235', p, "ITEL won more orders than APCOM.").
:-problem('235', p, "APCOM won ten orders.").
:-problem('235', q, "Did ITEL win at least eleven orders?").
:-problem('235', h, "ITEL won at least eleven orders.").
:-problem('235', a, "Yes").


:-switch_test('236', problem, [], ['fracas-problems']).
:-problem('236', id, '236').
:-problem('236', fracas_answer, "yes").
:-problem('236', p, "ITEL won more orders than the APCOM contract.").
:-problem('236', q, "Did ITEL win the APCOM contract?").
:-problem('236', h, "ITEL won the APCOM contract.").
:-problem('236', a, "Yes").


:-switch_test('237', problem, [], ['fracas-problems']).
:-problem('237', id, '237').
:-problem('237', fracas_answer, "yes").
:-problem('237', p, "ITEL won more orders than the APCOM contract.").
:-problem('237', q, "Did ITEL win more than one order?").
:-problem('237', h, "ITEL won more than one order.").
:-problem('237', a, "Yes").


:-switch_test('238', problem, [], ['fracas-problems']).
:-problem('238', id, '238').
:-problem('238', fracas_answer, "yes").
:-problem('238', p, "ITEL won twice as many orders than APCOM.").
:-problem('238', p, "APCOM won ten orders.").
:-problem('238', q, "Did ITEL win twenty orders?").
:-problem('238', h, "ITEL won twenty orders.").
:-problem('238', a, "Yes").


/*
 6.2 Clausal Complement

*/


:-switch_test('239', problem, [], ['fracas-problems']).
:-problem('239', id, '239').
:-problem('239', fracas_answer, "yes").
:-problem('239', p, "ITEL won more orders than APCOM lost.").
:-problem('239', q, "Did ITEL win some orders?").
:-problem('239', h, "ITEL won some orders.").
:-problem('239', a, "Yes").


:-switch_test('240', problem, [], ['fracas-problems']).
:-problem('240', id, '240').
:-problem('240', fracas_answer, "unknown").
:-problem('240', p, "ITEL won more orders than APCOM lost.").
:-problem('240', q, "Did APCOM lose some orders?").
:-problem('240', h, "APCOM lost some orders.").
:-problem('240', a, "Don't know").


:-switch_test('241', problem, [], ['fracas-problems']).
:-problem('241', id, '241').
:-problem('241', fracas_answer, "yes").
:-problem('241', p, "ITEL won more orders than APCOM lost.").
:-problem('241', p, "APCOM lost ten orders.").
:-problem('241', q, "Did ITEL win at least eleven orders?").
:-problem('241', h, "ITEL won at least eleven orders.").
:-problem('241', a, "Yes").


/*
 6.3 Measure Phrases

*/


:-switch_test('242', problem, [], ['fracas-problems']).
:-problem('242', id, '242').
:-problem('242', fracas_answer, "yes").
:-problem('242', p, "The PC-6082 is faster than 500 MIPS.").
:-problem('242', p, "The ITEL-ZX is slower than 500 MIPS.").
:-problem('242', q, "Is the PC-6082 faster than the ITEL-ZX?").
:-problem('242', h, "The PC-6082 is faster than the ITEL-ZX.").
:-problem('242', a, "Yes").


/*
 6.4 Differential Comparatives

*/


:-switch_test('243', problem, [], ['fracas-problems']).
:-problem('243', id, '243').
:-problem('243', fracas_answer, "yes").
:-problem('243', p, "ITEL sold 3000 more computers than APCOM.").
:-problem('243', p, "APCOM sold exactly 2500 computers.").
:-problem('243', q, "Did ITEL sell 5500 computers?").
:-problem('243', h, "ITEL sold 5500 computers.").
:-problem('243', a, "Yes").


/*
 6.5 Attributive Comparatives

*/


:-switch_test('244', problem, [], ['fracas-problems']).
:-problem('244', id, '244').
:-problem('244', fracas_answer, "yes").
:-problem('244', fracas_nonstandard, "true").
:-problem('244', p, "APCOM has a more important customer than ITEL.").
:-problem('244', q, "Does APCOM have a more important customer than ITEL is?").
:-problem('244', h, "APCOM has a more important customer than ITEL is.").
:-problem('244', a, "Yes, on one reading of the premise").


:-switch_test('245', problem, [], ['fracas-problems']).
:-problem('245', id, '245').
:-problem('245', fracas_answer, "yes").
:-problem('245', fracas_nonstandard, "true").
:-problem('245', p, "APCOM has a more important customer than ITEL.").
:-problem('245', q, "Does APCOM has a more important customer than ITEL has?").
:-problem('245', h, "APCOM has a more important customer than ITEL has.").
:-problem('245', a, "Yes, on one reading of the premise").
:-note(problem, ' Note ungrammaticality of question in original source: subject/verb agreement.\n  ').


/*
 6.6 Comparatives and Quantifiers

*/


:-switch_test('246', problem, [], ['fracas-problems']).
:-problem('246', id, '246').
:-problem('246', fracas_answer, "yes").
:-problem('246', p, "The PC-6082 is faster than every ITEL computer.").
:-problem('246', p, "The ITEL-ZX is an ITEL computer.").
:-problem('246', q, "Is the PC-6082 faster than the ITEL-ZX?").
:-problem('246', h, "The PC-6082 is faster than the ITEL-ZX.").
:-problem('246', a, "Yes").


:-switch_test('247', problem, [], ['fracas-problems']).
:-problem('247', id, '247').
:-problem('247', fracas_answer, "unknown").
:-problem('247', p, "The PC-6082 is faster than some ITEL computer.").
:-problem('247', p, "The ITEL-ZX is an ITEL computer.").
:-problem('247', q, "Is the PC-6082 faster than the ITEL-ZX?").
:-problem('247', h, "The PC-6082 is faster than the ITEL-ZX.").
:-problem('247', a, "Don't know").


:-switch_test('248', problem, [], ['fracas-problems']).
:-problem('248', id, '248').
:-problem('248', fracas_answer, "yes").
:-problem('248', p, "The PC-6082 is faster than any ITEL computer.").
:-problem('248', p, "The ITEL-ZX is an ITEL computer.").
:-problem('248', q, "Is the PC-6082 faster than the ITEL-ZX?").
:-problem('248', h, "The PC-6082 is faster than the ITEL-ZX.").
:-problem('248', a, "Yes").


:-switch_test('249', problem, [], ['fracas-problems']).
:-problem('249', id, '249').
:-problem('249', fracas_answer, "yes").
:-problem('249', p, "The PC-6082 is faster than the ITEL-ZX and the ITEL-ZY.").
:-problem('249', q, "Is the PC-6082 faster than the ITEL-ZX?").
:-problem('249', h, "The PC-6082 is faster than the ITEL-ZX.").
:-problem('249', a, "Yes").


:-switch_test('250', problem, [], ['fracas-problems']).
:-problem('250', id, '250').
:-problem('250', fracas_answer, "yes").
:-problem('250', fracas_nonstandard, "true").
:-problem('250', p, "The PC-6082 is faster than the ITEL-ZX or the ITEL-ZY.").
:-problem('250', q, "Is the PC-6082 faster than the ITEL-ZX?").
:-problem('250', h, "The PC-6082 is faster than the ITEL-ZX.").
:-problem('250', a, "Yes, on one reading of the premise").


/*
 7 TEMPORAL REFERENCE

*/


/*

  Inference patterns involving temporal reference are complicated by the
  interplay between tense, aspectual information, lexical semantics, defeasible
  interpretation principles such as narrative progression, rhetorical relations,
  a theory of action and causation, world knowledge, interaction between
  plurality, genericity and temporal/aspectual phenomena etc. Some of the
  inferences are very basic, some are more involved. The more complex examples
  give ample illustration of the fact that temporal phenomena are usually
  discourse phenomena.


*/


/*
 7.1 Standard Use of Tenses

*/


:-switch_test('251', problem, [], ['fracas-problems']).
:-problem('251', id, '251').
:-problem('251', fracas_answer, "yes").
:-problem('251', p, "ITEL has a factory in Birmingham.").
:-problem('251', q, "Does ITEL currently have a factory in Birmingham?").
:-problem('251', h, "ITEL currently has a factory in Birmingham.").
:-problem('251', a, "Yes").


:-switch_test('252', problem, [], ['fracas-problems']).
:-problem('252', id, '252').
:-problem('252', fracas_answer, "yes").
:-problem('252', p, "Since 1992 ITEL has been in Birmingham.").
:-problem('252', p, "It is now 1996.").
:-problem('252', q, "Was ITEL in Birmingham in 1993?").
:-problem('252', h, "Itel was in Birmingham in 1993.").
:-problem('252', a, "Yes").


/*
 (251) and (252) are instances of the subinterval property. This works
only with stative verbs. C.f. the following example involving an
accomplishment verb in the simple past:

*/


:-switch_test('253', problem, [], ['fracas-problems']).
:-problem('253', id, '253').
:-problem('253', fracas_answer, "unknown").
:-problem('253', p, "ITEL has developed a new editor since 1992.").
:-problem('253', p, "It is now 1996.").
:-problem('253', q, "Did ITEL develop a new editor in 1993?").
:-problem('253', h, "ITEL developed a new editor in 1993.").
:-problem('253', a, "Don't know").


/*
 Similarly with activity verbs and adverbial modification:

*/


:-switch_test('254', problem, [], ['fracas-problems']).
:-problem('254', id, '254').
:-problem('254', fracas_answer, "unknown").
:-problem('254', p, "ITEL has expanded since 1992.").
:-problem('254', p, "It is now 1996.").
:-problem('254', q, "Did ITEL expand in 1993?").
:-problem('254', h, "ITEL expanded in 1993.").
:-problem('254', a, "Don't know").


/*
 Also, the position of the "since" adverbial affects the range of readings
available:

*/


:-switch_test('255', problem, [], ['fracas-problems']).
:-problem('255', id, '255').
:-problem('255', fracas_answer, "yes").
:-problem('255', p, "Since 1992 ITEL has made a loss.").
:-problem('255', p, "It is now 1996.").
:-problem('255', q, "Did ITEL make a loss in 1993?").
:-problem('255', h, "ITEL made a loss in 1993.").
:-problem('255', a, "Yes").


:-switch_test('256', problem, [], ['fracas-problems']).
:-problem('256', id, '256').
:-problem('256', fracas_answer, "unknown").
:-problem('256', fracas_nonstandard, "true").
:-problem('256', p, "ITEL has made a loss since 1992.").
:-problem('256', p, "It is now 1996.").
:-problem('256', q, "Did ITEL make a loss in 1993?").
:-problem('256', h, "ITEL made a loss in 1993.").
:-problem('256', a, "Don't know, on one reading of the premise").


:-switch_test('257', problem, [], ['fracas-problems']).
:-problem('257', id, '257').
:-problem('257', fracas_answer, "yes").
:-problem('257', fracas_nonstandard, "true").
:-problem('257', p, "ITEL has made a loss since 1992.").
:-problem('257', p, "It is now 1996.").
:-problem('257', q, "Did ITEL make a loss in 1993?").
:-problem('257', h, "ITEL made a loss in 1993.").
:-problem('257', a, "Yes, on one reading of the premise").
:-note(problem, ' Note that this is formally identical with preceding. ').


:-switch_test('258', problem, [], ['fracas-problems']).
:-problem('258', id, '258').
:-problem('258', fracas_answer, "no").
:-problem('258', p, "In March 1993 APCOM founded ITEL.").
:-problem('258', q, "Did ITEL exist in 1992?").
:-problem('258', h, "ITEL existed in 1992.").
:-problem('258', a, "No").


/*
 (258) involves the lexical semantics of found.

*/


/*
 7.2 Temporal Adverbials

*/


/*
 7.2.1 Indexicals

*/


/*
 Non-context dependent indexicals are reasonably straightforward:

*/


:-switch_test('259', problem, [], ['fracas-problems']).
:-problem('259', id, '259').
:-problem('259', fracas_answer, "yes").
:-problem('259', p, "The conference started on July 4th, 1994.").
:-problem('259', p, "It lasted 2 days.").
:-problem('259', q, "Was the conference over on July 8th, 1994?").
:-problem('259', h, "The conference was over on July 8th, 1994.").
:-problem('259', a, "Yes").


/*
 Context dependent indexicals (e.g. today, yesterday) are evaluated
with respect to some temporal reference point (e.g. now):

*/


:-switch_test('260', problem, [], ['fracas-problems']).
:-problem('260', id, '260').
:-problem('260', fracas_answer, "yes").
:-problem('260', p, "Yesterday APCOM signed the contract.").
:-problem('260', p, "Today is Saturday, July 14th.").
:-problem('260', q, "Did APCOM sign the contract Friday, 13th.?").
:-problem('260', h, "APCOM signed the contract Friday, 13th.").
:-problem('260', a, "Yes").
:-note(problem, ' The odd punctuation in the question was in the original. ').


/*
 7.2.2 "Before", "After" (Temporal Subordinate Clauses)

*/


/*

  Ignoring counterfactual readings, 'before' and 'after' have the following
  transitivity properties: if X, Y and Z are either all state or accomplishment
  or achievement or activity denoting sentences we have
    X < Y.
    Y < Z.
    X < Z.
  where < \in {before; after}


*/


:-switch_test('261', problem, [], ['fracas-problems']).
:-problem('261', id, '261').
:-problem('261', fracas_answer, "yes").
:-problem('261', p, "Smith left before Jones left.").
:-problem('261', p, "Jones left before Anderson left.").
:-problem('261', q, "Did Smith leave before Anderson left?").
:-problem('261', h, "Smith left before Anderson left.").
:-problem('261', a, "Yes").
:-note(problem, ' Original is degenerate problem; this is my fabrication. ').


:-switch_test('262', problem, [], ['fracas-problems']).
:-problem('262', id, '262').
:-problem('262', fracas_answer, "yes").
:-problem('262', p, "Smith left after Jones left.").
:-problem('262', p, "Jones left after Anderson left.").
:-problem('262', q, "Did Smith leave after Anderson left?").
:-problem('262', h, "Smith left after Anderson left.").
:-problem('262', a, "Yes").


/*
 In general transitivity does not hold when we mix aspectual classes in
the premises:

*/


:-switch_test('263', problem, [], ['fracas-problems']).
:-problem('263', id, '263').
:-problem('263', fracas_answer, "unknown").
:-problem('263', p, "Smith was present after Jones left.").
:-problem('263', p, "Jones left after Anderson was present.").
:-problem('263', q, "Was Smith present after Anderson was present?").
:-problem('263', h, "Smith was present after Anderson was present.").
:-problem('263', a, "Don't know").


/*
 If X and Y are either all accomplishment or achievement denoting
sentences with simple tenses 'before' and 'after' are inverses of each other:


*/


/*
 X before Y iff Y after X.

*/


:-switch_test('264', problem, [], ['fracas-problems']).
:-problem('264', id, '264').
:-problem('264', fracas_answer, "yes").
:-problem('264', p, "Smith left.").
:-problem('264', p, "Jones left.").
:-problem('264', p, "Smith left before Jones left.").
:-problem('264', q, "Did Jones leave after Smith left?").
:-problem('264', h, "Jones left after Smith left.").
:-problem('264', a, "Yes").
:-note(problem, ' Original is degenerate problem; this is my fabrication. ').


:-switch_test('265', problem, [], ['fracas-problems']).
:-problem('265', id, '265').
:-problem('265', fracas_answer, "yes").
:-problem('265', p, "Smith left.").
:-problem('265', p, "Jones left.").
:-problem('265', p, "Smith left after Jones left.").
:-problem('265', q, "Did Jones leave before Smith left?").
:-problem('265', h, "Jones left before Smith left.").
:-problem('265', a, "Yes").


:-switch_test('266', problem, [], ['fracas-problems']).
:-problem('266', id, '266').
:-problem('266', fracas_answer, "yes").
:-problem('266', p, "Smith left.").
:-problem('266', p, "Jones left.").
:-problem('266', p, "Jones left before Smith left.").
:-problem('266', q, "Did Smith leave after Jones left?").
:-problem('266', h, "Smith left after Jones left.").
:-problem('266', a, "Yes").


:-switch_test('267', problem, [], ['fracas-problems']).
:-problem('267', id, '267').
:-problem('267', fracas_answer, "yes").
:-problem('267', p, "Jones revised the contract.").
:-problem('267', p, "Smith revised the contract.").
:-problem('267', p, "Jones revised the contract before Smith did.").
:-problem('267', q, "Did Smith revise the contract after Jones did?").
:-problem('267', h, "Smith revised the contract after Jones did.").
:-problem('267', a, "Yes").


:-switch_test('268', problem, [], ['fracas-problems']).
:-problem('268', id, '268').
:-problem('268', fracas_answer, "yes").
:-problem('268', p, "Jones revised the contract.").
:-problem('268', p, "Smith revised the contract.").
:-problem('268', p, "Jones revised the contract after Smith did.").
:-problem('268', q, "Did Smith revise the contract before Jones did?").
:-problem('268', h, "Smith revised the contract before Jones did.").
:-problem('268', a, "Yes").


/*
 In general this is not so with activity verbs:

*/


:-switch_test('269', problem, [], ['fracas-problems']).
:-problem('269', id, '269').
:-problem('269', fracas_answer, "unknown").
:-problem('269', p, "Smith swam.").
:-problem('269', p, "Jones swam.").
:-problem('269', p, "Smith swam before Jones swam.").
:-problem('269', q, "Did Jones swim after Smith swam?").
:-problem('269', h, "Jones swam after Smith swam.").
:-problem('269', a, "Don't know").


/*
 However we do get

*/


:-switch_test('270', problem, [], ['fracas-problems']).
:-problem('270', id, '270').
:-problem('270', fracas_answer, "yes").
:-problem('270', p, "Smith swam to the shore.").
:-problem('270', p, "Jones swam to the shore.").
:-problem('270', p, "Smith swam to the shore before Jones swam to the shore.").
:-problem('270', q, "Did Jones swim to the shore after Smith swam to the shore?").
:-problem('270', h, "Jones swam to the shore after Smith swam to the shore.").
:-problem('270', a, "Yes").


/*

  Here the PP "to the shore" provides an end point or conclusion for the activity.


*/


/*
 "Before" and "after" are not inverses for state-denoting sentences:

*/


:-switch_test('271', problem, [], ['fracas-problems']).
:-problem('271', id, '271').
:-problem('271', fracas_answer, "unknown").
:-problem('271', p, "Smith was present.").
:-problem('271', p, "Jones was present.").
:-problem('271', p, "Smith was present after Jones was present.").
:-problem('271', q, "Was Jones present before Smith was present?").
:-problem('271', h, "Jones was present before Smith was present.").
:-problem('271', a, "Don't know").


:-switch_test('272', problem, [], ['fracas-problems']).
:-problem('272', id, '272').
:-problem('272', fracas_answer, "unknown").
:-problem('272', p, "Smith was present.").
:-problem('272', p, "Jones was present.").
:-problem('272', p, "Smith was present before Jones was present.").
:-problem('272', q, "Was Jones present after Smith was present?").
:-problem('272', h, "Jones was present after Smith was present.").
:-problem('272', a, "Don't know").


:-switch_test('273', problem, [], ['fracas-problems']).
:-problem('273', id, '273').
:-problem('273', fracas_answer, "unknown").
:-problem('273', p, "Smith was writing a report.").
:-problem('273', p, "Jones was writing a report.").
:-problem('273', p, "Smith was writing a report before Jones was writing a report.").
:-problem('273', q, "Was Jones writing a report after Smith was writing a report.?").
:-problem('273', h, "Jones was writing a report after Smith was writing a report.").
:-problem('273', a, "Don't know").


:-switch_test('274', problem, [], ['fracas-problems']).
:-problem('274', id, '274').
:-problem('274', fracas_answer, "unknown").
:-problem('274', p, "Smith was writing a report.").
:-problem('274', p, "Jones was writing a report.").
:-problem('274', p, "Smith was writing a report after Jones was writing a report.").
:-problem('274', q, "Was Jones writing a report before Smith was writing a report?").
:-problem('274', h, "Jones was writing a report before Smith was writing a report.").
:-problem('274', a, "Don't know").


/*
 Also "before", but not "after", can have a counterfactual
meaning. Whether this is a distinct sense of "before" is open to debate:

*/


:-switch_test('275', problem, [], ['fracas-problems']).
:-problem('275', id, '275').
:-problem('275', fracas_answer, "unknown").
:-problem('275', p, "Smith left the meeting before he lost his temper.").
:-problem('275', q, "Did Smith lose his temper?").
:-problem('275', h, "Smith lost his temper.").
:-problem('275', a, "Don't know").


/*

  With "when" things are even more complicated. The problem is that it
  is often very difficult to tease apart the temporal from the causal
  dimension of "when", c.f.


*/


:-switch_test('276', problem, [], ['fracas-problems']).
:-problem('276', id, '276').
:-problem('276', fracas_answer, "undef").
:-problem('276', fracas_nonstandard, "true").
:-problem('276', p, "When they opened the M25, traffic increased.").
:-problem('276', q, "").
:-problem('276', h, "").
:-problem('276', a, "").
:-note(problem, ' Original is degenerate problem; no question or answer given. ').


/*
 7.2.3 "In", "For" and "On" Temporal Adverbials

*/


/*
 "In" and "for" adverbials can be used as tests for the aspectual class of
verb phrases (or sentences).

*/


:-switch_test('277', problem, [], ['fracas-problems']).
:-problem('277', id, '277').
:-problem('277', fracas_answer, "unknown").
:-problem('277', p, "Smith lived in Birmingham in 1991.").
:-problem('277', q, "Did Smith live in Birmingham in 1992?").
:-problem('277', h, "Smith lived in Birmingham in 1992.").
:-problem('277', a, "Don't know").
:-problem('277', why, "Stative").


:-switch_test('278', problem, [], ['fracas-problems']).
:-problem('278', id, '278').
:-problem('278', fracas_answer, "no").
:-problem('278', p, "Smith wrote his first novel in 1991.").
:-problem('278', q, "Did Smith write his first novel in 1992?").
:-problem('278', h, "Smith wrote his first novel in 1992.").
:-problem('278', a, "No").
:-problem('278', why, "(Unrepeatable) accomplishment").


:-switch_test('279', problem, [], ['fracas-problems']).
:-problem('279', id, '279').
:-problem('279', fracas_answer, "no").
:-problem('279', p, "Smith wrote a novel in 1991.").
:-problem('279', q, "Did Smith write it in 1992?").
:-problem('279', h, "Smith wrote it in 1992.").
:-problem('279', a, "No").
:-problem('279', why, "(Unrepeatable) accomplishment").


:-switch_test('280', problem, [], ['fracas-problems']).
:-problem('280', id, '280').
:-problem('280', fracas_answer, "unknown").
:-problem('280', p, "Smith wrote a novel in 1991.").
:-problem('280', q, "Did Smith write a novel in 1992?").
:-problem('280', h, "Smith wrote a novel in 1992.").
:-problem('280', a, "Don't know").
:-problem('280', why, "(Repeatable) accomplishment").


:-switch_test('281', problem, [], ['fracas-problems']).
:-problem('281', id, '281').
:-problem('281', fracas_answer, "unknown").
:-problem('281', p, "Smith was running a business in 1991.").
:-problem('281', q, "Was Smith running it in 1992?").
:-problem('281', h, "Smith was running it in 1992.").
:-problem('281', a, "Don't know").
:-problem('281', why, "Activity").


:-switch_test('282', problem, [], ['fracas-problems']).
:-problem('282', id, '282').
:-problem('282', fracas_answer, "no").
:-problem('282', p, "Smith discovered a new species in 1991.").
:-problem('282', q, "Did Smith discover it in 1992?").
:-problem('282', h, "Smith discovered it in 1992.").
:-problem('282', a, "No").
:-problem('282', why, "(Unrepeatable) achievement").


:-switch_test('283', problem, [], ['fracas-problems']).
:-problem('283', id, '283').
:-problem('283', fracas_answer, "unknown").
:-problem('283', p, "Smith discovered a new species in 1991.").
:-problem('283', q, "Did Smith discover a new species in 1992?").
:-problem('283', h, "Smith discovered a new species in 1992.").
:-problem('283', a, "Don't know").
:-problem('283', why, "(Repeatable) achievement").


:-switch_test('284', problem, [], ['fracas-problems']).
:-problem('284', id, '284').
:-problem('284', fracas_answer, "yes").
:-problem('284', p, "Smith wrote a report in two hours.").
:-problem('284', p, "Smith started writing the report at 8 am.").
:-problem('284', q, "Had Smith finished writing the report by 11 am?").
:-problem('284', h, "Smith had finished writing the report by 11 am.").
:-problem('284', a, "Yes").
:-problem('284', why, "Accomplishment").


:-switch_test('285', problem, [], ['fracas-problems']).
:-problem('285', id, '285').
:-problem('285', fracas_answer, "unknown").
:-problem('285', p, "Smith wrote a report in two hours.").
:-problem('285', q, "Did Smith spend two hours writing the report?").
:-problem('285', h, "Smith spent two hours writing the report.").
:-problem('285', a, "Don't know").
:-problem('285', why, "Smith may have written the report in less than two hours. It is unclear whether there are two different readings for the premise: one where Smith takes exactly two hours, and one where he does it within two hours.").


:-switch_test('286', problem, [], ['fracas-problems']).
:-problem('286', id, '286').
:-problem('286', fracas_answer, "no").
:-problem('286', p, "Smith wrote a report in two hours.").
:-problem('286', q, "Did Smith spend more than two hours writing the report?").
:-problem('286', h, "Smith spent more than two hours writing the report.").
:-problem('286', a, "No").


:-switch_test('287', problem, [], ['fracas-problems']).
:-problem('287', id, '287').
:-problem('287', fracas_answer, "unknown").
:-problem('287', p, "Smith wrote a report in two hours.").
:-problem('287', q, "Did Smith write a report in one hour?").
:-problem('287', h, "Smith wrote a report in one hour.").
:-problem('287', a, "Don't know").


:-switch_test('288', problem, [], ['fracas-problems']).
:-problem('288', id, '288').
:-problem('288', fracas_answer, "yes").
:-problem('288', p, "Smith wrote a report in two hours.").
:-problem('288', q, "Did Smith write a report?").
:-problem('288', h, "Smith wrote a report.").
:-problem('288', a, "Yes").


:-switch_test('289', problem, [], ['fracas-problems']).
:-problem('289', id, '289').
:-problem('289', fracas_answer, "no").
:-problem('289', p, "Smith discovered a new species in two hours.").
:-problem('289', q, "Did Smith spend two hours discovering the new species?").
:-problem('289', h, "Smith spent two hours discovering the new species.").
:-problem('289', a, "No").
:-problem('289', why, "Achievements are typically (more or less) instantaneous").


:-switch_test('290', problem, [], ['fracas-problems']).
:-problem('290', id, '290').
:-problem('290', fracas_answer, "yes").
:-problem('290', p, "Smith discovered a new species in two hours.").
:-problem('290', q, "Did Smith discover a new species?").
:-problem('290', h, "Smith discovered a new species.").
:-problem('290', a, "Yes").


:-switch_test('291', problem, [], ['fracas-problems']).
:-problem('291', id, '291').
:-problem('291', fracas_answer, "yes").
:-problem('291', fracas_nonstandard, "true").
:-problem('291', p, "Smith discovered many new species in two hours.").
:-problem('291', q, "Did Smith spend two hours discovering new species?").
:-problem('291', h, "Smith spent two hours discovering new species.").
:-problem('291', a, "?Yes").
:-problem('291', why, "Repeated achievement can last two hours.").


:-switch_test('292', problem, [], ['fracas-problems']).
:-problem('292', id, '292').
:-problem('292', fracas_answer, "unknown").
:-problem('292', p, "Smith was running his own business in two years.").
:-problem('292', q, "Did Smith spend two years running his own business?").
:-problem('292', h, "Smith spent two years running his own business.").
:-problem('292', a, "Don't know").
:-problem('292', why, "Premise refers to time taken to inception of activity, not duration of activity.").


:-switch_test('293', problem, [], ['fracas-problems']).
:-problem('293', id, '293').
:-problem('293', fracas_answer, "unknown").
:-problem('293', p, "Smith was running his own business in two years.").
:-problem('293', q, "Did Smith spend more than two years running his own business?").
:-problem('293', h, "Smith spent more than two years running his own business.").
:-problem('293', a, "Don't know").
:-problem('293', why, "Cf. similar inference for accomplishment, (286)").


:-switch_test('294', problem, [], ['fracas-problems']).
:-problem('294', id, '294').
:-problem('294', fracas_answer, "yes").
:-problem('294', p, "Smith was running his own business in two years.").
:-problem('294', q, "Did Smith run his own business?").
:-problem('294', h, "Smith ran his own business.").
:-problem('294', a, "Yes").


:-switch_test('295', problem, [], ['fracas-problems']).
:-problem('295', id, '295').
:-problem('295', fracas_answer, "unknown").
:-problem('295', p, "In two years Smith owned a chain of businesses.").
:-problem('295', q, "Did Smith own a chain of business for two years?").
:-problem('295', h, "Smith owned a chain of business for two years.").
:-problem('295', a, "Don't know").
:-problem('295', why, "States behave like activities.").
:-note(problem, ' Sic: the original did have "chain of business" in the question. ').


:-switch_test('296', problem, [], ['fracas-problems']).
:-problem('296', id, '296').
:-problem('296', fracas_answer, "unknown").
:-problem('296', p, "In two years Smith owned a chain of businesses.").
:-problem('296', q, "Did Smith own a chain of business for more than two years?").
:-problem('296', h, "Smith owned a chain of business for more than two years.").
:-problem('296', a, "Don't know").
:-note(problem, ' Sic: the original did have "chain of business" in the question. ').


:-switch_test('297', problem, [], ['fracas-problems']).
:-problem('297', id, '297').
:-problem('297', fracas_answer, "yes").
:-problem('297', p, "In two years Smith owned a chain of businesses.").
:-problem('297', q, "Did Smith own a chain of business?").
:-problem('297', h, "Smith owned a chain of business.").
:-problem('297', a, "Yes").
:-note(problem, ' Sic: the original did have "chain of business" in the question. ').


:-switch_test('298', problem, [], ['fracas-problems']).
:-problem('298', id, '298').
:-problem('298', fracas_answer, "yes").
:-problem('298', p, "Smith lived in Birmingham for two years.").
:-problem('298', q, "Did Smith live in Birmingham for a year?").
:-problem('298', h, "Smith lived in Birmingham for a year.").
:-problem('298', a, "Yes").
:-problem('298', why, "State").


:-switch_test('299', problem, [], ['fracas-problems']).
:-problem('299', id, '299').
:-problem('299', fracas_answer, "no").
:-problem('299', p, "Smith lived in Birmingham for two years.").
:-problem('299', q, "Did Smith live in Birmingham for exactly a year?").
:-problem('299', h, "Smith lived in Birmingham for exactly a year.").
:-problem('299', a, "No").


:-switch_test('300', problem, [], ['fracas-problems']).
:-problem('300', id, '300').
:-problem('300', fracas_answer, "yes").
:-problem('300', p, "Smith lived in Birmingham for two years.").
:-problem('300', q, "Did Smith live in Birmingham?").
:-problem('300', h, "Smith lived in Birmingham.").
:-problem('300', a, "Yes").


:-switch_test('301', problem, [], ['fracas-problems']).
:-problem('301', id, '301').
:-problem('301', fracas_answer, "yes").
:-problem('301', p, "Smith ran his own business for two years.").
:-problem('301', q, "Did Smith run his own business for a year?").
:-problem('301', h, "Smith ran his own business for a year.").
:-problem('301', a, "Yes").
:-problem('301', why, "Activity").


:-switch_test('302', problem, [], ['fracas-problems']).
:-problem('302', id, '302').
:-problem('302', fracas_answer, "yes").
:-problem('302', p, "Smith ran his own business for two years.").
:-problem('302', q, "Did Smith run his own business?").
:-problem('302', h, "Smith ran his own business.").
:-problem('302', a, "Yes").


:-switch_test('303', problem, [], ['fracas-problems']).
:-problem('303', id, '303').
:-problem('303', fracas_answer, "yes").
:-problem('303', p, "Smith wrote a report for two hours.").
:-problem('303', q, "Did Smith write a report for an hour?").
:-problem('303', h, "Smith wrote a report for an hour.").
:-problem('303', a, "Yes").
:-problem('303', why, "Accomplishment").


:-switch_test('304', problem, [], ['fracas-problems']).
:-problem('304', id, '304').
:-problem('304', fracas_answer, "unknown").
:-problem('304', p, "Smith wrote a report for two hours.").
:-problem('304', q, "Did Smith write a report?").
:-problem('304', h, "Smith wrote a report.").
:-problem('304', a, "Don't know").
:-problem('304', why, "He may not have finished it").


:-switch_test('305', problem, [], ['fracas-problems']).
:-problem('305', id, '305').
:-problem('305', fracas_answer, "undef").
:-problem('305', fracas_nonstandard, "true").
:-problem('305', p, "Smith discovered a new species for an hour.").
:-problem('305', q, "").
:-problem('305', h, "").
:-problem('305', a, "").
:-note(problem, ' Original is degenerate problem; no question or answer given. ').


:-switch_test('306', problem, [], ['fracas-problems']).
:-problem('306', id, '306').
:-problem('306', fracas_answer, "yes").
:-problem('306', p, "Smith discovered new species for two years.").
:-problem('306', q, "Did Smith discover new species?").
:-problem('306', h, "Smith discovered new species.").
:-problem('306', a, "Yes").
:-problem('306', why, "Repeated achievement").


/*
 7.2.4 Quantificational Adverbials

*/


:-switch_test('307', problem, [], ['fracas-problems']).
:-problem('307', id, '307').
:-problem('307', fracas_answer, "yes").
:-problem('307', p, "In 1994 ITEL sent a progress report every month.").
:-problem('307', q, "Did ITEL send a progress report in July 1994?").
:-problem('307', h, "ITEL sent a progress report in July 1994.").
:-problem('307', a, "Yes").


/*
 Quantificational adverbials also introduce scope ambiguities with respect
to other quantified NPs

*/


:-switch_test('308', problem, [], ['fracas-problems']).
:-problem('308', id, '308').
:-problem('308', fracas_answer, "undef").
:-problem('308', fracas_nonstandard, "true").
:-problem('308', p, "Smith wrote to a representative every week.").
:-problem('308', q, "Is there a representative that Smith wrote to every week?").
:-problem('308', h, "There is a representative that Smith wrote to every week.").
:-problem('308', a, "Yes on one scoping; unknown on another scoping").


/*
 7.3 Anaphoric Dimension

*/


/*

  Rhetorical relations like narrative progression are defeasible
  interpretation principles. They depend on a theory of action and
  causation and general world knowledge (c.f. (309) and (310)).


*/


:-switch_test('309', problem, [], ['fracas-problems']).
:-problem('309', id, '309').
:-problem('309', fracas_answer, "undef").
:-problem('309', fracas_nonstandard, "true").
:-problem('309', p, "Smith left the house at a quarter past five.").
:-problem('309', p, "She took a taxi to the station and caught the first train to Luxembourg.").
:-problem('309', q, "").
:-problem('309', h, "").
:-problem('309', a, "").
:-note(problem, ' Original is degenerate problem; no question or answer given. ').


:-switch_test('310', problem, [], ['fracas-problems']).
:-problem('310', id, '310').
:-problem('310', fracas_answer, "undef").
:-problem('310', fracas_nonstandard, "true").
:-problem('310', p, "Smith lost some files.").
:-problem('310', p, "They were destroyed when her hard disk crashed.").
:-problem('310', q, "").
:-problem('310', h, "").
:-problem('310', a, "").
:-note(problem, ' Original is degenerate problem; no question or answer given. ').


:-switch_test('311', problem, [], ['fracas-problems']).
:-problem('311', id, '311').
:-problem('311', fracas_answer, "yes").
:-problem('311', p, "Smith had left the house at a quarter past five.").
:-problem('311', p, "Then she took a taxi to the station.").
:-problem('311', q, "Did Smith leave the house before she took a taxi to the station?").
:-problem('311', h, "Smith left the house before she took a taxi to the station.").
:-problem('311', a, "Yes").


/*
 7.4 Adverbs of Quantification

*/


:-switch_test('312', problem, [], ['fracas-problems']).
:-problem('312', id, '312').
:-problem('312', fracas_answer, "yes").
:-problem('312', p, "ITEL always delivers reports late.").
:-problem('312', p, "In 1993 ITEL delivered reports.").
:-problem('312', q, "Did ITEL delivered reports late in 1993?").
:-problem('312', h, "ITEL delivered reports late in 1993.").
:-problem('312', a, "Yes").
:-note(problem, ' Sic: the original did have "delivered" in the question. ').


:-switch_test('313', problem, [], ['fracas-problems']).
:-problem('313', id, '313').
:-problem('313', fracas_answer, "no").
:-problem('313', p, "ITEL never delivers reports late.").
:-problem('313', p, "In 1993 ITEL delivered reports.").
:-problem('313', q, "Did ITEL delivered reports late in 1993?").
:-problem('313', h, "ITEL delivered reports late in 1993.").
:-problem('313', a, "No").
:-note(problem, ' Sic: the original did have "delivered" in the question. ').


/*
 7.5 Some more Complex Examples

*/


:-switch_test('314', problem, [], ['fracas-problems']).
:-problem('314', id, '314').
:-problem('314', fracas_answer, "yes").
:-problem('314', p, "Smith arrived in Paris on the 5th of May, 1995.").
:-problem('314', p, "Today is the 15th of May, 1995.").
:-problem('314', p, "She is still in Paris.").
:-problem('314', q, "Was Smith in Paris on the 7th of May, 1995?").
:-problem('314', h, "Smith was in Paris on the 7th of May, 1995.").
:-problem('314', a, "Yes").


:-switch_test('315', problem, [], ['fracas-problems']).
:-problem('315', id, '315').
:-problem('315', fracas_answer, "yes").
:-problem('315', p, "When Smith arrived in Katmandu she had been travelling for three days.").
:-problem('315', q, "Had Smith been travelling the day before she arrived in Katmandu?").
:-problem('315', h, "Smith had been travelling the day before she arrived in Katmandu.").
:-problem('315', a, "Yes").


:-switch_test('316', problem, [], ['fracas-problems']).
:-problem('316', id, '316').
:-problem('316', fracas_answer, "yes").
:-problem('316', p, "Jones graduated in March and has been employed ever since.").
:-problem('316', p, "Jones has been unemployed in the past.").
:-problem('316', q, "Was Jones unemployed at some time before he graduated?").
:-problem('316', h, "Jones was unemployed at some time before he graduated.").
:-problem('316', a, "Yes").


:-switch_test('317', problem, [], ['fracas-problems']).
:-problem('317', id, '317').
:-problem('317', fracas_answer, "yes").
:-problem('317', p, "Every representative has read this report.").
:-problem('317', p, "No two representatives have read it at the same time.").
:-problem('317', p, "No representative took less than half a day to read the report.").
:-problem('317', p, "There are sixteen representatives.").
:-problem('317', q, "Did it take the representatives more than a week to read the report?").
:-problem('317', h, "It took the representatives more than a week to read the report.").
:-problem('317', a, "Yes").


:-switch_test('318', problem, [], ['fracas-problems']).
:-problem('318', id, '318').
:-problem('318', fracas_answer, "no").
:-problem('318', p, "While Jones was updating the program, Mary came in and told him about the board meeting.").
:-problem('318', p, "She finished before he did.").
:-problem('318', q, "Did Mary's story last as long as Jones's updating the program?").
:-problem('318', h, "Mary's story lasted as long as Jones's updating the program.").
:-problem('318', a, "No").


:-switch_test('319', problem, [], ['fracas-problems']).
:-problem('319', id, '319').
:-problem('319', fracas_answer, "yes").
:-problem('319', p, "Before APCOM bought its present office building, it had been paying mortgage interest on the previous one for 8 years.").
:-problem('319', p, "Since APCOM bought its present office building it has been paying mortgage interest on it for more than 10 years.").
:-problem('319', q, "Has APCOM been paying mortgage interest for a total of 15 years or more?").
:-problem('319', h, "APCOM has been paying mortgage interest for a total of 15 years or more.").
:-problem('319', a, "Yes").


:-switch_test('320', problem, [], ['fracas-problems']).
:-problem('320', id, '320').
:-problem('320', fracas_answer, "yes").
:-problem('320', p, "When Jones got his job at the CIA, he knew that he would never be allowed to write his memoirs.").
:-problem('320', q, "Is it the case that Jones is not and will never be allowed to write his memoirs?").
:-problem('320', h, "It is the case that Jones is not and will never be allowed to write his memoirs.").
:-problem('320', a, "Yes").


:-switch_test('321', problem, [], ['fracas-problems']).
:-problem('321', id, '321').
:-problem('321', fracas_answer, "yes").
:-problem('321', p, "Smith has been to Florence twice in the past.").
:-problem('321', p, "Smith will go to Florence twice in the coming year.").
:-problem('321', q, "Two years from now will Smith have been to Florence at least four times?").
:-problem('321', h, "Two years from now Smith will have been to Florence at least four times.").
:-problem('321', a, "Yes").


:-switch_test('322', problem, [], ['fracas-problems']).
:-problem('322', id, '322').
:-problem('322', fracas_answer, "yes").
:-problem('322', p, "Last week I already knew that when, in a month's time, Smith would discover that she had been duped she would be furious.").
:-problem('322', q, "Will it be the case that in a few weeks Smith will discover that she has been duped; and will she be furious?").
:-problem('322', h, "It will be the case that in a few weeks Smith will discover that she has been duped; and she will be furious.").
:-problem('322', a, "Yes").


:-switch_test('323', problem, [], ['fracas-problems']).
:-problem('323', id, '323').
:-problem('323', fracas_answer, "yes").
:-problem('323', p, "No one gambling seriously stops until he is broke.").
:-problem('323', p, "No one can gamble when he is broke.").
:-problem('323', q, "Does everyone who starts gambling seriously stop the moment he is broke?").
:-problem('323', h, "Everyone who starts gambling seriously stops the moment he is broke.").
:-problem('323', a, "Yes").


:-switch_test('324', problem, [], ['fracas-problems']).
:-problem('324', id, '324').
:-problem('324', fracas_answer, "yes").
:-problem('324', p, "No one who starts gambling seriously stops until he is broke.").
:-problem('324', q, "Does everyone who starts gambling seriously continue until he is broke?").
:-problem('324', h, "Everyone who starts gambling seriously continues until he is broke.").
:-problem('324', a, "Yes").


:-switch_test('325', problem, [], ['fracas-problems']).
:-problem('325', id, '325').
:-problem('325', fracas_answer, "yes").
:-problem('325', p, "Nobody who is asleep ever knows that he is asleep.").
:-problem('325', p, "But some people know that they have been asleep after they have been asleep.").
:-problem('325', q, "Do some people discover that they have been asleep?").
:-problem('325', h, "Some people discover that they have been asleep.").
:-problem('325', a, "Yes").


/*
 8 VERBS

*/


/*
 8.1 Aspectual Classes

*/


/*
 See also the inference pertaining to in and for adverbials.

*/


:-switch_test('326', problem, [], ['fracas-problems']).
:-problem('326', id, '326').
:-problem('326', fracas_answer, "yes").
:-problem('326', p, "ITEL built MTALK in 1993.").
:-problem('326', q, "Did ITEL finish MTALK in 1993?").
:-problem('326', h, "ITEL finished MTALK in 1993.").
:-problem('326', a, "Yes").


:-switch_test('327', problem, [], ['fracas-problems']).
:-problem('327', id, '327').
:-problem('327', fracas_answer, "unknown").
:-problem('327', p, "ITEL was building MTALK in 1993.").
:-problem('327', q, "Did ITEL finish MTALK in 1993?").
:-problem('327', h, "ITEL finished MTALK in 1993.").
:-problem('327', a, "Don't know").


:-switch_test('328', problem, [], ['fracas-problems']).
:-problem('328', id, '328').
:-problem('328', fracas_answer, "yes").
:-problem('328', p, "ITEL won the contract from APCOM in 1993.").
:-problem('328', q, "Did ITEL win a contract in 1993?").
:-problem('328', h, "ITEL won a contract in 1993.").
:-problem('328', a, "Yes").


:-switch_test('329', problem, [], ['fracas-problems']).
:-problem('329', id, '329').
:-problem('329', fracas_answer, "unknown").
:-problem('329', p, "ITEL was winning the contract from APCOM in 1993.").
:-problem('329', q, "Did ITEL win a contract in 1993?").
:-problem('329', h, "ITEL won a contract in 1993.").
:-problem('329', a, "Don't know").


:-switch_test('330', problem, [], ['fracas-problems']).
:-problem('330', id, '330').
:-problem('330', fracas_answer, "yes").
:-problem('330', p, "ITEL owned APCOM from 1988 to 1992.").
:-problem('330', q, "Did ITEL own APCOM in 1990?").
:-problem('330', h, "ITEL owned APCOM in 1990.").
:-problem('330', a, "Yes").


/*
 8.2 Distributive and Collective Predication

*/


:-switch_test('331', problem, [], ['fracas-problems']).
:-problem('331', id, '331').
:-problem('331', fracas_answer, "yes").
:-problem('331', p, "Smith and Jones left the meeting.").
:-problem('331', q, "Did Smith leave the meeting?").
:-problem('331', h, "Smith left the meeting.").
:-problem('331', a, "Yes").


:-switch_test('332', problem, [], ['fracas-problems']).
:-problem('332', id, '332').
:-problem('332', fracas_answer, "yes").
:-problem('332', p, "Smith and Jones left the meeting.").
:-problem('332', q, "Did Jones leave the meeting?").
:-problem('332', h, "Jones left the meeting.").
:-problem('332', a, "Yes").


:-switch_test('333', problem, [], ['fracas-problems']).
:-problem('333', id, '333').
:-problem('333', fracas_answer, "yes").
:-problem('333', p, "Smith, Anderson and Jones met.").
:-problem('333', q, "Was there a group of people that met?").
:-problem('333', h, "There was a group of people that met.").
:-problem('333', a, "Yes").


/*
 9 ATTITUDES

*/


/*
 9.1 Epistemic, Intentional and Reportive Attitudes

*/


:-switch_test('334', problem, [], ['fracas-problems']).
:-problem('334', id, '334').
:-problem('334', fracas_answer, "yes").
:-problem('334', p, "Smith knew that ITEL had won the contract in 1992.").
:-problem('334', q, "Did ITEL win the contract in 1992?").
:-problem('334', h, "ITEL won the contract in 1992.").
:-problem('334', a, "Yes").


:-switch_test('335', problem, [], ['fracas-problems']).
:-problem('335', id, '335').
:-problem('335', fracas_answer, "unknown").
:-problem('335', p, "Smith believed that ITEL had won the contract in 1992.").
:-problem('335', q, "Did ITEL win the contract in 1992?").
:-problem('335', h, "ITEL won the contract in 1992.").
:-problem('335', a, "Don't know").
:-note(problem, ' Sic: the original had "believed / said / denied / feared / hoped" in the premise.  I changed this to just "tried".\n  ').


:-switch_test('336', problem, [], ['fracas-problems']).
:-problem('336', id, '336').
:-problem('336', fracas_answer, "yes").
:-problem('336', p, "ITEL managed to win the contract in 1992.").
:-problem('336', q, "Did ITEL win the contract in 1992?").
:-problem('336', h, "ITEL won the contract in 1992.").
:-problem('336', a, "Yes").


:-switch_test('337', problem, [], ['fracas-problems']).
:-problem('337', id, '337').
:-problem('337', fracas_answer, "unknown").
:-problem('337', p, "ITEL tried to win the contract in 1992.").
:-problem('337', q, "Did ITEL win the contract in 1992?").
:-problem('337', h, "ITEL won the contract in 1992.").
:-problem('337', a, "Don't know").
:-note(problem, ' Sic: the original had "tried / wanted" in the premise.  I changed this to just "tried".\n  ').


:-switch_test('338', problem, [], ['fracas-problems']).
:-problem('338', id, '338').
:-problem('338', fracas_answer, "yes").
:-problem('338', p, "It is true that ITEL won the contract in 1992.").
:-problem('338', q, "Did ITEL win the contract in 1992?").
:-problem('338', h, "ITEL won the contract in 1992.").
:-problem('338', a, "Yes").


:-switch_test('339', problem, [], ['fracas-problems']).
:-problem('339', id, '339').
:-problem('339', fracas_answer, "no").
:-problem('339', p, "It is false that ITEL won the contract in 1992.").
:-problem('339', q, "Did ITEL win the contract in 1992?").
:-problem('339', h, "ITEL won the contract in 1992.").
:-problem('339', a, "No").


/*
 9.2 Preceptive Attitudes: "See" with Bare Infinitive Complements

*/


/*
 9.2.1 Inferences we do not get

*/


:-switch_test('340', problem, [], ['fracas-problems']).
:-problem('340', id, '340').
:-problem('340', fracas_answer, "unknown").
:-problem('340', p, "Smith saw Jones sign the contract.").
:-problem('340', p, "If Jones signed the contract, his heart was beating.").
:-problem('340', q, "Did Smith see Jones' heart beat?").
:-problem('340', h, "Smith saw Jones' heart beat.").
:-problem('340', a, "Don't know").


:-switch_test('341', problem, [], ['fracas-problems']).
:-problem('341', id, '341').
:-problem('341', fracas_answer, "unknown").
:-problem('341', p, "Smith saw Jones sign the contract.").
:-problem('341', p, "When Jones signed the contract, his heart was beating.").
:-problem('341', q, "Did Smith see Jones' heart beat?").
:-problem('341', h, "Smith saw Jones' heart beat.").
:-problem('341', a, "Don't know").


/*
 9.2.2 Veridicality

*/


/*
 a saw \phi < \phi

*/


:-switch_test('342', problem, [], ['fracas-problems']).
:-problem('342', id, '342').
:-problem('342', fracas_answer, "yes").
:-problem('342', p, "Smith saw Jones sign the contract.").
:-problem('342', q, "Did Jones sign the contract?").
:-problem('342', h, "Jones signed the contract.").
:-problem('342', a, "Yes").


/*
 9.2.3 Substitution

*/


/*
 a saw \phi(b), b = c < a saw \phi(c)

*/


:-switch_test('343', problem, [], ['fracas-problems']).
:-problem('343', id, '343').
:-problem('343', fracas_answer, "yes").
:-problem('343', p, "Smith saw Jones sign the contract.").
:-problem('343', p, "Jones is the chairman of ITEL.").
:-problem('343', q, "Did Smith see the chairman of ITEL sign the contract?").
:-problem('343', h, "Smith saw the chairman of ITEL sign the contract.").
:-problem('343', a, "Yes").


/*
 9.2.4 Existential instantiation

*/


/*
 a saw \phi(b) < 9x a saw \phi(x)

*/


:-switch_test('344', problem, [], ['fracas-problems']).
:-problem('344', id, '344').
:-problem('344', fracas_answer, "yes").
:-problem('344', p, "Helen saw the chairman of the department answer the phone.").
:-problem('344', p, "The chairman of the department is a person.").
:-problem('344', q, "Is there anyone whom Helen saw answer the phone?").
:-problem('344', h, "There is someone whom Helen saw answer the phone.").
:-problem('344', a, "Yes").


/*
 9.2.5 Conjunction distribution

*/


/*
 a saw \phi and / < a saw \phi and a saw /

*/


:-switch_test('345', problem, [], ['fracas-problems']).
:-problem('345', id, '345').
:-problem('345', fracas_answer, "yes").
:-problem('345', p, "Smith saw Jones sign the contract and his secretary make a copy.").
:-problem('345', q, "Did Smith see Jones sign the contract?").
:-problem('345', h, "Smith saw Jones sign the contract.").
:-problem('345', a, "Yes").


/*
 9.2.6 Disjunction distribution

*/


/*
 A saw \phi or / < A saw \phi or A saw /

*/


:-switch_test('346', problem, [], ['fracas-problems']).
:-problem('346', id, '346').
:-problem('346', fracas_answer, "yes").
:-problem('346', p, "Smith saw Jones sign the contract or cross out the crucial clause.").
:-problem('346', q, "Did Smith either see Jones sign the contract or see Jones cross out the crucial clause?").
:-problem('346', h, "Smith either saw Jones sign the contract or saw Jones cross out the crucial clause.").
:-problem('346', a, "Yes").


