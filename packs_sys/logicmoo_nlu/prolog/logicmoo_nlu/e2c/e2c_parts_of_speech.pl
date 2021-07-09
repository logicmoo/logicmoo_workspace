% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% Example code from the book "Natural Language Processing in Prolog" %
%   published by Addison Wesley   %
% Copyright (c) 1989, Gerald Gazdar & Christopher Mellish. %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
%
% flexicon.pl [Chapter 4] Example lexicon - makes extensive use of features
%
%?- op(1200, xfx, --->).
%?- op(30, xfx, ':').
%

% Verbs

type_wrd_sem((modal_verb), ( will ; '\'ll' ), [ polarity=positive, tense=future, modal_sem=(would)]).
type_wrd_sem((modal_verb), ( would ; '\'d' ), [ polarity=positive, modal_sem=(would)]).
type_wrd_sem((modal_verb), can,[ polarity=positive, modal_sem=(can)]).
type_wrd_sem((modal_verb), could,[ polarity=positive, modal_sem=(could)]).
type_wrd_sem((modal_verb), should,[ polarity=positive, modal_sem=(should)]).
type_wrd_sem((modal_verb), must, [polarity=positive, modal_sem=(must)]).
type_wrd_sem((modal_verb), may,[ polarity=positive, modal_sem=(may)]).

type_wrd_sem((modal_verb), ([will, (not ; 'n\'t')] ; 'won''t' ), [slash=n, polarity=negative, modal_sem=(will), tense=future]).
type_wrd_sem((modal_verb), [would, (not ; 'n\'t')], [polarity=negative, modal_sem=(would)]).
type_wrd_sem((modal_verb), ( cannot ; [can, (not ; 'n\'t')]), [polarity=negative, modal_sem=(can)]).
type_wrd_sem((modal_verb), [could, (not ; 'n\'t')], [polarity=negative, modal_sem=(could)]).
type_wrd_sem((modal_verb), [should, (not ; 'n\'t')],[ polarity=negative, modal_sem=(should)]).
type_wrd_sem((modal_verb), [must, (not ; 'n\'t')], [polarity=negative, modal_sem=(must)]).
type_wrd_sem((modal_verb), [may, (not ; 'n\'t')] , [polarity=negative, modal_sem=(may)]).

type_wrd_sem((aux_have_verb), ( have ; '\'ve' ), [form=base, agr=_AnyAgr, aspect=perfect]).
type_wrd_sem((aux_have_verb), ( has ; '\'s' ), [form=finite, person=3, agr=sg, tense=present, aspect=perfect]).
type_wrd_sem((aux_have_verb), ( have ; '\'ve' ), [form=finite, slash=t, agr=(3+sg), tense=present, aspect=perfect]).
type_wrd_sem((aux_have_verb), ( had ; '\'d' ), [form=finite, agr=_AnyAgr, tense=past, aspect=perfect]).

type_wrd_sem((aux_have_verb), [have, (not ; 'n\'t')], [form=base, agr=_AnyAgr, slash=n, polarity=negative, aspect=perfect]).
type_wrd_sem((aux_have_verb), [has, (not;'n\'t')], [form=finite, person=3, agr=sg, slash=n, polarity=negative, tense=present, aspect=perfect]).
type_wrd_sem((aux_have_verb), [have, (not ; 'n\'t')] , [form=finite, slash=t, agr=(3+sg), polarity=negative, tense=present, aspect=perfect]).
type_wrd_sem((aux_have_verb), [had, (not;'n\'t')], [form=finite, agr=_AnyAgr, slash=n, polarity=negative, tense=past, aspect=perfect]).

type_wrd_sem((verb), [do, does, did, done, doing],[subcat=nx0vvx1_modal, inv=y, vp_vform=base, takes_advp=y, takes_adv_type=_]).
type_wrd_sem((verb), 
  [('don\'t' ;[do, not]), ('doesn\'t' ;[does, not]), ('didn\'t' ;[did, not]), xxxxx, xxxxx], 
   [polarity=negative, subcat=nx0vvx1_modal, inv=_, vp_vform=base, takes_advp=y, takes_adv_type=_]).

type_wrd_sem((be_verb), [], [subcat=nx0vvx1_modal, vp_vform=ing, passive=_, subj_nform=_, sem=takes_no_pps]).

type_wrd_sem((be_verb), [], [subcat=nx0vvx1_passive, passive=y, sem=takes_no_pps]).

type_wrd_sem((be_verb), [sem=verb_sem(existential, there_is)], 
	 [subcat=nx0vnx1_there_is, subj_nform=there, obj_sem_n_type=agent+(isa=thing_type), takes_advp=y, 
	 takes_loc_pp=y, takes_time_pp=y, takes_date_pp=y]).

%macro(verb_like_want_to(Forms, Sem), sem=verb(Forms, Sem, [subcat=nx0vvx1_to, inv=n, vp_vform=base, sem=takes_no_pps])).

type_wrd_sem((verb_like_want_to), [want, wants, wanted, wanted, wanting], [ type=verb_sem, sem=action( want_to)]).
type_wrd_sem((verb_like_want_to), [like, likes, liked, liked, liking], [ type=verb_sem, sem=action( like_to)]).
type_wrd_sem((verb_like_want_to), [need, needs, needed, needed, needing], [ type=verb_sem, sem=action( need_to)]).
type_wrd_sem((verb_like_want_to), [have, has, had, had, having], [ type=verb_sem, sem=action( have_to)]).


% Nouns

type_wrd_sem(noun, [one, ones], [sem=noun_sem(noun, one), 
  sem_n_type=agent,  
  % (takes_det_type=(def+indef+quant+ordinal+null)),
  isa=thing_type]).

%macro(day_of_week_noun(Words, Sem), sem=noun(Words, [day, Sem], [date], [sem=takes_no_pps, takes_det_type=(def+indef+quant+ordinal+null)])).

type_wrd_sem((day_of_week_noun), [monday, mondays], monday).
type_wrd_sem((day_of_week_noun), [tuesday, tuesdays], tuesday).
type_wrd_sem((day_of_week_noun), [wednesday, wednesdays], wednesday).
type_wrd_sem((day_of_week_noun), [thursday, thursdays], thursday).       
type_wrd_sem((day_of_week_noun), [friday, fridays], friday).
type_wrd_sem((day_of_week_noun), [saturday, saturdays], saturday).
type_wrd_sem((day_of_week_noun), [sunday, sundays], sunday).


% Pronouns

type_wrd_sem(personal_pronoun, you, [sem=you, agr=(2+sg)+(2+pl), varg=_SubjOrNonsubj]).
type_wrd_sem(personal_pronoun, 'I', [sem=i, person=1, agr=sg, varg=subj]).
type_wrd_sem(personal_pronoun, me, [sem=i, person=1, agr=sg, varg=nonsubj]).
type_wrd_sem(personal_pronoun, we, [sem=we, person=1, agr=pl, varg=subj]).
type_wrd_sem(personal_pronoun, us, [sem=we, person=1, agr=pl, varg=nonsubj]).
type_wrd_sem(personal_pronoun, he, [sem=he, person=3, agr=sg, varg=subj]).
type_wrd_sem(personal_pronoun, him, [sem=he, person=3, agr=sg, varg=nonsubj]).
type_wrd_sem(personal_pronoun, she, [sem=she, person=3, agr=sg, varg=subj]).
type_wrd_sem(personal_pronoun, her, [sem=she, person=3, agr=sg, varg=nonsubj]).

type_wrd_sem((non_personal_pronoun), (it), [sem=it, person=3, agr=sg, varg=_SubjOrNonsubj]).
type_wrd_sem((personal_or_non_personal_pronoun), (they), [sem=they, person=3, agr=pl, varg=subj]).
type_wrd_sem((personal_or_non_personal_pronoun), (them), [sem=they, person=3, agr=pl, varg=nonsubj]).
type_wrd_sem((personal_or_non_personal_pronoun),[them, all], [sem=they, person=3, agr=pl, varg=subj]).
type_wrd_sem((personal_or_non_personal_pronoun),[them, both], [sem=they, person=3, agr=pl, varg=subj]).

type_wrd_sem((wh_personal_pronoun), who, [sem=who, agr=3, varg=(subj+nonsubj)]).
type_wrd_sem((wh_personal_pronoun), whom, [sem=who, agr=3, varg=nonsubj]).
% Where are you from
type_wrd_sem((wh_non_personal_pronoun), where, [sem=where, person=3, agr=sg, varg=subj+nonsubj]).

type_wrd_sem(relative_pronoun, that, [sem=_Any, varg=(subj+nonsubj)]).
type_wrd_sem(relative_pronoun, which, [sem=_Any, varg=(subj+nonsubj)]).
type_wrd_sem(relative_pronoun, who, [sem=agent, varg=(subj+nonsubj)]).
type_wrd_sem(relative_pronoun, whom, [sem=agent, varg=nonsubj]).             

type_wrd_sem(reflexive_pronoun, myself, [sem=i, person=1, agr=sg, isa=agent]).
type_wrd_sem(reflexive_pronoun, yourself, [sem=you, person=2, agr=sg, isa=agent]).
type_wrd_sem(reflexive_pronoun, himself, [sem=he, person=3, agr=sg, isa=agent]).
type_wrd_sem(reflexive_pronoun, herself, [sem=she, person=3, agr=sg, isa=agent]).
type_wrd_sem(reflexive_pronoun,[each, other], [sem=each_other, person=3, agr=sg, isa=agent]).
type_wrd_sem(reflexive_pronoun, itself, [sem=it, person=3, agr=sg, isa=thing_type]).
type_wrd_sem(reflexive_pronoun, ourselves, [sem=we, person=1, agr=pl, isa=agent]).
type_wrd_sem(reflexive_pronoun, themselves, [sem=they, person=3, agr=pl, isa=_Any]).

% NPs

% Impersonal "there" subject
type_wrd_sem((impersonal_there_subject), there, []).

type_wrd_sem(not_personal_quantified_pronoun, anything, [person=3, agr=sg, isa=anything]).
type_wrd_sem(not_personal_quantified_pronoun, something, [person=3, agr=sg, isa=something]).
type_wrd_sem(not_personal_quantified_pronoun, everything, [person=3, agr=sg, isa=everything]).
type_wrd_sem(not_personal_quantified_pronoun, nothing, [person=3, agr=sg, isa=nothing]).

type_wrd_sem(personal_quantified_pronoun, ( anyone ; anybody), [person=3, agr=sg, isa=anyone]).
type_wrd_sem(personal_quantified_pronoun, ( someone ; somebody), [person=3, agr=sg, isa=someone]).
type_wrd_sem(personal_quantified_pronoun, ( everyone ; everybody), [person=3, agr=sg, isa=everyone]).
type_wrd_sem(personal_quantified_pronoun, ([no, one] ; nobody ), [person=3, agr=sg, isa=noone]).

% Possessives

type_wrd_sem((possessive), my, [sem=i, isa=agent]).
type_wrd_sem((possessive), your, [sem=you, isa=agent]).
type_wrd_sem((possessive), his, [sem=he, isa=agent]).
type_wrd_sem((possessive), her, [sem=she, isa=agent]).
type_wrd_sem((possessive), its, [sem=it, isa=thing_type]).
type_wrd_sem((possessive), our, [sem=we, isa=agent]).
type_wrd_sem((possessive), their, [sem=they, isa=thing_type]).

type_wrd_sem((possessive_pronoun), mine, [sem=i]).
type_wrd_sem((possessive_pronoun), yours, [sem=you]).
type_wrd_sem((possessive_pronoun), his, [sem=he]).
type_wrd_sem((possessive_pronoun), hers, [sem=she]).
type_wrd_sem((possessive_pronoun), its, [sem=it]).
type_wrd_sem((possessive_pronoun), ours, [sem=we]).
type_wrd_sem((possessive_pronoun), theirs, [sem=they]).


% As in "three times a week"
% type_wrd_sem6((uninflected_noun), times, [frequency, time], [timeunit], pl, [takes_frequency_pp=y, takes_det_type=(numeric+quant)]).

% Subordinating conjunctions

type_wrd_wrd_sem6((sc), if, if, [time], (s+vp), []).
type_wrd_wrd_sem6((sc), when, when, [time], (s+vp), []).
type_wrd_wrd_sem6((sc), while, while, [time], (s+vp), []).
type_wrd_wrd_sem6((sc), before, before, [time], (s+vp), []).
type_wrd_wrd_sem6((sc), after, after, [time], (s+vp), []).
type_wrd_wrd_sem6((sc), since, since, [time], (s+vp), []).


% Prepositions

%p:[sem=prep_sem(subj), sem_pp_type=passive_by, obj_sem_n_type=_], by).

type_wrd_sem5((preposition), on, on_date, [date, date], []).
type_wrd_sem5((preposition), during, during, [date, date+timeunit], []).
type_wrd_sem5((preposition), around, around_date, [date, date], []).
type_wrd_sem5((preposition), around, around_time, [time, time], []).
type_wrd_sem5((preposition), before, before_date, [date, date], []).
type_wrd_sem5((preposition), before, before_time, [time, time], []).
type_wrd_sem5((preposition), after, after_date, [date, date], []).
type_wrd_sem5((preposition), after, after_time, [time, time], []).

% PPs

type_wrd_frm5((lexical_pp), here, [loc, here], [loc], []).
type_wrd_frm5((lexical_pp), there, [loc, there], [loc], []).
type_wrd_frm5((wh_pp), where, [loc, where], [loc], []).
type_wrd_frm5((wh_pp), when, [time, when], [time], []).
type_wrd_frm5((wh_pp), how, [manner, how], [attribution], []).
type_wrd_frm5((wh_pp), why, [reason, why], [attribution], []).
type_wrd_frm5((wh_pp), (for, how, long), [duration, how_long], [duration], []).



% "Between" is unusual in requiring a plal NP
flexicon(p_with_agr, [sem=prep_sem(between_dates), agr=pl, sem_pp_type=date, obj_sem_n_type=date], between).
flexicon(p_with_agr, [sem=prep_sem(between_times), agr=pl, sem_pp_type=time, obj_sem_n_type=time], between).
flexicon((v), [sem=will, tense=future,  
   subcat=nx0vvx1_modal, vform=ing, vp_vform=base, passive=_, 
    sem=takes_no_pps,  takes_advp=n, subj_nform=_, can_be_gerund=n], ([going, to] ; gonna )).
                             

% "You idiot"

flexicon(you_intro, [], you).

% "You guys"

flexicon(you_np_intro, [sem=you, case=_Any], you).

% Comp

flexicon(comp, [sem=embedded_dcl_comp], that).
flexicon(comp, [sem=embedded_q], if).
flexicon(comp, [sem=embedded_q], whether).

% Dets

flexicon(det, [sem=the_sing, agr=sg, article=y, wh=n, det_type=def, preagr=n], the).
flexicon(det, [sem=the_pl, agr=pl, article=y, wh=n, det_type=def, preagr=y], the).
flexicon(det, [sem=a, agr=sg, article=y, wh=n, det_type=indef, preagr=n], [( a ; an) ]).

flexicon(det, [sem=such_a, agr=sg, article=n, wh=n, det_type=indef, preagr=n], [such, (a ; an)]).

flexicon(det, [sem=this, agr=sg, wh=n, det_type=def, preagr=n, can_be_np=y], this).
flexicon(det, [sem=that, agr=sg, wh=n, det_type=def, preagr=n, can_be_np=y], that).
flexicon(det, [sem=these, agr=pl, wh=n, det_type=def, preagr=n, can_be_np=y], these).
flexicon(det, [sem=those, agr=pl, wh=n, det_type=def, preagr=n, can_be_np=y], those).

flexicon(det, [sem=the_same, agr=sg, wh=n, det_type=def, preagr=n, can_be_np=y], [the, same]).
flexicon(det, [sem=the_same, agr=pl, wh=n, det_type=def, preagr=y, can_be_np=n], [the, same]).

flexicon(det, [sem=next, agr=sg, wh=n, det_type=ordinal, preagr=y], next).
flexicon(det, [sem=the_next, agr=(sg+pl), wh=n, det_type=ordinal, preagr=y], [the, next]).
flexicon(det, [sem=last, agr=sg, wh=n, det_type=ordinal, preagr=y], last).
flexicon(det, [sem=the_last, agr=(sg+pl), wh=n, det_type=ordinal, preagr=y], [the, last]).
flexicon(det, [sem=previous, agr=sg, wh=n, det_type=ordinal, preagr=y], previous).
flexicon(det, [sem=the_previous, agr=(sg+pl), wh=n, det_type=ordinal, preagr=y], [the, previous]).


flexicon(det, [sem=det_number_sem(1), agr=sg, article=n, wh=n, det_type=numeric, can_be_np=n, preagr=n], [( a ; an )]).

flexicon(det, [sem=any, agr=(sg+pl), wh=n, det_type=quant, preagr=n, can_be_np=y], any).
flexicon(det, [sem=some, agr=(sg+pl), wh=n, det_type=quant, preagr=n, can_be_np=y], some).
flexicon(det, [sem=some_more, agr=(sg+pl), wh=n, det_type=quant, preagr=n, can_be_np=y], [some, more]).
flexicon(det, [sem=the_other, agr=sg, wh=n, det_type=quant, preagr=n, can_be_np=y], [the, other]).
flexicon(det, [sem=the_other, agr=pl, wh=n, det_type=quant, preagr=y, can_be_np=n], [the, other]).
flexicon(det, [sem=any_other, agr=(sg+pl), wh=n, det_type=quant, preagr=n, can_be_np=y], [any, other]).
flexicon(det, [sem=other, agr=sg, wh=n, det_type=indef, preagr=n, can_be_np=y], another).
flexicon(det, [sem=other, agr=pl, wh=n, det_type=indef, preagr=y, can_be_np=n], another).
flexicon(det, [sem=other, agr=pl, wh=n, det_type=quant, preagr=n, can_be_np=n], other).
flexicon(det, [sem=no, agr=(sg+pl), wh=n, det_type=quant, preagr=n, can_be_np=n], no).

flexicon(det, [sem=both, agr=pl, wh=n, det_type=quant, preagr=n, can_be_np=y], both).
flexicon(det, [sem=all, agr=pl, wh=n, det_type=quant, preagr=y, can_be_np=y], all).
flexicon(det, [sem=every, agr=sg, wh=n, det_type=quant, preagr=y, can_be_np=n], every).
flexicon(det, [sem=all_the, agr=pl, wh=n, det_type=quant, preagr=y, can_be_np=n], [all, the]).
flexicon(det, [sem=all_the, agr=pl, wh=n, det_type=quant, preagr=y, can_be_np=n], [all, of, the]).
flexicon(det, [sem=a_few, agr=pl, wh=n, det_type=quant, preagr=n, can_be_np=y], [a, few]).
flexicon(det, [sem=several, agr=pl, wh=n, det_type=quant, preagr=n, can_be_np=y], several).

flexicon(det, [sem=more, agr=(sg+pl), wh=n, det_type=quant, preagr=n, can_be_np=y], more).

% "what" must be singular when it's an NP
% It can also be WH- in e.g. "it's not what you think"
flexicon(det, [sem=what, agr=sg, wh=(y+n), det_type=quant, preagr=n, can_be_np=y], what).		
flexicon(det, [sem=what, agr=pl, wh=y, det_type=quant, preagr=n, can_be_np=n], what).
flexicon(det, [sem=which, agr=(sg+pl), wh=y, det_type=quant, preagr=n, can_be_np=y], which).
flexicon(det, [sem=how_many, agr=pl, wh=y, det_type=quant, preagr=n, can_be_np=y], [how, many]).
flexicon(det, [sem=how_much, agr=sg, wh=y, det_type=quant, preagr=n, can_be_np=y], [how, much]).

flexicon(det, [agr=sg], a).
flexicon(det, [agr=sg], this).
flexicon(det, [agr=pl], these).
flexicon(det, [agr=_], the).
flexicon(det, [agr=_], her).

flexicon(np, [person=3, agr=sg, gender=masc, case=nominative], he).
flexicon(np, [person=3, agr=sg, gender=fem, case=nominative], she).
flexicon(np, [person=3, agr=sg, gender=masc, case=accusative], him).
flexicon(np, [person=3, agr=sg, gender=fem, case=accusative], her).
flexicon(np, [person=3, agr=sg, gender=none, case=_], it).
flexicon(np, [person=3, agr=pl, gender=_, case=nominative], they).
flexicon(np, [person=3, agr=pl, gender=_, case=accusative], them).
flexicon(np, [person=2, agr=_, gender=_, case=_], you).
flexicon(np, [person=1, agr=sg, gender=_, case=nominative], i).
flexicon(np, [person=1, agr=sg, gender=_, case=accusative], me).
flexicon(np, [person=1, agr=pl, gender=_, case=nominative], we).
flexicon(np, [person=1, agr=pl, gender=_, case=accusative], us).



flexicon(np, [person=3, agr=sg, gender=_, case=_], kim).
flexicon(np, [person=3, agr=sg, gender=_, case=_], sandy).
flexicon(np, [person=3, agr=sg, gender=_, case=_], lee).
flexicon(np, [person=3, agr=sg, gender=masc, case=_], john).
flexicon(np, [person=3, agr=sg, gender=fem, case=_], mary).

flexicon(nn, [agr=_, gender=_], sheep).
flexicon(nn, [agr=_, gender=_], fish).
flexicon(nn, [agr=pl, gender=none], scissors).

flexicon(nn, [agr=sg, gender=_], duck).
flexicon(nn, [agr=pl, gender=_], ducks).

flexicon(nn, [agr=sg, gender=none], saw).
flexicon(nn, [agr=pl, gender=none], saws).

flexicon(nn, [agr=sg, gender=_], child).
flexicon(nn, [agr=pl, gender=_], children).

flexicon(nn, [agr=sg, gender=male], man).
flexicon(nn, [agr=pl, gender=male], men).

flexicon(nn, [agr=sg, gender=female], woman).
flexicon(nn, [agr=pl, gender=female], women).


flexicon(bv, [person=_, agr=_, verb_form=infinitive], be).
flexicon(bv, [person=1, agr=sg, verb_form=tensed], am).
flexicon(bv, [person=2, agr=sg, verb_form=tensed], are).
flexicon(bv, [person=3, agr=sg, verb_form=tensed], is).
flexicon(bv, [person=_, agr=pl, verb_form=tensed], are).
flexicon(bv, [person=1, agr=sg, verb_form=tensed], was).
flexicon(bv, [person=2, agr=sg, verb_form=tensed], were).
flexicon(bv, [person=3, agr=sg, verb_form=tensed], was).
flexicon(bv, [person=_, agr=pl, verb_form=tensed], were).

flexicon(iv, [person=_, agr=_, verb_form=infinitive], duck).
flexicon(iv, [person=1, agr=sg, verb_form=tensed], duck).
flexicon(iv, [person=2, agr=sg, verb_form=tensed], duck).
flexicon(iv, [person=3, agr=sg, verb_form=tensed], ducks).
flexicon(iv, [person=_, agr=pl, verb_form=tensed], duck).
flexicon(iv, [person=_, agr=_, verb_form=tensed], ducked).

flexicon(iv, [person=_, agr=_, verb_form=infinitive], die).
flexicon(iv, [person=1, agr=sg, verb_form=tensed], die).
flexicon(iv, [person=2, agr=sg, verb_form=tensed], die).
flexicon(iv, [person=3, agr=sg, verb_form=tensed], dies).
flexicon(iv, [person=_, agr=pl, verb_form=tensed], die).
flexicon(iv, [person=_, agr=_, verb_form=tensed], died).

flexicon(tv, [person=_, agr=_, verb_form=infinitive], eat).
flexicon(tv, [person=1, agr=sg, verb_form=tensed], eat).
flexicon(tv, [person=2, agr=sg, verb_form=tensed], eat).
flexicon(tv, [person=3, agr=sg, verb_form=tensed], eats).
flexicon(tv, [person=_, agr=pl, verb_form=tensed], eat).
flexicon(tv, [person=_, agr=_, verb_form=tensed], ate).

flexicon(tv, [person=_, agr=_, verb_form=infinitive], see).
flexicon(tv, [person=1, agr=sg, verb_form=tensed], see).
flexicon(tv, [person=2, agr=sg, verb_form=tensed], see).
flexicon(tv, [person=3, agr=sg, verb_form=tensed], sees).
flexicon(tv, [person=_, agr=pl, verb_form=tensed], see).
flexicon(tv, [person=_, agr=_, verb_form=tensed], saw).

flexicon(xv, [person=_, agr=_, verb_form=infinitive], see).
flexicon(xv, [person=1, agr=sg, verb_form=tensed], see).
flexicon(xv, [person=2, agr=sg, verb_form=tensed], see).
flexicon(xv, [person=3, agr=sg, verb_form=tensed], sees).
flexicon(xv, [person=_, agr=pl, verb_form=tensed], see).
flexicon(xv, [person=_, agr=_, verb_form=tensed], saw).

flexicon(adj, [agr=_, gender=_], stupid).
flexicon(adj, [agr=pl, gender=_], numerous).
flexicon(adj, [agr=_, gender=female], pregnant).

flexicon(wh, [gender=male], who).
flexicon(wh, [gender=female], who).
flexicon(wh, [gender=_], that).


% Interjections

flexicon(interjection, [sem=yes], yes).
flexicon(interjection, [sem=yes], yeah).
flexicon(interjection, [sem=yes], affirmative).
flexicon(interjection, [sem=yes], sure).
flexicon(interjection, [sem=yes], correct).
flexicon(interjection, [sem=yes], right).
flexicon(interjection, [sem=yes], fine).


flexicon(interjection, [sem=no], no).
flexicon(interjection, [sem=no], [no, no]).
flexicon(interjection, [sem=no], negative).
%interjection:[sem=no], cancel).
flexicon(interjection, [sem=no], wrong).

flexicon(interjection, [sem=yes], [that, '\'s', right]).
flexicon(interjection, [sem=yes], [that, '\'s', fine]).

flexicon(interjection, [sem=okay], okay).

flexicon(interjection, [sem=correction], [no, i, said]).
flexicon(interjection, [sem=correction], [no, i, meant]).

flexicon(interjection, [sem=interjection_sem(whatever), interjection_type=whatever], whatever).

% Politeness

flexicon(politeness, [(sem = politeness_sem(please)),    
   % stype=(dcl+imp+ynq+whq+ellipsis)
   politeness_pos=post], please).

flexicon(politeness, 
 [(sem = politeness_sem(please)), 
  % stype=(imp+ynq+whq),
   politeness_pos=pre],
   please).

flexicon(politeness, [sem=thank_you_post], [thank, you]).
flexicon(politeness, [sem=thank_you_post], thanks).

flexicon(politeness, [sem=thank_you_pre], [thank, you, very, much]).
flexicon(politeness, [sem=thank_you_pre], [thank, you, so, much]).
flexicon(politeness, [sem=thank_you_pre], [thank, you]).
flexicon(politeness, [sem=thank_you_pre], thanks).
flexicon(politeness, [sem=thank_you_pre], [thanks, a, lot]).

flexicon(politeness, [sem=sorry_pre], sorry).
flexicon(politeness, [sem=sorry_post], sorry).
flexicon(politeness, [sem=sorry_pre], [excuse, me]).
flexicon(politeness, [sem=sorry_post], [excuse, me]).

flexicon(politeness, [sem=salutation, salutation_txt=(hello)], hello).
flexicon(politeness, [sem=salutation, salutation_txt=(good_morning)], [good, morning]).
flexicon(politeness, [sem=salutation, salutation_txt=(good_afternoon)], [good, afternoon]).
flexicon(politeness, [sem=salutation, salutation_txt=(good_evening)], [good, evening]).
flexicon(politeness, [sem=salutation, salutation_txt=(goodbye)], [( goodbye ; bye ;[bye, bye] ) ]).

flexicon(politeness, [sem=salutation, salutation_txt=(congratulations)], congratulations).
flexicon(politeness, [sem=salutation, salutation_txt=(good_job)], [good, job]).
flexicon(politeness, [sem=salutation, salutation_txt=(good_going)], [good, going]).
flexicon(politeness, [sem=salutation, salutation_txt=(great_job)], [great, job]).
flexicon(politeness, [sem=salutation, salutation_txt=(great)], great).
flexicon(politeness, [sem=salutation, salutation_txt=(greetings)], greetings).
flexicon(politeness, [sem=salutation, salutation_txt=(ha_ha)], [ha, ha]).
flexicon(politeness, [sem=salutation, salutation_txt=(happy_birthday)], [happy, birthday ]).
flexicon(politeness, [sem=salutation, salutation_txt=(hey)], hey).
flexicon(politeness, [sem=salutation, salutation_txt=(hi)], hi).
flexicon(politeness, [sem=salutation, salutation_txt=(howdy)], howdy).
flexicon(politeness, [sem=salutation, salutation_txt=(konichiwa)], konichiwa).
flexicon(politeness, [sem=salutation, salutation_txt=(sucks)], sucks).
flexicon(politeness, [sem=salutation, salutation_txt=(welcome)], welcome).
flexicon(politeness, [sem=salutation, salutation_txt=(whats_up)], [what, '\'s', up]).
flexicon(politeness, [sem=salutation, salutation_txt=(what_the_fuck)], [what, the, fuck]).
%politeness:[sem=salutation, salutation_txt=()], []).

flexicon(politeness, [sem=how_about], [how, about]).
flexicon(politeness, [sem=how_about], [what, about]).

flexicon(politeness, [sem=conj_intro], and).

% (Could you please...)
flexicon(adv, [sem=adv_sem(politeness, please), advpos=postv, adv_type=politeness], please).

% NP intro and after

flexicon(np_after, [], [as, well]).
flexicon(np_after, [], too).


% Numcompares (stupid name, but I can't think of a better one)

flexicon(numcompare, [sem=the_next], [the, next]).
flexicon(numcompare, [sem=the_last], [the, last]).
flexicon(numcompare, [sem=over], over).
flexicon(numcompare, [sem=under], under).
flexicon(numcompare, [sem=at_least], [at, least]).
flexicon(numcompare, [sem=at_most], [at, most]).
flexicon(numcompare, [sem=more_than], [( more ; higher ; greater ), than]).
flexicon(numcompare, [sem=less_than], [( less ; lower), than]).
flexicon(numcompare, [sem=not_more_than], [not, more, than]).
flexicon(numcompare, [sem=not_less_than], [not, less, than]).
flexicon(numcompare, [sem=another], another).

% Conj
flexicon(conj, [sem=conj(and,_)], and).
flexicon(conj, [sem=conj(and,after)], [and, then]).
flexicon(conj, [sem=conj(or,_)], or).

% Months

flexicon(month, [sem=1], january).
flexicon(month, [sem=2], february).
flexicon(month, [sem=3], march).
flexicon(month, [sem=4], april).
flexicon(month, [sem=5], may).
flexicon(month, [sem=6], june).
flexicon(month, [sem=7], july).
flexicon(month, [sem=8], august).
flexicon(month, [sem=9], september).
flexicon(month, [sem=10], october).
flexicon(month, [sem=11], november).
flexicon(month, [sem=12], december).

% Times

flexicon(timesuffix, [sem=morning, allows_minutes=y], [(a, m)]).
flexicon(timesuffix, [sem=afternoon, allows_minutes=y], [(p, m)]).
flexicon(timesuffix, [sem=any, allows_minutes=n], ['o\'clock']).

flexicon(timesuffix, [sem=morning, allows_minutes=y], [(in, the, morning)]).
flexicon(timesuffix, [sem=afternoon, allows_minutes=y], [(in, the, afternoon)]).


% Just list a few years around the present, since they are easy to confuse
flexicon(year,[sem=year( 1997)], [nineteen, ninety, seven]).
flexicon(year,[sem=year( 1998)], [nineteen, ninety, eight]).
flexicon(year,[sem=year( 1999)], [nineteen, ninety, nine]).
flexicon(year,[sem=year( 2000)], [two, thousand]).
flexicon(year,[sem=year( 2001)], [two, thousand, and, one]).
flexicon(year,[sem=year( 2002)], [two, thousand, and, two]).
flexicon(year,[sem=year( 2003)], [two, thousand, and, three]).
flexicon(year,[sem=year( 2004)], [two, thousand, and, four]).
flexicon(year,[sem=year( 2005)], [two, thousand, and, five]).
flexicon(year,[sem=year( 2006)], [two, thousand, and, six]).
flexicon(year,[sem=year( 2007)], [two, thousand, and, seven]).
flexicon(year,[sem=year( 2008)], [two, thousand, and, eight]).
flexicon(year,[sem=year( 2009)], [two, thousand, and, nine]).
flexicon(year,[sem=year( 2010)], [two, thousand, and, ten]).

% Times
flexicon(time, [sem=time_number_hundred_sem(12)], noon).

flexicon((frequency_np), once, once).
flexicon((frequency_np), at_least_once, [at, least, once]).
flexicon((frequency_np), more_than_once, [more, than, once]).
flexicon((frequency_np), twice, twice).
flexicon((frequency_np), at_least_twice, [at, least, twice]).
flexicon((frequency_np), more_than_twice, [more, than, twice]).



% Ordinals

flexicon(ordinal, [sem=1, num_type=digit], first).
flexicon(ordinal, [sem=2, num_type=digit], second).
flexicon(ordinal, [sem=3, num_type=digit], third).
flexicon(ordinal, [sem=4, num_type=digit], fourth).
flexicon(ordinal, [sem=5, num_type=digit], fifth).
flexicon(ordinal, [sem=6, num_type=digit], sixth).
flexicon(ordinal, [sem=7, num_type=digit], seventh).
flexicon(ordinal, [sem=8, num_type=digit], eighth).
flexicon(ordinal, [sem=9, num_type=digit], ninth).

flexicon(ordinal, [sem=10, num_type=ten], tenth).

flexicon(ordinal, [sem=11, num_type=teen], eleventh).
flexicon(ordinal, [sem=12, num_type=teen], twelfth).
flexicon(ordinal, [sem=13, num_type=teen], thirteenth).
flexicon(ordinal, [sem=14, num_type=teen], fourteenth).
flexicon(ordinal, [sem=15, num_type=teen], fifteenth).
flexicon(ordinal, [sem=16, num_type=teen], sixteenth).
flexicon(ordinal, [sem=17, num_type=teen], seventeenth).
flexicon(ordinal, [sem=18, num_type=teen], eighteenth).
flexicon(ordinal, [sem=19, num_type=teen], nineteenth).

flexicon(ordinal, [sem=20, num_type='xt20to99'], twentieth).
flexicon(ordinal, [sem=21, num_type='xt20to99'], [twenty, first]).
flexicon(ordinal, [sem=22, num_type='xt20to99'], [twenty, second]).
flexicon(ordinal, [sem=23, num_type='xt20to99'], [twenty, third]).
flexicon(ordinal, [sem=24, num_type='xt20to99'], [twenty, fourth]).
flexicon(ordinal, [sem=25, num_type='xt20to99'], [twenty, fifth]).
flexicon(ordinal, [sem=26, num_type='xt20to99'], [twenty, sixth]).
flexicon(ordinal, [sem=27, num_type='xt20to99'], [twenty, seventh]).
flexicon(ordinal, [sem=28, num_type='xt20to99'], [twenty, eighth]).
flexicon(ordinal, [sem=29, num_type='xt20to99'], [twenty, ninth]).
flexicon(ordinal, [sem=30, num_type='xt20to99'], thirtieth).
flexicon(ordinal, [sem=31, num_type='xt20to99'], [thirty, first]).

flexicon(ordinal, [sem=40, num_type='xt20to99'], fortieth).
flexicon(ordinal, [sem=50, num_type='xt20to99'], fiftieth).
flexicon(ordinal, [sem=60, num_type='xt20to99'], sixtieth).
flexicon(ordinal, [sem=70, num_type='xt20to99'], seventieth).
flexicon(ordinal, [sem=80, num_type='xt20to99'], eightieth).
flexicon(ordinal, [sem=90, num_type='xt20to99'], ninetieth).
flexicon(ordinal, [sem=32, num_type='xt20to99'], [thirty, second]).
flexicon(ordinal, [sem=33, num_type='xt20to99'], [thirty, third]).
flexicon(ordinal, [sem=34, num_type='xt20to99'], [thirty, fourth]).
flexicon(ordinal, [sem=35, num_type='xt20to99'], [thirty, fifth]).
flexicon(ordinal, [sem=36, num_type='xt20to99'], [thirty, sixth]).
flexicon(ordinal, [sem=37, num_type='xt20to99'], [thirty, seventh]).
flexicon(ordinal, [sem=38, num_type='xt20to99'], [thirty, eighth]).
flexicon(ordinal, [sem=39, num_type='xt20to99'], [thirty, ninth]).
flexicon(ordinal, [sem=41, num_type='xt20to99'], [forty, first]).
flexicon(ordinal, [sem=42, num_type='xt20to99'], [forty, second]).
flexicon(ordinal, [sem=43, num_type='xt20to99'], [forty, third]).
flexicon(ordinal, [sem=44, num_type='xt20to99'], [forty, fourth]).
flexicon(ordinal, [sem=45, num_type='xt20to99'], [forty, fifth]).
flexicon(ordinal, [sem=46, num_type='xt20to99'], [forty, sixth]).
flexicon(ordinal, [sem=47, num_type='xt20to99'], [forty, seventh]).
flexicon(ordinal, [sem=48, num_type='xt20to99'], [forty, eighth]).
flexicon(ordinal, [sem=49, num_type='xt20to99'], [forty, ninth]).
flexicon(ordinal, [sem=51, num_type='xt20to99'], [fifty, first]).
flexicon(ordinal, [sem=52, num_type='xt20to99'], [fifty, second]).
flexicon(ordinal, [sem=53, num_type='xt20to99'], [fifty, third]).
flexicon(ordinal, [sem=54, num_type='xt20to99'], [fifty, fourth]).
flexicon(ordinal, [sem=55, num_type='xt20to99'], [fifty, fifth]).
flexicon(ordinal, [sem=56, num_type='xt20to99'], [fifty, sixth]).
flexicon(ordinal, [sem=57, num_type='xt20to99'], [fifty, seventh]).
flexicon(ordinal, [sem=58, num_type='xt20to99'], [fifty, eighth]).
flexicon(ordinal, [sem=59, num_type='xt20to99'], [fifty, ninth]).
flexicon(ordinal, [sem=60, num_type='xt20to99'], [sixty, six, zero]).
flexicon(ordinal, [sem=61, num_type='xt20to99'], [sixty, first]).
flexicon(ordinal, [sem=62, num_type='xt20to99'], [sixty, second]).
flexicon(ordinal, [sem=63, num_type='xt20to99'], [sixty, third]).
flexicon(ordinal, [sem=64, num_type='xt20to99'], [sixty, fourth]).
flexicon(ordinal, [sem=65, num_type='xt20to99'], [sixty, fifth]).
flexicon(ordinal, [sem=66, num_type='xt20to99'], [sixty, sixth]).
flexicon(ordinal, [sem=67, num_type='xt20to99'], [sixty, seventh]).
flexicon(ordinal, [sem=68, num_type='xt20to99'], [sixty, eighth]).
flexicon(ordinal, [sem=69, num_type='xt20to99'], [sixty, ninth]).
flexicon(ordinal, [sem=71, num_type='xt20to99'], [seventy, first]).
flexicon(ordinal, [sem=72, num_type='xt20to99'], [seventy, second]).
flexicon(ordinal, [sem=73, num_type='xt20to99'], [seventy, third]).
flexicon(ordinal, [sem=74, num_type='xt20to99'], [seventy, fourth]).
flexicon(ordinal, [sem=75, num_type='xt20to99'], [seventy, fifth]).
flexicon(ordinal, [sem=76, num_type='xt20to99'], [seventy, sixth]).
flexicon(ordinal, [sem=77, num_type='xt20to99'], [seventy, seventh]).
flexicon(ordinal, [sem=78, num_type='xt20to99'], [seventy, eighth]).
flexicon(ordinal, [sem=79, num_type='xt20to99'], [seventy, ninth]).
flexicon(ordinal, [sem=81, num_type='xt20to99'], [eighty, first]).
flexicon(ordinal, [sem=82, num_type='xt20to99'], [eighty, second]).
flexicon(ordinal, [sem=83, num_type='xt20to99'], [eighty, third]).
flexicon(ordinal, [sem=84, num_type='xt20to99'], [eighty, fourth]).
flexicon(ordinal, [sem=85, num_type='xt20to99'], [eighty, fifth]).
flexicon(ordinal, [sem=86, num_type='xt20to99'], [eighty, sixth]).
flexicon(ordinal, [sem=87, num_type='xt20to99'], [eighty, seventh]).
flexicon(ordinal, [sem=88, num_type='xt20to99'], [eighty, eighth]).
flexicon(ordinal, [sem=89, num_type='xt20to99'], [eighty, ninth]).
flexicon(ordinal, [sem=91, num_type='xt20to99'], [ninety, first]).
flexicon(ordinal, [sem=92, num_type='xt20to99'], [ninety, second]).
flexicon(ordinal, [sem=93, num_type='xt20to99'], [ninety, third]).
flexicon(ordinal, [sem=94, num_type='xt20to99'], [ninety, fourth]).
flexicon(ordinal, [sem=95, num_type='xt20to99'], [ninety, fifth]).
flexicon(ordinal, [sem=96, num_type='xt20to99'], [ninety, sixth]).
flexicon(ordinal, [sem=97, num_type='xt20to99'], [ninety, seventh]).
flexicon(ordinal, [sem=98, num_type='xt20to99'], [ninety, eighth]).
flexicon(ordinal, [sem=99, num_type='xt20to99'], [ninety, ninth]).


% Numbers
flexicon(number, [sem=0, num_type=zero, agr=pl], oh).
flexicon(number, [sem=0, num_type=zero, agr=pl], zero).
flexicon(number, [sem=1, num_type=a, agr=sg], a).
flexicon(number, [sem=1, num_type=a, agr=sg], an).
flexicon(number, [sem=1, num_type=digit, agr=sg], one).

flexicon(number, [sem=2, num_type=digit, agr=pl], two).
flexicon(number, [sem=3, num_type=digit, agr=pl], three).
flexicon(number, [sem=4, num_type=digit, agr=pl], four).
flexicon(number, [sem=5, num_type=digit, agr=pl], five).
flexicon(number, [sem=6, num_type=digit, agr=pl], six).
flexicon(number, [sem=7, num_type=digit, agr=pl], seven).
flexicon(number, [sem=8, num_type=digit, agr=pl], eight).
flexicon(number, [sem=9, num_type=digit, agr=pl], nine).

flexicon(number, [sem=1], one).
flexicon(number, [sem=2], two).
flexicon(number, [sem=3], three).
flexicon(number, [sem=4], four).
flexicon(number, [sem=5], five).
flexicon(number, [sem=6], six).
flexicon(number, [sem=7], seven).
flexicon(number, [sem=8], eight).
flexicon(number, [sem=9], nine).
flexicon(number, [sem=10], ten).
flexicon(number, [sem=11], eleven).
flexicon(number, [sem=12], twelve).
flexicon(number, [sem=13], thirteen).
flexicon(number, [sem=14], fourteen).
flexicon(number, [sem=15], fifteen).
flexicon(number, [sem=15], [quarter, hour]).
flexicon(number, [sem=16], sixteen).
flexicon(number, [sem=17], seventeen).
flexicon(number, [sem=18], eighteen).
flexicon(number, [sem=19], nineteen).
flexicon(number, [sem=20], twenty).
flexicon(number, [sem=21], [twenty, one]).
flexicon(number, [sem=22], [twenty, two]).
flexicon(number, [sem=23], [twenty, three]).
flexicon(number, [sem=24], [twenty, four]).
flexicon(number, [sem=25], [twenty, five]).
flexicon(number, [sem=26], [twenty, six]).
flexicon(number, [sem=27], [twenty, seven]).
flexicon(number, [sem=28], [twenty, eight]).
flexicon(number, [sem=29], [twenty, nine]).

flexicon(number, [sem=10, 
  num_type=(ten+(hour+hour_up_to_twelve)), 
  agr=pl], ten).
flexicon(number, [sem=11, num_type=(teen+(hour+(hour_up_to_twelve))), agr=pl], eleven).
flexicon(number, [sem=12, (num_type=(teen+(hour+(hour_up_to_twelve)))), agr=pl], twelve).

flexicon(number, [sem=12, num_type=teen, agr=pl], [a, dozen]).

flexicon(number, [sem=13, num_type=teen+hour, agr=pl], thirteen).
flexicon(number, [sem=14, num_type=teen+hour, agr=pl], fourteen).
flexicon(number, [sem=15, num_type=teen+hour, agr=pl], fifteen).
flexicon(number, [sem=16, num_type=teen+hour, agr=pl], sixteen).
flexicon(number, [sem=17, num_type=teen+hour, agr=pl], seventeen).
flexicon(number, [sem=18, num_type=teen+hour, agr=pl], eighteen).
flexicon(number, [sem=19, num_type=teen+hour, agr=pl], nineteen).

flexicon(number, [sem=20, num_type='xt20to99'+hour, agr=pl], twenty).
flexicon(number, [sem=30, num_type='xt20to99', agr=pl], thirty).
flexicon(number, [sem=40, num_type='xt20to99', agr=pl], forty).
flexicon(number, [sem=50, num_type='xt20to99', agr=pl], fifty).
flexicon(number, [sem=60, num_type='xt20to99', agr=pl], sixty).
flexicon(number, [sem=70, num_type='xt20to99', agr=pl], seventy).
flexicon(number, [sem=80, num_type='xt20to99', agr=pl], eighty).
flexicon(number, [sem=90, num_type='xt20to99', agr=pl], ninety).

flexicon(number, [sem=0, num_type=oh_digit, agr=pl], [oh, oh]).
flexicon(number, [sem=0, num_type=oh_digit, agr=pl], [zero, zero]).
flexicon(number, [sem=1, num_type=oh_digit, agr=pl], [oh, one]).
flexicon(number, [sem=2, num_type=oh_digit, agr=pl], [oh, two]).
flexicon(number, [sem=3, num_type=oh_digit, agr=pl], [oh, three]).
flexicon(number, [sem=4, num_type=oh_digit, agr=pl], [oh, four]).
flexicon(number, [sem=5, num_type=oh_digit, agr=pl], [oh, five]).
flexicon(number, [sem=6, num_type=oh_digit, agr=pl], [oh, six]).
flexicon(number, [sem=7, num_type=oh_digit, agr=pl], [oh, seven]).
flexicon(number, [sem=8, num_type=oh_digit, agr=pl], [oh, eight]).
flexicon(number, [sem=9, num_type=oh_digit, agr=pl], [oh, nine]).
flexicon(number, [sem=1, num_type=oh_digit, agr=pl], [zero, one]).
flexicon(number, [sem=2, num_type=oh_digit, agr=pl], [zero, two]).
flexicon(number, [sem=3, num_type=oh_digit, agr=pl], [zero, three]).
flexicon(number, [sem=4, num_type=oh_digit, agr=pl], [zero, four]).
flexicon(number, [sem=5, num_type=oh_digit, agr=pl], [zero, five]).
flexicon(number, [sem=6, num_type=oh_digit, agr=pl], [zero, six]).
flexicon(number, [sem=7, num_type=oh_digit, agr=pl], [zero, seven]).
flexicon(number, [sem=8, num_type=oh_digit, agr=pl], [zero, eight]).
flexicon(number, [sem=9, num_type=oh_digit, agr=pl], [zero, nine]).

flexicon(number, [sem=12, num_type='xt20to99', agr=pl], [one, dozen]).

flexicon(number, [sem=13, num_type=teen+hour, agr=pl], [one, three]).
flexicon(number, [sem=14, num_type=teen+hour, agr=pl], [one, four]).
flexicon(number, [sem=15, num_type=teen+hour, agr=pl], [one, five]).
flexicon(number, [sem=16, num_type=teen+hour, agr=pl], [one, six]).
flexicon(number, [sem=17, num_type=teen+hour, agr=pl], [one, seven]).
flexicon(number, [sem=18, num_type=teen+hour, agr=pl], [one, eight]).
flexicon(number, [sem=19, num_type=teen+hour, agr=pl], [one, nine]).

flexicon(number, [sem=21, num_type=('xt20to99'+hour), agr=pl], [twenty, one]).
flexicon(number, [sem=22, num_type=('xt20to99'+hour), agr=pl], [twenty, two]).
flexicon(number, [sem=23, num_type=('xt20to99'+hour), agr=pl], [twenty, three]).
flexicon(number, [sem=24, num_type=('xt20to99'+hour), agr=pl], [twenty, four]).

flexicon(number, [sem=24, num_type='xt20to99', agr=pl], [two, dozen]).

flexicon(number, [sem=25, num_type='xt20to99', agr=pl], [twenty, five]).
flexicon(number, [sem=26, num_type='xt20to99', agr=pl], [twenty, six]).
flexicon(number, [sem=27, num_type='xt20to99', agr=pl], [twenty, seven]).
flexicon(number, [sem=28, num_type='xt20to99', agr=pl], [twenty, eight]).
flexicon(number, [sem=29, num_type='xt20to99', agr=pl], [twenty, nine]).

flexicon(number, [sem=30, num_type='xt20to99', agr=pl], (thirty; [three, zero])).
flexicon(number, [sem=40, num_type='xt20to99', agr=pl], (forty; [four, zero])).
flexicon(number, [sem=50, num_type='xt20to99', agr=pl], (fifty; [five, zero])).
flexicon(number, [sem=60, num_type='xt20to99', agr=pl], (sixty; [six, zero])).
flexicon(number, [sem=70, num_type='xt20to99', agr=pl], (seventy; [seven, zero])).
flexicon(number, [sem=80, num_type='xt20to99', agr=pl], (eighty; [eight, zero])).
flexicon(number, [sem=90, num_type='xt20to99', agr=pl], (ninety; [nine, zero])).


flexicon(number, [sem=31, num_type='xt20to99', agr=pl], [thirty, one]).
flexicon(number, [sem=32, num_type='xt20to99', agr=pl], [thirty, two]).
flexicon(number, [sem=33, num_type='xt20to99', agr=pl], [thirty, three]).
flexicon(number, [sem=34, num_type='xt20to99', agr=pl], [thirty, four]).
flexicon(number, [sem=35, num_type='xt20to99', agr=pl], [thirty, five]).
flexicon(number, [sem=36, num_type='xt20to99', agr=pl], [thirty, six]).
flexicon(number, [sem=37, num_type='xt20to99', agr=pl], [thirty, seven]).
flexicon(number, [sem=38, num_type='xt20to99', agr=pl], [thirty, eight]).
flexicon(number, [sem=39, num_type='xt20to99', agr=pl], [thirty, nine]).
flexicon(number, [sem=41, num_type='xt20to99', agr=pl], [forty, one]).
flexicon(number, [sem=42, num_type='xt20to99', agr=pl], [forty, two]).
flexicon(number, [sem=43, num_type='xt20to99', agr=pl], [forty, three]).
flexicon(number, [sem=44, num_type='xt20to99', agr=pl], [forty, four]).
flexicon(number, [sem=45, num_type='xt20to99', agr=pl], [forty, five]).
flexicon(number, [sem=46, num_type='xt20to99', agr=pl], [forty, six]).
flexicon(number, [sem=47, num_type='xt20to99', agr=pl], [forty, seven]).
flexicon(number, [sem=48, num_type='xt20to99', agr=pl], [forty, eight]).
flexicon(number, [sem=49, num_type='xt20to99', agr=pl], [forty, nine]).
flexicon(number, [sem=51, num_type='xt20to99', agr=pl], [fifty, one]).
flexicon(number, [sem=52, num_type='xt20to99', agr=pl], [fifty, two]).
flexicon(number, [sem=53, num_type='xt20to99', agr=pl], [fifty, three]).
flexicon(number, [sem=54, num_type='xt20to99', agr=pl], [fifty, four]).
flexicon(number, [sem=55, num_type='xt20to99', agr=pl], [fifty, five]).
flexicon(number, [sem=56, num_type='xt20to99', agr=pl], [fifty, six]).
flexicon(number, [sem=57, num_type='xt20to99', agr=pl], [fifty, seven]).
flexicon(number, [sem=58, num_type='xt20to99', agr=pl], [fifty, eight]).
flexicon(number, [sem=59, num_type='xt20to99', agr=pl], [fifty, nine]).

flexicon(number, [sem=61, num_type='xt20to99', agr=pl], [sixty, one]).
flexicon(number, [sem=62, num_type='xt20to99', agr=pl], [sixty, two]).
flexicon(number, [sem=63, num_type='xt20to99', agr=pl], [sixty, three]).
flexicon(number, [sem=64, num_type='xt20to99', agr=pl], [sixty, four]).
flexicon(number, [sem=65, num_type='xt20to99', agr=pl], [sixty, five]).
flexicon(number, [sem=66, num_type='xt20to99', agr=pl], [sixty, six]).
flexicon(number, [sem=67, num_type='xt20to99', agr=pl], [sixty, seven]).
flexicon(number, [sem=68, num_type='xt20to99', agr=pl], [sixty, eight]).
flexicon(number, [sem=69, num_type='xt20to99', agr=pl], [sixty, nine]).


flexicon(number, [sem=71, num_type='xt20to99', agr=pl], [seventy, one]).
flexicon(number, [sem=72, num_type='xt20to99', agr=pl], [seventy, two]).
flexicon(number, [sem=73, num_type='xt20to99', agr=pl], [seventy, three]).
flexicon(number, [sem=74, num_type='xt20to99', agr=pl], [seventy, four]).
flexicon(number, [sem=75, num_type='xt20to99', agr=pl], [seventy, five]).
flexicon(number, [sem=76, num_type='xt20to99', agr=pl], [seventy, six]).
flexicon(number, [sem=77, num_type='xt20to99', agr=pl], [seventy, seven]).
flexicon(number, [sem=78, num_type='xt20to99', agr=pl], [seventy, eight]).
flexicon(number, [sem=79, num_type='xt20to99', agr=pl], [seventy, nine]).

flexicon(number, [sem=81, num_type='xt20to99', agr=pl], [eighty, one]).
flexicon(number, [sem=82, num_type='xt20to99', agr=pl], [eighty, two]).
flexicon(number, [sem=83, num_type='xt20to99', agr=pl], [eighty, three]).
flexicon(number, [sem=84, num_type='xt20to99', agr=pl], [eighty, four]).
flexicon(number, [sem=85, num_type='xt20to99', agr=pl], [eighty, five]).
flexicon(number, [sem=86, num_type='xt20to99', agr=pl], [eighty, six]).
flexicon(number, [sem=87, num_type='xt20to99', agr=pl], [eighty, seven]).
flexicon(number, [sem=88, num_type='xt20to99', agr=pl], [eighty, eight]).
flexicon(number, [sem=89, num_type='xt20to99', agr=pl], [eighty, nine]).

flexicon(number, [sem=91, num_type='xt20to99', agr=pl], [ninety, one]).
flexicon(number, [sem=92, num_type='xt20to99', agr=pl], [ninety, two]).
flexicon(number, [sem=93, num_type='xt20to99', agr=pl], [ninety, three]).
flexicon(number, [sem=94, num_type='xt20to99', agr=pl], [ninety, four]).
flexicon(number, [sem=95, num_type='xt20to99', agr=pl], [ninety, five]).
flexicon(number, [sem=96, num_type='xt20to99', agr=pl], [ninety, six]).
flexicon(number, [sem=97, num_type='xt20to99', agr=pl], [ninety, seven]).
flexicon(number, [sem=98, num_type='xt20to99', agr=pl], [ninety, eight]).
flexicon(number, [sem=99, num_type='xt20to99', agr=pl], [ninety, nine]).

