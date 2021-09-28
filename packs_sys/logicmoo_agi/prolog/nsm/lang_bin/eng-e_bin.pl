morph_grammar_type(eng:e, dependency).
synt_grammar_type(eng:e, dependency).
max_dep_threshold(eng:e, 4).
morph_threshold(eng:e, 40).
dep_threshold(eng:e, 4, 500).
dep_threshold(eng:e, 3, 400).
dep_threshold(eng:e, 2, 300).
dep_threshold(eng:e, 1, 200).
transcr_table(eng:e, [`no-one`:`noone`, `cannot`:`cannot`, `another`:`aother`, `sometimes`:`sometimes`, `sh`:`S`, `ch`:`C`]).
phonetic_class(eng:e, e_triggers, `C`,`S`,`s`,`z`,`x`,`o`,`i`]).
phonetic_class(eng:e, vow, [`a`, `e`, `i`, `o`, `u` ]).
arc(eng:e, n(N), gen).
arc(eng:e, i, gen).
arc(eng:e, n(n), i).
arc(eng:e, v(V), i).
arc(eng:e, Kw, stop).
arc(eng:e, start, Arc_Start).
dep(eng:e, 410, (ct(s, s(S9, S8, S7, S6, P, p(think, [e:E, o:e|Kw]), S, S17))+ct(d(D), that)+ct(s, s(S16, S15, S14, S13, S12, S11, S10, S19))==>ct(s, s(S9, S8, S7, S6, P, p(think, [e:E, o:s(S16, S15, S14, S13, S12, S11, S10, S19)|Kw]), S, S17))), []).
dep(eng:e, 410, (ct(s, s(S9, S8, S7, S6, P, p(know, [e:E, o:e|Kw]), S, S17))+ct(d(D), that)+ct(s, s(S16, S15, S14, S13, S12, S11, S10, S19))==>ct(s, s(S9, S8, S7, S6, P, p(know, [e:E, o:s(S16, S15, S14, S13, S12, S11, S10, S19)|Kw]), S, S17))), []).
dep(eng:e, 53, (ct(p, l)+ct(n(n), sp(e, e, sing(e), [], front))+ct(p, gen)==>ct(adv(l), front)), []).
dep(eng:e, 425, (ct(s, As_Ct_S)+ct(clause(as), Ct_Clause)==>ct(s, Ct_ClauseasAs_Ct_S)), []).
dep(eng:e, 425, (ct(s, When_Ct_S)+ct(clause(when), Ct_Clause)==>ct(s, when(Ct_Clause, When_Ct_S))), []).
dep(eng:e, 425, (ct(clause(when), Ct_Clause)+ct(s, When_Ct_S)==>ct(s, when(Ct_Clause, When_Ct_S))), []).
dep(eng:e, 425, (ct(s, Because_Ct_S)+ct(clause(because), Ct_Clause)==>ct(s, because(Ct_Clause, Because_Ct_S))), []).
dep(eng:e, 425, (ct(clause(because), Ct_Clause)+ct(s, Because_Ct_S)==>ct(s, because(Ct_Clause, Because_Ct_S))), []).
dep(eng:e, 425, (ct(s, If_Ct_S)+ct(clause(if), Ct_Clause)==>ct(s, if(Ct_Clause, If_Ct_S))), []).
dep(eng:e, 425, (ct(clause(if), Ct_Clause)+ct(s, If_Ct_S)==>ct(s, if(Ct_Clause, If_Ct_S))), []).
dep(eng:e, 420, (ct(s, s(S7, S6, S5, S, P, p(Eval12, [o:sp(this, e, sing(e), [], something(fact))|Kw]), e, e))+ct(v(V), p(P8, [e, e, inf, e, e], P11))==>ct(s, s(S7, S6, S5, S, P, p(Eval12, [o:p(P8, P11)|Kw])))), [eval(Eval12)]).
dep(eng:e, 420, (ct(s, s(S7, S6, S5, S, P, p(Eval11, [o:sp(this, e, sing(e), [], something(fact))|Kw]), e, e))+ct(clause(if), Ct_Clause)==>ct(s, s(S7, S6, S5, S, P, p(Eval11, [o:Ct_Clause|Kw])))), [eval(Eval11)]).
dep(eng:e, 415, (ct(s, Ct_S)+ct(pp(cause), Because_Ct_Pp)==>ct(s, because(Ct_S, Because_Ct_Pp))), []).
dep(eng:e, 415, (ct(pp(cause), Because_Ct_Pp)+ct(s, Ct_S)==>ct(s, because(Ct_S, Because_Ct_Pp))), []).
dep(eng:e, 415, (ct(adv(sent), maybe)+ct(s, Maybe_Ct_S)==>ct(s, maybe(Maybe_Ct_S))), []).
dep(eng:e, 414, (ct(neg, not)+ct(pp(cause), Not_Ct_Pp)==>ct(pp(cause), not(Not_Ct_Pp))), []).
dep(eng:e, 413, (ct(conj, Clause_Ct_Conj)+ct(s, Clause_Ct_S)==>ct(clause(Clause_Ct_Conj), Clause_Ct_S)), []).
dep(eng:e, 410, (ct(s, s(S13, S12, S11, S10, P, p(want, [e:E, o:sp(Sp6, Sp5, Sp4, Sp, Sp25)]), S, S26))+ct(v(V), p(V_form21, [V_form, V_form20, inf, e, Pred_neg23], [Sp_E14:e|Kw15]))==>ct(s, s(S13, S12, S11, S10, P, p(want, [e:E, o:[p(Neg, [Sp_E14:sp(Sp6, Sp5, Sp4, Sp, Sp25)|Kw15])]]), S, S26))), [v_form(V_form21, V_form20, V_form, Form_Pred_neg), pred_neg(Pred_neg23, Form_Pred_neg, Neg)]).
dep(eng:e, 410, (ct(s, s(S8, S7, S6, S5, P, p(want, [e:E, o:e]), S, S19))+ct(v(V), p(V_form15, [V_form, V_form14, inf, e, Pred_neg17], [E9:e|Kw]))==>ct(s, s(S8, S7, S6, S5, P, p(want, [e:E, o:[p(Neg, [E9:e|Kw])]]), S, S19))), [v_form(V_form15, V_form14, V_form, Form_Pred_neg), pred_neg(Pred_neg17, Form_Pred_neg, Neg)]).
dep(eng:e, 410, (ct(s, s(S9, S8, S7, S6, P, p(think, [e:E, o:e|Kw]), S, S17))+ct(d(D), that)+ct(s, s(S16, S15, S14, S13, S12, S11, S10, S19))==>ct(s, s(S9, S8, S7, S6, P, p(think, [e:E, o:s(S16, S15, S14, S13, S12, S11, S10, S19)|Kw]), S, S17))), []).
dep(eng:e, 410, (ct(s, s(S9, S8, S7, S6, P, p(know, [e:E, o:e|Kw]), S, S17))+ct(d(D), that)+ct(s, s(S16, S15, S14, S13, S12, S11, S10, S19))==>ct(s, s(S9, S8, S7, S6, P, p(know, [e:E, o:s(S16, S15, S14, S13, S12, S11, S10, S19)|Kw]), S, S17))), []).
dep(eng:e, 320, (ct(s, s(S6, S, E, e, e, p(P, P9), P4, S10))+ct(pp(b), Ct_Pp)==>ct(s, s(S6, S, E, e, dur(Mod_Dur), p(P, P9), P4, S10))), [duration_mod(Ct_Pp, Mod_Dur)]).
dep(eng:e, 320, (ct(s, s(S6, S, E, e, e, p(P, P8), P4, S9))+ct(n(N), sp(e, e, plur(Freq_Plur11), [], time(time)))==>ct(s, s(S6, S, E, freq(Freq_Plur11), e, p(P, P8), P4, S9))), []).
dep(eng:e, 320, (ct(n(N), sp(e, e, plur(Freq_Plur9), [], time(time)))+ct(s, s(S7, S, E, e, e, p(P, P10), P5, S11))==>ct(s, s(S7, S, E, freq(Freq_Plur9), e, p(P, P10), P5, S11))), []).
dep(eng:e, 320, (ct(s, s(S6, S, E, e, e, p(P, P7), P4, S8))+ct(n(N), sp(e, e, sing(one), [], time(time)))==>ct(s, s(S6, S, E, freq(one), e, p(P, P7), P4, S8))), []).
dep(eng:e, 320, (ct(n(N), sp(e, e, sing(one), [], time(time)))+ct(s, s(S6, S, E, e, e, p(P, P8), P4, S9))==>ct(s, s(S6, S, E, freq(one), e, p(P, P8), P4, S9))), []).
dep(eng:e, 320, (ct(s, s(S7, Agr_S6, E, e, P4, p(P, P13), S, S14))+ct(adv(time(Time15)), time(Time9, Time, Time_agrTime9Time))==>ct(s, s(S7, SS7, E, e, P4, p(P, P13), S, S14))), [time_agr(Time9, Time, Time_agrTime9Time, SS7, Agr_S6)]).
dep(eng:e, 320, (ct(adv(time(Time13)), time(Time3, Time, Time_agrTime3Time))+ct(s, s(S9, Agr_S8, E, e, P6, p(P, P14), S, S15))==>ct(s, s(S9, SS9, E, e, P6, p(P, P14), S, S15))), [time_agr(Time3, Time, Time_agrTime3Time, SS9, Agr_S8)]).
dep(eng:e, 320, (ct(s, s(S7, Agr_S6, E, e, P4, p(P, P14), S, S15))+ct(pp(time), sp(Sp, Sing, sing(e), [], time(time)))==>ct(s, s(S7, SS7, E, e, P4, p(P, P14), S, S15))), [time_loc(Sp, Sing, Loc_At), time_agr(e, at, Loc_At, SS7, Agr_S6)]).
dep(eng:e, 320, (ct(pp(time), sp(Sp, Sing, sing(e), [], time(time)))+ct(s, s(S10, Agr_S9, E, e, P7, p(P, P14), S, S15))==>ct(s, s(S10, SS10, E, e, P7, p(P, P14), S, S15))), [time_loc(Sp, Sing, Loc_At), time_agr(e, at, Loc_At, SS10, Agr_S9)]).
dep(eng:e, 315, (ct(s, s(not, Neg_Not, E, e, P5, p(More_P, More), S, S9))+ct(adv(a), anymore)==>ct(s, s(neg, Neg_Not, E, e, P5, p(more(More_P), More), S, S9))), []).
dep(eng:e, 310, (ct(s, s(S8, S7, S6, S, P3, p(P, P9), E, e))+ct(adv, like(this))==>ct(s, s(S8, S7, S6, S, P3, p(P, P9), E, manner(this)))), []).
dep(eng:e, 310, (ct(s, s(S8, S7, S6, S, P3, p(P, P14), Sp_E, e))+ct(pp(l), sp(Sp12, Sp11, Sp, Manner, manner(Manner15)))==>ct(s, s(S8, S7, S6, S, P3, p(P, P14), Sp_E, sp(Sp12, Sp11, Sp, Manner, manner(Manner15))))), []).
dep(eng:e, 310, (ct(s, s(S8, S7, S6, S, P3, p(P, P14), e, S_L_E))+ct(pp(l), sp(Sp12, Sp11, Sp, Somewhere, somewhere(Somewhere16)))==>ct(s, s(S8, S7, S6, S, P3, p(P, P14), l(sp(Sp12, Sp11, Sp, Somewhere, somewhere(Somewhere16))), S_L_E))), []).
dep(eng:e, 310, (ct(s, s(S8, S7, S6, S, P3, p(P, P11), e, S_L_E))+ct(adv(l(L)), Ct_Adv)==>ct(s, s(S8, S7, S6, S, P3, p(P, P11), l(Ct_Adv), S_L_E))), []).
dep(eng:e, 310, (ct(s, s(S7, S6, S, Dur_E, e, p(P, P9), P4, S10))+ct(pp(b), sp(ref(unkn), e, sing(e), [Dur], time(1)))==>ct(s, s(S7, S6, S, Dur_E, dur(Dur), p(P, P9), P4, S10))), []).
dep(eng:e, 310, (ct(s, s(S7, S6, Sp_E, e, P4, p(P, P13), S, S14))+ct(n(N), sp(Sp11, Sp10, Sp, Times, times(Times16)))==>ct(s, s(S7, S6, Sp_E, sp(Sp11, Sp10, Sp, Times, times(Times16)), P4, p(P, P13), S, S14))), []).
dep(eng:e, 305, (ct(n(N), sp(Sp5, Sp4, Sp3, Sp, Case_Sp20))+ct(v(stat), p(V_form, [e, E14, Be, V_par, S11], [Sp_Kw:V_parES|Kw7]))==>ct(s, s(S11, S, E, e, e, p(Form, [Sp_Kw:sp(Sp5, Sp4, Sp3, Sp, Par_Sp_Subj_case)|Kw7]), e, e))), [subj_case(Par_Sp_Subj_case, Case_Sp20), v_par(be, Be, V_par, E14, e, E, S, V_parES, Sp3, Par_Sp_Subj_case), v_form(V_form, E14, e, Form)]).
dep(eng:e, 305, (ct(n(N), sp(Sp5, Sp4, Sp3, Sp, Case_Sp21))+ct(v(v), p(V_par18, [V_par, V_par15, V_par17, V_par16, S11], [Sp_Kw:V_parES|Kw7]))==>ct(s, s(S11, S, E, e, e, p(Form, [Sp_Kw:sp(Sp5, Sp4, Sp3, Sp, Par_Sp_Subj_case)|Kw7]), e, e))), [subj_case(Par_Sp_Subj_case, Case_Sp21), v_par(V_par18, V_par17, V_par16, V_par15, V_par, E, S, V_parES, Sp3, Par_Sp_Subj_case), v_form(V_par18, V_par15, V_par, Form)]).
dep(eng:e, 305, (ct(adv(l(i)), there)+ct(v(v), p(be, [V_par, Exist_V_par16, Be, V_par17, Neg_existence], [j:J, o:sp(Sp6, Sp5, Sp4, Sp, Case_Sp20)]))==>ct(s, s(Existence_S11, S, E, e, e, p(Form, [j:sp(Sp6, Sp5, Sp4, Sp, Par_Sp_Subj_case)]), e, e))), [subj_case(Par_Sp_Subj_case, Case_Sp20), indet_sp(Sp6, Sp4, Case_Sp20), neg_existence(Neg_existence, Sp6, Existence_S11), v_par(be, Be, V_par17, Exist_V_par16, V_par, E, S, J, Sp4, Par_Sp_Subj_case), v_form(exist, Exist_V_par16, V_par, Form)]).
dep(eng:e, 250, (ct(v(aux), p(have, Have, [j:J]))+ct(n(N), sp(Sp8, Sp7, Sp6, Sp, Sp11))==>ct(v(v), p(have, Have, [j:J, o:sp(Sp8, Sp7, Sp6, Sp, Sp11)]))), []).
dep(eng:e, 250, (ct(v(aux), p(be, Like_Be, [j:J]))+ct(adv, like(Like_O))==>ct(v(stat), p(like, Like_Be, [j:J, o:Like_O]))), []).
dep(eng:e, 250, (ct(v(aux), p(be, Be, [j:J]))+ct(adv(l(L)), Ct_Adv)==>ct(v(v), p(be, Be, [j:J, l:Ct_Adv]))), []).
dep(eng:e, 250, (ct(v(aux), p(be, Be, [j:O_J]))+ct(a, Ct_A)==>ct(v(stat), p(Ct_A, Be, [o:O_J|For_Kw7]))), [good_for(Ct_A, For_Kw7)]).
dep(eng:e, 250, (ct(v(aux), p(be, Be, [j:J]))+ct(n(N), sp(Sp8, Sp7, Sp6, Sp, Sp11))==>ct(v(v), p(be, Be, [j:J, o:sp(Sp8, Sp7, Sp6, Sp, Sp11)]))), []).
dep(eng:e, 250, (ct(v(aux), p(be, Be, [j:J]))+ct(pp(Pp_Sp9), sp(Sp8, Sp7, Sp6, Sp, Sp12))==>ct(v(v), p(be, Be, [j:J, Pp_Sp9:sp(Sp8, Sp7, Sp6, Sp, Sp12)]))), []).
dep(eng:e, 230, (ct(v(aux), p(_42106, [_43256, _42120, _42126, _42132, e], P))+ct(neg, not)==>ct(v(aux), p(_42106, [_45282, _42120, _42126, _42132, not], P))), []).
dep(eng:e, 225, (ct(v(V), p(P, _41626, [_41632, _41638, _41644, _41650, Pp_Sp_E:e|Kw]))+ct(pp(Pp_Sp_E), sp(Sp8, Sp7, Sp6, Sp, Sp12))==>ct(v(V), p(P, _41626, [_41632, _41638, _41644, _41650, Pp_Sp_E:sp(Sp8, Sp7, Sp6, Sp, Sp12)|Kw]))), []).
dep(eng:e, 220, (ct(v(V), p(P, _51806, [_51812, _51818, _51824, Pp_Sp_E:e|Kw]))+ct(pp(Pp_Sp_E), sp(Sp8, Sp7, Sp6, Sp, Sp12))==>ct(v(V), p(P, _51806, [_51812, _51818, _51824, Pp_Sp_E:sp(Sp8, Sp7, Sp6, Sp, Sp12)|Kw]))), []).
dep(eng:e, 215, (ct(v(V), p(P, _44558, [_44564, _44570, Pp_Sp_E:e|Kw]))+ct(pp(Pp_Sp_E), sp(Sp8, Sp7, Sp6, Sp, Sp12))==>ct(v(V), p(P, _44558, [_44564, _44570, Pp_Sp_E:sp(Sp8, Sp7, Sp6, Sp, Sp12)|Kw]))), []).
dep(eng:e, 210, (ct(v(V), p(P, _35850, [Kw:_35864, Pp_Sp_E:e|Kw4]))+ct(pp(Pp_Sp_E), sp(Sp9, Sp8, Sp7, Sp, Sp13))==>ct(v(V), p(P, _35850, [Kw:_35864, Pp_Sp_E:sp(Sp9, Sp8, Sp7, Sp, Sp13)|Kw4]))), []).
dep(eng:e, 207, (ct(v(V), p(P, _28844, [Kw:_28858, o:e|Kw3]))+ct(d(D), this)==>ct(v(V), p(P, _28844, [Kw:_28858, o:this|Kw3]))), []).
dep(eng:e, 205, (ct(v(V), p(P, _27426, [Kw:_27440, o:e|Kw3]))+ct(alt, more)==>ct(v(V), p(P, _27426, [Kw:_27440, o:more|Kw3]))), []).
dep(eng:e, 205, (ct(v(V), p(_11158, [_12592, _11172, _11178, _11184, Obj_det10], [Kw:_11204, o:e|Kw3]))+ct(n(N), sp(Sp7, Sp6, Sp5, Sp, Sp14))==>ct(v(V), p(_11158, [_15894, _11172, _11178, _11184, Obj_det], [Kw:_11204, o:sp(Det_Sp8, Sp6, Sp5, Sp, Sp14)|Kw3]))), [obj_det(Obj_det10, Obj_det, Sp7, Det_Sp8)]).
dep(eng:e, 125, (ct(p, Ct_This_P)+ct(d(D), this)==>ct(pp(Role_Pp), sp(this, e, sing(e), [], something(fact)))), [pp_role(Ct_This_P, this, Role_Pp)]).
dep(eng:e, 124, (ct(p, Ct_P)+ct(n(N7), Ct_Pp_N)==>ct(pp(Role_Pp), Ct_Pp_N)), [pp_role(Ct_P, Ct_Pp_N, Role_Pp)]).
dep(eng:e, 122, (ct(d(D), any)+ct(alt, more)==>ct(adv(a), anymore)), []).
dep(eng:e, 121, (ct(num, all)+ct(n(n), sp(Sp4, Plur, plur(e), Sp, Sp5))==>ct(n(n), sp(Sp4, Plur, plur(all), Sp, Sp5))), []).
dep(eng:e, 120, (ct(adv(time(tr)), time(Time, D_E, e))+ct(n(n), sp(D_E4, e, sing(e), [], time(time)))==>ct(adv(time(tr)), time(Time, D_E, d(D_E4)))), []).
dep(eng:e, 120, (ct(adv(l(tr)), Ct_Adv)+ct(adv(l(i)), Loc_Ct_Adv3)==>ct(adv(l(tr)), loc(Ct_Adv, Loc_Ct_Adv3))), []).
dep(eng:e, 120, (ct(adv(l(tr)), Ct_Adv)+ct(n(N5), Loc_Ct_N)==>ct(adv(l(tr)), loc(Ct_Adv, Loc_Ct_N))), []).
dep(eng:e, 119, (ct(n(n), sp(E, e, sing(e), Sing, time(time)))+ct(adv(time(tr)), time(e, E4, e))==>ct(adv(time(tr)), time(e(Ext_E6), E4, e))), [time_ext(E, Sing, Ext_E6)]).
dep(eng:e, 118, (ct(rel, sp(Sp5, Sp4, Sp3, Sp, Sp10))+ct(n(N), sp(Sp9, Sp8, Sp7, Sp6, Sp12))==>ct(n(n), sp(Sp9, Sp8, Sp7, [g:sp(Sp5, Sp4, Sp3, Sp, Sp10)|Sp6], Sp12))), []).
dep(eng:e, 117, (ct(as, sp(Same, same, Sp4, Sp, Same_Sp9))+ct(n(N), sp(Sp8, Sp7, Sp6, Sp5, Sp11))==>ct(n(n), sp(Same, same(sp(Sp8, Sp7, Sp6, Sp5, Sp11), Sp4, Sp, Same_Sp9)))), []).
dep(eng:e, 116, (ct(part, Ct_Part)+ct(n(N), sp(Sp5, Plur, plur(Plur12), Sp, Sp13))==>ct(n(n), sp(e, e, E, [], of(sp(Sp5, Plur, plur(Plur12), Sp, Sp13))))), [np_number(Ct_Part, Np_numberPart, E)]).
dep(eng:e, 115, (ct(n(n), sp(Sp5, Sp4, Sp3, Sp, Relational_Sp7))+ct(p, gen)==>ct(rel, sp(Sp5, Sp4, Sp3, Sp, Relational_Sp7))), [is_relational(Relational_Sp7)]).
dep(eng:e, 114, (ct(n(N), sp(Sp3, Sp, _45426, [Eval_for|_45436], Sp8))+ct(pp(b), Ct_Pp)==>ct(n(N), sp(Sp3, Sp, _45426, [For|_45436], Sp8))), [eval_for(Eval_for, Ct_Pp, For)]).
dep(eng:e, 113, (ct(n(N), sp(Sp5, Sp4, Like_Sp3, [], Sp9))+ct(adv, like(Like))==>ct(n(N), sp(Sp5, Sp4, Like_Sp3, like(Like), Sp9))), [indet_sp(Sp5, Like_Sp3, Sp9)]).
dep(eng:e, 105, (ct(comp, like)+ct(n(N3), Like_Ct_N)==>ct(adv, like(Like_Ct_N))), []).
dep(eng:e, 100, (ct(comp, like)+ct(d(D), this)==>ct(adv, like(this))), []).
dep(eng:e, 97, (ct(n(n), sp(ref(known), same, Same, Ref, Sp))+ct(conj, as)==>ct(as, sp(ref(known), same, Same, Ref, Sp))), []).
dep(eng:e, 95, (ct(gen(Gen7), Of_Ct_Gen)+ct(n(N), sp(e, e, E, Of_SpE, Sp_Of))==>ct(n(N), sp(e, e, E, Of_SpE, of(Sp_Of, Of_Ct_Gen)))), []).
dep(eng:e, 90, (ct(n(Gen_N3), Gen_Ct_N)+ct(gen, gen)==>ct(gen(Gen_N3), Gen_Ct_N)), []).
dep(eng:e, 85, (ct(num, some)+ct(n(n), sp(e, E, sing(Sing4), Sing, Sp))==>ct(n(n), sp(some, E, sing(Sing4), Sing, Sp))), []).
dep(eng:e, 85, (ct(d(Sp5), Ct_D)+ct(n(n), sp(e, E, Sp5, Sp, Sp7))==>ct(n(n), sp(Ct_D, E, Sp5, Sp, Sp7))), []).
dep(eng:e, 83, (ct(alt, same)+ct(n(n), sp(e, e, E, SpE, Sp))==>ct(n(n), sp(e, same, E, SpE, Sp))), []).
dep(eng:e, 80, (ct(num, Ct_Num)+ct(n(n), sp(e, E, Sp5, Sp, Sp9))==>ct(n(n), sp(e, E, Number_Sp6, Sp, Sp9))), [np_number(Ct_Num, Sp5, Number_Sp6)]).
dep(eng:e, 78, (ct(num, Ct_Part_Num)+ct(p, gen)==>ct(part, Ct_Part_Num)), []).
dep(eng:e, 76, (ct(alt, other(2))+ct(n(n), sp(e, e, E, SpE, Sp))==>ct(n(n), sp(ref(unkn), other, E, SpE, Sp))), []).
dep(eng:e, 75, (ct(alt, other)+ct(n(n), sp(e, e, E, SpE, Sp))==>ct(n(n), sp(e, other, E, SpE, Sp))), []).
dep(eng:e, 73, (ct(alt, more)+ct(n(n), sp(e, e, E, SpE, Sp))==>ct(n(n), sp(e, more, E, SpE, Sp))), []).
dep(eng:e, 72, (ct(n(d), sp(e, e, sing(e), Sing, Sp))+ct(a, Ct_A)==>ct(n(d), sp(e, e, sing(e), [Ct_A|Sing], Sp))), []).
dep(eng:e, 71, (ct(a, Ct_A)+ct(n(n), sp(e, e, plur(e), Plur, Sp))==>ct(n(n), sp(e, e, plur(e), [Ct_A|Plur], Sp))), []).
dep(eng:e, 71, (ct(a, Ct_A)+ct(n(n), sp(e, e, sing(e), Sing, Sp))==>ct(n(n), sp(e, e, sing(e), [Ct_A|Sing], Sp))), []).
dep(eng:e, 70, (ct(n(d), sp(E, e, sing(e), Sing, Sp))+ct(alt, other(1))==>ct(n(d), sp(E, other, sing(e), Sing, Sp))), []).
dep(eng:e, 57, (ct(adv(l(i)), very(far))+ct(p, src)==>ct(adv(l(tr)), very(far))), []).
dep(eng:e, 57, (ct(adv(l(i)), far)+ct(p, src)==>ct(adv(l(tr)), far)), []).
dep(eng:e, 57, (ct(v(aux), p(can, [e, e, _30868, e, _30880], P))+ct(v(V), p(_30896, [_35350, _30910, e, e, e], P4))==>ct(v(V), p(_30896, [_37376, _30910, _30868, can, _30880], P4))), []).
dep(eng:e, 57, (ct(v(aux), p(fut, [e, e, _34484, e, _34496], P))+ct(v(V), p(_34512, [_38966, _34526, e, e, e], P4))==>ct(v(V), p(_34512, [_40992, _34526, _34484, fut, _34496], P4))), []).
dep(eng:e, 56, (ct(neg, not)+ct(v(V), p(_10114, [_11260, _10128, inf, e, e], P))==>ct(v(V), p(_10114, [_13286, _10128, inf, e, not], P))), []).
dep(eng:e, 55, (ct(adv, very)+ct(adv(l(i)), far)==>ct(adv(l(i)), very(far))), []).
dep(eng:e, 55, (ct(adv, very)+ct(a, Very_Ct_A)==>ct(a, very(Very_Ct_A))), []).
dep(eng:e, 55, (ct(p, d)+ct(v(V), p(_46356, [_47502, _46370, e, e, e], P))==>ct(v(V), p(_46356, [_50780, _46370, inf, e, e], P))), []).
dep(eng:e, 54, (ct(n(d-very), sp(Sp4, Sp, _12596, [very(e)|Very], Sp6))+ct(a, Very_Ct_A)==>ct(n(d), sp(Sp4, Sp, _12596, [very(Very_Ct_A)|Very], Sp6))), []).
dep(eng:e, 54, (ct(v(aux), p(have, [e, e, _2630, _2636, _2642], [Kw:_2656|Kw3]))+ct(v(V), p(_2670, [_7176, ed, e, e, e], [E:e|Kw5]))==>ct(v(V), p(_2670, [_9216, pf, _2630, _2636, _2642], [E:_2656|Kw5]))), []).
dep(eng:e, 54, (ct(v(aux), p(have, [e, e, _13126, _13132, _13138], [Kw:_13152|Kw3]))+ct(v(V), p(_13166, [_17674, en, e, e, e], [E:e|Kw5]))==>ct(v(V), p(_13166, [_19758, pf, _13126, _13132, _13138], [E:_13152|Kw5]))), []).
dep(eng:e, 53, (ct(n(d), sp(Sp5, Sp4, Sp3, Very_Sp, Sp7))+ct(adv, very)==>ct(n(d-very), sp(Sp5, Sp4, Sp3, [very(e)|Very_Sp], Sp7))), []).
dep(eng:e, 53, (ct(p, l)+ct(n(n), sp(e, e, sing(e), [], front))+ct(p, gen)==>ct(adv(l), front)), []).
dep(eng:e, 53, (ct(v(aux), p(be, [e, _21062, _21068, _21074, _21080], [j:J]))+ct(v(V), p(_21108, [ing, e, e, e, e], [E:e|Kw5]))==>ct(v(V), p(_21108, [i, _21062, _21068, _21074, _21080], [E:J|Kw5]))), []).
dep(eng:e, 53, (ct(v(aux), p(do, [e, e, _13108, e, not], [Kw:_16340]))+ct(v(v), p(_13132, [e, e, e, e, e], [E:e|Kw5]))==>ct(v(v), p(_13132, [e, e, _13108, e, not], [E:_13122|Kw5]))), []).
dep(eng:e, 51, (ct(v(v), p(do, [e, e, _57930, e, e], [Kw:_57944|Kw3]))+ct(neg, not)==>ct(v(aux), p(do, [e, e, _57930, e, not], [Kw4:_63160]))), []).
dep(eng:e, 51, (ct(v(aux), p(_44842, [_45992, _44856, _44862, _44868, e], P))+ct(neg, not)==>ct(v(aux), p(_44842, [_49272, _44856, _44862, _44868, not], P))), []).
dep(eng:e, 50, (ct(conj, because)+ct(p, gen)==>ct(p, cause)), []).
dep(eng:e, 50, (ct(v(V), p(_4866, [e, e, e, e, e], P))+ct(i, s_form)==>ct(v(V), p(_4866, [e, e, s, e, e], P))), []).
dep(eng:e, 50, (ct(v(V), p(_25260, [e, e, e, e, e], P))+ct(i, d_form)==>ct(v(V), p(_25260, [e, e, ed, e, e], P))), []).
dep(eng:e, 50, (ct(v(V), p(_46628, [e, e, e, e, e], P))+ct(i, n_form)==>ct(v(V), p(_46628, [e, en, e, e, e], P))), []).
dep(eng:e, 50, (ct(v(V), p(_4102, [e, e, e, e, e], P))+ct(i, ing_form)==>ct(v(V), p(_4102, [ing, e, e, e, e], P))), []).
dep(eng:e, 42, (ct(d(D), this)+ct(n(d), sp(e, e, sing(e), [], someone(Someone)))==>ct(n(n), sp(this, e, sing(e), [], someone(Someone)))), []).
dep(eng:e, 20, (ct(n(n), sp(e, e, sing(e), [], Sp))+ct(i, s_form)==>ct(n(n), sp(e, e, plur(e), [], Sp))), []).
dg_class_macro(eng:e, n(d), it, sp(this, e, sing(e), [], something(fact))).
dg_class_macro(eng:e, n(n), people, sp(e, e, plur(e), [], people)).
dg_class_macro(eng:e, n(n), one_time, sp(e, e, sing(one), [], time(time))).
dg_class_macro(eng:e, n(n), two_times, sp(e, e, plur(two), [], time(time))).
dg_class_macro(eng:e, n(n), many_times, sp(e, e, plur(many), [], time(time))).
dg_class_macro(eng:e, n(n), all_times, sp(e, e, plur(all), [], time(time))).
dg_class_macro(eng:e, n(d), everything, sp(e, e, sing(all), [], something(thing))).
dg_class_macro(eng:e, n(d), everyone, sp(e, e, sing(all), [], someone(person))).
dg_class_macro(eng:e, n(d), everywhere, sp(e, e, sing(all), [], somewhere(place))).
dg_class_macro(eng:e, n(d), any(Sp_Any), sp(any, e, sing(e), [], Sp_Any)).
dg_class_macro(eng:e, n(d), no(Sp_No), sp(no, e, sing(e), [], Sp_No)).
dg_class_macro(eng:e, n(N4), Sp_N, sp(e, e, sing(e), [], Sp_N)).
dg_class_macro(eng:e, v(v), happen, p(happen, [e, e, e, e, e], [o:e, d:e])).
dg_class_macro(eng:e, v(v), do, p(do, [e, e, e, e, e], [a:e, o:e, d:e, c:e, i:e])).
dg_class_macro(eng:e, v(v), move, p(move, [e, e, e, e, e], [a:e])).
dg_class_macro(eng:e, v(v), die, p(die, [e, e, e, e, e], [o:e])).
dg_class_macro(eng:e, v(v), live, p(live, [e, e, e, e, e], [a:e, c:e])).
dg_class_macro(eng:e, v(v), feel, p(feel, [e, e, e, e, e], [e:e, o:e, g:e])).
dg_class_macro(eng:e, v(v), know, p(know, [e, e, e, e, e], [e:e, o:e, t:e])).
dg_class_macro(eng:e, v(v), think, p(think, [e, e, e, e, e], [e:e, o:e, t:e])).
dg_class_macro(eng:e, v(v), say, p(say, [e, e, e, e, e], [a:e, o:e, d:e, t:e])).
dg_class_macro(eng:e, v(v), hear, p(hear, [e, e, e, e, e], [a:e, o:e, t:e])).
dg_class_macro(eng:e, v(v), see, p(see, [e, e, e, e, e], [a:e, o:e])).
dg_class_macro(eng:e, v(v), want, p(want, [e, e, e, e, e], [e:e, o:e])).
dg_class_macro(eng:e, v(v), touch, p(touch, [e, e, e, e, e], [a:e, o:e])).
dg_class_macro(eng:e, v(v), exist, p(exist, [e, e, e, e, e], [j:e])).
dg_class_macro(eng:e, v(aux), am, p(be, [e, e, s, e, e], [j:am])).
dg_class_macro(eng:e, v(aux), is, p(be, [e, e, s, e, e], [j:is])).
dg_class_macro(eng:e, v(aux), are, p(be, [e, e, s, e, e], [j:are])).
dg_class_macro(eng:e, v(aux), was, p(be, [e, e, ed, e, e], [j:was])).
dg_class_macro(eng:e, v(aux), were, p(be, [e, e, ed, e, e], [j:were])).
dg_class_macro(eng:e, v(aux), V, p(V, [e, e, e, e, e], [j:e])).
dg_class_macro(eng:e, adv(time(Time)), Adv_E, time(e, Adv_E, e)).
paradigm(eng:e, neg_existence(e, no, not)).
paradigm(eng:e, neg_existence(Neg_existence3, Neg_existence, Neg_existence3)).
paradigm(eng:e, neg_existence(e, E, e)).
paradigm(eng:e, indet_sp(ref(unkn), Ref, Sp)).
paradigm(eng:e, indet_sp(any, Any, Sp)).
paradigm(eng:e, indet_sp(no, No, Sp)).
paradigm(eng:e, indet_sp(e, plur(Plur3), Sp_Plur)).
paradigm(eng:e, indet_sp(e, sing(e), someone(person))).
paradigm(eng:e, indet_sp(e, sing(e), something(thing))).
paradigm(eng:e, indet_sp(e, sing(all), someone(person))).
paradigm(eng:e, indet_sp(e, sing(all), something(thing))).
paradigm(eng:e, subj_case(me, me(subj))).
paradigm(eng:e, subj_case(Subj_case, Subj_case)).
paradigm(eng:e, v_form(Pf, pf, i, pf(i(Pf)))).
paradigm(eng:e, v_form(Pf, pf, e, pf(Pf))).
paradigm(eng:e, v_form(I_E, e, i, i(I_E))).
paradigm(eng:e, v_form(Form_E, e, e, Form_E)).
paradigm(eng:e, v_par(be, s, e, e, e, e, e, am, sing(e), me)).
paradigm(eng:e, v_par(be, s, e, e, e, e, e, are, sing(e), you)).
paradigm(eng:e, v_par(be, s, e, e, e, e, e, is, sing(Sing3), Par_Sing)).
paradigm(eng:e, v_par(be, s, e, e, e, e, e, are, plur(Plur3), Par_Plur)).
paradigm(eng:e, v_par(be, ed, e, e, e, e, before(now), was, sing(e), me)).
paradigm(eng:e, v_par(be, ed, e, e, e, e, before(now), were, sing(e), you)).
paradigm(eng:e, v_par(be, ed, e, e, e, e, before(now), was, sing(Sing3), Par_Sing)).
paradigm(eng:e, v_par(be, ed, e, e, e, e, before(now), were, plur(Plur3), Par_Plur)).
paradigm(eng:e, v_par(l(L), s, e, e, e, e, e, am, sing(e), me)).
paradigm(eng:e, v_par(l(L), s, e, e, e, e, e, are, sing(e), you)).
paradigm(eng:e, v_par(l(L), s, e, e, e, e, e, is, sing(Sing4), Par_Sing)).
paradigm(eng:e, v_par(l(L), s, e, e, e, e, e, are, plur(Plur4), Par_Plur)).
paradigm(eng:e, v_par(l(L), ed, e, e, e, e, before(now), was, sing(e), me)).
paradigm(eng:e, v_par(l(L), ed, e, e, e, e, before(now), were, sing(e), you)).
paradigm(eng:e, v_par(l(L), ed, e, e, e, e, before(now), was, sing(Sing4), Par_Sing)).
paradigm(eng:e, v_par(l(L), ed, e, e, e, e, before(now), were, plur(Plur4), Par_Plur)).
paradigm(eng:e, v_par(S, s, e, e, i, e, e, am, sing(e), me)).
paradigm(eng:e, v_par(S, s, e, e, i, e, e, are, sing(e), you)).
paradigm(eng:e, v_par(S, s, e, e, i, e, e, is, sing(Sing4), Par_Sing)).
paradigm(eng:e, v_par(S, s, e, e, i, e, e, are, plur(Plur4), Par_Plur)).
paradigm(eng:e, v_par(Ed, ed, e, e, i, e, before(now), was, sing(e), me)).
paradigm(eng:e, v_par(Ed, ed, e, e, i, e, before(now), were, sing(e), you)).
paradigm(eng:e, v_par(Ed, ed, e, e, i, e, before(now), was, sing(Sing4), Par_Sing)).
paradigm(eng:e, v_par(Ed, ed, e, e, i, e, before(now), were, plur(Plur4), Par_Plur)).
paradigm(eng:e, v_par(E, e, fut, Fut, E4, e, after(now), e, E5, Par)).
paradigm(eng:e, v_par(Ed, ed, fut, Fut, E, e, after(before), e, E5, Par)).
paradigm(eng:e, v_par(E, e, can, Can, Can4, can, e, e, E5, Par)).
paradigm(eng:e, v_par(Ed, ed, can, Can, Can4, can, before(now), e, E, Par)).
paradigm(eng:e, v_par(S, s, e, pf, Pf, e, e, e, sing(Sing), something(Something))).
paradigm(eng:e, v_par(S, s, e, pf, Pf, e, e, e, sing(Sing), someone(Someone))).
paradigm(eng:e, v_par(E, e, e, pf, Pf, e, e, e, E4, Par)).
paradigm(eng:e, v_par(Ed, ed, e, pf, Pf, e, before(before), e, E, Par)).
paradigm(eng:e, v_par(S, s, e, e, e, e, e, e, sing(Sing), something(Something))).
paradigm(eng:e, v_par(S, s, e, e, e, e, e, e, sing(Sing), someone(Someone))).
paradigm(eng:e, v_par(E, e, e, e, e, e, e, e, E3, me)).
paradigm(eng:e, v_par(E, e, e, e, e, e, e, e, E3, you)).
paradigm(eng:e, v_par(E, e, e, e, e, e, e, e, plur(Plur4), Par_Plur)).
paradigm(eng:e, v_par(Ed, ed, e, e, e, e, before(now), e, E, Par)).
paradigm(eng:e, duration_mod(sp(ref(unkn), e, sing(e), [Mod_Sp], time(time)), Mod_Sp)).
paradigm(eng:e, duration_mod(sp(some, e, sing(e), [], time(time)), some)).
paradigm(eng:e, time_ext(some, Some, some)).
paradigm(eng:e, time_ext(ref(unkn), [Ext], Ext)).
paradigm(eng:e, time_loc(this, This, this)).
paradigm(eng:e, time_loc(that, That, that)).
paradigm(eng:e, time_loc(some, Some, some)).
paradigm(eng:e, time_loc(Same, same, same)).
paradigm(eng:e, time_loc(e, e, e)).
paradigm(eng:e, time_agr(Before, before, that, time(Before, before, that), before(before))).
paradigm(eng:e, time_agr(After, after, that, time(After, after, that), after(before))).
paradigm(eng:e, time_agr(Before, before, Time, time(Before, before, Time), before(now))).
paradigm(eng:e, time_agr(After, after, Time, time(After, after, Time), after(now))).
paradigm(eng:e, time_agr(e, at, Time_At, time(e, at(after), Time_At), after(now))).
paradigm(eng:e, time_agr(e, at, Time_At, time(e, at(before), Time_At), before(now))).
paradigm(eng:e, time_agr(e, now, e, time(e, now, e), e)).
paradigm(eng:e, eval_for(good, Good, good(Good))).
paradigm(eng:e, eval_for(bad, Bad, bad(Bad))).
paradigm(eng:e, eval_for(very(good), Good_Very, very(good(Good_Very)))).
paradigm(eng:e, eval_for(very(bad), Bad_Very, very(bad(Bad_Very)))).
paradigm(eng:e, np_number(e, sing(e), sing(e))).
paradigm(eng:e, np_number(e, sing(all), sing(all))).
paradigm(eng:e, np_number(e, plur(e), plur(e))).
paradigm(eng:e, np_number(one, sing(e), sing(one))).
paradigm(eng:e, np_number(two, plur(e), plur(two))).
paradigm(eng:e, np_number(some, plur(e), plur(some))).
paradigm(eng:e, np_number(many, plur(e), plur(many))).
paradigm(eng:e, np_number(all, plur(e), plur(all))).
paradigm(eng:e, inst_arg(E, _21666, [E:e|Kw], [E:_21666|Kw])).
paradigm(eng:e, inst_arg(E, _32466, [_32474, E:e|Kw], [_32474, E:_32466|Kw])).
paradigm(eng:e, inst_arg(E, _43266, [_43274, _43280, E:e|Kw], [_43274, _43280, E:_43266|Kw])).
paradigm(eng:e, inst_arg(E, _52268, [_53416, _52282, _52288, E:e], [_56692, _52282, _52288, E:_52268])).
paradigm(eng:e, inst_adj(yes, Yes, E, e, e)).
paradigm(eng:e, inst_adj(no, l, E_L, E_L, e)).
paradigm(eng:e, inst_adj(no, m, Adj_E_M, e, Adj_E_M)).
paradigm(eng:e, is_relational(kind)).
paradigm(eng:e, is_relational(part)).
paradigm(eng:e, is_relational(side)).
paradigm(eng:e, pp_role(c, sp(Sp5, Sp4, Sp, Something, something(Something6)), i)).
paradigm(eng:e, pp_role(Pp_role3, Pp_role, Pp_role3)).
paradigm(eng:e, pred_neg(not, Not, not(Not))).
paradigm(eng:e, pred_neg(e, E, E)).
paradigm(eng:e, prop_obj(know)).
paradigm(eng:e, prop_obj(think)).
paradigm(eng:e, good_for(good, [b:e])).
paradigm(eng:e, good_for(bad, [b:e])).
paradigm(eng:e, good_for(Good_for, [])).
paradigm(eng:e, eval(good)).
paradigm(eng:e, eval(bad)).
paradigm(eng:e, eval(very(good))).
paradigm(eng:e, eval(very(bad))).
paradigm(eng:e, obj_det(not, not, any, e)).
paradigm(eng:e, obj_det(e, not, no, e)).
paradigm(eng:e, obj_det(e, e, E, E)).
paradigm(eng:e, obj_det(not, not, Det_Not, Det_Not)).
allo(eng:e, `es`, `s`, [], [E_triggers], [], [], [], ['<<'(E_triggers, e_triggers)]).
allo(eng:e, `d`, `ed`, [], [Vow], `#`, [], [], ['<<'(Vow, vow)]).
allo(eng:e, `d`, `ed`, [], `oul`, `#`, [], [], []).
allo(eng:e, `d`, `ed`, [], `hear`, `#`, [], [], []).
allo(eng:e, `t`, `ed`, [], `fel`, `#`, `feel`, [], []).
allo(eng:e, `ne`, `en`, [], `do`, `#`, [], [], []).
allo(eng:e, `'`, `'s`, [], `s`, `#`, [], [], []).
allo(eng:e, `peopl`, `people`, [], [], `ing#`, [], [], []).
allo(eng:e, `someon`, `someone`, [], [], `ing#`, [], [], []).
allo(eng:e, `sometim`, `sometime`, [], [], `ing#`, [], [], []).
allo(eng:e, `somewher`, `somewhere`, [], [], `ing#`, [], [], []).
allo(eng:e, `anyon`, `anyone`, [], [], `ing#`, [], [], []).
allo(eng:e, `anytim`, `anytime`, [], [], `ing#`, [], [], []).
allo(eng:e, `anywher`, `anywhere`, [], [], `ing#`, [], [], []).
allo(eng:e, `everyon`, `everyone`, [], [], `ing#`, [], [], []).
allo(eng:e, `everywher`, `everywhere`, [], [], `ing#`, [], [], []).
allo(eng:e, `noon`, `noone`, [], [], `ing#`, [], [], []).
allo(eng:e, `notim`, `notime`, [], [], `ing#`, [], [], []).
allo(eng:e, `nowher`, `nowhere`, [], [], `ing#`, [], [], []).
allo(eng:e, `tim`, `time`, [], [], `ing#`, [], [], []).
allo(eng:e, `plac`, `place`, [], [], `ing#`, [], [], []).
allo(eng:e, `m`, `me`, [], [], `ing#`, [], [], []).
allo(eng:e, `body`, `bodi`, [], [], `#`, [], [], []).
allo(eng:e, `on`, `one`, [], [], `ing#`, [], [], []).
allo(eng:e, `som`, `some`, [], [], `ing#`, [], [], []).
allo(eng:e, `mor`, `more`, [], [], `ing#`, [], [], []).
allo(eng:e, `th`, `the`, [], [], `ing#`, [], [], []).
allo(eng:e, `thes`, `these`, [], [], `ing#`, [], [], []).
allo(eng:e, `sam`, `same`, [], [], `ing#`, [], [], []).
allo(eng:e, `els`, `else`, [], [], `ing#`, [], [], []).
allo(eng:e, `lik`, `like`, [], [], `ing#`, [], [], []).
allo(eng:e, `b`, `be`, [], [], `ing#`, [], [], []).
allo(eng:e, `ar`, `are`, [], [], `ing#`, [], [], []).
allo(eng:e, `wer`, `were`, [], [], `ing#`, [], [], []).
allo(eng:e, `hav`, `have`, [], [], `ing#`, [], [], []).
allo(eng:e, `ha`, `have`, [], [], `d#`, [], `ed`, []).
allo(eng:e, `ha`, `have`, [], [], `s#`, [], `s`, []).
allo(eng:e, `woul`, `will`, [], [], `d#`, [], `ed`, []).
allo(eng:e, `coul`, `can`, [], [], `d#`, [], `ed`, []).
allo(eng:e, `di`, `do`, [], [], `d#`, [], `ed`, []).
allo(eng:e, `mov`, `move`, [], [], `ing#`, [], [], []).
allo(eng:e, `liv`, `live`, [], [], `ing#`, [], [], []).
allo(eng:e, `di`, `die`, [], [], `ing#`, [], [], []).
allo(eng:e, `tru`, `true`, [], [], `ing#`, [], [], []).
allo(eng:e, `say`, `sai`, [], [], `#`, [], [], []).
allo(eng:e, `se`, `see`, [], [], `ing#`, [], [], []).
allo(eng:e, `saw#`, `see`, `ed#`, [], [], [], [], []).
allo(eng:e, `fel`, `feel`, [], [], `t#`, [], `ed`, []).
allo(eng:e, `insid`, `inside`, [], [], `ing#`, [], [], []).
allo(eng:e, `abov`, `above`, [], [], `ing#`, [], [], []).
allo(eng:e, `her`, `here`, [], [], `ing#`, [], [], []).
allo(eng:e, `ther`, `there`, [], [], `ing#`, [], [], []).
allo(eng:e, `sid`, `side`, [], [], `ing#`, [], [], []).
allo(eng:e, `befor`, `before`, [], [], `ing#`, [], [], []).
allo(eng:e, `onc`, `once`, [], [], `ing#`, [], [], []).
allo(eng:e, `twic`, `twice`, [], [], `ing#`, [], [], []).
allo(eng:e, `becaus`, `because`, [], [], `ing#`, [], [], []).
allo(eng:e, `mayb`, `maybe`, [], [], `ing#`, [], [], []).
m(eng:e, i, `s`, s_form).
m(eng:e, i, `ing`, ing_form).
m(eng:e, i, `ed`, d_form).
m(eng:e, i, `en`, n_form).
m(eng:e, gen, `'s`, gen).
m(eng:e, n(n), `people`, people).
m(eng:e, n(d), `someone`, someone(person)).
m(eng:e, n(d), `something`, something(thing)).
m(eng:e, n(d), `sometime`, time(time)).
m(eng:e, n(d), `somewhere`, somewhere(place)).
m(eng:e, n(d), `somehow`, manner(manner)).
m(eng:e, n(d), `anyone`, any(someone(person))).
m(eng:e, n(d), `anything`, any(something(thing))).
m(eng:e, n(d), `anytime`, any(time(time))).
m(eng:e, n(d), `anywhere`, any(somewhere(place))).
m(eng:e, n(d), `anyhow`, any(manner(manner))).
m(eng:e, n(d), `everyone`, everyone).
m(eng:e, n(d), `everything`, everything).
m(eng:e, n(d), `everywhere`, everything).
m(eng:e, n(d), `noone`, no(someone(person))).
m(eng:e, n(d), `nothing`, no(something(thing))).
m(eng:e, n(d), `notime`, no(time(time))).
m(eng:e, n(d), `nowhere`, no(somewhere(place))).
m(eng:e, n(d), `nohow`, no(manner(manner))).
m(eng:e, n(n), `person`, someone(person)).
m(eng:e, n(n), `thing`, something(thing)).
m(eng:e, n(n), `time`, time(time)).
m(eng:e, n(n), `place`, somewhere(place)).
m(eng:e, n(n), `way`, manner(manner)).
m(eng:e, n(p), `I`, me(subj)).
m(eng:e, n(p), `me`, me).
m(eng:e, n(p), `you`, you).
m(eng:e, n(n), `bodi`, body).
m(eng:e, n(n), `kind`, kind).
m(eng:e, n(n), `part`, part).
m(eng:e, num, `one`, one).
m(eng:e, num, `two`, two).
m(eng:e, num, `some`, some).
m(eng:e, num, `many`, many).
m(eng:e, num, `all`, all).
m(eng:e, alt, `more`, more).
m(eng:e, a, `good`, good).
m(eng:e, a, `bad`, bad).
m(eng:e, a, `big`, big).
m(eng:e, a, `small`, small).
m(eng:e, d(sing(Sing)), `this`, this).
m(eng:e, d(sing(Sing)), `that`, that).
m(eng:e, d(sing(Sing)), `a`, ref(unkn)).
m(eng:e, d(sing(Sing)), `an`, ref(unkn)).
m(eng:e, d(D), `the`, ref(known)).
m(eng:e, d(D), `any`, any).
m(eng:e, d(D), `no`, no).
m(eng:e, d(plur(Plur)), `these`, this).
m(eng:e, n(d), `it`, it).
m(eng:e, alt, `same`, same).
m(eng:e, alt, `other`, other).
m(eng:e, alt, `else`, other(1)).
m(eng:e, alt, `another`, other(2)).
m(eng:e, adv, `very`, very).
m(eng:e, comp, `like`, like).
m(eng:e, v(aux), `be`, be).
m(eng:e, v(aux), `am`, am).
m(eng:e, v(aux), `is`, is).
m(eng:e, v(aux), `was`, was).
m(eng:e, v(aux), `are`, are).
m(eng:e, v(aux), `were`, were).
m(eng:e, v(aux), `have`, have).
m(eng:e, v(aux), `will`, fut).
m(eng:e, v(aux), `can`, can).
m(eng:e, v(v), `do`, do).
m(eng:e, v(v), `happen`, happen).
m(eng:e, v(v), `move`, move).
m(eng:e, v(v), `touC`, touch).
m(eng:e, v(v), `live`, live).
m(eng:e, v(v), `die`, die).
m(eng:e, v(v), `exist`, exist).
m(eng:e, n(n), `word`, word).
m(eng:e, a, `true`, true).
m(eng:e, v(v), `sai`, say).
m(eng:e, v(v), `hear`, hear).
m(eng:e, v(v), `see`, see).
m(eng:e, v(v), `think`, think).
m(eng:e, v(v), `feel`, feel).
m(eng:e, v(v), `know`, know).
m(eng:e, v(v), `want`, want).
m(eng:e, adv(l(tr)), `inside`, inside).
m(eng:e, adv(l(tr)), `above`, above).
m(eng:e, adv(l(tr)), `below`, below).
m(eng:e, adv(l(tr)), `near`, near).
m(eng:e, adv(l(i)), `far`, far).
m(eng:e, adv(l(i)), `here`, here).
m(eng:e, adv(l(i)), `there`, there).
m(eng:e, n(n), `front`, front).
m(eng:e, n(n), `side`, side).
m(eng:e, adv(time(i)), `now`, now).
m(eng:e, adv(time(tr)), `before`, before).
m(eng:e, adv(time(tr)), `after`, after).
m(eng:e, a, `long`, long).
m(eng:e, a, `Sort`, short).
m(eng:e, n(n), `once`, one_time).
m(eng:e, n(n), `twice`, two_times).
m(eng:e, n(n), `often`, many_times).
m(eng:e, n(n), `always`, all_times).
m(eng:e, neg, `not`, not).
m(eng:e, conj, `because`, because).
m(eng:e, conj, `if`, if).
m(eng:e, conj, `when`, if).
m(eng:e, conj, `as`, as).
m(eng:e, p, `to`, d).
m(eng:e, p, `in`, l).
m(eng:e, p, `on`, l).
m(eng:e, p, `with`, c).
m(eng:e, p, `about`, t).
m(eng:e, p, `for`, b).
m(eng:e, p, `at`, time).
m(eng:e, p, `of`, gen).
m(eng:e, p, `from`, src).
m(eng:e, p, `towards`, g).
m(eng:e, adv(sent), `maybe`, maybe).
m(eng:e, n(n), `God`, someone(god)).
m(eng:e, n(n), `bird`, something(bird)).
m(eng:e, group, [], begin).
m(eng:e, group, [], end).
