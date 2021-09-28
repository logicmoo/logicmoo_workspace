morph_grammar_type(eng:e, dependency).
synt_grammar_type(eng:e, dependency).
max_dep_threshold(eng:e, 4).
morph_threshold(eng:e, 40).
dep_threshold(eng:e, 4, 500).
dep_threshold(eng:e, 3, 400).
dep_threshold(eng:e, 2, 300).
dep_threshold(eng:e, 1, 200).
transcr_table(eng:e, [[110, 111, 45, 111, 110, 101]:[110, 111, 111, 110, 101], [99, 97, 110, 110, 111, 116]:[99, 97, 110, 32, 110, 111, 116], [115, 111, 109, 101, 116, 105, 109, 101, 115]:[115, 111, 109, 101, 32, 116, 105, 109, 101, 115], [115, 104]:[83], [99, 104]:[67]]).
phonetic_class(eng:e, e_triggers, [[67], [83], [115], [122], [120], [111], [105]]).
phonetic_class(eng:e, vow, [[97], [101], [105], [111], [117]]).
arc(eng:e, n(_G84319), gen).
arc(eng:e, i, gen).
arc(eng:e, n(n), i).
arc(eng:e, v(_G84319), i).
arc(eng:e, _G84316, stop).
arc(eng:e, start, _G84317).
dep(eng:e, 410, ct(s, s(_G84336, _G84337, _G84338, _G84339, _G84340, p(think, [e:_G84352, o:e|_G84355]), _G84342, _G84343))+ct(d(_G84363), that)+ct(s, s(_G84368, _G84369, _G84370, _G84371, _G84372, _G84373, _G84374, _G84375))==>ct(s, s(_G84336, _G84337, _G84338, _G84339, _G84340, p(think, [e:_G84352, o:s(_G84368, _G84369, _G84370, _G84371, _G84372, _G84373, _G84374, _G84375)|_G84355]), _G84342, _G84343)), []).
dep(eng:e, 410, ct(s, s(_G84336, _G84337, _G84338, _G84339, _G84340, p(know, [e:_G84352, o:e|_G84355]), _G84342, _G84343))+ct(d(_G84363), that)+ct(s, s(_G84368, _G84369, _G84370, _G84371, _G84372, _G84373, _G84374, _G84375))==>ct(s, s(_G84336, _G84337, _G84338, _G84339, _G84340, p(know, [e:_G84352, o:s(_G84368, _G84369, _G84370, _G84371, _G84372, _G84373, _G84374, _G84375)|_G84355]), _G84342, _G84343)), []).
dep(eng:e, 53, ct(p, l)+ct(n(n), sp(e, e, sing(e), [], front))+ct(p, gen)==>ct(adv(l), front), []).
dep(eng:e, 425, ct(s, _G84345)+ct(clause(as), _G84348)==>ct(s, as(_G84348, _G84345)), []).
dep(eng:e, 425, ct(s, _G84345)+ct(clause(when), _G84348)==>ct(s, when(_G84348, _G84345)), []).
dep(eng:e, 425, ct(clause(when), _G84345)+ct(s, _G84350)==>ct(s, when(_G84345, _G84350)), []).
dep(eng:e, 425, ct(s, _G84345)+ct(clause(because), _G84348)==>ct(s, because(_G84348, _G84345)), []).
dep(eng:e, 425, ct(clause(because), _G84345)+ct(s, _G84350)==>ct(s, because(_G84345, _G84350)), []).
dep(eng:e, 425, ct(s, _G84345)+ct(clause(if), _G84348)==>ct(s, if(_G84348, _G84345)), []).
dep(eng:e, 425, ct(clause(if), _G84345)+ct(s, _G84350)==>ct(s, if(_G84345, _G84350)), []).
dep(eng:e, 420, ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(_G84356, [o:sp(this, e, sing(e), [], something(fact))|_G84360]), e, e))+ct(v(_G84378), p(_G84380, [e, e, inf, e, e], _G84382))==>ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(_G84356, [o:p(_G84380, _G84382)|_G84360]))), [eval(_G84356)]).
dep(eng:e, 420, ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(_G84356, [o:sp(this, e, sing(e), [], something(fact))|_G84360]), e, e))+ct(clause(if), _G84376)==>ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(_G84356, [o:_G84376|_G84360]))), [eval(_G84356)]).
dep(eng:e, 415, ct(s, _G84345)+ct(pp(cause), _G84348)==>ct(s, because(_G84345, _G84348)), []).
dep(eng:e, 415, ct(pp(cause), _G84345)+ct(s, _G84350)==>ct(s, because(_G84350, _G84345)), []).
dep(eng:e, 415, ct(adv(sent), maybe)+ct(s, _G84350)==>ct(s, maybe(_G84350)), []).
dep(eng:e, 414, ct(neg, not)+ct(pp(cause), _G84348)==>ct(pp(cause), not(_G84348)), []).
dep(eng:e, 413, ct(conj, _G84345)+ct(s, _G84348)==>ct(clause(_G84345), _G84348), []).
dep(eng:e, 410, ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(want, [e:_G84363, o:sp(_G84371, _G84372, _G84373, _G84374, _G84375)]), _G84353, _G84354))+ct(v(_G84380), p(_G84382, [_G84386, _G84389, inf, e, _G84398], [_G84404:e|_G84402]))==>ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(want, [e:_G84363, o:[p(_G84437, [_G84404:sp(_G84371, _G84372, _G84373, _G84374, _G84375)|_G84402])]]), _G84353, _G84354)), [v_form(_G84382, _G84389, _G84386, _G84458), pred_neg(_G84398, _G84458, _G84437)]).
dep(eng:e, 410, ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(want, [e:_G84363, o:e]), _G84353, _G84354))+ct(v(_G84374), p(_G84376, [_G84380, _G84383, inf, e, _G84392], [_G84398:e|_G84396]))==>ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(want, [e:_G84363, o:[p(_G84431, [_G84398:e|_G84396])]]), _G84353, _G84354)), [v_form(_G84376, _G84383, _G84380, _G84446), pred_neg(_G84392, _G84446, _G84431)]).
dep(eng:e, 410, ct(s, s(_G84350, _G84351, _G84352, _G84353, _G84354, p(think, [e:_G84366, o:e|_G84369]), _G84356, _G84357))+ct(d(_G84377), that)+ct(s, s(_G84382, _G84383, _G84384, _G84385, _G84386, _G84387, _G84388, _G84389))==>ct(s, s(_G84350, _G84351, _G84352, _G84353, _G84354, p(think, [e:_G84366, o:s(_G84382, _G84383, _G84384, _G84385, _G84386, _G84387, _G84388, _G84389)|_G84369]), _G84356, _G84357)), []).
dep(eng:e, 410, ct(s, s(_G84350, _G84351, _G84352, _G84353, _G84354, p(know, [e:_G84366, o:e|_G84369]), _G84356, _G84357))+ct(d(_G84377), that)+ct(s, s(_G84382, _G84383, _G84384, _G84385, _G84386, _G84387, _G84388, _G84389))==>ct(s, s(_G84350, _G84351, _G84352, _G84353, _G84354, p(know, [e:_G84366, o:s(_G84382, _G84383, _G84384, _G84385, _G84386, _G84387, _G84388, _G84389)|_G84369]), _G84356, _G84357)), []).
dep(eng:e, 320, ct(s, s(_G84347, _G84348, _G84349, e, e, p(_G84356, _G84357), _G84353, _G84354))+ct(pp(b), _G84360)==>ct(s, s(_G84347, _G84348, _G84349, e, dur(_G84376), p(_G84356, _G84357), _G84353, _G84354)), [duration_mod(_G84360, _G84376)]).
dep(eng:e, 320, ct(s, s(_G84347, _G84348, _G84349, e, e, p(_G84356, _G84357), _G84353, _G84354))+ct(n(_G84362), sp(e, e, plur(_G84370), [], time(time)))==>ct(s, s(_G84347, _G84348, _G84349, freq(_G84370), e, p(_G84356, _G84357), _G84353, _G84354)), []).
dep(eng:e, 320, ct(n(_G84347), sp(e, e, plur(_G84355), [], time(time)))+ct(s, s(_G84362, _G84363, _G84364, e, e, p(_G84371, _G84372), _G84368, _G84369))==>ct(s, s(_G84362, _G84363, _G84364, freq(_G84355), e, p(_G84371, _G84372), _G84368, _G84369)), []).
dep(eng:e, 320, ct(s, s(_G84347, _G84348, _G84349, e, e, p(_G84356, _G84357), _G84353, _G84354))+ct(n(_G84362), sp(e, e, sing(one), [], time(time)))==>ct(s, s(_G84347, _G84348, _G84349, freq(one), e, p(_G84356, _G84357), _G84353, _G84354)), []).
dep(eng:e, 320, ct(n(_G84347), sp(e, e, sing(one), [], time(time)))+ct(s, s(_G84362, _G84363, _G84364, e, e, p(_G84371, _G84372), _G84368, _G84369))==>ct(s, s(_G84362, _G84363, _G84364, freq(one), e, p(_G84371, _G84372), _G84368, _G84369)), []).
dep(eng:e, 320, ct(s, s(_G84347, _G84348, _G84349, e, _G84351, p(_G84356, _G84357), _G84353, _G84354))+ct(adv(time(_G84364)), time(_G84366, _G84367, _G84368))==>ct(s, s(_G84347, _G84374, _G84349, e, _G84351, p(_G84356, _G84357), _G84353, _G84354)), [time_agr(_G84366, _G84367, _G84368, _G84374, _G84348)]).
dep(eng:e, 320, ct(adv(time(_G84349)), time(_G84351, _G84352, _G84353))+ct(s, s(_G84358, _G84359, _G84360, e, _G84362, p(_G84367, _G84368), _G84364, _G84365))==>ct(s, s(_G84358, _G84374, _G84360, e, _G84362, p(_G84367, _G84368), _G84364, _G84365)), [time_agr(_G84351, _G84352, _G84353, _G84374, _G84359)]).
dep(eng:e, 320, ct(s, s(_G84347, _G84348, _G84349, e, _G84351, p(_G84356, _G84357), _G84353, _G84354))+ct(pp(time), sp(_G84364, _G84365, sing(e), [], time(time)))==>ct(s, s(_G84347, _G84378, _G84349, e, _G84351, p(_G84356, _G84357), _G84353, _G84354)), [time_loc(_G84364, _G84365, _G84394), time_agr(e, at, _G84394, _G84378, _G84348)]).
dep(eng:e, 320, ct(pp(time), sp(_G84349, _G84350, sing(e), [], time(time)))+ct(s, s(_G84362, _G84363, _G84364, e, _G84366, p(_G84371, _G84372), _G84368, _G84369))==>ct(s, s(_G84362, _G84378, _G84364, e, _G84366, p(_G84371, _G84372), _G84368, _G84369)), [time_loc(_G84349, _G84350, _G84394), time_agr(e, at, _G84394, _G84378, _G84363)]).
dep(eng:e, 315, ct(s, s(not, _G84348, _G84349, e, _G84351, p(_G84356, _G84357), _G84353, _G84354))+ct(adv(a), anymore)==>ct(s, s(neg, _G84348, _G84349, e, _G84351, p(more(_G84356), _G84357), _G84353, _G84354)), []).
dep(eng:e, 310, ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(_G84356, _G84357), _G84353, e))+ct(adv, like(this))==>ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(_G84356, _G84357), _G84353, manner(this))), []).
dep(eng:e, 310, ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(_G84356, _G84357), _G84353, e))+ct(pp(l), sp(_G84364, _G84365, _G84366, _G84367, manner(_G84370)))==>ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(_G84356, _G84357), _G84353, sp(_G84364, _G84365, _G84366, _G84367, manner(_G84370)))), []).
dep(eng:e, 310, ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(_G84356, _G84357), e, _G84354))+ct(pp(l), sp(_G84364, _G84365, _G84366, _G84367, somewhere(_G84370)))==>ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(_G84356, _G84357), l(sp(_G84364, _G84365, _G84366, _G84367, somewhere(_G84370))), _G84354)), []).
dep(eng:e, 310, ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(_G84356, _G84357), e, _G84354))+ct(adv(l(_G84364)), _G84360)==>ct(s, s(_G84347, _G84348, _G84349, _G84350, _G84351, p(_G84356, _G84357), l(_G84360), _G84354)), []).
dep(eng:e, 310, ct(s, s(_G84347, _G84348, _G84349, _G84350, e, p(_G84356, _G84357), _G84353, _G84354))+ct(pp(b), sp(ref(unkn), e, sing(e), [_G84374], time(1)))==>ct(s, s(_G84347, _G84348, _G84349, _G84350, dur(_G84374), p(_G84356, _G84357), _G84353, _G84354)), []).
dep(eng:e, 310, ct(s, s(_G84347, _G84348, _G84349, e, _G84351, p(_G84356, _G84357), _G84353, _G84354))+ct(n(_G84362), sp(_G84364, _G84365, _G84366, _G84367, times(_G84370)))==>ct(s, s(_G84347, _G84348, _G84349, sp(_G84364, _G84365, _G84366, _G84367, times(_G84370)), _G84351, p(_G84356, _G84357), _G84353, _G84354)), []).
dep(eng:e, 305, ct(n(_G84347), sp(_G84349, _G84350, _G84351, _G84352, _G84353))+ct(v(stat), p(_G84360, [e, _G84367, _G84370, _G84373, _G84376], [_G84382:_G84383|_G84380]))==>ct(s, s(_G84376, _G84389, _G84390, e, e, p(_G84397, [_G84382:sp(_G84349, _G84350, _G84351, _G84352, _G84410)|_G84380]), e, e)), [subj_case(_G84410, _G84353), v_par(be, _G84370, _G84373, _G84367, e, _G84390, _G84389, _G84383, _G84351, _G84410), v_form(_G84360, _G84367, e, _G84397)]).
dep(eng:e, 305, ct(n(_G84347), sp(_G84349, _G84350, _G84351, _G84352, _G84353))+ct(v(v), p(_G84360, [_G84364, _G84367, _G84370, _G84373, _G84376], [_G84382:_G84383|_G84380]))==>ct(s, s(_G84376, _G84389, _G84390, e, e, p(_G84397, [_G84382:sp(_G84349, _G84350, _G84351, _G84352, _G84410)|_G84380]), e, e)), [subj_case(_G84410, _G84353), v_par(_G84360, _G84370, _G84373, _G84367, _G84364, _G84390, _G84389, _G84383, _G84351, _G84410), v_form(_G84360, _G84367, _G84364, _G84397)]).
dep(eng:e, 305, ct(adv(l(i)), there)+ct(v(v), p(be, [_G84360, _G84363, _G84366, _G84369, _G84372], [j:_G84379, o:sp(_G84387, _G84388, _G84389, _G84390, _G84391)]))==>ct(s, s(_G84396, _G84397, _G84398, e, e, p(_G84405, [j:sp(_G84387, _G84388, _G84389, _G84390, _G84418)]), e, e)), [subj_case(_G84418, _G84391), indet_sp(_G84387, _G84389, _G84391), neg_existence(_G84372, _G84387, _G84396), v_par(be, _G84366, _G84369, _G84363, _G84360, _G84398, _G84397, _G84379, _G84389, _G84418), v_form(exist, _G84363, _G84360, _G84405)]).
dep(eng:e, 250, ct(v(aux), p(have, _G84350, [j:_G84357]))+ct(n(_G84362), sp(_G84364, _G84365, _G84366, _G84367, _G84368))==>ct(v(v), p(have, _G84350, [j:_G84357, o:sp(_G84364, _G84365, _G84366, _G84367, _G84368)])), []).
dep(eng:e, 250, ct(v(aux), p(be, _G84350, [j:_G84357]))+ct(adv, like(_G84362))==>ct(v(stat), p(like, _G84350, [j:_G84357, o:_G84362])), []).
dep(eng:e, 250, ct(v(aux), p(be, _G84350, [j:_G84357]))+ct(adv(l(_G84364)), _G84360)==>ct(v(v), p(be, _G84350, [j:_G84357, l:_G84360])), []).
dep(eng:e, 250, ct(v(aux), p(be, _G84350, [j:_G84357]))+ct(a, _G84360)==>ct(v(stat), p(_G84360, _G84350, [o:_G84357|_G84372])), [good_for(_G84360, _G84372)]).
dep(eng:e, 250, ct(v(aux), p(be, _G84350, [j:_G84357]))+ct(n(_G84362), sp(_G84364, _G84365, _G84366, _G84367, _G84368))==>ct(v(v), p(be, _G84350, [j:_G84357, o:sp(_G84364, _G84365, _G84366, _G84367, _G84368)])), []).
dep(eng:e, 250, ct(v(aux), p(be, _G84350, [j:_G84357]))+ct(pp(_G84362), sp(_G84364, _G84365, _G84366, _G84367, _G84368))==>ct(v(v), p(be, _G84350, [j:_G84357, _G84362:sp(_G84364, _G84365, _G84366, _G84367, _G84368)])), []).
dep(eng:e, 230, ct(v(aux), p(_G84349, [_G84353, _G84356, _G84359, _G84362, e], _G84351))+ct(neg, not)==>ct(v(aux), p(_G84349, [_G84353, _G84356, _G84359, _G84362, not], _G84351)), []).
dep(eng:e, 225, ct(v(_G84347), p(_G84349, _G84350, [_G84353, _G84356, _G84359, _G84362, _G84368:e|_G84366]))+ct(pp(_G84368), sp(_G84376, _G84377, _G84378, _G84379, _G84380))==>ct(v(_G84347), p(_G84349, _G84350, [_G84353, _G84356, _G84359, _G84362, _G84368:sp(_G84376, _G84377, _G84378, _G84379, _G84380)|_G84366])), []).
dep(eng:e, 220, ct(v(_G84347), p(_G84349, _G84350, [_G84353, _G84356, _G84359, _G84365:e|_G84363]))+ct(pp(_G84365), sp(_G84373, _G84374, _G84375, _G84376, _G84377))==>ct(v(_G84347), p(_G84349, _G84350, [_G84353, _G84356, _G84359, _G84365:sp(_G84373, _G84374, _G84375, _G84376, _G84377)|_G84363])), []).
dep(eng:e, 215, ct(v(_G84347), p(_G84349, _G84350, [_G84353, _G84356, _G84362:e|_G84360]))+ct(pp(_G84362), sp(_G84370, _G84371, _G84372, _G84373, _G84374))==>ct(v(_G84347), p(_G84349, _G84350, [_G84353, _G84356, _G84362:sp(_G84370, _G84371, _G84372, _G84373, _G84374)|_G84360])), []).
dep(eng:e, 210, ct(v(_G84347), p(_G84349, _G84350, [_G84356:_G84357, _G84362:e|_G84360]))+ct(pp(_G84362), sp(_G84370, _G84371, _G84372, _G84373, _G84374))==>ct(v(_G84347), p(_G84349, _G84350, [_G84356:_G84357, _G84362:sp(_G84370, _G84371, _G84372, _G84373, _G84374)|_G84360])), []).
dep(eng:e, 207, ct(v(_G84347), p(_G84349, _G84350, [_G84356:_G84357, o:e|_G84360]))+ct(d(_G84368), this)==>ct(v(_G84347), p(_G84349, _G84350, [_G84356:_G84357, o:this|_G84360])), []).
dep(eng:e, 205, ct(v(_G84347), p(_G84349, _G84350, [_G84356:_G84357, o:e|_G84360]))+ct(alt, more)==>ct(v(_G84347), p(_G84349, _G84350, [_G84356:_G84357, o:more|_G84360])), []).
dep(eng:e, 205, ct(v(_G84347), p(_G84349, [_G84353, _G84356, _G84359, _G84362, _G84365], [_G84371:_G84372, o:e|_G84375]))+ct(n(_G84383), sp(_G84385, _G84386, _G84387, _G84388, _G84389))==>ct(v(_G84347), p(_G84349, [_G84353, _G84356, _G84359, _G84362, _G84412], [_G84371:_G84372, o:sp(_G84427, _G84386, _G84387, _G84388, _G84389)|_G84375])), [obj_det(_G84365, _G84412, _G84385, _G84427)]).
dep(eng:e, 125, ct(p, _G84345)+ct(d(_G84350), this)==>ct(pp(_G84355), sp(this, e, sing(e), [], something(fact))), [pp_role(_G84345, this, _G84355)]).
dep(eng:e, 124, ct(p, _G84345)+ct(n(_G84350), _G84348)==>ct(pp(_G84355), _G84348), [pp_role(_G84345, _G84348, _G84355)]).
dep(eng:e, 122, ct(d(_G84347), any)+ct(alt, more)==>ct(adv(a), anymore), []).
dep(eng:e, 121, ct(num, all)+ct(n(n), sp(_G84352, _G84353, plur(e), _G84355, _G84356))==>ct(n(n), sp(_G84352, _G84353, plur(all), _G84355, _G84356)), []).
dep(eng:e, 120, ct(adv(time(tr)), time(_G84351, _G84352, e))+ct(n(n), sp(_G84360, e, sing(e), [], time(time)))==>ct(adv(time(tr)), time(_G84351, _G84352, d(_G84360))), []).
dep(eng:e, 120, ct(adv(l(tr)), _G84345)+ct(adv(l(i)), _G84352)==>ct(adv(l(tr)), loc(_G84345, _G84352)), []).
dep(eng:e, 120, ct(adv(l(tr)), _G84345)+ct(n(_G84354), _G84352)==>ct(adv(l(tr)), loc(_G84345, _G84352)), []).
dep(eng:e, 119, ct(n(n), sp(_G84349, e, sing(e), _G84352, time(time)))+ct(adv(time(tr)), time(e, _G84367, e))==>ct(adv(time(tr)), time(e(_G84381), _G84367, e)), [time_ext(_G84349, _G84352, _G84381)]).
dep(eng:e, 118, ct(rel, sp(_G84347, _G84348, _G84349, _G84350, _G84351))+ct(n(_G84356), sp(_G84358, _G84359, _G84360, _G84361, _G84362))==>ct(n(n), sp(_G84358, _G84359, _G84360, [g:sp(_G84347, _G84348, _G84349, _G84350, _G84351)|_G84361], _G84362)), []).
dep(eng:e, 117, ct(as, sp(_G84347, same, _G84349, _G84350, _G84351))+ct(n(_G84356), sp(_G84358, _G84359, _G84360, _G84361, _G84362))==>ct(n(n), sp(_G84347, same(sp(_G84358, _G84359, _G84360, _G84361, _G84362), _G84349, _G84350, _G84351))), []).
dep(eng:e, 116, ct(part, _G84345)+ct(n(_G84350), sp(_G84352, _G84353, plur(_G84358), _G84355, _G84356))==>ct(n(n), sp(e, e, _G84367, [], of(sp(_G84352, _G84353, plur(_G84358), _G84355, _G84356)))), [np_number(_G84345, _G84385, _G84367)]).
dep(eng:e, 115, ct(n(n), sp(_G84349, _G84350, _G84351, _G84352, _G84353))+ct(p, gen)==>ct(rel, sp(_G84349, _G84350, _G84351, _G84352, _G84353)), [is_relational(_G84353)]).
dep(eng:e, 114, ct(n(_G84347), sp(_G84349, _G84350, _G84351, [_G84355|_G84356], _G84353))+ct(pp(b), _G84359)==>ct(n(_G84347), sp(_G84349, _G84350, _G84351, [_G84374|_G84356], _G84353)), [eval_for(_G84355, _G84359, _G84374)]).
dep(eng:e, 113, ct(n(_G84347), sp(_G84349, _G84350, _G84351, [], _G84353))+ct(adv, like(_G84358))==>ct(n(_G84347), sp(_G84349, _G84350, _G84351, like(_G84358), _G84353)), [indet_sp(_G84349, _G84351, _G84353)]).
dep(eng:e, 105, ct(comp, like)+ct(n(_G84350), _G84348)==>ct(adv, like(_G84348)), []).
dep(eng:e, 100, ct(comp, like)+ct(d(_G84350), this)==>ct(adv, like(this)), []).
dep(eng:e, 97, ct(n(n), sp(ref(known), same, _G84351, _G84352, _G84353))+ct(conj, as)==>ct(as, sp(ref(known), same, _G84351, _G84352, _G84353)), []).
dep(eng:e, 95, ct(gen(_G84347), _G84345)+ct(n(_G84352), sp(e, e, _G84356, _G84357, _G84358))==>ct(n(_G84352), sp(e, e, _G84356, _G84357, of(_G84358, _G84345))), []).
dep(eng:e, 90, ct(n(_G84347), _G84345)+ct(gen, gen)==>ct(gen(_G84347), _G84345), []).
dep(eng:e, 85, ct(num, some)+ct(n(n), sp(e, _G84353, sing(_G84358), _G84355, _G84356))==>ct(n(n), sp(some, _G84353, sing(_G84358), _G84355, _G84356)), []).
dep(eng:e, 85, ct(d(_G84347), _G84345)+ct(n(n), sp(e, _G84355, _G84347, _G84357, _G84358))==>ct(n(n), sp(_G84345, _G84355, _G84347, _G84357, _G84358)), []).
dep(eng:e, 83, ct(alt, same)+ct(n(n), sp(e, e, _G84354, _G84355, _G84356))==>ct(n(n), sp(e, same, _G84354, _G84355, _G84356)), []).
dep(eng:e, 80, ct(num, _G84345)+ct(n(n), sp(e, _G84353, _G84354, _G84355, _G84356))==>ct(n(n), sp(e, _G84353, _G84365, _G84355, _G84356)), [np_number(_G84345, _G84354, _G84365)]).
dep(eng:e, 78, ct(num, _G84345)+ct(p, gen)==>ct(part, _G84345), []).
dep(eng:e, 76, ct(alt, other(2))+ct(n(n), sp(e, e, _G84356, _G84357, _G84358))==>ct(n(n), sp(ref(unkn), other, _G84356, _G84357, _G84358)), []).
dep(eng:e, 75, ct(alt, other)+ct(n(n), sp(e, e, _G84354, _G84355, _G84356))==>ct(n(n), sp(e, other, _G84354, _G84355, _G84356)), []).
dep(eng:e, 73, ct(alt, more)+ct(n(n), sp(e, e, _G84354, _G84355, _G84356))==>ct(n(n), sp(e, more, _G84354, _G84355, _G84356)), []).
dep(eng:e, 72, ct(n(d), sp(e, e, sing(e), _G84352, _G84353))+ct(a, _G84358)==>ct(n(d), sp(e, e, sing(e), [_G84358|_G84352], _G84353)), []).
dep(eng:e, 71, ct(a, _G84345)+ct(n(n), sp(e, e, plur(e), _G84355, _G84356))==>ct(n(n), sp(e, e, plur(e), [_G84345|_G84355], _G84356)), []).
dep(eng:e, 71, ct(a, _G84345)+ct(n(n), sp(e, e, sing(e), _G84355, _G84356))==>ct(n(n), sp(e, e, sing(e), [_G84345|_G84355], _G84356)), []).
dep(eng:e, 70, ct(n(d), sp(_G84349, e, sing(e), _G84352, _G84353))+ct(alt, other(1))==>ct(n(d), sp(_G84349, other, sing(e), _G84352, _G84353)), []).
dep(eng:e, 57, ct(adv(l(i)), very(far))+ct(p, src)==>ct(adv(l(tr)), very(far)), []).
dep(eng:e, 57, ct(adv(l(i)), far)+ct(p, src)==>ct(adv(l(tr)), far), []).
dep(eng:e, 57, ct(v(aux), p(can, [e, e, _G84359, e, _G84365], _G84351))+ct(v(_G84371), p(_G84373, [_G84377, _G84380, e, e, e], _G84375))==>ct(v(_G84371), p(_G84373, [_G84377, _G84380, _G84359, can, _G84365], _G84375)), []).
dep(eng:e, 57, ct(v(aux), p(fut, [e, e, _G84359, e, _G84365], _G84351))+ct(v(_G84371), p(_G84373, [_G84377, _G84380, e, e, e], _G84375))==>ct(v(_G84371), p(_G84373, [_G84377, _G84380, _G84359, fut, _G84365], _G84375)), []).
dep(eng:e, 56, ct(neg, not)+ct(v(_G84350), p(_G84352, [_G84356, _G84359, inf, e, e], _G84354))==>ct(v(_G84350), p(_G84352, [_G84356, _G84359, inf, e, not], _G84354)), []).
dep(eng:e, 55, ct(adv, very)+ct(adv(l(i)), far)==>ct(adv(l(i)), very(far)), []).
dep(eng:e, 55, ct(adv, very)+ct(a, _G84348)==>ct(a, very(_G84348)), []).
dep(eng:e, 55, ct(p, d)+ct(v(_G84350), p(_G84352, [_G84356, _G84359, e, e, e], _G84354))==>ct(v(_G84350), p(_G84352, [_G84356, _G84359, inf, e, e], _G84354)), []).
dep(eng:e, 54, ct(n(d-very), sp(_G84352, _G84353, _G84354, [very(e)|_G84359], _G84356))+ct(a, _G84364)==>ct(n(d), sp(_G84352, _G84353, _G84354, [very(_G84364)|_G84359], _G84356)), []).
dep(eng:e, 54, ct(v(aux), p(have, [e, e, _G84359, _G84362, _G84365], [_G84371:_G84372|_G84369]))+ct(v(_G84377), p(_G84379, [_G84383, ed, e, e, e], [_G84401:e|_G84399]))==>ct(v(_G84377), p(_G84379, [_G84383, pf, _G84359, _G84362, _G84365], [_G84401:_G84372|_G84399])), []).
dep(eng:e, 54, ct(v(aux), p(have, [e, e, _G84359, _G84362, _G84365], [_G84371:_G84372|_G84369]))+ct(v(_G84377), p(_G84379, [_G84383, en, e, e, e], [_G84401:e|_G84399]))==>ct(v(_G84377), p(_G84379, [_G84383, pf, _G84359, _G84362, _G84365], [_G84401:_G84372|_G84399])), []).
dep(eng:e, 53, ct(n(d), sp(_G84349, _G84350, _G84351, _G84352, _G84353))+ct(adv, very)==>ct(n(d-very), sp(_G84349, _G84350, _G84351, [very(e)|_G84352], _G84353)), []).
dep(eng:e, 53, ct(p, l)+ct(n(n), sp(e, e, sing(e), [], front))+ct(p, gen)==>ct(adv(l), front), []).
dep(eng:e, 53, ct(v(aux), p(be, [e, _G84356, _G84359, _G84362, _G84365], [j:_G84372]))+ct(v(_G84377), p(_G84379, [ing, e, e, e, e], [_G84401:e|_G84399]))==>ct(v(_G84377), p(_G84379, [i, _G84356, _G84359, _G84362, _G84365], [_G84401:_G84372|_G84399])), []).
dep(eng:e, 53, ct(v(aux), p(do, [e, e, _G84359, e, not], [_G84371:_G84372]))+ct(v(v), p(_G84379, [e, e, e, e, e], [_G84401:e|_G84399]))==>ct(v(v), p(_G84379, [e, e, _G84359, e, not], [_G84401:_G84372|_G84399])), []).
dep(eng:e, 51, ct(v(v), p(do, [e, e, _G84359, e, e], [_G84371:_G84372|_G84369]))+ct(neg, not)==>ct(v(aux), p(do, [e, e, _G84359, e, not], [_G84404:_G84372])), []).
dep(eng:e, 51, ct(v(aux), p(_G84349, [_G84353, _G84356, _G84359, _G84362, e], _G84351))+ct(neg, not)==>ct(v(aux), p(_G84349, [_G84353, _G84356, _G84359, _G84362, not], _G84351)), []).
dep(eng:e, 50, ct(conj, because)+ct(p, gen)==>ct(p, cause), []).
dep(eng:e, 50, ct(v(_G84347), p(_G84349, [e, e, e, e, e], _G84351))+ct(i, s_form)==>ct(v(_G84347), p(_G84349, [e, e, s, e, e], _G84351)), []).
dep(eng:e, 50, ct(v(_G84347), p(_G84349, [e, e, e, e, e], _G84351))+ct(i, d_form)==>ct(v(_G84347), p(_G84349, [e, e, ed, e, e], _G84351)), []).
dep(eng:e, 50, ct(v(_G84347), p(_G84349, [e, e, e, e, e], _G84351))+ct(i, n_form)==>ct(v(_G84347), p(_G84349, [e, en, e, e, e], _G84351)), []).
dep(eng:e, 50, ct(v(_G84347), p(_G84349, [e, e, e, e, e], _G84351))+ct(i, ing_form)==>ct(v(_G84347), p(_G84349, [ing, e, e, e, e], _G84351)), []).
dep(eng:e, 42, ct(d(_G84347), this)+ct(n(d), sp(e, e, sing(e), [], someone(_G84362)))==>ct(n(n), sp(this, e, sing(e), [], someone(_G84362))), []).
dep(eng:e, 20, ct(n(n), sp(e, e, sing(e), [], _G84353))+ct(i, s_form)==>ct(n(n), sp(e, e, plur(e), [], _G84353)), []).
dg_class_macro(eng:e, n(d), it, sp(this, e, sing(e), [], something(fact))).
dg_class_macro(eng:e, n(n), people, sp(e, e, plur(e), [], people)).
dg_class_macro(eng:e, n(n), one_time, sp(e, e, sing(one), [], time(time))).
dg_class_macro(eng:e, n(n), two_times, sp(e, e, plur(two), [], time(time))).
dg_class_macro(eng:e, n(n), many_times, sp(e, e, plur(many), [], time(time))).
dg_class_macro(eng:e, n(n), all_times, sp(e, e, plur(all), [], time(time))).
dg_class_macro(eng:e, n(d), everything, sp(e, e, sing(all), [], something(thing))).
dg_class_macro(eng:e, n(d), everyone, sp(e, e, sing(all), [], someone(person))).
dg_class_macro(eng:e, n(d), everywhere, sp(e, e, sing(all), [], somewhere(place))).
dg_class_macro(eng:e, n(d), any(_G84351), sp(any, e, sing(e), [], _G84351)).
dg_class_macro(eng:e, n(d), no(_G84351), sp(no, e, sing(e), [], _G84351)).
dg_class_macro(eng:e, n(_G84349), _G84346, sp(e, e, sing(e), [], _G84346)).
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
dg_class_macro(eng:e, v(aux), _G84346, p(_G84346, [e, e, e, e, e], [j:e])).
dg_class_macro(eng:e, adv(time(_G84351)), _G84346, time(e, _G84346, e)).
paradigm(eng:e, neg_existence(e, no, not)).
paradigm(eng:e, neg_existence(_G84352, _G84353, _G84352)).
paradigm(eng:e, neg_existence(e, _G84353, e)).
paradigm(eng:e, indet_sp(ref(unkn), _G84353, _G84354)).
paradigm(eng:e, indet_sp(any, _G84353, _G84354)).
paradigm(eng:e, indet_sp(no, _G84353, _G84354)).
paradigm(eng:e, indet_sp(e, plur(_G84356), _G84354)).
paradigm(eng:e, indet_sp(e, sing(e), someone(person))).
paradigm(eng:e, indet_sp(e, sing(e), something(thing))).
paradigm(eng:e, indet_sp(e, sing(all), someone(person))).
paradigm(eng:e, indet_sp(e, sing(all), something(thing))).
paradigm(eng:e, subj_case(me, me(subj))).
paradigm(eng:e, subj_case(_G84352, _G84352)).
paradigm(eng:e, v_form(_G84352, pf, i, pf(i(_G84352)))).
paradigm(eng:e, v_form(_G84352, pf, e, pf(_G84352))).
paradigm(eng:e, v_form(_G84352, e, i, i(_G84352))).
paradigm(eng:e, v_form(_G84352, e, e, _G84352)).
paradigm(eng:e, v_par(be, s, e, e, e, e, e, am, sing(e), me)).
paradigm(eng:e, v_par(be, s, e, e, e, e, e, are, sing(e), you)).
paradigm(eng:e, v_par(be, s, e, e, e, e, e, is, sing(_G84363), _G84361)).
paradigm(eng:e, v_par(be, s, e, e, e, e, e, are, plur(_G84363), _G84361)).
paradigm(eng:e, v_par(be, ed, e, e, e, e, before(now), was, sing(e), me)).
paradigm(eng:e, v_par(be, ed, e, e, e, e, before(now), were, sing(e), you)).
paradigm(eng:e, v_par(be, ed, e, e, e, e, before(now), was, sing(_G84365), _G84361)).
paradigm(eng:e, v_par(be, ed, e, e, e, e, before(now), were, plur(_G84365), _G84361)).
paradigm(eng:e, v_par(l(_G84363), s, e, e, e, e, e, am, sing(e), me)).
paradigm(eng:e, v_par(l(_G84363), s, e, e, e, e, e, are, sing(e), you)).
paradigm(eng:e, v_par(l(_G84363), s, e, e, e, e, e, is, sing(_G84365), _G84361)).
paradigm(eng:e, v_par(l(_G84363), s, e, e, e, e, e, are, plur(_G84365), _G84361)).
paradigm(eng:e, v_par(l(_G84363), ed, e, e, e, e, before(now), was, sing(e), me)).
paradigm(eng:e, v_par(l(_G84363), ed, e, e, e, e, before(now), were, sing(e), you)).
paradigm(eng:e, v_par(l(_G84363), ed, e, e, e, e, before(now), was, sing(_G84367), _G84361)).
paradigm(eng:e, v_par(l(_G84363), ed, e, e, e, e, before(now), were, plur(_G84367), _G84361)).
paradigm(eng:e, v_par(_G84352, s, e, e, i, e, e, am, sing(e), me)).
paradigm(eng:e, v_par(_G84352, s, e, e, i, e, e, are, sing(e), you)).
paradigm(eng:e, v_par(_G84352, s, e, e, i, e, e, is, sing(_G84363), _G84361)).
paradigm(eng:e, v_par(_G84352, s, e, e, i, e, e, are, plur(_G84363), _G84361)).
paradigm(eng:e, v_par(_G84352, ed, e, e, i, e, before(now), was, sing(e), me)).
paradigm(eng:e, v_par(_G84352, ed, e, e, i, e, before(now), were, sing(e), you)).
paradigm(eng:e, v_par(_G84352, ed, e, e, i, e, before(now), was, sing(_G84365), _G84361)).
paradigm(eng:e, v_par(_G84352, ed, e, e, i, e, before(now), were, plur(_G84365), _G84361)).
paradigm(eng:e, v_par(_G84352, e, fut, _G84355, _G84356, e, after(now), e, _G84360, _G84361)).
paradigm(eng:e, v_par(_G84352, ed, fut, _G84355, _G84356, e, after(before), e, _G84360, _G84361)).
paradigm(eng:e, v_par(_G84352, e, can, _G84355, _G84356, can, e, e, _G84360, _G84361)).
paradigm(eng:e, v_par(_G84352, ed, can, _G84355, _G84356, can, before(now), e, _G84360, _G84361)).
paradigm(eng:e, v_par(_G84352, s, e, pf, _G84356, e, e, e, sing(_G84363), something(_G84365))).
paradigm(eng:e, v_par(_G84352, s, e, pf, _G84356, e, e, e, sing(_G84363), someone(_G84365))).
paradigm(eng:e, v_par(_G84352, e, e, pf, _G84356, e, e, e, _G84360, _G84361)).
paradigm(eng:e, v_par(_G84352, ed, e, pf, _G84356, e, before(before), e, _G84360, _G84361)).
paradigm(eng:e, v_par(_G84352, s, e, e, e, e, e, e, sing(_G84363), something(_G84365))).
paradigm(eng:e, v_par(_G84352, s, e, e, e, e, e, e, sing(_G84363), someone(_G84365))).
paradigm(eng:e, v_par(_G84352, e, e, e, e, e, e, e, _G84360, me)).
paradigm(eng:e, v_par(_G84352, e, e, e, e, e, e, e, _G84360, you)).
paradigm(eng:e, v_par(_G84352, e, e, e, e, e, e, e, plur(_G84363), _G84361)).
paradigm(eng:e, v_par(_G84352, ed, e, e, e, e, before(now), e, _G84360, _G84361)).
paradigm(eng:e, duration_mod(sp(ref(unkn), e, sing(e), [_G84353], time(time)), _G84353)).
paradigm(eng:e, duration_mod(sp(some, e, sing(e), [], time(time)), some)).
paradigm(eng:e, time_ext(some, _G84353, some)).
paradigm(eng:e, time_ext(ref(unkn), [_G84354], _G84354)).
paradigm(eng:e, time_loc(this, _G84353, this)).
paradigm(eng:e, time_loc(that, _G84353, that)).
paradigm(eng:e, time_loc(some, _G84353, some)).
paradigm(eng:e, time_loc(_G84352, same, same)).
paradigm(eng:e, time_loc(e, e, e)).
paradigm(eng:e, time_agr(_G84352, before, that, time(_G84352, before, that), before(before))).
paradigm(eng:e, time_agr(_G84352, after, that, time(_G84352, after, that), after(before))).
paradigm(eng:e, time_agr(_G84352, before, _G84354, time(_G84352, before, _G84354), before(now))).
paradigm(eng:e, time_agr(_G84352, after, _G84354, time(_G84352, after, _G84354), after(now))).
paradigm(eng:e, time_agr(e, at, _G84354, time(e, at(after), _G84354), after(now))).
paradigm(eng:e, time_agr(e, at, _G84354, time(e, at(before), _G84354), before(now))).
paradigm(eng:e, time_agr(e, now, e, time(e, now, e), e)).
paradigm(eng:e, eval_for(good, _G84353, good(_G84353))).
paradigm(eng:e, eval_for(bad, _G84353, bad(_G84353))).
paradigm(eng:e, eval_for(very(good), _G84353, very(good(_G84353)))).
paradigm(eng:e, eval_for(very(bad), _G84353, very(bad(_G84353)))).
paradigm(eng:e, np_number(e, sing(e), sing(e))).
paradigm(eng:e, np_number(e, sing(all), sing(all))).
paradigm(eng:e, np_number(e, plur(e), plur(e))).
paradigm(eng:e, np_number(one, sing(e), sing(one))).
paradigm(eng:e, np_number(two, plur(e), plur(two))).
paradigm(eng:e, np_number(some, plur(e), plur(some))).
paradigm(eng:e, np_number(many, plur(e), plur(many))).
paradigm(eng:e, np_number(all, plur(e), plur(all))).
paradigm(eng:e, inst_arg(_G84352, _G84353, [_G84352:e|_G84358], [_G84352:_G84353|_G84358])).
paradigm(eng:e, inst_arg(_G84352, _G84353, [_G84357, _G84352:e|_G84361], [_G84357, _G84352:_G84353|_G84361])).
paradigm(eng:e, inst_arg(_G84352, _G84353, [_G84357, _G84360, _G84352:e|_G84364], [_G84357, _G84360, _G84352:_G84353|_G84364])).
paradigm(eng:e, inst_arg(_G84352, _G84353, [_G84357, _G84360, _G84363, _G84352:e], [_G84357, _G84360, _G84363, _G84352:_G84353])).
paradigm(eng:e, inst_adj(yes, _G84353, _G84354, e, e)).
paradigm(eng:e, inst_adj(no, l, _G84354, _G84354, e)).
paradigm(eng:e, inst_adj(no, m, _G84354, e, _G84354)).
paradigm(eng:e, is_relational(kind)).
paradigm(eng:e, is_relational(part)).
paradigm(eng:e, is_relational(side)).
paradigm(eng:e, pp_role(c, sp(_G84356, _G84357, _G84358, _G84359, something(_G84362)), i)).
paradigm(eng:e, pp_role(_G84352, _G84353, _G84352)).
paradigm(eng:e, pred_neg(not, _G84353, not(_G84353))).
paradigm(eng:e, pred_neg(e, _G84353, _G84353)).
paradigm(eng:e, prop_obj(know)).
paradigm(eng:e, prop_obj(think)).
paradigm(eng:e, good_for(good, [b:e])).
paradigm(eng:e, good_for(bad, [b:e])).
paradigm(eng:e, good_for(_G84352, [])).
paradigm(eng:e, eval(good)).
paradigm(eng:e, eval(bad)).
paradigm(eng:e, eval(very(good))).
paradigm(eng:e, eval(very(bad))).
paradigm(eng:e, obj_det(not, not, any, e)).
paradigm(eng:e, obj_det(e, not, no, e)).
paradigm(eng:e, obj_det(e, e, _G84354, _G84354)).
paradigm(eng:e, obj_det(not, not, _G84354, _G84354)).
allo(eng:e, [101, 115], [115], [], [_G84401], [], [], [], [_G84401<<e_triggers]).
allo(eng:e, [100], [101, 100], [], [_G84401], [35], [], [], [_G84401<<vow]).
allo(eng:e, [100], [101, 100], [], [111, 117, 108], [35], [], [], []).
allo(eng:e, [100], [101, 100], [], [104, 101, 97, 114], [35], [], [], []).
allo(eng:e, [116], [101, 100], [], [102, 101, 108], [35], [102, 101, 101, 108], [], []).
allo(eng:e, [110, 101], [101, 110], [], [100, 111], [35], [], [], []).
allo(eng:e, [39], [39, 115], [], [115], [35], [], [], []).
allo(eng:e, [112, 101, 111, 112, 108], [112, 101, 111, 112, 108, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [115, 111, 109, 101, 111, 110], [115, 111, 109, 101, 111, 110, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [115, 111, 109, 101, 116, 105, 109], [115, 111, 109, 101, 116, 105, 109, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [115, 111, 109, 101, 119, 104, 101, 114], [115, 111, 109, 101, 119, 104, 101, 114, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [97, 110, 121, 111, 110], [97, 110, 121, 111, 110, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [97, 110, 121, 116, 105, 109], [97, 110, 121, 116, 105, 109, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [97, 110, 121, 119, 104, 101, 114], [97, 110, 121, 119, 104, 101, 114, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [101, 118, 101, 114, 121, 111, 110], [101, 118, 101, 114, 121, 111, 110, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [101, 118, 101, 114, 121, 119, 104, 101, 114], [101, 118, 101, 114, 121, 119, 104, 101, 114, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [110, 111, 111, 110], [110, 111, 111, 110, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [110, 111, 116, 105, 109], [110, 111, 116, 105, 109, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [110, 111, 119, 104, 101, 114], [110, 111, 119, 104, 101, 114, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [116, 105, 109], [116, 105, 109, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [112, 108, 97, 99], [112, 108, 97, 99, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [109], [109, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [98, 111, 100, 121], [98, 111, 100, 105], [], [], [35], [], [], []).
allo(eng:e, [111, 110], [111, 110, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [115, 111, 109], [115, 111, 109, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [109, 111, 114], [109, 111, 114, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [116, 104], [116, 104, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [116, 104, 101, 115], [116, 104, 101, 115, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [115, 97, 109], [115, 97, 109, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [101, 108, 115], [101, 108, 115, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [108, 105, 107], [108, 105, 107, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [98], [98, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [97, 114], [97, 114, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [119, 101, 114], [119, 101, 114, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [104, 97, 118], [104, 97, 118, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [104, 97], [104, 97, 118, 101], [], [], [100, 35], [], [101, 100], []).
allo(eng:e, [104, 97], [104, 97, 118, 101], [], [], [115, 35], [], [115], []).
allo(eng:e, [119, 111, 117, 108], [119, 105, 108, 108], [], [], [100, 35], [], [101, 100], []).
allo(eng:e, [99, 111, 117, 108], [99, 97, 110], [], [], [100, 35], [], [101, 100], []).
allo(eng:e, [100, 105], [100, 111], [], [], [100, 35], [], [101, 100], []).
allo(eng:e, [109, 111, 118], [109, 111, 118, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [108, 105, 118], [108, 105, 118, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [100, 105], [100, 105, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [116, 114, 117], [116, 114, 117, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [115, 97, 121], [115, 97, 105], [], [], [35], [], [], []).
allo(eng:e, [115, 101], [115, 101, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [115, 97, 119, 35], [115, 101, 101], [101, 100, 35], [], [], [], [], []).
allo(eng:e, [102, 101, 108], [102, 101, 101, 108], [], [], [116, 35], [], [101, 100], []).
allo(eng:e, [105, 110, 115, 105, 100], [105, 110, 115, 105, 100, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [97, 98, 111, 118], [97, 98, 111, 118, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [104, 101, 114], [104, 101, 114, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [116, 104, 101, 114], [116, 104, 101, 114, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [115, 105, 100], [115, 105, 100, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [98, 101, 102, 111, 114], [98, 101, 102, 111, 114, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [111, 110, 99], [111, 110, 99, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [116, 119, 105, 99], [116, 119, 105, 99, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [98, 101, 99, 97, 117, 115], [98, 101, 99, 97, 117, 115, 101], [], [], [105, 110, 103, 35], [], [], []).
allo(eng:e, [109, 97, 121, 98], [109, 97, 121, 98, 101], [], [], [105, 110, 103, 35], [], [], []).
m(eng:e, i, [115], s_form).
m(eng:e, i, [105, 110, 103], ing_form).
m(eng:e, i, [101, 100], d_form).
m(eng:e, i, [101, 110], n_form).
m(eng:e, gen, [39, 115], gen).
m(eng:e, n(n), [112, 101, 111, 112, 108, 101], people).
m(eng:e, n(d), [115, 111, 109, 101, 111, 110, 101], someone(person)).
m(eng:e, n(d), [115, 111, 109, 101, 116, 104, 105, 110, 103], something(thing)).
m(eng:e, n(d), [115, 111, 109, 101, 116, 105, 109, 101], time(time)).
m(eng:e, n(d), [115, 111, 109, 101, 119, 104, 101, 114, 101], somewhere(place)).
m(eng:e, n(d), [115, 111, 109, 101, 104, 111, 119], manner(manner)).
m(eng:e, n(d), [97, 110, 121, 111, 110, 101], any(someone(person))).
m(eng:e, n(d), [97, 110, 121, 116, 104, 105, 110, 103], any(something(thing))).
m(eng:e, n(d), [97, 110, 121, 116, 105, 109, 101], any(time(time))).
m(eng:e, n(d), [97, 110, 121, 119, 104, 101, 114, 101], any(somewhere(place))).
m(eng:e, n(d), [97, 110, 121, 104, 111, 119], any(manner(manner))).
m(eng:e, n(d), [101, 118, 101, 114, 121, 111, 110, 101], everyone).
m(eng:e, n(d), [101, 118, 101, 114, 121, 116, 104, 105, 110, 103], everything).
m(eng:e, n(d), [101, 118, 101, 114, 121, 119, 104, 101, 114, 101], everything).
m(eng:e, n(d), [110, 111, 111, 110, 101], no(someone(person))).
m(eng:e, n(d), [110, 111, 116, 104, 105, 110, 103], no(something(thing))).
m(eng:e, n(d), [110, 111, 116, 105, 109, 101], no(time(time))).
m(eng:e, n(d), [110, 111, 119, 104, 101, 114, 101], no(somewhere(place))).
m(eng:e, n(d), [110, 111, 104, 111, 119], no(manner(manner))).
m(eng:e, n(n), [112, 101, 114, 115, 111, 110], someone(person)).
m(eng:e, n(n), [116, 104, 105, 110, 103], something(thing)).
m(eng:e, n(n), [116, 105, 109, 101], time(time)).
m(eng:e, n(n), [112, 108, 97, 99, 101], somewhere(place)).
m(eng:e, n(n), [119, 97, 121], manner(manner)).
m(eng:e, n(p), [73], me(subj)).
m(eng:e, n(p), [109, 101], me).
m(eng:e, n(p), [121, 111, 117], you).
m(eng:e, n(n), [98, 111, 100, 105], body).
m(eng:e, n(n), [107, 105, 110, 100], kind).
m(eng:e, n(n), [112, 97, 114, 116], part).
m(eng:e, num, [111, 110, 101], one).
m(eng:e, num, [116, 119, 111], two).
m(eng:e, num, [115, 111, 109, 101], some).
m(eng:e, num, [109, 97, 110, 121], many).
m(eng:e, num, [97, 108, 108], all).
m(eng:e, alt, [109, 111, 114, 101], more).
m(eng:e, a, [103, 111, 111, 100], good).
m(eng:e, a, [98, 97, 100], bad).
m(eng:e, a, [98, 105, 103], big).
m(eng:e, a, [115, 109, 97, 108, 108], small).
m(eng:e, d(sing(_G84399)), [116, 104, 105, 115], this).
m(eng:e, d(sing(_G84399)), [116, 104, 97, 116], that).
m(eng:e, d(sing(_G84399)), [97], ref(unkn)).
m(eng:e, d(sing(_G84399)), [97, 110], ref(unkn)).
m(eng:e, d(_G84397), [116, 104, 101], ref(known)).
m(eng:e, d(_G84397), [97, 110, 121], any).
m(eng:e, d(_G84397), [110, 111], no).
m(eng:e, d(plur(_G84399)), [116, 104, 101, 115, 101], this).
m(eng:e, n(d), [105, 116], it).
m(eng:e, alt, [115, 97, 109, 101], same).
m(eng:e, alt, [111, 116, 104, 101, 114], other).
m(eng:e, alt, [101, 108, 115, 101], other(1)).
m(eng:e, alt, [97, 110, 111, 116, 104, 101, 114], other(2)).
m(eng:e, adv, [118, 101, 114, 121], very).
m(eng:e, comp, [108, 105, 107, 101], like).
m(eng:e, v(aux), [98, 101], be).
m(eng:e, v(aux), [97, 109], am).
m(eng:e, v(aux), [105, 115], is).
m(eng:e, v(aux), [119, 97, 115], was).
m(eng:e, v(aux), [97, 114, 101], are).
m(eng:e, v(aux), [119, 101, 114, 101], were).
m(eng:e, v(aux), [104, 97, 118, 101], have).
m(eng:e, v(aux), [119, 105, 108, 108], fut).
m(eng:e, v(aux), [99, 97, 110], can).
m(eng:e, v(v), [100, 111], do).
m(eng:e, v(v), [104, 97, 112, 112, 101, 110], happen).
m(eng:e, v(v), [109, 111, 118, 101], move).
m(eng:e, v(v), [116, 111, 117, 67], touch).
m(eng:e, v(v), [108, 105, 118, 101], live).
m(eng:e, v(v), [100, 105, 101], die).
m(eng:e, v(v), [101, 120, 105, 115, 116], exist).
m(eng:e, n(n), [119, 111, 114, 100], word).
m(eng:e, a, [116, 114, 117, 101], true).
m(eng:e, v(v), [115, 97, 105], say).
m(eng:e, v(v), [104, 101, 97, 114], hear).
m(eng:e, v(v), [115, 101, 101], see).
m(eng:e, v(v), [116, 104, 105, 110, 107], think).
m(eng:e, v(v), [102, 101, 101, 108], feel).
m(eng:e, v(v), [107, 110, 111, 119], know).
m(eng:e, v(v), [119, 97, 110, 116], want).
m(eng:e, adv(l(tr)), [105, 110, 115, 105, 100, 101], inside).
m(eng:e, adv(l(tr)), [97, 98, 111, 118, 101], above).
m(eng:e, adv(l(tr)), [98, 101, 108, 111, 119], below).
m(eng:e, adv(l(tr)), [110, 101, 97, 114], near).
m(eng:e, adv(l(i)), [102, 97, 114], far).
m(eng:e, adv(l(i)), [104, 101, 114, 101], here).
m(eng:e, adv(l(i)), [116, 104, 101, 114, 101], there).
m(eng:e, n(n), [102, 114, 111, 110, 116], front).
m(eng:e, n(n), [115, 105, 100, 101], side).
m(eng:e, adv(time(i)), [110, 111, 119], now).
m(eng:e, adv(time(tr)), [98, 101, 102, 111, 114, 101], before).
m(eng:e, adv(time(tr)), [97, 102, 116, 101, 114], after).
m(eng:e, a, [108, 111, 110, 103], long).
m(eng:e, a, [83, 111, 114, 116], short).
m(eng:e, n(n), [111, 110, 99, 101], one_time).
m(eng:e, n(n), [116, 119, 105, 99, 101], two_times).
m(eng:e, n(n), [111, 102, 116, 101, 110], many_times).
m(eng:e, n(n), [97, 108, 119, 97, 121, 115], all_times).
m(eng:e, neg, [110, 111, 116], not).
m(eng:e, conj, [98, 101, 99, 97, 117, 115, 101], because).
m(eng:e, conj, [105, 102], if).
m(eng:e, conj, [119, 104, 101, 110], if).
m(eng:e, conj, [97, 115], as).
m(eng:e, p, [116, 111], d).
m(eng:e, p, [105, 110], l).
m(eng:e, p, [111, 110], l).
m(eng:e, p, [119, 105, 116, 104], c).
m(eng:e, p, [97, 98, 111, 117, 116], t).
m(eng:e, p, [102, 111, 114], b).
m(eng:e, p, [97, 116], time).
m(eng:e, p, [111, 102], gen).
m(eng:e, p, [102, 114, 111, 109], src).
m(eng:e, p, [116, 111, 119, 97, 114, 100, 115], g).
m(eng:e, adv(sent), [109, 97, 121, 98, 101], maybe).
m(eng:e, n(n), [71, 111, 100], someone(god)).
m(eng:e, n(n), [98, 105, 114, 100], something(bird)).
m(eng:e, group, [], begin).
m(eng:e, group, [], end).
