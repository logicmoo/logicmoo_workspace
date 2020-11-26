
judged_dialogue_processing([[play,the,five,of,diamonds],[[imp,form(imperative,[[play,term(pro,you,[]),term(the_sing,5,[[of,term(null,diamond,[])]])]])]],[]],[[[imp,quant(def_sing,'$VAR'(0),[[you,'$VAR'(0)]],quant(exist,'$VAR'(1),[[diamond,'$VAR'(1)]],quant(def_sing,'$VAR'(2),[[5,'$VAR'(2)],[of,'$VAR'(2),'$VAR'(1)]],imperative(quant(exist,'$VAR'(3),[[play,'$VAR'(3),'$VAR'(0),'$VAR'(2)]],true)))))]]],im,ok).

judged_dialogue_processing([[win,the,trick,in,dummy],[[imp,form(imperative,[[win,term(pro,you,[]),term(the_sing,trick,[])],[loc,in_dummy]])]],[]],[[[imp,quant(def_sing,'$VAR'(0),[[you,'$VAR'(0)]],quant(def_sing,'$VAR'(1),[[trick,'$VAR'(1)]],imperative(quant(exist,'$VAR'(2),[[win,'$VAR'(2),'$VAR'(0),'$VAR'(1)],[loc,'$VAR'(2),in_dummy]],true))))]]],im,ok).

judged_dialogue_processing([[play,the,six,of,hearts],[[imp,form(imperative,[[play,term(pro,you,[]),term(the_sing,6,[[of,term(null,heart,[])]])]])]],[]],[[[imp,quant(def_sing,'$VAR'(0),[[you,'$VAR'(0)]],quant(exist,'$VAR'(1),[[heart,'$VAR'(1)]],quant(def_sing,'$VAR'(2),[[6,'$VAR'(2)],[of,'$VAR'(2),'$VAR'(1)]],imperative(quant(exist,'$VAR'(3),[[play,'$VAR'(3),'$VAR'(0),'$VAR'(2)]],true)))))]]],im,ok).

judged_dialogue_processing([[play,small],[[imp,form(imperative,[[play,term(pro,you,[]),term(null,[[card_quality,small]],[])]])]],[]],[[[imp,quant(def_sing,'$VAR'(0),[[you,'$VAR'(0)]],quant(exist,'$VAR'(1),[[[[card_quality,small]],'$VAR'(1)]],imperative(quant(exist,'$VAR'(2),[[play,'$VAR'(2),'$VAR'(0),'$VAR'(1)]],true))))]]],im,bad).

judged_dialogue_processing([[play,the,ace],[[imp,form(imperative,[[play,term(pro,you,[]),term(the_sing,a,[])]])]],[]],[[[imp,quant(def_sing,'$VAR'(0),[[you,'$VAR'(0)]],quant(def_sing,'$VAR'(1),[[a,'$VAR'(1)]],imperative(quant(exist,'$VAR'(2),[[play,'$VAR'(2),'$VAR'(0),'$VAR'(1)]],true))))]]],im,good).

judged_dialogue_processing([[i,said,play,the,ace],[[interjection,correction],[imp,form(imperative,[[play,term(pro,you,[]),term(the_sing,a,[])]])]],[]],[[[interjection,correction],[imp,quant(def_sing,'$VAR'(0),[[you,'$VAR'(0)]],quant(def_sing,'$VAR'(1),[[a,'$VAR'(1)]],imperative(quant(exist,'$VAR'(2),[[play,'$VAR'(2),'$VAR'(0),'$VAR'(1)]],true))))]]],im,good).

judged_dialogue_processing([[who,is,on,lead],[[whq,form(present,[[be,term(pro,who,[]),[on_play,term(null,lead,[])]]])]],[]],[[[whq,quant(def_sing,'$VAR'(0),[[who,'$VAR'(0)]],quant(exist,'$VAR'(1),[[lead,'$VAR'(1)]],quant(exist,'$VAR'(2),[[be,'$VAR'(2),'$VAR'(0),[on_play,'$VAR'(1)]],[tense,'$VAR'(2),present]],true)))]]],im,bad).

judged_dialogue_processing([[am,i,in,hand],[[ynq,form(present,[[be,term(pro,i,[]),[in_hand,term(null,hand,[])]]])]],[]],[[[ynq,quant(def_sing,'$VAR'(0),[[i,'$VAR'(0)]],quant(exist,'$VAR'(1),[[hand,'$VAR'(1)]],quant(exist,'$VAR'(2),[[be,'$VAR'(2),'$VAR'(0),[in_hand,'$VAR'(1)]],[tense,'$VAR'(2),present]],true)))]]],im,bad).

judged_dialogue_processing([[play,the,king],[[imp,form(imperative,[[play,term(pro,you,[]),term(the_sing,k,[])]])]],[]],[[[imp,quant(def_sing,'$VAR'(0),[[you,'$VAR'(0)]],quant(def_sing,'$VAR'(1),[[k,'$VAR'(1)]],imperative(quant(exist,'$VAR'(2),[[play,'$VAR'(2),'$VAR'(0),'$VAR'(1)]],true))))]]],im,good).

judged_dialogue_processing([[the,king,of,spades],[[elliptical,term(the_sing,k,[[of,term(null,spade,[])]])]],[]],[[[elliptical,term(the_sing,'$VAR'(0),[[k,'$VAR'(0)],[of,'$VAR'(0),term(null,'$VAR'(1),[[spade,'$VAR'(1)]])]])]]],im,ok).

judged_dialogue_processing([[can,i,have,my,card,back],[[ynq,form(can,[[have_back,term(pro,i,[]),term(null,card,[[possessive,[term(pro,i,[])]]]),back]])]],[]],[[[ynq,quant(def_sing,'$VAR'(0),[[i,'$VAR'(0)]],quant(def_sing,'$VAR'(1),[[i,'$VAR'(1)]],quant(exist,'$VAR'(2),[[card,'$VAR'(2)],[possessive,'$VAR'(2),['$VAR'(1)]]],can(quant(exist,'$VAR'(3),[[have_back,'$VAR'(3),'$VAR'(0),'$VAR'(2),back]],true)))))]]],im,ok).

judged_dialogue_processing([[i,want,to,play,the,ace],[[dcl,form(present,[[want_to,term(pro,i,[]),[[play,term(pro,i,[]),term(the_sing,a,[])]]]])]],[]],[[[dcl,quant(def_sing,'$VAR'(0),[[i,'$VAR'(0)]],quant(def_sing,'$VAR'(1),[[i,'$VAR'(1)]],quant(def_sing,'$VAR'(2),[[a,'$VAR'(2)]],quant(exist,'$VAR'(3),[[want_to,'$VAR'(3),'$VAR'(0),[[play,'$VAR'(1),'$VAR'(2)]]],[tense,'$VAR'(3),present]],true))))]]],im,ok).

judged_dialogue_processing([[play,small,from,dummy],[[imp,form(imperative,[[play,term(pro,you,[]),term(null,[[card_quality,small]],[])],[loc,from_dummy]])]],[]],[[[imp,quant(def_sing,'$VAR'(0),[[you,'$VAR'(0)]],quant(exist,'$VAR'(1),[[[[card_quality,small]],'$VAR'(1)]],imperative(quant(exist,'$VAR'(2),[[play,'$VAR'(2),'$VAR'(0),'$VAR'(1)],[loc,'$VAR'(2),from_dummy]],true))))]]],im,bad).

judged_dialogue_processing([[now,i,want,to,play,the,queen,of,clubs],[[duration,term(null,now,[])],[dcl,form(present,[[want_to,term(pro,i,[]),[[play,term(pro,i,[]),term(the_sing,q,[[of,term(null,club,[])]])]]]])]],[]],[[[duration,term(null,'$VAR'(0),[[now,'$VAR'(0)]])],[dcl,quant(def_sing,'$VAR'(1),[[i,'$VAR'(1)]],quant(def_sing,'$VAR'(2),[[i,'$VAR'(2)]],quant(exist,'$VAR'(3),[[club,'$VAR'(3)]],quant(def_sing,'$VAR'(4),[[q,'$VAR'(4)],[of,'$VAR'(4),'$VAR'(3)]],quant(exist,'$VAR'(5),[[want_to,'$VAR'(5),'$VAR'(1),[[play,'$VAR'(2),'$VAR'(4)]]],[tense,'$VAR'(5),present]],true)))))]]],im,bad).

judged_dialogue_processing([[put,the,king,on,it],[[imp,form(imperative,[[put,term(pro,you,[]),term(the_sing,k,[])],[on_suit_or_card,term(pro,it,[])]])]],[]],[[[imp,quant(def_sing,'$VAR'(0),[[you,'$VAR'(0)]],quant(def_sing,'$VAR'(1),[[k,'$VAR'(1)]],quant(def_sing,'$VAR'(2),[[thing,'$VAR'(2)]],imperative(quant(exist,'$VAR'(3),[[put,'$VAR'(3),'$VAR'(0),'$VAR'(1)],[on_suit_or_card,'$VAR'(3),'$VAR'(2)]],true)))))]]],im,ok).

judged_dialogue_processing([[now,play,the,jack,of,clubs],[[duration,term(null,now,[])],[imp,form(imperative,[[play,term(pro,you,[]),term(the_sing,j,[[of,term(null,club,[])]])]])]],[]],[[[duration,term(null,'$VAR'(0),[[now,'$VAR'(0)]])],[imp,quant(def_sing,'$VAR'(1),[[you,'$VAR'(1)]],quant(exist,'$VAR'(2),[[club,'$VAR'(2)]],quant(def_sing,'$VAR'(3),[[j,'$VAR'(3)],[of,'$VAR'(3),'$VAR'(2)]],imperative(quant(exist,'$VAR'(4),[[play,'$VAR'(4),'$VAR'(1),'$VAR'(3)]],true)))))]]],im,bad).

judged_dialogue_processing([[sorry,lead,the,seven,of,clubs],[[politeness,sorry],[imp,form(imperative,[[lead,term(pro,you,[]),term(the_sing,7,[[of,term(null,club,[])]])]])]],[]],[[[politeness,sorry],[imp,quant(def_sing,'$VAR'(0),[[you,'$VAR'(0)]],quant(exist,'$VAR'(1),[[club,'$VAR'(1)]],quant(def_sing,'$VAR'(2),[[7,'$VAR'(2)],[of,'$VAR'(2),'$VAR'(1)]],imperative(quant(exist,'$VAR'(3),[[lead,'$VAR'(3),'$VAR'(0),'$VAR'(2)]],true)))))]]],im,ok).

judged_dialogue_processing([[now,i,'\'m',going,to,play,the,jack],[[duration,term(null,now,[])],[dcl,form(future,[[play,term(pro,i,[]),term(the_sing,j,[])]])]],[]],[[[duration,term(null,'$VAR'(0),[[now,'$VAR'(0)]])],[dcl,quant(def_sing,'$VAR'(1),[[i,'$VAR'(1)]],quant(def_sing,'$VAR'(2),[[j,'$VAR'(2)]],quant(exist,'$VAR'(3),[[play,'$VAR'(3),'$VAR'(1),'$VAR'(2)],[tense,'$VAR'(3),future]],true)))]]],im,bad).

judged_dialogue_processing([[can,i,take,that,back],[[ynq,form(can,[[take_back,term(pro,i,[]),term(that,null,[]),back]])]],[]],[[[ynq,quant(def_sing,'$VAR'(0),[[i,'$VAR'(0)]],quant(def_sing,'$VAR'(1),true,can(quant(exist,'$VAR'(2),[[take_back,'$VAR'(2),'$VAR'(0),'$VAR'(1),back]],true))))]]],im,ok).

judged_dialogue_processing([[i,want,to,play,the,six,instead],[[dcl,form(present,[[want_to,term(pro,i,[]),[[play,term(pro,i,[]),term(the_sing,6,[])],[adverb,instead]]]])]],[]],[[[dcl,quant(def_sing,'$VAR'(0),[[i,'$VAR'(0)]],quant(def_sing,'$VAR'(1),[[i,'$VAR'(1)]],quant(def_sing,'$VAR'(2),[[6,'$VAR'(2)]],quant(exist,'$VAR'(3),[[want_to,'$VAR'(3),'$VAR'(0),[[play,'$VAR'(1),'$VAR'(2)],[adverb,instead]]],[tense,'$VAR'(3),present]],true))))]]],im,ok).

judged_dialogue_processing([[play,the,five,of,diamonds],[[imp,form(imperative,[[play,term(pro,you,[]),term(the_sing,5,[[of,term(null,diamond,[])]])]])]],[]],[[[imp,quant(def_sing,'$VAR'(0),[[you,'$VAR'(0)]],quant(exist,'$VAR'(1),[[diamond,'$VAR'(1)]],quant(def_sing,'$VAR'(2),[[specific_card,'$VAR'(2),[diamond,5]]],imperative(quant(exist,'$VAR'(3),[[play,'$VAR'(3),'$VAR'(0),'$VAR'(2)]],true)))))]]],im,good).

judged_dialogue_processing([[play,the,six,of,hearts],[[imp,form(imperative,[[play,term(pro,you,[]),term(the_sing,6,[[of,term(null,heart,[])]])]])]],[]],[[[imp,quant(def_sing,'$VAR'(0),[[you,'$VAR'(0)]],quant(exist,'$VAR'(1),[[heart,'$VAR'(1)]],quant(def_sing,'$VAR'(2),[[specific_card,'$VAR'(2),[heart,6]]],imperative(quant(exist,'$VAR'(3),[[play,'$VAR'(3),'$VAR'(0),'$VAR'(2)]],true)))))]]],im,good).

judged_dialogue_processing([[now,i,want,to,play,the,queen,of,clubs],[[duration,term(null,now,[])],[dcl,form(present,[[want_to,term(pro,i,[]),[[play,term(pro,i,[]),term(the_sing,q,[[of,term(null,club,[])]])]]]])]],[]],[[[duration,term(null,'$VAR'(0),[[now,'$VAR'(0)]])],[dcl,quant(def_sing,'$VAR'(1),[[i,'$VAR'(1)]],quant(def_sing,'$VAR'(2),[[i,'$VAR'(2)]],quant(exist,'$VAR'(3),[[club,'$VAR'(3)]],quant(def_sing,'$VAR'(4),[[specific_card,'$VAR'(4),[club,q]]],quant(exist,'$VAR'(5),[[want_to,'$VAR'(5),'$VAR'(1),[[play,'$VAR'(2),'$VAR'(4)]]],[tense,'$VAR'(5),present]],true)))))]]],im,bad).

judged_dialogue_processing([[now,play,the,jack,of,clubs],[[duration,term(null,now,[])],[imp,form(imperative,[[play,term(pro,you,[]),term(the_sing,j,[[of,term(null,club,[])]])]])]],[]],[[[duration,term(null,'$VAR'(0),[[now,'$VAR'(0)]])],[imp,quant(def_sing,'$VAR'(1),[[you,'$VAR'(1)]],quant(exist,'$VAR'(2),[[club,'$VAR'(2)]],quant(def_sing,'$VAR'(3),[[specific_card,'$VAR'(3),[club,j]]],imperative(quant(exist,'$VAR'(4),[[play,'$VAR'(4),'$VAR'(1),'$VAR'(3)]],true)))))]]],im,bad).

judged_dialogue_processing([[sorry,lead,the,seven,of,clubs],[[politeness,sorry],[imp,form(imperative,[[lead,term(pro,you,[]),term(the_sing,7,[[of,term(null,club,[])]])]])]],[]],[[[politeness,sorry],[imp,quant(def_sing,'$VAR'(0),[[you,'$VAR'(0)]],quant(exist,'$VAR'(1),[[club,'$VAR'(1)]],quant(def_sing,'$VAR'(2),[[specific_card,'$VAR'(2),[club,7]]],imperative(quant(exist,'$VAR'(3),[[lead,'$VAR'(3),'$VAR'(0),'$VAR'(2)]],true)))))]]],im,good).

%Judgements added 2009-11-09_15-21-19

judged_dialogue_processing([[play,small],[[imp,form(imperative,[[play,term(pro,you,[]),term(null,thing,[[card_quality,small]])]])]],[]],[[[imp,quant(def_sing,'$VAR'(0),[[you,'$VAR'(0)]],quant(exist,'$VAR'(1),[[thing,'$VAR'(1)],[card_quality,'$VAR'(1),small]],imperative(quant(exist,'$VAR'(2),[[play,'$VAR'(2),'$VAR'(0),'$VAR'(1)]],true))))]]],im,good).

judged_dialogue_processing([[can,i,have,my,card,back],[[ynq,form(can,[[have_back,term(pro,i,[]),term(null,card,[[possessive,term(pro,i,[])]]),back]])]],[]],[[[ynq,quant(def_sing,'$VAR'(0),[[i,'$VAR'(0)]],quant(def_sing,'$VAR'(1),[[i,'$VAR'(1)]],quant(exist,'$VAR'(2),[[card,'$VAR'(2)],[possessive,'$VAR'(2),'$VAR'(1)]],can(quant(exist,'$VAR'(3),[[have_back,'$VAR'(3),'$VAR'(0),'$VAR'(2),back]],true)))))]]],im,ok).

judged_dialogue_processing([[play,small,from,dummy],[[imp,form(imperative,[[play,term(pro,you,[]),term(null,thing,[[card_quality,small]])],[loc,from_dummy]])]],[]],[[[imp,quant(def_sing,'$VAR'(0),[[you,'$VAR'(0)]],quant(exist,'$VAR'(1),[[thing,'$VAR'(1)],[card_quality,'$VAR'(1),small]],imperative(quant(exist,'$VAR'(2),[[play,'$VAR'(2),'$VAR'(0),'$VAR'(1)],[loc,'$VAR'(2),from_dummy]],true))))]]],im,good).
