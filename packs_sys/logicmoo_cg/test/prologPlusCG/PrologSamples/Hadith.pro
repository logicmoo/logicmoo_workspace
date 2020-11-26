raoui(27, ElHossari, Ali, AbouMohamed, 65, 113).
attr(27, [islam, bolough, taqhoi, moroa, fahm]).
list_loc(27, [[ElKoufa, 65, 92], [ElMadinah, 92, 103], [Makkah, 103, 109], [ElMadinah, 109, 113]]).

// Un sanad est jugé sahih si: 1) tous les raouis qui y figurent sont tiqua et dabth, et 2) le sanad est maoussou
sahih(hadith(v_matn, l_sanad)) :-
  toutRaouiTiquaDabth(l_sanad),
  sanadMaoussoul(l_sanad), !.

toutRaouiTiquaDabth([]) :- !.
toutRaouiTiquaDabth([r_raoui|l]) :-
  tiqua(r_raoui),
  dabth(r_raoui),
  toutRaouiTiquaDabth(l), !.

// Redéfinition itérative de toutRaouiTiquaDabth
toutRaouiTiquaDabth(l_raouis) :-
  membre(r_raoui, l_raouis),
  ou(non(tiqua(r_raoui)), non(dabth(r_raoui))),
  !, fail.
toutRaouiTiquaDabth(l_raouis).

// Un raoui est jugé tiqua s'il possède les attributs suivants: islam, bolough, taqhoi, moroa ET ne possède pas 
// l'attribut iqhtirafElKabira
tiqua(r_raoui) :-
  attr(r_raoui, l_attrs),
  inclu([islam, bolough], l_attrs),
  adalah(r_raoui), !.

adalah(r_raoui) :-
  attr(r_raoui, l_attrs),
  inclu([taqhoi, moroa], l_attrs),
  non(membre(iqhtirafElKabira, l_attrs)), !.

// idem: un raoui est jugé dabth s'il possède les attributs suivants: fahm, tayaquod, hifdh

// Un sanad est jugé maoussoul s'il n'est pas morsal et il n'est pas monquatii.
// morsal: une rupture au début du sanad, monquatii: une rupture à l'intérieur du sanad.
sanad_maoussoul(l_sanad) :-
  non(morsal(l_sanad)),
  non(monquatii(l_sanad)), !.

morsal(l_sanad) :-
  dernier(l_sanad, r_dernier),
  irsal(r_dernier), !.

irsal(r_raoui) :-
  tabei(r_raoui), !.

tabei(r_raoui) :-
  raoui(r_raoui, r_nom, r_prenom, r_surnom, r_date_nais, r_date_mort),
  true is r_date_nais > 6.

// Un sanad est jugé monquatii si on parvient à déceler une discontinuité dans le sanad; il n'y a pas eu de 
// contact entre deux raouis consécutifs figurant dans le sanad.
monquatii([r1, r2|l_sanad]) :-
  non_samaa(r1, r2), !.
monquatii([r|l_sanad]) :-
  monquatii(l_sanad).

// etc. cf. chapitre 7 dans "Intelligence Artificielle en Lisp et Prolog"


  
  