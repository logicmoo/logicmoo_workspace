

:- include(world0).     	% data base
:- include(rivers).
:- include(cities).
:- include(contain).
:- include(borders).
:- include(countries).



chat_80_ed(A,B,C):- chat80_all(B,C,A).

/* Control loop */

chat80_test_q1([what,are,the,continents,no,country,in,which,contains,more,than,
    two,cities,whose,population,exceeds,(1),million, ? ]).

chat80_test_q2([which,country,that,borders,the,mediterranean,borders,a,country,
    that,is,bordered,by,a,country,whose,population,exceeds,
    the,population,of,india, ? ]).
/* ----------------------------------------------------------------------
	Simple questions
	These question do not require setOf/3 and are useful for early
	testing of a system.
   ---------------------------------------------------------------------- */

ed( [ does, america, contain, new, york, ? ] ,mini).
ed( [ is, new, york, in, america, ? ] ,mini).
ed( [ does, mexico, border, the, united_states, ? ] ,mini).
ed( [ is, the, population, of, china, greater, than, (200), million, ? ] ,mini).
ed( [ does, the, population, of, china, exceed, (1000), million, ? ] ,mini).
ed( [ is, the, population, of, china, (840), million, ? ] ,mini).
ed( [ does, the, population, of, china, exceed, the, population, of, india, ? ] ,mini).
ed( [ is, spain, bordered, by, the, pacific, ? ] ,mini).
ed( [ does, the, atlantic, border, spain, ? ] ,mini).
ed( [ is, the, rhine, in, switzerland, ? ] ,mini).
ed( [ is, the, united_kingdom, in, europe, ? ] ,mini).


/* ----------------------------------------------------------------------
	Standard question set
	This is the standard chat question set, originally put together
	by David and Fernando and use in their papers. Quintus uses this
	set as a standard for performance comparisons.
   ---------------------------------------------------------------------- */

ed(  1, [ what, rivers, are, there, ? ],

		[amazon, amu_darya, amur, brahmaputra, colorado,
		congo_river, cubango, danube, don, elbe, euphrates, ganges,
		hwang_ho, indus, irrawaddy, lena, limpopo, mackenzie,
		mekong, mississippi, murray, niger_river, nile, ob, oder,
		orange, orinoco, parana, rhine, rhone, rio_grande, salween,
		seine, senegal_river, tagus, vistula, volga, volta, yangtze,
		yenisei, yukon, zambesi]  ).

ed(  2, [ does, afghanistan, border, china, ? ],

		[true]  ).

ed(  3, [ what, is, the, capital, of, upper, volta, ? ],

		[ouagadougou]  ).

ed(  4, [ where, is, the, largest, country, ? ],

		[asia, northern_asia]  ).

ed(  5, [ which, countries, are, european, ? ],

		[albania, andorra, austria, belgium, bulgaria, cyprus,
		czechoslovakia, denmark, east_germany, eire, finland,
		france, greece, hungary, iceland, italy, liechtenstein,
		luxembourg, malta, monaco, netherlands, norway, poland,
		portugal, romania, san_marino, spain, sweden, switzerland,
		united_kingdom, west_germany, yugoslavia]  ).

ed(  6, [ which, country, '''', s, capital, is, london, ? ],

		[united_kingdom]  ).

ed(  7, [ which, is, the, largest, african, country, ? ],

		[sudan]  ).

ed(  8, [ how, large, is, the, smallest, american, country, ? ],

		[0--ksqmiles]  ).

ed(  9, [ what, is, the, ocean, that, borders, african, countries,
	  and, that, borders, asian, countries, ? ],

		[indian_ocean]  ).

ed( 10, [ what, are, the, capitals, of, the, countries, bordering, the,
	  baltic, ? ],

		[[[denmark]:[copenhagen], [east_germany]:[/*east_*/ berlin],
		[finland]:[helsinki], [poland]:[warsaw],
		[soviet_union]:[moscow], [sweden]:[stockholm],
		[west_germany]:[bonn]]]  ).

ed( 11, [ which, countries, are, bordered, by, two, seas, ? ],

		[egypt, iran, israel, saudi_arabia, turkey]  ).

ed( 12, [ how, many, countries, does, the, danube, flow, through, ? ],

		[6]  ).

ed( 13, [ what, is, the, total, area, of, countries, south, of, the, equator,
	  and, not, in, australasia, ? ],

		[10228--ksqmiles]  ).

ed( 14, [ what, is, the, average, area, of, the, countries, in, each,
	  continent, ? ],

		[[africa,233--ksqmiles], [america,496--ksqmiles],
		[asia,485--ksqmiles], [australasia,543--ksqmiles],
		[europe,58--ksqmiles]]  ).

ed( 15, [ is, there, more, than, one, country, in, each, continent, ? ],

		[false]  ).

ed( 16, [ is, there, some, ocean, that, does, not, border, any, country, ? ],

		[true]  ).

ed( 17, [ what, are, the, countries, from, which, a, river, flows, into,
	  the, black_sea, ? ],

		[[romania,soviet_union]]  ).

ed( 18, [ what, are, the, continents, no, country, in, which, contains, more,
	  than, two, cities, whose, population, exceeds, (1), million, ? ],

		[[africa,antarctica,australasia]]  ).

ed( 19, [ which, country, bordering, the, mediterranean, borders, a, country,
	  that, is, bordered, by, a, country, whose, population, exceeds,
	  the, population, of, india, ? ],

		[turkey]  ).

ed( 20, [ which, countries, have, a, population, exceeding, (10),
	  million, ? ],

		[afghanistan, algeria, argentina, australia, bangladesh,
		brazil, burma, canada, china, colombia, czechoslovakia,
		east_germany, egypt, ethiopia, france, india, indonesia,
		iran, italy, japan, kenya, malaysia, mexico, morocco, nepal,
		netherlands, nigeria, north_korea, pakistan, peru,
		philippines, poland, south_africa, south_korea,
		soviet_union, spain, sri_lanka, sudan, taiwan, tanzania,
		thailand, turkey, uganda, united_kingdom, united_states, venezuela,
		vietnam, west_germany, yugoslavia, zaire]  ).

ed( 21, [ which, countries, with, a, population, exceeding, (10), million,
	  border, the, atlantic, ? ],

		[argentina, brazil, canada, colombia, france, mexico,
		morocco, netherlands, nigeria, south_africa, spain,
		united_kingdom, united_states, venezuela, west_germany,
		zaire]  ).

ed( 22, [ what, percentage, of, countries, border, each, ocean, ? ],

		[[arctic_ocean,2], [atlantic,35], [indian_ocean,14],
		[pacific,20]]  ).

ed( 23, [ what, countries, are, there, in, europe, ? ],

		[albania, andorra, austria, belgium, bulgaria, cyprus,
		czechoslovakia, denmark, east_germany, eire, finland,
		france, greece, hungary, iceland, italy, liechtenstein,
		luxembourg, malta, monaco, netherlands, norway, poland,
		portugal, romania, san_marino, spain, sweden, switzerland,
		united_kingdom, west_germany, yugoslavia]  ).


ed( 24, [ what, are, the, areas, of, the, countries, bordering, the, baltic, ? ],

		[[ [denmark]:[--(16.615,ksqmiles)],
                 [east_germany]:[--(40.646,ksqmiles)],
                 [finland]:[--(130.119,ksqmiles)],
                 [poland]:[--(120.359,ksqmiles)],
                 [soviet_union]:[--(8347.25,ksqmiles)],
                 [sweden]:[--(173.665,ksqmiles)],
                 [west_germany]:[--(95.815,ksqmiles)]
               ]]  ).

ed( N, [ what, are, the, rivers, that, flow, through, the, countries, bordering, the,
	  baltic, ? ],

		[[[denmark]:[copenhagen], [east_germany]:[/*east_*/ berlin],
		[finland]:[helsinki], [poland]:[warsaw],
		[soviet_union]:[moscow], [sweden]:[stockholm],
		[west_germany]:[bonn]]]  ):- N==25.


ed( N, [ what, are, the, rivers, that, flow, through, each, country, bordering, the,
	  baltic, ? ],

		[[[denmark]:[copenhagen], [east_germany]:[/*east_*/ berlin],
		[finland]:[helsinki], [poland]:[warsaw],
		[soviet_union]:[moscow], [sweden]:[stockholm],
		[west_germany]:[bonn]]]  ):- N==26.


ed( N, [ what, are, the, capitals, of, the, countries, bordering, the,
	  baltic, ? ],

		[[[denmark]:[copenhagen], [east_germany]:[/*east_*/ berlin],
		[finland]:[helsinki], [poland]:[warsaw],
		[soviet_union]:[moscow], [sweden]:[stockholm],
		[west_germany]:[bonn]]]  ):- N==27.

ed( N, [ what, are, the, cities, in, countries, bordering, the,
	  baltic, ? ],

		[[[denmark]:[copenhagen], [east_germany]:[/*east_*/ berlin],
		[finland]:[helsinki], [poland]:[warsaw],
		[soviet_union]:[moscow], [sweden]:[stockholm],
		[west_germany]:[bonn]]]  ):- N==28.

ed( N, [ what, cities, do, the, countries, bordering, the,
	  baltic, contain, ? ],

		[[[denmark]:[copenhagen], [east_germany]:[/*east_*/ berlin],
		[finland]:[helsinki], [poland]:[warsaw],
		[soviet_union]:[moscow], [sweden]:[stockholm],
		[west_germany]:[bonn]]]  ):- N==29.



ed( N, W, _):- 
  clause(ed( W, mini), true, Ref),
  nth_clause(_,N0,Ref),N is N0+29.
ed( N, W, _):- 
  clause(ed1( W), true, Ref),
  nth_clause(_,N0,Ref),N is N0+99.


%ed1("iraq will border iran?").
%ed1("iraq has border iran?").
%ed1("iraq has a border china").


chat80_all_broken("What cities do the countries bordering the baltic contain ?",Baltic_Cities,673):- baltic_cities(Baltic_Cities).
%chat80_all("What is the total area of countries south of the equator and are not in australasia ?",[--(10228,ksqmiles)],1302).
chat80_all("What are the continents not containing a country ?",[antarctica],208).
chat80_all("Is there some ocean that does not border any country ?",[true],1600).
chat80_all("How many countries is the danube flowing through ?",[6],_).
chat80_all("How many countries is the danube flowing from ?",[6],_).
chat80_all("How many countries is the danube flowing into ?",[5],_).
%chat80_all("How many countries is the danube raising from ?",[1],_).
chat80_all("How many countries does the danube flow through ?",[6],_).
chat80_all("How many countries does the danube flow from ?",[6],_).
chat80_all("How many countries does the danube flow into ?",[5],_).
chat80_all("How many countries does the danube rise from ?",[1],_).

chat80_all("What is the total area of countries south of the equator and not in australasia ?",[--(10228,ksqmiles)],1300).
chat80_all("What is the total area of countries south of the equator and in australasia ?",[--(3263.6419999999994,ksqmiles)],1301).
chat80_all("How many countries does the danube flow through ?",[6],1200).
chat80_all("Which countries with a population exceeding 10 million border the atlantic ?",[argentina,brazil,canada,colombia,france,mexico,morocco,netherlands,nigeria,south_africa,spain,united_kingdom,united_states,venezuela,west_germany,zaire],2100).
chat80_all("Which countries have a population exceeding 10 million ?",[afghanistan,algeria,argentina,australia,bangladesh,brazil,burma,canada,china,colombia,czechoslovakia,east_germany,egypt,ethiopia,france,india,indonesia,iran,italy,japan,kenya,malaysia,mexico,morocco,nepal,netherlands,nigeria,north_korea,pakistan,peru,philippines,poland,south_africa,south_korea,soviet_union,spain,sri_lanka,sudan,taiwan,tanzania,thailand,turkey,uganda,united_kingdom,united_states,venezuela,vietnam,west_germany,yugoslavia,zaire],2000).
chat80_all("Which rivers are not in asia ?",_,31800).
chat80_all("How many rivers flow to west germany ?",_,21301).
chat80_all("How many rivers flow to switzerland ?",_,21901).
chat80_all("How many rivers flow through switzerland ?",_,22301).
%chat80_all("How many rivers flow into switzerland ?",false,21801).
chat80_all("How many rivers flow in switzerland ?",_,22010).
chat80_all("How many rivers flow from switzerland ?",_,22101).
chat80_all("How many rivers flow at switzerland ?",_,22201).

chat80_all("Which is the largest african country ?",[sudan],700).
chat80_all("Which country's capital is london ?",[united_kingdom],600).
chat80_all("Which countries contain more than 3 cities ?",_,31100).
chat80_all("Which countries contain more than 2 cities ?",_,31200).
chat80_all("Which countries contain a city ?",_,30800).
chat80_all("Which countries contain 3 cities ?",_,31000).
chat80_all("Which countries contain 2 cities ?",_,30900).
chat80_all("Which countries are european ?",[albania,andorra,austria,belgium,bulgaria,cyprus,czechoslovakia,denmark,east_germany,eire,finland,france,greece,hungary,iceland,italy,liechtenstein,luxembourg,malta,monaco,netherlands,norway,poland,portugal,romania,san_marino,spain,sweden,switzerland,united_kingdom,west_germany,yugoslavia],500).
chat80_all("Which countries are bordered by two seas ?",[egypt,iran,israel,saudi_arabia,turkey],1100).
chat80_all("Which continents contain more than 4 cities ?",_,31300).
chat80_all("Which asian countries have a population exceeding 10 million ?",_,31400).
chat80_all("Where is the largest country ?",[asia,northern_asia],400).
chat80_all("What rivers are there ?",[amazon,amu_darya,amur,brahmaputra,colorado,congo_river,cubango,danube,don,elbe,euphrates,ganges,hwang_ho,indus,irrawaddy,lena,limpopo,mackenzie,mekong,mississippi,murray,niger_river,nile,ob,oder,orange,orinoco,parana,rhine,rhone,rio_grande,salween,seine,senegal_river,tagus,vistula,volga,volta,yangtze,yenisei,yukon,zambesi],100).
chat80_all("What rivers are in countries bordering the baltic ?",_,66600).
chat80_all("What percentage of countries border each ocean ?",[[arctic_ocean,2],[atlantic,35],[indian_ocean,14],[pacific,20]],2200).
chat80_all("What is the ocean that borders african countries and that borders asian countries ?",[indian_ocean],900).
chat80_all("What is the capital of upper volta ?",[ouagadougou],300).
chat80_all("What is the average area of the countries in each continent ?",[[africa,--(233,ksqmiles)],[america,--(496,ksqmiles)],[asia,--(485,ksqmiles)],[australasia,--(543,ksqmiles)],[europe,--(58,ksqmiles)]],1400).
chat80_all("What is a river that is in asia ?",_,31700).
chat80_all("What is a river that is asian ?",_,31900).
chat80_all("What is a river ?",_,31600).
chat80_all("What countries are there in europe ?",[albania,andorra,austria,belgium,bulgaria,cyprus,czechoslovakia,denmark,east_germany,eire,finland,france,greece,hungary,iceland,italy,liechtenstein,luxembourg,malta,monaco,netherlands,norway,poland,portugal,romania,san_marino,spain,sweden,switzerland,united_kingdom,west_germany,yugoslavia],23).
chat80_all("What are the rivers that flow through the countries bordering the baltic ?",Baltic_Rivers,672):- baltic_rivers(Baltic_Rivers).
chat80_all("What are the rivers that flow through each country bordering the baltic ?",Baltic_Rivers,671):- baltic_rivers(Baltic_Rivers).
chat80_all("What are the rivers of the countries bordering the baltic ?",Baltic_Rivers,668):- baltic_rivers_real(Baltic_Rivers).
chat80_all("What are the rivers in countries bordering the baltic ?",Baltic_Rivers,670):- baltic_rivers(Baltic_Rivers).
chat80_all("What are the countries from which a river flows into the black sea ?",[[romania,soviet_union]],17).
chat80_all("What are the cities of the countries bordering the baltic ?",Baltic_Cities,667):- baltic_cities_real(Baltic_Cities).
chat80_all("What are the cities in countries bordering the baltic ?",Baltic_Cities,669):- baltic_cities(Baltic_Cities).
chat80_all("What are the capitals of the countries bordering the baltic ?",[[[denmark]:[copenhagen],[east_germany]:[/*east_*/ berlin],[finland]:[helsinki],[poland]:[warsaw],[soviet_union]:[moscow],[sweden]:[stockholm],[west_germany]:[bonn]]],10).
chat80_all("What are the areas of the countries bordering the baltic ?",[[[denmark]:[--(16.615,ksqmiles)],[east_germany]:[--(40.646,ksqmiles)],[finland]:[--(130.119,ksqmiles)],[poland]:[--(120.359,ksqmiles)],[soviet_union]:[--(8347.25,ksqmiles)],[sweden]:[--(173.665,ksqmiles)],[west_germany]:[--(95.815,ksqmiles)]]],2400).
chat80_all("The rhine flows to west germany ?",true,21300).
chat80_all("The rhine flows to switzerland ?",false,21900).
chat80_all("The rhine flows through west germany ?",true,21700).
chat80_all("The rhine flows through switzerland ?",true,22300).
chat80_all("The rhine flows into west germany ?",true,21200).
chat80_all("The rhine flows into switzerland ?",false,21800).
chat80_all("The rhine flows in west germany ?",true,21400).
chat80_all("The rhine flows in switzerland ?",true,22000).
chat80_all("The rhine flows from west germany ?",true,21500).
chat80_all("The rhine flows from switzerland ?",true,22100).
chat80_all("The rhine flows at west germany ?",true,21600).
chat80_all("The rhine flows at switzerland ?",true,22200).




chat80_all("Is there more than one country in each continent ?",[false],1500).
chat80_all("Is the united kingdom in europe ?",[true],_00).
chat80_all("Is the rhine in switzerland ?",_,_00).
chat80_all("Is the population of china 840 million ?",[true],_00).
chat80_all("Is the population of china greater than 200 million ?",[true],_00).
chat80_all("Is spain bordered by the pacific ?",[false],_00).
chat80_all("Is New York in america ?",[true],_00).
chat80_all("Is New York in south america ?",[false],_00).
chat80_all("Iraq will border iran ?",_,10300).
chat80_all("Iraq was bordering iran ?",_,10500).
chat80_all("Iraq is bordering iran ?",_,10400).
chat80_all("Iraq does border iran ?",[true],10100).
chat80_all("Iraq did border iran ?",_,10200).
chat80_all("Iraq borders iran ?",[true],10000).
chat80_all("Iran is bordered by iraq ?",_,10600).
chat80_all("How many rivers are not in asia ?",25,20300).
chat80_all("How many rivers are in asia ?",16,20400).
chat80_all("How many countries have a population greater than 10 million ?",50,20700).
chat80_all("How many countries have a population exceeding 10 million ?",50,20600).
chat80_all("How many asian countries have a population exceeding 10 million ?",20,20500).
chat80_all("How large is the smallest american country ?",[--(0.133,ksqmiles)],800).
chat80_all("What is the smallest country in america?",_,_).
chat80_all("Is the Caribbean in america?",[true],_).
chat80_all("What is the smallest country in north america?",_,_).
chat80_all("What is the smallest country in central america?",_,_).
chat80_all("What is the smallest country in south america?",_,_).
chat80_all("What is the smallest country in caribbean?",_,_).

chat80_all("Does the population of china exceed 1000 million ?",[false],_00).
chat80_all("Does the atlantic border spain ?",[true],_00).
chat80_all("Does Mexico border the United States ?",[true],_00).
chat80_all("Does america contain new York ?",[true],_00).
chat80_all("What countries that are not bordering romania does the danube flow through ?",[austria, czechoslovakia, romania, west_germany],_).
chat80_all("What countries not bordering romania does the danube flow through ?",[austria, czechoslovakia, romania, west_germany],_).
chat80_all("How many countries does the danube not flow through ?",[150],_).
chat80_all("What countries do rivers rise from ?", [[ afghanistan, angola,australia,brazil,burma,canada,
                   china,colombia,czechoslovakia,france, guinea,lesotho, mongolia,peru,poland, 
                   south_africa,soviet_union,spain,switzerland, turkey,uganda, united_states, upper_volta,
                 west_germany,zambia]],_).
chat80_all("Does afghanistan border china ?",[true],200).
chat80_all("What are the continents no country in which contains more than two cities that have a population exceeding 1 million ?",[[africa,antarctica,australasia]],18).
      chat80_all("What are the continents no country in which contains more than two cities whose population exceeds 1 million ?",[[africa,antarctica,australasia]],18).
chat80_all("Does the population of china exceed the population of india ?",[true],_00).
chat80_all("Which country bordering the mediterranean borders a country that is bordered by a country whose population exceeds the population of india ?",[turkey],1900).
chat80_all("What are the continents containing a country in which contains more than two cities whose population exceeds 1 million ?",[america,asia,europe],211).
chat80_all("What are the rivers of the countries that are bordering the baltic that flow?",[[east_germany]:[elbe],[poland]:[oder,vistula],[soviet_union]:[amu_darya,amur,don,lena,ob,volga,yenisei],[west_germany]:[danube,elbe,rhine]],_).
chat80_all("What are the capitals of the countries that are bordering the baltic that have a population exceeding 1 thousand?",[[east_germany]:[berlin],[poland]:[warsaw],[soviet_union]:[moscow]],_).

baltic_rivers(_):-!.
baltic_rivers_real(_):-!.
baltic_rivers_real([[[[east_germany]:[elbe],
                  [poland]:[oder,vistula],
                  [soviet_union] :[ amu_darya, amur,don,lena,ob,volga,yenisei],
                  [west_germany]:[danube,elbe,rhine]]]]).

baltic_cities(_):-!.
baltic_cities_real(_):-!.
baltic_cities_real([[ [[denmark]:[copenhagen],
                  [east_germany]:[/*east_*/ berlin],
                  [finland]:[helsinki],
                  [poland]:[warsaw],
                  [soviet_union]:[kiev,leningrad,moscow],
                  [sweden]:[stockholm],
                  [west_germany]:[/*west*/ berlin,bonn,hamburg]]]]).

%will,can,could,would,should,ought
chat8_all("will border").
chat8_all("was bordering").
chat8_all("is bordering").
chat8_all("does border").
chat8_all("did border").
chat8_all("borders").
chat8_all("will be bordering").
chat8_all("could be bordering").
chat8_all("would be bordering").
chat8_all("has been bordering").
chat8_all("will have been bordering").
chat8_all("could have been bordering").
chat8_all("is bordered by").



end_of_file.
end_of_file.
end_of_file.
end_of_file.

?- 
 cls,c88("What are the cities of the countries bordering the baltic ?").
 root /opt/logicmoo_workspace/bin/cls /dev/tty
% /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/chat80/original/geography/load_kb compiled into parser_chat80 0.12 sec, 0 clauses
%~ list_undefined([]).

%~ ?- c88("What are the cities of the countries bordering the baltic ?").

/* text_to_corenlp_tree = [ 'CORENLP',
                            [ 'SBAR',
                              [ 'NP',
                                ['WP','What']],
                              [ 'S',
                                ['VB',are],
                                [ 'NP',
                                  [ 'NP',
                                    ['DT',the],
                                    ['NNS',cities]],
                                  [ 'PP',
                                    ['IN',of],
                                    [ 'NP',
                                      ['DT',the],
                                      ['NNS',countries]]]],
                                [ 'VP',
                                  ['VBG',bordering],
                                  [ 'NP',
                                    ['DT',the],
                                    ['JJ',baltic]]]],
                            ['.',?]]].  */

%~ into_lexical_segs is runtime(1.992).

%~ ?- c88("What are the cities of the countries bordering the baltic ?").

/* into_lexical_segs = [ w( what, [
                           pos(wp), root(what),loc(1),lnks(3),txt("What"),
                           truecase('INIT_UPPER'),link(1,'NP',r('NP',seg(1,1))),
                           link(2,'SBAR',r('SBAR',seg(1,11))), link(3,'CORENLP',r('CORENLP',seg(1,11))),lex_winfo,
                           ac(determinerStrings,xWhatTheWord,"what"), cycWord(xWhatTheWord),ac(pronounStrings,xWhatTheWord,"what"),
                           ttholds(inflAdverb,'TTWord_What',"what"),
                           ttholds(inflDeterminer,'TTWord_What',"what"),
                           ttholds(inflPronoun,'TTWord_What',"what"),
                           ttholds(inflInterjection,'TTWord_What',"what"),
                           flexicon( det,
                             [ sem=what, agr=sg,wh=y+n,det_type=quant,
                               preagr=n,can_be_np=y],
                             what),
                           flexicon( det,
                             [ sem=what, agr=pl,wh=y,det_type=quant,
                               preagr=n,can_be_np=n],
                             what),
                           whpron_dict(what,tSomethingExisting), whpron_dict(what,tThing),talk_db(pronoun,what),
                           talk_db(interrog,what)]),
                         w( are, [
                           alt(pos(vbp)), root(be),pos(vb),loc(2),lnks(3),
                           txt("are"),truecase('LOWER'),
                           link(1,'S',r('S',seg(2,10))),
                           link(2,'SBAR',r('SBAR',seg(1,11))), link(3,'CORENLP',r('CORENLP',seg(1,11))),lex_winfo,
                           ttholds(inflNounSingular,'TTWord_Are',"are"),
                           ttholds(inflVerbPluralPresent,'TTWord_Be',"are"),
                           flexicon(bv,[person=2,agr=sg,verb_form=tensed],are),
                           flexicon(bv,[person=Person,agr=pl,verb_form=tensed],are),
                           lex_rinfo( be, [
                             fsr(be-v,performers_and_roles,[]),
                             flexicon(bv,[person=Person1,agr=Agr,verb_form=infinitive],be),
                             verbnet_map_wn( be,
                               [ 'be%2:42:03', 'be%2:42:06','be%2:42:05','be%2:42:02',
                                 'be%2:41:00'],
                               'seem-109-1-1'),
                             listof( concept(wn), [
                               'be%2:42:03', 'be%2:42:06','be%2:42:05','be%2:42:02',
                               'be%2:41:00'])]),
                           unused( [ ac(infinitive,xBeTheWord,"be"), cycWord(xBeTheWord),concept(fn,performers_and_roles),
                                     ttholds(inflExpletiveUnchecked,'TTWord_Be',"be"),
                                     ttholds(inflVerb,'TTWord_Be',"be"),
                                     concept(vn,'seem-109-1-1'),
                                     verbnet_to_framenet('seem-109-1-1',be,'be.01'),
                                     concept(fn,'be.01'),
                                     verbnet_to_framenet('seem-109-1-1',be,'be.03'), concept(fn,'be.03'),verbnet_word(be,'seem-109-1-1',"be")])]),
                         w( the, [
                           pos(dt), root(the),loc(3),lnks(5),txt("the"),
                           truecase('LOWER'),link(1,'NP',r('NP',seg(3,4))),
                           link(2,'NP',r('NP',seg(3,7))),link(3,'S',r('S',seg(2,10))),
                           link(4,'SBAR',r('SBAR',seg(1,11))), link(5,'CORENLP',r('CORENLP',seg(1,11))),lex_winfo,
                           ac(determinerStrings,xTheTheWord,"the"),
                           cycWord(xTheTheWord),
                           ttholds(inflDeterminerPlural,'TTWord_The',"the"),
                           ttholds(inflDeterminerSingular,'TTWord_The',"the"),
                           flexicon( det,
                             [ sem=the_sing, agr=sg,article=y,wh=n,det_type=def,
                               preagr=n],
                             the),
                           flexicon( det,
                             [ sem=the_pl, agr=pl,article=y,wh=n,det_type=def,
                               preagr=y],
                             the),
                           flexicon(det,[agr=Agr3],the),
                           flexicon(timesuffix,[sem=morning,allows_minutes=y],[(in,the,morning)]),
                           flexicon(timesuffix,[sem=afternoon,allows_minutes=y],[(in,the,afternoon)])]),
                         w( cities, [
                           pos(nns), root(city),loc(4),lnks(5),txt("cities"),
                           truecase('LOWER'),link(1,'NP',r('NP',seg(3,4))),
                           link(2,'NP',r('NP',seg(3,7))),link(3,'S',r('S',seg(2,10))),
                           link(4,'SBAR',r('SBAR',seg(1,11))), link(5,'CORENLP',r('CORENLP',seg(1,11))),lex_winfo,
                           ac(singular,xCityTheWord,city),cycWord(xCityTheWord),
                           clex_word(noun,cities,city,neutr+pl-count),
                           ttholds(inflNounPlural,'TTWord_City',"cities")]),
                         w( of, [
                           pos(in), root(of),loc(5),lnks(5),txt("of"),
                           truecase('LOWER'),link(1,'PP',r('PP',seg(5,7))),
                           link(2,'NP',r('NP',seg(3,7))),link(3,'S',r('S',seg(2,10))),
                           link(4,'SBAR',r('SBAR',seg(1,11))), link(5,'CORENLP',r('CORENLP',seg(1,11))),lex_winfo,
                           ac(prepositionStrings,xOfTheWord,"of"),cycWord(xOfTheWord),
                           cycTerm(possessiveRelation,rtUnderspecifiedPrepositionPredicate,rtBinaryPredicate),
                           cycTerm( temporallySubsumes,
                             nartR(tColOfSampleInstanceOfTypeForProgramFn,rtBinaryTemporalRelationPredicate,iCW_CycAnalyticEnvironmentTheProgram),
                             rtComplexTemporalPredicate), fsr(of-prep,origin,[]),concept(fn,origin),
                           fsr(of-prep,partitive,[]),concept(fn,partitive),
                           ttholds(inflPreposition,'TTWord_Of',"of"),
                           talk_db(preposition,of)]),
                         w( the, [
                           pos(dt), root(the),loc(6),lnks(6),txt("the"),
                           truecase('LOWER'),link(1,'NP',r('NP',seg(6,7))),
                           link(2,'PP',r('PP',seg(5,7))), link(3,'NP',r('NP',seg(3,7))),link(4,'S',r('S',seg(2,10))),
                           link(5,'SBAR',r('SBAR',seg(1,11))), link(6,'CORENLP',r('CORENLP',seg(1,11))),lex_winfo,
                           ac(determinerStrings,xTheTheWord,"the"),
                           cycWord(xTheTheWord),
                           ttholds(inflDeterminerPlural,'TTWord_The',"the"),
                           ttholds(inflDeterminerSingular,'TTWord_The',"the"),
                           flexicon( det,
                             [ sem=the_sing, agr=sg,article=y,wh=n,det_type=def,
                               preagr=n],
                             the),
                           flexicon( det,
                             [ sem=the_pl, agr=pl,article=y,wh=n,det_type=def,
                               preagr=y],
                             the),
                           flexicon(det,[agr=Agr4],the),
                           flexicon(timesuffix,[sem=morning,allows_minutes=y],[(in,the,morning)]),
                           flexicon(timesuffix,[sem=afternoon,allows_minutes=y],[(in,the,afternoon)])]),
                         w( countries, [
                           pos(nns), root(country),loc(7),lnks(6),
                           txt("countries"),truecase('LOWER'),
                           link(1,'NP',r('NP',seg(6,7))),
                           link(2,'PP',r('PP',seg(5,7))), link(3,'NP',r('NP',seg(3,7))),link(4,'S',r('S',seg(2,10))),
                           link(5,'SBAR',r('SBAR',seg(1,11))), link(6,'CORENLP',r('CORENLP',seg(1,11))),lex_winfo,
                           ac(massNumber,xCountryTheWord,country),
                           cycWord(xCountryTheWord),
                           ac(singular,xCountryTheWord,country),
                           clex_word(noun,countries,country,neutr+pl-count),
                           ttholds(inflNounPlural,'TTWord_Country',"countries")]),
                         w( bordering, [
                           pos(vbg), root(border),loc(8),lnks(4),txt("bordering"),
                           truecase('LOWER'),link(1,'VP',r('VP',seg(8,10))),
                           link(2,'S',r('S',seg(2,10))),
                           link(3,'SBAR',r('SBAR',seg(1,11))), link(4,'CORENLP',r('CORENLP',seg(1,11))),lex_winfo,
                           ac(infinitive,xBorderTheWord,border),
                           cycWord(xBorderTheWord),
                           talk_db(noun_or_verb,borders,bordering,border),
                           talk_db(intransitive,border,borders,bordered,bordering,bordered),
                           talk_db(transitive,border,borders,bordered,bordering,bordered),
                           lex_rinfo( border, [
                             fsr(border-n,boundary,[]),
                             fsr(border-v,locative_relation,[]),
                             verbnet_map_wn( border,
                               ['border%2:35:01','border%2:42:00','border%2:35:00'],
                               'contiguous_location-47.8'),
                             listof(concept(wn),['border%2:35:01','border%2:42:00','border%2:35:00'])]),
                           unused( [ ac(infinitive,xBorderTheWord,"border"),
                                     cycTerm(actBorderingSomething,ttFirstOrderCollection,ttThing),
                                     cycTerm('Bordering-Configuration',tCol,ttThing),
                                     cycTerm( nartR(actMakingAvailableFn,tObjectBorderStripNearTheBoundaryOfAnObject),
                                       ttTemporalStuffType,
                                       ttSetOrCollection),
                                     ac(singular,xBorderTheWord,"border"),
                                     cycTerm(iLoc_BorderBetweenFn,rtCoexistenceRelation,rtContemporaryInResultFunction),
                                     cycTerm(tBorder,ttCartographicFeatureType,ttFirstOrderCollection),
                                     cycTerm(iLoc_BorderFn,rtCoexistenceRelation,rtUnaryFunction),
                                     cycTerm(tObjectBorderStripNearTheBoundaryOfAnObject,ttExistingObjectType,ttThing),
                                     cycTerm(tBorderline,ttArtifactualFeatureType,ttFirstOrderCollection),
                                     cycTerm(iLoc_BorderBetweenPolitiesFn,rtBinaryFunction,rtReifiableFunction),
                                     ac(singular,xBorderTheWord,border),
                                     ac(singular,xBorderTheWord,border),
                                     ac(singular,xBorderTheWord,border),
                                     clex_word(verb,border,border,iv+infpl),
                                     clex_word(verb,border,border,tv+infpl),
                                     clex_word(noun,border,border,human+sg-count), concept(fn,boundary),concept(fn,locative_relation),
                                     ttholds(inflNounSingular,'TTWord_Border',"border"),
                                     concept(vn,'contiguous_location-47.8'),
                                     verbnet_to_framenet('contiguous_location-47.8',border,'border.01'),
                                     concept(fn,'border.01'),
                                     verbnet_word(border,'contiguous_location-47.8',"border")])]),
                         w( the, [
                           pos(dt), root(the),loc(9),lnks(5),txt("the"),
                           truecase('LOWER'),link(1,'NP',r('NP',seg(9,10))),
                           link(2,'VP',r('VP',seg(8,10))),
                           link(3,'S',r('S',seg(2,10))),
                           link(4,'SBAR',r('SBAR',seg(1,11))), link(5,'CORENLP',r('CORENLP',seg(1,11))),lex_winfo,
                           ac(determinerStrings,xTheTheWord,"the"),
                           cycWord(xTheTheWord),
                           ttholds(inflDeterminerPlural,'TTWord_The',"the"),
                           ttholds(inflDeterminerSingular,'TTWord_The',"the"),
                           flexicon( det,
                             [ sem=the_sing, agr=sg,article=y,wh=n,det_type=def,
                               preagr=n],
                             the),
                           flexicon( det,
                             [ sem=the_pl, agr=pl,article=y,wh=n,det_type=def,
                               preagr=y],
                             the),
                           flexicon(det,[agr=Agr5],the),
                           flexicon(timesuffix,[sem=morning,allows_minutes=y],[(in,the,morning)]),
                           flexicon(timesuffix,[sem=afternoon,allows_minutes=y],[(in,the,afternoon)])]),
                         w( baltic, [
                           pos(jj), root(baltic),loc(10),lnks(5),txt("baltic"),
                           truecase('INIT_UPPER'),link(1,'NP',r('NP',seg(9,10))),
                           link(2,'VP',r('VP',seg(8,10))),
                           link(3,'S',r('S',seg(2,10))),
                           link(4,'SBAR',r('SBAR',seg(1,11))), link(5,'CORENLP',r('CORENLP',seg(1,11))),lex_winfo,
                           talk_db(adj,baltic)]),
                         w( ?, [
                           pos('.'), root(?),loc(11),lnks(2),txt("?"),
                           link(1,'SBAR',r('SBAR',seg(1,11))), link(2,'CORENLP',r('CORENLP',seg(1,11))),lex_winfo,
                         terminator_lex(?,?),char_type_sentence(?,ask)])].  */

%~ ?- c88("What are the cities of the countries bordering the baltic ?").

%~ any_to_ace_str="What are the cities of the countries bordering the baltic ?".

%~ ?- c88("What are the cities of the countries bordering the baltic ?").

%~ try_ace_drs=failure.
penn_tree_to_segs(
       [ 'CORENLP',
         [ 'SBAR',
           [ 'NP',
             ['WP','What']],
           [ 'S',
             ['VB',are],
             [ 'NP',
               [ 'NP',
                 ['DT',the],
                 ['NNS',cities]],
               [ 'PP',
                 ['IN',of],
                 [ 'NP',
                   ['DT',the],
                   ['NNS',countries]]]],
             [ 'VP',
               ['VBG',bordering],
               [ 'NP',
                 ['DT',the],
                 ['JJ',baltic]]]],
           ['.',?]]],
       _15088).
%~ call_residue_vars=[].

%~ ?- c88("What are the cities of the countries bordering the baltic ?").

%~ e2c_80=failure.

%~ ?- c88("What are the cities of the countries bordering the baltic ?").

%~ redo( sentence8d( whq( Wh_S13,
%~                     s( np(3+_88874,wh(Wh_S13),[]),
%~                        verb(
%~                           aux+aux(be,[pres+fin]), aux(be,[pres+fin]),activeV,pres+fin,
%~                           [],
%~                           [ [], info(aux(be,[pres+fin])),info(pres+fin),
%~                             root]),
%~                        [ arg( dirO,
%~                            np( 3+pl,
%~                              np_head(Det,det(the(pl)),[],city),
%~                              [ prep_phrase( prep(of),
%~                                  np( 3+pl,
%~                                    np_head(Det3,det(the(pl)),[],country),
%~                                    [ reduced_rel( Wh_S,
%~                                        s( np(3+pl,wh(Wh_S),[]),
%~                                           verb(main+tv,border,activeV,inf,[progresiveV],[was_framed(frame_5295)]),
%~                                           [ arg(dirO,np(3+sg,nameOf(Baltic,baltic,[]),[]))],
%~                                           []))]))]))],
%~                        [])))).
%~ redo( sentence8d( whq( Wh_S13,
%~                     s( np(3+_58136,wh(Wh_S13),[]),
%~                        verb(
%~                           aux+aux(be,[pres+fin]), aux(be,[pres+fin]),activeV,pres+fin,
%~                           [],
%~                           [ [], info(aux(be,[pres+fin])),info(pres+fin),
%~                             root]),
%~                        [ arg( dirO,
%~                            np( 3+pl,
%~                              np_head(Det,det(the(pl)),[],city),
%~                              [ prep_phrase( prep(of),
%~                                  np( 3+pl,
%~                                    np_head(Det3,det(the(pl)),[],country),
%~                                    [ reduced_rel( Wh_S,
%~                                        s( np(3+pl,wh(Wh_S),[]),
%~                                           verb(main+tv,border,activeV,inf,[progresiveV],[was_framed(frame_5441)]),
%~                                           [ arg(dirO,np(3+sg,nameOf(Baltic,baltic,[]),[]))],
%~                                           []))]))]))],
%~                        [])))).
%~ redo( sentence8d( whq( Wh_S13,
%~                     s( np(3+_33664,wh(Wh_S13),[]),
%~                        verb(
%~                           aux+aux(be,[pres+fin]), aux(be,[pres+fin]),activeV,pres+fin,
%~                           [],
%~                           [ [], info(aux(be,[pres+fin])),info(pres+fin),
%~                             root]),
%~                        [ arg( dirO,
%~                            np( 3+pl,
%~                              np_head(Det,det(the(pl)),[],city),
%~                              [ prep_phrase( prep(of),
%~                                  np( 3+pl,
%~                                    np_head(Det3,det(the(pl)),[],country),
%~                                    [ reduced_rel( Wh_S,
%~                                        s( np(3+pl,wh(Wh_S),[]),
%~                                           verb(main+tv,border,activeV,inf,[progresiveV],[was_framed(frame_5587)]),
%~                                           [ arg(dirO,np(3+sg,nameOf(Baltic,baltic,[]),[]))],
%~                                           []))]))]))],
%~                        [])))).
%~ redo( sentence8d( whq( Wh_S13,
%~                     s( np(3+_87194,wh(Wh_S13),[]),
%~                        verb(
%~                           aux+aux(be,[pres+fin]), aux(be,[pres+fin]),activeV,pres+fin,
%~                           [],
%~                           [ [], info(aux(be,[pres+fin])),info(pres+fin),
%~                             root]),
%~                        [ arg( dirO,
%~                            np( 3+pl,
%~                              np_head(Det,det(the(pl)),[],city),
%~                              [ prep_phrase( prep(of),
%~                                  np( 3+pl,
%~                                    np_head(Det3,det(the(pl)),[],country),
%~                                    [ reduced_rel( Wh_S,
%~                                        s( np(3+pl,wh(Wh_S),[]),
%~                                           verb(main+tv,border,activeV,inf,[progresiveV],[was_framed(frame_6000)]),
%~                                           [ arg(dirO,np(3+sg,nameOf(Baltic,baltic,[]),[]))],
%~                                           []))]))]))],
%~                        [])))).
%~ redo( sentence8d( whq( Wh_S13,
%~                     s( np(3+_65730,wh(Wh_S13),[]),
%~                        verb(
%~                           aux+aux(be,[pres+fin]), aux(be,[pres+fin]),activeV,pres+fin,
%~                           [],
%~                           [ [], info(aux(be,[pres+fin])),info(pres+fin),
%~                             root]),
%~                        [ arg( dirO,
%~                            np( 3+pl,
%~                              np_head(Det,det(the(pl)),[],city),
%~                              [ prep_phrase( prep(of),
%~                                  np( 3+pl,
%~                                    np_head(Det3,det(the(pl)),[],country),
%~                                    [ reduced_rel( Wh_S,
%~                                        s( np(3+pl,wh(Wh_S),[]),
%~                                           verb(main+tv,border,activeV,inf,[progresiveV],[was_framed(frame_6146)]),
%~                                           [ arg(dirO,np(3+sg,nameOf(Baltic,baltic,[]),[]))],
%~                                           []))]))]))],
%~                        [])))).
%~ redo( sentence8d( whq( Wh_S13,
%~                     s( np(3+_50280,wh(Wh_S13),[]),
%~                        verb(
%~                           aux+aux(be,[pres+fin]), aux(be,[pres+fin]),activeV,pres+fin,
%~                           [],
%~                           [ [], info(aux(be,[pres+fin])),info(pres+fin),
%~                             root]),
%~                        [ arg( dirO,
%~                            np( 3+pl,
%~                              np_head(Det,det(the(pl)),[],city),
%~                              [ prep_phrase( prep(of),
%~                                  np( 3+pl,
%~                                    np_head(Det3,det(the(pl)),[],country),
%~                                    [ reduced_rel( Wh_S,
%~                                        s( np(3+pl,wh(Wh_S),[]),
%~                                           verb(main+tv,border,activeV,inf,[progresiveV],[was_framed(frame_6292)]),
%~                                           [ arg(dirO,np(3+sg,nameOf(Baltic,baltic,[]),[]))],
%~                                           []))]))]))],
%~                        [])))).
%~ redo( sentence8d( whq( Wh_S13,
%~                     s( np(3+_119974,wh(Wh_S13),[]),
%~                        verb(
%~                           aux+aux(be,[pres+fin]), aux(be,[pres+fin]),activeV,pres+fin,
%~                           [],
%~                           [[],root]),
%~                        [ arg( dirO,
%~                            np( 3+pl,
%~                              np_head(Det,det(the(pl)),[],city),
%~                              [ prep_phrase( prep(of),
%~                                  np( 3+pl,
%~                                    np_head(Det3,det(the(pl)),[],country),
%~                                    [ reduced_rel( Wh_S,
%~                                        s( np(3+pl,wh(Wh_S),[]),
%~                                           verb(main+tv,border,activeV,inf,[progresiveV],[was_framed(frame_6705)]),
%~                                           [ arg(dirO,np(3+sg,nameOf(Baltic,baltic,[]),[]))],
%~                                           []))]))]))],
%~                        [])))).
%~ redo( sentence8d( whq( Wh_S13,
%~                     s( np(3+_103352,wh(Wh_S13),[]),
%~                        verb(
%~                           aux+aux(be,[pres+fin]), aux(be,[pres+fin]),activeV,pres+fin,
%~                           [],
%~                           [[],root]),
%~                        [ arg( dirO,
%~                            np( 3+pl,
%~                              np_head(Det,det(the(pl)),[],city),
%~                              [ prep_phrase( prep(of),
%~                                  np( 3+pl,
%~                                    np_head(Det3,det(the(pl)),[],country),
%~                                    [ reduced_rel( Wh_S,
%~                                        s( np(3+pl,wh(Wh_S),[]),
%~                                           verb(main+tv,border,activeV,inf,[progresiveV],[was_framed(frame_6851)]),
%~                                           [ arg(dirO,np(3+sg,nameOf(Baltic,baltic,[]),[]))],
%~                                           []))]))]))],
%~                        [])))).
%~ redo( sentence8d( whq( Wh_S13,
%~                     s( np(3+_88580,wh(Wh_S13),[]),
%~                        verb(
%~                           aux+aux(be,[pres+fin]), aux(be,[pres+fin]),activeV,pres+fin,
%~                           [],
%~                           [[],root]),
%~                        [ arg( dirO,
%~                            np( 3+pl,
%~                              np_head(Det,det(the(pl)),[],city),
%~                              [ prep_phrase( prep(of),
%~                                  np( 3+pl,
%~                                    np_head(Det3,det(the(pl)),[],country),
%~                                    [ reduced_rel( Wh_S,
%~                                        s( np(3+pl,wh(Wh_S),[]),
%~                                           verb(main+tv,border,activeV,inf,[progresiveV],[was_framed(frame_6997)]),
%~                                           [ arg(dirO,np(3+sg,nameOf(Baltic,baltic,[]),[]))],
%~                                           []))]))]))],
%~                        [])))).

%~ sentence80 is runtime(4.262).

%~ ?- c88("What are the cities of the countries bordering the baltic ?").

/* sentence80 = whq( Wh_S13,
                  s( np(3+_59046,wh(Wh_S13),[]),
                     verb(
                        aux+aux(be,[pres+fin]), aux(be,[pres+fin]),activeV,pres+fin,
                        [],
                        [ [], info(aux(be,[pres+fin])),info(pres+fin),
                          root]),
                     [ arg( dirO,
                         np( 3+pl,
                           np_head(Det,det(the(pl)),[],city),
                           [ prep_phrase( prep(of),
                               np( 3+pl,
                                 np_head(Det3,det(the(pl)),[],country),
                                 [ reduced_rel( Wh_S,
                                     s( np(3+pl,wh(Wh_S),[]),
                                        verb(main+tv,border,activeV,inf,[progresiveV],[was_framed(frame_10138)]),
                                        [ arg(dirO,np(3+sg,nameOf(Baltic,baltic,[]),[]))],
                                        []))]))]))],
                   [])).  */

%~ ?- c88("What are the cities of the countries bordering the baltic ?").

/* i_sentence = question80( [thing&geo&Geo-_130040],
                  pred(
                     quantS( identityQ(IdentityQ),
                       thing&geo&Geo-_52390, '`'(true),'`'(true),
                       [],
                       thing&geo&Geo-Is),
                     identityQ([[],info(aux(be,[pres+fin])),info(pres+fin),root]),
                     bE(is,Is,BE),
                     [ quantS( set(- + (- 0)),
                         thing&geo&Geo-Prep_City, '`'(ti(city,Prep_City)),'`'(true),
                         [ ( quantS( set(- (- + (- 0))),
                               thing&Thing-Country, '`'(ti(country,Country)),'`'(true),
                               [],
                               thing&Thing-Generic) &
                             '`'(generic_pred(frame_10138,thing,prep(of),Prep_City,Generic))),
                           pred(
                              quantS( identityQ(IdentityQ4),
                                thing&geo&Geo-Prep_City, '`'(true),'`'(true),
                                [],
                                thing&geo&Geo-_86922),
                              identityQ([was_framed(frame_10138)]),
                              generic_pred(frame_10138,thing,mg(border),_69950,Generic5),
                              [ quantS( identityQ(IdentityQ6),
                                  thing&geo&seamass-Baltic, '`'(bE(named,Baltic,baltic)),'`'(true),
                                  [],
                                  thing&geo&seamass-Generic5)])],
                       thing&geo&Geo-BE)])).  */

%~ ?- c88("What are the cities of the countries bordering the baltic ?").

/* clausify80 = answer80([Is]) :-
                  ^( BE_SetOf9,
                    ( setOf( Mg_Prep_City,
                        ( ^( Generic_SetOf,
                            ( setOf(Country,((ti(country,Country),true),true),Generic_SetOf)  ,
                              bE(named,Generic_Baltic,baltic) ,
                              ti(city,Mg_Prep_City) ,
                              generic_pred(frame_10138,thing,prep(of),Mg_Prep_City,Generic_SetOf) ,
                              generic_pred(frame_10138,thing,mg(border),Mg_Prep_City,Generic_Baltic)))),
                        BE_SetOf9) ,
                    bE(is,Is,BE_SetOf9))).  */

%~ ?- c88("What are the cities of the countries bordering the baltic ?").





























/* simplify80 = answer80([Is]) :-
                  ^( BE_SetOf8,
                    ( setOf( Border_Prep_City,
                        ^( Generic_SetOf,
                          ( setOf(Ti_Country,ti(country,Ti_Country),Generic_SetOf)  ,
                            ti(city,Border_Prep_City) ,
                            generic_pred(frame_10138,thing,prep(of),Border_Prep_City,Generic_SetOf) ,
                            generic_pred(frame_10138,thing,border,Border_Prep_City,baltic))),
                        BE_SetOf8) ,
                    bE(is,Is,BE_SetOf8))).  */

%~ ?- c88("What are the cities of the countries bordering the baltic ?").

%~ results80=[].

%~ ?- c88("What are the cities of the countries bordering the baltic ?").

/* compile80 = ( setof_oR_nil( Is,
                   ^( [Generic_SetOf,BE_SetOf8],
                     ( setOf( Border_Prep_City,
                         ^( [Generic_SetOf],
                           ( setOf(Country,[]^database80(ti(country,Country)),
                           Generic_SetOf)  ,
                             database80(ti(city,Border_Prep_City)) ,
                             database80(generic_pred(frame_10138,thing,prep(of),Border_Prep_City,Generic_SetOf)) ,
                             database80(generic_pred(frame_10138,thing,border,Border_Prep_City,baltic)))),
                         BE_SetOf8) ,
                       database80(bE(is,Is,BE_SetOf8)))),
                   Respond_Nil) ,
               respond(Respond_Nil)).  */

%~ ?- c88("What are the cities of the countries bordering the baltic ?").

c88("What are the cities of the countries that are bordering the baltic ?").

%~ capture80="Nothing satisfies your question.\n".

true.

parser_chat80: 2699 ?-
