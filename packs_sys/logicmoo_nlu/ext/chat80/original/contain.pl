/*

_________________________________________________________________________
|       Copyright (C) 1982                                                |
|                                                                         |
|       David Warren,                                                     |
|               SRI International, 333 Ravenswood Ave., Menlo Park,       |
|               California 94025, USA;                                    |
|                                                                         |
|       Fernando Pereira,                                                 |
|               Dept. of Architecture, University of Edinburgh,           |
|               20 Chambers St., Edinburgh EH1 1JZ, Scotland              |
|                                                                         |
|       This program may be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|


*/

:- abolish(tmp80:trans_rel_cache_creating,2).
:- abolish(tmp80:trans_rel_cache_created,2).
:- abolish(tmp80:trans_rel_cache_insts,3).
:- abolish(tmp80:trans_rel_cache,4).

:- dynamic(tmp80:trans_rel_cache_creating/2).
:- dynamic(tmp80:trans_rel_cache_created/2).
:- dynamic(tmp80:trans_rel_cache_insts/3).
:- dynamic(tmp80:trans_rel_cache/4).

trans_rel(Spatial,Contain,X,Y):- \+ compound(Contain),!,
  trace_or_throw(wrong(trans_rel(Spatial,Contain,X,Y))),
  trans_rel(=,trans_direct(Spatial,Contain),X,Y).

trans_rel(P1,P2,X,Y) :- trans_rel_cache_create(P1,P2),!, tmp80:trans_rel_cache(P1,P2,X,Y).
trans_rel(P1,P2,X,Y):- trans_rel_nc(P1,P2,X,Y).

trans_rel_nc(P1,P2,X,Y) :- var(X),!, no_repeats(X, trans_rel_rl(P1,P2,X,Y)).
trans_rel_nc(P1,P2,X,Y) :- nonvar(Y), !, trans_rel_lr(P1,P2,X,Y), !.
trans_rel_nc(P1,P2,X,Y) :- no_repeats(Y, trans_rel_lr(P1,P2,X,Y)).

trans_rel_lr(P1,P2,X,Y) :- call(P2,X,W), ( call(P1,W,Y) ; trans_rel_lr(P1,P2,W,Y) ).
trans_rel_rl(P1,P2,X,Y) :- call(P2,W,Y), ( call(P1,W,X) ; trans_rel_rl(P1,P2,X,W) ).



trans_rel_cache_create(P1,P2):- must_be(ground,(P1,P2)),
                                tmp80:trans_rel_cache_created(P1,P2),!.
trans_rel_cache_create(P1,P2):- tmp80:trans_rel_cache_creating(P1,P2),dmsg(looped(trans_rel_cache_create(P1,P2))),fail.
trans_rel_cache_create(P1,P2):-
  asserta((tmp80:trans_rel_cache_creating(P1,P2)),Ref),
  dmsg(trans_rel_cache_creating(P1,P2)),
  forall(call(P2,XX,YY),
     (assert_if_new(tmp80:trans_rel_cache_insts(P1,P2,XX)),
      assert_if_new(tmp80:trans_rel_cache_insts(P1,P2,YY)))),
  forall(tmp80:trans_rel_cache_insts(P1,P2,E),
        (forall(trans_rel_nc(P1,P2,E,Y),assert_if_new(tmp80:trans_rel_cache(P1,P2,E,Y))),
         forall(trans_rel_nc(P1,P2,Y,E),assert_if_new(tmp80:trans_rel_cache(P1,P2,Y,E))))),
  dmsg(trans_rel_cache_created(P1,P2)),
  asserta((tmp80:trans_rel_cache_created(P1,P2))),!,
  erase(Ref),
  %listing(tmp80:trans_rel_cache_insts(P1,P2,_Instances)),
  %listing(tmp80:trans_rel_cache(P1,P2,_,_)),
  !.


%contain(X,Y) :- trans_direct(spatial,contain,X,Y).
%contain(X,Y) :- trans_direct(spatial,contain,X,W), contain(W,Y).


trans_pred(Spatial,Contain,X,Y) :- trans_rel(=,trans_direct(Spatial,Contain),X,Y).
%contain(X,X).

ti(region,R) :- continent_contains_region(_,R).
ti(continent,X):- continent(X).

continent(africa).
continent(america).
continent(antarctica).
continent(asia).
continent(australasia).
continent(europe).

:- multifile(trans_direct/4).
:- dynamic(trans_direct/4).


% Inversion of the 'in' relation.
% ------------------------------
trans_direct(spatial,contain,R1,R2):- geo_contains(R1,R2).


geo_contains(Continent,Region):- continent_contains_region(Continent,Region).
geo_contains(Region,Country):- region_contains_country(Region,Country).
geo_contains(Country,CityOrRiver):- country_contains_thing(Country,CityOrRiver).
geo_contains(Country,River):- path_pred(thru_from(river_flows),river,River,Country).


continent_contains_region(africa,central_africa).
continent_contains_region(africa,east_africa).
continent_contains_region(africa,north_africa).
continent_contains_region(africa,southern_africa).
continent_contains_region(africa,west_africa).

continent_contains_region(america,caribbean).
continent_contains_region(america,central_america).
continent_contains_region(america,north_america).
continent_contains_region(america,south_america).

continent_contains_region(asia,far_east).
continent_contains_region(asia,indian_subcontinent).
continent_contains_region(asia,middle_east).
continent_contains_region(asia,northern_asia).
continent_contains_region(asia,southeast_east).

continent_contains_region(australasia,oceania).

continent_contains_region(europe,eastern_europe).
continent_contains_region(europe,scandinavia).
continent_contains_region(europe,southern_europe).
continent_contains_region(europe,western_europe).

region_contains_country(R,C) :- c_r_l_l_s_cap_m(C,R,_,_,_,_,_,_).
/*region_contains_country(oceania,australia).
region_contains_country(oceania,fiji).
region_contains_country(oceania,new_zealand).
region_contains_country(oceania,papua_new_guinea).
region_contains_country(oceania,tonga).
region_contains_country(oceania,western_samoa).

region_contains_country(scandinavia,denmark).
region_contains_country(scandinavia,finland).
region_contains_country(scandinavia,norway).
region_contains_country(scandinavia,sweden).

region_contains_country(western_europe,austria).
region_contains_country(western_europe,belgium).
region_contains_country(western_europe,eire).
region_contains_country(western_europe,france).
region_contains_country(western_europe,iceland).
region_contains_country(western_europe,liechtenstein).
region_contains_country(western_europe,luxembourg).
region_contains_country(western_europe,netherlands).
region_contains_country(western_europe,switzerland).
region_contains_country(western_europe,united_kingdom).
region_contains_country(western_europe,west_germany).

region_contains_country(eastern_europe,bulgaria).
region_contains_country(eastern_europe,czechoslovakia).
region_contains_country(eastern_europe,east_germany).
region_contains_country(eastern_europe,hungary).
region_contains_country(eastern_europe,poland).
region_contains_country(eastern_europe,romania).

region_contains_country(southern_europe,albania).
region_contains_country(southern_europe,andorra).
region_contains_country(southern_europe,cyprus).
region_contains_country(southern_europe,greece).
region_contains_country(southern_europe,italy).
region_contains_country(southern_europe,malta).
region_contains_country(southern_europe,monaco).
region_contains_country(southern_europe,portugal).
region_contains_country(southern_europe,san_marino).
region_contains_country(southern_europe,spain).
region_contains_country(southern_europe,yugoslavia).

region_contains_country(north_america,canada).
region_contains_country(north_america,united_states).

region_contains_country(central_america,belize).
region_contains_country(central_america,costa_rica).
region_contains_country(central_america,el_salvador).
region_contains_country(central_america,guatemala).
region_contains_country(central_america,honduras).
region_contains_country(central_america,mexico).
region_contains_country(central_america,nicaragua).
region_contains_country(central_america,panama).

region_contains_country(caribbean,bahamas).
region_contains_country(caribbean,barbados).
region_contains_country(caribbean,cuba).
region_contains_country(caribbean,dominican_republic).
region_contains_country(caribbean,grenada).
region_contains_country(caribbean,haiti).
region_contains_country(caribbean,jamaica).
region_contains_country(caribbean,trinidad_and_tobago).

region_contains_country(south_america,argentina).
region_contains_country(south_america,bolivia).
region_contains_country(south_america,brazil).
region_contains_country(south_america,chile).
region_contains_country(south_america,colombia).
region_contains_country(south_america,ecuador).
region_contains_country(south_america,french_guiana).
region_contains_country(south_america,guyana).
region_contains_country(south_america,paraguay).
region_contains_country(south_america,peru).
region_contains_country(south_america,surinam).
region_contains_country(south_america,uruguay).
region_contains_country(south_america,venezuela).

region_contains_country(north_africa,algeria).
region_contains_country(north_africa,egypt).
region_contains_country(north_africa,libya).
region_contains_country(north_africa,morocco).
region_contains_country(north_africa,tunisia).

region_contains_country(west_africa,cameroon).
region_contains_country(west_africa,dahomey).
region_contains_country(west_africa,equatorial_guinea).
region_contains_country(west_africa,gambia).
region_contains_country(west_africa,ghana).
region_contains_country(west_africa,guinea).
region_contains_country(west_africa,guinea_bissau).
region_contains_country(west_africa,ivory_coast).
region_contains_country(west_africa,liberia).
region_contains_country(west_africa,mali).
region_contains_country(west_africa,mauritania).
region_contains_country(west_africa,niger).
region_contains_country(west_africa,nigeria).
region_contains_country(west_africa,senegal).
region_contains_country(west_africa,sierra_leone).
region_contains_country(west_africa,togo).
region_contains_country(west_africa,upper_volta).

region_contains_country(central_africa,burundi).
region_contains_country(central_africa,central_african_republic).
region_contains_country(central_africa,chad).
region_contains_country(central_africa,congo).
region_contains_country(central_africa,gabon).
region_contains_country(central_africa,rwanda).
region_contains_country(central_africa,sudan).
region_contains_country(central_africa,zaire).

region_contains_country(east_africa,djibouti).
region_contains_country(east_africa,ethiopia).
region_contains_country(east_africa,kenya).
region_contains_country(east_africa,seychelles).
region_contains_country(east_africa,somalia).
region_contains_country(east_africa,tanzania).
region_contains_country(east_africa,uganda).

region_contains_country(southern_africa,angola).
region_contains_country(southern_africa,botswana).
region_contains_country(southern_africa,lesotho).
region_contains_country(southern_africa,malagasy).
region_contains_country(southern_africa,malawi).
region_contains_country(southern_africa,mauritius).
region_contains_country(southern_africa,mozambique).
region_contains_country(southern_africa,south_africa).
region_contains_country(southern_africa,swaziland).
region_contains_country(southern_africa,zambia).
region_contains_country(southern_africa,zimbabwe).

region_contains_country(middle_east,bahrain).
region_contains_country(middle_east,iran).
region_contains_country(middle_east,iraq).
region_contains_country(middle_east,israel).
region_contains_country(middle_east,jordan).
region_contains_country(middle_east,kuwait).
region_contains_country(middle_east,lebanon).
region_contains_country(middle_east,oman).
region_contains_country(middle_east,qatar).
region_contains_country(middle_east,saudi_arabia).
region_contains_country(middle_east,south_yemen).
region_contains_country(middle_east,syria).
region_contains_country(middle_east,turkey).
region_contains_country(middle_east,united_arab_emirates).
region_contains_country(middle_east,yemen).

region_contains_country(indian_subcontinent,afghanistan).
region_contains_country(indian_subcontinent,bangladesh).
region_contains_country(indian_subcontinent,bhutan).
region_contains_country(indian_subcontinent,india).
region_contains_country(indian_subcontinent,maldives).
region_contains_country(indian_subcontinent,nepal).
region_contains_country(indian_subcontinent,pakistan).
region_contains_country(indian_subcontinent,sri_lanka).

region_contains_country(southeast_east,burma).
region_contains_country(southeast_east,cambodia).
region_contains_country(southeast_east,indonesia).
region_contains_country(southeast_east,laos).
region_contains_country(southeast_east,malaysia).
region_contains_country(southeast_east,philippines).
region_contains_country(southeast_east,singapore).
region_contains_country(southeast_east,thailand).
region_contains_country(southeast_east,vietnam).

region_contains_country(far_east,china).
region_contains_country(far_east,japan).
region_contains_country(far_east,north_korea).
region_contains_country(far_east,south_korea).
region_contains_country(far_east,taiwan).

region_contains_country(northern_asia,mongolia).
region_contains_country(northern_asia,soviet_union).
*/
country_contains_thing(afghanistan,amu_darya).

country_contains_thing(angola,cubango).
country_contains_thing(angola,zambesi).

country_contains_thing(argentina,buenos_aires).
country_contains_thing(argentina,parana).

country_contains_thing(australia,melbourne).
country_contains_thing(australia,murray).
country_contains_thing(australia,sydney).

country_contains_thing(austria,danube).
country_contains_thing(austria,vienna).

country_contains_thing(bangladesh,brahmaputra).

country_contains_thing(belgium,brussels).

country_contains_thing(brazil,amazon).
country_contains_thing(brazil,parana).
country_contains_thing(brazil,rio_de_janeiro).
country_contains_thing(brazil,sao_paulo).

country_contains_thing(burma,irrawaddy).
country_contains_thing(burma,salween).

country_contains_thing(cambodia,mekong).

country_contains_thing(canada,mackenzie).
country_contains_thing(canada,montreal).
country_contains_thing(canada,toronto).
country_contains_thing(canada,yukon).

country_contains_thing(chile,santiago).

country_contains_thing(china,amur).
country_contains_thing(china,brahmaputra).
country_contains_thing(china,canton).
country_contains_thing(china,chungking).
country_contains_thing(china,dairen).
country_contains_thing(china,ganges).
country_contains_thing(china,harbin).
country_contains_thing(china,hwang_ho).
country_contains_thing(china,indus).
country_contains_thing(china,kowloon).
country_contains_thing(china,mekong).
country_contains_thing(china,mukden).
country_contains_thing(china,peking).
country_contains_thing(china,salween).
country_contains_thing(china,shanghai).
country_contains_thing(china,sian).
country_contains_thing(china,tientsin).
country_contains_thing(china,yangtze).

country_contains_thing(colombia,orinoco).

country_contains_thing(czechoslovakia,danube).
country_contains_thing(czechoslovakia,elbe).
country_contains_thing(czechoslovakia,oder).

country_contains_thing(east_germany,berlin).
country_contains_thing(east_germany,elbe).

country_contains_thing(west_germany,danube).
country_contains_thing(west_germany,elbe).
country_contains_thing(west_germany,hamburg).
country_contains_thing(west_germany,rhine).


country_contains_thing(egypt,cairo).
country_contains_thing(egypt,nile).

country_contains_thing(france,paris).
country_contains_thing(france,rhone).
country_contains_thing(france,seine).

country_contains_thing(ghana,volta).

country_contains_thing(greece,athens).

country_contains_thing(guinea,niger_river).
country_contains_thing(guinea,senegal_river).

country_contains_thing(hungary,budapest).
country_contains_thing(hungary,danube).

country_contains_thing(india,bombay).
country_contains_thing(india,calcutta).
country_contains_thing(india,delhi).
country_contains_thing(india,ganges).
country_contains_thing(india,hyderabad).
country_contains_thing(india,indus).
country_contains_thing(india,madras).

country_contains_thing(indonesia,jakarta).

country_contains_thing(iran,tehran).

country_contains_thing(iraq,euphrates).

country_contains_thing(italy,milan).
country_contains_thing(italy,naples).
country_contains_thing(italy,rome).

country_contains_thing(japan,kobe).
country_contains_thing(japan,kyoto).
country_contains_thing(japan,nagoya).
country_contains_thing(japan,nanking).
country_contains_thing(japan,osaka).
country_contains_thing(japan,tokyo).
country_contains_thing(japan,yokohama).

country_contains_thing(laos,mekong).

country_contains_thing(lesotho,orange).

country_contains_thing(mali,niger_river).
country_contains_thing(mali,senegal_river).

country_contains_thing(mexico,colorado).
country_contains_thing(mexico,mexico_city).
country_contains_thing(mexico,rio_grande).

country_contains_thing(mongolia,amur).
country_contains_thing(mongolia,yenisei).

country_contains_thing(mozambique,limpopo).
country_contains_thing(mozambique,zambesi).

country_contains_thing(netherlands,rhine).
country_contains_thing(netherlands,amsterdam).
country_contains_thing(netherlands,amersfoort).

country_contains_thing(niger,niger_river).

country_contains_thing(nigeria,niger_river).

country_contains_thing(pakistan,indus).
country_contains_thing(pakistan,karachi).

country_contains_thing(paraguay,parana).

country_contains_thing(peru,amazon).
country_contains_thing(peru,lima).

country_contains_thing(philippines,manila).

country_contains_thing(poland,oder).
country_contains_thing(poland,vistula).
country_contains_thing(poland,warsaw).

country_contains_thing(portugal,tagus).

country_contains_thing(romania,bucharest).
country_contains_thing(romania,danube).

country_contains_thing(senegal,senegal_river).

country_contains_thing(singapore,singapore_city).

country_contains_thing(south_africa,cubango).
country_contains_thing(south_africa,johannesburg).
country_contains_thing(south_africa,limpopo).
country_contains_thing(south_africa,orange).

country_contains_thing(south_korea,pusan).
country_contains_thing(south_korea,seoul).

country_contains_thing(soviet_union,amu_darya).
country_contains_thing(soviet_union,amur).
country_contains_thing(soviet_union,don).
country_contains_thing(soviet_union,kiev).
country_contains_thing(soviet_union,lena).
country_contains_thing(soviet_union,leningrad).
country_contains_thing(soviet_union,moscow).
country_contains_thing(soviet_union,ob).
country_contains_thing(soviet_union,volga).
country_contains_thing(soviet_union,yenisei).

country_contains_thing(spain,barcelona).
country_contains_thing(spain,madrid).
country_contains_thing(spain,tagus).

country_contains_thing(sudan,nile).

country_contains_thing(switzerland,rhine).
country_contains_thing(switzerland,rhone).

country_contains_thing(syria,euphrates).

country_contains_thing(thailand,bangkok).

country_contains_thing(turkey,euphrates).
country_contains_thing(turkey,istanbul).

country_contains_thing(uganda,nile).

country_contains_thing(united_kingdom,birmingham).
country_contains_thing(united_kingdom,glasgow).
country_contains_thing(united_kingdom,london).

country_contains_thing(united_states,chicago).
country_contains_thing(united_states,colorado).
country_contains_thing(united_states,detroit).
country_contains_thing(united_states,los_angeles).
country_contains_thing(united_states,mississippi).
country_contains_thing(united_states,new_york).
country_contains_thing(united_states,philadelphia).
country_contains_thing(united_states,rio_grande).
country_contains_thing(united_states,yukon).

country_contains_thing(upper_volta,volta).

country_contains_thing(venezuela,caracas).
country_contains_thing(venezuela,orinoco).

country_contains_thing(vietnam,mekong).
country_contains_thing(vietnam,saigon).


country_contains_thing(yugoslavia,danube).

country_contains_thing(zaire,congo_river).

country_contains_thing(zambia,congo_river).
country_contains_thing(zambia,zambesi).

country_contains_thing(Country,City) :- clause(city_country_popu(City,Country,_),true).
country_contains_thing(Country,City) :- specific_pred(spatial,nation_capital,Country,City).

:- fixup_exports.

