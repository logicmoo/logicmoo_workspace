
% :-module(world0,[]).
/*

 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/
:- shared_parser_data((contains0/2,country/8,city/3,borders/2,in_continent/2)).





% Data for the World Database.
% ---------------------------
:-op(600,xfy,--).
in_continent(scandinavia, europe).
in_continent(western_europe, europe).
in_continent(eastern_europe, europe).
in_continent(southern_europe, europe).
in_continent(north_america, america).
in_continent(central_america, america).
in_continent(caribbean, america).
in_continent(south_america, america).
in_continent(north_africa, africa).
in_continent(west_africa, africa).
in_continent(central_africa, africa).
in_continent(east_africa, africa).
in_continent(southern_africa, africa).
in_continent(middle_east,  asia).
in_continent(indian_subcontinent, asia).
in_continent(southeast_east, asia).
in_continent(far_east, asia).
in_continent(northern_asia, asia).





contains0(africa,central_africa).
contains0(africa,east_africa).
contains0(africa,north_africa).
contains0(africa,southern_africa).
contains0(africa,west_africa).

contains0(america,caribbean).
contains0(america,central_america).
contains0(america,north_america).
contains0(america,south_america).

contains0(asia,far_east).
contains0(asia,indian_subcontinent).
contains0(asia,middle_east).
contains0(asia,northern_asia).
contains0(asia,southeast_east).

contains0(australasia,australia).
contains0(australasia,fiji).
contains0(australasia,new_zealand).
contains0(australasia,papua_new_guinea).
contains0(australasia,tonga).
contains0(australasia,western_samoa).

contains0(europe,eastern_europe).
contains0(europe,scandinavia).
contains0(europe,southern_europe).
contains0(europe,western_europe).

contains0(scandinavia,denmark).
contains0(scandinavia,finland).
contains0(scandinavia,norway).
contains0(scandinavia,sweden).

contains0(western_europe,austria).
contains0(western_europe,belgium).
contains0(western_europe,eire).
contains0(western_europe,france).
contains0(western_europe,iceland).
contains0(western_europe,liechtenstein).
contains0(western_europe,luxembourg).
contains0(western_europe,netherlands).
contains0(western_europe,switzerland).
contains0(western_europe,united_kingdom).
contains0(western_europe,west_germany).

contains0(eastern_europe,bulgaria).
contains0(eastern_europe,czechoslovakia).
contains0(eastern_europe,east_germany).
contains0(eastern_europe,hungary).
contains0(eastern_europe,poland).
contains0(eastern_europe,romania).

contains0(southern_europe,albania).
contains0(southern_europe,andorra).
contains0(southern_europe,cyprus).
contains0(southern_europe,greece).
contains0(southern_europe,italy).
contains0(southern_europe,malta).
contains0(southern_europe,monaco).
contains0(southern_europe,portugal).
contains0(southern_europe,san_marino).
contains0(southern_europe,spain).
contains0(southern_europe,yugoslavia).

contains0(north_america,canada).
contains0(north_america,united_states).

contains0(central_america,belize).
contains0(central_america,costa_rica).
contains0(central_america,el_salvador).
contains0(central_america,guatemala).
contains0(central_america,honduras).
contains0(central_america,mexico).
contains0(central_america,nicaragua).
contains0(central_america,panama).

contains0(caribbean,bahamas).
contains0(caribbean,barbados).
contains0(caribbean,cuba).
contains0(caribbean,dominican_republic).
contains0(caribbean,grenada).
contains0(caribbean,haiti).
contains0(caribbean,jamaica).
contains0(caribbean,trinidad_and_tobago).

contains0(south_america,argentina).
contains0(south_america,bolivia).
contains0(south_america,brazil).
contains0(south_america,chile).
contains0(south_america,colombia).
contains0(south_america,ecuador).
contains0(south_america,french_guiana).
contains0(south_america,guyana).
contains0(south_america,paraguay).
contains0(south_america,peru).
contains0(south_america,surinam).
contains0(south_america,uruguay).
contains0(south_america,venezuela).

contains0(north_africa,algeria).
contains0(north_africa,egypt).
contains0(north_africa,libya).
contains0(north_africa,morocco).
contains0(north_africa,tunisia).

contains0(west_africa,cameroon).
contains0(west_africa,dahomey).
contains0(west_africa,equatorial_guinea).
contains0(west_africa,gambia).
contains0(west_africa,ghana).
contains0(west_africa,guinea).
contains0(west_africa,guinea_bissau).
contains0(west_africa,ivory_coast).
contains0(west_africa,liberia).
contains0(west_africa,mali).
contains0(west_africa,mauritania).
contains0(west_africa,niger).
contains0(west_africa,nigeria).
contains0(west_africa,senegal).
contains0(west_africa,sierra_leone).
contains0(west_africa,togo).
contains0(west_africa,upper_volta).

contains0(central_africa,burundi).
contains0(central_africa,central_african_republic).
contains0(central_africa,chad).
contains0(central_africa,congo).
contains0(central_africa,gabon).
contains0(central_africa,rwanda).
contains0(central_africa,sudan).
contains0(central_africa,zaire).

contains0(east_africa,djibouti).
contains0(east_africa,ethiopia).
contains0(east_africa,kenya).
contains0(east_africa,seychelles).
contains0(east_africa,somalia).
contains0(east_africa,tanzania).
contains0(east_africa,uganda).

contains0(southern_africa,angola).
contains0(southern_africa,botswana).
contains0(southern_africa,lesotho).
contains0(southern_africa,malagasy).
contains0(southern_africa,malawi).
contains0(southern_africa,mauritius).
contains0(southern_africa,mozambique).
contains0(southern_africa,south_africa).
contains0(southern_africa,swaziland).
contains0(southern_africa,zambia).
contains0(southern_africa,zimbabwe).

contains0(middle_east,bahrain).
contains0(middle_east,iran).
contains0(middle_east,iraq).
contains0(middle_east,israel).
contains0(middle_east,jordan).
contains0(middle_east,kuwait).
contains0(middle_east,lebanon).
contains0(middle_east,oman).
contains0(middle_east,qatar).
contains0(middle_east,saudi_arabia).
contains0(middle_east,south_yemen).
contains0(middle_east,syria).
contains0(middle_east,turkey).
contains0(middle_east,united_arab_emirates).
contains0(middle_east,yemen).

contains0(indian_subcontinent,afghanistan).
contains0(indian_subcontinent,bangladesh).
contains0(indian_subcontinent,bhutan).
contains0(indian_subcontinent,india).
contains0(indian_subcontinent,maldives).
contains0(indian_subcontinent,nepal).
contains0(indian_subcontinent,pakistan).
contains0(indian_subcontinent,sri_lanka).

contains0(southeast_east,burma).
contains0(southeast_east,cambodia).
contains0(southeast_east,indonesia).
contains0(southeast_east,laos).
contains0(southeast_east,malaysia).
contains0(southeast_east,philippines).
contains0(southeast_east,singapore).
contains0(southeast_east,thailand).
contains0(southeast_east,vietnam).

contains0(far_east,china).
contains0(far_east,japan).
contains0(far_east,north_korea).
contains0(far_east,south_korea).
contains0(far_east,taiwan).

contains0(northern_asia,mongolia).
contains0(northern_asia,soviet_union).

contains0(afghanistan,amu_darya).

contains0(angola,cubango).
contains0(angola,zambesi).

contains0(argentina,buenos_aires).
contains0(argentina,parana).

contains0(australia,melbourne).
contains0(australia,murray).
contains0(australia,sydney).

contains0(austria,danube).
contains0(austria,vienna).

contains0(bangladesh,brahmaputra).

contains0(belgium,brussels).

contains0(brazil,amazon).
contains0(brazil,parana).
contains0(brazil,rio_de_janeiro).
contains0(brazil,sao_paulo).

contains0(burma,irrawaddy).
contains0(burma,salween).

contains0(cambodia,mekong).

contains0(canada,mackenzie).
contains0(canada,montreal).
contains0(canada,toronto).
contains0(canada,yukon).

contains0(chile,santiago).

contains0(china,amur).
contains0(china,brahmaputra).
contains0(china,canton).
contains0(china,chungking).
contains0(china,dairen).
contains0(china,ganges).
contains0(china,harbin).
contains0(china,hwang_ho).
contains0(china,indus).
contains0(china,kowloon).
contains0(china,mekong).
contains0(china,mukden).
contains0(china,peking).
contains0(china,salween).
contains0(china,shanghai).
contains0(china,sian).
contains0(china,tientsin).
contains0(china,yangtze).

contains0(colombia,orinoco).

contains0(czechoslovakia,danube).
contains0(czechoslovakia,elbe).
contains0(czechoslovakia,oder).

contains0(east_germany,berlin).
contains0(east_germany,elbe).

contains0(egypt,cairo).
contains0(egypt,nile).

contains0(france,paris).
contains0(france,rhone).

contains0(ghana,volta).

contains0(greece,athens).

contains0(guinea,niger_river).
contains0(guinea,senegal_river).

contains0(hungary,budapest).
contains0(hungary,danube).

contains0(india,bombay).
contains0(india,calcutta).
contains0(india,delhi).
contains0(india,ganges).
contains0(india,hyderabad).
contains0(india,indus).
contains0(india,madras).

contains0(indonesia,jakarta).

contains0(iran,tehran).

contains0(iraq,euphrates).

contains0(italy,milan).
contains0(italy,naples).
contains0(italy,rome).

contains0(japan,kobe).
contains0(japan,kyoto).
contains0(japan,nagoya).
contains0(japan,nanking).
contains0(japan,osaka).
contains0(japan,tokyo).
contains0(japan,yokohama).

contains0(laos,mekong).

contains0(lesotho,orange).

contains0(mali,niger_river).
contains0(mali,senegal_river).

contains0(mexico,colorado).
contains0(mexico,mexico_city).
contains0(mexico,rio_grande).

contains0(mongolia,amur).
contains0(mongolia,yenisei).

contains0(mozambique,limpopo).
contains0(mozambique,zambesi).

contains0(netherlands,rhine).

contains0(niger,niger_river).

contains0(nigeria,niger_river).

contains0(pakistan,indus).
contains0(pakistan,karachi).

contains0(paraguay,parana).

contains0(peru,amazon).
contains0(peru,lima).

contains0(philippines,manila).

contains0(poland,oder).
contains0(poland,vistula).
contains0(poland,warsaw).

contains0(portugal,tagus).

contains0(romania,bucharest).
contains0(romania,danube).

contains0(senegal,senegal_river).

contains0(singapore,singapore_city).

contains0(south_africa,cubango).
contains0(south_africa,johannesburg).
contains0(south_africa,limpopo).
contains0(south_africa,orange).

contains0(south_korea,pusan).
contains0(south_korea,seoul).

contains0(soviet_union,amu_darya).
contains0(soviet_union,amur).
contains0(soviet_union,don).
contains0(soviet_union,kiev).
contains0(soviet_union,lena).
contains0(soviet_union,leningrad).
contains0(soviet_union,moscow).
contains0(soviet_union,ob).
contains0(soviet_union,volga).
contains0(soviet_union,yenisei).

contains0(spain,barcelona).
contains0(spain,madrid).
contains0(spain,tagus).

contains0(sudan,nile).

contains0(switzerland,rhine).
contains0(switzerland,rhone).

contains0(syria,euphrates).

contains0(thailand,bangkok).

contains0(turkey,euphrates).
contains0(turkey,istanbul).

contains0(uganda,nile).

contains0(united_kingdom,birmingham).
contains0(united_kingdom,glasgow).
contains0(united_kingdom,london).

contains0(united_states,chicago).
contains0(united_states,colorado).
contains0(united_states,detroit).
contains0(united_states,los_angeles).
contains0(united_states,mississippi).
contains0(united_states,new_york).
contains0(united_states,philadelphia).
contains0(united_states,rio_grande).
contains0(united_states,yukon).

contains0(upper_volta,volta).

contains0(venezuela,caracas).
contains0(venezuela,orinoco).

contains0(vietnam,mekong).
contains0(vietnam,saigon).

contains0(west_germany,danube).
contains0(west_germany,elbe).
contains0(west_germany,hamburg).
contains0(west_germany,rhine).

contains0(yugoslavia,danube).

contains0(zaire,congo_river).

contains0(zambia,congo_river).
contains0(zambia,zambesi).


/*

 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

% Facts about countries.
% ---------------------

% country(Country,Region,Latitude,Longitude,
%         Area (sqmiles),
%         Population,
%         Capital,Currency)


country(afghanistan,indian_subcontinent,33,-65,254861,18290000,kabul,afghani).
country(albania,southern_europe,41,-20,11100,2350000,tirana,lek).
country(algeria,north_africa,35,-11,919951,15770000,algiers,dinar).
country(andorra,southern_europe,42,-1,179,25000,andorra_la_villa,franc_peseta).
country(angola,southern_africa,-12,-18,481351,5810000,luanda,?).
country(argentina,south_america,-35,66,1072067,23920000,buenos_aires,peso).
country(australia,australasia,-23,-135,2967909,13268000,canberra,australian_dollar).
country(austria,western_europe,47,-14,32374,7520000,vienna,schilling).
country(bahamas,caribbean,24,74,4404,190000,nassau,bahamian_dollar).
country(bahrain,middle_east,26,-50,231,230000,manama,dinar).
country(bangladesh,indian_subcontinent,24,-90,55126,71317000,dacca,taka).
country(barbados,caribbean,13,59,166,240000,bridgetown,east_carribean_dollar).
country(belgium,western_europe,51,-5,11779,9711000,brussels,franc).
country(belize,central_america,17,88,8866,82000,belize_town,?).
country(bhutan,indian_subcontinent,27,-90,19305,1150000,thimphu,indian_rupee).
country(bolivia,south_america,-17,64,424162,5330000,sucre,peso).
country(botswana,southern_africa,-22,-24,219815,650000,gaborone,south_african_rand).
country(brazil,south_america,-13,53,3286470,105137000,brasilia,cruzeiro).
country(bulgaria,eastern_europe,43,-25,42829,8620000,sofia,lev).
country(burma,southeast_east,21,-96,261789,29560000,rangoon,kyat).
country(burundi,central_africa,-3,-30,10739,3600000,bujumbura,franc).
country(cambodia,southeast_east,12,-105,69898,7640000,phnom_penh,riel).
country(cameroon,west_africa,3,-12,183568,6170000,yaounde,cfa_franc).
country(canada,north_america,60,100,3851809,22047000,ottawa,canadian_dollar).
country(central_african_republic,central_africa,7,-20,241313,1720000,bangui,cfa_franc).
country(chad,central_africa,12,-17,495752,3870000,n_djamena,cfa_franc).
country(chile,south_america,-35,71,286396,10230000,santiago,escudo).
country(china,far_east,30,-110,3691502,840000000.000000,peking,yuan).
country(colombia,south_america,4,73,455335,23210000,bogota,peso).
country(congo,central_africa,-1,-16,132046,1001000,brazzaville,cfa_franc).
country(costa_rica,central_america,10,84,19653,1890000,san_jose,colon).
country(cuba,caribbean,22,79,44218,8870000,havana,peso).
country(cyprus,southern_europe,35,-33,3572,660000,nicosia,pound).
country(czechoslovakia,eastern_europe,49,-17,49371,14580000,prague,koruna).
country(dahomey,west_africa,8,-2,43483,2910000,porto_novo,cfa_franc).
country(denmark,scandinavia,55,-9,16615,5130000,copenhagen,krone).
country(djibouti,east_africa,12,-42,9071,45000,djibouti,?).
country(dominican_republic,caribbean,19,70,18704,4430000,santa_domingo,peso).
country(east_germany,eastern_europe,52,-12,40646,16980000,east_berlin,ddr_mark).
country(ecuador,south_america,-2,78,105685,6730000,quito,sucre).
country(egypt,north_africa,28,-31,386872,35620000,cairo,egyptian_pound).
country(eire,western_europe,53,8,26600,3030000,dublin,irish_pound).
country(el_salvador,central_america,14,89,8260,3860000,san_salvador,colon).
country(equatorial_guinea,west_africa,1,-10,10832,300000,santa_isabel,peveta).
country(ethiopia,east_africa,8,-40,457142,26080000,addis_ababa,ethiopean_dollar).
country(fiji,australasia,-17,-179,7055,550000,suva,fiji_dollar).
country(finland,scandinavia,65,-27,130119,4660000,helsinki,markka).
country(france,western_europe,47,-3,212973,52350000,paris,franc).
country(french_guiana,south_america,4,53,34740,27000,cayenne,?).
country(gabon,central_africa,0,-10,102317,520000,libreville,cfa_franc).
country(gambia,west_africa,13,16,4003,490000,banjul,dalasi).
country(ghana,west_africa,6,1,92100,9360000,accra,cedi).
country(greece,southern_europe,40,-23,50547,9030000,athens,drachma).
country(grenada,caribbean,12,61,133,100000,st_georges,east_caribbean_dollar).
country(guatemala,central_america,15,90,42042,5540000,guatamala_city,quetzal).
country(guinea,west_africa,10,10,94925,4210000,conakry,syli).
country(guinea_bissau,west_africa,12,15,13948,510000,bissau,pataca).
country(guyana,south_america,5,59,83000,760000,georgetown,guyana_dollar).
country(haiti,caribbean,19,72,10714,5200000,port_au_prince,gourde).
country(honduras,central_america,14,86,43277,2780000,tegucigalpa,lempira).
country(hungary,eastern_europe,47,-19,35919,10410000,budapest,forint).
country(iceland,western_europe,65,19,39702,210000,reykjavik,krona).
country(india,indian_subcontinent,20,-80,1229919,574219776.000000,new_delhi,rupee).
country(indonesia,southeast_east,-5,-115,735268,124600000,jakarta,rupiah).
country(iran,middle_east,33,-53,636363,32001000,tehran,rial).
country(iraq,middle_east,33,-44,167567,10410000,baghdad,dinar).
country(israel,middle_east,32,-35,34493,3228000,jerusalem,israeli_pound).
country(italy,southern_europe,42,-13,116303,55262000,rome,lira).
country(ivory_coast,west_africa,7,5,124503,4640000,abidjan,cfa_franc).
country(jamaica,caribbean,18,77,4411,1980000,kingston,jamaican_dollar).
country(japan,far_east,36,-136,143574,108710000,tokyo,yen).
country(jordan,middle_east,31,-36,32297,2560000,amman,dinar).
country(kenya,east_africa,1,-38,224960,12480000,nairobi,kenya_shilling).
country(kuwait,middle_east,29,-47,7780,880000,kuwait_city,kuwaiti_dinar).
country(laos,southeast_east,18,-105,3180,3180000,vientiane,kip).
country(lebanon,middle_east,34,-36,4015,3213000,beirut,lebanese_pound).
country(lesotho,southern_africa,-30,-28,11716,1200000,masero,rand).
country(liberia,west_africa,6,9,43000,1660000,monrovia,us_dollar).
country(libya,north_africa,28,-17,679536,2257000,tripoli,libyan_dinar).
country(liechtenstein,western_europe,47,-9,62,23000,vaduz,swiss_franc).
country(luxembourg,western_europe,50,-6,999,350000,luxembourg,luxembourg_franc).
country(malagasy,southern_africa,-20,-47,203035,7655000,tananarive,ariary).
country(malawi,southern_africa,-13,-34,45747,4790000,zomba,kwacha).
country(malaysia,southeast_east,5,-110,128328,10920000,kuala_lumpa,malaysian_dollar).
country(maldives,indian_subcontinent,2,-73,115,123000,male,rupee).
country(mali,west_africa,15,10,464873,5380000,bamako,mali_franc).
country(malta,southern_europe,36,-14,122,319000,valetta,pound).
country(mauritania,west_africa,21,10,419229,1260000,nouakchott,ouguiya).
country(mauritius,southern_africa,-20,-57,787,870000,port_louis,rupee).
country(mexico,central_america,20,100,761601,54300000,mexico_city,peso).
country(monaco,southern_europe,44,-7,1,30000,monaco,french_franc).
country(mongolia,northern_asia,47,-103,604247,1360000,ulan_bator,tighrik).
country(morocco,north_africa,32,6,171953,16310000,rabat,dirham).
country(mozambique,southern_africa,-19,-35,303373,8820000,maputo,?).
country(nepal,indian_subcontinent,28,-84,54362,12020000,katmandu,nepalese_rupee).
country(netherlands,western_europe,52,-5,14192,13500000,amsterdam,guilder).
country(new_zealand,australasia,-40,-176,103736,2962000,wellington,new_zealand_dollar).
country(nicaragua,central_america,12,85,57143,2010000,managua,cordoba).
country(niger,west_africa,13,-10,489206,4300000,niamey,cfa_franc).
country(nigeria,west_africa,8,-8,356669,79759000,lagos,naira).
country(north_korea,far_east,40,-127,46768,15090000,pvongvang,won).
country(norway,scandinavia,64,-11,125181,3960000,oslo,krone).
country(oman,middle_east,23,-58,82000,720000,muscat,riyal_omani).
country(pakistan,indian_subcontinent,30,-70,342750,66750000,islamad,rupee).
country(panama,central_america,9,80,28753,1570000,panama,balboa).
country(papua_new_guinea,australasia,-8,-145,183540,2580000,port_harcourt,australian_dollar).
country(paraguay,south_america,-23,57,157047,2670000,asuncion,guarani).
country(peru,south_america,-8,75,496222,14910000,lima,sol).
country(philippines,southeast_east,12,-123,115707,40220000,quezon_city,piso).
country(poland,eastern_europe,52,-20,120359,33360000,warsaw,zloty).
country(portugal,southern_europe,40,7,35340,8560000,lisbon,escudo).
country(qatar,middle_east,25,-51,4000,115000,doha,riyal).
country(romania,eastern_europe,46,-25,91699,5690000,bucharest,leu).
country(rwanda,central_africa,-2,-30,10169,3980000,kigali,rwanda_franc).
country(san_marino,southern_europe,44,-12,24,20000,san_marino,italian_lira).
country(saudi_arabia,middle_east,26,-44,873000,8100000,riyadh,riyal).
country(senegal,west_africa,14,14,76124,4230000,dakar,cfa_franc).
country(seychelles,east_africa,-4,-55,40,156000,victoria,rupee).
country(sierra_leone,west_africa,9,12,27925,2860000,freetown,leone).
country(singapore,southeast_east,1,-104,226,2190000,singapore,singapore_dollar).
country(somalia,east_africa,7,-47,246155,3100000,mogadishu,somali_shilling).
country(south_africa,southern_africa,-30,-25,471819,23720000,pretoria,rand).
country(south_korea,far_east,36,-128,38031,33333000,seoul,won).
country(south_yemen,middle_east,15,-48,111000,1600000,aden,dinar).
country(soviet_union,northern_asia,57,-80,8347250,250900000,moscow,ruble).
country(spain,southern_europe,40,5,194883,34860000,madrid,peseta).
country(sri_lanka,indian_subcontinent,7,-81,25332,13250000,colombo,rupee).
country(sudan,central_africa,15,-30,967491,16900000,khartoum,pound).
country(surinam,south_america,4,56,55000,208000,paramaribo,?).
country(swaziland,southern_africa,-26,-31,6705,460000,mbabane,lilageru).
country(sweden,scandinavia,63,-15,173665,8144000,stockholm,krona).
country(switzerland,western_europe,46,-8,15941,6440000,bern,franc).
country(syria,middle_east,35,-38,71498,6895000,damascus,syrian_pound).
country(taiwan,far_east,23,-121,13592,15737000,taipei,taiwan_dollar).
country(tanzania,east_africa,-7,-34,363708,14000000,dar_es_salaam,tanzanian_shilling).
country(thailand,southeast_east,16,-102,198455,39950000,bangkok,baht).
country(togo,west_africa,8,-1,21853,2120000,lome,cfa_franc).
country(tonga,australasia,-20,173,269,90000,nukualofa,pa_anga).
country(trinidad_and_tobago,caribbean,10,61,1979,5510000,port_of_spain,trinidad_and_tobago_dollar).
country(tunisia,north_africa,33,-9,63378,5510000,tunis,dinar).
country(turkey,middle_east,39,-36,301380,37930000,ankara,lira).
country(uganda,east_africa,2,-32,91134,10810000,kampala,uganda_shilling).
country(united_arab_emirates,middle_east,24,-54,32278,210000,abu_dhabi,dirham).
country(united_kingdom,western_europe,54,2,94209,55930000,london,pound).
country(united_states,north_america,37,96,3615122,211210000,washington,dollar).
country(upper_volta,west_africa,12,2,105869,5740000,ouagadougou,cfa_franc).
country(uruguay,south_america,-32,55,68548,2990000,montevideo,peso).
country(venezuela,south_america,8,65,352143,11520000,caracas,bolivar).
country(vietnam,southeast_east,17,-107,126436,41850000,hanoi,dong).
country(west_germany,western_europe,52,-9,95815,61970000,bonn,deutsche_mark).
country(western_samoa,australasia,-14,172,1133,150000,apia,tala).
country(yemen,middle_east,15,-44,75289,1600000,sana,rial).
country(yugoslavia,southern_europe,44,-20,98766,21126000,belgrade,dinar).
country(zaire,central_africa,-3,-23,905063,23560000,kinshasa,zaire).
country(zambia,southern_africa,-15,-28,290724,4640000,lusaka,kwacha).
country(zimbabwe,southern_africa,-20,-30,150333,5690000,salisbury,rhodesian_dollar).

/*

 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

% Facts about cities.
% ------------------

city(athens,greece,1368).
city(bangkok,thailand,1178).
city(barcelona,spain,1280).
city(berlin,east_germany,3481).
city(birmingham,united_kingdom,1112).
city(bombay,india,2839).
city(brussels,belgium,986).
city(bucharest,romania,1237).
city(budapest,hungary,1757).
city(buenos_aires,argentina,3404).
city(cairo,egypt,2373).
city(calcutta,india,2549).
city(canton,china,1496).
city(caracas,venezuela,488).
city(chicago,united_states,3621).
city(chungking,china,1100).
city(dairen,china,544).
city(delhi,india,1744).
city(detroit,united_states,1850).
city(glasgow,united_kingdom,1090).
city(hamburg,west_germany,1700).
city(harbin,china,760).
city(hongkong_city,hongkong,2440).
city(hyderabad,india,1086).
city(istanbul,turkey,1215).
city(jakarta,indonesia,533).
city(johannesburg,south_africa,880).
city(karachi,pakistan,1126).
city(kiev,soviet_union,991).
city(kobe,japan,765).
city(kowloon,china,547).
city(kyoto,japan,1204).
city(leningrad,soviet_union,2800).
city(lima,peru,835).
city(london,united_kingdom,8346).
city(los_angeles,united_states,1970).
city(madras,india,1416).
city(madrid,spain,1700).
city(manila,philippines,1025).
city(melbourne,australia,1595).
city(mexico_city,mexico,3796).
city(milan,italy,1269).
city(montreal,canada,1109).
city(moscow,soviet_union,4800).
city(mukden,china,1551).
city(nagoya,japan,1337).
city(nanking,japan,1020).
city(naples,italy,1012).
city(new_york,united_states,7795).
city(osaka,japan,2547).
city(paris,france,2850).
city(peking,china,2031).
city(philadelphia,united_states,2072).
city(pusan,south_korea,474).
city(rio_de_janeiro,brazil,2413).
city(rome,italy,1760).
city(saigon,vietnam,695).
city(santiago,chile,1350).
city(sao_paulo,brazil,2228).
city(seoul,south_korea,1446).
city(shanghai,china,5407).
city(sian,china,629).
city(singapore_city,singapore,1264).
city(sydney,australia,1898).
city(tehran,iran,1010).
city(tientsin,china,1795).
city(tokyo,japan,8535).
city(toronto,canada,668).
city(vienna,austria,1766).
city(warsaw,poland,965).
city(yokohama,japan,1143).



% Facts about Europe.
% ------------------

borders(albania,greece).
borders(albania,yugoslavia).
borders(albania,mediterranean).

borders(andorra,france).
borders(andorra,spain).

borders(austria,czechoslovakia).
borders(austria,hungary).
borders(austria,italy).
borders(austria,liechtenstein).
borders(austria,switzerland).
borders(austria,west_germany).
borders(austria,yugoslavia).

borders(belgium,france).
borders(belgium,luxembourg).
borders(belgium,netherlands).
borders(belgium,west_germany).
borders(belgium,atlantic).

borders(bulgaria,greece).
borders(bulgaria,romania).
borders(bulgaria,turkey).
borders(bulgaria,yugoslavia).
borders(bulgaria,black_sea).

borders(cyprus,mediterranean).

borders(czechoslovakia,austria).
borders(czechoslovakia,east_germany).
borders(czechoslovakia,hungary).
borders(czechoslovakia,poland).
borders(czechoslovakia,soviet_union).
borders(czechoslovakia,west_germany).

borders(denmark,west_germany).
borders(denmark,atlantic).
borders(denmark,baltic).

borders(eire,united_kingdom).
borders(eire,atlantic).

borders(finland,norway).
borders(finland,soviet_union).
borders(finland,sweden).
borders(finland,baltic).

borders(france,andorra).
borders(france,belgium).
borders(france,italy).
borders(france,luxembourg).
borders(france,monaco).
borders(france,spain).
borders(france,switzerland).
borders(france,west_germany).
borders(france,atlantic).
borders(france,mediterranean).

borders(east_germany,czechoslovakia).
borders(east_germany,poland).
borders(east_germany,west_germany).
borders(east_germany,baltic).

borders(greece,albania).
borders(greece,bulgaria).
borders(greece,turkey).
borders(greece,yugoslavia).
borders(greece,mediterranean).

borders(hungary,austria).
borders(hungary,czechoslovakia).
borders(hungary,romania).
borders(hungary,soviet_union).
borders(hungary,yugoslavia).

borders(iceland,atlantic).

borders(italy,austria).
borders(italy,france).
borders(italy,san_marino).
borders(italy,switzerland).
borders(italy,yugoslavia).
borders(italy,mediterranean).

borders(liechtenstein,austria).
borders(liechtenstein,switzerland).

borders(luxembourg,belgium).
borders(luxembourg,france).
borders(luxembourg,west_germany).

borders(malta,mediterranean).

borders(monaco,france).
borders(monaco,mediterranean).

borders(netherlands,belgium).
borders(netherlands,west_germany).
borders(netherlands,atlantic).

borders(norway,finland).
borders(norway,sweden).
borders(norway,soviet_union).
borders(norway,arctic_ocean).
borders(norway,atlantic).

borders(poland,czechoslovakia).
borders(poland,east_germany).
borders(poland,soviet_union).
borders(poland,baltic).

borders(portugal,spain).
borders(portugal,atlantic).

borders(romania,bulgaria).
borders(romania,hungary).
borders(romania,soviet_union).
borders(romania,yugoslavia).
borders(romania,black_sea).

borders(san_marino,italy).
borders(san_marino,mediterranean).

borders(spain,andorra).
borders(spain,france).
borders(spain,portugal).
borders(spain,atlantic).
borders(spain,mediterranean).

borders(sweden,finland).
borders(sweden,norway).
borders(sweden,atlantic).
borders(sweden,baltic).

borders(switzerland,austria).
borders(switzerland,france).
borders(switzerland,italy).
borders(switzerland,liechtenstein).
borders(switzerland,west_germany).

borders(west_germany,austria).
borders(west_germany,belgium).
borders(west_germany,czechoslovakia).
borders(west_germany,denmark).
borders(west_germany,east_germany).
borders(west_germany,france).
borders(west_germany,luxembourg).
borders(west_germany,netherlands).
borders(west_germany,switzerland).
borders(west_germany,atlantic).
borders(west_germany,baltic).

borders(united_kingdom,eire).
borders(united_kingdom,atlantic).

borders(yugoslavia,albania).
borders(yugoslavia,austria).
borders(yugoslavia,bulgaria).
borders(yugoslavia,greece).
borders(yugoslavia,hungary).
borders(yugoslavia,italy).
borders(yugoslavia,romania).
borders(yugoslavia,mediterranean).

% Facts about Asia.
% ----------------

borders(afghanistan,china).
borders(afghanistan,iran).
borders(afghanistan,pakistan).
borders(afghanistan,soviet_union).

borders(bahrain,persian_gulf).

borders(bangladesh,burma).
borders(bangladesh,india).
borders(bangladesh,indian_ocean).

borders(bhutan,china).
borders(bhutan,india).

borders(burma,bangladesh).
borders(burma,china).
borders(burma,india).
borders(burma,laos).
borders(burma,thailand).
borders(burma,indian_ocean).

borders(cambodia,laos).
borders(cambodia,thailand).
borders(cambodia,vietnam).
borders(cambodia,pacific).

borders(china,afghanistan).
borders(china,bhutan).
borders(china,burma).
borders(china,india).
borders(china,laos).
borders(china,mongolia).
borders(china,nepal).
borders(china,north_korea).
borders(china,pakistan).
borders(china,soviet_union).
borders(china,vietnam).
borders(china,pacific).

borders(india,bangladesh).
borders(india,bhutan).
borders(india,burma).
borders(india,china).
borders(india,nepal).
borders(india,pakistan).
borders(india,indian_ocean).

borders(indonesia,malaysia).
borders(indonesia,papua_new_guinea).
borders(indonesia,indian_ocean).
borders(indonesia,pacific).

borders(iran,afghanistan).
borders(iran,iraq).
borders(iran,pakistan).
borders(iran,soviet_union).
borders(iran,turkey).
borders(iran,caspian).
borders(iran,persian_gulf).
borders(iran,indian_ocean).

borders(iraq,iran).
borders(iraq,jordan).
borders(iraq,kuwait).
borders(iraq,saudi_arabia).
borders(iraq,syria).
borders(iraq,turkey).
borders(iraq,persian_gulf).

borders(israel,egypt).
borders(israel,jordan).
borders(laos,burma).
borders(laos,cambodia).
borders(laos,china).
borders(laos,thailand).
borders(laos,vietnam).

borders(israel,lebanon).
borders(israel,syria).
borders(israel,mediterranean).
borders(israel,red_sea).

borders(japan,pacific).

borders(jordan,iraq).
borders(jordan,israel).
borders(jordan,saudi_arabia).
borders(jordan,syria).
borders(jordan,red_sea).

borders(kuwait,iraq).
borders(kuwait,saudi_arabia).
borders(kuwait,persian_gulf).

borders(lebanon,israel).
borders(lebanon,syria).
borders(lebanon,mediterranean).

borders(malaysia,indonesia).
borders(malaysia,singapore).
borders(malaysia,thailand).
borders(malaysia,indian_ocean).
borders(malaysia,pacific).

borders(maldives,indian_ocean).

borders(mongolia,china).
borders(mongolia,soviet_union).

borders(nepal,china).
borders(nepal,india).

borders(north_korea,china).
borders(north_korea,south_korea).
borders(north_korea,soviet_union).
borders(north_korea,pacific).

borders(oman,saudi_arabia).
borders(oman,united_arab_emirates).
borders(oman,south_yemen).
borders(oman,indian_ocean).

borders(pakistan,afghanistan).
borders(pakistan,china).
borders(pakistan,india).
borders(pakistan,iran).
borders(pakistan,indian_ocean).

borders(philippines,pacific).

borders(qatar,saudi_arabia).
borders(qatar,united_arab_emirates).
borders(qatar,persian_gulf).

borders(saudi_arabia,iraq).
borders(saudi_arabia,jordan).
borders(saudi_arabia,kuwait).
borders(saudi_arabia,oman).
borders(saudi_arabia,qatar).
borders(saudi_arabia,south_yemen).
borders(saudi_arabia,united_arab_emirates).
borders(saudi_arabia,yemen).
borders(saudi_arabia,persian_gulf).
borders(saudi_arabia,red_sea).

borders(singapore,malaysia).
borders(singapore,pacific).

borders(south_korea,north_korea).
borders(south_korea,pacific).

borders(south_yemen,oman).
borders(south_yemen,saudi_arabia).
borders(south_yemen,yemen).
borders(south_yemen,indian_ocean).

borders(soviet_union,afghanistan).
borders(soviet_union,china).
borders(soviet_union,czechoslovakia).
borders(soviet_union,finland).
borders(soviet_union,hungary).
borders(soviet_union,iran).
borders(soviet_union,mongolia).
borders(soviet_union,north_korea).
borders(soviet_union,norway).
borders(soviet_union,poland).
borders(soviet_union,romania).
borders(soviet_union,turkey).
borders(soviet_union,arctic_ocean).
borders(soviet_union,baltic).
borders(soviet_union,black_sea).
borders(soviet_union,caspian).
borders(soviet_union,pacific).

borders(sri_lanka,indian_ocean).

borders(syria,iraq).
borders(syria,israel).
borders(syria,jordan).
borders(syria,lebanon).
borders(syria,turkey).
borders(syria,mediterranean).

borders(taiwan,pacific).

borders(thailand,burma).
borders(thailand,cambodia).
borders(thailand,laos).
borders(thailand,malaysia).
borders(thailand,indian_ocean).
borders(thailand,pacific).

borders(turkey,bulgaria).
borders(turkey,greece).
borders(turkey,iran).
borders(turkey,iraq).
borders(turkey,soviet_union).
borders(turkey,syria).
borders(turkey,black_sea).
borders(turkey,mediterranean).

borders(united_arab_emirates,oman).
borders(united_arab_emirates,qatar).
borders(united_arab_emirates,saudi_arabia).
borders(united_arab_emirates,persian_gulf).

borders(vietnam,cambodia).
borders(vietnam,china).
borders(vietnam,laos).
borders(vietnam,pacific).

borders(yemen,south_yemen).
borders(yemen,saudi_arabia).
borders(yemen,red_sea).

% Facts about Africa.
% ------------------

borders(algeria,libya).
borders(algeria,mali).
borders(algeria,mauritania).
borders(algeria,morocco).
borders(algeria,niger).
borders(algeria,tunisia).
borders(algeria,mediterranean).

borders(angola,congo).
borders(angola,south_africa).
borders(angola,zaire).
borders(angola,zambia).
borders(angola,atlantic).

borders(botswana,south_africa).
borders(botswana,zimbabwe).

borders(burundi,rwanda).
borders(burundi,tanzania).
borders(burundi,zaire).

borders(cameroon,central_african_republic).
borders(cameroon,chad).
borders(cameroon,congo).
borders(cameroon,equatorial_guinea).
borders(cameroon,gabon).
borders(cameroon,nigeria).
borders(cameroon,atlantic).

borders(central_african_republic,cameroon).
borders(central_african_republic,chad).
borders(central_african_republic,congo).
borders(central_african_republic,sudan).
borders(central_african_republic,zaire).

borders(chad,cameroon).
borders(chad,central_african_republic).
borders(chad,libya).
borders(chad,niger).
borders(chad,nigeria).
borders(chad,sudan).

borders(congo,angola).
borders(congo,cameroon).
borders(congo,central_african_republic).
borders(congo,gabon).
borders(congo,zaire).
borders(congo,atlantic).

borders(dahomey,niger).
borders(dahomey,nigeria).
borders(dahomey,togo).
borders(dahomey,upper_volta).
borders(dahomey,atlantic).

borders(djibouti,ethiopia).
borders(djibouti,somalia).
borders(djibouti,indian_ocean).

borders(egypt,israel).
borders(egypt,libya).
borders(egypt,sudan).
borders(egypt,mediterranean).
borders(egypt,red_sea).

borders(equatorial_guinea,cameroon).
borders(equatorial_guinea,gabon).
borders(equatorial_guinea,atlantic).

borders(ethiopia,djibouti).
borders(ethiopia,kenya).
borders(ethiopia,somalia).
borders(ethiopia,sudan).
borders(ethiopia,red_sea).

borders(gabon,cameroon).
borders(gabon,congo).
borders(gabon,equatorial_guinea).
borders(gabon,atlantic).

borders(gambia,senegal).
borders(gambia,atlantic).

borders(ghana,ivory_coast).
borders(ghana,togo).
borders(ghana,upper_volta).
borders(ghana,atlantic).

borders(guinea,guinea_bissau).
borders(guinea,ivory_coast).
borders(guinea,liberia).
borders(guinea,mali).
borders(guinea,senegal).
borders(guinea,sierra_leone).
borders(guinea,atlantic).

borders(guinea_bissau,guinea).
borders(guinea_bissau,senegal).
borders(guinea_bissau,atlantic).

borders(ivory_coast,ghana).
borders(ivory_coast,guinea).
borders(ivory_coast,liberia).
borders(ivory_coast,mali).
borders(ivory_coast,upper_volta).
borders(ivory_coast,atlantic).

borders(kenya,ethiopia).
borders(kenya,somalia).
borders(kenya,sudan).
borders(kenya,tanzania).
borders(kenya,uganda).
borders(kenya,indian_ocean).

borders(lesotho,south_africa).

borders(liberia,ivory_coast).
borders(liberia,guinea).
borders(liberia,sierra_leone).
borders(liberia,atlantic).

borders(libya,algeria).
borders(libya,chad).
borders(libya,egypt).
borders(libya,niger).
borders(libya,sudan).
borders(libya,tunisia).
borders(libya,mediterranean).

borders(malagasy,indian_ocean).

borders(malawi,mozambique).
borders(malawi,tanzania).
borders(malawi,zambia).

borders(mali,algeria).
borders(mali,guinea).
borders(mali,ivory_coast).
borders(mali,mauritania).
borders(mali,niger).
borders(mali,senegal).
borders(mali,upper_volta).

borders(mauritania,algeria).
borders(mauritania,mali).
borders(mauritania,morocco).
borders(mauritania,senegal).
borders(mauritania,atlantic).

borders(mauritius,indian_ocean).

borders(morocco,algeria).
borders(morocco,mauritania).
borders(morocco,atlantic).
borders(morocco,mediterranean).

borders(mozambique,malawi).
borders(mozambique,south_africa).
borders(mozambique,swaziland).
borders(mozambique,tanzania).
borders(mozambique,zambia).
borders(mozambique,zimbabwe).
borders(mozambique,indian_ocean).

borders(niger,algeria).
borders(niger,chad).
borders(niger,dahomey).
borders(niger,libya).
borders(niger,mali).
borders(niger,nigeria).
borders(niger,upper_volta).

borders(nigeria,cameroon).
borders(nigeria,chad).
borders(nigeria,dahomey).
borders(nigeria,niger).
borders(nigeria,atlantic).

borders(rwanda,burundi).
borders(rwanda,tanzania).
borders(rwanda,uganda).
borders(rwanda,zaire).

borders(senegal,gambia).
borders(senegal,guinea).
borders(senegal,guinea_bissau).
borders(senegal,mali).
borders(senegal,mauritania).
borders(senegal,atlantic).

borders(seychelles,indian_ocean).

borders(sierra_leone,guinea).
borders(sierra_leone,liberia).
borders(sierra_leone,atlantic).

borders(somalia,djibouti).
borders(somalia,ethiopia).
borders(somalia,kenya).
borders(somalia,indian_ocean).

borders(south_africa,angola).
borders(south_africa,botswana).
borders(south_africa,lesotho).
borders(south_africa,mozambique).
borders(south_africa,swaziland).
borders(south_africa,zambia).
borders(south_africa,zimbabwe).
borders(south_africa,atlantic).
borders(south_africa,indian_ocean).

borders(sudan,chad).
borders(sudan,central_african_republic).
borders(sudan,egypt).
borders(sudan,ethiopia).
borders(sudan,kenya).
borders(sudan,libya).
borders(sudan,uganda).
borders(sudan,zaire).
borders(sudan,red_sea).

borders(swaziland,mozambique).
borders(swaziland,south_africa).

borders(tanzania,burundi).
borders(tanzania,kenya).
borders(tanzania,malawi).
borders(tanzania,mozambique).
borders(tanzania,rwanda).
borders(tanzania,uganda).
borders(tanzania,zaire).
borders(tanzania,zambia).
borders(tanzania,indian_ocean).

borders(togo,dahomey).
borders(togo,ghana).
borders(togo,upper_volta).
borders(togo,atlantic).

borders(tunisia,algeria).
borders(tunisia,libya).
borders(tunisia,mediterranean).

borders(uganda,kenya).
borders(uganda,rwanda).
borders(uganda,sudan).
borders(uganda,tanzania).
borders(uganda,zaire).

borders(upper_volta,dahomey).
borders(upper_volta,ghana).
borders(upper_volta,ivory_coast).
borders(upper_volta,mali).
borders(upper_volta,niger).
borders(upper_volta,togo).

borders(zaire,angola).
borders(zaire,burundi).
borders(zaire,central_african_republic).
borders(zaire,congo).
borders(zaire,rwanda).
borders(zaire,sudan).
borders(zaire,tanzania).
borders(zaire,uganda).
borders(zaire,zambia).
borders(zaire,atlantic).

borders(zambia,angola).
borders(zambia,malawi).
borders(zambia,mozambique).
borders(zambia,south_africa).
borders(zambia,tanzania).
borders(zambia,zaire).
borders(zambia,zimbabwe).

borders(zimbabwe,botswana).
borders(zimbabwe,mozambique).
borders(zimbabwe,south_africa).
borders(zimbabwe,zambia).


% Facts about America.
% -------------------

borders(argentina,bolivia).
borders(argentina,brazil).
borders(argentina,chile).
borders(argentina,paraguay).
borders(argentina,uruguay).
borders(argentina,atlantic).

borders(bahamas,atlantic).

borders(barbados,atlantic).

borders(belize,guatemala).
borders(belize,mexico).
borders(belize,atlantic).

borders(bolivia,argentina).
borders(bolivia,brazil).
borders(bolivia,chile).
borders(bolivia,paraguay).
borders(bolivia,peru).

borders(brazil,argentina).
borders(brazil,bolivia).
borders(brazil,colombia).
borders(brazil,french_guiana).
borders(brazil,guyana).
borders(brazil,paraguay).
borders(brazil,peru).
borders(brazil,surinam).
borders(brazil,uruguay).
borders(brazil,venezuela).
borders(brazil,atlantic).

borders(canada,united_states).
borders(canada,arctic_ocean).
borders(canada,atlantic).
borders(canada,pacific).

borders(chile,argentina).
borders(chile,bolivia).
borders(chile,peru).
borders(chile,pacific).

borders(colombia,brazil).
borders(colombia,ecuador).
borders(colombia,panama).
borders(colombia,peru).
borders(colombia,venezuela).
borders(colombia,atlantic).
borders(colombia,pacific).

borders(costa_rica,nicaragua).
borders(costa_rica,panama).
borders(costa_rica,atlantic).
borders(costa_rica,pacific).

borders(cuba,atlantic).

borders(dominican_republic,haiti).
borders(dominican_republic,atlantic).

borders(ecuador,colombia).
borders(ecuador,peru).
borders(ecuador,pacific).

borders(el_salvador,guatemala).
borders(el_salvador,honduras).
borders(el_salvador,pacific).

borders(french_guiana,brazil).
borders(french_guiana,surinam).

borders(greenland,arctic_ocean).
borders(greenland,atlantic).

borders(grenada,atlantic).

borders(guatemala,belize).
borders(guatemala,el_salvador).
borders(guatemala,honduras).
borders(guatemala,mexico).
borders(guatemala,atlantic).
borders(guatemala,pacific).

borders(guyana,brazil).
borders(guyana,surinam).
borders(guyana,venezuela).
borders(guyana,atlantic).

borders(haiti,dominican_republic).
borders(haiti,atlantic).

borders(honduras,el_salvador).
borders(honduras,guatemala).
borders(honduras,nicaragua).
borders(honduras,atlantic).
borders(honduras,pacific).

borders(jamaica,atlantic).

borders(mexico,belize).
borders(mexico,guatemala).
borders(mexico,united_states).
borders(mexico,atlantic).
borders(mexico,pacific).

borders(nicaragua,costa_rica).
borders(nicaragua,honduras).
borders(nicaragua,atlantic).
borders(nicaragua,pacific).

borders(panama,colombia).
borders(panama,costa_rica).
borders(panama,atlantic).
borders(panama,pacific).

borders(paraguay,argentina).
borders(paraguay,bolivia).
borders(paraguay,brazil).

borders(peru,bolivia).
borders(peru,brazil).
borders(peru,chile).
borders(peru,colombia).
borders(peru,ecuador).
borders(peru,pacific).

borders(surinam,brazil).
borders(surinam,french_guiana).
borders(surinam,guyana).

borders(trinidad_and_tobago,atlantic).

borders(united_states,canada).
borders(united_states,mexico).
borders(united_states,arctic_ocean).
borders(united_states,atlantic).
borders(united_states,pacific).

borders(uruguay,argentina).
borders(uruguay,brazil).
borders(uruguay,atlantic).

borders(venezuela,brazil).
borders(venezuela,colombia).
borders(venezuela,guyana).
borders(venezuela,atlantic).

% Facts about Australasia.
% -----------------------

borders(australia,indian_ocean).
borders(australia,pacific).

borders(fiji,pacific).

borders(new_zealand,pacific).

borders(papua_new_guinea,indonesia).
borders(papua_new_guinea,pacific).

borders(tonga,pacific).

borders(western_samoa,pacific).

borders(antarctica,southern_ocean).

% Facts about oceans and seas.
% ---------------------------

borders(arctic_ocean,atlantic).
borders(arctic_ocean,pacific).

borders(atlantic,arctic_ocean).
borders(atlantic,indian_ocean).
borders(atlantic,pacific).
borders(atlantic,southern_ocean).
borders(atlantic,baltic).
borders(atlantic,mediterranean).

borders(indian_ocean,atlantic).
borders(indian_ocean,pacific).
borders(indian_ocean,southern_ocean).
borders(indian_ocean,persian_gulf).
borders(indian_ocean,red_sea).

borders(pacific,arctic_ocean).
borders(pacific,atlantic).
borders(pacific,indian_ocean).
borders(pacific,southern_ocean).

borders(southern_ocean,atlantic).
borders(southern_ocean,indian_ocean).
borders(southern_ocean,pacific).

borders(baltic,atlantic).

borders(black_sea,mediterranean).

borders(mediterranean,atlantic).
borders(mediterranean,black_sea).

borders(persian_gulf,indian_ocean).

borders(red_sea,indian_ocean).

% Countries bordering each ocean and sea.
% --------------------------------------

borders(arctic_ocean,norway).
borders(arctic_ocean,soviet_union).
borders(arctic_ocean,canada).
borders(arctic_ocean,greenland).
borders(arctic_ocean,united_states).

borders(atlantic,belgium).
borders(atlantic,denmark).
borders(atlantic,eire).
borders(atlantic,france).
borders(atlantic,iceland).
borders(atlantic,netherlands).
borders(atlantic,norway).
borders(atlantic,portugal).
borders(atlantic,spain).
borders(atlantic,sweden).
borders(atlantic,west_germany).
borders(atlantic,united_kingdom).
borders(atlantic,angola).
borders(atlantic,cameroon).
borders(atlantic,congo).
borders(atlantic,dahomey).
borders(atlantic,equatorial_guinea).
borders(atlantic,gabon).
borders(atlantic,gambia).
borders(atlantic,ghana).
borders(atlantic,guinea).
borders(atlantic,guinea_bissau).
borders(atlantic,ivory_coast).
borders(atlantic,liberia).
borders(atlantic,mauritania).
borders(atlantic,morocco).
borders(atlantic,nigeria).
borders(atlantic,senegal).
borders(atlantic,sierra_leone).
borders(atlantic,south_africa).
borders(atlantic,togo).
borders(atlantic,zaire).
borders(atlantic,argentina).
borders(atlantic,bahamas).
borders(atlantic,barbados).
borders(atlantic,belize).
borders(atlantic,brazil).
borders(atlantic,canada).
borders(atlantic,colombia).
borders(atlantic,costa_rica).
borders(atlantic,cuba).
borders(atlantic,dominican_republic).
borders(atlantic,french_guiana).
borders(atlantic,greenland).
borders(atlantic,grenada).
borders(atlantic,guatemala).
borders(atlantic,guyana).
borders(atlantic,haiti).
borders(atlantic,honduras).
borders(atlantic,jamaica).
borders(atlantic,mexico).
borders(atlantic,nicaragua).
borders(atlantic,panama).
borders(atlantic,surinam).
borders(atlantic,trinidad_and_tobago).
borders(atlantic,united_states).
borders(atlantic,uruguay).
borders(atlantic,venezuela).

borders(indian_ocean,bangladesh).
borders(indian_ocean,burma).
borders(indian_ocean,india).
borders(indian_ocean,indonesia).
borders(indian_ocean,iran).
borders(indian_ocean,malaysia).
borders(indian_ocean,maldives).
borders(indian_ocean,oman).
borders(indian_ocean,pakistan).
borders(indian_ocean,south_yemen).
borders(indian_ocean,sri_lanka).
borders(indian_ocean,thailand).
borders(indian_ocean,djibouti).
borders(indian_ocean,kenya).
borders(indian_ocean,malagasy).
borders(indian_ocean,mauritius).
borders(indian_ocean,mozambique).
borders(indian_ocean,seychelles).
borders(indian_ocean,somalia).
borders(indian_ocean,south_africa).
borders(indian_ocean,tanzania).
borders(indian_ocean,australia).

borders(pacific,cambodia).
borders(pacific,china).
borders(pacific,indonesia).
borders(pacific,japan).
borders(pacific,malaysia).
borders(pacific,north_korea).
borders(pacific,philippines).
borders(pacific,singapore).
borders(pacific,south_korea).
borders(pacific,soviet_union).
borders(pacific,taiwan).
borders(pacific,thailand).
borders(pacific,vietnam).
borders(pacific,canada).
borders(pacific,chile).
borders(pacific,colombia).
borders(pacific,costa_rica).
borders(pacific,ecuador).
borders(pacific,el_salvador).
borders(pacific,guatemala).
borders(pacific,honduras).
borders(pacific,mexico).
borders(pacific,nicaragua).
borders(pacific,panama).
borders(pacific,peru).
borders(pacific,united_states).
borders(pacific,australia).
borders(pacific,fiji).
borders(pacific,new_zealand).
borders(pacific,papua_new_guinea).
borders(pacific,tonga).
borders(pacific,western_samoa).

borders(southern_ocean,antarctica).

borders(baltic,denmark).
borders(baltic,finland).
borders(baltic,east_germany).
borders(baltic,poland).
borders(baltic,sweden).
borders(baltic,west_germany).
borders(baltic,soviet_union).

borders(black_sea,bulgaria).
borders(black_sea,romania).
borders(black_sea,soviet_union).
borders(black_sea,turkey).

borders(caspian,iran).
borders(caspian,soviet_union).

borders(mediterranean,albania).
borders(mediterranean,cyprus).
borders(mediterranean,france).
borders(mediterranean,greece).
borders(mediterranean,italy).
borders(mediterranean,malta).
borders(mediterranean,monaco).
borders(mediterranean,san_marino).
borders(mediterranean,spain).
borders(mediterranean,yugoslavia).
borders(mediterranean,israel).
borders(mediterranean,lebanon).
borders(mediterranean,syria).
borders(mediterranean,turkey).
borders(mediterranean,algeria).
borders(mediterranean,egypt).
borders(mediterranean,libya).
borders(mediterranean,morocco).
borders(mediterranean,tunisia).

borders(persian_gulf,bahrain).
borders(persian_gulf,iran).
borders(persian_gulf,iraq).
borders(persian_gulf,kuwait).
borders(persian_gulf,qatar).
borders(persian_gulf,saudi_arabia).
borders(persian_gulf,united_arab_emirates).

borders(red_sea,israel).
borders(red_sea,jordan).
borders(red_sea,saudi_arabia).
borders(red_sea,yemen).
borders(red_sea,egypt).
borders(red_sea,ethiopia).
borders(red_sea,sudan).

/*

 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

% Facts about rivers.
% ------------------

river_pathlist(amazon,[atlantic,brazil,peru]).
river_pathlist(amu_darya,[aral_sea,soviet_union,afghanistan]).
river_pathlist(amur,[pacific,soviet_union,china,mongolia]).
river_pathlist(brahmaputra,[indian_ocean,bangladesh,china]).
river_pathlist(colorado,[pacific,mexico,united_states]).
river_pathlist(congo_river,[atlantic,zaire,zambia]).
river_pathlist(cubango,[botswana,south_africa,angola]).
river_pathlist(danube,[black_sea,romania,yugoslavia,hungary,czechoslovakia,austria,
              west_germany]).
river_pathlist(don,[black_sea,soviet_union]).
river_pathlist(elbe,[atlantic,west_germany,east_germany,czechoslovakia]).
river_pathlist(euphrates,[persian_gulf,iraq,syria,turkey]).
river_pathlist(ganges,[indian_ocean,india,china]).
river_pathlist(hwang_ho,[pacific,china]).
river_pathlist(indus,[indian_ocean,pakistan,india,china]).
river_pathlist(irrawaddy,[indian_ocean,burma]).
river_pathlist(lena,[arctic_ocean,soviet_union]).
river_pathlist(limpopo,[indian_ocean,mozambique,south_africa]).
river_pathlist(mackenzie,[arctic_ocean,canada]).
river_pathlist(mekong,[pacific,vietnam,cambodia,laos,china]).
river_pathlist(mississippi,[atlantic,united_states]).
river_pathlist(murray,[indian_ocean,australia]).
river_pathlist(niger_river,[atlantic,nigeria,niger,mali,guinea]).
river_pathlist(nile,[mediterranean,egypt,sudan,uganda]).
river_pathlist(ob,[arctic_ocean,soviet_union]).
river_pathlist(oder,[baltic,poland,czechoslovakia]).
river_pathlist(orange,[atlantic,south_africa,lesotho]).
river_pathlist(orinoco,[atlantic,venezuela,colombia]).
river_pathlist(parana,[atlantic,argentina,paraguay,brazil]).
river_pathlist(rhine,[atlantic,netherlands,west_germany,switzerland]).
river_pathlist(rhone,[mediterranean,france,switzerland]).
river_pathlist(rio_grande,[atlantic,mexico,united_states]).
river_pathlist(salween,[indian_ocean,burma,china]).
river_pathlist(senegal_river,[atlantic,senegal,mali,guinea]).
river_pathlist(tagus,[atlantic,portugal,spain]).
river_pathlist(vistula,[baltic,poland]).
river_pathlist(volga,[black_sea,soviet_union]).
river_pathlist(volta,[atlantic,ghana,upper_volta]).
river_pathlist(yangtze,[pacific,china]).
river_pathlist(yenisei,[arctic_ocean,soviet_union,mongolia]).
river_pathlist(yukon,[pacific,united_states,canada]).
river_pathlist(zambesi,[indian_ocean,mozambique,zambia,angola]).


/*
 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

:- style_check(-discontiguous).
:- style_check(-singleton).

% NDTABL - Meta-information about database relations.

nd(african,19,26).
nd(american,19,26).
nd(area,51,51).
nd(area,22,22,51).
nd(asian,21,26).
nd(aggregate80,103,3,100,51).
nd(one_of,99,200,-99).
nd(ratio,99,51,51,3).
nd(card,99,100,3).
nd(borders,29,22,22).
nd(capital,22,22).
nd(capital,22,22,23).
nd(city,18,18).
nd(continent,8,8).
nd(country,22,22).
nd(drains,16,16,10).
nd(eastof,40,22,22).
nd(european,19,26).
nd(exceeds,99,51,51).
nd(flows,19,16,22).
nd(flows,19,16,22,22).
nd(in,29,26,15).
nd(latitude80,23,23).
nd(latitude80,22,22,23).
nd(longitude80,26,26).
nd(longitude80,22,22,26).
nd(northof,40,22,22).
nd(ocean,7,7).
nd(population,51,51).
nd(population,23,23,51).
nd(region80,12,12).
nd(rises,16,16,22).
nd(river_pathlist,16,16).
nd(sea,8,8).
nd(place,23,23).
nd(seamass,10,10).
nd(southof,40,22,22).
nd(westof,40,22,22).
nd(=<,99,51,51).
nd(<,99,51,51).
nd(>,99,51,51).
nd(>=,99,51,51).





