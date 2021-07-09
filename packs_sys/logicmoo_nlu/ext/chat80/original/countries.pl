
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


% Facts about countries.
% ---------------------
specific_pred(spatial,nation_capital,C,Cap) :- c_r_l_l_s_cap_m(C,_,_,_,_,_,Cap,_).
position_pred(spatial,latitude,C,L--degrees) :- c_r_l_l_s_cap_m(C,_,L,_,_,_,_,_).
position_pred(spatial,longitude,C,L--degrees) :- c_r_l_l_s_cap_m(C,_,_,L,_,_,_,_).
count_pred(spatial,population/*citizens*/,C,P--million) :- c_r_l_l_s_cap_m(C,_,_,_,_,P0,_,_), P is integer(P0/1.0E6).
measure_pred(spatial,area,C,A--ksqmiles) :- c_r_l_l_s_cap_m(C,_,_,_,A0,_,_,_), A is A0/1000.

measure_pred(Spatial,Area,Where,Total) :- \+ c_r_l_l_s_cap_m(Where,_,_,_,_,_,_,_), 
 % ti(continent,Where),
 setof(Value:[Country],
               []^(database80(measure_pred(Spatial, Area, Country, Value)), 
               %database80(ti(country, Country)), 
               database80(trans_pred(Spatial,contain,Where,Country))),
               Setof),
         database80(aggregate80(total, Setof, Total)).


% c_r_l_l_s_cap_m(Country,Region,Latitude,Longitude,
%         Area/1000,Area mod 1000,
%         Population/1000000,Population mod 1000000 / 1000,
%         Capital,Currency)


c_r_l_l_s_cap_m(afghanistan,indian_subcontinent,33,-65,254861,18290000,kabul,afghani).
c_r_l_l_s_cap_m(albania,southern_europe,41,-20,11100,2350000,tirana,lek).
c_r_l_l_s_cap_m(algeria,north_africa,35,-11,919951,15770000,algiers,dinar).
c_r_l_l_s_cap_m(andorra,southern_europe,42,-1,179,25000,andorra_la_villa,franc_peseta).
c_r_l_l_s_cap_m(angola,southern_africa,-12,-18,481351,5810000,luanda,?).
c_r_l_l_s_cap_m(argentina,south_america,-35,66,1072067,23920000,buenos_aires,peso).
c_r_l_l_s_cap_m(australia,oceania,-23,-135,2967909,13268000,canberra,australian_dollar).
c_r_l_l_s_cap_m(austria,western_europe,47,-14,32374,7520000,vienna,schilling).
c_r_l_l_s_cap_m(bahamas,caribbean,24,74,4404,190000,nassau,bahamian_dollar).
c_r_l_l_s_cap_m(bahrain,middle_east,26,-50,231,230000,manama,dinar).
c_r_l_l_s_cap_m(bangladesh,indian_subcontinent,24,-90,55126,71317000,dacca,taka).
c_r_l_l_s_cap_m(barbados,caribbean,13,59,166,240000,bridgetown,east_carribean_dollar).
c_r_l_l_s_cap_m(belgium,western_europe,51,-5,11779,9711000,brussels,franc).
c_r_l_l_s_cap_m(belize,central_america,17,88,8866,82000,belize_town,?).
c_r_l_l_s_cap_m(bhutan,indian_subcontinent,27,-90,19305,1150000,thimphu,indian_rupee).
c_r_l_l_s_cap_m(bolivia,south_america,-17,64,424162,5330000,sucre,peso).
c_r_l_l_s_cap_m(botswana,southern_africa,-22,-24,219815,650000,gaborone,south_african_rand).
c_r_l_l_s_cap_m(brazil,south_america,-13,53,3286470,105137000,brasilia,cruzeiro).
c_r_l_l_s_cap_m(bulgaria,eastern_europe,43,-25,42829,8620000,sofia,lev).
c_r_l_l_s_cap_m(burma,southeast_east,21,-96,261789,29560000,rangoon,kyat).
c_r_l_l_s_cap_m(burundi,central_africa,-3,-30,10739,3600000,bujumbura,franc).
c_r_l_l_s_cap_m(cambodia,southeast_east,12,-105,69898,7640000,phnom_penh,riel).
c_r_l_l_s_cap_m(cameroon,west_africa,3,-12,183568,6170000,yaounde,cfa_franc).
c_r_l_l_s_cap_m(canada,north_america,60,100,3851809,22047000,ottawa,canadian_dollar).
c_r_l_l_s_cap_m(central_african_republic,central_africa,7,-20,241313,1720000,bangui,cfa_franc).
c_r_l_l_s_cap_m(chad,central_africa,12,-17,495752,3870000,n_djamena,cfa_franc).
c_r_l_l_s_cap_m(chile,south_america,-35,71,286396,10230000,santiago,escudo).
c_r_l_l_s_cap_m(china,far_east,30,-110,3691502,840000000.000000,peking,yuan).
c_r_l_l_s_cap_m(colombia,south_america,4,73,455335,23210000,bogota,peso).
c_r_l_l_s_cap_m(congo,central_africa,-1,-16,132046,1001000,brazzaville,cfa_franc).
c_r_l_l_s_cap_m(costa_rica,central_america,10,84,19653,1890000,san_jose,colon).
c_r_l_l_s_cap_m(cuba,caribbean,22,79,44218,8870000,havana,peso).
c_r_l_l_s_cap_m(cyprus,southern_europe,35,-33,3572,660000,nicosia,pound).
c_r_l_l_s_cap_m(czechoslovakia,eastern_europe,49,-17,49371,14580000,prague,koruna).
c_r_l_l_s_cap_m(dahomey,west_africa,8,-2,43483,2910000,porto_novo,cfa_franc).
c_r_l_l_s_cap_m(denmark,scandinavia,55,-9,16615,5130000,copenhagen,krone).
c_r_l_l_s_cap_m(djibouti,east_africa,12,-42,9071,45000,djibouti_city,?).
c_r_l_l_s_cap_m(dominican_republic,caribbean,19,70,18704,4430000,santa_domingo,peso).
c_r_l_l_s_cap_m(east_germany,eastern_europe,52,-12,40646,16980000,east_berlin,ddr_mark).
c_r_l_l_s_cap_m(ecuador,south_america,-2,78,105685,6730000,quito,sucre).
c_r_l_l_s_cap_m(egypt,north_africa,28,-31,386872,35620000,cairo,egyptian_pound).
c_r_l_l_s_cap_m(eire,western_europe,53,8,26600,3030000,dublin,irish_pound).
c_r_l_l_s_cap_m(el_salvador,central_america,14,89,8260,3860000,san_salvador,colon).
c_r_l_l_s_cap_m(equatorial_guinea,west_africa,1,-10,10832,300000,santa_isabel,peveta).
c_r_l_l_s_cap_m(ethiopia,east_africa,8,-40,457142,26080000,addis_ababa,ethiopean_dollar).
c_r_l_l_s_cap_m(fiji,oceania,-17,-179,7055,550000,suva,fiji_dollar).
c_r_l_l_s_cap_m(finland,scandinavia,65,-27,130119,4660000,helsinki,markka).
c_r_l_l_s_cap_m(france,western_europe,47,-3,212973,52350000,paris,franc).
c_r_l_l_s_cap_m(french_guiana,south_america,4,53,34740,27000,cayenne,?).
c_r_l_l_s_cap_m(gabon,central_africa,0,-10,102317,520000,libreville,cfa_franc).
c_r_l_l_s_cap_m(gambia,west_africa,13,16,4003,490000,banjul,dalasi).
c_r_l_l_s_cap_m(ghana,west_africa,6,1,92100,9360000,accra,cedi).
c_r_l_l_s_cap_m(greece,southern_europe,40,-23,50547,9030000,athens,drachma).
c_r_l_l_s_cap_m(grenada,caribbean,12,61,133,100000,st_georges,east_caribbean_dollar).
c_r_l_l_s_cap_m(guatemala,central_america,15,90,42042,5540000,guatamala_city,quetzal).
c_r_l_l_s_cap_m(guinea,west_africa,10,10,94925,4210000,conakry,syli).
c_r_l_l_s_cap_m(guinea_bissau,west_africa,12,15,13948,510000,bissau,pataca).
c_r_l_l_s_cap_m(guyana,south_america,5,59,83000 ,760000,georgetown,guyana_dollar).
c_r_l_l_s_cap_m(haiti,caribbean,19,72,10714,5200000,port_au_prince,gourde).
c_r_l_l_s_cap_m(honduras,central_america,14,86,43277,2780000,tegucigalpa,lempira).
c_r_l_l_s_cap_m(hungary,eastern_europe,47,-19,35919,10410000,budapest,forint).
c_r_l_l_s_cap_m(iceland,western_europe,65,19,39702,210000,reykjavik,krona).
c_r_l_l_s_cap_m(india,indian_subcontinent,20,-80,1229919,574219776.000000,new_delhi,rupee).
c_r_l_l_s_cap_m(indonesia,southeast_east,-5,-115,735268,124600000,jakarta,rupiah).
c_r_l_l_s_cap_m(iran,middle_east,33,-53,636363,32001000,tehran,rial).
c_r_l_l_s_cap_m(iraq,middle_east,33,-44,167567,10410000,baghdad,dinar).
c_r_l_l_s_cap_m(israel,middle_east,32,-35,34493,3228000,jerusalem,israeli_pound).
c_r_l_l_s_cap_m(italy,southern_europe,42,-13,116303,55262000,rome,lira).
c_r_l_l_s_cap_m(ivory_coast,west_africa,7,5,124503,4640000,abidjan,cfa_franc).
c_r_l_l_s_cap_m(jamaica,caribbean,18,77,4411,1980000,kingston,jamaican_dollar).
c_r_l_l_s_cap_m(japan,far_east,36,-136,143574,108710000,tokyo,yen).
c_r_l_l_s_cap_m(jordan,middle_east,31,-36,32297,2560000,amman,dinar).
c_r_l_l_s_cap_m(kenya,east_africa,1,-38,224960,12480000,nairobi,kenya_shilling).
c_r_l_l_s_cap_m(kuwait,middle_east,29,-47,7780,880000,kuwait_city,kuwaiti_dinar).
c_r_l_l_s_cap_m(laos,southeast_east,18,-105,3180,3180000,vientiane,kip).
c_r_l_l_s_cap_m(lebanon,middle_east,34,-36,4015,3213000,beirut,lebanese_pound).
c_r_l_l_s_cap_m(lesotho,southern_africa,-30,-28,11716,1200000,masero,rand).
c_r_l_l_s_cap_m(liberia,west_africa,6,9,43000 ,1660000,monrovia,us_dollar).
c_r_l_l_s_cap_m(libya,north_africa,28,-17,679536,2257000,tripoli,libyan_dinar).
c_r_l_l_s_cap_m(liechtenstein,western_europe,47,-9,62,23000,vaduz,swiss_franc).
c_r_l_l_s_cap_m(luxembourg,western_europe,50,-6,999,350000,luxembourg_city,luxembourg_franc).
c_r_l_l_s_cap_m(malagasy,southern_africa,-20,-47,203035,7655000,tananarive,ariary).
c_r_l_l_s_cap_m(malawi,southern_africa,-13,-34,45747,4790000,zomba,kwacha).
c_r_l_l_s_cap_m(malaysia,southeast_east,5,-110,128328,10920000,kuala_lumpa,malaysian_dollar).
c_r_l_l_s_cap_m(maldives,indian_subcontinent,2,-73,115,123000,male,rupee).
c_r_l_l_s_cap_m(mali,west_africa,15,10,464873,5380000,bamako,mali_franc).
c_r_l_l_s_cap_m(malta,southern_europe,36,-14,122,319000,valetta,pound).
c_r_l_l_s_cap_m(mauritania,west_africa,21,10,419229,1260000,nouakchott,ouguiya).
c_r_l_l_s_cap_m(mauritius,southern_africa,-20,-57,787,870000,port_louis,rupee).
c_r_l_l_s_cap_m(mexico,central_america,20,100,761601,54300000,mexico_city,peso).
c_r_l_l_s_cap_m(monaco,southern_europe,44,-7,1,30000,monaco_city,french_franc).
c_r_l_l_s_cap_m(mongolia,northern_asia,47,-103,604247,1360000,ulan_bator,tighrik).
c_r_l_l_s_cap_m(morocco,north_africa,32,6,171953,16310000,rabat,dirham).
c_r_l_l_s_cap_m(mozambique,southern_africa,-19,-35,303373,8820000,maputo,?).
c_r_l_l_s_cap_m(nepal,indian_subcontinent,28,-84,54362,12020000,katmandu,nepalese_rupee).
c_r_l_l_s_cap_m(netherlands,western_europe,52,-5,14192,13500000,amsterdam,guilder).
c_r_l_l_s_cap_m(new_zealand,oceania,-40,-176,103736,2962000,wellington,new_zealand_dollar).
c_r_l_l_s_cap_m(nicaragua,central_america,12,85,57143,2010000,managua,cordoba).
c_r_l_l_s_cap_m(niger,west_africa,13,-10,489206,4300000,niamey,cfa_franc).
c_r_l_l_s_cap_m(nigeria,west_africa,8,-8,356669,79759000,lagos,naira).
c_r_l_l_s_cap_m(north_korea,far_east,40,-127,46768,15090000,pvongvang,won).
c_r_l_l_s_cap_m(norway,scandinavia,64,-11,125181,3960000,oslo,krone).
c_r_l_l_s_cap_m(oman,middle_east,23,-58,82000 ,720000,muscat,riyal_omani).
c_r_l_l_s_cap_m(pakistan,indian_subcontinent,30,-70,342750,66750000,islamad,rupee).
c_r_l_l_s_cap_m(panama,central_america,9,80,28753,1570000,panama_city,balboa).
c_r_l_l_s_cap_m(papua_new_guinea,oceania,-8,-145,183540,2580000,port_harcourt,australian_dollar).
c_r_l_l_s_cap_m(paraguay,south_america,-23,57,157047,2670000,asuncion,guarani).
c_r_l_l_s_cap_m(peru,south_america,-8,75,496222,14910000,lima,sol).
c_r_l_l_s_cap_m(philippines,southeast_east,12,-123,115707,40220000,quezon_city,piso).
c_r_l_l_s_cap_m(poland,eastern_europe,52,-20,120359,33360000,warsaw,zloty).
c_r_l_l_s_cap_m(portugal,southern_europe,40,7,35340,8560000,lisbon,escudo).
c_r_l_l_s_cap_m(qatar,middle_east,25,-51,4000 ,115000,doha,riyal).
c_r_l_l_s_cap_m(romania,eastern_europe,46,-25,91699,5690000,bucharest,leu).
c_r_l_l_s_cap_m(rwanda,central_africa,-2,-30,10169,3980000,kigali,rwanda_franc).
c_r_l_l_s_cap_m(san_marino,southern_europe,44,-12,24,20000,san_marino_city,italian_lira).
c_r_l_l_s_cap_m(saudi_arabia,middle_east,26,-44,873000 , 8100000,riyadh,riyal).
c_r_l_l_s_cap_m(senegal,west_africa,14,14,76124,4230000,dakar,cfa_franc).
c_r_l_l_s_cap_m(seychelles,east_africa,-4,-55,40,156000,victoria,rupee).
c_r_l_l_s_cap_m(sierra_leone,west_africa,9,12,27925,2860000,freetown,leone).
c_r_l_l_s_cap_m(singapore,southeast_east,1,-104,226,2190000,singapore_city,singapore_dollar).
c_r_l_l_s_cap_m(somalia,east_africa,7,-47,246155,3100000,mogadishu,somali_shilling).
c_r_l_l_s_cap_m(south_africa,southern_africa,-30,-25,471819,23720000,pretoria,rand).
c_r_l_l_s_cap_m(south_korea,far_east,36,-128,38031,33333000,seoul,won).
c_r_l_l_s_cap_m(south_yemen,middle_east,15,-48,111000,1600000,aden,dinar).
c_r_l_l_s_cap_m(soviet_union,northern_asia,57,-80,8347250,250900000,moscow,ruble).
c_r_l_l_s_cap_m(spain,southern_europe,40,5,194883,34860000,madrid,peseta).
c_r_l_l_s_cap_m(sri_lanka,indian_subcontinent,7,-81,25332,13250000,colombo,rupee).
c_r_l_l_s_cap_m(sudan,central_africa,15,-30,967491,16900000,khartoum,pound).
c_r_l_l_s_cap_m(surinam,south_america,4,56,55000 ,208000,paramaribo,?).
c_r_l_l_s_cap_m(swaziland,southern_africa,-26,-31,6705,460000,mbabane,lilageru).
c_r_l_l_s_cap_m(sweden,scandinavia,63,-15,173665,8144000,stockholm,krona).
c_r_l_l_s_cap_m(switzerland,western_europe,46,-8,15941,6440000,bern,franc).
c_r_l_l_s_cap_m(syria,middle_east,35,-38,71498,6895000,damascus,syrian_pound).
c_r_l_l_s_cap_m(taiwan,far_east,23,-121,13592,15737000,taipei,taiwan_dollar).
c_r_l_l_s_cap_m(tanzania,east_africa,-7,-34,363708,14000000,dar_es_salaam,tanzanian_shilling).
c_r_l_l_s_cap_m(thailand,southeast_east,16,-102,198455,39950000,bangkok,baht).
c_r_l_l_s_cap_m(togo,west_africa,8,-1,21853,2120000,lome,cfa_franc).
c_r_l_l_s_cap_m(tonga,oceania,-20,173,269,90000,nukualofa,pa_anga).
c_r_l_l_s_cap_m(trinidad_and_tobago,caribbean,10,61,1979,5510000,port_of_spain,trinidad_and_tobago_dollar).
c_r_l_l_s_cap_m(tunisia,north_africa,33,-9,63378,5510000,tunis,dinar).
c_r_l_l_s_cap_m(turkey,middle_east,39,-36,301380,37930000,ankara,lira).
c_r_l_l_s_cap_m(uganda,east_africa,2,-32,91134,10810000,kampala,uganda_shilling).
c_r_l_l_s_cap_m(united_arab_emirates,middle_east,24,-54,32278,210000,abu_dhabi,dirham).
c_r_l_l_s_cap_m(united_kingdom,western_europe,54,2,94209,55930000,london,pound).
c_r_l_l_s_cap_m(united_states,north_america,37,96,3615122,211210000,washington_dc,dollar).
c_r_l_l_s_cap_m(upper_volta,west_africa,12,2,105869,5740000,ouagadougou,cfa_franc).
c_r_l_l_s_cap_m(uruguay,south_america,-32,55,68548,2990000,montevideo,peso).
c_r_l_l_s_cap_m(venezuela,south_america,8,65,352143,11520000,caracas,bolivar).
c_r_l_l_s_cap_m(vietnam,southeast_east,17,-107,126436,41850000,hanoi,dong).
c_r_l_l_s_cap_m(west_germany,western_europe,52,-9,95815,61970000,bonn,deutsche_mark).
c_r_l_l_s_cap_m(western_samoa,oceania,-14,172,1133,150000,apia,tala).
c_r_l_l_s_cap_m(yemen,middle_east,15,-44,75289,1600000,sana,rial).
c_r_l_l_s_cap_m(yugoslavia,southern_europe,44,-20,98766,21126000,belgrade,dinar).
c_r_l_l_s_cap_m(zaire,central_africa,-3,-23,905063,23560000,kinshasa,zaire).
c_r_l_l_s_cap_m(zambia,southern_africa,-15,-28,290724,4640000,lusaka,kwacha).
c_r_l_l_s_cap_m(zimbabwe,southern_africa,-20,-30,150333,5690000,salisbury,rhodesian_dollar).

:- fixup_exports.

