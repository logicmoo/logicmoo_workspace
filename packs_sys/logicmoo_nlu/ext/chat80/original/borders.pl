
/*  Copyright 1986-2020 David H. D. Warren, Fernando C. N. Pereira and
    Jan Wielemaker.

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

:- dynamic(direct_ss/4).

symmetric_pred(Spatial,B,X,C):- nonvar(X),nonvar(C),!,symmetric_direct(Spatial,B,X,C),!.
symmetric_pred(Spatial,B,X,C):- symmetric_direct(Spatial,B,X,C).

symmetric_direct(Spatial,B,X,C) :- direct_ss(Spatial,B,X,C).
symmetric_direct(Spatial,B,X,C) :- direct_ss(Spatial,B,C,X).

add_ss(Spatial,B,X,C):- X @> C, !, add_ss(Spatial,B,C,X).
add_ss(Spatial,B,X,C):- direct_ss(Spatial,B, X,C), !.
add_ss(Spatial,B,X,C):- assertz(direct_ss(Spatial,B, X,C)), !.

% Facts about Europe.
% ------------------
add_border(Albania,Greece):- add_ss(spatial,border,Albania,Greece).

:-add_border(albania,greece).
:-add_border(albania,yugoslavia).
:-add_border(albania,mediterranean).

:-add_border(andorra,france).
:-add_border(andorra,spain).

:-add_border(austria,czechoslovakia).
:-add_border(austria,hungary).
:-add_border(austria,italy).
:-add_border(austria,liechtenstein).
:-add_border(austria,switzerland).
:-add_border(austria,west_germany).
:-add_border(austria,yugoslavia).

:-add_border(belgium,france).
:-add_border(belgium,luxembourg).
:-add_border(belgium,netherlands).
:-add_border(belgium,west_germany).
:-add_border(belgium,atlantic).

:-add_border(bulgaria,greece).
:-add_border(bulgaria,romania).
:-add_border(bulgaria,turkey).
:-add_border(bulgaria,yugoslavia).
:-add_border(bulgaria,black_sea).

:-add_border(cyprus,mediterranean).

:-add_border(czechoslovakia,austria).
:-add_border(czechoslovakia,east_germany).
:-add_border(czechoslovakia,hungary).
:-add_border(czechoslovakia,poland).
:-add_border(czechoslovakia,soviet_union).
:-add_border(czechoslovakia,west_germany).

:-add_border(denmark,west_germany).
:-add_border(denmark,atlantic).
:-add_border(denmark,baltic).

:-add_border(eire,united_kingdom).
:-add_border(eire,atlantic).

:-add_border(finland,norway).
:-add_border(finland,soviet_union).
:-add_border(finland,sweden).
:-add_border(finland,baltic).

:-add_border(france,andorra).
:-add_border(france,belgium).
:-add_border(france,italy).
:-add_border(france,luxembourg).
:-add_border(france,monaco).
:-add_border(france,spain).
:-add_border(france,switzerland).
:-add_border(france,west_germany).
:-add_border(france,atlantic).
:-add_border(france,mediterranean).

:-add_border(east_germany,czechoslovakia).
:-add_border(east_germany,poland).
:-add_border(east_germany,west_germany).
:-add_border(east_germany,baltic).

:-add_border(greece,albania).
:-add_border(greece,bulgaria).
:-add_border(greece,turkey).
:-add_border(greece,yugoslavia).
:-add_border(greece,mediterranean).

:-add_border(hungary,austria).
:-add_border(hungary,czechoslovakia).
:-add_border(hungary,romania).
:-add_border(hungary,soviet_union).
:-add_border(hungary,yugoslavia).

:-add_border(iceland,atlantic).

:-add_border(italy,austria).
:-add_border(italy,france).
:-add_border(italy,san_marino).
:-add_border(italy,switzerland).
:-add_border(italy,yugoslavia).
:-add_border(italy,mediterranean).

:-add_border(liechtenstein,austria).
:-add_border(liechtenstein,switzerland).

:-add_border(luxembourg,belgium).
:-add_border(luxembourg,france).
:-add_border(luxembourg,west_germany).

:-add_border(malta,mediterranean).

:-add_border(monaco,france).
:-add_border(monaco,mediterranean).

:-add_border(netherlands,belgium).
:-add_border(netherlands,west_germany).
:-add_border(netherlands,atlantic).

:-add_border(norway,finland).
:-add_border(norway,sweden).
:-add_border(norway,soviet_union).
:-add_border(norway,arctic_ocean).
:-add_border(norway,atlantic).

:-add_border(poland,czechoslovakia).
:-add_border(poland,east_germany).
:-add_border(poland,soviet_union).
:-add_border(poland,baltic).

:-add_border(portugal,spain).
:-add_border(portugal,atlantic).

:-add_border(romania,bulgaria).
:-add_border(romania,hungary).
:-add_border(romania,soviet_union).
:-add_border(romania,yugoslavia).
:-add_border(romania,black_sea).

:-add_border(san_marino,italy).
:-add_border(san_marino,mediterranean).

:-add_border(spain,andorra).
:-add_border(spain,france).
:-add_border(spain,portugal).
:-add_border(spain,atlantic).
:-add_border(spain,mediterranean).

:-add_border(sweden,finland).
:-add_border(sweden,norway).
:-add_border(sweden,atlantic).
:-add_border(sweden,baltic).

:-add_border(switzerland,austria).
:-add_border(switzerland,france).
:-add_border(switzerland,italy).
:-add_border(switzerland,liechtenstein).
:-add_border(switzerland,west_germany).

:-add_border(west_germany,austria).
:-add_border(west_germany,belgium).
:-add_border(west_germany,czechoslovakia).
:-add_border(west_germany,denmark).
:-add_border(west_germany,east_germany).
:-add_border(west_germany,france).
:-add_border(west_germany,luxembourg).
:-add_border(west_germany,netherlands).
:-add_border(west_germany,switzerland).
:-add_border(west_germany,atlantic).
:-add_border(west_germany,baltic).

:-add_border(united_kingdom,eire).
:-add_border(united_kingdom,atlantic).

:-add_border(yugoslavia,albania).
:-add_border(yugoslavia,austria).
:-add_border(yugoslavia,bulgaria).
:-add_border(yugoslavia,greece).
:-add_border(yugoslavia,hungary).
:-add_border(yugoslavia,italy).
:-add_border(yugoslavia,romania).
:-add_border(yugoslavia,mediterranean).

% Facts about Asia.
% ----------------

:-add_border(afghanistan,china).
:-add_border(afghanistan,iran).
:-add_border(afghanistan,pakistan).
:-add_border(afghanistan,soviet_union).

:-add_border(bahrain,persian_gulf).

:-add_border(bangladesh,burma).
:-add_border(bangladesh,india).
:-add_border(bangladesh,indian_ocean).

:-add_border(bhutan,china).
:-add_border(bhutan,india).

:-add_border(burma,bangladesh).
:-add_border(burma,china).
:-add_border(burma,india).
:-add_border(burma,laos).
:-add_border(burma,thailand).
:-add_border(burma,indian_ocean).

:-add_border(cambodia,laos).
:-add_border(cambodia,thailand).
:-add_border(cambodia,vietnam).
:-add_border(cambodia,pacific).

:-add_border(china,afghanistan).
:-add_border(china,bhutan).
:-add_border(china,burma).
:-add_border(china,india).
:-add_border(china,laos).
:-add_border(china,mongolia).
:-add_border(china,nepal).
:-add_border(china,north_korea).
:-add_border(china,pakistan).
:-add_border(china,soviet_union).
:-add_border(china,vietnam).
:-add_border(china,pacific).

:-add_border(india,bangladesh).
:-add_border(india,bhutan).
:-add_border(india,burma).
:-add_border(india,china).
:-add_border(india,nepal).
:-add_border(india,pakistan).
:-add_border(india,indian_ocean).

:-add_border(indonesia,malaysia).
:-add_border(indonesia,papua_new_guinea).
:-add_border(indonesia,indian_ocean).
:-add_border(indonesia,pacific).

:-add_border(iran,afghanistan).
:-add_border(iran,iraq).
:-add_border(iran,pakistan).
:-add_border(iran,soviet_union).
:-add_border(iran,turkey).
:-add_border(iran,caspian_sea).
:-add_border(iran,persian_gulf).
:-add_border(iran,indian_ocean).

:-add_border(iraq,iran).
:-add_border(iraq,jordan).
:-add_border(iraq,kuwait).
:-add_border(iraq,saudi_arabia).
:-add_border(iraq,syria).
:-add_border(iraq,turkey).
:-add_border(iraq,persian_gulf).

:-add_border(israel,egypt).
:-add_border(israel,jordan).
:-add_border(laos,burma).
:-add_border(laos,cambodia).
:-add_border(laos,china).
:-add_border(laos,thailand).
:-add_border(laos,vietnam).

:-add_border(israel,lebanon).
:-add_border(israel,syria).
:-add_border(israel,mediterranean).
:-add_border(israel,red_sea).

:-add_border(japan,pacific).

:-add_border(jordan,iraq).
:-add_border(jordan,israel).
:-add_border(jordan,saudi_arabia).
:-add_border(jordan,syria).
:-add_border(jordan,red_sea).

:-add_border(kuwait,iraq).
:-add_border(kuwait,saudi_arabia).
:-add_border(kuwait,persian_gulf).

:-add_border(lebanon,israel).
:-add_border(lebanon,syria).
:-add_border(lebanon,mediterranean).

:-add_border(malaysia,indonesia).
:-add_border(malaysia,singapore).
:-add_border(malaysia,thailand).
:-add_border(malaysia,indian_ocean).
:-add_border(malaysia,pacific).

:-add_border(maldives,indian_ocean).

:-add_border(mongolia,china).
:-add_border(mongolia,soviet_union).

:-add_border(nepal,china).
:-add_border(nepal,india).

:-add_border(north_korea,china).
:-add_border(north_korea,south_korea).
:-add_border(north_korea,soviet_union).
:-add_border(north_korea,pacific).

:-add_border(oman,saudi_arabia).
:-add_border(oman,united_arab_emirates).
:-add_border(oman,south_yemen).
:-add_border(oman,indian_ocean).

:-add_border(pakistan,afghanistan).
:-add_border(pakistan,china).
:-add_border(pakistan,india).
:-add_border(pakistan,iran).
:-add_border(pakistan,indian_ocean).

:-add_border(philippines,pacific).

:-add_border(qatar,saudi_arabia).
:-add_border(qatar,united_arab_emirates).
:-add_border(qatar,persian_gulf).

:-add_border(saudi_arabia,iraq).
:-add_border(saudi_arabia,jordan).
:-add_border(saudi_arabia,kuwait).
:-add_border(saudi_arabia,oman).
:-add_border(saudi_arabia,qatar).
:-add_border(saudi_arabia,south_yemen).
:-add_border(saudi_arabia,united_arab_emirates).
:-add_border(saudi_arabia,yemen).
:-add_border(saudi_arabia,persian_gulf).
:-add_border(saudi_arabia,red_sea).

:-add_border(singapore,malaysia).
:-add_border(singapore,pacific).

:-add_border(south_korea,north_korea).
:-add_border(south_korea,pacific).

:-add_border(south_yemen,oman).
:-add_border(south_yemen,saudi_arabia).
:-add_border(south_yemen,yemen).
:-add_border(south_yemen,indian_ocean).

:-add_border(soviet_union,afghanistan).
:-add_border(soviet_union,china).
:-add_border(soviet_union,czechoslovakia).
:-add_border(soviet_union,finland).
:-add_border(soviet_union,hungary).
:-add_border(soviet_union,iran).
:-add_border(soviet_union,mongolia).
:-add_border(soviet_union,north_korea).
:-add_border(soviet_union,norway).
:-add_border(soviet_union,poland).
:-add_border(soviet_union,romania).
:-add_border(soviet_union,turkey).
:-add_border(soviet_union,arctic_ocean).
:-add_border(soviet_union,baltic).
:-add_border(soviet_union,black_sea).
:-add_border(soviet_union,caspian_sea).
:-add_border(soviet_union,pacific).

:-add_border(sri_lanka,indian_ocean).

:-add_border(syria,iraq).
:-add_border(syria,israel).
:-add_border(syria,jordan).
:-add_border(syria,lebanon).
:-add_border(syria,turkey).
:-add_border(syria,mediterranean).

:-add_border(taiwan,pacific).

:-add_border(thailand,burma).
:-add_border(thailand,cambodia).
:-add_border(thailand,laos).
:-add_border(thailand,malaysia).
:-add_border(thailand,indian_ocean).
:-add_border(thailand,pacific).

:-add_border(turkey,bulgaria).
:-add_border(turkey,greece).
:-add_border(turkey,iran).
:-add_border(turkey,iraq).
:-add_border(turkey,soviet_union).
:-add_border(turkey,syria).
:-add_border(turkey,black_sea).
:-add_border(turkey,mediterranean).

:-add_border(united_arab_emirates,oman).
:-add_border(united_arab_emirates,qatar).
:-add_border(united_arab_emirates,saudi_arabia).
:-add_border(united_arab_emirates,persian_gulf).

:-add_border(vietnam,cambodia).
:-add_border(vietnam,china).
:-add_border(vietnam,laos).
:-add_border(vietnam,pacific).

:-add_border(yemen,south_yemen).
:-add_border(yemen,saudi_arabia).
:-add_border(yemen,red_sea).

% Facts about Africa.
% ------------------

:-add_border(algeria,libya).
:-add_border(algeria,mali).
:-add_border(algeria,mauritania).
:-add_border(algeria,morocco).
:-add_border(algeria,niger).
:-add_border(algeria,tunisia).
:-add_border(algeria,mediterranean).

:-add_border(angola,congo).
:-add_border(angola,south_africa).
:-add_border(angola,zaire).
:-add_border(angola,zambia).
:-add_border(angola,atlantic).

:-add_border(botswana,south_africa).
:-add_border(botswana,zimbabwe).

:-add_border(burundi,rwanda).
:-add_border(burundi,tanzania).
:-add_border(burundi,zaire).

:-add_border(cameroon,central_african_republic).
:-add_border(cameroon,chad).
:-add_border(cameroon,congo).
:-add_border(cameroon,equatorial_guinea).
:-add_border(cameroon,gabon).
:-add_border(cameroon,nigeria).
:-add_border(cameroon,atlantic).

:-add_border(central_african_republic,cameroon).
:-add_border(central_african_republic,chad).
:-add_border(central_african_republic,congo).
:-add_border(central_african_republic,sudan).
:-add_border(central_african_republic,zaire).

:-add_border(chad,cameroon).
:-add_border(chad,central_african_republic).
:-add_border(chad,libya).
:-add_border(chad,niger).
:-add_border(chad,nigeria).
:-add_border(chad,sudan).

:-add_border(congo,angola).
:-add_border(congo,cameroon).
:-add_border(congo,central_african_republic).
:-add_border(congo,gabon).
:-add_border(congo,zaire).
:-add_border(congo,atlantic).

:-add_border(dahomey,niger).
:-add_border(dahomey,nigeria).
:-add_border(dahomey,togo).
:-add_border(dahomey,upper_volta).
:-add_border(dahomey,atlantic).

:-add_border(djibouti,ethiopia).
:-add_border(djibouti,somalia).
:-add_border(djibouti,indian_ocean).

:-add_border(egypt,israel).
:-add_border(egypt,libya).
:-add_border(egypt,sudan).
:-add_border(egypt,mediterranean).
:-add_border(egypt,red_sea).

:-add_border(equatorial_guinea,cameroon).
:-add_border(equatorial_guinea,gabon).
:-add_border(equatorial_guinea,atlantic).

:-add_border(ethiopia,djibouti).
:-add_border(ethiopia,kenya).
:-add_border(ethiopia,somalia).
:-add_border(ethiopia,sudan).
:-add_border(ethiopia,red_sea).

:-add_border(gabon,cameroon).
:-add_border(gabon,congo).
:-add_border(gabon,equatorial_guinea).
:-add_border(gabon,atlantic).

:-add_border(gambia,senegal).
:-add_border(gambia,atlantic).

:-add_border(ghana,ivory_coast).
:-add_border(ghana,togo).
:-add_border(ghana,upper_volta).
:-add_border(ghana,atlantic).

:-add_border(guinea,guinea_bissau).
:-add_border(guinea,ivory_coast).
:-add_border(guinea,liberia).
:-add_border(guinea,mali).
:-add_border(guinea,senegal).
:-add_border(guinea,sierra_leone).
:-add_border(guinea,atlantic).

:-add_border(guinea_bissau,guinea).
:-add_border(guinea_bissau,senegal).
:-add_border(guinea_bissau,atlantic).

:-add_border(ivory_coast,ghana).
:-add_border(ivory_coast,guinea).
:-add_border(ivory_coast,liberia).
:-add_border(ivory_coast,mali).
:-add_border(ivory_coast,upper_volta).
:-add_border(ivory_coast,atlantic).

:-add_border(kenya,ethiopia).
:-add_border(kenya,somalia).
:-add_border(kenya,sudan).
:-add_border(kenya,tanzania).
:-add_border(kenya,uganda).
:-add_border(kenya,indian_ocean).

:-add_border(lesotho,south_africa).

:-add_border(liberia,ivory_coast).
:-add_border(liberia,guinea).
:-add_border(liberia,sierra_leone).
:-add_border(liberia,atlantic).

:-add_border(libya,algeria).
:-add_border(libya,chad).
:-add_border(libya,egypt).
:-add_border(libya,niger).
:-add_border(libya,sudan).
:-add_border(libya,tunisia).
:-add_border(libya,mediterranean).

:-add_border(malagasy,indian_ocean).

:-add_border(malawi,mozambique).
:-add_border(malawi,tanzania).
:-add_border(malawi,zambia).

:-add_border(mali,algeria).
:-add_border(mali,guinea).
:-add_border(mali,ivory_coast).
:-add_border(mali,mauritania).
:-add_border(mali,niger).
:-add_border(mali,senegal).
:-add_border(mali,upper_volta).

:-add_border(mauritania,algeria).
:-add_border(mauritania,mali).
:-add_border(mauritania,morocco).
:-add_border(mauritania,senegal).
:-add_border(mauritania,atlantic).

:-add_border(mauritius,indian_ocean).

:-add_border(morocco,algeria).
:-add_border(morocco,mauritania).
:-add_border(morocco,atlantic).
:-add_border(morocco,mediterranean).

:-add_border(mozambique,malawi).
:-add_border(mozambique,south_africa).
:-add_border(mozambique,swaziland).
:-add_border(mozambique,tanzania).
:-add_border(mozambique,zambia).
:-add_border(mozambique,zimbabwe).
:-add_border(mozambique,indian_ocean).

:-add_border(niger,algeria).
:-add_border(niger,chad).
:-add_border(niger,dahomey).
:-add_border(niger,libya).
:-add_border(niger,mali).
:-add_border(niger,nigeria).
:-add_border(niger,upper_volta).

:-add_border(nigeria,cameroon).
:-add_border(nigeria,chad).
:-add_border(nigeria,dahomey).
:-add_border(nigeria,niger).
:-add_border(nigeria,atlantic).

:-add_border(rwanda,burundi).
:-add_border(rwanda,tanzania).
:-add_border(rwanda,uganda).
:-add_border(rwanda,zaire).

:-add_border(senegal,gambia).
:-add_border(senegal,guinea).
:-add_border(senegal,guinea_bissau).
:-add_border(senegal,mali).
:-add_border(senegal,mauritania).
:-add_border(senegal,atlantic).

:-add_border(seychelles,indian_ocean).

:-add_border(sierra_leone,guinea).
:-add_border(sierra_leone,liberia).
:-add_border(sierra_leone,atlantic).

:-add_border(somalia,djibouti).
:-add_border(somalia,ethiopia).
:-add_border(somalia,kenya).
:-add_border(somalia,indian_ocean).

:-add_border(south_africa,angola).
:-add_border(south_africa,botswana).
:-add_border(south_africa,lesotho).
:-add_border(south_africa,mozambique).
:-add_border(south_africa,swaziland).
:-add_border(south_africa,zambia).
:-add_border(south_africa,zimbabwe).
:-add_border(south_africa,atlantic).
:-add_border(south_africa,indian_ocean).

:-add_border(sudan,chad).
:-add_border(sudan,central_african_republic).
:-add_border(sudan,egypt).
:-add_border(sudan,ethiopia).
:-add_border(sudan,kenya).
:-add_border(sudan,libya).
:-add_border(sudan,uganda).
:-add_border(sudan,zaire).
:-add_border(sudan,red_sea).

:-add_border(swaziland,mozambique).
:-add_border(swaziland,south_africa).

:-add_border(tanzania,burundi).
:-add_border(tanzania,kenya).
:-add_border(tanzania,malawi).
:-add_border(tanzania,mozambique).
:-add_border(tanzania,rwanda).
:-add_border(tanzania,uganda).
:-add_border(tanzania,zaire).
:-add_border(tanzania,zambia).
:-add_border(tanzania,indian_ocean).

:-add_border(togo,dahomey).
:-add_border(togo,ghana).
:-add_border(togo,upper_volta).
:-add_border(togo,atlantic).

:-add_border(tunisia,algeria).
:-add_border(tunisia,libya).
:-add_border(tunisia,mediterranean).

:-add_border(uganda,kenya).
:-add_border(uganda,rwanda).
:-add_border(uganda,sudan).
:-add_border(uganda,tanzania).
:-add_border(uganda,zaire).

:-add_border(upper_volta,dahomey).
:-add_border(upper_volta,ghana).
:-add_border(upper_volta,ivory_coast).
:-add_border(upper_volta,mali).
:-add_border(upper_volta,niger).
:-add_border(upper_volta,togo).

:-add_border(zaire,angola).
:-add_border(zaire,burundi).
:-add_border(zaire,central_african_republic).
:-add_border(zaire,congo).
:-add_border(zaire,rwanda).
:-add_border(zaire,sudan).
:-add_border(zaire,tanzania).
:-add_border(zaire,uganda).
:-add_border(zaire,zambia).
:-add_border(zaire,atlantic).

:-add_border(zambia,angola).
:-add_border(zambia,malawi).
:-add_border(zambia,mozambique).
:-add_border(zambia,south_africa).
:-add_border(zambia,tanzania).
:-add_border(zambia,zaire).
:-add_border(zambia,zimbabwe).

:-add_border(zimbabwe,botswana).
:-add_border(zimbabwe,mozambique).
:-add_border(zimbabwe,south_africa).
:-add_border(zimbabwe,zambia).


% Facts about America.
% -------------------

:-add_border(argentina,bolivia).
:-add_border(argentina,brazil).
:-add_border(argentina,chile).
:-add_border(argentina,paraguay).
:-add_border(argentina,uruguay).
:-add_border(argentina,atlantic).

:-add_border(bahamas,atlantic).

:-add_border(barbados,atlantic).

:-add_border(belize,guatemala).
:-add_border(belize,mexico).
:-add_border(belize,atlantic).

:-add_border(bolivia,argentina).
:-add_border(bolivia,brazil).
:-add_border(bolivia,chile).
:-add_border(bolivia,paraguay).
:-add_border(bolivia,peru).

:-add_border(brazil,argentina).
:-add_border(brazil,bolivia).
:-add_border(brazil,colombia).
:-add_border(brazil,french_guiana).
:-add_border(brazil,guyana).
:-add_border(brazil,paraguay).
:-add_border(brazil,peru).
:-add_border(brazil,surinam).
:-add_border(brazil,uruguay).
:-add_border(brazil,venezuela).
:-add_border(brazil,atlantic).

:-add_border(canada,united_states).
:-add_border(canada,arctic_ocean).
:-add_border(canada,atlantic).
:-add_border(canada,pacific).

:-add_border(chile,argentina).
:-add_border(chile,bolivia).
:-add_border(chile,peru).
:-add_border(chile,pacific).

:-add_border(colombia,brazil).
:-add_border(colombia,ecuador).
:-add_border(colombia,panama).
:-add_border(colombia,peru).
:-add_border(colombia,venezuela).
:-add_border(colombia,atlantic).
:-add_border(colombia,pacific).

:-add_border(costa_rica,nicaragua).
:-add_border(costa_rica,panama).
:-add_border(costa_rica,atlantic).
:-add_border(costa_rica,pacific).

:-add_border(cuba,atlantic).

:-add_border(dominican_republic,haiti).
:-add_border(dominican_republic,atlantic).

:-add_border(ecuador,colombia).
:-add_border(ecuador,peru).
:-add_border(ecuador,pacific).

:-add_border(el_salvador,guatemala).
:-add_border(el_salvador,honduras).
:-add_border(el_salvador,pacific).

:-add_border(french_guiana,brazil).
:-add_border(french_guiana,surinam).

:-add_border(greenland,arctic_ocean).
:-add_border(greenland,atlantic).

:-add_border(grenada,atlantic).

:-add_border(guatemala,belize).
:-add_border(guatemala,el_salvador).
:-add_border(guatemala,honduras).
:-add_border(guatemala,mexico).
:-add_border(guatemala,atlantic).
:-add_border(guatemala,pacific).

:-add_border(guyana,brazil).
:-add_border(guyana,surinam).
:-add_border(guyana,venezuela).
:-add_border(guyana,atlantic).

:-add_border(haiti,dominican_republic).
:-add_border(haiti,atlantic).

:-add_border(honduras,el_salvador).
:-add_border(honduras,guatemala).
:-add_border(honduras,nicaragua).
:-add_border(honduras,atlantic).
:-add_border(honduras,pacific).

:-add_border(jamaica,atlantic).

:-add_border(mexico,belize).
:-add_border(mexico,guatemala).
:-add_border(mexico,united_states).
:-add_border(mexico,atlantic).
:-add_border(mexico,pacific).

:-add_border(nicaragua,costa_rica).
:-add_border(nicaragua,honduras).
:-add_border(nicaragua,atlantic).
:-add_border(nicaragua,pacific).

:-add_border(panama,colombia).
:-add_border(panama,costa_rica).
:-add_border(panama,atlantic).
:-add_border(panama,pacific).

:-add_border(paraguay,argentina).
:-add_border(paraguay,bolivia).
:-add_border(paraguay,brazil).

:-add_border(peru,bolivia).
:-add_border(peru,brazil).
:-add_border(peru,chile).
:-add_border(peru,colombia).
:-add_border(peru,ecuador).
:-add_border(peru,pacific).

:-add_border(surinam,brazil).
:-add_border(surinam,french_guiana).
:-add_border(surinam,guyana).

:-add_border(trinidad_and_tobago,atlantic).

:-add_border(united_states,canada).
:-add_border(united_states,mexico).
:-add_border(united_states,arctic_ocean).
:-add_border(united_states,atlantic).
:-add_border(united_states,pacific).

:-add_border(uruguay,argentina).
:-add_border(uruguay,brazil).
:-add_border(uruguay,atlantic).

:-add_border(venezuela,brazil).
:-add_border(venezuela,colombia).
:-add_border(venezuela,guyana).
:-add_border(venezuela,atlantic).

% Facts about Australasia.
% -----------------------

:-add_border(australia,indian_ocean).
:-add_border(australia,pacific).

:-add_border(fiji,pacific).

:-add_border(new_zealand,pacific).

:-add_border(papua_new_guinea,indonesia).
:-add_border(papua_new_guinea,pacific).

:-add_border(tonga,pacific).

:-add_border(western_samoa,pacific).

:-add_border(antarctica,southern_ocean).

% Facts about oceans and seas.
% ---------------------------

:-add_border(arctic_ocean,atlantic).
:-add_border(arctic_ocean,pacific).

:-add_border(atlantic,arctic_ocean).
:-add_border(atlantic,indian_ocean).
:-add_border(atlantic,pacific).
:-add_border(atlantic,southern_ocean).
:-add_border(atlantic,baltic).
:-add_border(atlantic,mediterranean).

:-add_border(indian_ocean,atlantic).
:-add_border(indian_ocean,pacific).
:-add_border(indian_ocean,southern_ocean).
:-add_border(indian_ocean,persian_gulf).
:-add_border(indian_ocean,red_sea).

:-add_border(pacific,arctic_ocean).
:-add_border(pacific,atlantic).
:-add_border(pacific,indian_ocean).
:-add_border(pacific,southern_ocean).

:-add_border(southern_ocean,atlantic).
:-add_border(southern_ocean,indian_ocean).
:-add_border(southern_ocean,pacific).

:-add_border(baltic,atlantic).

:-add_border(black_sea,mediterranean).

:-add_border(mediterranean,atlantic).
:-add_border(mediterranean,black_sea).

:-add_border(persian_gulf,indian_ocean).

:-add_border(red_sea,indian_ocean).

% Countries bordering each ocean and sea.
% --------------------------------------

:-add_border(arctic_ocean,norway).
:-add_border(arctic_ocean,soviet_union).
:-add_border(arctic_ocean,canada).
:-add_border(arctic_ocean,greenland).
:-add_border(arctic_ocean,united_states).

:-add_border(atlantic,belgium).
:-add_border(atlantic,denmark).
:-add_border(atlantic,eire).
:-add_border(atlantic,france).
:-add_border(atlantic,iceland).
:-add_border(atlantic,netherlands).
:-add_border(atlantic,norway).
:-add_border(atlantic,portugal).
:-add_border(atlantic,spain).
:-add_border(atlantic,sweden).
:-add_border(atlantic,west_germany).
:-add_border(atlantic,united_kingdom).
:-add_border(atlantic,angola).
:-add_border(atlantic,cameroon).
:-add_border(atlantic,congo).
:-add_border(atlantic,dahomey).
:-add_border(atlantic,equatorial_guinea).
:-add_border(atlantic,gabon).
:-add_border(atlantic,gambia).
:-add_border(atlantic,ghana).
:-add_border(atlantic,guinea).
:-add_border(atlantic,guinea_bissau).
:-add_border(atlantic,ivory_coast).
:-add_border(atlantic,liberia).
:-add_border(atlantic,mauritania).
:-add_border(atlantic,morocco).
:-add_border(atlantic,nigeria).
:-add_border(atlantic,senegal).
:-add_border(atlantic,sierra_leone).
:-add_border(atlantic,south_africa).
:-add_border(atlantic,togo).
:-add_border(atlantic,zaire).
:-add_border(atlantic,argentina).
:-add_border(atlantic,bahamas).
:-add_border(atlantic,barbados).
:-add_border(atlantic,belize).
:-add_border(atlantic,brazil).
:-add_border(atlantic,canada).
:-add_border(atlantic,colombia).
:-add_border(atlantic,costa_rica).
:-add_border(atlantic,cuba).
:-add_border(atlantic,dominican_republic).
:-add_border(atlantic,french_guiana).
:-add_border(atlantic,greenland).
:-add_border(atlantic,grenada).
:-add_border(atlantic,guatemala).
:-add_border(atlantic,guyana).
:-add_border(atlantic,haiti).
:-add_border(atlantic,honduras).
:-add_border(atlantic,jamaica).
:-add_border(atlantic,mexico).
:-add_border(atlantic,nicaragua).
:-add_border(atlantic,panama).
:-add_border(atlantic,surinam).
:-add_border(atlantic,trinidad_and_tobago).
:-add_border(atlantic,united_states).
:-add_border(atlantic,uruguay).
:-add_border(atlantic,venezuela).

:-add_border(indian_ocean,bangladesh).
:-add_border(indian_ocean,burma).
:-add_border(indian_ocean,india).
:-add_border(indian_ocean,indonesia).
:-add_border(indian_ocean,iran).
:-add_border(indian_ocean,malaysia).
:-add_border(indian_ocean,maldives).
:-add_border(indian_ocean,oman).
:-add_border(indian_ocean,pakistan).
:-add_border(indian_ocean,south_yemen).
:-add_border(indian_ocean,sri_lanka).
:-add_border(indian_ocean,thailand).
:-add_border(indian_ocean,djibouti).
:-add_border(indian_ocean,kenya).
:-add_border(indian_ocean,malagasy).
:-add_border(indian_ocean,mauritius).
:-add_border(indian_ocean,mozambique).
:-add_border(indian_ocean,seychelles).
:-add_border(indian_ocean,somalia).
:-add_border(indian_ocean,south_africa).
:-add_border(indian_ocean,tanzania).
:-add_border(indian_ocean,australia).

:-add_border(pacific,cambodia).
:-add_border(pacific,china).
:-add_border(pacific,indonesia).
:-add_border(pacific,japan).
:-add_border(pacific,malaysia).
:-add_border(pacific,north_korea).
:-add_border(pacific,philippines).
:-add_border(pacific,singapore).
:-add_border(pacific,south_korea).
:-add_border(pacific,soviet_union).
:-add_border(pacific,taiwan).
:-add_border(pacific,thailand).
:-add_border(pacific,vietnam).
:-add_border(pacific,canada).
:-add_border(pacific,chile).
:-add_border(pacific,colombia).
:-add_border(pacific,costa_rica).
:-add_border(pacific,ecuador).
:-add_border(pacific,el_salvador).
:-add_border(pacific,guatemala).
:-add_border(pacific,honduras).
:-add_border(pacific,mexico).
:-add_border(pacific,nicaragua).
:-add_border(pacific,panama).
:-add_border(pacific,peru).
:-add_border(pacific,united_states).
:-add_border(pacific,australia).
:-add_border(pacific,fiji).
:-add_border(pacific,new_zealand).
:-add_border(pacific,papua_new_guinea).
:-add_border(pacific,tonga).
:-add_border(pacific,western_samoa).

:-add_border(southern_ocean,antarctica).

:-add_border(baltic,denmark).
:-add_border(baltic,finland).
:-add_border(baltic,east_germany).
:-add_border(baltic,poland).
:-add_border(baltic,sweden).
:-add_border(baltic,west_germany).
:-add_border(baltic,soviet_union).

:-add_border(black_sea,bulgaria).
:-add_border(black_sea,romania).
:-add_border(black_sea,soviet_union).
:-add_border(black_sea,turkey).

:-add_border(caspian_sea,iran).
:-add_border(caspian_sea,soviet_union).

:-add_border(mediterranean,albania).
:-add_border(mediterranean,cyprus).
:-add_border(mediterranean,france).
:-add_border(mediterranean,greece).
:-add_border(mediterranean,italy).
:-add_border(mediterranean,malta).
:-add_border(mediterranean,monaco).
:-add_border(mediterranean,san_marino).
:-add_border(mediterranean,spain).
:-add_border(mediterranean,yugoslavia).
:-add_border(mediterranean,israel).
:-add_border(mediterranean,lebanon).
:-add_border(mediterranean,syria).
:-add_border(mediterranean,turkey).
:-add_border(mediterranean,algeria).
:-add_border(mediterranean,egypt).
:-add_border(mediterranean,libya).
:-add_border(mediterranean,morocco).
:-add_border(mediterranean,tunisia).

:-add_border(persian_gulf,bahrain).
:-add_border(persian_gulf,iran).
:-add_border(persian_gulf,iraq).
:-add_border(persian_gulf,kuwait).
:-add_border(persian_gulf,qatar).
:-add_border(persian_gulf,saudi_arabia).
:-add_border(persian_gulf,united_arab_emirates).

:-add_border(red_sea,israel).
:-add_border(red_sea,jordan).
:-add_border(red_sea,saudi_arabia).
:-add_border(red_sea,yemen).
:-add_border(red_sea,egypt).
:-add_border(red_sea,ethiopia).
:-add_border(red_sea,sudan).

