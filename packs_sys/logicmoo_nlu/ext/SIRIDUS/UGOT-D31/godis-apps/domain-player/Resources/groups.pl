
/*************************************************************************

         name: lexicon_groups.pl 
	 date: 2005-02-15
       author: Andreas Wallentin

       This file should represent all possible groups, not just
       the ones existing in the DB.

       This should be used from semsort.pl
       
*************************************************************************/
:-multifile synset/2.
:-discontiguous synset/2,group/1.

:- use_module( dbase,          [break_list/2] ).

%%% Atom == 'the legendary pink dots'
group_atom( Atom ):-
	atomic(Atom),
%	format("inne kollar gruppper ~w~n",[Atom]),
	name(Atom,CharList),
	break_list(CharList,GroupList),
	chk_groups(GroupList).

group([abba]).
group([adolphson,och,falk]).
group([annelie,ryde]).
group([bo,kaspers,orkester]).
group([britney,spears]).
group([cecilia,vennersten]).
group([christer,sandelin]).
group([covenant]).
group([depeche,mode]).
group([dia,psalma]).
group([docent,död]).
group([ebba,grön]).
group([eldkvarn]).
group([eva,dahlgren]).
group([flesh,field]).
group([freda]).
group([gyllene,tider]).
group([imperiet]).
group([irma]).
group([jakob,hellman]).
group([jan,johansen]).
group([kent]).
group([komputer]).
group([lars,winnerbäck]).
group([lisa,ekdahl]).
group([lisa,nilsson]).
group([lustans,lakejer]).
group([madonna]).
group([malin,julia]).
group([marie,fredriksson]).
group([mauro,scocco]).
group([mesh]).
group([mikael,rickfors]).
group([mikael,wiehe]).
group([monica,törnell]).
group([morlocks]).
group([morrissey]).
group([nitzer,ebb]).
group([orup]).
group([patrik,isaksson]).
group([peps,persson]).
group([petter]).
group([pet,shop,boys]).
group([peter,lemarc]).
group([pixies]).
group([rammstein]).
group([roger,pontare]).
group([spock]).
group([staffan,hellstrand]).
group([stina,nordenstam]).
group([legendary,pink,dots]).
group([the,legendary,pink,dots]).
group([the,cure]).
group([thomas,di,leva]).
group([tomas,ledin]).
group([tommy,nilsson]).
group([ulf,lundell]).
group([uno,svenningsson]).
group([viba,femba]).
group([wilmer,x]).

%% grupper med samma namn som album
group([jumper]).
group([nordman]).

%%% to use when compilation
group([best,of]).

synset([[covenant]],covenant).
synset([[flesh,field]],flesh_field).
synset([[komputer]],komputer).
synset([[mesh]],mesh).
synset([[morrissey]],morrissey).
synset([[nitzer,ebb]],nitzer).
synset([[pet,shop,boys]],pet_shop).
synset([[spock]],spock).
synset([[rammstein]],rammstein).
synset([[legendary,pink,dots],[the,legendary,pink,dots]],pink_dots).
synset([[depeche,mode]],depeche).
synset([[britney,spears]],britney).
synset([[stina,nordenstam]],nordenstam).
synset([[morlocks]],morlocks).
synset([[the,cure]],the_cure).
synset([[abba]],abba).
synset([[pixies]],pixies).
synset([[madonna]],madonna).


chk_groups( [] ).
chk_groups( [Group|Xs] ):-
	group(Group),
	chk_groups(Xs).

/*

om man ska kolla lite felstavade ord...
bara enkel stavningskoll

% Word är felstavat och ett alternativt ord är SimilarWord.
% similar_word( +Word, ?SimilarWord ).
similar_word( Word, SimilarWord ):-
	alike( Word, SimilarWord ).


% Word är felstavat och alternativa ord listas i SimilarWords.
% similar_words( +Word, ?SimilarWords ).
similar_words( Word, SimilarWords ):-
	setof( NyttOrd, lika( Word, NyttOrd ), SimilarWords ).


% alike( +Ord, ?RiktigtOrd ).
% Ord skall vara nästan som ett RiktigtOrd, som finns i databasen som kollas i lookup/1.


%% Tar bort en bokstav i ett ord och kollar mot lexikonen.
%% alike( +Ord, ?RiktigtOrd )
alike( Ord, RiktigtOrd ):-
	name(Ord, Lista),
	select( _, Lista, SLista ),
	name(RiktigtOrd, SLista),
	group( RiktigtOrd ).

%% Byter plats på två intilliggande bokstäver och kollar mot lexikonen.
%% alike( +Ord, ?RiktigtOrd )
alike( Ord, RiktigtOrd ):-
	name( Ord, Lista ),
	swap_places( Lista, NyLista ),
	name( RiktigtOrd, NyLista ),
	group( RiktigtOrd ).

%% Lägger till en bokstav  och kollar mot lexikonen.
%% alike( +Ord, ?RiktigtOrd )
alike( Ord, RiktigtOrd ):-
	name( Ord, Lista ),
	another( Lista, NyttOrd ),
	name( RiktigtOrd, NyttOrd ),
	group( RiktigtOrd ).

% Byter ut en valfri bokstav mot en ny och kollar mot lexikonen.
% alike( +Ord, ?RiktigtOrd )
alike( Ord, RiktigtOrd ):-
	name( Ord, Lista ),
	replace( Lista, NyLista ),
	name( RiktigtOrd, NyLista ),
	group( RiktigtOrd ).




swap_places( [], [] ).
swap_places( [X,Y|Zs], [Y,X|Zs] ).
swap_places( [X,Y|Zs], [X,Z|Ys] ):-
	swap_places( [Y|Zs], [Z|Ys] ).

replace( [], [] ).
replace( [X|Xs], Lista ):-
	letter( Y ),
	name( Y, Y2 ),
	append( Y2, Xs, Lista ),
	[X|Xs] \= Lista.
replace( [X|Xs], [X|Zs] ):-
	replace( Xs, Zs ).


another( Lista, NyLista ):-
	letter(Z),
	name(Z,Y),
	another_2(Y,Lista,NyLista).

another_2([X],Lista, NyLista):-
	insert(X,Lista,NyLista).


insert(X,[],[X]).
insert(X,[Y|Xs],[X,Y|Xs]).
insert(X,[Y|Xs],[Y|Zs]):-
	insert(X,Xs,Zs).
	
	



letter( X ):-
	select( X, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,å,ä,ö], _ ).
*/
