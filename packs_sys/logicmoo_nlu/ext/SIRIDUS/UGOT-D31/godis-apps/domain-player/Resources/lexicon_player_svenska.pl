 
/*************************************************************************

         name: lexicon_svenska_player.pl 
	 date: 2004-10-25
       author: Andreas Wallentin
 
*************************************************************************/

:- module( lexicon_player_svenska, [output_form/2,
				    input_form/2,
				    yn_answer/1]).

:- multifile synset/2.
:- discontiguous output_form/2, input_form/2.

resource_of_type(lexicon).

:- use_module( library(lists), [ member/2, select/3, append/3, is_list/1 ] ).
%%:- use_module( library(charsio), [ format_to_chars/3 ] ).

%% för mer variation av output
:- use_module( library(random) ).

%:- use_module( dbase ).
:- ensure_loaded( digits_svenska_player ).
:- ensure_loaded( semsort_player ).
%:- ensure_loaded( groups ).


/*----------------------------------------------------------------------
     output_form( +Move, -String )
     -- Canned output
----------------------------------------------------------------------*/
/*
För mer variation i output, slumpas olika fraser fram.
Samma för avsluten.
*/
greetings(['Musikspelaren är klar för användning.','Välkommen till musikspelaren']).
byes(['Hej då!','Hoppas att du hade det trevligt','Adjö adjö']).

% getNoXInList(+VilketIOrdning, +Lista, -UtvaltSvar).
getNoNInList(1,[X|_],X).
getNoNInList(Num, [_|Xs], Svar):-
	N is Num-1,
	getNoNInList(N,Xs,Svar).


output_form( action(top), ['toppnivå'] ).

%% Called the first time the program is running
%%
output_form( greet, [Greeting] ):-
	random(1,3,N),
	greetings(List),
	getNoNInList(N,List,Greeting).

output_form( quit, [Ends] ):-
	random(1,4,N),
	byes(List),
	getNoNInList(N,List,Ends).


% ask-moves
output_form( ask(X^(action(X))), ['Vad kan jag göra för dig?'] ).

output_form( ask(action(T)), Str ):-
	output_form(action(T), StrT ),
	append( ['Vill du '], StrT, Str0 ),
	append( Str0, ['?'], Str).


%% ta reda på saker från användaren
output_form( ask(X^playlist(X)),
	     ['Vilken spellista vill du öppna?'] ).
output_form( ask(X^itemAdd(X)),
	     ['Vilken låt vill du lägga till i spellistan?'] ).
output_form( ask(X^itemRem(X)),
	     ['Vilken låt(indexnummer) vill du ta bort från spellistan?'] ).
output_form( ask(X^groupToAdd(X)),
	     ['Vilken grupp är du ute efter?'] ).
output_form( ask(X^station(X)),
	     ['Vilken radiostation vill du lyssna på?'] ).
output_form( ask(X^listenTo(X)),
	     ['Vill du lyssna på radio eller låtar?'] ).
output_form( ask(X^artist(X)),
	     ['Vilken artist menar du?'] ).
output_form( ask(X^song(X)),
	     ['Vilken låt menar du?'] ).
output_form( ask(X^album(X)),
	     ['Vilket album menar du?'] ).
output_form( ask(X^song_artist(X)),
	     ['Vilken artist menar du?'] ).

output_form( ask(X^group(X)),
	     ['Vilken artist menar du?'] ).
output_form( ask(X^item(X)),
	     ['Vilken låt menar du?'] ).

output_form( ask(X^what_to_play(X)),
	     ['Vilken låt i spellistan vill du spela?'] ).

output_form( answer(path(Path)),                Ans ):-
	( Path = ''
	->
	    Ans = ['Det finns ingen sökväg som matchar sökkreterierna.']
	;
	    Ans = ['Sökvägen till låten är:',Path]
	).


output_form( answer(fail(Path^path(Path),no_matches)),                Ans ):-
	Ans = ['Sökvägen till låten är inte detnna:',Path].

output_form( answer(artists_song(Artist)),      ['Följande artist/-er har gjort den:',Artist] ).
output_form( answer(artists_album(Artist)),     Answer ):-
	(
	  Artist = ''
	->
	  Answer = ''
	;
	  (
	    Artist = 'best_of'
	  ->
	    Answer = ['Albumet är ett samlingsalbum']
	  ;
	    Answer = ['Albumet har gjorts av',Artist]
	  )
	).

output_form( answer(albums_by_artist(Albums)),  Answer ):-
	( Albums = ''
	-> Answer = ['Det finns inga album']
	; Answer = ['Följande album finns:',Albums]
	).
output_form( answer(current_song([A,B])),           Answer ):-
	Answer = ['Du lyssnar på',A,'-',B].

output_form( answer(songs_by_artist(Songs)),    ['De har gjort:',Songs] ).



output_form( issue(_^path(_)),            ['fråga väg'] ).
output_form( action(restart),           ['börja om'] ).

output_form( action(handle_player),     ['prata med spelaren'] ).
output_form( action(handle_playlist),   ['ändra i spellistan'] ).
output_form( action(handle_stations),   ['välja radiostationer'] ). 

output_form( action(start),             ['starta spelaren'] ).
output_form( action(start_specific),    ['spela en viss låt'] ).
output_form( action(stop),              ['stoppa spelaren'] ).
output_form( action(pause),             ['pausa musiken'] ).
output_form( action(resume),            ['återuppta musiken'] ).
output_form( action(fast_rewind),       ['spola i låten'] ).
output_form( action(start_playlist),    ['spela en viss spellista'] ).
output_form( action(fast_forward),             ['spola framåt'] ).
output_form( action(rewind),                   ['spola bakåt'] ).
output_form( action(next_song),                ['till nästa'] ).
output_form( action(previous_song),            ['till föregående'] ).

output_form( action(playlist_add),      ['lägga till en låt i spellistan'] ).
output_form( action(playlist_del_specific),      ['ta bort en låt från spellistan'] ).
output_form( action(playlist_del),      ['ta bort spellistan'] ).
output_form( action(playlist_shuffle),  ['blanda ordningen på låtarna'] ).
output_form( action(show_list),                ['visa spellistan'] ).

%%% confirming actions
output_form( confirm(handle_player),    ['klar med att hantera spelaren'] ).
output_form( confirm(handle_playlist),  ['klar med att hantera spellistor'] ).
output_form( confirm(handle_stations),  ['klar med att hantera radiostationer'] ).

output_form( confirm(start),            ['Startar musiken'] ).
output_form( confirm(start_specific),    ['Startar musiken'] ).
output_form( confirm(stop),             ['Musiken är stoppad'] ).
output_form( confirm(pause),            ['Pausar spelaren'] ).
output_form( confirm(resume),           ['Återupptar musiken'] ).
%output_form( confirm(fast_rewind),      ['soplar åt nåt håll'] ).
output_form( confirm(start_playlist),    ['Spelar spellista'] ).
output_form( confirm(fast_forward),     ['Spolar lite framåt'] ).
output_form( confirm(rewind),           ['Spolar lite bakåt'] ).

output_form( confirm(playlist_add),     ['Spellistan är utökad'] ).
output_form( confirm(playlist_del_specific),     ['Spellistan har reducerats'] ).
output_form( confirm(playlist_del),     ['Spellistan är borttagen'] ).
output_form( confirm(playlist_shuffle), ['Spellistans ordning har blandats'] ). 
output_form( confirm(show_list),        ['Spellistan visad'] ).

output_form( confirm(vol_up),           ['Ökar volymen'] ).
output_form( confirm(vol_down),         ['Sänker volymen'] ).
output_form( confirm(next_song),        ['Till nästa låt'] ).
output_form( confirm(previous_song),    ['Till föregående låt'] ).

output_form( report('PlaylistAdd', failed(G,S)), Ans ):-
   	make_name(G,Group),
    	make_name(S,Song),
   	Ans = ['Tyvärr finns inte',Song,'med',Group].

output_form( report('Resume', failed ),
	     ['Spelaren stod på inte på paus, så därför ingen resume'] ).

%%% output_form( report('Start', failed(Status) ), %%% spelare på paus
%%%	     ['Spelaren stod på',Status,'Då måste resume köras'] ).


altlist2altstr_and( [D], Str ):-
	alt2altstr( D, Str1 ),
	append( " och ", Str1, Str ).
altlist2altstr_and( [D|Ds], Str ):-
	alt2altstr( D, Str1 ),
	altlist2altstr_and( Ds, Str2 ),
	append( Str1, ", ", Str3 ),
	append(Str3, Str2, Str ).

altlist2altstr_or( [D], Str ):-
	alt2altstr( D, Str1 ),
	append( " eller ", Str1, Str ).
altlist2altstr_or( [D|Ds], Str ):-
	alt2altstr( D, Str1 ),
	altlist2altstr_or( Ds, Str2 ),
	append( Str1, ", ", Str3 ),
	append(Str3, Str2, Str ).

alt2altstr( D, Str ):-
	output_form( D, Str ).

alt2altstr( D, Str ):-
	name( D, Str ).




%%% used in output_form/2 with ask(set(...))
altlist2alts_or( [Alt], ['eller'|OutputAlt] ):-
	output_form(Alt, OutputAlt ).
altlist2alts_or( [Alt|Alts], [','|Output] ):-
	output_form(Alt, OutputAlt ),
	altlist2alts_or(Alts, AltsOr),
	append( OutputAlt, AltsOr, Output).


% object-level clarification and groundnig questions
output_form( ask(C), Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', är det korrekt?'], Output ).



output_form( ask(set([Alt0|Alts])), Output):-
	output_form(Alt0, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
	append(['Vill du '|Alt0out], AltsOr, Output0 ),
	append(Output0, ['.'], Output).

% output_form( ask(set([Alt0|Alts])), Output):-
% 	output_form(Alt0, Alt0out),
% 	altlist2alts_or( Alts, AltsOr ),
% 	append(['Vill du '|Alt0out], AltsOr, Output0 ),
% 	append(Output0, ['?'], Output).

output_form( Alt, OutputAlt ):-
	input_form( OutputAlt, answer( Alt ) ).


output_form( answer(notexist(X,Q)), [' Ledsen, det finns inget som matchar din fråga om'|InputQDot]):-
	input_form( InputQ, ask(X^Q) ),
	append( InputQ, ['.'], InputQDot ).
output_form( answer(unknown(Q)), ['Ledsen, det finns inget som matchar din fråga om'|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% for asking metaissue clarification question
output_form( issue(Q), ['fråga om'|Out] ):-
	input_form( Out, ask( Q ) ).


% for asking metaissue clarification question
%output_form( action(Action), ['to '|Out] ):-
%	input_form( Out, request( Action ) ).

% ICM

% contact
output_form( icm:con*neg, ['Hallå?'] ).


% perception
output_form( icm:per*int, ['Ursäkta?'] ).
output_form( icm:per*int, ['Vad sa du?'] ).
output_form( icm:per*neg, ['Ursäkta, Jag hörde inte vad du sa.'] ).


output_form( icm:per*pos:String, ['Jag tyckte du sa',Name,'.'] ):-
	name( Name, String ).

output_form( icm:sem*int, ['Vad menar du'] ).
output_form( icm:sem*neg, ['Förlåt, jag förstår inte.'] ).
output_form( icm:sem*pos:Move, InputDot ):-
	input_form( Input, Move ),
	append( Input, ['.'], InputDot ).


% understanding(pragmatic)
output_form( icm:und*neg, ['Jag förstår inte riktigt.']  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% kommer ett helt set??  kolla från generate i output-fönstret i startit...
%%% kolla i generate-modulen vilka icm som finns i repr. där.
output_form( icm:und*pos:usr*issue(G^group(G)),
	    ['Du vill svara på vilken artist'] ).
output_form( icm:und*pos:usr*issue(G^artist(G)),
	    ['Du vill svara på vilken artist'] ).
output_form( icm:und*pos:usr*issue(G^song_artist(G)),
	    ['Du vill svara på vilken artist'] ).

output_form( icm:und*pos:usr*issue(S^song(S)),
	    ['Du vill svara på vilken sång'] ).
output_form( icm:und*pos:usr*issue(S^item(S)),
	    ['Du vill svara på vilken sång'] ).

output_form( icm:und*pos:usr*issue(S^whatToPlay(S)),
	    ['Du vill svara på vilken låt du vill spela'] ).
output_form( icm:und*pos:usr*issue(S^itemRem(S)),
	    ['Du vill svara på vilket indexnummer du ska ta bort'] ).

output_form( icm:und*pos:usr*issue(S^station(S)),
	    ['Du vill svara på vilken radiostation'] ).

output_form( icm:und*pos:usr*issue(S^album(S)),
	    ['Du vill svara på vilket album'] ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


output_form( icm:und*pos:usr*issue(A^play_status(A)),
	     ['Du vill veta vad spelaren gör']  ).

output_form( icm:und*pos:usr*issue(play_status(Status)),
	     ['Du vill veta om videon',S0]  ):-
	status_output(Status,S0).

output_form( icm:und*pos:usr*issue(A^current_channel(A)),
	     ['Du vill veta vilken kanal som är på.'] ).



output_form( icm:und*pos:usr*(not issue(Q)), ['Du frågade inte:'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).


output_form( icm:und*pos:usr*(not P), AnsNotPDot  ):-
	output_form( icm:und*pos:usr*P, AnsPDot  ),
	append( ['inte'],AnsPDot,AnsNotPDot ).

output_form( icm:und*pos:usr*P, AnsPDot  ):-
	( output_form(P, AnsP);
	    input_form( AnsP, answer(P) ) ),
	append(AnsP,['.'],AnsPDot).


% 020702 SL
output_form( icm:und*pos:usr*PX, IcmPos ):-
	PX =.. [P,X],
	isa( P, P1 ),
	P1X =.. [P1,X],
	output_form( icm:und*pos:usr*P1X, IcmPos ).



output_form( icm:und*int:usr*C, IcmInt  ):-
	output_form( ask(C), IcmInt ).
	%output_form( icm:und*pos:C, IcmPos ),
	%append( IcmPos0,['.'],IcmPos),
	%append( IcmPos0, [', is that correct?'], IcmInt ).

%output_form( icm:und*int:usr*C, IcmInt  ):-
%	input_form( answer(C), IcmInt ).


output_form( icm:und*int:usr*C, Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', är det korrekt?'], Output ).




% clarification question
output_form( icm:und*int:usr*AltQ, Output):-
	output_form( ask(AltQ), Output).



% "acceptance"/integration

% icm-Type(-Polarity(-Args))
output_form( icm:acc*pos, ['Okej.'] ).

% reject(issue(Q))
output_form( icm:acc*neg:issue(Q), ['Ledsen, jag kan inte svara på frågor om'|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% reject proposition P
output_form( icm:acc*neg:P, ['Ledsen, '|Rest]):-
	input_form( InputP, answer(P) ),
	append( InputP, [' är inte en korrekt parameter.'], Rest ).

% indicate loading a plan (pushed by findPlan)
%output_form( icm:loadplan, ['I need some information.'] ).
output_form( icm:loadplan, ['Låt oss se.'] ).


% reraise issue explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:Q, ['Gå tillbaks till frågan om '|InputQDot]):-
	( input_form( InputQ, ask(Q) ); output_form( ask(Q), InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise action explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:A, ['Går tillbaks till '|InputQDot]):-
	( input_form( InputQ, request(A) ); output_form( action(A), InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise issue (system-initiated, where question follows immediately after)
output_form( icm:reraise, ['Så,']).

% accommodation
output_form( icm:accommodate:_, ['Visst.']  ).

output_form( icm:reaccommodate:Q, ['Gå tillbaks till frågan om'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).



output_form( not C, ['Inte'|S] ):- output_form( C, S ).


/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/

input_form( [inte|S], answer(not(C))):- input_form(S,answer(C)).

input_form( [ja], answer(yes) ).
input_form( [japp], answer(yes) ).
input_form( [jajamen], answer(yes) ).
input_form( [jajamensan], answer(yes) ).
input_form( [yes ], answer(yes) ).
input_form( [visst], answer(yes) ).
input_form( [nej], answer(no) ).
input_form( [nä], answer(no) ).
input_form( [no ], answer(no) ).

% simple stuff

input_form( [hej], greet ).
input_form( [hejsan], greet ).
input_form( [tjena], greet ).
input_form( [hej,då], quit ).
input_form( [sluta], quit ).
input_form( [avsluta], quit).
%input_form( [avbryt], quita ).

			% ICM

input_form( [förlåt], icm:per*neg ).
input_form( [ursäkta], icm:per*neg ).
input_form( [va], icm:per*neg ).
input_form( [vad,sa,du], icm:per*neg ).
input_form( [jag,hörde,inte], icm:per*neg ).
input_form( [okej], icm:acc*pos ).
input_form( [ok], icm:acc*pos ).
input_form( [visst], icm:acc*pos ).
input_form( [vet, inte], icm:acc*neg:issue ).


/******************************
            ACTIONS
******************************/

%%%%%  Requests  %%%%%
input_form( [börja,om],   request(restart) ).
%%input_form( [top],        request(top) ).
input_form( [toppnivå],   request(top) ).
input_form( [gå,uppåt],   request(up) ).
input_form( [tillbaka],   request(up) ).

%%% för svar på ask(set([...]))
input_form( [prata,med,spelaren],      request(handle_player) ).
input_form( [ändra,i,spellistan],     request(handle_playlist) ).
input_form( [välja,en,radiostation], request(handle_stations) ).
input_form( [starta,spelaren],        request(start) ).
input_form( [spela,en,viss,låt],      request(start_specific) ).
input_form( [stoppa,spelaren],        request(stop) ).
input_form( [pausa,musiken],          request(pause) ).
input_form( [återuppta,musiken],      request(resume) ).
input_form( [spola,i,låten],          request(fast_rewind) ).
input_form( [spela,en,viss,spellista],request(start_playlist) ).
input_form( [spola,framåt],           request(fast_forward) ).
input_form( [spola,bakåt],            request(rewind) ).
input_form( [till,nästa],             request(next_song) ).
input_form( [nästa,låt],             request(next_song) ).
input_form( [spela,nästa],             request(next_song) ).
input_form( [till,föregående],        request(previous_song) ).
input_form( [föregående,låt],        request(previous_song) ).
input_form( [spela,föregående],        request(previous_song) ).
input_form( [lägga,till,en,låt,i,spellistan], request(playlist_add) ).
input_form( [lägg,till,markerad,låt,till,spellista], request(playlist_add) ).
input_form( [ta,bort,en,låt,från,spellistan], request(playlist_del_specific) ).
input_form( [ta,bort,markerad,låt,från,spellistan], request(playlist_del_specific) ).
input_form( [ta,bort,spellistan],             request(playlist_del) ).
input_form( [rensa,spellistan],               request(playlist_del) ).
input_form( [blanda,ordningen,på,låtarna],    request(playlist_shuffle) ).
input_form( [visa,spellistan],                request(show_list) ).
%%% slut på frågealternativ

input_form( [lägga,till,en,låt],      request(playlist_add) ).
input_form( [lägg,till,en,låt],       request(playlist_add) ).

input_form( [lägg,till|Group], [request(playlist_add),answer(group(Sem))]):-
	lexsem(Group,Sem),
	sem_sort(Sem,group).

input_form( [lägg,till|Song], [request(playlist_add),answer(group(Sem))]):-
	lexsem(Song,Sem),
	sem_sort(Sem,item).


input_form( [lägg,till|GroupToPlaylist], [request(playlist_add),answer(group(Sem))]):-
	( ToPlaylist=[till,spellistan];ToPlaylist=[i,spellistan]),
	append(Group,ToPlaylist,GroupToPlaylist),
	lexsem(Group,Sem),
	sem_sort(Sem,group).

input_form( [lägg,till|SongToPlaylist], [request(playlist_add),answer(group(Sem))]):-( ToPlaylist=[till,spellistan];ToPlaylist=[i,spellistan]),
	append(Song,ToPlaylist,SongToPlaylist),
	lexsem(Song,Sem),
	sem_sort(Sem,item).

	
input_form( [Player],     request(handle_player) )     :- lexsem( Player,   player ).
input_form( [Playlist],   request(handle_playlist) )   :- lexsem( Playlist, playlist ).
input_form( [välja],      request(listen_to) ).

input_form( [spela|X],    [request(start_specific),answer(index(X))] ):-
	sem_sort(X,index).
input_form( [spela|Group], [request(start),request(playlist_add),answer(group(Sem))] ):-
	lexsem(Group,Sem),
	sem_sort(Sem,group).
input_form( [spela|Song], [request(start),request(playlist_add),answer(item(Sem))] ):-
	lexsem(Song,Sem),
	sem_sort(Sem,item).

input_form( [Station],    request(handle_stations) )   :- lexsem( Station,  station ). 

input_form( [Play],       request(start) )             :- lexsem( Play,     play ).
%input_form( [Play,nästa],       request(next_song) )          :- lexsem( Play,     play ).

%%% input_form( [Play,Song],      request(play_song) ):-
%%% 	sem_sort(Song,item),
%%% 	lexsem( Play,     play ).

input_form( [Stop],       request(stop) )              :- lexsem( Stop,     stop ).
input_form( [Pause],      request(pause) )             :- lexsem( Pause,    pause ).
input_form( [Resume],     request(resume) )            :- lexsem( Resume,   resume ).
input_form( [spola],      request(fast_rewind) ).
input_form( [bakåt],      request(rewind) ).
input_form( [framåt],     request(fast_forward) ).
input_form( [nästa],      request(next_song) ).
input_form( [föregående], request(previous_song) ).

input_form( [spela,spellista],  request(start_playlist) ).
input_form( [en,spellista],     request(start_playlist) ).
input_form( [lägga,till],       request(playlist_add) ).
input_form( [lägg,till],        request(playlist_add) ).
input_form( [sätta,på],         request(playlist_add) ).
input_form( [visa,listan],      request(show_list) ).
input_form( [visa,spellistan],  request(show_list) ).

input_form( [höra,på],    request(listen_to) ).
input_form( [lyssna,på],  request(listen_to) ).
input_form( [blanda],     request(playlist_shuffle) ).

%input_form( [List],       request(playlist_del) )      :- lexsem(List,list).
%input_form( [låt],        request(playlist_del_specific) ).

%% ny plan som frågar vad man vill ta bort
%input_form( [ta,bort],    request(remove) ).
input_form( [ta,bort,List],   request(playlist_del) )  :- lexsem(List,list).
input_form( [rensa,List],     request(playlist_del) )  :- lexsem(List,list).

input_form( [ta,bort|X],   [request(playlist_del_specific),
			    answer(index(C)) ] ):-
	lexsem(X,C),
	sem_sort(C,index).

input_form( X , answer(index(C)) ):-
	lexsem(X,C),
	sem_sort(C,index).

input_form( [ta,bort,en,låt],   request(playlist_del_specific) ).
input_form( [ta,bort],   request(playlist_del_specific) ).
input_form( [ta,bort,låt],      request(playlist_del_specific) ).
	  
input_form( [Inc],        request(vol_up) )            :- lexsem(Inc,increase).
input_form( [Dec],        request(vol_down) )          :- lexsem(Dec,decrease).



%%%%%  Answers  %%%%%
input_form( X,            answer(index(X)) ):-            sem_sort(X,index).
input_form( Station,      answer(station(Sem)) ):-
	lexsem(Station,Sem),sem_sort(Sem,station).
input_form( Group,        answer(group(Sem)) ):-
	lexsem(Group,Sem),  sem_sort(Sem,group).
input_form( Playlist,     answer(playlist(Playlist)) ):-  sem_sort(Playlist,playlist).
%%input_form( [Year],       answer(year(Year)) ):-          sem_sort(Year,year).
input_form( SongRadio,    answer(item(Sem)) ):-
	lexsem(SongRadio,Sem),sem_sort(Sem,item).
input_form( Album,        answer(album(Album)) ):-        sem_sort(Album,album).
input_form( Station,      answer(station(IP)) ):-
 	longNum(Station,IP),
 	sem_sort(IP,station).

%%%%%  Questions to DB  %%%%%

input_form( [vilka,album],          ask(A^albums_by_artist(A)) ).
input_form( [söka,efter,album],     ask(A^albums_by_artist(A)) ).
input_form( [vad,heter,den],        ask(X^current_song(X)) ).
input_form( [låten,som,spelas,nu],  ask(X^current_song(X)) ).
input_form( [vad,heter,den,här,låten],  ask(X^current_song(X))).
input_form( [vad,heter,denna,låten],  ask(X^current_song(X))).
input_form( [vad,heter,denna,låt],  ask(X^current_song(X))).
input_form( [vilken,låt,är,detta],  ask(X^current_song(X))).
input_form( [vilken,låt,är,det,här],  ask(X^current_song(X))).


input_form( [vem,har,gjort,albumet], ask(A^artists_album(A)) ).
input_form( [vem,har,gjort,låten],   ask(A^artists_song(A)) ).
input_form( [vilken,sökväg],         ask(A^path(A)) ).

%%% för mer explicit input
input_form( [vem,har,gjort,låten|Song],    [ask(A^artists_song(A)),answer(item(Sem))] ):-
	lexsem(Song,Sem),
   	sem_sort(Sem,item).
input_form( [vem,har,gjort,albumet|Album], [ask(A^artists_album(A)),answer(album(Album))] ):-
   	sem_sort(Album,album).

%%% mer generellt
input_form( [vem,har,skrivit|Song],  [ask(A^artists_song(A)),answer(item(Sem))] ):-
	lexsem(Sem,Song),
   	sem_sort(Sem,item).
nput_form( [vem,har,gjort|Song],    [ask(A^artists_song(A)),answer(item(Sem))] ):-	lexsem(Song,Sem),
	sem_sort(Sem,item).
input_form( [vem,har,skrivit|Album], [ask(A^artists_album(A)),answer(album(Album))] ):-
   	sem_sort(Album,album).
input_form( [vem,har,gjort|Album],   [ask(A^artists_album(A)),answer(album(Album))] ):-	
	sem_sort(Album,album).

input_form( [vilka,låtar],           ask(Songs^songs_by_artist(Songs)) ).

%%% input_form( [vilka,grupper],           ask(Groups^all_groups(Groups)) ).


/*

Kommande predikat...
input_form( [vilka,låtar,har,X,gjort], ask(Songs^songs_by_current_artist(Songs)) ):-
 	lexsem(X,ask_current).

input_form( [någonting,med], request(find_group) ).

*/

/*----------------------------------------------------------------------
     yn_answer( ?YN )
----------------------------------------------------------------------*/

yn_answer(A):-
	A = 'ja';
	A = 'nej'.


/*----------------------------------------------------------------------
     lexsem( ?Word, ?Concept )
     -- Lexical semantics
----------------------------------------------------------------------*/

% use semantics as surface forms (only possible for english)
lexsem( Word, Concept ):-
	synset( Words, Concept ),
	member( Word, Words ).

%synset( [[videon],[video]], vcr ).
synset( [spelare,spelaren,spelarn,musiken],                    player ).
synset( [spellista,spellistor,spellistan,spellistorna],        playlist ).
synset( [listor,listan,lista],                                 list ).
synset( [starta,play,plej,spela],                                   play ).
synset( [radio,station,radiostation,stationer,radiostationer], station ).
synset( [[lägg,till],[lägga,till]],                            add ).
synset( [stop,stopp,stoppa,stanna],                            stop ).
synset( [resume,risyum,återuppta],                                    resume ).
synset( [paus,pausa,pås],                                          pause ).

synset( [höj,höja,öka],                                        increase ).
synset( [sänk,sänka,minska],                                   decrease ).

synset( [han,hon,de,dem,dom],                                  ask_current ).

%index
synset( [[nästa]], next).
synset( [[föregående]], previous).
synset( [[ett]], 1).
synset( [[två]], 2).
synset( [[tre]], 3).
synset( [[fyra]], 4).
synset( [[fem]], 5).
synset( [[sex]], 6).
synset( [[sju]], 7).
synset( [[åtta]], 8).
synset( [[nio]], 9).
synset( [[tio]], 10).
synset( [[elva]],11).
synset( [[tolv]],12).
synset( [[tretton]],13).


synset([[wilmer,x]],wilmer_x).
synset([[uno,svenningsson]],uno_svenningsson).
synset([[ulf,lundell]],ulf_lundell).
synset([[tomas,ledin]],tomas_ledin).
synset([[tomas,ledin]],tomas_ledin).
synset([[thomas,di,leva]],thomas_di_leva).
synset([[staffan,hellstrand]],staffan_hellstrand).
synset([[petter]],petter).
synset([[peter,lemarc]],peter_lemarc).
synset([[peter,lemarc]],peter_lemarc).
synset([[patrik,isaksson]],patrik_isaksson).
synset([[orup]],orup).
synset([[monica,törnell]],monica_törnell).
synset([[mikael,wiehe]],mikael_wiehe).
synset([[mikael,rickfors]],mikael_rickfors).
synset([[mauro,scocco]],mauro_scocco).
synset([[mauro,scocco]],mauro_scocco).
synset([[marie,fredriksson]],marie_fredriksson).
synset([[lustans,lakejer]],lustans_lakejer).
synset([[lisa,nilsson]],lisa_nilsson).
synset([[lisa,ekdahl]],lisa_ekdahl).
synset([[lars,winnerbäck]],lars_winnerbäck).
synset([[kent]],kent).
synset([[jakob,hellman]],jakob_hellman).
synset([[irma]],irma).
synset([[imperiet]],imperiet).
synset([[gyllene,tider]],gyllene_tider).
synset([[freda]],freda).
synset([[eva,dahlgren]],eva_dahlgren).
synset([[eva,dahlgren]],eva_dahlgren).
synset([[eldkvarn]],eldkvarn).
synset([[ebba,grön]],ebba_grön).
synset([[docent,död]],docent_död).
synset([[christer,sandelin]],christer_sandelin).
synset([[bo,kaspers,orkester]],bo_kaspers_orkester).
synset([[annelie,ryde]],annelie_ryde).
synset([[adolphson,och,falk]],adolphson_och_falk).

synset([[teknikens,under]],teknikens_under).
synset([[under,ytan]],under_ytan).
synset([[öppna,landskap]],öppna_landskap).
synset([[sommaren,är,kort]],sommaren_är_kort).
synset([[en,del,av,mitt,hjärta]],en_del_av_mitt_hjärta).
synset([[vem,skall,jag,tro,på]],vem_skall_jag_tro_på).
synset([[lilla,fågel,blå]],lilla_fågel_blå).
synset([[vinden,har,vänt]],vinden_har_vänt).
synset([[säg,som,det,är]],säg_som_det_är).
synset([[håll,om,mig]],håll_om_mig).
synset([[du,får,göra,som,du,vill]],du_får_göra_som_du_vill).
synset([[jag,blir,hellre,jagad,av,vargar]],jag_blir_hellre_jagad_av_vargar).
synset([[vintersaga]],vintersaga).
synset([[flickan,och,kråkan]],flickan_och_kråkan).
synset([[vingar]],vingar).
synset([[sarah]],sarah).
synset([[det,finns]],det_finns).
synset([[efter,stormen]],efter_stormen).
synset([[diamanter]],diamanter).
synset([[himlen,runt,hörnet]],himlen_runt_hörnet).
synset([[vem,vet]],vem_vet).
synset([[kom,ihåg,mig]],kom_ihåg_mig).
synset([[om,du,var,här]],om_du_var_här).
synset([[vara,vänner]],vara_vänner).
synset([[precis,som,du]],precis_som_du).
synset([[du,ska,va,president]],du_ska_va_president).
synset([[flickorna,på,tv,två]],flickorna_på_tv_två).
synset([[vindarna]],vindarna).
synset([[ängeln,i,rummet]],ängeln_i_rummet).
synset([[vem,tänder,stjärnorna]],vem_tänder_stjärnorna).
synset([[kärlekens,tunga]],kärlekens_tunga).
synset([[åtta,hundra,grader]],åtta_hundra_grader).
synset([[solglasögon]],solglasögon).
synset([[det,hon,vill,ha]],det_hon_vill_ha).
synset([[undantag]],undantag).
synset([[segla,på,ett,moln]],segla_på_ett_moln).
synset([[blinkar,blå]],blinkar_blå).






