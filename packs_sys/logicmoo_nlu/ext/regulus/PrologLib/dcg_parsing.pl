
:- use_module(library(lists)).
:- use_module(library(system)).

:- module(dcg_parsing,
	  [pulist_line/3,
	   county_line/3,
	   place_line/3,
	   missing_line/3,
	   phonetic_lexicon_line/4,
	   web_hit_line/3,
	   places_wavfiles_line/3,
	   batchrec_line/3,
	   word_list_line/3,
	   zozma_server_message_line/3,

	   alphanum_char/1,
	   digit_char/1,
	   white_space_char/1,
	   space_char/1,
	   nonspace_char/1,
	   non_whitespace_char/1,
	   newline_char/1,
	   tab_char/1
       ]
   ).

zozma_server_message_line((Key = Value)) -->
	zozma_server_key_expr(Key),
	"=",
	zozma_server_value_expr(Value),
	!.

zozma_server_key_expr(run) -->
	"run",
	!.
zozma_server_key_expr([Arg, Index1, Index2]) -->
	zozma_server_arg_expr(Arg),
	zozma_server_index_expr(Index1),
	zozma_server_index_expr(Index2),
	!.
zozma_server_key_expr([Arg, Index1]) -->
	zozma_server_arg_expr(Arg),
	zozma_server_index_expr(Index1),
	!.
zozma_server_key_expr([Arg]) -->
	zozma_server_arg_expr(Arg).

zozma_server_arg_expr(arg(N)) -->
	"arg_",
	integer_word(N).
zozma_server_arg_expr(return_to_server) -->
	"return_to_server".

zozma_server_index_expr(index(Index1)) -->
	"[",
	integer_or_word(Index1),
	"]",
	!.
zozma_server_index_expr(index(Index1)) -->
	".",
	integer_or_word(Index1),
	!.	

zozma_server_value_expr(Value) -->
	integer_or_word(Value).

integer_or_word(Int) -->
	integer_word(Int),
	!.
integer_or_word(Word) -->
	non_equals_word(Word).

%------------------------------------------------------------------------------------

word_list_line(Words) -->
	white_spaces,
	word_list(Words),
	white_spaces.

pulist_line(process_and_pid(Process, Pid)) -->
	non_whitespace_word(Process0), {lowercase_atom(Process0, Process)},	    
	white_spaces_including_tabs,
	integer_word(Pid), 
	arbitrary_sequence.

county_line(county(Abbrev, County)) -->
	word(Abbrev),
	tab_preceded_by_whitespaces,
	word_list(County),
	white_spaces.

place_line(place(Place, CountyAbbrev)) -->
	white_spaces,
	word_list_joined_by_spaces_or_parens(Place),
	tab_surrounded_by_whitespaces,
	word(CountyAbbrev),
	white_spaces.

missing_line(missing_word(Word)) -->
	word(Word).

phonetic_lexicon_line(lexicon(Word, Entry, Tag), Tag) -->
	word(Word),
	white_spaces_including_tabs,
	word_list(Entry),
	white_spaces_including_tabs.

web_hit_line(web_hits(Word, Freq)) -->
	word(Word),
	",",
	integer_word(Freq).

places_wavfiles_line(discard) -->
	[].
places_wavfiles_line(discard) -->
	"mkdir places",
	white_spaces,
	integer_word(_),
	white_spaces.
places_wavfiles_line(place_file(TownName, FullFileName)) -->
	"places",
	white_spaces,
	integer_word(Index),
	white_spaces,
	word_list(TownName),
	{join_with_underscore(TownName, FileName),
	 join_with_underscore([places, Index], DirectoryName),
	 append_atoms([DirectoryName, FileName], 0'\\, FullFileName)}.
	
%------------------------------------------------------------------------------------

% File 30: swe_logfiles/2000/09September/28/17-31-07-ukcamdv09-Microsoft_Sound_Mapper/utt07.wav

batchrec_line(wavfile(N, Wavfile)) -->
	"File",
	!,
	spaces,
	integer_word(N),
	":",
	spaces,
	non_space_word(Wavfile).

% Grammar: .Utterance

batchrec_line(discard(grammar)) -->
	"Grammar",
	!,
	arbitrary_sequence.

% Transcription: sätt på spisen

batchrec_line(transcription(Transcription)) -->
	"Transcription:",
	!,
	spaces,
	transcription_words(Transcription).

% NL Transcript: <device1 "spis"> <onoff_direction "på"> <onoff_level 100> <operation "command"> <spec1 "det">

batchrec_line(nl_transcription(Slots)) -->
	"NL Transcript:",
	!,
	spaces,
	slot_value_list(Slots).

% Result #0:     sätt på spisen (conf: 59, NL conf: 61)

batchrec_line(result_confidence_nlconf(N, Rec, Conf, NLConf)) -->
	"Result #",
	!,
	integer_word(N),
	":",
	!,
	spaces,
	rec_result_list(Rec),
	spaces,
	"(conf:",
	spaces,
	integer_word(Conf),
	", NL conf:",
	spaces,
	integer_word(NLConf),
	")".

% NL Res.#0:     <device1 "spis"> <onoff_direction "på"> <onoff_level 100> <operation "command"> <spec1 "det">
% NL Res.#0[0]:     <choice "A1">

batchrec_line(nl_result(N, Slots)) -->
	"NL Res.#",
	integer_word(N),
	(   ":" ;
	    "[", integer_word(_Alterative), "]:"
	),
	!,
	spaces,
	slot_value_list(Slots).	

% Total Audio: 1.63 sec

batchrec_line(discard(total_audio)) -->
	"Total Audio:",
	!,
	arbitrary_sequence.

% Utt. Times: 0.43 secs 0.264xRT (0.42 usr 0.00 sys 0.257xcpuRT) 97%cpu

batchrec_line(discard(utt_times)) -->
	"Utt. Times:",
	!,
	arbitrary_sequence.	

% Ave. Times: 0.36 secs 0.160xRT (0.35 usr 0.00 sys 0.155xcpuRT) 96%cpu

batchrec_line(discard(ave_times)) -->
	"Ave. Times:",
	!,
	arbitrary_sequence.	

% Rec Errors: 0 ins, 0 del, 0 sub = 0.00% of 3 words.

batchrec_line(rec_errors(Ins, Del, Sub)) -->
	"Rec Errors:",
	!,
	spaces,
	integer_word(Ins),
	spaces,
	"ins,",
	spaces,
	integer_word(Del),
	spaces,
	"del,",
	spaces,
	integer_word(Sub),
	spaces,
	"sub",
	arbitrary_sequence.

% Rec Total:  2 ins, 1 del, 2 sub = 5.56% of 90 words (6.67% of 30 files).

batchrec_line(discard(rec_total)) -->
	"Rec Total:",
	!,
	arbitrary_sequence.	

% NL Status:  correct

batchrec_line(discard(nl_status)) -->
	"NL Status:",
	!,
	arbitrary_sequence.	

% NL Total:   0 rejects, 2 incorrect = 6.67% error on 30 files.

batchrec_line(discard(nl_total)) -->
	"NL Total:",
	!,
	arbitrary_sequence.	

batchrec_line(discard(X)) -->
	arbitrary_sequence(X),
	!.

batchrec_line(discard(empty)) -->
	[].

%------------------------------------------------------------------------------------

transcription_words(Transcription) -->
	word_list(Transcription).
transcription_words([no_transcription]) -->
	"(none)",
	spaces.

slot_value_list([F|R]) -->
	slot(F),
	!,
	spaces,
	slot_value_list(R).
slot_value_list([]) --> 
	[].
slot_value_list([]) -->
	"(none)",
	spaces.
slot_value_list([rejected]) -->
	"<rejected>",
	spaces.

slot(slot(SlotName, Value)) -->
	"<",
	word(SlotName),
	spaces,
	slot_value(Value),
	">",
	!.

slot_value(Value) -->
	"""",
	non_inverted_comma_word(Value),
	"""",	
	!.
slot_value(Value) -->
	word(Value),
	!.
slot_value(Value) -->
	integer_word(Value).

rec_result_list([rejected]) -->	
	"<rejected>".
rec_result_list(Rec) -->
	word_list(Rec).

non_space_word(Word) -->
	nonspace_char_list(L), {L \== [], atom_chars(Word, L)}.

nonspace_char_list([F|R]) -->
	[F], {nonspace_char(F)},
	!,
	nonspace_char_list(R).
nonspace_char_list([]) --> [].

non_whitespace_word(Word) -->
	non_whitespace_char_list(L), {L \== [], atom_chars(Word, L)}.

non_whitespace_char_list([F|R]) -->
	[F], {non_whitespace_char(F)},
	!,
	non_whitespace_char_list(R).
non_whitespace_char_list([]) --> [].	

non_inverted_comma_word(Word) -->
	noninverted_comma_char_list(L), {L \== [], atom_chars(Word, L)}.

noninverted_comma_char_list([F|R]) -->
	[F], {dif(F, 0'")},
	!,
	noninverted_comma_char_list(R).
noninverted_comma_char_list([]) --> [].

non_equals_word(Word) -->
	non_equals_char_list(L), {L \== [], atom_chars(Word, L)}.

non_equals_char_list([F|R]) -->
	[F], {non_equals_char(F)},
	!,
	non_equals_char_list(R).
non_equals_char_list([]) --> [].	

word(Word) -->
	alphanum_char_list(L), {L \== [], atom_chars(Word, L)}.

alphanum_char_list([F|R]) -->
	[F], {alphanum_char(F)},
	!,
	alphanum_char_list(R).
alphanum_char_list([]) --> [].

word_or_parenthesized_word_list(List) -->
	parenthesized_word_list(List),
	!.
word_or_parenthesized_word_list(Word) -->
	word(Word).

word_or_word_list(List) -->
	word_list(List),
	!.
word_or_word_list(Word) -->
	word(Word).

parenthesized_word_list(List) -->
	"(",
	word_list(List),
	")".

word_list_joined_by_hyphens([F|R]) -->
	word(F),
	!,
	word_list_joined_by_hyphens_rest(R).

word_list_joined_by_hyphens_rest(R) -->
	"-",
	!,
	word_list_joined_by_hyphens(R).
word_list_joined_by_hyphens_rest([]) -->
	[].

word_list([F|R]) -->
	word(F),
	!,
	white_spaces,
	word_list(R).
word_list([]) -->
	[].

word_list_joined_by_spaces_or_parens([F|R]) -->
	word(F),
	!,
	white_spaces_including_parens,
	word_list_joined_by_spaces_or_parens(R).
word_list_joined_by_spaces_or_parens([]) -->
	[].

integer_word(Word) -->
	digit_char_list(L), {L \== [], number_chars(Word, L)}.

digit_char_list([F|R]) -->
	[F], {digit_char(F)},
	!,
	digit_char_list(R).
digit_char_list([]) --> [].

white_spaces -->
	[Char], {white_space_char(Char)},
	!,
	white_spaces.
white_spaces -->
	[].    

white_spaces_including_parens -->
	[Char], {white_space_char(Char) ; parenthesis_char(Char)},
	!,
	white_spaces_including_parens.
white_spaces_including_parens -->
	[].    

white_spaces_including_tabs -->
	[Char], {white_space_char(Char) ; tab_char(Char)},
	!,
	white_spaces_including_tabs.
white_spaces_including_tabs -->
	[].    

tab_surrounded_by_whitespaces -->
	white_spaces,
	[TabChar], {tab_char(TabChar)},
	white_spaces.

tab_preceded_by_whitespaces -->
	white_spaces,
	[TabChar], {tab_char(TabChar)}.

spaces -->
	[Char], {space_char(Char)},
	!,
	spaces.
spaces -->
	[].    

arbitrary_sequence -->
	[_Char], 
	!,
	arbitrary_sequence.
arbitrary_sequence -->
	[].    

arbitrary_sequence(Word) -->
	char_list(L), {L \== [], atom_chars(Word, L)}.

char_list([F|R]) -->
	[F], 
	!,
	char_list(R).
char_list([]) --> [].

%------------------------------------------------------------------------------------

lowercase_atom_list([], []).
lowercase_atom_list([F|R], [F1|R1]) :-
    lowercase_atom(F, F1),
    !,
    lowercase_atom_list(R, R1).

uppercase_atom(A, A1) :-
    atom_chars(A, S),
    uppercase_string(S, S1),
    atom_chars(A1, S1).

uppercase_string([],[]).
uppercase_string([F|R], [F1|R1]) :-
    uppercase_char(F, F1),
    !,
    uppercase_string(R, R1).

uppercase_char(C, C1) :-
    lowercase_uppercase(C, C1),
    !.
uppercase_char(C, C).

lowercase_atom(A, A1) :-
    atom_chars(A, S),
    lowercase_string(S, S1),
    atom_chars(A1, S1).

lowercase_string([],[]).
lowercase_string([F|R], [F1|R1]) :-
    lowercase_char(F, F1),
    !,
    lowercase_string(R, R1).

lowercase_char(C, C1) :-
    lowercase_uppercase(C1, C),
    !.
lowercase_char(C, C).

lowercase_uppercase(0'a, 0'A).
lowercase_uppercase(0'b, 0'B).
lowercase_uppercase(0'c, 0'C).
lowercase_uppercase(0'd, 0'D).
lowercase_uppercase(0'e, 0'E).
lowercase_uppercase(0'f, 0'F).
lowercase_uppercase(0'g, 0'G).
lowercase_uppercase(0'h, 0'H).
lowercase_uppercase(0'i, 0'I).
lowercase_uppercase(0'j, 0'J).
lowercase_uppercase(0'k, 0'K).
lowercase_uppercase(0'l, 0'L).
lowercase_uppercase(0'm, 0'M).
lowercase_uppercase(0'n, 0'N).
lowercase_uppercase(0'o, 0'O).
lowercase_uppercase(0'p, 0'P).
lowercase_uppercase(0'q, 0'Q).
lowercase_uppercase(0'r, 0'R).
lowercase_uppercase(0's, 0'S).
lowercase_uppercase(0't, 0'T).
lowercase_uppercase(0'u, 0'U).
lowercase_uppercase(0'v, 0'V).
lowercase_uppercase(0'w, 0'W).
lowercase_uppercase(0'x, 0'X).
lowercase_uppercase(0'y, 0'Y).
lowercase_uppercase(0'z, 0'Z).

%------------------------------------------------------------------------------------

alphanum_char(0'a).
alphanum_char(0'b).
alphanum_char(0'c).
alphanum_char(0'd).
alphanum_char(0'e).
alphanum_char(0'f).
alphanum_char(0'g).
alphanum_char(0'h).
alphanum_char(0'i).
alphanum_char(0'j).
alphanum_char(0'k).
alphanum_char(0'l).
alphanum_char(0'm).
alphanum_char(0'n).
alphanum_char(0'o).
alphanum_char(0'p).
alphanum_char(0'q).
alphanum_char(0'r).
alphanum_char(0's).
alphanum_char(0't).
alphanum_char(0'u).
alphanum_char(0'v).
alphanum_char(0'w).
alphanum_char(0'x).
alphanum_char(0'y).
alphanum_char(0'z).
alphanum_char(0'å).
alphanum_char(0'ä).
alphanum_char(0'ö).
alphanum_char(0'A).
alphanum_char(0'B).
alphanum_char(0'C).
alphanum_char(0'D).
alphanum_char(0'E).
alphanum_char(0'F).
alphanum_char(0'G).
alphanum_char(0'H).
alphanum_char(0'I).
alphanum_char(0'J).
alphanum_char(0'K).
alphanum_char(0'L).
alphanum_char(0'M).
alphanum_char(0'N).
alphanum_char(0'O).
alphanum_char(0'P).
alphanum_char(0'Q).
alphanum_char(0'R).
alphanum_char(0'S).
alphanum_char(0'T).
alphanum_char(0'U).
alphanum_char(0'V).
alphanum_char(0'W).
alphanum_char(0'X).
alphanum_char(0'Y).
alphanum_char(0'Z).
alphanum_char(0'Å).
alphanum_char(0'Ä).
alphanum_char(0'Ö).
alphanum_char(0'1).
alphanum_char(0'2).
alphanum_char(0'3).
alphanum_char(0'4).
alphanum_char(0'5).
alphanum_char(0'6).
alphanum_char(0'7).
alphanum_char(0'8).
alphanum_char(0'9).
alphanum_char(0'0).
alphanum_char(0'_).
% Following required for Nuance phone set...
alphanum_char(0'&).
alphanum_char(0'').
alphanum_char(0'*).
alphanum_char(0'^).
alphanum_char(0'~).
alphanum_char(0'@).
alphanum_char(0'?).

digit_char(0'1).
digit_char(0'2).
digit_char(0'3).
digit_char(0'4).
digit_char(0'5).
digit_char(0'6).
digit_char(0'7).
digit_char(0'8).
digit_char(0'9).
digit_char(0'0).

white_space_char(0' ).
white_space_char(0'-).
white_space_char(0'").
white_space_char(0',).
white_space_char(0'$).
white_space_char(0'!).
%white_space_char(0'().
%white_space_char(0')).
%white_space_char(0'').
white_space_char(0'.).
white_space_char(0'/).
%white_space_char(0'\t).

standard_white_space_char(0' ).
standard_white_space_char(0'\t).

asterisk_char(0'*).

space_char(0' ).

equals_char(0'=).

nonspace_char(X) :-
	\+ space_char(X).

non_whitespace_char(X) :-
	\+ standard_white_space_char(X).

non_equals_char(X) :-
	\+ equals_char(X).

parenthesis_char(0'().
parenthesis_char(0')).

newline_char(0'\n).

tab_char(0'\t).

