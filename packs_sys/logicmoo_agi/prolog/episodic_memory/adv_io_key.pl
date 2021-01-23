/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% Copyright (C) 2004 Marty White under the GNU GPL
% Sept 20, 1999 - Douglas Miles
% July 10, 1996 - John Eikenberry
%
% Logicmoo Project changes:
%
% Main file.
%
*/
% Marty's Tokenizer/Scanner/Lexer, written in Prolog.
:- module(adv_io_key, [


 wait_for_key/0,
     wait_for_key/1,
     keyboard_init/0,
 pprint/2,
 init_logging/0,
 stop_logging/0,
 bug/1,
 agent_to_input/2,
 agent_to_output/2,
 get_overwritten_chars/2,
 restore_overwritten_chars/1,
 %setup_console/0,
 setup_console/1,
 current_error/1 % , set_error/1
 ]).

:- dumpST.
:- throw(dont_use_this_file(adv_io_key)).

mutex_create_safe(M):- notrace(catch(mutex_create(M), _, true)).

messages_init:-
 mutex_create_safe(messages).

int_or_var(V):- integer(V);var(V).

:- dynamic(adv_io_temp:adv_flag_val/2).

adv_flag(N, O, V):- int_or_var(O), int_or_var(V), !, flag(N, O, V).
adv_flag(N, O, V):- clause(adv_io_temp:adv_flag_val(N, W), true, Ref), !, O=W,
  erase(Ref), assert(adv_io_temp:adv_flag_val(N, V)).
adv_flag(N, _, V):- assert(adv_io_temp:adv_flag_val(N, V)).

adv_flag(N, V):-
 adv_flag(N, _, V).

% FIXME - word wrap
% post_message(M):-
% atom_length(M, N), N>72
post_message(M):-
 with_mutex(messages, (post_message_int(M),
      adv_flag(unacked_messages, _, 1))),
 nop(request_screen_update(0, 0, 1, 80)).

post_message(F, L):-
 atom(A) = AtomStream,
 format(AtomStream, F, L), post_message(A).

post_message_int(M):-
 adv_flag(line0, '', M),
 atom_length(M, L),
 adv_flag(requested_cursor_row, _, 0),
 adv_flag(requested_cursor_col, _, L), !.
post_message_int(M):-
 \+ (more_prompt),
 adv_flag(line0, OL),
 atom_length(OL, OLL),
 atom_length(M, ML),
 OLL+ML=<70,
 concat_atom([OL, ' ', M], NL),
 adv_flag(line0, _, NL),
 atom_length(NL, Len),
 adv_flag(requested_cursor_row, _, 0),
 adv_flag(requested_cursor_col, _, Len), !.
post_message_int(M):-
 more_prompt,
 recordz(messages, M), !.
post_message_int(M):-
 adv_flag(more_prompt, _, 1),
 adv_flag(line0, OL),
 atom_concat(OL, ' [More]', NL),
 adv_flag(line0, _, NL), !,
 post_message_int(M).

more_prompt:-adv_flag(more_prompt, 1).

ack_messages:-adv_flag(unacked_messages, 0).
ack_messages:-
 with_mutex(messages, (
  adv_flag(line0, _, ''),
  adv_flag(unacked_messages, _, 0),
  adv_flag(more_prompt, _, 0),
  adv_flag(requested_cursor_row, _, 0),
  adv_flag(requested_cursor_col, _, 0),
  gather_messages(L),
  (L=[];adv_flag(unacked_messages, _, 1), resend_messages(L))
 )),
 nop(request_screen_update(0, 0, 1, 80)).

gather_messages([H|T]):-
 recorded(messages, H, R),
 erase(R),
 !, gather_messages(T).
gather_messages([]).

resend_messages([H|T]):-
 post_message_int(H), !, resend_messages(T).
resend_messages([]).

/*
sv_message(S, V):-
 care_about(S),
 sconj(S, SC),
 vconj(S, V, VC),
 post_message('~w ~w.', [SC, VC]), !.
sv_message(_, _).

svo_message(S, V, O):-
 care_about(S), care_about(O),
 sconj(S, SC),
 vconj(S, V, VC),
 oconj(S, O, OC),
 post_message('~w ~w ~w.', [SC, VC, OC]), !.
svo_message(S, V, _):-
 care_about(S), % not care_about(O),
 sconj(S, SC),
 vconj(S, V, VC),
 post_message('~w ~w it.', [SC, VC]), !.
svo_message(_, V, O):-
 care_about(O), % not care_about(S),
 vconj(it, V, VC),
 oconj(it, O, OC),
 post_message('It ~w ~w.', [VC, OC]), !.
svo_message(_, _, _).

svi_message(S, V, I):-
 care_about(S),
 sconj(S, SC),
 vconj(S, V, VC),
 post_message('~w ~w ~w.', [SC, VC, I]), !.
svi_message(_, _, _).

svoi_message(S, V, O, I):-
 care_about(S), care_about(O),
 sconj(S, SC),
 vconj(S, V, VC),
 oconj(S, O, OC),
 post_message('~w ~w ~w ~w.', [SC, VC, OC, I]), !.
svoi_message(S, V, _, I):-
 care_about(S), % not care_about(O),
 sconj(S, SC),
 vconj(S, V, VC),
 post_message('~w ~w it ~w.', [SC, VC, I]), !.
svoi_message(_, V, O, I):-
 care_about(O), % not care_about(S),
 vconj(it, V, VC),
 oconj(it, O, OC),
 post_message('It ~w ~w ~w.', [VC, OC, I]), !.
svoi_message(_, _, _, _).

care_about(player).
care_about(O):-
 attribute(player-dungeon, D), attribute(O-dungeon, D),
 attribute(player-level, L), attribute(O-level, L),
 attribute(O-row, R), attribute(O-column, C),
 visible(R, C),
log_stuff('viz~n', []).
*/

:-dynamic key_translate/2.

%:-include(gheader).

%:-use_module(screen).
%:-use_module(messages).

% FIXME dynamify key translations

key_translate(['\014\'], redraw).  % '

key_translate(['\033\', '[', 'A'], up).  % '
key_translate(['\033\', '[', 'B'], down). % '
key_translate(['\033\', '[', 'C'], right). % '
key_translate(['\033\', '[', 'D'], left). % '

key_translate(['\033\', '[', '5', '~'], page_up).     % '
key_translate(['\033\', '[', '6', '~'], page_down).  % '

key_translate(['\033\', K], meta(K)):-char_type(K, alnum). %'

keyboard_thread(Buffer):-
 set_stream(user_input, buffer(none)),
 get_single_char(Code), char_code(Key, Code),
 append(Buffer, [Key], NB),
 !, process_buffer(NB).

process_buffer([]):-!, keyboard_thread([]).
process_buffer(NB):-
 key_translate(NNB, _),
 append(NB, X, NNB), X\=[],
 !, keyboard_thread(NB).
process_buffer(NB):-
 append(NBA, NBB, NB),
 key_translate(NBA, Key),
% log_stuff('got key ~w.~n', [Key]),
 global_key_hook(Key),
 !, process_buffer(NBB).
process_buffer([K|T]):-
 nop(log_stuff('got key ~w.~n', [K])), global_key_hook(K), !, process_buffer(T).
/*
global_key_hook(redraw):-!, request_screen_update(redraw).
global_key_hook(meta(hhh)):-!,
 attribute(player-hit_points, H),
 HH is H+random(6)+1,
 attribute(player-hit_points, _, HH),
 request_screen_update(23, 0, 1, 10).
global_key_hook(meta(t)):-!, threads.
global_key_hook(meta(l)):-
 (dungeon:player_dungeon(R, C, A, B),
 log_stuff('~w~n', [player_dungeon(R, C, A, B)]), fail;true), !.
*/
keystrokes_thread_name(keystrokes).

global_key_hook(Key):-
 keystrokes_thread_name(Keystrokes),
 with_mutex(messages, (
  nop(more_prompt), (Key=' ', nop(ack_messages);true);
  nop(ack_messages), thread_send_message(Keystrokes, Key)
 )), !.

keyboard_init:-
 keystrokes_thread_name(Keystrokes),
 (message_queue_property(_, alias(Keystrokes))->true;message_queue_create(Keystrokes)),
 thread_create(keyboard_thread([]), _, []).

wait_for_key(Key):-
 keystrokes_thread_name(Keystrokes),
 thread_get_message(Keystrokes, Key).

wait_for_key:-wait_for_key(_).

