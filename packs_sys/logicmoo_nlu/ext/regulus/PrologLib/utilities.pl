% utilities.pl

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(utilities,
	  [safe_directory_exists/1,
	   safe_nth/3,
	   safe_working_directory/2,
	   safe_mod_time_for_file/2,
	   safe_remove_duplicates/2,
	   safe_remove_duplicates_preserving_order/2,
	   safe_abolish/1,
	   safe_subsumes_chk/2,
	   safe_print_message/2,
	   %print_message_work_around/2,
	   safe_sum_list/2,
	   safe_max_list/2,
	   safe_min_list/2,
	   is_contiguous_sublist/2,
	   safe_prefix/2,
	   safe_suffix/2,	   
	   safe_trimcore/0,
	   sleep_if_sicstus4_and_using_gui/0,
	   safe_close_all_streams/0,
	   safe_process_id/1,
	   safe_exec/3,
	   safe_shell/1,
	   safe_shell/2,
	   safe_socket/2,
	   safe_socket_connect/4,
	   safe_socket_connect/5,
	   safe_socket_close/1,
	   safe_socket_server_open/2,
	   safe_socket_server_close/1,
	   safe_socket_server_accept/3,
	   safe_socket_server_accept/4,
	   safe_random_member/2,
	   safe_format_to_chars/3,
	   safe_read_from_chars/2,
	   safe_read_from_chars/3,

	   safe_read/2,
	   safe_close/1,

	   safe_average_list/2,

	   is_assoc_generic/1,
	   empty_assoc_generic/1,
	   assoc_generic_to_list/2,
	   list_to_assoc_generic/2,
	   get_assoc_generic/3,
	   put_assoc_generic/4,
	   min_assoc_generic/3,
	   get_next_assoc_generic/4,

	   require_sicstus_version/2,
	   running_under_windows/0,
	   nuance10_loaded/0,
	   on_cygwin_and_bin_dir_found/0,
	   find_sicstus_encoding_for_file/2,

	   replace_strings_with_atoms_in_xml/2,
	   remove_comments_in_xml/2,
	   set_random_generator_state_from_time/0,
	   safe_number_codes/2,
	   timeouts_unavailable/0,
	   disable_timeouts/0,
	   abolish_if_defined/1,
	   set_compile_timeout/1,
	   safe_compile/2,
	   safe_compile_list/2,
	   safe_compile_with_redefine_warnings_off/2,
	   safe_compile_with_discontiguous_warnings_off/2,
	   safe_compile_with_redefine_and_single_var_warnings_off/2,
	   safe_absolute_file_name/2,
	   safe_rename_file/2,
	   safe_rename_file_using_mv_if_possible/2,
	   safe_directory_files/2,
	   safe_is_directory/1,
	   directory_files_recursive/2,
	   safe_file_exists/1,
	   get_windows_ps_info/1,
	   exec_and_check_process_started/3,
	   kill_process/1,
	   consume_parameter_value_pair/3,
	   current_datime_formatted_as_HMS/1,
	   datime_for_directory/3,
	   datime_for_directory/4,
	   datime_for_directory_recursive/3,
	   datime_for_directory_recursive/4,
	   age_of_file/2,
	   file_is_more_recent_than_files/2,
	   order_files_by_age_most_recent_first/2,
	   datime_for_file/2,
	   datime_for_file_list/3,
	   real_datime/1,
	   earlier_datime/2,
	   datime_to_timestamp/2,
	   parse_datime/2,
	   set_notional_time/1,
	   unset_notional_time/0,
	   get_notional_time/1,
	   set_notional_speaker/1,
	   unset_notional_speaker/0,
	   get_notional_speaker/1,
	   atom_to_int/2,
	   is_number_atom/1,
	   coerce_atom_to_int/2,
	   coerce_int_to_atom/2,
	   timestamped_file/4,
	   next_numbered_file/3,
	   coerce_int_to_two_digit_atom/2,
	   coerce_int_to_three_digit_atom/2,
	   length_of_wavfile_in_seconds/2,
	   wavfile_format_and_encoding/3,
	   wavconvert_file/4,
	   get_transcription_from_wavfile/2,
	   add_transcription_to_wavfile/3,
	   add_transcription_to_wavfile_binary/3,
	   sort_file/2,
	   %get_pulist/1,
	   existing_absolute_file_name/2,
	   %files_in_directory/3,
	   %get_files_in_directory/2,
	   copy_file_between_directories/3,
	   %delete_all_files_in_directory/1,
	   delete_all_files_in_list/1,
	   delete_file_with_status/2,
	   pathname_has_extension/2,
	   split_off_extension_from_pathname/3,
	   change_extension_in_file/3,
	   derived_file_names/5,
	   escape_spaces_in_atom/2,
	   unescape_spaces_in_atom/2,
	   system_on_list/1,
	   system_on_list_to_result/2,
	   shell_writing_output_to_file_and_printing/4,
	   shell_sequence_operator/1,
	   get_java_version/1,
	   last_wavfile_in_dir/2,
	   add_directory_in_pathname/3,
	   wrap_file_names_with_search_paths/3,
	   absolute_windows_file_name/2,
	   directory_and_file_for_pathname/3,
	   create_directory_if_necessary/1,
	   create_directory_above_if_necessary/1,
	   create_directory_for_file_if_necessary/1,
	   create_directory_for_file_if_necessary/2,
	   parent_directory/2,
	   format_to_atom/3,
	   change_slash_to_backslash/2,
	   choose_from_text_menu/3,
	   get_confirmation/2,
	   open_regulus_file/3,
	   divide_file_into_n_parts/2,
	   read_lines_until_non_empty_line/2,
	   read_file_to_string/2,
	   read_file_to_string/3,
	   read_unicode_file_to_string/2,
	   read_unicode_file_to_string_list/2,
	   read_unicode_file_to_atom_list/2,
	   unicode_file_to_normal_file/2,
	   normal_file_to_unicode_file/2,
	   normal_file_to_unicode_file/3,
	   read_file_to_atom_list/2,
	   read_file_to_atom_list/3,
	   read_first_n_records_of_file_to_atom_list/3,
	   number_of_prolog_records_in_file/2,
	   safe_prolog_file_to_list/2,
	   safe_prolog_file_to_list/3,
	   prolog_file_to_list_vars_as_consts/3,
	   safe_prolog_file_to_list_printing_statistics/2,
	   safe_prolog_file_to_list_printing_statistics/3,
	   regulus_file_to_list/2,
           prolog_file_to_list_null_if_no_file/2,
	   prolog_file_to_list/2,
	   prolog_file_or_files_to_list/2,
	   prolog_file_or_files_to_list/3,
	   prolog_file_to_list_stream/2,
	   prolog_file_or_files_to_list_including_line_info/2,
	   list_to_prolog_file/2,
	   list_to_prolog_file_with_encoding/3,
	   normal_prolog_file_to_prolog_file_with_encoding/3,
	   safe_list_to_prolog_file_printing_statistics/2,
	   list_to_prolog_file_prettyprint/2,
	   list_to_prolog_file_prettyprint_with_encoding/3,
	   list_to_prolog_file_prettyprint_unicode/2,
	   list_to_regulus_file/2,
	   print_file/1,
	   cat_files/2,
	   cat_prolog_files/2,
	   cat_prolog_files/3,
	   copy_file/2,
	   copy_file_binary/2,
	   copy_file_to_stream/2,
	   copy_file_to_dir/2,
	   copy_files_to_dir/2,
	   subtract_files/3,
	   subtract_files/4,
	   print_lines_from_file/3,
	   create_timestamped_copy/2,
	   parse_file/4,
	   getline/2,
	   getline_as_words/2,
	   split_atom_into_words/2,
	   split_atom_into_words/3,
	   split_string_into_words/2,
	   split_string_into_words/3,
	   split_string_into_strings/3,
	   atom_name_contains_string/2,
	   timed_call/2,
	   absolute_timed_call/2,
	   safe_call_saving_output/3,
	   get_tmp_trace_file/1,
	   tmp_regulus_file/2,
	   csv_fields_contain_word_ignoring_case/2,
	   list_of_lists_to_csv_file/2,
	   list_of_lists_to_csv_file/3,
	   list_of_lists_to_csv_file/4,
	   csv_file_to_list_of_lists/2,
	   csv_file_to_list_of_lists/4,
	   csv_file_to_list_of_lists/5,
	   combine_columns_from_csv_files/5,
	   concatenate_headed_csv_files/2,
	   concatenate_headed_csv_files/3,
	   remove_null_lines_from_csv_file/2,
	   remove_null_lines_from_csv_file/3,
	   html_table_file_to_list/2,
	   html_table_file_to_list/3,
	   write_string_list_to_unicode_file/2,
	   write_atom_list_to_unicode_file/2,
	   write_atom_list_to_unicode_file/3,
	   write_atom_list_to_file/2,
	   write_atom_list_to_stream/2,
	   join_with_spaces/2,
	   join_with_underscore/2,
	   append_n_copies/3,
	   append_atoms/2,
	   append_atoms/3,
	   append_strings_with_char/3,
	   atom_or_number_codes/2,
	   term_contains_functor/2,
	   term_contains_subterm/2,
	   substitute_in_term/4,
	   read_prolog_file_and_assert_goals/2,
	   assert_list_of_goals/2,
	   append_list/2,
	   firstn_or_all/3,
	   split_off_firstn/4,
	   is_substring/2,
	   flatten/2,
	   flatten_comma_list/2,
	   is_comma_list/1,
	   is_curly_bracket_list/1,
	   length_of_curly_bracket_list/2,
	   length_of_comma_list/2,
	   curly_bracket_list_to_list/2,
	   comma_list_to_list/2,
	   list_to_curly_bracket_list/2,
	   list_to_comma_list/2,
	   first_elements_of_pairs/2,
	   second_elements_of_pairs/2,
	   normalise_prolog_dcg_clause_to_c_version/2,
	   unkey_list/2,
	   remove_duplicates_from_sorted_file/2,
	   id_member/2,
	   pick_n_random_members_from_list/3,
	   pick_n_random_members_from_list/4,
	   assoc_generics_to_lists_in_term/2,
	   inc_assoc_generic/3,
	   inc_assoc_generic/4,
	   get_assoc_generic_or_zero/3,
	   list_to_ordered_multiset/2,
	   insertions_deletions_substitutions/6,
	   insertions_deletions_substitutions_and_matches/7,
	   identical_up_to_variable_renaming/2,
	   var_or_grounded_var/1,
	   no_vars_or_grounded_vars_in_term/1,
	   make_ground/1,
	   unground/2,
	   try/1,
	   %assert_general/1,
	   numbervars/4,
	   align_word_atoms_for_printing/3,
	   is_list_of_atoms/1,
	   is_prolog_string/1,
	   is_list_of_non_negative_integers/1,
	   quote_layout_chars_in_string/2,
	   quote_layout_chars_in_string_for_json/2,
	   string_to_unicode_string/2,
	   string_to_json_unicode_string/2,
	   convert_strings_to_quoted_atoms/2,
	   strings_same_modulo_casing/2,
	   lowercase_atom_list/2,
	   uppercase_atom/2,
	   uppercase_string/2,
	   initial_uppercase_atom/2,
	   initial_uppercase_string/2,
	   lowercase_atom/2,
	   lowercase_string/2,
	   deaccent_atom/2,
	   deaccent_string/2,
	   whitespace_atom/1,
	   whitespace_char/1,
	   lowercase_string/1,
	   uppercase_string/1,
	   digit_char/1,
	   lowercase_char/1,
	   uppercase_char/1,
	   lowercase_uppercase/2,
	   accented_char/1,
	   deaccent_char/2,
	   punctuation_char/1,
	   
	   set_pp_width/1,
	   set_pp_depth/1,
	   
	   prettyprint/1,
	   prettyprint/2,
	   prettyprint/3,
	   prettyprint/4,
	   prettyprint_to_stream/2,
	   prettyprint_to_stream/3,
	   prettyprint_to_stream/4,
	   prettyprint_to_stream/5,
	   prettyprint_key_val_pair/2,

	   prettyprintq/1,
	   prettyprintq/2,
	   prettyprintq/3,
	   prettyprintq/4,
	   prettyprintq_key_val_pair/2,

	   prettyprintq_to_stream_unlimited_depth/2,
	   prettyprintq_to_stream/2,
	   prettyprintq_to_stream/3,
	   prettyprintq_to_stream/4,
	   prettyprintq_to_stream/5,
	   prettyprintq_to_stream_key_val_pair/3,

	   prettyprint_term_with_intro_grounded/2,
	   prettyprint_term_with_intro_grounded/3,
	   
	   prettyprint_term_with_intro/2,
	   prettyprint_term_with_intro/3,
	   
	   pp_debug/0,
	   no_pp_debug/0
	  ]
	 ).

'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
'SICSTUS4ONLY'( ( :- use_module(library(process)) ) ).
:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(sockets)).
:- use_module(library(random)).
:- use_module(library(xml)).

:- meta_predicate safe_call_saving_output(:, -, -).
:- meta_predicate timed_call(:, +).
:- meta_predicate absolute_timed_call(:, +).
:- meta_predicate parse_file(+, +, :, +).
:- meta_predicate try(:).
%:- meta_predicate assert_general(:).
 
%------------------------------------------------------------------------------------

%:- use_module(library(system)).
'SICSTUS3/4'( ( :- use_module(library(system)) ),
	      ( :- use_module(library(system3), [now/1, datime/1, datime/2, environ/2, shell/1, shell/2, sleep/1, exec/3, host_id/1, pid/1, host_name/1, mktemp/2, system/1, system/2, working_directory/2] ) ) ).

'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).
'SICSTUS3/4'( ( :- use_module(library(assoc)) ), ( :- use_module(library(avl) ) ) ).
%'SICSTUS3/4'( ( :- use_module(library(assoc)) ), ( :- use_module(library(assoc3) ) ) ).

%------------------------------------------------------------------------------------

'SICSTUS3/4'((
	     
safe_directory_exists(Dir) :-
	file_exists(Dir),
	file_property(Dir, type(directory))
),
(	     
safe_directory_exists(Dir) :-
	directory_exists(Dir)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((
	     
safe_nth(N, List, Elem) :-
	nth(N, List, Elem)
),
(	     
safe_nth(N, List, Elem) :-
	nth1(N, List, Elem)
)).	


%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_working_directory(Dir1, Dir2) :-
	working_directory(Dir1, Dir2)
),
(	     
safe_working_directory(Dir1, Dir2) :-
	current_directory(Dir1, Dir2)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_mod_time_for_file(File, UnixTime) :-
	file_exists(File),
	file_property(File, mod_time(UnixTime))
),
(	     
safe_mod_time_for_file(File, UnixTime) :-
	(   file_exists(File) ->
	    file_property(File, modify_timestamp, UnixTime)
	;
	    directory_exists(File) ->
	    directory_property(File, modify_timestamp, UnixTime)
	)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_directory_base_files(Dir, Files) :-
	directory_files(Dir, Files)
),
(	     
safe_directory_base_files(Dir, BaseFiles) :-
	directory_exists(Dir),
	file_members_of_directory(Dir, ProperFilePairs),
	directory_members_of_directory(Dir, DirectoryPairs),
	append(ProperFilePairs, DirectoryPairs, FilePairs),
	first_elements_in_pairs(FilePairs, BaseFiles)
)).	

%------------------------------------------------------------------------------------

% Nasty point: apparently, Sicstus 3 version preserves order, Sicstus 4 version sorts,
% and neither piece of functionality is documented!
'SICSTUS3/4'((

safe_remove_duplicates(L1, L2) :-
	remove_duplicates(L1, L2)
),
(	     
safe_remove_duplicates(L1, L2) :-
	remove_dups(L1, L2)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_remove_duplicates_preserving_order(L1, L2) :-
	remove_duplicates(L1, L2)
),
(	     
safe_remove_duplicates_preserving_order(L1, L2) :-
	remove_dups_preserving_order(L1, [], L2)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_abolish(Pred) :-
	abolish(Pred)
),
(	     
safe_abolish(Pred) :-
	abolish(Pred, [force(true)])
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_subsumes_chk(X, Y) :-
	subsumes_chk(X, Y)
),
(	     
safe_subsumes_chk(X, Y) :-
	subsumeschk(X, Y)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_print_message(X, Y) :-
	print_message_work_around(X, Y)
),
(	     
safe_print_message(X, Y) :-
	print_message(X, Y)
	%print_message_work_around(X, Y)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_sum_list(X, Y) :-
	sum_list(X, Y)
),
(	     
safe_sum_list(X, Y) :-
	sumlist(X, Y)
)).

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_max_list(List, Max) :-
	max_list(List, Max)
),
(	     
safe_max_list(List, Max) :-
	max_member(Max, List)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_min_list(List, Min) :-
	min_list(List, Min)
),
(	     
safe_min_list(List, Min) :-
	min_member(Min, List)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_trimcore :-
	trimcore
),
(	     
safe_trimcore :-
	true
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

sleep_if_sicstus4_and_using_gui :-
	true
),
(	     
sleep_if_sicstus4_and_using_gui :-
	sleep(1)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_close_all_streams :-
	true
),
(	     
safe_close_all_streams :-
	close_all_streams
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_process_id(PID) :-
	pid(PID)
),
(	     
safe_process_id(PID) :-
	process_id(PID)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_socket(Type, Socket) :-
	socket(Type, Socket)
),
(	     
safe_socket(Type, Socket) :-
	Socket = dummy_socket(Type)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_socket_connect(Socket, Host, Port, Stream) :-
	safe_socket_connect_sicstus3(Socket, Host, Port, Stream)
),
(	     
safe_socket_connect(_Socket, Host, Port, Stream) :-
	safe_socket_connect_sicstus4(Host, Port, Stream)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_socket_connect(_Socket, _Host, _Port, _Stream, _OpenStreamParameters) :-
	require_sicstus_version(4, safe_socket_connect/5)
),
(	     
safe_socket_connect(_Socket, Host, Port, Stream, OpenStreamParameters) :-
	safe_socket_connect_sicstus4(Host, Port, Stream, OpenStreamParameters)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_socket_close(Socket) :-
	safe_socket_close_sicstus3(Socket)
),
(	     
safe_socket_close(_Socket) :-
	true
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_socket_server_open(Port, Socket) :-
	safe_socket_server_open_sicstus3(Port, Socket)
),
(	     
safe_socket_server_open(Port, Socket) :-
	socket_server_open(Port, Socket)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_socket_server_close(Socket) :-
	safe_socket_close_sicstus3(Socket)
),
(	     
safe_socket_server_close(Socket) :-
	socket_server_close(Socket)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_socket_server_accept(Socket, Client, Stream) :-
	socket_accept(Socket, Client, Stream)
),
(	     
safe_socket_server_accept(Socket, Client, Stream) :-
	socket_server_accept(Socket, Client, Stream, [type(text)])
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((
safe_socket_server_accept(_Socket, _Client, _Stream, _OpenStreamParameters) :-
	require_sicstus_version(4, safe_socket_server_accept/4)
),
(	     
safe_socket_server_accept(Socket, Client, Stream, OpenStreamParameters) :-
	require_sicstus_version(4, socket_server_accept/4),
	socket_server_accept(Socket, Client, Stream, OpenStreamParameters)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

is_contiguous_sublist(SubList, List) :-
	is_contiguous_sublist_sicstus3(SubList, List)
),
(	     
is_contiguous_sublist(SubList, List) :-
	segment(List, SubList)
)).

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_prefix(Prefix, List) :-
	prefix(Prefix, List)
),
(	     
safe_prefix(Prefix, List) :-
	prefix(List, Prefix)
)).

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_suffix(Suffix, List) :-
	suffix(Suffix, List)
),
(	     
safe_suffix(Suffix, List) :-
	suffix(List, Suffix)
)).

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

is_assoc_generic(Assoc) :-
	is_assoc(Assoc)
),
(	     
is_assoc_generic(Assoc) :-
	is_avl(Assoc)
	%is_assoc(Assoc)
)).

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

empty_assoc_generic(Assoc) :-
	empty_assoc(Assoc)
),
(	     
empty_assoc_generic(Assoc) :-
	empty_avl(Assoc)
	%empty_assoc(Assoc)
)).

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

assoc_generic_to_list(Assoc, List) :-
	assoc_to_list(Assoc, List)
),
(	     
assoc_generic_to_list(Assoc, List) :-
	avl_to_list(Assoc, List)
	%assoc_to_list(Assoc, List)
)).

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

list_to_assoc_generic(List, Assoc) :-
	list_to_assoc(List, Assoc)
),
(	     
list_to_assoc_generic(List, Assoc) :-
	list_to_avl(List, Assoc)
	%list_to_assoc(List, Assoc)
)).

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

get_assoc_generic(Key, Assoc, Val) :-
	get_assoc(Key, Assoc, Val)
),
(	     
get_assoc_generic(Key, Assoc, Val) :-
	avl_fetch(Key, Assoc, Val)
	%get_assoc(Key, Assoc, Val)
)).

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

put_assoc_generic(Key, OldAssoc, Val, NewAssoc) :-
	put_assoc(Key, OldAssoc, Val, NewAssoc)
),
(	     
put_assoc_generic(Key, OldAssoc, Val, NewAssoc) :-
	avl_store(Key, OldAssoc, Val, NewAssoc)
	%put_assoc(Key, OldAssoc, Val, NewAssoc)
)).

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

min_assoc_generic(Assoc, Key, Val) :-
	min_assoc(Assoc, Key, Val)
),
(	     
min_assoc_generic(Assoc, Key, Val) :-
	avl_min(Assoc, Key, Val)
	%min_assoc(Assoc, Key, Val)
)).

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

get_next_assoc_generic(Key, Assoc, Knext, Vnext) :-
	get_next_assoc(Key, Assoc, Knext, Vnext)
),
(	     
get_next_assoc_generic(Key, Assoc, Knext, Vnext) :-
	avl_next(Key, Assoc, Knext, Vnext)
	%get_next_assoc(Key, Assoc, Knext, Vnext)
)).

%------------------------------------------------------------------------------------

'SICSTUS3/4'((

set_random_generator_state_from_time :-
	set_random_generator_state_from_time_sicstus3
),
(	     
set_random_generator_state_from_time :-
	set_random_generator_state_from_time_sicstus4
)).
	
%------------------------------------------------------------------------------------

'SICSTUS3/4'((

safe_random_member(X, List) :-
	safe_random_member_sicstus3(X, List)
),
(	     
safe_random_member(X, List) :-
	random_member(X, List)
)).
	
%------------------------------------------------------------------------------------

'SICSTUS3/4.0.2/4.other'((

safe_exec(Cmd, Streams, Pid) :-
	exec(Cmd, Streams, Pid)
),
% Patch definition to cover bug in Sicstus 4.0.2			 
(	     
safe_exec(Cmd, [Stdin, Stdout, Stderr], Pid) :-
	system3:system_binary(Binary, DashC),
	process_create(Binary, [DashC, Cmd],
                       [stdin(Stdin), stdout(Stdout), stderr(Stderr),
                        process(ProcessRef)]),
	process_id(ProcessRef, Pid)
),
(
safe_exec(Cmd, Streams, Pid) :-
	exec(Cmd, Streams, Pid)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4.0.2/4.other'((

safe_shell(Cmd) :-
	shell(Cmd)
),
(	     
safe_shell(Cmd) :-
	shell(Cmd)
),
% Problems with Sicstus 4.0.3's version of the system3 library
(
safe_shell(Cmd) :-
	%format('~N*** Warning: call "~w" may not work, due to compatibility library problems in SICStus 4.0.3 and upward~n', [shell(Cmd)]),
	shell(Cmd)
)).	

%------------------------------------------------------------------------------------

'SICSTUS3/4.0.2/4.other'((

safe_shell(Cmd, Status) :-
	shell(Cmd, Status)
),
(	     
safe_shell(Cmd, Status) :-
	shell(Cmd, Status)
),
% Problems with Sicstus 4.0.3's version of the system3 library
(
safe_shell(Cmd, Status) :-
	%format('~N*** Warning: call "~w" may not work, due to compatibility library problems in SICStus 4.0.3 and upward~n', [shell(Cmd, Status)]),
	shell(Cmd, Status)
)).

%------------------------------------------------------------------------------------

safe_average_list(List, Av) :-
	safe_sum_list(List, Total),
	length(List, N),
	(   N = 0 ->
	    Av = 0
	;
	    otherwise ->
	    Av is Total / N
	).

%------------------------------------------------------------------------------------

max_exceptions_when_reading_prolog_file(100).

%------------------------------------------------------------------------------------

safe_format_to_chars(FormatAtom, Args, Result) :-
	format_to_chars(FormatAtom, Args, Result).

safe_read_from_chars(Chars, Term) :-
	read_from_chars(Chars, Term).

safe_read_from_chars(Chars, Term, Options) :-
	read_from_chars(Chars, Term, Options).

%------------------------------------------------------------------------------------

require_sicstus_version(Version, Predicate) :-
	(   user:sicstus_version(Version) ->
	    true
	;
	    format('~N*** Error: ~w requires SICStus ~w~n', [Predicate, Version]),
	    fail
	).
	
%------------------------------------------------------------------------------------

running_under_windows :-
	\+ current_predicate(_Package:environ/2),
	!,
	format('~N*** Warning: the predicate environ/2, required to find out which operating system is being used, is not defined. Assuming we are not using Windows.~n', []),
	fail.
running_under_windows :-
	(   environ('OS', Env) ->
	    Env = 'Windows_NT'
	;
	    otherwise ->
	    format('~N*** Warning: attempt to look up value of environment variable \'OS\' failed. Assuming we are not using Windows.~n', []),
	    fail
	),
	!.

%------------------------------------------------------------------------------------

on_cygwin_and_bin_dir_found :-
	running_under_windows,
	environ('PATH', PathAtom),
	split_atom_into_words(PathAtom, 0';, Dirs),
	member(Dir, Dirs),
	safe_absolute_file_name(Dir, AbsDir),
	atom_codes(AbsDir, DirString),
	dir_pattern_string(DirPatternString),
	is_substring(DirPatternString, DirString),
	!.

dir_pattern_string("cygwin/bin").
dir_pattern_string("cygwin64/bin").

%------------------------------------------------------------------------------------

% NUANCE_VERSION is not set in N8.5, the only other version we care about for now.
nuance10_loaded :-
	environ('NUANCE_VERSION', _Version),
	!.

%------------------------------------------------------------------------------------

/*
$ file -bi english.txt
text/plain; charset=iso-8859-1

$ file -bi english_chinese.txt
text/plain; charset=utf-16le
*/

find_sicstus_encoding_for_file(File, Encoding) :-
	on_cygwin_and_bin_dir_found,
	safe_absolute_file_name(File, AbsFile),
	get_tmp_trace_file(TmpOutputFile),
	%absolute_file_name('$REGULUS/Prolog/tmp_output_xxx.txt', TmpOutputFile),
	system_on_list(['file -bi', AbsFile, '>', TmpOutputFile]),
	(   file_exists(TmpOutputFile) ->
	    
	    read_file_to_atom_list(TmpOutputFile, Result),
	    delete_file(TmpOutputFile);
	    
	    Result = []
	),
	Result = [ResultAtom],
	parse_file_encoding_result(ResultAtom, Encoding),
	!.
find_sicstus_encoding_for_file(File, Encoding) :-
	format('~N*** Error: bad call: ~w~n',
	       [find_sicstus_encoding_for_file(File, Encoding)]),
	fail.

parse_file_encoding_result(ResultAtom, Encoding) :-
	atom_codes(ResultAtom, ResultStr),
	file_encoding_result(Encoding0, ResultStr, []),
	cygwin_encoding_to_prolog_encoding(Encoding0, Encoding).

file_encoding_result(Encoding) -->
	string_ending_in_semicolon,
	optional_whitespace_sequence,
	"charset=",
	non_whitespace_word(Encoding).

cygwin_encoding_to_prolog_encoding(Unix, Prolog) :-
	uppercase_atom(Unix, UppercaseUnix),
	known_prolog_encoding(UppercaseUnix),
	UppercaseUnix = Prolog,
	!.
cygwin_encoding_to_prolog_encoding(Unix, Prolog) :-
	format('~N*** Warning: unknown encoding "~w" treated as "ISO-8859-1"', [Unix]),
	Prolog = 'ISO-8859-1',
	!.

string_ending_in_semicolon -->
	[0';],
	!.
string_ending_in_semicolon -->
	[F],
	{F \== 0';},
	!,
	string_ending_in_semicolon.

/*
The following character encodings are currently supported by SICStus Prolog.
ANSI_X3.4-1968
ISO-8859-1
ISO-8859-2
ISO-8859-15
windows 1252
UTF-8
UTF-16
UTF-16LE
UTF-16BE
UTF-32
UTF-32LE
UTF-32BE
*/

known_prolog_encoding('ANSI_X3.4-1968').
known_prolog_encoding('ISO-8859-1').
known_prolog_encoding('ISO-8859-2').
known_prolog_encoding('ISO-8859-15').
known_prolog_encoding('windows 1252').
known_prolog_encoding('UTF-8').
known_prolog_encoding('UTF-16').
known_prolog_encoding('UTF-16LE').
known_prolog_encoding('UTF-16BE').
known_prolog_encoding('UTF-32').
known_prolog_encoding('UTF-32LE').
known_prolog_encoding('UTF-32BE').

%------------------------------------------------------------------------------------

replace_strings_with_atoms_in_xml(Atom, Atom) :-
	atomic(Atom),
	!.
replace_strings_with_atoms_in_xml(pcdata(String), Atom) :-
	is_prolog_string(String),
	(   safe_number_codes(Atom, String)
	;
	    atom_codes(Atom, String)
	),
	!.
replace_strings_with_atoms_in_xml(String, Atom) :-
	is_prolog_string(String),
	(   safe_number_codes(Atom, String)
	;
	    atom_codes(Atom, String)
	),
	!.
replace_strings_with_atoms_in_xml(Term, Term1) :-
	functor(Term, F, N),
	functor(Term1, F, N),
	replace_strings_with_atoms_in_xml_args(N, Term, Term1).

replace_strings_with_atoms_in_xml_args(I, _Term, _Term1) :-
	I =< 0,
	!.
replace_strings_with_atoms_in_xml_args(I, Term, Term1) :-
	I > 0,
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	replace_strings_with_atoms_in_xml(Arg, Arg1),
	I1 is I - 1,
	!,
	replace_strings_with_atoms_in_xml_args(I1, Term, Term1).

%======================================================================

remove_comments_in_xml(Var, Var) :-
	var(Var),
	!.
remove_comments_in_xml(Atom, Atom) :-
	atomic(Atom),
	!.
remove_comments_in_xml(List, List1) :-
	is_list(List),
	remove_comments_in_xml_list(List, List1),
	!.
remove_comments_in_xml(Term, Term1) :-
	functor(Term, F, N),
	functor(Term1, F, N),
	remove_comments_in_xml_args(N, Term, Term1).

remove_comments_in_xml_list([], []).
remove_comments_in_xml_list([comment(_) | R], R1) :-
	!,
	remove_comments_in_xml_list(R, R1).
remove_comments_in_xml_list([F | R], [F1 | R1]) :-
	remove_comments_in_xml(F, F1),
	!,
	remove_comments_in_xml_list(R, R1).

remove_comments_in_xml_args(I, _Term, _Term1) :-
	I =< 0,
	!.
remove_comments_in_xml_args(I, Term, Term1) :-
	I > 0,
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	remove_comments_in_xml(Arg, Arg1),
	I1 is I - 1,
	!,
	remove_comments_in_xml_args(I1, Term, Term1).

%------------------------------------------------------------------------------------

safe_number_codes(N, Codes) :-
	on_exception(_Exception,
		     number_codes(N, Codes),
		     fail
		    ).

%------------------------------------------------------------------------------------

is_contiguous_sublist_sicstus3(SubList, List) :-
	prefix(SubList, List),
	!.
is_contiguous_sublist_sicstus3(SubList, [_F | R]) :-
	is_contiguous_sublist_sicstus3(SubList, R),
	!.

%------------------------------------------------------------------------------------

set_random_generator_state_from_time_sicstus3 :-
	random_number_params_from_time(A, B, C, _D),
	setrand(rand(A, B, C)).

set_random_generator_state_from_time_sicstus4 :-
	random_number_params_from_time(A, B, C, D),
	setrand(random(A, B, C, D)).

random_number_params_from_time(A, B, C, D) :-
	now(Time),
	A is Time mod 10000,
	B is A + 1000,
	C is A + 2000,
	D is A + 3000.	

%------------------------------------------------------------------------------------

% For some reason, the usual print_message doesn't interact correctly with with_output_to_chars
% in SICStus 3.
print_message_work_around(Severity, Exception) :-
	'SU_messages':generate_message(Exception, Lines, []),
	print_message_lines_work_around(Severity, Lines).

print_message_lines_work_around(_Severity, []).
print_message_lines_work_around(Severity, [F | R]) :-
	print_message_line_work_around(Severity, F),
	!,
	print_message_lines_work_around(Severity, R).

print_message_line_work_around(_Severity, Line) :-
	%format('~N! ', []),
	print_message_line_work_around1(Line).

print_message_line_work_around1(String-Args) :-
	format(String, Args),
	!.
print_message_line_work_around1(nl) :-
	format('~n', []),
	!.
print_message_line_work_around1(format(String, Args)) :-
	format(String, Args),
	!.
print_message_line_work_around1(write_term(Term, Options)) :-
	write_term(Term, Options),
	!.
print_message_line_work_around1(write_term(Term)) :-
	prolog_flag(toplevel_print_options, Options, Options),
	write_term(Term, Options),
	!.
print_message_line_work_around1(Other) :-
	format('~w', [Other]).

%------------------------------------------------------------------------------------

safe_read(Stream, Term) :-
	on_exception(Exception,
		     read(Stream, Term),
		     (   close(Stream),
			 raise_exception(Exception)
		     )
		    ).

safe_read_no_exceptions(Stream, InFile, Term) :-
	on_exception(_Exception,
		     read(Stream, Term),
		     (   format('~N*** Warning: bad Prolog term when reading from file ~w~n', [InFile]),
			 fail
		     )
		    ).

safe_read_no_exceptions_vars_as_consts(Stream, InFile, Term) :-
	on_exception(_Exception,
		     (   read_term(Stream, Term, [variable_names(Names)]),
			 instantiate_name_pairs(Names)
		     ),
		     (   format('~N*** Warning: bad Prolog term when reading from file ~w~n', [InFile]),
			 fail
		     )
		    ).

instantiate_name_pairs([]).
instantiate_name_pairs([Name=Var | R]) :-
	Name = Var,
	instantiate_name_pairs(R).

%------------------------------------------------------------------------------------

safe_close(S) :-
	on_exception(_Exception,
		     close(S),
		     true
		    ).

%------------------------------------------------------------------------------------

:- dynamic timeouts_disabled/0.

disable_timeouts :-
	timeouts_disabled,
	!.
disable_timeouts :-
	assertz(timeouts_disabled),
	format('~N--- Switching off timeouts~n', []),
	!.

timeouts_unavailable :-
	\+ predicate_property(time_out(_, _, _), _Property),
	!.
timeouts_unavailable :-
	timeouts_disabled.

%------------------------------------------------------------------------------------

exec_and_check_process_started(Command, Timeout, PID) :-
	safe_exec(Command, [null, null, null], PID),
	check_new_process_started(PID, Timeout),
	!.
exec_and_check_process_started(Command, Timeout, _PID) :-
	format('~N*** Error: command "~w" apparently not running after ~d seconds~n', [Command, Timeout]),
	fail.

check_new_process_started(PID, _Timeout) :-
	PID = '$process'(_),
	!.
check_new_process_started(PID, Timeout) :-
	integer(PID),
	Timeout > 0,
	get_windows_ps_info(Results),
	member(Line, Results),
	member(pid=PID, Line),
	!.
check_new_process_started(PID, Timeout) :-
	integer(PID),
	Timeout > 0,
	sleep(1),
	Timeout1 is Timeout - 1,
	!,
	check_new_process_started(PID, Timeout1).

%------------------------------------------------------------------------------------

kill_process(PID) :-
	system_on_list(['kill -f', PID]).

%------------------------------------------------------------------------------------

/*

Cygwin access to Windows processes

ps -W                      - get Windows processes
/bin/kill -f <PID>         - kill process with given Windows PID from Cygwin

Typical info from ps -W:

      PID    PPID    PGID     WINPID  TTY  UID    STIME COMMAND
     4260       0       0       4260    ?    0   Feb  7 C:\Program Files\Ghostgum\gsview\gsview32.exe
     5532       0       0       5532    ?    0   Feb  8 C:\Program Files\Adobe\Acrobat 7.0\Reader\AcroRd32.exe
     5736       0       0       5736    ?    0   Feb  8 C:\WINDOWS\system32\cmd.exe
     4688       1    4688       4688  con 1006   Feb  8 /usr/bin/bash
     4324       0       0       4324    ?    0   Feb  9 C:\Program Files\Mozilla Firefox\firefox.exe
     1240       0       0       1240    ?    0   Feb  9 C:\Program Files\SICStus Prolog 3.12.5\bin\spwin.exe
     4584       0       0       4584    ?    0 08:20:37 C:\WINDOWS\system32\cmd.exe
     4784       0       0       4784    ?    0 11:04:13 C:\WINDOWS\system32\cmd.exe
      928       1     928        928  con 1006 11:04:13 /usr/bin/bash
      204     928     204       1244  con 1006 11:04:21 /usr/bin/man
     4344     204     204       4344  con 1006 11:04:22 /usr/bin/sh
     5592    4344     204       5592  con 1006 11:04:22 /usr/bin/sh
I    5396    5592     204       5856  con 1006 11:04:22 /usr/bin/less
     5776    4688    5776       5528  con 1006 14:26:54 /usr/bin/ps

Primitives:

- Kill process with given Windows PID
     
*/

get_windows_ps_info(Result) :-
	%require_sicstus_version(3, get_windows_ps_info/1),
	absolute_file_name('$REGULUS/Prolog/tmp_output_xxx.txt', TmpOutputFile),
	system_on_list(['ps -W', '>', TmpOutputFile]),
	(   file_exists(TmpOutputFile) ->
	    
	    read_file_to_atom_list(TmpOutputFile, Result0),
	    delete_file(TmpOutputFile);
	    
	    Result0 = []
	),
	Result0 = [_Heading | Result0WithoutHeading],
	parse_ps_info_list(Result0WithoutHeading, Result),
	!.
get_windows_ps_info(Result) :-
	format('~N*** Error: bad call: ~w~n', [get_windows_ps_info(Result)]),
	fail.

parse_ps_info_list([], []).
parse_ps_info_list([F | R], [F1 | R1]) :-
	parse_ps_info_item(F, F1),
	!,
	parse_ps_info_list(R, R1).
parse_ps_info_list([_F | R], R1) :-
	!,
	parse_ps_info_list(R, R1).

parse_ps_info_item(Atom, Alist) :-
	atom_codes(Atom, Chars),
	ps_info_line(Alist, Chars, []),
	!.
parse_ps_info_item(Atom, _Alist) :-
	format('~N*** Warning: unable to parse ps line, "~w"~n', [Atom]),
	fail.

/*
      PID    PPID    PGID     WINPID  TTY  UID    STIME COMMAND
I    5396    5592     204       5856  con 1006 11:04:22 /usr/bin/less
     5776    4688    5776       5528  con 1006 14:26:54 /usr/bin/ps
*/

ps_info_line(Alist) -->
	ps_info_i_o_or_nothing,
	ps_info_pid(Alist-Alist1),
	ps_info_ppid(Alist1-Alist2),
	ps_info_pgid(Alist2-Alist3),
	ps_info_winpid(Alist3-Alist4),
	ps_info_tty(Alist4-Alist5),
	ps_info_uid(Alist5-Alist6),
	ps_info_time_or_date(Alist6-Alist7),
	ps_info_command(Alist7).

ps_info_i_o_or_nothing -->
	"I",
	optional_whitespace_sequence,
	!.
ps_info_i_o_or_nothing -->
	"O",
	optional_whitespace_sequence,
	!.
ps_info_i_o_or_nothing -->
	"S",
	optional_whitespace_sequence,
	!.
ps_info_i_o_or_nothing -->
	optional_whitespace_sequence.

ps_info_pid([pid=N | R]-R) -->
	integer_word(N),
	optional_whitespace_sequence.

ps_info_ppid([ppid=N | R]-R) -->
	integer_word(N),
	optional_whitespace_sequence.

ps_info_pgid([pgid=N | R]-R) -->
	integer_word(N),
	optional_whitespace_sequence.

ps_info_winpid([winpid=N | R]-R) -->
	integer_word(N),
	optional_whitespace_sequence.

ps_info_uid([uid=N | R]-R) -->
	integer_word(N),
	optional_whitespace_sequence.

ps_info_tty([tty=TTY | R]-R) -->
	non_whitespace_word(TTY),
	optional_whitespace_sequence.

ps_info_time_or_date([date_or_time=date(Month, Day) | R]-R) -->
	non_whitespace_word(Month),
	optional_whitespace_sequence,
	integer_word(Day),
	optional_whitespace_sequence,
	!.
ps_info_time_or_date([date_or_time=time(H, M, S) | R]-R) -->
	integer_word(H),
	":",
	integer_word(M),
	":",
	integer_word(S).

ps_info_command([command=Word]) -->
	any_chars_word(Word).

%------------------------------------------------------------------------------------

abolish_if_defined(PredSpec) :-
	current_predicate(PredSpec),
	%format('~N[Abolishing defined predicate ~w]~n', [PredSpec]),
	safe_abolish(PredSpec),
	!.
abolish_if_defined(_PredSpec).

%------------------------------------------------------------------------------------

safe_compile_with_redefine_warnings_off(Module, File) :-
	set_prolog_flags_for_redefine_warnings_off(OldFlags),
	on_exception(
	_Exception,
	safe_compile(Module, File),
	restore_prolog_flags_after_redefine_warnings_off(OldFlags)
    ),
	restore_prolog_flags_after_redefine_warnings_off(OldFlags).

set_prolog_flags_for_redefine_warnings_off(OldFlags) :-
	prolog_flag(redefine_warnings, OldRedefine, off),
	OldFlags = [OldRedefine].

restore_prolog_flags_after_redefine_warnings_off(OldFlags) :-
	OldFlags = [OldRedefine],
	prolog_flag(redefine_warnings, _Current, OldRedefine).	

%------------------------------------------------------------------------------------

safe_compile_with_discontiguous_warnings_off(Module, File) :-
	set_prolog_flags_for_discontiguous_warnings_off(OldFlags),
	on_exception(
	_Exception,
	safe_compile(Module, File),
	restore_prolog_flags_after_discontiguous_warnings_off(OldFlags)
    ),
	restore_prolog_flags_after_discontiguous_warnings_off(OldFlags).

set_prolog_flags_for_discontiguous_warnings_off(OldFlags) :-
	prolog_flag(discontiguous_warnings, OldDiscontiguous, off),
	OldFlags = [OldDiscontiguous].

restore_prolog_flags_after_discontiguous_warnings_off(OldFlags) :-
	OldFlags = [OldDiscontiguous],
	prolog_flag(discontiguous_warnings, _Current, OldDiscontiguous).	

%------------------------------------------------------------------------------------

safe_compile_with_redefine_and_single_var_warnings_off(Module, File) :-
	set_prolog_flags_for_redefine_and_single_var_warnings_off(OldFlags),
	on_exception(
	_Exception,
	safe_compile(Module, File),
	restore_prolog_flags_after_redefine_and_single_var_warnings_off(OldFlags)
    ),
	restore_prolog_flags_after_redefine_and_single_var_warnings_off(OldFlags).

set_prolog_flags_for_redefine_and_single_var_warnings_off(OldFlags) :-
	prolog_flag(redefine_warnings, OldRedefine, off),
	prolog_flag(single_var_warnings, OldSingleVar, off),
	OldFlags = [OldRedefine, OldSingleVar].

restore_prolog_flags_after_redefine_and_single_var_warnings_off(OldFlags) :-
	OldFlags = [OldRedefine, OldSingleVar],
	prolog_flag(redefine_warnings, _Current, OldRedefine),
	prolog_flag(single_var_warnings, _Current1, OldSingleVar).

%------------------------------------------------------------------------------------

safe_compile_list(_Module, []) :-
	!.
safe_compile_list(Module, List) :-
	is_list(List),
	List = [F | R],
	safe_compile(Module, F),
	!,
	safe_compile_list(Module, R).
safe_compile_list(Module, Other) :-
	format('*** Error: bad call ~w~n', [safe_compile_list(Module, Other)]),
	fail.

%------------------------------------------------------------------------------------

:- dynamic compile_timeout/1.

set_compile_timeout(Timeout) :-
	number(Timeout),
	Timeout > 0,
	retractall(compile_timeout(_)),
	assertz(compile_timeout(Timeout)),
	!.
set_compile_timeout(_Timeout).

%------------------------------------------------------------------------------------

safe_compile(Module, File) :-
	nonvar(File),
	safe_absolute_file_name(File, AbsFile),
	% Maybe wait a little bit to try and avoid possible race conditions.
	% This shouldn't be necessary, of course.
	(   compile_timeout(Timeout) ->
	    format('~N--- Waiting ~2f seconds before starting compilation~n', [Timeout]),
	    flush_output(user),
	    sleep(Timeout)
	;
	    true
	),
	safe_compile1(Module, AbsFile).

safe_compile1(_Module, File) :-
	\+ file_exists(File),
	!,
	format('*** Error: unable to find file ~w~n', [File]),
	fail.
safe_compile1(Module, File) :-
	on_exception(
	Exception, 
	compile(Module:File),
	( inform_about_error_in_compile(File, Exception) )
    ),
	!.

inform_about_error_in_compile(File, Exception) :-
	format('~N~nCompilation of file ~w aborted due to error:~n~n', [File]),
	print_message(error, Exception),
	format('~n', []),
	fail.

%------------------------------------------------------------------------------------

safe_absolute_file_name(Var, _) :-
	var(Var),
	!,
	format('*** Error: variable argument to safe_absolute_file_name/2~n', []),
	fail.
safe_absolute_file_name([], []) :-
	!.
safe_absolute_file_name([F | R], [F1 | R1]) :-
	safe_absolute_file_name(F, F1),
	!,
	safe_absolute_file_name(R, R1).
safe_absolute_file_name(File, AbsFile) :-
	possible_input_to_absolute_file_name(File),
	!,
	absolute_file_name(File, AbsFile).
safe_absolute_file_name(File, _AbsFile) :-
	format('*** Error: unable to interpret ~w as file description~n', [File]),
	fail.

possible_input_to_absolute_file_name(File) :-
	atom(File),
	!.
possible_input_to_absolute_file_name(FileDescription) :-
	compound(FileDescription),
	FileDescription =.. [FileSearchPath, File],
	(   \+ valid_file_search_path(FileSearchPath) ->
	    
	    format('*** Error: ~w is not currently declared as a file_search_path~n', [FileSearchPath]),
	    fail ;

	    \+ atom(File) ->
	    format('*** Error: ~w must be an atom~n', [File]),
	    fail ;

	    true
	).

valid_file_search_path(FileSearchPath) :-
	current_predicate(file_search_path, user:file_search_path(_, _)),
	user:file_search_path(FileSearchPath, _Value),
	!.

%------------------------------------------------------------------------------------

safe_rename_file(File1, File2) :-
	safe_absolute_file_name(File1, AbsFile1),
	safe_absolute_file_name(File2, AbsFile2),
	(   safe_file_exists(AbsFile2) ->
	    delete_file(AbsFile2)
	;
	    otherwise ->
	    true
	),
	rename_file(AbsFile1, AbsFile2).

%------------------------------------------------------------------------------------

safe_file_exists(File) :-
	safe_absolute_file_name(File, AbsFile),
	file_exists(AbsFile).

%------------------------------------------------------------------------------------

safe_directory_files(Dir, Files) :-
	on_exception(_Exception,
		     safe_directory_files1(Dir, Files),
		     ( format('~N*** Error: exception in: ~w~n', [safe_directory_files(Dir, Files)]), fail )
		    ).

safe_directory_files1(Dir, Files) :-
	safe_absolute_file_name(Dir, AbsDir),
	safe_directory_exists(AbsDir),
	safe_directory_base_files(AbsDir, Files).

%------------------------------------------------------------------------------------

first_elements_in_pairs([], []).
first_elements_in_pairs([F-_Second | R], [F | R1]) :-
	first_elements_in_pairs(R, R1).

%------------------------------------------------------------------------------------

safe_is_directory(Dir) :-
	safe_absolute_file_name(Dir, AbsDir),
	safe_directory_exists(AbsDir),
	!.

%------------------------------------------------------------------------------------

directory_files_recursive(Dir, AllFiles) :-
	safe_absolute_file_name(Dir, AbsDir),
	safe_is_directory(AbsDir),
	directory_files_recursive1(AbsDir, AllFiles-[]).

directory_files_recursive1(File, [File | Out]-Out) :-
	\+ safe_is_directory(File),
	!.
directory_files_recursive1(Dir, In-Out) :-
	safe_is_directory(Dir),
	!,
	safe_directory_files(Dir, Files0),
	reverse(Files0, Files),
	directory_files_recursive2(Files, Dir, In-Out).

directory_files_recursive2([], _Dir, In-In) :-
	!.
directory_files_recursive2([F | R], Dir, In-Out) :-
	dummy_dir_file(F),
	!,
	directory_files_recursive2(R, Dir, In-Out).	
directory_files_recursive2([F | R], Dir, In-Out) :-
	format_to_atom('~w/~w', [Dir, F], FullF),
	directory_files_recursive1(FullF, In-Next),
	!,
	directory_files_recursive2(R, Dir, Next-Out).	

dummy_dir_file('.').
dummy_dir_file('..').

%------------------------------------------------------------------------------------

safe_socket_connect_sicstus3(Socket, Host, Port, Stream) :-
	require_sicstus_version(3, safe_socket_connect_sicstus3/4),
	on_exception(Exception,
		     socket_connect(Socket, 'AF_INET'(Host, Port), Stream),
		     safe_socket_connect_sicstus3_handle_exception(Exception, Host, Port)
		    ),
	!.

safe_socket_connect_sicstus3_handle_exception(Exception, Host, Port) :-
	format('~N*** Error: exception raised when trying to connect to port ~w on ~w~n', [Port, Host]),
	print_message(error, Exception),
	fail.

%------------------------------------------------------------------------------------

safe_socket_connect_sicstus4(Host, Port, Stream) :-
	require_sicstus_version(4, safe_socket_connect_sicstus4/3),
	on_exception(Exception,
		     socket_client_open(Host:Port, Stream, [type(text)]),
		     safe_socket_connect_sicstus4_handle_exception(Exception, Host, Port)
		    ),
	!.

safe_socket_connect_sicstus4_handle_exception(Exception, Host, Port) :-
	format('~N*** Error: exception raised when trying to connect to port ~w on ~w~n', [Port, Host]),
	print_message(error, Exception),
	fail.

%------------------------------------------------------------------------------------

safe_socket_connect_sicstus4(Host, Port, Stream, OpenStreamParameters) :-
	require_sicstus_version(4, safe_socket_connect_sicstus4/4),
	on_exception(Exception,
		     socket_client_open(Host:Port, Stream, OpenStreamParameters),
		     safe_socket_connect_sicstus4_handle_exception(Exception, Host, Port, OpenStreamParameters)
		    ),
	!.

safe_socket_connect_sicstus4_handle_exception(Exception, Host, Port, _OpenStreamParameters) :-
	format('~N*** Error: exception raised when trying to connect to port ~w on ~w~n', [Port, Host]),
	print_message(error, Exception),
	fail.

%------------------------------------------------------------------------------------

safe_socket_server_open_sicstus3(Port, Socket) :-
	require_sicstus_version(3, safe_socket_server_open_sicstus3/2),
	current_host(Host),
	socket('AF_INET', Socket),
	socket_bind(Socket, 'AF_INET'(Host, Port)),
	socket_listen(Socket, 5).

%------------------------------------------------------------------------------------

safe_socket_close_sicstus3(Socket) :-
	require_sicstus_version(3, safe_socket_close_sicstus3/1),
	on_exception(Exception,
		     socket_close(Socket),
		     safe_socket_close_handle_exception(Exception, Socket)
		    ),
	!.

safe_socket_close_handle_exception(Exception, Socket) :-
	format('~N*** Error: exception raised when trying to close socket ~w~n', [Socket]),
	print_message(error, Exception),
	fail.

%------------------------------------------------------------------------------------

safe_random_member_sicstus3(X, List) :-
	length(List, N),
	random(0, N, I),
	IPlus1 is I + 1,
	safe_nth(IPlus1, List, X).

%------------------------------------------------------------------------------------

consume_parameter_value_pair(InList, FeatVal, OutList) :-
	\+ is_list(InList),
	!,
	format('~N*** Error: bad call ~q~n. First arg must be list~n', [consume_parameter_value_pair(InList, FeatVal, OutList)]),
	fail.
consume_parameter_value_pair(InList, FeatVal, OutList) :-
	\+ safe_subsumes_chk(_=_, FeatVal),
	!,
	format('~N*** Error: bad call ~q~n. Second arg must be of form X=Y~n', [consume_parameter_value_pair(InList, FeatVal, OutList)]),
	fail.
consume_parameter_value_pair([(X=Y) | R], (X=Y), R) :-
	!.
consume_parameter_value_pair([_F | R], FeatVal, Out) :-
	consume_parameter_value_pair(R, FeatVal, Out).

%------------------------------------------------------------------------------------

datime_for_file(File, Datime) :-
	safe_absolute_file_name(File, AbsFile),
	safe_mod_time_for_file(AbsFile, UnixTime),
	datime(UnixTime, Datime),
	!.

%------------------------------------------------------------------------------------

age_of_file(File, Age) :-
	safe_absolute_file_name(File, AbsFile),
	safe_mod_time_for_file(AbsFile, UnixTime),
	now(UnixTimeNow),
	Age is UnixTimeNow - UnixTime,
	!.

%------------------------------------------------------------------------------------

file_is_more_recent_than_files(File, Files) :-
	safe_file_exists(File),
	age_of_file(File, Age),
	files_exist_and_are_older_than_age(Files, Age).

files_exist_and_are_older_than_age([], _Age).
files_exist_and_are_older_than_age([F | R], Age) :-
	safe_file_exists(F),
	age_of_file(F, Age1),
	Age1 > Age,
	!,
	files_exist_and_are_older_than_age(R, Age).
	
%------------------------------------------------------------------------------------

order_files_by_age_most_recent_first(Files, SortedFiles) :-
	tag_files_by_age(Files, TaggedFiles),
	keysort(TaggedFiles, SortedTaggedFiles),
	unkey_list(SortedTaggedFiles, SortedFiles).

tag_files_by_age([], []).
tag_files_by_age([F | R], [Age-F | R1]) :-
	age_of_file(F, Age),
	!,
	tag_files_by_age(R, R1).
	
%------------------------------------------------------------------------------------

% Returns latest datime for all items in the directory, including the directory itself

datime_for_directory_recursive(Dir, EarliestOrLatest, ExtremeDatime) :-
	datime_for_directory_recursive(Dir, EarliestOrLatest, '*no_extension_specified*', ExtremeDatime).

datime_for_directory_recursive(Dir, EarliestOrLatest, Extensions, ExtremeDatime) :-
	datime_for_file(Dir, DirDatime),
	safe_directory_files(Dir, Files),
	datimes_for_directory_files_recursive(Files, Dir, Extensions, FileDatimes-[]),
	add_directory_time_if_included_in_extensions(FileDatimes, DirDatime, Extensions, AllFileDatimes),
	extreme_time_in_list(AllFileDatimes, EarliestOrLatest, ExtremeDatime).

datimes_for_directory_files_recursive([], _Dir, _Extensions, DatimesIn-DatimesIn).
datimes_for_directory_files_recursive([F | R], Dir, Extensions, DatimesIn-DatimesOut) :-
	format_to_atom('~w/~w', [Dir, F], SubDir),
	safe_is_directory(SubDir),
	datime_for_file(SubDir, DirDatime),
	safe_directory_files(SubDir, SubFiles),
	add_directory_time_if_included_in_extensions(DatimesNext, DirDatime, Extensions, DatimesIn),
	datimes_for_directory_files_recursive(SubFiles, SubDir, Extensions, DatimesNext-DatimesNext2),
	!,
	datimes_for_directory_files_recursive(R, Dir, Extensions, DatimesNext2-DatimesOut).
datimes_for_directory_files_recursive([F | R], Dir, Extensions, DatimesIn-DatimesOut) :-
	Extensions \== '*no_extension_specified*',
	split_off_extension_from_pathname(F, _WithoutExtension, Extension),
	\+ member(Extension, Extensions),
	!,
	datimes_for_directory_files_recursive(R, Dir, Extensions, DatimesIn-DatimesOut).
datimes_for_directory_files_recursive([F | R], Dir, Extensions, [Datime | DatimesNext]-DatimesOut) :-
	format_to_atom('~w/~w', [Dir, F], FullPathname),
	datime_for_file(FullPathname, Datime),
	!,
	datimes_for_directory_files_recursive(R, Dir, Extensions, DatimesNext-DatimesOut).

add_directory_time_if_included_in_extensions(FileDatimes, DirDatime, Extensions, AllFileDatimes) :-
	(   Extensions = '*no_extension_specified*' ->
	    AllFileDatimes = [DirDatime | FileDatimes]
	;
	    ( is_list(Extensions), member(directory, Extensions) ) ->
	     AllFileDatimes = [DirDatime | FileDatimes]
	;
	    otherwise ->
	    AllFileDatimes = FileDatimes
	).

%------------------------------------------------------------------------------------

% Returns latest datime for all items in the directory, including the directory itself

datime_for_directory(Dir, EarliestOrLatest, ExtremeDatime) :-
	datime_for_directory(Dir, EarliestOrLatest, '*no_extension_specified*', ExtremeDatime).

datime_for_directory(Dir, EarliestOrLatest, Extension, ExtremeDatime) :-
	datime_for_file(Dir, DirDatime),
	safe_directory_files(Dir, Files),
	datimes_for_directory_files(Files, Dir, Extension, FileDatimes),
	extreme_time_in_list([DirDatime | FileDatimes], EarliestOrLatest, ExtremeDatime).

datimes_for_directory_files([], _Dir, _Extension, []).
datimes_for_directory_files([F | R], Dir, Extension, R1) :-
	Extension \== '*no_extension_specified*',
	\+ pathname_has_extension(F, Extension),
	!,
	datimes_for_directory_files(R, Dir, Extension, R1).
datimes_for_directory_files([F | R], Dir, Extension, [Datime | Datimes]) :-
	format_to_atom('~w/~w', [Dir, F], FullPathname),
	datime_for_file(FullPathname, Datime),
	!,
	datimes_for_directory_files(R, Dir, Extension, Datimes).

%------------------------------------------------------------------------------------

datime_for_file_list(List, EarliestOrLatest, ExtremeDatime) :-
	datimes_for_file_list1(List, EarliestOrLatest, Datimes),
	extreme_time_in_list(Datimes, EarliestOrLatest, ExtremeDatime).

datimes_for_file_list1([], _EarliestOrLatest, []).
datimes_for_file_list1([F | R], EarliestOrLatest, R1) :-
	\+ file_exists(F),
	!,
	datimes_for_file_list1(R, EarliestOrLatest, R1).
datimes_for_file_list1([F | R], EarliestOrLatest, [F1 | R1]) :-
	(   safe_directory_exists(F) ->
	    datime_for_directory(F, EarliestOrLatest, F1)
	;
	    datime_for_file(F, F1)
	),
	!,
	datimes_for_file_list1(R, EarliestOrLatest, R1).
 
%------------------------------------------------------------------------------------

extreme_time_in_list(Datimes, EarliestOrLatest, ExtremeDatime) :-
	default_extreme_time(EarliestOrLatest, StartDatime),
	extreme_time_in_list1(Datimes, EarliestOrLatest, StartDatime-ExtremeDatime).

default_extreme_time(earliest, datime(2100, 1, 1, 0, 0, 0)) :-
	!.
default_extreme_time(latest, datime(0, 1, 1, 0, 0, 0)) :-
	!.
default_extreme_time(Other, D) :-
	format('~N*** Error: bad call: ~w~n', [default_extreme_time(Other, D)]),
	fail.

extreme_time_in_list1([], _EarliestOrLatest, Datime-Datime).
extreme_time_in_list1([F | R], EarliestOrLatest, InDatime-OutDatime) :-
	(   ( EarliestOrLatest = earliest, earlier_datime(F, InDatime) ) ->
	    NextDatime = F ;
	    ( EarliestOrLatest = latest, earlier_datime(InDatime, F) ) ->
	    NextDatime = F ;
	    NextDatime = InDatime
	),
	!,
	extreme_time_in_list1(R, EarliestOrLatest, NextDatime-OutDatime).

%------------------------------------------------------------------------------------

real_datime(T0) :-
	T0 = datime(Year0, Month0, Day0, Hour0, Minute0, Second0),
	all_numbers([Year0, Month0, Day0, Hour0, Minute0, Second0]).

earlier_datime(T0, T1) :-
	real_datime(T0),
	real_datime(T1),
	T0 = datime(Year0, Month0, Day0, Hour0, Minute0, Second0),
	T1 = datime(Year1, Month1, Day1, Hour1, Minute1, Second1),
	(   Year0 < Year1 ;
	    Year0 = Year1, Month0 < Month1 ;
	    Year0 = Year1, Month0 = Month1, Day0 < Day1 ;
	    Year0 = Year1, Month0 = Month1, Day0 = Day1, Hour0 < Hour1 ;
	    Year0 = Year1, Month0 = Month1, Day0 = Day1, Hour0 = Hour1, Minute0 < Minute1 ;
	    Year0 = Year1, Month0 = Month1, Day0 = Day1, Hour0 = Hour1, Minute0 = Minute1, Second0 < Second1
	).

all_numbers([]).
all_numbers([F | R]) :-
	number(F),
	!,
	all_numbers(R).

%------------------------------------------------------------------------------------

timestamped_file(Prefix, Datime, Extension, File) :-
	datime_to_timestamp(Datime, Timestamp),
	(   Extension = '' ->
	    
	    format_to_atom('~w_~w', [Prefix, Timestamp], File) ;
	    
	    format_to_atom('~w_~w.~w', [Prefix, Timestamp, Extension], File)
	),
	!.

%------------------------------------------------------------------------------------

% Find the next unused file of the form <Prefix>_NN.<Extension>

next_numbered_file(Prefix, Extension, File) :-
	next_numbered_file(Prefix, Extension, File, 0),
	!.
next_numbered_file(Prefix, Extension, File) :-
	format('~N*** Error: bad call: ~w~n', [next_numbered_file(Prefix, Extension, File)]),
	fail.

next_numbered_file(Prefix, Extension, File, N) :-
	N < 100,
	coerce_int_to_two_digit_atom(N, NAtom),
	format_to_atom('~w_~w.~w', [Prefix, NAtom, Extension], NextFile),
	!,
	(   file_exists(NextFile) ->
	    
	    N1 is N + 1,
	    next_numbered_file(Prefix, Extension, File, N1) ;

	    File = NextFile
	),
	!.

%------------------------------------------------------------------------------------

current_datime_formatted_as_HMS(DatimeAtom) :-
	datime(Datime),
	Datime = datime(_Y, _M, _D, H, Min, S),
	coerce_int_to_two_digit_atom(H, Hours),
	coerce_int_to_two_digit_atom(Min, Minutes),
	coerce_int_to_two_digit_atom(S, Seconds),
	format_to_atom('~w:~w:~w', [Hours, Minutes, Seconds], DatimeAtom),
	!.
current_datime_formatted_as_HMS(DatimeAtom) :-
	format('~N*** Error: bad call ~w~n', [current_datime_formatted_as_HMS(DatimeAtom)]),
	fail.
	
%------------------------------------------------------------------------------------

% Parse datime in format YYYY-MM-DD_HH-MM-SS

parse_datime(String, Datime) :-
	datime_string(Datime, String, []).

datime_string(datime(Y, M, D, H, Min, S)) -->
	optional_whitespace_sequence,
	integer_word(Y),
	"-",
	integer_word(M),
	"-",
	integer_word(D),
	"_",
	integer_word(H),
	"-",
	integer_word(Min),
	"-",
	integer_word(S),
	optional_whitespace_sequence.

%------------------------------------------------------------------------------------

:- dynamic notional_time/1.

set_notional_time(Datime) :-
	retractall(notional_time(_)),
	assertz(notional_time(Datime)).

unset_notional_time :-
	retractall(notional_time(_)).

get_notional_time(Datime) :-
	notional_time(Datime),
	!.

%------------------------------------------------------------------------------------

:- dynamic notional_speaker/1.

set_notional_speaker(Name) :-
	retractall(notional_speaker(_)),
	assertz(notional_speaker(Name)).

unset_notional_speaker :-
	retractall(notional_speaker(_)).

get_notional_speaker(Name) :-
	notional_speaker(Name),
	!.

%------------------------------------------------------------------------------------

datime_to_timestamp(Datime, Timestamp) :-
	Datime = datime(Y, M, D, H, Min, S),
	int_to_atom(Y, Year), 
	coerce_int_to_two_digit_atom(M, Month),
	coerce_int_to_two_digit_atom(D, Day), 
	coerce_int_to_two_digit_atom(H, Hours),
	coerce_int_to_two_digit_atom(Min, Minutes),
	coerce_int_to_two_digit_atom(S, Seconds),
	format_to_atom('~w-~w-~w_~w-~w-~w', [Year, Month, Day, Hours, Minutes, Seconds], Timestamp),
	!.

%------------------------------------------------------------------------------------

coerce_int_to_three_digit_atom(Int, AppendedAtom) :-
	Int < 10,
	safe_number_codes(Int, Chars),
	atom_codes(AppendedAtom, [0'0, 0'0 | Chars]),
	!.
coerce_int_to_three_digit_atom(Int, AppendedAtom) :-
	Int < 100,
	safe_number_codes(Int, Chars),
	atom_codes(AppendedAtom, [0'0 | Chars]),
	!.
coerce_int_to_three_digit_atom(Int, AppendedAtom) :-
	Int < 1000,
	int_to_atom(Int, AppendedAtom),
	!.

coerce_int_to_two_digit_atom(Int, AppendedAtom) :-
	Int < 10,
	safe_number_codes(Int, Chars),
	atom_codes(AppendedAtom, [0'0 | Chars]).
coerce_int_to_two_digit_atom(Int, AppendedAtom) :-
	Int < 100,
	int_to_atom(Int, AppendedAtom),
	!.

atom_to_int(Atom, Int) :-
	atom_codes(Atom, Chars),
	safe_number_codes(Int, Chars),
	integer(Int).

coerce_atom_to_int(Atom, Int) :-
	(   integer(Atom) ->
	    Atom = Int
	;
	    atom(Atom) ->
	    atom_to_int(Atom, Int)
	).

int_to_atom(Int, Atom) :-
	integer(Int),
	safe_number_codes(Int, Chars),
	atom_codes(Atom, Chars).

coerce_int_to_atom(Int, Atom) :-
	int_to_atom(Int, Atom),
	!.
coerce_int_to_atom(Int, Int).

is_number_atom(Atom) :-
	atom_to_int(Atom, _Int),
	!.

%------------------------------------------------------------------------------------

/*

Typical wavinfo output looks like this:

       Audio file information
       ----------------------
  file name:      wavfiles/countdown10_1.wav
  file size (B):  75862
  format:         riff
  encoding:       mulaw
  samping rate:   8000
  sample count:   75804
  min sample:     -12924
  max sample:     14460
  time (sec):     9.5
  tags:           0

We get the length of the file in seconds if we divide the sample count by the sample rate.

*/

length_of_wavfile_in_seconds(Wavfile, NSeconds) :-
	%require_sicstus_version(3, length_of_wavfile_in_seconds/2),
	absolute_windows_file_name(Wavfile, AbsWavfile),
	system_on_list_to_result([wavinfo, AbsWavfile], List),
	member(SampleCountLine, List),
	parse_as_sample_count_line(SampleCountLine, SampleCount),
	member(SampleRateLine, List),
	parse_as_sample_rate_line(SampleRateLine, SampleRate),
	NSeconds is SampleCount / SampleRate,
	!.
length_of_wavfile_in_seconds(Wavfile, NSeconds) :-
	format('~N*** Error: bad call: ~w~n', [length_of_wavfile_in_seconds(Wavfile, NSeconds)]),
	fail.

parse_as_sample_count_line(SampleCountLine, SampleCount) :-
	split_atom_into_words(SampleCountLine, Atoms),
	member(sample, Atoms),
	member('count:', Atoms),
	last(Atoms, NumberAtom),
	atom_to_int(NumberAtom, SampleCount).

parse_as_sample_rate_line(SampleRateLine, SampleRate) :-
	split_atom_into_words(SampleRateLine, Atoms),
	member(samping, Atoms),
	member( 'rate:', Atoms),
	last(Atoms, NumberAtom),
	atom_to_int(NumberAtom, SampleRate).

%------------------------------------------------------------------------------------

wavfile_format_and_encoding(Wavfile, Format, Encoding) :-
	absolute_windows_file_name(Wavfile, AbsWavfile),
	system_on_list_to_result([wavinfo, AbsWavfile], List),
	member(FormatLine, List),
	parse_as_format_line(FormatLine, Format),
	member(EncodingLine, List),
	parse_as_encoding_line(EncodingLine, Encoding),
	!.
wavfile_format_and_encoding(Wavfile, Format, Encoding) :-
	format('~N*** Error: bad call: ~w~n', [wavfile_format_and_encoding(Wavfile, Format, Encoding)]),
	fail.

%  format:         riff

parse_as_format_line(Line, Format) :-
	split_atom_into_words(Line, Atoms),
	member('format:', Atoms),
	last(Atoms, Format).

%  encoding:       mulaw

parse_as_encoding_line(Line, Encoding) :-
	split_atom_into_words(Line, Atoms),
	member('encoding:', Atoms),
	last(Atoms, Encoding).

%------------------------------------------------------------------------------------

wavconvert_file(AbsLocalWavfile, AbsGlobalWavfile, Encoding, Format) :-
	format_to_atom('wavconvert ~w ~w ~w ~w',
		       [AbsLocalWavfile, AbsGlobalWavfile, Encoding, Format],
		       Command),
	format('~N--- Executing command: ~w~n', [Command]),
	(   safe_shell(Command) ->
	    format('~N--- Converted ~w to ~w/~w, giving ~w~n',
		   [AbsLocalWavfile, Encoding, Format, AbsGlobalWavfile])
	;
	    otherwise ->
	    format('Error~n', []),
	    fail
	),
	!.

%------------------------------------------------------------------------------------

get_transcription_from_wavfile(Wavfile, WavfileTranscription) :-
	file_exists(Wavfile),
	open(Wavfile, read, S),
	get_transcription_from_wavfile_stream(S, WavfileTranscription),
	close(S),
	!,
	WavfileTranscription \== '*no_transcription*'.

/*
NIST_1A
   1024
channel_count -i 1
sample_count -i 18240
sample_rate -i 8000
sample_n_bytes -i 2
sample_sig_bits -i 16
sample_coding -s3 pcm
sample_byte_format -s2 01
recording_date -s11 09-Sep-2007
recording_time -s11 11:04:18.00
abs_safe_start -i 1760
rel_safe_start -i 0
rel_actual_start -i 4800
rel_actual_end -i 12880
rel_safe_end -i 18239
transcription -s22 is elisabeth attending
end_head
*/

get_transcription_from_wavfile_stream(S, WavfileTranscription) :-
	read_line(S, Line),
	(   Line = end_of_file ->
	    WavfileTranscription = '*no_transcription*'
	;
	    \+ is_prolog_string(Line) ->
	    get_transcription_from_wavfile_stream(S, WavfileTranscription)
	;
	    otherwise ->
	    split_string_into_words(Line, Words),
	    !,
	    (   Words = [end_head] ->
		WavfileTranscription = '*no_transcription*'
	    ;
		Words = [transcription, _ | WavfileTranscription] ->
		true
	    ;
		get_transcription_from_wavfile_stream(S, WavfileTranscription)
	    )
	).

%------------------------------------------------------------------------------------

add_transcription_to_wavfile(WavfileIn, TranscriptionAtom, WavfileOut) :-
	add_transcription_to_wavfile_binary(WavfileIn, TranscriptionAtom, WavfileOut).

%add_transcription_to_wavfile(WavfileIn, TranscriptionAtom, WavfileOut) :-
%	file_exists(WavfileIn),
%	open(WavfileIn, read, SIn),
%	open(WavfileOut, write, SOut),
%	atom_codes(TranscriptionAtom, TranscriptionChars),
%	length(TranscriptionChars, NCharsInTranscription),
%	format_to_atom('transcription -s~d ~w', [NCharsInTranscription, TranscriptionAtom], TranscriptionLineAtom),
%	
%	add_transcription_to_wavfile_stream(SIn, TranscriptionLineAtom, SOut),
%	
%	close(SIn),
%	close(SOut),
%	!.
%
%add_transcription_to_wavfile_stream(SIn, TranscriptionLineAtom, SOut) :-
%	read_line(SIn, Line),
%	(   Line = end_of_file ->
%	    format('~N*** Error: bad call to add_transcription_to_wavfile_stream/3: end of header not found~n', []),
%	    close(SIn),
%	    close(SOut),
%	    fail
%	;
%	    \+ is_prolog_string(Line) ->
%	    format('~N*** Error: bad call to add_transcription_to_wavfile_stream/3: end of header not found~n', []),
%	    close(SIn),
%	    close(SOut),
%	    fail
%	;
%	    otherwise ->
%	    split_string_into_words(Line, Words),
%	    !,
%	    (   Words = [end_head] ->
%		% No transcription line found; write out transcription line and end_head
%		format(SOut, '~N~w~nend_head~n', [TranscriptionLineAtom]),
%		copy_stream(SIn, SOut)
%	    ;
%		% Transcription line found; just write out transcription line
%		Words = [transcription | _Rest] ->
%		format(SOut, '~N~w~n', [TranscriptionLineAtom]),
%		copy_stream(SIn, SOut)
%	    ;
%		otherwise ->
%		format(SOut, '~N~s~n', [Line]),
%		add_transcription_to_wavfile_stream(SIn, TranscriptionLineAtom, SOut)
%	    )
%	).

%------------------------------------------------------------------------------------

add_transcription_to_wavfile_binary(WavfileIn, TranscriptionAtom, WavfileOut) :-
	file_exists(WavfileIn),
	atom_codes(TranscriptionAtom, TranscriptionChars),
	make_transcription_line(TranscriptionChars, TranscriptionLineChars),
	
	open(WavfileIn, read, SIn, [type(binary)]),
	open(WavfileOut, write, SOut, [type(binary)]),
	
	add_transcription_to_wavfile_stream_binary(SIn, TranscriptionLineChars, SOut),
	
	close(SIn),
	close(SOut),
	!.

make_transcription_line(TranscriptionChars, TranscriptionLineChars) :-
	length(TranscriptionChars, NCharsInTranscription),
	format_to_chars('transcription -s~d ~s\n', [NCharsInTranscription, TranscriptionChars], TranscriptionLineChars0),
	(   string_has_even_number_of_chars(TranscriptionLineChars0) ->
	    TranscriptionLineChars0 = TranscriptionLineChars
	;
	    otherwise ->
	    append(TranscriptionChars, " ", TranscriptionCharsWithExtraTrailingSpace),
	    make_transcription_line(TranscriptionCharsWithExtraTrailingSpace, TranscriptionLineChars)
	).

string_has_even_number_of_chars(Str) :-
	length(Str, N),
	0 is N mod 2.	

add_transcription_to_wavfile_stream_binary(SIn, TranscriptionLineChars, SOut) :-
	add_transcription_to_wavfile_stream_binary1(SIn, TranscriptionLineChars, SOut),
	!,
	copy_stream_binary(SIn, SOut).

add_transcription_to_wavfile_stream_binary1(SIn, TranscriptionLineChars, SOut) :-
	read_line_in_binary_mode(SIn, Line),
	(   Line = end_of_file ->
	    format('~N*** Error: bad call to add_transcription_to_wavfile_stream/3: end of header not found~n', []),
	    close(SIn),
	    close(SOut),
	    fail
	;
	    \+ is_prolog_string(Line) ->
	    format('~N*** Error: bad call to add_transcription_to_wavfile_stream/3: end of header not found~n', []),
	    close(SIn),
	    close(SOut),
	    fail
	;
	    otherwise ->
	    split_string_into_words(Line, Words),
	    !,
	    (   Words = [end_head] ->
		% No transcription line found; write out transcription line and end_head
		write_string_binary(SOut, TranscriptionLineChars),
		write_string_binary(SOut, Line)
	    ;
		% Transcription line found; just write out transcription line
		Words = [transcription | _Rest] ->
		write_string_binary(SOut, TranscriptionLineChars)
	    ;
		otherwise ->
		write_string_binary(SOut, Line),
		add_transcription_to_wavfile_stream_binary1(SIn, TranscriptionLineChars, SOut)
	    )
	).

%------------------------------------------------------------------------------------


sort_file(InFile, OutFile) :-
	%require_sicstus_version(3, sort_file/2),
        absolute_file_name(InFile,AbsIn),
        absolute_file_name(OutFile,AbsOut),
	system_on_list([sort, '<', AbsIn, '>', AbsOut]).	

%------------------------------------------------------------------------------------

%get_pulist(PuList) :-
%	system_on_list_to_result([pulist], [_Header | RawPuList]),
%	parse_pulist(RawPuList, PuList).
%
%parse_pulist([], []).
%parse_pulist([F | R], [F1 | R1]) :-
%	parse_pulist_line(F, F1),
%	parse_pulist(R, R1).
%
%parse_pulist_line(Atom, Line) :-
%	atom_codes(Atom, Chars),
%	parse_pulist_line_chars(Chars, Line).
%
%parse_pulist_line_chars(Chars, Line) :-
%	pulist_line(Line, Chars, []).
%
%pulist_line(process_and_pid(Process, Pid)) -->
%	non_whitespace_word(Process0), {lowercase_atom(Process0, Process)},	    
%	white_spaces_including_tabs,
%	integer_word(Pid), 
%	arbitrary_sequence.

%------------------------------------------------------------------------------------

existing_absolute_file_name(File0, File) :-
	absolute_file_name(File0, File),
	file_exists(File),
	!.
existing_absolute_file_name(File0, _File) :-
	format('~N*** Error: unable to interpret ~w as name of file~n', [File0]),
	fail.

%------------------------------------------------------------------------------------

%files_in_directory(Directory, Files, LongOrShort) :-
%	system_on_list_to_result([ls, Directory], PlainFiles),
%	(   LongOrShort = long ->
%	    add_directory_in_pathname_list(PlainFiles, Directory, Files) ;
%	    Files = PlainFiles
%	).

% We can't do it this way, since for some reason Windows gives a different result
% when we invoke 'dir' directly. Bizarre.
%get_files_in_directory(Directory, AllFiles) :-
%	absolute_windows_file_name(Directory, Directory1),
%	system_on_list_to_result([dir, '/b', Directory1], AllFiles).

% This always looked like a hack, and has now stopped working - Jul 30, 2005
% Should use directory_files/2 from library(system)
%get_files_in_directory(Directory, AllFiles) :-
%	environ('OS', 'Windows_NT'),
%	absolute_windows_file_name(Directory, Directory1),
%	join_with_spaces([dir, '/b', Directory1], Command),
%	write_atom_list_to_file([Command], 'C:\\tmp_command.bat'),
%	system_on_list_to_result(['C:\\tmp_command.bat'], OutputLines),
%	remove_dummy_intro_if_necessary(OutputLines, AllFiles),
%	!.
%get_files_in_directory(Directory, AllFiles) :-
%	\+ environ('OS', 'Windows_NT'),
%	system_on_list_to_result([ls, Directory], AllFiles),
%	!.
%
%remove_dummy_intro_if_necessary(OutputLines, AllFiles) :-
%	OutputLines = [_Dummy1, _Dummy2 | AllFiles],
%	!.
%remove_dummy_intro_if_necessary(OutputLines, OutputLines) :-
%	!.

add_directory_in_pathname_list([], _Directory, []).
add_directory_in_pathname_list([F | R], Directory, [F1 | R1]) :-
	add_directory_in_pathname(F, Directory, F1),
	add_directory_in_pathname_list(R, Directory, R1).

%------------------------------------------------------------------------------------

copy_file_between_directories(File, Dir1, Dir2) :-
	%require_sicstus_version(3, copy_file_between_directories/3),
	add_directory_in_pathname(File, Dir1, From),
	add_directory_in_pathname(File, Dir2, To),
	system_on_list([copy, From, To]). 

%------------------------------------------------------------------------------------

% Should be able to do it like this, but for some reason the /q switch
% doesn't seem to work when we call it through the Prolog interface...
%delete_all_files_in_directory(Directory) :-
%	add_directory_in_pathname('*.*', Directory, FullWildcardPathname),
%	system_on_list([del, '/q', FullWildcardPathname]).

%delete_all_files_in_directory(Directory) :-
%	files_in_directory(Directory, Files, long),
%	delete_all_files_in_list(Files).

delete_all_files_in_list([]).
delete_all_files_in_list([F | R]) :-
	delete_file(F),
	delete_all_files_in_list(R).

%------------------------------------------------------------------------------------

delete_file_with_status(FileString, Status) :-
	\+ is_prolog_string(FileString),
	!,
	format_to_chars('Error: ~w is not a string', [FileString], Status).
delete_file_with_status(FileString, Status) :-
	atom_codes(File, FileString),
	(   \+ file_exists(File) ->
	    format_to_chars('Warning: file ~w does not exist, no need to delete', [File], Status)
	;
	    otherwise ->
	    (   delete_file(File) ->
		Status = "ok"
	    ;
		otherwise ->
		format_to_chars('Error: unable to delete existing file ~w', [File], Status)
	    )
	).

%------------------------------------------------------------------------------------

pathname_has_extension(File, Extension) :-
	lowercase_atom(File, File1),
	lowercase_atom(Extension, Extension1),
	atom_codes(File1, File1Chars),
	atom_codes(Extension1, Extension1Chars),
	append(_Prefix, [0'. | Extension1Chars], File1Chars).

%------------------------------------------------------------------------------------

split_off_extension_from_pathname(Pathname, WithoutExtension, Extension) :-
	atom_codes(Pathname, PathnameChars),
	split_off_extension_from_pathname_chars(PathnameChars, WithoutExtensionChars, ExtensionChars),
	atom_codes(WithoutExtension, WithoutExtensionChars),
	atom_codes(Extension, ExtensionChars),
	!.

split_off_extension_from_pathname_chars([], [], []).
split_off_extension_from_pathname_chars([0'. | R], [], R) :-
	\+ member(0'., R),
	!.
split_off_extension_from_pathname_chars([F | R], [F | R1], ExtensionChars) :-
	!,
	split_off_extension_from_pathname_chars(R, R1, ExtensionChars).
	
%------------------------------------------------------------------------------------

change_extension_in_file(File, NewExtension, NewFile) :-
	safe_absolute_file_name(File, AbsFile),
	split_off_extension_from_pathname(AbsFile, FileWithoutExtension, _OldExtension),
	format_to_atom('~w.~w', [FileWithoutExtension, NewExtension], NewFile),
	!.
change_extension_in_file(File, NewExtension, NewFile) :-
	format('~N*** Error: bad call: ~w~n',
	       [change_extension_in_file(File, NewExtension, NewFile)]),
	fail.

%------------------------------------------------------------------------------------

derived_file_names(BaseFile, CommonLabel, Labels, Extensions, Files) :-
	check_args_for_derived_file_names(BaseFile, CommonLabel, Labels, Extensions, Files),
	safe_absolute_file_name(BaseFile, AbsBaseFile),
	split_off_extension_from_pathname(AbsBaseFile, AbsBaseFileWithoutExtension, _OldExtension),
	derived_file_names1(AbsBaseFileWithoutExtension, CommonLabel, Labels, Extensions, Files).

check_args_for_derived_file_names(BaseFile, CommonLabel, Labels, Extensions, Files) :-
	check_args_for_derived_file_names1(BaseFile, CommonLabel, Labels, Extensions, Files),
	!.
check_args_for_derived_file_names(BaseFile, CommonLabel, Labels, Extensions, Files) :-
	format('~N*** Error: bad call: ~w~n', [check_args_for_derived_file_names(BaseFile, CommonLabel, Labels, Extensions, Files)]),
	fail.
	       
check_args_for_derived_file_names1(BaseFile, CommonLabel, Labels, Extensions, Files) :-
	(   safe_absolute_file_name(BaseFile, _AbsBaseFile) ->
	    true
	;
	    format('~NCannot interpret first arg as file~n', []),
	    fail
	),
	(   atomic(CommonLabel) ->
	    true
	;
	    format('~NSecond arg is not atomic~n', []),
	    fail
	),
	length(Labels, NLabels),
	length(Extensions, NExtensions),
	length(Files, NFiles),
	sort([NLabels, NExtensions, NFiles], Ns),
	(   length(Ns, 1) ->
	    true
	;
	    format('~NList args have different lengths~n', []),
	    fail
	).

derived_file_names1(_Base, _CommonLabel, [], [], []).
derived_file_names1(Base, CommonLabel, [Label | Labels], [Extension | Extensions], [File | Files]) :-
	derived_file_name(Base, CommonLabel, Label, Extension, File),
	!,
	derived_file_names1(Base, CommonLabel, Labels, Extensions, Files).

derived_file_name(Base, CommonLabel, Label, Extension, File) :-
	(   CommonLabel = '' ->
	    format_to_atom('~w_~w.~w', [Base, Label, Extension], File)
	;
	    otherwise ->
	    format_to_atom('~w_~w_~w.~w', [Base, CommonLabel, Label, Extension], File)
	).

%------------------------------------------------------------------------------------

escape_spaces_in_atom(Atom, Atom1) :-
	atom_codes(Atom, Chars),
	escape_spaces_in_chars(Chars, Chars1),
	atom_codes(Atom1, Chars1).

escape_spaces_in_chars([], []).
% Don't escape already escaped spaces
escape_spaces_in_chars([0'\\, 0' | R], [0'\\, 0' | R1]) :-
	!,
	escape_spaces_in_chars(R, R1).
escape_spaces_in_chars([0' | R], [0'\\, 0' | R1]) :-
	!,
	escape_spaces_in_chars(R, R1).
escape_spaces_in_chars([F | R], [F | R1]) :-
	!,
	escape_spaces_in_chars(R, R1).

%------------------------------------------------------------------------------------

unescape_spaces_in_atom(Atom, Atom1) :-
	atom_codes(Atom, Chars),
	unescape_spaces_in_chars(Chars, Chars1),
	atom_codes(Atom1, Chars1).

unescape_spaces_in_chars([], []).
unescape_spaces_in_chars([0'\\, 0' | R], [0' | R1]) :-
	!,
	unescape_spaces_in_chars(R, R1).
unescape_spaces_in_chars([F | R], [F | R1]) :-
	!,
	unescape_spaces_in_chars(R, R1).

%------------------------------------------------------------------------------------

system_on_list(List) :-
	%require_sicstus_version(3, system_on_list/1),
	join_with_spaces(List, Command),
	safe_shell(Command, Status),
	(  Status \== 0 ->
	    format('~N*** Warning: command ~w returned with status = ~d~n', [Command, Status]) ;
	    true
	).

%------------------------------------------------------------------------------------

shell_writing_output_to_file_and_printing(Command, TraceFile1, TraceFile2, Label) :-
	(   running_under_windows ->
	    format_to_atom('~w > ~w 2> ~w', [Command, TraceFile1, TraceFile2], FullCommand)
	;
	    otherwise ->
	    format_to_atom('~w >& ~w', [Command, TraceFile1], FullCommand)
        ),

	format('~N~n--- Executing "~w"~n~n', [FullCommand]),
	safe_shell(FullCommand, Status),
	format('~N--- Output from ~w ---~n', [Label]),
	print_file(TraceFile1),
	(   running_under_windows ->
	    print_file(TraceFile2)
	;
	    otherwise ->
	    true
	),
	format('~N--- End of output from ~w ---~n', [Label]),
	(   Status = 0 ->
	    format('~N--- ~w completed', [Label])
	;
	    otherwise ->
	    format('~N--- ~w failed', [Label])
	).

%------------------------------------------------------------------------------------

shell_sequence_operator(Op) :-
	(      running_under_windows ->
               Op1='&'
	;
               Op1=';'
        ),
	Op = Op1,
	!.

%------------------------------------------------------------------------------------

system_on_list_to_result(List, Result) :-
	%require_sicstus_version(3, system_on_list_to_result/2),
	absolute_windows_file_name('$REGULUS/Prolog/tmp_command_output_xxx', TmpOutputFile),
	append(List, ['>', TmpOutputFile], FullList),
	system_on_list(FullList),
	(   file_exists(TmpOutputFile) ->
	    
	    read_file_to_atom_list(TmpOutputFile, Result),
	    delete_file(TmpOutputFile);
	    
	    Result = []
	).

%------------------------------------------------------------------------------------

get_java_version(Version) :-
	on_exception(
		     _Exception,
		     get_java_version1(Version),
		     fail
		    ).

get_java_version1(Version) :-
	safe_exec('java -version', [std, null, pipe(S)], _PID),
	read_line(S, FirstLine),
	close(S),
	extract_java_version_from_first_line_of_java_version(FirstLine, Version),
	!.

% Line should look something like this:
% java version "1.5.0_06"
extract_java_version_from_first_line_of_java_version(Line, Version) :-
	split_string_into_words(Line, Components),
	last(Components, LastComponent),
	atom_codes(LastComponent, LastComponentChars),
	java_version(Version, LastComponentChars, []).

java_version([First, Second, Third, Fourth]) -->
	[0'"],
	integer_word(First),
	[0'.],
	integer_word(Second),
	[0'.],
	integer_word(Third),
	[0'_],
	integer_word(Fourth),
	[0'"],
	any_chars,
	!.

any_chars -->
	[_X],
	!,
	any_chars.
any_chars -->
	[].

%------------------------------------------------------------------------------------

last_wavfile_in_dir(Dir, Wavfile) :-
	safe_absolute_file_name(Dir, AbsDir),
	(   safe_directory_exists(AbsDir) ->
	    true
	;
	    format('~N*** Error: can\'t find directory ~w~n', [AbsDir]),
	    fail
	),
	safe_directory_files(AbsDir, Wavfiles),
	tag_wavfiles_by_number(Wavfiles, TaggedWavfiles),
	(   TaggedWavfiles = [] ->
	    format('~N*** Error: no wavfiles found in directory ~w~n', [AbsDir]),
	    fail
	;
	    keysort(TaggedWavfiles, SortedTaggedWavfiles),
	    unkey_list(SortedTaggedWavfiles, SortedWavfiles),
	    reverse(SortedWavfiles, ReversedSortedWavfiles),
	    ReversedSortedWavfiles = [Wavfile | _Rest]
	),
	!.
last_wavfile_in_dir(Dir, Wavfile) :-
	format('~N*** Error: bad call: ~w~n', [last_wavfile_in_dir(Dir, Wavfile)]).

tag_wavfiles_by_number([], []).
tag_wavfiles_by_number([F | R], [Key-F | R1]) :-
	wavfile_number(F, Key),
	!,
	tag_wavfiles_by_number(R, R1).
tag_wavfiles_by_number([_F | R], R1) :-
	!,
	tag_wavfiles_by_number(R, R1).

wavfile_number(Wavfile, Number) :-
	atom_codes(Wavfile, WavfileChars),
	append("utt", RestChars, WavfileChars),
	append(NumberChars, ".wav", RestChars),
	safe_number_codes(Number, NumberChars),
	!.

%------------------------------------------------------------------------------------

add_directory_in_pathname(File, Directory, File1) :-
	append_atoms([Directory, File], 0'\\, File1).

%------------------------------------------------------------------------------------

% wrap_file_names_with_search_paths(+Files, +SearchPath, -Files1)

wrap_file_names_with_search_paths(Files, SearchPath, Files1) :-
	is_list(Files),
	atom(SearchPath),
	wrap_file_names_with_search_paths1(Files, SearchPath, Files1),
	!.
wrap_file_names_with_search_paths(Files, SearchPath, Files1) :-
	format('~N*** Error: bad call: ~w~n', [wrap_file_names_with_search_paths(Files, SearchPath, Files1)]).

wrap_file_names_with_search_paths1([], _, []).
wrap_file_names_with_search_paths1([F | R], SearchPath, [F1 | R1]) :-
	F1 =.. [SearchPath, F],
	!,
	wrap_file_names_with_search_paths1(R, SearchPath, R1).

%------------------------------------------------------------------------------------

absolute_windows_file_name(File, AbsoluteWindowsFile) :-
	absolute_file_name(File, AbsoluteFile),
	change_slash_to_backslash(AbsoluteFile, AbsoluteWindowsFile).

%------------------------------------------------------------------------------------

directory_and_file_for_pathname(Pathname, Directory, File) :-
	atom_codes(Pathname, PathnameChars),
	portion_after_last_slash(PathnameChars, FileChars),
	append(DirectoryChars, [0'/ | FileChars], PathnameChars),
	atom_codes(Directory, DirectoryChars),
	atom_codes(File, FileChars),
	!.
directory_and_file_for_pathname(Pathname, Directory, File) :-
	format('~N*** Error: bad call: ~w~n', [directory_and_file_for_pathname(Pathname, Directory, File)]),
	fail.

portion_after_last_slash([0'/ | R], End) :-
	\+ member(0'/, R),
	!,	
	R = End.
portion_after_last_slash([0'/ | R], End) :-
	portion_after_last_slash(R, End).
portion_after_last_slash([_F | R], End) :-
	portion_after_last_slash(R, End).

%------------------------------------------------------------------------------------

create_directory_if_necessary(Directory) :-
	safe_absolute_file_name(Directory, AbsDirectory),
	(   safe_directory_exists(AbsDirectory) ->
	    true
	;
	    otherwise ->
	    format('~N--- Making directory ~w~n', [AbsDirectory]),
	    make_directory(AbsDirectory)
	).

%------------------------------------------------------------------------------------

create_directory_above_if_necessary(Directory) :-
	safe_absolute_file_name(Directory, AbsDirectory),
	format_to_atom('~w/..', [AbsDirectory], DirectoryAbove),
	safe_absolute_file_name(DirectoryAbove, AbsDirectoryAbove),
	create_directory_if_necessary(AbsDirectoryAbove).

%------------------------------------------------------------------------------------

create_directory_for_file_if_necessary(Pathname) :-
	create_directory_for_file_if_necessary(Pathname, _AbsDirectory).

create_directory_for_file_if_necessary(Pathname, AbsDirectory) :-
	directory_and_file_for_pathname(Pathname, Directory, _File),
	safe_absolute_file_name(Directory, AbsDirectory),
	(   safe_directory_exists(Directory) ->
	    true
	;
	    otherwise ->
	    create_directory_for_file_if_necessary(Directory)
	),
	!,
	create_directory_if_necessary(Directory).

%------------------------------------------------------------------------------------

parent_directory(Dir, AbsParentDir) :-
	format_to_atom('~w/..', [Dir], ParentDir),
	safe_absolute_file_name(ParentDir, AbsParentDir).

%------------------------------------------------------------------------------------

format_to_atom(FormatString, Args, Atom) :-
	format_to_chars(FormatString, Args, String),
	atom_codes(Atom, String).

%------------------------------------------------------------------------------------

change_slash_to_backslash(N, N) :-
	number(N),
	!.
change_slash_to_backslash(A, A1) :-
	atom(A),
	!,
	atom_codes(A, Chars),
	change_slash_to_backslash_in_string(Chars, Chars1),
	atom_codes(A1, Chars1).
change_slash_to_backslash(T, T1) :-
	functor(T, F, N),
	functor(T1, F, N),
	change_slash_to_backslash_args(N, T, T1).

change_slash_to_backslash_args(0, _T, _T1).
change_slash_to_backslash_args(I, T, T1) :-
	I > 0,
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	change_slash_to_backslash(Arg, Arg1),
	I1 is I - 1,
	!,
	change_slash_to_backslash_args(I1, T, T1).

change_slash_to_backslash_in_string([], []).
change_slash_to_backslash_in_string([F | R], [F1 | R1]) :-
	(   F = 0'/ ->
	    F1 = 0'\\ ;
	    F1 = F
	),
	!,
	change_slash_to_backslash_in_string(R, R1).

%------------------------------------------------------------------------------------

choose_from_text_menu(Question, Alist, Answer) :-
	format('~N~n', []),
	length(Alist, NChoices),
	display_menu_alist(Alist, 1),
	format('~N~n~w ', [Question]),
	read_line(user, Chars),
	split_string_into_words(Chars, Words),
	(   ( Words = [IntAtom], atom_to_int(IntAtom, N), integer(N), 0 < N, N =< NChoices ) ->
	    safe_nth(N, Alist, Answer-_Text)
	;
	    otherwise ->
	    format('~N~nPlease give a number from 1 to ~d~n', [NChoices]),
	    choose_from_text_menu(Question, Alist, Answer)
	).

display_menu_alist([], _N).
display_menu_alist([_Answer-Text | R], I) :-
	format('~N ~d~4|~w~n', [I, Text]),
	I1 is I + 1,
	!,
	display_menu_alist(R, I1).

%------------------------------------------------------------------------------------

get_confirmation(FormatAtom, Args) :-
	format(FormatAtom, Args),
	read_line(user, Chars),
	split_string_into_words(Chars, Words),
	(   Words = [y] ->
	    true ;

	    Words = [n] ->
	    fail ;

	    format('~NPlease answer \'y\' or \'n\'~n', []),
	    !,
	    get_confirmation(FormatAtom, Args)
	).

%------------------------------------------------------------------------------------
% Read InFile, and represent the output as a string.

read_unicode_file_to_string(InFile, String) :-
	open(InFile, read, SIn, [encoding('UTF-8')]),
	read_stream_to_string_list(SIn, List),
	append_list(List, String),
	close(SIn).

%------------------------------------------------------------------------------------

read_lines_until_non_empty_line(Stream, String) :-
	read_line(Stream, String0),
	!,
	(   String0 = [] ->
	    read_lines_until_non_empty_line(Stream, String)
	;
	    otherwise ->
	    String = String0
	).

%------------------------------------------------------------------------------------
% Read InFile, and represent the output as a string.

read_file_to_string(InFile, String) :-
	open(InFile, read, SIn),
	read_stream_to_string_list(SIn, List),
	append_list(List, String),
	close(SIn).

read_file_to_string(InFile, default_encoding, String) :-
	!,
	read_file_to_string(InFile, String).
read_file_to_string(InFile, Encoding, String) :-
	open(InFile, read, SIn, [encoding(Encoding)]),
	read_stream_to_string_list(SIn, List),
	append_list(List, String),
	close(SIn).

read_stream_to_string_list(SIn, List) :-
	read_line(SIn, Line),
	read_stream_to_string_list1(Line, SIn, List).

read_stream_to_string_list1(Line, _SIn, List) :-
	Line = end_of_file,
	!,
	List = [].
read_stream_to_string_list1(Line, SIn, [LineWithNL | R]) :-
	append(Line, "\n", LineWithNL),
	!,
	read_stream_to_string_list(SIn, R).

%------------------------------------------------------------------------------------

unicode_file_to_normal_file(InFile, OutFile) :-
	read_unicode_file_to_atom_list(InFile, List),
	write_atom_list_to_file(List, OutFile).

%------------------------------------------------------------------------------------

normal_file_to_unicode_file(InFile, OutFile) :-
	normal_file_to_unicode_file(InFile, OutFile, 'UTF-8').

normal_file_to_unicode_file(InFile, OutFile, Encoding) :-
	read_file_to_atom_list(InFile, List),
	write_atom_list_to_unicode_file(List, OutFile, Encoding).

%------------------------------------------------------------------------------------
% Read InFile, and represent the output as a list of atoms, with each atom
% representing a line.

read_file_to_atom_list(InFile, List) :-
	read_file_to_atom_list(InFile, default_encoding, List).

read_unicode_file_to_atom_list(InFile, List) :-
	read_file_to_atom_list(InFile, 'UTF-8', List).

read_file_to_atom_list(InFile, Encoding, List) :-
	(   Encoding = default_encoding ->
	    open(InFile, read, SIn)
	;
	    otherwise ->
	    open(InFile, read, SIn, [encoding(Encoding)])
	),
	read_stream_to_atom_list(SIn, List, 1),
	close(SIn).

read_stream_to_atom_list(SIn, List, I) :-
	read_line(SIn, Line),
	read_stream_to_atom_list1(Line, SIn, List, I).

read_stream_to_atom_list1(Line, _SIn, List, _I) :-
	Line = end_of_file,
	!,
	List = [].
read_stream_to_atom_list1(Line, SIn, [F | R], I) :-
	safe_atom_codes_for_reading_line_to_atom(F, Line, I),
	I1 is I + 1,
	read_stream_to_atom_list(SIn, R, I1).

%------------------------------------------------------------------------------------
% Read InFile, and represent the output as a list of strings.

read_unicode_file_to_string_list(InFile, List) :-
	open(InFile, read, SIn, [encoding('UTF-8')]),
	read_stream_to_simple_string_list(SIn, List),
	close(SIn).

read_file_to_simple_string_list(InFile, List) :-
	open(InFile, read, SIn),
	read_stream_to_simple_string_list(SIn, List),
	close(SIn).

read_stream_to_simple_string_list(SIn, List) :-
	read_line(SIn, Line),
	read_stream_to_simple_string_list1(Line, SIn , List).

read_stream_to_simple_string_list1(Line, _SIn, List) :-
	Line = end_of_file,
	!,
	List = [].
read_stream_to_simple_string_list1(Line, SIn, [Line | R]) :-
	read_stream_to_simple_string_list(SIn, R).

%------------------------------------------------------------------------------------
% As in read_file_to_atom_list/2 above, but at most the first N lines.

read_first_n_records_of_file_to_atom_list(InFile, List, N) :-
	open(InFile, read, SIn),
	read_first_n_records_of_stream_to_atom_list(SIn, List, 0, N),
	close(SIn),
	!.
read_first_n_records_of_file_to_atom_list(InFile, List, N) :-
	format('~N*** Error: bad call: ~w~n', [read_first_n_records_of_file_to_atom_list(InFile, List, N)]),
	fail.

read_first_n_records_of_stream_to_atom_list(_SIn, List, I, N) :-
	I >= N,
	List = [],
	!.
read_first_n_records_of_stream_to_atom_list(SIn, List, I, N) :-
	I < N,
	read_line(SIn, Line),
	read_first_n_records_of_stream_to_atom_list1(Line, SIn, List, I, N).

read_first_n_records_of_stream_to_atom_list1(Line, _SIn, List, _I, _N) :-
	Line = end_of_file,
	!,
	List = [].
read_first_n_records_of_stream_to_atom_list1(Line, SIn, [F | R], I, N) :-
	safe_atom_codes_for_reading_line_to_atom(F, Line, I),
	I1 is I + 1,
	read_first_n_records_of_stream_to_atom_list(SIn, R, I1, N).	

%------------------------------------------------------------------------------------

safe_atom_codes_for_reading_line_to_atom(Atom, Line, _I) :-
	length(Line, LineLength),
	LineLength =< 50000,
	atom_codes(Atom, Line),
	!.
safe_atom_codes_for_reading_line_to_atom(Atom, Line, I) :-
	length(Init, 200),
	append(Init, _, Line),
	atom_codes(Atom, Init),
	format('~N*** Warning: line ~d ("~s...") in file is longer than 50000 chars, truncating to 200~n', [I, Init]).

%------------------------------------------------------------------------------------
% Count the number of Prolog terms in a file.

number_of_prolog_records_in_file(InFile, N) :-
	open(InFile, read, SIn),
	number_of_prolog_records_in_stream(SIn, 0-N),
	close(SIn),
	!.
number_of_prolog_records_in_file(InFile, N) :-
	format('~N*** Error: bad call: ~w~n', [number_of_prolog_records_in_file(InFile, N)]),
	fail.

number_of_prolog_records_in_stream(SIn, CIn-COut) :-
	read(SIn, Term),
	number_of_prolog_records_in_stream1(Term, SIn, CIn-COut).

number_of_prolog_records_in_stream1(Term, _SIn, CIn-CIn) :-
	Term = end_of_file,
	!.
number_of_prolog_records_in_stream1(_Term, SIn, CIn-COut) :-
	CNext is CIn + 1,
	number_of_prolog_records_in_stream(SIn, CNext-COut).

%------------------------------------------------------------------------------------
% Read a file of Prolog terms into a list, succeeds or fails

safe_prolog_file_to_list(InFile, List) :-
	safe_prolog_file_to_list(InFile, List, default_encoding).

safe_prolog_file_to_list(InFile, List, Encoding) :-
	on_exception(Exception,
		     prolog_file_or_files_to_list(InFile, List, Encoding),
		     safe_prolog_file_to_list_handle_exception(Exception, InFile, Encoding)
		    ),
	!.

safe_prolog_file_to_list_handle_exception(Exception, InFile, Encoding) :-
	format('~N*** Error: exception raised when trying to read ~w as a sequence of Prolog terms with encoding ~w~n', [InFile, Encoding]),
	print_message(error, Exception),
	fail.

%------------------------------------------------------------------------------------

safe_prolog_file_to_list_printing_statistics(InFile, List) :-
	safe_prolog_file_to_list_printing_statistics(InFile, List, default_encoding).

safe_prolog_file_to_list_printing_statistics(InFile, List, Encoding) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_prolog_file_to_list(AbsInFile, List, Encoding),
	length(List, N),
	format('~N--- Read file (~d records) ~w~n', [N, AbsInFile]),
	!.

%------------------------------------------------------------------------------------
% Read a file, or a list of files, of Prolog terms into a list

prolog_file_or_files_to_list(FileOrFiles, List) :-
	prolog_file_or_files_to_list(FileOrFiles, List, default_encoding).

prolog_file_or_files_to_list(FileOrFiles, List, Encoding) :-
	prolog_file_or_files_to_list1(FileOrFiles, List-[], Encoding).

prolog_file_or_files_to_list1([], In-In, _Encoding) :-
	!.
prolog_file_or_files_to_list1([F | R], In-Out, Encoding) :-
	(   Encoding = default_encoding ->
	    open(F, read, SIn)
	;
	    otherwise ->
	    open(F, read, SIn, [encoding(Encoding)])
	),
	(   prolog_file_to_list_stream(SIn, F, In-Next) ->
	    true
	;
	    otherwise ->
	    format('~N*** Warning: unable to read file ~w, treating as empty~n', [F]),
	    In = Next
	),
	close(SIn),
	!,
	prolog_file_or_files_to_list1(R, Next-Out, Encoding).
prolog_file_or_files_to_list1(File, In-Out, Encoding) :-
	(   Encoding = default_encoding ->
	    open(File, read, SIn)
	;
	    open(File, read, SIn, [encoding(Encoding)])
	),
	prolog_file_to_list_stream(SIn, File, In-Out),
	close(SIn),
	!.

%------------------------------------------------------------------------------------

cat_prolog_files(InFiles, OutFile) :-
	prolog_file_or_files_to_list(InFiles, List),
	list_to_prolog_file(List, OutFile).

cat_prolog_files(InFiles, OutFile, Encoding) :-
	prolog_file_or_files_to_list(InFiles, List, Encoding),
	list_to_prolog_file_with_encoding(List, OutFile, Encoding).

%------------------------------------------------------------------------------------
% Read a file of Prolog terms into a list

regulus_file_to_list(InFile, List) :-
	open_regulus_file(InFile, read, SIn),
	prolog_file_to_list_stream(SIn, InFile, List-[]),
	close(SIn),
	!.
regulus_file_to_list_file(InFile, List) :-
	format('~N*** Error: bad call: ~w~n', [regulus_file_to_list_file(InFile, List)]),
	fail.

%------------------------------------------------------------------------------------
% Read a file of Prolog terms into a list

prolog_file_to_list_null_if_no_file(InFile, List) :-
	\+ safe_file_exists(InFile),
	!,
	List = [].
prolog_file_to_list_null_if_no_file(InFile, List) :-
	prolog_file_to_list(InFile, List).

prolog_file_to_list(InFile, List) :-
	open(InFile, read, SIn),
	prolog_file_to_list_stream(SIn, InFile, List-[]),
	close(SIn),
	!.
prolog_file_to_list(InFile, List) :-
	format('~N*** Error: bad call: ~w~n', [prolog_file_to_list(InFile, List)]),
	fail.

%------------------------------------------------------------------------------------

prolog_file_to_list_stream(SIn, ListIn-ListOut) :-
	prolog_file_to_list_stream(SIn, '(unknown file)', ListIn-ListOut).

prolog_file_to_list_stream(SIn, InFile, ListIn-ListOut) :-
	max_exceptions_when_reading_prolog_file(MaxExceptions),
	prolog_file_to_list_stream(SIn, InFile, ListIn-ListOut, 0-_NExceptions, MaxExceptions).

%prolog_file_to_list_stream(SIn, ListIn-ListOut) :-
%	safe_read(SIn, Term),
%	prolog_file_to_list_stream1(Term, SIn, ListIn-ListOut).
prolog_file_to_list_stream(SIn, InFile, ListIn-ListOut, NExIn-NExOut, MaxEx) :-
	(   NExIn > MaxEx ->
	    format('~N*** Error: more than ~d exceptions when reading file, giving up~n', [MaxEx]),
	    close(SIn),
	    fail
	; 
	    safe_read_no_exceptions(SIn, InFile, Term) ->
	    prolog_file_to_list_stream1(Term, SIn, InFile, ListIn-ListOut, NExIn-NExOut, MaxEx)
	;
	    otherwise ->
	    NExNext is NExIn + 1,
	    prolog_file_to_list_stream(SIn, InFile, ListIn-ListOut, NExNext-NExOut, MaxEx)
	).

prolog_file_to_list_stream1(Term, _SIn, _InFile, ListIn-ListIn, NExIn-NExIn, _MaxEx) :-
	Term = end_of_file,
	!.
prolog_file_to_list_stream1(Term, SIn, InFile, ListIn-ListOut, NExIn-NExOut, MaxEx) :-
	ListIn = [Term | ListNext],
	prolog_file_to_list_stream(SIn, InFile, ListNext-ListOut, NExIn-NExOut, MaxEx).

%------------------------------------------------------------------------------------

prolog_file_to_list_vars_as_consts(InFile, List, Encoding) :-
	(   Encoding = default_encoding ->
	    open(InFile, read, SIn)
	;
	    open(InFile, read, SIn, [encoding(Encoding)])
	),
	prolog_file_to_list_stream_vars_as_consts(SIn, InFile, List-[]),
	close(SIn),
	!.
prolog_file_to_list_vars_as_consts(InFile, List, Encoding) :-
	format('~N*** Error: bad call: ~w~n',
	       [prolog_file_to_list_vars_as_consts(InFile, List, Encoding)]),
	fail.

prolog_file_to_list_stream_vars_as_consts(SIn, ListIn-ListOut) :-
	prolog_file_to_list_stream_vars_as_consts(SIn, '(unknown file)', ListIn-ListOut).

prolog_file_to_list_stream_vars_as_consts(SIn, InFile, ListIn-ListOut) :-
	max_exceptions_when_reading_prolog_file(MaxExceptions),
	prolog_file_to_list_stream_vars_as_consts(SIn, InFile, ListIn-ListOut, 0-_NExceptions, MaxExceptions).

%prolog_file_to_list_stream(SIn, ListIn-ListOut) :-
%	safe_read(SIn, Term),
%	prolog_file_to_list_stream1(Term, SIn, ListIn-ListOut).
prolog_file_to_list_stream_vars_as_consts(SIn, InFile, ListIn-ListOut, NExIn-NExOut, MaxEx) :-
	(   NExIn > MaxEx ->
	    format('~N*** Error: more than ~d exceptions when reading file, giving up~n', [MaxEx]),
	    close(SIn),
	    fail
	; 
	    safe_read_no_exceptions_vars_as_consts(SIn, InFile, Term) ->
	    prolog_file_to_list_stream1_vars_as_consts(Term, SIn, InFile, ListIn-ListOut, NExIn-NExOut, MaxEx)
	;
	    otherwise ->
	    NExNext is NExIn + 1,
	    prolog_file_to_list_stream_vars_as_consts(SIn, InFile, ListIn-ListOut, NExNext-NExOut, MaxEx)
	).

prolog_file_to_list_stream1_vars_as_consts(Term, _SIn, _InFile, ListIn-ListIn, NExIn-NExIn, _MaxEx) :-
	Term = end_of_file,
	!.
prolog_file_to_list_stream1_vars_as_consts(Term, SIn, InFile, ListIn-ListOut, NExIn-NExOut, MaxEx) :-
	ListIn = [Term | ListNext],
	prolog_file_to_list_stream_vars_as_consts(SIn, InFile, ListNext-ListOut, NExIn-NExOut, MaxEx).

%------------------------------------------------------------------------------------

:- dynamic previously_loaded_prolog_include_file/1.

prolog_file_or_files_to_list_including_line_info(File, List) :-
	\+ is_list(File),
	!,
	prolog_file_or_files_to_list_including_line_info([File], List).
prolog_file_or_files_to_list_including_line_info(Files, List) :-
	is_list(Files),
	retractall(previously_loaded_prolog_include_file(_)),
	prolog_files_to_list_including_line_info(Files, List-[]).

prolog_files_to_list_including_line_info([], ListIn-ListIn).
prolog_files_to_list_including_line_info([F | R], ListIn-ListOut) :-
	prolog_file_to_list_including_line_info(F, ListIn-ListNext),
	!,
	prolog_files_to_list_including_line_info(R, ListNext-ListOut).

prolog_file_to_list_including_line_info(File, ListIn-ListOut) :-
	safe_absolute_file_name(File, AbsoluteFile),
	open(AbsoluteFile, read, S),
	prolog_stream_to_list_including_line_info(S, ListIn-ListOut, 0-_LastItemNumber, 0, AbsoluteFile),
	close(S).

prolog_stream_to_list_including_line_info(S, ListIn-ListOut, ItemNumber0-OutItemNumber, LastLine0, File) :-
	ItemNumber is ItemNumber0 + 1,
	LastLine is LastLine0 + 1,
	read(S, Term),
	line_count(S, CurrentLine),
	prolog_stream_to_list_including_line_info1(Term, S, ListIn-ListOut, ItemNumber-OutItemNumber, LastLine-CurrentLine, File).

prolog_stream_to_list_including_line_info1(end_of_file, _S, ListIn-ListIn, ItemNumber-ItemNumber, _Lines, _File) :-
	!.
prolog_stream_to_list_including_line_info1(include(IncludeFile), S, ListIn-ListOut, InItemNumber-OutItemNumber, _LastLine-CurrentLine, File) :-
	!,
	prolog_include_file_to_list_including_line_info(IncludeFile, File, ListIn-ListNext, InItemNumber-NextItemNumber),
	prolog_stream_to_list_including_line_info(S, ListNext-ListOut, NextItemNumber-OutItemNumber, CurrentLine, File).
prolog_stream_to_list_including_line_info1(Term, S, ListIn-ListOut, InItemNumber-OutItemNumber, LastLine-CurrentLine, File) :-
	LineInfo = line_info(InItemNumber, LastLine-CurrentLine, File),
	ListIn = [term(Term, LineInfo) | ListNext],
	!,
	prolog_stream_to_list_including_line_info(S, ListNext-ListOut, InItemNumber-OutItemNumber, CurrentLine, File).

prolog_include_file_to_list_including_line_info(IncludeFile, CurrentFile, ListIn-ListOut, InItemNumber-OutItemNumber) :-
	safe_absolute_file_relative_to_current(IncludeFile, CurrentFile, AbsoluteIncludeFile),
	prolog_include_file_to_list_including_line_info1(AbsoluteIncludeFile, ListIn-ListOut, InItemNumber-OutItemNumber).

prolog_include_file_to_list_including_line_info1(AbsoluteIncludeFile, ListIn-ListIn, InItemNumber-InItemNumber) :-
	previously_loaded_prolog_include_file(AbsoluteIncludeFile),
	!.
prolog_include_file_to_list_including_line_info1(AbsoluteIncludeFile, ListIn-ListOut, InItemNumber-OutItemNumber) :-
	format('~N  [Including file ~w]~n', [AbsoluteIncludeFile]),
	% Do the assert before we start reading rather than after in case we get a recursive include.
	asserta(previously_loaded_prolog_include_file(AbsoluteIncludeFile)),
	open(AbsoluteIncludeFile, read, S),
	prolog_stream_to_list_including_line_info(S, ListIn-ListOut, InItemNumber-OutItemNumber, 0, AbsoluteIncludeFile),
	close(S),
	!.

safe_absolute_file_relative_to_current(File, CurrentFile, AbsoluteFile) :-
	on_exception(
	_Exception, 
	safe_absolute_file_for_reading_relative_to_current1(File, CurrentFile, AbsoluteFile),
	fail
    ),
	!.
safe_absolute_file_relative_to_current(File, CurrentFile, _AbsoluteFile) :-
	format('~NError: *** Unable to interpret ~w (included in ~w) as the name of a readable file.~n', [File, CurrentFile]),
	fail.

safe_absolute_file_for_reading_relative_to_current1(File, CurrentFile, AbsoluteFile) :-
	directory_and_file_for_pathname(CurrentFile, CurrentDirectory, _),
	safe_working_directory(LastDirectory, CurrentDirectory),
	safe_absolute_file_name(File, AbsoluteFile),
	safe_working_directory(_, LastDirectory).

%------------------------------------------------------------------------------------

safe_list_to_prolog_file_printing_statistics(List, File) :-
	on_exception(Exception,
		     list_to_prolog_file_printing_statistics(List, File),
		     safe_list_to_prolog_file_handle_exception(Exception, File)
		    ),
	!.

safe_list_to_prolog_file_handle_exception(Exception, File) :-
	format('~N*** Error: exception raised when trying to create Prolog file~n', [File]),
	print_message(error, Exception),
	fail.

list_to_prolog_file_printing_statistics(List, File) :-
	safe_absolute_file_name(File, AbsFile),
	list_to_prolog_file(List, File),
	length(List, N),
	format('~N--- Printed file (~d records) ~w~n', [N, AbsFile]),
	!.

%------------------------------------------------------------------------------------

open_regulus_file(File, Mode, Stream) :-
	current_predicate(user:default_regulus_encoding/1),
	user:default_regulus_encoding(Encoding),
	!,
	open(File, Mode, Stream, [encoding(Encoding)]).
open_regulus_file(File, Mode, Stream) :-
	open(File, Mode, Stream).

%------------------------------------------------------------------------------------

divide_file_into_n_parts(File, NParts) :-
	safe_absolute_file_name(File, AbsFile),
	read_file_to_atom_list(AbsFile, List),
	length(List, N),
	format('~N--- Read file ~w (~d records)~n', [AbsFile, N]),
	divide_list_into_n_parts(List, NParts, SubLists),
	split_off_extension_from_pathname(AbsFile, BaseFile, Extension),
	write_parts_of_file(SubLists, 1, BaseFile, Extension),
	!.
divide_file_into_n_parts(File, NParts) :-
	format('~N*** Error: bad call: ~w~n', [divide_file_into_n_parts(File, NParts)]),
	fail.

divide_list_into_n_parts(List, N, SubLists) :-
	divide_list_into_n_parts1(List, 0, N, SubLists).

divide_list_into_n_parts1(_List, I, N, []) :-
	I >= N,
	!.
divide_list_into_n_parts1(List, I, N, [SubList | SubLists]) :-
	get_mod_n_elements_of_list(List, 1, I, N, SubList),
	I1 is I + 1,
	!,
	divide_list_into_n_parts1(List, I1, N, SubLists).

get_mod_n_elements_of_list([], _I, _Mod, _N, []).
get_mod_n_elements_of_list([F | R], I, Mod, N, Result) :-
	(   Mod is I mod N ->
	    Result = [F | R1]
	;
	    otherwise ->
	    Result = R1
	),
	I1 is I + 1,
	!,
	get_mod_n_elements_of_list(R, I1, Mod, N, R1).

write_parts_of_file([], _I, _BaseFile, _Extension).
write_parts_of_file([F | R], I, BaseFile, Extension) :-
	format_to_atom('~w_~d.~w', [BaseFile, I, Extension], File),
	length(F, N),
	write_atom_list_to_file(F, File),
	format('~N--- Written file (~d elements) ~w~n', [N, File]),
	I1 is I + 1,
	!,
	write_parts_of_file(R, I1, BaseFile, Extension).
	
%------------------------------------------------------------------------------------

list_to_regulus_file(List, File) :-
	open_regulus_file(File, write, S),
	list_to_prolog_stream(List, S),
	close(S),
	!.

%------------------------------------------------------------------------------------

% Write a list of terms to a Prolog-readable file.

list_to_prolog_file_with_encoding(List, File, Encoding) :-
	open(File, write, S, [encoding(Encoding)]),
	list_to_prolog_stream(List, S),
	close(S),
	!.
list_to_prolog_file_with_encoding(List, File, Encoding) :-
	format('~N*** Error: bad call: ~w~n', [list_to_prolog_file_with_encoding(List, File, Encoding)]),
	fail.

list_to_prolog_file(List, File) :-
	open_regulus_file(File, write, S),
	list_to_prolog_stream(List, S),
	close(S),
	!.
list_to_prolog_file(List, File) :-
	format('~N*** Error: bad call: ~w~n', [list_to_prolog_file(List, File)]),
	fail.

list_to_prolog_stream([], _S).
list_to_prolog_stream([F | R], S) :-
	%format(S, '~N~q.~n', [F]),
	portray_clause(S, F),
	%prettyprintq_to_stream(S, F, 0, 100), format(S, '.~n~n', []),
	!,
	list_to_prolog_stream(R, S).

%------------------------------------------------------------------------------------

normal_prolog_file_to_prolog_file_with_encoding(InFile, OutFile, Encoding) :-
	prolog_file_to_list(InFile, List),
	list_to_prolog_file_with_encoding(List, OutFile, Encoding).
	
%------------------------------------------------------------------------------------
						
% Write a list of terms to a Prolog-readable file.

list_to_prolog_file_prettyprint_unicode(List, File) :-
	open(File, write, S, [encoding('UTF-8'), encoding_signature(true)]),
	list_to_prolog_stream_prettyprint(List, S),
	close(S),
	!.
list_to_prolog_file_prettyprint_unicode(_List, File) :-
	format('~N*** Error: bad call: ~w~n', [list_to_prolog_file_prettyprint_unicode((...), File)]),
	fail.

list_to_prolog_file_prettyprint(List, File) :-
	list_to_prolog_file_prettyprint_with_encoding(List, File, default_encoding).

list_to_prolog_file_prettyprint_with_encoding(List, File, Encoding) :-
	(   Encoding = default_encoding ->
	    open(File, write, S)
	;
	    otherwise ->
	    open(File, write, S, [encoding(Encoding), encoding_signature(true)])
	),
	list_to_prolog_stream_prettyprint(List, S),
	close(S),
	!.
list_to_prolog_file_prettyprint_with_encoding(List, File) :-
	format('~N*** Error: bad call: ~w~n', [list_to_prolog_file_prettyprint(List, File)]),
	fail.

list_to_prolog_stream_prettyprint([], _S).
list_to_prolog_stream_prettyprint([F | R], S) :-
	%portray_clause(S, F),
	prettyprintq_to_stream(S, F),
	format(S, '.~N~n', []),
	!,
	list_to_prolog_stream_prettyprint(R, S).

%------------------------------------------------------------------------------------
% Read InFiles, and copy them to OutFile line by line

cat_files(InFiles, OutFile) :-
	open(OutFile, write, SOut),
	copy_files_to_stream(InFiles, SOut),
	close(SOut).

copy_files_to_stream([], _SOut).
copy_files_to_stream([F | R], SOut) :-
	copy_file_to_stream(F, SOut),
	!,
	copy_files_to_stream(R, SOut).

%------------------------------------------------------------------------------------

safe_rename_file_using_mv_if_possible(InFile, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	safe_rename_file_using_mv_if_possible1(AbsInFile, AbsOutFile).

safe_rename_file_using_mv_if_possible1(AbsInFile, AbsOutFile) :-
	on_cygwin_and_bin_dir_found,
	format_to_atom('mv ~w ~w', [AbsInFile, AbsOutFile], Command),
	safe_shell(Command, Status),
	Status = 0,
	!.
safe_rename_file_using_mv_if_possible1(AbsInFile, AbsOutFile) :-
	safe_rename_file(AbsInFile, AbsOutFile).

%------------------------------------------------------------------------------------

copy_files_to_dir([], _Dir).
copy_files_to_dir([F | R], Dir) :-
	copy_file_to_dir(F, Dir),
	!,
	copy_files_to_dir(R, Dir).

copy_file_to_dir(Pathname, NewDir) :-
	directory_and_file_for_pathname(Pathname, _Dir, File),
	format_to_atom('~w/~w', [NewDir, File], NewPathname),
	copy_file(Pathname, NewPathname),
	!.

%------------------------------------------------------------------------------------
% Read InFile, and copy to OutFile line by line

copy_file(InFile, OutFile) :-
	on_cygwin_and_bin_dir_found,
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	format_to_atom('cp ~w ~w', [AbsInFile, AbsOutFile], Command),
	safe_shell(Command, Status),
	Status = 0,
	!.
copy_file(InFile, OutFile) :-
	open(OutFile, write, SOut),
	copy_file_to_stream(InFile, SOut),
	close(SOut).

copy_file_to_stream(InFile, SOut) :-
	open(InFile, read, SIn),
	copy_stream(SIn, SOut),
	close(SIn).	

copy_stream(SIn, SOut) :-
	read_line(SIn, Line),
	copy_stream1(Line, SIn, SOut).

copy_stream1(Line, _SIn, _SOut) :-
	Line = end_of_file,
	!.
copy_stream1(Line, SIn, SOut) :-
	format(SOut, '~N~s~n', [Line]),
	copy_stream(SIn, SOut).

%------------------------------------------------------------------------------------

copy_file_binary(InFile, OutFile) :-
	open(InFile, read, SIn, [type(binary)]),
	open(OutFile, write, SOut, [type(binary)]),

	copy_stream_binary(SIn, SOut),

	close(SIn),
	close(SOut),
	!.

%------------------------------------------------------------------------------------

copy_stream_binary(SIn, SOut) :-
	get_byte(SIn, Byte),
	!,
	copy_stream_binary1(Byte, SIn, SOut).

copy_stream_binary1(Byte, _SIn, _SOut) :-
	Byte = -1,
	!.
copy_stream_binary1(Byte, SIn, SOut) :-
	put_byte(SOut, Byte),
	!,
	copy_stream_binary(SIn, SOut).

%------------------------------------------------------------------------------------

read_line_in_binary_mode(SIn, Line) :-
	get_byte(SIn, Byte),
	!,
	read_line_in_binary_mode1(Byte, SIn, Line).

% Didn't find end-of-line.
read_line_in_binary_mode1(Byte, _SIn, _Line) :-
	Byte = -1,
	!,
	fail.
read_line_in_binary_mode1(Byte, SIn, Line) :-
	end_of_line_byte(Byte),
	!,
	consume_end_of_line_byte_if_possible(SIn, Rest),
	Line = [Byte | Rest].
read_line_in_binary_mode1(Byte, SIn, [Byte | Rest]) :-
	!,
	read_line_in_binary_mode(SIn, Rest).

consume_end_of_line_byte_if_possible(SIn, Rest) :-
	peek_byte(SIn, Byte),
	(   end_of_line_byte(Byte) ->
	    get_byte(SIn, EOLByte),
	    Rest = [EOLByte]
	;
	    otherwise ->
	    Rest = []
	).

end_of_line_byte(0'\r).
end_of_line_byte(0'\n).

%------------------------------------------------------------------------------------

write_string_binary(_SOut, []).
write_string_binary(SOut, [F | R]) :-
	put_byte(SOut, F),
	!,
	write_string_binary(SOut, R).

%------------------------------------------------------------------------------------

:- dynamic line_to_subtract/1.

subtract_files(InFile, FileToRemove, OutFile) :-
	subtract_files(InFile, FileToRemove, default_encoding, OutFile).

subtract_files(InFile, FileToRemove, Encoding, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(FileToRemove, AbsFileToRemove),
	safe_absolute_file_name(OutFile, AbsOutFile),
	store_file_to_subtract(AbsFileToRemove, Encoding),
	
	(   Encoding = default_encoding ->
	    open(AbsInFile, read, SIn)
	;
	    otherwise ->
	    open(AbsInFile, read, SIn, [encoding(Encoding)])
	),

	(   Encoding = default_encoding ->
	    open(AbsOutFile, write, SOut)
	;
	    otherwise ->
	    open(AbsOutFile, write, SOut, [encoding(Encoding)])
	),

	copy_stream_subtracting(SIn, SOut),
	close(SIn),
	close(SOut).

store_file_to_subtract(AbsFileToRemove, Encoding) :-	
	retractall(line_to_subtract(_)),
	read_file_to_atom_list(AbsFileToRemove, Encoding, List),
	store_file_to_subtract1(List).

store_file_to_subtract1([]).
store_file_to_subtract1([F | R]) :-
	assertz(line_to_subtract(F)),
	!,
	store_file_to_subtract1(R).

copy_stream_subtracting(SIn, SOut) :-
	read_line(SIn, Line),
	copy_stream_subtracting1(Line, SIn, SOut).

copy_stream_subtracting1(Line, _SIn, _SOut) :-
	Line = end_of_file,
	!.
copy_stream_subtracting1(Line, SIn, SOut) :-
	atom_codes(Atom, Line),
	(   line_to_subtract(Atom) ->
	    true
	;
	    otherwise ->
	    format(SOut, '~N~s~n', [Line])
	),
	!,
	copy_stream_subtracting(SIn, SOut).


%------------------------------------------------------------------------------------
% Print lines Start to Finish of File

print_lines_from_file(File, Start, Finish) :-
	open(File, read, S),
	print_lines_from_stream(S, 0, Start, Finish),
	close(S).

print_lines_from_stream(_S, N, _Start, Finish) :-
	N > Finish,
	!.
print_lines_from_stream(S, N, Start, Finish) :-
	N =< Finish,
	read_line(S, Line),
	N1 is N + 1,
	print_lines_from_stream1(Line, S, N1, Start, Finish).

print_lines_from_stream1(Line, _S, _N, _Start, _Finish) :-
	Line = end_of_file,
	!.
print_lines_from_stream1(Line, S, N, Start, Finish) :-
	(   ( Start =< N, N =< Finish ) ->
	    format('~N~s~n', [Line]) ;

	    true
	),
	!,
	print_lines_from_stream(S, N, Start, Finish).

%------------------------------------------------------------------------------------
% Create a timestamped copy of File, called BackupFile

create_timestamped_copy(File, BackupFile) :-
	absolute_file_name(File, AbsFile),
	file_exists(AbsFile, [read, write]),
	(   split_atom_into_words(AbsFile, 0'., [MainAbsFile, Extension]) ->
	    
	    true ;
	    
	    MainAbsFile = AbsFile,
	    Extension = ''
	),
	datime(Datime),
	timestamped_file(MainAbsFile, Datime, Extension, BackupFile),
	copy_file(File, BackupFile),
	!.
create_timestamped_copy(File, BackupFile) :-
	format('~N*** Error: bad call: ~w~n', [create_timestamped_copy(File, BackupFile)]),
	fail.

%------------------------------------------------------------------------------------
% Read InFile, and print it out line by line.

print_file(File) :-
	read_file_to_atom_list(File, List),
	print_list_line_by_line(List).

print_list_line_by_line([]).
print_list_line_by_line([F | R]) :-
	format('~N~w~n', [F]),
	print_list_line_by_line(R).

%------------------------------------------------------------------------------------

% Read InFile. Each line is read as a string of characters Line.
% Make the call ParsePred(Line, Output, Args) for each line, and
% write Output to OutFile.

parse_file(InFile, OutFile, ParsePred, Args) :-
	init_parse_file_counter,
	open(InFile, read, SIn),
	open(OutFile, write, SOut),
	parse_stream(SIn, SOut, ParsePred, Args),
	close(SIn),
	close(SOut).

parse_stream(SIn, SOut, ParsePred, Args) :-
	repeat,
	read_line(SIn, Line),
	inc_and_notify_parse_file_counter(100),
	parse_stream1(Line, SOut, ParsePred, Args).

parse_stream1(Line, _SOut, _ParsePred, _Args) :-
	Line = end_of_file,
	!.
parse_stream1(Line, SOut, ParsePred, Args) :-
	Call =.. [ParsePred, Line, Output | Args],
	(  call(Call) -> 
	    ( functor(Output, discard, _) -> 
		true ;  
		format(SOut, "~N~q.~n", [Output])
	    ) ;  
	    format("~N*** Error: unable to process line ~s~n", [Line])
	),
	fail.

%------------------------------------------------------------------------------------

% Keeping track of how many records have been processed, and notifying user.

init_parse_file_counter :-
	retractall(parse_file_counter(_)),
	assert(parse_file_counter(0)).

inc_and_notify_parse_file_counter(Interval) :-
	parse_file_counter(I),
	retract(parse_file_counter(I)),
	I1 is I + 1,
	assert(parse_file_counter(I1)),
	notify_number_if_divisible_by(I1, Interval),
	!.

notify_number_if_divisible_by(I1, Interval) :-
	Mod is I1 mod Interval,
	Mod == 0,
	!,
	format("~d ", [I1]),
	flush_output(user).
notify_number_if_divisible_by(_I1, _Interval).

%------------------------------------------------------------------------------------	

% Read a line from stream S and return Result as a list of characters.

getline(S, Result) :-
	at_end_of_stream(S),
	!,
	Result = eof.
getline(S, Result) :-
	getline1(S, Result).

getline1(S, []) :-
	at_end_of_stream(S),
	!.
getline1(S, Chars) :-
	get0(S, Char),
	getline2(Char, S, Chars).

getline2(Char, _S, []) :-
	newline_char(Char),
	!.
getline2(Char, S, [Char|Chars]) :-
	getline1(S, Chars).

%------------------------------------------------------------------------------------	

getline_as_words(S, Words) :-
	getline(S, Chars),
	split_string_into_words(Chars, Words).

%------------------------------------------------------------------------------------	

split_atom_into_words(Atom, Words) :-
	atom_codes(Atom, Chars),
	split_string_into_words(Chars, Words).

split_atom_into_words(Atom, Char, Words) :-
	atom_codes(Atom, Chars),
	split_string_into_words(Chars, Char, Words).

%------------------------------------------------------------------------------------	

%split_string_into_words(String, Words) :-
%	split_string_into_words(String, 0' , Words).

split_string_into_words(String, Words) :-
	split_string_into_words_at_whitespaces(String, Words).

split_string_into_words_at_whitespaces([], Result) :-
	!,
	Result = [].
split_string_into_words_at_whitespaces([F | R], Result) :-
	whitespace_char(F),
	!,
	split_string_into_words_at_whitespaces(R, Result).
split_string_into_words_at_whitespaces(String, [FWord | RestWords]) :-
	split_off_non_whitespace_chars(String, FChars, RestString),
	atom_codes(FWord, FChars),
	!,
	split_string_into_words_at_whitespaces(RestString, RestWords).

split_off_non_whitespace_chars([], [], []) :-
	!.
split_off_non_whitespace_chars([F | R], [], [F | R]) :-
	whitespace_char(F),
	!.
split_off_non_whitespace_chars([F | R], [F | RWordChars], RRest) :-
	!,
	split_off_non_whitespace_chars(R, RWordChars, RRest).

%------------------------------------------------------------------------------------	

split_string_into_words([], _Char, Result) :-
	!,
	Result = [].
split_string_into_words(String, Char, [Word | Words]) :-
	split_off_first_word_from_string(String, Char, Word, RestString, EndedOnSeparatorP),
	(   ( EndedOnSeparatorP = ended_on_separator, RestString = [] ) ->
	    Words = ['']
	;
	    RestString = [] ->
	    Words = []
	;
	    otherwise ->
	    split_string_into_words(RestString, Char, Words)
	).

split_off_first_word_from_string(String, Char, Word, RestString, EndedOnSeparatorP) :-
	split_off_first_word_from_string1(String, Char, WordChars, RestString, EndedOnSeparatorP),
	atom_codes(Word, WordChars).

split_off_first_word_from_string1([], _Char, [], [], didnt_end_on_separator) :-
	!.
split_off_first_word_from_string1([Char | R], Char, [], R, ended_on_separator) :-
	!.
split_off_first_word_from_string1([F | R], Char, [F | R1], RestString, EndedOnSeparatorP) :-
	split_off_first_word_from_string1(R, Char, R1, RestString, EndedOnSeparatorP).

%------------------------------------------------------------------------------------	

split_string_into_strings([], _Char, Result) :-
	!,
	Result = [].
split_string_into_strings([Char, Char | String], Char, ["" | Strings]) :-
	!,
	split_string_into_strings([Char | String], Char, Strings).
split_string_into_strings([Char | String], Char, Strings) :-
	!,
	split_string_into_strings(String, Char, Strings).
split_string_into_strings(String, Char, [FirstString | Strings]) :-
	split_off_first_string_from_string(String, Char, FirstString, RestString),
	split_string_into_strings(RestString, Char, Strings).

split_off_first_string_from_string([], _Char, [], []) :-
	!.
split_off_first_string_from_string([Char | R], Char, [], [Char | R]) :-
	!.
split_off_first_string_from_string([F | R], Char, [F | R1], RestString) :-
	split_off_first_string_from_string(R, Char, R1, RestString).

%------------------------------------------------------------------------------------	

atom_name_contains_string(Atom, SubAtom) :-
	atom_codes(Atom, String),
	atom_codes(SubAtom, SubString),
	is_contiguous_sublist(SubString, String),
	!.

%------------------------------------------------------------------------------------	

timed_call(Goal, TimeTaken) :-
	statistics(runtime, [StartTime, _]),
	call(Goal),
	statistics(runtime, [EndTime, _]),
	% Time returned is in milliseconds.
	TimeTaken is ( float(EndTime) - float(StartTime) ) / 1000.0.

%------------------------------------------------------------------------------------	

absolute_timed_call(Goal, TimeTaken) :-
	statistics(walltime, [StartTime, _]),
	call(Goal),
	statistics(walltime, [EndTime, _]),
	% Time returned is in milliseconds.
	TimeTaken is ( float(EndTime) - float(StartTime) ) / 1000.0.

%------------------------------------------------------------------------------------

safe_call_saving_output(Goal, Status, OutputAtomList) :-
	on_exception(Exception,
		     safe_call_saving_output1(Goal, Status, OutputAtomList),
		     %( safe_call_saving_output1(Goal, Status), OutputAtomList = ['no output'] ),
		     (   
		         format('~N*** Internal error when performing: ~w',
				[safe_call_saving_output1(Goal, Status, OutputAtomList)]),
			 print_message(error, Exception),
			 format('~n', []),
			 fail
		     )
		    ),
	!.
safe_call_saving_output(_Goal, error_saving_trace_output, []) :-
	!.

safe_call_saving_output1(Goal, Status, OutputAtomList) :-
	get_tmp_trace_file(File),
	open(File, write, S),
	tell(S),
	safe_call_saving_output1(Goal, Status),
	told,
	read_file_to_atom_list(File, OutputAtomList),
	delete_file(File),
	length(OutputAtomList, NLines),
	format('--- Output (~d lines) saved to ~w~n', [NLines, File]),
	!.

safe_call_saving_output1(Goal, Status) :-
	on_exception(Exception,
		     (   call(Goal),
			 Status = ok
		     ),
		     (   format('~N*** Error: ', []),
			 print_message(error, Exception),
			 format('~n', []),
			 Status = error
		     )
		    ),
	!.
safe_call_saving_output1(_Goal, failed) :-
	!.

/*
For the above to work properly with output to stderr, you need to have this definition
loaded somewhere.

user:message_hook(Severity, _Message, Lines):-
	telling(MyStream),
	print_message_lines(MyStream, Severity, Lines).
*/

%------------------------------------------------------------------------------------

get_tmp_trace_file(File) :-
	random(1, 10000, N),
	format_to_atom('trace_~d.txt', [N], BaseFile),
	tmp_regulus_file(BaseFile, File).
	
tmp_regulus_file(BaseFile, AbsFile) :-
	safe_absolute_file_name('$REGULUS/tmp', Dir),
	(   safe_directory_exists(Dir) ->
	    true
	;
	    otherwise ->
	    make_directory(Dir)
	),
	format_to_atom('~w/~w', [Dir, BaseFile], AbsFile),
	!.
tmp_regulus_file(BaseFile, AbsFile) :-
	format2error('~N*** Error: bad call: ~w~n', [tmp_regulus_file(BaseFile, AbsFile)]),
	fail.

%------------------------------------------------------------------------------------

csv_fields_contain_word_ignoring_case(L, Keyword) :-
	member(Atom, L),
	atom(Atom),
	split_atom_into_words(Atom, Words),
	member(Word, Words),
	lowercase_atom(Word, Word1),
	Word1 = Keyword,
	!.

%------------------------------------------------------------------------------------

%csv_file_to_list_of_lists(File, List) :-
%	read_file_to_atom_list(File, AtomList),
%	csv_atom_list_to_list_of_lists(AtomList, List).
%
%csv_atom_list_to_list_of_lists([], []).
%csv_atom_list_to_list_of_lists([F | R], [F1 | R1]) :-
%	csv_atom_to_list(F, F1),
%	!,
%	csv_atom_list_to_list_of_lists(R, R1).
%
%csv_atom_to_list(Atom, List) :-
%	atom_codes(Atom, String),
%	csv_line(List, String, []),
%	!.
%csv_atom_to_list(Atom, _List) :-
%	format('~N*** Error: bad line in CSV file: \'~w\'~n', [Atom]),
%	fail.

csv_file_to_list_of_lists(File, List) :-
	csv_file_to_list_of_lists(File, 0'", 0',, List). %"'

csv_file_to_list_of_lists(File, DelimiterChar, SeparatorChar, List) :-
	csv_file_to_list_of_lists(File, default_encoding, DelimiterChar, SeparatorChar, List).

csv_file_to_list_of_lists(File, Encoding, DelimiterChar, SeparatorChar, List) :-
	check_csv_char(DelimiterChar, delimiter),
	check_csv_char(SeparatorChar, separator),
	csv_file_to_list_of_lists1(File, Encoding, DelimiterChar, SeparatorChar, List).

check_csv_char(Char, Type) :-
	normal_csv_char(Char, Type),
	!.
check_csv_char(Char, Type) :-
	integer(Char),
	format('~N*** Warning: unusual ~w char "~c" in call to csv_file_to_list_of_lists', [Type, Char]),
	!.
check_csv_char(Char, Type) :-
	format('~N*** Error: bad ~w char "~w" in call to csv_file_to_list_of_lists', [Type, Char]),
	fail.

normal_csv_char(0'", delimiter). %"
normal_csv_char(0'\', delimiter).

normal_csv_char(0',, separator).
normal_csv_char(0';, separator).
normal_csv_char(0' , separator).
normal_csv_char(0'\t , separator).

csv_file_to_list_of_lists1(File, Encoding, DelimiterChar, SeparatorChar, List) :-
	csv_file_to_string_list(File, Encoding, DelimiterChar, StringList),
	!,
	csv_string_list_to_list_of_lists(StringList, DelimiterChar, SeparatorChar, List).

csv_file_to_string_list(File, Encoding, DelimiterChar, StringList) :-
	read_file_to_string(File, Encoding, String),
	csv_string_to_lines(String, DelimiterChar, StringList).

csv_string_to_lines([], _DelimiterChar, []).
csv_string_to_lines(String, DelimiterChar, [FirstLine | RestLines]) :-
	get_line_from_csv_string(String-StringNext, DelimiterChar, FirstLine),
	!,
	csv_string_to_lines(StringNext, DelimiterChar, RestLines).

get_line_from_csv_string(StringIn-StringOut, DelimiterChar, Line) :-
	get_line_from_csv_string1(StringIn-StringOut, Line, DelimiterChar, not_inside_quotes),
	!.
get_line_from_csv_string(StringIn-_StringOut, _DelimiterChar, _Line) :-
	format('~N*** Error: malformed line in CSV file: "~s"~n', [StringIn]),
	fail.

get_line_from_csv_string1([]-[], [], _DelimiterChar, not_inside_quotes) :-
	!.
get_line_from_csv_string1([NewlineChar | StringOut]-StringOut, [], _DelimiterChar, not_inside_quotes) :-
	newline_char(NewlineChar),
	!.
get_line_from_csv_string1([DelimiterChar, DelimiterChar | StringNext]-StringOut, [DelimiterChar, DelimiterChar | LineRest], DelimiterChar, inside_quotes) :-  
	!,
	get_line_from_csv_string1(StringNext-StringOut, LineRest, DelimiterChar, inside_quotes).
get_line_from_csv_string1([DelimiterChar | StringNext]-StringOut, [DelimiterChar | LineRest], DelimiterChar, InsideQuotesIn) :-  
	(   InsideQuotesIn = not_inside_quotes ->
	    InsideQuotesNext = inside_quotes
	;
	    otherwise ->
	    InsideQuotesNext = not_inside_quotes
	),
	!,
	get_line_from_csv_string1(StringNext-StringOut, LineRest, DelimiterChar, InsideQuotesNext).
get_line_from_csv_string1([F | StringNext]-StringOut, [F | LineRest], DelimiterChar, InsideQuotes) :-
	!,
	get_line_from_csv_string1(StringNext-StringOut, LineRest, DelimiterChar, InsideQuotes).

get_line_from_csv_string1([]-[], [], _DelimiterChar, not_inside_quotes).

csv_string_list_to_list_of_lists([], _DelimiterChar, _SeparatorChar, []).
csv_string_list_to_list_of_lists([F | R], DelimiterChar, SeparatorChar, [F1 | R1]) :-
	csv_string_to_list(F, DelimiterChar, SeparatorChar, F1),
	!,
	csv_string_list_to_list_of_lists(R, DelimiterChar, SeparatorChar, R1).

csv_string_to_list(String, DelimiterChar, SeparatorChar, List) :-
	csv_line(List, DelimiterChar, SeparatorChar, String, []),
	!.
csv_string_to_list(String, _DelimiterChar, _SeparatorChar, _List) :-
	format('~N*** Error: bad line in CSV file: \'~s\'~n', [String]),
	fail.

csv_line([F | R], DelimiterChar, SeparatorChar) -->
	csv_field(F, DelimiterChar, SeparatorChar),
	!,
	csv_line_rest(R, DelimiterChar, SeparatorChar).

csv_line_rest(List, DelimiterChar, SeparatorChar) -->
	[SeparatorChar],
	!,
	csv_line(List, DelimiterChar, SeparatorChar).
csv_line_rest([], _DelimiterChar, _SeparatorChar) -->
	[].

csv_field(Atom, DelimiterChar, SeparatorChar) -->
	[DelimiterChar],    
	!,
	non_double_quote_char_string(Chars, DelimiterChar, SeparatorChar),
	[DelimiterChar],    
	{atom_codes(Atom, Chars)}.
csv_field(Atom, DelimiterChar, SeparatorChar) -->
	non_comma_char_string(Chars, DelimiterChar, SeparatorChar),
	{atom_codes(Atom, Chars)}.

non_double_quote_char_string([DelimiterChar | R], DelimiterChar, SeparatorChar) -->
	[DelimiterChar, DelimiterChar],
	!,
	non_double_quote_char_string(R, DelimiterChar, SeparatorChar).
non_double_quote_char_string([F | R], DelimiterChar, SeparatorChar) -->
	[F],
	{F \== DelimiterChar},    
	!,
	non_double_quote_char_string(R, DelimiterChar, SeparatorChar).
non_double_quote_char_string([], _DelimiterChar, _SeparatorChar) -->
	[].

non_comma_char_string([F | R], _DelimiterChar, SeparatorChar) -->
	[F],
	{F \== SeparatorChar},    
	!,
	non_comma_char_string(R, _DelimiterChar, SeparatorChar).
non_comma_char_string([], _DelimiterChar, _SeparatorChar) -->
	[].

%csv_atom_to_list(Atom, List) :-
%	atom_codes(Atom, String),
%	remove_quotes_in_string(String, String1),
%	split_string_into_words(String1, 0',, List).
%
%remove_quotes_in_string([], []).
%remove_quotes_in_string([0'" | R], R1) :-  %" To keep Emacs happy
%	!,
%	remove_quotes_in_string(R, R1).
%remove_quotes_in_string([F | R], [F | R1]) :-  
%	!,
%	remove_quotes_in_string(R, R1).

%-------------------------------------------------------

concatenate_headed_csv_files(Files, OutFile) :-
	concatenate_headed_csv_files(Files, default_encoding, OutFile).

concatenate_headed_csv_files(Files, Encoding, OutFile) :-
	safe_absolute_file_name(OutFile, AbsOutFile),
	read_headed_csv_files(Files, Encoding, Lists, Headers),
	sort(Headers, SortedHeaders),
	(   SortedHeaders = [Header] ->
	    true
	;
	    format('~N*** Error in concatenate_headed_csv_files: different headers: ~w~n', [SortedHeaders]),
	    fail
	),
	append_list(Lists, List),
	length(List, N),
	list_of_lists_to_csv_file([Header | List], AbsOutFile, Encoding),
	format('~N--- Written concatenated CSV file (~d lines excluding header) ~w~n', [N, AbsOutFile]).

read_headed_csv_files([], _Encoding, [], []).
read_headed_csv_files([File | Files], Encoding, [List | Lists], [Header | Headers]) :-
	read_headed_csv_file(File, Encoding, List, Header),
	!,
	read_headed_csv_files(Files, Encoding, Lists, Headers).

read_headed_csv_file(File, Encoding, List, Header) :-
	safe_absolute_file_name(File, AbsFile),
	csv_file_to_list_of_lists(AbsFile, Encoding, 0'", 0',, FullList), %"'
	(   FullList = [Header | List] ->
	    length(List, N),
	    format('~N--- Read headed CSV file (~d lines excluding header) ~w~n', [N, AbsFile])
	;
	    format('~N*** Error in read_headed_csv_file: empty file: ~w~n', [AbsFile]),
	    fail
	).	

%-------------------------------------------------------

remove_null_lines_from_csv_file(InFile, OutFile) :-
	remove_null_lines_from_csv_file(InFile, default_encoding, OutFile).

remove_null_lines_from_csv_file(InFile, Encoding, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	
	csv_file_to_list_of_lists(AbsInFile, Encoding, 0'", 0',, InList), %"'
	length(InList, InN),
	format('~N--- Read headed CSV file (~d lines) ~w~n', [InN, AbsInFile]),
	
	remove_null_lines_from_csv_list(InList, OutList),
	
	length(OutList, OutN),
	list_of_lists_to_csv_file(OutList, AbsOutFile, Encoding),
	format('~N--- Written headed CSV file (~d lines) ~w~n', [OutN, AbsOutFile]).

remove_null_lines_from_csv_list([], []).
remove_null_lines_from_csv_list([F | R], R1) :-
	null_csv_line(F),
	!,
	remove_null_lines_from_csv_list(R, R1).
remove_null_lines_from_csv_list([F | R], [F | R1]) :-
	!,
	remove_null_lines_from_csv_list(R, R1).

null_csv_line([]).
null_csv_line(['' | R]) :-
	!,
	null_csv_line(R).

%-------------------------------------------------------

combine_columns_from_csv_files(InCSV1, InColumns1, InCSV2, InColumns2, OutCSV) :-
	safe_absolute_file_name(InCSV1, AbsInCSV1),
	safe_absolute_file_name(InCSV2, AbsInCSV2),
	safe_absolute_file_name(OutCSV, AbsOutCSV),

	csv_file_to_list_of_lists(AbsInCSV1, InList1),
	csv_file_to_list_of_lists(AbsInCSV2, InList2),

	length(InList1, InN1),
	length(InList2, InN2),

	(   InN1 = InN2 ->
	    true
	;
	    otherwise ->
	    format('~N*** Error in combine_columns_from_csvs/5: different numbers of records in input files.~n', []),
	    fail
	),

	combine_columns_from_csvs1(InList1, InColumns1, InList2, InColumns2, OutList),

	list_of_lists_to_csv_file(OutList, AbsOutCSV),
	!.
combine_columns_from_csv_files(InCSV1, InColumns1, InCSV2, InColumns2, OutCSV) :-
	format('~N*** Error: bad call: ~w~n',
	       [combine_columns_from_csv_files(InCSV1, InColumns1, InCSV2, InColumns2, OutCSV)]),
	fail.

combine_columns_from_csvs1([], _InColumns1, [], _InColumns2, []).
combine_columns_from_csvs1([F | R], InColumns1, [F1 | R1], InColumns2, [F2 | R2]) :-
	combine_columns_from_csvs_records(F, InColumns1, F1, InColumns2, F2),
	!,
	combine_columns_from_csvs1(R, InColumns1, R1, InColumns2, R2).

combine_columns_from_csvs_records(F, InColumns1, F1, InColumns2, OutColumns) :-
	extract_columns_from_csv_record(F, InColumns1, Columns1),
	extract_columns_from_csv_record(F1, InColumns2, Columns2),
	append(Columns1, Columns2, OutColumns),
	!.
combine_columns_from_csvs_records(F, InColumns1, F1, InColumns2, F2) :-
	format('~N*** Error: bad call: ~w~n', [combine_columns_from_csvs_records(F, InColumns1, F1, InColumns2, F2)]),
	fail.

extract_columns_from_csv_record([], _AllColumns, []).
extract_columns_from_csv_record([F | R], AllColumns, [F1 | R1]) :-
	safe_nth(F, AllColumns, F1),
	!,
	extract_columns_from_csv_record(R, AllColumns, R1).

%------------------------------------------------------------------------------------

list_of_lists_to_csv_file(List, File) :- 
	list_of_lists_to_csv_file(List, File, default_encoding),
	!.

list_of_lists_to_csv_file(List, File, Encoding) :-
	list_of_lists_to_csv_file(List, File, 0',, Encoding).

list_of_lists_to_csv_file(List, File, SeparatorChar, Encoding) :-
	list_of_atom_lists_to_csv_atom_list(List, SeparatorChar, AtomList),
	write_atom_list_to_unicode_file(AtomList, File, Encoding),
	!.
list_of_lists_to_csv_file(List, File, SeparatorChar, Encoding) :-
	format('~N*** Error: bad call: ~w~n', [list_of_lists_to_csv_file(List, File, SeparatorChar, Encoding)]),
	fail.

list_of_atom_lists_to_csv_atom_list([], _SeparatorChar, []).
list_of_atom_lists_to_csv_atom_list([F | R], SeparatorChar, [F1 | R1]) :-
	atom_list_to_csv_atom(F, SeparatorChar, F1),
	!,
	list_of_atom_lists_to_csv_atom_list(R, SeparatorChar, R1).
 
atom_list_to_csv_atom(List, SeparatorChar, Atom) :-
	(   is_atom_list(List) ->
	    put_quotes_around_atoms(List, List1),
	    append_atoms(List1, SeparatorChar, Atom)
	;
	    otherwise ->
	    format('~N*** Error: bad item in list in call to list_of_lists_to_csv_file/2: ~w~n', [List]),
	    fail
	).

is_atom_list(List) :-
	is_list(List),
	is_atom_list1(List).

is_atom_list1([]).
is_atom_list1([F | R]) :-
	atom(F),
	!,
	is_atom_list1(R).

put_quotes_around_atoms([], []).
put_quotes_around_atoms([F | R], [F1 | R1]) :-
	put_quotes_around_atom(F, F1),
	!,
	put_quotes_around_atoms(R, R1).

put_quotes_around_atom(Atom, AtomWithQuotes) :-
	atom_codes(Atom, Str),
	double_any_quotes(Str, Str1),
	format_to_atom('"~s"', [Str1], AtomWithQuotes).

double_any_quotes([], []).
double_any_quotes([0'" | R], [0'", 0'" | R1]) :-  %"
	!,
	double_any_quotes(R, R1).
double_any_quotes([F | R], [F | R1]) :- 
	!,
	double_any_quotes(R, R1).

%------------------------------------------------------------------------------------	

write_atom_list_to_file(AtomsList, File) :-
	open(File, write, S),
	write_atom_list_to_file1(AtomsList, S),
	close(S).

write_atom_list_to_unicode_file(AtomsList, File) :-
	write_atom_list_to_unicode_file(AtomsList, File, 'UTF-8').

write_atom_list_to_unicode_file(AtomsList, File, default_encoding) :-
	!,
	write_atom_list_to_file(AtomsList, File).
write_atom_list_to_unicode_file(AtomsList, File, Encoding) :-
	open(File, write, S, [encoding(Encoding)]),
	write_atom_list_to_file1(AtomsList, S),
	close(S).

write_atom_list_to_file1([], _S).
write_atom_list_to_file1([F | R], S) :-
	format(S, '~N~w~n', [F]),
	!,
	write_atom_list_to_file1(R, S).

%------------------------------------------------------------------------------------	

write_string_list_to_unicode_file(StringsList, File) :-
	write_string_list_to_unicode_file(StringsList, File, 'UTF-8').

write_string_list_to_unicode_file(StringsList, File, Encoding) :-
	open(File, write, S, [encoding(Encoding)]),
	write_string_list_to_file1(StringsList, S),
	close(S).

write_string_list_to_file1([], _S).
write_string_list_to_file1([F | R], S) :-
	format(S, '~N~s~n', [F]),
	!,
	write_string_list_to_file1(R, S).

%------------------------------------------------------------------------------------	
   
write_atom_list_to_stream(S, List) :-
	write_atom_list_to_stream(S, List, ' ').

write_atom_list_to_stream(_S, [], _Separator) :-
	!.
write_atom_list_to_stream(S, [F], _Separator) :-
	!,
	format(S, '~w', [F]).
write_atom_list_to_stream(S, [F|R], Separator) :-
	format(S, '~w~w', [F, Separator]),
	write_atom_list_to_stream(S, R, Separator).
	
%------------------------------------------------------------------------------------

html_table_file_to_list(File, List) :-
	html_table_file_to_list(File, List, default_encoding).

html_table_file_to_list(File, List, Encoding) :-
	read_file_to_string(File, Encoding, String),
	xml_parse(String, Doc),
	(   xml_subterm(Doc, element(tbody, _, TableBody))
	;
	    xml_subterm(Doc, element(table, _, TableBody))
	),
	resolve_pcdata_attribs_tr_and_td_in_xml(TableBody, List).

resolve_pcdata_attribs_tr_and_td_in_xml(Atom, Atom) :-
	atomic(Atom),
	!.
resolve_pcdata_attribs_tr_and_td_in_xml(pcdata(Str), Atom) :-
	atom_codes(Atom, Str),
	!.
resolve_pcdata_attribs_tr_and_td_in_xml(Key=Str, Key=Atom) :-
	atom_codes(Atom, Str),
	!.
resolve_pcdata_attribs_tr_and_td_in_xml(Term, TermOut) :-
	functor(Term, F, N),
	functor(Term1, F, N),
	resolve_pcdata_attribs_tr_and_td_in_xml_args(N, Term, Term1),
	resolve_tr_and_td(Term1, TermOut).

resolve_pcdata_attribs_tr_and_td_in_xml_args(I, _Term, _Term1) :-
	I < 1,
	!.
resolve_pcdata_attribs_tr_and_td_in_xml_args(I, Term, Term1) :-
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	resolve_pcdata_attribs_tr_and_td_in_xml(Arg, Arg1),
	I1 is I - 1,
	!,
	resolve_pcdata_attribs_tr_and_td_in_xml_args(I1, Term, Term1).

resolve_tr_and_td(element(tr, _, Body), Body) :-
	!.
resolve_tr_and_td(element(td, _, []), []) :-
	!.
resolve_tr_and_td(element(td, _, [Body]), Body) :-
	!.
resolve_tr_and_td(Other, Other).

%------------------------------------------------------------------------------------

append_n_copies(N, L, L1) :-
	integer(N),
	N >= 0,
	is_list(L),
	append_n_copies1(N, [], L, L1).

append_n_copies1(0, In, _L, In) :-
	!.
append_n_copies1(I, In, L, Out) :-
	I >= 0,
	append(In, L, Next),
	I1 is I - 1,
	!,
	append_n_copies1(I1, Next, L, Out).

%------------------------------------------------------------------------------------

join_with_spaces(Atoms, Result) :-
	append_atoms(Atoms, 0' , Result).

join_with_underscore(Atoms, Result) :-
	append_atoms(Atoms, 0'_, Result).

append_atoms([Atom], _Separator, Atom) :-
	!.
append_atoms([F | R], Separator, Atom) :-
	append_atoms(R, Separator, RAtom),
	atom_or_number_codes(F, FChars),
	atom_or_number_codes(RAtom, RChars),
	append(FChars, [Separator | RChars], AllChars),
	atom_codes(Atom, AllChars).
append_atoms([], _Separator, '').

append_atoms([Atom], Atom) :-
	!.
append_atoms([F | R], Atom) :-
	append_atoms(R, RAtom),
	atom_or_number_codes(F, FChars),
	atom_or_number_codes(RAtom, RChars),
	append(FChars, RChars, AllChars),
	atom_codes(Atom, AllChars).
append_atoms([], '').

atom_or_number_codes(Atom, Chars) :-
	atom(Atom),
	!,
	atom_codes(Atom, Chars).
atom_or_number_codes(Number, Chars) :-
	number(Number),
	!,
	safe_number_codes(Number, Chars).

%------------------------------------------------------------------------------------

append_strings_with_char([], _Separator, "") :-
	!.
append_strings_with_char([String], _Separator, String) :-
	!.
append_strings_with_char([F | R], Separator, String) :-
	append_strings_with_char(R, Separator, RString),
	append(F, [Separator | RString], String),
	!.

%------------------------------------------------------------------------------------

term_contains_functor(T, F/N) :-
	nonvar(T),
	functor(T, F, N),
	!.
term_contains_functor(T, F/N) :-
	nonvar(T),
	functor(T, _F1, N1),
	term_contains_functor_args(N1, T, F/N),
	!.

term_contains_functor_args(I, T, F/N) :-
	I > 0,
	arg(I, T, Arg),
	term_contains_functor(Arg, F/N), 
	!.
term_contains_functor_args(I, T, F/N) :-
	I > 1,
	I1 is I - 1, 
	!,
	term_contains_functor_args(I1, T, F/N).

%------------------------------------------------------------------------------------

term_contains_subterm(T, Subterm) :-
	safe_subsumes_chk(Subterm, T),
	!.
term_contains_subterm(T, Subterm) :-
	nonvar(T),
	functor(T, _F1, N1),
	term_contains_subterm_args(N1, T, Subterm),
	!.

term_contains_subterm_args(I, T, Subterm) :-
	I > 0,
	arg(I, T, Arg),
	term_contains_subterm(Arg, Subterm), 
	!.
term_contains_subterm_args(I, T, Subterm) :-
	I > 1,
	I1 is I - 1, 
	!,
	term_contains_subterm_args(I1, T, Subterm).

%------------------------------------------------------------------------------------

identical_up_to_variable_renaming(X, Y) :-
	safe_subsumes_chk(X, Y),
	safe_subsumes_chk(Y, X),
	!.

%------------------------------------------------------------------------------------

substitute_in_term(T, From, To, T1) :-
	T == From,
	!,
	T1 = To.
substitute_in_term(T, _From, _To, T) :-
	(   var(T) ;
	    atomic(T)
	),
	!.
substitute_in_term(T, From, To, T1) :-
	compound(T),
	functor(T, F, N),
	functor(T1, F, N),
	substitute_in_term_args(N, T, From, To, T1).

substitute_in_term_args(0, _T, _From, _To, _T1) :-
	!.
substitute_in_term_args(I, T, From, To, T1) :-
	I > 0,
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	substitute_in_term(Arg, From, To, Arg1),
	I1 is I - 1,
	!,
	substitute_in_term_args(I1, T, From, To, T1).

%------------------------------------------------------------------------------------

read_prolog_file_and_assert_goals(File, Package) :-
	safe_prolog_file_to_list_printing_statistics(File, List),
	assert_list_of_goals(List, Package),
	!.

%------------------------------------------------------------------------------------

assert_list_of_goals(List, Package) :-
	(   ( atom(Package), is_list_of_compounds_or_atoms(List) ) ->
	    assert_list_of_goals1(List, Package)
	;
	    otherwise ->
	    format('~N*** Error: bad call: ~w~n', [assert_list_of_goals(List, Package)])
	).

assert_list_of_goals1([], _Package).
assert_list_of_goals1([F | R], Package) :-
	assertz(Package:F),
	!,
	assert_list_of_goals1(R, Package).

is_list_of_compounds_or_atoms(List) :-
	is_list(List),
	is_list_of_compounds_or_atoms1(List).

is_list_of_compounds_or_atoms1([]).
is_list_of_compounds_or_atoms1([F | R]) :-
	(   compound(F)
	;
	    atom(F)
	),
	is_list_of_compounds_or_atoms1(R).

%------------------------------------------------------------------------------------

append_list([], []).
append_list([F | R], Result) :-
	append_list(R, Result1),
	append(F, Result1, Result).

%------------------------------------------------------------------------------------

is_substring(Substring, String) :-
	safe_prefix(Substring, String),
	!.
is_substring(Substring, [_F | R]) :-
	!,
	is_substring(Substring, R).

%------------------------------------------------------------------------------------

remove_dups_preserving_order([], _AlreadySeen, []).
remove_dups_preserving_order([F | R], AlreadySeen, R1) :-
	id_member(F, AlreadySeen),
	!,
	remove_dups_preserving_order(R, AlreadySeen, R1).
remove_dups_preserving_order([F | R], AlreadySeen, [F | R1]) :-
	!,
	remove_dups_preserving_order(R, [F | AlreadySeen], R1).

%------------------------------------------------------------------------------------

flatten(X, [X]) :-
	var(X),
	!.
flatten([], []) :-
	!.
flatten([F | R], Out) :-
	flatten(F, F1),
	flatten(R, R1),
	append(F1, R1, Out),
	!.
flatten(A, [A]).

%---------------------------------------------------------------

firstn_or_all([], _MaxN, []) :-
	!.
firstn_or_all(_List, 0, []) :-
	!.
firstn_or_all([F | R], MaxN, [F | R1]) :-
	MaxN > 0,
	MaxN1 is MaxN - 1,
	!,
	firstn_or_all(R, MaxN1, R1).

%---------------------------------------------------------------

split_off_firstn([], _N, [], []) :-
	!.
split_off_firstn(List, N, [], List) :-
	N =< 0,
	!.
split_off_firstn([F | R], N, [F | R1], Remaining) :-
	N > 0,
	N1 is N - 1,
	!,
	split_off_firstn(R, N1, R1, Remaining).

%---------------------------------------------------------------

length_of_curly_bracket_list({}, Length) :-
	!,
	Length = 0.
length_of_curly_bracket_list('{}'(CommaList), Length) :-
	length_of_comma_list(CommaList, 0-Length1),
	!,
	Length = Length1.
length_of_curly_bracket_list(X, Y) :-
	format('~N*** Error: bad call: ~w.~n', [length_of_curly_bracket_list(X, Y)]),
	fail.

length_of_comma_list((_F, R), In-Out) :-
	!,
	Next is In + 1,
	length_of_comma_list(R, Next-Out).
length_of_comma_list(_Other, In-Out) :-
	Out is In + 1.

%---------------------------------------------------------------
 
curly_bracket_list_to_list('{}', []).
curly_bracket_list_to_list('{}'(L), L1) :-
	comma_list_to_list(L, L1).

%------------------------------------------------------------------------------------

comma_list_to_list(V, [V]) :-
	var(V),
	!.
comma_list_to_list((F, R), [F | R1]) :-
	!,
	comma_list_to_list(R, R1).
comma_list_to_list(LastElt, [LastElt]).

%------------------------------------------------------------------------------------
 
flatten_comma_list(CommaListIn, CommaListOut) :-
	comma_list_to_list_recursive(CommaListIn, List),
	flatten(List, ListOut),
	list_to_comma_list(ListOut, CommaListOut).

comma_list_to_list_recursive(V, [V]) :-
	var(V),
	!.
comma_list_to_list_recursive((F, R), [F1 | R1]) :-
	!,
	comma_list_to_list_recursive(F, F1),
	comma_list_to_list(R, R1).
comma_list_to_list_recursive(LastElt, [LastElt]).

%------------------------------------------------------------------------------------

is_comma_list(List) :-
	compound(List),
	functor(List, ',', 2).

%------------------------------------------------------------------------------------

is_curly_bracket_list(List) :-
	compound(List),
	functor(List, '{}', 1).

%------------------------------------------------------------------------------------

list_to_curly_bracket_list([], '{}') :-
	!.
list_to_curly_bracket_list(List, '{}'(CommaList)) :-
	list_to_comma_list(List, CommaList).

%------------------------------------------------------------------------------------

list_to_comma_list([], _) :-
	raise_exception('Attempt to convert empty list to comma list').
list_to_comma_list([X], X) :-
	!.
list_to_comma_list([F | R], (F, R1)) :-
	list_to_comma_list(R, R1).

%------------------------------------------------------------------------------------

unkey_list([], []).
unkey_list([_Key-F | R], [F | R1]) :-
	unkey_list(R, R1).

%------------------------------------------------------------------------------------

first_elements_of_pairs([], []).
first_elements_of_pairs([[X, _Y] | R], [X | R1]) :-
	!,
	first_elements_of_pairs(R, R1).

second_elements_of_pairs([], []).
second_elements_of_pairs([[_X, Y] | R], [Y | R1]) :-
	!,
	second_elements_of_pairs(R, R1).

%------------------------------------------------------------------------------------

normalise_prolog_dcg_clause_to_c_version((H :- B), (H1 :- B1)) :-
	H1 = H,
	normalise_prolog_dcg_clause_body_to_c_version(B, B1),
	!.
normalise_prolog_dcg_clause_to_c_version(X, Y) :-
	regulus_error('~NBad call: ~w~n', [normalise_prolog_dcg_clause_to_c_version(X, Y)]).

normalise_prolog_dcg_clause_body_to_c_version(Var, Var) :-
	var(Var),
	!.
normalise_prolog_dcg_clause_body_to_c_version((P, Q), (P1, Q1)) :-
	!,
	normalise_prolog_dcg_clause_body_to_c_version(P, P1), 
	normalise_prolog_dcg_clause_body_to_c_version(Q, Q1).
normalise_prolog_dcg_clause_body_to_c_version((P ; Q), (P1 ; Q1)) :-
	!,
	normalise_prolog_dcg_clause_body_to_c_version(P, P1),
	normalise_prolog_dcg_clause_body_to_c_version(Q, Q1).
normalise_prolog_dcg_clause_body_to_c_version((In = Out), Result) :-
	var(In),
	compound(Out),
	Out = [Word | Rest],
	atom(Word),
	var(Rest),
	!,
	Result = 'C'(In, Word, Rest).
normalise_prolog_dcg_clause_body_to_c_version(Other, Other).

%------------------------------------------------------------------------------------

remove_duplicates_from_sorted_file(InFile, OutFile) :-
	open(InFile, read, SIn),
	open(OutFile, write, SOut),
	remove_duplicates_from_sorted_file1(SIn, SOut, '*dummy*'),
	close(SIn),
	close(SOut).

remove_duplicates_from_sorted_file1(SIn, SOut, LastTerm) :-
	read(SIn, Term),
	remove_duplicates_from_sorted_file2(Term, SIn, SOut, LastTerm).

remove_duplicates_from_sorted_file2(end_of_file, _SIn, _SOut, _LastTerm) :-
	!.
remove_duplicates_from_sorted_file2(Term, SIn, SOut, LastTerm) :-
	Term = LastTerm,
	!,
	remove_duplicates_from_sorted_file1(SIn, SOut, Term).
remove_duplicates_from_sorted_file2(Term, SIn, SOut, _LastTerm) :-
	format(SOut, '~N~q.~n', [Term]),
	!,
	remove_duplicates_from_sorted_file1(SIn, SOut, Term).

%------------------------------------------------------------------------------------

id_member(X, [F | _R]) :-
	X == F, 
	!.
id_member(X, [_F | R]) :-
	id_member(X, R).

%----------------------------------------------------------------------

pick_n_random_members_from_list(N, List, List1) :-
	length(List, Length),
	(   N =< Length ->
	    true
	;
	    otherwise ->
	    format('~N*** Error: bad call to pick_n_random_members_from_list/3: list contains ~d members, trying to find ~d random members~n', [Length, N]),
	    fail
	),
	random_permutation(List, RandomisedList),
	prefix_length(RandomisedList, List1, N),
	!.
pick_n_random_members_from_list(N, List, List1) :-
	format('~N*** Error: bad call: ~w~n',
	       [pick_n_random_members_from_list(N, List, List1)]),
	fail.

pick_n_random_members_from_list(N, List, List1, Rest) :-
	length(List, Length),
	(   N =< Length ->
	    true
	;
	    otherwise ->
	    format('~N*** Error: bad call to pick_n_random_members_from_list/3: list contains ~d members, trying to find ~d random members~n', [Length, N]),
	    fail
	),
	random_permutation(List, RandomisedList),
	prefix_length(RandomisedList, List1, N),
	append(List1, Rest, RandomisedList),
	!.
pick_n_random_members_from_list(N, List, List1, Rest) :-
	format('~N*** Error: bad call: ~w~n',
	       [pick_n_random_members_from_list(N, List, List1, Rest)]),
	fail.

%----------------------------------------------------------------------

assoc_generics_to_lists_in_term(Var, Var) :-
	var(Var),
	!.
assoc_generics_to_lists_in_term(X, X) :-
	atomic(X),
	!.
assoc_generics_to_lists_in_term(Assoc, List) :-
	is_assoc_generic(Assoc),
	!,
	assoc_generic_to_list(Assoc, List).
assoc_generics_to_lists_in_term(T, T1) :-
	compound(T),
	functor(T, F, N),
	functor(T1, F, N),
	assoc_generics_to_lists_in_term_args(N, T, T1).

assoc_generics_to_lists_in_term_args(I, _T, _T1) :-
	I < 1.
assoc_generics_to_lists_in_term_args(I, T, T1) :-
	I >= 1,
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	assoc_generics_to_lists_in_term(Arg, Arg1),
	I1 is I - 1,
	!,
	assoc_generics_to_lists_in_term_args(I1, T, T1).

%----------------------------------------------------------------------

inc_assoc_generic(AssocIn, Key, AssocOut) :-
	get_assoc_generic_or_zero(Key, AssocIn, OldVal),
	number(OldVal),
	NewVal is OldVal + 1,
	put_assoc_generic(Key, AssocIn, NewVal, AssocOut),
	!.
inc_assoc_generic(AssocIn, Key, AssocOut) :-
	format('~N*** Error: bad call: ~w~n', [inc_assoc_generic(AssocIn, Key, AssocOut)]),
	fail.

%----------------------------------------------------------------------

inc_assoc_generic(AssocIn, Key, Increment, AssocOut) :-
	get_assoc_generic_or_zero(Key, AssocIn, OldVal),
	number(OldVal),
	NewVal is OldVal + Increment,
	put_assoc_generic(Key, AssocIn, NewVal, AssocOut),
	!.
inc_assoc_generic(AssocIn, Key, Increment, AssocOut) :-
	format('~N*** Error: bad call: ~w~n', [inc_assoc_generic(AssocIn, Key, Increment, AssocOut)]),
	fail.

%----------------------------------------------------------------------

get_assoc_generic_or_zero(Key, Assoc, Val) :-
	get_assoc_generic(Key, Assoc, Val),
	!.
get_assoc_generic_or_zero(_Key, _Assoc, Val) :-
	Val = 0,
	!.
%---------------------------------------------------------------

list_to_ordered_multiset([], []) :-
	!.
list_to_ordered_multiset(List, MultiSet) :-
	add_unique_tags_to_list(List, TaggedList, 0),
	sort(TaggedList, SortedList),
	SortedList = [tag(SortedListFirst, _) | SortedListRest],
	sorted_list_to_multiset(SortedListRest, MultiSet0, 1-SortedListFirst),
	keysort(MultiSet0, MultiSet1),
	reverse(MultiSet1, MultiSet).

add_unique_tags_to_list([], [], _N) :-
	!.
add_unique_tags_to_list([F | R], [tag(F, I) | R1], I) :-
	I1 is I + 1,
	!,
	add_unique_tags_to_list(R, R1, I1).

sorted_list_to_multiset([], [N-LastItem], N-LastItem) :-
	!.
sorted_list_to_multiset([tag(F, _) | R], MultiSet, N-CurrentItem) :-
	F == CurrentItem,
	N1 is N + 1,
	!,
	sorted_list_to_multiset(R, MultiSet, N1-CurrentItem).
sorted_list_to_multiset([tag(F, _) | R], [(N-CurrentItem) | MultiSet], N-CurrentItem) :-
	!,
	sorted_list_to_multiset(R, MultiSet, 1-F).

%------------------------------------------------------------------------------------

% L1 and L2 are lists. Returns the number of insertions, deletions and substitutions
% needed to turn L2 into L1. Also works for atoms representing lists of words.

% i_d_s_tmp(Pos1, Pos2, Total, I, D, S, MatchList)
%
% The best match between L1 up to Pos1 and L2 up to Pos2
% involves Total (I, D, S) insertions, deletions and substitutions, encoded as MatchList

:- dynamic i_d_s_tmp/7.

insertions_deletions_substitutions(A1, A2, Total, I, D, S) :-
	insertions_deletions_substitutions(A1, A2, Total, I, D, S, no_matches, _Matches).

insertions_deletions_substitutions_and_matches(A1, A2, Total, I, D, S, Matches) :-
	insertions_deletions_substitutions(A1, A2, Total, I, D, S, matches, Matches).

insertions_deletions_substitutions(A1, A2, Total, I, D, S, MatchesP, Matches) :-
	atom(A1),
	atom(A2),
	split_atom_into_words(A1, L1),
	split_atom_into_words(A2, L2),
	insertions_deletions_substitutions(L1, L2, Total, I, D, S, MatchesP, Matches),
	!.
insertions_deletions_substitutions(L1, L2, Total, I, D, S, MatchesP, Matches) :-
	is_list(L1),
	is_list(L2),
	init_insertions_deletions_substitutions,
	length(L1, LastPos1),
	length(L2, LastPos2),
	N is LastPos1 + LastPos2,
	insertions_deletions_substitutions1(1, N, LastPos1, LastPos2, L1, L2, MatchesP),
	i_d_s_tmp(LastPos1, LastPos2, Total, I, D, S, Matches0),
	reverse(Matches0, Matches),
	!.
insertions_deletions_substitutions(L1, L2, Total, I, D, S, Matches) :-
	format('~N*** Error: bad call: ~w.~n', [insertions_deletions_substitutions(L1, L2, Total, I, D, S, Matches)]),
	fail.

init_insertions_deletions_substitutions :-
	retractall(i_d_s_tmp(_Pos1, _Pos2, _Total, _I, _D, _S, _M)),
	assertz(i_d_s_tmp(0, 0, 0, 0, 0, 0, [])),
	!.

insertions_deletions_substitutions1(I, N, _LastPos1, _LastPos2, _L1, _L2, _MatchesP) :-
	I > N,
	!.
insertions_deletions_substitutions1(I, N, LastPos1, LastPos2, L1, L2, MatchesP) :-
	I =< N,
	insertions_deletions_substitutions2(0, I, LastPos1, LastPos2, L1, L2, MatchesP),
	I1 is I + 1,
	!,
	insertions_deletions_substitutions1(I1, N, LastPos1, LastPos2, L1, L2, MatchesP).

insertions_deletions_substitutions2(Pos1, I, _LastPos1, _LastPos2, _L1, _L2, _MatchesP) :-
	Pos1 > I,
	!.
insertions_deletions_substitutions2(Pos1, I, LastPos1, LastPos2, L1, L2, MatchesP) :-
	Pos1 =< I,
	Pos2 is I - Pos1,
	insertions_deletions_substitutions3(Pos1, Pos2, LastPos1, LastPos2, L1, L2, MatchesP),
	Pos1Next is Pos1 + 1,
	!,
	insertions_deletions_substitutions2(Pos1Next, I, LastPos1, LastPos2, L1, L2, MatchesP).

insertions_deletions_substitutions3(Pos1, Pos2, LastPos1, LastPos2, _L1, _L2, _MatchesP) :-
	( Pos1 > LastPos1 ; Pos2 > LastPos2 ),
	!.
insertions_deletions_substitutions3(Pos1, Pos2, _LastPos1, _LastPos2, L1, L2, MatchesP) :-
	insertion_score(Pos1, Pos2, L1, L2, InsTotal-[InsI, InsD, InsS, InsMatches], MatchesP),
	deletion_score(Pos1, Pos2, L1, L2, DelTotal-[DelI, DelD, DelS, DelMatches], MatchesP),
	substitution_score(Pos1, Pos2, L1, L2, SubTotal-[SubI, SubD, SubS, SubMatches], MatchesP),	
	best_i_d_s_score(InsTotal-[InsI, InsD, InsS, InsMatches],
			 DelTotal-[DelI, DelD, DelS, DelMatches],
			 SubTotal-[SubI, SubD, SubS, SubMatches],
			 BestTotal-[BestI, BestD, BestS, BestMatches]),
	assertz(i_d_s_tmp(Pos1, Pos2, BestTotal, BestI, BestD, BestS, BestMatches)).

best_i_d_s_score(InsTotal-InsInfo,
		 DelTotal-DelInfo,
		 SubTotal-SubInfo,
		 BestTotal-BestInfo) :-
	keysort([InsTotal-InsInfo,
		 DelTotal-DelInfo,
		 SubTotal-SubInfo],
		SortedList),
	SortedList = [BestTotal-BestInfo | _Rest],
	!.

% We can't insert to reach position 0
insertion_score(Pos1, _Pos2, _L1, _L2, Total-[I, D, S, M], _MatchP) :-
	Pos1 = 0,
	impossible_score(Total-[I, D, S, M]),
	!.
insertion_score(Pos1, Pos2, L1, _L2, Total-[I, D, S, M], MatchesP) :-
	Pos1Minus1 is Pos1 - 1,
	i_d_s_tmp(Pos1Minus1, Pos2, _NextTotal, NextI, NextD, NextS, NextM),
	I is NextI + 1,
	D is NextD,
	S is NextS,
	Total is I + D + S,
	(   MatchesP = matches ->
	    safe_nth(Pos1, L1, Word1),
	    M = [ins(Word1) | NextM]
	;
	    otherwise ->
	    M = []
	),
	!.

% We can't delete from position 0
deletion_score(_Pos1, Pos2, _L1, _L2, Total-[I, D, S, M], _MatchesP) :-
	Pos2 = 0,
	impossible_score(Total-[I, D, S, M]),
	!.
deletion_score(Pos1, Pos2, _L1, L2, Total-[I, D, S, M], MatchesP) :-
	Pos2Minus1 is Pos2 - 1,
	i_d_s_tmp(Pos1, Pos2Minus1, _NextTotal, NextI, NextD, NextS, NextM),
	I is NextI,
	D is NextD + 1,
	S is NextS,
	Total is I + D + S,
	(   MatchesP = matches ->
	    safe_nth(Pos2, L2, Word2),
	    M = [del(Word2) | NextM]
	;
	    otherwise ->
	    M = []
	),
	!.

% We can't substitute if either string is at position 0
substitution_score(Pos1, Pos2, _L1, _L2, Total-[I, D, S, M], _MatchesP) :-
	( Pos1 = 0 ; Pos2 = 0 ),
	impossible_score(Total-[I, D, S, M]),
	!.
substitution_score(Pos1, Pos2, L1, L2, Total-[I, D, S, M], MatchesP) :-
	Pos1Minus1 is Pos1 - 1,	
	Pos2Minus1 is Pos2 - 1,
	safe_nth(Pos1, L1, Word1),
	safe_nth(Pos2, L2, Word2),
	i_d_s_tmp(Pos1Minus1, Pos2Minus1, _NextTotal, NextI, NextD, NextS, NextM),
	I is NextI,
	D is NextD,
	(   Word1 = Word2 ->
	    S is NextS,
	    (   MatchesP = matches ->
		M = [same(Word1) | NextM]
	    ;
		otherwise ->
		M = []
	    )
	;
	    otherwise ->
	    S is NextS + 1,
	    (   MatchesP = matches ->
		M = [sub(Word1, Word2) | NextM]
	    ;
		otherwise ->
		M = []
	    )
	),
	Total is I + D + S,	
	!.

impossible_score(3000-[1000, 1000, 1000, []]).

%---------------------------------------------------------------

make_ground(Term) :-
	numbervars(Term, 0, _).

%---------------------------------------------------------------

var_or_grounded_var(X) :-
	var(X),
	!.
var_or_grounded_var(X) :-
	compound(X),
	functor(X, '$VAR', 1).

%------------------------------------------------------------------------------------

no_vars_or_grounded_vars_in_term(X) :-
	var_or_grounded_var(X),
	!,
	fail.
no_vars_or_grounded_vars_in_term(Atom) :-
	atomic(Atom),
	!.
no_vars_or_grounded_vars_in_term(Term) :-
	compound(Term),
	functor(Term, _F, N),
	no_vars_or_grounded_vars_in_term_args(N, Term).

no_vars_or_grounded_vars_in_term_args(I, _Term) :-
	I < 1,
	!.
no_vars_or_grounded_vars_in_term_args(I, Term) :-
	I >= 1,
	arg(I, Term, Arg),
	no_vars_or_grounded_vars_in_term(Arg),
	I1 is I - 1,
	!,
	no_vars_or_grounded_vars_in_term_args(I1, Term).

%------------------------------------------------------------------------------------

% Assume we'll never have more than 255 distinct variables
% in something we need to make unground... would be better 
% in principle to use an assoc, but this code needs to be
% fast.

unground(T, T1) :-
	%empty_assoc(Assoc),
	functor(Assoc, table, 255),
	unground1(T, T1, Assoc).

unground1(V, V, _Assoc) :-
	var(V),
	!.
unground1(A, A, _Assoc) :-
	atomic(A),
	!.
unground1('$VAR'(N), V, Assoc) :-
	!,
	%(  ( N < 0 ; N > 499 ) ->
	(  ( N < 0 ; N > 254 ) ->
	   raise_exception(internal_error_in_make_unground(N)) ;
	   N1 is N + 1,
	   arg(N1, Assoc, V)
       ).
unground1(T, T1, Assoc) :-
	functor(T, F, N),
	functor(T1, F, N),
	unground1_args(N, T, T1, Assoc).

unground1_args(0, _T, _T1, _Assoc).
unground1_args(I, T, T1, Assoc) :-
	I > 0,
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	unground1(Arg, Arg1, Assoc),
	I1 is I - 1,
	unground1_args(I1, T, T1, Assoc).

%------------------------------------------------------------------------------------

% Stuff from Stanford parser

try(A) :-
	A,
	!.
try(_).

/*
assert_general(A) :-
	\+ A,
	!,
	assert(A).

assert_general(A) :-
	numbervars(A, 0, _),
	A,
	!,
	fail.

assert_general(A) :-
	copy_term(A,A1),
	clause(A1,true,Ref),
	clause(A2,true,Ref),
	numbervars(A2,0,_),
	A = A2,
	erase_safe(clause(A2,true,Ref),Ref),
	fail.

assert_general(A) :-
	assert(A).
*/
numbervars(Term, _Marker, In, Out):-
    numbervars(Term, In, Out).

%------------------------------------------------------------------------------------

is_list_of_atoms(List) :-
	nonvar(List),
	is_list_of_atoms1(List).

is_list_of_atoms1([]).
is_list_of_atoms1([F | R]) :-
	atomic(F),
	!,
	is_list_of_atoms1(R).

%------------------------------------------------------------------------------------

align_word_atoms_for_printing(Atoms, Method, Atoms1) :-
	(   member(Method, [tabs, spaces]) ->
	    on_exception(_Exception,
			 align_word_atoms_for_printing1(Atoms, Method, Atoms1),
			 fail
			)
	;
	    otherwise ->
	    format('~N*** Error: bad call: ~w~n', [align_word_atoms_for_printing(Atoms, Method, Atoms1)]),
	    format('~N*** second arg must be one of [tabs, spaces]~n', []),
	    fail
	).

align_word_atoms_for_printing1(Atoms, Method, Atoms1) :-
	split_atoms_in_list(Atoms, SplitAtoms),
	align_atom_lists_for_printing(SplitAtoms, Method, SplitAtoms1),
	join_atoms_in_list(SplitAtoms1, Atoms1).

split_atoms_in_list([], []).
split_atoms_in_list([F | R], [F1 | R1]) :-
	split_atom_into_words(F, F1),
	!,
	split_atoms_in_list(R, R1).

align_atom_lists_for_printing(List, _Method, List) :-
	all_empty_lists(List),
	!.
align_atom_lists_for_printing(List, Method, List1) :-
	first_and_rest_on_list(List, Firsts, Rests),
	align_atoms_for_printing(Firsts, Method, Firsts1),
	align_atom_lists_for_printing(Rests, Method, Rests1),
	first_and_rest_on_list(List1, Firsts1, Rests1).

all_empty_lists([]).
all_empty_lists([[] | R]) :-
	all_empty_lists(R).

first_and_rest_on_list([], [], []).
first_and_rest_on_list([[F | R] | Lists], [F | FLists], [R | RLists]) :-
	first_and_rest_on_list(Lists, FLists, RLists).

align_atoms_for_printing(Atoms, Method, Atoms1) :-
	max_length_of_atoms(Atoms, 0, Max),
	fill_atoms_to_max_length(Atoms, Max, Method, Atoms1).

max_length_of_atoms([], Max, Max).
max_length_of_atoms([F | R], MaxIn, MaxOut) :-
	atom_codes(F, FStr),
	length(FStr, N),
	(   N > MaxIn ->
	    MaxNext is N
	;
	    MaxNext is MaxIn
	),
	!,
	max_length_of_atoms(R, MaxNext, MaxOut).

fill_atoms_to_max_length([], _Max, _Method, []).
fill_atoms_to_max_length([F | R], Max, Method, [F1 | R1]) :-
	fill_atom_to_length(F, Max, Method, F1),
	!,
	fill_atoms_to_max_length(R, Max, Method, R1).

fill_atom_to_length(Atom, Max, Method, Atom1) :-
	atom_codes(Atom, Str),
	length(Str, N),
	Missing is Max - N,
	Missing >= 0,
	(   Method = spaces ->
	    n_copies_of_char(Missing, 0' , Filler)
	;
	    otherwise ->
	    NTabs is 1 + Missing // 10,
	    n_copies_of_char(NTabs, 0'\t, Filler)
	),
	append(Str, Filler, Str1),
	atom_codes(Atom1, Str1),
	!.

n_copies_of_char(0, _Char, []).
n_copies_of_char(N, Char, [Char  | R]) :-
	N1 is N - 1,
	n_copies_of_char(N1, Char, R).

join_atoms_in_list([], []).
join_atoms_in_list([F | R], [F1 | R1]) :-
	join_with_spaces(F, F1),
	!,
	join_atoms_in_list(R, R1).
	
%------------------------------------------------------------------------------------

quote_layout_chars_in_string_for_json(InS, OutS) :-
	with_output_to_chars(print_string_with_layout_chars_for_json(InS), OutS),
	!.
quote_layout_chars_in_string_for_json(InS, OutS) :-
	is_list_of_non_negative_integers(InS),
	format('~N*** Error: bad call: print_string_with_layout_chars_for_json("~s", ~q)~n', [InS, OutS]),
	!,
	fail.
quote_layout_chars_in_string_for_json(InS, OutS) :-
	format('~N*** Error: bad call: ~w~n', [quote_layout_chars_in_string_for_json(InS, OutS)]),
	fail.

print_string_with_layout_chars_for_json([]) :-
	!.
print_string_with_layout_chars_for_json([NonAsciiChar | R]) :-
	NonAsciiChar > 255,
	format('\\u~16r\\', [NonAsciiChar]),
	!,
	print_string_with_layout_chars_for_json(R).
% Escape newline, tab, " and \
print_string_with_layout_chars_for_json([LayoutChar | R]) :-
	json_layout_char_representation(LayoutChar, LayoutChar1),
	format('\\~c', [LayoutChar1]),
	!,
	print_string_with_layout_chars_for_json(R).
print_string_with_layout_chars_for_json([F | R]) :-
	format('~c', [F]),
	!,
	print_string_with_layout_chars_for_json(R).
print_string_with_layout_chars_for_json(X) :-
	format('~N*** Error: bad call: ~w~n', [print_string_with_layout_chars_for_json(X)]),
	fail.

json_layout_char_representation(0'\n, 0'n).
json_layout_char_representation(0'\t, 0't).
json_layout_char_representation(0'", 0'").
json_layout_char_representation(0'\\, 0'\\).

%------------------------------------------------------------------------------------

quote_layout_chars_in_string(InS, OutS) :-
	with_output_to_chars(print_string_with_layout_chars(InS), OutS),
	!.
quote_layout_chars_in_string(InS, OutS) :-
	is_list_of_non_negative_integers(InS),
	format('~N*** Error: bad call: print_string_with_layout_chars("~s", ~q)~n', [InS, OutS]),
	!,
	fail.
quote_layout_chars_in_string(InS, OutS) :-
	format('~N*** Error: bad call: ~w~n', [quote_layout_chars_in_string(InS, OutS)]),
	fail.

print_string_with_layout_chars([]) :-
	!.
print_string_with_layout_chars([NonAsciiChar | R]) :-
	NonAsciiChar > 255,
	format('\\x~16r\\', [NonAsciiChar]),
	!,
	print_string_with_layout_chars(R).
% Don't escape a \x escape sequence
print_string_with_layout_chars([0'\\, 0'x | R]) :-
	member(0'\\, R),
	format('\\x', []),
	print_chars_up_to_backslash(R, R1),
	!,
	print_string_with_layout_chars(R1).
% Otherwise, escape newline, tab, " and \
print_string_with_layout_chars([LayoutChar | R]) :-
	layout_char_representation(LayoutChar, LayoutChar1),
	format('\\~c', [LayoutChar1]),
	!,
	print_string_with_layout_chars(R).
print_string_with_layout_chars([F | R]) :-
	format('~c', [F]),
	!,
	print_string_with_layout_chars(R).
print_string_with_layout_chars(X) :-
	format('~N*** Error: bad call: ~w~n', [print_string_with_layout_chars(X)]),
	fail.

print_chars_up_to_backslash([0'\\ | R], R) :-
	format('\\', []),
	!.
print_chars_up_to_backslash([F | R], Rest) :-
	format('~c', [F]),
	!,
	print_chars_up_to_backslash(R, Rest).

layout_char_representation(0'\n, 0'n).
layout_char_representation(0'\t, 0't).
layout_char_representation(0'", 0'").
layout_char_representation(0'\\, 0'\\).

%------------------------------------------------------------------------------------

string_to_unicode_string(InS, OutS) :-
	string_to_unicode_string1(InS, prolog, OutS).

string_to_json_unicode_string(InS, OutS) :-
	string_to_unicode_string1(InS, json, OutS).

string_to_unicode_string1(InS, Mode, OutS) :-
	with_output_to_chars(print_string_to_unicode_string(InS, Mode), OutS),
	!.
string_to_unicode_string1(InS, Mode, OutS) :-
	is_list_of_non_negative_integers(InS),
	format('~N*** Error: bad call: string_to_unicode_string1("~s", ~q)~n', [InS, Mode, OutS]),
	!,
	fail.
string_to_unicode_string1(InS, Mode, OutS) :-
	format('~N*** Error: bad call: ~w~n', [string_to_unicode_string1(InS, Mode, OutS)]),
	fail.

print_string_to_unicode_string([], _Mode) :-
	!.
print_string_to_unicode_string([Char | R], prolog) :-
	format('\\x~16r\\', [Char]),
	!,
	print_string_to_unicode_string(R, prolog).
print_string_to_unicode_string([Code | R], json) :-
	format('\\u', []),
	print_number_as_four_digit_hex(Code),
	!,
	print_string_to_unicode_string(R, json).
print_string_to_unicode_string(X, Mode) :-
	format('~N*** Error: bad call: ~w~n', [print_string_to_unicode_string(X, Mode)]),
	fail.

print_number_as_four_digit_hex(N) :-
	integer(N),
	0 =< N, N < ( 16 * 16 * 16 * 16 ),
	(   N < 16 ->
	    format('000', [])
	;
	    N < 16 * 16 ->
	    format('00', [])
	;
	    N < 16 * 16 * 16 ->
	    format('0', [])
	;
	    otherwise ->
	    true
	),
	format('~16r', [N]),
	!.
print_number_as_four_digit_hex(N) :-
	format('~N*** Error: bad call: ~w~n', [print_number_as_four_digit_hex(N)]),
	fail.

%------------------------------------------------------------------------------------

is_list_of_non_negative_integers([]).
is_list_of_non_negative_integers([F | R]) :-
	non_negative_integer(F),
	!,
	is_list_of_non_negative_integers(R).

non_negative_integer(I) :-
	integer(I),
	I >= 0.

%------------------------------------------------------------------------------------

is_prolog_string(X) :-
	is_list(X),
	is_prolog_string1(X).

is_prolog_string1([]).
is_prolog_string1([F | R]) :-
	is_prolog_char(F),
	is_prolog_string1(R).

is_prolog_char(X) :-
	integer(X),
	1 =< X, X =< 255.

%------------------------------------------------------------------------------------

convert_strings_to_quoted_atoms(Var, Var) :-
	var(Var),
	!.
convert_strings_to_quoted_atoms(Atom, Atom) :-
	atomic(Atom),
	!.
convert_strings_to_quoted_atoms(String, Atom) :-
	is_prolog_string(String),
	atom_codes(Atom1, String),
	format_to_atom('"~w"', [Atom1], Atom),
	!.
convert_strings_to_quoted_atoms(Term, Term1) :-
	compound(Term),
	functor(Term, F, N),
	functor(Term1, F, N),
	convert_strings_to_quoted_atoms_args(N, Term, Term1),
	!.
convert_strings_to_quoted_atoms(Term, Term1) :-
	format('~N*** Error: bad call: ~w~n', [convert_strings_to_quoted_atoms(Term, Term1)]),
	fail.

convert_strings_to_quoted_atoms_args(0, _Term1, _Term2) :-
	!.
convert_strings_to_quoted_atoms_args(I, Term1, Term2) :-
	I > 0,
	arg(I, Term1, Arg1),
	arg(I, Term2, Arg2),
	convert_strings_to_quoted_atoms(Arg1, Arg2),
	I1 is I - 1,
	!,
	convert_strings_to_quoted_atoms_args(I1, Term1, Term2).

%------------------------------------------------------------------------------------

optional_whitespace_sequence -->
	[F],
	{whitespace_char(F)},
	!,
	optional_whitespace_sequence.
optional_whitespace_sequence --> [].

%------------------------------------------------------------------------------------

non_whitespace_word(Word) -->
	non_whitespace_list(L), {L \== [], atom_codes(Word, L)}.

non_whitespace_list([F | R]) -->
	[F], {\+ whitespace_char(F)},
	!,
	non_whitespace_list(R).
non_whitespace_list([]) --> [].

%------------------------------------------------------------------------------------

any_chars_word(Word) -->
	any_chars_list(L), {L \== [], atom_codes(Word, L)}.

any_chars_list([F | R]) -->
	[F], 
	!,
	any_chars_list(R).
any_chars_list([]) --> [].

%------------------------------------------------------------------------------------

integer_word(Word) -->
	digit_char_list(L), {L \== [], safe_number_codes(Word, L)}.

digit_char_list_word(Word) -->
	digit_char_list(L), {L \== [], atom_codes(Word, L)}.

digit_char_list([F | R]) -->
	[F], {digit_char(F)},
	!,
	digit_char_list(R).
digit_char_list([]) --> [].

%------------------------------------------------------------------------------------

lowercase_atom_list([], []).
lowercase_atom_list([F|R], [F1|R1]) :-
    lowercase_atom(F, F1),
    !,
    lowercase_atom_list(R, R1).

uppercase_atom(A, A1) :-
    atom_codes(A, S),
    uppercase_string(S, S1),
    atom_codes(A1, S1).

initial_uppercase_atom(A, A1) :-
    atom_codes(A, S),
    initial_uppercase_string(S, S1),
    atom_codes(A1, S1).

initial_uppercase_string([],[]).
initial_uppercase_string([F | R], [F1 | R]) :-
    uppercase_char(F, F1),
    !.

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
    atom_codes(A, S),
    lowercase_string(S, S1),
    atom_codes(A1, S1).

lowercase_string([],[]).
lowercase_string([F|R], [F1|R1]) :-
    lowercase_char(F, F1),
    !,
    lowercase_string(R, R1).

lowercase_char(C, C1) :-
    lowercase_uppercase(C1, C),
    !.
lowercase_char(C, C).

deaccent_atom(A, A1) :-
	atom_codes(A, S),
	deaccent_string(S, S1),
	atom_codes(A1, S1).

deaccent_string([F | R], [F1 | R1]) :-
	deaccent_char(F, F1),
	!,
	deaccent_string(R, R1).
deaccent_string([F | R], [F | R1]) :-
	!,
	deaccent_string(R, R1).

strings_same_modulo_casing(X, Y) :-
	( var(X) ; var(Y) ),
	!,
	fail.
strings_same_modulo_casing([], []) :-
	!.
strings_same_modulo_casing([F | R], [F1 | R1]) :-
	chars_same_modulo_casing(F, F1),
	!,
	strings_same_modulo_casing(R, R1).

chars_same_modulo_casing(F, F) :-
	!.
chars_same_modulo_casing(F, F1) :-
	lowercase_uppercase(F, F1),
	!.
chars_same_modulo_casing(F, F1) :-
	lowercase_uppercase(F1, F),
	!.

whitespace_atom(Atom) :-
	atom_codes(Atom, Str),
	whitespace_string(Str).

whitespace_string([]).
whitespace_string([F | R]) :-
	whitespace_char(F),
	!,
	whitespace_string(R).

lowercase_string([]).
lowercase_string([F | R]) :-
	lowercase_char(F),
	!,
	lowercase_string(R).

uppercase_string([]).
uppercase_string([F | R]) :-
	uppercase_char(F),
	!,
	uppercase_string(R).

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
lowercase_uppercase(0'à, 0'À).
lowercase_uppercase(0'á, 0'Á).
lowercase_uppercase(0'â, 0'Â).
lowercase_uppercase(0'ä, 0'Ä).
lowercase_uppercase(0'æ, 0'Æ).
lowercase_uppercase(0'å, 0'Å).
lowercase_uppercase(0'ç, 0'Ç).
lowercase_uppercase(0'è, 0'È).
lowercase_uppercase(0'é, 0'É).
lowercase_uppercase(0'ê, 0'Ê).
lowercase_uppercase(0'ë, 0'Ë).
lowercase_uppercase(0'ì, 0'Ì).
lowercase_uppercase(0'í, 0'Í).
lowercase_uppercase(0'î, 0'Î).
lowercase_uppercase(0'ï, 0'Ï).
lowercase_uppercase(0'ò, 0'Ò).
lowercase_uppercase(0'ó, 0'Ó).
lowercase_uppercase(0'ö, 0'Ö).
lowercase_uppercase(0'ô, 0'Ô).
lowercase_uppercase(0'ù, 0'Ù).
lowercase_uppercase(0'ú, 0'Ú).
lowercase_uppercase(0'û, 0'Û).
lowercase_uppercase(0'ü, 0'Ü).

newline_char(0'\n).

whitespace_char(0' ).
whitespace_char(0'\n).
whitespace_char(0'\t).

punctuation_char(0'.).
punctuation_char(0',).
punctuation_char(0'!).
punctuation_char(0'?).
punctuation_char(0'().
punctuation_char(0')).

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

lowercase_char(0'a).
lowercase_char(0'b).
lowercase_char(0'c).
lowercase_char(0'd).
lowercase_char(0'e).
lowercase_char(0'f).
lowercase_char(0'g).
lowercase_char(0'h).
lowercase_char(0'i).
lowercase_char(0'j).
lowercase_char(0'k).
lowercase_char(0'l).
lowercase_char(0'm).
lowercase_char(0'n).
lowercase_char(0'o).
lowercase_char(0'p).
lowercase_char(0'q).
lowercase_char(0'r).
lowercase_char(0's).
lowercase_char(0't).
lowercase_char(0'u).
lowercase_char(0'v).
lowercase_char(0'w).
lowercase_char(0'x).
lowercase_char(0'y).
lowercase_char(0'z).
lowercase_char(0'à).
lowercase_char(0'á).
lowercase_char(0'â).
lowercase_char(0'ä).
lowercase_char(0'æ).
lowercase_char(0'å).
lowercase_char(0'ç).
lowercase_char(0'è).
lowercase_char(0'é).
lowercase_char(0'ê).
lowercase_char(0'ë).
lowercase_char(0'ì).
lowercase_char(0'í).
lowercase_char(0'î).
lowercase_char(0'ï).
lowercase_char(0'ò).
lowercase_char(0'ó).
lowercase_char(0'ö).
lowercase_char(0'ô).
lowercase_char(0'ù).
lowercase_char(0'ú).
lowercase_char(0'û).
lowercase_char(0'ü).

uppercase_char(0'A).
uppercase_char(0'B).
uppercase_char(0'C).
uppercase_char(0'D).
uppercase_char(0'E).
uppercase_char(0'F).
uppercase_char(0'G).
uppercase_char(0'H).
uppercase_char(0'I).
uppercase_char(0'J).
uppercase_char(0'K).
uppercase_char(0'L).
uppercase_char(0'M).
uppercase_char(0'N).
uppercase_char(0'O).
uppercase_char(0'P).
uppercase_char(0'Q).
uppercase_char(0'R).
uppercase_char(0'S).
uppercase_char(0'T).
uppercase_char(0'U).
uppercase_char(0'V).
uppercase_char(0'W).
uppercase_char(0'X).
uppercase_char(0'Y).
uppercase_char(0'Z).
uppercase_char(0'À).
uppercase_char(0'Á).
uppercase_char(0'Â).
uppercase_char(0'Ä).
uppercase_char(0'Æ).
uppercase_char(0'Å).
uppercase_char(0'Ç).
uppercase_char(0'È).
uppercase_char(0'É).
uppercase_char(0'Ê).
uppercase_char(0'Ë).
uppercase_char(0'Ì).
uppercase_char(0'Í).
uppercase_char(0'Î).
uppercase_char(0'Ï).
uppercase_char(0'Ò).
uppercase_char(0'Ó).
uppercase_char(0'Ö).
uppercase_char(0'Ô).
uppercase_char(0'Ù).
uppercase_char(0'Ú).
uppercase_char(0'Û).
uppercase_char(0'Ü).

accented_char(0'à).
accented_char(0'á).
accented_char(0'â).
accented_char(0'ä).
accented_char(0'æ).
accented_char(0'å).
accented_char(0'ç).
accented_char(0'è).
accented_char(0'é).
accented_char(0'ê).
accented_char(0'ë).
accented_char(0'ì).
accented_char(0'í).
accented_char(0'î).
accented_char(0'ï).
accented_char(0'ò).
accented_char(0'ó).
accented_char(0'ö).
accented_char(0'ô).
accented_char(0'ù).
accented_char(0'ú).
accented_char(0'û).
accented_char(0'ü).
accented_char(0'À).
accented_char(0'Á).
accented_char(0'Â).
accented_char(0'Ä).
accented_char(0'Æ).
accented_char(0'Å).
accented_char(0'Ç).
accented_char(0'È).
accented_char(0'É).
accented_char(0'Ê).
accented_char(0'Ë).
accented_char(0'Ì).
accented_char(0'Í).
accented_char(0'Î).
accented_char(0'Ï).
accented_char(0'Ò).
accented_char(0'Ó).
accented_char(0'Ö).
accented_char(0'Ô).
accented_char(0'Ù).
accented_char(0'Ú).
accented_char(0'Û).
accented_char(0'Ü).

deaccent_char(0'à, 0'a).
deaccent_char(0'á, 0'a).
deaccent_char(0'â, 0'a).
deaccent_char(0'ä, 0'a).
deaccent_char(0'æ, 0'e).
deaccent_char(0'å, 0'a).
deaccent_char(0'ç, 0'c).
deaccent_char(0'è, 0'e).
deaccent_char(0'é, 0'e).
deaccent_char(0'ê, 0'e).
deaccent_char(0'ë, 0'e).
deaccent_char(0'ì, 0'i).
deaccent_char(0'í, 0'i).
deaccent_char(0'î, 0'i).
deaccent_char(0'ï, 0'i).
deaccent_char(0'ò, 0'o).
deaccent_char(0'ó, 0'o).
deaccent_char(0'ö, 0'o).
deaccent_char(0'ô, 0'o).
deaccent_char(0'ù, 0'u).
deaccent_char(0'ú, 0'u).
deaccent_char(0'û, 0'u).
deaccent_char(0'ü, 0'u).
deaccent_char(0'À, 0'A).
deaccent_char(0'Á, 0'A).
deaccent_char(0'Â, 0'A).
deaccent_char(0'Ä, 0'A).
deaccent_char(0'Æ, 0'E).
deaccent_char(0'Å, 0'A).
deaccent_char(0'Ç, 0'C).
deaccent_char(0'È, 0'E).
deaccent_char(0'É, 0'E).
deaccent_char(0'Ê, 0'E).
deaccent_char(0'Ë, 0'E).
deaccent_char(0'Ì, 0'I).
deaccent_char(0'Í, 0'I).
deaccent_char(0'Î, 0'I).
deaccent_char(0'Ï, 0'I).
deaccent_char(0'Ò, 0'O).
deaccent_char(0'Ó, 0'O).
deaccent_char(0'Ö, 0'O).
deaccent_char(0'Ô, 0'O).
deaccent_char(0'Ù, 0'U).
deaccent_char(0'Ú, 0'U).
deaccent_char(0'Û, 0'U).
deaccent_char(0'Ü, 0'U).

%------------------------------------------------------------------------------------

/*

Prettyprinting predicates. You can specify the desired maximum line length and print depth.
Lines longer than the maximum length are broken if possible by inserting newlines, and
terms nested more deeply than the maximum depth are replaced by an ellipsis (...). If
the term you are prettyprinting starts in the middle of a line, you can also supply
an "indent" value.

There are several different versions of the predicate, depending on how much information
you wish to supply explicitly:

  prettyprint(Term, Indent, DesiredMaxLineLength, MaxDepth)

Supply all arguments explicitly.

  prettyprint(Term, Indent, DesiredMaxLineLength)

Use default value of MaxDepth

  prettyprint(Term, Indent)

Use default values of DesiredMaxLineLength and MaxDepth

  prettyprint(Term)

Use default values of DesiredMaxLineLength and MaxDepth and a zero value of Indent

You can set the default values of MaxDepth and DesiredMaxLineLength using the
predicates set_pp_depth/1 and set_pp_width/1. For example,

set_pp_width(80)

sets the default value of DesiredMaxLineLength to 80.

You can also use the prettyprinter to print trace output when debugging. You
can switch this functionality on and off using the predicates pp_debug/0 and
no_pp_debug/0.

*/

prettyprint(Term) :-
	get_pp_width(Width),
	get_pp_depth(Depth),
	prettyprint(Term, 0, Width, Depth).

prettyprint(Term, Indent) :-
	get_pp_width(Width),
	get_pp_depth(Depth),
	prettyprint(Term, Indent, Width, Depth).

prettyprint(Term, Indent, Width) :-
	get_pp_depth(Depth),
	prettyprint(Term, Indent, Width, Depth).

prettyprintq(Term) :-
	get_pp_width(Width),
	get_pp_depth(Depth),
	prettyprintq(Term, 0, Width, Depth).

prettyprintq(Term, Indent) :-
	get_pp_width(Width),
	get_pp_depth(Depth),
	prettyprintq(Term, Indent, Width, Depth).

prettyprintq(Term, Indent, Width) :-
	get_pp_depth(Depth),
	prettyprintq(Term, Indent, Width, Depth).

%------------------------------------------------------------------------------------

prettyprint_to_stream(S, Term) :-
	get_pp_width(Width),
	get_pp_depth(Depth),
	prettyprint_to_stream(S, Term, 0, Width, Depth).

prettyprint_to_stream(S, Term, Indent) :-
	get_pp_width(Width),
	get_pp_depth(Depth),
	prettyprint_to_stream(S, Term, Indent, Width, Depth).

prettyprint_to_stream(S, Term, Indent, Width) :-
	get_pp_depth(Depth),
	prettyprint_to_stream(S, Term, Indent, Width, Depth).

prettyprint_to_stream(S, Term, Indent, Width, Depth) :-
	with_output_to_chars(prettyprint(Term, Indent, Width, Depth), Chars),
	format(S, '~s', [Chars]).

prettyprintq_to_stream_unlimited_depth(S, Term) :-
	prettyprintq_to_stream(S, Term, 0, 100, 10000000).

prettyprintq_to_stream(S, Term) :-
	get_pp_width(Width),
	get_pp_depth(Depth),
	prettyprintq_to_stream(S, Term, 0, Width, Depth).

prettyprintq_to_stream(S, Term, Indent) :-
	get_pp_width(Width),
	get_pp_depth(Depth),
	prettyprintq_to_stream(S, Term, Indent, Width, Depth).

prettyprintq_to_stream(S, Term, Indent, Width) :-
	get_pp_depth(Depth),
	prettyprintq_to_stream(S, Term, Indent, Width, Depth).

prettyprintq_to_stream(S, Term, Indent, Width, Depth) :-
	with_output_to_chars(prettyprintq(Term, Indent, Width, Depth), Chars),
	format(S, '~s', [Chars]).

%------------------------------------------------------------------------------------

prettyprint_term_with_intro_grounded(Intro, Term) :-
	copy_term(Term, Term1),
	make_ground(Term1),
	prettyprint_term_with_intro(user, Intro, Term1).

prettyprint_term_with_intro(Intro, Term) :-
	prettyprint_term_with_intro(user, Intro, Term).

prettyprint_term_with_intro_grounded(S, Intro, Term) :-
	copy_term(Term, Term1),
	make_ground(Term1),
	prettyprint_term_with_intro(S, Intro, Term1).

prettyprint_term_with_intro(S, Intro, Term) :-
	format_to_chars('~w', Intro, IntroChars),
	format(S, '~N~s: ', [IntroChars]),
	length(IntroChars, Len),
	Indent is Len + 2,
	prettyprint_to_stream(S, Term, Indent),
	format(S, '~n', []),
	!.
	
%------------------------------------------------------------------------------------

:- dynamic pp_width/1, pp_depth/1.

set_pp_width(X) :-
	retractall(pp_width(_)),
	assertz(pp_width(X)),
	!.

get_pp_width(X) :-
	pp_width(X),
	!.
get_pp_width(X) :-
	default_pp_width(X),
	!.

default_pp_width(100).

set_pp_depth(X) :-
	retractall(pp_depth(_)),
	assertz(pp_depth(X)),
	!.

get_pp_depth(X) :-
	pp_depth(X),
	!.
get_pp_depth(X) :-
	default_pp_depth(X),
	!.

default_pp_depth(1000).

%------------------------------------------------------------------------------------

% Print, trying to make sure that no line is more than DesiredMaxLineLength long - so
% if a line looks like it will be longer than this, insert a newline. The list is indented
% by Indent spaces.

prettyprint(Term, Indent, DesiredMaxLineLength, MaxDepth) :-
	DesiredMaxLineLength1 is DesiredMaxLineLength - 5,
	prettyprint1(Term, Indent-_PosOut, Indent, DesiredMaxLineLength1, 0-MaxDepth, start_of_line-_, w, _),
	!.
prettyprint(Term, Indent, DesiredMaxLineLength, MaxDepth) :-
	format('~N*** Error: bad call: ~q~n',
	       [prettyprint(Term, Indent, DesiredMaxLineLength, MaxDepth)]).

prettyprintq(Term, Indent, DesiredMaxLineLength, MaxDepth) :-
	DesiredMaxLineLength1 is DesiredMaxLineLength - 5,
	prettyprint1(Term, Indent-_PosOut, Indent, DesiredMaxLineLength1, 0-MaxDepth, start_of_line-_, q, _),
	!.
prettyprintq(Term, Indent, DesiredMaxLineLength, MaxDepth) :-
	format('~N*** Error: bad call: ~q~n',
	       [prettyprintq(Term, Indent, DesiredMaxLineLength, MaxDepth)]).

%------------------------------------------------------------------------------------

% If we can just print the term normally without reaching the end of the line, do that.
% Alternately, see if we do a normal print on the next line.
prettyprint1(Term0, PosIn-PosOut, Indent, DesiredMaxLineLength, Depth-MaxDepth,
	     Start-not_start_of_line, WQ, no_prettyprint) :-
	(   Depth =< MaxDepth ->
	    Term = Term0 ;
	    Term = '...'
	),
	(   term_requiring_enclosing_parentheses(Term) ->
	    (   WQ = w ->
		format_to_chars('(~w)', [Term], Chars) ;
		format_to_chars('(~q)', [Term], Chars)
	    ) ;
	    (   WQ = w ->
		format_to_chars('~w', [Term], Chars) ;
		format_to_chars('~q', [Term], Chars)
	    )
	),
	length(Chars, NChars),
	(   PosIn + NChars =< DesiredMaxLineLength ->
	    
	    format('~s', [Chars]),
	    PosOut is PosIn + NChars ;

	    ( Start = not_start_of_line, Indent + NChars =< DesiredMaxLineLength ) ->
	    % Newline, Indent spaces and Term
	    format('~n~*c~s', [Indent, 0' ,Chars]),
	    PosOut is Indent + NChars
	),
	!.
% We're prettyprinting some kind of list. Move to start of next line if necessary. 
% Print an appropriate opening bracket, increment position and indent,
% and print the rest of the list, ending with an appropraite closing bracket.
prettyprint1(List, _PosIn-PosOut, Indent, DesiredMaxLineLength, Depth-MaxDepth,
	     StartIn-start_of_line, WQ, prettyprint) :-
	is_generalised_list(List, List1, OpeningBracket, ClosingBracket),
	!,
	(   StartIn = not_start_of_line ->
	    % Newline and then Indent spaces
	    format('~n~*c', [Indent, 0' ]) ;
	    true
	),	    
	format('~w', [OpeningBracket]),
	PosNext is Indent + 1,
	Indent1 is Indent + 1,
	Depth1 is Depth + 1,
	prettyprint2(List1, PosNext-PosOut, Indent1, DesiredMaxLineLength, Depth1-MaxDepth, WQ,
		     ClosingBracket, start_of_line).
% We're printing a non-list, non-operator compound term.
% Similar to list, but print functor and opening ( at start and ) at end.
prettyprint1(Term, PosIn-PosOut, Indent, DesiredMaxLineLength, Depth-MaxDepth,
	     StartIn-start_of_line, WQ, prettyprint) :-
	compound(Term),
	Term =.. [Functor | Args],
	% It's not a binary operator on exactly 2 args...
	( \+ binary_operator(Functor) ; \+ length(Args, 2) ),
	Args \== [],
	!,
	(   StartIn = not_start_of_line ->
	    % Newline and then Indent spaces
	    format('~n~*c', [Indent, 0' ]) ;
	    true
	),	
	%atom_codes(Functor, FunctorChars),
	(   WQ = w ->
	    format_to_chars('~w', Functor, FunctorChars) ;
	    format_to_chars('~q', Functor, FunctorChars)
	),
	length(FunctorChars, NFunctorChars),
	format('~s(', [FunctorChars]),
	PosNext is PosIn + NFunctorChars + 1,
	Indent1 is Indent + NFunctorChars + 1,
	Depth1 is Depth + 1,
	prettyprint2(Args, PosNext-PosOut, Indent1, DesiredMaxLineLength, Depth1-MaxDepth, WQ,
		     ')', start_of_line),
	!.
% We're printing a term with a binary operator. Print left paren, first arg on new line, try to print operator
% on same line, try to print second arg on same line if first one wasn't prettyprinted, right paren.
prettyprint1(Term, PosIn-PosOut, Indent, DesiredMaxLineLength, Depth-MaxDepth,
	     StartIn-StartOut, WQ, prettyprint) :-
	compound(Term),
	Term =.. [Op, Arg1, Arg2],
	binary_operator(Op),
	!,
	(   StartIn = not_start_of_line ->
	    % Newline and then Indent spaces
	    format('~n~*c', [Indent, 0' ]) ;
	    true
	),
	Depth1 is Depth + 1,
	format('(', []),
	PosIn1 is PosIn + 1,
	Indent1 is Indent + 1,
	prettyprint1(Arg1, PosIn1-PosNext1, Indent1, DesiredMaxLineLength, Depth1-MaxDepth, 
		     start_of_line-_StartNext, WQ, Prettyprint1),
	atom_codes(Op, OpChars),
	length(OpChars, NOpChars),
	(   PosNext1 + NOpChars + 2 =< DesiredMaxLineLength ->
	    format(' ~w ', [Op]),
	    PosNext2 is PosNext1 + NOpChars + 2 ;
	    % Newline, Indent1 spaces, Op, space
	    format('~n~*c~w ', [Indent1, 0' ,Op]),
	    PosNext2 is Indent + NOpChars + 2
	),
	(   Prettyprint1 = no_prettyprint ->
	    
	    StartNext = not_start_of_line,
	    PosNext3 = PosNext2 ;

	    % Newline, Indent1 spaces
	    format('~n~*c', [Indent1, 0' ]),
	    StartNext = start_of_line,
	    PosNext3 = Indent1
	),	    
	prettyprint1(Arg2, PosNext3-PosNext4, Indent1, DesiredMaxLineLength, Depth1-MaxDepth, 
		     StartNext-StartOut, WQ, _Prettyprint2),
	format(')', []),
	PosOut is PosNext4 + 1,
	!.
% Worst case: atom, and too long for line, so no solution. If not at start of line, go to new line,
% and just print it out.
prettyprint1(Atom, _PosIn-PosOut, Indent, _DesiredMaxLineLength, _Depth,
	     StartIn-not_start_of_line, WQ, no_prettyprint) :-
	( atomic(Atom) ; var(Atom) ),
	!,
	(   StartIn = not_start_of_line ->
	    % Newline and then Indent spaces
	    format('~n~*c', [Indent, 0' ]) ;
	    true
	),
	(   WQ = w ->
	    format_to_chars('~w', [Atom], Chars) ;
	    format_to_chars('~q', [Atom], Chars)
	),
	length(Chars, NChars),
	format('~s', [Chars]),
	PosOut is Indent + NChars,
	!.

% We've reached the end of the list or term.
% Print the closing bracket, and add a newline + indent first if line is long.
prettyprint2([], PosIn-PosOut, Indent, DesiredMaxLineLength, _Depth, _WQ, ClosingBracket, _Start) :-
	(   PosIn < DesiredMaxLineLength ->
	    format('~w', [ClosingBracket]),
	    PosOut is PosIn + 1;

	    % Newline, Indent spaces and right bracket
	    format('~n~*c~w', [Indent, 0' , ClosingBracket]),
	    PosOut is Indent + 1
	),
	!.
prettyprint2(_List, PosIn-PosOut, Indent, DesiredMaxLineLength, Depth-MaxDepth, WQ,
	     ClosingBracket, StartIn) :-
	Depth >= MaxDepth,
	prettyprint2(['...'], PosIn-PosOut, Indent, DesiredMaxLineLength, 0-1000, WQ,
		     ClosingBracket, StartIn),
	!.
prettyprint2([F | R], PosIn-PosOut, Indent, DesiredMaxLineLength, Depth-MaxDepth, WQ,
	     ClosingBracket, StartIn) :-
	Depth1 is Depth + 1,
	prettyprint1(F, PosIn-PosNext1, Indent, DesiredMaxLineLength, Depth1-MaxDepth, 
		     StartIn-StartNext, WQ, Prettyprint),
	(   % No more items in list. Print a closing bracket.
	    ( R = [] ) ->

	    format('~w', [ClosingBracket]),
	    PosOut is PosNext1 + 1 ;

	    % First element was prettyprinted and we know there are more elements.
	    % Print comma, newline and then Indent spaces, then continue.
	    ( Prettyprint = prettyprint ) ->
	    
	    format(',~n~*c', [Indent, 0' ]),
	    PosNext2 = Indent,
	    prettyprint2(R, PosNext2-PosOut, Indent, DesiredMaxLineLength, Depth1-MaxDepth, WQ,
			 ClosingBracket, start_of_line) ;

	    % Not at start of line, and room to print a comma and a space. Do that and continue on same line.
	    (  StartNext = not_start_of_line, PosNext1 + 2 =< DesiredMaxLineLength
	    ) ->
	    format(', ', []),
	    PosNext is PosNext1 + 2,
	    prettyprint2(R, PosNext-PosOut, Indent, DesiredMaxLineLength, Depth1-MaxDepth, WQ,
			 ClosingBracket, not_start_of_line) ;

	    % No room to print comma and space. Just print a comma and move to next line.
	    format(',~n~*c', [Indent, 0' ]),
	    PosNext is Indent,
	    prettyprint2(R, PosNext-PosOut, Indent, DesiredMaxLineLength, Depth1-MaxDepth, WQ,
			 ClosingBracket, start_of_line)
	),
	!.
prettyprint2(List, Pos, Indent, DesiredMaxLineLength, Depth, WQ, EndChar, Start) :-
	format('~N*** Error: bad call: ~q~n',
	       [prettyprint2(List, Pos, Indent, DesiredMaxLineLength, Depth, WQ, EndChar, Start)]).

%------------------------------------------------------------------------------------

prettyprint_key_val_pair(Key, Val) :-
	format_to_chars('~w = ', [Key], KeyChars),
	length(KeyChars, NKeyChars),
	format('~N~s', [KeyChars]),
	prettyprint(Val, NKeyChars),
	format('~n', []).

prettyprintq_key_val_pair(Key, Val) :-
	format_to_chars('~q = ', [Key], KeyChars),
	length(KeyChars, NKeyChars),
	format('~N~s', [KeyChars]),
	prettyprintq(Val, NKeyChars),
	format('~n', []).

prettyprintq_to_stream_key_val_pair(S, Key, Val) :-
	format_to_chars('~q = ', [Key], KeyChars),
	length(KeyChars, NKeyChars),
	format(S, '~N~s', [KeyChars]),
	prettyprintq_to_stream(S, Val, NKeyChars).
 
%------------------------------------------------------------------------------------

is_generalised_list(List, List, '[', ']') :-
	is_list(List),
	!.	
is_generalised_list(List, List1, '(', ')') :-
	is_comma_list(List),
	comma_list_to_list(List, List1),
	!.
is_generalised_list(List, List1, '{', '}') :-
	is_curly_bracket_list(List),
	curly_bracket_list_to_list(List, List1),
	!.

binary_operator(Op) :-
	current_op(_Precedence, Type, Op),
	member(Type, [xfx, yfx, xfy]),
	!.

term_requiring_enclosing_parentheses(Term) :-
	compound(Term),
	functor(Term, F, N),
	functor_requiring_enclosing_parentheses(F/N).

functor_requiring_enclosing_parentheses(Op/2) :-
	binary_operator(Op).
functor_requiring_enclosing_parentheses(','/2).
functor_requiring_enclosing_parentheses('-->'/2).
functor_requiring_enclosing_parentheses(':-'/2).
	
%------------------------------------------------------------------------------------

pp_debug :-
	asserta((user:portray(X) :- prettyprintq(X))).

no_pp_debug :-
	retractall((user:portray(_))).


	
