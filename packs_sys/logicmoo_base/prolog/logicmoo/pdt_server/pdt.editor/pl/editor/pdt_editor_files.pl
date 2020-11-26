:- module(pdt_editor_files, [file_to_reload_for_included_file/2]).

:- use_module(library(lists), [
	member/2
]).

file_to_reload_for_included_file(IncludedFile, FileToLoad) :-
	setof(
		F,
		IncludedFile^L^source_file_property(IncludedFile, included_in(F, L)),
		Fs
	),
	(	member(FileToLoad, Fs)
	;	source_file(IncludedFile),
		FileToLoad = IncludedFile
	).
