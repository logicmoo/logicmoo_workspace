

sciff_files([	browser,
							ccopy,
							debug,
							defaults,
							domains,
							graphviz,
							help,
							history_parser,
							ics_parser,
							ics_quant,
							parser_utils,
							pretty_print,
							print_clp_constraints,
							project,
							proof_util,
							quantif,
							reified_unif,
							ruleml_parser,
							sciff,
							sciff_options,
							sokb_parser,
							svg]).



%:- sciff_files(L), compile(L), save_files(L, 'sciff.po'), halt.
%:-compile('sciff_java_gui.pl'), save_program('sciff.po'), halt.


compile_all([]).
compile_all([H|T]) :- compile(H), save_files(H, H), compile_all(T).

:- sciff_files(L), compile_all(L), halt.