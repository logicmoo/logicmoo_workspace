foreign_file('int.o', [int_init]).

foreign(int_init, int_init).

:- load_foreign_files(['int.o'],[]), int_init.


