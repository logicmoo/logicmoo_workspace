% Loads all tests. Names are relative to CWD.

:- load_files([
    tests/list_handling,
    tests/charts
], [ if(not_loaded) ]).

