% ensure this file does not get unloaded with mpred_reset
:- prolog_load_context(file,F), ain(mpred_unload_option(F,never)).

