#!/usr/bin/env swipl

:- ensure_loaded(system:library(logicmoo_utils)).

:- use_listing_vars.

foo(Bar):-baz(Bar).

:-must(( with_output_to_chars(listing(foo/1),Chars),name(String,Chars),sub_string(String, _Before, Length, _After, "Bar"),Length==3)).


