# instant_prolog_docs
Magically document prolog source files based on predicate and variable naming conventions


```prolog

 :- pack_install('https://github.com/TeamSPoon/instant_prolog_docs.git').

```


```prolog

:- use_module(library(instant_prolog_docs)).

?- autodoc_file('some_file.pl').

```

I haven't ran this for a very very long time so carefull.


# Some TODOs

Make the Anglification system read a : instant_prolog_docs.config  (instead of being in the module itself)
Document this pack!
Write tests
Untangle the 'pack' install deps
Still in progress (Moving predicates over here from logicmoo_base)


[BSD 2-Clause License](LICENSE.md)

Copyright (c) 2017, 
Douglas Miles <logicmoo@gmail.com> and
TeamSPoon - All rights reserved.

# Not _obligated_ to maintain a git fork just to contribute

Dislike having tons of forks that are several commits behind the main git repo?

Be old school - Please ask to be added to TeamSPoon and Contribute directly !
Still, we wont stop you from doing it the Fork+PullRequest method

