:- randomizable utterance//1, exclamation//1, sentence//1.

utterance(DialogAct) --> exclamation(DialogAct).
utterance(DialogAct) --> sentence(DialogAct).

exclamation(interruption) --> [excuse, me].
exclamation(acknowledgement) --> [yes, '?'].

exclamation(greeting-reply) --> exclamation(greeting).

exclamation(greeting) --> [X], member(X, [hey, hello, hi]).
exclamation(greeting) --> [hi, there].

exclamation(apology) --> [sorry].
exclamation(apology-minimization) --> [no, problem].
exclamation(apology-minimization) --> [quite, alright].

exclamation(parting) --> [X], member(X, [bye, byebye, goodbye]).
exclamation(parting) --> [see, you].
exclamation(parting) --> [be, seeing, you].
exclamation(parting-reply) --> exclamation(parting).
