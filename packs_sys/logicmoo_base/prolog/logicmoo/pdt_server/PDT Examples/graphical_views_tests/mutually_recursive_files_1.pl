/* The only purpose of the files in this folder is to test
 * whether the PDT ContextView and GlobalView display 
 * correctly files that contain mutually recursive predicates
 * where the recursion crosses the file borders, making the 
 * files mutually recursive too:
 * --> p/1 from file 1 calls q/1 from file 2 and vice-versa.
 */
 
:- module( mutuall_recursive_files_1, [p/1]).

p(X) :- a(X).
p(X) :- q(x).

a(1).
a(2).
a(3).