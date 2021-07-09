:- module(hostname, [hostname/1]).
/** <module> Get hostname

 This module provides just one predicate to access the machine hostname.
 It first looks in the HOSTNAME environment variable. If this is unsuccessful,
 it call the system hostname command once and keeps the result for reuse.

 TODO: what happens if the calling hostname fails?
 */

user:term_expansion(hostname(_),hostname(H)) :-
   (  getenv('HOSTNAME',H) -> true
   ;  setup_call_cleanup(open(pipe('hostname -s'),read,S),
                         read_line_to_codes(S,Codes),
                         close(S)), 
      atom_codes(H,Codes)
   ),
   debug(hostname, 'Found hostname: "~w"', [H]).

hostname(_).
