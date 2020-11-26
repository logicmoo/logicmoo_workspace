type_restriction(member(A,B),[atomic(A),list(B)]).
type_restriction(components(A,B,C),[list(A),atomic(B),list(C)]).

list([]).
list([A|B]):- atomic(A),list(B).


components([X|Y],X,Y).


ex(member(e,[e,o,z]),+).
ex(member(o,[o,z]),+).
ex(member(o,[e,o,z]),+).
ex(member(z,[z]),+).
ex(member(z,[o,z]),+).
ex(member(z,[e,o,z]),+).



ex(member(e,e),-).
ex(member(e,o),-).
ex(member(e,z),-).
ex(member(e,[]),-).
ex(member(e,[z]),-).
ex(member(e,[o,z]),-).
ex(member(o,e),-).
ex(member(o,o),-).
ex(member(o,z),-).
ex(member(o,[]),-).
ex(member(o,[z]),-).
ex(member(z,e),-).
ex(member(z,o),-).
ex(member(z,z),-).
ex(member(z,[]),-).
ex(member([],e),-).
ex(member([],o),-).
ex(member([],z),-).
ex(member([],[]),-).
ex(member([],[z]),-).
ex(member([],[o,z]),-).
ex(member([],[e,o,z]),-).
ex(member([z],e),-).
ex(member([z],o),-).
ex(member([z],z),-).
ex(member([z],[]),-).
ex(member([z],[z]),-).
ex(member([z],[o,z]),-).
ex(member([z],[e,o,z]),-).
ex(member([o,z],e),-).
ex(member([o,z],o),-).
ex(member([o,z],z),-).
ex(member([o,z],[]),-).
ex(member([o,z],[z]),-).
ex(member([o,z],[o,z]),-).
ex(member([o,z],[e,o,z]),-).
ex(member([e,o,z],e),-).
ex(member([e,o,z],o),-).
ex(member([e,o,z],z),-).
ex(member([e,o,z],[]),-).
ex(member([e,o,z],[z]),-).
ex(member([e,o,z],[o,z]),-).
ex(member([e,o,z],[e,o,z]),-).
