% The knowledge of the teacher about different topics

% Fact theory

db_entry(teacher:gilchrist_family,parent(euan,warren),[]).
db_entry(teacher:gilchrist_family,parent(berenice,warren),[]).
db_entry(teacher:gilchrist_family,parent(warren,catherine),[]).
db_entry(teacher:gilchrist_family,parent(warren,charles),[]).
db_entry(teacher:gilchrist_family,parent(warren,david),[]).
db_entry(teacher:gilchrist_family,parent(warren,julia),[]).
db_entry(teacher:gilchrist_family,parent(kate,catherine),[]).
db_entry(teacher:gilchrist_family,parent(kate,charles),[]).
db_entry(teacher:gilchrist_family,parent(kate,david),[]).
db_entry(teacher:gilchrist_family,parent(kate,julia),[]).
db_entry(teacher:gilchrist_family,parent(charles,lucinda),[]).

% Fact and Rule Theory

def_theory(teacher:entropy, [teacher:warm,teacher:cold,teacher:door]).
db_entry(teacher:warm, warm(kitchen),[]).
db_entry(teacher:warm, warm(living_room),[]).
db_entry(teacher:cold, cold(fridge),[]).
db_entry(teacher:cold, cold(street),[]).
db_entry(teacher:cold, cold(garden),[]).
db_entry(teacher:door, door(living_room,kitchen),[]).
db_entry(teacher:door, door(living_room,street),[]).
db_entry(teacher:door, door(living_room,garden),[]).
db_entry(teacher:door, door(kitchen,garden),[]).
db_entry(teacher:door, door(kitchen,fridge),[]).
db_entry(teacher:door, door(street,garden),[]).
db_entry(teacher:entropy, entropy_increases(A,B),[warm(A),cold(B),door(A,B)]).
db_entry(teacher:entropy, entropy_increases(A,B),[cold(A),warm(B),door(A,B)]).
db_entry(teacher:entropy, entropy_increases(A,B),[warm(A),cold(B),door(B,A)]).
db_entry(teacher:entropy, entropy_increases(A,B),[cold(A),warm(B),door(B,A)]).

% Recusive Theory

db_entry(teacher:t_member, member(A,[A|_]),[]).
db_entry(teacher:t_member, member(A,[_|B]),[member(A,B)]).

db_entry(teacher:t_append, append([],List,List),[]).
db_entry(teacher:t_append, append([First|Rest],List,[First|TempList]),
			   [append(Rest,List,TempList)]).

def_theory(teacher:t_reverse,[teacher:t_append]).
db_entry(teacher:t_reverse,reverse([],[]),[]).
db_entry(teacher:t_reverse,reverse([X|Y],Z),
			   [reverse(Y,Y1),append(Y1,[X],Z)]).

def_theory(teacher:qsort,[teacher:partition,teacher:t_append]).
db_entry(teacher:qsort,qsort([],[]),[]).
db_entry(teacher:qsort,qsort([X|L],L5),
		       [partition(L,X,L1,L2),qsort(L1,L3),
			qsort(L2,L4),append(L3,[X|L4],L5)]).

db_entry(teacher:partition,partition([],_,[],[]),[]).
db_entry(teacher:partition,partition([X|L],Y,[X|L1],L2),
			   [X < Y, partition(L,Y,L1,L2)]).
db_entry(teacher:partition,partition([X|L],Y,L1,[X|L2]),
			   [X >= Y, partition(L,Y,L1,L2)]).
