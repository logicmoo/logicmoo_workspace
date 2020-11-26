% Right knowledge of a learner about different topics

% Fact theory

db_entry(learner:gilchrist_family,parent(euan,warren),[]).
db_entry(learner:gilchrist_family,parent(berenice,warren),[]).
db_entry(learner:gilchrist_family,parent(warren,catherine),[]).
db_entry(learner:gilchrist_family,parent(warren,charles),[]).
db_entry(learner:gilchrist_family,parent(warren,david),[]).
db_entry(learner:gilchrist_family,parent(warren,julia),[]).
db_entry(learner:gilchrist_family,parent(kate,catherine),[]).
db_entry(learner:gilchrist_family,parent(kate,charles),[]).
db_entry(learner:gilchrist_family,parent(kate,david),[]).
db_entry(learner:gilchrist_family,parent(kate,julia),[]).
db_entry(learner:gilchrist_family,parent(charles,lucinda),[]).

% Fact and Rule Theory

def_theory(learner:entropy, [learner:warm,learner:cold,learner:door]).
db_entry(learner:warm, warm(kitchen),[]).
db_entry(learner:warm, warm(living_room),[]).
db_entry(learner:cold, cold(fridge),[]).
db_entry(learner:cold, cold(street),[]).
db_entry(learner:cold, cold(garden),[]).
db_entry(learner:door, door(living_room,kitchen),[]).
db_entry(learner:door, door(living_room,street),[]).
db_entry(learner:door, door(living_room,garden),[]).
db_entry(learner:door, door(kitchen,garden),[]).
db_entry(learner:door, door(kitchen,fridge),[]).
db_entry(learner:door, door(street,garden),[]).
db_entry(learner:entropy, entropy_increases(A,B),[warm(A),cold(B),door(A,B)]).
db_entry(learner:entropy, entropy_increases(A,B),[cold(A),warm(B),door(A,B)]).
db_entry(learner:entropy, entropy_increases(A,B),[warm(A),cold(B),door(B,A)]).
db_entry(learner:entropy, entropy_increases(A,B),[cold(A),warm(B),door(B,A)]).

% Recusive Theory

db_entry(learner:t_member, member(A,[A|_]),[]).
db_entry(learner:t_member, member(A,[_|B]),[member(A,B)]).

db_entry(learner:t_append, append([],List,List),[]).
db_entry(learner:t_append, append([First|Rest],List,[First|TempList]),
			   [append(Rest,List,TempList)]).

def_theory(learner:t_reverse,[learner:t_append]).
db_entry(learner:t_reverse,reverse([],[]),[]).
db_entry(learner:t_reverse,reverse([X|Y],Z),
			   [reverse(Y,Y1),append(Y1,[X],Z)]).

def_theory(learner:qsort,[learner:partition,learner:t_append]).
db_entry(learner:qsort,qsort([],[]),[]).
db_entry(learner:qsort,qsort([X|L],L5),
		       [partition(L,X,L1,L2),qsort(L1,L3),
		        qsort(L2,L4),append(L3,[X|L4],L5)]).

db_entry(learner:partition,partition([],_,[],[]),[]).
db_entry(learner:partition,partition([X|L],Y,[X|L1],L2),
			   [X < Y, partition(L,Y,L1,L2)]).
db_entry(learner:partition,partition([X|L],Y,L1,[X|L2]),
			   [X >= Y, partition(L,Y,L1,L2)]).
