% translate BW4T messages.
% Sender IDs and IDs in message contents are all BW4T IDs. Do not use GOAL agent names.

% joining strings.
strjoin([Str],Str).
strjoin([Str1|Rest],Str12) :-  strjoin(Rest,Str2), atom_concat(Str1,Str2,Str12).
	
% imperative messages
translate(Sender,imp(in(Sender,Place)),T) :- strjoin(['I am going to room ',Place],T).
translate(Sender,imp(in(X,Place)),T) :- not(X=Sender), strjoin([X,' go to room ',Place],T).

translate(Sender,imp(found(Sender,Color)),T) :- strjoin(['I am looking for a ',Color,' block'],T).
translate(Sender,imp(found(X,Color)),T) :- not(X=Sender), strjoin([X,' find a ',Color,' block'],T).

translate(Sender,imp(pickedUpFrom(Sender,Color,Room)),T) :- 
	strjoin(['I am going to room ',Room,' to get a ',Color, ' block'],T).
translate(Sender,imp(pickedUpFrom(X,Color,Room)),T) :- 
	not(X=Sender), strjoin([X,' get the ',Color, ' block from room ',Room],T).
		
translate(Sender,imp(holding(Sender,Color)),T) :- strjoin(['I will get a ',Color,' block'],T).

translate(Sender,imp(putDown(Sender)),T) :- strjoin(['I am going to put down a block'],T).

translate(Sender, imp(putDown(PlayerID)),T) :- not(PlayerID=Sender), strjoin([PlayerID,', put down the block you are holding'],T).

% declarative messages
translate(Sender,at(Color,Room),T) :- strjoin(['Room ', Room,' contains a ',Color,' block'],T). % works also for at(Block,Room)
translate(Sender, at(Number,Color,Room),T) :- strjoin(['Room ', Room, ' contains ', Number, ' ', Color, ' blocks'],T).
translate(Sender,empty(Room),T) :- strjoin(['Room ', Room, ' is empty'],T).
translate(Sender,checked(Room),T) :- strjoin(['Room ',Room,' has been checked'],T).
translate(Sender,checked(Player,Room),T) :- strjoin(['Room ',Room,' has been checked by player ',Player],T).
translate(Sender,pickedUpFrom(Sender,Color,Room),T) :- strjoin(['I have a ',Color,' block from room ',Room],T).
translate(Sender,holding(Sender,Color),T) :- strjoin(['I have a ',Color,' block'],T).
translate(Sender,pickedUpFrom(PlayerID,Color,Room),T) :- not(PlayerID = Sender), strjoin([PlayerID,' has picked up a ',Color,' block from room ',Room],T).
translate(Sender,found(PlayerID, Color),T) :- strjoin([PlayerID, 'has found ',Color],T).
translate(Sender,putDown(Sender), T) :- T = 'I just dropped off a block'.  
translate(Sender,putDown(Sender,Color), T) :- strjoin(['I just dropped off a ',Color,' block'],T). 
translate(Sender,waitingOutside(Sender, Room),T) :- strjoin(['I am waiting outside room ',Room],T).
translate(Sender,willBeLong(PlayerID),T) :- strjoin([PlayerID,' will be long'],T).
translate(Sender,need(Color),T) :- strjoin(['We need a ',Color,' block'],T).
translate(Sender,ok(Room),T) :- strjoin(['Ok, room ',Room],T).
translate(Sender,atBox(Color),T) :- strjoin(['I am at a ',Color,' block'],T).
		
translate(Sender,in(Sender,Place),T) :- strjoin(['I am in room ',Place],T).
translate(Sender,holding(Sender,Color),T) :- strjoin(['I have a ',Color,' block'],T).
		
% ask messages
translate(Sender,int(at(unknown, Room)),T) :- strjoin(['What is in room ',Room,'?'],T).
translate(Sender,int(areClose(Player)),T) :- strjoin([Player,', are you close'],T).
translate(Sender,int(at(Color,unknown)),T) :- strjoin(['Where is a ',Color,' block?'],T).
translate(Sender,int(in(unknown,Room)),T) :- strjoin(['Who is in room ',Room,'?'],T).
translate(Sender,int(imp(in(Sender,unknown))),T) :- T='Where should I go?'.
translate(Sender,int(imp(holding(Sender,unknown))),T) :- T='What color should I get?'.
translate(Sender,int(imp(in(unknown,Room))),T) :- strjoin(['Is anybody going to room ',Room,'?'],T).
translate(Sender,int(willBeLong(Player)),T) :- strjoin([Player,', will you be long?'],T).
translate(Sender,int(checked(unknown,Room)),T) :- strjoin(['Has anybody checked room ',Room,'?'],T).
translate(Sender,int(holding(unknown,Color)),T) :- strjoin(['Who has a ',Color,' block?'],T).
		
% answer messages (all added by maaike)
translate(Sender,yes,T) :- strjoin(['Yes'],T).
translate(Sender,no,T) :- strjoin(['No'],T).
translate(Sender,dontknow,T) :- strjoin(['I do not know'],T).
translate(Sender,wait,T) :- strjoin(['Wait'],T).
translate(Sender,ok,T) :- strjoin(['OK'],T).
translate(Sender,idont,T) :- strjoin(['I do not'],T).
translate(Sender,ido,T) :- strjoin(['I do'],T).
translate(Sender,ontheway,T) :- strjoin(['I am on the way'],T).
translate(Sender,faraway,T) :- strjoin(['I am far away'],T).
translate(Sender,delayed,T) :- strjoin(['I am delayed'],T).
translate(Sender,almostthere,T) :- strjoin(['I am almost there'],T).
translate(Sender,couldnot,T) :- strjoin(['I could not'],T).
