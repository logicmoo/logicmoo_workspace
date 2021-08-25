order(neqkns for orderedIntegers) using neqkns for intbase+neqkns for totalOrder.
greater([['$r-ms','$inj-object-bool']]).
greater([['$r',\==]]).
greater([['$r-ms','\==-bool-object-object']]).
greater([['\==-bool-object-object','$inj-object-bool']]).
greater([['$r',==]]).
greater([['$r-ms','==-bool-object-object']]).
greater([['==-bool-object-object','$inj-object-bool']]).
greater([[\==,==]]).
greater([['$r-ms','$inj-object-nat']]).
greater([['==-bool-object-object','$inj-object-nat']]).
greater([['\==-bool-object-object','$inj-object-nat']]).
greater([['$r-ms','$inj-nat-zero']]).
greater([['==-bool-object-object','$inj-nat-zero']]).
greater([['\==-bool-object-object','$inj-nat-zero']]).
greater([['$r-ms','$inj-nat-posint']]).
greater([['==-bool-object-object','$inj-nat-posint']]).
greater([['\==-bool-object-object','$inj-nat-posint']]).
greater([['$r',s]]).
greater([['$r-ms','s-posint-nat']]).
greater([['s-posint-nat','$inj-object-bool']]).
greater([['s-posint-nat','$inj-object-nat']]).
greater([['s-posint-nat','$inj-nat-zero']]).
greater([['s-posint-nat','$inj-nat-posint']]).
greater([['$r-ms','$inj-object-int']]).
greater([['==-bool-object-object','$inj-object-int']]).
greater([['\==-bool-object-object','$inj-object-int']]).
greater([['s-posint-nat','$inj-object-int']]).
greater([['$r-ms','$inj-int-nat']]).
greater([['$inj-object-nat','$inj-int-nat']]).
greater([['==-bool-object-object','$inj-int-nat']]).
greater([['\==-bool-object-object','$inj-int-nat']]).
greater([['s-posint-nat','$inj-int-nat']]).
greater([['$r-ms','$inj-nzInt-posint']]).
greater([['==-bool-object-object','$inj-nzInt-posint']]).
greater([['\==-bool-object-object','$inj-nzInt-posint']]).
greater([['s-posint-nat','$inj-nzInt-posint']]).
greater([['$r-ms','$inj-int-nzInt']]).
greater([['==-bool-object-object','$inj-int-nzInt']]).
greater([['\==-bool-object-object','$inj-int-nzInt']]).
greater([['s-posint-nat','$inj-int-nzInt']]).
greater([['$r',-]]).
greater([['$r-ms','--int-int']]).
greater([['--int-int','$inj-object-bool']]).
greater([['--int-int','$inj-object-nat']]).
greater([['--int-int','$inj-nat-zero']]).
greater([['--int-int','$inj-nat-posint']]).
greater([['--int-int','$inj-object-int']]).
greater([['--int-int','$inj-int-nat']]).
greater([['--int-int','$inj-nzInt-posint']]).
greater([['--int-int','$inj-int-nzInt']]).
greater([['$r-ms','--nzInt-nzInt']]).
greater([['--nzInt-nzInt','$inj-object-bool']]).
greater([['--nzInt-nzInt','$inj-object-nat']]).
greater([['--nzInt-nzInt','$inj-nat-zero']]).
greater([['--nzInt-nzInt','$inj-nat-posint']]).
greater([['--nzInt-nzInt','$inj-object-int']]).
greater([['--nzInt-nzInt','$inj-int-nat']]).
greater([['--nzInt-nzInt','$inj-nzInt-posint']]).
greater([['--nzInt-nzInt','$inj-int-nzInt']]).
greater([['--int-int','--nzInt-nzInt']]).
greater([['$inj-object-int','$inj-object-nat']]).
greater([['$inj-nat-posint','$inj-int-nzInt']]).
greater([['$inj-nat-posint','$inj-nzInt-posint']]).
greater([['$inj-object-int','$inj-nat-posint']]).
greater([['$r',=<]]).
greater([['$r-ms','=<-bool-int-int']]).
greater([['=<-bool-int-int','$inj-object-bool']]).
greater([['=<-bool-int-int','$inj-object-int']]).
greater([['$r',>]]).
greater([['$r-ms','>-bool-int-int']]).
greater([['>-bool-int-int','$inj-object-bool']]).
greater([['>-bool-int-int','$inj-object-int']]).
greater([['$neq6-ms','$r-ms']]).
greater([['$neq6','$r']]).
greater([[>,=<]]).
greater([[>,\==]]).
greater([['$neq7-ms','$r-ms']]).
greater([['$neq7','$r']]).
greater([['$neq8-ms','$r-ms']]).
greater([['$neq8','$r']]).
greater([['$r-ms','$inj-nzInt-nzNat']]).
greater([['--int-int','$inj-nzInt-nzNat']]).
greater([['--nzInt-nzInt','$inj-nzInt-nzNat']]).
greater([['=<-bool-int-int','$inj-nzInt-nzNat']]).
greater([['==-bool-object-object','$inj-nzInt-nzNat']]).
greater([['>-bool-int-int','$inj-nzInt-nzNat']]).
greater([['\==-bool-object-object','$inj-nzInt-nzNat']]).
greater([['s-posint-nat','$inj-nzInt-nzNat']]).
status(['$r':ms]).
status(['$r-ms':ms]).
status(['$inj-object-bool':ms]).
status(['==-bool-object-object':ms]).
status([== :ms]).
status(['\==-bool-object-object':ms]).
status([\== :ms]).
status(['$inj-object-nat':ms]).
status(['$inj-nat-zero':ms]).
status(['$inj-nat-posint':ms]).
status(['$inj-object-int':ms]).
status(['$inj-int-nat':ms]).
status(['$inj-nzInt-posint':ms]).
status(['$inj-int-nzInt':ms]).
status(['$neq6-ms':lr]).
status(['$neq6':lr]).
status(['$neq7-ms':lr]).
status(['$neq7':lr]).
status(['$neq8-ms':lr]).
status(['$neq8':lr]).
status(['$inj-nzInt-nzNat':ms]).
status([=< :ms]).

constructor('true-bool').
constructor('false-bool').
constructor('0-zero').

/* ordering of the literals is ms for
/*	x:object==y:object = y:object==x:object */
action(status(ms),equation([],['==-bool-object-object'(@'x-object',@'y-object')='==-bool-object-object'(@'y-object',@'x-object')])).

/* ordering of the literals is ms for
/*	x:object\==y:object = y:object\==x:object */
action(status(ms),equation([],['\==-bool-object-object'(@'x-object',@'y-object')='\==-bool-object-object'(@'y-object',@'x-object')])).

/* declare as nonoperational :
/*	x:object==y:object = true => x:object = y:object */
action(orient(n),equation(['==-bool-object-object'(@'x-object',@'y-object')='true-bool'],[@'x-object'= @'y-object'])).

/* ordering of the literals is ms for
/*	x:object==y:object = true => x:object = y:object */
action(status(ms),equation(['==-bool-object-object'(@'x-object',@'y-object')='true-bool'],[@'x-object'= @'y-object'])).

/* declare as nonoperational :
/*	s (n:nat) = s (m:nat) => n:nat = m:nat */
action(orient(n),equation(['s-posint-nat'(@'n-nat')='s-posint-nat'(@'m-nat')],[@'n-nat'= @'m-nat'])).

/* ordering of the literals is ms for
/*	s (n:nat) = s (m:nat) => n:nat = m:nat */
action(status(ms),equation(['s-posint-nat'(@'n-nat')='s-posint-nat'(@'m-nat')],[@'n-nat'= @'m-nat'])).

/* declare as nonoperational :
/*	- (i:int) = - (j:int) => i:int = j:int */
action(orient(n),equation(['--int-int'(@'i-int')='--int-int'(@'j-int')],[@'i-int'= @'j-int'])).

/* ordering of the literals is ms for
/*	- (i:int) = - (j:int) => i:int = j:int */
action(status(ms),equation(['--int-int'(@'i-int')='--int-int'(@'j-int')],[@'i-int'= @'j-int'])).

/* declare as nonoperational :
/*	int(- (X1:nzInt)) = - (j:int) => int(X1:nzInt) = j:int */
action(orient(n),equation(['$inj-int-nzInt'('--nzInt-nzInt'(@'X1-nzInt'))='--int-int'(@'j-int')],['$inj-int-nzInt'(@'X1-nzInt')= @'j-int'])).

/* declare as nonoperational :
/*	int(- (X1:nzInt)) = int(- (X:nzInt)) => X:nzInt = X1:nzInt */
action(orient(n),equation(['$inj-int-nzInt'('--nzInt-nzInt'(@'X1-nzInt'))='$inj-int-nzInt'('--nzInt-nzInt'(@'X-nzInt'))],[@'X-nzInt'= @'X1-nzInt'])).

/* declare as nonoperational :
/*	int(nat(0)) = - (j:int) => int(nat(0)) = j:int */
action(orient(n),equation(['$inj-int-nat'('$inj-nat-zero'('0-zero'))='--int-int'(@'j-int')],['$inj-int-nat'('$inj-nat-zero'('0-zero'))= @'j-int'])).

/* declare as nonoperational :
/*	int(- (X1:nzInt)) = int(nat(0)) => int(nat(0)) = int(X1:nzInt) */
action(orient(n),equation(['$inj-int-nzInt'('--nzInt-nzInt'(@'X1-nzInt'))='$inj-int-nat'('$inj-nat-zero'('0-zero'))],['$inj-int-nat'('$inj-nat-zero'('0-zero'))='$inj-int-nzInt'(@'X1-nzInt')])).

/* declare as nonoperational :
/*	int(nat(0)) = int(- (X1:nzInt)) => int(X1:nzInt) = int(nat(0)) */
action(orient(n),equation(['$inj-int-nat'('$inj-nat-zero'('0-zero'))='$inj-int-nzInt'('--nzInt-nzInt'(@'X1-nzInt'))],['$inj-int-nzInt'(@'X1-nzInt')='$inj-int-nat'('$inj-nat-zero'('0-zero'))])).

/* declare as nonoperational :
/*	object(A:nat) = object(B:int) => int(A:nat) = B:int */
action(orient(n),equation(['$inj-object-nat'(@'A-nat')='$inj-object-int'(@'B-int')],['$inj-int-nat'(@'A-nat')= @'B-int'])).

/* declare as nonoperational :
/*	object(A:nat) = object(A1:nat) => A1:nat = A:nat */
action(orient(n),equation(['$inj-object-nat'(@'A-nat')='$inj-object-nat'(@'A1-nat')],[@'A1-nat'= @'A-nat'])).

/* declare as nonoperational :
/*	int(nzInt(A:posint)) = int(B:nat) => nat(A:posint) = B:nat */
action(orient(n),equation(['$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint'))='$inj-int-nat'(@'B-nat')],['$inj-nat-posint'(@'A-posint')= @'B-nat'])).

/* declare as nonoperational :
/*	int(nzInt(A:posint)) = int(nzInt(A1:posint)) => A1:posint = A:posint */
action(orient(n),equation(['$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint'))='$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint'))],[@'A1-posint'= @'A-posint'])).

/* declare as nonoperational :
/*	i:int = - (j:int) => - (i:int) = j:int */
action(orient(n),equation([@'i-int'='--int-int'(@'j-int')],['--int-int'(@'i-int')= @'j-int'])).

/* declare as nonoperational :
/*	int(- (X1:nzInt)) = i:int => - (i:int) = int(X1:nzInt) */
action(orient(n),equation(['$inj-int-nzInt'('--nzInt-nzInt'(@'X1-nzInt'))= @'i-int'],['--int-int'(@'i-int')='$inj-int-nzInt'(@'X1-nzInt')])).

/* declare as nonoperational :
/*	int(nat(0)) = i:int => - (i:int) = i:int */
action(orient(n),equation(['$inj-int-nat'('$inj-nat-zero'('0-zero'))= @'i-int'],['--int-int'(@'i-int')= @'i-int'])).

/* declare as nonoperational :
/*	int(X1:nzInt) = i:int => - (i:int) = int(- (X1:nzInt)) */
action(orient(n),equation(['$inj-int-nzInt'(@'X1-nzInt')= @'i-int'],['--int-int'(@'i-int')='$inj-int-nzInt'('--nzInt-nzInt'(@'X1-nzInt'))])).

/* declare as nonoperational :
/*	object(nat(A:posint)) = object(A:nat) => int(A:nat) = int(nzInt(A:posint)) */
action(orient(n),equation(['$inj-object-nat'('$inj-nat-posint'(@'A-posint'))='$inj-object-nat'(@'A-nat')],['$inj-int-nat'(@'A-nat')='$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint'))])).

/* declare as nonoperational :
/*	[y:int=<x:int] = [x:int=<y:int,false] */
action(orient(n),equation([],[['=<-bool-int-int'(@'y-int',@'x-int')]=['=<-bool-int-int'(@'x-int',@'y-int'),'false-bool']])).

/* ordering of the literals is [1,0] for
/*	x:int=<y:int = false => y:int=<x:int = true */
action(status([1,0]),equation(['=<-bool-int-int'(@'x-int',@'y-int')='false-bool'],['=<-bool-int-int'(@'y-int',@'x-int')='true-bool'])).

/* declare as nonoperational :
/*	x:int=<y:int = true and y:int=<x:int = true => x:int = y:int */
action(orient(n),equation(['=<-bool-int-int'(@'x-int',@'y-int')='true-bool','=<-bool-int-int'(@'y-int',@'x-int')='true-bool'],[@'x-int'= @'y-int'])).

/* condition number 1 is the selected condition for
/*	x:int=<y:int = true and y:int=<x:int = true => x:int = y:int */
action(selectcondition(1),equation(['=<-bool-int-int'(@'x-int',@'y-int')='true-bool','=<-bool-int-int'(@'y-int',@'x-int')='true-bool'],[@'x-int'= @'y-int'])).

/* ordering of the literals is [0,1,2] for
/*	x:int=<y:int = true and y:int=<x:int = true => x:int = y:int */
action(status([0,1,2]),equation(['=<-bool-int-int'(@'x-int',@'y-int')='true-bool','=<-bool-int-int'(@'y-int',@'x-int')='true-bool'],[@'x-int'= @'y-int'])).

/* declare as nonoperational :
/*	x:int=<y:int = true and y:int=<z:int = true => x:int=<z:int = true */
action(orient(n),equation(['=<-bool-int-int'(@'x-int',@'y-int')='true-bool','=<-bool-int-int'(@'y-int',@'z-int')='true-bool'],['=<-bool-int-int'(@'x-int',@'z-int')='true-bool'])).

/* condition number 1 is the selected condition for
/*	x:int=<y:int = true and y:int=<z:int = true => x:int=<z:int = true */
action(selectcondition(1),equation(['=<-bool-int-int'(@'x-int',@'y-int')='true-bool','=<-bool-int-int'(@'y-int',@'z-int')='true-bool'],['=<-bool-int-int'(@'x-int',@'z-int')='true-bool'])).

/* ordering of the literals is [0,1,2] for
/*	x:int=<y:int = true and y:int=<z:int = true => x:int=<z:int = true */
action(status([0,1,2]),equation(['=<-bool-int-int'(@'x-int',@'y-int')='true-bool','=<-bool-int-int'(@'y-int',@'z-int')='true-bool'],['=<-bool-int-int'(@'x-int',@'z-int')='true-bool'])).

/* declare as nonoperational :
/*	int(nzInt(n:posint))=<z:int = true => int(nat(0))=<z:int = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'n-posint')),@'z-int')='true-bool'],['=<-bool-int-int'('$inj-int-nat'('$inj-nat-zero'('0-zero')),@'z-int')='true-bool'])).

/* declare as nonoperational :
/*	int(nat(0))=<int(k:nat) = true and int(nat(0))=<z:int = true => -int(k:nat)=<z:int = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nat'('$inj-nat-zero'('0-zero')),'$inj-int-nat'(@'k-nat'))='true-bool','=<-bool-int-int'('$inj-int-nat'('$inj-nat-zero'('0-zero')),@'z-int')='true-bool'],['=<-bool-int-int'('--int-int'('$inj-int-nat'(@'k-nat')),@'z-int')='true-bool'])).

/* condition number 1 is the selected condition for
/*	int(nat(0))=<int(k:nat) = true and int(nat(0))=<z:int = true => -int(k:nat)=<z:int = true */
action(selectcondition(1),equation(['=<-bool-int-int'('$inj-int-nat'('$inj-nat-zero'('0-zero')),'$inj-int-nat'(@'k-nat'))='true-bool','=<-bool-int-int'('$inj-int-nat'('$inj-nat-zero'('0-zero')),@'z-int')='true-bool'],['=<-bool-int-int'('--int-int'('$inj-int-nat'(@'k-nat')),@'z-int')='true-bool'])).

/* declare as nonoperational :
/*	int(nat(0))=<z:int = true => int(-nzInt(A:posint))=<z:int = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nat'('$inj-nat-zero'('0-zero')),@'z-int')='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('--nzInt-nzInt'('$inj-nzInt-posint'(@'A-posint'))),@'z-int')='true-bool'])).

/* declare as nonoperational :
/*	int(m:nat)=<int(nat(0)) = true and -int(m:nat)=<z:int = true => int(nat(0))=<z:int = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nat'('$inj-nat-zero'('0-zero')))='true-bool','=<-bool-int-int'('--int-int'('$inj-int-nat'(@'m-nat')),@'z-int')='true-bool'],['=<-bool-int-int'('$inj-int-nat'('$inj-nat-zero'('0-zero')),@'z-int')='true-bool'])).

/* condition number 1 is the selected condition for
/*	int(m:nat)=<int(nat(0)) = true and -int(m:nat)=<z:int = true => int(nat(0))=<z:int = true */
action(selectcondition(1),equation(['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nat'('$inj-nat-zero'('0-zero')))='true-bool','=<-bool-int-int'('--int-int'('$inj-int-nat'(@'m-nat')),@'z-int')='true-bool'],['=<-bool-int-int'('$inj-int-nat'('$inj-nat-zero'('0-zero')),@'z-int')='true-bool'])).

/* declare as nonoperational :
/*	int(m:nat)=<int(nat(0)) = true => int(m:nat)=<int(nzInt(A:posint)) = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nat'('$inj-nat-zero'('0-zero')))='true-bool'],['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')))='true-bool'])).

/* declare as nonoperational :
/*	int(nzInt(n:posint))=< -int(m:nat) = true => -int(m:nat) = int(nzInt(n:posint)) */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'n-posint')),'--int-int'('$inj-int-nat'(@'m-nat')))='true-bool'],['--int-int'('$inj-int-nat'(@'m-nat'))='$inj-int-nzInt'('$inj-nzInt-posint'(@'n-posint'))])).

/* declare as nonoperational :
/*	int(nzInt(n:posint))=<z:int = true => -int(m:nat)=<z:int = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'n-posint')),@'z-int')='true-bool'],['=<-bool-int-int'('--int-int'('$inj-int-nat'(@'m-nat')),@'z-int')='true-bool'])).

/* declare as nonoperational :
/*	int(m:nat)=<int(k:nat) = true and -int(m:nat)=<z:int = true => -int(k:nat)=<z:int = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nat'(@'k-nat'))='true-bool','=<-bool-int-int'('--int-int'('$inj-int-nat'(@'m-nat')),@'z-int')='true-bool'],['=<-bool-int-int'('--int-int'('$inj-int-nat'(@'k-nat')),@'z-int')='true-bool'])).

/* condition number 1 is the selected condition for
/*	int(m:nat)=<int(k:nat) = true and -int(m:nat)=<z:int = true => -int(k:nat)=<z:int = true */
action(selectcondition(1),equation(['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nat'(@'k-nat'))='true-bool','=<-bool-int-int'('--int-int'('$inj-int-nat'(@'m-nat')),@'z-int')='true-bool'],['=<-bool-int-int'('--int-int'('$inj-int-nat'(@'k-nat')),@'z-int')='true-bool'])).

/* declare as nonoperational :
/*	int(m:nat)=<int(nzInt(A:posint)) = true and -int(m:nat)=<z:int = true => int(-nzInt(A:posint))=<z:int = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')))='true-bool','=<-bool-int-int'('--int-int'('$inj-int-nat'(@'m-nat')),@'z-int')='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('--nzInt-nzInt'('$inj-nzInt-posint'(@'A-posint'))),@'z-int')='true-bool'])).

/* condition number 1 is the selected condition for
/*	int(m:nat)=<int(nzInt(A:posint)) = true and -int(m:nat)=<z:int = true => int(-nzInt(A:posint))=<z:int = true */
action(selectcondition(1),equation(['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')))='true-bool','=<-bool-int-int'('--int-int'('$inj-int-nat'(@'m-nat')),@'z-int')='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('--nzInt-nzInt'('$inj-nzInt-posint'(@'A-posint'))),@'z-int')='true-bool'])).

/* declare as nonoperational :
/*	int(nzInt(A:posint))=<int(k:nat) = true and int(-nzInt(A:posint))=<z:int = true => -int(k:nat)=<z:int = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nat'(@'k-nat'))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('--nzInt-nzInt'('$inj-nzInt-posint'(@'A-posint'))),@'z-int')='true-bool'],['=<-bool-int-int'('--int-int'('$inj-int-nat'(@'k-nat')),@'z-int')='true-bool'])).

/* condition number 2 is the selected condition for
/*	int(nzInt(A:posint))=<int(k:nat) = true and int(-nzInt(A:posint))=<z:int = true => -int(k:nat)=<z:int = true */
action(selectcondition(2),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nat'(@'k-nat'))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('--nzInt-nzInt'('$inj-nzInt-posint'(@'A-posint'))),@'z-int')='true-bool'],['=<-bool-int-int'('--int-int'('$inj-int-nat'(@'k-nat')),@'z-int')='true-bool'])).

/* declare as nonoperational :
/*	int(nzInt(A:posint))=<int(nzInt(A1:posint)) = true and int(-nzInt(A:posint))=<z:int = true => int(-nzInt(A1:posint))=<z:int = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('--nzInt-nzInt'('$inj-nzInt-posint'(@'A-posint'))),@'z-int')='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('--nzInt-nzInt'('$inj-nzInt-posint'(@'A1-posint'))),@'z-int')='true-bool'])).

/* condition number 2 is the selected condition for
/*	int(nzInt(A:posint))=<int(nzInt(A1:posint)) = true and int(-nzInt(A:posint))=<z:int = true => int(-nzInt(A1:posint))=<z:int = true */
action(selectcondition(2),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('--nzInt-nzInt'('$inj-nzInt-posint'(@'A-posint'))),@'z-int')='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('--nzInt-nzInt'('$inj-nzInt-posint'(@'A1-posint'))),@'z-int')='true-bool'])).

/* declare as nonoperational :
/*	int(m:nat)=<z:int = true => int(-nzInt(n:posint))=<z:int = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),@'z-int')='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('--nzInt-nzInt'('$inj-nzInt-posint'(@'n-posint'))),@'z-int')='true-bool'])).

/* declare as nonoperational :
/*	int(nzInt(A:posint))=<int(k:nat) = true => -int(k:nat)=<int(m:nat) = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'],['=<-bool-int-int'('--int-int'('$inj-int-nat'(@'k-nat')),'$inj-int-nat'(@'m-nat'))='true-bool'])).

/* declare as nonoperational :
/*	int(m:nat)=<int(nzInt(A1:posint)) = true and int(nzInt(A1:posint))=<int(k:nat) = true => int(m:nat)=<int(k:nat) = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'],['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nat'(@'k-nat'))='true-bool'])).

/* condition number 1 is the selected condition for
/*	int(m:nat)=<int(nzInt(A1:posint)) = true and int(nzInt(A1:posint))=<int(k:nat) = true => int(m:nat)=<int(k:nat) = true */
action(selectcondition(1),equation(['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'],['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nat'(@'k-nat'))='true-bool'])).

/* declare as nonoperational :
/*	int(nzInt(A:posint))=<int(nzInt(A1:posint)) = true and int(nzInt(A1:posint))=<int(k:nat) = true => int(nzInt(A:posint))=<int(k:nat) = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'])).

/* condition number 1 is the selected condition for
/*	int(nzInt(A:posint))=<int(nzInt(A1:posint)) = true and int(nzInt(A1:posint))=<int(k:nat) = true => int(nzInt(A:posint))=<int(k:nat) = true */
action(selectcondition(1),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'])).

/* declare as nonoperational :
/*	int(m:nat)=<int(nzInt(A2:posint)) = true and int(nzInt(A2:posint))=<int(nzInt(A1:posint)) = true => int(m:nat)=<int(nzInt(A1:posint)) = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A2-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A2-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool'],['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool'])).

/* condition number 1 is the selected condition for
/*	int(m:nat)=<int(nzInt(A2:posint)) = true and int(nzInt(A2:posint))=<int(nzInt(A1:posint)) = true => int(m:nat)=<int(nzInt(A1:posint)) = true */
action(selectcondition(1),equation(['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A2-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A2-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool'],['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool'])).

/* declare as nonoperational :
/*	int(nzInt(A:posint))=<int(nzInt(A2:posint)) = true and int(nzInt(A2:posint))=<int(nzInt(A1:posint)) = true => int(nzInt(A:posint))=<int(nzInt(A1:posint)) = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A2-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A2-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool'])).

/* condition number 1 is the selected condition for
/*	int(nzInt(A:posint))=<int(nzInt(A2:posint)) = true and int(nzInt(A2:posint))=<int(nzInt(A1:posint)) = true => int(nzInt(A:posint))=<int(nzInt(A1:posint)) = true */
action(selectcondition(1),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A2-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A2-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool'])).

/* declare as nonoperational :
/*	int(m:nat)=<int(k:nat) = true and int(nzInt(s (k:nat)))=<z:int = true => int(nzInt(s (m:nat)))=<z:int = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nat'(@'k-nat'))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'('s-posint-nat'(@'k-nat'))),@'z-int')='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'('s-posint-nat'(@'m-nat'))),@'z-int')='true-bool'])).

/* condition number 2 is the selected condition for
/*	int(m:nat)=<int(k:nat) = true and int(nzInt(s (k:nat)))=<z:int = true => int(nzInt(s (m:nat)))=<z:int = true */
action(selectcondition(2),equation(['=<-bool-int-int'('$inj-int-nat'(@'m-nat'),'$inj-int-nat'(@'k-nat'))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'('s-posint-nat'(@'k-nat'))),@'z-int')='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'('s-posint-nat'(@'m-nat'))),@'z-int')='true-bool'])).

/* declare as nonoperational :
/*	int(k1:nat)=<int(k:nat) = true and int(m1:nat)=<int(k1:nat) = true => int(m1:nat)=<int(k:nat) = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nat'(@'k1-nat'),'$inj-int-nat'(@'k-nat'))='true-bool','=<-bool-int-int'('$inj-int-nat'(@'m1-nat'),'$inj-int-nat'(@'k1-nat'))='true-bool'],['=<-bool-int-int'('$inj-int-nat'(@'m1-nat'),'$inj-int-nat'(@'k-nat'))='true-bool'])).

/* condition number 2 is the selected condition for
/*	int(k1:nat)=<int(k:nat) = true and int(m1:nat)=<int(k1:nat) = true => int(m1:nat)=<int(k:nat) = true */
action(selectcondition(2),equation(['=<-bool-int-int'('$inj-int-nat'(@'k1-nat'),'$inj-int-nat'(@'k-nat'))='true-bool','=<-bool-int-int'('$inj-int-nat'(@'m1-nat'),'$inj-int-nat'(@'k1-nat'))='true-bool'],['=<-bool-int-int'('$inj-int-nat'(@'m1-nat'),'$inj-int-nat'(@'k-nat'))='true-bool'])).

/* declare as nonoperational :
/*	int(m1:nat)=<int(nzInt(A:posint)) = true and int(nzInt(A:posint))=<int(k:nat) = true => int(m1:nat)=<int(k:nat) = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nat'(@'m1-nat'),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'],['=<-bool-int-int'('$inj-int-nat'(@'m1-nat'),'$inj-int-nat'(@'k-nat'))='true-bool'])).

/* condition number 1 is the selected condition for
/*	int(m1:nat)=<int(nzInt(A:posint)) = true and int(nzInt(A:posint))=<int(k:nat) = true => int(m1:nat)=<int(k:nat) = true */
action(selectcondition(1),equation(['=<-bool-int-int'('$inj-int-nat'(@'m1-nat'),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'],['=<-bool-int-int'('$inj-int-nat'(@'m1-nat'),'$inj-int-nat'(@'k-nat'))='true-bool'])).

/* declare as nonoperational :
/*	int(nzInt(A:posint))=<int(k1:nat) = true and int(k1:nat)=<int(k:nat) = true => int(nzInt(A:posint))=<int(k:nat) = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nat'(@'k1-nat'))='true-bool','=<-bool-int-int'('$inj-int-nat'(@'k1-nat'),'$inj-int-nat'(@'k-nat'))='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'])).

/* condition number 1 is the selected condition for
/*	int(nzInt(A:posint))=<int(k1:nat) = true and int(k1:nat)=<int(k:nat) = true => int(nzInt(A:posint))=<int(k:nat) = true */
action(selectcondition(1),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nat'(@'k1-nat'))='true-bool','=<-bool-int-int'('$inj-int-nat'(@'k1-nat'),'$inj-int-nat'(@'k-nat'))='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'])).

/* declare as nonoperational :
/*	int(nzInt(A:posint))=<int(nzInt(A1:posint)) = true and int(nzInt(A1:posint))=<int(k:nat) = true => int(nzInt(A:posint))=<int(k:nat) = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'])).

/* condition number 1 is the selected condition for
/*	int(nzInt(A:posint))=<int(nzInt(A1:posint)) = true and int(nzInt(A1:posint))=<int(k:nat) = true => int(nzInt(A:posint))=<int(k:nat) = true */
action(selectcondition(1),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'])).

/* declare as nonoperational :
/*	int(nzInt(A1:posint))=<int(nzInt(A:posint)) = true and int(nzInt(A:posint))=<int(k:nat) = true => int(nzInt(A1:posint))=<int(k:nat) = true */
action(orient(n),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'])).

/* condition number 1 is the selected condition for
/*	int(nzInt(A1:posint))=<int(nzInt(A:posint)) = true and int(nzInt(A:posint))=<int(k:nat) = true => int(nzInt(A1:posint))=<int(k:nat) = true */
action(selectcondition(1),equation(['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')),'$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')))='true-bool','=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'],['=<-bool-int-int'('$inj-int-nzInt'('$inj-nzInt-posint'(@'A1-posint')),'$inj-int-nat'(@'k-nat'))='true-bool'])).
