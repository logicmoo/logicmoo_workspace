order poly3 for qsort using poly3 for lists +
			    poly1 for totalOrder.


setInterpretation([sort(x)     : [ 2 * x, 2 * x, 2 * x ],
		   split(x,y)  : [ x + y + 1, x + 2 * y, x + 2 * y ],
		   (x,y)       : [ x + y, x + y, x + y ]]).

setInterpretation(['$h24_0'(x,y) : [ 2 * x + 3 * y + 1, 2 * x + 2 * y, 2 * x + 2 * y ],
%    (for sort quasireductive)
		   '$h25_0'(x,y) : [ x + 3 * y + 1, x + y + 1, x + y + 1 ],
%    (for split quasireductive)
		   '$h26_0'(x,y) : [ x + 3 * y + 1, x + y + 1, x + y + 1 ]]).
%    (for split quasireductive, same as above)


/* ordering of the literals is ms for
/*	n==m = m==n */
action(status(ms),equation([],['==-bool-elem-elem'(@'n-elem',@'m-elem')='==-bool-elem-elem'(@'m-elem',@'n-elem')])).

/* ordering of the literals is ms for
/*	n\==m = m\==n */
action(status(ms),equation([],['\==-bool-elem-elem'(@'n-elem',@'m-elem')='\==-bool-elem-elem'(@'m-elem',@'n-elem')])).

/* declare as nonoperational :
/*	n==m = true => n = m */
action(orient(n),equation(['==-bool-elem-elem'(@'n-elem',@'m-elem')='true-bool'],[@'n-elem'= @'m-elem'])).

/* ordering of the literals is ms for
/*	n==m = true => n = m */
action(status(ms),equation(['==-bool-elem-elem'(@'n-elem',@'m-elem')='true-bool'],[@'n-elem'= @'m-elem'])).

/* declare as nonoperational :
/*	[y=<x] = [x=<y,false] */
action(orient(n),equation([],[['=<-bool-elem-elem'(@'y-elem',@'x-elem')]=['=<-bool-elem-elem'(@'x-elem',@'y-elem'),'false-bool']])).

/* ordering of the literals is ms for
/*	x=<y = false => y=<x = true */
action(status(ms),equation(['=<-bool-elem-elem'(@'x-elem',@'y-elem')='false-bool'],['=<-bool-elem-elem'(@'y-elem',@'x-elem')='true-bool'])).

/* declare as nonoperational :
/*	x=<y = true and y=<x = true => x = y */
action(orient(n),equation(['=<-bool-elem-elem'(@'x-elem',@'y-elem')='true-bool','=<-bool-elem-elem'(@'y-elem',@'x-elem')='true-bool'],[@'x-elem'= @'y-elem'])).

/* condition number 1 is the selected condition for
/*	x=<y = true and y=<x = true => x = y */
action(selectcondition(1),equation(['=<-bool-elem-elem'(@'x-elem',@'y-elem')='true-bool','=<-bool-elem-elem'(@'y-elem',@'x-elem')='true-bool'],[@'x-elem'= @'y-elem'])).

/* ordering of the literals is ms for
/*	x=<y = true and y=<x = true => x = y */
action(status(ms),equation(['=<-bool-elem-elem'(@'x-elem',@'y-elem')='true-bool','=<-bool-elem-elem'(@'y-elem',@'x-elem')='true-bool'],[@'x-elem'= @'y-elem'])).

/* declare as nonoperational :
/*	x=<y = true and y=<z = true => x=<z = true */
action(orient(n),equation(['=<-bool-elem-elem'(@'x-elem',@'y-elem')='true-bool','=<-bool-elem-elem'(@'y-elem',@'z-elem')='true-bool'],['=<-bool-elem-elem'(@'x-elem',@'z-elem')='true-bool'])).

/* condition number 1 is the selected condition for
/*	x=<y = true and y=<z = true => x=<z = true */
action(selectcondition(1),equation(['=<-bool-elem-elem'(@'x-elem',@'y-elem')='true-bool','=<-bool-elem-elem'(@'y-elem',@'z-elem')='true-bool'],['=<-bool-elem-elem'(@'x-elem',@'z-elem')='true-bool'])).

/* ordering of the literals is ms for
/*	x=<y = true and y=<z = true => x=<z = true */
action(status(ms),equation(['=<-bool-elem-elem'(@'x-elem',@'y-elem')='true-bool','=<-bool-elem-elem'(@'y-elem',@'z-elem')='true-bool'],['=<-bool-elem-elem'(@'x-elem',@'z-elem')='true-bool'])).

/* declare as nonoperational :
/*	x=<y = true and x>y = true => x = y */
action(orient(n),equation(['=<-bool-elem-elem'(@'x-elem',@'y-elem')='true-bool','>-bool-elem-elem'(@'x-elem',@'y-elem')='true-bool'],[@'x-elem'= @'y-elem'])).

/* condition number 1 is the selected condition for
/*	x=<y = true and x>y = true => x = y */
action(selectcondition(1),equation(['=<-bool-elem-elem'(@'x-elem',@'y-elem')='true-bool','>-bool-elem-elem'(@'x-elem',@'y-elem')='true-bool'],[@'x-elem'= @'y-elem'])).

/* ordering of the literals is ms for
/*	x=<y = true and x>y = true => x = y */
action(status(ms),equation(['=<-bool-elem-elem'(@'x-elem',@'y-elem')='true-bool','>-bool-elem-elem'(@'x-elem',@'y-elem')='true-bool'],[@'x-elem'= @'y-elem'])).

/* split(x,l) = l1,l2 => sort([x|l]) = append(sort(l1),[x|sort(l2)])
/* should be checked for quasi-reductivity */
action(orient(c),equation(['split-pair-elem-list'(@'x-elem',@'l-list')=',-pair-list-list'(@'l1-list',@'l2-list')],['sort-list-list'('.-list-elem-list'(@'x-elem',@'l-list'))='append-list-list-list'('sort-list-list'(@'l1-list'),'.-list-elem-list'(@'x-elem','sort-list-list'(@'l2-list')))])).

/* annotation of literals is [l,l] in
/*	split(x,l) = l1,l2 => sort([x|l]) = append(sort(l1),[x|sort(l2)]) */
action(annotation([l,l]),equation(['split-pair-elem-list'(@'x-elem',@'l-list')=',-pair-list-list'(@'l1-list',@'l2-list')],['sort-list-list'('.-list-elem-list'(@'x-elem',@'l-list'))='append-list-list-list'('sort-list-list'(@'l1-list'),'.-list-elem-list'(@'x-elem','sort-list-list'(@'l2-list')))])).

/* y=<x = true and split(x,l) = l1,l2 => split(x,[y|l]) = [y|l1],l2
/* should be checked for quasi-reductivity */
action(orient(c),equation(['=<-bool-elem-elem'(@'y-elem',@'x-elem')='true-bool','split-pair-elem-list'(@'x-elem',@'l-list')=',-pair-list-list'(@'l1-list',@'l2-list')],['split-pair-elem-list'(@'x-elem','.-list-elem-list'(@'y-elem',@'l-list'))=',-pair-list-list'('.-list-elem-list'(@'y-elem',@'l1-list'),@'l2-list')])).

/* annotation of literals is [l,l,l] in
/*	y=<x = true and split(x,l) = l1,l2 => split(x,[y|l]) = [y|l1],l2 */
action(annotation([l,l,l]),equation(['=<-bool-elem-elem'(@'y-elem',@'x-elem')='true-bool','split-pair-elem-list'(@'x-elem',@'l-list')=',-pair-list-list'(@'l1-list',@'l2-list')],['split-pair-elem-list'(@'x-elem','.-list-elem-list'(@'y-elem',@'l-list'))=',-pair-list-list'('.-list-elem-list'(@'y-elem',@'l1-list'),@'l2-list')])).

/* y>x = true and split(x,l) = l1,l2 => split(x,[y|l]) = l1,[y|l2]
/* should be checked for quasi-reductivity */
action(orient(c),equation(['>-bool-elem-elem'(@'y-elem',@'x-elem')='true-bool','split-pair-elem-list'(@'x-elem',@'l-list')=',-pair-list-list'(@'l1-list',@'l2-list')],['split-pair-elem-list'(@'x-elem','.-list-elem-list'(@'y-elem',@'l-list'))=',-pair-list-list'(@'l1-list','.-list-elem-list'(@'y-elem',@'l2-list'))])).

/* annotation of literals is [l,l,l] in
/*	y>x = true and split(x,l) = l1,l2 => split(x,[y|l]) = l1,[y|l2] */
action(annotation([l,l,l]),equation(['>-bool-elem-elem'(@'y-elem',@'x-elem')='true-bool','split-pair-elem-list'(@'x-elem',@'l-list')=',-pair-list-list'(@'l1-list',@'l2-list')],['split-pair-elem-list'(@'x-elem','.-list-elem-list'(@'y-elem',@'l-list'))=',-pair-list-list'(@'l1-list','.-list-elem-list'(@'y-elem',@'l2-list'))])).

/* declare as nonoperational :
/*	l1,l2 = l4,l3 and split(x1,l5) = l1,l2 => append(sort(l1),[x1|sort(l2)]) = append(sort(l4),[x1|sort(l3)]) */
action(orient(n),equation([',-pair-list-list'(@'l1-list',@'l2-list')=',-pair-list-list'(@'l4-list',@'l3-list'),'split-pair-elem-list'(@'x1-elem',@'l5-list')=',-pair-list-list'(@'l1-list',@'l2-list')],['append-list-list-list'('sort-list-list'(@'l1-list'),'.-list-elem-list'(@'x1-elem','sort-list-list'(@'l2-list')))='append-list-list-list'('sort-list-list'(@'l4-list'),'.-list-elem-list'(@'x1-elem','sort-list-list'(@'l3-list')))])).

/* condition number 1 is the selected condition for
/*	l1,l2 = l4,l3 and split(x1,l5) = l1,l2 => append(sort(l1),[x1|sort(l2)]) = append(sort(l4),[x1|sort(l3)]) */
action(selectcondition(1),equation([',-pair-list-list'(@'l1-list',@'l2-list')=',-pair-list-list'(@'l4-list',@'l3-list'),'split-pair-elem-list'(@'x1-elem',@'l5-list')=',-pair-list-list'(@'l1-list',@'l2-list')],['append-list-list-list'('sort-list-list'(@'l1-list'),'.-list-elem-list'(@'x1-elem','sort-list-list'(@'l2-list')))='append-list-list-list'('sort-list-list'(@'l4-list'),'.-list-elem-list'(@'x1-elem','sort-list-list'(@'l3-list')))])).

/* declare as nonoperational :
/*	l1,l2 = l4,l3 and y1=<x1 = true and split(x1,l5) = l1,l2 => [y1|l1],l2 = [y1|l4],l3 */
action(orient(n),equation([',-pair-list-list'(@'l1-list',@'l2-list')=',-pair-list-list'(@'l4-list',@'l3-list'),'=<-bool-elem-elem'(@'y1-elem',@'x1-elem')='true-bool','split-pair-elem-list'(@'x1-elem',@'l5-list')=',-pair-list-list'(@'l1-list',@'l2-list')],[',-pair-list-list'('.-list-elem-list'(@'y1-elem',@'l1-list'),@'l2-list')=',-pair-list-list'('.-list-elem-list'(@'y1-elem',@'l4-list'),@'l3-list')])).

/* condition number 1 is the selected condition for
/*	l1,l2 = l4,l3 and y1=<x1 = true and split(x1,l5) = l1,l2 => [y1|l1],l2 = [y1|l4],l3 */
action(selectcondition(1),equation([',-pair-list-list'(@'l1-list',@'l2-list')=',-pair-list-list'(@'l4-list',@'l3-list'),'=<-bool-elem-elem'(@'y1-elem',@'x1-elem')='true-bool','split-pair-elem-list'(@'x1-elem',@'l5-list')=',-pair-list-list'(@'l1-list',@'l2-list')],[',-pair-list-list'('.-list-elem-list'(@'y1-elem',@'l1-list'),@'l2-list')=',-pair-list-list'('.-list-elem-list'(@'y1-elem',@'l4-list'),@'l3-list')])).

/* declare as nonoperational :
/*	l1,l2 = l4,l3 and y1>x1 = true and split(x1,l5) = l1,l2 => l1,[y1|l2] = l4,[y1|l3] */
action(orient(n),equation([',-pair-list-list'(@'l1-list',@'l2-list')=',-pair-list-list'(@'l4-list',@'l3-list'),'>-bool-elem-elem'(@'y1-elem',@'x1-elem')='true-bool','split-pair-elem-list'(@'x1-elem',@'l5-list')=',-pair-list-list'(@'l1-list',@'l2-list')],[',-pair-list-list'(@'l1-list','.-list-elem-list'(@'y1-elem',@'l2-list'))=',-pair-list-list'(@'l4-list','.-list-elem-list'(@'y1-elem',@'l3-list'))])).

/* condition number 1 is the selected condition for
/*	l1,l2 = l4,l3 and y1>x1 = true and split(x1,l5) = l1,l2 => l1,[y1|l2] = l4,[y1|l3] */
action(selectcondition(1),equation([',-pair-list-list'(@'l1-list',@'l2-list')=',-pair-list-list'(@'l4-list',@'l3-list'),'>-bool-elem-elem'(@'y1-elem',@'x1-elem')='true-bool','split-pair-elem-list'(@'x1-elem',@'l5-list')=',-pair-list-list'(@'l1-list',@'l2-list')],[',-pair-list-list'(@'l1-list','.-list-elem-list'(@'y1-elem',@'l2-list'))=',-pair-list-list'(@'l4-list','.-list-elem-list'(@'y1-elem',@'l3-list'))])).
