/*
	Grammar for the linear form of a Conceptual Graph.
	This file contains the source code for the
		lexical analyser
		symbol table access routines
		creating the graph data structure
		plus other routines required

        Written by Maurice Pagnucco , Oct 1990

                Basser Department of Computer Science
                Madsen Building, F09
                University of Sydney
                NSW, 2006
                AUSTRALIA

                email: morri@cs.su.oz.au
*/

#include	"cog.h"
#include	"y.tab.h"
#include	<ctype.h>

/*
	Can use the following function if the parser is to be called
	from elsewhere.
*/

/*
void parse(char* filename)
{
	init();

	printf("file %s:\n", filename);
	iobuf = fopen(filename, "r");
	if(iobuf == NULL) 
		printf("cannot open %s\n", filename);
	else
		yyparse();
	return;
}
*/

main(argc, argv)
char *argv[];
{

	xargc = argc;
	xargv = argv;
	init();
	/*PrintSym*/
	if(!nextio())
		iobuf = stdin;
	yyparse();
	exit(0);
}

/*
* open next file in sequence.
*/
nextio()
{

loop:
	lineno = 1;
	if(xargc <= 1)
		return 0;
	xargc--;
	xargv++;
	printf("file %s:\n", *xargv);
	iobuf = fopen(*xargv, "r");
	if(iobuf == NULL) {
		printf("cannot open %s\n", *xargv);
		goto loop;
	}
	return 1;
}


/*
yacc-called lexical routine.
1. remove white space
2. remove / * ... * / comments
3. lookup NAMEs
4. evaluate NUMBERs
5. anything else as a character.
*/
yylex()
{
	register c;
	register char *p;

loop:
	/*
	 * pick up leftover character
	 */
	c = peekc;
	if(c == 0)
		c = fgetc(iobuf);
	peekc = 0;

	/*
	 skip white space
	 count lines
	*/
	while(isspace(c)) {
		if(c == '\n') {
			lineno++;
			return(c);
		}
		c = fgetc(iobuf);
	}

	/*
	NAME is alpha(alpha|digit)*
	*/
	if(isalpha(c)) {
		p = tokenbuf;
		for(;;) {
			*p++ = c;
			c = fgetc(iobuf);
			if(!isalpha(c) && !isdigit(c))
				break;
		}
		*p = 0;
		peekc = c;
		yylval.sym = lookup(tokenbuf);
		return yylval.sym->type;
	}

	/*
	a NUMBER is
	digit*[. digit*]
	*/
	if(isdigit(c)) {
		p = tokenbuf;
		while(isdigit(c) || c == '.') {
			*p++ = c;
			c = fgetc(iobuf);
		}
		*p = 0;
		peekc = c;
		yylval.numb = atof(tokenbuf);
		return NUMBER;
	}
	/*
	comments / * ... * /
	*/
	if(c == '/') {
		c = fgetc(iobuf);
		if(c != '*') {
			peekc = c;
			yylval.token = '/';
			return '/';
		}
		for(;;) {
			c = fgetc(iobuf);
			while(c == '*') {
				c = fgetc(iobuf);
				if(c == '/')
					goto loop;
			}
			if(c == EOF) {
				yyerror("EOF in a comment");
				break;
			}
			if(c == '\n')
				lineno++;
		}
	}
	/*
		\ - allows carriage returns in the middle of a graph
	*/
	if (c == '\\'){
		c = fgetc(iobuf);
		if (c == '\n'){
			lineno++;
			peekc = 0;
			goto loop;
		}
		else {
			peekc = c;
			c = '\\';
		}
	}

	/*
	otherwise,
	it represents itself
	*/
	if(c == EOF) {
		fclose(iobuf);
		peekc = 0;
#ifdef	DEBUG
		printf("yylex -> EOF\n");
#endif
		if (!nextio())
			return EOF;
		else
			goto loop;
	}

#ifdef DEBUG
	printf("yylex-> %c\n",c);
#endif

	yylval.token = c;
	return c;
}

/*
diagnostics
*/
yyerror(s)
char *s;
{

	printf("%d: %s\n", lineno, s);
#ifdef	DEBUG
	PrintSym();
#endif
	exit(0);
}

/*
variable initialization.
called before every independent compilation.
*/
init()
{
	register Sym *s;

	HeadNode = G;
	Hd = GL;
	Chd = CL;
	Thd = TL;
	CLevel = 0;
	Nid = 0;
	Gid = 0;

	for(s = symspace; s < symspace+SYMSIZE; s++)
		s->name[0] = 0;
	lookup("type")->type = TYPE;
	lookup("relation")->type = RELATION;
	lookup("individual")->type = INDIVIDUAL;
	lookup("prototype")->type = PROTOTYPE;
	lookup("schema")->type = SCHEMA;
	lookup("subtype")->type = SUBTYPE;
	lookup("is")->type = IS;
	lookup("for")->type = FOR;
	lookup("of")->type = OF;
	/*lookup("PROPOSITION")->type = PROPOSITION;*/
	lookup("dist")->type = DIST;
	lookup("resp")->type = RESP;
}

/*
symbol table lookup.
starts with a hash probe into symbol
table and then linear to resolve
collisions.
*/
Sym*
lookup(s)
register char *s;
{
	register h;
	register Sym *p;
	register char *s1;

loop:
	h = strlen(s);
	if(h > NAMESIZE) {
		printf("symbol %s: truncated to ", s);
		s[NAMESIZE] = 0;
		printf("%s\n", s);
	}
	h = 0;
	for(s1=s; *s1;)
		h = h*71 + *s1++;
	if(h < 0)
		h = ~h;
	p = symspace + (h % SYMSIZE);
	for(h = SYMSIZE; h; h--) {
		if(p->name[0] == 0) {
			strcpy(p->name, s);
			p->type = NAME;
			return p;
		}
		if(strcmp(s, p->name) == 0)
			return p;
		p++;
		if(p >= symspace + SYMSIZE)
			p = symspace;
	}

	yyerror("symbol space exhausted");

	for(p = symspace; p < symspace+SYMSIZE; p++)
		p->name[0] = 0;
	goto loop;
}

/*
create a new node and add it to the graph data structure
*/

GNode *new(t, n, r)
enum NodeType	t;
char		*n, *r;
{
	GNode	*findref();
	GNode	*temp;
	int	i;

	if ( !(*r == '*') || !(temp=findref(r)))
	{
#ifdef	DEBUG
		printf("new: %s %s", n, r);
		if (t == CONC)
			printf("concept node\n");
		else
			printf("relation node\n");
#endif

		temp = (GNode *)malloc(sizeof(GNode));
		temp->type=t;
		temp->nid = ++Nid;
		temp->next=G;
		temp->eptr=E;
		strcpy(temp->name, n);
		strcpy(temp->ref, r);

		temp->next = HeadNode;
		HeadNode = temp;
	}
	return(temp);
}

/*
connect two nodes in direction dir (specified by `<` or `>` character
Can actually dispense with dir and have the ordering of the parameters
imply a direction.
*/

connectnodes(a, b, dir)
GNode	*a, *b;
int	dir;
{
	switch(dir) {
	case '>':
		insert_edge(a, b);
		printf("Connect %s to %s\n",a->name, b->name);
		break;
	case '<':
		insert_edge(b, a);
		printf("Connect %s to %s\n",b->name, a->name);
		break;
	default:
		printf("Hey, what's going on?\n");
		break;
	}

}

/*
insert an edge from node n to node e
*/

insert_edge(n, e)
GNode	*n, *e;
{
	GNode	*tptr;
	ENode	*teptr, *tmp_node;

	tmp_node = (ENode *)malloc(sizeof(ENode)); /* reseerve space for edge */
	tmp_node->nptr = e;

	/* find node n in node list */
	for(tptr = HeadNode; tptr != n; tptr = tptr->next)
		;

	/* insert e at front of edge list */
	tmp_node->eptr = tptr->eptr;
	tptr->eptr = tmp_node;
}

/*
stack manipulation routines
about three stacks are used
1. temporarily hold nodes when a `-` is encountered
   so we know what node to connect the nodes after the newline to.

2. temporarily hold a graphs while a nested subgraph(s) is
   being processed

3. temporarily store concept/relation node pairs if the direction
   of the arc between them cannot be determined
*/
LPush(e)
GNode	*e;
{
	GList	*tmp;

	tmp = (GList *)malloc(sizeof(GList));
	tmp->elmnt = e;
	tmp->next = Hd;
	Hd = tmp;
}

GNode	*LPop()
{
	GNode	*tmp1;
	GList	*tmp2;

	tmp1 = Hd->elmnt;
	tmp2 = Hd;
	Hd = Hd->next;
	free(tmp2);
	return(tmp1);
}

GNode	*LTop()
{
	return(Hd->elmnt);
}

CPush(e)
GNode	*e;
{
	CList*	tmp;

	tmp = (CList*) malloc(sizeof(CList));

	tmp->elmnt = e;
	tmp->next = Chd;
	Chd = tmp;
}

GNode	*CPop()
{
	GNode	*tmp1;
	CList	*tmp2;

	Gid = Chd->id;
	tmp1 = Chd->elmnt;
	tmp2 = Chd;
	Chd = Chd->next;
	free(tmp2);
	return(tmp1);
}

TPush(c, r)
GNode	*c;
GNode	*r;
{
	TList	*tmp;

	tmp = (TList*) malloc(sizeof(TList));
	tmp->cptr = c;
	tmp->rptr = r;
	tmp->next = Thd;
	Thd = tmp;
#ifdef	DEBUG
	printf("Push:\n");
	printf("Pushing %s and %s\n", tmp->cptr->name, tmp->rptr->name);
#endif
}

TPop()
{
	TList	*tmp;

	tmp = Thd;
	Thd = Thd->next;
	free(tmp);
}

/*
resolve any conflicts that may remain
for connecting a relation to a concept
(all remaining concept/relation node pairs will be held in a stack)
this should actually be more involved and take into account
the numbering of the arcs (where provided)
*/

resolve_conflicts()
{
	while (Thd)
	{
		printf("resolving conflict: %s node and %s\n", Thd->rptr->name, Thd->cptr->name);
		if (Thd->rptr->eptr)
			connectnodes(Thd->cptr, Thd->rptr, '>');
		else
			connectnodes(Thd->rptr, Thd->cptr, '>');
		TPop();
	}
}

/*
	find a node with typelabel name
*/

GNode	*findnode(name)
char	*name;
{
	GNode	*tptr;

	for(tptr=HeadNode; (tptr != G) && strcmp(name, tptr->name); tptr = tptr->next)
		;
	return tptr;
}

/*
find a node with referent ref
*/

GNode	*findref(ref)
char	*ref;
{
	GNode	*tptr;

	for(tptr=HeadNode; (tptr != G) && strcmp(ref, tptr->ref); tptr = tptr->next)
		;
	return tptr;
}

/*****************/

/*
print out the current graph data structure
*/

PrintGraph(ghd)
GNode	*ghd;
{
	GNode	*tptr;
	ENode	*teptr;

	for(tptr = ghd; tptr != G; tptr = tptr->next)
	{
		printf("%s has edges to:\n",tptr->name);
		for(teptr = tptr->eptr; teptr != E; teptr = teptr->eptr)
			printf("\t %s\n",teptr->nptr->name);
			printf("\n");
	}
printf("End of graph reached\n");
}

/*
print a list of the currently existing nodes
*/

PrintNodeList(head)
GNode	*head;
{
	GNode *tmp_ptr;

	printf("Printing list of node\n");
	for(tmp_ptr = head; tmp_ptr != G; tmp_ptr = tmp_ptr->next)
	{
		printf("Current node is: %s\n",tmp_ptr->name);
	}
}

/*
print the values currently held in the symbol table
*/

PrintSym()
{
	register Sym	*s;

	for(s = symspace;s < symspace + SYMSIZE;s++)
		if(s->name[0] != 0)
			printf("Entry: %s\n",s->name);
}

/*
deallocate memory used to store an edge list
*/

edgedealloc(e)
ENode	*e;
{
	if (!e)
		return;
	edgedealloc(e->eptr);
	free(e);
}

/*
deallocate memory used to store a graph
*/

memdealloc(n)
GNode	*n;
{
	if (!n)
		return;
	memdealloc(n->next);
	edgedealloc(n->eptr);
	free(n);
}

/*
utility routines
*/

void	itoa(n, s)
int	n;
char	*s;
{
	int	i, sign;

	if ((sign = n) < 0)
		n = -n;
	i = 0;
	do {
		s[i++] = n% 10 + '0';
	} while ((n /= 10) > 0);
	if (sign < 0)
		s[i++] = '-';
	s[i] = '\0';
	reverse(s);
}

void	reverse(s)
char	*s;
{
	int c, i, j;

	for (i = 0, j = strlen(s) - 1; i < j; i++, j--) {
		c = s[i];
		s[i] = s[j];
		s[j] = c;
	}
}
