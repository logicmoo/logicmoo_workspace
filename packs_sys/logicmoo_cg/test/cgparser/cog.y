/*
	Grammar for the linear form of a Conceptual Graph.
 	See Appendix A6 plus Chapters 3 & 4 of John Sowa's
	book : "Conceptual Structures - Information Processing
	for Mind and Machine", Addison Wesley, 1984.

	Written by Maurice Pagnucco , Oct 1990
		
		Basser Department of Computer Science
		Madsen Building, F09
		University of Sydney
		NSW, 2006
		AUSTRALIA

		email: morri@cs.su.oz.au
*/

%{
#include	"cog.h"
%}
%union	{
	Sym	*sym;
	GNode	*gnode;
	float	numb;
	int	token;
	char	name[NAMESIZE];
}
%token	<sym>	TYPE RELATION INDIVIDUAL SCHEMA SUBTYPE IS FOR OF
%token	<sym>	PROTOTYPE DIST RESP
%token	<numb>	NUMBER
%token	<sym>	NAME
%token	<token>	';' ',' '(' ')' '-' '[' ']' '<' '>' '"' '*' '$' '#' '@' '{' '}' '|' '\n' '.'

%type	<gnode>	concept relation abdef
%type	<gnode>	cgraph glist cg element file
%type	<token>	arc
%type	<token> rlink conlink rlist conlist
%type	<name>	typelabel typefield measure nmarker gmarker reffield stype varlist

%%
file		:	element sep
		|	file sep element sep
		;

sep		: 	sep '\n'
		|
		;

element		:	cg '.'
		|	cg ';'
		;

cg		:	cgraph

		|	stype
			{
				$$ = (GNode*)0;
			}
		|	abdef
		;

abdef		:	TYPE NAME '(' NAME ')' IS '\n'
			{
				printf("Concept type definition for %s\n", $2->name);
			}
			cgraph
			{
				$$ = $9;
			}
		|	RELATION NAME '(' varlist ')' IS '\n'
			{
				printf("Relation type definition for %s\n", $2->name);
			}
			cgraph
			{
				$$ = $9;
			}
		|	INDIVIDUAL NAME '(' NAME ')' IS '\n'
			{
				printf("Individual definition for %s\n", $2->name);
			}
			cgraph
			{
				$$ = $9;
			}
		|	PROTOTYPE FOR NAME '(' NAME ')' IS '\n'
			{
				printf("Prototype definition for %s\n", $2->name);
			}
			cgraph
			{
				$$ = $10;
			}
		|	SCHEMA FOR NAME '(' NAME ')' IS '\n'
			{
				printf("Schema definition for %s\n", $2->name);
			}
			cgraph
			{
				$$ = $10;
			}
		;
		 /* for each of these abstract definitions we should
		 ensure that each VAR appears in cgraph	*/

cgraph		:	concept
			{
				resolve_conflicts();
				/*PrintNodeList(HeadNode);*/
				PrintGraph(HeadNode);
				memdealloc(HeadNode);
				HeadNode = G;
			}
		|	concept
			{
				LPush($1);
			}
			rlink
			{
				resolve_conflicts();
				/*PrintNodeList(HeadNode);*/
				PrintGraph(HeadNode);
				memdealloc(HeadNode);
				HeadNode = G;
			}
		|	relation
			{
				LPush($1);
			}
			conlink
			{
				resolve_conflicts();
				/*PrintNodeList(HeadNode);*/
				PrintGraph(HeadNode);
				memdealloc(HeadNode);
				HeadNode = G;
			}
		;

rlink		:	arc relation
			{
				connectnodes(LPop(), $2, $1);
			}
		|	arc relation
			{
				connectnodes(LPop(), $2, $1);
				LPush($2);
			}
			conlink
		|	'-' rlist ','
			{
				LPop();
			}
		;

conlink		:	arc concept
			{
				connectnodes(LPop(), $2, $1);
			}
		|	arc concept
			{
				connectnodes(LPop(), $2, $1);
				LPush($2);
			}
			rlink
		|	'-' conlist ','
			{
				LPop();
			}
		;

rlist		:	'\n' relation
			{
				if ($2->eptr)
					connectnodes(LTop(), $2, '>');
				else
					TPush(LTop(), $2);
			}

		|	'\n' relation
			{
				if ($2->eptr)
					connectnodes(LTop(), $2, '>');
				else
					TPush(LTop(), $2);
				LPush($2);
			}
			conlink

		|	rlist '\n' relation
			{
				if ($3->eptr)
					connectnodes(LTop(), $3, '>');
				else
					TPush(LTop(), $3);
				LPush($3);
			}

		|	rlist '\n' relation
			{
				if ($3->eptr)
					connectnodes(LTop(), $3, '>');
				else
					TPush(LTop(), $3);
				LPush($3);
			}
			conlink
		;

conlist		:	'\n' arc concept
			{
				connectnodes(LTop(), $3, $2);
			}

		|	'\n' arc concept
			{
				connectnodes(LTop(), $3, $2);
				LPush($3);
			}
			rlink

		|	conlist
			'\n' arc concept
			{
				connectnodes(LTop(), $4, $3);
				$$=$2;
				LPush($4);
			}

		|	conlist
			'\n' arc concept
			{
				connectnodes(LTop(), $4, $3);
				LPush($4);
			}
			rlink
			{
				$$=$2;
			}
		;

concept		:	'[' typefield ']'
			{
				$$=new(CONC, $2, "");
			}
		|	'[' typefield ':' reffield ']'
			{
				$$=new(CONC, $2, $4);
			}
		|	'[' typefield ':'
			{
				GNode* tmp;

				tmp = new(CONC, $2, "");
				/* push current graph onto stack and
				   process all the nested subgraphs */
				CPush(tmp);
				CLevel++;
				HeadNode = 0;
			}
			glist ']'
			{
				CLevel--;
				HeadNode = CPop();
				$$ = HeadNode;
			}
		|	'['
			{
				GNode* tmp;

				tmp = new(CONC, "PROPOSITION", "");
				/* push current graph onto stack and
				   process all the nested subgraphs */
				CPush(tmp);
				CLevel++;
				HeadNode = 0;
			}
			glist ']'
			{
				CLevel--;
				HeadNode = CPop();
				$$ = HeadNode;
			}
		;

relation	:	'(' typelabel ')'
			{
				$$=new(REL, $2, "");
			}
		;

arc		:	'<' '-'
		|	NUMBER '<' '-'
			{
				$$=$2;
			}
		|	'-' '>'
			{
				$$=$2;
			}
		|	NUMBER '-' '>'
			{
				$$=$3;
			}
		;
		/*
			Have ignored number but it should be taken into account to
			resolve conflicts in direction of arrow in <conlist> rule.
		*/

typefield	:	NAME
			{
				strcpy($$,$1->name);
			}
		;

glist		:	cgraph
			{
				memdealloc(HeadNode);
				HeadNode = G;
			}
		|	glist cgraph
			{
				memdealloc(HeadNode);
				HeadNode = G;
			}
		;

reffield	:	NAME
			{
				strcpy($$,$1->name);
			}
		|	'*' gmarker
			{
				$$[0]='*';
				strcpy(($$)+1,$2);
			}
		|	'$' gmarker
		{
			$$[0]='$';
			strcpy(($$)+1,$2);
														}
		|	'#' nmarker
		{
			$$[0]='#';
			strcpy(($$)+1,$2);
														}
		|	NAME '#' nmarker
		{
			strcpy($$,$1);
			strcat($$,"#");
			strcat($$,$3);
														}
		|	'#' nmarker '@' measure
		{
			$$[0]='#';
			strcpy(($$)+1,$2);
			strcat($$,"@");
			strcat($$,$4);
														}
		|	'@' measure
		{
			$$[0]='@';
			strcpy(($$)+1,$2);
														}
		;
			/* check that NUMBER is the true cardinality of 
			   set; also NUMBER must be an integer */

gmarker		:	NAME
			{
				strcpy($$,$1);
			}
		|	{strcpy($$, "");}/* empty rule */
		;

nmarker		:	NUMBER	/* this number should be a positive integer */
			{
				char	tmp[10];

				itoa((int)$1, tmp);
				strcpy($$,tmp);
			}
		|	{strcpy($$, "");}/* empty rule */
		;

measure		:	NUMBER NAME
			{
				strcpy($$,$2->name);
			}
		;


varlist		:	NAME
			{
				strcpy($$, $1->name);
			}
		|	varlist ','  NAME
			{
				strcpy($$, $1);
				strcat($$, ",");
				strcat($$, $3->name);
			}
		;

typelabel	:	NAME
			{
				strcpy($$,$1->name);
			}
		;

stype		:	SUBTYPE OF NAME IS NAME
			{
				printf ("%s is a subtype of %s\n", $5->name, $3->name);
			}
		;

%%

/* end of grammar */
