/*
	Include file for YACC grammar for the linear form of a Conceptual Graph.

	Written by Maurice Pagnucco , Oct 1990

		Basser Department of Computer Science
		Madsen Building, F09
		University of Sydney
		NSW, 2006
		AUSTRALIA

		email: morri@cs.su.oz.au
*/

#include	<stdio.h>

/*
	manifest sizes of
	various structures.
*/
#define	NAMESIZE	20
#define	SYMSIZE		257
#define	TOKENSIZE	100
#define	REFSIZE		20

int 	Gid, Nid;	/*Graph id number and node id numbers - may be useful*/
			/*I have assigned node id's but don't make use of them
			  I haven't mad use of graph id's at all here
			  but they can be used, for example, to associate
			  nested subgraphs to the concept node in which
			  they are nested*/

int	CLevel;		/*Current context level - i.e., level of nesting*/

enum	NodeType	{CONC, REL};

/*
	graph data structure
*/

typedef struct ENode{
	struct ENode	*eptr;
	struct GNode	*nptr;
	} ENode;
#define	E	((ENode*)0)

typedef	struct GNode{
	char		name[NAMESIZE+1];
	char		ref[REFSIZE];
	int		nid;
	enum NodeType	type;
	struct GNode	*next;
	struct ENode	*eptr;
	} GNode;
#define	G	((GNode*)0)

GNode	*HeadNode;

/*
	symbol table structure.
	an empty slot has name[0] == 0.
*/
typedef struct Sym
{
	char	name[NAMESIZE+1];
	short	type;
} Sym;
#define	S	((Sym*)0)
Sym	symspace[SYMSIZE];

/*
	stack data structure for temporarily storing nodes
*/
typedef	struct GList{
	GNode	*elmnt;
	struct GList	*next;
	} GList;
#define	GL	((GList*)0)
GList	*Hd;

/*
	stack data structure used to save data while loading contexts
*/
typedef	struct CList{
	GNode	*elmnt;
	int	x, y, id;
	struct	CList	*next;
	} CList;
#define	CL	((CList*)0)
CList	*Chd;

/*
	stack data structure for temporarily storing concept/relation node pairs
	if the orientation of the connector between them cannot be resolved immediately
*/

typedef	struct TList{
	GNode	*rptr;
	GNode	*cptr;
	struct	TList	*next;
	} TList;

#define	TL	((TList*)0)
TList	*Thd;

/*
	lexical token buffer.
	collection of raw NAMEs and NUMBs.
*/
char	tokenbuf[TOKENSIZE];
int	peekc;	/*lookahead character*/

/*
	random variables
*/
char	**xargv;
int	xargc;
int	lineno;
FILE	*iobuf;

/*
	typed functions
*/
Sym	*lookup();
double	atof();
char	*memset();
char	*strcpy();
double	pow();
double	sqrt();
void	itoa();
void	reverse();

GNode	*new();
GNode	*LPop();
GNode	*LTop();
GNode	*CPop();
int	LPush();
int	CPush();
int	TPush();
int	TPop();
int	connectnodes();
int	resolve_conflicts();
int	PrintGraph();
int	PrintNodeList();
