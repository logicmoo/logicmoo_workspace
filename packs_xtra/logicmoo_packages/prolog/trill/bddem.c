/*
	LPAD and CP-Logic interpreter

Copyright (c) 2007, Fabrizio Riguzzi

This package uses the library cudd, see http://vlsi.colorado.edu/~fabio/CUDD/
for the relative license.


	This file contains the functions for interfacing Yap and C
	The arguments of the predicate compute_prob are parsed and translated into C data
	structures

NOTE:
1. le variabili Value e Bit in ProbPathBool (algoritmo 6) corrispondono a bits e posBvar risp. nel codice
2. Per  eta^value (Rule) userei un array di puntatori, in cui gli elementi puntano ad un'area di memoria allocata dinamicamente (con malloc) che
   contiene un array con un elemento per ogni possibile value. L'array di puntatori è lungo quante sono le regole.
3. Per sigma(var) puoi usare un array che indicizzi con il valore di var che e' un intero.Per "valore di var" intende l'indice della variabile multivalore.
4. Per e userei una hash table, dato che deve memorizzare valori in corrispondenza dei nodi che non sono in sequenza.
   Nella hash table, memorizza un puntatore ad un array con tanti elementi quanti i valori della variabile multivalore associata a  VarRootNode
5. L'indice 65535 indica un nodo foglia
*/

#include "util.h"
#include "cuddInt.h"
//#include "dddmp.h"
#include "dddmp.h"
//#include "array.h"
#include "mtr.h"
//#include "avl.h"
#include "YapInterface.h"
#include <glib.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <mcheck.h>
//#include "memwatch.h"
#define ArrayElements( array ) ( sizeof( array ) / sizeof( array[0] ) )
#define LOGZERO log(0.000001)
#define CACHE_SLOTS 1 
#define UNIQUE_SLOTS 1//8192 //con questo numero arriva al max num di esempi processati, aumentandolo cala di nuovo il num di esempi     //default:CUDD_UNIQUE_SLOTS 


typedef struct
  {
    int nVal,nRule;
    int firstBoolVar;
  } variable;


typedef struct {
  DdNode *key;
  double value;
} rowel;

typedef struct  {
  int cnt;
  rowel *row;
} tablerow;

tablerow * table;
//static int array;
static unsigned long  memuseT=0,memuse,maxmem;

static variable * vars;
static variable ** vars_ex;
static  int * bVar2mVar; //per ogni var. booleana contiene l'indice della var. multivalore nell'array vars:bVar2mVar[indice var.booleana]=ind.var.mult.
static  int ** bVar2mVar_ex;
static  double * sigma;	//sigma(V)
static  double ***eta;	//eta^value(Rule)
static  double ***eta_temp;	//eta^value(Rule)
static  double **arrayprob;
static  int *rules;	//array indicizzato dalle regole
//double *e_temp;
static DdManager *mgr;
static DdManager **mgr_ex;
static int *nVars;
static int *nVars_ex;
static int nRules;	// num. variabili multivalore;num regole
double * probs;
double * nodes_probs_ex;
double ** probs_ex;
static int * boolVars; // numero totale di variabili booleane per 1 gruppo di esempi;e' globale e viene incrementata ogni volta che viene aggiunta una variabile
static int * boolVars_ex; 
//GHashTable  * nodesB; /* hash table that associates nodes with their probability Backward if already computed, it is defined in glib */
//GHashTable  * nodesF; /* hash table that associates nodes with their probability Forward*/
tablerow * nodesB;
tablerow * nodesF;
//GHashTable  * eTable;
int ex,cycle,esempio;
DdNode *** nodesToVisit;
int * NnodesToVisit;
double * example_prob;
static int ret_prob(void);
static int expec(void);
double Prob(DdNode *node,int comp_par);
static int end_bdd(void);
static int dump_bdd(void);
static int init_test(void);

static int add_var(void);
static int init(void);
static int end(void);
static int EM(void);
static int Q(void);
double ProbPath(DdNode *node, int comp_par);

static int rec_deref(void);
int indexMvar(DdNode *node);

void Forward(DdNode *node);
void GetForward(DdNode *node, double ForwProbPath);
void UpdateForward(DdNode * node);
double GetOutsideExpe(DdNode *root,double ex_prob);
void Maximization(void);
static double Expectation(DdNode **nodes_ex, int lenNodes);
void init_my_predicates(void);
FILE *open_file(char *filename, const char *mode);
void reverse(char s[]);


tablerow* init_table(int varcnt);
double * get_value(tablerow *tab,  DdNode *node);
void add_or_replace_node(tablerow *tab, DdNode *node, double value);
void add_node(tablerow *tab, DdNode *node, double value);
void destroy_table(tablerow *tab,int varcnt);

gint my_equal(gconstpointer v,gconstpointer v2);
guint my_hash(gconstpointer key);
void  dealloc(gpointer key,gpointer value,gpointer user_data);

static int init(void)	//riceve in ingresso il numero di regole e il num. di atomi testa per ogni regola 
{
	int j,i;
	YAP_Term arg1,arg2,list;	//num.regole

	//printf("init\n");

	//nbVars=0
	//////printf("nbvar %d\n",nbVars);
	//////printf("nvar %d\n",nVars);
	ex=0;
	cycle=0;

	arg1=YAP_ARG1;
	arg2=YAP_ARG2;
	nRules=YAP_IntOfTerm(arg1);	//numero di regole

//srand ( time(NULL) );


	//Cudd_AutodynEnable(mgr,CUDD_REORDER_SAME);
  	vars_ex=NULL; //array indicizzato dalle var. multivalue
	nVars_ex=NULL;//array indicizzato dagli esempi,per ogni esempio tiene il numero delle variabili booleane
	
	//printf("nRules = %d \n",nRules);
	eta= (double ***) malloc(nRules * sizeof(double **));		//array indicizzato dalle regole
	eta_temp= (double ***) malloc(nRules * sizeof(double **));
	rules= (int *) malloc(nRules * sizeof(int));		//array indicizzato dalle regole
	arrayprob=(double **) malloc(nRules * sizeof(double *));
	probs_ex=NULL;
	bVar2mVar_ex=NULL;
	boolVars_ex=NULL;
	mgr_ex=NULL;
	nodes_probs_ex=NULL;
	/* dividend is a global variable used by my_hash
	   it is equal to an unsigned int with binary representation 11..1 */
	list=arg2; //lista di interi, rappresentati il n° di atomi nella testa, per tutte le regole
	for (j=0;j<nRules;j++)  
	{
		rules[j]=YAP_IntOfTerm(YAP_HeadOfTerm(list));
		//printf("rule %d n°heads %d\n",j,rules[j]);
		//array_insert(double,v.probabilities,i,p);
		list=YAP_TailOfTerm(list);
		eta[j]= (double **) malloc((rules[j]-1)*sizeof(double *));
		eta_temp[j]= (double **) malloc((rules[j]-1)*sizeof(double *));
		arrayprob[j]= (double *) malloc((rules[j]-1)*sizeof(double));  //alloca lo spazio per rules[r] teste (cioè parametri theta per la regola r)

		for (i=0;i<rules[j]-1;i++)
		{
			//printf("head %d\n",i);
			eta[j][i]=(double *) malloc(2*sizeof(double));
			eta_temp[j][i]=(double *) malloc(2*sizeof(double));
		}
	}

	return 1;
}
static int init_bdd(void)	
{
	DdManager **mgr_ex_temp;
	int ** bVar2mVar_ex_temp;
	variable ** vars_ex_temp;
	int *nVars_ex_temp;
	double ** probs_ex_temp;
	int * boolVars_ex_temp;

	mgr=Cudd_Init(0,0,UNIQUE_SLOTS,CACHE_SLOTS,5120/*512000=500Kb,1048576=1M,33554432=32M,4294967295=4096M,8737678950*/);
	Cudd_AutodynEnable(mgr, CUDD_REORDER_GROUP_SIFT);
	Cudd_SetMaxCacheHard(mgr, 0/*1024*1024*1024*/);
	Cudd_SetLooseUpTo(mgr, 0/*1024*1024*512*/);
//	Cudd_EnableReorderingReporting(mgr);
	Cudd_SetMinHit(mgr, 15);

	//printf("\n**************Init_bdd --> creo manager per ex=%d\n\n",ex);
	//printf("\n ex=%d \n",ex);
	//printf("mgr %d %d\n",mgr_ex,mgr);
	//printf("size %d %d\n",sizeof(mgr_ex),(ex+1)* sizeof(DdManager *));
//fflush(stdout);	
		mgr_ex_temp=(DdManager **) realloc(mgr_ex, (ex+1)* sizeof(DdManager *)); 	if (mgr_ex_temp==NULL)
	{
		printf("mgr_ex_temp==NULL\n");
	}
	else
		mgr_ex=mgr_ex_temp;
	mgr_ex[ex]=mgr;

	//printf("bvar2mvar\n");	
//fflush(stdout);	
	bVar2mVar_ex_temp=(int **) realloc(bVar2mVar_ex, (ex+1)* sizeof(int *));	if (bVar2mVar_ex_temp==NULL)
		printf("bVar2mVar_ex_temp==NULL\n");
	else
		bVar2mVar_ex=bVar2mVar_ex_temp;
	bVar2mVar_ex[ex]=NULL;
	bVar2mVar=bVar2mVar_ex[ex];

	//printf("vars\n");	
	//fflush(stdout);	
	vars_ex_temp=(variable **) realloc(vars_ex, (ex+1)* sizeof(variable *));
	if (vars_ex_temp==NULL)
		printf("vars_ex_temp==NULL\n");
	else
		vars_ex=vars_ex_temp;
	vars_ex[ex]=NULL;
	vars=vars_ex[ex];
	

	//printf("nVars\n");	
//fflush(stdout);	
	nVars_ex_temp=(int *) realloc(nVars_ex, (ex+1)* sizeof(int ));
	if (nVars_ex_temp==NULL)
		printf("nVars_ex_temp==NULL\n");
	else
		nVars_ex=nVars_ex_temp;
	nVars=nVars_ex+ex;
	*nVars=0;
//	printf("nVars %d nVars_ex[ex] %d\n",nVars,&nVars_ex[ex]);
//printf("probs_ex\n");
//fflush(stdout);	

	probs_ex_temp=(double **) realloc(probs_ex, (ex+1)* sizeof(double *)); 
	if (probs_ex_temp==NULL)
		printf("probs_ex_temp==NULL\n");
	else
		probs_ex=probs_ex_temp;
//printf("probs_ex1 %d\n",probs_ex);
	probs_ex[ex]=NULL;
	probs=probs_ex[ex];
	
//	printf("boolvars\n");	
fflush(stdout);	
	boolVars_ex_temp=(int *) realloc(boolVars_ex, (ex+1)* sizeof(int ));
	if (boolVars_ex_temp==NULL)
		printf("boolVars_ex_temp==NULL\n");
	else
		boolVars_ex=boolVars_ex_temp;
	boolVars=boolVars_ex+ex;
	*boolVars=0;
//	printf("*boolvars %d boolVars_ex[ex] %d\n",boolVars,&boolVars_ex[ex]);

	return 1;
	//out=YAP_MkIntTerm((YAP_Int) mgr); 
	//return(YAP_Unify(out,arg1));
}

static int end_bdd(void)
{

       // printf("***************end_bdd\n");
	bVar2mVar_ex[ex]=bVar2mVar;
	probs_ex[ex]=probs;
	vars_ex[ex]=vars;
//	printf("Boolvars ex %d\n",boolVars_ex[ex]);
	ex=ex+1;
        memuse=Cudd_ReadMemoryInUse(mgr);//Returns the memory in use by the manager measured in bytes.
//        printf("Memoria in uso dopo dump=%lu\n",memuse);
	memuseT=memuseT+memuse;
//	printf("Memoria dal primo esempio=%lu\n\n",memuseT);
	maxmem=Cudd_ReadMaxMemory(mgr);
//	printf("Memoria max=%lu\n\n",maxmem);
//	Cudd_PrintInfo(mgr,stdout);
	return 1;
}


static int dump_bdd(void)
{
FILE *fdump;
char fname[100],fname1[100];
int retValue1;
//unsigned long maxmem;
DdNode *node,*bdd_reloaded;
YAP_Term arg1,arg2,out;



arg1=YAP_ARG1;
arg2=YAP_ARG2;
node = (DdNode *)YAP_IntOfTerm(arg1);

strcpy(fname,"BDD_mem.txt");
strcpy(fname1,"BDD1_mem.txt");
fdump=fopen(fname,"w");

memuse=Cudd_ReadMemoryInUse(mgr);//INCREMENTO -Returns the memory in use by the manager measured in bytes.
//printf("\nMemoria in uso corrente(prima dump)=%lu\n",memuse);
/*memuseT=memuseT+memuse;
printf("Memoria dal primo esempio=%u\n\n",memuseT);i*/
/* f=fopen("bVar2mVar_mem.txt","a");
 fprintf(f,"ex=%d, BoolVars= %d\n\n",ex,*boolVars);	
 fclose(f);*/

//printf("\n NVARS= %d\t",*nVars);
//printf("NVARS_Ex= %d - ex=%d\n",nVars_ex[ex],ex);

/*for (i=0;i<*boolVars;i++)
	{  printf("posizione corrente della variabile %d nell'ordine: %d\n",i,Cudd_ReadPerm(mgr,i));}
*/

//printf("prima di bdd_store\n");
//salvataggio su file
/*
 Dddmp_cuddBddStore(
   DdManager * ddMgr, IN: DD Manager
   char * ddname, IN: DD name (or NULL)
   DdNode * f, IN: BDD root to be stored
   char ** varnames, IN: array of variable names (or NULL)
   int * auxids, IN: array of converted var ids
   int  mode, IN: storing mode selector
   Dddmp_VarInfoType  varinfo, IN: extra info for variables in text mode
   char * fname, IN: File name
   FILE * fp IN: File pointer to the store file
		   )
 */
retValue1 = Dddmp_cuddBddStore(mgr,NULL,node,NULL,NULL,DDDMP_MODE_TEXT,4,fname,fdump);
fclose(fdump);

//Deletes resources associated with a DD manager and resets the global statistical counters.
Cudd_Quit(mgr);                           
//printf("*********Quit mgr per ex=%d\n\n",ex);

//Creates a new DD manager
mgr=Cudd_Init(0,0,UNIQUE_SLOTS,CACHE_SLOTS,512000/*=500K,1048576=1M,33554432=32M,4294967295=4096M,8737678950*/);
	Cudd_AutodynEnable(mgr, CUDD_REORDER_GROUP_SIFT);
	Cudd_SetMaxCacheHard(mgr,0/*1024*1024*1024*/);
	Cudd_SetLooseUpTo(mgr, 0/*1024*1024*512*/);
//	Cudd_EnableReorderingReporting(mgr);
//	Cudd_SetMinHit(mgr, 15);

mgr_ex[ex]=mgr;
//printf("**********Init new mgr \n");
/*for (i=0;i<*boolVars;i++)
	{  printf("posizione corrente della variabile %d nell'ordine: %d\n",i,Cudd_ReadPerm(mgr,i));}
*/

fdump=fopen(fname,"r");

/*
 DdNode * Dddmp_cuddBddLoad(
  DdManager * ddMgr, IN: DD Manager
  Dddmp_VarMatchType  varMatchMode, IN: storing mode selector
  char ** varmatchnames, IN: array of variable names - by IDs
  int * varmatchauxids, IN: array of variable auxids - by IDs
  int * varcomposeids, IN: array of new ids accessed - by IDs
  int  mode, IN: requested input file format
  char * file, IN: file name
  FILE * fp IN: file pointer
)
Retrieve the BDD root*/
bdd_reloaded= Dddmp_cuddBddLoad(mgr,DDDMP_VAR_MATCHIDS,NULL,NULL,NULL,DDDMP_MODE_TEXT,fname,fdump);
//printf("dopo bdd_load,bdd=%d\n",(int)bdd_reloaded);
fclose(fdump);

//Cudd_PrintInfo(mgr,stdout);

/*for (j=0; j<*boolVars; j++)
{
  printf("\nj = %d \n",j);
   bVarIndex=Cudd_ReadInvPerm(mgr,j);
   printf("bVarIndex=%d\n",bVarIndex);
   if (bVarIndex==-1)  
   {
   	fprintf(f,"\nbVarIndex=%d esempio %d\n",bVarIndex,esempio);
	bVarIndex=j;
	}
   mVarIndex=bVar2mVar[bVarIndex];
   printf("mVarIndex=%d\n",mVarIndex);
} */

/*fdump=fopen(fname1,"w");
Dddmp_cuddBddStore(mgr,NULL,bdd_reloaded,NULL,NULL,DDDMP_MODE_TEXT,4,fname1,fdump);
fclose(fdump);
*/

out=YAP_MkIntTerm((YAP_Int)bdd_reloaded);
//printf("DUMP per ex=%d\n",ex);
//maxmem=Cudd_ReadMaxMemory(mgr);
//memuse=Cudd_ReadMemoryInUse(mgr);
//printf("maxmem=%u memuse=%u\n\n",maxmem,memuse);
//Cudd_PrintInfo(mgr,stdout);
return(YAP_Unify(out,arg2));

}

static int init_test(void)	//riceve in ingresso il numero di regole e la lista di probabilità
{
	YAP_Term arg1;	//num.regole

	
	//printf("init\n");

	//nbVars=0
	//////printf("nbvar %d\n",nbVars);
	//////printf("nvar %d\n",nVars);
	//printf("nRules = %d \n",nRules);
	/* dividend is a global variable used by my_hash
	   it is equal to an unsigned int with binary representation 11..1 */
	////printf("letta lista prob.\n");
	arg1=YAP_ARG1;
	nRules=YAP_IntOfTerm(arg1);	//numero di regole


	mgr=Cudd_Init(0,0,UNIQUE_SLOTS,CACHE_SLOTS,0);
	Cudd_AutodynEnable(mgr, CUDD_REORDER_GROUP_SIFT);
	Cudd_SetMaxCacheHard(mgr, 1024*1024*1024);
	Cudd_SetLooseUpTo(mgr, 1024*1024*512);
//	Cudd_EnableReorderingReporting(mgr);
	rules= (int *) malloc(nRules * sizeof(int));	
	
	bVar2mVar=NULL;
	probs=NULL;	
	vars=NULL;
	
	nVars=(int *) malloc(sizeof(int ));
	*nVars=0;
	
	boolVars=(int *) malloc(sizeof(int ));
	*boolVars=0;
	
	return 1;
	//out=YAP_MkIntTerm((YAP_Int) mgr); 
	//return(YAP_Unify(out,arg1));
}

static int end_test(void)
{
	//Cudd_PrintInfo(mgr,stdout);

	//printf("end_bdd\n");
	free(bVar2mVar);
	free(vars);
	free(nVars);
	free(boolVars);
	Cudd_Quit(mgr);
	free(probs);
	free(rules);
	//free(eta);
	return 1;
}



static double Expectation(DdNode **nodes_ex,int lenNodes)
{
	int i;
	double rootProb,CLL=0;
	
//	printf("\n EXPECTATION \n");

//  printf("nVars %d\n",*nVars);
/*	for(i=0;i<lenNodes;i++)		
	{
	//printf("nodes[%d]=%x \n",i,(unsigned int)nodes_ex[i]);
	}
	////printf("prova\n");
*/
	//out=YAP_MkFloatTerm(ProbPath(node)); //output; 
	
//	printf("Lunghezza array lenNodes: %d\n\n",lenNodes);
  for(i=0;i<lenNodes;i++)	
    {
    	if (!Cudd_IsConstant(nodes_ex[i]))
	{
//        printf("\n\n\nEsempio i=%d boolvars %d vars %d\n\n",i,*boolVars,Cudd_ReadSize(mgr_ex[i]));
	esempio=i;
  //      nodesB=g_hash_table_new_full(NULL,NULL,NULL,free);
//	printf("b\n");
//	nodesF=g_hash_table_new_full(NULL,NULL,NULL,free);
	////printf("F\n");
//	eTable=g_hash_table_new(my_hash,my_equal);
//	printf("prima di mgr\n");
	mgr=mgr_ex[i];
//	printf("dopo mgr\n");
	probs=probs_ex[i];
//	printf("probs\n");
	boolVars=boolVars_ex+i;
  	nodesB=init_table(*boolVars);
  	nodesF=init_table(*boolVars);
///	if (*boolVars!=0)
///	printf("boolVars %d\n",*boolVars);
	bVar2mVar=bVar2mVar_ex[i];
//	printf("bVar2mVar %x\n",(int)bVar2mVar);
	vars=vars_ex[i];
  //printf(" \n nodes[%d]=%x \n",i,(unsigned int)nodes_ex[i]);
  //printf("bv %d\n",*boolVars);
		//scanf("%d",&in);
	Forward(nodes_ex[i]);
        //printf("dopo Forward\n");
	//fflush(stdout);

	rootProb=GetOutsideExpe(nodes_ex[i],example_prob[i]);
	//printf("ex %i rootProb %f \n ",i,rootProb);
	//printf("CLL prima di agg. = %f \n",CLL);
		if (rootProb<=0.0)
		{printf("ex %d neg prob %lf\n",i,rootProb);
		CLL = CLL + LOGZERO*example_prob[i];
		}
		else
		CLL = CLL + log(rootProb)*example_prob[i];
	//log=logaritmo naturale in base e
	nodes_probs_ex[i]=rootProb;
//	printf("CLL dopo agg. = %f \n\n",CLL);
	//scanf("%d",&in);

	////printf("nodes[%d]=%x \n",i,(unsigned int)nodes_ex[i]);
	//g_hash_table_foreach(nodesB,dealloc,NULL);
	//g_hash_table_foreach(nodesF,dealloc,NULL);
//	g_hash_table_foreach(eTable,dealloc,NULL);
	destroy_table(nodesB,*boolVars);
	destroy_table(nodesF,*boolVars);
	}
//	g_hash_table_destroy(nodesB);
//	g_hash_table_destroy(nodesF);}
	else
	{
		//printf("logzero %d node %d\n",Cudd_ReadLogicZero(mgr_ex[i]),nodes_ex[i]);
		if (nodes_ex[i]==Cudd_ReadLogicZero(mgr_ex[i]))
	{		
	//printf("logzero\n");
	CLL=CLL+LOGZERO*example_prob[i];
	}
	}
}
//	g_hash_table_destroy(eTable);}

/*	}else
	printf("0 boolvars\n");
*/
//printf("\n FINE EXPECTATION \n");
	return CLL;
}
static int end(void)
{
	int r,i;

//	printf("end ex %d\n",ex);
	for (i=0;i<ex;i++)
	{
	//	printf("i %d mgr_ex %d\n",i,mgr_ex[i]);
		//Cudd_PrintInfo(mgr_ex[i],stdout);
		Cudd_Quit(mgr_ex[i]);
	//	printf("bVar\n");
		free(bVar2mVar_ex[i]);
	//	printf("vars_ex\n");
		free(vars_ex[i]);
	//	printf("probs_ex\n");
		free(probs_ex[i]);
	fflush(stdout);
	}
	
//	printf("free\n");
	free(mgr_ex);
	free(bVar2mVar_ex);
	free(vars_ex);
	free(probs_ex);
	free(nVars_ex);
	free(boolVars_ex);
	free(nodes_probs_ex);
	for (r=0;r<nRules;r++)
        {
		for (i=0;i<rules[r]-1;i++)
		{
	//		printf("r %d i %d \n",r,i);
			free(eta[r][i]);
			free(eta_temp[r][i]);
		}
		free(eta[r]);
		free(eta_temp[r]);

	}
	free(eta);
	free(eta_temp);
	for (r=0;r<nRules;r++)
        {
		free(arrayprob[r]);
	}
	free(arrayprob);
	free(rules);

	return 1;
}

static int expec(void)	//MODIFICARE ****
{
	YAP_Term arg1,arg2,out;
	DdNode * node;
	double rootProb,CLL;
//	int i;
	CLL=0;
	arg1=YAP_ARG1;
	arg2=YAP_ARG2;  //prob. di uscita
	node=(DdNode *)YAP_IntOfTerm(arg1);			//nodo radice del BDD
	////printf("nodesB\n");
//	nodesB=g_hash_table_new(my_hash,my_equal);
//	nodesF=g_hash_table_new(my_hash,my_equal);
  	nodesB=init_table(*boolVars);
  	nodesF=init_table(*boolVars);

	////printf("nodesF\n");
//	eTable=g_hash_table_new(my_hash,my_equal);

	//out=YAP_MkFloatTerm(ProbPath(node)); //output; 
	
	Forward(node); 
	printf("dopo fwd\n");
	rootProb=GetOutsideExpe(node,1.0);
//	Maximization();

	/*for (i=0;i<nVars;i++)
	{
		////printf("\n COSTRUZIONE DI NODES, i=%d, nodo=%x \n",i,vars[i].booleanVars[0]);
		
		nodes[i]=vars[i].booleanVars[0];
		////printf("nodes[%d]=%x",i,nodes[i]);

	}
	Expectation(nodes);*/

	if (rootProb==0.0)
		CLL = CLL + LOGZERO;
		else
		CLL = CLL + log(rootProb);

	out=YAP_MkFloatTerm(CLL);

//	g_hash_table_foreach(nodesB,dealloc,NULL);
//	g_hash_table_foreach(nodesF,dealloc,NULL);
//	g_hash_table_foreach(eTable,dealloc,NULL);
        destroy_table(nodesB,*boolVars);
	destroy_table(nodesF,*boolVars);

//	g_hash_table_destroy(nodesB);
//	g_hash_table_destroy(nodesF);
//	g_hash_table_destroy(eTable);
	return(YAP_Unify(out,arg2));
	//return 1;
//	return CLL;
}

static int ret_prob(void)
{
	YAP_Term arg1,arg2,out;
	DdNode * node;
	
	arg1=YAP_ARG1;
	arg2=YAP_ARG2;
	node=(DdNode *)YAP_IntOfTerm(arg1);
//	nodesB=g_hash_table_new(my_hash,my_equal);
//	printf("ret_prob\n");

	if (!Cudd_IsConstant(node))
	{
//	printf("boolvars %d\n",*boolVars);
	table=init_table(*boolVars);
//	nodesF=g_hash_table_new(my_hash,my_equal);

	out=YAP_MkFloatTerm(Prob(node,0));

	destroy_table(table,*boolVars);
	}
	else
	{
		//printf("node %d node Cudd_ReadOne(mgr)%d\n",node,Cudd_ReadOne(mgr));
		if (node==Cudd_ReadOne(mgr))
		{
			out=YAP_MkFloatTerm(1.0);
		}
		else	
			out=YAP_MkFloatTerm(0.0);
	}

//	Forward(node);
//	g_hash_table_foreach(nodesB,dealloc,NULL);
//	g_hash_table_destroy(nodesB);
//	g_hash_table_destroy(nodesF);
	return(YAP_Unify(out,arg2));
//	return 1;

}
double Prob(DdNode *node,int comp_par)  //tiene in conto una nuova variabile multivalore
/* compute the probability of the expression rooted at node.
nodesB is used to store nodesB for which the probability has alread been computed
so that it is not recomputed
 */
{
  int index,mVarIndex,comp,pos;
  variable v;
  double res;
  double p,pt,pf,BChild0,BChild1;
  double * value_p;
  DdNode *nodekey,*nodefw,*T,*F;
//  double *rp;

	//nodekey=(DdNode **)malloc(sizeof(DdNode *));
  /*//printf("comp %d\n",comp);
  index=node->index;
  //printf("ProbPath \t node %d\n",node);
  index=Cudd_NodeReadIndex(node);
  ////printf("Prob INdex %d\n",index);*/

  //printf("\nPROBPATH - nodo in input %x con indice %d \n",(unsigned int)node,Cudd_NodeReadIndex(node));
  comp=Cudd_IsComplement(node);   
  comp=(comp && !comp_par) ||(!comp && comp_par);
  ////printf("complementato=%d \n",comp);
  if (Cudd_IsConstant(node))                  //********NODO FOGLIA-Returns 1 if the node is a constant node (rather than an  internal node).
  {
   //printf("Nodo foglia %x \t",(unsigned int)node);
    //printf("valore %f comp %d\n",value,comp);

    if (comp)								//Cudd_IsComplement(node)):restituisce 1 se il puntatore node è complementato (in pratica se e' pari)
	//return (1.0-value);
	//if (value>0)
	{
	//////printf("return 0");
	return 0.0;
	}
	else
	{
	//////printf("return 1");
	return 1.0;
	}
/*else
	if (value>0)
	return 1.0;
	else
	return 0.0;
*/    //return value;
  }
  else
{
	////printf("dentro else \n");
	nodefw=Cudd_Regular(node);
	if (comp)
	 nodekey=Cudd_Complement(nodefw);
	else
	 nodekey=nodefw;
	//printf("nodekey=%x\t comp=%d",(unsigned int)nodekey,comp);
	//	value_p=g_hash_table_lookup(nodesB,nodekey);  //probabilità del nodo node
		value_p=get_value(table,nodekey);
	//value_p=NULL;
	//printf("value_p=%f\n",*value_p);
	if (value_p!=NULL)
	{
	//	printf("value_p found %e\n",*value_p);
/*		if (comp)
			return 1-*value_p;
		else */
			return *value_p;
	}
	else
  	{//	printf("value_p null \n");
		index=Cudd_NodeReadIndex(node);  //Returns the index of the node. The node pointer can be either regular or complemented.
										 //The index field holds the name of the variable that labels the node. The index of a variable is a permanent attribute that reflects the order of creation.
		//////printf("node absent %d comp %d\n",node,comp);
    	//mVarIndex=array_fetch(int,bVar2mVar,index);

		////printf("\n\n*******************PROBPATH- root node %x index %d \n",(unsigned int)node,index);
		p=probs[index];
      T = Cudd_T(node);//node->type.kids.T;		//figlio 1
      F = Cudd_E(node);//node->type.kids.E;		//figlio 0
	  //printf("FIGLI: T %x index %d F %x index %d \n",(int) T,Cudd_NodeReadIndex(T),(int)F,Cudd_NodeReadIndex(F));
	  pf=Prob(F,comp);
	  pt=Prob(T,comp);
		BChild0=pf*(1-p);
		//printf("node %x F %x ProbPath(F)=%f p=%f 1-p=%f \n",(int)node,(int)F,pf,p,1-p);
		BChild1=pt*p;
		//printf("node %x T %x ProbPath(T)=%f p=%f \n",(int)node,(int)T,pt,p);
		
	//	printf("prima di value_p da hash_table, node=%x nodefw=%x \n", node,nodefw);
		mVarIndex=bVar2mVar[index];
		v=vars[mVarIndex];	
			//eta
			////printf("vRN.nRule = %d \n",vRN.nRule);
			pos=index-v.firstBoolVar;
			//printf("pos=%d v.nRule=%d\n",pos,v.nRule);
//		*key=nodekey;
//		rp=(double *)malloc(sizeof(double));
		res=BChild0+BChild1;
//		*rp=res;
	//	printf("\n RES=Bchild0+Bchild1 = %f\n",*rp);
//		g_hash_table_insert(nodesB, nodekey, rp); //Inserts a new key and value into a GHashTable. Inserisce nella tabella il nodo key e la prob. rp
		add_node(table,nodekey,res);
			
		//printf("T %x index %d\n",(int)T,Cudd_NodeReadIndex(T));
    		return res;
		//}
  	}
}
}





static int add_var(void)	//chiamata dentro a dir.pl
{
	YAP_Term arg1,arg2,arg3,arg4,out,probTerm,probTerm_temp;
	variable * v,*vars_temp; //variabile multivalore; puntatore ad array di var. multiv.
	int i;
	DdNode * node;
	double p,p0;
	double * probs_temp;
	int * bVar2mVar_temp;


	arg1=YAP_ARG1; //num. di valori della variabile mult.
	arg2=YAP_ARG2; //lista di probabilità
	arg3=YAP_ARG3; //numero della regola a cui si riferisce la variabile mult.
	arg4=YAP_ARG4; //output
	*nVars=*nVars+1;
	vars_temp=(variable *) realloc(vars,*nVars * sizeof(variable)); //vars=array delle var. multivalore, realloc estende l'array; nVars è incrementato quindi rialloco lo spazio
	if (vars_temp==NULL)
		printf("add_var vars_temp null\n");
	else
		vars=vars_temp;
	v=&vars[*nVars-1];  //puntatore a nuova variabile
	v->nVal=YAP_IntOfTerm(arg1);
	v->nRule=YAP_IntOfTerm(arg3);
	v->firstBoolVar=*boolVars;
	//////printf("varIndex %d nbit %d\n",varIndex,v.nBit);

	//////printf("boolVars %d ",boolVars);

       // printf("\n ADDED VAR %d\n",*nVars-1);
	//printf("Rule %d\n",v->nRule);
	probs_temp=(double *) realloc(probs,(((*boolVars+v->nVal-1)* sizeof(double))));
	probs=probs_temp;
	//printf("dopo probs\n");
	bVar2mVar_temp=(int *) realloc(bVar2mVar,((*boolVars+v->nVal-1)* sizeof(int)));
	bVar2mVar=bVar2mVar_temp;
	//printf("n b var %d\n",*boolVars+v->nVal-1); 
	probTerm=arg2; 
	p0=1;
       // printf("nVal=%d\n\n", v->nVal);
	for (i=0;i<v->nVal-1;i++)
	{
	//	printf("i=%d \n",i);
		node=Cudd_bddIthVar(mgr,*boolVars+i);
	//	printf("nodo %x\t",(int) node);
		p=YAP_FloatOfTerm(YAP_HeadOfTerm(probTerm));
		//p=YAP_IntOfTerm(YAP_HeadOfTerm(probTerm));
		//printf("p=%f \n\n",p);
	//	printf("Index=%d\n",Cudd_NodeReadIndex(node));
		////printf("add_var - booleanNode= %x \n",(unsigned int)node);
		bVar2mVar[*boolVars+i]=*nVars-1;
		//printf("add_var - bVar2mVar[%d]=%d \n",*boolVars+i,*nVars-1);
		//////printf("node %d\n",node);
		//array_insert(DdNode *,v.booleanVars,i,node);
		//array_insert(int,bVar2mVar,b+i,varIndex);
		probs[*boolVars+i]=p/p0;
		//printf("probs[%d]=%f \n",*boolVars+i,probs[*boolVars+i]);
		probTerm_temp=YAP_TailOfTerm(probTerm);
		//free(probTerm);
		probTerm=probTerm_temp;
		p0=p0*(1-p/p0);
	}


	*boolVars=*boolVars+v->nVal-1;
	//printf("bVar2mVar %x\n",(int)bVar2mVar);	
//	printf("BoolVars= %d\n\n",*boolVars);	
       		rules[v->nRule]= v->nVal;  //quando si chiama add_var, ad esempio add_var(2,[0.3,0.7],0,V1),prendi il numero dei valori e lo memorizzi in un                                            array globale indicizzato con il numero della regola
		
//	eta[v->nRule]= (double **) malloc(v->nVal*sizeof(double));
//      eta[v->nRule][v->nVal]=(double *) malloc(2*sizeof(double));
	out=YAP_MkIntTerm((YAP_Int)* nVars-1); //crea un integer term
	//printf("prima di return\n\n");
	return YAP_Unify(out,arg4); //YAP_Unify(YAP_Term, YAP_Term) which in turn returns an integer denoting success or failure of the unification
								//restituisce l'ultima var. multivalore
}

static int equality(void)
{
	YAP_Term arg1,arg2,arg3,out;

	int varIndex;
	int value;
	int i;
	variable v;
	DdNode * node, * tmp,*var;

	arg1=YAP_ARG1; //var 
	arg2=YAP_ARG2; //value
	arg3=YAP_ARG3; //node
	varIndex=YAP_IntOfTerm(arg1);
	value=YAP_IntOfTerm(arg2);
	//////printf("index %d value %d\n",varIndex,value);
	//v=array_fetch(variable, vars, varIndex);
	v=vars[varIndex];
	i=v.firstBoolVar;
	//////printf("var obtained\n");
	tmp=Cudd_ReadOne(mgr);
	Cudd_Ref(tmp);
	node=NULL;
	for (i=v.firstBoolVar;i<v.firstBoolVar+value;i++)
	{
		////printf("cyc %d\n",i);
		var=Cudd_bddIthVar(mgr,i);
		////printf("index %d\n",Cudd_NodeReadIndex(var));
		////printf("var %d \n",var);

		node=Cudd_bddAnd(mgr,tmp,Cudd_Not(var));
		Cudd_Ref(node);
		Cudd_RecursiveDeref(mgr,tmp);
		tmp=node;
	}
	if (!(value==v.nVal-1))
	{
		////printf("last bit %d\n",value);
		var=Cudd_bddIthVar(mgr,v.firstBoolVar+value);
		node=Cudd_bddAnd(mgr,tmp,var);
		Cudd_Ref(node);
		Cudd_RecursiveDeref(mgr,tmp);
	}
	out=YAP_MkIntTerm((YAP_Int) node);

	return(YAP_Unify(out,arg3));

}

static int one(void) //restituisce un BDD con il nodo costante 1
{
	YAP_Term arg,out;
	DdNode * node;
	arg=YAP_ARG1;
	node =  Cudd_ReadOne(mgr);
	Cudd_Ref(node);
	out=YAP_MkIntTerm((YAP_Int) node);
	return(YAP_Unify(out,arg));
}
static int zero(void)
{
	YAP_Term arg,out;
	DdNode * node;
	arg=YAP_ARG1;
	node = Cudd_ReadLogicZero(mgr);
	Cudd_Ref(node);
	out=YAP_MkIntTerm((YAP_Int) node);
	return(YAP_Unify(out,arg));
}
static int bdd_not(void)
{
	YAP_Term arg1,arg2,out;
	DdNode * node;
	arg1=YAP_ARG1;
	arg2=YAP_ARG2;

	node = (DdNode *)YAP_IntOfTerm(arg1);
	node=Cudd_Not(node);
	out=YAP_MkIntTerm((YAP_Int) node);
	return(YAP_Unify(out,arg2));
}

static int and(void)
{
	YAP_Term arg1,arg2,arg3,out;
	DdNode * node1, *node2,*nodeout;

	arg1=YAP_ARG1;
	arg2=YAP_ARG2;
	arg3=YAP_ARG3;
	node1=(DdNode *)YAP_IntOfTerm(arg1);
	node2=(DdNode *)YAP_IntOfTerm(arg2);
	nodeout=Cudd_bddAnd(mgr,node1,node2);
	Cudd_Ref(nodeout);
//	Cudd_RecursiveDeref(mgr,node1);
//	Cudd_RecursiveDeref(mgr,node2);
	out=YAP_MkIntTerm((YAP_Int) nodeout);
	return(YAP_Unify(out,arg3));
}
static int or(void)
{
	YAP_Term arg1,arg2,arg3,out;
	DdNode * node1,*node2,*nodeout;

	arg1=YAP_ARG1;
	arg2=YAP_ARG2;
	arg3=YAP_ARG3;
	node1=(DdNode *)YAP_IntOfTerm(arg1);
	node2=(DdNode *)YAP_IntOfTerm(arg2);
	nodeout=Cudd_bddOr(mgr,node1,node2);
	Cudd_Ref(nodeout);
//	Cudd_RecursiveDeref(mgr,node1);
//	Cudd_RecursiveDeref(mgr,node2);
	out=YAP_MkIntTerm((YAP_Int) nodeout);
	return(YAP_Unify(out,arg3));
}

static int garbage_collect(void)
{
    YAP_Term arg1,arg2,out;
	YAP_Int nodes,clearCache;
	arg1=YAP_ARG1;
	arg2=YAP_ARG2;
	clearCache=YAP_IntOfTerm(arg1);
	nodes=(YAP_Int)cuddGarbageCollect(mgr,clearCache);
	out=YAP_MkIntTerm(nodes);
	return(YAP_Unify(out,arg2));
}

static int bdd_to_add(void)
{
        YAP_Term arg1,arg2,out;
	DdNode * node1,*node2;
	arg1=YAP_ARG1;
	arg2=YAP_ARG2;
        node1=(DdNode *)YAP_IntOfTerm(arg1);
	node2= Cudd_BddToAdd(mgr,node1);
	out=YAP_MkIntTerm((YAP_Int) node2);
	return(YAP_Unify(out,arg2));

}

static int create_dot(void)
{
	char * onames[]={"Out"};
	char ** inames;
	//char * names[1000];
 	DdNode * array[1];
	YAP_Term arg1,arg2;
	int i,b,index;
	variable v;
	char numberVar[10],numberBit[10],filename[1000];
	FILE * file;
	
	arg1=YAP_ARG1;
	arg2=YAP_ARG2;

	YAP_StringToBuffer(arg2,filename,1000);
	////printf("create\n");
	//nvars=array_n(vars);
	//nBVars=array_n(bVar2mVar);
	inames= (char **) malloc(sizeof(char *)*(*boolVars));
	////printf("bool var %d\n",boolVars);
	index=0;
	for (i=0;i<*nVars;i++)
	{
		v=vars[i];
		for (b=0;b<v.nVal-1;b++)
		{	
			inames[b+index]=(char *) malloc(sizeof(char)*20);
			strcpy(inames[b+index],"X");
			sprintf(numberVar,"%d",i);
			strcat(inames[b+index],numberVar);
			strcat(inames[b+index],"_");
			sprintf(numberBit,"%d",b);
			strcat(inames[b+index],numberBit);
	//	//printf("i %d b %d index %d %s\n",i,b,index,inames[b+index]);
		}
		index=index+v.nVal-1;
	}
/*	for(i=0;i<nbVars;i++)
	{
		names[i]=inames[i];
		////printf("%s\n",names[i]);
	}
	*/
	array[0]=(DdNode *)YAP_IntOfTerm(arg1);
	////printf("%d \n",array[0]);
//	onames[0]="Out";
	file = open_file(filename, "w");

	//Cudd_PrintInfo(mgr,stdout);

	Cudd_DumpDot(mgr,1,array,inames,onames,file);
	//Cudd_DumpDot(mgr,1,array,NULL,onames,file);
	fclose(file);
	index=0;
	for (i=0;i<*nVars;i++)
	{
		v=vars[i];
		for (b=0;b<v.nVal-1;b++)
		{	
			free(inames[b+index]);
		}
		index=index+v.nVal-1;
	}
	free(inames);
	return 1;
}


static int rec_deref(void)
{
	YAP_Term arg1;
	DdNode * node;



	arg1=YAP_ARG1;
	node=(DdNode *) YAP_IntOfTerm(arg1);
        Cudd_RecursiveDeref(mgr, node);
	return 1;
}



//****************************************************************************BACKWARD*******************************************************************

double ProbPath(DdNode *node,int comp_par)  //tiene in conto una nuova variabile multivalore
/* compute the probability of the expression rooted at node.
nodesB is used to store nodesB for which the probability has alread been computed
so that it is not recomputed
 */
{
  int index,mVarIndex,comp,pos,position,boolVarIndex;
  variable v;
  double res;
  double value,p,pt,pf,BChild0,BChild1,e0,e1;
  double * value_p,** eta_rule;
  DdNode *nodekey,*nodefw,*T,*F;
//  double *rp;

  //printf("comp %d\n",comp);
  //index=node->index;
  //printf("ProbPath \t node %x\n",node);
  //index=Cudd_NodeReadIndex(node);
  ////printf("Prob INdex %d\n",index);

  //printf("\nPROBPATH - nodo in input %x con indice %d \n",(unsigned int)node,Cudd_NodeReadIndex(node));
  comp=Cudd_IsComplement(node);   
  comp=(comp && !comp_par) ||(!comp && comp_par);
  //printf("complementato=%d \n",comp);
  if (Cudd_IsConstant(node))                  //********NODO FOGLIA-Returns 1 if the node is a constant node (rather than an  internal node).
  {
   //printf("Nodo foglia %x \n",(unsigned int)node);
    value=Cudd_V(node);//node->type.value;	  //********NON SERVE: Returns the value of a constant node.
    //printf("valore %f comp %d\n",value,comp);

    if (comp)								//Cudd_IsComplement(node)):restituisce 1 se il puntatore node è complementato (in pratica se e' pari)
	//return (1.0-value);
	//if (value>0)
	{
//	printf("return 0\n");
	return 0.0;
	}
	else
	{
	//////printf("return 1");
	return 1.0;
	}
/*else
	if (value>0)
	return 1.0;
	else
	return 0.0;
*/    //return value;
  }
  else
{
	////printf("dentro else \n");
	nodefw=Cudd_Regular(node);
	if (comp)
	 nodekey=Cudd_Complement(nodefw);
	else
	 nodekey=nodefw;
	//printf("nodekey=%x\t comp=%d",(unsigned int)nodekey,comp);
		//value_p=g_hash_table_lookup(nodesB,nodekey);  //probabilità del nodo node
		value_p=get_value(nodesB,nodefw);
	//printf("value_p=%f\n",*value_p);
	if (value_p!=NULL)
	{
	//	printf("value_p found %e\n",*value_p);
/*		if (comp)
			return 1-*value_p;
		else */
			return *value_p;
	}
	else
  	{//	printf("value_p null \n");
		index=Cudd_NodeReadIndex(node);  //Returns the index of the node. The node pointer can be either regular or complemented.
										 //The index field holds the name of the variable that labels the node. The index of a variable is a permanent attribute that reflects the order of creation.
		//////printf("node absent %d comp %d\n",node,comp);
    	//mVarIndex=array_fetch(int,bVar2mVar,index);

		////printf("\n\n*******************PROBPATH- root node %x index %d \n",(unsigned int)node,index);
		p=probs[index];
      T = Cudd_T(node);//node->type.kids.T;		//figlio 1
      F = Cudd_E(node);//node->type.kids.E;		//figlio 0
	  //printf("FIGLI: T %x index %d F %x index %d \n",(int) T,Cudd_NodeReadIndex(T),(int)F,Cudd_NodeReadIndex(F));
	  pf=ProbPath(F,comp);
	  //printf("pf\n");
	  pt=ProbPath(T,comp);
	  //printf("pt\n");
		BChild0=pf*(1-p);
		//printf("node %x F %x ProbPath(F)=%f p=%f 1-p=%f \n",(int)node,(int)F,pf,p,1-p);
		BChild1=pt*p;
		//printf("BChild1=pt*p= %f=%f*%f\n", BChild1,pt,p);
		//printf("node %x T %x ProbPath(T)=%f p=%f \n",(int)node,(int)T,pt,p);
		
	//	printf("prima di value_p da hash_table, node=%x nodefw=%x \n", node,nodefw);
		//value_p=g_hash_table_lookup(nodesF,nodefw);
		value_p=get_value(nodesF,nodefw);
		//printf("\n value_p(da nodesF) = %x associato al nodo %x\n BChild0=%f \t Bchild1=%f\n",value_p,(int)nodefw,BChild0,BChild1);
		e0 = (*value_p)*BChild0; 
		//if (e0<0.0) printf("e0 %lf\n",e0);
		e1 = (*value_p)*BChild1; 
		//printf("e1-valuep= %f\n", *value_p);
		//if (e1<0.0) printf("e1 %lf\n",e1);
//	printf("e0=value_p*B0=%f   e1=value_p*B1=%f \n",e0,e1);

		mVarIndex=bVar2mVar[index];
		v=vars[mVarIndex];	
			//eta
			////printf("vRN.nRule = %d \n",vRN.nRule);
			pos=index-v.firstBoolVar;
			//printf("pos=%d v.nRule=%d\n",pos,v.nRule);
			eta_rule=eta_temp[v.nRule];
		//printf("\n ####### prima dell'aggiornamento: eta_rule[%d][0] = %f \t eta_rule[%d][1] = %f\n", pos,eta_rule[pos][0],pos,eta_rule[pos][1]);

			eta_rule[pos][0]=eta_rule[pos][0]+e0;
			eta_rule[pos][1]=eta_rule[pos][1]+e1;
		//	printf("dopo l'aggiornamento: eta_rule[%d][0] = %f \t eta_rule[%d][1] = %f\n", pos,eta_rule[pos][0],pos,eta_rule[pos][1]);


//		key=(DdNode **)malloc(sizeof(DdNode *));
//		*key=nodekey;
		//rp=(double *)malloc(sizeof(double));
		res=BChild0+BChild1;
		//*rp=res;
	//	printf("\n RES=Bchild0+Bchild1 = %f\n",*rp);
		add_node(nodesB,nodefw,res);
		//g_hash_table_insert(nodesB, nodekey, rp); //Inserts a new key and value into a GHashTable. Inserisce nella tabella il nodo key e la prob. rp

			
			position=Cudd_ReadPerm(mgr,index);
	//		printf("POS: %d",pos);
			position=position+1;
			boolVarIndex=Cudd_ReadInvPerm(mgr,position);  //Returns the index of the variable currently in the i-th position of the order. 

//			if(boolVarIndex!=-1)
			if (position<*boolVars)
			{
				//Vsucc
				//printf("\nprima - sigma[%d] = %f\n ",boolVarIndex,sigma[boolVarIndex]);
				sigma[position]=sigma[position]+e0+e1;
				//printf("dopo - sigma[%d] = %f\n ",boolVarIndex,sigma[boolVarIndex]);
			}

			// scandire l'ordine fino a che non trovi una variabile booleana che fa parte di una variabile multivalore successiva
			////printf("\n \n 2^ chiamata a indexMVar");
			
		//printf("T %x index %d\n",(int)T,Cudd_NodeReadIndex(T));
		if(!Cudd_IsConstant(T)){
			index=Cudd_NodeReadIndex(T);	
			position=Cudd_ReadPerm(mgr,index);
			//printf("prima - per nodo(T) %x - sigma[%d] = %f\n ",(unsigned int)T,index,sigma[index]);
			sigma[position]=sigma[position]-e1;
		//	if (sigma[index]<0) printf("sigma[%d]=%lf\n",index,sigma[index]);
			//printf("dopo - per nodo(T) %x - sigma[%d] = %f\n ",(unsigned int)T,index,sigma[index]);
			}
			
		if(!Cudd_IsConstant(F)){
			index=Cudd_NodeReadIndex(F);
			position=Cudd_ReadPerm(mgr,index);
			//printf("prima - per nodo(F) %x - sigma[%d] = %f\n ",(unsigned int)F,index,sigma[index]);
			sigma[position]=sigma[position]-e0;
		//	if (sigma[index]<0) printf("sigma[%d]=%lf\n",index,sigma[index]);

			//printf("dopo - per nodo(F) %x - sigma[%d] = %f\n ",(unsigned int)F,index,sigma[index]);
		}
		
/*    	if (comp)
		{	//printf("ProbPath 1-res = %f, comp = %d\n",1-res, comp);
			return 1-res;
		}
		else
		{  */
			//printf("\n ProbPath-res= %f \n",res);
    		return res;
		//}
  	}
}
}





/*******************************************************************************FORWARD******************************************************************/

void Forward(DdNode *root)  //tiene in conto una nuova variabile multivalore
/* compute the probability of the expression rooted at node.
nodesB is used to store nodesB for which the probability has alread been computed so that it is not recomputed
 */
{
int i,j;
  //DdNode **key;
  //double *rp;
//////printf("lungh. bVar2mVar= %d",ArrayElements(bVar2mVar));
//printf("********************************FORWARD \t root %x \n",(unsigned int)root);

/*key=(DdNode **)malloc(sizeof(DdNode *));
*key=root;
rp=(double *)malloc(sizeof(double));
*rp=1;
g_hash_table_insert(nodesF, key, rp);*/
if (*boolVars)
{
nodesToVisit= (DdNode ***)malloc(sizeof(DdNode **)* *boolVars);
NnodesToVisit= (int *)malloc(sizeof(int)* *boolVars);
nodesToVisit[0]=(DdNode **)malloc(sizeof(DdNode *)); 
nodesToVisit[0][0]=root;
NnodesToVisit[0]=1;
for(i=1;i<*boolVars;i++)
{
	nodesToVisit[i]=NULL;
	NnodesToVisit[i]=0;
}

//rp=(double *)malloc(sizeof(double));
//*rp=1;  //value
//printf("prima di hash table insert\n");
add_node(nodesF,Cudd_Regular(root),1);
//printf("dopo hash table insert\n");
//printf("boolvars %d\n",*boolVars);
for(i=0;i<*boolVars;i++)
{
//	printf("Level %d node %d\n",i,NnodesToVisit[i]);
	for(j=0;j<NnodesToVisit[i];j++)
		UpdateForward(nodesToVisit[i][j]);
}
for(i=0;i<*boolVars;i++)
{
//	printf("free %d %d\n",i,mprobe(nodesToVisit[i]));
	free(nodesToVisit[i]);
}
free(nodesToVisit);
free(NnodesToVisit);
}
else
{
//rp=(double *)malloc(sizeof(double));
//*rp=1;  //value
add_node(nodesF,Cudd_Regular(root),1);
}
}

void UpdateForward(DdNode *node)
{

int index,position,mVarIndex;
DdNode *T,*E,*nodereg;
variable v;
double *value_p,*value_p_T,*value_p_F,p;//,*rp;
DdNode ** array_temp;

//printf("update forward\n");
if (Cudd_IsConstant(node)) 
{	//printf("nodo foglia %x",node);
	return;
}
else
{

//printf("G \t");
index=Cudd_NodeReadIndex(node);  //Returns the index of the node. The node pointer can be either regular or complemented.
//printf("\nGETFORWARD - node = %x,Indice %d\n",(unsigned int)node,index);
								 //The index field holds the name of the variable that labels the node. The index of a variable is a permanent attribute that reflects the order of creation.
mVarIndex=bVar2mVar[index];      //recupera l'indice della var. multivalore
//printf("mVarIndex %d\n",mVarIndex);
v=vars[mVarIndex];				//vars=array delle var. multivalore; v=variabile multivalore

p=probs[index];
//printf("p=probs[%d]=%f \n",index,p);




 	////printf("bits=%d probs[0]=%f probs[1]=%f \n",bits,probs[0],probs[1]);
	nodereg=Cudd_Regular(node);
//	value_p=g_hash_table_lookup(nodesF,nodereg); //recupera la probabilità F(node)
	value_p=get_value(nodesF,nodereg);
	//printf("dopo lookup\n");
	if (value_p== NULL)
	{
		printf("Error\n");
                //  printf("1\t");
		return;
	   }
	else
	{


	T = Cudd_T(node);//node->type.kids.T;		//figlio 1
        E = Cudd_E(node);//node->type.kids.E;		//figlio 0

//	printf("node %x  T %x  E %x  index_T %d pos_T %d index_E=%d pos_E %d \n",(int)node,(int)T,(int)E,Cudd_NodeReadIndex(T),Cudd_ReadPerm(mgr,Cudd_NodeReadIndex(T)),Cudd_NodeReadIndex(E),Cudd_ReadPerm(mgr,Cudd_NodeReadIndex(E)));
	if (!Cudd_IsConstant(T)) 
	{
//	  value_p_T=g_hash_table_lookup(nodesF,T);
	  value_p_T=get_value(nodesF,T);

        if (value_p_T!= NULL)
	{
		              // printf("ForwProbPath=%f\n",ForwProbPath);
				//                //  printf("1\t");
				//
		*value_p_T= *value_p_T+*value_p*p;
	}
	else
	{
		//rp=(double *)malloc(sizeof(double));
		//*rp=*value_p*p;  //value
//		g_hash_table_insert(nodesF, T, rp);
		add_or_replace_node(nodesF,Cudd_Regular(T),*value_p*p);
		index=Cudd_NodeReadIndex(T);
		position=Cudd_ReadPerm(mgr,index);
		array_temp=(DdNode **)realloc(nodesToVisit[position],  (NnodesToVisit[position]+1)* sizeof(DdNode *));
		nodesToVisit[position]=array_temp;
		//printf("mprobe %d\n",mprobe(nodesToVisit[position]));

		nodesToVisit[position][NnodesToVisit[position]]=T;
		NnodesToVisit[position]=NnodesToVisit[position]+1;
		
	}
	}
	if (!Cudd_IsConstant(E)) 
	{
//	  value_p_F=g_hash_table_lookup(nodesF,Cudd_Regular(E));
	  value_p_F=get_value(nodesF,Cudd_Regular(E));

        if (value_p_F!= NULL)
	{
		             //   printf("ForwProbPath=%f\n",ForwProbPath);
				//                //  printf("1\t");
				//
		*value_p_F= *value_p_F+*value_p*(1-p);
	}
	else
	{
//		rp=(double *)malloc(sizeof(double));
//		*rp= *value_p*(1-p);  //value
//		g_hash_table_insert(nodesF, Cudd_Regular(E), rp);
		add_or_replace_node(nodesF,Cudd_Regular(E),*value_p*(1-p));
		index=Cudd_NodeReadIndex(E);
		position=Cudd_ReadPerm(mgr,index);
		array_temp=(DdNode **)realloc(nodesToVisit[position],  (NnodesToVisit[position]+1)* sizeof(DdNode *));
		nodesToVisit[position]=array_temp;

		//printf("mprobe %d\n",mprobe(nodesToVisit[position]));
		nodesToVisit[position][NnodesToVisit[position]]=E;
		NnodesToVisit[position]=NnodesToVisit[position]+1;
	}
	}
//printf("end fw\n");
	return;
}
}
}


int indexMvar(DdNode * node) //dato un nodo restituisce l'indice della variabile multivalore ad essa associato.
{
	int index,mVarIndex;

	////printf("\n *FUNZ. INDEXMVAR \n");

	index=Cudd_NodeReadIndex(node); //indice della variabile booleana associata al nodo
	////printf("node %x index_node_bool=%d \n",node,index);

	////printf("lungh. bVar2mVar: %d \n", ArrayElements(bVar2mVar));
	//////printf("bVar2mVar[0]=%d \t",bVar2mVar[0]);
	//////printf("bVar2mVar[1]=%d \n",bVar2mVar[1]);	

	mVarIndex=bVar2mVar[index];
	
	////printf("IndexmVar= %d \n",mVarIndex);
	return mVarIndex;
}



/**************************************************************GETOUTSIDEXPE***************************************************************************/

double GetOutsideExpe(DdNode *root,double ex_prob)  

{
int i,j,mVarIndex,bVarIndex;
double **eta_rule;
double theta,rootProb, T=0;

//f=fopen("bVar2mVar.txt","a");

sigma=(double *)malloc(*boolVars * sizeof(double));

 for (j=0; j<*boolVars; j++)
 {
	 sigma[j]=0;
 }


//printf("\n GETOUTSIDEXPE - prima di ProbPath \n");
for (j=0; j<nRules; j++)
   {
   	for (i=0; i<rules[j]-1; i++)
	{
		eta_temp[j][i][0]=0;
		eta_temp[j][i][1]=0;
	}
   }


rootProb=ProbPath(root,0);
if (rootProb>0.0)
{
//printf("GETOUTSIDEXPE - dopo ProbPath, rootProb=%f\n",rootProb);

//Cudd_PrintInfo(mgr,stdout);
//printf("\nboolVars=%d\n",*boolVars); 

for (j=0; j<*boolVars; j++)
{
//  printf("\n CICLO ESTERNO, j = %d \n",j);
   T += sigma[j];
//   if (T<-0.00000000000001)
  // 	printf("j %d T %30.28lf \n",j,T);
  // printf("\nT = %f \t sigma[%d] = %f\n",T,j,sigma[j]); 
   bVarIndex=Cudd_ReadInvPerm(mgr,j);
//   printf("bVarIndex=%d\n",bVarIndex);
   if (bVarIndex==-1)  
   {
//   	fprintf(f,"\nbVarIndex=%d esempio %d\n",bVarIndex,esempio);
	bVarIndex=j;
	}

  /* if (esempio==29)	
	create_dot(root);*/
   mVarIndex=bVar2mVar[bVarIndex];
//   printf("mVarIndex=%d\n",mVarIndex);
   eta_rule=eta_temp[vars[mVarIndex].nRule];  //Vj.nRule
//   printf("rule %d\n",vars[mVarIndex].nRule);
  // printf("vars[mVarIndex].nVal=%d\n",vars[mVarIndex].nVal);
   for (i=0; i<vars[mVarIndex].nVal-1;i++)
   {
   // printf("***   i = %d   ***\n", i);
		theta=probs[bVarIndex];
	//	printf("theta = %f \n", theta);

		//printf("eta_rule[%d][0] = %f prima di agg.\n",i,eta_rule[i][0]);
	   
	    eta_rule[i][0]=eta_rule[i][0]+T*(1-theta);  //bits=i
	//	printf("eta_rule[%d][0] = %f dopo agg.\n",i,eta_rule[i][0]);

		//printf("eta_rule[%d][1] = %f prima di agg.\n",i,eta_rule[i][1]);
		eta_rule[i][1]=eta_rule[i][1]+T*theta;  //bits=i
	//	printf("eta_rule[%d][1] = %f dopo agg.\n",i,eta_rule[i][1]);
   }   
}//for
//fclose(f);

   for (j=0; j<nRules; j++)
   {
   	for (i=0; i<rules[j]-1; i++)
	{
		//printf("eta[%d][%d][0]=%f\n",j,i,eta[j][i][0]);
		eta[j][i][0]=eta[j][i][0]+eta_temp[j][i][0]*ex_prob/rootProb;
		//printf("eta[%d][%d][1]=%f\n",j,i,eta[j][i][1]);
		eta[j][i][1]=eta[j][i][1]+eta_temp[j][i][1]*ex_prob/rootProb;
		//printf("eta_temp[%d][%d][0]=%f\n",j,i,eta_temp[j][i][0]*ex_prob/rootProb);
		//printf("eta_temp[%d][%d][1]=%f\n",j,i,eta_temp[j][i][1]*ex_prob/rootProb);
		//printf("eta[%d][%d][0] = %f \n",j,i,eta[j][i][0]);
		//printf("eta[%d][%d][1] = %f \n",j,i,eta[j][i][1]);	
	}
   }
   }
  // else 
   //	printf("root prob %lf\n",rootProb);
free(sigma);
return rootProb;
}

/********************************************************************EXPECTATION************************************************************/


/********************************************************************MAXIMIZATION**********************************************************/

void Maximization(void)
{
	int r,i,j,e;
	double sum=0;
	double *probs_rule,**eta_rule;  

	//nvalues(r_j) e' il numero di atomi nella testa di una regola (e quindi il numero di valore di tutte le variabili random associate 
	//alle istanziazioni della regola). 
//	probs=(double **) malloc(nRules * sizeof(double *));	//array temporaneo indicizzato sulle regole per memorizzare theta(R,i)

//	printf("MAXIMIZATION\n");
	for (r=0;r<nRules;r++)
	{
		//printf("\n\n CALCOLO DI THETA - REGOLA %d \n",r);
		eta_rule=eta[r];
				
	for (i=0;i<rules[r]-1;i++)
	       {   //i=n° valori/teste della regola r (è v.nVal)
		 //printf("\n i=%d, prima di sum - sum = %f \n",i,sum);
		 sum=(eta_rule[i][0]+eta_rule[i][1]);
		// printf("Sum = eta_rule[%d][0]+eta_rule[%d][1]= %f+%f =%f \n",i,i,eta_rule[i][0],eta_rule[i][1],sum);
		
		////printf("\n CALCOLO DI THETA - REGOLA %d \n",r);
		if (sum==0.0)
		{
		// printf("eta_rule[%d][0] %lf sum %lf\n",i,eta_rule[i][0],sum);
		 arrayprob[r][i]=0;
                }
		else 
		
		 arrayprob[r][i]=eta_rule[i][1]/sum;		
	//	printf("arrayprob[%d][%d]=%f \n",r,i,arrayprob[r][i]);}
		}
       
	}//for

	////printf("\n Ciclo di copia di probs_rule in v.probabilities\n");
	//printf("\nCopia di arrayprob in probs \n");
for(e=0;e<ex;e++)
{
	nVars=nVars_ex+e;
	probs=probs_ex[e];
	vars=vars_ex[e];

	for (j=0;j<*nVars;j++)
	{
		r=vars[j].nRule;
		//printf("\nRegola %d\n",r);
		probs_rule=arrayprob[r];
		//printf("rules[r]=%d\n",rules[r]);
		for(i=0;i<rules[r]-1;i++)
	//	for (i=0; i<vars[j].nVal-1;i++)	
		{		
		 //printf("dentro for, i=%d\n",i);
		 probs[vars[j].firstBoolVar+i]=probs_rule[i];		 
		// if(probs[vars[j].firstBoolVar+i]<0)
		 //	printf("probs[%d]=%f\n",vars[j].firstBoolVar+i,probs[vars[j].firstBoolVar+i]);
		}
	}
}
// for (j=0; j<nVars; j++)
//   {
//           ////printf(" Var %d\n",j);
//	   rule=vars[j].nRule;
//	  	theta=Theta_rules[rule];
//		p0=1;
//		for (i=0; i<vars[j].nVal-1;i++)	
//	   {		
////	   	//printf("probs[%d]= %f \n",i,probs[i]);
//		 	probs[vars[j].firstBoolVar+i]=theta[i]/p0;
//		//printf(" probs[%d]=%f \n",vars[j].firstBoolVar+i,probs[vars[j].firstBoolVar+i]);
//		p0=p0*(1-theta[i]/p0);
//		 }


	//printf("\n FINE Maximization\n");
	/*////printf("\n verifica \n");
		for (r=0;r<nRules;r++)
		{	for(i=0;i<rules[r];i++) ////printf("r = %d, i = %d, parametro in array v.prob = %f \n ", r,i,vars[r].probabilities[i]);	    }*/

}

/*************************************************************** EM ************************************************************************/
static int randomize(void)	//esegue le righe 8-14 della Procedure Learn (algoritmo 1)
{
	int i,j,e,rule;
	double * theta,p0;
	double pmass,par;  //pmass=Sum (riga 10)
	double **Theta_rules;	//array indicizzato dalle regole
	
	Theta_rules=(double **)malloc(nRules *sizeof(double *));

//	printf("randomize \n");
	for (j=0;j<nRules;j++)
	{
		////printf("Primo for-rule %d val %d\n",j,rules[j]);
		Theta_rules[j]=(double *)malloc(rules[j]*sizeof(double));
	}


	for (j=0;j<nRules;j++)
	{
		////printf("Secondo for-rule %d \n",j);
		theta=Theta_rules[j];
		////printf("dopo theta\n");
		pmass=0;
		for (i=0;i<rules[j]-1;i++)
		{
		//	//printf("terzo for-i=%d\n",i);
			par=((double)rand())/RAND_MAX*(1-pmass);
			////printf("parametro random  %f\n",par);
			pmass=pmass+par;
			theta[i]=par;
		}
		theta[rules[j]-1]=1-pmass;
	}
for(e=0;e<ex;e++)
{
	nVars=nVars_ex+e;
	probs=probs_ex[e];
	vars=vars_ex[e];
  for (j=0; j<*nVars; j++)
   {
           ////printf(" Var %d\n",j);
	   rule=vars[j].nRule;
	  	theta=Theta_rules[rule];
		p0=1;
		for (i=0; i<vars[j].nVal-1;i++)	
	   {		
//	   	//printf("probs[%d]= %f \n",i,probs[i]);
		 	probs[vars[j].firstBoolVar+i]=theta[i]/p0;
		//printf(" probs[%d]=%f \n",vars[j].firstBoolVar+i,probs[vars[j].firstBoolVar+i]);
		p0=p0*(1-theta[i]/p0);
		 }
   }
  }
   	for (j=0;j<nRules;j++)
	{
		free(Theta_rules[j]);
	}
	free(Theta_rules);
	return 1;
}
static int EM(void)       //predicato prolog em() in em.pl
{
   YAP_Term arg1,arg2,arg3,arg4,arg5,arg6,arg7,out1,out2,nodesTerm,ruleTerm,tail,pair,/*rTerm,*/compoundTerm;
   DdNode * node1,**nodes_ex;
   int r,lenNodes,i,iter;
   long iter1;
   double CLL0= -2.2*pow(10,10);  //-inf
   double CLL1= -1.7*pow(10,8);	//+inf   
   double p,p0,**eta_rule,ea,er; //probs=array di prob. di una variabile
   double ratio,diff;
   time_t rawtime;
   struct tm * timeinfo;

        arg1=YAP_ARG1;	//nodes
	arg2=YAP_ARG2;	//ea
	arg3=YAP_ARG3;	//er
	arg4=YAP_ARG4;	//lunghezza di nodes
	arg5=YAP_ARG5;  //iter (iterazioni del while in EM)
	arg6=YAP_ARG6;  //CLL
	arg7=YAP_ARG7;  //param
//	arg8=YAP_ARG8;  //list of probs

	//input
	nodesTerm=arg1; 
	ea=YAP_FloatOfTerm(arg2);
	er=YAP_FloatOfTerm(arg3);
	lenNodes=YAP_IntOfTerm(arg4);	
	iter=YAP_IntOfTerm(arg5);	
//	printf("iter %d\t",iter);

//	printf("\nEM \n");
	nodes_ex=(DdNode **)malloc(lenNodes*sizeof(DdNode*));
	nodes_probs_ex=(double *)malloc(lenNodes*sizeof(double));
	example_prob=(double *)malloc(lenNodes*sizeof(double));
//	printf("lenNode=%d\n",lenNodes);
//	printf("nRules=%d\n",nRules);

	for (i=0;i<lenNodes;i++)  //lista scandita e messa nell'array nodes 
	{
		pair=YAP_HeadOfTerm(nodesTerm);
		node1=(DdNode *)YAP_IntOfTerm(YAP_HeadOfTerm(pair));
		nodes_ex[i]=node1;
		pair=YAP_TailOfTerm(pair);
		example_prob[i]=YAP_FloatOfTerm(YAP_HeadOfTerm(pair));
		////printf("nodesex[%d]=%x\n",i,nodes_ex[i]);
		nodesTerm=YAP_TailOfTerm(nodesTerm);
		//scanf("%d",&in);
	}
	//nodes[lenNodes-1]=(DdNode *)YAP_IntOfTerm(YAP_HeadOfTerm(nodesTerm));
	////printf("lungh. di nodes=%d, nodes[lenNodes-1]=%x\n",lenNodes,nodes[lenNodes-1]);
	////printf("eta %x\n",eta);
	////printf("ea %f er %f\n",ea,er);
	////printf("CLL0 %f CLL1 %f\n",CLL0,CLL1);
	diff=CLL1-CLL0;
	////printf("diff %lf\n",diff);
	ratio=diff/fabs(CLL0);
	////printf("ratio %lf\n",ratio);
	if (iter==-1)
		iter1= 2147000000;   //+inf
	else iter1=iter;
//	printf("iter %d\t iter1 %ld\n",iter,iter1);
	
	time(&rawtime);
        timeinfo = localtime(&rawtime);

   while  ( (diff>ea) && (ratio>er) && (cycle<iter1) )
   {
	   cycle++;
	   //printf("\nCycle %d \n",cycle);
	   for (r=0;r<nRules;r++)
		 {
		 	////printf("eta %x\n",eta);
			//inizializza eta a 0 per tt le regole
			for (i=0;i<rules[r]-1;i++)	
			{   //i=n° valori/teste della regola r (è v.nVal)
			 eta_rule=eta[r];
			 ////printf("eta rule %x\n",eta_rule);
			 eta_rule[i][0]=0;
			 eta_rule[i][1]=0;
			 ////printf("eta rule %f\n",eta_rule[i]);
			}
		  }
	//	  printf("dopo for\n");
	CLL0 = CLL1;
	CLL1 = Expectation(nodes_ex,lenNodes);
//	printf("expectation %d \n",CLL1),
	Maximization();
//	printf("Maximization \n"),
	diff=CLL1-CLL0;
	ratio=diff/fabs(CLL0);
//	printf("\n CLL1 %lf CLL0 %lf diff %lf ratio %lf\n",CLL1,CLL0,diff,ratio);
	////printf("\n Ritorno in em \n");
	//scanf("%d",&in);

   }

   //RETURN CLL1,THETA (riga 10 algoritmo)
   //printf("\n Lista dei parametri per ogni regola\n");
   out2= YAP_TermNil();

  for (r=0; r<nRules; r++)
   {
   	   //printf("Rule %d \n",r);

	   //probs=arrayprob[r];
	   tail=YAP_TermNil();
	   p0=1;
	   for (i=0;i<rules[r]-1;i++)	
	    {
	       //printf("arrayprob[%d][%d] %lf\n",r,i,arrayprob[r][i]);
	    	p=arrayprob[r][i]*p0;
		tail=YAP_MkPairTerm(YAP_MkFloatTerm(p),tail);		//termine: lista di parametri (es.[0.2,0.8])
		p0=p0*(1-arrayprob[r][i]); 
 	    }
           tail=YAP_MkPairTerm(YAP_MkFloatTerm(p0),tail);	
	   //rule=vars[j].nRule;
	  
	   ruleTerm=YAP_MkIntTerm(r);	//termine: num.regola (es. 1)

	  compoundTerm=YAP_MkPairTerm(ruleTerm,YAP_MkPairTerm(tail,YAP_TermNil()));
	  out2=YAP_MkPairTerm(compoundTerm,out2);
   }

   out1=YAP_MkFloatTerm(CLL1);
   YAP_Unify(out1,arg6);  //cll1  
   free(nodes_ex);
   free(example_prob);

/*
   out8= YAP_TermNil();

	for (i=0;i<lenNodes;i++)  //lista scandita e messa nell'array nodes 
	{
		out8=YAP_MkPairTerm(YAP_MkFloatTerm(nodes_probs_ex[i]),out8);	
	}

	YAP_Unify(out8,arg8);*/
//	printf("\nFine EM\n");
   return (YAP_Unify(out2,arg7)); //lista dei par. theta 
   }

static int Q(void)      //fa un passo di aspettazione per calcolare eta_rule e CLL; output:liste per ogni regola:[n°.regola,[lista eta_rule[i][0],eta_rule[i][1]]]
{
   YAP_Term arg1,arg2,arg3,arg4,out,out1,term,nodesTerm,ruleTerm,tail,pair,/*rTerm,*/compoundTerm;
   /*YAP_Term args[2], compoundTerms[nRules];
   YAP_Atom rAtom;
   YAP_Functor rFunctor;*/
   DdNode * node1,**nodes_ex;
   int r,lenNodes,i;
   double p1,p0,**eta_rule,CLL; 
   time_t rawtime;
   struct tm * timeinfo;

        arg1=YAP_ARG1;	//nodes
	arg2=YAP_ARG2;	//lunghezza di nodes
	arg3=YAP_ARG3;  //counts (Stat)
	arg4=YAP_ARG4;  //CLL

	nodesTerm=arg1; 
	lenNodes=YAP_IntOfTerm(arg2);	

//printf("\nQ \n");
	nodes_ex=(DdNode **)malloc(lenNodes*sizeof(DdNode*));
	example_prob=(double *)malloc(lenNodes*sizeof(double));

	for (i=0;i<lenNodes;i++)  //lista scandita e messa nell'array nodes 
	{
		pair=YAP_HeadOfTerm(nodesTerm);
		node1=(DdNode *)YAP_IntOfTerm(YAP_HeadOfTerm(pair));
		nodes_ex[i]=node1;
		pair=YAP_TailOfTerm(pair);
		example_prob[i]=YAP_FloatOfTerm(YAP_HeadOfTerm(pair));
		nodesTerm=YAP_TailOfTerm(nodesTerm);
		//scanf("%d",&in);
	}
	//printf("lungh. di nodes=%d\n",lenNodes);
	////printf("eta %x\n",eta);
	////printf("ea %f er %f\n",ea,er);

	time(&rawtime);
        timeinfo = localtime(&rawtime);

	   for (r=0;r<nRules;r++)
		 {
		 	//printf("eta %x\n",eta);
			//inizializza eta a 0 per tt le regole
			for (i=0;i<rules[r]-1;i++)	
			{   //i=n° teste della regola r (è v.nVal)
			 eta_rule=eta[r];
			 ////printf("eta rule %x\n",eta_rule);
			 eta_rule[i][0]=0;
			 eta_rule[i][1]=0;
			 ////printf("eta rule %f\n",eta_rule[i]);
			}
		  }
	CLL=Expectation(nodes_ex,lenNodes);
        out= YAP_TermNil();

  for (r=0; r<nRules; r++)
   {
        //printf("Rule %d \n",r);
	 tail=YAP_TermNil();
	 eta_rule=eta[r];		
	 for (i=0;i<rules[r]-1;i++)	
	    {
	        //printf("arrayprob[%d][%d] %lf\n",r,i,arrayprob[r][i]);
	    	 p0=eta_rule[i][0];  //eta^x(i,k)
		// printf("eta_rule[%d][0]= %f \n",i,p0);
	    	 p1=eta_rule[i][1];
		// printf("eta_rule[%d][0]= %f \n",i,p1);
		 term=YAP_MkPairTerm(YAP_MkFloatTerm(p0),YAP_MkPairTerm(YAP_MkFloatTerm(p1),YAP_TermNil()));
		 tail=YAP_MkPairTerm(term,tail);		
 	    }

         ruleTerm=YAP_MkIntTerm(r);//termine: num.regola (es. 1)
	 compoundTerm=YAP_MkPairTerm(ruleTerm,YAP_MkPairTerm(tail,YAP_TermNil()));
	 out=YAP_MkPairTerm(compoundTerm,out);
}

	  //compoundTerms[j]=compoundTerm;
      //////printf("j %d\n\n",j);
	free(nodes_ex);
	free(example_prob);

 out1=YAP_MkFloatTerm(CLL);
   YAP_Unify(out1,arg4);
	//printf("Fine Q\n");
   return (YAP_Unify(out,arg3)); //lista dei par. theta: [[3=regola,[[0.0490418961579503,0.133901982631393]]] 

   }

static int paths_to_non_zero(void)
{
	double paths;
	YAP_Term arg1,arg2,out;
	DdNode * node;
	arg1=YAP_ARG1;
	arg2=YAP_ARG2;
	node=(DdNode *)YAP_IntOfTerm(arg1);
	paths=Cudd_CountPathsToNonZero(node);
	out=YAP_MkFloatTerm(paths);
	return(YAP_Unify(out,arg2));
}
static int paths(void)
{
    double paths;
	YAP_Term arg1,arg2,out;
	DdNode * node;
	arg1=YAP_ARG1;
	arg2=YAP_ARG2;
	node=(DdNode *)YAP_IntOfTerm(arg1);
	paths=Cudd_CountPath(node);
	out=YAP_MkFloatTerm(paths);
	return(YAP_Unify(out,arg2));
}
static int dag_size(void)
{
        int size;
	YAP_Term arg1,arg2,out;
	DdNode * node;
	arg1=YAP_ARG1;
	arg2=YAP_ARG2;
	node=(DdNode *)YAP_IntOfTerm(arg1);
	size=Cudd_DagSize(node);
	out=YAP_MkIntTerm(size);
	return(YAP_Unify(out,arg2));
}

void init_my_predicates()
/* function required by YAP for intitializing the predicates defined by a C function*/
{
	YAP_UserCPredicate("init",init,2);
	YAP_UserCPredicate("init_bdd",init_bdd,0);
	YAP_UserCPredicate("end",end,0);
	YAP_UserCPredicate("end_bdd",end_bdd,0);
	YAP_UserCPredicate("dump_bdd",dump_bdd,2);
	YAP_UserCPredicate("add_var",add_var,4);
	YAP_UserCPredicate("equality",equality,3);
	YAP_UserCPredicate("and",and,3);
	YAP_UserCPredicate("one",one,1);
	YAP_UserCPredicate("zero",zero,1);
	YAP_UserCPredicate("or",or,3);
	YAP_UserCPredicate("bdd_not",bdd_not,2);
	YAP_UserCPredicate("create_dot",create_dot,2);
	YAP_UserCPredicate("expec",expec,2);
	YAP_UserCPredicate("init_test",init_test,1);
	YAP_UserCPredicate("end_test",end_test,0);
	YAP_UserCPredicate("ret_prob",ret_prob,2);
	YAP_UserCPredicate("em",EM,7);
	YAP_UserCPredicate("q",Q,4);
	YAP_UserCPredicate("randomize",randomize,0);
	YAP_UserCPredicate("deref",rec_deref,1);
	YAP_UserCPredicate("garbage_collect",garbage_collect,2);
	YAP_UserCPredicate("bdd_to_add",bdd_to_add,2);
	YAP_UserCPredicate("paths_to_non_zero",paths_to_non_zero,2);
	YAP_UserCPredicate("paths",paths,2);
	YAP_UserCPredicate("dag_size",dag_size,2);
}
 FILE *
open_file(char *filename, const char *mode)
/* opens a file */
{
    FILE *fp;

    if ((fp = fopen(filename, mode)) == NULL) {
        perror(filename);
        exit(1);
    }
    return fp;

}
void reverse(char s[])
/* reverses a string */
{
	int i,c,j;
	for (i=0,j=strlen(s)-1;i<j;i++,j--)
	{
		c=s[i];
		s[i]=s[j];
		s[j]=c;
	}
}

gint my_equal(gconstpointer v,gconstpointer v2)
/* function used by GHashTable to compare two keys */
{
	DdNode *a,*b;
	a=*(DdNode **)v;
	b=*(DdNode **)v2;
	return (a==b);
}
guint my_hash(gconstpointer key)
/* function used by GHashTable to hash a key */
{
	
	return g_direct_hash(*(DdNode **)key);
}
void  dealloc(gpointer key,gpointer value,gpointer user_data)
{
	free(key);
	free(value);
}

tablerow* init_table(int varcnt) {
  int i;
    tablerow *tab;
//    printf("varcnt %d\n",varcnt);
      tab = (tablerow *) malloc(sizeof(rowel) * varcnt);
        for (i = 0; i < varcnt; i++) {
	tab[i].row = NULL;
	tab[i].cnt = 0;
	 }
	return tab;
}


void add_node(tablerow *tab, DdNode *node, double value) {
  int index = Cudd_NodeReadIndex(node);
//  printf("index %d\n",index);
    tab[index].row = (rowel *) realloc(tab[index].row, (tab[index].cnt + 1) * sizeof(rowel));
      tab[index].row[tab[index].cnt].key = node;
      tab[index].row[tab[index].cnt].value = value;
	tab[index].cnt += 1;
}
void add_or_replace_node(tablerow *tab, DdNode *node, double value)
{
  int i;
  int index = Cudd_NodeReadIndex(node);
  //printf("add_or_replace node %x\n",node);
  for(i = 0; i < tab[index].cnt; i++) {
          if (tab[index].row[i].key == node) 
	  {tab[index].row[i].value=value;
	  //printf("replaced\n");
	  return;}
	    }
//printf("added \n");	
    tab[index].row = (rowel *) realloc(tab[index].row, (tab[index].cnt + 1) * sizeof(rowel));
      tab[index].row[tab[index].cnt].key = node;
      tab[index].row[tab[index].cnt].value = value;
	tab[index].cnt += 1;
}

double * get_value(tablerow *tab,  DdNode *node) {
  int i;
  int index = Cudd_NodeReadIndex(node);
      for(i = 0; i < tab[index].cnt; i++) {
          if (tab[index].row[i].key == node)
	  {
//	  	printf("found\n");
	  	return &tab[index].row[i].value;
	}
	    }
	      return NULL;
}

void destroy_table(tablerow *tab,int varcnt)
{
	int i;
        for (i = 0; i < varcnt; i++) {
	free(tab[i].row);
	 }
	free(tab);
}
