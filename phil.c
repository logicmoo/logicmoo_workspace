
/*
This module performs parameter learning on Hierarchical Probabilistic Logic Programs (HPLP)
using gradient descent (dphil) or Expectation Maximization (emphil).

@author Arnaud Nguembang Fadja
@copyright Arnaud Nguembang Fadja
 PHIL: Parameter learning for HIerarchical Probabilistic Logic programs (DPHIL and EMPHIL)
 Copyright (c) 2018, Arnaud Nguembang Fadja
*/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <SWI-Prolog.h>
#include <unistd.h>
#include <sys/stat.h>
//#include <sys/types.h>
#define MaxType 10
#define MaxName 100
#define RETURN_IF_FAIL if (ret!=TRUE) return ret;

typedef struct node_1 {
    char type[MaxType]; // Type of node: can be and,or,not,leaf                         
    double value; // node value.
    int index; // number of the rule for leaf node
    double tp; // the message to propagate to the children in the backward step of EM
    double gradient; // the derivative of the error with respect to the node
    struct node_1 *child;   // point to children of this node
    struct node_1 *next;    // point to next node at same level
}node;
// Gobal variables
double ZERO= 0.0;
int em=0; // contains 1 if the algorithm is emphil and 0 otherwise. usefull during the construction of an AC
int *Counts; // vector of count when performing emphil
int Init=0;  // if 1 InitParameters contains the initial parameters
double *InitParameters; // Contains the initial parameter if Init=1 (Intial values of the parameters are set in the program)

// ++++++++++ Common declaration util functions for Gradient descent and EM +++++++++++++++
node * new_node(double val,char type[]);
node * add_sibling(node * n, double val,char type[]);
node * add_child(node * n, double val,char type[]);
void construct_leaves_node(node **leaves_node,int lenRules);
void printAC(node* root,char*Choice);
double randInRange(double min, double max);
node *convertACToTree(term_t AC);
double product_sum(node*nod,double Probabilities[]);
double product(node*nod,double Probabilities[]);
void forward(double Probabilities[], int NR, node*root);
void printCommonParamameters(double EA,double ER, int MaxIteration,int lenNodes,char*statisticsFolder,char*save,char*seeded,int seed);
int getParameters(term_t Parameters, double Probabilities[],int NR);
int getHyperParameters (term_t Params,term_t StopCond,term_t Folder, int *MaxIter,double* EA,double*  ER,int *NR, double*ZERO,char**statisticsFolder, char**save,char** seeded, int* seed);
int getTrees(term_t Nodes,node ***nodes_ex,int *lenNodes);
int setResults(double CLL, double Probabilities[],int NR,term_t *CLLFinal, term_t *ProbFinal);
void printTree(node* root, FILE* fp);
void printTrees(node**nodes_ex , int lenNodes, char *FileName);
void printTrees2(node**nodes_ex , int init, int max, char *FileName);

// ++++++++++++++++++++++++ Gradient descent declaration functions  ++++++++++++++++++++++++++

void openFilesGD (char * statisticsFolder, FILE**probsFile, FILE** weightsFile,FILE** Moments0File,FILE** Moments1File,FILE** lls);
void closeFilesGD (FILE**probsFile, FILE** weightsFile,FILE** Moments0File,FILE** Moments1File,FILE** lls);
void saveStatisticsGD(double Probabilities[],double Weights [],double Moments0 [],double Moments1 [],int NR, FILE*probsFile, FILE* weightsFile,FILE* Moments0File,FILE* Moments1File,FILE* lls, double CLL);
void sigma_vec(double Weights[],double Probabilities[],int NR);
void initialize_weights_moments(double weights[],double Gradient[],double moments0[],double moments1[],int NR,double Max_W,char* seeded, int seed);
void  printHyperparamsGD(double Eta,double Beta1,double Beta2, long double Adam_hat,double Max_W,int BatchSize,char* strategy);
void backwardGD(double Probabilities[], double Gradient[],int NR, node*root);
void update_weights_Adam(double Weights[], double Gradients[], int NR, double Moment_0[],double Moment_1[],int Iter,double Eta,double Beta1,double Beta2,long double Epsilon_adam_hat);
void nextBatch(int*from,int*to,int lenNodes,int BatchSize,char*strategy);
void normalize_grad(double Gradients[],int NR,int Num_nodes);
double forward_backwardGD(node**Nodes,int lenNodes,int from,int to,double Weights[],double Gradients[],int NR,char *strategy,char* seeded, int seed);
double dphil(node **Nodes,int lenNodes,int MaxIteration,double Probabilities [],double Weights[],int NR,double EA,double ER,double Eta,double Beta1,double Beta2,long double Epsilon_adam_hat,double Max_W,int BatchSize,char*strategy,char* statisticsFolder, char* save,char* seeded, int seed);


// ++++++++++++++++++++++++ Expectation Maximization declaration functions  ++++++++++++++++++++++++++

void openFilesEM (char * statisticsFolder, FILE**probsFile, FILE** expectationsFile,FILE**counts, FILE** lls);
void closeFilesEM (FILE**probsFile, FILE** expectationsFile,FILE** lls);
void initialize_expectations_Probabilities(double Probabilities[],double expectations[],int NR,char* seeded, int seed);
void saveCountsEM(FILE *countsFile,int NR);
void saveStatisticsEM(double Probabilities[],double expectations [],int NR, FILE*probsFile, FILE*expectationsFile, FILE* lls, double CLL);
void backwardEM(double expectations[],int NR, node*root);
double expectation(node**Nodes,int lenNodes,double Probabilities[],double expectations[],int NR);
void maximization(double Probabilities [],double expectations[],int NR);
double emphil(node **Nodes,int lenNodes,double Probabilities[],double expectations[],int NR, int MaxIteration,double EA, double ER,char* statisticsFolder, char* save,char* seeded, int seed);

// ++++++++++++++++++++++++ Parameter learning declaration functions  ++++++++++++++++++++++++++
static foreign_t pl_dphil(term_t Nodes,term_t Params,term_t StopCond, term_t Folder, term_t Adam, term_t Params2, term_t CLLFinal, term_t ProbFinal);
static foreign_t pl_emphil(term_t Nodes,term_t Params,term_t StopCond,term_t Folder, term_t CLLFinal, term_t ProbFinal);

// ++++++++++++++++++++++++ Forward step use in phil.pl  ++++++++++++++++++++++++++
static foreign_t pl_forward(term_t Circuit,term_t Parameters,term_t NR1,term_t Output);


// ++++++++++ Common util functions for Gradient descent and EM +++++++++++++++
node * new_node(double val,char type[]){
    node *new_node = malloc(sizeof(node));

    if ( new_node ){
        new_node->next = NULL;
        new_node->child = NULL;
        new_node->gradient= 0.0;
        new_node->value = val;
        new_node->index =-1;
        strcpy(new_node->type,type);
    }
    return new_node;
}

node * add_sibling(node * n, double val,char type[]){
    if ( n == NULL )
        return NULL;

    while (n->next)
        n = n->next;

    return (n->next = new_node(val,type));
}

node * add_child(node * n, double val,char type[]){
    if ( n == NULL )
        return NULL;

    if ( n->child )
        return add_sibling(n->child, val,type);
    else
        return (n->child = new_node(val,type));
}

void printTree(node* root, FILE* f){
   node* n=NULL;
    if(root!=NULL){
      if(strcmp(root->type,"leaf")!=0 && strcmp(root->type,"zero")!=0 && strcmp(root->type,"one")!=0)
         fprintf(f,"%s([",root->type);
      if(strcmp(root->type,"leaf")==0){
          fprintf(f,"%d",root->index);
      }else{
          if(strcmp(root->type,"zero")==0){
          fprintf(f,"%s",root->type);
          }else{
               if(strcmp(root->type,"one")==0){
               fprintf(f,"%s",root->type);
          }else{
              n=root->child;
              while(n!=NULL){
                printTree(n,f);     
                fprintf(f,",");
                n=n->next; 
              }
            }  
         }   
      }
      if(strcmp(root->type,"leaf")!=0 && strcmp(root->type,"zero")!=0 && strcmp(root->type,"one")!=0)
        fprintf(f,"])");
     }
}

void printTrees(node**nodes_ex , int lenNodes, char *FileName){
   int i;
   FILE *f;
   f=fopen(FileName,"w");
   for(i=0;i<lenNodes;i++){
    printTree(nodes_ex[i],f);
     fprintf(f,"\n");
   }
   fclose(f);
}

void printTrees2(node**nodes_ex , int init, int max, char *FileName){
   int i;
   FILE *f;
   f=fopen(FileName,"w");
   for(i=init;i<max;i++){
    printTree(nodes_ex[i],f);
     fprintf(f,"\n");
   }
   fclose(f);
}

void setSeed(char* seeded, int seed){
  if(strcmp(seeded,"yes")==0 || strcmp(seeded,"Yes")==0 || strcmp(seeded,"YES")==0)
    srand((unsigned)seed);
  else
      srand(time(NULL));
}

void make_directory(const char* name)
{
  #if defined(__linux__) || defined(__APPLE__)
      mkdir(name, 0700);
  #else
      _mkdir(name);
  #endif
}
double randInRange(double min, double max)
{
  return min + (double)rand() / (RAND_MAX / (max - min));
}
// Construct a list of shared leaf node: Not used 
void construct_leaves_node(node **leaves_node,int lenRules){
  int i;
  for(i=0;i<lenRules;i++){
    leaves_node[i]=new_node(0.0,"leaf");
    leaves_node[i]->index=i;
  }
}

// Converts an arithmetic circuit (a term prolog) into n-aries tree in C
node *convertACToTree(term_t AC){
  int ret,ind,arity,i,j,lenNodes1;
  size_t lenNodes;
  atom_t name;
  char *type;
  term_t current_term,current_List_term, temp_term;
  node *root, *ActualChild,*nextChild; //**nodes_ex;

  current_term=PL_new_term_ref();
  temp_term=PL_new_term_ref();
  current_List_term=PL_new_term_ref();
  if(PL_is_compound(AC)){ // if the AC is a compound term like not(..) or(..) and(...) then
      ret=PL_get_compound_name_arity(AC,&name,&arity);
      ret=PL_put_atom(temp_term,name);
      ret=PL_get_atom_chars(temp_term,&type);
      root=new_node(0.0,type);
      for(i=1;i<=arity;i++){ // cycle over the argument: normally there is just one list argument for a particular term. Example: or[..]
       ret=PL_get_arg(i,AC,current_List_term);
       if (PL_is_list(current_List_term))
       {
       ret=PL_skip_list(current_List_term,0,&lenNodes);
       lenNodes1= (int)lenNodes;
       if(lenNodes==1){ // The list has a single term. Example [2]
          ret=PL_get_list(current_List_term,current_term,current_List_term);
          root->child=convertACToTree(current_term);
       }else{// The list has many terms
            ret=PL_get_list(current_List_term,current_term,current_List_term);
            root->child=convertACToTree(current_term);
            ActualChild=root->child;
            for(j=1;j<lenNodes1;j++){
                ret=PL_get_list(current_List_term,current_term,current_List_term);
                nextChild=convertACToTree(current_term);
                ActualChild->next=nextChild;
                ActualChild= nextChild;
            }// end for          
          }// end else lenNodes==1
       }// end convert of a list
       else{
            root->child=convertACToTree(current_List_term);
       } 
     }//end cycle
  }else{ // if the AC is an atomic that is an integer or the atoms one/zero or other atoms
    if(PL_is_integer(AC)){ 
     ret=PL_get_integer(AC,&ind);
     root=new_node(0.0,"leaf");
     root->index=ind;
     if(em==1)
      Counts[ind]++;
    }else{
     ret=PL_get_atom_chars(AC,&type);
     root=new_node(0.0,type);
     if(strcmp(type,"zero")==0){ 
       root->value=0.0; root->index=-1; 
     } 
     else
     {
        if(strcmp(type,"one")==0){
          root->value=1.0; root->index=-2;
        }else{
            root->value=1.0; root->index=-3;  
        }
     }
    } 
  }// end atomic
  ret=ret*2; // to avoid warnings message
  return root;
}

// computes the oplus of sibling values: oplus activation function
double product_sum(node*nod,double Probabilities[]){
   double prod_s=1;
   int index;
   node * n=nod;
   while (n!=NULL){
     if(strcmp(n->type,"leaf")==0){
        index=n->index;
        prod_s=prod_s*(1-Probabilities[index]);
     }else
     {
        prod_s=prod_s*(1-(n->value));
     }
     n = n->next;
  }
  return 1-prod_s;
}

// Compute the product of sibling values: X activation function
double product(node*nod,double Probabilities[]){
   double prod=1;
   int index;
   node * n=nod;
   while (n!=NULL){
     if(strcmp(n->type,"leaf")==0){
        index=n->index;
        prod=prod*Probabilities[index];
     }else
     {
        prod=prod*(n->value);
     }
     n = n->next;
  }
  return prod;
}
// Prints common hyperparameters 
void  printCommonParamameters(double EA,double ER, int MaxIteration,int lenNodes,char*statisticsFolder,char*save,char*seeded,int seed){
  printf("\n Hyperparameters of %s:\n\n", statisticsFolder);
  printf("Save statistics? %s\n",save);
  printf("Number of arithmetic circuits: %d \n",lenNodes);
  printf("Max Iteration= %d \n",MaxIteration);
  printf("Epsilon= %lf \n",EA);
  printf("Delta= %lf \n",ER);
  printf("Value used as zero= %.9lf\n",ZERO);
  if(strcmp(seeded,"yes")==0 || strcmp(seeded,"Yes")==0 || strcmp(seeded,"YES")==0)
  {
    printf("Seed =%d \n",seed);
  }  
   else
  {
    printf("Seed = Time clock \n");
  } 
}

// The forward pass evaluates the tree
void forward(double Probabilities[], int NR, node*root){
  node * n;
  int index;
 if(root!=NULL){
    if(strcmp(root->type,"not")==0){
        if(root->child!=NULL){
            forward(Probabilities,NR,root->child);
            root->value=1-(root->child)->value;
         }
    }else{
        if(strcmp(root->type,"or")==0){
         // iterate on sibling
         n=root->child;
         while (n!=NULL){
           forward(Probabilities,NR,n);
           n = n->next;
         }
         root->value=product_sum(root->child,Probabilities);
       }else{
          if(strcmp(root->type,"and")==0){
         // iterate on sibling
         n=root->child;
         while (n!=NULL){
           forward(Probabilities,NR,n);
           n = n->next;
        }
        root->value=product(root->child,Probabilities);
       }else{ // leaf node;
           if(strcmp(root->type,"leaf")==0){
            index=root->index;
            root->value=Probabilities[index];
           }
           else
           {
            if(strcmp(root->type,"zero")==0){
                root->value=0.0;
              }else{
                     root->value=1.0;  
                 }
           }
         }
       }
     }
  }else{
   perror("Forward pass:NULL node");
 }  
}


int setResults(double CLL, double Probabilities[],int NR,term_t *CLLFinal, term_t *ProbFinal){
  term_t out1,out2,pterm;
  int i,ret;
  pterm=PL_new_term_ref();
  out1=PL_new_term_ref();
  out2=PL_new_term_ref();
  ret=PL_put_float(out1,CLL);
  ret=PL_unify(*CLLFinal,out1);

  ret=PL_put_nil(out2);
  for(i=NR-1;i>=0;i--){
     ret=PL_put_float(pterm,Probabilities[i]);
     ret=PL_cons_list(out2,pterm,out2);
  }
  ret=PL_unify(out2,*ProbFinal);
  return ret;
}
int getParameters( term_t Parameters, double Probabilities[],int NR){
  int i,ret=0;
  term_t ParamatersTerm, head;
  head=PL_new_term_ref();
  ParamatersTerm=PL_copy_term_ref(Parameters); 
  for(i=0;i<NR;i++){
    ret=PL_get_list(ParamatersTerm,head,ParamatersTerm);
    ret=PL_get_float(head,&Probabilities[i]);
  }
  return ret;
}
// Get the hyperparameters for learning
int getHyperParameters(term_t Params,term_t StopCond,term_t Folder, int *MaxIter,double* EA,double*  ER,int *NR, double*ZERO,char**statisticsFolder, char**save,char**seeded, int*seed){
  int ret; 
  term_t nodesTerm1,nodesTerm2,nodesTerm3,head;
  char  *initialized;
  head=PL_new_term_ref();
  //get the arguments

  nodesTerm1=PL_copy_term_ref(Params); // Contains the Stop criterion and
  nodesTerm2=PL_copy_term_ref(StopCond); // Contains the Stop criterion and 
  nodesTerm3=PL_copy_term_ref(Folder);  // Contains the name of the folder and the save flag (yes or no)
  
  // Get the number of rule and the Zero setting
  ret=PL_get_list(nodesTerm1,head,nodesTerm1);
  ret=PL_get_integer(head,NR);
  // Get the zero value
  ret=PL_get_list(nodesTerm1,head,nodesTerm1);
  ret=PL_get_float(head,ZERO);
   // Get the zero value
  ret=PL_get_list(nodesTerm1,head,nodesTerm1);
  ret=PL_get_atom_chars(head, seeded);
   // Get the zero value
  ret=PL_get_list(nodesTerm1,head,nodesTerm1);
  ret=PL_get_integer(head,seed);

  // get the initial parameters if set
  ret=PL_get_list(nodesTerm1,head,nodesTerm1);
  ret=PL_get_atom_chars(head,&initialized);
  //ret=PL_get_chars(head,&initialized,CVT_STRING);
  
  if(strcmp(initialized,"yes")==0 || strcmp(initialized,"Yes")==0 || strcmp(initialized,"YES")==0){
    Init=1;
    InitParameters=(double*)malloc((*NR)*sizeof(double));
    ret=PL_get_list(nodesTerm1,head,nodesTerm1);
    getParameters(head,InitParameters,*NR);
  }else
    Init=0;
  
  // Get the stop conditions
  ret=PL_get_list(nodesTerm2,head,nodesTerm2);
  ret=PL_get_integer(head,MaxIter);
  ret=PL_get_list(nodesTerm2,head,nodesTerm2);
  ret=PL_get_float(head,EA);
  ret=PL_get_list(nodesTerm2,head,nodesTerm2);
  ret=PL_get_float(head,ER);
 
  // Get the statistic folder and a flag for saving statistics
  ret=PL_get_list(nodesTerm3,head,nodesTerm3);
  ret=PL_get_atom_chars(head,save);
  //ret=PL_get_chars(head, save,CVT_STRING);
  ret=PL_get_list(nodesTerm3,head,nodesTerm3);
  ret=PL_get_atom_chars(head,statisticsFolder);
  //ret=PL_get_chars(head, statisticsFolder,CVT_STRING);
 return ret;
}

// Given the ACs in Nodes, converts them in trees saved in nodes_ex. lenNodes return the number of trees
int getTrees(term_t Nodes,node ***nodes_ex,int *lenNodes){
  term_t nodesTerm, head;
  int i,ret;
  size_t lenNodes1;
  head=PL_new_term_ref();
  nodesTerm=PL_copy_term_ref(Nodes); // Contains the Arithmetic Circuits
  ret=PL_skip_list(Nodes,0,&lenNodes1);

  *lenNodes=(int)lenNodes1;
  *nodes_ex=(node **)malloc((*lenNodes)*sizeof(node*));
  for(i=0;i<*lenNodes;i++){
    ret=PL_get_list(nodesTerm,head,nodesTerm);
    (*nodes_ex)[i]=convertACToTree(head);
  }
  return ret;
}


// ++++++++++++++++++++++++ Gradient descent functions  ++++++++++++++++++++++++++

 // Creates if it does not exist the directory for the dataset and also create the necessary files 
void openFilesGD (char * statisticsFolder, FILE**probsFile, FILE** weightsFile,FILE** Moments0File,FILE** Moments1File,FILE** lls){
  struct stat st = {0};
  char statisticsFolder2 [MaxName];
  char nameFileProbs [40]="./";
  char nameFileWeights [40]="./";
  char nameFileClls [40]="./";
  char nameFileMoments0 [40]="./";
  char nameFileMoments1 [40]="./";
  strcpy(statisticsFolder2, statisticsFolder);
  strcat(statisticsFolder, "_Statistics/GD");
   // create the directory to save information
  if (stat(statisticsFolder, &st) == -1) {
    strcat(statisticsFolder2, "_Statistics");
     if (stat(statisticsFolder2, &st) == -1) {
       make_directory(statisticsFolder2);
       strcat(statisticsFolder2, "/GD");
       make_directory(statisticsFolder2);
     }else{
         make_directory(statisticsFolder);
     }  
  }

  strcat(nameFileProbs,statisticsFolder); strcat(nameFileProbs,"/Probabilities.txt");
  strcat(nameFileWeights,statisticsFolder); strcat(nameFileWeights,"/Weights.txt");
  strcat(nameFileClls,statisticsFolder); strcat(nameFileClls,"/clls.txt");
  strcat(nameFileMoments0,statisticsFolder); strcat(nameFileMoments0,"/Moments0.txt");
  strcat(nameFileMoments1,statisticsFolder); strcat(nameFileMoments1,"/Moments1.txt");

  *probsFile = fopen(nameFileProbs,"w");
  *weightsFile= fopen(nameFileWeights,"w");
  *lls=fopen(nameFileClls,"w");
  *Moments0File=fopen(nameFileMoments0,"w");
  *Moments1File=fopen(nameFileMoments1,"w");

  if(*probsFile==NULL || *weightsFile==NULL || *lls==NULL || *Moments0File==NULL || *Moments1File==NULL){
    printf("Problem in opening a file");
    exit(1);
  }
}
// Closes all the opened files
void closeFilesGD (FILE**probsFile, FILE** weightsFile,FILE** Moments0File,FILE** Moments1File,FILE** lls){
  fclose(*probsFile);
  fclose(*weightsFile);
  fclose(*Moments0File);
  fclose(*Moments1File);
  fclose(*lls);
}

//Saves the log-likelihood (CLL), the moments and the learned weights/parameters
void saveStatisticsGD(double Probabilities[],double Weights [],double Moments0 [],double Moments1 [],int NR, FILE*probsFile, FILE* weightsFile,FILE* Moments0File,FILE* Moments1File,FILE* lls, double CLL){
  int i;
  for(i=0; i<NR; i++){
     fprintf(probsFile,"%f ",Probabilities[i]);
     fprintf(weightsFile,"%f ",Weights[i]);
     fprintf(Moments0File,"%f ",Moments0[i]);
     fprintf(Moments1File,"%f ",Moments1[i]);
  }
  fprintf(lls,"%f \n",CLL);
  fprintf(probsFile,"\n \n");
  fprintf(weightsFile,"\n \n");
  fprintf(Moments0File,"\n \n");
  fprintf(Moments1File,"\n \n");
}

// Applies the sigmoid function to the weights.
void sigma_vec(double Weights[],double Probabilities[],int NR){
  int i;
  for(i=0;i<NR;i++){
   Probabilities[i]=1/(1+exp(-Weights[i]));
  }
}

// Applies the inverse of  sigmoid function to a single probability.
double sigma_vec_inv(double Probability){
  double temp;
   if(Probability==0)
      temp=1/ZERO;
   else
      temp=(1-Probability)/Probability;
    if(temp==1)
       temp=0.9999999999;
    if (temp <=0)
        temp=ZERO;
   return -log(temp);
}

// Initializes the weights the moments and the gradients
void initialize_weights_moments(double weights[],double Gradient[],double moments0[],double moments1[],int NR,double Max_W,char*seeded,int seed){
  int i;
  setSeed(seeded,seed);
    for(i=0;i<NR;i++){
        if(Init==1){
            weights[i]=sigma_vec_inv(InitParameters[i]);
        }else{
            weights[i]=randInRange(-Max_W,Max_W);
        }
        moments0[i]=0.0;
        moments1[i]=0.0;
        Gradient[i]=0.0;
    }
}

// Print the hyperparameters for gradient descent
void  printHyperparamsGD(double Eta,double Beta1,double Beta2, long double Adam_hat,double Max_W,int BatchSize,char* strategy){
  if(Init==1){
     printf("Parameters are initialized with the initial program parameters\n");
  }else{
      printf("Parameters are initialized with sigma of weights in the range [%.2lf %.2lf]\n",-Max_W,Max_W);
  }
  printf("Eta= %lf \n",Eta);
  printf("Beta1= %lf \n",Beta1);
  printf("Beta2= %lf \n",Beta2);
  printf("Adam_hat= %.10Lf  \n",Adam_hat);
  printf("BatchSize= %d \n",BatchSize);
  printf("Strategy= %s \n",strategy);
}

// Performs the backwardGD pass to compute the gradient.
void backwardGD(double Probabilities[], double Gradient[],int NR, node*root){
 node * n;
 double Child_Gradient,Root_Gradient;
 
 if(root!=NULL){
  Root_Gradient=root->gradient;
  if(strcmp(root->type,"not")==0){
    n=root->child;
    while (n!=NULL){
      n->gradient=-Root_Gradient;
      backwardGD(Probabilities,Gradient,NR,n);
      n = n->next;
    }
  }else{
      if(strcmp(root->type,"or")==0){
       // iterate over sibling
       n=root->child;
       while (n!=NULL){
         if(n->value!=0)
            Child_Gradient=Root_Gradient*(root->value/n->value);
         else
            Child_Gradient=Root_Gradient*(root->value/ZERO);
         n->gradient=Child_Gradient;
         backwardGD(Probabilities,Gradient,NR,n);
         n = n->next;
       }
     }else{
       if(strcmp(root->type,"and")==0){
         // iterate on sibling
         n=root->child;
         while (n!=NULL){       
           if(strcmp(n->type,"leaf")==0){
             Child_Gradient=Root_Gradient*(root->value)*(1-(n->value));
             n->gradient=Child_Gradient;
             backwardGD(Probabilities,Gradient,NR,n); // propagate the parent value
           }else{
              if(1-(n->value)!=0)
                Child_Gradient=Root_Gradient*((1-(root->value))/1-(n->value));
              else
                Child_Gradient=Root_Gradient*((1-(root->value))/ZERO);
              n->gradient=Child_Gradient;
              backwardGD(Probabilities,Gradient,NR,n);
           }
           n = n->next;
         }
     }else{
           if(strcmp(root->type,"leaf")==0){
              int index=root->index;
              Gradient[index]=Gradient[index]+ root->gradient;
           }
     }
   }
 }
 }else{
    perror("backwardGD pass:NULL node");
  }
}

// Updates weights with Adam techniques
void update_weights_Adam(double Weights[], double Gradients[], int NR, double Moment_0[],double Moment_1[],int Iter,double Eta,double Beta1,double Beta2,long double Epsilon_adam_hat){
  int Iter_new,i;
  double Result1,Result2,Eta_new;
  Iter_new =Iter+1;
  Result1=sqrt(1-pow(Beta2,Iter_new));
  if((1-pow(Beta1,Iter_new))!=0)
    Eta_new=Eta*Result1/(1-pow(Beta1,Iter_new));
  else
    Eta_new=Eta;
  for(i=0;i<NR;i++){
     Moment_0[i]=Beta1*Moment_0[i] + (1-Beta1)*Gradients[i];
     Moment_1[i]=Beta2*Moment_1[i] + (1-Beta2)*Gradients[i]*Gradients[i];
     Result2=sqrt(Moment_1[i]);
     Weights[i]=Weights[i]-Eta_new*Moment_0[i]/(Result2+Epsilon_adam_hat);
     // reinitialize the gradient after updating
     Gradients[i]=0.0;
  }
}
  // This function computes the range index of the next batch 
void nextBatch(int*from,int*to,int lenNodes,int BatchSize,char*strategy){
  int from1=*from,to1=*to;
  if(strcmp(strategy,"minibatch")==0){
    if(from1+BatchSize>=lenNodes){
      from1=0;to1=BatchSize;
    }else{
      if(to1+BatchSize>=lenNodes){
        from1=to1;to1=lenNodes;
      }else{
        from1+=BatchSize;to1+=BatchSize;
      }
    }
  }else{//end minibatch
    if(strcmp(strategy,"stochastic")==0){
        from1=0; to1=BatchSize;
    }else{
      if(strcmp(strategy,"batch")==0){
        from1=0; to1=lenNodes;
      }else{
        perror("strategy not found");
      }
    }
  }
*from=from1; *to=to1;
}
// Normalizes the gradient
void normalize_grad(double Gradients[],int NR,int Num_nodes){
  int i;
  for(i=0;i<NR;i++){
    Gradients[i]=Gradients[i]/Num_nodes;
  }
}
// Performs the forward and the backwardGD steps
double forward_backwardGD(node**Nodes,int lenNodes,int from,int to,double Weights[],double Gradients[],int NR,char *strategy,char* seeded, int seed){
  double Root_Value;
  double Probabilities[NR],CLL=0;
  int i,index;
  setSeed(seeded,seed);
  sigma_vec(Weights,Probabilities,NR);
  for(i=from;i<to;i++){
     if(strcmp(strategy,"stochastic")==0){
        index=rand()%lenNodes; // generate a number between 0 and lenNodes-1
     }else{
       index=i;
     }
     //printf("\nIndex= %d",index);
    forward(Probabilities,NR,Nodes[index]);
    Root_Value=Nodes[index]->value;
    if(Root_Value!=0){
      CLL=CLL+log(Root_Value);
      Nodes[index]->gradient=-1/(Root_Value);
    }else{
      CLL=CLL+log(ZERO);
      Nodes[index]->gradient=-1/ZERO; 
    }  
    // backwardGD pass
    backwardGD(Probabilities,Gradients,NR,Nodes[index]);
  }
  return CLL;
}
//Performs gradient descent 
double dphil(node **Nodes,int lenNodes,int MaxIteration,double Probabilities [],double Weights[],int NR,double EA,double ER,double Eta,double Beta1,double Beta2,long double Epsilon_adam_hat,double Max_W,int BatchSize,char*strategy,char* statisticsFolder, char* save,char* seeded, int seed){
  double Gradients[NR],Moments0[NR],Moments1[NR];
  int from,to,Iter,saved=0;
  double CLL0= -2.2*pow(10,10); //-inf
  double CLL1= -1.7*pow(10,8);  //+inf
  double ratio,diff,MaxIteration1=MaxIteration;
  FILE*probsFile,*weightsFile, *lls, *Moments0File, *Moments1File; 
  
  if(BatchSize >lenNodes){ // if the batch size is greater than the training set, consider the cardinality of the training set
      BatchSize=lenNodes;
  }
  
  diff=fabs(CLL1-CLL0);
  ratio=diff/fabs(CLL0);
  if (MaxIteration==-1)
    MaxIteration1= 2147000000;
  Iter=0;
  if(strcmp(save,"Yes")==0 || strcmp(save,"yes")==0 || strcmp(save,"YES")==0 || strcmp(save,"YeS")==0)
     saved=1;
  initialize_weights_moments(Weights,Gradients,Moments0,Moments1,NR,Max_W,seeded,seed);
  if(saved==1){
      openFilesGD (statisticsFolder,&probsFile, &weightsFile,&Moments0File,&Moments1File,&lls);
        sigma_vec(Weights,Probabilities,NR);
        saveStatisticsGD(Probabilities,Weights,Moments0,Moments1,NR,probsFile,weightsFile,Moments0File,Moments1File,lls,CLL1);
            
  }
  from=0; to=BatchSize;
  while(Iter<MaxIteration1 && diff>EA && ratio>ER){
      CLL0 = CLL1;
      CLL1=forward_backwardGD(Nodes,lenNodes,from,to,Weights,Gradients,NR,strategy,seeded,seed);
      normalize_grad(Gradients,NR,to-from+1);
      diff=fabs(CLL1-CLL0);
      ratio=diff/fabs(CLL0);
      update_weights_Adam(Weights,Gradients,NR,Moments0,Moments1,Iter,Eta,Beta1,Beta2,Epsilon_adam_hat);
      sigma_vec(Weights,Probabilities,NR);
      if(saved==1)
        saveStatisticsGD(Probabilities,Weights,Moments0,Moments1,NR,probsFile,weightsFile,Moments0File,Moments1File,lls,CLL1);
      Iter++;
      nextBatch(&from,&to,lenNodes,BatchSize,strategy);
  }//end while
  if(saved==1)
    closeFilesGD (&probsFile, &weightsFile,&Moments0File,&Moments1File,&lls);
  return CLL1;
}

// ++++++++++++++++++++++++ Expectation Maximization functions  ++++++++++++++++++++++++++


 // Creates if it does not exist the directory for the dataset and also create the necessary files 
void openFilesEM (char * statisticsFolder, FILE**probsFile, FILE** expectationsFile, FILE**countsFile ,FILE** lls)
{
  struct stat st = {0};
  char statisticsFolder2 [MaxName];
  char nameFileProbs [100]="./";
  char nameFileExpects [100]="./";
  char nameFileCount [100]="./";
  char nameFileClls [100]="./";
 strcpy(statisticsFolder2, statisticsFolder);

  strcat(statisticsFolder, "_Statistics/EM");
   // create the directory to save information
  if (stat(statisticsFolder, &st) == -1) {
    strcat(statisticsFolder2, "_Statistics");
     if (stat(statisticsFolder2, &st) == -1) {
       make_directory(statisticsFolder2);
       strcat(statisticsFolder2, "/EM");
       make_directory(statisticsFolder2);
     }else{
         make_directory(statisticsFolder);
     }  
  }
 strcat(nameFileProbs,statisticsFolder); strcat(nameFileProbs,"/probabilitiesEM.txt");
  strcat(nameFileExpects,statisticsFolder); strcat(nameFileExpects,"/expectationsEM.txt");
  strcat(nameFileCount,statisticsFolder); strcat(nameFileCount,"/countsEM.txt");
  strcat(nameFileClls,statisticsFolder); strcat(nameFileClls,"/cllsEM.txt");
  *probsFile = fopen(nameFileProbs,"w");
  *expectationsFile= fopen(nameFileExpects,"w");
  *countsFile=fopen(nameFileCount,"w"); 
  *lls=fopen(nameFileClls,"w");
  
  if(*probsFile==NULL || *expectationsFile==NULL || *countsFile==NULL || *lls==NULL){
    printf("File opening problem");
    exit(1);
  }
}

void closeFilesEM (FILE**probsFile, FILE** expectationsFile,FILE** lls){
  fclose(*probsFile);
  fclose(*expectationsFile);
  fclose(*lls);
}

void saveCountsEM(FILE *countsFile,int NR){
    int i;
    for(i=0; i<NR; i++){
     fprintf(countsFile,"%d ",Counts[i]);
  }
}
//Saves the log-likelihood (CLL), The expectations and the count.
void saveStatisticsEM(double Probabilities[],double expectations [],int NR, FILE*probsFile, FILE*expectationsFile, FILE* lls, double CLL){
  int i;
  for(i=0; i<NR; i++){
     fprintf(probsFile,"%f ",Probabilities[i]);
     fprintf(expectationsFile,"%f ",expectations[i]);
     expectations[i]=0; // reinitialized the expectations
  }
  fprintf(lls,"%f \n",CLL);
  fprintf(probsFile,"\n \n");
  fprintf(expectationsFile,"\n \n");
}

void initialize_Counts(int NR){
    int i;
    for(i=0;i<NR;i++){
        Counts[i]=0; 
    }
}

// initialize the probabilities the expectations and the counters 
void initialize_expectations_Probabilities(double Probabilities[],double expectations[],int NR,char *seeded,int seed){
    int i;
    setSeed(seeded,seed);
    for(i=0;i<NR;i++){
        if(Init==1){
            Probabilities[i]=InitParameters[i];
        }else{
            Probabilities[i]=randInRange(0,1);
        }
        expectations[i]=0.0; 
    }
}

// Performs the backward step in the message passing
void backwardEM(double expectations[], int NR, node*root){
 node * n;
 double tn,tp,temp,denominator;
 if(root!=NULL){
  tp=root->tp;
  if(strcmp(root->type,"not")==0){
    n=root->child;
    while (n!=NULL){
       n->tp=1-tp;
      backwardEM(expectations,NR,n);
      n = n->next;
    }
  }else{
      if(strcmp(root->type,"or")==0){
       n=root->child;
       while (n!=NULL){
         if((1-n->value)!=0)
          temp=1-(1-root->value)/(1-n->value); // to compute v(node) \ominus v(n)
         else
          temp=1-(1-root->value)/ZERO; // to compute v(node) \ominus v(n)

         denominator=tp+temp*tp+(1-temp)*(1-tp);
         if(denominator!=0){
            tn=tp/denominator;
           }
          else{
            tn=tp/ZERO;
          }
         n->tp=tn;
         backwardEM(expectations,NR,n);
         n = n->next;
       }
     }else{
       if(strcmp(root->type,"and")==0){
         n=root->child;
         while (n!=NULL){
            if((n->value)!=0)
              temp=root->value/(n->value);
            else
               temp=root->value/ZERO;
              denominator=tp*temp+(1-tp)*(1-temp)+1-tp;
              
            if(denominator!=0){
              tn=(tp*temp+(1-tp)*(1-temp))/denominator;
              }
            else{            
              tn=(tp*temp+ (1-tp)*(1-temp))/ZERO;
              }
            n->tp=tn;
            backwardEM(expectations,NR,n); 
            n = n->next;
         }
     }else{
           if(strcmp(root->type,"leaf")==0){
              int index=root->index;
              double pi=root->value, tp=root->tp; 
              denominator=pi*tp+(1-pi)*(1-tp);
              if(denominator!=0){
                  temp=(pi*tp)/denominator;
                 }
              else{
                temp=(pi*tp)/ZERO;
              }
              if(index<NR && index >=0){
                  expectations[index]=expectations[index]+ temp;
              }
           }
     }
   }
 }
 }else{
    perror("backwardEM pass:NULL node");
  }
}

// Computes the expectations of each parameters and the CLL.
double expectation(node**Nodes,int lenNodes,double Probabilities[],double expectations[],int NR){
  double Root_Value,CLL=0;
  int i;
  for(i=0;i<lenNodes;i++){
    // forward pass
    forward(Probabilities,NR,Nodes[i]);
    Root_Value=Nodes[i]->value;
    if(Root_Value!=0){
      CLL=CLL+log(Root_Value);
    }
    else 
       CLL=CLL+ log(ZERO);
    Nodes[i]->tp=1.0; // the initial message from the root must be 1 (see the paper emphil)
    backwardEM(expectations,NR,Nodes[i]);
  }
  return CLL;
}

// Maximization step: computes new values the probabilities 
void maximization(double Probabilities [],double expectations[],int NR){
    int i;
    for(i=0;i<NR;i++){
      if(Counts[i]!=0){
        Probabilities[i]=expectations[i]/Counts[i];
      }else
           Probabilities[i]=0.0;
    }
}

// Performs Expectation Maximization
double emphil(node **Nodes,int lenNodes,double Probabilities[],double expectations[],int NR, int MaxIteration,double EA, double ER,char* statisticsFolder, char* save,char* seeded, int seed)
{
  int Iter, saved=0;
  double CLL0= -2.2*pow(10,10); //-inf
  double CLL1= -1.7*pow(10,8);  //+inf
  double ratio,diff;
  int MaxIteration1;
  FILE*probsFile,*expectationsFile,*countsFile, *lls;
  diff=fabs(CLL1-CLL0);
  ratio=diff/fabs(CLL0);

  if (MaxIteration==-1)
    MaxIteration1= 2147000000;
  else 
    MaxIteration1=MaxIteration;

  if(strcmp(save,"Yes")==0 || strcmp(save,"yes")==0 || strcmp(save,"YES")==0 || strcmp(save,"YeS")==0)
     saved=1;
  initialize_expectations_Probabilities(Probabilities,expectations,NR,seeded,seed);
  if(saved==1){ 
    openFilesEM (statisticsFolder,&probsFile,&expectationsFile,&countsFile,&lls);
    saveCountsEM(countsFile,NR);
    fclose(countsFile);

    saveStatisticsEM(Probabilities,expectations,NR,probsFile,expectationsFile,lls,CLL1);
  }
   Iter=0;
  while(Iter<MaxIteration1 && diff>EA && ratio>ER){
      CLL0 = CLL1;
      //Expectation step
      CLL1=expectation(Nodes,lenNodes,Probabilities,expectations,NR);
      //Maximization step
      maximization(Probabilities,expectations,NR);
      // save the current statistics
      if(saved==1)
        saveStatisticsEM(Probabilities,expectations,NR,probsFile,expectationsFile,lls,CLL1);
      diff=fabs(CLL1-CLL0);
      ratio=diff/fabs(CLL0);
      Iter++;
  }
  // close the different files
  if(saved==1)
    closeFilesEM(&probsFile,&expectationsFile,&lls);
  return CLL1;
}

// ++++++++++++++++++++++++ Parameter learning function  ++++++++++++++++++++++++++


// Learns the parameters of HPLP using Gradient Descent
static foreign_t pl_dphil(term_t Nodes,term_t Params,term_t StopCond, term_t Folder, term_t Adam, term_t Params2, term_t CLLFinal, term_t ProbFinal){ // 
  int ret,NR,MaxIter,lenNodes,BatchSize,seed;
  char *statisticsFolder, *save,*strategy, *seeded;
  node **nodes_ex=NULL;
  double *Probabilities,  *Weights;
  double EA,ER,CLL,Max_W,Eta,Beta1,Beta2,Adam_hat;
  term_t nodesTerm1,nodesTerm2,head;
  head=PL_new_term_ref();
  // get common hyperparameters between dphil emphil
  ret=getHyperParameters(Params,StopCond,Folder,&MaxIter,&EA,&ER,&NR, &ZERO,&statisticsFolder,&save,&seeded, &seed);
  // get other parameters
  nodesTerm1=PL_copy_term_ref(Adam); // Contains the list of Adam arguments
  nodesTerm2=PL_copy_term_ref(Params2); // 
  //Get the adam parameters
  ret=PL_get_list(nodesTerm1,head,nodesTerm1);
  ret=PL_get_float(head,&Eta);
  ret=PL_get_list(nodesTerm1,head,nodesTerm1);
  ret=PL_get_float(head,&Beta1);
  ret=PL_get_list(nodesTerm1,head,nodesTerm1);
  ret=PL_get_float(head,&Beta2);
  ret=PL_get_list(nodesTerm1,head,nodesTerm1);
  ret=PL_get_float(head,&Adam_hat);
 
  // Get the strategy (minibatch, stochastic, batch)
  ret=PL_get_list(nodesTerm2,head,nodesTerm2);
  ret=PL_get_atom_chars(head,&strategy);
  //ret=PL_get_chars(head, &strategy,CVT_STRING);
  // Get the BatchSize
  ret=PL_get_list(nodesTerm2,head,nodesTerm2);
  ret=PL_get_integer(head,&BatchSize);
  // Get the maximun initial weights
  ret=PL_get_list(nodesTerm2,head,nodesTerm2);
  ret=PL_get_float(head,&Max_W);

  // Convert all the ACs in trees 
  ret=getTrees(Nodes,&nodes_ex,&lenNodes);
  // Write the trees in the file Tree.txt 
  //printTrees(nodes_ex,lenNodes,"Trees.txt");
  // Print the hyperparameters of dphil
  printCommonParamameters(EA,ER,MaxIter,lenNodes,statisticsFolder,save,seeded,seed);
  printHyperparamsGD(Eta,Beta1,Beta2,Adam_hat,Max_W,BatchSize,strategy);
  // Performs parameter learning with gradient descent descent// Performs parameter learning with gradient descent descent
 Probabilities=(double*)malloc(NR*sizeof(double));
 Weights=(double*)malloc(NR*sizeof(double));
 CLL=dphil(nodes_ex,lenNodes,MaxIter,Probabilities,Weights,NR,EA,ER,Eta,Beta1,Beta2,Adam_hat,Max_W,BatchSize,strategy,statisticsFolder,save,seeded, seed);  // return the CLL and the learned probabilities
 // return the CLL and the learned probabilies
 ret=setResults(CLL,Probabilities,NR,&CLLFinal, &ProbFinal);
 return ret;
}

// Learns the parameters of HPLP using  Expectation Maximization and the return 
//the last CLL in CLLFinal and the learned probabilities in ProbFinal
static foreign_t pl_emphil(term_t Nodes,term_t Params,term_t StopCond,term_t Folder, term_t CLLFinal, term_t ProbFinal){ // 
  int ret,NR,MaxIter,lenNodes,seed;
  char *statisticsFolder, *save,*seeded;
  double *Probabilities,  *Expectations;
  double EA,ER,CLL;
  node **nodes_ex=NULL;
  em=1; // activate the emphil algorithm em is a global variable
  ret=getHyperParameters(Params,StopCond,Folder,&MaxIter,&EA,&ER,&NR, &ZERO,&statisticsFolder,&save,&seeded, &seed);
  Probabilities=(double*)malloc(NR*sizeof(double));
  Counts=(int*)malloc(NR*sizeof(int));
  Expectations=(double*)malloc(NR*sizeof(double));
   // Convert the ACs into Trees
  initialize_Counts(NR);
  ret=getTrees(Nodes,&nodes_ex,&lenNodes);
  // Write the trees a file 
  //printTrees(nodes_ex,lenNodes,"Trees.txt");
   // Print the Hyperparameters
  printCommonParamameters(EA,ER,MaxIter,lenNodes,statisticsFolder,save,seeded,seed);

  // Performs parameter learning with EM
  CLL=emphil(nodes_ex,lenNodes,Probabilities,Expectations,NR,MaxIter,EA,ER,statisticsFolder,save,seeded,seed);
  // return the CLL and the learned probabilities
  ret=setResults(CLL,Probabilities,NR,&CLLFinal, &ProbFinal);
  return ret;
}
// Given a single AC Circuit, the list of parameters and the number of rules NR, evaluates the AC 
// and output the result in Output
static foreign_t pl_forward(term_t Circuit,term_t Parameters,term_t NR,term_t Output){
  node * root;
  int NR1,ret;
  double *Probabilities;
  term_t  out;
  out=PL_new_term_ref();
  ret=PL_get_integer(NR,&NR1);
  Probabilities=(double*)malloc(NR*sizeof(double));
  ret=getParameters(Parameters,Probabilities,(int)NR1);
  root=convertACToTree(Circuit);
  forward(Probabilities,(int)NR1,root);
  // return the computed value
  ret=PL_put_float(out,root->value);
  ret=PL_unify(Output,out);
  return ret;
}

install_t
install()
{ 
  PL_register_foreign("dphil",8, pl_dphil, 0);
  PL_register_foreign("emphil",6, pl_emphil, 0);
  PL_register_foreign("forward",4, pl_forward, 0);
}
