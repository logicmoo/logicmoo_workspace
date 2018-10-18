
/*
This module performs parameter learning on Hierarchical Probabilistic Logic Programs (HPLP)
using gradient descent (dphil) or Expectation Maximization (emphil).

@author Arnaud Nguembang Fadja
@copyright Arnaud Nguembang Fadja
 PHIL: Parameter learning for HIerarchical Probabilistic Logic programs
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
#define MaxName 50
#define RETURN_IF_FAIL if (ret!=TRUE) return ret;

double ZERO= 0.0;
typedef struct node_1 {
    char type[MaxType]; // Type of node: can be and,or,not,leaf                         
    double value; // node value.
    int index; // number of the rule for leaf node
    double tp; // the message to propagate to the children in the backward step of EM
    double gradient; // the derivative of the error with respect to the node
    struct node_1 *child;   // point to children of this node
    struct node_1 *next;    // point to next node at same level
}node;

// ++++++++++ Common declaration util functions for Gradient descent and EM +++++++++++++++
node * new_node(double val,char type[]);
node * add_sibling(node * n, double val,char type[]);
node * add_child(node * n, double val,char type[]);
void construct_leaves_node(node **leaves_node,int lenRules);
void printAC(node* root,char*Choice);
double randInRange(double min, double max);
node *convertACToTree(term_t AC,node **leaves_node,int lenRules);
double product_sum(node*nod,double Probabilities[]);
double product(node*nod,double Probabilities[]);
void forward_C(double Probabilities[], int NR, node*root);
void commonParamameters(double EA,double ER, int MaxIteration,int lenNodes,char*statisticsFolder,char*save,char*seeded,int seed);
int getNodes_Parameters (term_t Nodes,term_t Params,term_t StopCond,term_t Folder,node ***leaves_node,node***nodes_ex,int* lenNodes, int *MaxIter,double* EA,double*  ER,int *NR, double*ZERO,char**statisticsFolder, char**save,char** seeded, int* seed);
int setResults(double CLL, double Probabilities[],int NR,term_t *CLLFinal, term_t *ProbFinal);

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

void openFilesEM (char * statisticsFolder, FILE**probsFile, FILE** expectationsFile, FILE**countsFile ,FILE** lls);
void closeFilesEM (FILE**probsFile, FILE** expectationsFile, FILE**countsFile,FILE** lls);
void initialize_expectations_Counters(double Probabilities[],double expectations[],int Count [],int NR,char* seeded, int seed);
void backwardEM(double expectations[], int Count[],int NR, node*root);
double expectation(node**Nodes,int lenNodes,double Probabilities[],double expectations[],int Counts [],int NR);
void maximization(double Probabilities [],double expectations[],int Count [],int NR);
double emphil(node **Nodes,int lenNodes,double Probabilities[],double expectations[],int Counts [],int NR, int MaxIteration,double EA, double ER,char* statisticsFolder, char* save,char* seeded, int seed);

// ++++++++++++++++++++++++ Parameter learning declaration functions  ++++++++++++++++++++++++++
static foreign_t pl_dphil(term_t Nodes,term_t Params,term_t StopCond, term_t Folder, term_t Adam, term_t Params2, term_t CLLFinal, term_t ProbFinal);
static foreign_t pl_emphil(term_t Nodes,term_t Params,term_t StopCond,term_t Folder, term_t CLLFinal, term_t ProbFinal);

// ++++++++++++++++++++++++ Forward step use in phil.pl  ++++++++++++++++++++++++++


static foreign_t pl_forward(term_t Circuit,term_t Parameters,term_t NR1,term_t Output);




// ++++++++++ Common util functions for Gradient descent and EM +++++++++++++++
node * new_node(double val,char type[MaxType]){
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

void construct_leaves_node(node **leaves_node,int lenRules){
  int i;
  for(i=0;i<lenRules;i++){
    leaves_node[i]=new_node(0.0,"leaf");
    leaves_node[i]->index=i;
  }
}

void printAC(node* root,char*Choice){
  if(root!=NULL){
      node* n=root->child;
      if(strcmp(Choice,"values")==0){
          printf("%s(%lf) ",root->type,root->value);
      }else{
          if(strcmp(Choice,"gradients")==0){
             printf("%s(%lf) ",root->type,root->gradient);
          }
      }
      while(n!=NULL){
          printAC(n,Choice);
          n=n->next;
      }
  }else{
     perror("Null AC");
  }  
}
void setSeed(char* seeded, int seed){
  if(strcmp(seeded,"yes")==0 || strcmp(seeded,"Yes")==0 || strcmp(seeded,"YES")==0)
    srand((unsigned)seed);
  else
      srand(time(NULL));
}
double randInRange(double min, double max)
{
  return min + (double)rand() / (RAND_MAX / (max - min));
}

/*float randInRange2()
{
     return (float)rand()/RAND_MAX;
}*/

void print_Vector(double Vector[],int NR,char*VectorType){
 int i;
 char T[1];
 if(strcmp(VectorType,"gradient")==0){
   T[0]='G';
 }else{
    if(strcmp(VectorType,"weights")==0){
      T[0]='W';
    }else{
       if(strcmp(VectorType,"probabilities")==0){
         T[0]='P';
       } 
    }
 }
  printf("\n%s[",T);
  for(i=0;i<NR-1;i++){
   printf("%lf,",Vector[i]);
  }
  printf("%lf",Vector[i]);
  printf("]\n");
}

// Converts an arithmetic circuit (a term prolog) into n-aries tree in C
node *convertACToTree(term_t AC,node **leaves_node,int lenRules){
  int ret,ind,arity,i,j,lenNodes1;
  size_t lenNodes;
  atom_t name;
  char *type;
  term_t current_term,current_List_term, temp_term;
  node *root,**nodes_ex;

  current_term=PL_new_term_ref();
  temp_term=PL_new_term_ref();
  current_List_term=PL_new_term_ref();
  if (!PL_is_integer(AC)){
        ret=PL_get_compound_name_arity(AC,&name,&arity);
     }else arity=0;

  if(arity!=0){ // non leaf node
      ret=PL_put_atom(temp_term,name);
      ret=PL_get_atom_chars(temp_term,&type);
      root=new_node(0.0,type);
      
      for(i=1;i<=arity;i++){ // cycle over the argument: normally there are just one list argument for a particular term. Example: or[..]
       ret=PL_get_arg(i,AC,current_List_term);
       if (PL_is_list(current_List_term))
       {
       ret=PL_skip_list(current_List_term,0,&lenNodes);
       lenNodes1= (int)lenNodes;
       if(lenNodes==1){ // The list has a single term. Example [2]
          ret=PL_get_list(current_List_term,current_term,current_List_term);
          root->child=convertACToTree(current_term,leaves_node,lenRules);
       }else{// The list has many terms
            nodes_ex=(node **)malloc(lenNodes*sizeof(node*));
            ret=PL_get_list(current_List_term,current_term,current_List_term);
            nodes_ex[0]=convertACToTree(current_term,leaves_node,lenRules);
            root->child=nodes_ex[0];
            for(j=1;j<lenNodes1;j++){
                ret=PL_get_list(current_List_term,current_term,current_List_term);
                nodes_ex[j]=convertACToTree(current_term,leaves_node,lenRules);
                nodes_ex[j-1]->next=nodes_ex[j];
            }// end for            
       }// end else lenNodes==1
       }
       else
       root->child=convertACToTree(current_List_term,leaves_node,lenRules);
     }//end cycle
  }else{ // arity=0 -> leaf node
    if(PL_is_integer(AC)){
     ret=PL_get_integer(AC,&ind);
     root=leaves_node[ind];
    }else{
     root=new_node(1.0,"leaf");
     root->index=-1;
    } 
  }// end arity!=0
  ret=ret*2; // to avoid warning message
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
void  commonParamameters(double EA,double ER, int MaxIteration,int lenNodes,char*statisticsFolder,char*save,char*seeded,int seed){
  printf("\n Hyperparameters of %s:\n\n", statisticsFolder);
  printf("Save the statistics? %s\n",save);
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
void forward_C(double Probabilities[], int NR, node*root){
  node * n;
  int index;
 if(root!=NULL){
    if(strcmp(root->type,"not")==0){
      forward_C(Probabilities,NR,root->child);
      root->value=1-(root->child)->value;
    }else{
        if(strcmp(root->type,"or")==0){
         // iterate on sibling
         n=root->child;
         while (n!=NULL){
           forward_C(Probabilities,NR,n);
           n = n->next;
         }
         root->value=product_sum(root->child,Probabilities);
       }else{
          if(strcmp(root->type,"and")==0){
         // iterate on sibling
         n=root->child;
         while (n!=NULL){
           forward_C(Probabilities,NR,n);
           n = n->next;
        }
        root->value=product(root->child,Probabilities);
       }else{ // leaf node;
           if(strcmp(root->type,"leaf")==0){
            index=root->index;
            root->value=Probabilities[index];
           }
         }
       }
     }
  }else{
   perror("Forward pass:NULL node");
 }  
}

int getNodes_Parameters (term_t Nodes,term_t Params,term_t StopCond,term_t Folder,node ***leaves_node,node***nodes_ex,int* lenNodes, int *MaxIter,double* EA,double*  ER,int *NR, double*ZERO,char**statisticsFolder, char**save,char**seeded, int*seed){
  int ret,i; // type=0 (only emphil is computed), type=1 (only dphil is computed) type=2(both emphil and dphil are computed)
  term_t nodesTerm1,nodesTerm2,nodesTerm3,nodesTerm4,head;
  size_t lenNodes1;
  node * root;
  head=PL_new_term_ref();
  //get the arguments

  nodesTerm1=PL_copy_term_ref(Params); // Contains the Stop criterion and
  nodesTerm2=PL_copy_term_ref(StopCond); // Contains the Stop criterion and 
  nodesTerm3=PL_copy_term_ref(Folder);  // Contains the name of the folder and the save flag (yes or no)
  nodesTerm4=PL_copy_term_ref(Nodes); // Contains the Arithmetic Circuits
  
  // Get the number of rule and the Zero setting
   ret=PL_get_list(nodesTerm1,head,nodesTerm1);
  ret=PL_get_integer(head,NR);
  // Get the zero value
  ret=PL_get_list(nodesTerm1,head,nodesTerm1);
  ret=PL_get_float(head,ZERO);
   // Get the zero value
  ret=PL_get_list(nodesTerm1,head,nodesTerm1);
  ret=PL_get_chars(head, seeded,CVT_STRING);
   // Get the zero value
  ret=PL_get_list(nodesTerm1,head,nodesTerm1);
  ret=PL_get_integer(head,seed);

  // Get the stop conditions
  ret=PL_get_list(nodesTerm2,head,nodesTerm2);
  ret=PL_get_integer(head,MaxIter);
  ret=PL_get_list(nodesTerm2,head,nodesTerm2);
  ret=PL_get_float(head,EA);
  ret=PL_get_list(nodesTerm2,head,nodesTerm2);
  ret=PL_get_float(head,ER);
 
  // Get the statistic folder and a flag for saving statistics
  ret=PL_get_list(nodesTerm3,head,nodesTerm3);
  ret=PL_get_chars(head, save,CVT_STRING);
  ret=PL_get_list(nodesTerm3,head,nodesTerm3);
  ret=PL_get_chars(head, statisticsFolder,CVT_STRING);
  

  ret=PL_skip_list(Nodes,0,&lenNodes1);
  *lenNodes=(int)lenNodes1;
  *nodes_ex=(node **)malloc((*lenNodes)*sizeof(node*));
  *leaves_node=(node **)malloc((*NR)*sizeof(node*));
  // Construct the leave nodes
  construct_leaves_node(*leaves_node,*NR);
  // construct the ACs: convert all the Acs into n-ary tress
  for(i=0;i<*lenNodes;i++){
     ret=PL_get_list(nodesTerm4,head,nodesTerm4);
     root=convertACToTree(head,*leaves_node,*NR);
     (*nodes_ex)[i]=root;     
  }
 return ret;
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
       mkdir(statisticsFolder2, 0700);
       strcat(statisticsFolder2, "/GD");
       mkdir(statisticsFolder2, 0700);
     }else{
         mkdir(statisticsFolder, 0700);
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

// Initializes the weights the moments and the gradients
void initialize_weights_moments(double weights[],double Gradient[],double moments0[],double moments1[],int NR,double Max_W,char*seeded,int seed){
  int i;
  setSeed(seeded,seed);
    for(i=0;i<NR;i++){
        weights[i]=randInRange(-Max_W,Max_W);
        //weights[i]=0.5;
        moments0[i]=0.0;
        moments1[i]=0.0;
        Gradient[i]=0.0;
    }
}

// Print the hyperparameters for gradient descent
void  printHyperparamsGD(double Eta,double Beta1,double Beta2, long double Adam_hat,double Max_W,int BatchSize,char* strategy){
  printf("Initialize weights in the range [%.2lf %.2lf]\n",-Max_W,Max_W);
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
    forward_C(Probabilities,NR,Nodes[index]);
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
  
  if(BatchSize >lenNodes){ // if the batch size is great than the training set it to the cardinality of the training set
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
  sigma_vec(Weights,Probabilities,NR);
  if(saved==1){
      openFilesGD (statisticsFolder,&probsFile, &weightsFile,&Moments0File,&Moments1File,&lls); 
      saveStatisticsGD(Probabilities,Weights,Moments0,Moments1,NR,probsFile,weightsFile,Moments0File,Moments1File,lls,CLL1);
  }
  from=0; to=BatchSize;
  while(Iter<MaxIteration1 && diff>EA && ratio>ER){ //   && diff>EA && ratio>ER
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
  char nameFileProbs [40]="./";
  char nameFileExpects [40]="./";
  char nameFileCount [40]="./";
  char nameFileClls [40]="./";
 strcpy(statisticsFolder2, statisticsFolder);

  strcat(statisticsFolder, "_Statistics/EM");
   // create the directory to save information
  if (stat(statisticsFolder, &st) == -1) {
    strcat(statisticsFolder2, "_Statistics");
     if (stat(statisticsFolder2, &st) == -1) {
       mkdir(statisticsFolder2, 0700);
       strcat(statisticsFolder2, "/EM");
       mkdir(statisticsFolder2, 0700);
     }else{
         mkdir(statisticsFolder, 0700);
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

void closeFilesEM (FILE**probsFile, FILE** expectationsFile, FILE**countsFile,FILE** lls){
  fclose(*probsFile);
  fclose(*expectationsFile);
  fclose(*countsFile);
  fclose(*lls);
}

//Saves the log-likelihood (CLL), The expectations and the count.
void saveStatisticsEM(double Probabilities[],double expectations [], int Counts [],int NR, FILE*probsFile, FILE*expectationsFile, FILE *countsFile, FILE* lls, double CLL){
  int i;
  for(i=0; i<NR; i++){
     fprintf(probsFile,"%f ",Probabilities[i]);
     fprintf(expectationsFile,"%f ",expectations[i]);
     expectations[i]=0; // reinitialized the expectations
     fprintf(countsFile,"%d ",Counts[i]);
     Counts[i]=0;  //reinitialized the count
  }
  fprintf(lls,"%f \n",CLL);
  fprintf(probsFile,"\n \n");
  fprintf(expectationsFile,"\n \n");
  fprintf(countsFile,"\n \n");
}

// initialize the probabilities the expectations and the counters 
void initialize_expectations_Counters(double Probabilities[],double expectations[],int Count [],int NR,char *seeded,int seed){
    int i;
    setSeed(seeded,seed);
    for(i=0;i<NR;i++){
        Probabilities[i]=randInRange(0,1);
        //Probabilities[i]=0.5;
        expectations[i]=0.0; 
        Count[i]=0;
    }
}

// Performs the backward step in the message passing
void backwardEM(double expectations[], int Count[],int NR, node*root){
 node * n;
 double tn,tp,temp,denominator;
 if(root!=NULL){
  tp=root->tp;
  if(strcmp(root->type,"not")==0){
    n=root->child;
    while (n!=NULL){
       n->tp=1-tp;
      backwardEM(expectations,Count,NR,n);
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
         backwardEM(expectations,Count,NR,n);
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
            backwardEM(expectations,Count,NR,n); 
            n = n->next;
         }
     }else{
           if(strcmp(root->type,"leaf")==0){
              int index=root->index;
              double pi=root->value, tp=root->tp; 
              denominator=pi*tp+(1-pi)*(1-tp);
              if(denominator!=0)
                temp=(pi*tp)/denominator;
              else 
                temp=(pi*tp)/ZERO;
              if(index!=-1){
                  expectations[index]=expectations[index]+ temp;
                  Count[index]=Count[index]+1;
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
double expectation(node**Nodes,int lenNodes,double Probabilities[],double expectations[],int Counts [],int NR){
  double Root_Value,CLL=0;
  int i;
  for(i=0;i<lenNodes;i++){
    // forward pass
    forward_C(Probabilities,NR,Nodes[i]);
    Root_Value=Nodes[i]->value;
    if(Root_Value!=0){
      CLL=CLL+log(Root_Value);
    }
    else 
       CLL=CLL+ log(ZERO);
    Nodes[i]->tp=1.0; // initial message from the root node to the children
    //Nodes[i]->tp1=0.0;
    // backwardEM pass
    backwardEM(expectations,Counts,NR,Nodes[i]);
  }
  return CLL;
}

// Maximization step: computes new values the probabilities 
void maximization(double Probabilities [],double expectations[],int Count [],int NR){
   int i;
   for(i=0;i<NR;i++){
      if(Count[i]!=0){
        Probabilities[i]=expectations[i]/Count[i];
      } 
    }
}

// Performs Expectation Maximization
double emphil(node **Nodes,int lenNodes,double Probabilities[],double expectations[],int Counts [],int NR, int MaxIteration,double EA, double ER,char* statisticsFolder, char* save,char* seeded, int seed)
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

  initialize_expectations_Counters(Probabilities,expectations,Counts,NR,seeded,seed);
  if(saved==1){ 
    openFilesEM (statisticsFolder,&probsFile,&expectationsFile,&countsFile,&lls);
    saveStatisticsEM(Probabilities,expectations,Counts,NR,probsFile,expectationsFile,countsFile,lls,CLL1);
  }
   Iter=0;
  while(Iter<MaxIteration1 && diff>EA && ratio>ER){ // MaxIteration1  && diff>EA && ratio>ER
      CLL0 = CLL1;
      //Expectation step
      CLL1=expectation(Nodes,lenNodes,Probabilities,expectations,Counts,NR);
      //Maximization step
      maximization(Probabilities,expectations,Counts,NR);
      // save the values of probabilities
      if(saved==1)
        saveStatisticsEM(Probabilities,expectations,Counts,NR,probsFile,expectationsFile,countsFile,lls,CLL1);
      diff=fabs(CLL1-CLL0);
      ratio=diff/fabs(CLL0);
      Iter++;
  }//end while

  if(saved==1)
    closeFilesEM(&probsFile,&expectationsFile,&countsFile,&lls);
  return CLL1;
}


// ++++++++++++++++++++++++ Parameter learning function  ++++++++++++++++++++++++++


// Learns the parameters of HPLP using Gradient Descent
static foreign_t pl_dphil(term_t Nodes,term_t Params,term_t StopCond, term_t Folder, term_t Adam, term_t Params2, term_t CLLFinal, term_t ProbFinal){ // 
  int ret,NR,MaxIter,lenNodes,BatchSize,seed; 
  char *statisticsFolder, *save,*strategy, *seeded;
  node **leaves_node=NULL,**nodes_ex=NULL;
  double *Probabilities,  *Weights;
  double EA,ER,CLL,Max_W,Eta,Beta1,Beta2,Adam_hat;
  term_t nodesTerm1,nodesTerm2,head;
  head=PL_new_term_ref();
  // get the nodes and the parameters
  ret=getNodes_Parameters(Nodes,Params,StopCond,Folder,&leaves_node,&nodes_ex,&lenNodes,&MaxIter,&EA,&ER,&NR, &ZERO,&statisticsFolder,&save,&seeded, &seed);
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
  ret=PL_get_chars(head, &strategy,CVT_STRING);
  // Get the BatchSize
  ret=PL_get_list(nodesTerm2,head,nodesTerm2);
  ret=PL_get_integer(head,&BatchSize);
  // Get the maximun initial weights
  ret=PL_get_list(nodesTerm2,head,nodesTerm2);
  ret=PL_get_float(head,&Max_W);

  // Print Hyperparameters
  commonParamameters(EA,ER,MaxIter,lenNodes,statisticsFolder,save,seeded,seed);
  printHyperparamsGD(Eta,Beta1,Beta2,Adam_hat,Max_W,BatchSize,strategy);
  Probabilities=(double*)malloc(NR*sizeof(double));
  Weights=(double*)malloc(NR*sizeof(double));
  // Performs paramter learning with gradient descent descent
 CLL=dphil(nodes_ex,lenNodes,MaxIter,Probabilities,Weights,NR,EA,ER,Eta,Beta1,Beta2,Adam_hat,Max_W,BatchSize,strategy,statisticsFolder,save,seeded, seed);  // return the CLL and the learned probabilities
 ret=setResults(CLL,Probabilities,NR,&CLLFinal, &ProbFinal);
 return ret;
}

// Learns the parameters of HPLP using  Expectation Maximization
static foreign_t pl_emphil(term_t Nodes,term_t Params,term_t StopCond,term_t Folder, term_t CLLFinal, term_t ProbFinal){ // 
  int ret,NR,MaxIter,lenNodes,seed; // type=0 (only emphil is computed), type=1 (only dphil is computed) type=2(both emphil and dphil are computed)
  char *statisticsFolder, *save,*seeded;
  node **leaves_node=NULL,**nodes_ex=NULL;
  double *Probabilities,  *Expectations;
  double EA,ER,CLL;
  int *Counts;

  // get the nodes and the parameters
  ret=getNodes_Parameters (Nodes,Params,StopCond,Folder,&leaves_node,&nodes_ex,&lenNodes,&MaxIter,&EA,&ER,&NR, &ZERO,&statisticsFolder,&save,&seeded, &seed);
  commonParamameters(EA,ER,MaxIter,lenNodes,statisticsFolder,save,seeded,seed);
  Counts=(int*)malloc(NR*sizeof(int));
  Expectations=(double*)malloc(NR*sizeof(double));
  Probabilities=(double*)malloc(NR*sizeof(double));
  // Performs paramter learning with EM
  CLL=emphil(nodes_ex,lenNodes,Probabilities,Expectations,Counts,NR,MaxIter,EA,ER,statisticsFolder,save,seeded,seed);
  // return the CLL and the learned probabilities
  ret=setResults(CLL,Probabilities,NR,&CLLFinal, &ProbFinal);
  return ret;
}
// Evaluates the AC in phil.pl at test time 
static foreign_t pl_forward(term_t Circuit,term_t Parameters,term_t NR1,term_t Output){
  node * root, **leaves_node;
  int NR,i,ret;
  double *Probabilities;
  term_t ParamatersTerm, head,out;
  head=PL_new_term_ref();
  out=PL_new_term_ref();
  ret=PL_get_integer(NR1,&NR);
  leaves_node=(node **)malloc(NR*sizeof(node*));
  Probabilities=(double*)malloc(NR*sizeof(double));
  leaves_node=(node **)malloc(NR*sizeof(node*));
  ParamatersTerm=PL_copy_term_ref(Parameters); 
  for(i=0;i<NR;i++){
    ret=PL_get_list(ParamatersTerm,head,ParamatersTerm);
    ret=PL_get_float(head,&(Probabilities[i]));
  }
  construct_leaves_node(leaves_node,NR);
  root=convertACToTree(Circuit,leaves_node,NR);
  forward_C(Probabilities,NR,root);

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
