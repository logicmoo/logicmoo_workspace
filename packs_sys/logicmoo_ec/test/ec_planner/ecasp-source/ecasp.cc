/* 
  *File: ecasp.cc (v.0.92)
  *Last Updated on 8/25/09 
  *By Tae-Won Kim

  *Turn an event calculus description in the input language
   of the DEC reasoner to a logic program.
   => The syntax of ASP style

  1)Version 0.90: First developed (12/23/08).
  2)Version 0.91: Allowed to use ASP directives (3/31/09).
                  - for handling the transitive closure
  3)Version 0.92: Implemented to check for syntax errors (6/18/09).              
*/

#include <iostream>
#include <cstdlib>
#include <string> 
#include <fstream>
#include <cctype>  
#include <sstream>
#include <time.h>

using namespace std;


#define FORM_COL 300      // Maximum number of characters in a Formula
#define NUM_SORTS 20      
#define NUM_SUB_SORTS 10
#define NUM_SUB_SORT_RULES 30
#define NUM_SUB_SORT_INTEGER 20
#define NUM_EVENT_TERMS 2
#define NUM_CONSTANTS 20
#define NUM_CONST_OBJECTS 30
#define NUM_FLUENTS 20
#define NUM_EVENTS 20
#define NUM_PREDICATES 20
#define RANGE_COL 20      // Maximum number of characters in Range statement
#define NUM_VARS 20
#define NUM_ALL_VARS 1000 
#define NUM_TOKENS_FORM 50 //30
#define NUM_CHILDNODES 40
#define NUM_ANCESTORS 20
#define NUM_SUB_FORMS 20
#define NUM_SUB_TREES 20
#define NUM_SUB_FORMS_BI_IMPL 2
#define NUM_NEWPRE_FORMS 50
#define NUM_FINAL_FORMS 100
#define NUM_FINAL_TREES 100
#define NUM_RULES 400
#define NUM_FILES 10

struct Sort
{
  string sName;
  string subSorts[NUM_SUB_SORTS];
  int num;
};

Sort sorts[NUM_SORTS];
int sortRow=0;
int indexOfSort;

string subSortRules[NUM_SUB_SORT_RULES];
int subSortRow=0;

string subSortOfInteger[NUM_SUB_SORT_INTEGER];
int subSortInt=0;

struct Const
{
  string sName; // sort name
  string objects[NUM_CONST_OBJECTS];
  int num;      // number of objects
};

Const constants[NUM_CONSTANTS];
int constRow=0;
string sortNameOfConst("");

struct Fluent
{
  string pName; 
  string srts[NUM_SORTS];
  int num;      // number of sorts
};

Fluent fluents[NUM_FLUENTS];
int fluentRow=0;

string declarationOfFluents[NUM_FLUENTS];
int decFluentRow=0;

struct Event
{
  string pName; 
  string srts[NUM_SORTS];
  int num;      // number of sorts
};

Event events[NUM_EVENTS];
int eventRow=0;

string declarationOfEvents[NUM_EVENTS];
int decEventRow=0;

struct Predicate
{
  string pName; 
  string srts[NUM_SORTS];
  int num;      // number of sorts
};

Predicate predicates[NUM_PREDICATES];
int predicateRow=0; 

string rulesOfASP[NUM_RULES];
int nOfRulesOfASP=0;

string showPredicates[NUM_PREDICATES];
int showPredicateRow=0;

string rangeForm[RANGE_COL];
int rangeCol=0;

string varsQuan[NUM_VARS];
int numOfvarsQuan=0;

string variables[NUM_VARS];
int numOfvariables=0;

string varsUniQuan[NUM_VARS];
int numOfvarsUniQuan=0;
int numOfuniQuan=0;

string varsExstQuan[NUM_VARS];
int numOfvarsExstQuan=0;

string newPreHeads[NUM_VARS];
int numOfnewPreHeads=0;

// #domain for each variable
string allVars[NUM_ALL_VARS];
int numOfallVars=0;

string tempVars[NUM_VARS];
int numOftempVars=0;

string varsOfnewPre[NUM_VARS];
int numOfvarsOfnewPre=0;

string tokenOfForm[NUM_TOKENS_FORM]; 
int ntokensOfForm=0;
int numOftokensOfForm=0;

string finalForms[NUM_FINAL_FORMS];
int numberOfForms=0;

string rules[NUM_RULES];
int ruleRow=0;

string orgFormula("");
string Initiates = "Initiates";
string Terminates = "Terminates";
string Releases = "Releases";
string HoldsAt = "HoldsAt";
string ReleasedAt = "ReleasedAt";
string Happens = "Happens";
string Happens3= "Happens3";
string Trajectory = "Trajectory";
string AntiTrajectory = "AntiTrajectory";
string StoppedIn = "StoppedIn";
string StartedIn = "StartedIn";
string Clipped = "Clipped";
string Declipped = "Declipped";
string Started = "Started";
string Stopped = "Stopped";
string Initiated = "Initiated";
string Terminated = "Terminated";
string domain("");
string newPre="newPre"; // New predicates
string rform("");
string maxOfTimepoint("");
int newPreNum=1;
int rangeMaxTime=0;
int rangeMinTime=0;
int rangeMaxOffset=0;
int rangeMinOffset=0;
int flag_colon=0;
int declaration=0;
int numOfcomma=0;
bool flag_completion=false;
bool notRemoveDisjAndConj=false;
bool flag_uniQuan=false;
bool flag_trajectory=false;


struct stack
{
  int top;
  char item[30];
};
struct node 
{
  struct node *left;
  struct node *right;
  struct node *parent;
  string str;
};

typedef struct node *nodePtr; 
nodePtr root=NULL;
nodePtr newTree=NULL;

nodePtr subFormsBiImpl[NUM_SUB_FORMS_BI_IMPL];
int numOfsubFormsBiImpl=0;

nodePtr subFormsC1[NUM_SUB_FORMS];  
int numOfsubFormsC1=0;

nodePtr subFormsC2[NUM_SUB_FORMS]; 
int numOfsubFormsC2=0;

nodePtr newPredicateForms[NUM_NEWPRE_FORMS];
int nOfnewPreForms=0;

nodePtr finalFormTrees[NUM_FINAL_TREES];
int numberOfTrees=0;

nodePtr childNodes[NUM_CHILDNODES];
int nOfchildNodes=0;


///////// Function prototypes
void usage();
void initializeSorts();
int isWhiteSpace(string aLine);
int loadComment(string aLine);
void removeComment(string& target);
int loadOption(string aLine);
string split(string &str, string delimiter);
int loadLoad(string aLine);
int loadSort(string aLine);
void checkParsErrorsOfDeclaration(string orgLine, string aLine);
void trimSpace(string& aLine);
void showSyntaxErrorMsg(string orgLine);
int isSort(string token);
int existSortName(string token);
int loadConstant(string aLine);
int findIndexOfConst(string sN);
int isConstant(string token);
int loadFluent(string aLine);
int isPredicate(string token);
string integerToString(int i);
bool existVarsInExstQuan(string target);
bool existInAllVars(string target);
int loadEvent(string aLine);
int loadPredicate(string aLine);  // need to be completed
int loadRulesOfASP(istream& inFile, string aLine);
void checkSyntaxOfASPDirective(string& target);
int loadCompletion(string aLine);
int isAbPredicate(string target);
int loadRange(string aLine);
int isNumber(string target);
int isSubsortOfInteger(string s);
int stringToInteger(string number);
int isEndOfFormula(string aLine);
void checkParsError(string form);
void giveParsError();
void removeOutermostParen(string& form);
void changeSym(string& form);
void makeTree(string form);
void markTupleEnd(string& form);
void freshTokensOfForm();
void infixToPrefix(char *input, char *output);
int isNegation(char *input, int i);
void push(struct stack *ps, char n);
void pop(struct stack *ps);
int isempty(struct stack *ps);
int prcd(char stktop, char op);
int full(struct stack *ps);
void getTokensOfForm(char *prefix);
nodePtr prefixToTree();
bool universalQuan(string uq);
bool existVarsInQuan(string target);
bool existentialQuan(string eq);
void checkParsError2();
void getChildNodes(nodePtr tree);
bool checkFormOne(nodePtr cn);
string differentiateVars(string form);
int validFluentTerm(string token);
int isSubsortOf(string sub, string super);
bool existConstant(string target);
void addVars(string target);
bool existVariables(string target);
int validTimepointTerm(string token);
bool checkHappensForm(nodePtr cn);
int validEventTerm(string token);
bool checkFormTwo(nodePtr cn);
bool checkFormThree(nodePtr cn);
bool checkHappens3Form(nodePtr cn);
bool checkFormFour(nodePtr cn);
bool checkFormFive(nodePtr cn);
bool checkFormSix(nodePtr cn);
void freshChildNodes();
void freshVariables();
void freshVarsQuan();
void checkBiImpl(string form);
nodePtr copyTree(nodePtr org, nodePtr parent);
bool isVariable(string var);
void returnInfixForm(nodePtr tree);
nodePtr searchNode(nodePtr tree, string target);
void applyC1(nodePtr tree);
void eliminateDisjunction(nodePtr tree);
//void checkAncestorsWithNegOrExstQuan(nodePtr child);
void checkAncestorsWithExstQuan(nodePtr child);
void applyC2(nodePtr tree);
void eliminateConjunction(nodePtr tree);
void displayTree(nodePtr tree);
void moveExstQuan(nodePtr tree);
void eliminateExstQuan(nodePtr tree);
void eliminateUniQuan(nodePtr tree);
void getVarsInUniQuan(nodePtr tree);
void freshVarsInUniQuan();
void freshVarsInNewPre();
void eliminateNegExstQuan(nodePtr target);
void getVarsInExstQuan(nodePtr target);
void findTargets(nodePtr target);
bool existInTempVars(string target);
bool existInVarsOfnewPre(string target);
void renameVarsInExstQuan(nodePtr target);
string removeExtraParentheses(string& form);
string convertToLP(string form);
void handleHead(string& form);
void convertSyms(string& form);
void eliminatePosExstQuan(nodePtr target);
void getVarsInAllExstQuans(nodePtr tree);
void getVarsOfNewPre(nodePtr tree);
void freshTempVars();
void renameVarsInUniQuan(nodePtr target);
void freshVarsInExstQuan();
void freshSubformsC1();
void freshSubformsC2();
void freshFinalFormTrees();
void freshNewPreForms();
void freshSubformsBiImpl();
void setUpDomainPredicates();


           
int main(int argc, char *argv[])
{
  int len=0;
  string line("");
  string formula("");
  string intermediateFileName("");

  // Check the number of arguments.
  if(argc !=  2)
  {
    usage();
    exit(EXIT_FAILURE);
  }
                                         
  ifstream inFile (argv[1]);               
  if (!inFile.is_open())                     
    {
      cout << "Unable to open " << argv[1] << endl;
      exit(EXIT_FAILURE);
    }
  
  else // Check the file extension (name).
    if(strstr(argv[1], ".e")==NULL)
      {
	cout << "The file '" << argv[1]
             << "' is not applicable to ECASP.\n" << endl;
	exit(EXIT_FAILURE);
      }
 
  initializeSorts(); // Add sort time and offset as default.

  while(!inFile.eof())
  {
    getline(inFile, line);

    // The line only consists one or more whitespace.
    if(isWhiteSpace(line))
      continue;
    
    // The line is a comment.
    if(loadComment(line))
      continue;

    // The line is an 'option' statement.
    if(loadOption(line))
      continue;

    // The line is a 'load' statement. 
    if(loadLoad(line))
      continue;

    // The line is for sort declaration.
    if(loadSort(line))
      continue;

    // The line is for constant declaration.
    if(loadConstant(line))
      continue;
    
    // The line is for fluent declaration.
    if(loadFluent(line))
      continue;
 
    // The line is for event declaration.
    if(loadEvent(line))
      continue;   

    // The line is for predicate declaration.
    if(loadPredicate(line))
      continue;
    
    // The line is a 'completion' statement.
    if(loadCompletion(line))
    {
      flag_completion=true; 
      continue;
    }

    // The line is a 'range' statement.
    if(loadRange(line))
      continue;

    // The line is for rules of ASP.
    if(loadRulesOfASP(inFile, line))
      continue;

    // The line is for a formula.
    formula="";
    removeComment(line);
    formula.append(line);
	
    while(!isEndOfFormula(formula) & !inFile.eof())
    {  
      getline(inFile, line);     
      if(isWhiteSpace(line))
        continue;

      if(loadComment(line))
	continue;

      formula.append(1, ' ');
      removeComment(line);
      formula.append(line);   

      if(loadOption(line)     || loadLoad(line)      || 
         loadSort(line)       || loadConstant(line)  || 
         loadFluent(line)     || loadEvent(line)     || 
         loadPredicate(line)  || loadRulesOfASP(inFile, line)||
         loadCompletion(line) || loadRange(line)) 
	 {
	   cout << "\nERROR: unable to parse: '" 
		<< formula << "'" << endl;
	   exit(EXIT_FAILURE);
	 }
    }
    
    if(!isEndOfFormula(formula)) 
    {
      cout << "\nERROR: unable to parse: '" 
	   << formula << "'" << endl;
      exit(EXIT_FAILURE);
    }
   
    orgFormula=formula;
    trimSpace(formula);

    // Check Parse errors (syntax errors)
    checkParsError(formula); 

    formula.erase(formula.length()-1,1);// Erase '.' in the formula.
    changeSym(formula);

    makeTree(formula); 

    /* Check Parse erorrs (validity for terms and predicates)
       and check free variables */

    checkParsError2(); 

    // Check whether Bi-Implication is in the formula.
    checkBiImpl(formula); 

    for(int r=0; r<numOfsubFormsBiImpl; r++)
    {
      freshSubformsC1();
      applyC1(subFormsBiImpl[r]);

      freshSubformsC2();
      for(int p=0; p<numOfsubFormsC1; p++)
	applyC2(subFormsC1[p]);

      for(int q=0; q<numOfsubFormsC2; q++)
      {
        //Rewrite a formula so that existential quantifiers
        //occurs only in the antecedent of the formula
        moveExstQuan(subFormsC2[q]);
       
        // Remove existential quantifiers
        eliminateExstQuan(newTree);
        finalFormTrees[numberOfTrees++]=newTree;

        for(int t=0; t<nOfnewPreForms; t++)
        {
	  eliminateExstQuan(newPredicateForms[t]);
	  finalFormTrees[numberOfTrees++]=newPredicateForms[t];
	}
	freshNewPreForms();

        for(int y=0; y<numberOfTrees; y++)
        {
          freshSubformsC1();
          applyC1(finalFormTrees[y]); 

          for(int x=0; x<numOfsubFormsC1; x++)
	  {
            //Remove universal Quantifiers
            eliminateUniQuan(subFormsC1[x]);
	    
            //Turn into the input language of LPARSE
            finalForms[numberOfForms++]
              =convertToLP(removeExtraParentheses(rform));

            rform="";
          }
        }
        freshFinalFormTrees();
      }

    } // end of the first 'for' loop
    freshSubformsBiImpl();

  } // end of 'while' loop

      inFile.close();
      cout << endl;
     
      // Add domain predicates for variables
      setUpDomainPredicates();
      rules[ruleRow++]=domain;

      // Add New Lines
      rules[ruleRow++]="\n\n";

      // Add range of sorts that are subsorts of integer
      for(int i=0; i<rangeCol; i++)
      {
	rules[ruleRow++]=rangeForm[i];
	rules[ruleRow++]="\n";
      }

      // Add a New Line
      rules[ruleRow++]="\n";

      // Add Constants 
      for(int j=0; j<constRow; j++)
      {
	for(int k=0; k<constants[j].num; k++)
	{
	  string obj=constants[j].objects[k];
	  string srtName=constants[j].sName;
	  //$ srtName[0]=tolower(srtName[0]);
	  obj[0]=tolower(obj[0]);
	  rules[ruleRow++]=srtName+"("+obj+").";
	  rules[ruleRow++]="\n";
	}
      }

      // Add a New Line
      rules[ruleRow++]="\n";

      // Add Subsort relationship if applicable
      for(int r=0; r<subSortRow; r++)
	rules[ruleRow++]=subSortRules[r];


      // Add a New Line
      rules[ruleRow++]="\n";


      // Add Fluents
      for(int l=0; l<decFluentRow; l++)
      {	
	rules[ruleRow++]=declarationOfFluents[l];
	rules[ruleRow++]="\n";
      }

      // Add a New Line
      rules[ruleRow++]="\n";


      // Add Events
      for(int m=0; m<decEventRow; m++)
      {	
	rules[ruleRow++]=declarationOfEvents[m];
	rules[ruleRow++]="\n";
      }

      // Add New Lines
      rules[ruleRow++]="\n";


      // Add formulas translated
      for(int n=0; n<numberOfForms; n++)
      {
	rules[ruleRow++]=finalForms[n];
	rules[ruleRow++]="\n\n";
      }

     
      // Add the rules in the ASP directive
      for(int w=0; w<nOfRulesOfASP; w++)	
      {
	rules[ruleRow++]=rulesOfASP[w];
	rules[ruleRow++]="\n\n";	
      }


      // For fluents and events that do not include
      // any arguments
      for(int s=0; s<ruleRow; s++)
      {
	string r=rules[s];
	int len=r.length();

	for(int e=0; e<len; e++)
	{
	  if(e<len-2)
	    if(r[e]=='(' && r[e+1]==')')
            {
	      r.replace(e, 2, "");
	      len=len-2;
	    }
	  rules[s]=r;
	}
      }
      

      // Add a choice rule for happens predicate
      if(!flag_completion)
      {
	string cHappens="{happens(E, T)} :- T<maxstep.";
        rules[ruleRow++]=cHappens+"\n";
      }

      // Add 'hide' statement
      rules[ruleRow++]="\n\nhide.\n\n";

      // Add 'show' statement
      string show ="show holdsAt(F,T), happens(E,T), happens3(E,T,T2).";
      if(showPredicateRow>0)
      {
	show = show+"\nshow ";
	for(int s2=0; s2<showPredicateRow; s2++)
	  show.append(showPredicates[s2]+", ");
	show.erase(show.length()-2,2);
	show.append(".");
      }
      rules[ruleRow++]=show+"\n";


     // Write on a file
      ofstream outputFile;
      intermediateFileName=argv[1];
      intermediateFileName.replace(intermediateFileName.length()-2,3,".lp");
      outputFile.open(intermediateFileName.c_str());

       for(int p=0; p<ruleRow; p++)
         outputFile << rules[p];

      outputFile.close();

      cout << "The translated file '" << intermediateFileName
           << "' is created." << endl;
      cout << "The Maximum Timepoint is " << maxOfTimepoint << endl << endl;

      return 0;
}
////////////// END OF MAIN /////////////////////////////

void usage()
{
  cout << "Usage: ecasp <filename>" << endl;
  cout << endl;
}

void initializeSorts()
{
  sorts[sortRow++].sName="fluent";           
  sorts[sortRow++].sName="event";            
  sorts[sortRow++].sName="time";           //$ "Time"
  subSortOfInteger[subSortInt++]="time";   //$ "Time"
  sorts[sortRow++].sName="offset";         //$ "Offset"
  subSortOfInteger[subSortInt++]="offset"; //$ "Offset
}

int isWhiteSpace(string aLine)
{
  int len=aLine.length();
  for(int i=0; i<len; i++)
  {
    if(aLine[i]!=' ' &&  aLine[i]!='\t' 
       && aLine[i]!='\n')
      return 0;
    else;
  }
  return 1;
}

int loadComment(string aLine)
{
  int len=aLine.length();
  for(int i=0; i<len; i++)
  {
    if(aLine[i]==' ' || aLine[i]=='\t')
      continue;
    else 
    {  
      if(aLine[i]==';')     
	return 1;
      else
  	return 0;
    }
  }
}

void removeComment(string& target)
{
  int len=target.length();
  int found=-1;
  if(len!=0)
  {
    for(int i=0; i<len; i++)
      if(target[i]==';')
	{found=i; break;}

    if(found>0)
      target=target.substr(0,found);
  }
}

int loadOption(string aLine)
{
  removeComment(aLine);
  int numOfToken=0;
  string orgLine=aLine;
  string token = split(aLine, " \t");
  int key=-1;
  int error=0;

  while(token.compare("")!=0)
  {
    numOfToken++;
    
    if(numOfToken==1)
      if(token.compare("option")!=0)
	return 0;
  
    // ##  option statements except for finalstatefile and weighted
    if(numOfToken==2)
    {
      if(token.compare("debug")==0 ||
	 token.compare("manualrelease")==0 ||
	 token.compare("modeldiff")==0 ||
	 token.compare("renaming")==0 ||
	 token.compare("showpred")==0 ||
	 token.compare("timediff")==0 ||
	 token.compare("tmpfilerm")==0 ||
	 token.compare("trajectory")==0)
	key=1;
      else if(token.compare("encoding")==0)
	key=2; // The value of this option will be 2 or 3
      else if(token.compare("solver")==0)
	key=3; // relsat, walksat, minisat, and maxwalksat
      else
      {
	cout << "ERROR: Unknown option in '" 
             << orgLine << "'" << endl << endl;
         exit(EXIT_FAILURE);
      }
    }
  
    if(numOfToken==3)
    {
      switch(key){
      case 1: if(token.compare("on")!=0 &&
                 token.compare("off")!=0)
	error=1; break;
      case 2: if(stringToInteger(token)!=2 && 
                 stringToInteger(token)!=3)
	error=1; break;
      case 3: if(token.compare("relsat")!=0 &&
                 token.compare("walksat")!=0 &&
                 token.compare("minisat")!=0 &&
                 token.compare("maxwalksat")!=0)
	error=2; break;
      }
    }

    token = split(aLine, " \t");
  } // end of while loop

  if(numOfToken!=3)
  {
      cout << "ERROR: option not followed by optionname optionvalue.\n\n";
      exit(EXIT_FAILURE);
  }

  if(error==1)
  {
         cout << "ERROR: The invalid value in '"
	      << orgLine << "'" << endl << endl;
         exit(EXIT_FAILURE);
  }
  else if(error==2)
  {
         cout << "ERROR: Unknown solver in '"
	      << orgLine << "'" << endl << endl;
         exit(EXIT_FAILURE);
  }
  else
    return 1;
}

string split(string &str, string delimiter)
{
  string token("");

  int index=str.find_first_of(delimiter, 0);
  if(index>=0)
  {
    if(index==0)
      return split(str.erase(0,1), delimiter);

    token=str.substr(0,index);
    str.erase(0,index+1);
    return token;
  }
  else
  {token=str; str=""; return token;} // Last element
}

int loadLoad(string aLine)
{
  removeComment(aLine);
  string temp("");
  string ec="foundations/EC.e";
  string dec="foundations/DEC.e";
  string root="foundations/Root.e";
  string eccausal="foundations/ECCausal.e";
  int numOfToken=0;
  string token = split(aLine, " \t");

  while(token.compare("")!=0)
  {
    numOfToken++;
    
    if(numOfToken==1)
      if(token.compare("load")!=0)
	return 0;

    if(numOfToken==2)
    {
      if(token.compare(ec)!=0 && token.compare(dec)!=0 &&
         token.compare(root)!=0 && token.compare(eccausal)!=0)
      {
	cout << "ERROR: No such file or directory '"
	     << token << "'" << endl << endl;
	exit(EXIT_FAILURE);
      }
 
      temp=token;
    }

    if(numOfToken==3)
    {
      temp=temp+" "+token;
      cout << "ERROR: No such file or directory '"
           << temp << "'" << endl << endl;
      exit(EXIT_FAILURE);
    }

    token = split(aLine, " \t");
  } // end of while loop
  
  if(numOfToken==1) // Exist only 'load' keyword
  {
      cout << "ERROR: No file to load" << endl << endl;
      exit(EXIT_FAILURE);
  }
  else 
    return 1;
}

int loadSort(string aLine)
{
  removeComment(aLine);
  string orgLine=aLine;
  string subsort="", token="";
  int numOfToken=1;
  int number=0;
  flag_colon=0;
  int subsort_error=0;
  int numOfsorts=0;

  token=split(aLine," \t");
  if(token.compare("sort")!=0)
    return 0;
 
  declaration=1; // For Sort
  checkParsErrorsOfDeclaration(orgLine,aLine); 
  
  if(flag_colon)
    token = split(aLine, " :\t");
  else
    token = split(aLine, " ,\t");

  while(token.compare("")!=0)
  {
    numOfToken++;

    if( numOfToken==2 || 
       (numOfToken>=3 && !flag_colon))
      if(!isSort(token))
      {
	cout << "ERROR: The invalid sort name '" << token 
             << "' in '" << orgLine << "'\n\n";
	exit(EXIT_FAILURE);
      }
      else // Adds a token to sorts
      {
	if(existSortName(token))
	{
	  cout << "ERROR: The sort name '" << token <<"' in '" 
               << orgLine << "' has been already declared.\n\n";
	  exit(EXIT_FAILURE);
	}

	// Get sort names
	numOfsorts++;
	subsort=token;
	sorts[sortRow++].sName=token;
	constants[constRow++].sName=token;
	// For variables in universal quantifier
	token[0]=toupper(token[0]);
	if(!existInAllVars(token))
	  allVars[numOfallVars++]=token; 
      }

    if(numOfToken==3 && flag_colon)
    {
      if(token.compare("integer")==0)
	subSortOfInteger[subSortInt++]=subsort;      
      else if(existSortName(token))
      {
	number=sorts[indexOfSort].num; 
	sorts[indexOfSort].subSorts[number++]=subsort;
	sorts[indexOfSort].num=number;
	subSortRules[subSortRow++]
	=token+"("+"Subsort"+")"+" :- "+subsort+"("+"Subsort"+").";
      }      
      else
      {subsort_error=1; break;}
    }
    
    if(flag_colon)
      token = split(aLine, " :\t");
    else
      token = split(aLine, " ,\t");
  }   

  if(flag_colon && numOfToken!=3 || subsort_error)
  {
      cout << "ERROR: Subsort declaration in '" << orgLine << "'\n\n";
      exit(EXIT_FAILURE);
  }
  else if(numOfsorts!=numOfcomma+1)
    showSyntaxErrorMsg(orgLine);
  else
    return 1;
}

void checkParsErrorsOfDeclaration(string orgLine, string aLine)
{
  int leftParen=-1, rightParen=-1;
  int numberOfToken=0;
  string temp=aLine;
  string token("");
  int flag_error=0;
  int numOfcolon=0;
  int numOfleft_parenthesis=0;
  int numOfright_parenthesis=0;
  numOfcomma=0;

  /* Case 1) Only keywords or constant's sort declared. */
  token=split(temp, " \t");
  if(token.compare("")==0)
  {
    if(declaration==1)
      cout << "ERROR: No sort name declared!\n\n";
    if(declaration==2)
      cout << "ERROR: Only constant's sort '" 
           << orgLine << "' declared! (No constants)\n\n";
    if(declaration==3)
      cout << "ERROR: No fluent name declared!\n\n";
    if(declaration==4)
      cout << "ERROR: No event name declared!\n\n";
    if(declaration==5)
      cout << "ERROR: No predicate name declared!\n\n";
    //    if(declaration==6)
    //  cout << "ERROR: No predicate name declared for completion!\n\n";
    exit(EXIT_FAILURE);
  }
 
  // Case 2) Syntax error regarding ',' , ':', '(', or ')'
  temp=aLine;
  trimSpace(temp);
  for(int i=0; i<temp.length(); i++)
  {
    if(i<temp.length()-1)
      if( (temp[i]==',' && temp[i+1]==',') ||
          (temp[i]=='(' && temp[i+1]=='(') ||
          (temp[i]==')' && temp[i+1]==')') ||
          (temp[i]==')' && temp[i+1]=='(') ||
          (temp[i]=='(' && temp[i+1]==',') ||
          (temp[i]==',' && temp[i+1]=='(') ||
          (temp[i]==')' && temp[i+1]==',') ||
          (temp[i]==',' && temp[i+1]==')') )
	{flag_error=1; break;} 
    if(temp[i]==':')
      {flag_colon=1; numOfcolon++;}
    if(temp[i]==',')
      numOfcomma++;
    if(temp[temp.length()-1]==',' || temp[i]=='.' || 
       !isalpha(temp[0]))
      {flag_error=1; break;}
    if(temp[i]=='(')
      {numOfleft_parenthesis++; leftParen=i;}
    if(temp[i]==')')
      {numOfright_parenthesis++; rightParen=i;}
  }

  if(flag_error || numOfcolon>=2)
    showSyntaxErrorMsg(orgLine);

  // Case 3) For fluent, event or predicate declaration 
  if(declaration==3 || declaration==4 || declaration==5)
  {
    numberOfToken=0;
    temp=aLine;
    string errorMsg("");

    if(  numOfright_parenthesis!=1 ||
         numOfleft_parenthesis!=1  || 
	 leftParen>rightParen ||
         temp[temp.length()-1]!=')' )
      showSyntaxErrorMsg(orgLine); // For parentheses

    token=split(temp," ()\t");
    if(!isPredicate(token))
    {
      if(declaration==3)
	errorMsg="ERROR: The invalid fluent name '"; 
      if(declaration==4)
	errorMsg="ERROR: The invalid event name '"; 
      if(declaration==5)
	errorMsg="ERROR: The invalid predicate name '"; 
      cout << errorMsg << token << "' in " << orgLine << "\n\n";
      exit(EXIT_FAILURE);
    }    
  } 
}

void showSyntaxErrorMsg(string orgLine)
{
    cout << "ERROR: please check syntax in '" 
	 << orgLine << "'\n\n";
    exit(EXIT_FAILURE);
}

void trimSpace(string& aLine)
{
  int indexOfSpace=aLine.find_first_of(' ',0);
  while(indexOfSpace>=0)
  {
    aLine.erase(indexOfSpace,1);      // Remove space
    indexOfSpace=aLine.find_first_of(' ',0);
  }

  int indexOfTab=aLine.find_first_of('\t',0);
  while(indexOfTab>=0)
  {
    aLine.erase(indexOfTab,1);        // Remove tab
    indexOfTab=aLine.find_first_of('\t',0);
  }   
}

int isSort(string token)
{
  for(int i=0; i<token.length(); i++)
  { 
    if(!islower(token[0])) //$ !isupper(token[0]))
       return 0;
    if(!isalpha(token[i]))
       return 0;
  } 
  return 1;
}

int existSortName(string token)
{
  indexOfSort=-1;

  for(int i=0; i<sortRow; i++)
    if(sorts[i].sName.compare(token)==0) //sorts[i].compare(token)==0)
    {
      indexOfSort=i;
      return 1;
    }

  return 0;
}

int loadConstant(string aLine)
{
  removeComment(aLine);
  string orgLine=aLine;
  int numOfToken=1;
  int index=0;
  int number=0;
  int numOfcons=0;
  string token = split(aLine, " \t");
  
  if(token.compare("fluent")==0 ||
     token.compare("event")==0  ||
     !existSortName(token))
    return 0;
  else
    index=findIndexOfConst(token);
  
  declaration=2; // For Constant 
  checkParsErrorsOfDeclaration(orgLine,aLine);

  token = split(aLine, " ,\t");
  while(token.compare("") != 0)
  {
    numOfToken++;

    if(numOfToken>1)
      if(!isConstant(token))
      {                    
	cout << "ERROR: The invalid constant name '" 
	     << token << "' in '" << orgLine << "'\n\n";
	exit(EXIT_FAILURE);
      }
      else
      { 
	numOfcons++;
	number=constants[index].num;
	constants[index].objects[number++]=token;
	constants[index].num=number;
      }
   
    token = split(aLine, " ,\t");
  }

  if(numOfcons!=numOfcomma+1)
    showSyntaxErrorMsg(orgLine);
  return 1;
}

int findIndexOfConst(string sN)
{
  if(constRow==0)
    return 0;

  for(int i=0; i<constRow; i++)
    if(constants[i].sName.compare(sN)==0)
      return i; 
}

int isConstant(string token)
{
// A constant consists of one or more digits
// or an lowercase letter followed by zero or 
// more letters or digits
  if(isNumber(token))
    return 1;
  else
  {  for(int i=0; i<token.length(); i++)
      { 
	if(!isupper(token[0])) //$ islower....
	  return 0;
	if(!isalnum(token[i]))
	  return 0;
      }
  } 
  return 1;
}

int loadFluent(string aLine)
{   
  removeComment(aLine);
  string orgLine=aLine;
  int numOfToken=1;
  int number=0;
  int numOfsorts=0;
  string predicateName("");
  string fstr("");
  string token=split(aLine," \t");
  if(token.compare("fluent")!=0)
    return 0;

  declaration=3; // For Fluent
  checkParsErrorsOfDeclaration(orgLine,aLine); 

  token = split(aLine, " (,)\t");
  while(token.compare("")!=0)
  {
    numOfToken++;

    if(numOfToken==2)
    {
      fluents[fluentRow].pName=token;
      predicateName=token;
    }
    if(numOfToken>2) // for arguments of fluent
      if(!existSortName(token))
      {
	cout << "ERROR: The undefined sort name '" 
             << token << "' in " << orgLine << endl << endl;
	exit(EXIT_FAILURE);
      }
      else
      {
	numOfsorts++;
	fluents[fluentRow].srts[number++]=token;
      }

    token = split(aLine, " (,)\t");
  } // end of while

   if(numOfsorts!=0 && numOfsorts!=numOfcomma+1)
     showSyntaxErrorMsg(orgLine); 

  fluents[fluentRow].num=number;
  string f="fluent(";
  predicateName[0]=tolower(predicateName[0]);
  //$   predicateName[0]=tolower(predicateName[0]);
  f=f+predicateName+"(";

  string temp("");
  for(int j=0; j<number; j++)
  {
    fstr=fluents[fluentRow].srts[j];
    if(fstr.compare(temp)==0)
      fstr=fstr+integerToString(j);
    
    temp=fstr;
    fstr[0]=toupper(fstr[0]);
    //$ fstr[0]=toupper(fstr[0]);
      
    if(!existInAllVars(fstr))
      allVars[numOfallVars++]=fstr;     
    f=f+fstr;
    
    if(j+1<number)
      f=f+",";
  }

  f=f+")).";

  declarationOfFluents[decFluentRow++]=f;
  fluentRow++;
  return 1;
}

int isPredicate(string token)
{
  // predicatesymbols consist of an uppercase letter  
  // followed by zero or more letters or digits
  for(int i=0; i<token.length(); i++)
  { 
    if(!isupper(token[0]))  
       return 0;
    if(!isalnum(token[i]))
       return 0;
  } 
  return 1;
}

string integerToString(int i)
{
  string s;
  stringstream out;
  out << i;
  s=out.str();
  return s;
}

bool existVarsInExstQuan(string target)
{
  if(numOfvarsExstQuan==0)
    return false;
  else
  {
    for(int i=0; i<numOfvarsExstQuan; i++)
      if(target.compare(varsExstQuan[i])==0)
	return true;
  }
  return false;
}

bool existInAllVars(string target)
{
  if(numOfallVars==0)
    return false;

  else
  {
    for(int i=0; i<numOfallVars; i++)
      if(target.compare(allVars[i])==0)
	return true;
  }

  return false;
}

int loadEvent(string aLine)
{   
  removeComment(aLine);
  string orgLine=aLine;
  int numOfToken=1;
  int number=0;
  int numOfsorts=0;
  string predicateName("");
  string estr("");
  string token=split(aLine," \t");
  if(token.compare("event")!=0)
    return 0;

  declaration=4; // For Event
  checkParsErrorsOfDeclaration(orgLine,aLine);
  
  token = split(aLine, " (,)\t");
  while(token.compare("")!=0)
  {
    numOfToken++;

    if(numOfToken==2) 
    {
      events[eventRow].pName=token;
      predicateName=token;
    }
    if(numOfToken>2) // for arguments of event
      if(!existSortName(token))
	{
	  cout << "ERROR: The undefined sort name '" 
               << token << "' in " << orgLine << endl << endl;
	  exit(EXIT_FAILURE);
	}
      else
      {
	numOfsorts++;
	events[eventRow].srts[number++]=token;
      }

    token = split(aLine, " (,)\t");
  } // end of while

   if(numOfsorts!=0 && numOfsorts!=numOfcomma+1)
     showSyntaxErrorMsg(orgLine); 

  events[eventRow].num=number;
  string e="event(";
  predicateName[0]=tolower(predicateName[0]);
  //$ predicateName[0]=tolower(predicateName[0]);
  e=e+predicateName+"(";
  
  string temp("");
  for(int j=0; j<number; j++)
  {
    estr=events[eventRow].srts[j];
    if(estr.compare(temp)==0)
      estr=estr+integerToString(j);

    temp=estr;
    estr[0]=toupper(estr[0]);
    //$ estr[0]=toupper(estr[0]);

    if(!existInAllVars(estr))
      allVars[numOfallVars++]=estr; 
      
    e=e+estr;
   
    if(j+1<number)
      e=e+",";
  }

  e=e+")).";
  
  declarationOfEvents[decEventRow++]=e;
  eventRow++;
  return 1;
}
  
// Pass predicate declarations
int loadPredicate(string aLine)
{  
  removeComment(aLine);
  string orgLine=aLine;
  int numOfToken=1;
  int number=0;
  int numOfsorts=0;
  string predicateName("");
  string prestr("");
  string token=split(aLine," \t");
  if(token.compare("predicate")!=0)
    return 0;
  
  declaration=5; // For Predicate declared explicitly.
  checkParsErrorsOfDeclaration(orgLine,aLine);

  token = split(aLine, " (,)\t");
  while(token.compare("")!=0)
  {
    numOfToken++;

    if(numOfToken==2) 
    {
      predicates[predicateRow].pName=token;
      predicateName=token;
    }
    if(numOfToken>2) // for arguments of predicate
      if(!existSortName(token))
	{
	  cout << "ERROR: The undefined sort name '" 
               << token << "' in " << orgLine << endl << endl;
	  exit(EXIT_FAILURE);
	}
      else
      {
	numOfsorts++;
	predicates[predicateRow].srts[number++]=token;
      }

    token = split(aLine, " (,)\t");
  } // end of while

   if(numOfsorts!=0 && numOfsorts!=numOfcomma+1)
     showSyntaxErrorMsg(orgLine); 

  predicates[predicateRow].num=number;
  string p="";
  predicateName[0]=tolower(predicateName[0]);
  //$ predicateName[0]=tolower(predicateName[0]);
  p=predicateName+"(";
  
  string temp("");
  for(int j=0; j<number; j++)
  {
    prestr=predicates[predicateRow].srts[j];
    if(prestr.compare(temp)==0)
      prestr=prestr+integerToString(j);

    temp=prestr;
    prestr[0]=toupper(prestr[0]);
    //$ estr[0]=toupper(estr[0]);

    if(!existInAllVars(prestr))
      allVars[numOfallVars++]=prestr; 
    p=p+prestr;
   
    if(j+1<number)
      p=p+",";
  }

  p=p+")";
   
  showPredicates[showPredicateRow++]=p;
  predicateRow++;
  return 1;
}

// ####
int loadCompletion (string aLine)
{
  removeComment(aLine);
  string orgLine=aLine;
  int numOfToken=0;
  string token = split(aLine, " \t");
  while(token.compare("")!=0)
  {
    numOfToken++;

    if(numOfToken==1)
      if(token.compare("completion")!=0)
        return 0;

    if(numOfToken==2)
      if(token.compare("Happens")!=0)
      {
        cout << "ERROR: completion is NOT applicable to '"
             << token << "' in " << orgLine << endl << endl; 
        exit(EXIT_FAILURE);
      }

    token = split(aLine, " \t");
  }

  if(numOfToken==1) // Only exists 'completion' keyword in Line
    {
      cout << "ERROR: No predicate for completion"
           << endl << endl;
      exit(EXIT_FAILURE);
    }
  else if(numOfToken>2)
    {
      cout << "ERROR: completion not followed by Happens predicate.\n\n";
      exit(EXIT_FAILURE);
    }
  else
    return 1;

// Restriction to only Happens and Abnormal predicates at this time
  /*
  declaration=6; // For completion
  checkParsErrorsOfDeclaration(orgLine,aLine); 

  token = split(aLine, " (,)\t");
  while(token.compare("")!=0)
  {
    numOfToken++;

    if(numOfToken==2)
      if(token.compare("Happens")!=0 || !isAbPredicate(token))
      {
	cout << "ERROR: completion is NOT applicable to '"
	     << token << "' predicate\n\n";
	exit(EXIT_FAILURE);
      }
      else

    token = split(aLine, " (,)\t");
  }
  */
}

int isAbPredicate(string target)
{
  int flag=0;
  for(int i=0; i<target.length(); i++)
  {
    if(target[0]!='A' || target[1]!='b' ||
       (i>1 && !isdigit(target[i])))
      {flag=1; break;}
  }

  if(flag)
    return 0; 
 
  return 1;
}

int loadRange(string aLine)
{
  removeComment(aLine);
  string orgLine=aLine;
  int numOfToken=0;
  string sortName("");
  int rangeMax=0;
  int rangeMin=0;
  bool flag_offset=false;
  string token = split(aLine, " \t");
  while(token.compare("")!=0)
  {
    numOfToken++;
    
    if(numOfToken==1)
      if(token.compare("range")!=0)
	return 0;
    if(numOfToken==2)
    {
      if(!isSubsortOfInteger(token))
      {
	cout << "ERROR: The sort name '" << token << "' in '"
             << orgLine << "' is not a subsort of Integer.\n\n";
	exit(EXIT_FAILURE);
      }
      else
      {
	sortName=token;
	if(sortName.compare("offset")==0) 
	  flag_offset=true;
      }
    }
    if(numOfToken==3)
      if(isNumber(token))
	rangeMin = stringToInteger(token);
      else
      {
	cout << "ERROR: The min value '" << token 
             << "' in '" << orgLine << "' is not valid.\n\n";
	exit(EXIT_FAILURE);
      }
    if(numOfToken==4)
      if(isNumber(token))
      {
	rangeMax = stringToInteger(token);
   
	if(sortName.compare("time")==0)
	{
	  rangeMaxTime=rangeMax;
	  rangeMinTime=rangeMin;
	  maxOfTimepoint=token;
	}
      }
      else
      {
	cout << "ERROR: The max value '" << token 
             << "' in '" << orgLine << "' is not valid.\n\n";
	exit(EXIT_FAILURE);
      }
    
    token = split(aLine, " \t");
  }

  if(numOfToken!=4) 
  {
    cout << "ERROR: range not followed by sort min max: '" 
         << orgLine << "'\n\n"; 
    exit(EXIT_FAILURE);
  }
  else if(flag_offset && (rangeMin<1 || rangeMax<1))
  {
    cout << "ERROR: Invalid offset values in '" << orgLine << "'\n\n"; 
    exit(EXIT_FAILURE);
  }   
  else if(rangeMax<rangeMin)
  {
    cout << "ERROR: The max value is less than the min value: '"
         << orgLine << "'\n\n"; 
    exit(EXIT_FAILURE);
  }   
  else 
  {
    rangeForm[rangeCol++]=sortName+"("+integerToString(rangeMin)+".."
                                  +integerToString(rangeMax)+").";    
    if(flag_offset)
    {
      rangeMaxOffset=rangeMax;
      rangeMinOffset=rangeMin;
    }
    return 1;
  }
}

int isSubsortOfInteger(string s)
{
  for(int i=0; i<subSortInt; i++)
    if(s.compare(subSortOfInteger[i])==0)
      return 1;
       
  return 0;
}

int isNumber (string target)
{
  int len=target.length();
  if(len==0)
    return 0;

  int start=0;

  if(target[0]=='-')
    start=1;

  for(int i=start; i<len; i++)
    if(!isdigit(target[i]))
      return 0;
  return 1;
}

int stringToInteger(string number)
{
  int i;
  stringstream out;
  out.str(number);
  out >> i;
  return i;
}

int isEndOfFormula(string aLine)
{
  // Every formula has one period at the end.
  removeComment(aLine);
  int len=aLine.length();
  for(int i=0; i<len; i++)
  {
    if(i!=len-1)
    {  if(aLine[i]=='.')
       {
	 for(int j=i+1; j<len; j++)
	   if(aLine[j]!=' ' && aLine[j]!='\t')
	   {
	     cout << "ERROR: Not allowed to write" 
                  << " any character after the period"
                  << " on the same line.\n\n";
	     exit(EXIT_FAILURE);
	   }

	 return 1;
       }
    }
    else
    {
      if(aLine[i]=='.')
	return 1;
      else 
	return 0;
    }
  }
}

int loadRulesOfASP(istream& inFile, string aLine)
{
  removeComment(aLine);
  string temp=aLine;
  trimSpace(aLine);
  if(aLine.find("_asp")!=0 && aLine.find("_ASP")!=0)
    return 0;

  int len=aLine.length();  
  while( !(aLine[len-2]=='}' && aLine[len-1]=='.') && 
	 !inFile.eof() )
  {
    getline(inFile, aLine);
    if(isWhiteSpace(aLine))
      continue;
    temp.append(1, ' ');
    temp.append(aLine);
    trimSpace(aLine);
    len=aLine.length();
  }

  checkSyntaxOfASPDirective(temp);

  string token=split(temp,".");
  while(token.compare("")!=0)
  {
    if(isWhiteSpace(token));
    else
      rulesOfASP[nOfRulesOfASP++]=token+"."; 
    token=split(temp,".");
  }
  return 1;
}

// Check syntax of ASP directives and
// return ASP rules without the directives
void checkSyntaxOfASPDirective(string& target)
{
  string org = target;
  trimSpace(target);
  int len=target.length();
  if(target[4]!='{' || target[len-2]!='}' || target[len-1]!='.')
  {
    cout << "Error: " 
         << "Check Syntax of ASP directives!!\n"<<endl;
    exit(EXIT_FAILURE);
  }

  int s=org.find_first_of('{',0);
  int e=org.find_last_of('}',org.length()-1);
  target=org.substr(s+1,e-s-1);
}

// Check parse errors (syntax errors)
void checkParsError(string form)
{
  string copyForm=orgFormula;
  string next(""), prev("");
  int l=0;
  next=split(copyForm, " \t\n");
  while(next.compare("")!=0)
  {  
    l=prev.length();
    if(l!=0)
      if(isalnum(prev[l-1]) &&
	 isalnum(next[0]))
	giveParsError(); // If there exists white space
                         // between names 
	 
    prev=next;
    next=split(copyForm, " \t\n");
  }

  int len=form.length();
  int tuple_start=0;
  int count=0; // for matching '(' and ')' 
  int count2=0;// for matching '{' and '}'
  int count3=0;// for matching '[' and ']'
  struct stack s;
  s.top=-1; 

  for(int i=0; i<len; i++)
  {
    if(form[i]==34  ||  // " 
       form[i]==35  ||  // # 
       form[i]==36  ||  // $   
       form[i]==39  ||  // '
       form[i]==58  ||  // :
       form[i]==63  ||  // ?
       form[i]==64  ||  // @
       form[i]==92  ||  // \
       form[i]==94  ||  // ^
       form[i]==95  ||  // _
       form[i]==96  ||  // `
       form[i]==126 )   // ~
      giveParsError();

    if(form[i]=='.')
      if(len==1)
      {
	cout << "ERROR: Only one period in a line" << endl << endl;
	exit(EXIT_FAILURE);
      }
      else
	if(!isalnum(form[i-1]) && form[i-1]!=')')
	  giveParsError();

    if(i==0)
    {
      if(form[i]=='(')
	count++;
      else if(form[i]=='{')
	count2++;
      else if(form[i]=='[')
	count3++;
      else if(isalnum(form[i]));
      else if(form[i]=='!');
      else giveParsError();
    }
    else
    {  
      if(isalnum(form[i]))
	if(form[i-1]==')')
	  giveParsError();

      if(tuple_start)
	if(!isalnum(form[i]) && form[i]!='(' &&
	   form[i]!=')' && form[i]!=',' && 
           form[i]!='-')          
 	  giveParsError();

      if(form[i]=='(')
      { 
	count++;
	if(form[i-1]==')' || form[i-1]=='{' ||
           form[i-1]=='[' || form[i-1]=='!')
 	  giveParsError();
      
	if(isalnum(form[i-1]))
	{
	  tuple_start=1; push(&s, '(');
	}
      }

      if(form[i]==')')
      {
	count--;
	if(count<0)
	   giveParsError();
	
	if(!isalnum(form[i-1]) && form[i-1]!=')' 
	   && form[i-1]!='(')
	  giveParsError();

        if(tuple_start)
	{
	  pop(&s);
	  if(isempty(&s))
	    tuple_start=0;
	}
      }

      if(form[i]=='{')
      {
	count2++;
	if(//form[i-1]!='!' && 
           form[i-1]!='(' &&
	   form[i-1]!='|' && form[i-1]!='&' &&
	   form[i-1]!='>' && form[i-1]!=']')
	  giveParsError();
      }

      if(form[i]=='}')
      {
	count2--;
	if(count2<0 || !isalnum(form[i-1]))
	  giveParsError();
      }

      if(form[i]=='[')
	giveParsError();

      if(form[i]==']')
      {
	count3--;
	if(count3<0 || !isalnum(form[i-1]))
	  giveParsError();
      }

      switch(form[i])
      {
        case '+':
        case '*':
        case '/':
        case '%':
        case ',':
        case '|':
	case '&':
	case '<': // also for "<=" and for "<->"
        	  if(!isalnum(form[i-1]) && form[i-1]!=')')
      	            giveParsError(); 
      }
    
      if(form[i]=='!') // and for "!="
        if(!isalnum(form[i-1]) && form[i-1]!='>' &&
	   form[i-1]!='(' && form[i-1]!=')' &&
           form[i-1]!=']' && form[i-1]!='}' &&
           form[i-1]!='|' && form[i-1]!='&')
	  giveParsError(); 

      if(form[i]=='-') // and for "->"
      	if(form[i-1]!='(' && form[i-1]!=')' &&
           form[i-1]!='<' && form[i-1]!='>' &&
           form[i-1]!='|' && form[i-1]!='&' && 
           form[i-1]!='=' && form[i-1]!=',' &&
           form[i-1]!='}' && !isalnum(form[i-1]))
      	  giveParsError(); 

      if(form[i]=='=')
      	if(form[i-1]!=')' && form[i-1]!='!' &&
	   form[i-1]!='<' && form[i-1]!='>' &&
	   !isalnum(form[i-1]))
	  giveParsError(); 
   
      if(form[i]=='>') // and for ">="
	if(form[i-1]!=')' && form[i-1]!='-' &&
	   !isalnum(form[i-1]))
	  giveParsError();     
    }
  } // end of 'for' loop

  if(count!=0 || count2!=0 || count3!=0)
    giveParsError();
}

void giveParsError()
{
  cout << "\nSYNTAX ERROR in the following formula: " 
       << endl << orgFormula << endl << endl;
  exit(EXIT_FAILURE);
}

/*
  Remove outermost parentheses recursively
  such as ..((variable)).. and ..((constant))..
*/
void removeOutermostParen(string& form)
{
  int index=form.find_first_of('(', 0);
  int index2=0; string target(""); 
  int len=0; int flag=0;

  while(index>0 && index<form.length()-2)
  {
    if(!isalnum(form[index-1]))
    {
      index2=form.find_first_of(')',index);
      target=form.substr(index+1,index2-index-1);
      len=target.length();
      if(len!=0)
      {
	for(int i=0; i<len; i++)
	  if(!isalnum(target[i]))
	    {flag=1; break;}
	if(!flag)
	{
	  form=form.erase(index,1);
	  form=form.erase(index2-1,1);
	  index=form.find_first_of('(', index-1);
	}
	else
	{  
	  index=form.find_first_of('(', index+1);
	  flag=0;
	}
      }
    }      
    else
      index=form.find_first_of('(', index+1);
  }
}

void changeSym(string& form)
{
  /*
    change <= to @
    change >= to # 
    change != to ~
    change -> to :
    change <->to $
  */

  int len=form.length();
  for (int i=0; i<len; i++)
  {
      //  if(i+1>=len)
      //  continue;
    
    if(i+1<len)
      {
	if(form[i]=='<' && form[i+1]=='=')
	  {form[i]='@'; form[i+1]=' ';}
	
	if(form[i]=='>' && form[i+1]=='=')
	  {form[i]='#'; form[i+1]=' ';}
	
	if(form[i]=='!' && form[i+1]=='=')
	  {form[i]='~'; form[i+1]=' ';}
	
	if(i>0 && form[i]=='-' && form[i+1]=='>' &&
	   form[i-1]!='<')
	  {form[i]=':'; form[i+1]=' ';}
      }

    if(i+2<len)
      if(form[i]=='<' && form[i+1]=='-' &&
	 form[i+2]=='>')
	{form[i]='$'; form[i+1]=' '; form[i+2]=' ';}
  }

  int len2=form.length();
  int numberOfimpl=0;
  int numberOfbi_impl=0;
  for (int i=0; i<len2; i++)
  {
    if(form[i]=='#')
      if(form[i-1]=='-')
    giveParsError();

    if(form[i]==':')
    {  
      numberOfimpl++;
      if(form[i-1]=='<' || form[i-1]=='>')
	giveParsError();
    }
    if(form[i]=='$')
      numberOfbi_impl++;
  }

  if(numberOfimpl+numberOfbi_impl>1)
    giveParsError();
}

void makeTree(string form)
{
  /* Remove outermost parentheses such as
     ..((variable)).. and ..((constant)).. */
  removeOutermostParen(form);

  markTupleEnd(form);
  freshTokensOfForm();

  char formIN[FORM_COL]={(char)NULL};
  char formPRE[FORM_COL]={(char)NULL};
  char *inFormula = (char *)&formIN;
  char *preFormula= (char *)&formPRE;

  inFormula=(char *)form.c_str();
  infixToPrefix(inFormula, preFormula);

  getTokensOfForm(preFormula); 
  root=prefixToTree();
}

/*
  Add "?" right after each (maximal) tuple_end  
*/
void markTupleEnd(string& form)
{
  int tuple_start=0;
  int len=form.length();
  struct stack s;
  s.top=-1; 

  for(int i=0; i<len; i++)
  { 
    if(i>0)
    {  
      if(form[i]=='(' && isalnum(form[i-1]))
	{tuple_start=1; push(&s,'(');}
    }

    if(form[i]==')' && tuple_start)
    {  
      pop(&s);
      if(isempty(&s))
      {
	form.insert(i+1, "?"); len++;
        tuple_start=0; 
      }
    }
   
  }
  //  return form;
}

void freshTokensOfForm()
{
  for(int i=0; i<NUM_TOKENS_FORM; i++)
    tokenOfForm[i]="";
  ntokensOfForm=0;
}

void infixToPrefix(char *input, char *output)
{
  struct stack s0,s;
  char optop;
  int len=strlen(input);
  int i=0,j=0,k=0;
  int m=0;
  int tuple_start=0;
  s0.top=-1;
  s.top=-1;
                     /* now size of infix expression is known and
                       counter 'i' indicates to end of expression*/
  i=len-1;
  int space=0;
 
  for(int u=0; u<=i; u++)
  {
    if(u>0)
      if(input[u]=='(' && isalnum(input[u-1]))
	tuple_start=1;

    if( (input[u]=='(' && !tuple_start) || 
        (input[u]==')' && !tuple_start) || 
	(input[u]=='?')
      )
      continue;
    
    else if(input[u]==')' && input[u+1]=='?' && tuple_start)
    { 
      tuple_start=0; 
      j++;
    }

    else
      j++;


    // Make space so as to easily tokenize an input string
    if(!isalnum(input[u])  && input[u]!=',' &&
        input[u]!='(' && input[u]!=')' &&
        input[u]!='{' && input[u]!='}' &&
        input[u]!='[' && input[u]!=']' && 
        input[u]!='_' &&
        !isNegation(input,u)
      )
      space=space+2;

    if(isNegation(input,u) && tuple_start==0)
      space=space+3;

    if(input[u]=='}' || input[u]==']' ||
       input[u]=='{' || input[u]=='[' ) 
      space=space+1;

  }
  
  j=j+space;
  m=j;
  j--;              /* now j indicates end of prefix expression */
  
  while(i>=0)
  {
    if(input[i]==')' && input[i+1]=='?')
        tuple_start=1;
    
    if(input[i]=='?')
      {i--; continue;}

    if(isalnum(input[i]) || tuple_start   ||
           input[i]=='{' || input[i]=='[' ||
           input[i]=='!' ||
           input[i]==',' || input[i]=='_'  ||
           isNegation(input, i))
    {
      if(input[i]==')')
	push(&s0,'T');

      if(input[i]=='(')
      {
	pop(&s0);
	if(isempty(&s0))
	  tuple_start=0;
      }
      
      if(input[i]=='!')
      {output[j--]=' '; output[j--]=input[i]; 
       output[j--]=' ';}

      else if(isNegation(input, i) && tuple_start==0)
	{output[j--]=' '; output[j--]=input[i]; 
         output[j--]=input[i]; output[j--]=' ';}

      else if(input[i]=='{' || input[i]=='[')
	{output[j--]=input[i]; output[j--]=' ';}
      
      else
	{output[j--]=input[i];}
     
    }
    else
    {
      while(!isempty(&s)&&(prcd(s.item[s.top],input[i])))
      {
	optop=s.item[s.top];
	pop(&s);
	output[j--]=' ';
	output[j--]=optop;
      }

      if(input[i]=='}' || input[i]==']')
      {
	output[j--]=' ';
	output[j--]=input[i];
      }

      else if(isempty(&s)||input[i]!='(')
      {
	push(&s,input[i]); 
	if(input[i]!=')')
	  output[j--]=' ';
      }
      else
      {
	pop(&s);
      }
    }

    i--;
  }
  
  while(!isempty(&s))
  {
    output[j--]=' ';
    optop=s.item[s.top];
    pop(&s);
    output[j--]=optop;
  }
}

// Except timepoints
int isNegation(char *input, int i)
{
  /*  if(i==0)  
    if(input[i]=='-')
      return 1;
  */
  // i+1<=strlen(input)-1

  /*  if(input[i]=='-')
    if(input[i-1]==',') // inside Tuple, it is a timepoint
    return 0; */

  if(input[i]=='-' && input[i-1]!=')' &&
     !isalnum(input[i-1]) && 
     (isalnum(input[i+1]) || input[i+1]=='(') )
    return 1;
  else 
    return 0;
}

void push(struct stack *ps, char n)
{
  if(!full(ps))
    {
      ps->top++;              // Incrementing the top
      ps->item[ps->top]=n; // Storing an element
    }
  else
    {
      cout << "ERROR: Can't push in a FULL Stack!!" 
           << endl << endl;
      exit(EXIT_FAILURE);
    }
}

void pop(struct stack *ps)
{
  if(!isempty(ps))
  {  
    //    elem=s1->data[s1->top];
    ps->top--;
  }
  else
    {
      cout << "ERROR: Can't pop in an EMPTY Stack!!\n"
	   << "       Check out Syntax!!" << endl << endl;
      exit(EXIT_FAILURE);
    }
}

int isempty(struct stack *ps)
{
  if(ps->top==-1)
    return 1;

  return 0;
}

int prcd(char stktop,char op)
{
  if(stktop==op)
    return 0;

  if(stktop==')')
    return 0;
 
  if((stktop!='(')&&(op==')'))
    return 0;
  
  if((stktop!=')')&&(op=='('))
    return 1;


///////// '}', ']' //////////////////////////////////////////////
  if( ((stktop==':')||(stktop=='$')||
       (stktop=='|')||(stktop=='&')) &&            
      ((op=='}')||(op==']'))
    )
    return 0;
  
  if( (stktop!=':')&&(stktop!='$')&&
      (stktop!='|')&&(stktop!='&')&&
      ((op=='}')||(op==']'))
    )
    return 1;


/////////  '*', '/', '%' ////////////////////////////////////////
  if((op=='*')||(op=='/')||(op=='%'))
    return 0;
  if( ((stktop=='*')||(stktop=='/')||(stktop=='%'))&&
      op!=')')
    return 1;


/////////  '+', '-' /////////////////////////////////////////////
  if( ((stktop=='+')||(stktop=='-'))&&
      ((op=='*')||(op=='/')||(op=='%')||(op=='!')))
    return 0;
  if( ((stktop=='*')||(stktop=='/')||(stktop=='%')||(stktop=='!'))
      &&((op=='+')||(op=='-')))
    return 1;

  if( (stktop!='*')&&(stktop!='/')&&(stktop!='%')&&(stktop!='!')
      &&((op=='+')||(op=='-')))
     return 0;
  if( ((stktop=='+')||(stktop=='-'))&&
      (op!='*')&&(op!='/')&&(op!='%')&&(op!='!')&&(op!=')'))
    return 1;


/////////  '<','<=','=','>=','>','!=' ///////////////////////////
///////// Change <= to @, >= to #, and != to ~
  if( ((stktop=='<')||(stktop=='@')||(stktop=='=')||(stktop=='#')||
      (stktop=='>')||(stktop=='~')) &&
       (op!='&')&&(op!='|')&&(op!='$')&&(op!=':')&&
       (op!='(')&&(op!=')'))
    return 0;
  if( (stktop!='&')&&(stktop!='|')&&(stktop!='$')&&(stktop!=':')&&
      (stktop!='(')&&(stktop!=')')&&
      ((op=='<')||(op=='@')||(op=='=')||(op=='#')|| 
       (op=='>')||(op=='~')))
    return 1;

  if( ((stktop=='&')||(stktop=='|')||(stktop=='$')||(stktop==':'))&&
      ((op=='<')||(op=='@')||(op=='=')||(op=='#')|| 
       (op=='>')||(op=='~')))
    return 0;
  if( ((stktop=='<')||(stktop=='@')||(stktop=='=')||(stktop=='#')||
       (stktop=='>')||(stktop=='~')) &&
      ((op=='&')||(op=='|')||(op=='$')||(op==':')))
    return 1;


/////////  '|', '& '//////////////////////////////////////////
  if(((stktop=='&')||(stktop=='|'))&&
     (op!='(')&&(op!=')')&&(op!=':')&&(op!='$'))
    return 0;
  if((stktop!='(')&&(stktop!=')')&&(stktop!='$')&&(stktop!=':')&&
     ((op=='|')||(op=='&')))
    return 1;

  if(((stktop==':')||(stktop=='$'))&&((op=='|')||(op=='&')))
    return 0;
  if(((stktop=='|')||(stktop=='&'))&&((op==':')||(op=='$')))
    return 1;


/////////  '->', '<->'($) //////////////////////////////////////
  if(((stktop==':')||(stktop=='$'))&&(op!='(')&&(op!=')'))
    return 0;
  if((stktop!='(')&&(stktop!=')')&&((op==':')||(op=='$')))
    return 1;
  
  /// '->','<->'  VS. '(',')' ///
  // Nothing to add

}

int full(struct stack *ps)
{
  if(ps->top==30)
    return 1;

  return 0;
}

void getTokensOfForm(char *prefix)
{ 
  char *token=strtok(prefix, " ");
  numOftokensOfForm=0;
 
  while(token != NULL)
  {
    tokenOfForm[numOftokensOfForm++]=token;
    token = strtok(NULL, " ");
  }
}

nodePtr prefixToTree()
{
  // Creates a new node
  nodePtr newNode = (nodePtr) new(struct node);

  newNode->left=NULL;
  newNode->right=NULL;
  newNode->parent=NULL;
  newNode->str=tokenOfForm[ntokensOfForm++];
  string key=newNode->str;

  if(isalnum(key[0]))
    return newNode;

  if(universalQuan(key) || existentialQuan(key) || 
     key.compare("!")==0 || key.compare("--")==0)
    newNode->left=NULL;
  else
    newNode->left=prefixToTree();
  
  newNode->right=prefixToTree(); 
  

  if(newNode->left!=NULL)
    newNode->left->parent=newNode;
  newNode->right->parent=newNode;

  return newNode;
}

bool universalQuan(string uq)
{
  string org=uq;

  if(uq[0]!='[')
    return false;

  numOfuniQuan++;
  if(numOfuniQuan==2)
  {
    cout << "ERROR: there are more than one universal" 
         << " quantifier in a formula.\n\n";
    exit(EXIT_FAILURE);
  }    

  string token=split(uq, "[,]");

  while(token.compare("")!=0)
  {
    if(!isVariable(token))
    {
      cout << "ERROR: '" << token
	   << "' in " << org << " is not a variable.\n\n";
      exit(EXIT_FAILURE);
    }
    if(!existVarsInQuan(token))
      varsQuan[numOfvarsQuan++]=token;

    token=split(uq, "[,]");
  }

  return true;
}

bool existVarsInQuan(string target)
{
  if(numOfvarsQuan==0)
    return false;
  else
  {
    for(int i=0; i<numOfvarsQuan; i++)
      if(target.compare(varsQuan[i])==0)
	return true;
  }
  return false;
}

bool existentialQuan(string eq)
{
  string org=eq;

  if(eq[0]!='{')
    return false;  

  string token=split(eq, "{,}");

  while(token.compare("")!=0)
  {
    if(!isVariable(token))
    {
      cout << "ERROR: '" << token
	   << "' in " << org << " is not a variable.\n\n";
      exit(EXIT_FAILURE);
    }
    if(!existVarsInQuan(token))
      varsQuan[numOfvarsQuan++]=token;    

    token=split(eq, "{,}");
  }

  return true;
}

/* Check parse errors (validity): for whether 
   a term is valid and a predicate constant is valid.
   Also check free variables */
void checkParsError2()
{
  if(ntokensOfForm!=numOftokensOfForm)
    giveParsError();

  getChildNodes(root);

  for(int i=0; i<nOfchildNodes; i++)
  {  
    /* For HoldsAt, ReleasedAt, Started, Stopped, 
       Initiated, Terminated */
    if(checkFormOne(childNodes[i]));

    /* For Happens */
    else if(checkHappensForm(childNodes[i]));

    /* For Initiates, Terminates, and Releases */
    else if(checkFormTwo(childNodes[i]));

    /* For Trajectory and AntiTrajectory */
    else if(checkFormThree(childNodes[i]));

    /* For Happens3 */
    else if(checkHappens3Form(childNodes[i]));

    /* For Clipped, Declipped, StoppedIn, and StartedIn */
    else if(checkFormFour(childNodes[i]));

    /* For variables and constants */
    else if(checkFormFive(childNodes[i]));

    /* For predicates that are declared explicitly */
    else if(checkFormSix(childNodes[i]));
    else
    {
      cout << "ERROR: Invalid (predicate) constant '" 
           << childNodes[i]->str << "' in the following formula:\n" 
           << orgFormula << endl << endl;
      exit(EXIT_FAILURE);
    }
  }

  // Check variables
  // Case 1: Free variables exist
  for (int i=0; i<numOfvariables; i++)
    if(!existVarsInQuan(variables[i]) &&
       variables[i].length()!=0)
    {
      cout << "\n ERROR: '" << variables[i] 
           << "' is a free variable in the following formula:\n"
           << orgFormula << endl << endl;
      exit(EXIT_FAILURE);
    }    

  // Case 2: Variables in Quantifiers are not used
  for (int j=0; j<numOfvarsQuan; j++)
    if(!existVariables(varsQuan[j]) &&
       varsQuan[j].length()!=0)
    {
      cout << "\n ERROR: The variable '" << varsQuan[j] 
           << "' is not used in the following formula:\n"
           << orgFormula << endl << endl;
      exit(EXIT_FAILURE);
    }    

  freshChildNodes();
  freshVariables();
  freshVarsQuan();
}

void getChildNodes(nodePtr tree)
{
  if(tree)
  {
    if(tree->left==NULL && tree->right==NULL)
      childNodes[nOfchildNodes++]=tree;

    getChildNodes(tree->left);
    getChildNodes(tree->right);
  }
  else
    return;
}

/* check whether a node is a form of HoldsAt, ReleasedAt, 
   Started, Stopped, Initiated, Terminated */
bool checkFormOne(nodePtr cn)
{
  string form=cn->str;
  string form2=cn->str;
  int lenOfpredicate=0;

  if(form.find(HoldsAt, 0)==0)
    lenOfpredicate=HoldsAt.length();
  else if(form.find(ReleasedAt, 0)==0)
    lenOfpredicate=ReleasedAt.length();  
  else if(form.find(Started, 0)==0)
    lenOfpredicate=Started.length();  
  else if(form.find(Stopped, 0)==0)
    lenOfpredicate=Stopped.length();  
  else if(form.find(Initiated, 0)==0)
    lenOfpredicate=Initiated.length();  
  else if(form.find(Terminated, 0)==0)
    lenOfpredicate=Terminated.length();  
  else
    return false;

  int index=form.find_first_of('(', 0);
  if(index!=lenOfpredicate)
  {
    cout << "ERROR: Invalid Predicate Name '"
         << form.substr(0,index)
         << "' in '" << cn->str << "'\n" << endl;
    exit(EXIT_FAILURE);
  }

  // Checks out valid terms
  // (Fluent_term, timepoint_term)
  form.erase(0, lenOfpredicate);   // Erases predicate name
  form.erase(0, 1);               // Erases "("
  form.erase(form.length()-1);   // Erases ")"

  form=differentiateVars(form);  // Change ',' to '`'

  string token=split(form, ",");
  int numOfToken=0;

  while(token.compare("")!=0)
  {
    numOfToken++;

    if(numOfToken==1)
    {
      if(!validFluentTerm(token))
      {
        cout << "ERROR: Invalid Fluent Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
    }
    if(numOfToken==2)
    {
      if(!validTimepointTerm(token))
      {
        cout << "ERROR: Invalid Timepoint Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
    }
  
    token=split(form, ",");
  }

  if(numOfToken!=2)
  {
    cout << "ERROR: check out the number of arguments in '"
         <<  cn->str << "'\n" << endl;
    exit(EXIT_FAILURE);
  }

  return true;
}

string differentiateVars(string form)
{
  int tuple_start=0;
  int len=form.length();
  struct stack s;
  s.top=-1;

  for(int i=0; i<len; i++)
  {
    if(i>0)
    {
      if(form[i]=='(' && isalnum(form[i-1]))
        {tuple_start=1; push(&s,'(');}
    }

    if(tuple_start)
      if(form[i]==',')
        form[i]='`';

    if(form[i]==')' && tuple_start)
    {
      pop(&s);
      if(isempty(&s))
        tuple_start=0;
    }

  }

  return form;
}

int validFluentTerm(string token)
{
  int number=0;
  int key=-1; // It will be the index of a fluent found
  string t=split(token, "(`)");
  int scan=0;

  while(t.compare("")!=0)
  {
    number++;
    
    if(number==1)
    {
      if(t.find("fluent", 0)==0)
      {
	if(t.compare("fluent")==0)
	  {addVars(t); return 1;}
	else
	{
	  string sub=t.substr(6);
	  // sort associated with a variable
	  if(isNumber(sub))
	    {addVars(t); return 1;}
	  else
	  {
	    cout << "ERROR: Invalid variable '" << t
                 << "' in Fluent Term" << endl;
	    return 0;
	  }
	}
      } 
      for(int i=0; i<fluentRow; i++)
	if(fluents[i].pName.compare(t)==0)
	{
	  key=i;
	  break;
	}

      if(key==-1)
      {
	cout << "ERROR: Invalid Fluent Name: '" 
	     << t << "'" << endl;
	return 0;
      }

    }
    else
    {
      if(scan==fluents[key].num) // Has more arguments
      {
	cout << "ERROR: Check out the number of arguments in" 
	     << " Fluent Term" <<endl;
	return 0;
      }

      string str = fluents[key].srts[scan];

      if(isConstant(t))
      {
	if(str.compare("time")==0) 
	{ 
	  int n=stringToInteger(t);
	  if(n<rangeMinTime || n>rangeMaxTime)
	  {
	    cout << "ERROR: " << t << " is Out of range for time."
                 << endl << endl;
	    exit(EXIT_FAILURE);
	  }
	}
	else
	{ 
	  if(isSubsortOfInteger(str))
	    if(isNumber(t))
	      return 1;
	    else 
	      return 0;

	  if(existConstant(t))
	  {
	    if(sortNameOfConst.compare(str)!=0)
	    {
	      if(isSubsortOf(sortNameOfConst,str))
		scan++;
	      else
		return 0;
	    }
	    else
	      scan++;
	  }
	  else
	    return 0;
	}
      }
      
      else // Is not a constant
      {
	if(isSubsortOf(t, str))
	  scan++;
	else
	{
	  if(t.find(str, 0)!=0)
	  { 
	    cout << "ERROR: Invalid sort '" << t
		 << "' in Fluent Term" << endl;  
	    return 0;
	  }
	  if(t.compare(str)!=0) // t.length()>str.length()
          {
	    string sub=t.substr(str.length());
	    // sort associated with a variable
	    if(isNumber(sub)) 	
	      scan++;
	    else
 	    {
	      cout << "ERROR: Invalid variable '" << t 
		   << "' in Fluent Term" <<endl;
	      return 0;
	    }
	  }
	  else
	    scan++;
	}

	addVars(t);
      }
    }
    
    t=split(token, "(`)");
  }

  if(number!=(fluents[key].num+1))
  {
    cout << "ERROR: Check out the number of arguments in" 
	 << " Fluent Term" <<endl;
    return 0;
  }

  return 1;
}

void addVars(string target)
{
  if(!existVariables(target))
   variables[numOfvariables++]=target;
}

bool existVariables(string target)
{
  if(numOfvariables==0)
    return false;
  else
  {
    for(int i=0; i<numOfvariables; i++)
      if(target.compare(variables[i])==0)
	return true;
  }
  return false;
}

int isSubsortOf(string sub, string super)
{
  if(!islower(sub[0]))
    return 0;

  int key=-1;
  string target("");
  for(int j=0; j<sub.length(); j++)
    if(sub[j]>='0' && sub[j]<='9')
      {key=j; break;}

  if(key!=-1)
    target=sub.substr(0,key);
  else
    target=sub;

  if(!existSortName(target))
    return 0;

  existSortName(super); // Get 'indexOfSort'


  for(int i=0; i<sorts[indexOfSort].num; i++)
    if(target.compare(sorts[indexOfSort].subSorts[i])==0)
      return 1;

  return 0;
}

bool existConstant(string target)
{ 
  sortNameOfConst="";

  if(constRow==0)
      return false;
  
  for(int i=0; i<constRow; i++)
  {
    int number=constants[i].num;
    for(int j=0; j<number; j++)
      if(constants[i].objects[j].compare(target)==0)
      {
	sortNameOfConst=constants[i].sName;
	return true;
      }
  }
  return false;
}

int validTimepointTerm(string token)
{
  if(isNumber(token))
  {
    int number=stringToInteger(token);

    if(!flag_trajectory)
    {
      if(number>rangeMaxTime || number<rangeMinTime)
      {
	cout << "Time '" << token 
	     << "' is out of range for Timepoint" << endl;
	return 0;
      }
      else
	return 1;
    }
    else
    { 
      if(number<rangeMinOffset || number>rangeMaxOffset)
      {
	cout << "ERROR: Time '" << token 
	     << "' is out of range for Offset" << endl;
	return 0;
      }
      return 1;
    }
  }
  else
  {
    if(token.compare("offset")==0 ||
       token.compare("time")==0)
      {addVars(token); return 1;}

    string sub("");
    if(token.find("offset", 0)==0) 
      sub=token.substr(6);
    if(token.find("time", 0)==0)
      sub=token.substr(4);

    // sort associated with a variable
    if(isNumber(sub)) 	
      {addVars(token); return 1;}
    else
    {
      cout << "ERROR: Invalid variable '" << token
	   << "' in Timepoint Term" <<endl;
      return 0;
    }
  }
}

/* check whether a node is a form of Happens*/
bool checkHappensForm(nodePtr cn)
{
  string form=cn->str;
  string form2=cn->str;
  int lenOfpredicate=0;

  if(form.find(Happens, 0)==0)
    if(form.find(Happens3,0)==0)
      return false;
    else
      lenOfpredicate=Happens.length();
  else
    return false;

  int index=form.find_first_of('(', 0);
  if(index!=lenOfpredicate)
  {
    cout << "ERROR: Invalid Predicate Name '"
         << form.substr(0,index)
         << "' in '" << cn->str << "'\n" << endl;
    exit(EXIT_FAILURE);
  }

  // Checks out valid terms
  // (Fluent_term, timepoint_term)
  form.erase(0, lenOfpredicate);   // Erases 'Happens'
  form.erase(0, 1);               // Erases "("
  form.erase(form.length()-1);   // Erases ")"

  form=differentiateVars(form);  // Change ',' to '`'

  string token=split(form, ",");
  int numOfToken=0;

  while(token.compare("")!=0)
  {
    numOfToken++;

    if(numOfToken==1)
    {
      if(!validEventTerm(token))
      {
        cout << "ERROR: Invalid Event Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
    }
    if(numOfToken==2)
    {
      if(!validTimepointTerm(token))
      {
        cout << "ERROR: Invalid Timepoint Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
    }
  
    token=split(form, ",");
  }

  if(numOfToken!=2)
  {
    cout << "ERROR: check out the number of arguments in '"
         <<  cn->str << "'\n" << endl;
    exit(EXIT_FAILURE);
  }

  return true;
}

int validEventTerm(string token)
{
  int number=0;
  int key=-1; // It will be the index of an event found
  string t=split(token, "(`)");
  int scan=0;

  while(t.compare("")!=0)
  {
    number++;
    
    if(number==1)
    {
      if(t.find("event", 0)==0)
      {
	if(t.compare("event")==0)
	  {addVars(t); return 1;}
	else
	{
	  string sub=t.substr(5);
	  // sort associated with a variable
	  if(isNumber(sub))
	    {addVars(t); return 1;}
	  else
	  {
	    cout << "ERROR: Invalid variable '" << t
                 << "' in Event Term" << endl;
	    return 0;
	  }
	}
      } 
      for(int i=0; i<eventRow; i++)
	if(events[i].pName.compare(t)==0)
	{
	  key=i;
	  break;
	}

      if(key==-1)
      {
	cout << "ERROR: Invalid Predicate Name: '" 
	     << t << "'" << endl;
	return 0;
      }

    }
    else
    {
      if(scan==events[key].num) // Has more arguments
      {
	cout << "ERROR: Check out the number of arguments in"
	     << " Event Term" <<endl;
	return 0;
      }
      
      string str=events[key].srts[scan];

      if(isConstant(t))
      {
	if(str.compare("Time")==0) 
	{ 
	  int n=stringToInteger(t);
	  if(n<rangeMinTime || n>rangeMaxTime)
	  {
	    cout << "ERROR: " << t << " is Out of range for time."
                 << endl << endl;
	    exit(EXIT_FAILURE);
	  }
	}
	else
	{ 
	  if(isSubsortOfInteger(str))
	    if(isNumber(t))
	      return 1;
	    else 
	      return 0;

	  if(existConstant(t))
	  {
	    if(sortNameOfConst.compare(str)!=0)
	    {
	      if(isSubsortOf(sortNameOfConst,str))
		scan++;
	      else
		return 0;
	    }
	    else
	      scan++;
	  }
	  else
	    return 0; 
	}
      }

      else
      {
	if(isSubsortOf(t, str))
	  scan++;
	else
	{
	  if(t.find(str, 0)!=0)
	  {
	    cout << "ERROR: Invalid sort '" << t 
		 << "' in Event Term" <<endl;
	    return 0;
	  }
	 
	  if(t.compare(str)!=0) // t.length()>str.length()
	  {
	    string sub=t.substr(str.length());
	    // sort associated with a variable
	    if(isNumber(sub)) 	
	      scan++;
	    else
	    {
	      cout << "ERROR: Invalid variable '" << t 
		   << "' in Event Term" <<endl;
	      return 0;
	    }
	  }
	  else
	    scan++;
	}

	addVars(t);
      }
    }

    t=split(token, "(`)");
  }

  if(number!=(events[key].num+1))
  {
    cout << "ERROR: Check out the number of arguments in"
	 << " Event Term" <<endl;
    return 0;
  }

  return 1;
}

/* check whether a node is a form of
   Initiates, Terminates, and Releases */
bool checkFormTwo(nodePtr cn)
{
  string form=cn->str;
  string form2=cn->str;
  int lenOfpredicate=0;

  if(form.find(Initiates, 0)==0)
    lenOfpredicate=Initiates.length();
  else if(form.find(Terminates, 0)==0)
    lenOfpredicate=Terminates.length();  
  else if(form.find(Releases, 0)==0)
    lenOfpredicate=Releases.length();  
  else
    return false;

  int index=form.find_first_of('(', 0);
  if(index!=lenOfpredicate)
  {
    cout << "ERROR: Invalid Predicate Name '"
         << form.substr(0,index)
         << "' in '" << cn->str << "'\n" << endl;
    exit(EXIT_FAILURE);
  }

  // Checks out valid terms
  // (Fluent_term, timepoint_term)
  form.erase(0, lenOfpredicate);   // Erases predicate name
  form.erase(0, 1);               // Erases "("
  form.erase(form.length()-1);   // Erases ")"

  form=differentiateVars(form);  // Change ',' to '`'

  string token=split(form, ",");
  int numOfToken=0;

  while(token.compare("")!=0)
  {
    numOfToken++;

    if(numOfToken==1)
    {
      if(!validEventTerm(token))
      {
        cout << "ERROR: Invalid Event Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
    }
    if(numOfToken==2)
    {
      if(!validFluentTerm(token))
      {
        cout << "ERROR: Invalid Fluent Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
    }
    if(numOfToken==3)
    {
      if(!validTimepointTerm(token))
      {
        cout << "ERROR: Invalid Timepoint Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
    }
  
    token=split(form, ",");
  }

  if(numOfToken!=3)
  {
    cout << "ERROR: check out the number of arguments in '"
         <<  cn->str << "'\n" << endl;
    exit(EXIT_FAILURE);
  }

  return true;
}

/* check whether a node is a form of
   Trajectory and AntiTrajectory */
bool checkFormThree(nodePtr cn)
{
  string form=cn->str;
  string form2=cn->str;
  int lenOfpredicate=0;

  if(form.find(Trajectory, 0)==0)
    lenOfpredicate=Trajectory.length();
  else if(form.find(AntiTrajectory, 0)==0)
    lenOfpredicate=AntiTrajectory.length();  
  else
    return false;

  int index=form.find_first_of('(', 0);
  if(index!=lenOfpredicate)
  {
    cout << "ERROR: Invalid Predicate Name '"
         << form.substr(0,index)
         << "' in '" << cn->str << "'\n" << endl;
    exit(EXIT_FAILURE);
  }

  // Checks out valid terms
  // (Fluent_term, timepoint_term)
  form.erase(0, lenOfpredicate);   // Erases predicate name
  form.erase(0, 1);               // Erases "("
  form.erase(form.length()-1);   // Erases ")"

  form=differentiateVars(form);  // Change ',' to '`'

  string token=split(form, ",");
  int numOfToken=0;

  while(token.compare("")!=0)
  {
    numOfToken++;

    if(numOfToken==1)
    {
      if(!validFluentTerm(token))
      {
        cout << "ERROR: Invalid Fluent Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
    }
    if(numOfToken==2)
    {
      if(!validTimepointTerm(token))
      {
        cout << "ERROR: Invalid Timepoint Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
    }
    if(numOfToken==3)
    {
      if(!validFluentTerm(token))
      {
        cout << "ERROR: Invalid Fluent Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
    }
    if(numOfToken==4)
    {
      flag_trajectory=true;
      if(!validTimepointTerm(token))
      {
        cout << "ERROR: Invalid Timepoint Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
      flag_trajectory=false;
    } 
    token=split(form, ",");
  }

  if(numOfToken!=4)
  {
    cout << "ERROR: check out the number of arguments in '"
         <<  cn->str << "'\n" << endl;
    exit(EXIT_FAILURE);
  }

  return true;
}

/* check whether a node is a form of Happens3 */
bool checkHappens3Form(nodePtr cn)
{
  string form=cn->str;
  string form2=cn->str;
  int lenOfpredicate=0;

  if(form.find(Happens3, 0)==0)
    lenOfpredicate=Happens3.length();
  else
    return false;

  int index=form.find_first_of('(', 0);
  if(index!=lenOfpredicate)
  {
    cout << "ERROR: Invalid Predicate Name '"
         << form.substr(0,index)
         << "' in '" << cn->str << "'\n" << endl;
    exit(EXIT_FAILURE);
  }

  // Checks out valid terms
  // (Fluent_term, timepoint_term)
  form.erase(0, lenOfpredicate);   // Erases "Happens3"
  form.erase(0, 1);               // Erases "("
  form.erase(form.length()-1);   // Erases ")"

  form=differentiateVars(form);  // Change ',' to '`'

  string token=split(form, ",");
  int numOfToken=0;

  while(token.compare("")!=0)
  {
    numOfToken++;

    if(numOfToken==1)
    {
      if(!validEventTerm(token))
      {
        cout << "ERROR: Invalid Event Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
    }
    if(numOfToken==2)
    {
      if(!validTimepointTerm(token))
      {
        cout << "ERROR: Invalid Timepoint Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
    }
    if(numOfToken==3)
    {
      if(!validTimepointTerm(token))
      {
        cout << "ERROR: Invalid Timepoint Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
    }
  
    token=split(form, ",");
  }

  if(numOfToken!=3)
  {
    cout << "ERROR: check out the number of arguments in '"
         <<  cn->str << "'\n" << endl;
    exit(EXIT_FAILURE);
  }

  return true;
}

/* check whether a node is a form of 
   Clipped, Declipped, StoppedIn, and StartedIn */
bool checkFormFour(nodePtr cn)
{
  string form=cn->str;
  string form2=cn->str;
  int min=0, max=0; 
  int flag=false;
  int lenOfpredicate=0;

  if(form.find(Clipped, 0)==0)
    lenOfpredicate=Clipped.length();
  else if(form.find(Declipped, 0)==0)
    lenOfpredicate=Declipped.length();
  else if(form.find(StartedIn, 0)==0)
    lenOfpredicate=StartedIn.length();
  else if(form.find(StoppedIn, 0)==0)
    lenOfpredicate=StoppedIn.length();
  else
    return false;

  int index=form.find_first_of('(', 0);
  if(index!=lenOfpredicate)
  {
    cout << "ERROR: Invalid Predicate Name '"
         << form.substr(0,index)
         << "' in '" << cn->str << "'\n" << endl;
    exit(EXIT_FAILURE);
  }

  // Checks out valid terms
  // (Fluent_term, timepoint_term)
  form.erase(0, lenOfpredicate);   // Erases predicate name
  form.erase(0, 1);               // Erases "("
  form.erase(form.length()-1);   // Erases ")"

  form=differentiateVars(form);  // Change ',' to '`'

  string token=split(form, ",");
  int numOfToken=0;

  while(token.compare("")!=0)
  {
    numOfToken++;

    if(numOfToken==1)
    {
      if(!validTimepointTerm(token))
      {
        cout << "ERROR: Invalid Timepoint Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
      else
      {
	if(isNumber(token))
	  {min=stringToInteger(token);flag=true;}
      }
    }
    if(numOfToken==2)
    {
      if(!validFluentTerm(token))
      {
        cout << "ERROR: Invalid Fluent Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
    }
    if(numOfToken==3)
    {
      if(!validTimepointTerm(token))
      {
        cout << "ERROR: Invalid Timepoint Term "
             << "in '" << cn->str << "'\n" << endl;
        exit(EXIT_FAILURE);
      }
      else
      {
	if(isNumber(token))
	{
	  max=stringToInteger(token);
	  if(flag)
	    if(min>=max)
            {
	      cout << "ERROR: min is not less than max "
	           << "in '" << cn->str << "'\n" << endl;
	      exit(EXIT_FAILURE);
	    }
	}
      }
    }
  
    token=split(form, ",");
  }

  if(numOfToken!=3)
  {
    cout << "ERROR: check out the number of arguments in '"
         <<  cn->str << "'\n" << endl;
    exit(EXIT_FAILURE);
  }

  return true;
}

/* check whether a node is a variable or constant */
bool checkFormFive(nodePtr cn)
{
  string term=cn->str;
  if(isVariable(term))
    {addVars(term); return true;}
  else if(existConstant(term) || isNumber(term))
    return true;
  else
    return false;
}

/* Check whether a node is a predicate
   that are declared explicitly */
bool checkFormSix(nodePtr cn)
{
  int numOfToken=0;
  int key=-1; // It will be the index of a predicate found
  int scan=0;
  string form=cn->str;
  string token=split(form,"(,)");

  while(token.compare("")!=0)
  {
    numOfToken++;
    if(numOfToken==1)
    {
      for(int i=0; i<predicateRow; i++)
	if(predicates[i].pName.compare(token)==0)
	  {key=i; break;}

      if(key==-1)
	return false;
    }
    else
    {
      if(scan==predicates[key].num) // Has more arguments
      {
	cout << "ERROR: Check out the number of arguments in '" 
	     << cn->str << "'\n" <<endl;
	exit(EXIT_FAILURE);
      }

      string str = predicates[key].srts[scan];

      if(isConstant(token))
      {
	if(str.compare("time")==0) 
	{ 
	  int n=stringToInteger(token);
	  if(n<rangeMinTime || n>rangeMaxTime)
	  {
	    cout << "ERROR: '" << token << "' in '" << cn->str 
                 << "' is Out of range for time." << endl << endl;
	    exit(EXIT_FAILURE);
	  }
	}
	else
	{ 
	  if(isSubsortOfInteger(str))
	    if(isNumber(token))
	      return 1;
	    else 
	    {
	      cout << "ERROR: Invalid term '" << token 
                   << "' in '" << cn->str  << "'\n" << endl;
	      exit(EXIT_FAILURE);
	    }

	  if(existConstant(token))
	  {
	    if(sortNameOfConst.compare(str)!=0)
	    {
	      if(isSubsortOf(sortNameOfConst,str))
		scan++;
	      else
	      {
		cout << "ERROR: Invalid term '" << token 
		     << "' in '" << cn->str  << "'\n" << endl;
		exit(EXIT_FAILURE);
	      }
	    }
	    else
	      scan++;
	  }
	  else
	  {
	    cout << "ERROR: Invalid term '" << token 
		 << "' in '" << cn->str  << "'\n" << endl;
	    exit(EXIT_FAILURE);
	  }
	}
      }
      
      else // Is not a constant
      {
	if(isSubsortOf(token, str))
	  scan++;
	else
	{
	  if(token.find(str, 0)!=0)
	  { 
	    cout << "ERROR: Invalid sort '" << token
		 << "' in '" << cn->str << "'\n" << endl;  
	    exit(EXIT_FAILURE);
	  }
	  if(token.compare(str)!=0) 
          {
	    string sub=token.substr(str.length());
	    // sort associated with a variable
	    if(isNumber(sub)) 	
	      scan++;
	    else
 	    {
	      cout << "ERROR: Invalid variable '" << token
		 << "' in '" << cn->str << "'\n" << endl;  
	      exit(EXIT_FAILURE);
	    }
	  }
	  else
	    scan++;
	}

	addVars(token);
      }
    }
    
    token=split(form, "(,)");
  }

  if(numOfToken!=(predicates[key].num+1))
  {
    cout << "ERROR: Check out the number of arguments in '" 
	 << cn->str << "'\n" <<endl;
    exit(EXIT_FAILURE);
  }

  return 1;
}

void freshChildNodes()
{
  nOfchildNodes=0;
  for(int i=0; i<NUM_CHILDNODES; i++)
    childNodes[i]=NULL;
}

void freshVariables() 
{ 
  numOfvariables=0;
  for(int i=0; i<NUM_VARS; i++)
    variables[i]="";
}

void freshVarsQuan() 
{ 
  numOfvarsQuan=0;
  for(int i=0; i<NUM_VARS; i++)
    varsQuan[i]="";
}

void checkBiImpl(string form)
{
  nodePtr target=searchNode(root, "$");
  if(target==NULL)
   subFormsBiImpl[numOfsubFormsBiImpl++]=root;
  else
  {
    target->str=":";

    // Left to Right
    subFormsBiImpl[numOfsubFormsBiImpl++]=copyTree(root,NULL);

    // Right to Left
    nodePtr target=searchNode(root, ":");
    nodePtr temp=target->left;
    target->left=target->right;
    target->right=temp;
    subFormsBiImpl[numOfsubFormsBiImpl++]=copyTree(root,NULL);
  }
}

nodePtr copyTree(nodePtr org, nodePtr parent)
{
  if(org==NULL)
    return NULL;
  
  nodePtr temp = (nodePtr) new(struct node);
  temp->str=org->str;
  temp->parent=parent;
  temp->left=copyTree(org->left,temp);
  temp->right=copyTree(org->right,temp);

  return temp;
}

bool isVariable(string var)
{
  /*
    A variable consists of one or more lowercase letters
    followed by zero or more digits.
    The sort associated with a variable is determined by
    removing digits from the variable.
  */
  int len=var.length();
  string temp("");  

  if(!islower(var[0])) //$ isupper
    return false;

  for(int i=0; i<sortRow; i++)
  {     
    if(var.find(sorts[i].sName,0)==0)
    { 
      if(var.compare(sorts[i].sName)==0)
	return true;
      else
      {
	temp=var.substr(sorts[i].sName.length());
	if(isNumber(temp)) 
	  return true;    
      }
    }
  }

  cout << "ERROR: sort name for variable '"<< var 
       << "' was not defined." << endl << endl;
  exit(EXIT_FAILURE);
}

nodePtr searchNode(nodePtr tree, string target)
{
  nodePtr result=NULL;
  if(tree==NULL);
  else if(tree->str.find(target, 0)==0)
    result=tree;
  else
  {
    if(tree->left!=NULL)
      result=searchNode(tree->left, target);
    if(result==NULL && tree->right!=NULL)
      result=searchNode(tree->right, target);
  }

  return result;
}

/*
  Eliminate disjunctions in the antecedent recursively
*/
void applyC1(nodePtr tree)
{
  bool flag=false;
  nodePtr target=searchNode(tree,":");
  if(target!=NULL)
  {
    nodePtr target2=searchNode(target->left,"|");
    if(target2!=NULL)
      {eliminateDisjunction(tree);flag=true;}
  }
  if(!flag)
    subFormsC1[numOfsubFormsC1++]=tree;
}

void eliminateDisjunction(nodePtr tree)
{
  nodePtr temp=searchNode(tree,":");
  nodePtr target=searchNode(temp->left,"|");
  nodePtr rc,lc,pt; // right-child, left-child, and parent
  
  checkAncestorsWithExstQuan(target);
  //  checkAncestorsWithNegOrExstQuan(target);

  if(target==NULL || notRemoveDisjAndConj)
  {
    subFormsC1[numOfsubFormsC1++]=copyTree(tree,NULL);
    notRemoveDisjAndConj=false;
  }
  else
  {
    lc=target->left;
    rc=target->right;
    pt=target->parent;

    if(pt->left==target)
    {
      pt->left=lc;
      lc->parent=pt;
  
      target->parent=NULL;
      target->left=NULL;
      target->right=NULL;
      rc->parent=NULL;

      eliminateDisjunction(tree);

      lc->parent=NULL;
      // lc->left=NULL;
      // lc->right=NULL;
  
      pt->left=rc;
      rc->parent=pt;

      eliminateDisjunction(tree);  
    }
    else
    {
      pt->right=rc;
      rc->parent=pt;
  
      target->parent=NULL;
      target->left=NULL;
      target->right=NULL;
      lc->parent=NULL;

      eliminateDisjunction(tree);

      rc->parent=NULL;
      //rc->left=NULL;
      //rc->right=NULL;
  
      pt->right=lc;
      lc->parent=pt;
      
      eliminateDisjunction(tree);
    }    
  }
}

//void checkAncestorsWithNegOrExstQuan(nodePtr child)
void checkAncestorsWithExstQuan(nodePtr child)
{
  if(child==NULL)
    return ;
  else
  {
    if(child->parent!=NULL)
    {  
      string s=child->parent->str;
      //      if(s[0]=='!' || s[0]=='{')
      if(s[0]=='{')
	{notRemoveDisjAndConj=true; return;}
      else
	checkAncestorsWithExstQuan(child->parent);
      //checkAncestorsWithNegOrExstQuan(child->parent)
    }
    else
      return ;
  }
}

/*
  Eliminate conjunctions in the consequent recursively
*/
void applyC2(nodePtr tree)
{
  bool flag=false;
  nodePtr target=searchNode(tree,":");
  if(target!=NULL)
  {
    nodePtr target2=searchNode(target->right,"&");
    if(target2!=NULL)
      {eliminateConjunction(tree);flag=true;}
  }
  if(!flag)
    subFormsC2[numOfsubFormsC2++]=tree;
}

void eliminateConjunction(nodePtr tree)
{
  nodePtr temp=searchNode(tree,":");
  nodePtr target=searchNode(temp->right,"&");
  nodePtr rc,lc,pt; // right-child, left-child, and parent

  checkAncestorsWithExstQuan(target);
  //  checkAncestorsWithNegOrExstQuan(target);

  if(target==NULL || notRemoveDisjAndConj)
  {
    subFormsC2[numOfsubFormsC2++]=copyTree(tree,NULL);
    notRemoveDisjAndConj=false;
  }
  else
  {   
    lc=target->left;
    rc=target->right;
    pt=target->parent;

    if(pt->left==target)
    {
      pt->left=lc;
      lc->parent=pt;
  
      target->parent=NULL;
      target->left=NULL;
      target->right=NULL;
      rc->parent=NULL;

      eliminateConjunction(tree);

      lc->parent=NULL;
      //lc->left=NULL;
      //lc->right=NULL;
  
      pt->left=rc;
      rc->parent=pt;

      eliminateConjunction(tree);
    }
    else
    {     
      pt->right=rc;
      rc->parent=pt;
  
      target->parent=NULL;
      target->left=NULL;
      target->right=NULL;
      lc->parent=NULL;

      eliminateConjunction(tree);
      
      rc->parent=NULL;
      //rc->left=NULL;
      //rc->right=NULL;
  
      pt->right=lc;
      lc->parent=pt;

      eliminateConjunction(tree);
    }    
  }
}

void displayTree(nodePtr tree)
{
  if(tree)
  {
    displayTree(tree->left);
    cout << tree->str << endl;
    displayTree(tree->right);
  }
  else
    return;
}

/*
  Rewrite a formula so that existential quantifier
  occurs only in the antecedent of the formula
*/
void moveExstQuan(nodePtr tree)
{
  nodePtr temp=searchNode(tree,":");
  nodePtr pt;

  if(temp==NULL)
  {
    nodePtr target2=searchNode(tree,"{");
    if(target2!=NULL)
    {
      nodePtr p=target2->parent;
      p->right=NULL;
      target2->parent=NULL;

      nodePtr temp2=(nodePtr) new(struct node);
      temp2->str=":";
      temp2->left=NULL;
      temp2->right=NULL;
      temp2->parent=NULL;
      nodePtr temp3=(nodePtr) new(struct node);
      temp3->str="!";
      temp3->left=NULL;
      temp3->right=NULL;
      temp3->parent=NULL;

      temp2->left=temp3;
      temp3->parent=temp2;
      target2->parent=temp3;
      temp3->right=target2;
      p->right=temp2;
      temp2->parent=p;
    }
    newTree=copyTree(tree,NULL);
    return;
  }

  nodePtr lChild=temp->left;
  nodePtr rChild=temp->right;

  if(rChild==NULL)
    newTree=copyTree(tree,NULL);
  else
  {
    nodePtr target=searchNode(rChild,"{");
    bool flag_left=false;

    if(target==NULL)
      newTree=copyTree(tree,NULL);
    else
    {
      nodePtr newNode=(nodePtr) new(struct node);
      bool neg=false;

      // Right-hand side of implication
      if(target->parent->str.compare("!")==0)
        //{newNode=target; neg=true;}
      {
        newNode->str=target->str;
        newNode->left=NULL;
        newNode->right=target->right;
        neg=true;
      }
      else
      {
        newNode->str="!";
        newNode->left=NULL;
        newNode->right=target;
        //      target->parent=newNode;
      }

      if(neg)
        pt=target->parent->parent;
      else
        pt=target->parent;


      // Connects target's sibling to target's grandparent
      if(pt->str.compare(":")==0)
        pt->right=NULL;
      else
      {
        nodePtr grand_pt=pt->parent;

        if(neg)
        {
          if(pt->left==target->parent)
            flag_left=true;
        }
        else
          if(pt->left==target)
            flag_left=true;

        if(grand_pt->left==pt)
        {
          if(flag_left)
          {
            nodePtr rc=pt->right;
            rc->parent=grand_pt;
            grand_pt->left=rc;
          }
          else
          {
            nodePtr lc=pt->left;
            lc->parent=grand_pt;
            grand_pt->left=lc;
          }
        }
        else
        {
          if(flag_left)
          {
            nodePtr rc=pt->right;
            rc->parent=grand_pt;
            grand_pt->right=rc;
          }
          else
          {
            nodePtr lc=pt->left;
            lc->parent=grand_pt;
            grand_pt->right=lc;
          }
        }

        pt->left=NULL;
        pt->right=NULL;
        pt->parent=NULL;
      }

      // Left-hand side of implication
      nodePtr newNode2=(nodePtr) new(struct node);
      newNode2->str="&";

      temp->left=newNode2;
      lChild->parent=newNode2;

      newNode2->parent=temp;
      newNode2->left=lChild;
      newNode2->right=newNode;
      newNode->parent=newNode2;

      moveExstQuan(tree);
    }
  }
}

void eliminateExstQuan(nodePtr tree)
{    
  nodePtr target = searchNode(tree,"{");
  if(!target)
    return;

  while(target)
  {
    if(target->parent->str.compare("!")!=0)
      eliminateNegExstQuan(target);
    else
      eliminatePosExstQuan(target);

    target = searchNode(tree,"{");
  }
 
  freshVarsInExstQuan();
}

void eliminateUniQuan(nodePtr tree)
{ 
  getVarsInUniQuan(tree);

  if(!flag_uniQuan)
  {
    returnInfixForm(tree);
    return;
  }
  // Remove the universal quantifier
  nodePtr rc = tree->right;
  rc->parent=NULL;
  tree->right=NULL;

  //Convert a tree to a string
  returnInfixForm(rc);
  int len=rform.length();
  string var(""), varRename(""), tempName(""); 
  int varFound,lenOfvar; bool checked;

  for(int k=0; k<numOfvarsUniQuan; k++) 
  {
    checked=false;
    var=varsUniQuan[k]; 
    lenOfvar=var.length();
    varFound=rform.find(var, 0);

    do //Find out a position to rename
    {	
      if(isalnum(rform[varFound+lenOfvar]) ||
	 rform[varFound+lenOfvar]=='_');
      else
      { 	  
	//Decide the variable name
	if(!checked)
	{
	  checked=true;
	  varRename=var; int index=-1; int subName=1;
	  varRename[0]=toupper(varRename[0]);
	  int key=rform.find(varRename,0);
	  if(key>=0 && key<len)
	  { 
	    for(int i=0; i<lenOfvar; i++)
	      if(isdigit(var[i]))
		{index=i; break;}
	    if(index<lenOfvar && index>=0)
	      varRename=varRename.substr(0,index); 
	    do
	    {  
	      tempName=varRename+integerToString(subName++);
	      key=rform.find(tempName,0);
	    }while(key>=0 && key<len);
	    varRename=tempName;
	  }
	}
	if(!existInAllVars(varRename))
	  allVars[numOfallVars++]=varRename;

	rform.replace(varFound,lenOfvar,varRename);
	rform.insert(varFound,"_v"); // To make a variable marker
	len=len+(varRename.length()-lenOfvar)+2;
      }
      varFound=rform.find(var, varFound+lenOfvar);
    }while(varFound>=0 && varFound<len);   
  } // end of 'for' loop

  flag_uniQuan=false;
  freshVarsInUniQuan();
}

void getVarsInUniQuan(nodePtr tree)
{
  string key=tree->str;
  if(key[0]=='[')
  {
    flag_uniQuan=true;
    string token=split(key,"[,]");
    while(token.compare("")!=0)
    {
      varsUniQuan[numOfvarsUniQuan++]=token;
      token=split(key,"[,]");
    }
  }
}

void returnInfixForm(nodePtr tree)
{
  int flag=0;

  if(tree)
  {
    if( (tree->left!=NULL && tree->right!=NULL)
        || tree->str.compare(":")==0)
      flag=1;

    if(flag)
      rform.append("(");

    returnInfixForm(tree->left);
    
    rform.append(tree->str);

    returnInfixForm(tree->right);
    
    if(flag)
      rform.append(")");
  }
  else
    return;
}


/* 
  Eliminate maximal negative occurrence of
  an existential quantifier in the antecedent 
  of a formula
=> Change existential quantifier to
   universal quantifier by converting to PNF
   - Rename variables and remove existential quantifiers
*/

void eliminateNegExstQuan(nodePtr target)
{
  freshTempVars();

  //Get variables in existential quantifier
  getVarsInExstQuan(target);

  //Rename variables
  findTargets(target->right);

  //Remove existential quantifier
  nodePtr pt=target->parent;

  // case 1) target is a left child
  if(pt->left==target) 
  {
    pt->left=target->right;
    target->right->parent=pt;
    target->parent=NULL;
    target->right=NULL;
  }
  // case 2) target is a right child
  else
  {
    pt->right=target->right;
    target->right->parent=pt;
    target->parent=NULL;
    target->right=NULL;
  }


  for(int i=0; i<numOftempVars; i++)    
    if(!existVarsInExstQuan(tempVars[i]))
      varsExstQuan[numOfvarsExstQuan++]=tempVars[i];
}

void getVarsInExstQuan(nodePtr target)
{
  string temp=target->str;
  string token=split(temp, "{,}");
  while(token.compare("")!=0)
  {
    tempVars[numOftempVars++]=token;
    token=split(temp, "{,}");
  }
}

void findTargets(nodePtr target)
{
  if(target)
  { 
    string key=target->str;
    if(key[0]=='{')
    {
      string token=split(key,"{,}");
      while(token.compare("")!=0)
      {
	if(existInTempVars(token))
	  return;
	token=split(key,"{,}");
      }
    }
    findTargets(target->left);    
    if(target->left==NULL && target->right==NULL)
      renameVarsInExstQuan(target);
    findTargets(target->right);
  }
  else 
    return;
}

bool existInTempVars(string target)
{
  if(numOftempVars==0)
    return false;
  else
  {
    for(int i=0; i<numOftempVars; i++)
      if(target.compare(tempVars[i])==0)
	return true;
  }
  return false;
}

bool existInVarsOfnewPre(string target)
{
  if(numOfvarsOfnewPre==0)
    return false;
  else
  {
    for(int i=0; i<numOfvarsOfnewPre; i++)
      if(target.compare(varsOfnewPre[i])==0)
	return true;
  }
  return false;
}

void renameVarsInExstQuan(nodePtr target)
{ 
  bool sameVars=false;
  string form=target->str;
  int len=form.length();
  string var(""), varRename(""); 
  int varFound,lenOfvar;
  for(int k=0; k<numOftempVars; k++) 
  {
    if(existVarsInExstQuan(tempVars[k]))
      sameVars=true;
    var=tempVars[k]; 
    lenOfvar=var.length();
    varFound=form.find(var, 0);

    if(varFound<0 || varFound>=len)
      continue;

    varRename=var; int index=-1; 
    string tempName(""); int subName=1;
    /* Same variable names Between vars in Existential 
       Quantifiers in a formula */
    if(sameVars)
    { 
      for(int i=0; i<lenOfvar; i++)
	if(isdigit(var[i]))
	  {index=i; break;}
      if(index<lenOfvar && index>=0)
	varRename=varRename.substr(0,index); 
      do
      { 
	tempName=varRename+integerToString(subName++);
      }
      while(existVarsInExstQuan(tempName));
      varRename=tempName;
    }
    varRename[0]=toupper(varRename[0]);

    if(!existInAllVars(varRename))
      allVars[numOfallVars++]=varRename;

    do
    {	
      if(isalnum(form[varFound+lenOfvar]) ||
	 form[varFound+lenOfvar]=='_');
      else
      { 
	form.replace(varFound,lenOfvar,varRename);
	form.insert(varFound,"_v"); // To make a variable marker
	len=len+(varRename.length()-lenOfvar)+2;
      }
      varFound=form.find(var, varFound+lenOfvar);
    }while(varFound>=0 && varFound<len);
    sameVars=false;
  } // end of 'for' loop

  target->str=form;
}

string removeExtraParentheses(string& form)
{
  int tuple_start=0;
  int len=form.length();
  struct stack s;
  s.top=-1; 

  for(int i=0; i<len; i++)
  { 
    if(i==0)
    {  
      if(form[i]=='(')
	form.replace(i,1," ");
    }
    else
    {  
      if(form[i]=='(')
	if(isalnum(form[i-1]))
	{tuple_start=1; push(&s,'(');}
	else
	  form.replace(i,1," ");
   
      if(form[i]==')')
	if(tuple_start)
        {  
	  pop(&s);
	  if(isempty(&s))
	    tuple_start=0; 
	}
	else
	  form.replace(i,1," ");
    }
  }

  trimSpace(form);

  return form;
}

string convertToLP(string form)
{
  string result("");
  string head("");
  string body("");
  string token("");
  int end=0;
  int index=0;
  int len=form.length();
  int indexOfimpl=-1;

  for(int i=0; i<len; i++)
  {
    if(form[i]==':')
      {indexOfimpl=i; break;}
  }


  if(indexOfimpl==-1)
  {  
    if(form[0]=='!')
      result=":- "+form.erase(0,1)+".";
    else
      result=form+".";
  }
  else
  {
    head=form.substr(indexOfimpl+1);
    body=form.substr(0,indexOfimpl);

    if(head.length()==0)
      result=":- "+body+".";
    else
    {
      if(head[0]=='!')
      {
	head=head.substr(1);
	result=":- "+body+", "+head+".";
      }
      else
	result=head+" "+":-"+" "+body+".";
    }

    int l=result.length();
    for(int j=0; j<l; j++)
      if(result[j]=='!')
	{result.replace(j,1,"not "); l=l+4;}
  }

  handleHead(result);

  // Small -> Big for variables
  // Big -> Small for events, fluents, constants, predicates of EC  
  int found=0;
  int len2=result.length();
  int exit=0, k2=0;


  for(int k=0; k<len2; k++)
  {
    found=result.find(Initiates, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+Initiates.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }

    found=result.find(Terminates, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+Terminates.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }

    found=result.find(Releases, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+Releases.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }    

    found=result.find(HoldsAt, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+HoldsAt.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }

    found=result.find(ReleasedAt, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+ReleasedAt.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }

    found=result.find(Happens, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+Happens.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }

    found=result.find(Happens3, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+Happens3.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }

    found=result.find(Trajectory, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+Trajectory.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }

    found=result.find(AntiTrajectory, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+AntiTrajectory.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }

    found=result.find(StoppedIn, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+StoppedIn.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }

    found=result.find(StartedIn, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+StartedIn.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    } 

    found=result.find(Clipped, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+Clipped.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }

    found=result.find(Declipped, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+Declipped.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }

    found=result.find(Started, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+Started.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }

    found=result.find(Stopped, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+Stopped.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }

    found=result.find(Initiated, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+Initiated.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }

    found=result.find(Terminated, k);
    if(found>=0 && found<len2)
    { 
      if(!isalnum(result[found+Terminated.length()]) &&
	 !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);
    }

    for(int n=0; n<constRow; n++)
    {
      int number=constants[n].num;
      for(int o=0; o<number; o++)
      {
	k2=k; exit=0;
	do
	{
	  found=result.find(constants[n].objects[o], k2);
	  if(found>=0 && found<len2)
	  {  
	    if(!isalnum(result[found+constants[n].objects[o].length()]) &&
	       !isalnum(result[found-1]))
	      {
		result[found]=tolower(result[found]);
		result.insert(found, "_c"); // To make a constant marker
	      }
	    
	    k2=found+constants[n].objects[o].length()+2;
	  }
	  else
	    exit=1; // break;
	}while(k2<len2 && exit!=1);
      }
    }

    for(int l=0; l<fluentRow; l++)
    {
      k2=k; exit=0;
      do
      {
	found=result.find(fluents[l].pName, k2);
	if(found>=0 && found<len2)
	{ 
	  if(!isalnum(result[found+fluents[l].pName.length()]) &&
	     !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);

	  k2=found+fluents[l].pName.length();
	}
	else
	  exit=1; // break;
      }while(k2<len2 && exit!=1);
    }

    for(int m=0; m<eventRow; m++)
    {
      k2=k; exit=0;
      do
      {
	found=result.find(events[m].pName, k2);
	if(found>=0 && found<len2)
	{
	  if(!isalnum(result[found+events[m].pName.length()]) &&
	     !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);

	  k2=found+events[m].pName.length(); 
	}
	else 
	  exit=1; // break;
      }while(k2<len2 && exit!=1);
    }
    
    for(int n=0; n<predicateRow; n++)
    {
      k2=k; exit=0;
      do
      {
	found=result.find(predicates[n].pName, k2);
	if(found>=0 && found<len2)
	{
	  if(!isalnum(result[found+predicates[n].pName.length()]) &&
	     !isalnum(result[found-1]))
	    result[found]=tolower(result[found]);

	  k2=found+predicates[n].pName.length(); 
	}
	else 
	  exit=1; // break;
      }while(k2<len2 && exit!=1);
    } 
  }

  convertSyms(result);

  return result;	     
}

void handleHead(string& form)
{
  int len=form.length();
  int key=form.find(":-", 0);
  int key2, gte, lt, eq, neq, lte, gt, change=0;
  string head(""), body(""), token(""), newHead("");

  if(key>0 && key<len)
  { 
    form.erase(len-1, 1); // Remove '.'
    head=form.substr(0, key-1);
    body=form.substr(key+3);
    string token = split(head, "|");
    
    while(token.compare("")!=0)
    {
      gte=token.find("#"); // >=
      lt=token.find("<");
      eq=token.find("=");
      neq=token.find("~"); // !=
      lte=token.find("@"); // <=
      gt=token.find(">"); 
     
      if(gte>0)
	{token.replace(gte,1,"<"); change=1;}
      else if(lt>0)	
	{token.replace(lt,1,"#"); change=1;}
      else if(eq>0)
	{token.replace(eq,1,"~"); change=1;}
      else if(neq>0)
	{token.replace(neq,1,"="); change=1;}
      else if(gt>0)
	{token.replace(gt,1,"@"); change=1;}
      else if(lte>0)
	{token.replace(lte,1,">"); change=1;}
      else
	newHead=newHead+token+" | ";

      if(change)
	{body=body+", "+token; change=0;}

      token = split(head, "|");
    }
    
    if(newHead.compare("")!=0)
    {
      newHead.erase(newHead.length()-3,3);
      form = newHead + " :- " + body + ".";
    }
    else
      form = ":- " + body + ".";
  }
  else;
}

void convertSyms(string& form)
{
  // ######### '=' ==> '=='

  int len=form.length();

  for(int i=0; i<len; i++)
  {
    if(form[i]=='&')
    { 
      form.replace(i,1,", ");
      len++;
    }

    else if(form[i]=='%')
    { 
      form.replace(i,1," mod ");
      len=len+4;
    }
 
    else if(form[i]=='@')
    {
      form.replace(i,1,"<=");
      len++;
    }
    else if(form[i]=='#')
    {
      form.replace(i,1,">=");
      len++;
    }
    else if(form[i]=='~')
    {
      form.replace(i,1,"!=");
      len++;
    }
    else if(i+1<len && form[i]!='!' && form[i+1]=='=')
    {
      form.replace(i+1,1,"==");
      len++; i++;
    }
    else if(i+1<len && form[i]=='-' && form[i+1]=='-')
    {
      form.replace(i,2,"-");
      len--;
    }
    else if
     (i+1<len && form[i]=='_' && 
      (form[i+1]=='v' || // Removing a variable marker
       form[i+1]=='c')   // Removing a constant marker            
     )
    {      
      form.replace(i,2,"");
      len--;
    }
  }
}

void eliminatePosExstQuan(nodePtr target)
{
  freshTempVars();
  freshVarsInNewPre();
  string newPreHead("");
  string vars[NUM_VARS];
  int numOfvars=0;  
  string temp=target->str;
  string token=split(temp, "{,}");
  while(token.compare("")!=0)
  {
    vars[numOfvars++]=token;
    token=split(temp, "{,}");
  }

  //Get variables in all the existential quantifiers under target
  getVarsInAllExstQuans(target);

  /* Get free variables x of newPre(x) 
     where newPre is a new predicate constant */
  getVarsOfNewPre(target->right);

  //Introduce a new Predicate
  newPreHeads[numOfnewPreHeads]=newPre+integerToString(newPreNum++);
  newPreHead.append(newPreHeads[numOfnewPreHeads]+"(");
  numOfnewPreHeads++;
  for(int i=0; i<numOfvarsOfnewPre; i++)
  {
    newPreHead.append(varsOfnewPre[i]);
    newPreHead.append(",");
    vars[numOfvars++]=varsOfnewPre[i];
  }
  newPreHead.erase(newPreHead.length()-1,1); //Remove ','
  newPreHead.append(")");
  nodePtr pt=target->parent;
  nodePtr newSubTree = (nodePtr) new(struct node);
  newSubTree->str=newPreHead;
  newSubTree->parent=pt;
  newSubTree->left=NULL;
  newSubTree->right=NULL;

  if(pt->right==target) // case 1) target is a right child
    pt->right=newSubTree;
  else                  // case 2) target is a left child
    pt->left=newSubTree;


  string temp2="[";
  for(int j=0; j<numOfvars; j++)
  {  
    temp2.append(vars[j]);
    temp2.append(",");
  }
  temp2.erase(temp2.length()-1,1);
  temp2.append("]");
  nodePtr newPreForm = (nodePtr) new(struct node);
  newPreForm->str=temp2;
  newPreForm->parent=NULL;
  newPreForm->left=NULL;
  nodePtr impl = (nodePtr) new(struct node);
  impl->str=":";
  impl->parent=newPreForm;
  newPreForm->right=impl;
  impl->left=target->right;
  target->right->parent=impl;
  target->right=NULL;
  target->parent=NULL;
  nodePtr cpNewSubTree=copyTree(newSubTree, NULL);
  impl->right=cpNewSubTree;
  cpNewSubTree->parent=impl;
  newPredicateForms[nOfnewPreForms++]=newPreForm;
} 

void getVarsInAllExstQuans(nodePtr tree)
{
  if(tree)
  {
    string key=tree->str;
    if(key[0]=='{')
      getVarsInExstQuan(tree);

    getVarsInAllExstQuans(tree->left);
    getVarsInAllExstQuans(tree->right);
  }
  else
    return;
}

void getVarsOfNewPre(nodePtr tree)
{
  if(tree)
  {
    if(tree->left==NULL && tree->right==NULL)
    {
      string key=tree->str;
      string token=split(key, "(,)");
      while(token.compare("")!=0)
      {
	if(isVariable(token))
	  if(!existInTempVars(token) && !existInVarsOfnewPre(token))
	    varsOfnewPre[numOfvarsOfnewPre++]=token;
	token=split(key,"(,)");
      }
    }
    getVarsOfNewPre(tree->left);
    getVarsOfNewPre(tree->right);
  }
  else
    return;
}

void freshTempVars() // For tempVars array
{
  numOftempVars=0;
  for(int i=0; i<NUM_VARS; i++)
    tempVars[i]="";
}

void freshVarsInExstQuan() 
{ 
  numOfvarsExstQuan=0;
  for(int i=0; i<NUM_VARS; i++)
    varsExstQuan[i]="";
}

void freshVarsInUniQuan() 
{ 
  numOfvarsUniQuan=0;
  numOfuniQuan=0;
  for(int i=0; i<NUM_VARS; i++)
    varsUniQuan[i]="";
}

void freshVarsInNewPre()
{
  numOfvarsOfnewPre=0;
  for(int i=0; i<NUM_VARS; i++)
    varsOfnewPre[i]="";
}

void freshSubformsC1()
{
  numOfsubFormsC1=0;
  for(int i=0; i<NUM_SUB_FORMS; i++)
    subFormsC1[i]=NULL;
}

void freshSubformsC2()
{
  numOfsubFormsC2=0;
  for(int i=0; i<NUM_SUB_FORMS; i++)
    subFormsC2[i]=NULL;
}

void freshNewPreForms()
{
  nOfnewPreForms=0;
  for(int i=0; i<NUM_NEWPRE_FORMS; i++)
    newPredicateForms[i]=NULL;
}

void freshFinalFormTrees()
{
  numberOfTrees=0;
  for(int i=0; i<NUM_FINAL_TREES; i++)
    finalFormTrees[i]=NULL;
}

void freshSubformsBiImpl()
{
  numOfsubFormsBiImpl=0;
  for(int i=0; i<NUM_SUB_FORMS; i++)
    subFormsBiImpl[i]=NULL;
}

void setUpDomainPredicates()
{
  string s(""); // a sort name
  int count;
  domain="#domain ";

  for(int i=0; i<sortRow; i++)
  {
    count=0;
    s=sorts[i].sName;
    // $$$$ s[0]=tolower(s[0]);
    //    s=sorts[i];
    domain=domain+s+"(";

    s[0]=toupper(s[0]);

    for(int j=0; j<numOfallVars; j++)
    {
      if(allVars[j].find(s,0)==0)
      {
	domain=domain+allVars[j]+";";
	count++;
      }
    }
   
    if(count>0)
    {
      domain.erase(domain.length()-1, 1);
      domain=domain+"), ";
    }
    else
      domain.erase(domain.length()-s.length()-1, s.length()+1);
  }  
  
  domain.replace(domain.length()-2, 2, ".");
}

// End of File
