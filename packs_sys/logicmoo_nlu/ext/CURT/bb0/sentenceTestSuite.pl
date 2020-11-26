/*************************************************************************

         name: sentenceTestSuite.pl
      version: April 19, 2001
  description: Testsuite with example sentences 
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(sentenceTestSuite,[sentence/1]).

/*========================================================================
   Sentences
========================================================================*/

sentence([a,man,walks]).

sentence([mia,dances]).

sentence([if,vincent,dances,then,mia,dances]).

sentence([who,dances]).

sentence([which,robber,dies]).

sentence([mia,or,vincent,dances]).

sentence([every,customer,smokes]).

sentence([a,customer,smokes]).

sentence([mia,or,a,man,dances]).

sentence([every,woman,or,a,man,dances]).

sentence([every,man,or,woman,dances]).

sentence([every,man,that,dances,smokes]).

sentence([every,customer,in,a,restaurant,smokes]).

sentence([mia,knows,a,man]).

sentence([if,butch,shoots,vincent,then,vincent,dies]).

sentence([who,likes,mia]).

sentence([who,likes,who]).

sentence([mia,likes,who]).

sentence([which,boxer,shoots,vincent]).

sentence([which,boxer,shoots,a,criminal]).

sentence([mia,or,vincent,eats,a,quarter,pounder,with,cheese]).

sentence([every,customer,drinks,a,five,dollar,shake]).

sentence([a,customer,knows,mia,or,a,man]).

sentence([vincent,knows,every,woman,or,a,man]).

sentence([vincent,knows,every,man,or,woman]).

sentence([mia,dates,every,man,that,dances]).

sentence([a,robber,likes,every,customer,in,a,restaurant]).

sentence([butch,growls,or,dies]).

sentence([every,boxer,growls,or,dies]).

sentence([butch,kills,a,criminal,or,dies]).

sentence([butch,kills,a,criminal,or,shoots,vincent]).

sentence([butch,kills,a,criminal,or,shoots,a,criminal]).

sentence([butch,is,a,boxer]).

sentence([butch,is,not,vincent]).

sentence([butch,does,not,die]).

sentence([a,boxer,does,not,die]).

sentence([every,boxer,does,not,die]).

sentence([vincent,knows,mia,or,does,not,dance]).

sentence([vincent,does,not,smoke,or,dance]).

sentence([a,man,does,not,smoke,or,dance]).

sentence([every,customer,in,a,restaurant,eats,a,big,kahuna,burger]).

sentence([if,every,man,knows,a,woman,then,every,woman,knows,a,man]).

sentence([vincent,eats,a,big,kahuna,burger]).
