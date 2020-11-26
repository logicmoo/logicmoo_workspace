/*************************************************************************

         name: discourseTestSuite.pl
      version: July 28, 2001
  description: Testsuite with example discourses
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(discourseTestSuite,[discourse/1]).

/*========================================================================
   Discourse
========================================================================*/

discourse([a,man,walks,he,smokes]).

discourse([every,man,walks,he,smokes]).

discourse([mia,dances,she,smokes]).

discourse([if,vincent,dances,then,mia,dances]).

discourse([mia,or,vincent,dances]).

discourse([mia,or,a,man,dances]).

discourse([every,woman,or,a,man,dances]).

discourse([every,man,or,woman,dances]).

discourse([every,customer,that,eats,a,big,kahuna,burger,likes,it]).

discourse([vincent,likes,mia,she,smokes]).