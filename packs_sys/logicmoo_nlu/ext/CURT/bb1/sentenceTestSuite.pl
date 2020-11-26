/*************************************************************************

    File: sentenceTestSuite.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.2 (August 2005).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(sentenceTestSuite,[sentence/2]).


/*========================================================================
   Sentences
========================================================================*/

sentence([a,man,walks],1).

sentence([mia,dances],1).

sentence([if,vincent,dances,then,mia,dances],1).

sentence([who,dances],1).

sentence([which,robber,dies],1).

sentence([mia,or,vincent,dances],1).

sentence([every,customer,smokes],1).

sentence([a,customer,smokes],1).

sentence([mia,or,a,man,dances],1).

sentence([every,woman,or,a,man,dances],2).

sentence([every,man,or,woman,dances],1).

sentence([every,man,that,dances,smokes],1).

sentence([every,customer,in,a,restaurant,smokes],2).

sentence([mia,knows,a,man],1).

sentence([if,butch,shoots,vincent,then,vincent,dies],1).

sentence([who,likes,mia],1).

sentence([which,boxer,shoots,vincent],1).

sentence([which,boxer,shoots,a,criminal],1).

sentence([mia,or,vincent,eats,a,quarter,pounder,with,cheese],2).

sentence([every,customer,drinks,a,five,dollar,shake],2).

sentence([a,customer,knows,mia,or,a,man],2).

sentence([vincent,knows,every,woman,or,a,man],2).

sentence([vincent,knows,every,man,or,woman],1).

sentence([mia,dates,every,man,that,dances],1).

sentence([a,robber,likes,every,customer,in,a,restaurant],5).

sentence([butch,growls,or,dies],1).

sentence([every,boxer,growls,or,dies],1).

sentence([butch,kills,a,criminal,or,dies],1).

sentence([butch,kills,a,criminal,or,shoots,vincent],1).

sentence([butch,kills,a,criminal,or,shoots,a,criminal],2).

sentence([butch,is,a,boxer],1).

sentence([butch,is,not,vincent],1).

sentence([butch,does,not,die],1).

sentence([butch,does,die],1).

sentence([a,boxer,does,not,die],2).

sentence([every,boxer,does,not,die],2).

sentence([vincent,knows,mia,or,does,not,dance],1).

sentence([vincent,does,not,smoke,or,dance],1).

sentence([a,man,does,not,smoke,or,dance],2).

sentence([every,customer,in,a,restaurant,eats,a,big,kahuna,burger],5).

sentence([every,customer,in,a,restaurant,does,not,eat,a,big,kahuna,burger],18).

sentence([every,man,in,a,restaurant,knows,a,woman,with,a,car],14).

sentence([if,every,man,knows,a,woman,then,every,woman,knows,a,man],4).

sentence([vincent,eats,a,big,kahuna,burger],1).

sentence([either,vincent,eats,a,big,kahuna,burger,or,jules,smokes],1).

