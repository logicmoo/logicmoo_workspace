/*************************************************************************

    File: lambdaTestSuite.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB2, version 1.0 (June 2004).

    BB2 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB2 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB2; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(lambdaTestSuite,[discourse/2]).


/*========================================================================
    Discourses
========================================================================*/

discourse([a,man,walks],1).

discourse([mia,dances],1).

discourse([if,vincent,dances,then,mia,dances],1).

discourse([mia,or,vincent,dances],1).

discourse([every,customer,smokes],1).

discourse([a,customer,smokes],1).

discourse([mia,or,a,man,dances],1).

discourse([every,woman,or,a,man,dances],2).

discourse([every,man,or,woman,dances],1).

discourse([every,man,that,dances,smokes],1).

discourse([every,customer,in,a,restaurant,smokes],2).

discourse([mia,knows,a,man],1).

discourse([if,butch,shoots,vincent,then,vincent,dies],1).

discourse([mia,or,vincent,eats,a,quarter,pounder,with,cheese],2).

discourse([every,customer,drinks,a,five,dollar,shake],2).

discourse([a,customer,knows,mia,or,a,man],2).

discourse([vincent,knows,every,woman,or,a,man],2).

discourse([vincent,knows,every,man,or,woman],1).

discourse([mia,dates,every,man,that,dances],1).

discourse([a,robber,likes,every,customer,in,a,restaurant],5).

discourse([butch,growls,or,dies],1).

discourse([every,boxer,growls,or,dies],1).

discourse([butch,kills,a,criminal,or,dies],1).

discourse([butch,kills,a,criminal,or,shoots,vincent],1).

discourse([butch,kills,a,criminal,or,shoots,a,criminal],2).

discourse([butch,is,a,boxer],1).

discourse([butch,is,not,vincent],1).

discourse([butch,does,not,die],1).

discourse([butch,does,die],1).

discourse([a,boxer,does,not,die],2).

discourse([every,boxer,does,not,die],2).

discourse([vincent,knows,mia,or,does,not,dance],1).

discourse([vincent,does,not,smoke,or,dance],1).

discourse([a,man,does,not,smoke,or,dance],2).

discourse([every,customer,in,a,restaurant,eats,a,big,kahuna,burger],5).

discourse([every,customer,in,a,restaurant,does,not,eat,a,big,kahuna,burger],18).

discourse([every,man,in,a,restaurant,knows,a,woman,with,a,car],14).

discourse([if,every,man,knows,a,woman,then,every,woman,knows,a,man],4).

discourse([vincent,eats,a,big,kahuna,burger],1).

discourse([either,vincent,eats,a,big,kahuna,burger,or,jules,smokes],1).

