/*************************************************************************

    File: pronounTestSuite.pl
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

:- module(pronounTestSuite,[discourse/2]).


/*========================================================================
    Example Discourses
========================================================================*/

discourse([every,man,likes,himself],1).

discourse([no,man,likes,himself],1).

discourse([no,man,likes,herself],0).

discourse([if,a,man,walks,then,he,smokes],1).

discourse([if,a,man,walks,then,she,smokes],0).

discourse([a,man,walks,he,smokes],1).

discourse([mia,dances,she,likes,vincent],1).

discourse([mia,dances,she,does,not,like,vincent],1).

discourse([mia,walks,she,smokes],1).

discourse([mia,knows,jody,she,smokes],2).
