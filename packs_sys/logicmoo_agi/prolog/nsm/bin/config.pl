/*
    This file is part of NSM-DALIA, an extensible parser and generator
    for NSM grammars.
       
    Copyright (C) 2009 Francesco Zamblera.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/
    
:- module(config,[
		  lang_src_dir/1,
		  lang_bin_dir/1,
		  supported_markup_list/1
		 ]).

/** <module> Configuration parameters

*/

%%	lang_src_dir(-SourceDirectory) is det
%
%	PROLOG fact containing the path to the _|lang_src|_ directory,
%	containing the grammar source files.
%	
%	Invoked as lang_src_dir(-SourceDir), to get SourceDir
%	instantiated to the path to the _|lang_src|_ directory.
%	
lang_src_dir("./lang_src/").

%%	lang_bin_dir(+BinaryDirectory) is det
%
%	Contains the path to the _|lang_bin|_ directory,
%	containing the grammar compiled files (although it is
%	callse _bin_, these are not binary files,
%	but PROLOG source files as well, though much less human
%	readable than the actual grammar source codes.
%	
lang_bin_dir("./lang_bin/").

%%	supported_markup_list(List)
%
%	The only argument is a list of codes representing the supported 
%	markup formats available for grammar formatting and NSM-input
%	file parsing.
%	
%	Version 1.0 supports rtf and plain text.
%	
supported_markup_list([rtf,txt]).

