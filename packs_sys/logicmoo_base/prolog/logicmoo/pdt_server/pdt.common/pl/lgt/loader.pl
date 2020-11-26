/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).
logtalk_library_path(pdt_common_pl_lgt, Library) :-
	absolute_file_name(pdt_common_pl('lgt/loader.pl'), FilePath),
	file_directory_name(FilePath,Directory),
	atom_concat(Directory, '/', Library).

load_lgt_adapter :-
    (current_predicate(user:logtalk_load/1)
    -> logtalk_load([
            library(types_loader),
            library(metapredicates_loader),
            help(loader),
		    pdt_common_pl_lgt(utils4entities),
			pdt_common_pl_lgt(logtalk_adapter)
       ])
	;  true
	).
	
:- initialization( load_lgt_adapter ). 


