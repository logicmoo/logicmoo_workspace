/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Tobias Rho, Lukas Degener, Andreas Becker, Fabian Noth
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module( split_file_path,
      [ split_file_path/5      % (+FullPath, ?Directory, ?FileName,?BaseName,?Extension)
      ]).
      
      
%% split_file_path(+FullPath,?Directory,?FileName,?BaseName,?Extension) is det
%
split_file_path(FullPath, Directory, FileName,BaseName,Extension):-
    file_directory_name(FullPath, Directory0),           % SWI-Prolog
    atom_concat(Directory0,'/',Directory),               % SWI-Prolog
    file_base_name(FullPath,FileName),                   % SWI-Prolog
    file_name_extension(BaseName,Extension,FileName).    % SWI-Prolog
            


