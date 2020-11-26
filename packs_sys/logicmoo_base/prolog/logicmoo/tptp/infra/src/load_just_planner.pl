/*
* Copyright (C) 2007 Christoph Wernhard
* 
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the Free
* Software Foundation; either version 2 of the License, or (at your option)
* any later version.
* 
* This program is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
* more details.
* 
* You should have received a copy of the GNU General Public License along with
* this program; if not, see <http://www.gnu.org/licenses/>.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Load Just Planner
%%%%
%%%% Loading this file effects loading of just the planner modules.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Instead of using the configuration mechanism in config.pl, just
%%%% the configuration items required by module planner_dotgraph
%%%% are specified here.
%%%% 
config:config(planner_dotgraph_fontname, 'Courier-Bold').
config:config(planner_dotgraph_fontsize, '11').
config:config(planner_image_viewer, xli).

%%%%
%%%% Load the planner.
%%%%
%%%% Further modules are loaded indirectly:
%%%% planner_cm, and planner_convert
%%%% 
:- use_module(planner_run).
:- use_module(planner_dotgraph).

%%%% 
%%%% These modules provide pp/1 and onto_file/2 which are
%%%% convenient for handling planner outputs.
%%%% 
:- use_module('swilib/pretty').
:- use_module('swilib/fromonto').

