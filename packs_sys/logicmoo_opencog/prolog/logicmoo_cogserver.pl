/*
 * LOGICMOO CogServer Facade for Logicmoo Server
 *
 * Copyright (c) 2022 Logicmoo Co <support@logicmoo.org>
 *
 * LICENSE:
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

:- module(logicmoo_cogserver,[]).
/** <module> MODULE LOGICMOO OPENCOG
- @author Douglas R. Miles
*/

% [Required] Load the Logicmoo Library Utils
:- ensure_loaded(library(logicmoo_common)).

:- reexport(opencog/cogserver_shell).

%:- use_module(library(logicmoo_opencog)).
%:- use_module(library(logicmoo_clif)).


% This replicates Eric Mullers DEC reasoner from logicmoo (for OpenCog).

