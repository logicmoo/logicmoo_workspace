%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright 2003-2010, Renate Schmidt,  University of Manchester
%           2009-2010, Ullrich Hustadt, University of Liverpool
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Main file for loading the source files of a PDL tableau prover
%
% Ways of running the prover from the command line:
%
%     1. Basic
%        pl -f pdl_tableau.pl 
%        pl -f pdl_tableau.pl -g "provable(and(dia(comp(b,star(a)),p), box(comp(star(b),star(a)), not(p))), Result)." -t halt
%        pl -f pdl_tableau.pl -g "satisfiable(and(dia(comp(b,star(a)),p), box(comp(star(b),star(a)), not(p))), Result)." -t halt
%
%     2. Testing for verification purposes on a collection of problems
%        pl -f pdl_tableau.pl -g "problem(51, satisfiable, Result)." -t halt
%        pl -f pdl_tableau.pl -g 'testing(routine)' -t halt
%        (`routine' refers to the routine problems in the problem list.)
%        pl -f pdl_tableau.pl -g "load_calculus(pdl_dGM_nd), testing(routine)." -t halt > & /dev/null
%
%     3. Enable/Disable output of derivations
%        By default, this version of pdl-tableau disables most output by
%        loading 'no_output.pl', see 'ensure_loaded([...])' below
%        To enable output, one has to load 'output.pl' instead of 'no_output.pl'.
%        Furthermore, the set_output_format predicate at the end of this file
%        determines the output format that is used in the file /tmp/search.log
%        By default it is set to 'none', but can be changed to 'txt', for plain
%        text output, and 'latex', for latex output.
%
% For further configuration option see the comments on the calculus variations
% below.


%       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Basic source files

:- ensure_loaded([
       normalise,
       prover,
       caching,
       problems,
       testing,
       settings,
       no_output
   ]).

:- dynamic inconsistent/1.
:- dynamic consistent/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Calculus (first the rule, then the corresponding file)

% Uses
%       X_{<a*>p}
%   (X)-----------
%       p | <a>X
%
%         F v G
%   (v) -------------
%         F | G
%
%:- load_calculus(pdl_dGM_nd).
% 'nd' is short for `not disjoint' branches

% Uses
%       X_{<a*>p}
%   (X)-----------
%       p | <a>X
%
%         F v G
%   (v) -------------
%         F | -F, G
%
%:- load_calculus(pdl_dGM_bcs).
% 'bcs' is short for `boolean complement splitting'


% Uses
%        X_{<a*>p}
%   (X)--------------
%       p | -p, <a>X
%
%         F v G
%   (v) -------------
%         F | G
%
:- load_calculus(pdl_dGM).

% Uses
%        X_{<a*>p}
%   (X)--------------
%       p | -p, <a>X
%
%         F v G
%   (v) -------------
%         F | -F, G
%
%:- load_calculus(pdl_dGM_fcs).
% 'fcs' is short for 'full complement splitting'


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Inference closure rules (first the rule, then the corresponding file)
%

% Uses
%
%   p, -p
%   ----- (p must be atomic and X-free)
%   clash
%
%:- use_module(inf_closure_atomic).

% Uses
%
%   F, -F
%   ----- (F must be X-free)
%   clash
%
%:- use_module(inf_closure_strict).

% Uses
%
%   F, -F
%   ----- (F must be X-free)
%   clash
%
%   X_{<a*>p}, -<a*>p
%   -----------------
%        clash
%
:- use_module(inf_closure_x_free).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Logical settings

%:- set_negate_first(yes).    % negate problem first (does not
                              % change the theory)
:- set_negate_first(no).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Miscellaneous settings

% Set ouput format for search/proof steps
%:- set_output_format(txt).
%:- set_output_format(latex).
:- set_output_format(none).

:- set_search_output_filename('/tmp/search.log').
%:- set_search_output_filename('search.log').
:- set_test_output_filename('/tmp/testing.log').
%:- set_test_output_filename('testing.log').
