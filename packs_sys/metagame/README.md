# metagame
============================================================
METAGAME Game-Playing Workbench 2.0.0
============================================================

I am announcing the availability of the latest version of the METAGAME
workbench.  This is a workbench for developing and playing metagame
programs.  The workbench also allows users to enter and modify the
rules of chess-variants, and test them out against other humans or
programs. This release contains the following:

* Generator for Symmetric Chess-like Games

  - Automatically generates new games in this diverse class.

  - Generator can be controlled by parameters affecting board-size,
    piece complexity, types of goals, branching factor, and so on. 

  - Generated pieces are given mnemonic names, which can be 
    modified by the user.

* Definitions of chess, checkers, chinese_chess, shogi, 
  lose_chess, lose_checkers, french_checkers, tic_tac_toe
  translated into symmetric chess-like games.

* Text interface for generating and playing games, with
  extensive help menus and move completion.

* Legal move generator

  - Takes in the rules of any game in this class (either generated 
    or user defined), an returns the set of legal moves from any
    board position. 

* Simple legal players: INSTANT, RANDOM, CAUTIOUS, RANDOM_AGGRESSIVE

* A sophisticated player: ITERATE

  - This is an iterative-deepending alpha-beta searcher, 
    with resource bounds, randomized ordering on ties,
    and the principal continuation heuristic

  - The supplied evaluation function uses features of MATERIAL,
    THREATS, PIECE-MOBILITY, TOTAL-MOBILITY, and PIECE-SQUARE tables.
    The weights for these features can be set by users or programs
    for different games. 

  - The evaluation function used in this searcher can be 
    entirely customized or replaced by the user or program.  

* Efficient indexing, for fast search and pattern matching.

* Game Clock / Timing Facilities

* Portability: This Workbench can now be used in Quintus or Sictus 
  Prolog.  


This is an EXPERIMENTAL system. You may take it and use it at your
own risk. No warranties are given or implied.  The primary concern has
been to develop a system which could play all the games in this class,
using a declarative respresentation suitable for machine learning and 
meta-reasoning.  

------------------------------------------------------------------------------
To build the Metagame Workbench:
------------------------------------------------------------------------------

The metagame system is implemented in SICStus Prolog, version 2.1,
which is available by email to:  sicstus-request@sics.se
This new release also supports Quintus Prolog. 

 8) Change into the Metagame directory

 cd Metagame

 9) Build the system

 make install

 10) Run the system

 bin/metagame

  That will put you in the system proper.  After that, use the help
  menus to find out what to do.

  Remember that if you ever encounter an error which fails to prolog,
  just type: "metagame."  to get back into the system.

 11) Have fun with the METAGAME system...

Note that some of these instructions (1-7) apply only to Berkeley derived 
Unix, eg BSD, SunOS, etc.
 


