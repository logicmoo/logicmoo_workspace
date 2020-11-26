;;; -*- Mode: LISP; Package: SUBLISP-CL; Syntax: Common-lisp;  -*- 
;;; Mon Jan 21 16:21:20 1991 by Mark Kantrowitz <mkant@GLINDA.OZ.CS.CMU.EDU>
;;; sublisp-cl.lisp·

;;; ****************************************************************
;;; List Callers: A Static Analysis Cross Referencing Tool for Lisp 
;;; ****************************************************************
;;; 
;;; The List Callers system is a portable Common Lisp cross referencing
;;; utility. It grovels over a set of files and compiles a database of the
;;; locations of all references for each symbol used in the files.
;;; List Callers is similar to the Symbolics Who-Calls and the
;;; Xerox Masterscope facilities.
;;;
;;; When you change a function or variable definition, it can be useful
;;; to know its callers, in order to update each of them to the new
;;; definition. Similarly, having a graphic display of the structure 
;;; (e.g., call graph) of a program can help make undocumented code more
;;; understandable. This static code analyzer facilitates both capabilities.
;;; The database compiled by sublisp-cl is suitable for viewing by a graphical 
;;; browser. (Note: the reference graph is not necessarily a DAG. Since many
;;; graphical browsers assume a DAG, this will lead to infinite loops.
;;; Some code which is useful in working around this problem is included,
;;; as well as a sample text-indenting outliner and an interface to Bates'
;;; PSGraph Postscript Graphing facility.) 
;;;
;;; Written by Mark Kantrowitz, July 1990.
;;;
;;; Address: School of Computer Science
;;;    Carnegie Mellon University
;;;    Pittsburgh, PA 15213
;;;
;;; Copyright (c) 1990. All rights reserved.
;;;
;;; See general license below.
;;;

;;; ****************************************************************
;;; General License Agreement and Lack of Warranty *****************
;;; ****************************************************************
;;;
;;; This software is distributed in the hope that it will be useful (both
;;; in and of itself and as an example of lisp programming), but WITHOUT
;;; ANY WARRANTY. The author(s) do not accept responsibility to anyone for
;;; the consequences of using it or for whether it serves any particular
;;; purpose or works at all. No warranty is made about the software or its
;;; performance. 
;;; 
;;; Use and copying of this software and the preparation of derivative
;;; works based on this software are permitted, so long as the following
;;; conditions are met:
;;;   o  The copyright notice and this entire notice are included intact
;;;    and prominently carried on all copies and supporting documentation.
;;;   o  No fees or compensation are charged for use, copies, or
;;;    access to this software. You may charge a nominal
;;;    distribution fee for the physical act of transferring a
;;;    copy, but you may not charge for the program itself. 
;;;   o  If you modify this software, you must cause the modified
;;;    file(s) to carry prominent notices (a Change Log)
;;;    describing the changes, who made the changes, and the date
;;;    of those changes.
;;;   o  Any work distributed or published that in whole or in part
;;;    contains or is a derivative of this software or any part 
;;;    thereof is subject to the terms of this agreement. The 
;;;    aggregation of another unrelated program with this software
;;;    or its derivative on a volume of storage or distribution
;;;    medium does not bring the other program under the scope
;;;    of these terms.
;;;   o  Permission is granted to manufacturers and distributors of
;;;    lisp compilers and interpreters to include this software
;;;    with their distribution. 
;;; 
;;; This software is made available AS IS, and is distributed without 
;;; warranty of any kind, either expressed or implied.
;;; 
;;; In no event will the author(s) or their institutions be liable to you
;;; for damages, including lost profits, lost monies, or other special,
;;; incidental or consequential damages arising out of or in connection
;;; with the use or inability to use (including but not limited to loss of
;;; data or data being rendered inaccurate or losses sustained by third
;;; parties or a failure of the program to operate as documented) the 
;;; program, even if you have been advised of the possibility of such
;;; damanges, or for any claim by any other party, whether in an action of
;;; contract, negligence, or other tortious action.
;;; 
;;; The current version of this software and a variety of related utilities
;;; may be obtained by anonymous ftp from ftp.cs.cmu.edu in the directory
;;;  user/ai/lang/lisp/code/tools/sublisp-cl/
;;; 
;;; Please send bug reports, comments, questions and suggestions to
;;; mkant@cs.cmu.edu. We would also appreciate receiving any changes
;;; or improvements you may make. 
;;; 
;;; If you wish to be added to the Lisp-Utilities@cs.cmu.edu mailing list, 
;;; send email to Lisp-Utilities-Request@cs.cmu.edu with your name, email
;;; address, and affiliation. This mailing list is primarily for
;;; notification about major updates, bug fixes, and additions to the lisp
;;; utilities collection. The mailing list is intended to have low traffic.
;;;

;;; ********************************
;;; Change Log *********************
;;; ********************************
;;;
;;; 27-FEB-91 mk Added insert arg to psgraph-sublisp-cl to allow the postscript
;;;      graphs to be inserted in Scribe documents.
;;; 21-FEB-91 mk Added warning if not compiled.
;;; 07-FEB-91 mk Fixed bug in record-callers with regard to forms at 
;;;      toplevel.
;;; 21-JAN-91 mk Added file sublisp-cl-test.lisp to test sublisp-cl.
;;; 16-JAN-91 mk Added definition WHO-CALLS to parallel the Symbolics syntax.
;;; 16-JAN-91 mk Added macroexpansion capability to record-callers. Also
;;;      added parameter *handle-macro-forms*, defaulting to T.
;;; 16-JAN-91 mk Modified print-caller-tree and related functions
;;;      to allow the user to specify root nodes. If the user
;;;      doesn't specify them, it will default to all root
;;;      nodes, as before. 
;;; 16-JAN-91 mk Added parameter *default-graphing-mode* to specify
;;;      the direction of the graphing. Either :call-graph,
;;;      where the children of a node are those functions called
;;;      by the node, or :caller-graph where the children of a
;;;      node are the callers of the node. :call-graph is the
;;;      default.
;;; 16-JAN-91 mk Added parameter *indent-amount* to control the indentation
;;;      in print-indented-tree.
;;; 16-JUL-90 mk Functions with argument lists of () were being ignored
;;;      because of a (when form) wrapped around the body of
;;;      record-callers. Then intent of (when form) was as an extra
;;;      safeguard against infinite looping. This wasn't really
;;;      necessary, so it has been removed.
;;; 16-JUL-90 mk PSGraph-SUBLISP-CL now has keyword arguments, instead of
;;;      optionals.
;;; 16-JUL-90 mk Added PRINT-CLASS-HIERARCHY to use psgraph to graph the
;;;      CLOS class hierarchy. This really doesn't belong here,
;;;      and should be moved to psgraph.lisp as an example of how
;;;      to use psgraph.
;;; 16-JUL-90 mk Fixed several caller patterns. The pattern for member
;;;      had an error which caused many references to be missed.
;;; 16-JUL-90 mk Added ability to save/load processed databases.
;;;  5-JUL-91 mk  Fixed warning of needing compilation to occur only when the
;;;       source is loaded.
;;; 20-SEP-93 mk  Added fix from Peter Norvig to allow Sublisp-cl to sublisp-cl itself.
;;;       The arg to macro-function must be a symbol.

;;; ********************************
;;; To Do **************************
;;; ********************************
;;;
;;; Verify that:
;;;  o  null forms don't cause it to infinite loop.
;;;  o  nil matches against null argument lists.
;;;  o  declarations and doc are being ignored.
;;;
;;; Would be nice if in addition to showing callers of a function, it
;;; displayed the context of the calls to the function (e.g., the
;;; immediately surrounding form). This entails storing entries of
;;; the form (symbol context*) in the database and augmenting
;;; record-callers to keep the context around. The only drawbacks is
;;; that it would cons a fair bit. If we do this, we should store
;;; additional information as well in the database, such as the caller
;;; pattern type (e.g., variable vs. function).
;;;
;;; Write a translator from BNF (at least as much of BNF as is used
;;; in CLtL2), to the format used here.
;;;
;;; Should automatically add new patterns for new functions and macros
;;; based on their arglists. Probably requires much more than this
;;; simple code walker, so there isn't much we can do.
;;;
;;; Defmacro is a problem, because it often hides internal function
;;; calls within backquote and quote, which we normally ignore. If
;;; we redefine QUOTE's pattern so that it treats the arg like a FORM,
;;; we'll probably get them (though maybe the syntax will be mangled),
;;; but most likely a lot of spurious things as well. 
;;;
;;; Define an operation for Defsystem which will run SUBLISP-CL-FILE on the
;;; files of the system. Or yet simpler, when SUBLISP-CL sees a LOAD form
;;; for which the argument is a string, tries to recursively call
;;; SUBLISP-CL-FILE on the specified file. Then one could just SUBLISP-CL-FILE
;;; the file which loads the system. (This should be a program
;;; parameter.)
;;;
;;; Have special keywords which the user may place in a file to have
;;; SUBLISP-CL-FILE ignore a region.
;;;
;;; Should we distinguish flet and labels from defun? I.e., note that
;;; flet's definitions are locally defined, instead of just lumping
;;; them in with regular definitions.
;;;
;;; Add patterns for series, loop macro.
;;;
;;; Need to integrate the variable reference database with the other
;;; databases, yet maintain separation. So we can distinguish all
;;; the different types of variable and function references, without
;;; multiplying databases.
;;;
;;; Would pay to comment record-callers and record-callers* in more
;;; depth.
;;; 
;;; (&OPTIONAL &REST &KEY &AUX &BODY &WHOLE &ALLOW-OTHER-KEYS &ENVIRONMENT)

;;; ********************************
;;; Notes **************************
;;; ********************************
;;;
;;;  SUBLISP-CL has been tested (successfully) in the following lisps:
;;;   CMU Common Lisp (M2.9 15-Aug-90, Compiler M1.8 15-Aug-90)
;;;   Macintosh Allegro Common Lisp (1.3.2)
;;;   ExCL (Franz Allegro CL 3.1.12 [DEC 3100] 3/30/90)
;;;   Lucid CL (Version 2.1 6-DEC-87)
;;;  
;;;  SUBLISP-CL has been tested (unsuccessfully) in the following lisps:
;;;   Ibuki Common Lisp (01/01, October 15, 1987)
;;;     - if interpreted, runs into stack overflow
;;;     - does not compile (tried ibcl on Suns, PMAXes and RTs)
;;;     seems to be due to a limitation in the c compiler.
;;;  
;;;  SUBLISP-CL needs to be tested in the following lisps:
;;;   Symbolics Common Lisp (8.0)
;;;   Lucid Common Lisp (3.0, 4.0)
;;;   KCL (June 3, 1987 or later)
;;;   AKCL (1.86, June 30, 1987 or later)
;;;   TI (Release 4.1 or later)
;;;   Golden Common Lisp (3.1 IBM-PC)
;;;   VAXLisp (2.0, 3.1)
;;;   HP Common Lisp (same as Lucid?)
;;;   Procyon Common Lisp


;;; ****************************************************************
;;; Documentation **************************************************
;;; ****************************************************************
;;;
;;; SUBLISP-CL analyzes a user's program, determining which functions call a
;;; given function, and the location of where variables are bound/assigned
;;; and used. The user may retrieve this information for either a single
;;; symbol, or display the call graph of portions of the program
;;; (including the entire program). This allows the programmer to debug
;;; and document the program's structure.
;;; 
;;; SUBLISP-CL is primarily intended for analyzing large programs, where it is
;;; difficult, if not impossible, for the programmer to grasp the structure
;;; of the whole program. Nothing precludes using SUBLISP-CL for smaller programs,
;;; where it can be useful for inspecting the relationships between pieces
;;; of the program and for documenting the program.
;;; 
;;; Two aspects of the Lisp programming language greatly simplify the
;;; analysis of Lisp programs:
;;;   o  Lisp programs are naturally represented as data.
;;;    Successive definitions from a file are easily read in
;;;    as list structure.
;;;   o  The basic syntax of Lisp is uniform. A list program
;;;    consists of a set of nested forms, where each form is
;;;    a list whose car is a tag (e.g., function name) that
;;;    specifies the structure of the rest of the form.
;;; Thus Lisp programs, when represented as data, can be considered to be
;;; parse trees. Given a grammar of syntax patterns for the language, SUBLISP-CL
;;; recursively descends the parse tree for a given definition, computing
;;; a set of relations that hold for the definition at each node in the
;;; tree. For example, one kind of relation is that the function defined
;;; by the definition calls the functions in its body. The relations are
;;; stored in a database for later examination by the user.
;;; 
;;; While SUBLISP-CL currently only works for programs written in Lisp, it could
;;; be extended to other programming languages by writing a function to
;;; generate parse trees for definitions in that language, and a core
;;; set of patterns for the language's syntax.
;;; 
;;; Since SUBLISP-CL normally does a static syntactic analysis of the program, 
;;; it does not detect references due to the expansion of a macro definition. 
;;; To do this in full generality SUBLISP-CL would have to have knowledge about the
;;; semantics of the program (e.g., macros which call other functions to
;;; do the expansion). This entails either modifying the compiler to
;;; record the relationships (e.g., Symbolics Who-Calls Database) or doing
;;; a walk of loaded code and macroexpanding as needed (PCL code walker).
;;; The former is not portable, while the latter requires that the code
;;; used by macros be loaded and in working order. On the other hand, then
;;; we would need no special knowledge about macros (excluding the 24 special
;;; forms of Lisp).
;;;
;;; Parameters may be set to enable macro expansion in SUBLISP-CL. Then SUBLISP-CL
;;; will expand any macros for which it does not have predefined patterns.
;;; (For example, most Lisps will implement dolist as a macro. Since SUBLISP-CL
;;; has a pattern defined for dolist, it will not call macroexpand-1 on
;;; a form whose car is dolist.) For this to work properly, the code must
;;; be loaded before being processed by SUBLISP-CL, and SUBLISP-CL's parameters should
;;; be set so that it processes forms in their proper packages. 
;;;
;;; If macro expansion is disabled, the default rules for handling macro
;;; references may not be sufficient for some user-defined macros, because
;;; macros allow a variety of non-standard syntactic extensions to the
;;; language. In this case, the user may specify additional templates in
;;; a manner similar to that in which the core Lisp grammar was specified.
;;;


;;; ********************************
;;; User Guide *********************
;;; ********************************
;;; -----
;;; The following functions are called to cross reference the source files.
;;;
;;; SUBLISP-CL-FILES (&rest files)              [FUNCTION]
;;;  Grovels over the lisp code located in source file FILES, using
;;;  sublisp-cl-file.
;;;
;;; SUBLISP-CL-FILE (filename &optional clear-tables verbose)   [Function]
;;;  Cross references the function and variable calls in FILENAME by
;;;  walking over the source code located in the file. Defaults type of
;;;  filename to ".lisp". Chomps on the code using record-callers and
;;;  record-callers*. If CLEAR-TABLES is T (the default), it clears the
;;;  callers database before processing the file. Specify CLEAR-TABLES as
;;;  nil to append to the database. If VERBOSE is T (the default), prints
;;;  out the name of the file, one progress dot for each form processed,
;;;  and the total number of forms.
;;;
;;; -----
;;; The following functions display information about the uses of the 
;;; specified symbol as a function, variable, or constant.
;;;
;;; LIST-CALLERS (symbol)               [FUNCTION]
;;;  Lists all functions which call SYMBOL as a function (function
;;;  invocation).
;;;
;;; LIST-READERS (symbol)               [FUNCTION]
;;;  Lists all functions which refer to SYMBOL as a variable
;;;  (variable reference).
;;;
;;; LIST-SETTERS (symbol)               [FUNCTION]
;;;  Lists all functions which bind/set SYMBOL as a variable
;;;  (variable mutation).
;;;
;;; LIST-USERS (symbol)               [FUNCTION]
;;;  Lists all functions which use SYMBOL as a variable or function.
;;;
;;; WHO-CALLS (symbol &optional how)          [FUNCTION]
;;;  Lists callers of symbol. HOW may be :function, :reader, :setter,
;;;  or :variable."
;;;
;;; WHAT-FILES-CALL (symbol)              [FUNCTION]
;;;  Lists names of files that contain uses of SYMBOL
;;;  as a function, variable, or constant.
;;;
;;; SOURCE-FILE (symbol)              [FUNCTION]
;;;  Lists the names of files in which SYMBOL is defined/used.
;;;
;;; LIST-CALLEES (symbol)               [FUNCTION]
;;;  Lists names of functions and variables called by SYMBOL.
;;;
;;; -----
;;; The following functions may be useful for viewing the database and
;;; debugging the calling patterns.
;;;
;;; *LAST-FORM* ()                [VARIABLE]
;;;  The last form read from the file. Useful for figuring out what went
;;;  wrong when sublisp-cl-file drops into the debugger.
;;;
;;; *SUBLISP-CL-VERBOSE* t                [VARIABLE]
;;;  When T, sublisp-cl-file(s) prints out the names of the files it looks at,
;;;  progress dots, and the number of forms read.
;;;
;;; *TYPES-TO-IGNORE* (quote (:lisp :lisp2))        [VARIABLE]
;;;  Default set of caller types (as specified in the patterns) to ignore
;;;  in the database handling functions. :lisp is CLtL 1st edition,
;;;  :lisp2 is additional patterns from CLtL 2nd edition.
;;;
;;; *HANDLE-PACKAGE-FORMS* ()             [VARIABLE]
;;;  When non-NIL, and SUBLISP-CL-FILE sees a package-setting form like
;;;  IN-PACKAGE, sets the current package to the specified package by
;;;  evaluating the form. When done with the file, sublisp-cl-file resets the
;;;  package to its original value. In some of the displaying functions,
;;;  when this variable is non-NIL one may specify that all symbols from a
;;;  particular set of packages be ignored. This is only useful if the
;;;  files use different packages with conflicting names.
;;;
;;; *HANDLE-FUNCTION-FORMS* t             [VARIABLE]
;;;  When T, SUBLISP-CL-FILE tries to be smart about forms which occur in
;;;  a function position, such as lambdas and arbitrary Lisp forms.
;;;  If so, it recursively calls record-callers with pattern 'FORM.
;;;  If the form is a lambda, makes the caller a caller of
;;;  :unnamed-lambda.
;;;
;;; *HANDLE-MACRO-FORMS* t              [VARIABLE]
;;;  When T, if the file was loaded before being processed by SUBLISP-CL, and
;;;  the car of a form is a macro, it notes that the parent calls the
;;;  macro, and then calls macroexpand-1 on the form.
;;;
;;; *DEFAULT-GRAPHING-MODE* :call-graph         [VARIABLE]
;;;  Specifies whether we graph up or down. If :call-graph, the children
;;;  of a node are the functions it calls. If :caller-graph, the
;;;  children of a node are the functions that call it.
;;;
;;; *INDENT-AMOUNT* 3               [VARIABLE]
;;;  Number of spaces to indent successive levels in PRINT-INDENTED-TREE.
;;;
;;; DISPLAY-DATABASE (&optional database types-to-ignore)   [FUNCTION]
;;;  Prints out the name of each symbol and all its callers. Specify
;;;  database :callers (the default) to get function call references,
;;;  :file to the get files in which the symbol is called, :readers to get
;;;  variable references, and :setters to get variable binding and
;;;  assignments. Ignores functions of types listed in types-to-ignore.
;;;
;;; PRINT-CALLER-TREES (&key (mode *default-graphing-mode*)   [FUNCTION]
;;;       (types-to-ignore *types-to-ignore*)
;;;       compact root-nodes)
;;;  Prints the calling trees (which may actually be a full graph and not
;;;  necessarily a DAG) as indented text trees using
;;;  PRINT-INDENTED-TREE. MODE is :call-graph for trees where the children
;;;  of a node are the functions called by the node, or :caller-graph for
;;;  trees where the children of a node are the functions the node calls.
;;;  TYPES-TO-IGNORE is a list of funcall types (as specified in the
;;;  patterns) to ignore in printing out the database. For example,
;;;  '(:lisp) would ignore all calls to common lisp functions. COMPACT is
;;;  a flag to tell the program to try to compact the trees a bit by not
;;;  printing trees if they have already been seen. ROOT-NODES is a list
;;;  of root nodes of trees to display. If ROOT-NODES is nil, tries to
;;;  find all root nodes in the database.
;;;
;;; MAKE-CALLER-TREE (&optional (mode *default-graphing-mode*)  [FUNCTION]
;;;       (types-to-ignore *types-to-ignore*)
;;;       compact)
;;;  Outputs list structure of a tree which roughly represents the
;;;  possibly cyclical structure of the caller database.
;;;  If mode is :call-graph, the children of a node are the functions
;;;  it calls. If mode is :caller-graph, the children of a node are the
;;;  functions that call it.
;;;  If compact is T, tries to eliminate the already-seen nodes, so
;;;  that the graph for a node is printed at most once. Otherwise it will
;;;  duplicate the node's tree (except for cycles). This is usefull
;;;  because the call tree is actually a directed graph, so we can either
;;;  duplicate references or display only the first one.
;;;
;;; DETERMINE-FILE-DEPENDENCIES (&optional database)    [FUNCTION]
;;;  Makes a hash table of file dependencies for the references listed in
;;;  DATABASE. This function may be useful for automatically resolving
;;;  file references for automatic creation of a system definition
;;;  (defsystem).
;;;
;;; PRINT-FILE-DEPENDENCIES (&optional database)      [FUNCTION]
;;;  Prints a list of file dependencies for the references listed in
;;;  DATABASE. This function may be useful for automatically computing
;;;  file loading constraints for a system definition tool.
;;;
;;; WRITE-CALLERS-DATABASE-TO-FILE (filename)       [FUNCTION]
;;;  Saves the contents of the current callers database to a file. This
;;;  file can be loaded to restore the previous contents of the
;;;  database. (For large systems it can take a long time to crunch
;;;  through the code, so this can save some time.)
;;;
;;; -----
;;; The following macros define new function and macro call patterns.
;;; They may be used to extend the static analysis tool to handle
;;; new def forms, extensions to Common Lisp, and program defs.
;;;
;;; DEFINE-PATTERN-SUBSTITUTION (name pattern)        [MACRO]
;;;  Defines NAME to be equivalent to the specified pattern. Useful for
;;;  making patterns more readable. For example, the LAMBDA-LIST is
;;;  defined as a pattern substitution, making the definition of the
;;;  DEFUN caller-pattern simpler.
;;;
;;; DEFINE-CALLER-PATTERN (name pattern &optional caller-type)  [MACRO]
;;;  Defines NAME as a function/macro call with argument structure
;;;  described by PATTERN. CALLER-TYPE, if specified, assigns a type to
;;;  the pattern, which may be used to exclude references to NAME while
;;;  viewing the database. For example, all the Common Lisp definitions
;;;  have a caller-type of :lisp or :lisp2, so that you can exclude
;;;  references to common lisp functions from the calling tree.
;;;
;;; DEFINE-VARIABLE-PATTERN (name &optional caller-type)    [MACRO]
;;;  Defines NAME as a variable reference of type CALLER-TYPE. This is
;;;  mainly used to establish the caller-type of the variable.
;;;
;;; DEFINE-CALLER-PATTERN-SYNONYMS (source destinations)    [MACRO]
;;;  For defining function caller pattern syntax synonyms. For each name
;;;  in DESTINATIONS, defines its pattern as a copy of the definition
;;;  of SOURCE. Allows a large number of identical patterns to be defined
;;;  simultaneously. Must occur after the SOURCE has been defined.
;;;
;;; -----
;;; This system includes pattern definitions for the latest
;;; common lisp specification, as published in Guy Steele,
;;; Common Lisp: The Language, 2nd Edition.
;;;
;;; Patterns may be either structures to match, or a predicate
;;; like symbolp/numberp/stringp. The pattern specification language
;;; is similar to the notation used in CLtL2, but in a more lisp-like 
;;; form:
;;;  (:eq name)     The form element must be eq to the symbol NAME.
;;;  (:test test)   TEST must be true when applied to the form element.
;;;  (:typep type)    The form element must be of type TYPE.
;;;  (:or pat1 pat2 ...)  Tries each of the patterns in left-to-right order,
;;;         until one succeeds.
;;;         Equivalent to { pat1 | pat2 | ... }
;;;  (:rest pattern)  The remaining form elements are grouped into a
;;;         list which is matched against PATTERN.
;;;  (:optional pat1 ...) The patterns may optionally match against the
;;;         form element.
;;;         Equivalent to [ pat1 ... ].
;;;  (:star pat1 ...)   The patterns may match against the patterns
;;;         any number of times, including 0.
;;;         Equivalent to { pat1 ... }*.
;;;  (:plus pat1 ...)   The patterns may match against the patterns
;;;         any number of times, but at least once.
;;;         Equivalent to { pat1 ... }+.
;;;  &optional, &key,   Similar in behavior to the corresponding
;;;  &rest      lambda-list keywords.
;;;  FORM       A random lisp form. If a cons, assumes the
;;;         car is a function or macro and tries to
;;;         match the args against that symbol's pattern.
;;;         If a symbol, assumes it's a variable reference.
;;;  :ignore      Ignores the corresponding form element.
;;;  NAME       The corresponding form element should be
;;;         the name of a new definition (e.g., the
;;;         first arg in a defun pattern is NAME.
;;;  FUNCTION, MACRO  The corresponding form element should be
;;;         a function reference not handled by FORM.
;;;         Used in the definition of apply and funcall.
;;;  VAR      The corresponding form element should be
;;;         a variable definition or mutation. Used
;;;         in the definition of let, let*, etc.
;;;  VARIABLE     The corresponding form element should be
;;;         a variable reference. 
;;;
;;; In all other pattern symbols, it looks up the symbols pattern substitution
;;; and recursively matches against the pattern. Automatically destructures
;;; list structure that does not include consing dots.
;;;
;;; Among the pattern substitution names defined are:
;;;  STRING, SYMBOL, NUMBER  Appropriate :test patterns.
;;;  LAMBDA-LIST     Matches against a lambda list.
;;;  BODY        Matches against a function body definition.
;;;  FN        Matches against #'function, 'function,
;;;          and lambdas. This is used in the definition
;;;          of apply, funcall, and the mapping patterns.
;;;  and others...
;;;
;;; Here's some sample pattern definitions:
;;; (define-caller-pattern defun 
;;; (name lambda-list
;;; (:star (:or documentation-string declaration))
;;; pbody)
;;;  :lisp)
;;; (define-caller-pattern funcall (fn pbody) :lisp)
;;;
;;; In general, the system is intelligent enough to handle any sort of
;;; simple funcall. One only need specify the syntax for functions and
;;; macros which use optional arguments, keyword arguments, or some
;;; argument positions are special, such as in apply and funcall, or
;;; to indicate that the function is of the specified caller type.
;;;
;;;
;;; NOTES:
;;;
;;;  sublisp-cl assumes syntactically correct lisp code.
;;;
;;;  This is by no means perfect. For example, let and let* are treated
;;;  identically, instead of differentiating between serial and parallel
;;;  binding. But it's still a useful tool. It can be helpful in 
;;;  maintaining code, debugging problems with patch files, determining
;;;  whether functions are multiply defined, and help you remember where
;;;  a function is defined or called.
;;;
;;;  SUBLISP-CL runs best when compiled.

;;; ********************************
;;; References *********************
;;; ********************************
;;;
;;; Xerox Interlisp Masterscope Program:
;;; Larry M Masinter, Global program analysis in an interactive environment
;;; PhD Thesis, Stanford University, 1980. 
;;;
;;; Symbolics Who-Calls Database:
;;; User's Guide to Symbolics Computers, Volume 1, Cambridge, MA, July 1986
;;; Genera 7.0, pp 183-185.
;;; 

;;; ********************************
;;; Example ************************
;;; ********************************
;;; 
;;; Here is an example of running SUBLISP-CL on a short program.
;;; [In Scribe documentation, give a simple short program and resulting
;;;  SUBLISP-CL output, including postscript call graphs.]
#|
<cl> (sublisp-cl:sublisp-cl-file  "/afs/cs/user/mkant/Lisp/Graph-Dag/graph-dag.lisp")
Cross-referencing file /afs/cs/user/mkant/Lisp/Graph-Dag/graph-dag.lisp.
................................................
48 forms processed.
<cl> (sublisp-cl:display-database :readers)

*DISPLAY-CUTOFF-DEPTH* is referenced by CALCULATE-LEVEL-POSITION CALCULATE-LEVEL-POSITION-BEFORE CALCULATE-POSITION-IN-LEVEL.
*OFFSET-FROM-EDGE-OF-PANE* is referenced by CALCULATE-LEVEL-POSITION CALCULATE-LEVEL-POSITION-BEFORE.
*WITHIN-LEVEL-SPACING* is referenced by BREADTH CALCULATE-POSITION-INFO.
*DIRECTION* is referenced by CREATE-POSITION-INFO.
*LINK-OFFSET* is referenced by OFFSET-OF-LINK-FROM-ATTACHMENT-POINT.
*ROOT-IS-SEQUENCE* is referenced by GRAPH.
*LEVEL-SPACING* is referenced by CALCULATE-LEVEL-POSITION CALCULATE-LEVEL-POSITION-BEFORE.
*ORIENTATION* is referenced by BREADTH CALCULATE-LEVEL-POSITION CALCULATE-LEVEL-POSITION-BEFORE CALCULATE-POSITION-IN-LEVEL.
*DEFAULT-GRAPH-POSITION* is referenced by CREATE-POSITION-INFO.
*GRAPHING-CUTOFF-DEPTH* is referenced by CREATE-NODE-STRUCTURE.
*LIST-OF-NODES* is referenced by CALCULATE-LEVEL-POSITION CALCULATE-LEVEL-POSITION-BEFORE CREATE-NODE FIND-NODE.
*GRAPH-TYPE* is referenced by CREATE-NODE-STRUCTURE.
<cl> (sublisp-cl:print-caller-trees :root-nodes '(display-graph))

Rooted calling trees:
DISPLAY-GRAPH
CREATE-POSITION-INFO
CALCULATE-POSITION-INFO
CALCULATE-POSITION
NODE-POSITION-ALREADY-SET-FLAG
NODE-LEVEL-ALREADY-SET-FLAG
CALCULATE-POSITION-IN-LEVEL
NODE-CHILDREN
NODE-LEVEL
CALCULATE-POSITION
NEW-CALCULATE-BREADTH
NODE-CHILDREN
BREADTH
OPPOSITE-DIMENSION
NODE-HEIGHT
NODE-WIDTH
NEW-CALCULATE-BREADTH
NODE-PARENTS
OPPOSITE-DIMENSION
NODE-HEIGHT
NODE-WIDTH
OPPOSITE-POSITION
NODE-Y
NODE-X
NODE-LEVEL
CALCULATE-LEVEL-POSITION
NODE-LEVEL
NODE-POSITION
NODE-X
NODE-Y
DIMENSION
NODE-WIDTH
NODE-HEIGHT
CALCULATE-LEVEL-POSITION-BEFORE
NODE-LEVEL
NODE-POSITION
NODE-X
NODE-Y
NODE-WIDTH
NODE-HEIGHT
DIMENSION
NODE-WIDTH
NODE-HEIGHT
|#

;;; ****************************************************************
;;; List Callers ***************************************************
;;; ****************************************************************

#|
dmiles
(defpackage :sublisp-cl
  (:use :common-lisp)
  (:export #:list-callers 
           #:list-users 
           #:list-readers 
           #:list-setters
           #:what-files-call
           #:who-calls 
           #:list-callees 
           #:source-file 
           #:clear-tables
           #:define-pattern-substitution 
           #:define-caller-pattern 
           #:define-variable-pattern 
           #:define-caller-pattern-synonyms
           #:clear-patterns
           #:*last-form* 
           #:*sublisp-cl-verbose* 
           #:*handle-package-forms* 
           #:*handle-function-forms*
           #:*handle-macro-forms*
           #:*types-to-ignore*
           #:*last-caller-tree* 
           #:*default-graphing-mode* 
           #:*indent-amount*
           #:sublisp-cl-file 
           #:sublisp-cl-files
           #:write-callers-database-to-file
           #:display-database
           #:print-caller-trees 
           #:make-caller-tree 
           #:print-indented-tree 
           #:determine-file-dependencies 
           #:print-file-dependencies
           #:psgraph-sublisp-cl
           ))

(in-package "SUBLISP-CL")
|#
;;; Warn user if they're loading the source instead of compiling it first.
;(eval-when (compile load eval)
;  (defvar compiled-p nil))
;(eval-when (compile load)
;  (setq compiled-p t))
;(eval-when (load eval)
;  (unless compiled-p
;  (warn "This file should be compiled before loading for best results.")))
(eval-when (eval) (warn "This file should be compiled before loading for best results."))

(make-package "SUBLISP")
;;; ********************************
;;; Primitives *********************
;;; ********************************
(defun lookup (symbol environment)
  (dolist (frame environment)
    (when (member symbol frame)
      (return symbol))))

(defun car-eq (list item)
  (and (consp list)
       (eq (car list) item)))

;;; ********************************
;;; Callers Database ***************
;;; ********************************
(defvar *file-callers-database* (make-hash-table :test #'equal)
  "Contains name and list of file callers (files which call) for that name.")
(defvar *callers-database* (make-hash-table :test #'equal)
  "Contains name and list of callers (function invocation) for that name.")
(defvar *readers-database* (make-hash-table :test #'equal)
  "Contains name and list of readers (variable use) for that name.")
(defvar *setters-database* (make-hash-table :test #'equal)
  "Contains name and list of setters (variable mutation) for that name.")
(defvar *callees-database* (make-hash-table :test #'equal)
  "Contains name and list of functions and variables it calls.")
(defun callers-list (name &optional (database :callers))
  (case database
    (:file  (gethash name *file-callers-database*))
    (:callees (gethash name *callees-database*))
    (:callers (gethash name *callers-database*))
    (:readers (gethash name *readers-database*))
    (:setters (gethash name *setters-database*))))
(defsetf callers-list (name &optional (database :callers)) (caller)
  `(setf (gethash ,name (case ,database
                          (:file  *file-callers-database*)
                          (:callees *callees-database*)
                          (:callers *callers-database*)
                          (:readers *readers-database*)
                          (:setters *setters-database*)))
     ,caller))

(defun list-callers (symbol)
  "Lists all functions which call SYMBOL as a function (function invocation)."
  (callers-list symbol :callers))
(defun list-readers (symbol)
  "Lists all functions which refer to SYMBOL as a variable 
 (variable reference)."
  (callers-list symbol :readers))
(defun list-setters (symbol)
  "Lists all functions which bind/set SYMBOL as a variable 
 (variable mutation)."
  (callers-list symbol :setters))
(defun list-users (symbol)
  "Lists all functions which use SYMBOL as a variable or function."
  (values (list-callers symbol)
          (list-readers symbol)
          (list-setters symbol)))
(defun who-calls (symbol &optional how)
  "Lists callers of symbol. HOW may be :function, :reader, :setter,
 or :variable."
  ;; would be nice to have :macro and distinguish variable
  ;; binding from assignment. (i.e., variable binding, assignment, and use)
  (case how
    (:function (list-callers symbol))
    (:reader (list-readers symbol))
    (:setter (list-setters symbol))
    (:variable (append (list-readers symbol) 
                       (list-setters symbol)))
    (otherwise (append (list-callers symbol)
                       (list-readers symbol)
                       (list-setters symbol)))))
(defun what-files-call (symbol)
  "Lists names of files that contain uses of SYMBOL 
 as a function, variable, or constant."
  (callers-list symbol :file))
(defun list-callees (symbol)
  "Lists names of functions and variables called by SYMBOL."
  (callers-list symbol :callees))

(defvar *source-file* (make-hash-table :test #'equal)
  "Contains function name and source file for that name.")
(defun source-file (symbol)
  "Lists the names of files in which SYMBOL is defined/used."
  (gethash symbol *source-file*))
(defsetf source-file (name) (value)
  `(setf (gethash ,name *source-file*) ,value))

(defun clear-tables ()
  (clrhash *file-callers-database*)
  (clrhash *callers-database*)
  (clrhash *callees-database*)
  (clrhash *readers-database*)
  (clrhash *setters-database*)
  (clrhash *source-file*))


;;; ********************************
;;; Pattern Database ***************
;;; ********************************
;;; Pattern Types
(defvar *pattern-caller-type* (make-hash-table :test #'equal))
(defun pattern-caller-type (name)
  (gethash name *pattern-caller-type*))
(defsetf pattern-caller-type (name) (value)
  `(setf (gethash ,name *pattern-caller-type*) ,value))

;;; Pattern Substitutions
(defvar *pattern-substitution-table* (make-hash-table :test #'equal)
  "Stores general patterns for function destructuring.")
(defun lookup-pattern-substitution (name)
  (gethash name *pattern-substitution-table*))
(defmacro define-pattern-substitution (name pattern)
  "Defines NAME to be equivalent to the specified pattern. Useful for
 making patterns more readable. For example, the LAMBDA-LIST is 
 defined as a pattern substitution, making the definition of the
 DEFUN caller-pattern simpler."
  `(setf (gethash ',name *pattern-substitution-table*)
     ',pattern))

;;; Function/Macro caller patterns: 
;;; The car of the form is skipped, so we don't need to specify
;;; (:eq function-name) like we would for a substitution.
;;;
;;; Patterns must be defined in the SUBLISP-CL package because the pattern
;;; language is tested by comparing symbols (using #'equal) and not
;;; their printreps. This is fine for the lisp grammer, because the SUBLISP-CL
;;; package depends on the LISP package, so a symbol like 'sublisp-cl::cons is
;;; translated automatically into 'lisp::cons. However, since
;;; (equal 'foo::bar 'baz::bar) returns nil unless both 'foo::bar and
;;; 'baz::bar are inherited from the same package (e.g., LISP), 
;;; if package handling is turned on the user must specify package 
;;; names in the caller pattern definitions for functions that occur
;;; in packages other than LISP, otherwise the symbols will not match.
;;; 
;;; Perhaps we should enforce the definition of caller patterns in the
;;; SUBLISP-CL package by wrapping the body of define-caller-pattern in
;;; the SUBLISP-CL package:
;;;  (defmacro define-caller-pattern (name value &optional caller-type)
;;;  (let ((old-package *package*))
;;;    (setf *package* (find-package "SUBLISP-CL"))
;;;    (prog1
;;;    `(progn
;;;     (when ',caller-type
;;;      (setf (pattern-caller-type ',name) ',caller-type))
;;;     (when ',value 
;;;     (setf (gethash ',name *caller-pattern-table*)
;;;       ',value)))
;;;    (setf *package* old-package)))) 
;;; Either that, or for the purpose of pattern testing we should compare
;;; printreps. [The latter makes the primitive patterns like VAR
;;; reserved words.]
(defvar *caller-pattern-table* (make-hash-table :test #'equal)
  "Stores patterns for function destructuring.")
(defun lookup-caller-pattern (name)
  (gethash name *caller-pattern-table*))
(defmacro define-caller-pattern (name pattern &optional caller-type)
  "Defines NAME as a function/macro call with argument structure
 described by PATTERN. CALLER-TYPE, if specified, assigns a type to
 the pattern, which may be used to exclude references to NAME while
 viewing the database. For example, all the Common Lisp definitions
 have a caller-type of :lisp or :lisp2, so that you can exclude 
 references to common lisp functions from the calling tree."
  `(progn
     (when ',caller-type
       (setf (pattern-caller-type ',name) ',caller-type))
     (when ',pattern 
       (setf (gethash ',name *caller-pattern-table*)
         ',pattern))))

;;; For defining variables
(defmacro define-variable-pattern (name &optional caller-type)
  "Defines NAME as a variable reference of type CALLER-TYPE. This is
 mainly used to establish the caller-type of the variable."
  `(progn
     (when ',caller-type
       (setf (pattern-caller-type ',name) ',caller-type))))

;;; For defining synonyms. Means much less space taken up by the patterns.
(defmacro define-caller-pattern-synonyms (source destinations)
  "For defining function caller pattern syntax synonyms. For each name
 in DESTINATIONS, defines its pattern as a copy of the definition of SOURCE.
 Allows a large number of identical patterns to be defined simultaneously.
 Must occur after the SOURCE has been defined."
  `(let ((source-type (pattern-caller-type ',source))
         (source-pattern (gethash ',source *caller-pattern-table*)))
     (when source-type
       (dolist (dest ',destinations)
         (setf (pattern-caller-type dest) source-type)))
     (when source-pattern
       (dolist (dest ',destinations)
         (setf (gethash dest *caller-pattern-table*)
           source-pattern)))))

(defun clear-patterns ()
  (clrhash *pattern-substitution-table*)
  (clrhash *caller-pattern-table*)
  (clrhash *pattern-caller-type*))

;;; ********************************
;;; Cross Reference Files **********
;;; ********************************
(defvar *last-form* ()
  "The last form read from the file. Useful for figuring out what went wrong
 when sublisp-cl-file drops into the debugger.")

(defvar *sublisp-cl-verbose* t
  "When T, sublisp-cl-file(s) prints out the names of the files it looks at,
 progress dots, and the number of forms read.")

;;; This needs to first clear the tables?
(defun sublisp-cl-files (&rest files)
  "Grovels over the lisp code located in source file FILES, using sublisp-cl-file."
  ;; If the arg is a list, use it.
  (when (listp (car files)) (setq files (car files)))
  (dolist (file files)
    (sublisp-cl-file file nil))
  (values))

(defvar *handle-package-forms* nil  ;'(lisp::in-package)
  "When non-NIL, and SUBLISP-CL-FILE sees a package-setting form like IN-PACKAGE,
 sets the current package to the specified package by evaluating the
 form. When done with the file, sublisp-cl-file resets the package to its 
 original value. In some of the displaying functions, when this variable
 is non-NIL one may specify that all symbols from a particular set of
 packages be ignored. This is only useful if the files use different
 packages with conflicting names.")

(defvar *normal-readtable* (copy-readtable nil) "Normal, unadulterated CL readtable.")


(defun sublisp-cl-file (filename &optional (clear-tables t) (verbose *sublisp-cl-verbose*)(clean-src-file-1st nil))
  "Cross references the function and variable calls in FILENAME by
 walking over the source code located in the file. Defaults type of
 filename to \".lisp\". Chomps on the code using record-callers and
 record-callers*. If CLEAR-TABLES is T (the default), it clears the callers
 database before processing the file. Specify CLEAR-TABLES as nil to
 append to the database. If VERBOSE is T (the default), prints out the
 name of the file, one progress dot for each form processed, and the
 total number of forms."
  
  ;; Default type to "lisp"
  (when (and (null (pathname-type filename))
             (not  (probe-file filename)))
    (cond ((stringp filename)
           (setf filename (concatenate 'string filename ".lisp")))
          ((pathnamep filename)
           (setf filename (merge-pathnames filename
                                           (make-pathname :type "lisp"))))))
  
  (when clear-tables (clear-tables))
  
  (let ((count 0)
        (old-package *package*)
        ;;(newfile (if clean-src-file-1st (clean-src-file filename) filename))
        (*readtable* *normal-readtable*))
    (when verbose (format t "~&Cross-referencing file ~A.~&" filename))
    
    (with-open-file (stream filename :direction :input)
      (do ((form (read stream nil :eof) (read stream nil :eof)))
          ((eq form :eof))
        (incf count)
        (when verbose
          (format *standard-output* ".")
          (force-output *standard-output*))
        (setq *last-form* form)
        (record-callers filename form)
        ;; Package Magic.
        (when (and *handle-package-forms*
                   (consp form)
                   (member (car form) *handle-package-forms*))
          (eval form))))
    (when verbose 
      (format t "~&~D forms processed." count))
    (setq *package* old-package)
    (values)))

(defvar *handle-function-forms* t
  "When T, SUBLISP-CL-FILE tries to be smart about forms which occur in
 a function position, such as lambdas and arbitrary Lisp forms.
 If so, it recursively calls record-callers with pattern 'FORM.
 If the form is a lambda, makes the caller a caller of :unnamed-lambda.") 

(defvar *handle-macro-forms* t
  "When T, if the file was loaded before being processed by SUBLISP-CL, and the
 car of a form is a macro, it notes that the parent calls the macro,
 and then calls macroexpand-1 on the form.") 

(defvar *callees-database-includes-variables* nil)

(defun record-callers (filename form 
                                &optional pattern parent (environment nil)
                                funcall)
  "RECORD-CALLERS is the main routine used to walk down the code. It matches
 the PATTERN against the FORM, possibly adding statements to the database.
 PARENT is the name defined by the current outermost definition; it is
 the caller of the forms in the body (e.g., FORM). ENVIRONMENT is used
 to keep track of the scoping of variables. FUNCALL deals with the type
 of variable assignment and hence how the environment should be modified.
 RECORD-CALLERS handles atomic patterns and simple list-structure patterns.
 For complex list-structure pattern destructuring, it calls RECORD-CALLERS*."
  ;  (when form)
  (unless pattern (setq pattern 'FORM))
  (cond ((symbolp pattern)
         (case pattern
           (:IGNORE
            ;; Ignores the rest of the form.
            (values t parent environment))
           (NAME  
            ;; This is the name of a new definition.
            (push filename (source-file form))
            (values t form environment))
           ((FUNCTION MACRO)
            ;; This is the name of a call.
            (cond ((and *handle-function-forms* (consp form))
                   ;; If we're a cons and special handling is on,
                   (when (eq (car form) 'lambda)
                     (pushnew filename (callers-list :unnamed-lambda :file))
                     (when parent
                       (pushnew parent (callers-list :unnamed-lambda
                                                     :callers))
                       (pushnew :unnamed-lambda (callers-list parent
                                                              :callees))))
                   (record-callers filename form 'form parent environment))
                  (t 
                   ;; If we're just a regular function name call.
                   (pushnew filename (callers-list form :file))
                   (when parent
                     (pushnew parent (callers-list form :callers))
                     (pushnew form (callers-list parent :callees)))
                   (values t parent environment)))) 
           (VAR   
            ;; This is the name of a new variable definition.
            ;; Includes arglist parameters.
            (when (and (symbolp form) (not (keywordp form))
                       (not (member form lambda-list-keywords)))
              (pushnew form (car environment))
              (pushnew filename (callers-list form :file))
              (when parent 
                ;   (pushnew form (callers-list parent :callees))
                (pushnew parent (callers-list form :setters)))
              (values t parent environment)))
           (VARIABLE
            ;; VAR reference
            (pushnew filename (callers-list form :file))
            (when (and parent (not (lookup form environment)))
              (pushnew parent (callers-list form :readers))
              (when *callees-database-includes-variables*
                (pushnew form (callers-list parent :callees))))
            (values t parent environment))
           (FORM  
            ;; A random form (var or funcall).
            (cond ((consp form)
                   ;; Get new pattern from TAG.
                   (let ((new-pattern (lookup-caller-pattern (car form))))
                     (pushnew filename (callers-list (car form) :file))
                     (when parent
                       (pushnew parent (callers-list (car form) :callers))
                       (pushnew (car form) (callers-list parent :callees)))
                     (cond ((and new-pattern (cdr form))
                            ;; Special Pattern and there's stuff left
                            ;; to be processed. Note that we check if
                            ;; a pattern is defined for the form before
                            ;; we check to see if we can macroexpand it.
                            (record-callers filename (cdr form) new-pattern
                                            parent environment :funcall))
                           ((and *handle-macro-forms*
                                 (symbolp (car form)) ; pnorvig 9/9/93
                                 (macro-function (car form)))
                            ;; The car of the form is a macro and
                            ;; macro processing is turned on. Macroexpand-1
                            ;; the form and try again.
                            (record-callers filename 
                                            (macroexpand-1 form)
                                            'form parent environment 
                                            :funcall))
                           ((null (cdr form))
                            ;; No more left to be processed. Note that
                            ;; this must occur after the macros clause,
                            ;; since macros can expand into more code.
                            (values t parent environment))
                           (t
                            ;; Random Form. We assume it is a function call.
                            (record-callers filename (cdr form)
                                            '((:star FORM))
                                            parent environment :funcall)))))
                  (t 
                   (when (and (not (lookup form environment))
                              (not (numberp form))
                              ;; the following line should probably be 
                              ;; commented out?
                              (not (keywordp form))
                              (not (stringp form))
                              (not (eq form t))
                              (not (eq form nil)))
                     (pushnew filename (callers-list form :file))
                     ;; ??? :callers
                     (when parent
                       (pushnew parent (callers-list form :readers))
                       (when *callees-database-includes-variables*
                         (pushnew form (callers-list parent :callees)))))
                   (values t parent environment))))
           (otherwise 
            ;; Pattern Substitution
            (let ((new-pattern (lookup-pattern-substitution pattern)))
              (if new-pattern
                  (record-callers filename form new-pattern 
                                  parent environment)
                (when (eq pattern form)
                  (values t parent environment)))))))
        ((consp pattern)
         (case (car pattern)
           (:eq  (when (eq (second pattern) form)
                   (values t parent environment)))
           (:test  (when (funcall (eval (second pattern)) form)
                     (values t parent environment)))
           (:typep (when (typep form (second pattern))
                     (values t parent environment)))
           (:or  (dolist (subpat (rest pattern))
                   (multiple-value-bind (processed parent environment)
                       (record-callers filename form subpat
                                       parent environment)
                     (when processed
                       (return (values processed parent environment))))))
           (:rest     ; (:star :plus :optional :rest)
            (record-callers filename form (second pattern)
                            parent environment))
           (otherwise
            (multiple-value-bind (d p env)
                (record-callers* filename form pattern 
                                 parent (cons nil environment))
              (values d p (if funcall environment env))))))))

(defun record-callers* (filename form pattern parent environment
                                 &optional continuation 
                                 in-optionals in-keywords)
  "RECORD-CALLERS* handles complex list-structure patterns, such as
 ordered lists of subpatterns, patterns involving :star, :plus,
 &optional, &key, &rest, and so on. CONTINUATION is a stack of
 unprocessed patterns, IN-OPTIONALS and IN-KEYWORDS are corresponding
 stacks which determine whether &rest or &key has been seen yet in
 the current pattern." 
  ;; form must be a cons or nil.
  ;  (when form)
  (if (null pattern)
      (if (null continuation)
          (values t parent environment)
        (record-callers* filename form (car continuation) parent environment
                         (cdr continuation) 
                         (cdr in-optionals)
                         (cdr in-keywords)))
    (let ((pattern-elt (car pattern)))
      (cond ((car-eq pattern-elt :optional)
             (if (null form) 
                 (values t parent environment)
               (multiple-value-bind (form processed par env)
                   (record-callers* filename form (cdr pattern-elt)
                                    parent environment
                                    (cons (cdr pattern) continuation)
                                    (cons (car in-optionals) in-optionals)
                                    (cons (car in-keywords) in-keywords))
                 (if processed
                     (values processed par env)
                   (record-callers* filename form (cdr pattern)
                                    parent environment continuation
                                    in-optionals in-keywords)))))
            ((car-eq pattern-elt :star)
             (if (null form)
                 (values t parent environment)
               (multiple-value-bind (form processed par env)
                   (record-callers* filename form (cdr pattern-elt)
                                    parent environment
                                    (cons pattern continuation)
                                    (cons (car in-optionals) in-optionals)
                                    (cons (car in-keywords) in-keywords))
                 (if processed
                     (values processed par env)
                   (record-callers* filename form (cdr pattern)
                                    parent environment continuation
                                    in-optionals in-keywords)))))
            ((car-eq pattern-elt :plus)
             (record-callers* filename form (cdr pattern-elt)
                              parent environment
                              (cons (cons (cons :star (cdr pattern-elt))
                                          (cdr pattern))
                                    continuation)
                              (cons (car in-optionals) in-optionals)
                              (cons (car in-keywords) in-keywords)))
            ((car-eq pattern-elt :rest)
             (record-callers filename form pattern-elt parent environment))
            ((eq pattern-elt '&optional)
             (record-callers* filename form (cdr pattern)
                              parent environment continuation
                              (cons t in-optionals)
                              (cons (car in-keywords) in-keywords)))
            ((eq pattern-elt '&rest)
             (record-callers filename form (second pattern)
                             parent environment))
            ((eq pattern-elt '&key)
             (record-callers* filename form (cdr pattern)
                              parent environment continuation
                              (cons (car in-optionals) in-optionals)
                              (cons t in-keywords)))
            ((null form)
             (when (or (car in-keywords) (car in-optionals))
               (values t parent environment)))
            ((consp form)
             (multiple-value-bind (processed parent environment)
                 (record-callers filename (if (car in-keywords)
                                              (cadr form)
                                            (car form))
                                 pattern-elt
                                 parent environment)
               (cond (processed
                      (record-callers* filename (if (car in-keywords)
                                                    (cddr form)
                                                  (cdr form))
                                       (cdr pattern)
                                       parent environment
                                       continuation
                                       in-optionals in-keywords))
                     ((or (car in-keywords)
                          (car in-optionals))
                      (values t parent environment)))))))))


;;; ********************************
;;; Misc Utilities *****************
;;; ********************************
(defvar *types-to-ignore*
    '(:lisp     ; CLtL 1st Edition
      :lisp2      ; CLtL 2nd Edition additional patterns
      :sublisp      ; Cyccorp's SubL
      )
  "Default set of caller types (as specified in the patterns) to ignore
 in the database handling functions. :lisp is CLtL 1st edition,
 :lisp2 is additional patterns from CLtL 2nd edition.")

(defun display-database (&optional (database :callers) 
                                   (types-to-ignore *types-to-ignore*))
  "Prints out the name of each symbol and all its callers. Specify database
 :callers (the default) to get function call references, :fill to the get
 files in which the symbol is called, :readers to get variable references,
 and :setters to get variable binding and assignments. Ignores functions
 of types listed in types-to-ignore."
  (maphash #'(lambda (name callers)
               (unless (or (member (pattern-caller-type name)
                                   types-to-ignore)
                           ;; When we're doing fancy package crap,
                           ;; allow us to ignore symbols based on their
                           ;; packages.
                           (when *handle-package-forms*
                             (member (symbol-package name)
                                     types-to-ignore
                                     :key #'find-package)))
                 (format t "~&~S is referenced by~{ ~S~}."
                   name callers)))
           (ecase database
             (:file  *file-callers-database*)
             (:callers *callers-database*)
             (:readers *readers-database*)
             (:setters *setters-database*))))

(defun write-callers-database-to-file (filename)
  "Saves the contents of the current callers database to a file. This
 file can be loaded to restore the previous contents of the
 database. (For large systems it can take a long time to crunch
 through the code, so this can save some time.)"
  (with-open-file (stream filename :direction :output)
    (format stream "~&(clear-tables)")
    (maphash #'(lambda (x y) 
                 (format stream "~&(setf (source-file '~S) '~S)"
                   x y))
             *source-file*)
    (maphash #'(lambda (x y) 
                 (format stream "~&(setf (callers-list '~S :file) '~S)"
                   x y))
             *file-callers-database*)
    (maphash #'(lambda (x y) 
                 (format stream "~&(setf (callers-list '~S :callers) '~S)"
                   x y))
             *callers-database*)
    (maphash #'(lambda (x y) 
                 (format stream "~&(setf (callers-list '~S :callees) '~S)"
                   x y))
             *callees-database*)
    (maphash #'(lambda (x y) 
                 (format stream "~&(setf (callers-list '~S :readers) '~S)"
                   x y))
             *readers-database*)
    (maphash #'(lambda (x y) 
                 (format stream "~&(setf (callers-list '~S :setters) '~S)"
                   x y))
             *setters-database*)))


;;; ********************************
;;; Print Caller Trees *************
;;; ********************************
;;; The following function is useful for reversing a caller table into
;;; a callee table. Possibly later we'll extend sublisp-cl to create two 
;;; such database hash tables. Needs to include vars as well.
(defun invert-hash-table (table &optional (types-to-ignore *types-to-ignore*))
  "Makes a copy of the hash table in which (name value*) pairs
 are inverted to (value name*) pairs."
  (let ((target (make-hash-table :test #'equal)))
    (maphash #'(lambda (key values)
                 (dolist (value values)
                   (unless (member (pattern-caller-type key) 
                                   types-to-ignore)
                     (pushnew key (gethash value target)))))
             table)
    target))

;;; Resolve file references for automatic creation of a defsystem file.
(defun determine-file-dependencies (&optional (database *callers-database*))
  "Makes a hash table of file dependencies for the references listed in
 DATABASE. This function may be useful for automatically resolving
 file references for automatic creation of a system definition (defsystem)."
  (let ((file-ref-ht  (make-hash-table :test #'equal)))
    (maphash #'(lambda (key values)
                 (let ((key-file (source-file key)))
                   (when key
                     (dolist (value values)
                       (let ((value-file (source-file value)))
                         (when value-file
                           (dolist (s key-file)
                             (dolist (d value-file)
                               (pushnew d (gethash s file-ref-ht))))))))))
             database)
    file-ref-ht))

(defun print-file-dependencies (&optional (database *callers-database*))
  "Prints a list of file dependencies for the references listed in DATABASE.
 This function may be useful for automatically computing file loading
 constraints for a system definition tool."
  (maphash #'(lambda (key value) (format t "~&~S --> ~S" key value))
           (determine-file-dependencies database)))

;;; The following functions demonstrate a possible way to interface
;;; sublisp-cl to a graphical browser such as psgraph to mimic the capabilities
;;; of Masterscope's graphical browser. 

(defvar *last-caller-tree* nil)

(defvar *default-graphing-mode* :call-graph
  "Specifies whether we graph up or down. If :call-graph, the children
 of a node are the functions it calls. If :caller-graph, the children
 of a node are the functions that call it.") 

(defun gather-tree (parents &optional already-seen 
                            (mode *default-graphing-mode*)
                            (types-to-ignore *types-to-ignore*) compact)
  "Extends the tree, copying it into list structure, until it repeats
 a reference (hits a cycle)."
  (let ((*already-seen* nil)
        (database (case mode
                    (:call-graph *callees-database*)
                    (:caller-graph *callers-database*))))
    (declare (special *already-seen*))
    (labels 
        ((amass-tree
          (parents &optional already-seen)
          (let (result this-item)
            (dolist (parent parents)
              (unless (member (pattern-caller-type parent)
                              types-to-ignore)
                (pushnew parent *already-seen*)
                (if (member parent already-seen)
                    (setq this-item nil) ; :ignore
                  (if compact 
                      (multiple-value-setq (this-item already-seen)
                        (amass-tree (gethash parent database)
                                    (cons parent already-seen)))
                    (setq this-item
                          (amass-tree (gethash parent database)
                                      (cons parent already-seen)))))
                (setq parent (format nil "~S" parent))
                (when (consp parent) (setq parent (cons :sublisp-cl-list parent)))
                (unless (eq this-item :ignore)
                  (push (if this-item
                            (list parent this-item)
                          parent) 
                        result))))
            (values result  ;(reverse result)
                    already-seen))))
      (values (amass-tree parents already-seen)
              *already-seen*))))

(defun find-roots-and-cycles (&optional (mode *default-graphing-mode*)
                                        (types-to-ignore *types-to-ignore*))
  "Returns a list of uncalled callers (roots) and called callers (potential
 cycles)."
  (let ((uncalled-callers nil)
        (called-callers nil)
        (database (ecase mode
                    (:call-graph *callers-database*)
                    (:caller-graph *callees-database*)))
        (other-database (ecase mode
                          (:call-graph *callees-database*)
                          (:caller-graph *callers-database*))))
    (maphash #'(lambda (name value)
                 (declare (ignore value))
                 (unless (member (pattern-caller-type name) 
                                 types-to-ignore)
                   (if (gethash name database)
                       (push name called-callers)
                     (push name uncalled-callers))))
             other-database)
    (values uncalled-callers called-callers)))

(defun make-caller-tree (&optional (mode *default-graphing-mode*)
                                   (types-to-ignore *types-to-ignore*) compact)
  "Outputs list structure of a tree which roughly represents the possibly
 cyclical structure of the caller database.
 If mode is :call-graph, the children of a node are the functions it calls.
 If mode is :caller-graph, the children of a node are the functions that
 call it.
 If compact is T, tries to eliminate the already-seen nodes, so that
 the graph for a node is printed at most once. Otherwise it will duplicate
 the node's tree (except for cycles). This is usefull because the call tree
 is actually a directed graph, so we can either duplicate references or
 display only the first one."
  ;; Would be nice to print out line numbers and whenever we skip a duplicated
  ;; reference, print the line number of the full reference after the node.
  (multiple-value-bind (uncalled-callers called-callers)
      (find-roots-and-cycles mode types-to-ignore)
    (multiple-value-bind (trees already-seen)
        (gather-tree uncalled-callers nil mode types-to-ignore compact)
      (setq *last-caller-tree* trees)
      (let ((more-trees (gather-tree (set-difference called-callers
                                                     already-seen)
                                     already-seen 
                                     mode types-to-ignore compact)))
        (values trees more-trees)))))

(defvar *indent-amount* 3
  "Number of spaces to indent successive levels in PRINT-INDENTED-TREE.")

(defun print-indented-tree (trees &optional (indent 0))
  "Simple code to print out a list-structure tree (such as those created
 by make-caller-tree) as indented text."
  (when trees
    (dolist (tree trees)
      (cond ((and (listp tree) (eq (car tree) :sublisp-cl-list))
             (format t "~&~VT~A" indent (cdr tree)))
            ((listp tree)
             (format t "~&~VT~A" indent (car tree))
             (print-indented-tree (cadr tree) (+ indent *indent-amount*)))
            (t
             (format t "~&~VT~A" indent tree))))))

(defun print-caller-trees (&key (mode *default-graphing-mode*)
                                (types-to-ignore *types-to-ignore*)
                                compact
                                root-nodes)
  "Prints the calling trees (which may actually be a full graph and not
 necessarily a DAG) as indented text trees using PRINT-INDENTED-TREE.
 MODE is :call-graph for trees where the children of a node are the
 functions called by the node, or :caller-graph for trees where the
 children of a node are the functions the node calls. TYPES-TO-IGNORE
 is a list of funcall types (as specified in the patterns) to ignore
 in printing out the database. For example, '(:lisp) would ignore all
 calls to common lisp functions. COMPACT is a flag to tell the program
 to try to compact the trees a bit by not printing trees if they have
 already been seen. ROOT-NODES is a list of root nodes of trees to 
 display. If ROOT-NODES is nil, tries to find all root nodes in the
 database."
  (multiple-value-bind (rooted cycles)
      (if root-nodes
          (values (gather-tree root-nodes nil mode types-to-ignore compact))
        (make-caller-tree mode types-to-ignore compact))
    (when rooted
      (format t "~&Rooted calling trees:")
      (print-indented-tree rooted 2))
    (when cycles
      (when rooted  
        (format t "~2%"))
      (format t "~&Cyclic calling trees:")
      (print-indented-tree cycles 2))))


;;; ********************************
;;; Interface to PSGraph ***********
;;; ********************************
#|
;;; Interface to Bates' PostScript Graphing Utility
(load "/afs/cs/user/mkant/Lisp/PSGraph/psgraph")

(defparameter *postscript-output-directory* "")
(defun psgraph-sublisp-cl (&key (mode *default-graphing-mode*)
                                (output-directory *postscript-output-directory*)
                                (types-to-ignore *types-to-ignore*)
                                (compact t)
                                (shrink t)
                                root-nodes
                                insert)
  ;; If root-nodes is a non-nil list, uses that list as the starting
  ;; position. Otherwise tries to find all roots in the database.
  (multiple-value-bind (rooted cycles)
      (if root-nodes
          (values (gather-tree root-nodes nil mode types-to-ignore compact))
        (make-caller-tree mode types-to-ignore compact))
    (psgraph-output (append rooted cycles) output-directory shrink insert)))

(defun psgraph-output (list-of-trees directory shrink &optional insert)
  (let ((psgraph:*fontsize* 9)
        (psgraph:*second-fontsize* 7)
        ; (psgraph:*boxkind* "fill")
        (psgraph:*boxgray* "0")   ; .8
        (psgraph:*edgewidth* "1")
        (psgraph:*edgegray* "0"))
    (labels ((stringify (thing)
                        (cond ((stringp thing) (string-downcase thing))
                              ((symbolp thing) (string-downcase (symbol-name thing)))
                              ((and (listp thing) (eq (car thing) :sublisp-cl-list))
                               (stringify (cdr thing)))
                              ((listp thing) (stringify (car thing)))
                              (t (string thing)))))
      (dolist (item list-of-trees)
        (let* ((fname (stringify item))
               (filename (concatenate 'string directory
                           (string-trim '(#\: #\|) fname)
                           ".ps")))
          (format t "~&Creating PostScript file ~S." filename)
          (with-open-file (*standard-output* filename
                                             :direction :output
                                             :if-does-not-exist :create
                                             :if-exists :supersede)
            ;; Note that the #'eq prints the DAG as a tree. If
            ;; you replace it with #'equal, it will print it as
            ;; a DAG, which I think is slightly ugly.
            (psgraph:psgraph item
                             #'caller-tree-children #'caller-info shrink
                             insert #'eq)))))))

(defun caller-tree-children (tree)
  (when (and tree (listp tree) (not (eq (car tree) :sublisp-cl-list)))
    (cadr tree)))

(defun caller-tree-node (tree)
  (when tree
    (cond ((and (listp tree) (eq (car tree) :sublisp-cl-list))
           (cdr tree))
          ((listp tree)
           (car tree))
          (t
           tree))))

(defun caller-info (tree)
  (let ((node (caller-tree-node tree)))
    (list node)))
|#
#|
;;; Code to print out graphical trees of CLOS class hierarchies.
(defun print-class-hierarchy (&optional (start-class 'anything) 
                                        (file "classes.ps"))
  (let ((start (find-class start-class)))
    (when start
      (with-open-file (*standard-output* file :direction :output)
        (psgraph:psgraph start 
                         #'clos::class-direct-subclasses
                         #'(lambda (x) 
                             (list (format nil "~A" (clos::class-name x))))
                         t nil #'eq)))))

|#

 
;;; ****************************************************************
;;; Cross Referencing Patterns for Common Lisp *********************
;;; ****************************************************************
(clear-patterns)

;;; ********************************
;;; Pattern Substitutions **********
;;; ********************************
(define-pattern-substitution integer (:test #'integerp))
(define-pattern-substitution rational (:test #'rationalp))
(define-pattern-substitution symbol  (:test #'symbolp))
(define-pattern-substitution string  (:test #'stringp))
(define-pattern-substitution number  (:test #'numberp))
(define-pattern-substitution lambda-list
    ((:star var)
     (:optional (:eq &optional)
                (:star (:or var
                            (var (:optional form (:optional var))))))
     (:optional (:eq &rest) var)
     (:optional (:eq &key) (:star (:or var
                                       ((:or var
                                             (keyword var))
                                        (:optional form (:optional var)))))
                (:optional &allow-other-keys))
     (:optional (:eq &aux)
                (:star (:or var
                            (var (:optional form)))))))
(define-pattern-substitution test form)
(define-pattern-substitution body ((:star (:or declaration documentation-string)) pbody))
(define-pattern-substitution documentation-string string)
(define-pattern-substitution initial-value form)
(define-pattern-substitution tag symbol)
(define-pattern-substitution declaration ((:eq declare)(:rest :ignore)))
(define-pattern-substitution destination form)
(define-pattern-substitution control-string string)
(define-pattern-substitution format-arguments 
    (pbody))
(define-pattern-substitution fn
    (:or ((:eq quote) function) 
         ((:eq function) function)
         function))

;;; ********************************
;;; Caller Patterns ****************
;;; ********************************

;;; Types Related
(define-caller-pattern coerce (form :ignore) :lisp)
(define-caller-pattern type-of (form) :lisp)
(define-caller-pattern upgraded-array-element-type (:ignore) :lisp2)
(define-caller-pattern upgraded-complex-part-type (:ignore) :lisp2)

;;; Lambdas and Definitions
(define-variable-pattern lambda-list-keywords :lisp)
(define-variable-pattern lambda-parameters-limit :lisp)
(define-caller-pattern lambda (lambda-list pbody) :lisp)

(define-caller-pattern defun (name lambda-list (:star (:or documentation-string declaration)) pbody) :lisp)

;;; perhaps this should use VAR, instead of NAME
(define-caller-pattern defvar  (var (:optional initial-value (:optional documentation-string))) :lisp)
(define-caller-pattern defparameter (var initial-value (:optional documentation-string)) :lisp)
(define-caller-pattern defconstant (var initial-value (:optional documentation-string)) :lisp)

(define-caller-pattern eval-when (:ignore  ; the situations
                        pbody) :lisp)

;;; Logical Values
(define-variable-pattern nil :lisp)
(define-variable-pattern t :lisp)

;;; Predicates
(define-caller-pattern typep (form form) :lisp)
(define-caller-pattern subtypep (form form) :lisp)

(define-caller-pattern null (form) :lisp)
(define-caller-pattern symbolp (form) :lisp)
(define-caller-pattern atom (form) :lisp)
(define-caller-pattern consp (form) :lisp)
(define-caller-pattern listp (form) :lisp)
(define-caller-pattern numberp (form) :lisp)
(define-caller-pattern integerp (form) :lisp)
(define-caller-pattern rationalp (form) :lisp)
(define-caller-pattern floatp (form) :lisp)
(define-caller-pattern realp (form) :lisp2)
(define-caller-pattern complexp (form) :lisp)
(define-caller-pattern characterp (form) :lisp)
(define-caller-pattern stringp (form) :lisp)
(define-caller-pattern bit-vector-p (form) :lisp)
(define-caller-pattern vectorp (form) :lisp)
(define-caller-pattern simple-vector-p (form) :lisp)
(define-caller-pattern simple-string-p (form) :lisp)
(define-caller-pattern simple-bit-vector-p (form) :lisp)
(define-caller-pattern arrayp (form) :lisp)
(define-caller-pattern packagep (form) :lisp)
(define-caller-pattern functionp (form) :lisp)
(define-caller-pattern compiled-function-p (form) :lisp)
(define-caller-pattern commonp (form) :lisp)

;;; Equality Predicates
(define-caller-pattern eq (form form) :lisp)
(define-caller-pattern eql (form form) :lisp)
(define-caller-pattern equal (form form) :lisp)
(define-caller-pattern equalp (form form) :lisp)

;;; Logical Operators
(define-caller-pattern not (form) :lisp)
(define-caller-pattern or (pbody) :lisp)
(define-caller-pattern and (pbody) :lisp)

;;; Reference

;;; Quote is a problem. In Defmacro & friends, we'd like to actually
;;; look at the argument, 'cause it hides internal function calls
;;; of the defmacro. 
(define-caller-pattern quote (:ignore) :lisp)

(define-caller-pattern function ((:or fn form)) :lisp)
(define-caller-pattern symbol-value (form) :lisp)
(define-caller-pattern symbol-function (form) :lisp)
(define-caller-pattern fdefinition (form) :lisp2)
(define-caller-pattern boundp (form) :lisp)
(define-caller-pattern fboundp (form) :lisp)
(define-caller-pattern special-form-p (form) :lisp)

;;; Assignment
(define-caller-pattern setq ((:star var form)) :lisp)
(define-caller-pattern psetq ((:star var form)) :lisp)
(define-caller-pattern set (form form) :lisp)
(define-caller-pattern makunbound (form) :lisp)
(define-caller-pattern fmakunbound (form) :lisp)

;;; Generalized Variables
(define-caller-pattern setf ((:star form form)) :lisp)
(define-caller-pattern psetf ((:star form form)) :lisp)
(define-caller-pattern shiftf ((:plus form) form) :lisp)
(define-caller-pattern rotatef (pbody) :lisp)
(define-caller-pattern define-modify-macro 
    (name
     lambda-list
     fn
     (:optional documentation-string))
  :lisp)
(define-caller-pattern defsetf 
    (:or (name name (:optional documentation-string))
         (name lambda-list (var)
               (:star (:or declaration documentation-string))
               pbody))
  :lisp)
(define-caller-pattern define-setf-method
    (name lambda-list
          (:star (:or declaration documentation-string))
          pbody)
  :lisp)
(define-caller-pattern get-setf-method (form) :lisp)
(define-caller-pattern get-setf-method-multiple-value (form) :lisp)


;;; Function invocation
(define-caller-pattern apply (fn form pbody) :lisp)
(define-caller-pattern funcall (fn pbody) :lisp)


;;; Simple sequencing
(define-caller-pattern progn (pbody) :lisp)
(define-caller-pattern prog1 (form pbody) :lisp)
(define-caller-pattern prog2 (form form pbody) :lisp)

;;; Variable bindings
(define-caller-pattern let
    (((:star (:or var (var &optional form))))
     (:star declaration)
     pbody)
  :lisp)
(define-caller-pattern let*
    (((:star (:or var (var &optional form))))
     (:star declaration)
     pbody)
  :lisp)
(define-caller-pattern compiler-let
    (((:star (:or var (var form))))
     pbody)
  :lisp)
(define-caller-pattern progv (form form pbody) :lisp)
(define-caller-pattern flet (((:star 
    (name lambda-list (:star (:or declaration documentation-string)) pbody)))
     pbody)
  :lisp)
(define-caller-pattern labels
    (((:star (name lambda-list 
                   (:star (:or declaration
                               documentation-string))
                   pbody)))
     pbody)
  :lisp)
(define-caller-pattern macrolet
    (((:star (name lambda-list 
                   (:star (:or declaration
                               documentation-string))
                   pbody)))
     pbody)
  :lisp)
(define-caller-pattern symbol-macrolet
    (((:star (var form))) (:star declaration) pbody)
  :lisp2)

;;; Conditionals
(define-caller-pattern if (test form (:optional form)) :lisp)
(define-caller-pattern when (test pbody) :lisp)
(define-caller-pattern unless (test pbody) :lisp)
(define-caller-pattern cond ((:star (test pbody))) :lisp)
(define-caller-pattern case
    (form
     (:star ((:or symbol
                  ((:star symbol)))
             pbody))) 
  :lisp)
(define-caller-pattern typecase (form (:star (symbol pbody))) 
  :lisp)

;;; Blocks and Exits
(define-caller-pattern block (name pbody) :lisp)
(define-caller-pattern return-from (function (:optional form)) :lisp)
(define-caller-pattern return ((:optional form)) :lisp)

;;; Iteration
(define-caller-pattern loop (pbody) :lisp)
(define-caller-pattern do
    (((:star (:or var
                  (var (:optional form (:optional form)))))) ; init step
     (form pbody)    ; end-test result
     (:star declaration)
     (:star (:or tag form)))    ; statement
  :lisp)
(define-caller-pattern do*
    (((:star (:or var
                  (var (:optional form (:optional form)))))) 
     (form pbody)
     (:star declaration)
     (:star (:or tag form)))
  :lisp)
(define-caller-pattern dolist
    ((var form (:optional form))
     (:star declaration)
     (:star (:or tag form)))
  :lisp)
(define-caller-pattern dotimes
    ((var form (:optional form))
     (:star declaration)
     (:star (:or tag form)))
  :lisp)

;;; Mapping
(define-caller-pattern mapcar (fn form pbody) :lisp)
(define-caller-pattern maplist (fn form pbody) :lisp)
(define-caller-pattern mapc (fn form pbody) :lisp)
(define-caller-pattern mapl (fn form pbody) :lisp)
(define-caller-pattern mapcan (fn form pbody) :lisp)
(define-caller-pattern mapcon (fn form pbody) :lisp)

;;; The "Program Feature"
(define-caller-pattern tagbody ((:star (:or tag form))) :lisp)
(define-caller-pattern prog
    (((:star (:or var (var (:optional form)))))
     (:star declaration)
     (:star (:or tag form)))
  :lisp)
(define-caller-pattern prog*  
    (((:star (:or var (var (:optional form)))))
     (:star declaration)
     (:star (:or tag form)))
  :lisp)
(define-caller-pattern go (tag) :lisp)

;;; Multiple Values
(define-caller-pattern values (pbody) :lisp)
(define-variable-pattern multiple-values-limit :lisp)
(define-caller-pattern values-list (form) :lisp)
(define-caller-pattern multiple-value-list (form) :lisp)
(define-caller-pattern multiple-value-call (fn pbody) :lisp)
(define-caller-pattern multiple-value-prog1 (form pbody) :lisp)
(define-caller-pattern multiple-value-bind
    (((:star var)) form
     (:star declaration)
     pbody)
  :lisp)
(define-caller-pattern multiple-value-setq (((:star var)) form) :lisp)
(define-caller-pattern nth-value (form form) :lisp2)

;;; Dynamic Non-Local Exits
(define-caller-pattern catch (tag pbody) :lisp)
(define-caller-pattern throw (tag form) :lisp)
(define-caller-pattern unwind-protect (form pbody) :lisp)

;;; Macros
(define-caller-pattern macro-function (form) :lisp)
(define-caller-pattern defmacro
    (name
     lambda-list
     (:star (:or declaration documentation-string))
     pbody)
  :lisp)
(define-caller-pattern macroexpand (form (:optional :ignore)) :lisp)
(define-caller-pattern macroexpand-1 (form (:optional :ignore)) :lisp)
(define-variable-pattern *macroexpand-hook* :lisp)

;;; Destructuring
(define-caller-pattern destructuring-bind 
    (lambda-list form
                 (:star declaration)
                 pbody)
  :lisp2)

;;; Compiler Macros
(define-caller-pattern define-compiler-macro
    (name lambda-list
          (:star (:or declaration documentation-string))
          pbody)
  :lisp2)
(define-caller-pattern compiler-macro-function (form) :lisp2)
(define-caller-pattern compiler-macroexpand (form (:optional :ignore)) :lisp2)
(define-caller-pattern compiler-macroexpand-1 (form (:optional :ignore)) :lisp2)

;;; Environments
(define-caller-pattern variable-information (form &optional :ignore) 
  :lisp2)
(define-caller-pattern function-information (fn &optional :ignore) :lisp2)
(define-caller-pattern declaration-information (form &optional :ignore) :lisp2)
(define-caller-pattern augment-environment (form &key (:star :ignore)) :lisp2)
(define-caller-pattern define-declaration 
    (name
     lambda-list
     pbody) 
  :lisp2)
(define-caller-pattern parse-macro (name lambda-list form) :lisp2)
(define-caller-pattern enclose (form &optional :ignore) :lisp2)


;;; Declarations
(define-caller-pattern declare ((:rest :ignore)) :lisp)
(define-caller-pattern proclaim ((:rest :ignore)) :lisp)
(define-caller-pattern locally ((:star declaration) pbody) :lisp)
(define-caller-pattern declaim ((:rest :ignore)) :lisp2)
(define-caller-pattern the (form form) :lisp)

;;; Symbols
(define-caller-pattern get (form form (:optional form)) :lisp)
(define-caller-pattern remprop (form form) :lisp)
(define-caller-pattern symbol-plist (form) :lisp)
(define-caller-pattern getf (form form (:optional form)) :lisp)
(define-caller-pattern remf (form form) :lisp)
(define-caller-pattern get-properties (form form) :lisp)

(define-caller-pattern symbol-name (form) :lisp)
(define-caller-pattern make-symbol (form) :lisp)
(define-caller-pattern copy-symbol (form (:optional :ignore)) :lisp)
(define-caller-pattern gensym ((:optional :ignore)) :lisp)
(define-variable-pattern *gensym-counter* :lisp2)
(define-caller-pattern gentemp ((:optional :ignore :ignore)) :lisp)
(define-caller-pattern symbol-package (form) :lisp)
(define-caller-pattern keywordp (form) :lisp)

;;; Packages
(define-variable-pattern *package* :lisp)
(define-caller-pattern make-package ((:rest :ignore)) :lisp)
(define-caller-pattern in-package ((:rest :ignore)) :lisp)
(define-caller-pattern find-package ((:rest :ignore)) :lisp)
(define-caller-pattern package-name ((:rest :ignore)) :lisp)
(define-caller-pattern package-nicknames ((:rest :ignore)) :lisp)
(define-caller-pattern rename-package ((:rest :ignore)) :lisp)
(define-caller-pattern package-use-list ((:rest :ignore)) :lisp)
(define-caller-pattern package-used-by-list ((:rest :ignore)) :lisp)
(define-caller-pattern package-shadowing-symbols ((:rest :ignore)) :lisp)
(define-caller-pattern list-all-packages () :lisp)
(define-caller-pattern delete-package ((:rest :ignore)) :lisp2)
(define-caller-pattern intern (form &optional :ignore) :lisp)
(define-caller-pattern find-symbol (form &optional :ignore) :lisp)
(define-caller-pattern unintern (form &optional :ignore) :lisp)

(define-caller-pattern export ((:or symbol ((:star symbol)))
                               &optional :ignore) :lisp)
(define-caller-pattern unexport ((:or symbol ((:star symbol)))
                                 &optional :ignore) :lisp)
(define-caller-pattern import ((:or symbol ((:star symbol)))
                               &optional :ignore) :lisp)
(define-caller-pattern shadowing-import ((:or symbol ((:star symbol)))
                                         &optional :ignore) :lisp)
(define-caller-pattern shadow ((:or symbol ((:star symbol)))
                               &optional :ignore) :lisp)

(define-caller-pattern use-package ((:rest :ignore)) :lisp)
(define-caller-pattern unuse-package ((:rest :ignore)) :lisp)
(define-caller-pattern defpackage (name (:rest :ignore)) :lisp2)
(define-caller-pattern find-all-symbols (form) :lisp)
(define-caller-pattern do-symbols 
    ((var (:optional form (:optional form)))
     (:star declaration) 
     (:star (:or tag form))) 
  :lisp)
(define-caller-pattern do-external-symbols 
    ((var (:optional form (:optional form)))
     (:star declaration) 
     (:star (:or tag form))) 
  :lisp)
(define-caller-pattern do-all-symbols 
    ((var (:optional form))
     (:star declaration) 
     (:star (:or tag form))) 
  :lisp)
(define-caller-pattern with-package-iterator
    ((name form (:plus :ignore))
     pbody)
  :lisp2)

;;; Modules
(define-variable-pattern *modules* :lisp)
(define-caller-pattern provide (form) :lisp)
(define-caller-pattern require (form &optional :ignore) :lisp)


;;; Numbers
(define-caller-pattern zerop (form) :lisp)
(define-caller-pattern plusp (form) :lisp)
(define-caller-pattern minusp (form) :lisp)
(define-caller-pattern oddp (form) :lisp)
(define-caller-pattern evenp (form) :lisp)

(define-caller-pattern = (form pbody) :lisp)
(define-caller-pattern /= (form pbody) :lisp)
(define-caller-pattern > (form pbody) :lisp)
(define-caller-pattern < (form pbody) :lisp)
(define-caller-pattern <= (form pbody) :lisp)
(define-caller-pattern >= (form pbody) :lisp)

(define-caller-pattern max (form pbody) :lisp)
(define-caller-pattern min (form pbody) :lisp)

(define-caller-pattern - (form pbody) :lisp)
(define-caller-pattern + (form pbody) :lisp)
(define-caller-pattern * (form pbody) :lisp)
(define-caller-pattern / (form pbody) :lisp)
(define-caller-pattern 1+ (form) :lisp)
(define-caller-pattern 1- (form) :lisp)

(define-caller-pattern incf (form form) :lisp)
(define-caller-pattern decf (form form) :lisp)

(define-caller-pattern conjugate (form) :lisp)

(define-caller-pattern gcd (pbody) :lisp)
(define-caller-pattern lcm (pbody) :lisp)

(define-caller-pattern exp (form) :lisp)
(define-caller-pattern expt (form form) :lisp)
(define-caller-pattern log (form (:optional form)) :lisp)
(define-caller-pattern sqrt (form) :lisp)
(define-caller-pattern isqrt (form) :lisp)

(define-caller-pattern abs (form) :lisp)
(define-caller-pattern phase (form) :lisp)
(define-caller-pattern signum (form) :lisp)
(define-caller-pattern sin (form) :lisp)
(define-caller-pattern cos (form) :lisp)
(define-caller-pattern tan (form) :lisp)
(define-caller-pattern cis (form) :lisp)
(define-caller-pattern asin (form) :lisp)
(define-caller-pattern acos (form) :lisp)
(define-caller-pattern atan (form &optional form) :lisp)
(define-variable-pattern pi :lisp)

(define-caller-pattern sinh (form) :lisp)
(define-caller-pattern cosh (form) :lisp)
(define-caller-pattern tanh (form) :lisp)
(define-caller-pattern asinh (form) :lisp)
(define-caller-pattern acosh (form) :lisp)
(define-caller-pattern atanh (form) :lisp)

;;; Type Conversions and Extractions
(define-caller-pattern float (form (:optional form)) :lisp)
(define-caller-pattern rational (form) :lisp)
(define-caller-pattern rationalize (form) :lisp)
(define-caller-pattern numerator (form) :lisp)
(define-caller-pattern denominator (form) :lisp)

(define-caller-pattern floor (form (:optional form)) :lisp)
(define-caller-pattern ceiling (form (:optional form)) :lisp)
(define-caller-pattern truncate (form (:optional form)) :lisp)
(define-caller-pattern round (form (:optional form)) :lisp)

(define-caller-pattern mod (form form) :lisp)
(define-caller-pattern rem (form form) :lisp)

(define-caller-pattern ffloor (form (:optional form)) :lisp)
(define-caller-pattern fceiling (form (:optional form)) :lisp)
(define-caller-pattern ftruncate (form (:optional form)) :lisp)
(define-caller-pattern fround (form (:optional form)) :lisp)

(define-caller-pattern decode-float (form) :lisp)
(define-caller-pattern scale-float (form form) :lisp)
(define-caller-pattern float-radix (form) :lisp)
(define-caller-pattern float-sign (form (:optional form)) :lisp)
(define-caller-pattern float-digits (form) :lisp)
(define-caller-pattern float-precision (form) :lisp)
(define-caller-pattern integer-decode-float (form) :lisp)

(define-caller-pattern complex (form (:optional form)) :lisp)
(define-caller-pattern realpart (form) :lisp)
(define-caller-pattern imagpart (form) :lisp)

(define-caller-pattern logior (pbody) :lisp)
(define-caller-pattern logxor (pbody) :lisp)
(define-caller-pattern logand (pbody) :lisp)
(define-caller-pattern logeqv (pbody) :lisp)

(define-caller-pattern lognand (form form) :lisp)
(define-caller-pattern lognor (form form) :lisp)
(define-caller-pattern logandc1 (form form) :lisp)
(define-caller-pattern logandc2 (form form) :lisp)
(define-caller-pattern logorc1 (form form) :lisp)
(define-caller-pattern logorc2 (form form) :lisp)

(define-caller-pattern boole (form form form) :lisp)
(define-variable-pattern boole-clr :lisp)
(define-variable-pattern boole-set :lisp)
(define-variable-pattern boole-1 :lisp)
(define-variable-pattern boole-2 :lisp)
(define-variable-pattern boole-c1 :lisp)
(define-variable-pattern boole-c2 :lisp)
(define-variable-pattern boole-and :lisp)
(define-variable-pattern boole-ior :lisp)
(define-variable-pattern boole-xor :lisp)
(define-variable-pattern boole-eqv :lisp)
(define-variable-pattern boole-nand :lisp)
(define-variable-pattern boole-nor :lisp)
(define-variable-pattern boole-andc1 :lisp)
(define-variable-pattern boole-andc2 :lisp)
(define-variable-pattern boole-orc1 :lisp)
(define-variable-pattern boole-orc2 :lisp)

(define-caller-pattern lognot (form) :lisp)
(define-caller-pattern logtest (form form) :lisp)
(define-caller-pattern logbitp (form form) :lisp)
(define-caller-pattern ash (form form) :lisp)
(define-caller-pattern logcount (form) :lisp)
(define-caller-pattern integer-length (form) :lisp)

(define-caller-pattern byte (form form) :lisp)
(define-caller-pattern byte-size (form) :lisp)
(define-caller-pattern byte-position (form) :lisp)
(define-caller-pattern ldb (form form) :lisp)
(define-caller-pattern ldb-test (form form) :lisp)
(define-caller-pattern mask-field (form form) :lisp)
(define-caller-pattern dpb (form form form) :lisp)
(define-caller-pattern deposit-field (form form form) :lisp)

;;; Random Numbers
(define-caller-pattern random (form (:optional form)) :lisp)
(define-variable-pattern *random-state* :lisp)
(define-caller-pattern make-random-state ((:optional form)) :lisp)
(define-caller-pattern random-state-p (form) :lisp)

;;; Implementation Parameters
(define-variable-pattern most-positive-fixnum :lisp)
(define-variable-pattern most-negative-fixnum :lisp)
(define-variable-pattern most-positive-short-float :lisp)
(define-variable-pattern least-positive-short-float :lisp)
(define-variable-pattern least-negative-short-float :lisp)
(define-variable-pattern most-negative-short-float :lisp)
(define-variable-pattern most-positive-single-float :lisp)
(define-variable-pattern least-positive-single-float :lisp)
(define-variable-pattern least-negative-single-float :lisp)
(define-variable-pattern most-negative-single-float :lisp)
(define-variable-pattern most-positive-double-float :lisp)
(define-variable-pattern least-positive-double-float :lisp)
(define-variable-pattern least-negative-double-float :lisp)
(define-variable-pattern most-negative-double-float :lisp)
(define-variable-pattern most-positive-long-float :lisp)
(define-variable-pattern least-positive-long-float :lisp)
(define-variable-pattern least-negative-long-float :lisp)
(define-variable-pattern most-negative-long-float :lisp)
(define-variable-pattern least-positive-normalized-short-float :lisp2)
(define-variable-pattern least-negative-normalized-short-float :lisp2)
(define-variable-pattern least-positive-normalized-single-float :lisp2)
(define-variable-pattern least-negative-normalized-single-float :lisp2)
(define-variable-pattern least-positive-normalized-double-float :lisp2)
(define-variable-pattern least-negative-normalized-double-float :lisp2)
(define-variable-pattern least-positive-normalized-long-float :lisp2)
(define-variable-pattern least-negative-normalized-long-float :lisp2)
(define-variable-pattern short-float-epsilon :lisp)
(define-variable-pattern single-float-epsilon :lisp)
(define-variable-pattern double-float-epsilon :lisp)
(define-variable-pattern long-float-epsilon :lisp)
(define-variable-pattern short-float-negative-epsilon :lisp)
(define-variable-pattern single-float-negative-epsilon :lisp)
(define-variable-pattern double-float-negative-epsilon :lisp)
(define-variable-pattern long-float-negative-epsilon :lisp)

;;; Characters 
(define-variable-pattern char-code-limit :lisp)
(define-variable-pattern char-font-limit :lisp)
(define-variable-pattern char-bits-limit :lisp)
(define-caller-pattern standard-char-p (form) :lisp)
(define-caller-pattern graphic-char-p (form) :lisp)
(define-caller-pattern string-char-p (form) :lisp)
(define-caller-pattern alpha-char-p (form) :lisp)
(define-caller-pattern upper-case-p (form) :lisp)
(define-caller-pattern lower-case-p (form) :lisp)
(define-caller-pattern both-case-p (form) :lisp)
(define-caller-pattern digit-char-p (form (:optional form)) :lisp)
(define-caller-pattern alphanumericp (form) :lisp)

(define-caller-pattern char= (pbody) :lisp)
(define-caller-pattern char/= (pbody) :lisp)
(define-caller-pattern char< (pbody) :lisp)
(define-caller-pattern char> (pbody) :lisp)
(define-caller-pattern char<= (pbody) :lisp)
(define-caller-pattern char>= (pbody) :lisp)

(define-caller-pattern char-equal (pbody) :lisp)
(define-caller-pattern char-not-equal (pbody) :lisp)
(define-caller-pattern char-lessp (pbody) :lisp)
(define-caller-pattern char-greaterp (pbody) :lisp)
(define-caller-pattern char-not-greaterp (pbody) :lisp)
(define-caller-pattern char-not-lessp (pbody) :lisp)

(define-caller-pattern char-code (form) :lisp)
(define-caller-pattern char-bits (form) :lisp)
(define-caller-pattern char-font (form) :lisp)
(define-caller-pattern code-char (form (:optional form form)) :lisp)
(define-caller-pattern make-char (form (:optional form form)) :lisp)
(define-caller-pattern characterp (form) :lisp)
(define-caller-pattern char-upcase (form) :lisp)
(define-caller-pattern char-downcase (form) :lisp)
(define-caller-pattern digit-char (form (:optional form form)) :lisp)
(define-caller-pattern char-int (form) :lisp)
(define-caller-pattern int-char (form) :lisp)
(define-caller-pattern char-name (form) :lisp)
(define-caller-pattern name-char (form) :lisp)
(define-variable-pattern char-control-bit :lisp)
(define-variable-pattern char-meta-bit :lisp)
(define-variable-pattern char-super-bit :lisp)
(define-variable-pattern char-hyper-bit :lisp)
(define-caller-pattern char-bit (form form) :lisp)
(define-caller-pattern set-char-bit (form form form) :lisp)

;;; Sequences
(define-caller-pattern complement (fn) :lisp2)
(define-caller-pattern elt (form form) :lisp)
(define-caller-pattern subseq (form form &optional form) :lisp)
(define-caller-pattern copy-seq (form) :lisp)
(define-caller-pattern length (form) :lisp)
(define-caller-pattern reverse (form) :lisp)
(define-caller-pattern nreverse (form) :lisp)
(define-caller-pattern make-sequence (form form &key form) :lisp)

(define-caller-pattern concatenate (form pbody) :lisp)
(define-caller-pattern map (form fn form pbody) :lisp)
(define-caller-pattern map-into (form fn pbody) :lisp2)

(define-caller-pattern some (fn form pbody) :lisp)
(define-caller-pattern every (fn form pbody) :lisp)
(define-caller-pattern notany (fn form pbody) :lisp)
(define-caller-pattern notevery (fn form pbody) :lisp)

(define-caller-pattern reduce (fn form &key pbody) :lisp)
(define-caller-pattern fill (form form &key pbody) :lisp)
(define-caller-pattern replace (form form &key pbody) :lisp)
(define-caller-pattern remove (form form &key pbody) :lisp)
(define-caller-pattern remove-if (fn form &key pbody) :lisp)
(define-caller-pattern remove-if-not (fn form &key pbody) :lisp)
(define-caller-pattern delete (form form &key pbody) :lisp)
(define-caller-pattern delete-if (fn form &key pbody) :lisp)
(define-caller-pattern delete-if-not (fn form &key pbody) :lisp)
(define-caller-pattern remove-duplicates (form &key pbody) :lisp)
(define-caller-pattern delete-duplicates (form &key pbody) :lisp)
(define-caller-pattern substitute (form form form &key pbody) :lisp)
(define-caller-pattern substitute-if (form fn form &key pbody) :lisp)
(define-caller-pattern substitute-if-not (form fn form &key pbody) :lisp)
(define-caller-pattern nsubstitute (form form form &key pbody) :lisp)
(define-caller-pattern nsubstitute-if (form fn form &key pbody) :lisp)
(define-caller-pattern nsubstitute-if-not (form fn form &key pbody) :lisp)
(define-caller-pattern find (form form &key pbody) :lisp)
(define-caller-pattern find-if (fn form &key pbody) :lisp)
(define-caller-pattern find-if-not (fn form &key pbody) :lisp)
(define-caller-pattern position (form form &key pbody) :lisp)
(define-caller-pattern position-if (fn form &key pbody) :lisp)
(define-caller-pattern position-if-not (fn form &key pbody) :lisp)
(define-caller-pattern count (form form &key pbody) :lisp)
(define-caller-pattern count-if (fn form &key pbody) :lisp)
(define-caller-pattern count-if-not (fn form &key pbody) :lisp)
(define-caller-pattern mismatch (form form &key pbody) :lisp)
(define-caller-pattern search (form form &key pbody) :lisp)
(define-caller-pattern sort (form fn &key pbody) :lisp)
(define-caller-pattern stable-sort (form fn &key pbody) :lisp)
(define-caller-pattern merge (form form form fn &key pbody) :lisp)

;;; Lists
(define-caller-pattern car (form) :lisp)
(define-caller-pattern cdr (form) :lisp)
(define-caller-pattern caar (form) :lisp)
(define-caller-pattern cadr (form) :lisp)
(define-caller-pattern cdar (form) :lisp)
(define-caller-pattern cddr (form) :lisp)
(define-caller-pattern caaar (form) :lisp)
(define-caller-pattern caadr (form) :lisp)
(define-caller-pattern cadar (form) :lisp)
(define-caller-pattern caddr (form) :lisp)
(define-caller-pattern cdaar (form) :lisp)
(define-caller-pattern cdadr (form) :lisp)
(define-caller-pattern cddar (form) :lisp)
(define-caller-pattern cdddr (form) :lisp)
(define-caller-pattern caaaar (form) :lisp)
(define-caller-pattern caaadr (form) :lisp)
(define-caller-pattern caadar (form) :lisp)
(define-caller-pattern caaddr (form) :lisp)
(define-caller-pattern cadaar (form) :lisp)
(define-caller-pattern cadadr (form) :lisp)
(define-caller-pattern caddar (form) :lisp)
(define-caller-pattern cadddr (form) :lisp)
(define-caller-pattern cdaaar (form) :lisp)
(define-caller-pattern cdaadr (form) :lisp)
(define-caller-pattern cdadar (form) :lisp)
(define-caller-pattern cdaddr (form) :lisp)
(define-caller-pattern cddaar (form) :lisp)
(define-caller-pattern cddadr (form) :lisp)
(define-caller-pattern cdddar (form) :lisp)
(define-caller-pattern cddddr (form) :lisp)

(define-caller-pattern cons (form form) :lisp)
(define-caller-pattern tree-equal (form form &key (:star fn)) :lisp)
(define-caller-pattern endp (form) :lisp)
(define-caller-pattern list-length (form) :lisp)
(define-caller-pattern nth (form form) :lisp)

(define-caller-pattern first (form) :lisp)
(define-caller-pattern second (form) :lisp)
(define-caller-pattern third (form) :lisp)
(define-caller-pattern fourth (form) :lisp)
(define-caller-pattern fifth (form) :lisp)
(define-caller-pattern sixth (form) :lisp)
(define-caller-pattern seventh (form) :lisp)
(define-caller-pattern eighth (form) :lisp)
(define-caller-pattern ninth (form) :lisp)
(define-caller-pattern tenth (form) :lisp)

(define-caller-pattern rest (form) :lisp)
(define-caller-pattern nthcdr (form form) :lisp)
(define-caller-pattern last (form (:optional form)) :lisp)
(define-caller-pattern list (pbody) :lisp)
(define-caller-pattern list* (pbody) :lisp)
(define-caller-pattern make-list (form &key pbody) :lisp)
(define-caller-pattern append (pbody) :lisp)
(define-caller-pattern copy-list (form) :lisp)
(define-caller-pattern copy-alist (form) :lisp)
(define-caller-pattern copy-tree (form) :lisp)
(define-caller-pattern revappend (form form) :lisp)
(define-caller-pattern nconc (pbody) :lisp)
(define-caller-pattern nreconc (form form) :lisp)
(define-caller-pattern push (form form) :lisp)
(define-caller-pattern pushnew (form form &key pbody) :lisp)
(define-caller-pattern pop (form) :lisp)
(define-caller-pattern butlast (form (:optional form)) :lisp)
(define-caller-pattern nbutlast (form (:optional form)) :lisp)
(define-caller-pattern ldiff (form form) :lisp)
(define-caller-pattern rplaca (form form) :lisp)
(define-caller-pattern rplacd (form form) :lisp)

(define-caller-pattern subst (form form form &key pbody) :lisp)
(define-caller-pattern subst-if (form fn form &key pbody) :lisp)
(define-caller-pattern subst-if-not (form fn form &key pbody) :lisp)
(define-caller-pattern nsubst (form form form &key pbody) :lisp)
(define-caller-pattern nsubst-if (form fn form &key pbody) :lisp)
(define-caller-pattern nsubst-if-not (form fn form &key pbody) :lisp)
(define-caller-pattern sublis (form form &key pbody) :lisp)
(define-caller-pattern nsublis (form form &key pbody) :lisp)
(define-caller-pattern member (form form &key pbody) :lisp)
(define-caller-pattern member-if (fn form &key pbody) :lisp)
(define-caller-pattern member-if-not (fn form &key pbody) :lisp)

(define-caller-pattern tailp (form form) :lisp)
(define-caller-pattern adjoin (form form &key pbody) :lisp)
(define-caller-pattern union (form form &key pbody) :lisp)
(define-caller-pattern nunion (form form &key pbody) :lisp)
(define-caller-pattern intersection (form form &key pbody) :lisp)
(define-caller-pattern nintersection (form form &key pbody) :lisp)
(define-caller-pattern set-difference (form form &key pbody) :lisp)
(define-caller-pattern nset-difference (form form &key pbody) :lisp)
(define-caller-pattern set-exclusive-or (form form &key pbody) :lisp)
(define-caller-pattern nset-exclusive-or (form form &key pbody) :lisp)
(define-caller-pattern subsetp (form form &key pbody) :lisp)

(define-caller-pattern acons (form form form) :lisp)
(define-caller-pattern pairlis (form form (:optional form)) :lisp)
(define-caller-pattern assoc (form form &key pbody) :lisp)
(define-caller-pattern assoc-if (fn form) :lisp)
(define-caller-pattern assoc-if-not (fn form) :lisp)
(define-caller-pattern rassoc (form form &key pbody) :lisp)
(define-caller-pattern rassoc-if (fn form &key pbody) :lisp)
(define-caller-pattern rassoc-if-not (fn form &key pbody) :lisp)

;;; Hash Tables
(define-caller-pattern make-hash-table (&key pbody) :lisp)
(define-caller-pattern hash-table-p (form) :lisp)
(define-caller-pattern gethash (form form (:optional form)) :lisp)
(define-caller-pattern remhash (form form) :lisp)
(define-caller-pattern maphash (fn form) :lisp)
(define-caller-pattern clrhash (form) :lisp)
(define-caller-pattern hash-table-count (form) :lisp)
(define-caller-pattern with-hash-table-iterator
    ((name form) pbody) :lisp2)
(define-caller-pattern hash-table-rehash-size (form) :lisp2)
(define-caller-pattern hash-table-rehash-threshold (form) :lisp2)
(define-caller-pattern hash-table-size (form) :lisp2)
(define-caller-pattern hash-table-test (form) :lisp2)
(define-caller-pattern sxhash (form) :lisp)

;;; Arrays
(define-caller-pattern make-array (form &key pbody) :lisp)
(define-variable-pattern array-rank-limit :lisp)
(define-variable-pattern array-dimension-limit :lisp)
(define-variable-pattern array-total-size-limit :lisp)
(define-caller-pattern vector (pbody) :lisp)
(define-caller-pattern aref (form pbody) :lisp)
(define-caller-pattern svref (form form) :lisp)
(define-caller-pattern array-element-type (form) :lisp)
(define-caller-pattern array-rank (form) :lisp)
(define-caller-pattern array-dimension (form form) :lisp)
(define-caller-pattern array-dimensions (form) :lisp)
(define-caller-pattern array-total-size (form) :lisp)
(define-caller-pattern array-in-bounds-p (form pbody) :lisp)
(define-caller-pattern array-row-major-index (form pbody) :lisp)
(define-caller-pattern row-major-aref (form form) :lisp2)
(define-caller-pattern adjustable-array-p (form) :lisp)

(define-caller-pattern bit (form pbody) :lisp)
(define-caller-pattern sbit (form pbody) :lisp)

(define-caller-pattern bit-and (form form (:optional form)) :lisp)
(define-caller-pattern bit-ior (form form (:optional form)) :lisp)
(define-caller-pattern bit-xor (form form (:optional form)) :lisp)
(define-caller-pattern bit-eqv (form form (:optional form)) :lisp)
(define-caller-pattern bit-nand (form form (:optional form)) :lisp)
(define-caller-pattern bit-nor (form form (:optional form)) :lisp)
(define-caller-pattern bit-andc1 (form form (:optional form)) :lisp)
(define-caller-pattern bit-andc2 (form form (:optional form)) :lisp)
(define-caller-pattern bit-orc1 (form form (:optional form)) :lisp)
(define-caller-pattern bit-orc2 (form form (:optional form)) :lisp)
(define-caller-pattern bit-not (form (:optional form)) :lisp)

(define-caller-pattern array-has-fill-pointer-p (form) :lisp)
(define-caller-pattern fill-pointer (form) :lisp)
(define-caller-pattern vector-push (form form) :lisp)
(define-caller-pattern vector-push-extend (form form (:optional form)) :lisp)
(define-caller-pattern vector-pop (form) :lisp)
(define-caller-pattern adjust-array (form form &key pbody) :lisp)

;;; Strings
(define-caller-pattern char (form form) :lisp)
(define-caller-pattern schar (form form) :lisp)
(define-caller-pattern string= (form form &key pbody) :lisp)
(define-caller-pattern string-equal (form form &key pbody) :lisp)
(define-caller-pattern string< (form form &key pbody) :lisp)
(define-caller-pattern string> (form form &key pbody) :lisp)
(define-caller-pattern string<= (form form &key pbody) :lisp)
(define-caller-pattern string>= (form form &key pbody) :lisp)
(define-caller-pattern string/= (form form &key pbody) :lisp)
(define-caller-pattern string-lessp (form form &key pbody) :lisp)
(define-caller-pattern string-greaterp (form form &key pbody) :lisp)
(define-caller-pattern string-not-greaterp (form form &key pbody) :lisp)
(define-caller-pattern string-not-lessp (form form &key pbody) :lisp)
(define-caller-pattern string-not-equal (form form &key pbody) :lisp)

(define-caller-pattern make-string (form &key pbody) :lisp)
(define-caller-pattern string-trim (form form) :lisp)
(define-caller-pattern string-left-trim (form form) :lisp)
(define-caller-pattern string-right-trim (form form) :lisp)
(define-caller-pattern string-upcase (form &key pbody) :lisp)
(define-caller-pattern string-downcase (form &key pbody) :lisp)
(define-caller-pattern string-capitalize (form &key pbody) :lisp)
(define-caller-pattern nstring-upcase (form &key pbody) :lisp)
(define-caller-pattern nstring-downcase (form &key pbody) :lisp)
(define-caller-pattern nstring-capitalize (form &key pbody) :lisp)
(define-caller-pattern string (form) :lisp)

;;; Structures
(define-caller-pattern defstruct 
    ((:or name (name (:rest :ignore)))
     (:optional documentation-string)
     (:plus :ignore))
  :lisp)

;;; The Evaluator
(define-caller-pattern eval (form) :lisp)
(define-variable-pattern *evalhook* :lisp)
(define-variable-pattern *applyhook* :lisp)
(define-caller-pattern evalhook (form fn fn &optional :ignore) :lisp)
(define-caller-pattern applyhook (fn form fn fn &optional :ignore) :lisp)
(define-caller-pattern constantp (form) :lisp)

;;; Streams
(define-variable-pattern *standard-input* :lisp)
(define-variable-pattern *standard-output* :lisp)
(define-variable-pattern *error-output* :lisp)
(define-variable-pattern *query-io* :lisp)
(define-variable-pattern *debug-io* :lisp)
(define-variable-pattern *terminal-io* :lisp)
(define-variable-pattern *trace-output* :lisp)
(define-caller-pattern make-synonym-stream (symbol) :lisp)
(define-caller-pattern make-broadcast-stream (pbody) :lisp)
(define-caller-pattern make-concatenated-stream (pbody) :lisp)
(define-caller-pattern make-two-way-stream (form form) :lisp)
(define-caller-pattern make-echo-stream (form form) :lisp)
(define-caller-pattern make-string-input-stream (form &optional form form) :lisp) 
(define-caller-pattern make-string-output-stream (&key pbody) :lisp)
(define-caller-pattern get-output-stream-string (form) :lisp)

(define-caller-pattern with-open-stream
    ((var form)
     (:star declaration)
     pbody)
  :lisp)

(define-caller-pattern with-input-from-string
    ((var form &key pbody)
     (:star declaration)
     pbody)
  :lisp)

(define-caller-pattern with-output-to-string
    ((var (:optional form))
     (:star declaration)
     pbody)
  :lisp)
(define-caller-pattern streamp (form) :lisp)
(define-caller-pattern open-stream-p (form) :lisp2)
(define-caller-pattern input-stream-p (form) :lisp)
(define-caller-pattern output-stream-p (form) :lisp)
(define-caller-pattern stream-element-type (form) :lisp)
(define-caller-pattern close (form (:rest :ignore)) :lisp)
(define-caller-pattern broadcast-stream-streams (form) :lisp2)
(define-caller-pattern concatenated-stream-streams (form) :lisp2)
(define-caller-pattern echo-stream-input-stream (form) :lisp2)
(define-caller-pattern echo-stream-output-stream (form) :lisp2)
(define-caller-pattern synonym-stream-symbol (form) :lisp2)
(define-caller-pattern two-way-stream-input-stream (form) :lisp2)
(define-caller-pattern two-way-stream-output-stream (form) :lisp2)
(define-caller-pattern interactive-stream-p (form) :lisp2)
(define-caller-pattern stream-external-format (form) :lisp2)

;;; Reader
(define-variable-pattern *read-base* :lisp)
(define-variable-pattern *read-suppress* :lisp)
(define-variable-pattern *read-eval* :lisp2)
(define-variable-pattern *readtable* :lisp)
(define-caller-pattern copy-readtable (&optional form form) :lisp)
(define-caller-pattern readtablep (form) :lisp)
(define-caller-pattern set-syntax-from-char (form form &optional form form) :lisp)
(define-caller-pattern set-macro-character (form fn &optional form) :lisp)
(define-caller-pattern get-macro-character (form (:optional form)) :lisp)
(define-caller-pattern make-dispatch-macro-character (form &optional form form)
  :lisp)
(define-caller-pattern set-dispatch-macro-character
    (form form fn (:optional form)) :lisp)
(define-caller-pattern get-dispatch-macro-character
    (form form (:optional form)) :lisp)
(define-caller-pattern readtable-case (form) :lisp2)
(define-variable-pattern *print-readably* :lisp2)
(define-variable-pattern *print-escape* :lisp)
(define-variable-pattern *print-pretty* :lisp)
(define-variable-pattern *print-circle* :lisp)
(define-variable-pattern *print-base* :lisp)
(define-variable-pattern *print-radix* :lisp)
(define-variable-pattern *print-case* :lisp)
(define-variable-pattern *print-gensym* :lisp)
(define-variable-pattern *print-level* :lisp)
(define-variable-pattern *print-length* :lisp)
(define-variable-pattern *print-array* :lisp)
(define-caller-pattern with-standard-io-syntax 
    ((:star declaration)
     pbody)
  :lisp2)

(define-caller-pattern read (&optional form form form form) :lisp)
(define-variable-pattern *read-default-float-format* :lisp)
(define-caller-pattern read-preserving-whitespace
    (&optional form form form form) :lisp)
(define-caller-pattern read-delimited-list (form &optional form form) :lisp)
(define-caller-pattern read-line (&optional form form form form) :lisp)
(define-caller-pattern read-char (&optional form form form form) :lisp)
(define-caller-pattern unread-char (form (:optional form)) :lisp)
(define-caller-pattern peek-char (&optional form form form form) :lisp)
(define-caller-pattern listen ((:optional form)) :lisp)
(define-caller-pattern read-char-no-hang (pbody) :lisp)
(define-caller-pattern clear-input ((:optional form)) :lisp)
(define-caller-pattern read-from-string (form pbody) :lisp)
(define-caller-pattern parse-integer (form &rest :ignore) :lisp)
(define-caller-pattern read-byte (pbody) :lisp)

(define-caller-pattern write (form &key pbody) :lisp)
(define-caller-pattern prin1 (form (:optional form)) :lisp)
(define-caller-pattern print (form (:optional form)) :lisp)
(define-caller-pattern pprint (form (:optional form)) :lisp)
(define-caller-pattern princ (form (:optional form)) :lisp)
(define-caller-pattern write-to-string (form &key pbody) :lisp)
(define-caller-pattern prin1-to-string (form) :lisp)
(define-caller-pattern princ-to-string (form) :lisp)
(define-caller-pattern write-char (form (:optional form)) :lisp)
(define-caller-pattern write-string (form &optional form &key pbody) :lisp)
(define-caller-pattern write-line (form &optional form &key pbody) :lisp)
(define-caller-pattern terpri ((:optional form)) :lisp)
(define-caller-pattern fresh-line ((:optional form)) :lisp)
(define-caller-pattern finish-output ((:optional form)) :lisp)
(define-caller-pattern force-output ((:optional form)) :lisp)
(define-caller-pattern clear-output ((:optional form)) :lisp)
(define-caller-pattern print-unreadable-object 
    ((form form &key pbody)
     (:star declaration)
     pbody)
  :lisp2)
(define-caller-pattern write-byte (form form) :lisp)
(define-caller-pattern format
    (destination
     control-string
     (:rest format-arguments))
  :lisp)

(define-caller-pattern y-or-n-p (control-string pbody) :lisp)
(define-caller-pattern yes-or-no-p (control-string pbody) :lisp)

;;; Pathnames
(define-caller-pattern wild-pathname-p (form &optional form) :lisp2)
(define-caller-pattern pathname-match-p (form form) :lisp2)
(define-caller-pattern translate-pathname (form form form &key pbody)
  :lisp2)

(define-caller-pattern logical-pathname (form) :lisp2)
(define-caller-pattern translate-logical-pathname (form &key pbody) :lisp2)
(define-caller-pattern logical-pathname-translations (form) :lisp2)
(define-caller-pattern load-logical-pathname-translations (form) :lisp2)
(define-caller-pattern compile-file-pathname (form &key form) :lisp2)

(define-caller-pattern pathname (form) :lisp)
(define-caller-pattern truename (form) :lisp)
(define-caller-pattern parse-namestring (pbody) :lisp)
(define-caller-pattern merge-pathnames (pbody) :lisp)
(define-variable-pattern *default-pathname-defaults* :lisp)
(define-caller-pattern make-pathname (pbody) :lisp)
(define-caller-pattern pathnamep (form) :lisp)
(define-caller-pattern pathname-host (form) :lisp)
(define-caller-pattern pathname-device (form) :lisp)
(define-caller-pattern pathname-directory (form) :lisp)
(define-caller-pattern pathname-name (form) :lisp)
(define-caller-pattern pathname-type (form) :lisp)
(define-caller-pattern pathname-version (form) :lisp)
(define-caller-pattern namestring (form) :lisp)
(define-caller-pattern file-namestring (form) :lisp)
(define-caller-pattern directory-namestring (form) :lisp)
(define-caller-pattern host-namestring (form) :lisp)
(define-caller-pattern enough-namestring (form (:optional form)) :lisp)
(define-caller-pattern user-homedir-pathname (&optional form) :lisp)
(define-caller-pattern open (form &key pbody) :lisp)
(define-caller-pattern with-open-file
    ((var form (:rest :ignore))
     (:star declaration)
     pbody)
  :lisp)

(define-caller-pattern rename-file (form form) :lisp)
(define-caller-pattern delete-file (form) :lisp)
(define-caller-pattern probe-file (form) :lisp)
(define-caller-pattern file-write-date (form) :lisp)
(define-caller-pattern file-author (form) :lisp)
(define-caller-pattern file-position (form (:optional form)) :lisp)
(define-caller-pattern file-length (form) :lisp)
(define-caller-pattern file-string-length (form form) :lisp2)
(define-caller-pattern load (form &key pbody) :lisp)
(define-variable-pattern *load-verbose* :lisp)
(define-variable-pattern *load-print* :lisp2)
(define-variable-pattern *load-pathname* :lisp2)
(define-variable-pattern *load-truename* :lisp2)
(define-caller-pattern make-load-form (form) :lisp2)
(define-caller-pattern make-load-form-saving-slots (form &optional form)
  :lisp2)
(define-caller-pattern directory (form &key pbody) :lisp)

;;; Errors
(define-caller-pattern error (form pbody) :lisp)
(define-caller-pattern cerror (form form pbody) :lisp)
(define-caller-pattern warn (form pbody) :lisp)
(define-variable-pattern *break-on-warnings* :lisp)
(define-caller-pattern break (&optional form pbody) :lisp)
(define-caller-pattern check-type (form form (:optional form)) :lisp)
(define-caller-pattern assert 
    (form
     (:optional ((:star var))
                (:optional form pbody))) 
  :lisp)
(define-caller-pattern etypecase (form (:star (symbol pbody))) :lisp)
(define-caller-pattern ctypecase (form (:star (symbol pbody))) :lisp)
(define-caller-pattern ecase
    (form
     (:star ((:or symbol ((:star symbol)))
             pbody)))
  :lisp)
(define-caller-pattern ccase 
    (form
     (:star ((:or symbol ((:star symbol)))
             pbody)))
  :lisp)

;;; The Compiler
(define-caller-pattern compile (form (:optional form)) :lisp)
(define-caller-pattern compile-file (form &key pbody) :lisp)
(define-variable-pattern *compile-verbose* :lisp2)
(define-variable-pattern *compile-print* :lisp2)
(define-variable-pattern *compile-file-pathname* :lisp2)
(define-variable-pattern *compile-file-truename* :lisp2)
(define-caller-pattern load-time-value (form (:optional form)) :lisp2)
(define-caller-pattern disassemble (form) :lisp)
(define-caller-pattern function-lambda-expression (fn) :lisp2)
(define-caller-pattern with-compilation-unit (((:star :ignore)) pbody) 
  :lisp2)

;;; Documentation
(define-caller-pattern documentation (form form) :lisp)
(define-caller-pattern trace (pbody) :lisp)
(define-caller-pattern untrace (pbody) :lisp)
(define-caller-pattern step (form) :lisp)
(define-caller-pattern time (form) :lisp)
(define-caller-pattern describe (form &optional form) :lisp)
(define-caller-pattern describe-object (form &optional form) :lisp2)
(define-caller-pattern inspect (form) :lisp)
(define-caller-pattern room ((:optional form)) :lisp)
(define-caller-pattern ed ((:optional form)) :lisp)
(define-caller-pattern dribble ((:optional form)) :lisp)
(define-caller-pattern apropos (form (:optional form)) :lisp)
(define-caller-pattern apropos-list (form (:optional form)) :lisp)
(define-caller-pattern get-decoded-time () :lisp)
(define-caller-pattern get-universal-time () :lisp)
(define-caller-pattern decode-universal-time (form &optional form) :lisp)
(define-caller-pattern encode-universal-time 
    (form form form form form form &optional form) :lisp)
(define-caller-pattern get-internal-run-time () :lisp)
(define-caller-pattern get-internal-real-time () :lisp)
(define-caller-pattern sleep (form) :lisp)

(define-caller-pattern lisp-implementation-type () :lisp)
(define-caller-pattern lisp-implementation-version () :lisp)
(define-caller-pattern machine-type () :lisp)
(define-caller-pattern machine-version () :lisp)
(define-caller-pattern machine-instance () :lisp)
(define-caller-pattern software-type () :lisp)
(define-caller-pattern software-version () :lisp)
(define-caller-pattern short-site-name () :lisp)
(define-caller-pattern long-site-name () :lisp)
(define-variable-pattern *features* :lisp)

(define-caller-pattern identity (form) :lisp)

;;; Pretty Printing
(define-variable-pattern *print-pprint-dispatch* :lisp2)
(define-variable-pattern *print-right-margin* :lisp2)
(define-variable-pattern *print-miser-width* :lisp2)
(define-variable-pattern *print-lines* :lisp2)
(define-caller-pattern pprint-newline (form &optional form) :lisp2)
(define-caller-pattern pprint-logical-block
    ((var form &key pbody)
     pbody)
  :lisp2)
(define-caller-pattern pprint-exit-if-list-exhausted () :lisp2)
(define-caller-pattern pprint-pop () :lisp2)
(define-caller-pattern pprint-indent (form form &optional form) :lisp2)
(define-caller-pattern pprint-tab (form form form &optional form) :lisp2)
(define-caller-pattern pprint-fill (form form &optional form form) :lisp2)
(define-caller-pattern pprint-linear (form form &optional form form) :lisp2)
(define-caller-pattern pprint-tabular (form form &optional form form form) :lisp2)
(define-caller-pattern formatter (control-string) :lisp2)
(define-caller-pattern copy-pprint-dispatch (&optional form) :lisp2)
(define-caller-pattern pprint-dispatch (form &optional form) :lisp2)
(define-caller-pattern set-pprint-dispatch (form form &optional form form)
  :lisp2)

;;; CLOS
(define-caller-pattern add-method (fn form) :lisp2)
(define-caller-pattern call-method (form form) :lisp2)
(define-caller-pattern call-next-method (pbody) :lisp2)
(define-caller-pattern change-class (form form) :lisp2)
(define-caller-pattern class-name (form) :lisp2)
(define-caller-pattern class-of (form) :lisp2)
(define-caller-pattern compute-applicable-methods (fn pbody) :lisp2)
(define-caller-pattern defclass (name &rest :ignore) :lisp2)
(define-caller-pattern defgeneric (name lambda-list &rest :ignore) :lisp2)
(define-caller-pattern define-method-combination 
      (name lambda-list ((:star :ignore)) (:optional ((:eq :arguments) :ignore)) (:optional ((:eq :generic-function) :ignore))
          (:star (:or declaration documentation-string))
          pbody)
  :lisp2)
(define-caller-pattern defmethod (name (:star symbol) lambda-list 
        (:star (:or declaration documentation-string))
          pbody)
  :lisp2)
(define-caller-pattern ensure-generic-function (name &key pbody) :lisp2)
(define-caller-pattern find-class (form &optional form form) :lisp2)
(define-caller-pattern find-method (fn &rest :ignore) :lisp2)
(define-caller-pattern function-keywords (&rest :ignore) :lisp2)
(define-caller-pattern generic-flet (((:star (name lambda-list))) pbody)
  :lisp2)
(define-caller-pattern generic-labels 
    (((:star (name lambda-list))) pbody)
  :lisp2)
(define-caller-pattern generic-function (lambda-list) :lisp2)
(define-caller-pattern initialize-instance (form &key pbody) :lisp2)
(define-caller-pattern invalid-method-error (fn form pbody) :lisp2)
(define-caller-pattern make-instance (fn pbody) :lisp2)
(define-caller-pattern make-instances-obsolete (fn) :lisp2)
(define-caller-pattern method-combination-error (form pbody) :lisp2)
(define-caller-pattern method-qualifiers (fn) :lisp2)
(define-caller-pattern next-method-p () :lisp2)
(define-caller-pattern no-applicable-method (fn pbody) :lisp2)
(define-caller-pattern no-next-method (fn pbody) :lisp2)
(define-caller-pattern print-object (form form) :lisp2)
(define-caller-pattern reinitialize-instance (form pbody) :lisp2)
(define-caller-pattern remove-method (fn form) :lisp2)
(define-caller-pattern shared-initialize (form form pbody) :lisp2)
(define-caller-pattern slot-boundp (form form) :lisp2)
(define-caller-pattern slot-exists-p (form form) :lisp2)
(define-caller-pattern slot-makeunbound (form form) :lisp2)
(define-caller-pattern slot-missing (fn form form form &optional form) :lisp2)
(define-caller-pattern slot-unbound (fn form form) :lisp2)
(define-caller-pattern slot-value (form form) :lisp2)
(define-caller-pattern update-instance-for-different-class 
    (form form pbody) :lisp2)
(define-caller-pattern update-instance-for-redefined-class 
    (form form pbody) :lisp2)
(define-caller-pattern with-accessors
    (((:star :ignore)) form
     (:star declaration)
     pbody)
  :lisp2)
(define-caller-pattern with-added-methods
    ((name lambda-list) form
     pbody)
  :lisp2)
(define-caller-pattern with-slots
    (((:star :ignore)) form
     (:star declaration)
     pbody)
  :lisp2)

;;; Conditions
(define-caller-pattern signal (form pbody) :lisp2)
(define-variable-pattern *break-on-signals* :lisp2)
(define-caller-pattern handler-case (form (:star (form ((:optional var))
                                                       pbody)))
  :lisp2)
(define-caller-pattern ignore-errors (pbody) :lisp2)
(define-caller-pattern handler-bind (((:star (form form)))
                                     pbody)
  :lisp2)
(define-caller-pattern define-condition (name &rest :ignore) :lisp2)
(define-caller-pattern make-condition (form &rest :ignore) :lisp2)
(define-caller-pattern with-simple-restart
    ((name form pbody) pbody) :lisp2)
(define-caller-pattern restart-case 
    (form
     (:star (form form pbody)))
  :lisp2)
(define-caller-pattern restart-bind
    (((:star (name fn &key pbody)))
     pbody)
  :lisp2)
(define-caller-pattern with-condition-restarts
    (form form
          (:star declaration)
          pbody)
  :lisp2)
(define-caller-pattern compute-restarts (&optional form) :lisp2)
(define-caller-pattern restart-name (form) :lisp2)
(define-caller-pattern find-restart (form &optional form) :lisp2)
(define-caller-pattern invoke-restart (form pbody) :lisp2)
(define-caller-pattern invoke-restart-interactively (form) :lisp2)
(define-caller-pattern abort (&optional form) :lisp2)
(define-caller-pattern continue (&optional form) :lisp2)
(define-caller-pattern muffle-warning (&optional form) :lisp2)
(define-caller-pattern store-value (form &optional form) :lisp2)
(define-caller-pattern use-value (form &optional form) :lisp2)
(define-caller-pattern invoke-debugger (form) :lisp2)
(define-variable-pattern *debugger-hook* :lisp2)
(define-caller-pattern simple-condition-format-string (form) :lisp2)
(define-caller-pattern simple-condition-format-arguments (form) :lisp2)
(define-caller-pattern type-error-datum (form) :lisp2)
(define-caller-pattern type-error-expected-type (form) :lisp2)
(define-caller-pattern package-error-package (form) :lisp2)
(define-caller-pattern stream-error-stream (form) :lisp2)
(define-caller-pattern file-error-pathname (form) :lisp2)
(define-caller-pattern cell-error-name (form) :lisp2)
(define-caller-pattern arithmetic-error-operation (form) :lisp2)
(define-caller-pattern arithmetic-error-operands (form) :lisp2)

;;; For ZetaLisp Flavors
(define-caller-pattern send (form fn pbody) :flavors)

;;; Thu Nov 15 21:50:05 1990 by Mark Kantrowitz <mkant@A.GP.CS.CMU.EDU>
;;; xref-patterns-for-macl.lisp
;;; ****************************************************************
;;; XREF Patterns for MACL ObjectLisp Functions ********************
;;; ****************************************************************
;;;
;;; Written by Rodney Daughtrey, BCS AI Center, Huntsville, AL  
;;; <rodney@huntsai.boeing.com>. Please also send bug reports to
;;; <mkant+@cs.cmu.edu>.
;;;
;;; This file contains XREF patterns defined for use with ObjectLisp
;;; functions in Macintosh Allegro Common Lisp (MACL). They have not
;;; been tested extensively, so there may be bugs. If you find any
;;; bugs, please send them to mkant+@cs.cmu.edu. 
;;;
;;; To Do:
;;;  define patterns for other MACL-specific functions

(define-pattern-substitution keyword-value-pairs
    (:star symbol form))

(define-caller-pattern defobject
    (symbol
     (:star symbol)) :macl)

(define-caller-pattern oneof (form keyword-value-pairs) :macl)
(define-caller-pattern kindof (:star symbol) :macl)
(define-caller-pattern remake-object (form (:optional form)) :macl)
(define-caller-pattern exist (keyword-value-pairs) :macl)

(define-caller-pattern init-list-default
    ((keyword-value-pairs)
     symbol form
     keyword-value-pairs) :macl)

(define-caller-pattern ask (:plus form) :macl)
(define-caller-pattern talkto (:optional form) :macl)

(define-caller-pattern defobfun
    ((name form)
     lambda-list
     (:star (:or documentation-string declaration))
     pbody)
  :macl)

(define-caller-pattern fset (form form) :macl)
(define-caller-pattern fset-globally (form form) :2macl)

(define-caller-pattern nfunction
    (symbol 
     ((:eq LAMBDA)
      ((:star symbol))
      pbody))
  :1macl)

(define-caller-pattern have (form form) :macl)
(define-caller-pattern fhave (form fn) :macl)
(define-caller-pattern makunbound-all (form) :macl)
(define-caller-pattern fmakunbound-all (form) :macl)
(define-caller-pattern ownp (form) :macl)
(define-caller-pattern fownp (form) :macl)
(define-caller-pattern bound-anywhere-p (form) :macl)
(define-caller-pattern fbound-anywhere-p (form) :macl)
(define-caller-pattern where (form) :m3acl)
(define-caller-pattern fwhere (form) :macl)
(define-caller-pattern self (form) :macl)
(define-caller-pattern objectp (form) :macl)
(define-caller-pattern inherit-from-p (form form) :macl)
(define-caller-pattern object-parents (form) :macl)
(define-caller-pattern object-ancestors (form) :macl)
(define-caller-pattern print-self (:optional form) :macl)
(define-caller-pattern object-license (form) :macl)
(define-caller-pattern license-to-object (form) :macl)
(define-caller-pattern next-license-to-object (form) :macl)
(define-caller-pattern highest-license-number :macl)
(define-caller-pattern license-to-object (form) :macl)

(define-caller-pattern do-all-objects
    ((symbol form)
     pbody)
  :macl)

(define-caller-pattern do-object-variables
    ((variable form form)
     pbody)
  :macl)

(define-caller-pattern do-object-functions
    ((variable form form) progn)
  :macl)



;;(use-package "SUBLISP-CL")
;;(in-package "SUBLISP-CL")




;  Copyright (c) 1983 by Gordon S. Novak Jr. 

;  This file defines a program for converting Interlisp code to
;  common lisp code.  It was originally written by Gordon Novak to
;  run under Interlisp, but was then adapted by Peter Karp to run
;  under common lisp.
;  


; The main flow of control in this program is:
;   ltrancoms
;   ltrandocoms
;   lisptrans 
;   pfndef
;   tprogn
;   tprint
;   transfm
;   dialecttransfm 
;   glcommonlisptransfm
;   glcommonlispfn



(proclaim '(special fexprflg filefexprs filefns fileglobalvars filespecvars 
                    fnname glambdaflg ltnatom ltcompatibilityfnsused 
                    outputdialect untranslatedfns gltypesubs lttimestamp 
                    ltgetpairs))

(proclaim '(special lisptranscompatibilityfns lisptransglispfns 
                    lisptraninitialized lisptranstandardfns 
                    lisptranstranslatedfns lisptransunglispify))





(setq userxforms '((clock get-internal-real-time)
                   (position il-position)))

(setq stop 'Done!)    ; Needed for loading Interlisp files



; edited: 17-Mar-89 15:09 =========================== glcommontranscond

; TRANSLATE A COND INTO IF, WHEN, UNLESS IF APPROPRIATE FOR COMMON 
; LISP 

(defun glcommontranscond (x)
  (cond ((null (cddr x))
         (cond ((and (consp (caadr x))
                     (member (caaadr x)
                             '(not null)))
                (cons 'unless
                      (cons (cadr (caadr x))
                            (cdadr x))))
               ((null (cddadr x))
                (cons 'if
                      (cadr x)))
               (t (cons 'when
                        (cadr x)))))
        ((and (null (cdddr x))
              (eq (caaddr x)
                  t))
         (cons 'if
               (cons (caadr x)
                     (cons (if (cddadr x)
                               (cons 'progn
                                     (cdadr x))
                             (cadadr x))
                           (cond ((null (cadr (caddr x)))
                                  nil)
                                 ((null (cddr (caddr x)))
                                  (list (cadr (caddr x))))
                                 (t (list (cons 'progn
                                                (cdaddr x)))))))))
        (t x)))


; edited: 17-Mar-89 15:59 ============================ glcommontransprog

; TRANSLATE A PROG EXPRESSION FOR COMMON LISP, TURNING IT INTO A LET 
; IF APPROPRIATE 

(defun glcommontransprog (x)
  (prog ((lastex (car (last x)))
         nret res tmp)
    (setq res
          (if (and (consp lastex)
                   (setq nret (glnoccurs 'return
                                         x))
                   (or (and (eq (car lastex)
                                'return)
                            (eql nret 1))
                       (eql nret 0))
                   (not (some #'atom
                              (cddr x)))
                   (not (gloccurs 'go
                                  x)))
              (cons 'let
                    (mapcon #'(lambda (y)
                                (if (or (cdr y)
                                        (eql nret 0))
                                    (list (car y))
                                  (list (cadar y))))
                      (cdr x)))
            x))
    (mapl #'(lambda (y)
              (cond ((null (cdr y)))
                    ((null (setq tmp (cadr y)))
                     (setf (cdr y)
                       (cddr y))
                     (setq tmp (cadr y)))
                    ((atom tmp))
                    ((and (eq (car tmp)
                              'setq)
                          (consp (caddr tmp))
                          (eq (caaddr tmp)
                              'cdr)
                          (eq (cadr tmp)
                              (cadr (caddr tmp))))
                     (rplaca (cdr y)
                             (list 'pop
                                   (cadr tmp))))
                    ((eq (car tmp)
                         'progn)
                     (rplacd y (nconc (cdr tmp)
                                      (cddr y))))))
      (cdr res))
    (return res)))


; edited: 10-Sep-86 14:16 ============================== commonlisptranserror

; TRANSLATE A CALL TO ERROR INTO A FORM ACCEPTABLE TO COMMON LISP 

(defun commonlisptranserror (x)
  (prog (args str origargs)
    (unless (cdr x)
      (return (list (car x)
                    "NOMSG")))
    (setq origargs (cdr x))
    (if (and (consp (car origargs))
             (null (cdr origargs))
             (eq (caar origargs)
                 'list))
        (setq origargs (cdar origargs)))
    (setq str "")
    (dolist (z origargs)
      (if (stringp z)
          (setq str (concat str z))
        (progn (setq str (concat str " ~S "))
          (push z args))))
    (return (cons (car x)
                  (cons str (nreverse args))))))


; edited: 19-May-89 12:09 ===================================== commonptmatch


(defun commonptmatch (x busy)
  (setq x (ltptmatch x 'ltuserpatterns))
  (setq x (ltptmatch x 'ltpatterns))
  (unless busy (setq x (ltptmatch x 'ltnbpatterns)))
  x)


; edited: 16-Mar-89 11:35 ================================ commontransglerror

; Translate GLERROR calls for Common Lisp. 

(defun commontransglerror (x)
  (prog (str args l item stritem)
    (if (or (atom (caddr x))
            (not (eq (caaddr x)
                     'list)))
        (return x))
    (setq l (cdaddr x))
    lp
    (unless l (return (list (car x)
                            (cadr x)
                            (cons 'list
                                  (cons str (nreverse args))))))
    (setq item (pop l))
    (if (stringp item)
        (setq stritem item)
      (progn (setq stritem " ~A ")
        (push item args)))
    (setq str (if (null str)
                  stritem
                (concat str " " stritem)))
    (go lp)))


; edited:  4-MAR-83 15:58 ======================================== countargs
;
;  This function counts the number of arguments in the definition of each
;  function to be converted, so that in all calls to that function we can
;  check if it is supplied with the right number of arguments.  If it is
;  not, we pad the call with NIL arguments (since Interlisp defaults
;  the value of unsupplied arguments to NIL).  
;
;  The problem with this approach is that Interlisp Nospread functions 
;  have variable numbers of arguments, which is indicated by defining
;  the function as (LAMBDA ARGS) rather than (LAMBDA (ARG1 .. ARGn)) .
;  For nospread functions we store their number of arguments as 'NOSPREAD
;  instead of as an integer; the code that does call checking ignores
;  nospread functions.

(defun countargs (fn)
  (let (def tmp gltypesubs)
    (setq def (ltgetd fn))
    (cond ((and (eq (car def)
                    'nlambda)
                (atom (cadr def)))
           (setf (get fn 'nargs)
             'fexpr)
           (setq filefexprs (nconc1 filefexprs fn)))
          ((member (car def)
                   '(lambda nlambda))
           (setf (get fn 'nargs)
             (if (consp (cadr def))
                 (length (cadr def))
               'NOSPREAD)))
          ((eq (car def)
               'glambda)
           (setq tmp (gldecl (cadr def)
                             '(t nil)
                             (list nil)
                             nil nil))
           (setf (get fn 'nargs)
             (length tmp))))))


; edited: 20-Aug-86 10:56 =================================== dialecttransfm

; Transform an expression X for another Lisp dialect. 

(defun dialecttransfm (x)
  (case 
    outputdialect 
    (commonlisp (glcommonlisptransfm x))
    (maclisp (glmaclisptransfm x))
    (franzlisp (glfranzlisptransfm x))
    (ucilisp (glucilisptransfm x))
    (psl (glpsltransfm x))
    (t (error "NOMSG"))))


; edited: 22-Aug-86 16:01 ==================================== dialecttransfn

; Transform A FUNCTION NAME X for another Lisp dialect. 

(defun dialecttransfn (x)
  (case outputdialect (commonlisp (glcommonlispfn x))
    (maclisp (glmaclispfn x))
    (franzlisp (glfranzlispfn x))
    (ucilisp (glucilispfn x))
    (psl (glpslfn x))
    (t (error "NOMSG"))))

(function-lambda-expression #'(lambda (hi) (print hi)))

; edited: 22-Aug-86 15:42 ======================================= dialectinit

; Initialize for a particular output dialect. 

(defun dialectinit (outputdialect)
  (let nil (dolist 
        (x (case outputdialect 
            (ucilisp '((terpri 0)
                                                     (print 1)
                                                     (prin1 1)
                                                     (prin2 1)))))
             (setf (get (car x)
                        'nargs)
               (cadr x)))
    (dolist (x (cdr (assoc outputdialect lisptransdialectstandardfns)))
      (setf (get x 'lisptransstandardfn) t))
    (setf (get 'gllispdialect 'lisptransevalwhenconst) t)
    (setf (get 'gllispdialect 'lisptransconstantvalue) outputdialect)))


; edited: 22-Mar-89 14:29 =============================== glcommonlispfn

; Transform a function name FN for COMMON LISP dialect. 

(defun glcommonlispfn (fn)
  (let (tmp)
    (setq tmp (assoc fn '((memb member)
                          (fmemb member)
                          (fassoc assoc)
                          (map mapl)
                          (litatom symbolp)
                          (fixp integerp)
                          (getprop get)
                          (getproplist symbol-plist)
                          (listget getf)
                          (listp consp)
                          (nlistp consp)
                          (neq eq)
                          (igreaterp >)
                          (greaterp >)
                          (igeq >=)
                          (geq >=)
                          (ilessp <)
                          (lessp <)
                          (ileq <=)
                          (leq <=)
                          (iplus +)
                          (plus +)
                          (idifference -)
                          (difference -)
                          (iminus -)
                          (minus -)
                          (itimes *)
                          (times *)
                          (iquotient /)
                          (quotient /)
                          (add1 1+)
                          (* comment)
                          (sub1 1-)
                          (mapconc mapcan)
                          (apply* funcall)
                          (declare comment)
                          (unpack explode)
                          (pack readlist)
                          (dreverse nreverse)
                          (strequal string=)
                          (alphorder string<=)
                          (glstrgreaterp string>)
                          (glstrgep string>=)
                          (glstrlessp string<)
                          (dremove delete)
                          (antilog exp)
                          (print interlispprint)
                          (copy copy-tree)
                          (dsubst nsubst)
                          (eqp eql)
                          (ldifference set-difference)
                          (ltoccurs gloccurs)
                          (ltnoccurs glnoccurs)
                          (glfboundp fboundp))))
    (and tmp (cadr tmp))))


; edited: 23-May-89 12:30 ============================== glcommonlisptransfm

; Transform an expression X for COMMON LISP dialect. 

(defun glcommonlisptransfm (x)
  (prog (tmp tmpb notflg radflg fn)
    
    ; first do argument reversals. 
    
    (cond ((atom x)
           (return x))
          ((member (car x)
                   '(map mapc mapcar mapconc maplist mapcon push some every 
                      subset))
           (setq x (list (car x)
                         (caddr x)
                         (cadr x)))
           (if (and (eq (car x)
                        'mapc)
                    (consp (cadadr x))
                    (eq (car (cadadr x))
                        'lambda))
               (setq x (cons 'dolist
                             (cons (list (caadr (cadadr x))
                                         (caddr x))
                                   (cddr (cadadr x))))))))
    
    ; now see if the result will be negated. 
    
    (setq notflg (member (car x)
                         '(neq nlistp)))
    
    ;  Here is where we do the actual work of translating expressions from one
    ;  Lisp to another.  We check for calls to various different functions by
    ;  name and use specific translations for them.  If the current call
    ;  does not match any of the named functions, we use function GLCOMMONLISPFN
    ;  to see if the name of the function being called should be changed.
    
    (case (car x)
      (prin1 (if (stringp (cadr x)) (setq x (cons 'princ (cdr x)))))
      (arctan2 (setq radflg (cadddr x))
               (setq x (list 'atan
                             (cadr x)
                             (caddr x)))
               (or radflg (setq x (list '* x 57.29578)))) 
      (equal (if (or (numberp (cadr x))
                     (numberp (caddr x)))
                 (setq x (cons 'eql
                               (cdr x)))))
      ((sin cos)
       (setq radflg (caddr x))
       (setq x (if radflg (list (car x)
                                (cadr x))
                 (list (car x)
                       (list '*
                             (cadr x)
                             .01745329)))))
      (append (unless (cddr x)
                (setq x (list (car x)
                              (cadr x)
                              nil))))
      (selectq (setq x (cons 'case
                             (copy-tree (cdr x))))
               (setq tmp (nleft x 2))
               (setf (cdr tmp)
                 (if (null (cadr tmp))
                     nil
                   (list (list t (cadr tmp)))))
               (dolist (y (cddr x))
                 (when (consp (car y))
                   (setq tmp (l-casecopy (car y)))
                   (setf (car y)
                     (remove-duplicates tmp)))
                 (if (and (consp (car y))
                          (null (cdar y)))
                     (rplaca y (caar y)))))
      (nth (setq x (list 'nthcdr
                         (if (integerp (caddr x))
                             (1- (caddr x))
                           (list '1-
                                 (caddr x)))
                         (cadr x))))
      ;  Added by PKarp
      (nchars (setq x (list 'length
                            (list 'string
                                  (cadr x)))))
      ;  Added by PKarp
      (gensym (setq x `(intern (gensym (string ,(cadr x))))))
      ;  Added by PKarp
      (sort (setq x (list 'sort
                          (cadr x)
                          '(function string-lessp))))
      ((member memb fmemb)
       (setq fn (car x))
       (setq x (cons 'member
                     (copy-tree (cdr x))))
       (cond 
        ((and (consp (setq tmp (caddr x)))
              (eq (car tmp) 'quote)
              (consp (cadr tmp))
              (every #'atom
                     (cadr tmp)))
         (setq tmpb (l-casecopy (cadr tmp)))
         (setf (cadr tmp)
           (remove-duplicates tmpb))
         (unless (cdadr tmp)
           (setq x (list (cond ((symbolp (caadr tmp)) 'eq)
                               ((numberp (caadr tmp)) 'eql)
                               (t 'equal))
                         (cadr x)
                         (kwote (caadr tmp))))))
        ((eq fn 'member)
         (setq x (append x (list ':test
                                 '(function equal)))))))
      ((getprop putprop)
       (cond ((and (consp (caddr x))
                   (eq (caaddr x)
                       'quote)
                   (setq tmp (cadr (assoc (cadr (caddr x))
                                          ltgetpairs))))
              (setq x (if (eq (car x)
                              'getprop)
                          (list tmp (cadr x))
                        (list 'setf
                              (list tmp (cadr x))
                              (cadddr x)))))
             ((eq (car x)
                  'putprop)
              (setq x (list 'setf
                            (list 'get
                                  (cadr     x)
                                  (caddr x))
                            (cadddr x))))
             (t (setq x (list 'get
                              (cadr x)
                              (caddr x))))))
      (glerror (setq x (commontransglerror x)))
      (putd (setq x (list 'setf
                          (list 'symbol-function
                                (cadr x))
                          (caddr x))))
      (prog2 (if (and (cddr x)
                      (null (cdddr x)))
                 (setq x (cons 'progn
                               (cdr x)))))
      (t (if (setq tmp (glcommonlispfn (car x)))
             (setq x (cons tmp (cdr x))))))
    (return (if notflg (list 'not
                             x)
              x))))


; edited: 16-Dec-83 13:17 ================================ glfranzlisptransfm

; Transform an expression X for FRANZ LISP dialect. 

(defun glfranzlisptransfm (x)
  (prog (tmp notflg radflg)
    
    ; first do argument reversals. 
    
    (cond ((atom x)
           (return x))
          ((member (car x)
                   '(map mapc mapcar mapconc maplist mapcon push 
                      glstrgreaterp alphorder subset))
           (setq x (list (car x)
                         (caddr x)
                         (cadr x))))
          ((eq (car x)
               'putprop)
           (setq x (list (car x)
                         (cadr x)
                         (cadddr x)
                         (caddr x)))))
    
    ; now see if the result should be negated. 
    
    (setq notflg (member (car x)
                         '(alphorder geq leq glstrgep nlistp)))
    (cond ((and (eq (car x)
                    'prin1)
                (stringp (cadr x)))
           (setq x (cons 'princ
                         (cdr x))))
          ((eq (car x)
               'arctan2)
           (setq radflg (cadddr x))
           (setq x (list 'atan (cadr x)
                         (caddr x)))
           (unless radflg (setq x (list 'times x 57.29578))))
          ((member (car x)
                   '
                   (sin (* cos .01745329)))
           (setq radflg (caddr x))
           (setq x (if radflg (list (car x)
                                    (cadr x))
                     (list (car x)
                           (list 'times (cadr x)
                                 .01745329)))))
          ((setq tmp (glfranzlispfn (car x)))
           (setq x (cons tmp (cdr x))))
          ((and (member (car x)
                        '(some every))
                (null (cdddr x)))
           (setq x (list (car x)
                         (cadr x)
                         (caddr x)
                         nil)))
          ((and (eq (car x)
                    'append)
                (null (cddr x)))
           (setq x (list (car x)
                         (cadr x)
                         nil)))
          ((eq (car x)
               'nth)
           (setq x (list 'nthcdr
                         (if (integerp (caddr x))
                             (1- (caddr x))
                           (list '1-
                                 (caddr x)))
                         (cadr x))))
          ((eq (car x)
               'selectq)
           (setf (car x)
             'caseq)
           (setq tmp (nleft x 2))
           (setf (cdr tmp)
             (if (null (cadr tmp))
                 nil
               (list (list t (cadr tmp)))))))
    (return (if notflg (list 'not
                             x)
              x))))


; edited: 22-Dec-83 11:20 ==================================== glfranzlispfn

; Transform a function name FN for FRANZ LISP dialect. 

(defun glfranzlispfn (fn)
  (let (tmp)
    (setq tmp (assoc fn '((memb memq)
                          (fmemb memq)
                          (fassoc assq)
                          (litatom symbolp)
                          (getprop get)
                          (getproplist plist)
                          (igreaterp >)
                          (igeq >=)
                          (geq lessp)
                          (ilessp <)
                          (ileq <=)
                          (leq greaterp)
                          (iplus +)
                          (idifference -)
                          (itimes *)
                          (iquotient /)
                          (add1 1+)
                          (sub1 1-)
                          (* comment)
                          (eqp =)
                          (mapconc mapcan)
                          (apply* funcall)
                          (declare comment)
                          (nchars flatc)
                          (listp dtpr)
                          (nlistp dtpr)
                          (unpack explode)
                          (pack readlist)
                          (strequal equal)
                          (glstrlessp alphalessp)
                          (alphorder alphalessp)
                          (glstrgreaterp alphalessp)
                          (glstrgep alphalessp)
                          (dreverse nreverse)
                          (dremove delq)
                          (antilog exp)
                          (prin1 print)
                          (print interlispprint)
                          (concat uconcat))))
    (and tmp (cadr tmp))))


; edited: 16-Dec-83 13:18 ================================== glmaclisptransfm

; Transform an expression X for MACLISP dialect. 

(defun glmaclisptransfm (x)
  (prog (tmp notflg radflg)
    
    ; first do argument reversals. 
    
    (cond ((atom x)
           (return x))
          ((member (car x)
                   '(map mapc mapcar mapconc maplist mapcon push some every 
                      subset glstrgreaterp alphorder))
           (setq x (list (car x)
                         (caddr x)
                         (cadr x))))
          ((eq (car x)
               'putprop)
           (setq x (list (car x)
                         (cadr x)
                         (cadddr x)
                         (caddr x)))))
    
    ; now see if the result will be negated. 
    
    (setq notflg (member (car x)
                         '(alphorder geq leq glstrgep neq nlistp)))
    (cond ((and (eq (car x)
                    'prin1)
                (stringp (cadr x)))
           (setq x (cons 'princ
                         (cdr x))))
          ((eq (car x)
               'arctan2)
           (setq radflg (cadddr x))
           (setq x (list 'atan (cadr x)
                         (caddr x)))
           (unless radflg (setq x (list 'times x 57.29578))))
          ((member (car x)
                   '
                   (sin (* cos .01745329)))
           (setq radflg (caddr x))
           (setq x (if radflg (list (car x)
                                    (cadr x))
                     (list (car x)
                           (list 'times (cadr x)
                                 .01745329)))))
          ((setq tmp (glmaclispfn (car x)))
           (setq x (cons tmp (cdr x))))
          ((and (eq (car x)
                    'return)
                (null (cdr x)))
           (setq x (list (car x)
                         nil)))
          ((and (eq (car x)
                    'append)
                (null (cddr x)))
           (setq x (list (car x)
                         (cadr x)
                         nil)))
          ((eq (car x)
               'selectq)
           (setf (car x)
             'caseq)
           (setq tmp (nleft x 2))
           (setf (cdr tmp)
             (if (null (cadr tmp))
                 nil
               (list (list t (cadr tmp))))))
          ((eq (car x)
               'nth)
           (setq x (list 'nthcdr
                         (if (integerp (caddr x))
                             (1- (caddr x))
                           (list '1-
                                 (caddr x)))
                         (cadr x)))))
    (return (if notflg (list 'not
                             x)
              x))))


; edited:  8-Sep-86 15:17 ======================================= glmaclispfn

; Transform a function name FN for MACLISP dialect. 

(defun glmaclispfn (fn)
  (let (tmp)
    (setq tmp (assoc fn '((memb memq)
                          (fmemb memq)
                          (fassoc assq)
                          (litatom symbolp)
                          (getprop get)
                          (getproplist plist)
                          (listp pairp)
                          (nlistp pairp)
                          (neq eq)
                          (igreaterp >)
                          (igeq >=)
                          (geq lessp)
                          (ilessp <)
                          (ileq <=)
                          (leq greaterp)
                          (iplus +)
                          (idifference -)
                          (iminus -)
                          (itimes *)
                          (iquotient //)
                          (add1 1+)
                          (sub1 1-)
                          (* comment)
                          (mapconc mapcan)
                          (apply* funcall)
                          (declare comment)
                          (nchars flatc)
                          (unpack explode)
                          (pack readlist)
                          (dreverse nreverse)
                          (strequal equal)
                          (alphorder alphalessp)
                          (glstrgreaterp alphalessp)
                          (glstrgep alphalessp)
                          (glstrlessp alphalessp)
                          (dremove delq)
                          (antilog exp)
                          (print interlispprint)
                          (concat concatl))))
    (and tmp (cadr tmp))))


; edited: 18-Aug-86 10:35 ====================================== glpsltransfm

; Transform an expression X for Portable Standard Lisp dialect. 

(defun glpsltransfm (x)
  (prog (tmp notflg radflg)
    
    ; first do argument reversals. 
    
    (cond ((atom x)
           (return x))
          ((eq (car x)
               'push)
           (setq x (list (car x)
                         (caddr x)
                         (cadr x))))
          ((member (car x)
                   nil)
           (setq x (list (car x)
                         (cadr x)
                         (cadddr x)
                         (caddr x))))
          ((eq (car x)
               'apply*)
           (setq x (list 'apply
                         (cadr x)
                         (cons 'list
                               (cddr x))))))
    
    ; now see if the result will be negated. 
    
    (setq notflg (member (car x)
                         '(nlistp boundp geq leq igeq ileq)))
    (cond ((and (eq (car x)
                    'prin1)
                (stringp (cadr x)))
           (setq x (cons 'princ
                         (cdr x))))
          ((eq (car x)
               'arctan2)
           (setq radflg (cadddr x))
           (setq x (list 'atan (cadr x)
                         (caddr x)))
           (unless radflg (setf (car x)
                            'atand)))
          ((member (car x)
                   '
                   (sin (* cos .01745329)))
           (setq radflg (caddr x))
           (setq x (list (if radflg (car x)
                           (cdr (assoc (car x)
                                       '((sin . sind)
                                         (cod . cosd)))))
                         (cadr x))))
          ((setq tmp (glpslfn (car x)))
           (setq x (cons tmp (cdr x))))
          ((and (eq (car x)
                    'return)
                (null (cdr x)))
           (setq x (list (car x)
                         nil)))
          ((and (eq (car x)
                    'append)
                (null (cddr x)))
           (setq x (list (car x)
                         (cadr x)
                         nil)))
          ((eq (car x)
               'error)
           (setq x (list (car x)
                         0
                         (cond ((null (cdr x))
                                nil)
                               ((null (cddr x))
                                (cadr x))
                               (t (cons 'list
                                        (cdr x)))))))
          ((eq (car x)
               'selectq)
           (setf (car x)
             'caseq)
           (setq tmp (nleft x 2))
           (setf (cdr tmp)
             (if (null (cadr tmp))
                 nil
               (list (list t (cadr tmp)))))))
    (return (if notflg (list 'not
                             x)
              x))))


; edited: 22-Dec-83 11:22 ======================================== glpslfn

; Transform a function name FN for Portable Standard Lisp dialect. 

(defun glpslfn (fn)
  (let (tmp)
    (setq tmp (assoc fn '((memb memq)
                          (fmemb memq)
                          (fassoc assoc)
                          (litatom idp)
                          (getprop get)
                          (getproplist prop)
                          (putprop put)
                          (listp pairp)
                          (nlistp pairp)
                          (neq ne)
                          (igreaterp greaterp)
                          (igeq lessp)
                          (geq lessp)
                          (ilessp lessp)
                          (ileq greaterp)
                          (leq greaterp)
                          (iplus plus)
                          (idifference difference)
                          (itimes times)
                          (iquotient quotient)
                          (* commentoutcode)
                          (mapconc mapcan)
                          (declare commentoutcode)
                          (nchars flatsize2)
                          (nthchar glnthchar)
                          (dreverse reversip)
                          (strequal string!=)
                          (alphorder string!<!=)
                          (glstrgreaterp string!>)
                          (glstrgep string!>!=)
                          (glstrlessp string!<)
                          (eqp eqn)
                          (last lastpair)
                          (nth pnth)
                          (nconc1 aconc)
                          (u-case glucase)
                          (dsubst substip)
                          (boundp unboundp)
                          (unpack explode)
                          (pack implode)
                          (dremove deletip)
                          (antilog exp)
                          (substring ilsubstring)
                          (getd getddd)
                          (putd putddd)
                          (concat concatl))))
    (and tmp (cadr tmp))))


; edited:  8-Aug-86 10:08 ================================== glucilisptransfm

; Transform an expression X for UCI LISP dialect. 

(defun glucilisptransfm (x)
  (prog (tmp notflg radflg)
    
    ; first do argument reversals. 
    
    (cond ((atom x)
           (return x))
          ((member (car x)
                   '(map mapc mapcar mapconc maplist mapcon some every 
                      subset glstrgep glstrlessp))
           (setq x (list (car x)
                         (caddr x)
                         (cadr x))))
          ((eq (car x)
               'putprop)
           (setq x (list (car x)
                         (cadr x)
                         (cadddr x)
                         (caddr x)))))
    
    ; next see if the result should be negated. 
    
    (setq notflg (member (car x)
                         '(glstrgreaterp glstrlessp)))
    
    ; now do function renamings. 
    
    (cond ((and (eq (car x)
                    'prin1)
                (stringp (cadr x)))
           (setq x (cons 'princ
                         (cdr x))))
          ((eq (car x)
               'arctan2)
           (setq radflg (cadddr x))
           (setq x (list 'atan (cadr x)
                         (caddr x)))
           (unless radflg (setq x (list 'times x 57.29578))))
          ((member (car x)
                   '
                   (sin (* cos .01745329)))
           (setq radflg (caddr x))
           (setq x (if radflg (list (car x)
                                    (cadr x))
                     (list (car x)
                           (list 'times (cadr x)
                                 .01745329)))))
          ((setq tmp (glucilispfn (car x)))
           (setq x (cons tmp (cdr x))))
          ((and (eq (car x)
                    'return)
                (null (cdr x)))
           (setq x (list (car x)
                         nil)))
          ((and (eq (car x)
                    'append)
                (null (cddr x)))
           (setq x (list (car x)
                         (cadr x)
                         nil)))
          ((eq (car x)
               'apply*)
           
           ; change apply* into apply. 
           
           (setq x (list 'apply
                         (cadr x)
                         (cons 'list
                               (cddr x)))))
          ((eq (car x)
               'error)
           
           ; make error have only a single argument. 
           
           (setq x (list (car x)
                         (cons 'list
                               (cdr x))))))
    (return (if notflg (list 'not
                             x)
              x))))


; edited: 16-Dec-83 13:20 ======================================= glucilispfn

; Transform a function name FN for UCILISP dialect. 

(defun glucilispfn (fn)
  (let (tmp)
    (setq tmp (assoc fn '((memb memq)
                          (fmemb memq)
                          (fassoc assoc)
                          (getprop get)
                          (getproplist glgetproplist)
                          (eqp =)
                          (igreaterp >)
                          (igeq ge)
                          (geq ge)
                          (ilessp <)
                          (ileq le)
                          (leq le)
                          (iplus +)
                          (idifference -)
                          (itimes *)
                          (iquotient //)
                          (maplist mapl)
                          (mapcar mapcl)
                          (* comment)
                          (declare comment)
                          (nchars flatsizec)
                          (pack readlist)
                          (unpack explode)
                          (fixp inump)
                          (pop pop)
                          (push push)
                          (listp consp)
                          (alphorder lexorder)
                          (glstrgreaterp lexorder)
                          (glstrlessp lexorder)
                          (strequal eqstr)
                          (antilog exp)
                          (glstrgep lexorder))))
    (and tmp (cadr tmp))))


; edited: 29-JUL-82 09:43 ======================================== l-casecopy

; Make a lower-case copy of a structure. 

(defun l-casecopy (x)
  (cond ((symbolp x)
         (l-case x))
        ((atom x)
         x)
        ((null (cdr x))
         (cons (l-casecopy (car x))
               nil))
        (t (cons (l-casecopy (car x))
                 (l-casecopy (cdr x))))))


; edited: 21-Aug-86 17:10 ======================================== lisptrans

; Translate an INTERLISP function into another LISP dialect. 

(defun lisptrans (fnname)
  (prog (args defn gotdate fexprflg glnatom glambdaflg)
    
    (format *terminal-io* "~A " fnname)
    (force-output *terminal-io*)
    
    (setq ltnatom 0)
    (cond ((atom (setq defn (ltgetd fnname)))
           (prin1 fnname t)
           (princ " is not defined as an EXPR." t)
           (terpri t)
           (return))
          ((and (eq (car defn)
                    'nlambda)
                (atom (cadr defn)))
           (setq fexprflg t))
          ((eq (car defn)
               'nlambda)
           (prin1 fnname t)
           (princ " is NLAMBDA-spread, which is not translated properly." t)
           (terpri t)
           (setq fexprflg t))
          ((eq (car defn)
               'glambda)
           (setq glambdaflg t))
          ((not (eq (car defn)
                    'lambda))
           (prin1 fnname t)
           (princ " has bad form." t)
           (terpri t)
           (return)))
    (terpri)
    (terpri)
    (setq args (cadr defn))
    (setq defn (cddr defn))
    
    ;  Print a banner line before the function definition that includes the
    ;  name of the function.
    
    (princ ";  ")
    (princ (make-string (- 70 (length (string fnname)))
                        :initial-element '#\=))
    (princ "  ")
    (princ fnname)
    (terpri)  (terpri)
    
    ;  Loop through any leading comments in the function definition, and print
    ;  them before we print the definition.
    
    a
    (cond ((and defn (consp (car defn))
                (eq (caar defn)
                    '*))
           (if (or (string= (cadar defn)
                            "GSN: ")
                   (eq (cadar defn)
                       'gsn))
               (setf (car (cdar defn))
                 'edited\:))
           (when (and gotdate (eq (cadar defn)
                                  'edited\:))
             (pop defn)
             (go a))
           (setq gotdate (or gotdate (eq (cadar defn)
                                         'edited\:)))
           (prcomment (car defn))
           (terpri)   ; Insert a little white space
           (pop defn)     ; Go to next element in definition
           (go a))
          ((and defn (consp (car defn))
                (eq (caar defn)
                    'declare))
           (pop defn)
           (go a)))
    
    ; print the start of the function definition. 
    
    (pfndef fnname args fexprflg defn)
    (if (and (eq outputdialect 'franzlisp)
             (not glambdaflg))
        (prin1 '\)))
    (terpri)))


; edited: 22-Aug-86 15:39 ===================================== lisptransinit


(defun lisptransinit nil
  (let nil (dolist (x lisptranscompatibilityfns)
             (setf (get x 'lisptranscompatibilityfn)
               t))
    (dolist (x lisptransstandardfns)
      (setf (get x 'lisptransstandardfn)
        t))
    (dolist (x lisptranstranslatedfns)
      (setf (get x 'lisptranstranslatedfn)
        t))
    (dolist (x lisptransglispfns)
      (setf (get x 'lisptransglispfn)
        t))
    (setq lisptransunglispify nil)))


; edited: 20-Aug-86 10:58 ======================================== ltcase

; Translate an atom into the appropriate case. 

(defun ltcase (x)
  (case outputdialect ((commonlisp maclisp franzlisp)
                       (l-casecopy x))
    (t x)))


; edited: 24-Mar-89 10:04 ==================================== ltdefpatterns


(defun ltdefpatterns (l patwd)
  (dolist (pat l)
    (pushnew pat 
    (get (caar pat) patwd)
             :test #'equal)))


; edited:  4-FEB-83 13:56 ======================================== ltgetd

; Get the definition of FN. 

(defun ltgetd (fn)
  ;;;;pkarp  (or (and lisptransunglispify (get fn 'glcompiled))
  ;;;;pkarp  (getd fn)))
  (get fn 'interlisp-definition))  ;;;;pkarp

; edited: 18-APR-83 12:22 ======================================== ltmkvar

; Make a variable name for function translations. 

(defun ltmkvar nil
  (let nil (incf ltnatom)
    (readlist (append '(l t v a r)
                      (explode ltnatom)))))


; edited:  4-FEB-83 13:03 ======================================== ltnnils

; Make a list of N NILs. 

(defun ltnnils (n)
  (prog (lst)
    lp
    (if (<= n 0)
        (return lst))
    (push nil lst)
    (decf n)
    (go lp)))


; edited:  6-Aug-86 16:42 ======================================== glnoccurs

; COUNT OCCURENCES OF ATM IN STR 

(defun glnoccurs (atm str)
  (cond ((atom str)
         (if (eq atm str)
             1 0))
        ((consp str)
         (+ (glnoccurs atm (car str))
            (glnoccurs atm (cdr str))))
        (t 0)))


; edited: 22-Mar-89 14:46 ======================================== ltnoticefn

; Notice a call to a function FN. Check whether FN is undefined within 
; this file. 

(defun ltnoticefn (fnlst)
  (prog
    (lst fn)
    (setq fn (car fnlst))
    (cond
     ((not (symbolp fn))
      (when (and (not glambdaflg)
                 (not (numberp fn)))
        (princ "The function " t)
        (prin1 fnname t)
        (princ " contains a fn call whose CAR is non-atomic." t)
        (terpri t)
        (prin1 fn t)
        (terpri t)))
     ((or (get fn 'lisptransstandardfn)
          (get fn 'lisptranstranslatedfn)
          (get fn 'lisptransuserfn)
          (dialecttransfn fn))
      (return))
     ((get fn 'lisptranscompatibilityfn)
      (or (member fn ltcompatibilityfnsused)
          (push fn ltcompatibilityfnsused)))
     ((and glambdaflg (get fn 'lisptransglispfn)))
     (t (setq lst (get fn 'ltreferences))
        (or (equal lttimestamp (get fn 'lttimestamp))
            (if glambdaflg
                (progn 
                  ; see if this is a glisp expression rather than a function reference. 
                  
                  (glsepinit fn)
                  (if (or (not (eq fn (glsepnxt)))
                          (and (atom (cadr fnlst))
                               (progn (glsepinit (cadr fnlst))
                                 (gloperator? (glsepnxt)))))
                      (return)))
              (progn (push fn untranslatedfns)
                (setf (get fn 'lttimestamp)
                  lttimestamp))))
        (or (and lst (eq (car lst)
                         fnname))
            (setf (get fn 'ltreferences)
              (cons fnname lst)))))))


; edited: 24-Mar-89 10:19 ======================================== ltobjtrans

; Translate a line of a GLISP object description. Code in the response 
; slot is translated. 

(defun ltobjtrans (line)
  (if (atom (cadr line))
      line
    (cons (car line)
          (cons (if (atom (caadr line))
                    (transfm (cadr line)
                             t)
                  (mapcar #'(lambda (x)
                              (transfm x t))
                    (cadr line)))
                (cddr line)))))


; edited: 18-APR-83 12:19 ======================================== gloccurs

; See if X occurs in STR, using EQ. 

(defun gloccurs (x str)
  (cond ((eq x str)
         t)
        ((atom str)
         nil)
        (t (or (gloccurs x (car str))
               (gloccurs x (cdr str))))))


; edited: 23-May-89 12:35 ======================================== ltpatinit

;;;; I removed these translations because they're not strictly
;;;; correct.  (not (not x)) always returns T or NIL, whereas
;;;; X can take on any random value.
;;;;
;;;;     ((not (not x))
;;;;    x)
;;;;   ((not (null x))
;;;;    x)
;;;;   ((null (not x))
;;;;    x)
;;;;   ((null (null x))
;;;;    x)

(defun ltpatinit nil
  (ltdefpatterns '(((not (consp x))
                    (atom x))
                   ((not (atom x))
                    (consp x))
                   ((if (not x)
                        a b)
                    (if x b a))
                   ((if (null x)
                        a b)
                    (if x b a))
                   ((if (not c)
                        a)
                    (unless c a))
                   ((if (null c)
                        a)
                    (unless c a))
                   ((if a b nil)
                    (if a b))
                   ((setq place (cons new place))
                    (push new place))
                   ((setf place (cons new place))
                    (push new place))
                   ((unless (member new place)
                      (push new place))
                    (pushnew new place))
                   ((unless (member new place :test tst)
                      (push new place))
                    (pushnew new place :test tst))
                   ((car (nthcdr n x))
                    (nth n x))
                   ((setq place (1+ place))
                    (incf place))
                   ((setf place (1+ place))
                    (incf place))
                   ((setq place (1- place))
                    (decf place))
                   ((setf place (1- place))
                    (decf place))
                   ((setq place (+ place n))
                    (incf place n))
                   ((setf place (+ place n))
                    (incf place n))
                   ((setq place (- place n))
                    (decf place n))
                   ((setf place (- place n))
                    (decf place n))
                   ((glgetassoc x l)
                    (cdr (assoc x l)))
                   ((u-case x)
                    x)
                   ((if p (setf x y)
                      (setf x z))
                    (setf x (if p y z)))
                   ((if p (setq x y)
                      (setq x z))
                    (setq x (if p y z)))
                   ((if p (return x)
                      (return y))
                    (return (if p x y)))
                   ((cond (u (return uu))
                          (v (return vv))
                          (w (return ww)))
                    (return (cond (u uu)
                                  (v vv)
                                  (w ww))))
                   ((cond (u (return uu))
                          (v (return vv))
                          (w (return ww))
                          (x (return xx)))
                    (return (cond (u uu)
                                  (v vv)
                                  (w ww)
                                  (x xx))))
                   ((cond (u (return uu))
                          (v (return vv))
                          (w (return ww))
                          (x (return xx))
                          (y (return yy)))
                    (return (cond (u uu)
                                  (v vv)
                                  (w ww)
                                  (x xx)
                                  (y yy))))
                   ((cond (u (return uu))
                          (v (return vv))
                          (w (return ww))
                          (x (return xx))
                          (y (return yy))
                          (z (return zz)))
                    (return (cond (u uu)
                                  (v vv)
                                  (w ww)
                                  (x xx)
                                  (y yy)
                                  (z zz)))))
                 'ltpatterns)
  ; Patterns to use when the result is not busy. 
  (ltdefpatterns '(((rplaca x y)
                    (setf (car x)
                      y))
                   ((rplaca (cdr x)
                            y)
                    (setf (cadr x)
                      y))
                   ((rplaca (car x)
                            y)
                    (setf (caar x)
                      y))
                   ((rplaca (cddr x)
                            y)
                    (setf (caddr x)
                      y))
                   ((rplacd x y)
                    (setf (cdr x)
                      y))
                   ((setq x (cdr x))
                    (pop x))
                   ((setf x (cdr x))
                    (pop x)))
                 'ltnbpatterns))


; edited:  4-FEB-83 11:41 =============================== ltprettyprintconst


(defun ltprettyprintconst (lst)
  (let nil (terpri)
    (terpri)
    (prin1 '\()
    (prin1 (ltcase 'glispconstants))
    (dolist (x lst)
      (terpri)
      (prin1 '\()
      (prin1 (ltcase x))
      (spaces 1)
      (printdef (ltcase (get x 'glisporigconstval)))
      (spaces 1)
      (printdef (ltcase (get x 'glispconstanttype)))
      (prin1 '\)))
    (terpri)
    (prin1 '\))
    (terpri)
    (terpri)))


; edited:  4-FEB-83 11:44 ============================== ltprettyprintglobals


(defun ltprettyprintglobals (lst)
  (let nil (terpri)
    (terpri)
    (prin1 '\()
    (prin1 (ltcase 'glispglobals))
    (dolist (x lst)
      (terpri)
      (prin1 '\()
      (prin1 (ltcase x))
      (spaces 1)
      (printdef (ltcase (get x 'glispglobalvartype)))
      (prin1 '\))
      (terpri))
    (terpri)
    (prin1 '\))
    (terpri)
    (terpri)))


; edited: 18-APR-83 11:48 ================================= ltprettyprintstrs

; Pretty-print GLISP structure definitions for file package output. 

(defun ltprettyprintstrs (lst)
  (prog
    (tmp obj)
    (terpri)
    (terpri)
    (prin1 '\()
    (interlispprint (ltcase 'glispobjects))
    lp
    (unless lst (terpri)
      (prin1 '\))
      (terpri)
      (terpri)
      (return))
    (setq obj (pop lst))
    (when (setq tmp (get obj 'glstructure))
      (terpri)
      (terpri)
      (prin1 '\()
      (prin1 (ltcase obj))
      (spaces 1)
      (printdef (ltcase (car tmp)))
      (mapl #'(lambda (rest)
                (terpri)
                (prin1 (ltcase (car rest)))
                (spaces 1)
                (printdef (ltcase (if (member (car rest)
                                              '(prop adj isa msg))
                                      (mapcar #'ltobjtrans
                                        (cadr rest))
                                    (cadr rest)))
                          8))
        (cdr tmp))
      (prin1 '\))
      (terpri))
    (go lp)))


; edited:  5-May-89 18:09 ====================================== ltprintplain


(defun ltprintplain (form pos)
  (prin1 '\()(prin1 (car form))(spaces 1)(printdef (cdr form)
                                                   (+ pos 8)
                                                   t t)(prin1 '\)))


; edited: 24-Mar-89 10:27 ======================================= ltprintprop

; Print a property value. FLG is T to print it even if NIL. 

(defun ltprintprop (atm prop val flg)
  (when (or val flg)
    (terpri)
    (tprint (list 'putprop
                  (list 'quote
                        atm)
                  (list 'quote
                        prop)
                  (kwote val))
            nil 1)))


; edited:  4-FEB-83 14:30 ====================================== ltprintprops

; Print specified PROPS for ATM. PROPS may be a single prop, list of 
; props, or ALL. FLG is T to print props even when NIL. 

(defun ltprintprops (atm props flg)
  (prog nil (cond ((eq props 'all)
                   (mapl #'(lambda (l)
                             (unless (member (car l)
                                             sysprops)
                               (ltprintprop atm (car l)
                                            (cadr l)
                                            flg)))
                     (symbol-plist atm))
                   (return))
                  ((atom props)
                   (ltprintprop atm props (get atm props)
                                flg)
                   (return)))
    (dolist (x props)
      (ltprintprop atm x (get atm x)
                   flg))))


; edited: 24-Mar-89 10:00 ======================================== ltptmatch

; Try to match INP against optimization patterns. If a match is found, 
; the right-hand side of the pattern is returned with appropriate 
; substitutions. 

(defun ltptmatch (inp patwd)
  (prog (patterns)
    (setq ltptmatchbindings nil)
    top
    (and (consp inp)
         (atom (car inp))
         (setq patterns (get (car inp) patwd)))
    lp
    (cond 
        ((null patterns)(return inp))
          ((ltptmatcha (caar patterns) inp)
           (setq inp (sublis ltptmatchbindings (cadar patterns)))
           (go top)))
    (setq ltptmatchbindings nil)
    (pop patterns)
    (go lp)))


; edited: 15-Mar-89 10:33 ======================================== ltptmatcha

; Match a pattern against an input. If PAT is a list, it is at the 
; front of a Lisp function call. 

(defun ltptmatcha (pat inp)
  (let (tmp)
    (cond
     
     ;  If (car pat) matches (car inp) then execute the pattern on the cdrs.
     ((consp pat)
      (and (consp inp)
           (eq (car pat)
               (car inp))
           (ltptmatchl (cdr pat)
                       (cdr inp))))
     ((or (numberp pat)
          (null pat)
          (eq pat t))
      (eql pat inp))
     ((symbolp pat)
      (cond ((setq tmp (assoc pat ltptmatchbindings))
             (equal inp (cdr tmp)))
            ((or (not (member pat '(m n)))
                 (numberp inp))
             (push (cons pat inp)
                   ltptmatchbindings)))))))


; edited: 15-Mar-89 10:36 ======================================== ltptmatchl

; Match two lists of args 

(defun ltptmatchl (patl inpl)
  (cond ((null patl)
         (null inpl))
        ((consp patl)
         (and (consp inpl)
              (ltptmatcha (car patl)
                          (car inpl))
              (ltptmatchl (cdr patl)
                          (cdr inpl))))))


; edited: 19-May-89 12:12 ======================================== ltrancoms

; Translate all the funtions on a FNS list from INTERLISP to another 
; LISP dialect. 

(defun ltrancoms (outputdialect outfilename coms ltusertransforms ltgetpairs)
  (let
      (normfile dialects *glnatom* filefns filespecvars fileglobalvars filefexprs 
                untranslatedfns fullfilename ltcompatibilityfnsused glambdaflg 
                fnname lttimestamp)
    (setq *glnatom* 0)
    
    ; make sure the output dialect is legal. 
    
    (setq dialects '(commonlisp franzlisp maclisp psl ucilisp))
    (unless (member outputdialect dialects)
      (error "Dialect must be a member of ~S " dialects))
    (setq lttimestamp (get-internal-real-time))
    (terpri t)
    (terpri t)
    (terpri t)
    (format t "[Translating into file ~A in dialect ~A]" outfilename outputdialect)
    (terpri t)
    (terpri t)
    (ltpatinit)
    (if ltusertransforms (ltdefpatterns ltusertransforms 'ltuserpatterns))
    (dialectinit outputdialect)
    
    (ltraninterpretcoms coms)
    (dolist (x filefns)
      (setf (get x 'lisptransuserfn)
        t))
    
    (if lisptransunglispify (dolist (fn filefns)
                              (if (and (consp (getd fn))
                                       (eq (car (getd fn))
                                           'glambda))
                                  (glcc fn))))
    
    ; count number of function arguments for error checking. 
    
    (mapc #'countargs
      filefns)
    
    (setq normfile (open outfilename :direction :io))
    (output normfile)
    
    ; print a header on the file. 
    
    (terpri)
    (prcomment (list '*
                     fullfilename))
    
    (terpri)
    (terpri)
    (terpri)
    (ltranspecials filespecvars)
    (ltranglobals fileglobalvars)
    (ltranfexprs filefexprs)
    (mapc #'ltrandocoms   ; Translate functions
      coms)
    (output t)
    (when ltcompatibilityfnsused (terpri)
      (princ 
       "The following functions in the compatibility package are used:")
      (terpri)
      (interlispprint ltcompatibilityfnsused))
    (ltreporterrors untranslatedfns)
    fullfilename))


; edited:  4-FEB-83 13:36 =================================== ltrancomsvalue

; Get the value of an element of a COMS list; elements are of the form
; (KEYWORD VALUE1 .. VALUEn) or  (KEYWORD * VARIABLE) .  We return either
; (VALUE1 .. VALUEn) or the value of VARIABLE.

(defun ltrancomsvalue (comslist)
  (if (eq (cadr comslist)
          '*)
      (eval (caddr comslist))
    (cdr comslist)))


; edited:  8-MAR-83 16:27 ======================================= ltrandocoms

; Process one item from a COMS list. 

(defun ltrandocoms (comslst)
  (prog (lst)
    (when (eq (car comslst)
              '*)
      (terpri)
      (terpri)
      (terpri)
      (prcomment comslst)
      (terpri)
      (terpri)
      (terpri)
      (return))
    (setq lst (ltrancomsvalue comslst))
    
    ;  Dispatch according to the type of the current COMS element:
    
    (case (car comslst)
      
      ;   Translate function definitions
      
      (fns (mapc #'lisptrans
             lst))
      
      ;   Translate variable definitions
      
      ((vars initvars)
       
       ;   For each definition, write out a form that SETQs the variable
       ;   to either its currently defined value, or to a value defined
       ;   here within the COMS.
       
       (dolist (name lst)
         (terpri)
         (princ '\()
         (prin1 (ltcase 'setq))
         (spaces 1)
         (if (atom name)
             ;        Current value
             (progn (prin1 (ltcase name))
               (spaces 1)
               (printdef (ltcase (kwote (eval name))
                                 )))
           ;        Value within COMS
           (progn (prin1 (ltcase (car name)))
             (spaces 1)
             (printdef (ltcase (cadr name)))))
         (princ '\))
         (terpri)))
      (glispobjects (ltprettyprintstrs lst))
      (glispconstants (ltprettyprintconst lst))
      (glispglobals (ltprettyprintglobals lst))
      (p (dolist (x lst)
           (terpri)
           (printdef (ltcase x))
           (terpri)))
      (prop (dolist (x (cddr comslst))
              (ltprintprops x (cadr comslst)
                            t)))
      (ifprop (dolist (x (cddr comslst))
                (ltprintprops x (cadr comslst)
                              nil)))
      (props (dolist (x (cdr comslst))
               (ltprintprops (car x)
                             (cadr x)
                             t))))))


; edited:  4-FEB-83 14:09 ======================================= ltranfexprs

; Output declarations for FEXPR functions. 

(defun ltranfexprs (lst)
  (if lst (case outputdialect ((maclisp franzlisp)
                               (terpri)
                               (terpri)
                               (printdef (list 'declare
                                               (cons '*fexpr
                                                     (mapcar #'l-case
                                                       lst))))))))


; edited:  4-FEB-83 12:43 ======================================== ltranfns

; Translate all the funtions on a FNS list from INTERLISP to another 
; LISP dialect. 

(defun ltranfns (outputdialect outfilename fns)
  (ltrancoms outputdialect outfilename (list (cons 'fns
                                                   fns))
             nil nil))


; edited: 20-Aug-86 10:59 ====================================== ltranglobals

; Output declarations for global variables. 

(defun ltranglobals (lst)
  (when lst (terpri)
    (terpri)
    (printdef (case outputdialect ((maclisp franzlisp)
                                   (list 'declare
                                         (cons 'special
                                               (mapcar #'l-case
                                                 lst))))
                (commonlisp (list 'proclaim
                                  (list 'quote
                                        (cons 'special
                                              (mapcar #'l-case
                                                lst)))))
                (ucilisp (list 'declare
                               (cons 'special
                                     lst)))
                (psl (list 'global
                           (list 'quote
                                 lst)))))))


; edited: 18-APR-83 11:31 ================================ ltraninterpretcoms

; Look through a COMS list to extract certain kinds of information 
; before translation begins.  Note that we don't extract ALL the information
; now; some we deal with later in LTRANDOCOMS.

(defun ltraninterpretcoms (coms)
  (prog (comslst)
    lp
    (unless coms (return))
    (setq comslst (pop coms))
    (case (car comslst)
      (fns (setq filefns (append (ltrancomsvalue comslst)
                                 filefns)))
      (specvars (setq filespecvars (append (ltrancomsvalue comslst)
                                           filespecvars)))
      (globalvars (setq fileglobalvars (union  (ltrancomsvalue comslst)
                                              fileglobalvars)))
      (initvars (setq fileglobalvars
                      (union  (mapcar #'car
                                (ltrancomsvalue comslst))
                             fileglobalvars)))
      ((glispobjects glispglobals glispconstants)
       (pushnew (car comslst)
                filefexprs)))
    (go lp)))


; edited: 20-Aug-86 11:00 ==================================== ltranspecials

; Output declarations for special variables. 

(defun ltranspecials (lst)
  (when lst (terpri)
    (terpri)
    (printdef (case outputdialect ((maclisp franzlisp)
                                   (list 'declare
                                         (cons 'special
                                               (mapcar #'l-case
                                                 lst))))
                (commonlisp (list 'proclaim
                                  (list 'quote
                                        (cons 'special
                                              (mapcar #'l-case
                                                lst)))))
                (ucilisp (list 'declare
                               (cons 'special
                                     lst)))
                (psl (list 'fluid
                           (list 'quote
                                 lst)))))))


; edited:  4-FEB-83 13:23 ==================================== ltremovecommas

; Remove top-level commas in a GLISP A function. 

(defun ltremovecommas (l)
  (if (member '\,
              l)
      (mapcan #'(lambda (x)
                  (if (eq x '\,)
                      nil
                    (cons x nil)))
        l)
    l))


; edited:  4-FEB-83 14:19 ==================================== ltreporterrors

; Report untranslated fns and where they were referenced. 

(defun ltreporterrors (fns)
  (prog nil (or fns (return))
    (terpri)
    (terpri)
    (princ "The following functions are not in this file:")
    (terpri)
    (dolist (fn (sort fns #'string-lessp))
      (terpri)
      (interlispprint fn)
      (interlispprint (sort (get fn 'ltreferences) #'string-lessp))
      (setf (get fn 'ltreferences)
        nil))))


; edited: 23-Mar-89 12:56 ====================================== lttransprog

; Translate places where a PROG variable is initialized to a value as 
; allowed by Interlisp. This is done by adding a SETQ to set the 
; value of each PROG variable which is initialized. In some cases, a 
; change of variable name is required to preserve the same 
; semantics. 

(defun lttransprog (x)
  (let
      (tmp argvals setvars rest flg)
    (mapl
        #'(lambda (y)
            (cond
             ((and (not flg)
                   (consp (car y)))
              
              ; if possible, use the same variable; otherwise, make a new one. 
              
              (setq
               tmp
                (if (or (some #'(lambda (z)
                                  (and (consp z)
                                       (gloccurs (car z)
                                                 (cadar y))))
                              (cadr x))
                        (some #'(lambda (z)
                                  (gloccurs (caar y)
                                            z))
                              argvals))
                    (ltmkvar)
                  (caar y)))
              (setq setvars (nconc1 setvars (list 'setq
                                                  tmp
                                                  (cadar y))))
              (setq rest (nsubst tmp (caar y)
                                 (cddr x)))
              (push (cadar y)
                    argvals)
              (rplaca y tmp))
             ((and (atom (car y))
                   (eq (nthchar (car y)
                                -1)
                       '\:)
                   (consp (cadr y))
                   (member (caadr y)*gltypenames*))
              (setq flg t))
             (t (setq flg nil))))
      (cadr x))
    (if setvars (setf (cdr (cdr x))
                  (nconc setvars rest)))
    x))


; edited: 10-Sep-86 13:36 ======================================== pfndef

; Print the definition of a function for another Lisp dialect. 

(defun pfndef (fnname args fexprflg defn)
  (prog
    (newfnname)
    (princ '\()
    
    ;  Print the DEFUN and the function name and arguments.
    
    (case
        outputdialect
      (commonlisp
       (princ (cond (fexprflg 'defmacro)
                    (glambdaflg 'gldefun)
                    (t 'defun)))
       (spaces 1)
       (princ (l-case fnname))
       (spaces 1)
       (if
           fexprflg
           (progn (printdef (cons '&rest
                                  (list (l-case args))))
             (spaces 1)
             (princ '\`)
             (if (and defn (null (cdr defn))
                      (null (cddar defn))
                      (eq (cadar defn)
                          args))
                 (progn (printdef (list (l-case (caar defn))
                                        (concat "',"
                                                (format nil "~A" (l-case args)))))
                   (princ '\))
                   (terpri)
                   (return))
               (progn (printdef (list (l-case (setq newfnname
                                                    (concat (string fnname)
                                                            "-expr")))
                                      (concat "',"
                                              (format nil "~A" (l-case args)))))
                 (princ '\))
                 (terpri)
                 (pfndef newfnname (list args)
                         nil defn)
                 (return))))
         (printdef (if (atom args)
                       (l-case args)
                     (mapcar #'l-case
                       args)))))
      (maclisp (prin1 (if glambdaflg 'gldefun
                        'defun))
               (spaces 1)
               (prin1 (l-case fnname))
               (spaces 1)
               (when fexprflg (prin1 'fexpr)
                 (spaces 1)
                 (if (and args (atom args))
                     (setq args (list args))))
               (setq args (if (atom args)
                              (l-case args)
                            (mapcar #'l-case
                              args)))
               (printdef args))
      (franzlisp (prin1 (if glambdaflg 'gldefun
                          'def))
                 (spaces 1)
                 (prin1 (l-case fnname))
                 (spaces 1)
                 (cond (fexprflg (prin1 '\()
                                 (prin1 'nlambda))
                       (glambdaflg)
                       (t (prin1 '\()
                          (prin1 'lambda)))
                 (spaces 1)
                 (if (and args (atom args))
                     (setq args (list args)))
                 (setq args (if (atom args)
                                (l-case args)
                              (mapcar #'l-case
                                args)))
                 (printdef args))
      ((ucilisp psl)
       (prin1 (cond (fexprflg 'df)
                    (glambdaflg 'dg)
                    (t 'de)))
       (spaces 1)
       (prin1 fnname)
       (spaces 1)
       (if (and fexprflg args (atom args))
           (setq args (list args)))
       (printdef args))
      (t (error "NOMSG")))
    ;
    ;  Print the definition of the function
    ;
    ;;;;  (terpri)
    (tprogn defn nil)
    (princ '\))))


; edited:  8-Aug-86 17:35 ======================================== prcomment


; Print an Interlisp comment for another dialect. 


(defun prcomment (com)
  (prog (col nc (firstele t))
    (if (> (il-position (output))
           1)
        (terpri))
    
    (case outputdialect
      ((commonlisp maclisp franzlisp psl)
       (princ (case outputdialect ((commonlisp maclisp franzlisp)
                                   '\;)
                (psl '%%)
                (t (error "NOMSG"))))
       (spaces 2)
       (setq col 3)
       
       ;  If we have a comment like (* * Foo), get ride of the second asterisk.
       
       (if (eq '* (cadr com))
           (setq com (cdr com)))
       
       ; Loop through each element of the comment
       
       (dolist (x (cdr com))
         (setq nc (1+ (length (format nil "~A" x))))
         ; Emit a newline if we're at the edge of the page.
         
         (when (> (+ col nc)
                  71)
           (terpri)
           (princ (case outputdialect
                    ((commonlisp maclisp franzlisp)
                     '\;)
                    (psl '%%)
                    (t (error "NOMSG"))))
           (spaces 2)
           (setq col 3))
         
         ; Emit current element (capitalize the first word in a comment).
         
         (princ (if firstele
                    (string-capitalize (format nil "~A" x))
                  x))
         (spaces 1)
         (setq col (+ col nc))
         (setq firstele nil)))
      
      (ucilisp (prin1 '{)
               (prin1 '\;)
               (spaces 1)
               (setq col 4)
               (dolist (x (cdr com))
                 (prog (n)
                   lp
                   (cond ((setq n (strpos ";" x))
                          (setq x (rplstring x n "."))
                          (go lp))
                         ((setq n (strpos "{" x))
                          (setq x (rplstring x n "("))
                          (go lp))
                         ((setq n (strpos "}" x))
                          (setq x (rplstring x n ")"))
                          (go lp)))
                   (setq nc (1+ (length (format nil "~A" x))))
                   (if (> nc 60)
                       nil
                     (progn (when (> (+ col nc)
                                     71)
                              (terpri)
                              (spaces 3)
                              (setq col 4))
                       (prin1 x)
                       (spaces 1)
                       (setq col (+ col nc))))))
               (prin1 '})
               (terpri))
      (t (error "NOMSG")))
    (terpri)))


; edited: 22-Aug-86 16:07 ======================================== remove


;;dmiles (defun remove-duplicates (x) (intersection x x))


; edited: 24-Mar-89 10:25 ======================================== tprint

; Print an expression, translated for another Lisp dialect. 

(defun tprint (x busy col)
  (cond ((symbolp x)
         (printdef (case outputdialect ((commonlisp maclisp franzlisp)
                                        (l-case x))
                     (ucilisp x)
                     (t x))
                   col))
        ((atom x)
         (printdef x col))
        ((eq (car x)
             '*)
         (prcomment x))
        ((eq (car x)
             'quote)
         (princ '\')
         (printdef (case outputdialect ((commonlisp maclisp franzlisp)
                                        (l-casecopy (cadr x)))
                     (ucilisp (u-casecopy (cadr x)))
                     (t (cadr x)))
                   (1+ col)))
        (t
         (printdef (case outputdialect ((commonlisp maclisp franzlisp)
                                        (l-casecopy (transfm x busy)))
                     (ucilisp (u-casecopy (transfm x busy)))
                     (t (transfm x busy)))
                   col))))


; edited: 24-Mar-89 10:26 ======================================== tprogn

; Output a list which is an implicit PROGN for another Lisp dialect. 

(defun tprogn (lst col)
  (mapl #'(lambda (x)
            (tprint (car x)
                    (null (cdr x))
                    col))
    lst))


; edited: 24-Mar-89 10:57 ======================================== transcond

; Translate a COND expression. 

(defun transcond (x busy)
  (let
      (tmp)
    (setq
     tmp
      (cons
       'cond
       (mapcar
           #'(lambda (y)
               (maplist #'(lambda (z)
                            (transfm (car z)
                                     (or (eq z y)
                                         (and busy (null (cdr z))))))
                        y))
         (cdr x))))
    (case outputdialect 
        (commonlisp (glcommontranscond tmp))
        (t tmp))))


; edited: 24-Mar-89 10:18 ======================================== transfm

; Transform an expression X for another Lisp dialect. 

(defun transfm (x busy)
  (prog
    (tmp nactual)
    (cond ((or (atom x)
               (stringp x))
           (return x))
          
          ; Special transformations for a few functions
          
          ((member (car x)
                   '(quote  function cond selectq case setq prog * if))
           (setq x
                 (case (car x)
                   (quote (transquote x busy))
                   (function (transfunction x busy))
                   (cond (transcond x busy))
                   (if (transif x busy))
                   ((selectq case)
                    (setq tmp (transselectq x busy))
                    
                    ; the selectq may be optimized away, in which case we want to avoid 
                    ; transforming the result a second time. 
                    
                    (if (and (consp tmp)
                             (eq (car tmp)
                                 (car x)))
                        (dialecttransfm tmp)
                      tmp))
                   (setq (transsetq x busy))
                   (prog (transprog x busy))
                   (* (cons 'comment
                            (cdr x)))
                   (t (error "NOMSG")))))
          ((and (eq outputdialect 'commonlisp)
                (eq (car x)
                    'error))
           (setq x (commonlisptranserror x)))
          ((member (car x)
                   filefexprs))
          (t (setq x (mapcar #'(lambda (y)
                                 (if (and (eq y '/)
                                          (eq outputdialect 'maclisp))
                                     '//
                                   (transfm y t)))
                       x))
             
             ; Check for correct number of arguments to converted functions.  If
             ; If too few args are supplied we add NIL arguments.
             
             (if (and (not glambdaflg)
                      (integerp (setq tmp (get (car x) 'nargs)))
                      (not (eql tmp (setq nactual (length (cdr x))))))
                 (if (> nactual tmp)
                     (progn (princ "***** " t)
                       (prin1 fnname t)
                       (princ " has too many args in call to " t)
                       (interlispprint (car x)
                                       t))
                   (progn (nconc x (ltnnils (- tmp nactual)))
                     (prin1 fnname t)
                     (princ ": NILs added in call to " t)
                     (interlispprint (car x)
                                     t))))
             
             ; now see if any transformations need to be made for the output 
             ; dialect of lisp. 
             
             (if (and glambdaflg (member (car x)
                                         '(a an)))
                 (setq x (ltremovecommas x)))
             (ltnoticefn x)
             (setq x (dialecttransfm x))))
    (if (eq outputdialect 'commonlisp)
        (setq x (commonptmatch x busy)))
    (return x)))


; edited: 24-Mar-89 10:39 ================================ transfunction

; Translate a FUNCTION expression. 

(defun transfunction (x busy)
  (if
      (atom (cadr x))
      (list (car x)
            (or (dialecttransfn (cadr x))
                (cadr x)))
    (list (car x)
          (cons (caadr x)
                (cons (cadadr x)
                      (maplist #'(lambda (y)
                                   (transfm (car y)
                                            (and busy (null (cdr y)))))
                               (cddadr x)))))))


; edited: 24-Mar-89 11:03 ======================================== transprog

; Transform a PROG expression. 

(defun transprog (x busy)
  (let (tmp)
    (setq tmp (cons (mapcar #'(lambda (v)
                                (if (atom v)
                                    v
                                  (list (car v)
                                        (transfm (cadr v)
                                                 t))))
                      (cadr x))
                    (mapcar #'(lambda (y) (transfm y nil))
                      (cddr x))))
    (case outputdialect 
      (commonlisp 
       (glcommontransprog (cons 'prog tmp))) 
      (t (lttransprog (cons 'prog (if (some #'consp (car tmp)) (copy-tree tmp) tmp)))))))


; edited:  8-May-89 13:54 ======================================== transquote

; Transform a QUOTEd expression for another Lisp dialect. Atom 
; substitutions are made in case single-character atoms require an 
; escape character in the target dialect. 

(defun transquote (x busy)
  
  x
  
  ;;;;(list 'quote
  ;;;;  (sublis (case outputdialect (commonlisp (cons (cons (character 39)
  ;;;;          '\')
  ;;;;          '((\\ . \\)
  ;;;;          (\: . \:)
  ;;;;          (\:= . \:=)
  ;;;;          (\, . \\\,)
  ;;;;          (\; . \\\;)
  ;;;;          (\  . \ )
  ;;;;          (\. . \.)
  ;;;;          (\( . \()
  ;;;;          (\) . \))
  ;;;;          (\~ . \~))))
  ;;;;    (maclisp '((/ . //)
  ;;;;       (\, . /\,)
  ;;;;       (\' . /\')
  ;;;;       (\  . /\ )
  ;;;;       (\. . /\.)
  ;;;;       (\( . /\()
  ;;;;       (\) . /\))))
  ;;;;    (franzlisp '((\\ . \\)
  ;;;;     (\' . \')
  ;;;;     (\, . \,)
  ;;;;     (+_ . +\_)
  ;;;;     (-_ . -\_)
  ;;;;     (\  . \ )
  ;;;;     (\. . \.)
  ;;;;     (\( . \()
  ;;;;     (\) . \))))
  ;;;;    (ucilisp '((/ . //)
  ;;;;       (\, . /\,)
  ;;;;       (\' . /\')
  ;;;;       (\  . /\ )
  ;;;;       (\. . /\.)
  ;;;;       (\( . /\()
  ;;;;       (\) . /\))))
  ;;;;    (psl '((! . !!)
  ;;;;     (\, . !\,)
  ;;;;     (\' . !\')
  ;;;;     (\  . !\ )
  ;;;;     (\. . !\.)
  ;;;;     (\( . !\()
  ;;;;     (\\ . !)
  ;;;;     (\) . !\)))))
  ;;;;    (cadr x)
  ;;;;    t))
  )


; edited: 24-Mar-89 11:05 ====================================== transselectq


(defun transselectq (x busy)
  (prog
    (l sel)
    (if
        (or (consp (cadr x))
            (not (get (cadr x)
                      'lisptransevalwhenconst)))
        (return
          (cons
           (car x)
           (cons
            (transfm (cadr x)
                     t)
            (maplist
             #'(lambda (y)
                 (cond
                  ((atom (car y))
                   (transfm (car y)
                            t))
                  ((cdr y)
                   (cons
                    (caar y)
                    (maplist #'(lambda (z)
                                 (transfm (car z)
                                          (and busy (null (cdr z)))))
                             (cdar y))))
                  (t (transfm (car y)
                              busy))))
             (cddr x))))))
    (setq sel (get (cadr x)
                   'lisptransconstantvalue))
    (setq l (cddr x))
    lp
    (cond
     ((null (cdr l))
      (return (transfm (car l)
                       busy)))
     ((or (eq sel (caar l))
          (and (consp (caar l))
               (member sel (caar l))))
      (return
        (if (cddar l)
            (cons 'progn
                  (maplist #'(lambda (z)
                               (transfm (car z)
                                        (and busy (null (cdr z)))))
                           (cdar l)))
          (transfm (cadar l)
                   busy)))))
    (pop l)
    (go lp)))


; edited: 24-Mar-89 11:01 ======================================== transsetq

; Translate a SETQ expression. 

(defun transsetq (x busy)
  (list (car x)
        (cadr x)
        (transfm (caddr x)
                 t)))


;  ===========================================================  transif
;  PKarp
;  
;  Convert Interlisp (IF a THEN (foo) (bar) ELSE (baz)) to
;        (if a (progn (foo) (bar)) (baz))

(defun transif (x busy)
  (let ((c1 nil) (c2 nil))
    
    ;  Collect THEN clauses
    
    (dolist (y (cdddr x))
      (if (eq 'else y)
          (return)
        (push y c1)))
    (setq c1 (reverse c1))
    (if (> (length c1) 1)
        (push 'progn c1)
      (setq c1 (car c1)))
    
    ;  Collect ELSE clauses
    
    (setq c2 (cdr (member 'else (cdr (cdddr x)))))
    (if (> (length c2) 1)
        (push 'progn c2)
      (setq c2 (car c2)))
    
    ;  Construct new expression
    
    (if c2
        (list 'if
              (transfm (cadr x) t)
              (transfm c1 t)
              (transfm c2 t))
      (list 'if
            (transfm (cadr x) t)
            (transfm c1 t)))
    ))



; edited: 26-AUG-82 15:32 ======================================== u-casecopy

; Make an UPPER-CASE copy of a structure. 

(defun u-casecopy (x)
  (cond ((symbolp x)
         x)
        ((atom x)
         x)
        ((null (cdr x))
         (cons (u-casecopy (car x))
               nil))
        (t (cons (u-casecopy (car x))
                 (u-casecopy (cdr x))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;    The following lists various Lisp function names and is 
;    used by the translator to identify functions that are
;    undefined in a given file, and hence may be problematic.

; Functions that are in an Interlisp-compatibility package that we in theory
; have.

(setq lisptranscompatibilityfns '(concat copy dsubst eqp every fixp floatp 
                                         getd intersection kwote ldifference 
                                         listget nconc1 nthchar pop pop push 
                                         push putd remove some spaces stringp 
                                         subatom subset union for mkatom
                                         mkstring))

(setq lisptransglispfns '(a an an case case for for if if repeat repeat send 
                            sendprop send sendprop the those the those while 
                            while a an case for if repeat send sendprop the 
                            those while _))

;  Standard functions in all lisps

(setq lisptransstandardfns '(and apply assoc atom boundp caaaar caaadr caaar 
                                 caadar caaddr caadr caar cadaar cadadr cadar 
                                 caddar cadddr caddr cadr car cdaaar cdaadr 
                                 cdaar cdadar cdaddr cdadr cdar cddaar cddadr 
                                 cddar cdddar cddddr cdddr cddr cdr cond cons 
                                 eq equal eval go last length list member 
                                 minus nconc not null numberp
                                 or print prin1 progn prog1 prog2 read 
                                 rplaca rplacd set setq sublis subst 
                                 terpri zerop))

;  Interlisp functions that we translate inline into other code.

(setq lisptranstranslatedfns '(add1 alphorder append apply* declare dremove 
                                    dreverse eqp error every fassoc fmemb geq 
                                    getprop getproplist glstrgep glstrgreaterp 
                                    glstrlessp idifference igeq igreaterp ileq 
                                    ilessp plus minus difference times 
                                    quotient greaterp lessp iplus iquotient 
                                    itimes leq listp litatom map mapc mapcar 
                                    mapcon mapconc maplist memb nchars neq 
                                    nlistp nth pack push putprop return 
                                    selectq some strequal sub1 subset unpack 
                                    push sin cos arctan2 nchars sort))

;  Functions specific to certain Lisp dialects

(setq lisptransdialectstandardfns '
    ((commonlisp aref arrayp make-array princ proclaim setf every stringp floatp pop push gensym if remove-duplicates remprop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(setq lisptransunglispify nil)

(setq ltgetpairs nil)

(setq ltptmatchbindings nil)

(lisptransinit)


(defstruct filecomment start end src block-p)
(defstruct filefunction src)


(defvar *code-hdr* ())
(defvar *code-src* ())
(defvar *code-fns* ())

(defun clear-src-table ()
  (let ((size (+ (length *code-src*)(length *code-hdr*))))
    (setq *code-src* ())
    (setq *code-hdr* ())
    (format t  "Cleared ~S forms.~%" size)))

(clear-src-table)

(defun  load-src-table (file)
  (let ( (forms 0) (lcomments 0) (bcomments 0) (functions 0)
        (blockcmt (GET-DISPATCH-MACRO-CHARACTER #\# #\| ))
        (fnreader (GET-DISPATCH-MACRO-CHARACTER #\# #\' ))
        (lncmt (get-macro-character #\; )) )
    (with-open-file (*standard-input* file :direction :input )
      (SET-DISPATCH-MACRO-CHARACTER #\# #\| 
                                    #'(lambda (stream SUBCHAR ARG) 
                                        (incf bcomments)
                                        (let ((start (file-position stream)))
                                          (SYSTEM::COMMENT-READER stream SUBCHAR ARG)
                                          (values (make-filecomment :start start 
                                                                    :SRC (stream-segment stream  start (file-position stream)) 
                                                                    :END (file-position stream) :BLOCK-P t)))))
#|
      (SET-DISPATCH-MACRO-CHARACTER #\# #\' 
                                    #'(lambda (stream SUBCHAR ARG) 
                                        (incf functions)
                                        (let ((funct (read stream)))
                                          (pushnew  (list 'function funct) *code-fns*)
                                          (values (list 'function funct)))))
      ;; (values (make-filefunction (read)))
  |#    (SET-MACRO-CHARACTER #\; 
                           #'(lambda (stream ARG) 
                               (incf lcomments)
                               (let ((start (file-position stream)))
                                 (SYSTEM::LINE-COMMENT-READER stream ARG)
                                 (values (make-filecomment :start start 
                                                           :SRC (stream-segment stream  start (file-position stream)) 
                                                           :END (file-position stream) :BLOCK-P nil)))))
      (do ((red (READ *standard-input* nil :eof)(READ *standard-input* nil :eof))) ((equal red :eof))
        (incf forms)
        (setq *code-src* (append *code-src* (list red)))))
    (format t "(:LOADED (forms ~S) (lcomments ~S) (bcomments ~S) (functions ~S))~%"  forms lcomments  bcomments functions )
    (SET-MACRO-CHARACTER #\; lncmt)
    (SET-DISPATCH-MACRO-CHARACTER #\# #\|  blockcmt)    
    (SET-DISPATCH-MACRO-CHARACTER #\# #\'  fnreader)))

(defun stream-segment (stream start end)
  (let ((finalpos (FILE-POSITION stream))
        (buffer (make-string (- end start))))
    (file-position stream start)
    (do ((charpos 0 (+ charpos 1))(start start (+ start 1)))
        ((>= start end)(file-position stream finalpos))
      (setf (char buffer charpos) (read-char stream)))
    (values buffer))) 

;;(set-pprint-dispatch 'symbol (defun print-filecomment (pstream object) (debug)))

 
(defun print-filecomment (pstream object)
    (cond
    ((filecomment-block-p object)
     (princ "#|" pstream) 
     (princ (string-trim '(#\X #\; #\Newline #\Return #\Null)
         (substitute-if #\X 
                        #'(lambda (x) (member x '(#\# #\| #\Null))) (filecomment-src object))) pstream)
     (princ "|#" pstream))
    (t
     (princ ";;" pstream) 
     (princ (string-trim '(#\Newline #\Return #\Null) 
         (substitute-if #\; 
                        #'(lambda (x) (member x '(#\# #\| #\Newline #\Return #\Null))) 
                        (string-right-trim '(#\Newline #\Return #\Null) (filecomment-src object))))
            pstream)
     (pprint-newline :fill pstream))))

(set-pprint-dispatch 'filecomment #'print-filecomment)

 (defun print-symbol (pstream sym)
        (unless (keywordp sym)
            (progn
                (princ (package-name (symbol-package sym)) pstream)
                (princ ":" pstream)))
        (princ ":" pstream)
        (princ (symbol-name sym) pstream))


 (defun print-function (pstream fn)
    pprint (list 'function (function-lambda-expression fn) pstream) )




(defun  write-src-table (file)
  (let ((rewrites 0)(total 0)(trans :EOF))
    (with-open-file (*standard-output* file :direction :output)
      ;;(set-pprint-dispatch 'symbol #'print-symbol)
      (set-pprint-dispatch 'function #'print-function)
      (dolist (red *code-src*) 
        (setq trans (translate red))
        (incf total)
        (unless (equal red trans) (incf rewrites))
        (if (typep trans 'filecomment) (pprint trans)
            (pprint (list 'TRACE-LISP trans)))))
    (format t "Wrote ~S with ~S forms (~S translated)" file total rewrites)))




;;gets reader info
(defun clean-src-file (file &optional newfile)
  (unless newfile (setq newfile (concatenate 'string file ".readable")))
  (let ((prev-pack *package*))
    ;; (defpackage :readable-lisp (:use :common-lisp) (:export #:none))
    ;; (in-package "READABLE-LISP")
    ;;(load file)
    (load-src-table file)
    ;;  (use-package prev-pack)
    (values newfile (write-src-table newfile))))


(defvar *sublisp-missing* '(defun or))
(defvar *cl-missing* '(define))
(defvar *verbatum* '(symbol-function))



(defun applicable-symbol (strf symbol &optional strl)
  (if symbol (if (symbolp symbol) (values  (intern (concatenate 'string strf (symbol-name symbol) strl) )))))

(defun applicable-function (usefnstr &optional symbol (requestpack *package*) )  
  (when (symbolp usefnstr) (setq usefnstr (symbol-name usefnstr)))
  (setq symbol (find-symbol (concatenate 'string  "CL-" usefnstr)))
  (if (fboundp symbol) (return symbol))
  (setq symbol (find-symbol usefnstr))
  (if (fboundp symbol) (ret symbol))
  (unless symbol (setq symbol (intern usefnstr)))
  (format t "do function ~A~%" symbol)
  (print (list usefnstr 'to symbol 'fboundp= (fboundp symbol) 'as symbol 'from (symbol-package symbol)))
  symbol)

#|
(defun testRet (num)
  (list
   (case 
       1 'one
     (2 (return 'two))
     (otherwise  'otherwise))))

|#

;; clisp -i sublisp-cl.lisp -repl -x '(in-package "SUBLISP-CL")' -x (translate-op 'or)

(defun prepend-op (op args)
  (let ((transop (or (translate-op op) op))
        ;;    (wrapper (op-wrapper transop)))
        (wrapper ()))
    (if wrapper 
        (cons wrapper (cons transop args))
      (cons transop args))))

(defun translate-op (op)
    (if op (translate-op-if #'(lambda (x)(equal x op)))))

(defun translate-op-if (unaryfnp)
  (or 
   (member-if unaryfnp *cl-missing*)
   (member-if unaryfnp *sublisp-missing*)
   (applicable-symbol "CL-" (member-if unaryfnp *incompatable*))
   (dolist (trans *op-translate*) 
     (when (member-if unaryfnp (car trans)) (car (second trans))))))
   

(defun form-of (this that)
  (if (or (equal (write-to-string this)(write-to-string that)) (equal this that)) that))


(defun prognitize (form)
    (if (member (car form) '(clet progn ret)) form 
        (if (and (consp form)(cdr form)(consp (car form))) (cons 'TRACE-PROGN form) (car form))))
#|
lambda-list::= (var*    [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
[&rest var] 
[&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]] 
[&aux {var | (var [init-form])}*]) 

|#
(defun transform-args-body (blockctx args body &optional (funcall nil))
    (let ((trans (transform-args-body22 blockctx args body funcall)))
        (list (car trans) 
             `(TRACE-LISP-funcall ',blockctx ,(arg-params (car trans)) ,@(cdr trans)))))

        
(defun  arg-params (seq)
    (if (consp seq)
        (remove 'nil
        (mapcar #'(lambda (x)
            (let ((xx (if (atom x) x (car x)))) 
             (if (and xx (symbolp xx) (not (char= #\& (char (symbol-name xx) 0)))) xx)))  seq))))
        

(defun zkeys-header (args bodyin) 
   (let ((body bodyin))(let ((nargs 
      (mapcar #'(lambda (x)(if (consp x) 
       (progn (setq body (cons `(init-keyval ,(car x) ,(second x)) body))(car x)) x)) args)))
       `(clet ,nargs ,@body))))
                

(defun hash-dollar-reader (stream subchar arg) (declare (ignore subchar arg))(case-sensitive-read stream t nil t))
(defun hash-comma-reader (stream subchar arg) (declare (ignore subchar arg))(list 'unquote (case-sensitive-read stream t nil t)))
(set-dispatch-macro-character #\# #\, #'hash-comma-reader)
(set-dispatch-macro-character #\# #\$ #'hash-dollar-reader)
(defun case-sensitive-read (&optional stream (eof-err-p t) eof-val rec-p)
    (let ((old-readtable-case (readtable-case *readtable*)))
      (loop
       (handler-case
           (unwind-protect
               (progn
                 (setf (readtable-case *readtable*) :preserve)
                 (return (read stream eof-err-p eof-val rec-p)))
             (setf (readtable-case *readtable*) old-readtable-case))
         (error
          (error)
          ;; FLE 25Jul2005: more understandable error message (typep and ~a)
          (cerror "Ignore error and return."
                  (if (typep error 'end-of-file)
                      "During case-sensitive-read, certainly a premature end-of-file:~%~a"
                    "During case-sensitive-read:~%~a")
                  error))))))


(defvar *incompatable* '(

 CREATE-INSTANCE 
 ISA 
 ALL-INSTANCES 
 COMMENT 
 ARITY
 LOAD-KB 
 FLATTEN 
 ASSOC-EQUAL 
 ORDERED-SET-DIFFERENCE 
 ORDERED-INTERSECTION 
 QUOTIFY 
 PERMUTE 
 TRIM-WHITESPACE 
 FIRST-CHAR 
 LAST-CHAR 
 ENDS-WITH 
 STARTS-WITH 
 STRING-TO-NUMBER 

        READ
        make-string defmacro STRING-DOWNCASE MAKE-HASH-TABLE 
        LOOP
        INTERSECTION defstruct EQUAL MEMBER REMOVE REMOVE-DUPLICATES DELETE-DUPLICATES SUBSETP ))
(defvar *op-translate* '(
                         ((let let*) (clet))
                         ((setq setq*) (csetq))
                         ((setf setf*) (csetf))
                         ((do) (cdo))
                         ((dolist) (cdolist))
                         ((dotimes) (cdotimes))
                         ;;((case) (pcase))
                         ;;((cond) (pcond))
                         ((not) (cnot))
                         ;;((if) (fif))
                         ((inc inc) (cinc))
                         ((decf dec)(cdec))
                         ;;((and)(cand))
                         ;;((or)(cor))
                         ((progn)(TRACE-PROGN))
                         ((unless)(funless))
                         ((return)(ret))
                         ((svref) (aref))
                         ((defsetf) (SUBLISP::_DEF-CSETF))
                         ((UNWIND-PROTECT)(CUNWIND-PROTECT))
                         ))

(defun transform-args-body22 (blockctx args bodyin &optional (funcall nil))
  (if (and args (atom args))
    (list args bodyin)
    (let ((nargs ())(body bodyin))
    (when (and (stringp (car body)) (cdr body)) (setq body (cdr body)))
    (when (equal (car body) 'progn) (setq body (cdr body)))
    (setq body (mapcar #'(lambda (x)(translate x 'FORM blockctx)) body))
    (do ((op (car args)(car args))(args (cdr args) (cdr args)))
      ((null op)
        (if body (list nargs `(ret ,(prognitize body))) (list nargs)))
         (setq nargs  
           (append nargs                    
             (cond         
              ((equal '&key op) (setq  body (zkeys-header args body))(setq args ())(list '&rest 'lkeys))
              ((consp op) (setq body (cons (cons 'sublisp-initvar op)  body))(list (car op)))
              (t (list op)))))))))

(defun translate ( form &optional (pattern 'FORM) blockctx (environment nil) funcall)
  (unless pattern (setq pattern 'FORM))
  (cond 
    ((functionp form) (translate (list 'function (sublisp::function-lambda-expression form)) pattern blockctx environment funcall))
    ((member form *incompatable*)(applicable-symbol "CL-" form))
    ((dolist (trans *op-translate*)(if (member form (car trans)) (return (car (second trans))))))
    ((or (null form)(typep form 'filecomment)(atom form)(stringp form)(numberp form)) form)
    (t
     (let ((op (car form))(cdrform (cdr form)))
      (let ((new-pattern (lookup-caller-pattern op)))
     (cond
      ((member op '(quote list cons)) form)
      ;;((and (member op lisptransstandardfns)(member new-pattern '((form)))) `(cl-funcall ,op ,(translate (translate (car cdrform)) pattern op environment funcall)))
      ((member op '(EVAL-WHEN)) (translate (third form) pattern blockctx environment funcall))
      ;;((and (member op '(or)) (proper-list-p cdrform));; name                              name            args               body              type
        ;;(cons 'pcond (mapcar #'(lambda (x) (list (translate x 'FORM))) cdrform)))
      ;;((and (member op *operators*) (proper-list-p cdrform));; name                              name            args               body              type
        ;;(cons 'pcond (mapcar #'(lambda (x) (list (translate x 'FORM))) cdrform)))
      ((member op '(defun));; name                                              name                   args               body             type
       (cons 'define (cons (translate (car cdrform)) (transform-args-body (translate (car cdrform)) (car (cdr cdrform)) (cdr (cdr cdrform)) :define))))
      ((member op '(defmacro));; name                                          
       (list 'defmacro (translate (car cdrform)) (car (cdr cdrform)) (list 'ret (car (translate (cdr (cdr cdrform)))))))
      ;;((member op '(function macro))    
      ;; (list 'function (translate (translate (car cdrform)) OP (gensym) environment :funcall)))
      ((member op '(lambda))        ;; name   args               body   type
       (cons OP (transform-args-body (gensym)  (car cdrform) (cdr cdrform) OP)))
      ;;((and new-pattern (equal (car new-pattern) 'pbody)) (cons (translate OP 'OP op environment :op) (translate-forms cdrform 'FORM op environment :op)))
      ;;((and(member op *incompatable*)(proper-list-p form))(mapcar #'translate form))
      ;;((and new-pattern (member (car new-pattern) '(var (:star form form) (:star var form))))
        ;;(cons (translate OP 'OP op environment :op)(cons (translate (car cdrform)) (translate-forms (cdr cdrform) 'FORM op environment :op))))
      ;;((and(equal pattern 'FORM)(symbolp op)(fboundp op))(list 'print-debug (mapcar #'translate form)))
      ((proper-list-p form)(mapcar #'translate form))
      (t form)))))))


(defun safe-wrap (form) (if (proper-list-p form) `(TRACE-LISP ,form) form))

(print (translate '(eval-when (a b c) (defun ta (x) (print x)))));;(blockctx args body &optional (funcall nil)) (let ((nargs () ))(when (and (stringp (car body)) (cdr body)) (setq body (cdr body)))(when (equal (car body) 'progn) (setq body (cdr body)))(setq body (mapcar #'(lambda (x)(translate x 'FORM blockctx)) body))(do ((op (car args)(car args))(args args (cdr args)))((null args)(list nargs `(ret (progn ,@body))))(setq nargs  (append nargs (list (cond         ((consp op) (setq body (cons (cons 'sublisp-initvar op)  body))(car op))(t op))))))))))

(defun clisp-symbol (package name) )



;;(load "kmp.lisp")
;;(load-src-table "kmp.lisp")
;;(write-src-table "kmp.subl")
(load-src-table "V4.lisp")
(write-src-table "V4.subl")
;;(write-src-table "sublisp-cl.subl")

