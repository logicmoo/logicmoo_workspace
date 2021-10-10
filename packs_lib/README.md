# SYSTEM application packs

This directory is attached  to  SWISH and ALL  as  a  provider  for application
specific packages. This works particularly  well   for  add-ons that are
available as a git repository as we can   register  the package as a git
submodule and thus have full control over the version.

````
total 152
drwxr-xr-x  5 root     root     4096 Feb 21 02:54 aleph
drwxr-xr-x  5 root     root     4096 Feb 21 03:11 arouter
drwxr-xr-x  6 root     root     4096 Feb 21 03:02 assertions
drwxr-xr-x  4 root     root     4096 Feb 21 02:58 auc
drwxr-xr-x  3 root     root     4096 Feb 21 03:11 blog_core
drwxr-xr-x  7 root     root     4096 Feb 21 03:16 cplint
drwxr-xr-x  7 root     root     4096 Feb 21 03:14 cplint_r
drwxr-xr-x  4 root     root     4096 Feb 21 03:11 dict_schema
drwxr-xr-x  4 root     root     4096 Feb 21 03:11 docstore
drwxr-xr-x  4 root     root     4096 Feb 22 01:09 Downloads
drwxr-xr-x 10 root     root     4096 Feb 21 04:09 ffi
drwxr-xr-x  4 root     root     4096 Feb 21 03:09 func
drwxr-xr-x  3 root     root     4096 Feb 21 03:09 function_expansion
drwxr-xr-x  8 logicmoo www-data 4096 Feb 20 00:15 hdt
drwxr-xr-x  3 root     root     4096 Feb 21 03:04 lambda
drwxr-xr-x  7 root     root     4096 Feb 22 01:05 lbfgs
drwxr-xr-x  4 root     root     4096 Feb 21 03:09 list_util
drwxr-xr-x  4 root     root     4096 Feb 21 03:09 logtalk
drwxr-xr-x  4 root     root     4096 Feb 21 03:11 markdown
drwxr-xr-x  4 root     root     4096 Feb 21 02:58 matrix
drwxr-xr-x  5 root     root     4096 Feb 21 03:00 mpi
drwxr-xr-x  4 logicmoo www-data 4096 Feb 20 00:17 pcache
drwxr-xr-x  4 logicmoo www-data 4096 Feb 20 00:15 profile
-rw-r--r--  1 logicmoo www-data  301 Feb 20 00:13 README.md
drwxr-xr-x  8 root     root     4096 Feb 21 03:32 real
drwxr-xr-x  7 root     root     4096 Feb 22 01:09 rocksdb
drwxr-xr-x  6 logicmoo www-data 4096 Feb 20 00:15 rserve_client
drwxr-xr-x  6 root     root     4096 Feb 21 03:02 rtchecks
drwxr-xr-x  9 root     root     4096 Feb 21 03:40 serd-0.26.0
drwxr-xr-x  4 root     root     4096 Feb 21 03:11 simple_template
drwxr-xr-x  4 root     root     4096 Feb 21 03:32 sldnfdraw
drwxr-xr-x  3 logicmoo www-data 4096 Feb 20 00:15 smtp
drwxr-xr-x  4 root     root     4096 Feb 21 03:11 sort_dict
drwxr-xr-x  7 root     root     4096 Feb 21 03:17 trill
drwxr-xr-x  3 logicmoo www-data 4096 Feb 20 00:15 wordnet
drwxr-xr-x  5 root     root     4096 Feb 21 03:02 xlibrary
drwxr-xr-x  5 root     root     4096 Feb 21 03:02 xtools
drwxr-xr-x  5 logicmoo www-data 4096 Jan 17 07:10 yesbot
````

````
root@gitlab:/opt/logicmoo_workspace/packs_lib# swipl -f /dev/null -p pack=.
Welcome to SWI-Prolog (threaded, 64 bits, version 7.7.10-4-g74d6ac4-DIRTY)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- pack_list_installed.
Installed packages (34):

i aleph@5                   - Aleph Inductive Logic Prorgramming system
i arouter@1.1.1             - Alternative HTTP path router
i assertions@0.0.1          - Assertion Reader for SWI-Prolog
i auc@1.0                   - Library for computing Areas Under the Receiving Operating Charactersitics and Precision Recall curves
i blog_core@1.1.1           - Blog/CMS framework
i cplint@3.1.0              - A suite of programs for reasoning with probabilistic logic programs
i cplint_r@1.0.0            - R plotting predicates for cplint
i dict_schema@0.0.2         - Dict converter and validator
i docstore@2.0.1            - Document-oriented database for Prolog
i ffi@0.1                   - Dynamically call C functions
i func@0.4.2                - Function application and composition
i function_expansion@0.1.2  - Help for writing function-like macros
i hdt@0.1                   - Access RDF HDT files
i lambda@1.0.0              - Lambda expressions
i lbfgs@1.0                 - An interface to call libLBFGS from within SWI-Prolog
i list_util@0.12.0          - Predicates for working with lists
i logtalk@3.14.0            - Logtalk - Object-Oriented Logic Programming Language
i markdown@0.0.2            - Markdown parser for SWI-Prolog
i matrix@1.0                - Operations with matrices
i mpi@1.0                   - Porting of the LAMMPI library of Yap Prolog to SWI-Prolog
i pcache@0.1.0              - Persistent answer cache
i profile@0.1.0             - Manage user profiles
i real@2.0                  - Integrative statistics with R
i rocksdb@0.7.0             - SWI-Prolog interface to RocksDB
i rserve_client@1.1.0       - R Rserve client
i rtchecks@0.0.1            - Run-Time Checker for Assertions
i simple_template@1.2.0     - Logic-free text (HTML) templates
i sldnfdraw@1.6             - SLDNF Draw is a Prolog program that draws SLDNF Trees in LaTeX
i smtp@1.0.0                - An (E)SMTP client for sending mail
i sort_dict@0.0.3           - Sorts dicts by key
i trill@5.0.0               - A tableau probabilistic reasoner in three different versions
i wordnet@0.9.1             - Access to WordNet database
i xlibrary@0.0.2            - Extended Libraries for Prolog
i xtools@0.0.2              - Extended Tools for SWI-Prolog
true.

?-

````

````bash
linuxOnly/bddem/
linuxOnly/cplint/
linuxOnly/cplint_r/
linuxOnly/hdt/
linuxOnly/phil/
linuxOnly/phil_datasets/
linuxOnly/rocksdb/
linuxOnly/serd-0.26.0/
linuxOnly/trill/
````

