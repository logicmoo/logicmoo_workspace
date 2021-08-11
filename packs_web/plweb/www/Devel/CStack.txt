---+ Avoiding using the C-stack for traversing terms

SWI-Prolog (like many other Prolog systems) uses C recursive functions
to traverse terms.  This is problematic because

  1. There is no good portable way to catch stack overflow and recover
     the status before traversing the term started.
  2. The C-stack is often quite limited in size.  Even if this is not
     the case, each _thread_ needs to reserve the stack in the virtual
     address space.  This is painful for many-threaded applications on
     32-bit hardware.

The table below lists Prolog predicates whose C implementation contains
recursive functions operating on terms. There are two classes of
functions.

  * The functions in *bold* describe fully recursive C-functions
  * The other functions are functions that perform Last Argument
  Optimization (LAO): they do not create a new stack frame
  for processing the last argument. This implies they pose no limit on
  structures nested on the last argument. Lists is the common example of
  such terms.

This table is created using the tools in =|src/tools|= in the source
distribution.  These tools require a recent Linux system and GCC.

Last updated for version: V5.11.22-78-g19d83e4

| $add_directive_wic/1 | *do_save_qlf_term* |
| $clause_term_position/3 | *find_if_then_end* |
| $record_clause/2 | analyseVariables2, compileArgument, *compileBody* |
| $record_clause/3 | analyseVariables2, compileArgument, *compileBody* |
| asserta/1 | analyseVariables2, compileArgument, *compileBody* |
| asserta/2 | analyseVariables2, compileArgument, *compileBody* |
| assertz/1 | analyseVariables2, compileArgument, *compileBody* |
| assertz/2 | analyseVariables2, compileArgument, *compileBody* |
| atom_to_term/3 | *complex_term*, *writeTerm* |
| format/2 | *writeTerm* |
| format/3 | *writeTerm* |
| instance/2 | *decompileBody* |
| print/1 | *writeTerm* |
| print/2 | *writeTerm* |
| read/1 | *complex_term* |
| read/2 | *complex_term* |
| read_clause/1 | *complex_term* |
| read_clause/2 | *complex_term* |
| read_term/2 | *complex_term* |
| read_term/3 | *complex_term* |
| term_hash/2 | *termHashValue* |
| term_hash/4 | *termHashValue* |
| term_to_atom/2 | *complex_term*, *writeTerm* |
| write/1 | *writeTerm* |
| write/2 | *writeTerm* |
| write_canonical/1 | *writeTerm* |
| write_canonical/2 | *writeTerm* |
| write_term/2 | *writeTerm* |
| write_term/3 | *writeTerm* |
| writeq/1 | *writeTerm* |
| writeq/2 | *writeTerm* |
