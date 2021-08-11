---+ C-Stack avoidance workplan

| function/predicate | time | Notes |
| term_hash/2 | 4 |
| Standard unification | 4 | incl. Performance enhancements to term-agenda code |
| ground/1 | 2 | More term-agenda enhancements |
| unify_with_occurs_check/2 | 2 |
| numbervars | 1 |
| bagof/setof | 3 | incl. speedup of template analysis. |
| markAtomsRecord() | 1 |
| copy_record() | 1 |
| copy_term/2 and friends | 32 |
| eval_expression | 10 |
| (a)cyclic_term | 16 |
| Read/Write | 16 | Simplified, reduced stack usage of read to 1/3th |