
:- module(module_meta_pred_tst,[colon_module_tst/1,zero_module_tst/1,plus_module_tst/1,non_module_tst/1,i_am_a_pear/1,apple:i_am_an_apple/1,user:i_am_an_orange/1]).

:- add_import_module(apple,module_meta_pred_tst,end).
:- add_import_module(orange,module_meta_pred_tst,end).

:- include(meta_pred_tst).


end_of_file.



true.

apple:  ?- [meta_pred_tst_each].
module_meta_pred_tst:colon_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=module_meta_pred_tst

module_meta_pred_tst:colon_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=module_meta_pred_tst

colon_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

colon_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=module_meta_pred_tst

user:colon_module_tst(i_am_an_apple(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:14:
        '<meta-call>'/1: Undefined procedure: colon_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:14:
        Goal (directive) failed: apple:(writeq(user:colon_module_tst(i_am_an_apple(0))),nl,user:colon_module_tst(i_am_an_apple(0)),nl)
user:colon_module_tst(user:i_am_an_apple(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:16:
        '<meta-call>'/1: Undefined procedure: colon_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:16:
        Goal (directive) failed: apple:(writeq(user:colon_module_tst(user:i_am_an_apple(0))),nl,user:colon_module_tst(user:i_am_an_apple(0)),nl)
colon_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

colon_module_tst(user:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=user

apple:colon_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

apple:colon_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

colon_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

colon_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

orange:colon_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=orange

orange:colon_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=orange

colon_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

colon_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=orange

module_meta_pred_tst:colon_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=module_meta_pred_tst

module_meta_pred_tst:colon_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=module_meta_pred_tst

colon_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

colon_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=module_meta_pred_tst

user:colon_module_tst(i_am_an_orange(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:46:
        '<meta-call>'/1: Undefined procedure: colon_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:46:
        Goal (directive) failed: apple:(writeq(user:colon_module_tst(i_am_an_orange(0))),nl,user:colon_module_tst(i_am_an_orange(0)),nl)
user:colon_module_tst(user:i_am_an_orange(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:48:
        '<meta-call>'/1: Undefined procedure: colon_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:48:
        Goal (directive) failed: apple:(writeq(user:colon_module_tst(user:i_am_an_orange(0))),nl,user:colon_module_tst(user:i_am_an_orange(0)),nl)
colon_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

colon_module_tst(user:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=user

apple:colon_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

apple:colon_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

colon_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

colon_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

orange:colon_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=orange

orange:colon_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=orange

colon_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

colon_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=orange

module_meta_pred_tst:colon_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=module_meta_pred_tst

module_meta_pred_tst:colon_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=module_meta_pred_tst

colon_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

colon_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=module_meta_pred_tst

user:colon_module_tst(i_am_a_pear(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:78:
        '<meta-call>'/1: Undefined procedure: colon_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:78:
        Goal (directive) failed: apple:(writeq(user:colon_module_tst(i_am_a_pear(0))),nl,user:colon_module_tst(i_am_a_pear(0)),nl)
user:colon_module_tst(user:i_am_a_pear(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:80:
        '<meta-call>'/1: Undefined procedure: colon_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:80:
        Goal (directive) failed: apple:(writeq(user:colon_module_tst(user:i_am_a_pear(0))),nl,user:colon_module_tst(user:i_am_a_pear(0)),nl)
colon_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

colon_module_tst(user:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=user

apple:colon_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

apple:colon_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

colon_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

colon_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

orange:colon_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=orange

orange:colon_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=orange

colon_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=apple

colon_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst
m=orange

module_meta_pred_tst:zero_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:zero_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

user:zero_module_tst(i_am_an_apple(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:110:
        '<meta-call>'/1: Undefined procedure: zero_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:110:
        Goal (directive) failed: apple:(writeq(user:zero_module_tst(i_am_an_apple(0))),nl,user:zero_module_tst(i_am_an_apple(0)),nl)
user:zero_module_tst(user:i_am_an_apple(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:112:
        '<meta-call>'/1: Undefined procedure: zero_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:112:
        Goal (directive) failed: apple:(writeq(user:zero_module_tst(user:i_am_an_apple(0))),nl,user:zero_module_tst(user:i_am_an_apple(0)),nl)
zero_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(user:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:zero_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:zero_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:zero_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:zero_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:zero_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:zero_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

user:zero_module_tst(i_am_an_orange(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:142:
        '<meta-call>'/1: Undefined procedure: zero_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:142:
        Goal (directive) failed: apple:(writeq(user:zero_module_tst(i_am_an_orange(0))),nl,user:zero_module_tst(i_am_an_orange(0)),nl)
user:zero_module_tst(user:i_am_an_orange(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:144:
        '<meta-call>'/1: Undefined procedure: zero_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:144:
        Goal (directive) failed: apple:(writeq(user:zero_module_tst(user:i_am_an_orange(0))),nl,user:zero_module_tst(user:i_am_an_orange(0)),nl)
zero_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(user:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:zero_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:zero_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:zero_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:zero_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:zero_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:zero_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

user:zero_module_tst(i_am_a_pear(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:174:
        '<meta-call>'/1: Undefined procedure: zero_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:174:
        Goal (directive) failed: apple:(writeq(user:zero_module_tst(i_am_a_pear(0))),nl,user:zero_module_tst(i_am_a_pear(0)),nl)
user:zero_module_tst(user:i_am_a_pear(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:176:
        '<meta-call>'/1: Undefined procedure: zero_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:176:
        Goal (directive) failed: apple:(writeq(user:zero_module_tst(user:i_am_a_pear(0))),nl,user:zero_module_tst(user:i_am_a_pear(0)),nl)
zero_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(user:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:zero_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:zero_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:zero_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:zero_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

zero_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:plus_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:plus_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

user:plus_module_tst(i_am_an_apple(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:206:
        '<meta-call>'/1: Undefined procedure: plus_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:206:
        Goal (directive) failed: apple:(writeq(user:plus_module_tst(i_am_an_apple(0))),nl,user:plus_module_tst(i_am_an_apple(0)),nl)
user:plus_module_tst(user:i_am_an_apple(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:208:
        '<meta-call>'/1: Undefined procedure: plus_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:208:
        Goal (directive) failed: apple:(writeq(user:plus_module_tst(user:i_am_an_apple(0))),nl,user:plus_module_tst(user:i_am_an_apple(0)),nl)
plus_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(user:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:plus_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:plus_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:plus_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:plus_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:plus_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:plus_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

user:plus_module_tst(i_am_an_orange(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:238:
        '<meta-call>'/1: Undefined procedure: plus_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:238:
        Goal (directive) failed: apple:(writeq(user:plus_module_tst(i_am_an_orange(0))),nl,user:plus_module_tst(i_am_an_orange(0)),nl)
user:plus_module_tst(user:i_am_an_orange(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:240:
        '<meta-call>'/1: Undefined procedure: plus_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:240:
        Goal (directive) failed: apple:(writeq(user:plus_module_tst(user:i_am_an_orange(0))),nl,user:plus_module_tst(user:i_am_an_orange(0)),nl)
plus_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(user:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:plus_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:plus_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:plus_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:plus_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:plus_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:plus_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

user:plus_module_tst(i_am_a_pear(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:270:
        '<meta-call>'/1: Undefined procedure: plus_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:270:
        Goal (directive) failed: apple:(writeq(user:plus_module_tst(i_am_a_pear(0))),nl,user:plus_module_tst(i_am_a_pear(0)),nl)
user:plus_module_tst(user:i_am_a_pear(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:272:
        '<meta-call>'/1: Undefined procedure: plus_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:272:
        Goal (directive) failed: apple:(writeq(user:plus_module_tst(user:i_am_a_pear(0))),nl,user:plus_module_tst(user:i_am_a_pear(0)),nl)
plus_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(user:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:plus_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:plus_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:plus_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:plus_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

plus_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:non_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:non_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

user:non_module_tst(i_am_an_apple(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:302:
        '<meta-call>'/1: Undefined procedure: non_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:302:
        Goal (directive) failed: apple:(writeq(user:non_module_tst(i_am_an_apple(0))),nl,user:non_module_tst(i_am_an_apple(0)),nl)
user:non_module_tst(user:i_am_an_apple(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:304:
        '<meta-call>'/1: Undefined procedure: non_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:304:
        Goal (directive) failed: apple:(writeq(user:non_module_tst(user:i_am_an_apple(0))),nl,user:non_module_tst(user:i_am_an_apple(0)),nl)
non_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(user:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:non_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:non_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:non_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:non_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:non_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:non_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

user:non_module_tst(i_am_an_orange(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:334:
        '<meta-call>'/1: Undefined procedure: non_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:334:
        Goal (directive) failed: apple:(writeq(user:non_module_tst(i_am_an_orange(0))),nl,user:non_module_tst(i_am_an_orange(0)),nl)
user:non_module_tst(user:i_am_an_orange(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:336:
        '<meta-call>'/1: Undefined procedure: non_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:336:
        Goal (directive) failed: apple:(writeq(user:non_module_tst(user:i_am_an_orange(0))),nl,user:non_module_tst(user:i_am_an_orange(0)),nl)
non_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(user:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:non_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:non_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:non_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:non_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:non_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

module_meta_pred_tst:non_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

user:non_module_tst(i_am_a_pear(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:366:
        '<meta-call>'/1: Undefined procedure: non_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:366:
        Goal (directive) failed: apple:(writeq(user:non_module_tst(i_am_a_pear(0))),nl,user:non_module_tst(i_am_a_pear(0)),nl)
user:non_module_tst(user:i_am_a_pear(0))
ERROR: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:368:
        '<meta-call>'/1: Undefined procedure: non_module_tst/1
Warning: /opt/PrologMUD/pack/logicmoo_base/t/examples/bugs/meta_pred_tst_each.pl:368:
        Goal (directive) failed: apple:(writeq(user:non_module_tst(user:i_am_a_pear(0))),nl,user:non_module_tst(user:i_am_a_pear(0)),nl)
non_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(user:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:non_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

apple:non_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:non_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

orange:non_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

non_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=apple
'$current_typein_module'=apple
context_module=module_meta_pred_tst

true.

apple:  ?-

