

module_body([ function_group_nodes( [ [ false : async, comments:[],decorators:[]],


                                            alias_name_node( "identity",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("x")],[argument_type("Any")]),
                                                 block_statements([expr_value(string_value(' identity function ')),return_value("x")])))]),
              
                                            alias_name_node( "add",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' addition ')),
                                                                           if_test_body_orelse(
                                                                              bool_op_values( ['python:And'], [
                                                                                call_func_args("isinstance",["a","int"]),
                                                                                call_func_args("isinstance",["b","int"])]),
                                                                              body_stmts([return_value(bin_op_left_right(add_token(+),"a","b"))]),
                                                                              orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            bool_op_values( ['python:And'], [
                                                                                                              call_func_args("isinstance",["a","tuple"]),
                                                                                                              call_func_args("isinstance",["b","tuple"])]),
                                                                                                            body_stmts( [ return_value( [ bin_op_left_right(add_token(+),subscript_value_slice("a",0),subscript_value_slice("b",0)),
                                                                                                                                                      bin_op_left_right(add_token(+),subscript_value_slice("a",1),subscript_value_slice("b",1))])])
                                                                                                         ,  orelse_else_stmts( [ if_test_body(
                                                                                                                                          bool_op_values( ['python:And'], [
                                                                                                                                            call_func_args("isinstance",["a","int"]),
                                                                                                                                            call_func_args("isinstance",["b","tuple"])]),
                                                                                                                                          body_stmts( [ return_value( [ bin_op_left_right(add_token(+),"a",subscript_value_slice("b",0)),
                                                                                                                                                                                    bin_op_left_right(add_token(+),"a",subscript_value_slice("b",1))])])
                                                                                                                                       )])
                                                                                                         )])
                                                                         ),
                                                                           return_value( [ bin_op_left_right(add_token(+),subscript_value_slice("a",0),"b"),
                                                                                                 bin_op_left_right(add_token(+),subscript_value_slice("a",1),"b")])
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "subtract",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' subtraction ')),
                                                                           if_test_body_orelse(
                                                                              bool_op_values( ['python:And'], [
                                                                                call_func_args("isinstance",["a","int"]),
                                                                                call_func_args("isinstance",["b","int"])]),
                                                                              body_stmts([return_value(bin_op_left_right(sub_token(-),"a","b"))]),
                                                                              orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            bool_op_values( ['python:And'], [
                                                                                                              call_func_args("isinstance",["a","tuple"]),
                                                                                                              call_func_args("isinstance",["b","tuple"])]),
                                                                                                            body_stmts( [ return_value( [ bin_op_left_right(sub_token(-),subscript_value_slice("a",0),subscript_value_slice("b",0)),
                                                                                                                                                      bin_op_left_right(sub_token(-),subscript_value_slice("a",1),subscript_value_slice("b",1))])])
                                                                                                         ,  orelse_else_stmts( [ if_test_body(
                                                                                                                                          bool_op_values( ['python:And'], [
                                                                                                                                            call_func_args("isinstance",["a","int"]),
                                                                                                                                            call_func_args("isinstance",["b","tuple"])]),
                                                                                                                                          body_stmts( [ return_value( [ bin_op_left_right(sub_token(-),"a",subscript_value_slice("b",0)),
                                                                                                                                                                                    bin_op_left_right(sub_token(-),"a",subscript_value_slice("b",1))])])
                                                                                                                                       )])
                                                                                                         )])
                                                                         ),
                                                                           return_value( [ bin_op_left_right(sub_token(-),subscript_value_slice("a",0),"b"),
                                                                                                 bin_op_left_right(sub_token(-),subscript_value_slice("a",1),"b")])
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "multiply",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' multiplication ')),
                                                                           if_test_body_orelse(
                                                                              bool_op_values( ['python:And'], [
                                                                                call_func_args("isinstance",["a","int"]),
                                                                                call_func_args("isinstance",["b","int"])]),
                                                                              body_stmts([return_value(bin_op_left_right(mult_token(*),"a","b"))]),
                                                                              orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            bool_op_values( ['python:And'], [
                                                                                                              call_func_args("isinstance",["a","tuple"]),
                                                                                                              call_func_args("isinstance",["b","tuple"])]),
                                                                                                            body_stmts( [ return_value( [ bin_op_left_right(mult_token(*),subscript_value_slice("a",0),subscript_value_slice("b",0)),
                                                                                                                                                      bin_op_left_right(mult_token(*),subscript_value_slice("a",1),subscript_value_slice("b",1))])])
                                                                                                         ,  orelse_else_stmts( [ if_test_body(
                                                                                                                                          bool_op_values( ['python:And'], [
                                                                                                                                            call_func_args("isinstance",["a","int"]),
                                                                                                                                            call_func_args("isinstance",["b","tuple"])]),
                                                                                                                                          body_stmts( [ return_value( [ bin_op_left_right(mult_token(*),"a",subscript_value_slice("b",0)),
                                                                                                                                                                                    bin_op_left_right(mult_token(*),"a",subscript_value_slice("b",1))])])
                                                                                                                                       )])
                                                                                                         )])
                                                                         ),
                                                                           return_value( [ bin_op_left_right(mult_token(*),subscript_value_slice("a",0),"b"),
                                                                                                 bin_op_left_right(mult_token(*),subscript_value_slice("a",1),"b")])
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "divide",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' floor division ')),
                                                                           if_test_body_orelse(
                                                                              bool_op_values( ['python:And'], [
                                                                                call_func_args("isinstance",["a","int"]),
                                                                                call_func_args("isinstance",["b","int"])]),
                                                                              body_stmts([return_value(bin_op_left_right(floor_div_token(//),"a","b"))]),
                                                                              orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            bool_op_values( ['python:And'], [
                                                                                                              call_func_args("isinstance",["a","tuple"]),
                                                                                                              call_func_args("isinstance",["b","tuple"])]),
                                                                                                            body_stmts( [ return_value( [ bin_op_left_right(floor_div_token(//),subscript_value_slice("a",0),subscript_value_slice("b",0)),
                                                                                                                                                      bin_op_left_right(floor_div_token(//),subscript_value_slice("a",1),subscript_value_slice("b",1))])])
                                                                                                         ,  orelse_else_stmts( [ if_test_body(
                                                                                                                                          bool_op_values( ['python:And'], [
                                                                                                                                            call_func_args("isinstance",["a","int"]),
                                                                                                                                            call_func_args("isinstance",["b","tuple"])]),
                                                                                                                                          body_stmts( [ return_value( [ bin_op_left_right(floor_div_token(//),"a",subscript_value_slice("b",0)),
                                                                                                                                                                                    bin_op_left_right(floor_div_token(//),"a",subscript_value_slice("b",1))])])
                                                                                                                                       )])
                                                                                                         )])
                                                                         ),
                                                                           return_value( [ bin_op_left_right(floor_div_token(//),subscript_value_slice("a",0),"b"),
                                                                                                 bin_op_left_right(floor_div_token(//),subscript_value_slice("a",1),"b")])
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "invert",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("n")],[argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' inversion with respect to addition ')),
                                                                           return_value( if_exp_test_body_orelse(
                                                                                                  call_func_args("isinstance",["n","int"]),
                                                                                                  unary_op_operand(us_ub_token(-),"n"),
                                                                                                  [ unary_op_operand(us_ub_token(-),subscript_value_slice("n",0)),
                                                                                                    unary_op_operand(us_ub_token(-),subscript_value_slice("n",1))]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "even",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("n")],[argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' evenness ')),
                                                                           return_value( compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                 bin_op_left_right(mod_token('%'),"n",2),
                                                                                                 comparators([0])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "double",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("n")],[argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' scaling by two ')),
                                                                           return_value( if_exp_test_body_orelse(
                                                                                                  call_func_args("isinstance",["n","int"]),
                                                                                                  bin_op_left_right(mult_token(*),"n",2),
                                                                                                  [ bin_op_left_right(mult_token(*),subscript_value_slice("n",0),2),
                                                                                                    bin_op_left_right(mult_token(*),subscript_value_slice("n",1),2)]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "halve",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("n")],[argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' scaling by one half ')),
                                                                           return_value( if_exp_test_body_orelse(
                                                                                                  call_func_args("isinstance",["n","int"]),
                                                                                                  bin_op_left_right(floor_div_token(//),"n",2),
                                                                                                  [ bin_op_left_right(floor_div_token(//),subscript_value_slice("n",0),2),
                                                                                                    bin_op_left_right(floor_div_token(//),subscript_value_slice("n",1),2)]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "flip",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("b")],[argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' logical not ')),
                                                                           return_value(unary_op_operand(['python:Not'],"b"))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "equality",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' equality ')),
                                                                           return_value(compare_ops_left_comparators(ops([eq_token(==)]),"a",comparators(["b"])))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "contained",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("value"),
                                                      argument_name("container")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' element of ')),
                                                                           return_value( compare_ops_left_comparators(ops([['python:In']]),"value",comparators(["container"])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "combine",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Container")]),
                                                 block_statements( [ expr_value(string_value(' union ')),
                                                                           return_value( call_func_args(call_func_args("type",["a"]),[[starred_value("a"),starred_value("b")]]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "intersection",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' returns the intersection of two containers ')),
                                                                           return_value(bin_op_left_right(bit_and_token(&),"a","b"))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "difference",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' set difference ')),
                                                                           return_value(bin_op_left_right(sub_token(-),"a","b"))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "dedupe",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("tup")],[argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' remove duplicates ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators( "e", [
                                                                                                   comprehension_ifs_target_iter(
                                                                                                      [ compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                          call_func_args(qualified_identifier_identifiers(["tup",boxed_attribute_value("index")]),["e"]),
                                                                                                          comparators(["i"]))],
                                                                                                      ["i","e"],
                                                                                                      call_func_args("enumerate",["tup"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "order",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("compfunc")],
                                                    [argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' order container by custom key ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 call_func_args_keywords("sorted",["container"],[keyword_value("compfunc")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "repeat",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("item"),
                                                      argument_name("num")],
                                                    [argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' repetition of item within vector ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators("item",[comprehension_target_iter("i",call_func_args("range",["num"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "greater",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' greater ')),
                                                                           return_value(compare_ops_left_comparators(ops([gt_token(>)]),"a",comparators(["b"])))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "size",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' cardinality ')),
                                                                           return_value(call_func_args("len",["container"]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "merge",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("containers")],[argument_type("Container")]),
                                                 block_statements( [ expr_value(string_value(' merging ')),
                                                                           return_value( call_func_args(
                                                                                                  call_func_args("type",["containers"]),
                                                                                                  [ generator_exp_elt_generators( "e", [
                                                                                                      comprehension_target_iter("c","containers"),
                                                                                                      comprehension_target_iter("e","c")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "maximum",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' maximum ')),
                                                                           return_value(call_func_args_keywords("max",["container"],[keyword_value(0)]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "minimum",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' minimum ')),
                                                                           return_value(call_func_args_keywords("min",["container"],[keyword_value(0)]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "valmax",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("compfunc")],
                                                    [argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' maximum by custom function ')),
                                                                           return_value( call_func_args( "compfunc", [
                                                                                                 call_func_args_keywords("max",["container"],[keyword_value("compfunc"),keyword_value(0)])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "valmin",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("compfunc")],
                                                    [argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' minimum by custom function ')),
                                                                           return_value( call_func_args( "compfunc", [
                                                                                                 call_func_args_keywords("min",["container"],[keyword_value("compfunc"),keyword_value(0)])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "argmax",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("compfunc")],
                                                    [argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' largest item by custom order ')),
                                                                           return_value(call_func_args_keywords("max",["container"],[keyword_value("compfunc")]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "argmin",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("compfunc")],
                                                    [argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' smallest item by custom order ')),
                                                                           return_value(call_func_args_keywords("min",["container"],[keyword_value("compfunc")]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "mostcommon",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' most common item ')),
                                                                           return_value( call_func_args_keywords( "max",
                                                                                                 [ call_func_args("set",["container"])],
                                                                                                 [ keyword_value(qualified_identifier_identifiers(["container",boxed_attribute_value("count")]))]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "leastcommon",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' least common item ')),
                                                                           return_value( call_func_args_keywords( "min",
                                                                                                 [ call_func_args("set",["container"])],
                                                                                                 [ keyword_value(qualified_identifier_identifiers(["container",boxed_attribute_value("count")]))]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "initset",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("value")],[argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' initialize container ')),
                                                                           return_value(call_func_args("frozenset",[set_elts(["value"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "both",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' logical and ')),
                                                                           return_value(bool_op_values(['python:And'],["a","b"]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "either",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' logical or ')),
                                                                           return_value(bool_op_values(['python:Or'],["a","b"]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "increment",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("x")],[argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' incrementing ')),
                                                                           return_value( if_exp_test_body_orelse(
                                                                                                  call_func_args("isinstance",["x","int"]),
                                                                                                  bin_op_left_right(add_token(+),"x",1),
                                                                                                  [ bin_op_left_right(add_token(+),subscript_value_slice("x",0),1),
                                                                                                    bin_op_left_right(add_token(+),subscript_value_slice("x",1),1)]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "decrement",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("x")],[argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' decrementing ')),
                                                                           return_value( if_exp_test_body_orelse(
                                                                                                  call_func_args("isinstance",["x","int"]),
                                                                                                  bin_op_left_right(sub_token(-),"x",1),
                                                                                                  [ bin_op_left_right(sub_token(-),subscript_value_slice("x",0),1),
                                                                                                    bin_op_left_right(sub_token(-),subscript_value_slice("x",1),1)]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "crement",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("x")],[argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' incrementing positive and decrementing negative ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["x","int"]),
                                                                              body_stmts( [ return_value( if_exp_test_body_orelse(
                                                                                                                         compare_ops_left_comparators(ops([eq_token(==)]),"x",comparators([0])),
                                                                                                                         0,
                                                                                                                         if_exp_test_body_orelse(
                                                                                                                            compare_ops_left_comparators(ops([gt_token(>)]),"x",comparators([0])),
                                                                                                                            bin_op_left_right(add_token(+),"x",1),
                                                                                                                            bin_op_left_right(sub_token(-),"x",1))))])
                                                                         ),
                                                                           return_value( [ if_exp_test_body_orelse(
                                                                                                    compare_ops_left_comparators(ops([eq_token(==)]),subscript_value_slice("x",0),comparators([0])),
                                                                                                    0,
                                                                                                    if_exp_test_body_orelse(
                                                                                                       compare_ops_left_comparators(ops([gt_token(>)]),subscript_value_slice("x",0),comparators([0])),
                                                                                                       bin_op_left_right(add_token(+),subscript_value_slice("x",0),1),
                                                                                                       bin_op_left_right(sub_token(-),subscript_value_slice("x",0),1))),
                                                                                                 if_exp_test_body_orelse(
                                                                                                    compare_ops_left_comparators(ops([eq_token(==)]),subscript_value_slice("x",1),comparators([0])),
                                                                                                    0,
                                                                                                    if_exp_test_body_orelse(
                                                                                                       compare_ops_left_comparators(ops([gt_token(>)]),subscript_value_slice("x",1),comparators([0])),
                                                                                                       bin_op_left_right(add_token(+),subscript_value_slice("x",1),1),
                                                                                                       bin_op_left_right(sub_token(-),subscript_value_slice("x",1),1)))])
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "sign",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("x")],[argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' sign ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["x","int"]),
                                                                              body_stmts( [ return_value( if_exp_test_body_orelse(
                                                                                                                         compare_ops_left_comparators(ops([eq_token(==)]),"x",comparators([0])),
                                                                                                                         0,
                                                                                                                         if_exp_test_body_orelse(
                                                                                                                            compare_ops_left_comparators(ops([gt_token(>)]),"x",comparators([0])), 1,unary_op_operand(us_ub_token(-),1))))])
                                                                         ),
                                                                           return_value( [ if_exp_test_body_orelse(
                                                                                                    compare_ops_left_comparators(ops([eq_token(==)]),subscript_value_slice("x",0),comparators([0])),
                                                                                                    0,
                                                                                                    if_exp_test_body_orelse(
                                                                                                       compare_ops_left_comparators(ops([gt_token(>)]),subscript_value_slice("x",0),comparators([0])), 1,unary_op_operand(us_ub_token(-),1))),
                                                                                                 if_exp_test_body_orelse(
                                                                                                    compare_ops_left_comparators(ops([eq_token(==)]),subscript_value_slice("x",1),comparators([0])),
                                                                                                    0,
                                                                                                    if_exp_test_body_orelse(
                                                                                                       compare_ops_left_comparators(ops([gt_token(>)]),subscript_value_slice("x",1),comparators([0])), 1,unary_op_operand(us_ub_token(-),1)))])
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "positive",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("x")],[argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' positive ')),
                                                                           return_value(compare_ops_left_comparators(ops([gt_token(>)]),"x",comparators([0])))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "toivec",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("i")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' vector pointing vertically ')),
                                                                           return_value(["i",0])])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "tojvec",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("j")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' vector pointing horizontally ')),
                                                                           return_value([0,"j"])])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "sfilter",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("condition")],
                                                    [argument_type("Container")]),
                                                 block_statements( [ expr_value(string_value(' keep elements in container that satisfy condition ')),
                                                                           return_value( call_func_args(
                                                                                                  call_func_args("type",["container"]),
                                                                                                  [ generator_exp_elt_generators( "e", [
                                                                                                      comprehension_ifs_target_iter([call_func_args("condition",["e"])],"e","container")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "mfilter",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("function")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' filter and merge ')),
                                                                           return_value(call_func_args("merge",[call_func_args("sfilter",["container","function"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "extract",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("condition")],
                                                    [argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' first element of container that satisfies condition ')),
                                                                           return_value( call_func_args( "next", [
                                                                                                 generator_exp_elt_generators( "e", [
                                                                                                   comprehension_ifs_target_iter([call_func_args("condition",["e"])],"e","container")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "totuple",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' conversion to tuple ')),
                                                                           return_value(call_func_args("tuple",["container"]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "first",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' first item of container ')),
                                                                           return_value(call_func_args("next",[call_func_args("iter",["container"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "last",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' last item of container ')),
                                                                           return_value( subscript_value_slice(call_func_args("max",[call_func_args("enumerate",["container"])]),1))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "insert",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("value"),
                                                      argument_name("container")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' insert item into container ')),
                                                                           return_value( call_func_args(
                                                                                                  qualified_identifier_identifiers(["container",boxed_attribute_value("union")]),
                                                                                                  [ call_func_args("frozenset",[set_elts(["value"])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "remove",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("value"),
                                                      argument_name("container")],
                                                    [argument_type("Container")]),
                                                 block_statements( [ expr_value(string_value(' remove item from container ')),
                                                                           return_value( call_func_args(
                                                                                                  call_func_args("type",["container"]),
                                                                                                  [ generator_exp_elt_generators( "e", [
                                                                                                      comprehension_ifs_target_iter(
                                                                                                         [ compare_ops_left_comparators(ops([not_eq_token('!=')]),"e",comparators(["value"]))], "e","container")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "other",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("value")],
                                                    [argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' other value in the container ')),
                                                                           return_value(call_func_args("first",[call_func_args("remove",["value","container"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "interval",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("start"), argument_name("stop"),argument_name("step")],
                                                    [argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' range ')),
                                                                           return_value(call_func_args("tuple",[call_func_args("range",["start","stop","step"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "astuple",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' constructs a tuple ')),
                                                                           return_value(["a","b"])])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "product",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' cartesian product ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators( ["i","j"], [
                                                                                                   comprehension_target_iter("j","b"),
                                                                                                   comprehension_target_iter("i","a")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "pair",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("TupleTuple")]),
                                                 block_statements( [ expr_value(string_value(' zipping of two tuples ')),
                                                                           return_value(call_func_args("tuple",[call_func_args("zip",["a","b"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "branch",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("condition"), argument_name("a"),argument_name("b")],
                                                    [argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' if else branching ')),
                                                                           return_value(if_exp_test_body_orelse("condition","a","b"))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "compose",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("outer"),
                                                      argument_name("inner")],
                                                    [argument_type("Callable")]),
                                                 block_statements( [ expr_value(string_value(' function composition ')),
                                                                           return_value( lambda_args_body( arguments_args([argument_name("x")]),
                                                                                                 body_stmts(call_func_args("outer",[call_func_args("inner",["x"])]))))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "chain",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("h"), argument_name("g"),argument_name("f")],
                                                    [argument_type("Callable")]),
                                                 block_statements( [ expr_value(string_value(' function composition with three functions ')),
                                                                           return_value( lambda_args_body( arguments_args([argument_name("x")]),
                                                                                                 body_stmts(call_func_args("h",[call_func_args("g",[call_func_args("f",["x"])])]))))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "matcher",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"),
                                                      argument_name("target")],
                                                    [argument_type("Callable")]),
                                                 block_statements( [ expr_value(string_value(' construction of equality function ')),
                                                                           return_value( lambda_args_body( arguments_args([argument_name("x")]),
                                                                                                 body_stmts( compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                     call_func_args("function",["x"]),
                                                                                                                     comparators(["target"])))))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "rbind",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"),
                                                      argument_name("fixed")],
                                                    [argument_type("Callable")]),
                                                 block_statements( [ expr_value(string_value(' fix the rightmost argument ')),
                                                                           assign_targets_value( ["n"],
                                                                             qualified_identifier_identifiers( [ "function",
                                                                                                                       boxed_attribute_value("__code__"),
                                                                                                                       boxed_attribute_value("co_argcount")])),
                                                                           if_test_body_orelse(
                                                                              compare_ops_left_comparators(ops([eq_token(==)]),"n",comparators([2])),
                                                                              body_stmts( [ return_value( lambda_args_body( arguments_args([argument_name("x")]),
                                                                                                                        body_stmts(call_func_args("function",["x","fixed"]))))])
                                                                         ,    orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            compare_ops_left_comparators(ops([eq_token(==)]),"n",comparators([3])),
                                                                                                            body_stmts( [ return_value( lambda_args_body(
                                                                                                                                                       arguments_args([argument_name("x"),argument_name("y")]),
                                                                                                                                                       body_stmts(call_func_args("function",["x","y","fixed"]))))])
                                                                                                         ,  orelse_else_stmts( [ return_value( lambda_args_body(
                                                                                                                                                              arguments_args([argument_name("x"),argument_name("y"),argument_name("z")]),
                                                                                                                                                              body_stmts(call_func_args("function",["x","y","z","fixed"]))))])
                                                                                                         )])
                                                                         )])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "lbind",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"),
                                                      argument_name("fixed")],
                                                    [argument_type("Callable")]),
                                                 block_statements( [ expr_value(string_value(' fix the leftmost argument ')),
                                                                           assign_targets_value( ["n"],
                                                                             qualified_identifier_identifiers( [ "function",
                                                                                                                       boxed_attribute_value("__code__"),
                                                                                                                       boxed_attribute_value("co_argcount")])),
                                                                           if_test_body_orelse(
                                                                              compare_ops_left_comparators(ops([eq_token(==)]),"n",comparators([2])),
                                                                              body_stmts( [ return_value( lambda_args_body( arguments_args([argument_name("y")]),
                                                                                                                        body_stmts(call_func_args("function",["fixed","y"]))))])
                                                                         ,    orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            compare_ops_left_comparators(ops([eq_token(==)]),"n",comparators([3])),
                                                                                                            body_stmts( [ return_value( lambda_args_body(
                                                                                                                                                       arguments_args([argument_name("y"),argument_name("z")]),
                                                                                                                                                       body_stmts(call_func_args("function",["fixed","y","z"]))))])
                                                                                                         ,  orelse_else_stmts( [ return_value( lambda_args_body(
                                                                                                                                                              arguments_args([argument_name("y"),argument_name("z"),argument_name("a")]),
                                                                                                                                                              body_stmts(call_func_args("function",["fixed","y","z","a"]))))])
                                                                                                         )])
                                                                         )])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "power",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"),
                                                      argument_name("n")],
                                                    [argument_type("Callable")]),
                                                 block_statements( [ expr_value(string_value(' power of function ')),
                                                                           if_test_body(
                                                                              compare_ops_left_comparators(ops([eq_token(==)]),"n",comparators([1])),
                                                                              body_stmts([return_value("function")])),
                                                                           return_value( call_func_args( "compose", [
                                                                                                 "function",
                                                                                                 call_func_args("power",["function",bin_op_left_right(sub_token(-),"n",1)])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "fork",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("outer"), argument_name("a"),argument_name("b")],
                                                    [argument_type("Callable")]),
                                                 block_statements( [ expr_value(string_value(' creates a wrapper function ')),
                                                                           return_value( lambda_args_body( arguments_args([argument_name("x")]),
                                                                                                 body_stmts( call_func_args("outer",[call_func_args("a",["x"]),call_func_args("b",["x"])]))))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "apply",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"),
                                                      argument_name("container")],
                                                    [argument_type("Container")]),
                                                 block_statements( [ expr_value(string_value(' apply function to each item in container ')),
                                                                           return_value( call_func_args(
                                                                                                  call_func_args("type",["container"]),
                                                                                                  [ generator_exp_elt_generators(
                                                                                                       call_func_args("function",["e"]),
                                                                                                       [comprehension_target_iter("e","container")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "rapply",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("functions"),
                                                      argument_name("value")],
                                                    [argument_type("Container")]),
                                                 block_statements( [ expr_value(string_value(' apply each function in container to value ')),
                                                                           return_value( call_func_args(
                                                                                                  call_func_args("type",["functions"]),
                                                                                                  [ generator_exp_elt_generators(
                                                                                                       call_func_args("function",["value"]),
                                                                                                       [comprehension_target_iter("function","functions")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "mapply",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"),
                                                      argument_name("container")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' apply and merge ')),
                                                                           return_value(call_func_args("merge",[call_func_args("apply",["function","container"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "papply",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"), argument_name("a"),argument_name("b")],
                                                    [argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' apply function on two vectors ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args("function",["i","j"]),
                                                                                                    [ comprehension_target_iter(["i","j"],call_func_args("zip",["a","b"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "mpapply",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"), argument_name("a"),argument_name("b")],
                                                    [argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' apply function on two vectors and merge ')),
                                                                           return_value(call_func_args("merge",[call_func_args("papply",["function","a","b"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "prapply",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"), argument_name("a"),argument_name("b")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' apply function on cartesian product ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args("function",["i","j"]),
                                                                                                    [ comprehension_target_iter("j","b"),
                                                                                                      comprehension_target_iter("i","a")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "mostcolor",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("element")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' most common color ')),
                                                                           assign_targets_value( ["values"],
                                                                             if_exp_test_body_orelse(
                                                                                call_func_args("isinstance",["element","tuple"]),
                                                                                list_comp_elt_generators( "v", [
                                                                                  comprehension_target_iter("r","element"),
                                                                                  comprehension_target_iter("v","r")]),
                                                                                list_comp_elt_generators("v",[comprehension_target_iter(["v","_"],"element")]))),
                                                                           return_value( call_func_args_keywords( "max",
                                                                                                 [ call_func_args("set",["values"])],
                                                                                                 [ keyword_value(qualified_identifier_identifiers(["values",boxed_attribute_value("count")]))]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "leastcolor",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("element")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' least common color ')),
                                                                           assign_targets_value( ["values"],
                                                                             if_exp_test_body_orelse(
                                                                                call_func_args("isinstance",["element","tuple"]),
                                                                                list_comp_elt_generators( "v", [
                                                                                  comprehension_target_iter("r","element"),
                                                                                  comprehension_target_iter("v","r")]),
                                                                                list_comp_elt_generators("v",[comprehension_target_iter(["v","_"],"element")]))),
                                                                           return_value( call_func_args_keywords( "min",
                                                                                                 [ call_func_args("set",["values"])],
                                                                                                 [ keyword_value(qualified_identifier_identifiers(["values",boxed_attribute_value("count")]))]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "height",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' height of grid or patch ')),
                                                                           if_test_body(
                                                                              compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                call_func_args("len",["piece"]),
                                                                                comparators([0])),
                                                                              body_stmts([return_value(0)])),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["piece","tuple"]),
                                                                              body_stmts([return_value(call_func_args("len",["piece"]))])),
                                                                           return_value( bin_op_left_right( add_token(+),
                                                                                                 bin_op_left_right( sub_token(-),
                                                                                                   call_func_args("lowermost",["piece"]),
                                                                                                   call_func_args("uppermost",["piece"])),
                                                                                                 1))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "width",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' width of grid or patch ')),
                                                                           if_test_body(
                                                                              compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                call_func_args("len",["piece"]),
                                                                                comparators([0])),
                                                                              body_stmts([return_value(0)])),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["piece","tuple"]),
                                                                              body_stmts([return_value(call_func_args("len",[subscript_value_slice("piece",0)]))])),
                                                                           return_value( bin_op_left_right( add_token(+),
                                                                                                 bin_op_left_right( sub_token(-),
                                                                                                   call_func_args("rightmost",["piece"]),
                                                                                                   call_func_args("leftmost",["piece"])),
                                                                                                 1))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "shape",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' height and width of grid or patch ')),
                                                                           return_value([call_func_args("height",["piece"]),call_func_args("width",["piece"])])])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "portrait",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether height is greater than width ')),
                                                                           return_value( compare_ops_left_comparators( ops([gt_token(>)]),
                                                                                                 call_func_args("height",["piece"]),
                                                                                                 comparators([call_func_args("width",["piece"])])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "colorcount",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("element"),
                                                      argument_name("value")],
                                                    [argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' number of cells with color ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["element","tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "sum", [
                                                                                                                        generator_exp_elt_generators(
                                                                                                                           call_func_args(
                                                                                                                              qualified_identifier_identifiers(["row",boxed_attribute_value("count")]),
                                                                                                                              ["value"]),
                                                                                                                           [comprehension_target_iter("row","element")])]))])
                                                                         ),
                                                                           return_value( call_func_args( "sum", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    compare_ops_left_comparators(ops([eq_token(==)]),"v",comparators(["value"])),
                                                                                                    [ comprehension_target_iter(["v","_"],"element")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "colorfilter",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("objs"),
                                                      argument_name("value")],
                                                    [argument_type("Objects")]),
                                                 block_statements( [ expr_value(string_value(' filter objects by color ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators( "obj", [
                                                                                                   comprehension_ifs_target_iter(
                                                                                                      [ compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                          subscript_value_slice(call_func_args("next",[call_func_args("iter",["obj"])]),0),
                                                                                                          comparators(["value"]))], "obj","objs")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "sizefilter",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("n")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' filter items by size ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators( "item", [
                                                                                                   comprehension_ifs_target_iter(
                                                                                                      [ compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                          call_func_args("len",["item"]),
                                                                                                          comparators(["n"]))], "item","container")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "asindices",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' indices of all grid cells ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators( ["i","j"], [
                                                                                                   comprehension_target_iter("i",call_func_args("range",[call_func_args("len",["grid"])])),
                                                                                                   comprehension_target_iter( "j",
                                                                                                     call_func_args("range",[call_func_args("len",[subscript_value_slice("grid",0)])]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "ofcolor",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("value")],
                                                    [argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' indices of all grid cells with value ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators( ["i","j"], [
                                                                                                   comprehension_target_iter(["i","r"],call_func_args("enumerate",["grid"])),
                                                                                                   comprehension_ifs_target_iter(
                                                                                                      [ compare_ops_left_comparators(ops([eq_token(==)]),"v",comparators(["value"]))],
                                                                                                      ["j","v"],
                                                                                                      call_func_args("enumerate",["r"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "ulcorner",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' index of upper left corner ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 call_func_args( "map", [
                                                                                                   "min",
                                                                                                   call_func_args("zip",[starred_value(call_func_args("toindices",["patch"]))])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "urcorner",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' index of upper right corner ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 call_func_args( "map", [
                                                                                                   lambda_args_body( arguments_args([argument_name("ix")]),
                                                                                                     body_stmts( call_func_args(
                                                                                                                          subscript_value_slice(
                                                                                                                             dict_keys_values([0,1],["min","max"]),
                                                                                                                             subscript_value_slice("ix",0)),
                                                                                                                          [subscript_value_slice("ix",1)]))),
                                                                                                   call_func_args( "enumerate", [
                                                                                                     call_func_args("zip",[starred_value(call_func_args("toindices",["patch"]))])])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "llcorner",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' index of lower left corner ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 call_func_args( "map", [
                                                                                                   lambda_args_body( arguments_args([argument_name("ix")]),
                                                                                                     body_stmts( call_func_args(
                                                                                                                          subscript_value_slice(
                                                                                                                             dict_keys_values([0,1],["max","min"]),
                                                                                                                             subscript_value_slice("ix",0)),
                                                                                                                          [subscript_value_slice("ix",1)]))),
                                                                                                   call_func_args( "enumerate", [
                                                                                                     call_func_args("zip",[starred_value(call_func_args("toindices",["patch"]))])])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "lrcorner",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' index of lower right corner ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 call_func_args( "map", [
                                                                                                   "max",
                                                                                                   call_func_args("zip",[starred_value(call_func_args("toindices",["patch"]))])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "crop",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"), argument_name("start"),argument_name("dims")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' subgrid specified by start and dimension ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    subscript_value_slice( "r",
                                                                                                      slice_lower_upper( subscript_value_slice("start",1),
                                                                                                        bin_op_left_right(add_token(+),subscript_value_slice("start",1),subscript_value_slice("dims",1)))),
                                                                                                    [ comprehension_target_iter( "r",
                                                                                                        subscript_value_slice( "grid",
                                                                                                          slice_lower_upper( subscript_value_slice("start",0),
                                                                                                            bin_op_left_right(add_token(+),subscript_value_slice("start",0),subscript_value_slice("dims",0)))))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "toindices",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' indices of object cells ')),
                                                                           if_test_body(
                                                                              compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                call_func_args("len",["patch"]),
                                                                                comparators([0])),
                                                                              body_stmts([return_value(call_func("frozenset"))])),
                                                                           if_test_body(
                                                                              call_func_args( "isinstance", [
                                                                                subscript_value_slice(call_func_args("next",[call_func_args("iter",["patch"])]),1),
                                                                                "tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                        generator_exp_elt_generators("index",[comprehension_target_iter(["value","index"],"patch")])]))])
                                                                         ),
                                                                           return_value("patch")])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "recolor",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("value"),
                                                      argument_name("patch")],
                                                    [argument_type("Object")]),
                                                 block_statements( [ expr_value(string_value(' recolor patch ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators( ["value","index"], [
                                                                                                   comprehension_target_iter("index",call_func_args("toindices",["patch"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "shift",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("patch"),
                                                      argument_name("directions")],
                                                    [argument_type("Patch")]),
                                                 block_statements( [ expr_value(string_value(' shift patch ')),
                                                                           assign_targets_value([["di","dj"]],"directions"),
                                                                           if_test_body(
                                                                              call_func_args( "isinstance", [
                                                                                subscript_value_slice(call_func_args("next",[call_func_args("iter",["patch"])]),1),
                                                                                "tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                        generator_exp_elt_generators(
                                                                                                                           [ "value",
                                                                                                                             [ bin_op_left_right(add_token(+),"i","di"),
                                                                                                                               bin_op_left_right(add_token(+),"j","dj")]],
                                                                                                                           [ comprehension_target_iter(["value",["i","j"]],"patch")])]))])
                                                                         ),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ bin_op_left_right(add_token(+),"i","di"),
                                                                                                      bin_op_left_right(add_token(+),"j","dj")],
                                                                                                    [ comprehension_target_iter(["i","j"],"patch")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "normalize",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Patch")]),
                                                 block_statements( [ expr_value(string_value(' moves upper left corner to origin ')),
                                                                           return_value( call_func_args( "shift", [
                                                                                                 "patch",
                                                                                                 [ unary_op_operand(us_ub_token(-),call_func_args("uppermost",["patch"])),
                                                                                                   unary_op_operand(us_ub_token(-),call_func_args("leftmost",["patch"]))]]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "dneighbors",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("loc")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' directly adjacent indices ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 set_elts( [ [ bin_op_left_right(sub_token(-),subscript_value_slice("loc",0),1),
                                                                                                                     subscript_value_slice("loc",1)],
                                                                                                                   [ bin_op_left_right(add_token(+),subscript_value_slice("loc",0),1),
                                                                                                                     subscript_value_slice("loc",1)],
                                                                                                                   [ subscript_value_slice("loc",0),
                                                                                                                     bin_op_left_right(sub_token(-),subscript_value_slice("loc",1),1)],
                                                                                                                   [ subscript_value_slice("loc",0),
                                                                                                                     bin_op_left_right(add_token(+),subscript_value_slice("loc",1),1)]])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "ineighbors",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("loc")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' diagonally adjacent indices ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 set_elts( [ [ bin_op_left_right(sub_token(-),subscript_value_slice("loc",0),1),
                                                                                                                     bin_op_left_right(sub_token(-),subscript_value_slice("loc",1),1)],
                                                                                                                   [ bin_op_left_right(sub_token(-),subscript_value_slice("loc",0),1),
                                                                                                                     bin_op_left_right(add_token(+),subscript_value_slice("loc",1),1)],
                                                                                                                   [ bin_op_left_right(add_token(+),subscript_value_slice("loc",0),1),
                                                                                                                     bin_op_left_right(sub_token(-),subscript_value_slice("loc",1),1)],
                                                                                                                   [ bin_op_left_right(add_token(+),subscript_value_slice("loc",0),1),
                                                                                                                     bin_op_left_right(add_token(+),subscript_value_slice("loc",1),1)]])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "neighbors",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("loc")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' adjacent indices ')),
                                                                           return_value( bin_op_left_right( bit_or_token('|'),
                                                                                                 call_func_args("dneighbors",["loc"]),
                                                                                                 call_func_args("ineighbors",["loc"])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "objects",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"), argument_name("univalued"),argument_name("diagonal"),
                                                      argument_name("without_bg")],
                                                    [argument_type("Objects")]),
                                                 block_statements( [ expr_value(string_value(' objects occurring on the grid ')),
                                                                           assign_targets_value( ["bg"],
                                                                             if_exp_test_body_orelse( "without_bg",
                                                                               call_func_args("mostcolor",["grid"]),
                                                                               none_literal_value_token('None','None'))),
                                                                           assign_targets_value(["objs"],call_func("set")),
                                                                           assign_targets_value(["occupied"],call_func("set")),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value(["unvisited"],call_func_args("asindices",["grid"])),
                                                                           assign_targets_value(["diagfun"],if_exp_test_body_orelse("diagonal","neighbors","dneighbors")),
                                                                           for_target_iter_body( "loc",
                                                                             "unvisited",
                                                                             body_stmts( [ if_test_body(
                                                                                                    compare_ops_left_comparators(ops([['python:In']]),"loc",comparators(["occupied"])),
                                                                                                    body_stmts([['python:Continue']])),
                                                                                                 assign_targets_value( ["val"],
                                                                                                   subscript_value_slice(subscript_value_slice("grid",subscript_value_slice("loc",0)),subscript_value_slice("loc",1))),
                                                                                                 if_test_body(
                                                                                                    compare_ops_left_comparators(ops([eq_token(==)]),"val",comparators(["bg"])),
                                                                                                    body_stmts([['python:Continue']])),
                                                                                                 assign_targets_value(["obj"],set_elts([["val","loc"]])),
                                                                                                 assign_targets_value(["cands"],set_elts(["loc"])),
                                                                                                 while_test_body(
                                                                                                    compare_ops_left_comparators( ops([gt_token(>)]),
                                                                                                      call_func_args("len",["cands"]),
                                                                                                      comparators([0])),
                                                                                                    body_stmts( [ assign_targets_value(["neighborhood"],call_func("set")),
                                                                                                                        for_target_iter_body( "cand",
                                                                                                                          "cands",
                                                                                                                          body_stmts( [ assign_targets_value( ["v"],
                                                                                                                                                subscript_value_slice(subscript_value_slice("grid",subscript_value_slice("cand",0)),subscript_value_slice("cand",1))),
                                                                                                                                              if_test_body(
                                                                                                                                                 if_exp_test_body_orelse( "univalued",
                                                                                                                                                   compare_ops_left_comparators(ops([eq_token(==)]),"val",comparators(["v"])),
                                                                                                                                                   compare_ops_left_comparators(ops([not_eq_token('!=')]),"v",comparators(["bg"]))),
                                                                                                                                                 body_stmts( [ expr_value( call_func_args(
                                                                                                                                                                                          qualified_identifier_identifiers(["obj",boxed_attribute_value("add")]),
                                                                                                                                                                                          [ [  "v"  ,"cand"]])),
                                                                                                                                                                     expr_value( call_func_args(
                                                                                                                                                                                          qualified_identifier_identifiers(["occupied",boxed_attribute_value("add")]),
                                                                                                                                                                                          ["cand"]))
                                                                                                                                                                   , aug_assign_op_target_value( bit_or_token('|'),
                                                                                                                                                                       "neighborhood",
                                                                                                                                                                       set_comp_elt_generators( ["i","j"], [
                                                                                                                                                                         comprehension_ifs_target_iter(
                                                                                                                                                                            [ bool_op_values( ['python:And'], [
                                                                                                                                                                                compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
                                                                                                                                                                                compare_ops_left_comparators(
                                                                                                                                                                                   ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])],
                                                                                                                                                                            ["i","j"],
                                                                                                                                                                            call_func_args("diagfun",["cand"]))]))])
                                                                                                                                            )])
                                                                                                                      ),
                                                                                                                        assign_targets_value(["cands"],bin_op_left_right(sub_token(-),"neighborhood","occupied"))])
                                                                                               ),
                                                                                                 expr_value( call_func_args(
                                                                                                                      qualified_identifier_identifiers(["objs",boxed_attribute_value("add")]),
                                                                                                                      [ call_func_args("frozenset",["obj"])]))
                                                                                               ])
                                                                         ),
                                                                           return_value(call_func_args("frozenset",["objs"]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "partition",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Objects")]),
                                                 block_statements( [ expr_value(string_value(' each cell with the same value part of the same object ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "frozenset", [
                                                                                                      generator_exp_elt_generators(
                                                                                                         [ "v",
                                                                                                           ["i","j"]],
                                                                                                         [ comprehension_target_iter(["i","r"],call_func_args("enumerate",["grid"])),
                                                                                                           comprehension_ifs_target_iter(
                                                                                                              [ compare_ops_left_comparators(ops([eq_token(==)]),"v",comparators(["value"]))],
                                                                                                              ["j","v"],
                                                                                                              call_func_args("enumerate",["r"]))])]),
                                                                                                    [ comprehension_target_iter("value",call_func_args("palette",["grid"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "fgpartition",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Objects")]),
                                                 block_statements( [ expr_value(string_value(' each cell with the same value part of the same object without background ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "frozenset", [
                                                                                                      generator_exp_elt_generators(
                                                                                                         [ "v",
                                                                                                           ["i","j"]],
                                                                                                         [ comprehension_target_iter(["i","r"],call_func_args("enumerate",["grid"])),
                                                                                                           comprehension_ifs_target_iter(
                                                                                                              [ compare_ops_left_comparators(ops([eq_token(==)]),"v",comparators(["value"]))],
                                                                                                              ["j","v"],
                                                                                                              call_func_args("enumerate",["r"]))])]),
                                                                                                    [ comprehension_target_iter( "value",
                                                                                                        bin_op_left_right( sub_token(-),
                                                                                                          call_func_args("palette",["grid"]),
                                                                                                          set_elts([call_func_args("mostcolor",["grid"])])))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "uppermost",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' row index of uppermost occupied cell ')),
                                                                           return_value( call_func_args( "min", [
                                                                                                 generator_exp_elt_generators( "i", [
                                                                                                   comprehension_target_iter(["i","j"],call_func_args("toindices",["patch"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "lowermost",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' row index of lowermost occupied cell ')),
                                                                           return_value( call_func_args( "max", [
                                                                                                 generator_exp_elt_generators( "i", [
                                                                                                   comprehension_target_iter(["i","j"],call_func_args("toindices",["patch"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "leftmost",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' column index of leftmost occupied cell ')),
                                                                           return_value( call_func_args( "min", [
                                                                                                 generator_exp_elt_generators( "j", [
                                                                                                   comprehension_target_iter(["i","j"],call_func_args("toindices",["patch"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "rightmost",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' column index of rightmost occupied cell ')),
                                                                           return_value( call_func_args( "max", [
                                                                                                 generator_exp_elt_generators( "j", [
                                                                                                   comprehension_target_iter(["i","j"],call_func_args("toindices",["patch"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "square",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether the piece forms a square ')),
                                                                           return_value( if_exp_test_body_orelse(
                                                                                                  call_func_args("isinstance",["piece","tuple"]),
                                                                                                  compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                    call_func_args("len",["piece"]),
                                                                                                    comparators([call_func_args("len",[subscript_value_slice("piece",0)])])),
                                                                                                  bool_op_values( ['python:And'], [
                                                                                                    compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                      bin_op_left_right(mult_token(*),call_func_args("height",["piece"]),call_func_args("width",["piece"])),
                                                                                                      comparators([call_func_args("len",["piece"])])),
                                                                                                    compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                      call_func_args("height",["piece"]),
                                                                                                      comparators([call_func_args("width",["piece"])]))])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vline",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether the piece forms a vertical line ')),
                                                                           return_value( bool_op_values( ['python:And'], [
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("height",["patch"]),
                                                                                                   comparators([call_func_args("len",["patch"])])),
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("width",["patch"]),
                                                                                                   comparators([1]))]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hline",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether the piece forms a horizontal line ')),
                                                                           return_value( bool_op_values( ['python:And'], [
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("width",["patch"]),
                                                                                                   comparators([call_func_args("len",["patch"])])),
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("height",["patch"]),
                                                                                                   comparators([1]))]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hmatching",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether there exists a row for which both patches have cells ')),
                                                                           return_value( compare_ops_left_comparators( ops([gt_token(>)]),
                                                                                                 call_func_args( "len", [
                                                                                                   bin_op_left_right( bit_and_token(&),
                                                                                                     call_func_args( "set", [
                                                                                                       generator_exp_elt_generators( "i", [
                                                                                                         comprehension_target_iter(["i","j"],call_func_args("toindices",["a"]))])]),
                                                                                                     call_func_args( "set", [
                                                                                                       generator_exp_elt_generators( "i", [
                                                                                                         comprehension_target_iter(["i","j"],call_func_args("toindices",["b"]))])]))]),
                                                                                                 comparators([0])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vmatching",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether there exists a column for which both patches have cells ')),
                                                                           return_value( compare_ops_left_comparators( ops([gt_token(>)]),
                                                                                                 call_func_args( "len", [
                                                                                                   bin_op_left_right( bit_and_token(&),
                                                                                                     call_func_args( "set", [
                                                                                                       generator_exp_elt_generators( "j", [
                                                                                                         comprehension_target_iter(["i","j"],call_func_args("toindices",["a"]))])]),
                                                                                                     call_func_args( "set", [
                                                                                                       generator_exp_elt_generators( "j", [
                                                                                                         comprehension_target_iter(["i","j"],call_func_args("toindices",["b"]))])]))]),
                                                                                                 comparators([0])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "manhattan",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' closest manhattan distance between two patches ')),
                                                                           return_value( call_func_args( "min", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    bin_op_left_right( add_token(+),
                                                                                                      call_func_args("abs",[bin_op_left_right(sub_token(-),"ai","bi")]),
                                                                                                      call_func_args("abs",[bin_op_left_right(sub_token(-),"aj","bj")])),
                                                                                                    [ comprehension_target_iter(["ai","aj"],call_func_args("toindices",["a"])),
                                                                                                      comprehension_target_iter(["bi","bj"],call_func_args("toindices",["b"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "adjacent",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether two patches are adjacent ')),
                                                                           return_value( compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                 call_func_args("manhattan",["a","b"]),
                                                                                                 comparators([1])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "bordering",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("patch"),
                                                      argument_name("grid")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether a patch is adjacent to a grid border ')),
                                                                           return_value( bool_op_values( ['python:Or'], [
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("uppermost",["patch"]),
                                                                                                   comparators([0])),
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("leftmost",["patch"]),
                                                                                                   comparators([0])),
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("lowermost",["patch"]),
                                                                                                   comparators([bin_op_left_right(sub_token(-),call_func_args("len",["grid"]),1)])),
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("rightmost",["patch"]),
                                                                                                   comparators( [ bin_op_left_right(sub_token(-),call_func_args("len",[subscript_value_slice("grid",0)]),1)])
                                                                                               )]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "centerofmass",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' center of mass ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 call_func_args( "map", [
                                                                                                   lambda_args_body( arguments_args([argument_name("x")]),
                                                                                                     body_stmts( bin_op_left_right(floor_div_token(//),call_func_args("sum",["x"]),call_func_args("len",["patch"])))),
                                                                                                   call_func_args("zip",[starred_value(call_func_args("toindices",["patch"]))])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "palette",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("element")],[argument_type("IntegerSet")]),
                                                 block_statements( [ expr_value(string_value(' colors occurring in object or grid ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["element","tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                        set_comp_elt_generators( "v", [
                                                                                                                          comprehension_target_iter("r","element"),
                                                                                                                          comprehension_target_iter("v","r")])]))])
                                                                         ),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 set_comp_elt_generators("v",[comprehension_target_iter(["v","_"],"element")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "numcolors",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("element")],[argument_type("IntegerSet")]),
                                                 block_statements( [ expr_value(string_value(' number of colors occurring in object or grid ')),
                                                                           return_value(call_func_args("len",[call_func_args("palette",["element"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "color",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("obj")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' color of object ')),
                                                                           return_value(subscript_value_slice(call_func_args("next",[call_func_args("iter",["obj"])]),0))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "toobject",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("patch"),
                                                      argument_name("grid")],
                                                    [argument_type("Object")]),
                                                 block_statements( [ expr_value(string_value(' object from patch and grid ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ subscript_value_slice(subscript_value_slice("grid","i"),"j"),
                                                                                                      ["i","j"]],
                                                                                                    [ comprehension_ifs_target_iter(
                                                                                                         [ bool_op_values( ['python:And'], [
                                                                                                             compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
                                                                                                             compare_ops_left_comparators(
                                                                                                                ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])],
                                                                                                         ["i","j"],
                                                                                                         call_func_args("toindices",["patch"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "asobject",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Object")]),
                                                 block_statements( [ expr_value(string_value(' conversion of grid to object ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ "v",
                                                                                                      ["i","j"]],
                                                                                                    [ comprehension_target_iter(["i","r"],call_func_args("enumerate",["grid"])),
                                                                                                      comprehension_target_iter(["j","v"],call_func_args("enumerate",["r"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "rot90",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' quarter clockwise rotation ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators( "row", [
                                                                                                   comprehension_target_iter( "row",
                                                                                                     call_func_args( "zip", [
                                                                                                       starred_value(subscript_value_slice("grid",slice_step(unary_op_operand(us_ub_token(-),1))))]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "rot180",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' half rotation ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args("tuple",[subscript_value_slice("row",slice_step(unary_op_operand(us_ub_token(-),1)))]),
                                                                                                    [ comprehension_target_iter("row",subscript_value_slice("grid",slice_step(unary_op_operand(us_ub_token(-),1))))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "rot270",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' quarter anticlockwise rotation ')),
                                                                           return_value( subscript_value_slice(
                                                                                                  call_func_args( "tuple", [
                                                                                                    generator_exp_elt_generators(
                                                                                                       call_func_args("tuple",[subscript_value_slice("row",slice_step(unary_op_operand(us_ub_token(-),1)))]),
                                                                                                       [ comprehension_target_iter( "row",
                                                                                                           call_func_args( "zip", [
                                                                                                             starred_value(subscript_value_slice("grid",slice_step(unary_op_operand(us_ub_token(-),1))))]))])]),
                                                                                                  slice_step(unary_op_operand(us_ub_token(-),1))))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hmirror",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Piece")]),
                                                 block_statements( [ expr_value(string_value(' mirroring along horizontal ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["piece","tuple"]),
                                                                              body_stmts([return_value(subscript_value_slice("piece",slice_step(unary_op_operand(us_ub_token(-),1))))])),
                                                                           assign_targets_value( ["d"],
                                                                             bin_op_left_right( add_token(+),
                                                                               subscript_value_slice(call_func_args("ulcorner",["piece"]),0),
                                                                               subscript_value_slice(call_func_args("lrcorner",["piece"]),0))),
                                                                           if_test_body(
                                                                              call_func_args( "isinstance", [
                                                                                subscript_value_slice(call_func_args("next",[call_func_args("iter",["piece"])]),1),
                                                                                "tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                        generator_exp_elt_generators(
                                                                                                                           [ "v",
                                                                                                                             [ bin_op_left_right(sub_token(-),"d","i"),
                                                                                                                               "j"]],
                                                                                                                           [ comprehension_target_iter(["v",["i","j"]],"piece")])]))])
                                                                         ),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ bin_op_left_right(sub_token(-),"d","i"),
                                                                                                      "j"],
                                                                                                    [ comprehension_target_iter(["i","j"],"piece")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vmirror",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Piece")]),
                                                 block_statements( [ expr_value(string_value(' mirroring along vertical ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["piece","tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "tuple", [
                                                                                                                        generator_exp_elt_generators(
                                                                                                                           subscript_value_slice("row",slice_step(unary_op_operand(us_ub_token(-),1))),
                                                                                                                           [comprehension_target_iter("row","piece")])]))])
                                                                         ),
                                                                           assign_targets_value( ["d"],
                                                                             bin_op_left_right( add_token(+),
                                                                               subscript_value_slice(call_func_args("ulcorner",["piece"]),1),
                                                                               subscript_value_slice(call_func_args("lrcorner",["piece"]),1))),
                                                                           if_test_body(
                                                                              call_func_args( "isinstance", [
                                                                                subscript_value_slice(call_func_args("next",[call_func_args("iter",["piece"])]),1),
                                                                                "tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                        generator_exp_elt_generators(
                                                                                                                           [ "v",
                                                                                                                             [ "i",
                                                                                                                               bin_op_left_right(sub_token(-),"d","j")]],
                                                                                                                           [ comprehension_target_iter(["v",["i","j"]],"piece")])]))])
                                                                         ),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ "i",
                                                                                                      bin_op_left_right(sub_token(-),"d","j")],
                                                                                                    [ comprehension_target_iter(["i","j"],"piece")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "dmirror",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Piece")]),
                                                 block_statements( [ expr_value(string_value(' mirroring along diagonal ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["piece","tuple"]),
                                                                              body_stmts( [ return_value(call_func_args("tuple",[call_func_args("zip",[starred_value("piece")])]))])
                                                                         ),
                                                                           assign_targets_value([["a","b"]],call_func_args("ulcorner",["piece"])),
                                                                           if_test_body(
                                                                              call_func_args( "isinstance", [
                                                                                subscript_value_slice(call_func_args("next",[call_func_args("iter",["piece"])]),1),
                                                                                "tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                        generator_exp_elt_generators(
                                                                                                                           [ "v",
                                                                                                                             [ bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"j","b"),"a"),
                                                                                                                               bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"i","a"),"b")]],
                                                                                                                           [ comprehension_target_iter(["v",["i","j"]],"piece")])]))])
                                                                         ),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"j","b"),"a"),
                                                                                                      bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"i","a"),"b")],
                                                                                                    [ comprehension_target_iter(["i","j"],"piece")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "cmirror",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Piece")]),
                                                 block_statements( [ expr_value(string_value(' mirroring along counterdiagonal ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["piece","tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "tuple", [
                                                                                                                        call_func_args( "zip", [
                                                                                                                          starred_value( generator_exp_elt_generators(
                                                                                                                                                  subscript_value_slice("r",slice_step(unary_op_operand(us_ub_token(-),1))),
                                                                                                                                                  [ comprehension_target_iter("r",subscript_value_slice("piece",slice_step(unary_op_operand(us_ub_token(-),1))))]))])]))])
                                                                         ),
                                                                           return_value( call_func_args("vmirror",[call_func_args("dmirror",[call_func_args("vmirror",["piece"])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "fill",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"), argument_name("value"),argument_name("patch")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' fill value at indices ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value( ["grid_filled"],
                                                                             call_func_args( "list", [
                                                                               generator_exp_elt_generators(call_func_args("list",["row"]),[comprehension_target_iter("row","grid")])])),
                                                                           for_target_iter_body( ["i","j"],
                                                                             call_func_args("toindices",["patch"]),
                                                                             body_stmts( [ if_test_body(
                                                                                                    bool_op_values( ['python:And'], [
                                                                                                      compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
                                                                                                      compare_ops_left_comparators(
                                                                                                         ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
                                                                                                    body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("grid_filled","i"),"j")],"value")])
                                                                                                 )])
                                                                         ),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args("tuple",["row"]),
                                                                                                    [comprehension_target_iter("row","grid_filled")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "paint",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("obj")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' paint object to grid ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value( ["grid_painted"],
                                                                             call_func_args( "list", [
                                                                               generator_exp_elt_generators(call_func_args("list",["row"]),[comprehension_target_iter("row","grid")])])),
                                                                           for_target_iter_body(
                                                                              [ "value",
                                                                                ["i","j"]],
                                                                              "obj",
                                                                              body_stmts( [ if_test_body(
                                                                                                     bool_op_values( ['python:And'], [
                                                                                                       compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
                                                                                                       compare_ops_left_comparators(
                                                                                                          ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
                                                                                                     body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("grid_painted","i"),"j")],"value")])
                                                                                                  )])
                                                                         ),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args("tuple",["row"]),
                                                                                                    [comprehension_target_iter("row","grid_painted")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "underfill",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"), argument_name("value"),argument_name("patch")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' fill value at indices that are background ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value(["bg"],call_func_args("mostcolor",["grid"])),
                                                                           assign_targets_value( ["g"],
                                                                             call_func_args( "list", [
                                                                               generator_exp_elt_generators(call_func_args("list",["r"]),[comprehension_target_iter("r","grid")])])),
                                                                           for_target_iter_body( ["i","j"],
                                                                             call_func_args("toindices",["patch"]),
                                                                             body_stmts( [ if_test_body(
                                                                                                    bool_op_values( ['python:And'], [
                                                                                                      compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
                                                                                                      compare_ops_left_comparators(
                                                                                                         ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
                                                                                                    body_stmts( [ if_test_body(
                                                                                                                           compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                             subscript_value_slice(subscript_value_slice("g","i"),"j"),
                                                                                                                             comparators(["bg"])),
                                                                                                                           body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],"value")])
                                                                                                                        )])
                                                                                                 )])
                                                                         ),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(call_func_args("tuple",["r"]),[comprehension_target_iter("r","g")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "underpaint",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("obj")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' paint object to grid where there is background ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value(["bg"],call_func_args("mostcolor",["grid"])),
                                                                           assign_targets_value( ["g"],
                                                                             call_func_args( "list", [
                                                                               generator_exp_elt_generators(call_func_args("list",["r"]),[comprehension_target_iter("r","grid")])])),
                                                                           for_target_iter_body(
                                                                              [ "value",
                                                                                ["i","j"]],
                                                                              "obj",
                                                                              body_stmts( [ if_test_body(
                                                                                                     bool_op_values( ['python:And'], [
                                                                                                       compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
                                                                                                       compare_ops_left_comparators(
                                                                                                          ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
                                                                                                     body_stmts( [ if_test_body(
                                                                                                                            compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                              subscript_value_slice(subscript_value_slice("g","i"),"j"),
                                                                                                                              comparators(["bg"])),
                                                                                                                            body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],"value")])
                                                                                                                         )])
                                                                                                  )])
                                                                         ),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(call_func_args("tuple",["r"]),[comprehension_target_iter("r","g")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hupscale",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("factor")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' upscale grid horizontally ')),
                                                                           assign_targets_value(["g"],call_func("tuple")),
                                                                           for_target_iter_body( "row",
                                                                             "grid",
                                                                             body_stmts( [ assign_targets_value(["r"],call_func("tuple")),
                                                                                                 for_target_iter_body( "value",
                                                                                                   "row",
                                                                                                   body_stmts( [ assign_targets_value( ["r"],
                                                                                                                         bin_op_left_right( add_token(+),
                                                                                                                           "r",
                                                                                                                           call_func_args( "tuple", [
                                                                                                                             generator_exp_elt_generators( "value", [
                                                                                                                               comprehension_target_iter("num",call_func_args("range",["factor"]))])])))])
                                                                                               ),
                                                                                                 assign_targets_value(["g"],bin_op_left_right(add_token(+),"g",["r"]))])
                                                                         ),
                                                                           return_value("g")])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vupscale",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("factor")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' upscale grid vertically ')),
                                                                           assign_targets_value(["g"],call_func("tuple")),
                                                                           for_target_iter_body( "row",
                                                                             "grid",
                                                                             body_stmts( [ assign_targets_value( ["g"],
                                                                                                   bin_op_left_right( add_token(+),
                                                                                                     "g",
                                                                                                     call_func_args( "tuple", [
                                                                                                       generator_exp_elt_generators( "row", [
                                                                                                         comprehension_target_iter("num",call_func_args("range",["factor"]))])])))])
                                                                         ),
                                                                           return_value("g")])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "upscale",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("element"),
                                                      argument_name("factor")],
                                                    [argument_type("Element")]),
                                                 block_statements( [ expr_value(string_value(' upscale object or grid ')),
                                                                           if_test_body_orelse(
                                                                              call_func_args("isinstance",["element","tuple"]),
                                                                              body_stmts( [ assign_targets_value(["g"],call_func("tuple")),
                                                                                                  for_target_iter_body( "row",
                                                                                                    "element",
                                                                                                    body_stmts( [ assign_targets_value(["upscaled_row"],call_func("tuple")),
                                                                                                                        for_target_iter_body( "value",
                                                                                                                          "row",
                                                                                                                          body_stmts( [ assign_targets_value( ["upscaled_row"],
                                                                                                                                                bin_op_left_right( add_token(+),
                                                                                                                                                  "upscaled_row",
                                                                                                                                                  call_func_args( "tuple", [
                                                                                                                                                    generator_exp_elt_generators( "value", [
                                                                                                                                                      comprehension_target_iter("num",call_func_args("range",["factor"]))])])))])
                                                                                                                      ),
                                                                                                                        assign_targets_value( ["g"],
                                                                                                                          bin_op_left_right( add_token(+),
                                                                                                                            "g",
                                                                                                                            call_func_args( "tuple", [
                                                                                                                              generator_exp_elt_generators( "upscaled_row", [
                                                                                                                                comprehension_target_iter("num",call_func_args("range",["factor"]))])])))])
                                                                                                ),
                                                                                                  return_value("g")])
                                                                         ,    orelse_else_stmts( [ if_test_body(
                                                                                                            compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                              call_func_args("len",["element"]),
                                                                                                              comparators([0])),
                                                                                                            body_stmts([return_value(call_func("frozenset"))])),
                                                                                                         assign_targets_value([["di_inv","dj_inv"]],call_func_args("ulcorner",["element"])),
                                                                                                         assign_targets_value( [ [ "di","dj"]], [
                                                                                                           unary_op_operand(us_ub_token(-),"di_inv"),
                                                                                                           unary_op_operand(us_ub_token(-),"dj_inv")]),
                                                                                                         assign_targets_value( ["normed_obj"],
                                                                                                           call_func_args("shift",["element",["di","dj"]])),
                                                                                                         assign_targets_value(["o"],call_func("set")),
                                                                                                         for_target_iter_body(
                                                                                                            [ "value",
                                                                                                              ["i","j"]],
                                                                                                            "normed_obj",
                                                                                                            body_stmts( [ for_target_iter_body( "io",
                                                                                                                                  call_func_args("range",["factor"]),
                                                                                                                                  body_stmts( [ for_target_iter_body( "jo",
                                                                                                                                                        call_func_args("range",["factor"]),
                                                                                                                                                        body_stmts( [ expr_value( call_func_args(
                                                                                                                                                                                                 qualified_identifier_identifiers(["o",boxed_attribute_value("add")]),
                                                                                                                                                                                                 [ [ "value",
                                                                                                                                                                                                     [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"i","factor"),"io"),
                                                                                                                                                                                                       bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"j","factor"),"jo")]]]))])
                                                                                                                                                      )])
                                                                                                                                )])
                                                                                                       ),
                                                                                                         return_value( call_func_args("shift",[call_func_args("frozenset",["o"]),["di_inv","dj_inv"]]))
                                                                                                       ])
                                                                         )])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "downscale",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("factor")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' downscale grid ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value(["g"],call_func("tuple")),
                                                                           for_target_iter_body( "i",
                                                                             call_func_args("range",["h"]),
                                                                             body_stmts( [ assign_targets_value(["r"],call_func("tuple")),
                                                                                                 for_target_iter_body( "j",
                                                                                                   call_func_args("range",["w"]),
                                                                                                   body_stmts( [ if_test_body(
                                                                                                                          compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                            bin_op_left_right(mod_token('%'),"j","factor"),
                                                                                                                            comparators([0])),
                                                                                                                          body_stmts( [ assign_targets_value( ["r"],
                                                                                                                                                bin_op_left_right(add_token(+),"r",[subscript_value_slice(subscript_value_slice("grid","i"),"j")]))])
                                                                                                                       )])
                                                                                               ),
                                                                                                 assign_targets_value(["g"],bin_op_left_right(add_token(+),"g",["r"]))])
                                                                         ),
                                                                           assign_targets_value(["h"],call_func_args("len",["g"])),
                                                                           assign_targets_value(["dsg"],call_func("tuple")),
                                                                           for_target_iter_body( "i",
                                                                             call_func_args("range",["h"]),
                                                                             body_stmts( [ if_test_body(
                                                                                                    compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                      bin_op_left_right(mod_token('%'),"i","factor"),
                                                                                                      comparators([0])),
                                                                                                    body_stmts( [ assign_targets_value(["dsg"],bin_op_left_right(add_token(+),"dsg",[subscript_value_slice("g","i")]))])
                                                                                                 )])
                                                                         ),
                                                                           return_value("dsg")])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hconcat",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("a"),argument_name("b")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' concatenate two grids horizontally ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    bin_op_left_right(add_token(+),"i","j"),
                                                                                                    [ comprehension_target_iter(["i","j"],call_func_args("zip",["a","b"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vconcat",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("a"),argument_name("b")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' concatenate two grids vertically ')),
                                                                           return_value(bin_op_left_right(add_token(+),"a","b"))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "subgrid",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("patch"),
                                                      argument_name("grid")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' smallest subgrid containing object ')),
                                                                           return_value( call_func_args( "crop", [
                                                                                                 "grid",
                                                                                                 call_func_args("ulcorner",["patch"]),
                                                                                                 call_func_args("shape",["patch"])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hsplit",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("n")],
                                                    [argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' split grid horizontally ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             bin_op_left_right(floor_div_token(//),call_func_args("len",[subscript_value_slice("grid",0)]),"n")]),
                                                                           assign_targets_value( ["offset"],
                                                                             compare_ops_left_comparators( ops([not_eq_token('!=')]),
                                                                               bin_op_left_right(mod_token('%'),call_func_args("len",[subscript_value_slice("grid",0)]),"n"),
                                                                               comparators([0]))),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "crop", [
                                                                                                      "grid",
                                                                                                      [ 0,
                                                                                                        bin_op_left_right( add_token(+),
                                                                                                          bin_op_left_right(mult_token(*),"w","i"),
                                                                                                          bin_op_left_right(mult_token(*),"i","offset"))],
                                                                                                      ["h","w"]]),
                                                                                                    [ comprehension_target_iter("i",call_func_args("range",["n"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vsplit",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("n")],
                                                    [argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' split grid vertically ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             bin_op_left_right(floor_div_token(//),call_func_args("len",["grid"]),"n"),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value( ["offset"],
                                                                             compare_ops_left_comparators( ops([not_eq_token('!=')]),
                                                                               bin_op_left_right(mod_token('%'),call_func_args("len",["grid"]),"n"),
                                                                               comparators([0]))),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "crop", [
                                                                                                      "grid",
                                                                                                      [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"h","i"),bin_op_left_right(mult_token(*),"i","offset")),
                                                                                                        0],
                                                                                                      ["h","w"]]),
                                                                                                    [ comprehension_target_iter("i",call_func_args("range",["n"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "cellwise",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"), argument_name("b"),argument_name("fallback")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' cellwise match of two grids ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["a"]),
                                                                             call_func_args("len",[subscript_value_slice("a",0)])]),
                                                                           assign_targets_value(["resulting_grid"],call_func("tuple")),
                                                                           for_target_iter_body( "i",
                                                                             call_func_args("range",["h"]),
                                                                             body_stmts( [ assign_targets_value(["row"],call_func("tuple")),
                                                                                                 for_target_iter_body( "j",
                                                                                                   call_func_args("range",["w"]),
                                                                                                   body_stmts( [ assign_targets_value(["a_value"],subscript_value_slice(subscript_value_slice("a","i"),"j")),
                                                                                                                       assign_targets_value( ["value"],
                                                                                                                         if_exp_test_body_orelse(
                                                                                                                            compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                              "a_value",
                                                                                                                              comparators([subscript_value_slice(subscript_value_slice("b","i"),"j")])), "a_value","fallback")),
                                                                                                                       assign_targets_value(["row"],bin_op_left_right(add_token(+),"row",["value"]))])
                                                                                               ),
                                                                                                 assign_targets_value(["resulting_grid"],bin_op_left_right(add_token(+),"resulting_grid",["row"]))])
                                                                         ),
                                                                           return_value("resulting_grid")])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "replace",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"), argument_name("replacee"),argument_name("replacer")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' color substitution ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "tuple", [
                                                                                                      generator_exp_elt_generators(
                                                                                                         if_exp_test_body_orelse(
                                                                                                            compare_ops_left_comparators(ops([eq_token(==)]),"v",comparators(["replacee"])), "replacer","v"),
                                                                                                         [comprehension_target_iter("v","r")])]),
                                                                                                    [comprehension_target_iter("r","grid")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "switch",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"), argument_name("a"),argument_name("b")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' color switching ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "tuple", [
                                                                                                      generator_exp_elt_generators(
                                                                                                         if_exp_test_body_orelse(
                                                                                                            bool_op_values( ['python:And'], [
                                                                                                              compare_ops_left_comparators(ops([not_eq_token('!=')]),"v",comparators(["a"])),
                                                                                                              compare_ops_left_comparators(ops([not_eq_token('!=')]),"v",comparators(["b"]))]),
                                                                                                            "v",
                                                                                                            subscript_value_slice(dict_keys_values(["a","b"],["b","a"]),"v")),
                                                                                                         [comprehension_target_iter("v","r")])]),
                                                                                                    [comprehension_target_iter("r","grid")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "center",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' center of the patch ')),
                                                                           return_value( [ bin_op_left_right( add_token(+),
                                                                                                   call_func_args("uppermost",["patch"]),
                                                                                                   bin_op_left_right(floor_div_token(//),call_func_args("height",["patch"]),2)),
                                                                                                 bin_op_left_right( add_token(+),
                                                                                                   call_func_args("leftmost",["patch"]),
                                                                                                   bin_op_left_right(floor_div_token(//),call_func_args("width",["patch"]),2))])
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "position",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' relative position between two patches ')),
                                                                           assign_targets_value( [ [ "ia","ja"]],
                                                                             call_func_args("center",[call_func_args("toindices",["a"])])),
                                                                           assign_targets_value( [ [ "ib","jb"]],
                                                                             call_func_args("center",[call_func_args("toindices",["b"])])),
                                                                           if_test_body_orelse(
                                                                              compare_ops_left_comparators(ops([eq_token(==)]),"ia",comparators(["ib"])),
                                                                              body_stmts( [ return_value( [ 0,
                                                                                                                        if_exp_test_body_orelse(
                                                                                                                           compare_ops_left_comparators(ops([lt_token(<)]),"ja",comparators(["jb"])), 1,unary_op_operand(us_ub_token(-),1))])])
                                                                         ,    orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            compare_ops_left_comparators(ops([eq_token(==)]),"ja",comparators(["jb"])),
                                                                                                            body_stmts( [ return_value( [ if_exp_test_body_orelse(
                                                                                                                                                         compare_ops_left_comparators(ops([lt_token(<)]),"ia",comparators(["ib"])), 1,unary_op_operand(us_ub_token(-),1)),
                                                                                                                                                      0])])
                                                                                                         ,  orelse_else_stmts( [ if_test_body_orelse(
                                                                                                                                          compare_ops_left_comparators(ops([lt_token(<)]),"ia",comparators(["ib"])),
                                                                                                                                          body_stmts( [ return_value( [ 1,
                                                                                                                                                                                    if_exp_test_body_orelse(
                                                                                                                                                                                       compare_ops_left_comparators(ops([lt_token(<)]),"ja",comparators(["jb"])), 1,unary_op_operand(us_ub_token(-),1))])])
                                                                                                                                       ,  orelse_else_stmts( [ if_test_body(
                                                                                                                                                                        compare_ops_left_comparators(ops([gt_token(>)]),"ia",comparators(["ib"])),
                                                                                                                                                                        body_stmts( [ return_value( [ unary_op_operand(us_ub_token(-),1),
                                                                                                                                                                                                                  if_exp_test_body_orelse(
                                                                                                                                                                                                                     compare_ops_left_comparators(ops([lt_token(<)]),"ja",comparators(["jb"])), 1,unary_op_operand(us_ub_token(-),1))])])
                                                                                                                                                                     )])
                                                                                                                                       )])
                                                                                                         )])
                                                                         )])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "index",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("loc")],
                                                    [argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' color at location ')),
                                                                           assign_targets_value([["i","j"]],"loc"),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           if_test_body(
                                                                              unary_op_operand( ['python:Not'],
                                                                                bool_op_values( ['python:And'], [
                                                                                  compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
                                                                                  compare_ops_left_comparators(
                                                                                     ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])),
                                                                              body_stmts([return_value(none_literal_value_token('None','None'))])),
                                                                           return_value( subscript_value_slice(subscript_value_slice("grid",subscript_value_slice("loc",0)),subscript_value_slice("loc",1)))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "canvas",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("value"),
                                                      argument_name("dimensions")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' grid construction ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "tuple", [
                                                                                                      generator_exp_elt_generators( "value", [
                                                                                                        comprehension_target_iter("j",call_func_args("range",[subscript_value_slice("dimensions",1)]))])]),
                                                                                                    [ comprehension_target_iter("i",call_func_args("range",[subscript_value_slice("dimensions",0)]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "corners",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' indices of corners ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 set_elts( [ call_func_args("ulcorner",["patch"]),
                                                                                                                   call_func_args("urcorner",["patch"]),
                                                                                                                   call_func_args("llcorner",["patch"]),
                                                                                                                   call_func_args("lrcorner",["patch"])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "connect",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' line between two points ')),
                                                                           assign_targets_value([["ai","aj"]],"a"),
                                                                           assign_targets_value([["bi","bj"]],"b"),
                                                                           assign_targets_value(["si"],call_func_args("min",["ai","bi"])),
                                                                           assign_targets_value( ["ei"],
                                                                             bin_op_left_right(add_token(+),call_func_args("max",["ai","bi"]),1)),
                                                                           assign_targets_value(["sj"],call_func_args("min",["aj","bj"])),
                                                                           assign_targets_value( ["ej"],
                                                                             bin_op_left_right(add_token(+),call_func_args("max",["aj","bj"]),1)),
                                                                           if_test_body_orelse(
                                                                              compare_ops_left_comparators(ops([eq_token(==)]),"ai",comparators(["bi"])),
                                                                              body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                        generator_exp_elt_generators( ["ai","j"], [
                                                                                                                          comprehension_target_iter("j",call_func_args("range",["sj","ej"]))])]))])
                                                                         ,    orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            compare_ops_left_comparators(ops([eq_token(==)]),"aj",comparators(["bj"])),
                                                                                                            body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                                                      generator_exp_elt_generators( ["i","aj"], [
                                                                                                                                                        comprehension_target_iter("i",call_func_args("range",["si","ei"]))])]))])
                                                                                                         ,  orelse_else_stmts( [ if_test_body_orelse(
                                                                                                                                          compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                                            bin_op_left_right(sub_token(-),"bi","ai"),
                                                                                                                                            comparators([bin_op_left_right(sub_token(-),"bj","aj")])),
                                                                                                                                          body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                                                                                    generator_exp_elt_generators( ["i","j"], [
                                                                                                                                                                                      comprehension_target_iter( ["i","j"],
                                                                                                                                                                                        call_func_args( "zip", [
                                                                                                                                                                                          call_func_args("range",["si","ei"]),
                                                                                                                                                                                          call_func_args("range",["sj","ej"])]))])]))])
                                                                                                                                       ,  orelse_else_stmts( [ if_test_body(
                                                                                                                                                                        compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                                                                          bin_op_left_right(sub_token(-),"bi","ai"),
                                                                                                                                                                          comparators([bin_op_left_right(sub_token(-),"aj","bj")])),
                                                                                                                                                                        body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                                                                                                                  generator_exp_elt_generators( ["i","j"], [
                                                                                                                                                                                                                    comprehension_target_iter( ["i","j"],
                                                                                                                                                                                                                      call_func_args( "zip", [
                                                                                                                                                                                                                        call_func_args("range",["si","ei"]),
                                                                                                                                                                                                                        call_func_args( "range", [
                                                                                                                                                                                                                          bin_op_left_right(sub_token(-),"ej",1),
                                                                                                                                                                                                                          bin_op_left_right(sub_token(-),"sj",1),
                                                                                                                                                                                                                          unary_op_operand(us_ub_token(-),1)])]))])]))])
                                                                                                                                                                     )])
                                                                                                                                       )])
                                                                                                         )])
                                                                         ),
                                                                           return_value(call_func("frozenset"))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "cover",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("patch")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' remove object from grid ')),
                                                                           return_value( call_func_args( "fill", [
                                                                                                 "grid",
                                                                                                 call_func_args("mostcolor",["grid"]),
                                                                                                 call_func_args("toindices",["patch"])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "trim",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' trim border of grid ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    subscript_value_slice("r",slice_lower_upper(1,unary_op_operand(us_ub_token(-),1))),
                                                                                                    [ comprehension_target_iter("r",subscript_value_slice("grid",slice_lower_upper(1,unary_op_operand(us_ub_token(-),1))))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "move",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"), argument_name("obj"),argument_name("offset")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' move object on grid ')),
                                                                           return_value( call_func_args( "paint", [
                                                                                                 call_func_args("cover",["grid","obj"]),
                                                                                                 call_func_args("shift",["obj","offset"])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "tophalf",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' upper half of grid ')),
                                                                           return_value( subscript_value_slice( "grid",
                                                                                                 slice_upper(bin_op_left_right(floor_div_token(//),call_func_args("len",["grid"]),2))))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "bottomhalf",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' lower half of grid ')),
                                                                           return_value( subscript_value_slice( "grid",
                                                                                                 slice_lower( bin_op_left_right( add_token(+),
                                                                                                                      bin_op_left_right(floor_div_token(//),call_func_args("len",["grid"]),2),
                                                                                                                      bin_op_left_right(mod_token('%'),call_func_args("len",["grid"]),2)))))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "lefthalf",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' left half of grid ')),
                                                                           return_value( call_func_args("rot270",[call_func_args("tophalf",[call_func_args("rot90",["grid"])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "righthalf",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' right half of grid ')),
                                                                           return_value( call_func_args("rot270",[call_func_args("bottomhalf",[call_func_args("rot90",["grid"])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vfrontier",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("location")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' vertical frontier ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ "i",
                                                                                                      subscript_value_slice("location",1)],
                                                                                                    [ comprehension_target_iter("i",call_func_args("range",[30]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hfrontier",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("location")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' horizontal frontier ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ subscript_value_slice("location",0),
                                                                                                      "j"],
                                                                                                    [ comprehension_target_iter("j",call_func_args("range",[30]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "backdrop",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' indices in bounding box of patch ')),
                                                                           assign_targets_value(["indices"],call_func_args("toindices",["patch"])),
                                                                           assign_targets_value([["si","sj"]],call_func_args("ulcorner",["indices"])),
                                                                           assign_targets_value([["ei","ej"]],call_func_args("lrcorner",["patch"])),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators( ["i","j"], [
                                                                                                   comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)])),
                                                                                                   comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "delta",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' indices in bounding box but not part of patch ')),
                                                                           return_value( bin_op_left_right( sub_token(-),
                                                                                                 call_func_args("backdrop",["patch"]),
                                                                                                 call_func_args("toindices",["patch"])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "gravitate",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("source"),
                                                      argument_name("destination")],
                                                    [argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' direction to move source until adjacent to destination ')),
                                                                           assign_targets_value([["si","sj"]],call_func_args("center",["source"])),
                                                                           assign_targets_value([["di","dj"]],call_func_args("center",["destination"])),
                                                                           assign_targets_value([["i","j"]],[0,0]),
                                                                           if_test_body_orelse(
                                                                              call_func_args("vmatching",["source","destination"]),
                                                                              body_stmts( [ assign_targets_value( ["i"],
                                                                                                    if_exp_test_body_orelse(
                                                                                                       compare_ops_left_comparators(ops([lt_token(<)]),"si",comparators(["di"])), 1,unary_op_operand(us_ub_token(-),1)))])
                                                                         ,    orelse_else_stmts( [ assign_targets_value( ["j"],
                                                                                                           if_exp_test_body_orelse(
                                                                                                              compare_ops_left_comparators(ops([lt_token(<)]),"sj",comparators(["dj"])), 1,unary_op_operand(us_ub_token(-),1)))])
                                                                         ),
                                                                           assign_targets_value([["gi","gj"]],["i","j"]),
                                                                           assign_targets_value(["c"],0),
                                                                           while_test_body(
                                                                              bool_op_values( ['python:And'], [
                                                                                unary_op_operand(['python:Not'],call_func_args("adjacent",["source","destination"])),
                                                                                compare_ops_left_comparators(ops([lt_token(<)]),"c",comparators([42]))]),
                                                                              body_stmts( [ aug_assign_op_target_value(add_token(+),"c",1),
                                                                                                  aug_assign_op_target_value(add_token(+),"gi","i"),
                                                                                                  aug_assign_op_target_value(add_token(+),"gj","j"),
                                                                                                  assign_targets_value(["source"],call_func_args("shift",["source",["i","j"]]))])
                                                                         ),
                                                                           return_value([bin_op_left_right(sub_token(-),"gi","i"),bin_op_left_right(sub_token(-),"gj","j")])])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "inbox",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' inbox for patch ')),
                                                                           assign_targets_value( [ [ "ai","aj"]], [
                                                                             bin_op_left_right(add_token(+),call_func_args("uppermost",["patch"]),1),
                                                                             bin_op_left_right(add_token(+),call_func_args("leftmost",["patch"]),1)]),
                                                                           assign_targets_value( [ [ "bi","bj"]], [
                                                                             bin_op_left_right(sub_token(-),call_func_args("lowermost",["patch"]),1),
                                                                             bin_op_left_right(sub_token(-),call_func_args("rightmost",["patch"]),1)]),
                                                                           assign_targets_value( [ [ "si","sj"]], [
                                                                             call_func_args("min",["ai","bi"]),
                                                                             call_func_args("min",["aj","bj"])]),
                                                                           assign_targets_value( [ [ "ei","ej"]], [
                                                                             call_func_args("max",["ai","bi"]),
                                                                             call_func_args("max",["aj","bj"])]),
                                                                           assign_targets_value( ["vlines"],
                                                                             bin_op_left_right( bit_or_token('|'),
                                                                               set_comp_elt_generators( ["i","sj"], [
                                                                                 comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]),
                                                                               set_comp_elt_generators( ["i","ej"], [
                                                                                 comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]))),
                                                                           assign_targets_value( ["hlines"],
                                                                             bin_op_left_right( bit_or_token('|'),
                                                                               set_comp_elt_generators( ["si","j"], [
                                                                                 comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]),
                                                                               set_comp_elt_generators( ["ei","j"], [
                                                                                 comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]))),
                                                                           return_value(call_func_args("frozenset",[bin_op_left_right(bit_or_token('|'),"vlines","hlines")]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "outbox",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' outbox for patch ')),
                                                                           assign_targets_value( [ [ "ai","aj"]], [
                                                                             bin_op_left_right(sub_token(-),call_func_args("uppermost",["patch"]),1),
                                                                             bin_op_left_right(sub_token(-),call_func_args("leftmost",["patch"]),1)]),
                                                                           assign_targets_value( [ [ "bi","bj"]], [
                                                                             bin_op_left_right(add_token(+),call_func_args("lowermost",["patch"]),1),
                                                                             bin_op_left_right(add_token(+),call_func_args("rightmost",["patch"]),1)]),
                                                                           assign_targets_value( [ [ "si","sj"]], [
                                                                             call_func_args("min",["ai","bi"]),
                                                                             call_func_args("min",["aj","bj"])]),
                                                                           assign_targets_value( [ [ "ei","ej"]], [
                                                                             call_func_args("max",["ai","bi"]),
                                                                             call_func_args("max",["aj","bj"])]),
                                                                           assign_targets_value( ["vlines"],
                                                                             bin_op_left_right( bit_or_token('|'),
                                                                               set_comp_elt_generators( ["i","sj"], [
                                                                                 comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]),
                                                                               set_comp_elt_generators( ["i","ej"], [
                                                                                 comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]))),
                                                                           assign_targets_value( ["hlines"],
                                                                             bin_op_left_right( bit_or_token('|'),
                                                                               set_comp_elt_generators( ["si","j"], [
                                                                                 comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]),
                                                                               set_comp_elt_generators( ["ei","j"], [
                                                                                 comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]))),
                                                                           return_value(call_func_args("frozenset",[bin_op_left_right(bit_or_token('|'),"vlines","hlines")]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "box",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' outline of patch ')),
                                                                           assign_targets_value([["ai","aj"]],call_func_args("ulcorner",["patch"])),
                                                                           assign_targets_value([["bi","bj"]],call_func_args("lrcorner",["patch"])),
                                                                           assign_targets_value( [ [ "si","sj"]], [
                                                                             call_func_args("min",["ai","bi"]),
                                                                             call_func_args("min",["aj","bj"])]),
                                                                           assign_targets_value( [ [ "ei","ej"]], [
                                                                             call_func_args("max",["ai","bi"]),
                                                                             call_func_args("max",["aj","bj"])]),
                                                                           assign_targets_value( ["vlines"],
                                                                             bin_op_left_right( bit_or_token('|'),
                                                                               set_comp_elt_generators( ["i","sj"], [
                                                                                 comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]),
                                                                               set_comp_elt_generators( ["i","ej"], [
                                                                                 comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]))),
                                                                           assign_targets_value( ["hlines"],
                                                                             bin_op_left_right( bit_or_token('|'),
                                                                               set_comp_elt_generators( ["si","j"], [
                                                                                 comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]),
                                                                               set_comp_elt_generators( ["ei","j"], [
                                                                                 comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]))),
                                                                           return_value(call_func_args("frozenset",[bin_op_left_right(bit_or_token('|'),"vlines","hlines")]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "shoot",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("start"),
                                                      argument_name("direction")],
                                                    [argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' line from starting point and direction ')),
                                                                           return_value( call_func_args( "connect", [
                                                                                                 "start",
                                                                                                 [ bin_op_left_right( add_token(+),
                                                                                                     subscript_value_slice("start",0),
                                                                                                     bin_op_left_right(mult_token(*),42,subscript_value_slice("direction",0))),
                                                                                                   bin_op_left_right( add_token(+),
                                                                                                     subscript_value_slice("start",1),
                                                                                                     bin_op_left_right(mult_token(*),42,subscript_value_slice("direction",1)))]]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "occurrences",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("obj")],
                                                    [argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' locations of occurrences of object in grid ')),
                                                                           assign_targets_value(["occs"],call_func("set")),
                                                                           assign_targets_value(["normed"],call_func_args("normalize",["obj"])),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value([["oh","ow"]],call_func_args("shape",["obj"])),
                                                                           assign_targets_value( [ [ "h2","w2"]], [
                                                                             bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"h","oh"),1),
                                                                             bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"w","ow"),1)]),
                                                                           for_target_iter_body( "i",
                                                                             call_func_args("range",["h2"]),
                                                                             body_stmts( [ for_target_iter_body( "j",
                                                                                                   call_func_args("range",["w2"]),
                                                                                                   body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(true),'True')),
                                                                                                                       for_target_iter_body(
                                                                                                                          [ "v",
                                                                                                                            ["a","b"]],
                                                                                                                          call_func_args("shift",["normed",["i","j"]]),
                                                                                                                          body_stmts( [ if_test_body(
                                                                                                                                                 unary_op_operand( ['python:Not'],
                                                                                                                                                   bool_op_values( ['python:And'], [
                                                                                                                                                     compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["a","h"])),
                                                                                                                                                     compare_ops_left_comparators(
                                                                                                                                                        ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["b","w"])),
                                                                                                                                                     compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                                                       subscript_value_slice(subscript_value_slice("grid","a"),"b"),
                                                                                                                                                       comparators(["v"]))])),
                                                                                                                                                 body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(false),'False')),
                                                                                                                                                                     ['python:Break']])
                                                                                                                                              )])
                                                                                                                     ),
                                                                                                                       if_test_body( "occurs",
                                                                                                                         body_stmts( [ expr_value( call_func_args(
                                                                                                                                                                  qualified_identifier_identifiers(["occs",boxed_attribute_value("add")]),
                                                                                                                                                                  [ [ "i","j"]]))]))])
                                                                                                 )])
                                                                         ),
                                                                           return_value(call_func_args("frozenset",["occs"]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "frontiers",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Objects")]),
                                                 block_statements( [ expr_value(string_value(' set of frontiers ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value( ["row_indices"],
                                                                             call_func_args( "tuple", [
                                                                               generator_exp_elt_generators( "i", [
                                                                                 comprehension_ifs_target_iter(
                                                                                    [ compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                        call_func_args("len",[call_func_args("set",["r"])]),
                                                                                        comparators([1]))],
                                                                                    ["i","r"],
                                                                                    call_func_args("enumerate",["grid"]))])])),
                                                                           assign_targets_value( ["column_indices"],
                                                                             call_func_args( "tuple", [
                                                                               generator_exp_elt_generators( "j", [
                                                                                 comprehension_ifs_target_iter(
                                                                                    [ compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                        call_func_args("len",[call_func_args("set",["c"])]),
                                                                                        comparators([1]))],
                                                                                    ["j","c"],
                                                                                    call_func_args("enumerate",[call_func_args("dmirror",["grid"])]))])])),
                                                                           assign_targets_value( ["hfrontiers"],
                                                                             call_func_args( "frozenset", [
                                                                               set_comp_elt_generators(
                                                                                  call_func_args( "frozenset", [
                                                                                    set_comp_elt_generators(
                                                                                       [ subscript_value_slice(subscript_value_slice("grid","i"),"j"),
                                                                                         ["i","j"]],
                                                                                       [ comprehension_target_iter("j",call_func_args("range",["w"]))])]),
                                                                                  [comprehension_target_iter("i","row_indices")])])),
                                                                           assign_targets_value( ["vfrontiers"],
                                                                             call_func_args( "frozenset", [
                                                                               set_comp_elt_generators(
                                                                                  call_func_args( "frozenset", [
                                                                                    set_comp_elt_generators(
                                                                                       [ subscript_value_slice(subscript_value_slice("grid","i"),"j"),
                                                                                         ["i","j"]],
                                                                                       [ comprehension_target_iter("i",call_func_args("range",["h"]))])]),
                                                                                  [comprehension_target_iter("j","column_indices")])])),
                                                                           return_value(bin_op_left_right(bit_or_token('|'),"hfrontiers","vfrontiers"))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "compress",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' removes frontiers from grid ')),
                                                                           assign_targets_value( ["ri"],
                                                                             call_func_args( "tuple", [
                                                                               generator_exp_elt_generators( "i", [
                                                                                 comprehension_ifs_target_iter(
                                                                                    [ compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                        call_func_args("len",[call_func_args("set",["r"])]),
                                                                                        comparators([1]))],
                                                                                    ["i","r"],
                                                                                    call_func_args("enumerate",["grid"]))])])),
                                                                           assign_targets_value( ["ci"],
                                                                             call_func_args( "tuple", [
                                                                               generator_exp_elt_generators( "j", [
                                                                                 comprehension_ifs_target_iter(
                                                                                    [ compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                        call_func_args("len",[call_func_args("set",["c"])]),
                                                                                        comparators([1]))],
                                                                                    ["j","c"],
                                                                                    call_func_args("enumerate",[call_func_args("dmirror",["grid"])]))])])),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "tuple", [
                                                                                                      generator_exp_elt_generators( "v", [
                                                                                                        comprehension_ifs_target_iter(
                                                                                                           [ compare_ops_left_comparators(ops([['python:NotIn']]),"j",comparators(["ci"]))],
                                                                                                           ["j","v"],
                                                                                                           call_func_args("enumerate",["r"]))])]),
                                                                                                    [ comprehension_ifs_target_iter(
                                                                                                         [ compare_ops_left_comparators(ops([['python:NotIn']]),"i",comparators(["ri"]))],
                                                                                                         ["i","r"],
                                                                                                         call_func_args("enumerate",["grid"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hperiod",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("obj")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' horizontal periodicity ')),
                                                                           assign_targets_value(["normalized"],call_func_args("normalize",["obj"])),
                                                                           assign_targets_value(["w"],call_func_args("width",["normalized"])),
                                                                           for_target_iter_body( "p",
                                                                             call_func_args("range",[1,"w"]),
                                                                             body_stmts( [ assign_targets_value( ["offsetted"],
                                                                                                   call_func_args("shift",["normalized",[0,unary_op_operand(us_ub_token(-),"p")]])),
                                                                                                 assign_targets_value( ["pruned"],
                                                                                                   call_func_args( "frozenset", [
                                                                                                     set_comp_elt_generators(
                                                                                                        [ "c",
                                                                                                          ["i","j"]],
                                                                                                        [ comprehension_ifs_target_iter(
                                                                                                             [ compare_ops_left_comparators(ops([gt_e_token(>=)]),"j",comparators([0]))],
                                                                                                             [ "c",
                                                                                                               ["i","j"]],
                                                                                                             "offsetted")])])),
                                                                                                 if_test_body(
                                                                                                    call_func_args(
                                                                                                       qualified_identifier_identifiers(["pruned",boxed_attribute_value("issubset")]),
                                                                                                       ["normalized"]),
                                                                                                    body_stmts([return_value("p")]))])
                                                                         ),
                                                                           return_value("w")])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vperiod",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("obj")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' vertical periodicity ')),
                                                                           assign_targets_value(["normalized"],call_func_args("normalize",["obj"])),
                                                                           assign_targets_value(["h"],call_func_args("height",["normalized"])),
                                                                           for_target_iter_body( "p",
                                                                             call_func_args("range",[1,"h"]),
                                                                             body_stmts( [ assign_targets_value( ["offsetted"],
                                                                                                   call_func_args("shift",["normalized",[unary_op_operand(us_ub_token(-),"p"),0]])),
                                                                                                 assign_targets_value( ["pruned"],
                                                                                                   call_func_args( "frozenset", [
                                                                                                     set_comp_elt_generators(
                                                                                                        [ "c",
                                                                                                          ["i","j"]],
                                                                                                        [ comprehension_ifs_target_iter(
                                                                                                             [ compare_ops_left_comparators(ops([gt_e_token(>=)]),"i",comparators([0]))],
                                                                                                             [ "c",
                                                                                                               ["i","j"]],
                                                                                                             "offsetted")])])),
                                                                                                 if_test_body(
                                                                                                    call_func_args(
                                                                                                       qualified_identifier_identifiers(["pruned",boxed_attribute_value("issubset")]),
                                                                                                       ["normalized"]),
                                                                                                    body_stmts([return_value("p")]))])
                                                                         ),
                                                                           return_value("h")])
                                              ))])
            ])














end_of_file.





from arc_types import *


def identity(
    x: Any
) -> Any:
    """ identity function """
    return x


def add(
    a: Numerical,
    b: Numerical
) -> Numerical:
    """ addition """
    if isinstance(a, int) and isinstance(b, int):
        return a + b
    elif isinstance(a, tuple) and isinstance(b, tuple):
        return (a[0] + b[0], a[1] + b[1])
    elif isinstance(a, int) and isinstance(b, tuple):
        return (a + b[0], a + b[1])
    return (a[0] + b, a[1] + b)


def subtract(
    a: Numerical,
    b: Numerical
) -> Numerical:
    """ subtraction """
    if isinstance(a, int) and isinstance(b, int):
        return a - b
    elif isinstance(a, tuple) and isinstance(b, tuple):
        return (a[0] - b[0], a[1] - b[1])
    elif isinstance(a, int) and isinstance(b, tuple):
        return (a - b[0], a - b[1])
    return (a[0] - b, a[1] - b)


def multiply(
    a: Numerical,
    b: Numerical
) -> Numerical:
    """ multiplication """
    if isinstance(a, int) and isinstance(b, int):
        return a * b
    elif isinstance(a, tuple) and isinstance(b, tuple):
        return (a[0] * b[0], a[1] * b[1])
    elif isinstance(a, int) and isinstance(b, tuple):
        return (a * b[0], a * b[1])
    return (a[0] * b, a[1] * b)
    

def divide(
    a: Numerical,
    b: Numerical
) -> Numerical:
    """ floor division """
    if isinstance(a, int) and isinstance(b, int):
        return a // b
    elif isinstance(a, tuple) and isinstance(b, tuple):
        return (a[0] // b[0], a[1] // b[1])
    elif isinstance(a, int) and isinstance(b, tuple):
        return (a // b[0], a // b[1])
    return (a[0] // b, a[1] // b)


def invert(
    n: Numerical
) -> Numerical:
    """ inversion with respect to addition """
    return -n if isinstance(n, int) else (-n[0], -n[1])


def even(
    n: Integer
) -> Boolean:
    """ evenness """
    return n % 2 == 0


def double(
    n: Numerical
) -> Numerical:
    """ scaling by two """
    return n * 2 if isinstance(n, int) else (n[0] * 2, n[1] * 2)


def halve(
    n: Numerical
) -> Numerical:
    """ scaling by one half """
    return n // 2 if isinstance(n, int) else (n[0] // 2, n[1] // 2)


def flip(
    b: Boolean
) -> Boolean:
    """ logical not """
    return not b


def equality(
    a: Any,
    b: Any
) -> Boolean:
    """ equality """
    return a == b


def contained(
    value: Any,
    container: Container
) -> Boolean:
    """ element of """
    return value in container


def combine(
    a: Container,
    b: Container
) -> Container:
    """ union """
    return type(a)((*a, *b))


def intersection(
    a: FrozenSet,
    b: FrozenSet
) -> FrozenSet:
    """ returns the intersection of two containers """
    return a & b


def difference(
    a: FrozenSet,
    b: FrozenSet
) -> FrozenSet:
    """ set difference """
    return a - b


def dedupe(
    tup: Tuple
) -> Tuple:
    """ remove duplicates """
    return tuple(e for i, e in enumerate(tup) if tup.index(e) == i)


def order(
    container: Container,
    compfunc: Callable
) -> Tuple:
    """ order container by custom key """
    return tuple(sorted(container, key=compfunc))


def repeat(
    item: Any,
    num: Integer
) -> Tuple:
    """ repetition of item within vector """
    return tuple(item for i in range(num))


def greater(
    a: Integer,
    b: Integer
) -> Boolean:
    """ greater """
    return a > b


def size(
    container: Container
) -> Integer:
    """ cardinality """
    return len(container)


def merge(
    containers: ContainerContainer
) -> Container:
    """ merging """
    return type(containers)(e for c in containers for e in c)


def maximum(
    container: IntegerSet
) -> Integer:
    """ maximum """
    return max(container, default=0)


def minimum(
    container: IntegerSet
) -> Integer:
    """ minimum """
    return min(container, default=0)


def valmax(
    container: Container,
    compfunc: Callable
) -> Integer:
    """ maximum by custom function """
    return compfunc(max(container, key=compfunc, default=0))


def valmin(
    container: Container,
    compfunc: Callable
) -> Integer:
    """ minimum by custom function """
    return compfunc(min(container, key=compfunc, default=0))


def argmax(
    container: Container,
    compfunc: Callable
) -> Any:
    """ largest item by custom order """
    return max(container, key=compfunc)


def argmin(
    container: Container,
    compfunc: Callable
) -> Any:
    """ smallest item by custom order """
    return min(container, key=compfunc)


def mostcommon(
    container: Container
) -> Any:
    """ most common item """
    return max(set(container), key=container.count)


def leastcommon(
    container: Container
) -> Any:
    """ least common item """
    return min(set(container), key=container.count)


def initset(
    value: Any
) -> FrozenSet:
    """ initialize container """
    return frozenset({value})


def both(
    a: Boolean,
    b: Boolean
) -> Boolean:
    """ logical and """
    return a and b


def either(
    a: Boolean,
    b: Boolean
) -> Boolean:
    """ logical or """
    return a or b


def increment(
    x: Numerical
) -> Numerical:
    """ incrementing """
    return x + 1 if isinstance(x, int) else (x[0] + 1, x[1] + 1)


def decrement(
    x: Numerical
) -> Numerical:
    """ decrementing """
    return x - 1 if isinstance(x, int) else (x[0] - 1, x[1] - 1)


def crement(
    x: Numerical
) -> Numerical:
    """ incrementing positive and decrementing negative """
    if isinstance(x, int):
        return 0 if x == 0 else (x + 1 if x > 0 else x - 1)
    return (
        0 if x[0] == 0 else (x[0] + 1 if x[0] > 0 else x[0] - 1),
        0 if x[1] == 0 else (x[1] + 1 if x[1] > 0 else x[1] - 1)
    )


def sign(
    x: Numerical
) -> Numerical:
    """ sign """
    if isinstance(x, int):
        return 0 if x == 0 else (1 if x > 0 else -1)
    return (
        0 if x[0] == 0 else (1 if x[0] > 0 else -1),
        0 if x[1] == 0 else (1 if x[1] > 0 else -1)
    )


def positive(
    x: Integer
) -> Boolean:
    """ positive """
    return x > 0


def toivec(
    i: Integer
) -> IntegerTuple:
    """ vector pointing vertically """
    return (i, 0)


def tojvec(
    j: Integer
) -> IntegerTuple:
    """ vector pointing horizontally """
    return (0, j)


def sfilter(
    container: Container,
    condition: Callable
) -> Container:
    """ keep elements in container that satisfy condition """
    return type(container)(e for e in container if condition(e))


def mfilter(
    container: Container,
    function: Callable
) -> FrozenSet:
    """ filter and merge """
    return merge(sfilter(container, function))


def extract(
    container: Container,
    condition: Callable
) -> Any:
    """ first element of container that satisfies condition """
    return next(e for e in container if condition(e))


def totuple(
    container: FrozenSet
) -> Tuple:
    """ conversion to tuple """
    return tuple(container)


def first(
    container: Container
) -> Any:
    """ first item of container """
    return next(iter(container))


def last(
    container: Container
) -> Any:
    """ last item of container """
    return max(enumerate(container))[1]


def insert(
    value: Any,
    container: FrozenSet
) -> FrozenSet:
    """ insert item into container """
    return container.union(frozenset({value}))


def remove(
    value: Any,
    container: Container
) -> Container:
    """ remove item from container """
    return type(container)(e for e in container if e != value)


def other(
    container: Container,
    value: Any
) -> Any:
    """ other value in the container """
    return first(remove(value, container))


def interval(
    start: Integer,
    stop: Integer,
    step: Integer
) -> Tuple:
    """ range """
    return tuple(range(start, stop, step))


def astuple(
    a: Integer,
    b: Integer
) -> IntegerTuple:
    """ constructs a tuple """
    return (a, b)


def product(
    a: Container,
    b: Container
) -> FrozenSet:
    """ cartesian product """
    return frozenset((i, j) for j in b for i in a)


def pair(
    a: Tuple,
    b: Tuple
) -> TupleTuple:
    """ zipping of two tuples """
    return tuple(zip(a, b))


def branch(
    condition: Boolean,
    a: Any,
    b: Any
) -> Any:
    """ if else branching """
    return a if condition else b


def compose(
    outer: Callable,
    inner: Callable
) -> Callable:
    """ function composition """
    return lambda x: outer(inner(x))


def chain(
    h: Callable,
    g: Callable,
    f: Callable,
) -> Callable:
    """ function composition with three functions """
    return lambda x: h(g(f(x)))


def matcher(
    function: Callable,
    target: Any
) -> Callable:
    """ construction of equality function """
    return lambda x: function(x) == target


def rbind(
    function: Callable,
    fixed: Any
) -> Callable:
    """ fix the rightmost argument """
    n = function.__code__.co_argcount
    if n == 2:
        return lambda x: function(x, fixed)
    elif n == 3:
        return lambda x, y: function(x, y, fixed)
    else:
        return lambda x, y, z: function(x, y, z, fixed)


def lbind(
    function: Callable,
    fixed: Any
) -> Callable:
    """ fix the leftmost argument """
    n = function.__code__.co_argcount
    if n == 2:
        return lambda y: function(fixed, y)
    elif n == 3:
        return lambda y, z: function(fixed, y, z)
    else:
        return lambda y, z, a: function(fixed, y, z, a)


def power(
    function: Callable,
    n: Integer
) -> Callable:
    """ power of function """
    if n == 1:
        return function
    return compose(function, power(function, n - 1))


def fork(
    outer: Callable,
    a: Callable,
    b: Callable
) -> Callable:
    """ creates a wrapper function """
    return lambda x: outer(a(x), b(x))


def apply(
    function: Callable,
    container: Container
) -> Container:
    """ apply function to each item in container """
    return type(container)(function(e) for e in container)


def rapply(
    functions: Container,
    value: Any
) -> Container:
    """ apply each function in container to value """
    return type(functions)(function(value) for function in functions)


def mapply(
    function: Callable,
    container: ContainerContainer
) -> FrozenSet:
    """ apply and merge """
    return merge(apply(function, container))


def papply(
    function: Callable,
    a: Tuple,
    b: Tuple
) -> Tuple:
    """ apply function on two vectors """
    return tuple(function(i, j) for i, j in zip(a, b))


def mpapply(
    function: Callable,
    a: Tuple,
    b: Tuple
) -> Tuple:
    """ apply function on two vectors and merge """
    return merge(papply(function, a, b))


def prapply(
    function,
    a: Container,
    b: Container
) -> FrozenSet:
    """ apply function on cartesian product """
    return frozenset(function(i, j) for j in b for i in a)


def mostcolor(
    element: Element
) -> Integer:
    """ most common color """
    values = [v for r in element for v in r] if isinstance(element, tuple) else [v for v, _ in element]
    return max(set(values), key=values.count)
    

def leastcolor(
    element: Element
) -> Integer:
    """ least common color """
    values = [v for r in element for v in r] if isinstance(element, tuple) else [v for v, _ in element]
    return min(set(values), key=values.count)


def height(
    piece: Piece
) -> Integer:
    """ height of grid or patch """
    if len(piece) == 0:
        return 0
    if isinstance(piece, tuple):
        return len(piece)
    return lowermost(piece) - uppermost(piece) + 1


def width(
    piece: Piece
) -> Integer:
    """ width of grid or patch """
    if len(piece) == 0:
        return 0
    if isinstance(piece, tuple):
        return len(piece[0])
    return rightmost(piece) - leftmost(piece) + 1


def shape(
    piece: Piece
) -> IntegerTuple:
    """ height and width of grid or patch """
    return (height(piece), width(piece))


def portrait(
    piece: Piece
) -> Boolean:
    """ whether height is greater than width """
    return height(piece) > width(piece)


def colorcount(
    element: Element,
    value: Integer
) -> Integer:
    """ number of cells with color """
    if isinstance(element, tuple):
        return sum(row.count(value) for row in element)
    return sum(v == value for v, _ in element)


def colorfilter(
    objs: Objects,
    value: Integer
) -> Objects:
    """ filter objects by color """
    return frozenset(obj for obj in objs if next(iter(obj))[0] == value)


def sizefilter(
    container: Container,
    n: Integer
) -> FrozenSet:
    """ filter items by size """
    return frozenset(item for item in container if len(item) == n)


def asindices(
    grid: Grid
) -> Indices:
    """ indices of all grid cells """
    return frozenset((i, j) for i in range(len(grid)) for j in range(len(grid[0])))


def ofcolor(
    grid: Grid,
    value: Integer
) -> Indices:
    """ indices of all grid cells with value """
    return frozenset((i, j) for i, r in enumerate(grid) for j, v in enumerate(r) if v == value)


def ulcorner(
    patch: Patch
) -> IntegerTuple:
    """ index of upper left corner """
    return tuple(map(min, zip(*toindices(patch))))


def urcorner(
    patch: Patch
) -> IntegerTuple:
    """ index of upper right corner """
    return tuple(map(lambda ix: {0: min, 1: max}[ix[0]](ix[1]), enumerate(zip(*toindices(patch)))))


def llcorner(
    patch: Patch
) -> IntegerTuple:
    """ index of lower left corner """
    return tuple(map(lambda ix: {0: max, 1: min}[ix[0]](ix[1]), enumerate(zip(*toindices(patch)))))


def lrcorner(
    patch: Patch
) -> IntegerTuple:
    """ index of lower right corner """
    return tuple(map(max, zip(*toindices(patch))))


def crop(
    grid: Grid,
    start: IntegerTuple,
    dims: IntegerTuple
) -> Grid:
    """ subgrid specified by start and dimension """
    return tuple(r[start[1]:start[1]+dims[1]] for r in grid[start[0]:start[0]+dims[0]])


def toindices(
    patch: Patch
) -> Indices:
    """ indices of object cells """
    if len(patch) == 0:
        return frozenset()
    if isinstance(next(iter(patch))[1], tuple):
        return frozenset(index for value, index in patch)
    return patch


def recolor(
    value: Integer,
    patch: Patch
) -> Object:
    """ recolor patch """
    return frozenset((value, index) for index in toindices(patch))


def shift(
    patch: Patch,
    directions: IntegerTuple
) -> Patch:
    """ shift patch """
    di, dj = directions
    if isinstance(next(iter(patch))[1], tuple):
        return frozenset((value, (i + di, j + dj)) for value, (i, j) in patch)
    return frozenset((i + di, j + dj) for i, j in patch)


def normalize(
    patch: Patch
) -> Patch:
    """ moves upper left corner to origin """
    return shift(patch, (-uppermost(patch), -leftmost(patch)))


def dneighbors(
    loc: IntegerTuple
) -> Indices:
    """ directly adjacent indices """
    return frozenset({(loc[0] - 1, loc[1]), (loc[0] + 1, loc[1]), (loc[0], loc[1] - 1), (loc[0], loc[1] + 1)})


def ineighbors(
    loc: IntegerTuple
) -> Indices:
    """ diagonally adjacent indices """
    return frozenset({(loc[0] - 1, loc[1] - 1), (loc[0] - 1, loc[1] + 1), (loc[0] + 1, loc[1] - 1), (loc[0] + 1, loc[1] + 1)})


def neighbors(
    loc: IntegerTuple
) -> Indices:
    """ adjacent indices """
    return dneighbors(loc) | ineighbors(loc)


def objects(
    grid: Grid,
    univalued: Boolean,
    diagonal: Boolean,
    without_bg: Boolean
) -> Objects:
    """ objects occurring on the grid """
    bg = mostcolor(grid) if without_bg else None
    objs = set()
    occupied = set()
    h, w = len(grid), len(grid[0])
    unvisited = asindices(grid)
    diagfun = neighbors if diagonal else dneighbors
    for loc in unvisited:
        if loc in occupied:
            continue
        val = grid[loc[0]][loc[1]]
        if val == bg:
            continue
        obj = {(val, loc)}
        cands = {loc}
        while len(cands) > 0:
            neighborhood = set()
            for cand in cands:
                v = grid[cand[0]][cand[1]]
                if (val == v) if univalued else (v != bg):
                    obj.add((v, cand))
                    occupied.add(cand)
                    neighborhood |= {
                        (i, j) for i, j in diagfun(cand) if 0 <= i < h and 0 <= j < w
                    }
            cands = neighborhood - occupied
        objs.add(frozenset(obj))
    return frozenset(objs)


def partition(
    grid: Grid
) -> Objects:
    """ each cell with the same value part of the same object """
    return frozenset(
        frozenset(
            (v, (i, j)) for i, r in enumerate(grid) for j, v in enumerate(r) if v == value
        ) for value in palette(grid)
    )


def fgpartition(
    grid: Grid
) -> Objects:
    """ each cell with the same value part of the same object without background """
    return frozenset(
        frozenset(
            (v, (i, j)) for i, r in enumerate(grid) for j, v in enumerate(r) if v == value
        ) for value in palette(grid) - {mostcolor(grid)}
    )


def uppermost(
    patch: Patch
) -> Integer:
    """ row index of uppermost occupied cell """
    return min(i for i, j in toindices(patch))


def lowermost(
    patch: Patch
) -> Integer:
    """ row index of lowermost occupied cell """
    return max(i for i, j in toindices(patch))


def leftmost(
    patch: Patch
) -> Integer:
    """ column index of leftmost occupied cell """
    return min(j for i, j in toindices(patch))


def rightmost(
    patch: Patch
) -> Integer:
    """ column index of rightmost occupied cell """
    return max(j for i, j in toindices(patch))


def square(
    piece: Piece
) -> Boolean:
    """ whether the piece forms a square """
    return len(piece) == len(piece[0]) if isinstance(piece, tuple) else height(piece) * width(piece) == len(piece) and height(piece) == width(piece)


def vline(
    patch: Patch
) -> Boolean:
    """ whether the piece forms a vertical line """
    return height(patch) == len(patch) and width(patch) == 1


def hline(
    patch: Patch
) -> Boolean:
    """ whether the piece forms a horizontal line """
    return width(patch) == len(patch) and height(patch) == 1


def hmatching(
    a: Patch,
    b: Patch
) -> Boolean:
    """ whether there exists a row for which both patches have cells """
    return len(set(i for i, j in toindices(a)) & set(i for i, j in toindices(b))) > 0


def vmatching(
    a: Patch,
    b: Patch
) -> Boolean:
    """ whether there exists a column for which both patches have cells """
    return len(set(j for i, j in toindices(a)) & set(j for i, j in toindices(b))) > 0


def manhattan(
    a: Patch,
    b: Patch
) -> Integer:
    """ closest manhattan distance between two patches """
    return min(abs(ai - bi) + abs(aj - bj) for ai, aj in toindices(a) for bi, bj in toindices(b))


def adjacent(
    a: Patch,
    b: Patch
) -> Boolean:
    """ whether two patches are adjacent """
    return manhattan(a, b) == 1


def bordering(
    patch: Patch,
    grid: Grid
) -> Boolean:
    """ whether a patch is adjacent to a grid border """
    return uppermost(patch) == 0 or leftmost(patch) == 0 or lowermost(patch) == len(grid) - 1 or rightmost(patch) == len(grid[0]) - 1


def centerofmass(
    patch: Patch
) -> IntegerTuple:
    """ center of mass """
    return tuple(map(lambda x: sum(x) // len(patch), zip(*toindices(patch))))


def palette(
    element: Element
) -> IntegerSet:
    """ colors occurring in object or grid """
    if isinstance(element, tuple):
        return frozenset({v for r in element for v in r})
    return frozenset({v for v, _ in element})


def numcolors(
    element: Element
) -> IntegerSet:
    """ number of colors occurring in object or grid """
    return len(palette(element))


def color(
    obj: Object
) -> Integer:
    """ color of object """
    return next(iter(obj))[0]


def toobject(
    patch: Patch,
    grid: Grid
) -> Object:
    """ object from patch and grid """
    h, w = len(grid), len(grid[0])
    return frozenset((grid[i][j], (i, j)) for i, j in toindices(patch) if 0 <= i < h and 0 <= j < w)


def asobject(
    grid: Grid
) -> Object:
    """ conversion of grid to object """
    return frozenset((v, (i, j)) for i, r in enumerate(grid) for j, v in enumerate(r))


def rot90(
    grid: Grid
) -> Grid:
    """ quarter clockwise rotation """
    return tuple(row for row in zip(*grid[::-1]))


def rot180(
    grid: Grid
) -> Grid:
    """ half rotation """
    return tuple(tuple(row[::-1]) for row in grid[::-1])


def rot270(
    grid: Grid
) -> Grid:
    """ quarter anticlockwise rotation """
    return tuple(tuple(row[::-1]) for row in zip(*grid[::-1]))[::-1]


def hmirror(
    piece: Piece
) -> Piece:
    """ mirroring along horizontal """
    if isinstance(piece, tuple):
        return piece[::-1]
    d = ulcorner(piece)[0] + lrcorner(piece)[0]
    if isinstance(next(iter(piece))[1], tuple):
        return frozenset((v, (d - i, j)) for v, (i, j) in piece)
    return frozenset((d - i, j) for i, j in piece)


def vmirror(
    piece: Piece
) -> Piece:
    """ mirroring along vertical """
    if isinstance(piece, tuple):
        return tuple(row[::-1] for row in piece)
    d = ulcorner(piece)[1] + lrcorner(piece)[1]
    if isinstance(next(iter(piece))[1], tuple):
        return frozenset((v, (i, d - j)) for v, (i, j) in piece)
    return frozenset((i, d - j) for i, j in piece)


def dmirror(
    piece: Piece
) -> Piece:
    """ mirroring along diagonal """
    if isinstance(piece, tuple):
        return tuple(zip(*piece))
    a, b = ulcorner(piece)
    if isinstance(next(iter(piece))[1], tuple):
        return frozenset((v, (j - b + a, i - a + b)) for v, (i, j) in piece)
    return frozenset((j - b + a, i - a + b) for i, j in piece)


def cmirror(
    piece: Piece
) -> Piece:
    """ mirroring along counterdiagonal """
    if isinstance(piece, tuple):
        return tuple(zip(*(r[::-1] for r in piece[::-1])))
    return vmirror(dmirror(vmirror(piece)))


def fill(
    grid: Grid,
    value: Integer,
    patch: Patch
) -> Grid:
    """ fill value at indices """
    h, w = len(grid), len(grid[0])
    grid_filled = list(list(row) for row in grid)
    for i, j in toindices(patch):
        if 0 <= i < h and 0 <= j < w:
            grid_filled[i][j] = value
    return tuple(tuple(row) for row in grid_filled)


def paint(
    grid: Grid,
    obj: Object
) -> Grid:
    """ paint object to grid """
    h, w = len(grid), len(grid[0])
    grid_painted = list(list(row) for row in grid)
    for value, (i, j) in obj:
        if 0 <= i < h and 0 <= j < w:
            grid_painted[i][j] = value
    return tuple(tuple(row) for row in grid_painted)


def underfill(
    grid: Grid,
    value: Integer,
    patch: Patch
) -> Grid:
    """ fill value at indices that are background """
    h, w = len(grid), len(grid[0])
    bg = mostcolor(grid)
    g = list(list(r) for r in grid)
    for i, j in toindices(patch):
        if 0 <= i < h and 0 <= j < w:
            if g[i][j] == bg:
                g[i][j] = value
    return tuple(tuple(r) for r in g)


def underpaint(
    grid: Grid,
    obj: Object
) -> Grid:
    """ paint object to grid where there is background """
    h, w = len(grid), len(grid[0])
    bg = mostcolor(grid)
    g = list(list(r) for r in grid)
    for value, (i, j) in obj:
        if 0 <= i < h and 0 <= j < w:
            if g[i][j] == bg:
                g[i][j] = value
    return tuple(tuple(r) for r in g)


def hupscale(
    grid: Grid,
    factor: Integer
) -> Grid:
    """ upscale grid horizontally """
    g = tuple()
    for row in grid:
        r = tuple()
        for value in row:
            r = r + tuple(value for num in range(factor))
        g = g + (r,)
    return g


def vupscale(
    grid: Grid,
    factor: Integer
) -> Grid:
    """ upscale grid vertically """
    g = tuple()
    for row in grid:
        g = g + tuple(row for num in range(factor))
    return g


def upscale(
    element: Element,
    factor: Integer
) -> Element:
    """ upscale object or grid """
    if isinstance(element, tuple):
        g = tuple()
        for row in element:
            upscaled_row = tuple()
            for value in row:
                upscaled_row = upscaled_row + tuple(value for num in range(factor))
            g = g + tuple(upscaled_row for num in range(factor))
        return g
    else:
        if len(element) == 0:
            return frozenset()
        di_inv, dj_inv = ulcorner(element)
        di, dj = (-di_inv, -dj_inv)
        normed_obj = shift(element, (di, dj))
        o = set()
        for value, (i, j) in normed_obj:
            for io in range(factor):
                for jo in range(factor):
                    o.add((value, (i * factor + io, j * factor + jo)))
        return shift(frozenset(o), (di_inv, dj_inv))


def downscale(
    grid: Grid,
    factor: Integer
) -> Grid:
    """ downscale grid """
    h, w = len(grid), len(grid[0])
    g = tuple()
    for i in range(h):
        r = tuple()
        for j in range(w):
            if j % factor == 0:
                r = r + (grid[i][j],)
        g = g + (r, )
    h = len(g)
    dsg = tuple()
    for i in range(h):
        if i % factor == 0:
            dsg = dsg + (g[i],)
    return dsg


def hconcat(
    a: Grid,
    b: Grid
) -> Grid:
    """ concatenate two grids horizontally """
    return tuple(i + j for i, j in zip(a, b))


def vconcat(
    a: Grid,
    b: Grid
) -> Grid:
    """ concatenate two grids vertically """
    return a + b


def subgrid(
    patch: Patch,
    grid: Grid
) -> Grid:
    """ smallest subgrid containing object """
    return crop(grid, ulcorner(patch), shape(patch))


def hsplit(
    grid: Grid,
    n: Integer
) -> Tuple:
    """ split grid horizontally """
    h, w = len(grid), len(grid[0]) // n
    offset = len(grid[0]) % n != 0
    return tuple(crop(grid, (0, w * i + i * offset), (h, w)) for i in range(n))


def vsplit(
    grid: Grid,
    n: Integer
) -> Tuple:
    """ split grid vertically """
    h, w = len(grid) // n, len(grid[0])
    offset = len(grid) % n != 0
 
    hs = h * i + i * offset,
    g = crop(grid, (hs, 0), (h, w)),
    return tuple(g for i in range(n))


def cellwise(
    a: Grid,
    b: Grid,
    fallback: Integer
) -> Grid:
    """ cellwise match of two grids """
    h, w = len(a), len(a[0])
    resulting_grid = tuple()
    for i in range(h):
        row = tuple()
        for j in range(w):
            a_value = a[i][j]
            value = a_value if a_value == b[i][j] else fallback
            row = row + (value,)
        resulting_grid = resulting_grid + (row, )
    return resulting_grid


def replace(
    grid: Grid,
    replacee: Integer,
    replacer: Integer
) -> Grid:
    """ color substitution """
    return tuple(tuple(replacer if v == replacee else v for v in r) for r in grid)


def switch(
    grid: Grid,
    a: Integer,
    b: Integer
) -> Grid:
    """ color switching """
    return tuple(tuple(v if (v != a and v != b) else {a: b, b: a}[v] for v in r) for r in grid)


def center(
    patch: Patch
) -> IntegerTuple:
    """ center of the patch """
    return (uppermost(patch) + height(patch) // 2, leftmost(patch) + width(patch) // 2)


def position(
    a: Patch,
    b: Patch
) -> IntegerTuple:
    """ relative position between two patches """
    ia, ja = center(toindices(a))
    ib, jb = center(toindices(b))
    if ia == ib:
        return (0, 1 if ja < jb else -1)
    elif ja == jb:
        return (1 if ia < ib else -1, 0)
    elif ia < ib:
        return (1, 1 if ja < jb else -1)
    elif ia > ib:
        return (-1, 1 if ja < jb else -1)


def index(
    grid: Grid,
    loc: IntegerTuple
) -> Integer:
    """ color at location """
    i, j = loc
    h, w = len(grid), len(grid[0])
    if not (0 <= i < h and 0 <= j < w):
        return None
    return grid[loc[0]][loc[1]] 


def canvas(
    value: Integer,
    dimensions: IntegerTuple
) -> Grid:
    """ grid construction """
    return tuple(tuple(value for j in range(dimensions[1])) for i in range(dimensions[0]))


def corners(
    patch: Patch
) -> Indices:
    """ indices of corners """
    return frozenset({ulcorner(patch), urcorner(patch), llcorner(patch), lrcorner(patch)})


def connect(
    a: IntegerTuple,
    b: IntegerTuple
) -> Indices:
    """ line between two points """
    ai, aj = a
    bi, bj = b
    si = min(ai, bi)
    ei = max(ai, bi) + 1
    sj = min(aj, bj)
    ej = max(aj, bj) + 1
    if ai == bi:
        return frozenset((ai, j) for j in range(sj, ej))
    elif aj == bj:
        return frozenset((i, aj) for i in range(si, ei))
    elif bi - ai == bj - aj:
        return frozenset((i, j) for i, j in zip(range(si, ei), range(sj, ej)))
    elif bi - ai == aj - bj:
        return frozenset((i, j) for i, j in zip(range(si, ei), range(ej - 1, sj - 1, -1)))
    return frozenset()


def cover(
    grid: Grid,
    patch: Patch
) -> Grid:
    """ remove object from grid """
    return fill(grid, mostcolor(grid), toindices(patch))


def trim(
    grid: Grid
) -> Grid:
    """ trim border of grid """
    return tuple(r[1:-1] for r in grid[1:-1])


def move(
    grid: Grid,
    obj: Object,
    offset: IntegerTuple
) -> Grid:
    """ move object on grid """
    return paint(cover(grid, obj), shift(obj, offset))


def tophalf(
    grid: Grid
) -> Grid:
    """ upper half of grid """
    return grid[:len(grid) // 2]


def bottomhalf(
    grid: Grid
) -> Grid:
    """ lower half of grid """
    return grid[len(grid) // 2 + len(grid) % 2:]


def lefthalf(
    grid: Grid
) -> Grid:
    """ left half of grid """
    return rot270(tophalf(rot90(grid)))


def righthalf(
    grid: Grid
) -> Grid:
    """ right half of grid """
    return rot270(bottomhalf(rot90(grid)))


def vfrontier(
    location: IntegerTuple
) -> Indices:
    """ vertical frontier """
    return frozenset((i, location[1]) for i in range(30))


def hfrontier(
    location: IntegerTuple
) -> Indices:
    """ horizontal frontier """
    return frozenset((location[0], j) for j in range(30))


def backdrop(
    patch: Patch
) -> Indices:
    """ indices in bounding box of patch """
    indices = toindices(patch)
    si, sj = ulcorner(indices)
    ei, ej = lrcorner(patch)
    return frozenset((i, j) for i in range(si, ei + 1) for j in range(sj, ej + 1))


def delta(
    patch: Patch
) -> Indices:
    """ indices in bounding box but not part of patch """
    return backdrop(patch) - toindices(patch)


def gravitate(
    source: Patch,
    destination: Patch
) -> IntegerTuple:
    """ direction to move source until adjacent to destination """
    si, sj = center(source)
    di, dj = center(destination)
    i, j = 0, 0
    if vmatching(source, destination):
        i = 1 if si < di else -1
    else:
        j = 1 if sj < dj else -1
    gi, gj = i, j
    c = 0
    while not adjacent(source, destination) and c < 42:
        c += 1
        gi += i
        gj += j
        source = shift(source, (i, j))
    return (gi - i, gj - j)


def inbox(
    patch: Patch
) -> Indices:
    """ inbox for patch """
    ai, aj = uppermost(patch) + 1, leftmost(patch) + 1
    bi, bj = lowermost(patch) - 1, rightmost(patch) - 1
    si, sj = min(ai, bi), min(aj, bj)
    ei, ej = max(ai, bi), max(aj, bj)
    vlines = {(i, sj) for i in range(si, ei + 1)} | {(i, ej) for i in range(si, ei + 1)}
    hlines = {(si, j) for j in range(sj, ej + 1)} | {(ei, j) for j in range(sj, ej + 1)}
    return frozenset(vlines | hlines)


def outbox(
    patch: Patch
) -> Indices:
    """ outbox for patch """
    ai, aj = uppermost(patch) - 1, leftmost(patch) - 1
    bi, bj = lowermost(patch) + 1, rightmost(patch) + 1
    si, sj = min(ai, bi), min(aj, bj)
    ei, ej = max(ai, bi), max(aj, bj)
    vlines = {(i, sj) for i in range(si, ei + 1)} | {(i, ej) for i in range(si, ei + 1)}
    hlines = {(si, j) for j in range(sj, ej + 1)} | {(ei, j) for j in range(sj, ej + 1)}
    return frozenset(vlines | hlines)


def box(
    patch: Patch
) -> Indices:
    """ outline of patch """
    ai, aj = ulcorner(patch)
    bi, bj = lrcorner(patch)
    si, sj = min(ai, bi), min(aj, bj)
    ei, ej = max(ai, bi), max(aj, bj)
    vlines = {(i, sj) for i in range(si, ei + 1)} | {(i, ej) for i in range(si, ei + 1)}
    hlines = {(si, j) for j in range(sj, ej + 1)} | {(ei, j) for j in range(sj, ej + 1)}
    return frozenset(vlines | hlines)


def shoot(
    start: IntegerTuple,
    direction: IntegerTuple
) -> Indices:
    """ line from starting point and direction """
    return connect(start, (start[0] + 42 * direction[0], start[1] + 42 * direction[1]))


def occurrences(
    grid: Grid,
    obj: Object
) -> Indices:
    """ locations of occurrences of object in grid """
    occs = set()
    normed = normalize(obj)
    h, w = len(grid), len(grid[0])
    oh, ow = shape(obj)
    h2, w2 = h - oh + 1, w - ow + 1
    for i in range(h2):
        for j in range(w2):
            occurs = True
            for v, (a, b) in shift(normed, (i, j)):
                if not (0 <= a < h and 0 <= b < w and grid[a][b] == v):
                    occurs = False
                    break
            if occurs:
                occs.add((i, j))
    return frozenset(occs)


def frontiers(
    grid: Grid
) -> Objects:
    """ set of frontiers """
    h, w = len(grid), len(grid[0])
    row_indices = tuple(i for i, r in enumerate(grid) if len(set(r)) == 1)
    column_indices = tuple(j for j, c in enumerate(dmirror(grid)) if len(set(c)) == 1)
    hfrontiers = frozenset({frozenset({(grid[i][j], (i, j)) for j in range(w)}) for i in row_indices})
    vfrontiers = frozenset({frozenset({(grid[i][j], (i, j)) for i in range(h)}) for j in column_indices})
    return hfrontiers | vfrontiers


def compress(
    grid: Grid
) -> Grid:
    """ removes frontiers from grid """
    ri = tuple(i for i, r in enumerate(grid) if len(set(r)) == 1)
    ci = tuple(j for j, c in enumerate(dmirror(grid)) if len(set(c)) == 1)
    return tuple(tuple(v for j, v in enumerate(r) if j not in ci) for i, r in enumerate(grid) if i not in ri)


def hperiod(
    obj: Object
) -> Integer:
    """ horizontal periodicity """
    normalized = normalize(obj)
    w = width(normalized)
    for p in range(1, w):
        offsetted = shift(normalized, (0, -p))
        pruned = frozenset({(c, (i, j)) for c, (i, j) in offsetted if j >= 0})
        if pruned.issubset(normalized):
            return p
    return w


def vperiod(
    obj: Object
) -> Integer:
    """ vertical periodicity """
    normalized = normalize(obj)
    h = height(normalized)
    for p in range(1, h):
        offsetted = shift(normalized, (-p, 0))
        pruned = frozenset({(c, (i, j)) for c, (i, j) in offsetted if i >= 0})
        if pruned.issubset(normalized):
            return p
    return h
