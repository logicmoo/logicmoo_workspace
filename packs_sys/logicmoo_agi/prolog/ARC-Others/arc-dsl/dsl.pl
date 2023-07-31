
% ?- uast_test.


%~ % Universal AST Pass #0
%~ def( "identity",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("x")],[argument_type("Any")]),
%~      block_statements([expr_value(string_value(' identity function ')),return_value("x")])))
%~
% Compiled KL-1 for identity
identity(X_01,ANY_02) :-
  willBeType(ANY_02,'Any') ,
  comment(' identity function ') ,
  call(ANY_02=X_01) ,
  exit_proc(ANY_02).
%~ % Universal AST Pass #0
%~ def( "add",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("Numerical")]),
%~      block_statements( [ expr_value(string_value(' addition ')),
%~                          if_test_body_orelse(
%~                             bool_op_values( ['python:And'], [
%~                               call_func_args("isinstance",["a","int"]),
%~                               call_func_args("isinstance",["b","int"])]),
%~                             body_stmts([return_value(bin_op_left_right(add_token(+),"a","b"))]),
%~                             orelse_else_stmts( [ if_test_body_orelse(
%~                                                     bool_op_values( ['python:And'], [
%~                                                       call_func_args("isinstance",["a","tuple"]),
%~                                                       call_func_args("isinstance",["b","tuple"])]),
%~                                                     body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(add_token(+),subscript_value_slice("a",0),subscript_value_slice("b",0)),
%~                                                                                               bin_op_left_right(add_token(+),subscript_value_slice("a",1),subscript_value_slice("b",1))]))]),
%~                                                     orelse_else_stmts( [ if_test_body(
%~                                                                             bool_op_values( ['python:And'], [
%~                                                                               call_func_args("isinstance",["a","int"]),
%~                                                                               call_func_args("isinstance",["b","tuple"])]),
%~                                                                             body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(add_token(+),"a",subscript_value_slice("b",0)),
%~                                                                                                                       bin_op_left_right(add_token(+),"a",subscript_value_slice("b",1))]))]))]))])),
%~                          return_value( tuple_elts( [ bin_op_left_right(add_token(+),subscript_value_slice("a",0),"b"),
%~                                                      bin_op_left_right(add_token(+),subscript_value_slice("a",1),"b")]))])))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body_orelse(
%~                                             bool_op_values( ['python:And'], [
%~                                               call_func_args("isinstance",[A_01,"tuple"]),
%~                                               call_func_args("isinstance",[B_02,"tuple"])]),
%~                                             body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(add_token(+),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                                                       bin_op_left_right(add_token(+),subscript_value_slice(A_01,1),subscript_value_slice(B_02,1))]))]),
%~                                             orelse_else_stmts( [ if_test_body(
%~                                                                     bool_op_values( ['python:And'], [
%~                                                                       call_func_args("isinstance",[A_01,"int"]),
%~                                                                       call_func_args("isinstance",[B_02,"tuple"])]),
%~                                                                     body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(add_token(+),A_01,subscript_value_slice(B_02,0)),
%~                                                                                                               bin_op_left_right(add_token(+),A_01,subscript_value_slice(B_02,1))]))]))]))]))
%~
%~ replacing_with_var(body_stmts([return_value(bin_op_left_right(add_token(+),A_01,B_02))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[A_01,"int"]))
%~
%~ replacing_with_var(call_func_args("isinstance",[B_02,"int"]))
%~
%~ replacing_with_var( if_test_body_orelse(
%~                        bool_op_values( ['python:And'], [
%~                          call_func_args("isinstance",[A_01,"tuple"]),
%~                          call_func_args("isinstance",[B_02,"tuple"])]),
%~                        body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(add_token(+),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                                  bin_op_left_right(add_token(+),subscript_value_slice(A_01,1),subscript_value_slice(B_02,1))]))]),
%~                        orelse_else_stmts( [ if_test_body(
%~                                                bool_op_values( ['python:And'], [
%~                                                  call_func_args("isinstance",[A_01,"int"]),
%~                                                  call_func_args("isinstance",[B_02,"tuple"])]),
%~                                                body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(add_token(+),A_01,subscript_value_slice(B_02,0)),
%~                                                                                          bin_op_left_right(add_token(+),A_01,subscript_value_slice(B_02,1))]))]))])))
%~
%~ replacing_with_var(return_value(bin_op_left_right(add_token(+),A_01,B_02)))
%~
%~ replacing_with_var( body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(add_token(+),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                               bin_op_left_right(add_token(+),subscript_value_slice(A_01,1),subscript_value_slice(B_02,1))]))]))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body(
%~                                             bool_op_values( ['python:And'], [
%~                                               call_func_args("isinstance",[A_01,"int"]),
%~                                               call_func_args("isinstance",[B_02,"tuple"])]),
%~                                             body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(add_token(+),A_01,subscript_value_slice(B_02,0)),
%~                                                                                       bin_op_left_right(add_token(+),A_01,subscript_value_slice(B_02,1))]))]))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[A_01,"tuple"]))
%~
%~ replacing_with_var(call_func_args("isinstance",[B_02,"tuple"]))
%~
%~ replacing_with_var( return_value( tuple_elts( [ bin_op_left_right(add_token(+),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                 bin_op_left_right(add_token(+),subscript_value_slice(A_01,1),subscript_value_slice(B_02,1))])))
%~
%~ replacing_with_var( if_test_body(
%~                        bool_op_values( ['python:And'], [
%~                          call_func_args("isinstance",[A_01,"int"]),
%~                          call_func_args("isinstance",[B_02,"tuple"])]),
%~                        body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(add_token(+),A_01,subscript_value_slice(B_02,0)),
%~                                                                  bin_op_left_right(add_token(+),A_01,subscript_value_slice(B_02,1))]))])))
%~
%~ replacing_with_var(call_func_args(add_token(+),[A_01,B_02]))
%~
%~ replacing_with_var( tuple_elts( [ bin_op_left_right(add_token(+),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                   bin_op_left_right(add_token(+),subscript_value_slice(A_01,1),subscript_value_slice(B_02,1))]))
%~
%~ replacing_with_var( body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(add_token(+),A_01,subscript_value_slice(B_02,0)),
%~                                                               bin_op_left_right(add_token(+),A_01,subscript_value_slice(B_02,1))]))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[A_01,"int"]))
%~
%~ replacing_with_var(call_func_args("isinstance",[B_02,"tuple"]))
%~
%~ replacing_with_var( return_value( tuple_elts( [ bin_op_left_right(add_token(+),A_01,subscript_value_slice(B_02,0)),
%~                                                 bin_op_left_right(add_token(+),A_01,subscript_value_slice(B_02,1))])))
%~
%~ replacing_with_var( tuple_elts( [ bin_op_left_right(add_token(+),A_01,subscript_value_slice(B_02,0)),
%~                                   bin_op_left_right(add_token(+),A_01,subscript_value_slice(B_02,1))]))
%~
% Compiled KL-1 for add
add(A_01,B_02,NUMERICAL_03) :-
  willBeType(NUMERICAL_03,'Numerical') ,
  comment(' addition ') ,
  subscript_value_slice(A_01,0,ARG_027) ,
  subscript_value_slice(B_02,0,ARG_028) ,
  call([op_call(add_token(+,ARG_027,ARG_028,ARG_022))]) ,
  subscript_value_slice(A_01,1,ARG_029) ,
  subscript_value_slice(B_02,1,ARG_030) ,
  call([op_call(add_token(+,ARG_029,ARG_030,ARG_023))]) ,
  tuple_elts(ARG_022,ARG_023,ARG_021) ,
  return_value(ARG_021,ARG_018) ,
  body_stmts([ARG_018],ARG_014) ,
  subscript_value_slice(B_02,0,ARG_035) ,
  call([op_call(add_token(+,A_01,ARG_035,ARG_033))]) ,
  subscript_value_slice(B_02,1,ARG_036) ,
  call([op_call(add_token(+,A_01,ARG_036,ARG_034))]) ,
  tuple_elts(ARG_033,ARG_034,ARG_032) ,
  return_value(ARG_032,ARG_031) ,
  body_stmts([ARG_031],ARG_024) ,
  if_test_body(
     [ isinstance(A_01,INT,ARG_025),
       isinstance(B_02,TUPLE,ARG_026),
       bool_op_values(['python:And'],[ARG_025,ARG_026])], ARG_024,ARG_019) ,
  orelse_else_stmts([ARG_019],ARG_015) ,
  if_test_body_orelse(
     [ isinstance(A_01,TUPLE,ARG_016),
       isinstance(B_02,TUPLE,ARG_017),
       bool_op_values(['python:And'],[ARG_016,ARG_017])], ARG_014,ARG_015,
     ARG_010) ,
  orelse_else_stmts([ARG_010],ARG_04) ,
  (/*2*/
    [ call([op_call(add_token(+,A_01,B_02,ARG_020))]),
      return_value(ARG_020,ARG_011),
      body_stmts([ARG_011],ARG_05),
      testif( [ isinstance(A_01,INT,ARG_06),
                isinstance(B_02,INT,ARG_07),
                bool_op_values(['python:And'],[ARG_06,ARG_07])]) ->
        ARG_05] ;
    ARG_04) ,
  subscript_value_slice(A_01,0,ARG_012) ,
  call([op_call(add_token(+,ARG_012,B_02,ARG_08))]) ,
  subscript_value_slice(A_01,1,ARG_013) ,
  call([op_call(add_token(+,ARG_013,B_02,ARG_09))]) ,
  tuple_elts(ARG_08,ARG_09,NUMERICAL_03) ,
  exit_proc(NUMERICAL_03).
%~ % Universal AST Pass #0
%~ def( "subtract",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("Numerical")]),
%~      block_statements( [ expr_value(string_value(' subtraction ')),
%~                          if_test_body_orelse(
%~                             bool_op_values( ['python:And'], [
%~                               call_func_args("isinstance",["a","int"]),
%~                               call_func_args("isinstance",["b","int"])]),
%~                             body_stmts([return_value(bin_op_left_right(sub_token(-),"a","b"))]),
%~                             orelse_else_stmts( [ if_test_body_orelse(
%~                                                     bool_op_values( ['python:And'], [
%~                                                       call_func_args("isinstance",["a","tuple"]),
%~                                                       call_func_args("isinstance",["b","tuple"])]),
%~                                                     body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(sub_token(-),subscript_value_slice("a",0),subscript_value_slice("b",0)),
%~                                                                                               bin_op_left_right(sub_token(-),subscript_value_slice("a",1),subscript_value_slice("b",1))]))]),
%~                                                     orelse_else_stmts( [ if_test_body(
%~                                                                             bool_op_values( ['python:And'], [
%~                                                                               call_func_args("isinstance",["a","int"]),
%~                                                                               call_func_args("isinstance",["b","tuple"])]),
%~                                                                             body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(sub_token(-),"a",subscript_value_slice("b",0)),
%~                                                                                                                       bin_op_left_right(sub_token(-),"a",subscript_value_slice("b",1))]))]))]))])),
%~                          return_value( tuple_elts( [ bin_op_left_right(sub_token(-),subscript_value_slice("a",0),"b"),
%~                                                      bin_op_left_right(sub_token(-),subscript_value_slice("a",1),"b")]))])))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body_orelse(
%~                                             bool_op_values( ['python:And'], [
%~                                               call_func_args("isinstance",[A_01,"tuple"]),
%~                                               call_func_args("isinstance",[B_02,"tuple"])]),
%~                                             body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(sub_token(-),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                                                       bin_op_left_right(sub_token(-),subscript_value_slice(A_01,1),subscript_value_slice(B_02,1))]))]),
%~                                             orelse_else_stmts( [ if_test_body(
%~                                                                     bool_op_values( ['python:And'], [
%~                                                                       call_func_args("isinstance",[A_01,"int"]),
%~                                                                       call_func_args("isinstance",[B_02,"tuple"])]),
%~                                                                     body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(sub_token(-),A_01,subscript_value_slice(B_02,0)),
%~                                                                                                               bin_op_left_right(sub_token(-),A_01,subscript_value_slice(B_02,1))]))]))]))]))
%~
%~ replacing_with_var(body_stmts([return_value(bin_op_left_right(sub_token(-),A_01,B_02))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[A_01,"int"]))
%~
%~ replacing_with_var(call_func_args("isinstance",[B_02,"int"]))
%~
%~ replacing_with_var( if_test_body_orelse(
%~                        bool_op_values( ['python:And'], [
%~                          call_func_args("isinstance",[A_01,"tuple"]),
%~                          call_func_args("isinstance",[B_02,"tuple"])]),
%~                        body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(sub_token(-),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                                  bin_op_left_right(sub_token(-),subscript_value_slice(A_01,1),subscript_value_slice(B_02,1))]))]),
%~                        orelse_else_stmts( [ if_test_body(
%~                                                bool_op_values( ['python:And'], [
%~                                                  call_func_args("isinstance",[A_01,"int"]),
%~                                                  call_func_args("isinstance",[B_02,"tuple"])]),
%~                                                body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(sub_token(-),A_01,subscript_value_slice(B_02,0)),
%~                                                                                          bin_op_left_right(sub_token(-),A_01,subscript_value_slice(B_02,1))]))]))])))
%~
%~ replacing_with_var(return_value(bin_op_left_right(sub_token(-),A_01,B_02)))
%~
%~ replacing_with_var( body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(sub_token(-),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                               bin_op_left_right(sub_token(-),subscript_value_slice(A_01,1),subscript_value_slice(B_02,1))]))]))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body(
%~                                             bool_op_values( ['python:And'], [
%~                                               call_func_args("isinstance",[A_01,"int"]),
%~                                               call_func_args("isinstance",[B_02,"tuple"])]),
%~                                             body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(sub_token(-),A_01,subscript_value_slice(B_02,0)),
%~                                                                                       bin_op_left_right(sub_token(-),A_01,subscript_value_slice(B_02,1))]))]))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[A_01,"tuple"]))
%~
%~ replacing_with_var(call_func_args("isinstance",[B_02,"tuple"]))
%~
%~ replacing_with_var( return_value( tuple_elts( [ bin_op_left_right(sub_token(-),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                 bin_op_left_right(sub_token(-),subscript_value_slice(A_01,1),subscript_value_slice(B_02,1))])))
%~
%~ replacing_with_var( if_test_body(
%~                        bool_op_values( ['python:And'], [
%~                          call_func_args("isinstance",[A_01,"int"]),
%~                          call_func_args("isinstance",[B_02,"tuple"])]),
%~                        body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(sub_token(-),A_01,subscript_value_slice(B_02,0)),
%~                                                                  bin_op_left_right(sub_token(-),A_01,subscript_value_slice(B_02,1))]))])))
%~
%~ replacing_with_var(call_func_args(sub_token(-),[A_01,B_02]))
%~
%~ replacing_with_var( tuple_elts( [ bin_op_left_right(sub_token(-),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                   bin_op_left_right(sub_token(-),subscript_value_slice(A_01,1),subscript_value_slice(B_02,1))]))
%~
%~ replacing_with_var( body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(sub_token(-),A_01,subscript_value_slice(B_02,0)),
%~                                                               bin_op_left_right(sub_token(-),A_01,subscript_value_slice(B_02,1))]))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[A_01,"int"]))
%~
%~ replacing_with_var(call_func_args("isinstance",[B_02,"tuple"]))
%~
%~ replacing_with_var( return_value( tuple_elts( [ bin_op_left_right(sub_token(-),A_01,subscript_value_slice(B_02,0)),
%~                                                 bin_op_left_right(sub_token(-),A_01,subscript_value_slice(B_02,1))])))
%~
%~ replacing_with_var( tuple_elts( [ bin_op_left_right(sub_token(-),A_01,subscript_value_slice(B_02,0)),
%~                                   bin_op_left_right(sub_token(-),A_01,subscript_value_slice(B_02,1))]))
%~
% Compiled KL-1 for subtract
subtract(A_01,B_02,NUMERICAL_03) :-
  willBeType(NUMERICAL_03,'Numerical') ,
  comment(' subtraction ') ,
  subscript_value_slice(A_01,0,ARG_027) ,
  subscript_value_slice(B_02,0,ARG_028) ,
  call([op_call(sub_token(-,ARG_027,ARG_028,ARG_022))]) ,
  subscript_value_slice(A_01,1,ARG_029) ,
  subscript_value_slice(B_02,1,ARG_030) ,
  call([op_call(sub_token(-,ARG_029,ARG_030,ARG_023))]) ,
  tuple_elts(ARG_022,ARG_023,ARG_021) ,
  return_value(ARG_021,ARG_018) ,
  body_stmts([ARG_018],ARG_014) ,
  subscript_value_slice(B_02,0,ARG_035) ,
  call([op_call(sub_token(-,A_01,ARG_035,ARG_033))]) ,
  subscript_value_slice(B_02,1,ARG_036) ,
  call([op_call(sub_token(-,A_01,ARG_036,ARG_034))]) ,
  tuple_elts(ARG_033,ARG_034,ARG_032) ,
  return_value(ARG_032,ARG_031) ,
  body_stmts([ARG_031],ARG_024) ,
  if_test_body(
     [ isinstance(A_01,INT,ARG_025),
       isinstance(B_02,TUPLE,ARG_026),
       bool_op_values(['python:And'],[ARG_025,ARG_026])], ARG_024,ARG_019) ,
  orelse_else_stmts([ARG_019],ARG_015) ,
  if_test_body_orelse(
     [ isinstance(A_01,TUPLE,ARG_016),
       isinstance(B_02,TUPLE,ARG_017),
       bool_op_values(['python:And'],[ARG_016,ARG_017])], ARG_014,ARG_015,
     ARG_010) ,
  orelse_else_stmts([ARG_010],ARG_04) ,
  (/*2*/
    [ call([op_call(sub_token(-,A_01,B_02,ARG_020))]),
      return_value(ARG_020,ARG_011),
      body_stmts([ARG_011],ARG_05),
      testif( [ isinstance(A_01,INT,ARG_06),
                isinstance(B_02,INT,ARG_07),
                bool_op_values(['python:And'],[ARG_06,ARG_07])]) ->
        ARG_05] ;
    ARG_04) ,
  subscript_value_slice(A_01,0,ARG_012) ,
  call([op_call(sub_token(-,ARG_012,B_02,ARG_08))]) ,
  subscript_value_slice(A_01,1,ARG_013) ,
  call([op_call(sub_token(-,ARG_013,B_02,ARG_09))]) ,
  tuple_elts(ARG_08,ARG_09,NUMERICAL_03) ,
  exit_proc(NUMERICAL_03).
%~ % Universal AST Pass #0
%~ def( "multiply",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("Numerical")]),
%~      block_statements( [ expr_value(string_value(' multiplication ')),
%~                          if_test_body_orelse(
%~                             bool_op_values( ['python:And'], [
%~                               call_func_args("isinstance",["a","int"]),
%~                               call_func_args("isinstance",["b","int"])]),
%~                             body_stmts([return_value(bin_op_left_right(mult_token(*),"a","b"))]),
%~                             orelse_else_stmts( [ if_test_body_orelse(
%~                                                     bool_op_values( ['python:And'], [
%~                                                       call_func_args("isinstance",["a","tuple"]),
%~                                                       call_func_args("isinstance",["b","tuple"])]),
%~                                                     body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(mult_token(*),subscript_value_slice("a",0),subscript_value_slice("b",0)),
%~                                                                                               bin_op_left_right(mult_token(*),subscript_value_slice("a",1),subscript_value_slice("b",1))]))]),
%~                                                     orelse_else_stmts( [ if_test_body(
%~                                                                             bool_op_values( ['python:And'], [
%~                                                                               call_func_args("isinstance",["a","int"]),
%~                                                                               call_func_args("isinstance",["b","tuple"])]),
%~                                                                             body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(mult_token(*),"a",subscript_value_slice("b",0)),
%~                                                                                                                       bin_op_left_right(mult_token(*),"a",subscript_value_slice("b",1))]))]))]))])),
%~                          return_value( tuple_elts( [ bin_op_left_right(mult_token(*),subscript_value_slice("a",0),"b"),
%~                                                      bin_op_left_right(mult_token(*),subscript_value_slice("a",1),"b")]))])))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body_orelse(
%~                                             bool_op_values( ['python:And'], [
%~                                               call_func_args("isinstance",[A_01,"tuple"]),
%~                                               call_func_args("isinstance",[B_02,"tuple"])]),
%~                                             body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(mult_token(*),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                                                       bin_op_left_right(mult_token(*),subscript_value_slice(A_01,1),subscript_value_slice(B_02,1))]))]),
%~                                             orelse_else_stmts( [ if_test_body(
%~                                                                     bool_op_values( ['python:And'], [
%~                                                                       call_func_args("isinstance",[A_01,"int"]),
%~                                                                       call_func_args("isinstance",[B_02,"tuple"])]),
%~                                                                     body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(mult_token(*),A_01,subscript_value_slice(B_02,0)),
%~                                                                                                               bin_op_left_right(mult_token(*),A_01,subscript_value_slice(B_02,1))]))]))]))]))
%~
%~ replacing_with_var(body_stmts([return_value(bin_op_left_right(mult_token(*),A_01,B_02))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[A_01,"int"]))
%~
%~ replacing_with_var(call_func_args("isinstance",[B_02,"int"]))
%~
%~ replacing_with_var( if_test_body_orelse(
%~                        bool_op_values( ['python:And'], [
%~                          call_func_args("isinstance",[A_01,"tuple"]),
%~                          call_func_args("isinstance",[B_02,"tuple"])]),
%~                        body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(mult_token(*),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                                  bin_op_left_right(mult_token(*),subscript_value_slice(A_01,1),subscript_value_slice(B_02,1))]))]),
%~                        orelse_else_stmts( [ if_test_body(
%~                                                bool_op_values( ['python:And'], [
%~                                                  call_func_args("isinstance",[A_01,"int"]),
%~                                                  call_func_args("isinstance",[B_02,"tuple"])]),
%~                                                body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(mult_token(*),A_01,subscript_value_slice(B_02,0)),
%~                                                                                          bin_op_left_right(mult_token(*),A_01,subscript_value_slice(B_02,1))]))]))])))
%~
%~ replacing_with_var(return_value(bin_op_left_right(mult_token(*),A_01,B_02)))
%~
%~ replacing_with_var( body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(mult_token(*),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                               bin_op_left_right(mult_token(*),subscript_value_slice(A_01,1),subscript_value_slice(B_02,1))]))]))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body(
%~                                             bool_op_values( ['python:And'], [
%~                                               call_func_args("isinstance",[A_01,"int"]),
%~                                               call_func_args("isinstance",[B_02,"tuple"])]),
%~                                             body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(mult_token(*),A_01,subscript_value_slice(B_02,0)),
%~                                                                                       bin_op_left_right(mult_token(*),A_01,subscript_value_slice(B_02,1))]))]))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[A_01,"tuple"]))
%~
%~ replacing_with_var(call_func_args("isinstance",[B_02,"tuple"]))
%~
%~ replacing_with_var( return_value( tuple_elts( [ bin_op_left_right(mult_token(*),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                 bin_op_left_right(mult_token(*),subscript_value_slice(A_01,1),subscript_value_slice(B_02,1))])))
%~
%~ replacing_with_var( if_test_body(
%~                        bool_op_values( ['python:And'], [
%~                          call_func_args("isinstance",[A_01,"int"]),
%~                          call_func_args("isinstance",[B_02,"tuple"])]),
%~                        body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(mult_token(*),A_01,subscript_value_slice(B_02,0)),
%~                                                                  bin_op_left_right(mult_token(*),A_01,subscript_value_slice(B_02,1))]))])))
%~
%~ replacing_with_var(call_func_args(mult_token(*),[A_01,B_02]))
%~
%~ replacing_with_var( tuple_elts( [ bin_op_left_right(mult_token(*),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                   bin_op_left_right(mult_token(*),subscript_value_slice(A_01,1),subscript_value_slice(B_02,1))]))
%~
%~ replacing_with_var( body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(mult_token(*),A_01,subscript_value_slice(B_02,0)),
%~                                                               bin_op_left_right(mult_token(*),A_01,subscript_value_slice(B_02,1))]))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[A_01,"int"]))
%~
%~ replacing_with_var(call_func_args("isinstance",[B_02,"tuple"]))
%~
%~ replacing_with_var( return_value( tuple_elts( [ bin_op_left_right(mult_token(*),A_01,subscript_value_slice(B_02,0)),
%~                                                 bin_op_left_right(mult_token(*),A_01,subscript_value_slice(B_02,1))])))
%~
%~ replacing_with_var( tuple_elts( [ bin_op_left_right(mult_token(*),A_01,subscript_value_slice(B_02,0)),
%~                                   bin_op_left_right(mult_token(*),A_01,subscript_value_slice(B_02,1))]))
%~
% Compiled KL-1 for multiply
multiply(A_01,B_02,NUMERICAL_03) :-
  willBeType(NUMERICAL_03,'Numerical') ,
  comment(' multiplication ') ,
  subscript_value_slice(A_01,0,ARG_027) ,
  subscript_value_slice(B_02,0,ARG_028) ,
  call([op_call(mult_token(*,ARG_027,ARG_028,ARG_022))]) ,
  subscript_value_slice(A_01,1,ARG_029) ,
  subscript_value_slice(B_02,1,ARG_030) ,
  call([op_call(mult_token(*,ARG_029,ARG_030,ARG_023))]) ,
  tuple_elts(ARG_022,ARG_023,ARG_021) ,
  return_value(ARG_021,ARG_018) ,
  body_stmts([ARG_018],ARG_014) ,
  subscript_value_slice(B_02,0,ARG_035) ,
  call([op_call(mult_token(*,A_01,ARG_035,ARG_033))]) ,
  subscript_value_slice(B_02,1,ARG_036) ,
  call([op_call(mult_token(*,A_01,ARG_036,ARG_034))]) ,
  tuple_elts(ARG_033,ARG_034,ARG_032) ,
  return_value(ARG_032,ARG_031) ,
  body_stmts([ARG_031],ARG_024) ,
  if_test_body(
     [ isinstance(A_01,INT,ARG_025),
       isinstance(B_02,TUPLE,ARG_026),
       bool_op_values(['python:And'],[ARG_025,ARG_026])], ARG_024,ARG_019) ,
  orelse_else_stmts([ARG_019],ARG_015) ,
  if_test_body_orelse(
     [ isinstance(A_01,TUPLE,ARG_016),
       isinstance(B_02,TUPLE,ARG_017),
       bool_op_values(['python:And'],[ARG_016,ARG_017])], ARG_014,ARG_015,
     ARG_010) ,
  orelse_else_stmts([ARG_010],ARG_04) ,
  (/*2*/
    [ call([op_call(mult_token(*,A_01,B_02,ARG_020))]),
      return_value(ARG_020,ARG_011),
      body_stmts([ARG_011],ARG_05),
      testif( [ isinstance(A_01,INT,ARG_06),
                isinstance(B_02,INT,ARG_07),
                bool_op_values(['python:And'],[ARG_06,ARG_07])]) ->
        ARG_05] ;
    ARG_04) ,
  subscript_value_slice(A_01,0,ARG_012) ,
  call([op_call(mult_token(*,ARG_012,B_02,ARG_08))]) ,
  subscript_value_slice(A_01,1,ARG_013) ,
  call([op_call(mult_token(*,ARG_013,B_02,ARG_09))]) ,
  tuple_elts(ARG_08,ARG_09,NUMERICAL_03) ,
  exit_proc(NUMERICAL_03).
%~ % Universal AST Pass #0
%~ def( "divide",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("Numerical")]),
%~      block_statements( [ expr_value(string_value(' floor division ')),
%~                          if_test_body_orelse(
%~                             bool_op_values( ['python:And'], [
%~                               call_func_args("isinstance",["a","int"]),
%~                               call_func_args("isinstance",["b","int"])]),
%~                             body_stmts([return_value(bin_op_left_right(floor_div_token(//),"a","b"))]),
%~                             orelse_else_stmts( [ if_test_body_orelse(
%~                                                     bool_op_values( ['python:And'], [
%~                                                       call_func_args("isinstance",["a","tuple"]),
%~                                                       call_func_args("isinstance",["b","tuple"])]),
%~                                                     body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(floor_div_token(//),subscript_value_slice("a",0),subscript_value_slice("b",0)),
%~                                                                                               bin_op_left_right(floor_div_token(//),subscript_value_slice("a",1),subscript_value_slice("b",1))]))]),
%~                                                     orelse_else_stmts( [ if_test_body(
%~                                                                             bool_op_values( ['python:And'], [
%~                                                                               call_func_args("isinstance",["a","int"]),
%~                                                                               call_func_args("isinstance",["b","tuple"])]),
%~                                                                             body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(floor_div_token(//),"a",subscript_value_slice("b",0)),
%~                                                                                                                       bin_op_left_right(floor_div_token(//),"a",subscript_value_slice("b",1))]))]))]))])),
%~                          return_value( tuple_elts( [ bin_op_left_right(floor_div_token(//),subscript_value_slice("a",0),"b"),
%~                                                      bin_op_left_right(floor_div_token(//),subscript_value_slice("a",1),"b")]))])))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body_orelse(
%~                                             bool_op_values( ['python:And'], [
%~                                               call_func_args("isinstance",[A_01,"tuple"]),
%~                                               call_func_args("isinstance",[B_02,"tuple"])]),
%~                                             body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(floor_div_token(//),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                                                       bin_op_left_right( floor_div_token(//),
%~                                                                                         subscript_value_slice(A_01,1),
%~                                                                                         subscript_value_slice(B_02,1))]))]),
%~                                             orelse_else_stmts( [ if_test_body(
%~                                                                     bool_op_values( ['python:And'], [
%~                                                                       call_func_args("isinstance",[A_01,"int"]),
%~                                                                       call_func_args("isinstance",[B_02,"tuple"])]),
%~                                                                     body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(floor_div_token(//),A_01,subscript_value_slice(B_02,0)),
%~                                                                                                               bin_op_left_right(floor_div_token(//),A_01,subscript_value_slice(B_02,1))]))]))]))]))
%~
%~ replacing_with_var( body_stmts([return_value(bin_op_left_right(floor_div_token(//),A_01,B_02))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[A_01,"int"]))
%~
%~ replacing_with_var(call_func_args("isinstance",[B_02,"int"]))
%~
%~ replacing_with_var( if_test_body_orelse(
%~                        bool_op_values( ['python:And'], [
%~                          call_func_args("isinstance",[A_01,"tuple"]),
%~                          call_func_args("isinstance",[B_02,"tuple"])]),
%~                        body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(floor_div_token(//),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                                  bin_op_left_right( floor_div_token(//),
%~                                                                    subscript_value_slice(A_01,1),
%~                                                                    subscript_value_slice(B_02,1))]))]),
%~                        orelse_else_stmts( [ if_test_body(
%~                                                bool_op_values( ['python:And'], [
%~                                                  call_func_args("isinstance",[A_01,"int"]),
%~                                                  call_func_args("isinstance",[B_02,"tuple"])]),
%~                                                body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(floor_div_token(//),A_01,subscript_value_slice(B_02,0)),
%~                                                                                          bin_op_left_right(floor_div_token(//),A_01,subscript_value_slice(B_02,1))]))]))])))
%~
%~ replacing_with_var(return_value(bin_op_left_right(floor_div_token(//),A_01,B_02)))
%~
%~ replacing_with_var( body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(floor_div_token(//),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                               bin_op_left_right( floor_div_token(//),
%~                                                                 subscript_value_slice(A_01,1),
%~                                                                 subscript_value_slice(B_02,1))]))]))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body(
%~                                             bool_op_values( ['python:And'], [
%~                                               call_func_args("isinstance",[A_01,"int"]),
%~                                               call_func_args("isinstance",[B_02,"tuple"])]),
%~                                             body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(floor_div_token(//),A_01,subscript_value_slice(B_02,0)),
%~                                                                                       bin_op_left_right(floor_div_token(//),A_01,subscript_value_slice(B_02,1))]))]))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[A_01,"tuple"]))
%~
%~ replacing_with_var(call_func_args("isinstance",[B_02,"tuple"]))
%~
%~ replacing_with_var( return_value( tuple_elts( [ bin_op_left_right(floor_div_token(//),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                                 bin_op_left_right( floor_div_token(//),
%~                                                   subscript_value_slice(A_01,1),
%~                                                   subscript_value_slice(B_02,1))])))
%~
%~ replacing_with_var( if_test_body(
%~                        bool_op_values( ['python:And'], [
%~                          call_func_args("isinstance",[A_01,"int"]),
%~                          call_func_args("isinstance",[B_02,"tuple"])]),
%~                        body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(floor_div_token(//),A_01,subscript_value_slice(B_02,0)),
%~                                                                  bin_op_left_right(floor_div_token(//),A_01,subscript_value_slice(B_02,1))]))])))
%~
%~ replacing_with_var(call_func_args(floor_div_token(//),[A_01,B_02]))
%~
%~ replacing_with_var( tuple_elts( [ bin_op_left_right(floor_div_token(//),subscript_value_slice(A_01,0),subscript_value_slice(B_02,0)),
%~                                   bin_op_left_right( floor_div_token(//),
%~                                     subscript_value_slice(A_01,1),
%~                                     subscript_value_slice(B_02,1))]))
%~
%~ replacing_with_var( body_stmts( [ return_value( tuple_elts( [ bin_op_left_right(floor_div_token(//),A_01,subscript_value_slice(B_02,0)),
%~                                                               bin_op_left_right(floor_div_token(//),A_01,subscript_value_slice(B_02,1))]))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[A_01,"int"]))
%~
%~ replacing_with_var(call_func_args("isinstance",[B_02,"tuple"]))
%~
%~ replacing_with_var( return_value( tuple_elts( [ bin_op_left_right(floor_div_token(//),A_01,subscript_value_slice(B_02,0)),
%~                                                 bin_op_left_right(floor_div_token(//),A_01,subscript_value_slice(B_02,1))])))
%~
%~ replacing_with_var( tuple_elts( [ bin_op_left_right(floor_div_token(//),A_01,subscript_value_slice(B_02,0)),
%~                                   bin_op_left_right(floor_div_token(//),A_01,subscript_value_slice(B_02,1))]))
%~
% Compiled KL-1 for divide
divide(A_01,B_02,NUMERICAL_03) :-
  willBeType(NUMERICAL_03,'Numerical') ,
  comment(' floor division ') ,
  subscript_value_slice(A_01,0,ARG_027) ,
  subscript_value_slice(B_02,0,ARG_028) ,
  call([op_call(floor_div_token(//,ARG_027,ARG_028,ARG_022))]) ,
  subscript_value_slice(A_01,1,ARG_029) ,
  subscript_value_slice(B_02,1,ARG_030) ,
  call([op_call(floor_div_token(//,ARG_029,ARG_030,ARG_023))]) ,
  tuple_elts(ARG_022,ARG_023,ARG_021) ,
  return_value(ARG_021,ARG_018) ,
  body_stmts([ARG_018],ARG_014) ,
  subscript_value_slice(B_02,0,ARG_035) ,
  call([op_call(floor_div_token(//,A_01,ARG_035,ARG_033))]) ,
  subscript_value_slice(B_02,1,ARG_036) ,
  call([op_call(floor_div_token(//,A_01,ARG_036,ARG_034))]) ,
  tuple_elts(ARG_033,ARG_034,ARG_032) ,
  return_value(ARG_032,ARG_031) ,
  body_stmts([ARG_031],ARG_024) ,
  if_test_body(
     [ isinstance(A_01,INT,ARG_025),
       isinstance(B_02,TUPLE,ARG_026),
       bool_op_values(['python:And'],[ARG_025,ARG_026])], ARG_024,ARG_019) ,
  orelse_else_stmts([ARG_019],ARG_015) ,
  if_test_body_orelse(
     [ isinstance(A_01,TUPLE,ARG_016),
       isinstance(B_02,TUPLE,ARG_017),
       bool_op_values(['python:And'],[ARG_016,ARG_017])], ARG_014,ARG_015,
     ARG_010) ,
  orelse_else_stmts([ARG_010],ARG_04) ,
  (/*2*/
    [ call([op_call(floor_div_token(//,A_01,B_02,ARG_020))]),
      return_value(ARG_020,ARG_011),
      body_stmts([ARG_011],ARG_05),
      testif( [ isinstance(A_01,INT,ARG_06),
                isinstance(B_02,INT,ARG_07),
                bool_op_values(['python:And'],[ARG_06,ARG_07])]) ->
        ARG_05] ;
    ARG_04) ,
  subscript_value_slice(A_01,0,ARG_012) ,
  call([op_call(floor_div_token(//,ARG_012,B_02,ARG_08))]) ,
  subscript_value_slice(A_01,1,ARG_013) ,
  call([op_call(floor_div_token(//,ARG_013,B_02,ARG_09))]) ,
  tuple_elts(ARG_08,ARG_09,NUMERICAL_03) ,
  exit_proc(NUMERICAL_03).
%~ % Universal AST Pass #0
%~ def( "invert",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("n")],[argument_type("Numerical")]),
%~      block_statements( [ expr_value(string_value(' inversion with respect to addition ')),
%~                          return_value( if_exp_test_body_orelse(
%~                                           call_func_args("isinstance",["n","int"]),
%~                                           unary_op_operand(us_ub_token(-),"n"),
%~                                           tuple_elts( [ unary_op_operand(us_ub_token(-),subscript_value_slice("n",0)),
%~                                                         unary_op_operand(us_ub_token(-),subscript_value_slice("n",1))])))])))
%~
%~ replacing_with_var(call_func_args("isinstance",[N_01,"int"]))
%~
%~ replacing_with_var(subscript_value_slice(N_01,0))
%~
%~ replacing_with_var(subscript_value_slice(N_01,1))
%~
% Compiled KL-1 for invert
invert(N_01,NUMERICAL_02) :-
  willBeType(NUMERICAL_02,'Numerical') ,
  comment(' inversion with respect to addition ') ,
  (/*2*/
    [ isinstance(N_01,INT,ARG_03),
      testif(ARG_03)] ->
      [ unary_op_operand(us_ub_token(-),N_01,NUMERICAL_02)] ;
    [ subscript_value_slice(N_01,0,ARG_06),
      unary_op_operand(us_ub_token(-),ARG_06,ARG_04),
      subscript_value_slice(N_01,1,ARG_07),
      unary_op_operand(us_ub_token(-),ARG_07,ARG_05),
      tuple_elts(ARG_04,ARG_05,NUMERICAL_02)]) ,
  exit_proc(NUMERICAL_02).
%~ % Universal AST Pass #0
%~ def( "even",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("n")],[argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' evenness ')),
%~                          return_value(compare_ops_left_comparators(eq_token(==),bin_op_left_right(mod_token('%'),"n",2),0))])))
%~
%~ replacing_with_var(bin_op_left_right(mod_token('%'),N_01,2))
%~
% Compiled KL-1 for even
even(N_01,BOOLEAN_02) :-
  willBeType(BOOLEAN_02,'Boolean') ,
  comment(' evenness ') ,
  call([op_call(mod_token('%',N_01,2,ARG_03))]) ,
  compare_ops_left_comparators(eq_token(==),ARG_03,0,BOOLEAN_02) ,
  exit_proc(BOOLEAN_02).
%~ % Universal AST Pass #0
%~ def( "double",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("n")],[argument_type("Numerical")]),
%~      block_statements( [ expr_value(string_value(' scaling by two ')),
%~                          return_value( if_exp_test_body_orelse(
%~                                           call_func_args("isinstance",["n","int"]),
%~                                           bin_op_left_right(mult_token(*),"n",2),
%~                                           tuple_elts( [ bin_op_left_right(mult_token(*),subscript_value_slice("n",0),2),
%~                                                         bin_op_left_right(mult_token(*),subscript_value_slice("n",1),2)])))])))
%~
%~ replacing_with_var(call_func_args("isinstance",[N_01,"int"]))
%~
% Compiled KL-1 for double
double(N_01,NUMERICAL_02) :-
  willBeType(NUMERICAL_02,'Numerical') ,
  comment(' scaling by two ') ,
  (/*2*/
    [ isinstance(N_01,INT,ARG_03),
      testif(ARG_03)] ->
      [call([op_call(mult_token(*,N_01,2,NUMERICAL_02))])] ;
    [ subscript_value_slice(N_01,0,ARG_06),
      call([op_call(mult_token(*,ARG_06,2,ARG_04))]),
      subscript_value_slice(N_01,1,ARG_07),
      call([op_call(mult_token(*,ARG_07,2,ARG_05))]),
      tuple_elts(ARG_04,ARG_05,NUMERICAL_02)]) ,
  exit_proc(NUMERICAL_02).
%~ % Universal AST Pass #0
%~ def( "halve",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("n")],[argument_type("Numerical")]),
%~      block_statements( [ expr_value(string_value(' scaling by one half ')),
%~                          return_value( if_exp_test_body_orelse(
%~                                           call_func_args("isinstance",["n","int"]),
%~                                           bin_op_left_right(floor_div_token(//),"n",2),
%~                                           tuple_elts( [ bin_op_left_right(floor_div_token(//),subscript_value_slice("n",0),2),
%~                                                         bin_op_left_right(floor_div_token(//),subscript_value_slice("n",1),2)])))])))
%~
%~ replacing_with_var(call_func_args("isinstance",[N_01,"int"]))
%~
% Compiled KL-1 for halve
halve(N_01,NUMERICAL_02) :-
  willBeType(NUMERICAL_02,'Numerical') ,
  comment(' scaling by one half ') ,
  (/*2*/
    [ isinstance(N_01,INT,ARG_03),
      testif(ARG_03)] ->
      [call([op_call(floor_div_token(//,N_01,2,NUMERICAL_02))])] ;
    [ subscript_value_slice(N_01,0,ARG_06),
      call([op_call(floor_div_token(//,ARG_06,2,ARG_04))]),
      subscript_value_slice(N_01,1,ARG_07),
      call([op_call(floor_div_token(//,ARG_07,2,ARG_05))]),
      tuple_elts(ARG_04,ARG_05,NUMERICAL_02)]) ,
  exit_proc(NUMERICAL_02).
%~ % Universal AST Pass #0
%~ def( "flip",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("b")],[argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' logical not ')),
%~                          return_value(unary_op_operand(['python:Not'],"b"))])))
%~
% Compiled KL-1 for flip
flip(B_01,BOOLEAN_02) :-
  willBeType(BOOLEAN_02,'Boolean') ,
  comment(' logical not ') ,
  unary_op_operand(['python:Not'],B_01,BOOLEAN_02) ,
  exit_proc(BOOLEAN_02).
%~ % Universal AST Pass #0
%~ def( "equality",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' equality ')),
%~                          return_value(compare_ops_left_comparators(eq_token(==),"a","b"))])))
%~
% Compiled KL-1 for equality
equality(A_01,B_02,BOOLEAN_03) :-
  willBeType(BOOLEAN_03,'Boolean') ,
  comment(' equality ') ,
  compare_ops_left_comparators(eq_token(==),A_01,B_02,BOOLEAN_03) ,
  exit_proc(BOOLEAN_03).
%~ % Universal AST Pass #0
%~ def( "contained",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("value"),
%~           argument_name("container")],
%~         [argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' element of ')),
%~                          return_value(compare_ops_left_comparators(['python:In'],"value","container"))])))
%~
% Compiled KL-1 for contained
contained(VALUE_01,CONTAINER_02,BOOLEAN_03) :-
  willBeType(BOOLEAN_03,'Boolean') ,
  comment(' element of ') ,
  compare_ops_left_comparators(['python:In'],VALUE_01,CONTAINER_02,BOOLEAN_03) ,
  exit_proc(BOOLEAN_03).
%~ % Universal AST Pass #0
%~ def( "combine",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("Container")]),
%~      block_statements( [ expr_value(string_value(' union ')),
%~                          return_value( call_func_args(
%~                                           call_func_args("type",["a"]),
%~                                           [ tuple_elts([starred_value("a"),starred_value("b")])]))])))
%~
% Compiled KL-1 for combine
combine(A_01,B_02,CONTAINER_03) :-
  willBeType(CONTAINER_03,'Container') ,
  comment(' union ') ,
  starred_value(A_01,ARG_05) ,
  starred_value(B_02,ARG_06) ,
  tuple_elts(ARG_05,ARG_06,ARG_04) ,
  call([op_call(call_func_args("type",[A_01],ARG_04,CONTAINER_03))]) ,
  exit_proc(CONTAINER_03).
%~ % Universal AST Pass #0
%~ def( "intersection",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("FrozenSet")]),
%~      block_statements( [ expr_value(string_value(' returns the intersection of two containers ')),
%~                          return_value(bin_op_left_right(bit_and_token(&),"a","b"))])))
%~
% Compiled KL-1 for intersection
intersection(A_01,B_02,FROZENSET_03) :-
  willBeType(FROZENSET_03,'FrozenSet') ,
  comment(' returns the intersection of two containers ') ,
  call([op_call(bit_and_token(&,A_01,B_02,FROZENSET_03))]) ,
  exit_proc(FROZENSET_03).
%~ % Universal AST Pass #0
%~ def( "difference",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("FrozenSet")]),
%~      block_statements( [ expr_value(string_value(' set difference ')),
%~                          return_value(bin_op_left_right(sub_token(-),"a","b"))])))
%~
% Compiled KL-1 for difference
difference(A_01,B_02,FROZENSET_03) :-
  willBeType(FROZENSET_03,'FrozenSet') ,
  comment(' set difference ') ,
  call([op_call(sub_token(-,A_01,B_02,FROZENSET_03))]) ,
  exit_proc(FROZENSET_03).
%~ % Universal AST Pass #0
%~ def( "dedupe",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("tup")],[argument_type("Tuple")]),
%~      block_statements( [ expr_value(string_value(' remove duplicates ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators( "e", [
%~                                            comprehension_target_iter_ifs( tuple_elts(["i","e"]),
%~                                              call_func_args("enumerate",["tup"]),
%~                                              [ compare_ops_left_comparators( eq_token(==),
%~                                                  call_func_args(qualified_identifier_identifiers(["tup",boxed_attribute_value("index")]),["e"]),
%~                                                  "i")])])]))])))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( tuple_elts(["i","e"]),
%~                       call_func_args("enumerate",[TUP_01]),
%~                       [ compare_ops_left_comparators( eq_token(==),
%~                           call_func_args(
%~                              qualified_identifier_identifiers([TUP_01,boxed_attribute_value("index")]),
%~                              ["e"]),
%~                           "i")]))
%~
%~ replacing_with_var(tuple_elts(["i","e"]))
%~
%~ replacing_with_var(call_func_args("enumerate",[TUP_01]))
%~
%~ replacing_with_var( compare_ops_left_comparators( eq_token(==),
%~                       call_func_args(
%~                          qualified_identifier_identifiers([TUP_01,boxed_attribute_value("index")]),
%~                          ["e"]),
%~                       "i"))
%~
%~ replacing_with_var( call_func_args(
%~                        qualified_identifier_identifiers([TUP_01,boxed_attribute_value("index")]),
%~                        ["e"]))
%~
%~ replacing_with_var(boxed_attribute_value("index"))
%~
% Compiled KL-1 for dedupe
dedupe(TUP_01,TUPLE_02) :-
  willBeType(TUPLE_02,'Tuple') ,
  comment(' remove duplicates ') ,
  into_tuple(I,E,ARG_05) ,
  enumerate(TUP_01,ARG_06) ,
  call( [ op_call( [ boxed_attribute_value("index",ARG_09),
                     qualified_identifier_identifiers([TUP_01,ARG_09],E,ARG_08)])]) ,
  compare_ops_left_comparators(eq_token(==),ARG_08,I,ARG_07) ,
  comprehension_target_iter_ifs(ARG_05,ARG_06,[ARG_07],ARG_04) ,
  generator_exp_elt_generators(E,[ARG_04],ARG_03) ,
  tuple(ARG_03,TUPLE_02) ,
  exit_proc(TUPLE_02).
%~ % Universal AST Pass #0
%~ def( "order",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("container"),
%~           argument_name("compfunc")],
%~         [argument_type("Tuple")]),
%~      block_statements( [ expr_value(string_value(' order container by custom key ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          call_func_args_keywords("sorted",["container"],[keyword_value("compfunc")])]))])))
%~
% Compiled KL-1 for order
order(CONTAINER_01,COMPFUNC_02,TUPLE_03) :-
  willBeType(TUPLE_03,'Tuple') ,
  comment(' order container by custom key ') ,
  call_func_args_keywords( "sorted",
    [CONTAINER_01],
    [keyword_value(COMPFUNC_02)],
    ARG_04) ,
  tuple(ARG_04,TUPLE_03) ,
  exit_proc(TUPLE_03).
%~ % Universal AST Pass #0
%~ def( "repeat",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("item"),
%~           argument_name("num")],
%~         [argument_type("Tuple")]),
%~      block_statements( [ expr_value(string_value(' repetition of item within vector ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators("item",[comprehension_target_iter("i",call_func_args("range",["num"]))])]))])))
%~
%~ replacing_with_var(comprehension_target_iter("i",call_func_args("range",[NUM_02])))
%~
%~ replacing_with_var(call_func_args("range",[NUM_02]))
%~
% Compiled KL-1 for repeat
repeat(ITEM_01,NUM_02,TUPLE_03) :-
  willBeType(TUPLE_03,'Tuple') ,
  comment(' repetition of item within vector ') ,
  range(NUM_02,ARG_06) ,
  comprehension_target_iter("i",ARG_06,ARG_05) ,
  generator_exp_elt_generators(ITEM_01,[ARG_05],ARG_04) ,
  tuple(ARG_04,TUPLE_03) ,
  exit_proc(TUPLE_03).
%~ % Universal AST Pass #0
%~ def( "greater",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' greater ')),
%~                          return_value(compare_ops_left_comparators(gt_token(>),"a","b"))])))
%~
% Compiled KL-1 for greater
greater(A_01,B_02,BOOLEAN_03) :-
  willBeType(BOOLEAN_03,'Boolean') ,
  comment(' greater ') ,
  compare_ops_left_comparators(gt_token(>),A_01,B_02,BOOLEAN_03) ,
  exit_proc(BOOLEAN_03).
%~ % Universal AST Pass #0
%~ def( "size",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("container")],[argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' cardinality ')),
%~                          return_value(call_func_args("len",["container"]))])))
%~
% Compiled KL-1 for size
size(CONTAINER_01,INTEGER_02) :-
  willBeType(INTEGER_02,'Integer') ,
  comment(' cardinality ') ,
  len(CONTAINER_01,INTEGER_02) ,
  exit_proc(INTEGER_02).
%~ % Universal AST Pass #0
%~ def( "merge",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("containers")],[argument_type("Container")]),
%~      block_statements( [ expr_value(string_value(' merging ')),
%~                          return_value( call_func_args(
%~                                           call_func_args("type",["containers"]),
%~                                           [ generator_exp_elt_generators( "e", [
%~                                               comprehension_target_iter("c","containers"),
%~                                               comprehension_target_iter("e","c")])]))])))
%~
%~ replacing_with_var(comprehension_target_iter("e","c"))
%~
% Compiled KL-1 for merge
merge(CONTAINERS_01,CONTAINER_02) :-
  willBeType(CONTAINER_02,'Container') ,
  comment(' merging ') ,
  comprehension_target_iter("e","c",ARG_04) ,
  generator_exp_elt_generators( "e",
    [ assign_targets_value1("c",CONTAINERS_01),
      ARG_04],
    ARG_03) ,
  call( [ op_call(call_func_args("type",[CONTAINERS_01],ARG_03,CONTAINER_02))]) ,
  exit_proc(CONTAINER_02).
%~ % Universal AST Pass #0
%~ def( "maximum",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("container")],[argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' maximum ')),
%~                          return_value(call_func_args_keywords("max",["container"],[keyword_value(0)]))])))
%~
%~ replacing_with_var(keyword_value(0))
%~
% Compiled KL-1 for maximum
maximum(CONTAINER_01,INTEGER_02) :-
  willBeType(INTEGER_02,'Integer') ,
  comment(' maximum ') ,
  keyword_value(0,ARG_03) ,
  call_func_args_keywords("max",[CONTAINER_01],[ARG_03],INTEGER_02) ,
  exit_proc(INTEGER_02).
%~ % Universal AST Pass #0
%~ def( "minimum",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("container")],[argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' minimum ')),
%~                          return_value(call_func_args_keywords("min",["container"],[keyword_value(0)]))])))
%~
%~ replacing_with_var(keyword_value(0))
%~
% Compiled KL-1 for minimum
minimum(CONTAINER_01,INTEGER_02) :-
  willBeType(INTEGER_02,'Integer') ,
  comment(' minimum ') ,
  keyword_value(0,ARG_03) ,
  call_func_args_keywords("min",[CONTAINER_01],[ARG_03],INTEGER_02) ,
  exit_proc(INTEGER_02).
%~ % Universal AST Pass #0
%~ def( "valmax",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("container"),
%~           argument_name("compfunc")],
%~         [argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' maximum by custom function ')),
%~                          return_value( call_func_args( "compfunc", [
%~                                          call_func_args_keywords("max",["container"],[keyword_value("compfunc"),keyword_value(0)])]))])))
%~
%~ replacing_with_var(keyword_value(0))
%~
% Compiled KL-1 for valmax
valmax(CONTAINER_01,COMPFUNC_02,INTEGER_03) :-
  willBeType(INTEGER_03,'Integer') ,
  comment(' maximum by custom function ') ,
  keyword_value(0,ARG_05) ,
  call_func_args_keywords( "max",
    [CONTAINER_01],
    [ keyword_value(COMPFUNC_02),
      ARG_05],
    ARG_04) ,
  call([op_call(op_call(COMPFUNC_02,ARG_04,INTEGER_03))]) ,
  exit_proc(INTEGER_03).
%~ % Universal AST Pass #0
%~ def( "valmin",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("container"),
%~           argument_name("compfunc")],
%~         [argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' minimum by custom function ')),
%~                          return_value( call_func_args( "compfunc", [
%~                                          call_func_args_keywords("min",["container"],[keyword_value("compfunc"),keyword_value(0)])]))])))
%~
%~ replacing_with_var(keyword_value(0))
%~
% Compiled KL-1 for valmin
valmin(CONTAINER_01,COMPFUNC_02,INTEGER_03) :-
  willBeType(INTEGER_03,'Integer') ,
  comment(' minimum by custom function ') ,
  keyword_value(0,ARG_05) ,
  call_func_args_keywords( "min",
    [CONTAINER_01],
    [ keyword_value(COMPFUNC_02),
      ARG_05],
    ARG_04) ,
  call([op_call(op_call(COMPFUNC_02,ARG_04,INTEGER_03))]) ,
  exit_proc(INTEGER_03).
%~ % Universal AST Pass #0
%~ def( "argmax",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("container"),
%~           argument_name("compfunc")],
%~         [argument_type("Any")]),
%~      block_statements( [ expr_value(string_value(' largest item by custom order ')),
%~                          return_value(call_func_args_keywords("max",["container"],[keyword_value("compfunc")]))])))
%~
% Compiled KL-1 for argmax
argmax(CONTAINER_01,COMPFUNC_02,ANY_03) :-
  willBeType(ANY_03,'Any') ,
  comment(' largest item by custom order ') ,
  call_func_args_keywords( "max",
    [CONTAINER_01],
    [keyword_value(COMPFUNC_02)],
    ANY_03) ,
  exit_proc(ANY_03).
%~ % Universal AST Pass #0
%~ def( "argmin",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("container"),
%~           argument_name("compfunc")],
%~         [argument_type("Any")]),
%~      block_statements( [ expr_value(string_value(' smallest item by custom order ')),
%~                          return_value(call_func_args_keywords("min",["container"],[keyword_value("compfunc")]))])))
%~
% Compiled KL-1 for argmin
argmin(CONTAINER_01,COMPFUNC_02,ANY_03) :-
  willBeType(ANY_03,'Any') ,
  comment(' smallest item by custom order ') ,
  call_func_args_keywords( "min",
    [CONTAINER_01],
    [keyword_value(COMPFUNC_02)],
    ANY_03) ,
  exit_proc(ANY_03).
%~ % Universal AST Pass #0
%~ def( "mostcommon",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("container")],[argument_type("Any")]),
%~      block_statements( [ expr_value(string_value(' most common item ')),
%~                          return_value( call_func_args_keywords( "max",
%~                                          [ call_func_args("set",["container"])],
%~                                          [ keyword_value(qualified_identifier_identifiers(["container",boxed_attribute_value("count")]))]))])))
%~
%~ replacing_with_var(call_func_args("set",[CONTAINER_01]))
%~
%~ replacing_with_var( keyword_value(qualified_identifier_identifiers([CONTAINER_01,boxed_attribute_value("count")])))
%~
%~ replacing_with_var( qualified_identifier_identifiers([CONTAINER_01,boxed_attribute_value("count")]))
%~
%~ replacing_with_var(boxed_attribute_value("count"))
%~
% Compiled KL-1 for mostcommon
mostcommon(CONTAINER_01,ANY_02) :-
  willBeType(ANY_02,'Any') ,
  comment(' most common item ') ,
  set(CONTAINER_01,ARG_03) ,
  boxed_attribute_value("count",ARG_06) ,
  qualified_identifier_identifiers([CONTAINER_01,ARG_06],ARG_05) ,
  keyword_value(ARG_05,ARG_04) ,
  call_func_args_keywords("max",[ARG_03],[ARG_04],ANY_02) ,
  exit_proc(ANY_02).
%~ % Universal AST Pass #0
%~ def( "leastcommon",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("container")],[argument_type("Any")]),
%~      block_statements( [ expr_value(string_value(' least common item ')),
%~                          return_value( call_func_args_keywords( "min",
%~                                          [ call_func_args("set",["container"])],
%~                                          [ keyword_value(qualified_identifier_identifiers(["container",boxed_attribute_value("count")]))]))])))
%~
%~ replacing_with_var(call_func_args("set",[CONTAINER_01]))
%~
%~ replacing_with_var( keyword_value(qualified_identifier_identifiers([CONTAINER_01,boxed_attribute_value("count")])))
%~
%~ replacing_with_var( qualified_identifier_identifiers([CONTAINER_01,boxed_attribute_value("count")]))
%~
%~ replacing_with_var(boxed_attribute_value("count"))
%~
% Compiled KL-1 for leastcommon
leastcommon(CONTAINER_01,ANY_02) :-
  willBeType(ANY_02,'Any') ,
  comment(' least common item ') ,
  set(CONTAINER_01,ARG_03) ,
  boxed_attribute_value("count",ARG_06) ,
  qualified_identifier_identifiers([CONTAINER_01,ARG_06],ARG_05) ,
  keyword_value(ARG_05,ARG_04) ,
  call_func_args_keywords("min",[ARG_03],[ARG_04],ANY_02) ,
  exit_proc(ANY_02).
%~ % Universal AST Pass #0
%~ def( "initset",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("value")],[argument_type("FrozenSet")]),
%~      block_statements( [ expr_value(string_value(' initialize container ')),
%~                          return_value(call_func_args("frozenset",[set_elts(["value"])]))])))
%~
% Compiled KL-1 for initset
initset(VALUE_01,FROZENSET_02) :-
  willBeType(FROZENSET_02,'FrozenSet') ,
  comment(' initialize container ') ,
  set_elts([VALUE_01],ARG_03) ,
  frozenset(ARG_03,FROZENSET_02) ,
  exit_proc(FROZENSET_02).
%~ % Universal AST Pass #0
%~ def( "both",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' logical and ')),
%~                          return_value(bool_op_values(['python:And'],["a","b"]))])))
%~
% Compiled KL-1 for both
both(A_01,B_02,BOOLEAN_03) :-
  willBeType(BOOLEAN_03,'Boolean') ,
  comment(' logical and ') ,
  bool_op_values(['python:And'],[A_01,B_02],BOOLEAN_03) ,
  exit_proc(BOOLEAN_03).
%~ % Universal AST Pass #0
%~ def( "either",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' logical or ')),
%~                          return_value(bool_op_values(['python:Or'],["a","b"]))])))
%~
% Compiled KL-1 for either
either(A_01,B_02,BOOLEAN_03) :-
  willBeType(BOOLEAN_03,'Boolean') ,
  comment(' logical or ') ,
  bool_op_values(['python:Or'],[A_01,B_02],BOOLEAN_03) ,
  exit_proc(BOOLEAN_03).
%~ % Universal AST Pass #0
%~ def( "increment",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("x")],[argument_type("Numerical")]),
%~      block_statements( [ expr_value(string_value(' incrementing ')),
%~                          return_value( if_exp_test_body_orelse(
%~                                           call_func_args("isinstance",["x","int"]),
%~                                           bin_op_left_right(add_token(+),"x",1),
%~                                           tuple_elts( [ bin_op_left_right(add_token(+),subscript_value_slice("x",0),1),
%~                                                         bin_op_left_right(add_token(+),subscript_value_slice("x",1),1)])))])))
%~
%~ replacing_with_var(call_func_args("isinstance",[X_01,"int"]))
%~
% Compiled KL-1 for increment
increment(X_01,NUMERICAL_02) :-
  willBeType(NUMERICAL_02,'Numerical') ,
  comment(' incrementing ') ,
  (/*2*/
    [ isinstance(X_01,INT,ARG_03),
      testif(ARG_03)] ->
      [call([op_call(add_token(+,X_01,1,NUMERICAL_02))])] ;
    [ subscript_value_slice(X_01,0,ARG_06),
      call([op_call(add_token(+,ARG_06,1,ARG_04))]),
      subscript_value_slice(X_01,1,ARG_07),
      call([op_call(add_token(+,ARG_07,1,ARG_05))]),
      tuple_elts(ARG_04,ARG_05,NUMERICAL_02)]) ,
  exit_proc(NUMERICAL_02).
%~ % Universal AST Pass #0
%~ def( "decrement",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("x")],[argument_type("Numerical")]),
%~      block_statements( [ expr_value(string_value(' decrementing ')),
%~                          return_value( if_exp_test_body_orelse(
%~                                           call_func_args("isinstance",["x","int"]),
%~                                           bin_op_left_right(sub_token(-),"x",1),
%~                                           tuple_elts( [ bin_op_left_right(sub_token(-),subscript_value_slice("x",0),1),
%~                                                         bin_op_left_right(sub_token(-),subscript_value_slice("x",1),1)])))])))
%~
%~ replacing_with_var(call_func_args("isinstance",[X_01,"int"]))
%~
% Compiled KL-1 for decrement
decrement(X_01,NUMERICAL_02) :-
  willBeType(NUMERICAL_02,'Numerical') ,
  comment(' decrementing ') ,
  (/*2*/
    [ isinstance(X_01,INT,ARG_03),
      testif(ARG_03)] ->
      [call([op_call(sub_token(-,X_01,1,NUMERICAL_02))])] ;
    [ subscript_value_slice(X_01,0,ARG_06),
      call([op_call(sub_token(-,ARG_06,1,ARG_04))]),
      subscript_value_slice(X_01,1,ARG_07),
      call([op_call(sub_token(-,ARG_07,1,ARG_05))]),
      tuple_elts(ARG_04,ARG_05,NUMERICAL_02)]) ,
  exit_proc(NUMERICAL_02).
%~ % Universal AST Pass #0
%~ def( "crement",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("x")],[argument_type("Numerical")]),
%~      block_statements( [ expr_value(string_value(' incrementing positive and decrementing negative ')),
%~                          if_test_body(
%~                             call_func_args("isinstance",["x","int"]),
%~                             body_stmts( [ return_value( if_exp_test_body_orelse(
%~                                                            compare_ops_left_comparators(eq_token(==),"x",0),
%~                                                            0,
%~                                                            if_exp_test_body_orelse(
%~                                                               compare_ops_left_comparators(gt_token(>),"x",0),
%~                                                               bin_op_left_right(add_token(+),"x",1),
%~                                                               bin_op_left_right(sub_token(-),"x",1))))])),
%~                          return_value( tuple_elts( [ if_exp_test_body_orelse(
%~                                                         compare_ops_left_comparators(eq_token(==),subscript_value_slice("x",0),0),
%~                                                         0,
%~                                                         if_exp_test_body_orelse(
%~                                                            compare_ops_left_comparators(gt_token(>),subscript_value_slice("x",0),0),
%~                                                            bin_op_left_right(add_token(+),subscript_value_slice("x",0),1),
%~                                                            bin_op_left_right(sub_token(-),subscript_value_slice("x",0),1))),
%~                                                      if_exp_test_body_orelse(
%~                                                         compare_ops_left_comparators(eq_token(==),subscript_value_slice("x",1),0),
%~                                                         0,
%~                                                         if_exp_test_body_orelse(
%~                                                            compare_ops_left_comparators(gt_token(>),subscript_value_slice("x",1),0),
%~                                                            bin_op_left_right(add_token(+),subscript_value_slice("x",1),1),
%~                                                            bin_op_left_right(sub_token(-),subscript_value_slice("x",1),1)))]))])))
%~
%~ replacing_with_var( body_stmts( [ return_value( if_exp_test_body_orelse(
%~                                                    compare_ops_left_comparators(eq_token(==),X_01,0),
%~                                                    0,
%~                                                    if_exp_test_body_orelse(
%~                                                       compare_ops_left_comparators(gt_token(>),X_01,0),
%~                                                       bin_op_left_right(add_token(+),X_01,1),
%~                                                       bin_op_left_right(sub_token(-),X_01,1))))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[X_01,"int"]))
%~
%~ replacing_with_var( return_value( if_exp_test_body_orelse(
%~                                      compare_ops_left_comparators(eq_token(==),X_01,0),
%~                                      0,
%~                                      if_exp_test_body_orelse(
%~                                         compare_ops_left_comparators(gt_token(>),X_01,0),
%~                                         bin_op_left_right(add_token(+),X_01,1),
%~                                         bin_op_left_right(sub_token(-),X_01,1)))))
%~
%~ replacing_with_var( if_exp_test_body_orelse(
%~                        compare_ops_left_comparators(eq_token(==),X_01,0),
%~                        0,
%~                        if_exp_test_body_orelse(
%~                           compare_ops_left_comparators(gt_token(>),X_01,0),
%~                           bin_op_left_right(add_token(+),X_01,1),
%~                           bin_op_left_right(sub_token(-),X_01,1))))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),subscript_value_slice(X_01,0),0))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),subscript_value_slice(X_01,1),0))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),X_01,0))
%~
%~ replacing_with_var(subscript_value_slice(X_01,0))
%~
%~ replacing_with_var(compare_ops_left_comparators(gt_token(>),subscript_value_slice(X_01,0),0))
%~
%~ replacing_with_var(subscript_value_slice(X_01,1))
%~
%~ replacing_with_var(compare_ops_left_comparators(gt_token(>),subscript_value_slice(X_01,1),0))
%~
%~ replacing_with_var(compare_ops_left_comparators(gt_token(>),X_01,0))
%~
%~ replacing_with_var(subscript_value_slice(X_01,0))
%~
%~ replacing_with_var(subscript_value_slice(X_01,1))
%~
% Compiled KL-1 for crement
crement(X_01,NUMERICAL_02) :-
  willBeType(NUMERICAL_02,'Numerical') ,
  comment(' incrementing positive and decrementing negative ') ,
  (/*2*/
    [ (/*2*/
        [ compare_ops_left_comparators(eq_token(==),X_01,0,ARG_011),
          testif(ARG_011)] ->
          call(ARG_08=0) ;
        [ (/*2*/
            [ compare_ops_left_comparators(gt_token(>),X_01,0,ARG_020),
              testif(ARG_020)] ->
              [call([op_call(add_token(+,X_01,1,ARG_08))])] ;
            [call([op_call(sub_token(-,X_01,1,ARG_08))])])]),
      return_value(ARG_08,ARG_07),
      body_stmts([ARG_07],ARG_03),
      [ isinstance(X_01,INT,ARG_04),
        testif(ARG_04)] ->
        ARG_03]) ,
  (/*2*/
    [ subscript_value_slice(X_01,0,ARG_012),
      compare_ops_left_comparators(eq_token(==),ARG_012,0,ARG_09),
      testif(ARG_09)] ->
      call(ARG_05=0) ;
    [ (/*2*/
        [ subscript_value_slice(X_01,0,ARG_021),
          compare_ops_left_comparators(gt_token(>),ARG_021,0,ARG_013),
          testif(ARG_013)] ->
          [ subscript_value_slice(X_01,0,ARG_014),
            call([op_call(add_token(+,ARG_014,1,ARG_05))])] ;
        [ subscript_value_slice(X_01,0,ARG_015),
          call([op_call(sub_token(-,ARG_015,1,ARG_05))])])]) ,
  (/*2*/
    [ subscript_value_slice(X_01,1,ARG_016),
      compare_ops_left_comparators(eq_token(==),ARG_016,0,ARG_010),
      testif(ARG_010)] ->
      call(ARG_06=0) ;
    [ (/*2*/
        [ subscript_value_slice(X_01,1,ARG_022),
          compare_ops_left_comparators(gt_token(>),ARG_022,0,ARG_017),
          testif(ARG_017)] ->
          [ subscript_value_slice(X_01,1,ARG_018),
            call([op_call(add_token(+,ARG_018,1,ARG_06))])] ;
        [ subscript_value_slice(X_01,1,ARG_019),
          call([op_call(sub_token(-,ARG_019,1,ARG_06))])])]) ,
  tuple_elts(ARG_05,ARG_06,NUMERICAL_02) ,
  exit_proc(NUMERICAL_02).
%~ % Universal AST Pass #0
%~ def( "sign",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("x")],[argument_type("Numerical")]),
%~      block_statements( [ expr_value(string_value(' sign ')),
%~                          if_test_body(
%~                             call_func_args("isinstance",["x","int"]),
%~                             body_stmts( [ return_value( if_exp_test_body_orelse(
%~                                                            compare_ops_left_comparators(eq_token(==),"x",0),
%~                                                            0,
%~                                                            if_exp_test_body_orelse(compare_ops_left_comparators(gt_token(>),"x",0),1,unary_op_operand(us_ub_token(-),1))))])),
%~                          return_value( tuple_elts( [ if_exp_test_body_orelse(
%~                                                         compare_ops_left_comparators(eq_token(==),subscript_value_slice("x",0),0),
%~                                                         0,
%~                                                         if_exp_test_body_orelse(
%~                                                            compare_ops_left_comparators(gt_token(>),subscript_value_slice("x",0),0), 1,unary_op_operand(us_ub_token(-),1))),
%~                                                      if_exp_test_body_orelse(
%~                                                         compare_ops_left_comparators(eq_token(==),subscript_value_slice("x",1),0),
%~                                                         0,
%~                                                         if_exp_test_body_orelse(
%~                                                            compare_ops_left_comparators(gt_token(>),subscript_value_slice("x",1),0), 1,unary_op_operand(us_ub_token(-),1)))]))])))
%~
%~ replacing_with_var( body_stmts( [ return_value( if_exp_test_body_orelse(
%~                                                    compare_ops_left_comparators(eq_token(==),X_01,0),
%~                                                    0,
%~                                                    if_exp_test_body_orelse(
%~                                                       compare_ops_left_comparators(gt_token(>),X_01,0), 1,unary_op_operand(us_ub_token(-),1))))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[X_01,"int"]))
%~
%~ replacing_with_var( return_value( if_exp_test_body_orelse(
%~                                      compare_ops_left_comparators(eq_token(==),X_01,0),
%~                                      0,
%~                                      if_exp_test_body_orelse(
%~                                         compare_ops_left_comparators(gt_token(>),X_01,0), 1,unary_op_operand(us_ub_token(-),1)))))
%~
%~ replacing_with_var( if_exp_test_body_orelse(
%~                        compare_ops_left_comparators(eq_token(==),X_01,0),
%~                        0,
%~                        if_exp_test_body_orelse(
%~                           compare_ops_left_comparators(gt_token(>),X_01,0), 1,unary_op_operand(us_ub_token(-),1))))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),subscript_value_slice(X_01,0),0))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),subscript_value_slice(X_01,1),0))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),X_01,0))
%~
%~ replacing_with_var(subscript_value_slice(X_01,0))
%~
%~ replacing_with_var(compare_ops_left_comparators(gt_token(>),subscript_value_slice(X_01,0),0))
%~
%~ replacing_with_var(subscript_value_slice(X_01,1))
%~
%~ replacing_with_var(compare_ops_left_comparators(gt_token(>),subscript_value_slice(X_01,1),0))
%~
%~ replacing_with_var(compare_ops_left_comparators(gt_token(>),X_01,0))
%~
%~ replacing_with_var(subscript_value_slice(X_01,0))
%~
%~ replacing_with_var(subscript_value_slice(X_01,1))
%~
% Compiled KL-1 for sign
sign(X_01,NUMERICAL_02) :-
  willBeType(NUMERICAL_02,'Numerical') ,
  comment(' sign ') ,
  (/*2*/
    [ (/*2*/
        [ compare_ops_left_comparators(eq_token(==),X_01,0,ARG_011),
          testif(ARG_011)] ->
          call(ARG_08=0) ;
        [ (/*2*/
            [ compare_ops_left_comparators(gt_token(>),X_01,0,ARG_016),
              testif(ARG_016)] ->
              call(ARG_08=1) ;
            [ unary_op_operand(us_ub_token(-),1,ARG_08)])]),
      return_value(ARG_08,ARG_07),
      body_stmts([ARG_07],ARG_03),
      [ isinstance(X_01,INT,ARG_04),
        testif(ARG_04)] ->
        ARG_03]) ,
  (/*2*/
    [ subscript_value_slice(X_01,0,ARG_012),
      compare_ops_left_comparators(eq_token(==),ARG_012,0,ARG_09),
      testif(ARG_09)] ->
      call(ARG_05=0) ;
    [ (/*2*/
        [ subscript_value_slice(X_01,0,ARG_017),
          compare_ops_left_comparators(gt_token(>),ARG_017,0,ARG_013),
          testif(ARG_013)] ->
          call(ARG_05=1) ;
        [ unary_op_operand(us_ub_token(-),1,ARG_05)])]) ,
  (/*2*/
    [ subscript_value_slice(X_01,1,ARG_014),
      compare_ops_left_comparators(eq_token(==),ARG_014,0,ARG_010),
      testif(ARG_010)] ->
      call(ARG_06=0) ;
    [ (/*2*/
        [ subscript_value_slice(X_01,1,ARG_018),
          compare_ops_left_comparators(gt_token(>),ARG_018,0,ARG_015),
          testif(ARG_015)] ->
          call(ARG_06=1) ;
        [ unary_op_operand(us_ub_token(-),1,ARG_06)])]) ,
  tuple_elts(ARG_05,ARG_06,NUMERICAL_02) ,
  exit_proc(NUMERICAL_02).
%~ % Universal AST Pass #0
%~ def( "positive",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("x")],[argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' positive ')),
%~                          return_value(compare_ops_left_comparators(gt_token(>),"x",0))])))
%~
% Compiled KL-1 for positive
positive(X_01,BOOLEAN_02) :-
  willBeType(BOOLEAN_02,'Boolean') ,
  comment(' positive ') ,
  compare_ops_left_comparators(gt_token(>),X_01,0,BOOLEAN_02) ,
  exit_proc(BOOLEAN_02).
%~ % Universal AST Pass #0
%~ def( "toivec",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("i")],[argument_type("IntegerTuple")]),
%~      block_statements( [ expr_value(string_value(' vector pointing vertically ')),
%~                          return_value(tuple_elts(["i",0]))])))
%~
% Compiled KL-1 for toivec
toivec(I_01,INTEGERTUPLE_02) :-
  willBeType(INTEGERTUPLE_02,'IntegerTuple') ,
  comment(' vector pointing vertically ') ,
  into_tuple(I_01,0,INTEGERTUPLE_02) ,
  exit_proc(INTEGERTUPLE_02).
%~ % Universal AST Pass #0
%~ def( "tojvec",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("j")],[argument_type("IntegerTuple")]),
%~      block_statements( [ expr_value(string_value(' vector pointing horizontally ')),
%~                          return_value(tuple_elts([0,"j"]))])))
%~
% Compiled KL-1 for tojvec
tojvec(J_01,INTEGERTUPLE_02) :-
  willBeType(INTEGERTUPLE_02,'IntegerTuple') ,
  comment(' vector pointing horizontally ') ,
  into_tuple(0,J_01,INTEGERTUPLE_02) ,
  exit_proc(INTEGERTUPLE_02).
%~ % Universal AST Pass #0
%~ def( "sfilter",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("container"),
%~           argument_name("condition")],
%~         [argument_type("Container")]),
%~      block_statements( [ expr_value(string_value(' keep elements in container that satisfy condition ')),
%~                          return_value( call_func_args(
%~                                           call_func_args("type",["container"]),
%~                                           [ generator_exp_elt_generators( "e", [
%~                                               comprehension_target_iter_ifs("e","container",[call_func_args("condition",["e"])])])]))])))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( "e",
%~                       CONTAINER_01,
%~                       [ call_func_args(CONDITION_02,["e"])]))
%~
%~ replacing_with_var(call_func_args(CONDITION_02,["e"]))
%~
% Compiled KL-1 for sfilter
sfilter(CONTAINER_01,CONDITION_02,CONTAINER_03) :-
  willBeType(CONTAINER_03,'Container') ,
  comment(' keep elements in container that satisfy condition ') ,
  call([op_call(op_call(CONDITION_02,E,ARG_06))]) ,
  comprehension_target_iter_ifs(E,CONTAINER_01,[ARG_06],ARG_05) ,
  generator_exp_elt_generators(E,[ARG_05],ARG_04) ,
  call( [ op_call(call_func_args("type",[CONTAINER_01],ARG_04,CONTAINER_03))]) ,
  exit_proc(CONTAINER_03).
%~ % Universal AST Pass #0
%~ def( "mfilter",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("container"),
%~           argument_name("function")],
%~         [argument_type("FrozenSet")]),
%~      block_statements( [ expr_value(string_value(' filter and merge ')),
%~                          return_value(call_func_args("merge",[call_func_args("sfilter",["container","function"])]))])))
%~
% Compiled KL-1 for mfilter
mfilter(CONTAINER_01,FUNCTION_02,FROZENSET_03) :-
  willBeType(FROZENSET_03,'FrozenSet') ,
  comment(' filter and merge ') ,
  sfilter(CONTAINER_01,FUNCTION_02,ARG_04) ,
  merge(ARG_04,FROZENSET_03) ,
  exit_proc(FROZENSET_03).
%~ % Universal AST Pass #0
%~ def( "extract",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("container"),
%~           argument_name("condition")],
%~         [argument_type("Any")]),
%~      block_statements( [ expr_value(string_value(' first element of container that satisfies condition ')),
%~                          return_value( call_func_args( "next", [
%~                                          generator_exp_elt_generators( "e", [
%~                                            comprehension_target_iter_ifs("e","container",[call_func_args("condition",["e"])])])]))])))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( "e",
%~                       CONTAINER_01,
%~                       [ call_func_args(CONDITION_02,["e"])]))
%~
%~ replacing_with_var(call_func_args(CONDITION_02,["e"]))
%~
% Compiled KL-1 for extract
extract(CONTAINER_01,CONDITION_02,ANY_03) :-
  willBeType(ANY_03,'Any') ,
  comment(' first element of container that satisfies condition ') ,
  call([op_call(op_call(CONDITION_02,E,ARG_06))]) ,
  comprehension_target_iter_ifs(E,CONTAINER_01,[ARG_06],ARG_05) ,
  generator_exp_elt_generators(E,[ARG_05],ARG_04) ,
  next(ARG_04,ANY_03) ,
  exit_proc(ANY_03).
%~ % Universal AST Pass #0
%~ def( "totuple",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("container")],[argument_type("Tuple")]),
%~      block_statements( [ expr_value(string_value(' conversion to tuple ')),
%~                          return_value(call_func_args("tuple",["container"]))])))
%~
% Compiled KL-1 for totuple
totuple(CONTAINER_01,TUPLE_02) :-
  willBeType(TUPLE_02,'Tuple') ,
  comment(' conversion to tuple ') ,
  tuple(CONTAINER_01,TUPLE_02) ,
  exit_proc(TUPLE_02).
%~ % Universal AST Pass #0
%~ def( "first",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("container")],[argument_type("Any")]),
%~      block_statements( [ expr_value(string_value(' first item of container ')),
%~                          return_value(call_func_args("next",[call_func_args("iter",["container"])]))])))
%~
% Compiled KL-1 for first
first(CONTAINER_01,ANY_02) :-
  willBeType(ANY_02,'Any') ,
  comment(' first item of container ') ,
  iter(CONTAINER_01,ARG_03) ,
  next(ARG_03,ANY_02) ,
  exit_proc(ANY_02).
%~ % Universal AST Pass #0
%~ def( "last",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("container")],[argument_type("Any")]),
%~      block_statements( [ expr_value(string_value(' last item of container ')),
%~                          return_value( subscript_value_slice(call_func_args("max",[call_func_args("enumerate",["container"])]),1))])))
%~
% Compiled KL-1 for last
last(CONTAINER_01,ANY_02) :-
  willBeType(ANY_02,'Any') ,
  comment(' last item of container ') ,
  enumerate(CONTAINER_01,ARG_04) ,
  max(ARG_04,ARG_03) ,
  subscript_value_slice(ARG_03,1,ANY_02) ,
  exit_proc(ANY_02).
%~ % Universal AST Pass #0
%~ def( "insert",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("value"),
%~           argument_name("container")],
%~         [argument_type("FrozenSet")]),
%~      block_statements( [ expr_value(string_value(' insert item into container ')),
%~                          return_value( call_func_args(
%~                                           qualified_identifier_identifiers(["container",boxed_attribute_value("union")]),
%~                                           [ call_func_args("frozenset",[set_elts(["value"])])]))])))
%~
%~ replacing_with_var(boxed_attribute_value("union"))
%~
% Compiled KL-1 for insert
insert(VALUE_01,CONTAINER_02,FROZENSET_03) :-
  willBeType(FROZENSET_03,'FrozenSet') ,
  comment(' insert item into container ') ,
  set_elts([VALUE_01],ARG_05) ,
  frozenset(ARG_05,ARG_04) ,
  call( [ op_call( [ boxed_attribute_value("union",ARG_06),
                     qualified_identifier_identifiers( [CONTAINER_02,ARG_06],
                       ARG_04,
                       FROZENSET_03)])]) ,
  exit_proc(FROZENSET_03).
%~ % Universal AST Pass #0
%~ def( "remove",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("value"),
%~           argument_name("container")],
%~         [argument_type("Container")]),
%~      block_statements( [ expr_value(string_value(' remove item from container ')),
%~                          return_value( call_func_args(
%~                                           call_func_args("type",["container"]),
%~                                           [ generator_exp_elt_generators( "e", [
%~                                               comprehension_target_iter_ifs("e","container",[compare_ops_left_comparators(not_eq_token('!='),"e","value")])])]))])))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( "e",
%~                       CONTAINER_02,
%~                       [ compare_ops_left_comparators(not_eq_token('!='),"e",VALUE_01)]))
%~
%~ replacing_with_var(call_func_args('!=',["e",VALUE_01]))
%~
% Compiled KL-1 for remove
remove(VALUE_01,CONTAINER_02,CONTAINER_03) :-
  willBeType(CONTAINER_03,'Container') ,
  comment(' remove item from container ') ,
  call([op_call(op_call('!=',E,VALUE_01,ARG_06))]) ,
  comprehension_target_iter_ifs(E,CONTAINER_02,[ARG_06],ARG_05) ,
  generator_exp_elt_generators(E,[ARG_05],ARG_04) ,
  call( [ op_call(call_func_args("type",[CONTAINER_02],ARG_04,CONTAINER_03))]) ,
  exit_proc(CONTAINER_03).
%~ % Universal AST Pass #0
%~ def( "other",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("container"),
%~           argument_name("value")],
%~         [argument_type("Any")]),
%~      block_statements( [ expr_value(string_value(' other value in the container ')),
%~                          return_value(call_func_args("first",[call_func_args("remove",["value","container"])]))])))
%~
% Compiled KL-1 for other
other(CONTAINER_01,VALUE_02,ANY_03) :-
  willBeType(ANY_03,'Any') ,
  comment(' other value in the container ') ,
  remove(VALUE_02,CONTAINER_01,ARG_04) ,
  first(ARG_04,ANY_03) ,
  exit_proc(ANY_03).
%~ % Universal AST Pass #0
%~ def( "interval",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("start"), argument_name("stop"),argument_name("step")],
%~         [argument_type("Tuple")]),
%~      block_statements( [ expr_value(string_value(' range ')),
%~                          return_value(call_func_args("tuple",[call_func_args("range",["start","stop","step"])]))])))
%~
% Compiled KL-1 for interval
interval(START_01,STOP_02,STEP_03,TUPLE_04) :-
  willBeType(TUPLE_04,'Tuple') ,
  comment(' range ') ,
  range(START_01,STOP_02,STEP_03,ARG_05) ,
  tuple(ARG_05,TUPLE_04) ,
  exit_proc(TUPLE_04).
%~ % Universal AST Pass #0
%~ def( "astuple",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("IntegerTuple")]),
%~      block_statements( [ expr_value(string_value(' constructs a tuple ')),
%~                          return_value(tuple_elts(["a","b"]))])))
%~
% Compiled KL-1 for astuple
astuple(A_01,B_02,INTEGERTUPLE_03) :-
  willBeType(INTEGERTUPLE_03,'IntegerTuple') ,
  comment(' constructs a tuple ') ,
  into_tuple(A_01,B_02,INTEGERTUPLE_03) ,
  exit_proc(INTEGERTUPLE_03).
%~ % Universal AST Pass #0
%~ def( "product",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("FrozenSet")]),
%~      block_statements( [ expr_value(string_value(' cartesian product ')),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                            comprehension_target_iter("j","b"),
%~                                            comprehension_target_iter("i","a")])]))])))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
% Compiled KL-1 for product
product(A_01,B_02,FROZENSET_03) :-
  willBeType(FROZENSET_03,'FrozenSet') ,
  comment(' cartesian product ') ,
  into_tuple(I,J,ARG_05) ,
  generator_exp_elt_generators( ARG_05,
    [ assign_targets_value1(J,B_02),
      assign_targets_value1(I,A_01)],
    ARG_04) ,
  frozenset(ARG_04,FROZENSET_03) ,
  exit_proc(FROZENSET_03).
%~ % Universal AST Pass #0
%~ def( "pair",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("TupleTuple")]),
%~      block_statements( [ expr_value(string_value(' zipping of two tuples ')),
%~                          return_value(call_func_args("tuple",[call_func_args("zip",["a","b"])]))])))
%~
% Compiled KL-1 for pair
pair(A_01,B_02,TUPLETUPLE_03) :-
  willBeType(TUPLETUPLE_03,'TupleTuple') ,
  comment(' zipping of two tuples ') ,
  zip(A_01,B_02,ARG_04) ,
  tuple(ARG_04,TUPLETUPLE_03) ,
  exit_proc(TUPLETUPLE_03).
%~ % Universal AST Pass #0
%~ def( "branch",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("condition"), argument_name("a"),argument_name("b")],
%~         [argument_type("Any")]),
%~      block_statements( [ expr_value(string_value(' if else branching ')),
%~                          return_value(if_exp_test_body_orelse("condition","a","b"))])))
%~
% Compiled KL-1 for branch
branch(CONDITION_01,A_02,B_03,ANY_04) :-
  willBeType(ANY_04,'Any') ,
  comment(' if else branching ') ,
  (/*2*/
    testif(CONDITION_01)->call(ANY_04=A_02) ;
    call(ANY_04=B_03)) ,
  exit_proc(ANY_04).
%~ % Universal AST Pass #0
%~ def( "compose",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("outer"),
%~           argument_name("inner")],
%~         [argument_type("Callable")]),
%~      block_statements( [ expr_value(string_value(' function composition ')),
%~                          return_value( lambda_args_body( arguments_args([argument_name("x")]),
%~                                          body_stmts(call_func_args("outer",[call_func_args("inner",["x"])]))))])))
%~
%~ replacing_with_var(arguments_args([argument_name("x")]))
%~
%~ replacing_with_var( body_stmts(call_func_args(OUTER_01,[call_func_args(INNER_02,["x"])])))
%~
%~ replacing_with_var(argument_name("x"))
%~
%~ replacing_with_var(call_func_args(OUTER_01,[call_func_args(INNER_02,["x"])]))
%~
% Compiled KL-1 for compose
compose(OUTER_01,INNER_02,CALLABLE_03) :-
  willBeType(CALLABLE_03,'Callable') ,
  comment(' function composition ') ,
  argument_name(X,ARG_06) ,
  arguments_args([ARG_06],ARG_04) ,
  call([op_call(op_call(INNER_02,X,ARG_08))]) ,
  call([op_call(op_call(OUTER_01,ARG_08,ARG_07))]) ,
  body_stmts(ARG_07,ARG_05) ,
  lambda_args_body(ARG_04,ARG_05,CALLABLE_03) ,
  exit_proc(CALLABLE_03).
%~ % Universal AST Pass #0
%~ def( "chain",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("h"), argument_name("g"),argument_name("f")],
%~         [argument_type("Callable")]),
%~      block_statements( [ expr_value(string_value(' function composition with three functions ')),
%~                          return_value( lambda_args_body( arguments_args([argument_name("x")]),
%~                                          body_stmts(call_func_args("h",[call_func_args("g",[call_func_args("f",["x"])])]))))])))
%~
%~ replacing_with_var(arguments_args([argument_name("x")]))
%~
%~ replacing_with_var( body_stmts( call_func_args( H_01, [
%~                                   call_func_args(G_02,[call_func_args(F_03,["x"])])])))
%~
%~ replacing_with_var(argument_name("x"))
%~
%~ replacing_with_var( call_func_args( H_01, [
%~                       call_func_args(G_02,[call_func_args(F_03,["x"])])]))
%~
% Compiled KL-1 for chain
chain(H_01,G_02,F_03,CALLABLE_04) :-
  willBeType(CALLABLE_04,'Callable') ,
  comment(' function composition with three functions ') ,
  argument_name(X,ARG_07) ,
  arguments_args([ARG_07],ARG_05) ,
  call([op_call(op_call(F_03,X,ARG_010))]) ,
  call([op_call(op_call(G_02,ARG_010,ARG_09))]) ,
  call([op_call(op_call(H_01,ARG_09,ARG_08))]) ,
  body_stmts(ARG_08,ARG_06) ,
  lambda_args_body(ARG_05,ARG_06,CALLABLE_04) ,
  exit_proc(CALLABLE_04).
%~ % Universal AST Pass #0
%~ def( "matcher",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("function"),
%~           argument_name("target")],
%~         [argument_type("Callable")]),
%~      block_statements( [ expr_value(string_value(' construction of equality function ')),
%~                          return_value( lambda_args_body( arguments_args([argument_name("x")]),
%~                                          body_stmts(compare_ops_left_comparators(eq_token(==),call_func_args("function",["x"]),"target"))))])))
%~
%~ replacing_with_var(arguments_args([argument_name("x")]))
%~
%~ replacing_with_var( body_stmts( compare_ops_left_comparators(eq_token(==),call_func_args(FUNCTION_01,["x"]),TARGET_02)))
%~
%~ replacing_with_var(argument_name("x"))
%~
%~ replacing_with_var( call_func_args(==,[call_func_args(FUNCTION_01,["x"]),TARGET_02]))
%~
% Compiled KL-1 for matcher
matcher(FUNCTION_01,TARGET_02,CALLABLE_03) :-
  willBeType(CALLABLE_03,'Callable') ,
  comment(' construction of equality function ') ,
  argument_name(X,ARG_06) ,
  arguments_args([ARG_06],ARG_04) ,
  call([op_call(op_call(FUNCTION_01,X,ARG_08))]) ,
  call([op_call(op_call(==,ARG_08,TARGET_02,ARG_07))]) ,
  body_stmts(ARG_07,ARG_05) ,
  lambda_args_body(ARG_04,ARG_05,CALLABLE_03) ,
  exit_proc(CALLABLE_03).
%~ % Universal AST Pass #0
%~ def( "rbind",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("function"),
%~           argument_name("fixed")],
%~         [argument_type("Callable")]),
%~      block_statements( [ expr_value(string_value(' fix the rightmost argument ')),
%~                          assign_targets_value( ["n"],
%~                            qualified_identifier_identifiers( [ "function",
%~                                                                boxed_attribute_value("__code__"),
%~                                                                boxed_attribute_value("co_argcount")])),
%~                          if_test_body_orelse(
%~                             compare_ops_left_comparators(eq_token(==),"n",2),
%~                             body_stmts( [ return_value( lambda_args_body( arguments_args([argument_name("x")]),
%~                                                           body_stmts(call_func_args("function",["x","fixed"]))))]),
%~                             orelse_else_stmts( [ if_test_body_orelse(
%~                                                     compare_ops_left_comparators(eq_token(==),"n",3),
%~                                                     body_stmts( [ return_value( lambda_args_body(
%~                                                                                    arguments_args([argument_name("x"),argument_name("y")]),
%~                                                                                    body_stmts(call_func_args("function",["x","y","fixed"]))))]),
%~                                                     orelse_else_stmts( [ return_value( lambda_args_body(
%~                                                                                           arguments_args([argument_name("x"),argument_name("y"),argument_name("z")]),
%~                                                                                           body_stmts(call_func_args("function",["x","y","z","fixed"]))))]))]))])))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body_orelse(
%~                                             compare_ops_left_comparators(eq_token(==),"n",3),
%~                                             body_stmts( [ return_value( lambda_args_body(
%~                                                                            arguments_args([argument_name("x"),argument_name("y")]),
%~                                                                            body_stmts(call_func_args(FUNCTION_01,["x","y",FIXED_02]))))]),
%~                                             orelse_else_stmts( [ return_value( lambda_args_body(
%~                                                                                   arguments_args([argument_name("x"),argument_name("y"),argument_name("z")]),
%~                                                                                   body_stmts(call_func_args(FUNCTION_01,["x","y","z",FIXED_02]))))]))]))
%~
%~ replacing_with_var( body_stmts( [ return_value( lambda_args_body( arguments_args([argument_name("x")]),
%~                                                   body_stmts(call_func_args(FUNCTION_01,["x",FIXED_02]))))]))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),"n",2))
%~
%~ replacing_with_var(boxed_attribute_value("__code__"))
%~
%~ replacing_with_var(boxed_attribute_value("co_argcount"))
%~
%~ replacing_with_var( if_test_body_orelse(
%~                        compare_ops_left_comparators(eq_token(==),"n",3),
%~                        body_stmts( [ return_value( lambda_args_body(
%~                                                       arguments_args([argument_name("x"),argument_name("y")]),
%~                                                       body_stmts(call_func_args(FUNCTION_01,["x","y",FIXED_02]))))]),
%~                        orelse_else_stmts( [ return_value( lambda_args_body(
%~                                                              arguments_args([argument_name("x"),argument_name("y"),argument_name("z")]),
%~                                                              body_stmts(call_func_args(FUNCTION_01,["x","y","z",FIXED_02]))))])))
%~
%~ replacing_with_var( return_value( lambda_args_body( arguments_args([argument_name("x")]),
%~                                     body_stmts(call_func_args(FUNCTION_01,["x",FIXED_02])))))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),"n",3))
%~
%~ replacing_with_var( body_stmts( [ return_value( lambda_args_body(
%~                                                    arguments_args([argument_name("x"),argument_name("y")]),
%~                                                    body_stmts(call_func_args(FUNCTION_01,["x","y",FIXED_02]))))]))
%~
%~ replacing_with_var( orelse_else_stmts( [ return_value( lambda_args_body(
%~                                                           arguments_args([argument_name("x"),argument_name("y"),argument_name("z")]),
%~                                                           body_stmts(call_func_args(FUNCTION_01,["x","y","z",FIXED_02]))))]))
%~
%~ replacing_with_var( lambda_args_body( arguments_args([argument_name("x")]),
%~                       body_stmts(call_func_args(FUNCTION_01,["x",FIXED_02]))))
%~
%~ replacing_with_var( return_value( lambda_args_body(
%~                                      arguments_args([argument_name("x"),argument_name("y")]),
%~                                      body_stmts(call_func_args(FUNCTION_01,["x","y",FIXED_02])))))
%~
%~ replacing_with_var( return_value( lambda_args_body(
%~                                      arguments_args([argument_name("x"),argument_name("y"),argument_name("z")]),
%~                                      body_stmts(call_func_args(FUNCTION_01,["x","y","z",FIXED_02])))))
%~
%~ replacing_with_var(arguments_args([argument_name("x")]))
%~
%~ replacing_with_var(body_stmts(call_func_args(FUNCTION_01,["x",FIXED_02])))
%~
%~ replacing_with_var( lambda_args_body(
%~                        arguments_args([argument_name("x"),argument_name("y")]),
%~                        body_stmts(call_func_args(FUNCTION_01,["x","y",FIXED_02]))))
%~
%~ replacing_with_var( lambda_args_body(
%~                        arguments_args([argument_name("x"),argument_name("y"),argument_name("z")]),
%~                        body_stmts(call_func_args(FUNCTION_01,["x","y","z",FIXED_02]))))
%~
%~ replacing_with_var(argument_name("x"))
%~
%~ replacing_with_var(call_func_args(FUNCTION_01,["x",FIXED_02]))
%~
%~ replacing_with_var(arguments_args([argument_name("x"),argument_name("y")]))
%~
%~ replacing_with_var( body_stmts(call_func_args(FUNCTION_01,["x","y",FIXED_02])))
%~
%~ replacing_with_var(arguments_args([argument_name("x"),argument_name("y"),argument_name("z")]))
%~
%~ replacing_with_var( body_stmts(call_func_args(FUNCTION_01,["x","y","z",FIXED_02])))
%~
%~ replacing_with_var(argument_name("x"))
%~
%~ replacing_with_var(argument_name("y"))
%~
%~ replacing_with_var(call_func_args(FUNCTION_01,["x","y",FIXED_02]))
%~
%~ replacing_with_var(argument_name("x"))
%~
%~ replacing_with_var(argument_name("y"))
%~
%~ replacing_with_var(argument_name("z"))
%~
%~ replacing_with_var(call_func_args(FUNCTION_01,["x","y","z",FIXED_02]))
%~
% Compiled KL-1 for rbind
rbind(FUNCTION_01,FIXED_02,CALLABLE_03) :-
  willBeType(CALLABLE_03,'Callable') ,
  comment(' fix the rightmost argument ') ,
  boxed_attribute_value("__code__",ARG_07) ,
  boxed_attribute_value("co_argcount",ARG_08) ,
  qualified_identifier_identifiers( [FUNCTION_01,ARG_07,ARG_08],
    N) ,
  compare_ops_left_comparators(eq_token(==),N,3,ARG_011) ,
  argument_name(X,ARG_027) ,
  argument_name(Y,ARG_028) ,
  arguments_args([ARG_027,ARG_028],ARG_023) ,
  call([op_call(op_call(FUNCTION_01,X,Y,FIXED_02,ARG_029))]) ,
  body_stmts(ARG_029,ARG_024) ,
  lambda_args_body(ARG_023,ARG_024,ARG_019) ,
  return_value(ARG_019,ARG_015) ,
  body_stmts([ARG_015],ARG_012) ,
  argument_name(X,ARG_030) ,
  argument_name(Y,ARG_031) ,
  argument_name(Z,ARG_032) ,
  arguments_args([ARG_030,ARG_031,ARG_032],ARG_025) ,
  call( [ op_call(op_call(FUNCTION_01,X,Y,Z,FIXED_02,ARG_033))]) ,
  body_stmts(ARG_033,ARG_026) ,
  lambda_args_body(ARG_025,ARG_026,ARG_020) ,
  return_value(ARG_020,ARG_016) ,
  orelse_else_stmts([ARG_016],ARG_013) ,
  if_test_body_orelse(ARG_011,ARG_012,ARG_013,ARG_09) ,
  orelse_else_stmts([ARG_09],ARG_04) ,
  (/*2*/
    [ argument_name(X,ARG_021),
      arguments_args([ARG_021],ARG_017),
      call([op_call(op_call(FUNCTION_01,X,FIXED_02,ARG_022))]),
      body_stmts(ARG_022,ARG_018),
      lambda_args_body(ARG_017,ARG_018,ARG_014),
      return_value(ARG_014,ARG_010),
      body_stmts([ARG_010],ARG_05),
      [ compare_ops_left_comparators(eq_token(==),N,2,ARG_06),
        testif(ARG_06)] ->
        ARG_05] ;
    ARG_04).
%~ % Universal AST Pass #0
%~ def( "lbind",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("function"),
%~           argument_name("fixed")],
%~         [argument_type("Callable")]),
%~      block_statements( [ expr_value(string_value(' fix the leftmost argument ')),
%~                          assign_targets_value( ["n"],
%~                            qualified_identifier_identifiers( [ "function",
%~                                                                boxed_attribute_value("__code__"),
%~                                                                boxed_attribute_value("co_argcount")])),
%~                          if_test_body_orelse(
%~                             compare_ops_left_comparators(eq_token(==),"n",2),
%~                             body_stmts( [ return_value( lambda_args_body( arguments_args([argument_name("y")]),
%~                                                           body_stmts(call_func_args("function",["fixed","y"]))))]),
%~                             orelse_else_stmts( [ if_test_body_orelse(
%~                                                     compare_ops_left_comparators(eq_token(==),"n",3),
%~                                                     body_stmts( [ return_value( lambda_args_body(
%~                                                                                    arguments_args([argument_name("y"),argument_name("z")]),
%~                                                                                    body_stmts(call_func_args("function",["fixed","y","z"]))))]),
%~                                                     orelse_else_stmts( [ return_value( lambda_args_body(
%~                                                                                           arguments_args([argument_name("y"),argument_name("z"),argument_name("a")]),
%~                                                                                           body_stmts(call_func_args("function",["fixed","y","z","a"]))))]))]))])))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body_orelse(
%~                                             compare_ops_left_comparators(eq_token(==),"n",3),
%~                                             body_stmts( [ return_value( lambda_args_body(
%~                                                                            arguments_args([argument_name("y"),argument_name("z")]),
%~                                                                            body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y","z"]))))]),
%~                                             orelse_else_stmts( [ return_value( lambda_args_body(
%~                                                                                   arguments_args([argument_name("y"),argument_name("z"),argument_name("a")]),
%~                                                                                   body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y","z","a"]))))]))]))
%~
%~ replacing_with_var( body_stmts( [ return_value( lambda_args_body( arguments_args([argument_name("y")]),
%~                                                   body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y"]))))]))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),"n",2))
%~
%~ replacing_with_var(boxed_attribute_value("__code__"))
%~
%~ replacing_with_var(boxed_attribute_value("co_argcount"))
%~
%~ replacing_with_var( if_test_body_orelse(
%~                        compare_ops_left_comparators(eq_token(==),"n",3),
%~                        body_stmts( [ return_value( lambda_args_body(
%~                                                       arguments_args([argument_name("y"),argument_name("z")]),
%~                                                       body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y","z"]))))]),
%~                        orelse_else_stmts( [ return_value( lambda_args_body(
%~                                                              arguments_args([argument_name("y"),argument_name("z"),argument_name("a")]),
%~                                                              body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y","z","a"]))))])))
%~
%~ replacing_with_var( return_value( lambda_args_body( arguments_args([argument_name("y")]),
%~                                     body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y"])))))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),"n",3))
%~
%~ replacing_with_var( body_stmts( [ return_value( lambda_args_body(
%~                                                    arguments_args([argument_name("y"),argument_name("z")]),
%~                                                    body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y","z"]))))]))
%~
%~ replacing_with_var( orelse_else_stmts( [ return_value( lambda_args_body(
%~                                                           arguments_args([argument_name("y"),argument_name("z"),argument_name("a")]),
%~                                                           body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y","z","a"]))))]))
%~
%~ replacing_with_var( lambda_args_body( arguments_args([argument_name("y")]),
%~                       body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y"]))))
%~
%~ replacing_with_var( return_value( lambda_args_body(
%~                                      arguments_args([argument_name("y"),argument_name("z")]),
%~                                      body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y","z"])))))
%~
%~ replacing_with_var( return_value( lambda_args_body(
%~                                      arguments_args([argument_name("y"),argument_name("z"),argument_name("a")]),
%~                                      body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y","z","a"])))))
%~
%~ replacing_with_var(arguments_args([argument_name("y")]))
%~
%~ replacing_with_var(body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y"])))
%~
%~ replacing_with_var( lambda_args_body(
%~                        arguments_args([argument_name("y"),argument_name("z")]),
%~                        body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y","z"]))))
%~
%~ replacing_with_var( lambda_args_body(
%~                        arguments_args([argument_name("y"),argument_name("z"),argument_name("a")]),
%~                        body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y","z","a"]))))
%~
%~ replacing_with_var(argument_name("y"))
%~
%~ replacing_with_var(call_func_args(FUNCTION_01,[FIXED_02,"y"]))
%~
%~ replacing_with_var(arguments_args([argument_name("y"),argument_name("z")]))
%~
%~ replacing_with_var( body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y","z"])))
%~
%~ replacing_with_var(arguments_args([argument_name("y"),argument_name("z"),argument_name("a")]))
%~
%~ replacing_with_var( body_stmts(call_func_args(FUNCTION_01,[FIXED_02,"y","z","a"])))
%~
%~ replacing_with_var(argument_name("y"))
%~
%~ replacing_with_var(argument_name("z"))
%~
%~ replacing_with_var(call_func_args(FUNCTION_01,[FIXED_02,"y","z"]))
%~
%~ replacing_with_var(argument_name("y"))
%~
%~ replacing_with_var(argument_name("z"))
%~
%~ replacing_with_var(argument_name("a"))
%~
%~ replacing_with_var(call_func_args(FUNCTION_01,[FIXED_02,"y","z","a"]))
%~
% Compiled KL-1 for lbind
lbind(FUNCTION_01,FIXED_02,CALLABLE_03) :-
  willBeType(CALLABLE_03,'Callable') ,
  comment(' fix the leftmost argument ') ,
  boxed_attribute_value("__code__",ARG_07) ,
  boxed_attribute_value("co_argcount",ARG_08) ,
  qualified_identifier_identifiers( [FUNCTION_01,ARG_07,ARG_08],
    N) ,
  compare_ops_left_comparators(eq_token(==),N,3,ARG_011) ,
  argument_name(Y,ARG_027) ,
  argument_name(Z,ARG_028) ,
  arguments_args([ARG_027,ARG_028],ARG_023) ,
  call([op_call(op_call(FUNCTION_01,FIXED_02,Y,Z,ARG_029))]) ,
  body_stmts(ARG_029,ARG_024) ,
  lambda_args_body(ARG_023,ARG_024,ARG_019) ,
  return_value(ARG_019,ARG_015) ,
  body_stmts([ARG_015],ARG_012) ,
  argument_name(Y,ARG_030) ,
  argument_name(Z,ARG_031) ,
  argument_name(A,ARG_032) ,
  arguments_args([ARG_030,ARG_031,ARG_032],ARG_025) ,
  call( [ op_call(op_call(FUNCTION_01,FIXED_02,Y,Z,A,ARG_033))]) ,
  body_stmts(ARG_033,ARG_026) ,
  lambda_args_body(ARG_025,ARG_026,ARG_020) ,
  return_value(ARG_020,ARG_016) ,
  orelse_else_stmts([ARG_016],ARG_013) ,
  if_test_body_orelse(ARG_011,ARG_012,ARG_013,ARG_09) ,
  orelse_else_stmts([ARG_09],ARG_04) ,
  (/*2*/
    [ argument_name(Y,ARG_021),
      arguments_args([ARG_021],ARG_017),
      call([op_call(op_call(FUNCTION_01,FIXED_02,Y,ARG_022))]),
      body_stmts(ARG_022,ARG_018),
      lambda_args_body(ARG_017,ARG_018,ARG_014),
      return_value(ARG_014,ARG_010),
      body_stmts([ARG_010],ARG_05),
      [ compare_ops_left_comparators(eq_token(==),N,2,ARG_06),
        testif(ARG_06)] ->
        ARG_05] ;
    ARG_04).
%~ % Universal AST Pass #0
%~ def( "power",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("function"),
%~           argument_name("n")],
%~         [argument_type("Callable")]),
%~      block_statements( [ expr_value(string_value(' power of function ')),
%~                          if_test_body(compare_ops_left_comparators(eq_token(==),"n",1),body_stmts([return_value("function")])),
%~                          return_value( call_func_args( "compose", [
%~                                          "function",
%~                                          call_func_args("power",["function",bin_op_left_right(sub_token(-),"n",1)])]))])))
%~
%~ replacing_with_var(body_stmts([return_value(FUNCTION_01)]))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),N_02,1))
%~
% Compiled KL-1 for power
power(FUNCTION_01,N_02,CALLABLE_03) :-
  willBeType(CALLABLE_03,'Callable') ,
  comment(' power of function ') ,
  (/*2*/
    [ body_stmts(
         [ call(CALLABLE_03=FUNCTION_01),
           exit_proc(CALLABLE_03)],
         ARG_04),
      [ compare_ops_left_comparators(eq_token(==),N_02,1,ARG_05),
        testif(ARG_05)] ->
        ARG_04]) ,
  call([op_call(sub_token(-,N_02,1,ARG_07))]) ,
  power(FUNCTION_01,ARG_07,ARG_06) ,
  compose(FUNCTION_01,ARG_06,CALLABLE_03) ,
  exit_proc(CALLABLE_03).
%~ % Universal AST Pass #0
%~ def( "fork",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("outer"), argument_name("a"),argument_name("b")],
%~         [argument_type("Callable")]),
%~      block_statements( [ expr_value(string_value(' creates a wrapper function ')),
%~                          return_value( lambda_args_body( arguments_args([argument_name("x")]),
%~                                          body_stmts( call_func_args("outer",[call_func_args("a",["x"]),call_func_args("b",["x"])]))))])))
%~
%~ replacing_with_var(arguments_args([argument_name("x")]))
%~
%~ replacing_with_var( body_stmts( call_func_args( OUTER_01, [
%~                                   call_func_args(A_02,["x"]),
%~                                   call_func_args(B_03,["x"])])))
%~
%~ replacing_with_var(argument_name("x"))
%~
%~ replacing_with_var( call_func_args( OUTER_01, [
%~                       call_func_args(A_02,["x"]),
%~                       call_func_args(B_03,["x"])]))
%~
% Compiled KL-1 for fork
fork(OUTER_01,A_02,B_03,CALLABLE_04) :-
  willBeType(CALLABLE_04,'Callable') ,
  comment(' creates a wrapper function ') ,
  argument_name(X,ARG_07) ,
  arguments_args([ARG_07],ARG_05) ,
  call([op_call(op_call(A_02,X,ARG_09))]) ,
  call([op_call(op_call(B_03,X,ARG_010))]) ,
  call([op_call(op_call(OUTER_01,ARG_09,ARG_010,ARG_08))]) ,
  body_stmts(ARG_08,ARG_06) ,
  lambda_args_body(ARG_05,ARG_06,CALLABLE_04) ,
  exit_proc(CALLABLE_04).
%~ % Universal AST Pass #0
%~ def( "apply",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("function"),
%~           argument_name("container")],
%~         [argument_type("Container")]),
%~      block_statements( [ expr_value(string_value(' apply function to each item in container ')),
%~                          return_value( call_func_args(
%~                                           call_func_args("type",["container"]),
%~                                           [ generator_exp_elt_generators(
%~                                                call_func_args("function",["e"]),
%~                                                [comprehension_target_iter("e","container")])]))])))
%~
%~ replacing_with_var(call_func_args(FUNCTION_01,["e"]))
%~
% Compiled KL-1 for apply
apply(FUNCTION_01,CONTAINER_02,CONTAINER_03) :-
  willBeType(CONTAINER_03,'Container') ,
  comment(' apply function to each item in container ') ,
  call([op_call(op_call(FUNCTION_01,E,ARG_05))]) ,
  generator_exp_elt_generators( ARG_05,
    [assign_targets_value1(E,CONTAINER_02)],
    ARG_04) ,
  call( [ op_call(call_func_args("type",[CONTAINER_02],ARG_04,CONTAINER_03))]) ,
  exit_proc(CONTAINER_03).
%~ % Universal AST Pass #0
%~ def( "rapply",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("functions"),
%~           argument_name("value")],
%~         [argument_type("Container")]),
%~      block_statements( [ expr_value(string_value(' apply each function in container to value ')),
%~                          return_value( call_func_args(
%~                                           call_func_args("type",["functions"]),
%~                                           [ generator_exp_elt_generators(
%~                                                call_func_args("function",["value"]),
%~                                                [comprehension_target_iter("function","functions")])]))])))
%~
%~ replacing_with_var(call_func_args("function",[VALUE_02]))
%~
% Compiled KL-1 for rapply
rapply(FUNCTIONS_01,VALUE_02,CONTAINER_03) :-
  willBeType(CONTAINER_03,'Container') ,
  comment(' apply each function in container to value ') ,
  function(VALUE_02,ARG_05) ,
  generator_exp_elt_generators( ARG_05,
    [assign_targets_value1("function",FUNCTIONS_01)],
    ARG_04) ,
  call( [ op_call(call_func_args("type",[FUNCTIONS_01],ARG_04,CONTAINER_03))]) ,
  exit_proc(CONTAINER_03).
%~ % Universal AST Pass #0
%~ def( "mapply",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("function"),
%~           argument_name("container")],
%~         [argument_type("FrozenSet")]),
%~      block_statements( [ expr_value(string_value(' apply and merge ')),
%~                          return_value(call_func_args("merge",[call_func_args("apply",["function","container"])]))])))
%~
% Compiled KL-1 for mapply
mapply(FUNCTION_01,CONTAINER_02,FROZENSET_03) :-
  willBeType(FROZENSET_03,'FrozenSet') ,
  comment(' apply and merge ') ,
  apply(FUNCTION_01,CONTAINER_02,ARG_04) ,
  merge(ARG_04,FROZENSET_03) ,
  exit_proc(FROZENSET_03).
%~ % Universal AST Pass #0
%~ def( "papply",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("function"), argument_name("a"),argument_name("b")],
%~         [argument_type("Tuple")]),
%~      block_statements( [ expr_value(string_value(' apply function on two vectors ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators(
%~                                             call_func_args("function",["i","j"]),
%~                                             [ comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("zip",["a","b"]))])]))])))
%~
%~ replacing_with_var(call_func_args(FUNCTION_01,["i","j"]))
%~
%~ replacing_with_var( comprehension_target_iter( tuple_elts(["i","j"]),
%~                       call_func_args("zip",[A_02,B_03])))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var(call_func_args("zip",[A_02,B_03]))
%~
% Compiled KL-1 for papply
papply(FUNCTION_01,A_02,B_03,TUPLE_04) :-
  willBeType(TUPLE_04,'Tuple') ,
  comment(' apply function on two vectors ') ,
  call([op_call(op_call(FUNCTION_01,I,J,ARG_06))]) ,
  into_tuple(I,J,ARG_08) ,
  zip(A_02,B_03,ARG_09) ,
  comprehension_target_iter(ARG_08,ARG_09,ARG_07) ,
  generator_exp_elt_generators(ARG_06,[ARG_07],ARG_05) ,
  tuple(ARG_05,TUPLE_04) ,
  exit_proc(TUPLE_04).
%~ % Universal AST Pass #0
%~ def( "mpapply",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("function"), argument_name("a"),argument_name("b")],
%~         [argument_type("Tuple")]),
%~      block_statements( [ expr_value(string_value(' apply function on two vectors and merge ')),
%~                          return_value(call_func_args("merge",[call_func_args("papply",["function","a","b"])]))])))
%~
% Compiled KL-1 for mpapply
mpapply(FUNCTION_01,A_02,B_03,TUPLE_04) :-
  willBeType(TUPLE_04,'Tuple') ,
  comment(' apply function on two vectors and merge ') ,
  papply(FUNCTION_01,A_02,B_03,ARG_05) ,
  merge(ARG_05,TUPLE_04) ,
  exit_proc(TUPLE_04).
%~ % Universal AST Pass #0
%~ def( "prapply",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("function"), argument_name("a"),argument_name("b")],
%~         [argument_type("FrozenSet")]),
%~      block_statements( [ expr_value(string_value(' apply function on cartesian product ')),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators(
%~                                             call_func_args("function",["i","j"]),
%~                                             [ comprehension_target_iter("j","b"),
%~                                               comprehension_target_iter("i","a")])]))])))
%~
%~ replacing_with_var(call_func_args(FUNCTION_01,["i","j"]))
%~
% Compiled KL-1 for prapply
prapply(FUNCTION_01,A_02,B_03,FROZENSET_04) :-
  willBeType(FROZENSET_04,'FrozenSet') ,
  comment(' apply function on cartesian product ') ,
  call([op_call(op_call(FUNCTION_01,I,J,ARG_06))]) ,
  generator_exp_elt_generators( ARG_06,
    [ assign_targets_value1(J,B_03),
      assign_targets_value1(I,A_02)],
    ARG_05) ,
  frozenset(ARG_05,FROZENSET_04) ,
  exit_proc(FROZENSET_04).
%~ % Universal AST Pass #0
%~ def( "mostcolor",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("element")],[argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' most common color ')),
%~                          assign_targets_value( ["values"],
%~                            if_exp_test_body_orelse(
%~                               call_func_args("isinstance",["element","tuple"]),
%~                               list_comp_elt_generators( "v", [
%~                                 comprehension_target_iter("r","element"),
%~                                 comprehension_target_iter("v","r")]),
%~                               list_comp_elt_generators("v",[comprehension_target_iter(tuple_elts(["v","_"]),"element")]))),
%~                          return_value( call_func_args_keywords( "max",
%~                                          [ call_func_args("set",["values"])],
%~                                          [ keyword_value(qualified_identifier_identifiers(["values",boxed_attribute_value("count")]))]))])))
%~
%~ replacing_with_var(call_func_args("isinstance",[ELEMENT_01,"tuple"]))
%~
%~ replacing_with_var(call_func_args("set",["values"]))
%~
%~ replacing_with_var( keyword_value(qualified_identifier_identifiers(["values",boxed_attribute_value("count")])))
%~
%~ replacing_with_var(comprehension_target_iter("v","r"))
%~
%~ replacing_with_var(qualified_identifier_identifiers(["values",boxed_attribute_value("count")]))
%~
%~ replacing_with_var(tuple_elts(["v","_"]))
%~
%~ replacing_with_var(boxed_attribute_value("count"))
%~
% Compiled KL-1 for mostcolor
mostcolor(ELEMENT_01,INTEGER_02) :-
  willBeType(INTEGER_02,'Integer') ,
  comment(' most common color ') ,
  (/*2*/
    [ isinstance(ELEMENT_01,TUPLE,ARG_03),
      testif(ARG_03)] ->
      [ comprehension_target_iter(V,"r",ARG_06),
        list_comp_elt_generators( V,
          [ assign_targets_value1("r",ELEMENT_01),
            ARG_06],
          VALUES)] ;
    [ list_comp_elt_generators( V,
        [ into_tuple(V,"_",ARG_08),
          assign_targets_value1(ARG_08,ELEMENT_01)],
        VALUES)]) ,
  set(VALUES,ARG_04) ,
  boxed_attribute_value("count",ARG_09) ,
  qualified_identifier_identifiers([VALUES,ARG_09],ARG_07) ,
  keyword_value(ARG_07,ARG_05) ,
  call_func_args_keywords("max",[ARG_04],[ARG_05],INTEGER_02) ,
  exit_proc(INTEGER_02).
%~ % Universal AST Pass #0
%~ def( "leastcolor",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("element")],[argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' least common color ')),
%~                          assign_targets_value( ["values"],
%~                            if_exp_test_body_orelse(
%~                               call_func_args("isinstance",["element","tuple"]),
%~                               list_comp_elt_generators( "v", [
%~                                 comprehension_target_iter("r","element"),
%~                                 comprehension_target_iter("v","r")]),
%~                               list_comp_elt_generators("v",[comprehension_target_iter(tuple_elts(["v","_"]),"element")]))),
%~                          return_value( call_func_args_keywords( "min",
%~                                          [ call_func_args("set",["values"])],
%~                                          [ keyword_value(qualified_identifier_identifiers(["values",boxed_attribute_value("count")]))]))])))
%~
%~ replacing_with_var(call_func_args("isinstance",[ELEMENT_01,"tuple"]))
%~
%~ replacing_with_var(call_func_args("set",["values"]))
%~
%~ replacing_with_var( keyword_value(qualified_identifier_identifiers(["values",boxed_attribute_value("count")])))
%~
%~ replacing_with_var(comprehension_target_iter("v","r"))
%~
%~ replacing_with_var(qualified_identifier_identifiers(["values",boxed_attribute_value("count")]))
%~
%~ replacing_with_var(tuple_elts(["v","_"]))
%~
%~ replacing_with_var(boxed_attribute_value("count"))
%~
% Compiled KL-1 for leastcolor
leastcolor(ELEMENT_01,INTEGER_02) :-
  willBeType(INTEGER_02,'Integer') ,
  comment(' least common color ') ,
  (/*2*/
    [ isinstance(ELEMENT_01,TUPLE,ARG_03),
      testif(ARG_03)] ->
      [ comprehension_target_iter(V,"r",ARG_06),
        list_comp_elt_generators( V,
          [ assign_targets_value1("r",ELEMENT_01),
            ARG_06],
          VALUES)] ;
    [ list_comp_elt_generators( V,
        [ into_tuple(V,"_",ARG_08),
          assign_targets_value1(ARG_08,ELEMENT_01)],
        VALUES)]) ,
  set(VALUES,ARG_04) ,
  boxed_attribute_value("count",ARG_09) ,
  qualified_identifier_identifiers([VALUES,ARG_09],ARG_07) ,
  keyword_value(ARG_07,ARG_05) ,
  call_func_args_keywords("min",[ARG_04],[ARG_05],INTEGER_02) ,
  exit_proc(INTEGER_02).
%~ % Universal AST Pass #0
%~ def( "height",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("piece")],[argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' height of grid or patch ')),
%~                          if_test_body(
%~                             compare_ops_left_comparators(eq_token(==),call_func_args("len",["piece"]),0),
%~                             body_stmts([return_value(0)])),
%~                          if_test_body(
%~                             call_func_args("isinstance",["piece","tuple"]),
%~                             body_stmts([return_value(call_func_args("len",["piece"]))])),
%~                          return_value( bin_op_left_right( add_token(+),
%~                                          bin_op_left_right( sub_token(-),
%~                                            call_func_args("lowermost",["piece"]),
%~                                            call_func_args("uppermost",["piece"])),
%~                                          1))])))
%~
%~ replacing_with_var(body_stmts([return_value(0)]))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),call_func_args("len",[PIECE_01]),0))
%~
%~ replacing_with_var(body_stmts([return_value(call_func_args("len",[PIECE_01]))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[PIECE_01,"tuple"]))
%~
%~ replacing_with_var(return_value(0))
%~
%~ replacing_with_var(call_func_args("len",[PIECE_01]))
%~
%~ replacing_with_var(return_value(call_func_args("len",[PIECE_01])))
%~
%~ replacing_with_var(call_func_args("len",[PIECE_01]))
%~
% Compiled KL-1 for height
height(PIECE_01,INTEGER_02) :-
  willBeType(INTEGER_02,'Integer') ,
  comment(' height of grid or patch ') ,
  (/*2*/
    [ return_value(0,ARG_08),
      body_stmts([ARG_08],ARG_03),
      [ len(PIECE_01,ARG_09),
        compare_ops_left_comparators(eq_token(==),ARG_09,0,ARG_04),
        testif(ARG_04)] ->
        ARG_03]) ,
  (/*2*/
    [ len(PIECE_01,ARG_013),
      return_value(ARG_013,ARG_010),
      body_stmts([ARG_010],ARG_05),
      [ isinstance(PIECE_01,TUPLE,ARG_06),
        testif(ARG_06)] ->
        ARG_05]) ,
  lowermost(PIECE_01,ARG_011) ,
  uppermost(PIECE_01,ARG_012) ,
  call([op_call(sub_token(-,ARG_011,ARG_012,ARG_07))]) ,
  call([op_call(add_token(+,ARG_07,1,INTEGER_02))]) ,
  exit_proc(INTEGER_02).
%~ % Universal AST Pass #0
%~ def( "width",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("piece")],[argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' width of grid or patch ')),
%~                          if_test_body(
%~                             compare_ops_left_comparators(eq_token(==),call_func_args("len",["piece"]),0),
%~                             body_stmts([return_value(0)])),
%~                          if_test_body(
%~                             call_func_args("isinstance",["piece","tuple"]),
%~                             body_stmts([return_value(call_func_args("len",[subscript_value_slice("piece",0)]))])),
%~                          return_value( bin_op_left_right( add_token(+),
%~                                          bin_op_left_right( sub_token(-),
%~                                            call_func_args("rightmost",["piece"]),
%~                                            call_func_args("leftmost",["piece"])),
%~                                          1))])))
%~
%~ replacing_with_var(body_stmts([return_value(0)]))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),call_func_args("len",[PIECE_01]),0))
%~
%~ replacing_with_var( body_stmts([return_value(call_func_args("len",[subscript_value_slice(PIECE_01,0)]))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[PIECE_01,"tuple"]))
%~
%~ replacing_with_var(return_value(0))
%~
%~ replacing_with_var(call_func_args("len",[PIECE_01]))
%~
%~ replacing_with_var(return_value(call_func_args("len",[subscript_value_slice(PIECE_01,0)])))
%~
%~ replacing_with_var(call_func_args("len",[subscript_value_slice(PIECE_01,0)]))
%~
% Compiled KL-1 for width
width(PIECE_01,INTEGER_02) :-
  willBeType(INTEGER_02,'Integer') ,
  comment(' width of grid or patch ') ,
  (/*2*/
    [ return_value(0,ARG_08),
      body_stmts([ARG_08],ARG_03),
      [ len(PIECE_01,ARG_09),
        compare_ops_left_comparators(eq_token(==),ARG_09,0,ARG_04),
        testif(ARG_04)] ->
        ARG_03]) ,
  (/*2*/
    [ subscript_value_slice(PIECE_01,0,ARG_014),
      len(ARG_014,ARG_013),
      return_value(ARG_013,ARG_010),
      body_stmts([ARG_010],ARG_05),
      [ isinstance(PIECE_01,TUPLE,ARG_06),
        testif(ARG_06)] ->
        ARG_05]) ,
  rightmost(PIECE_01,ARG_011) ,
  leftmost(PIECE_01,ARG_012) ,
  call([op_call(sub_token(-,ARG_011,ARG_012,ARG_07))]) ,
  call([op_call(add_token(+,ARG_07,1,INTEGER_02))]) ,
  exit_proc(INTEGER_02).
%~ % Universal AST Pass #0
%~ def( "shape",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("piece")],[argument_type("IntegerTuple")]),
%~      block_statements( [ expr_value(string_value(' height and width of grid or patch ')),
%~                          return_value( tuple_elts([call_func_args("height",["piece"]),call_func_args("width",["piece"])]))])))
%~
% Compiled KL-1 for shape
shape(PIECE_01,INTEGERTUPLE_02) :-
  willBeType(INTEGERTUPLE_02,'IntegerTuple') ,
  comment(' height and width of grid or patch ') ,
  height(PIECE_01,ARG_03) ,
  width(PIECE_01,ARG_04) ,
  tuple_elts(ARG_03,ARG_04,INTEGERTUPLE_02) ,
  exit_proc(INTEGERTUPLE_02).
%~ % Universal AST Pass #0
%~ def( "portrait",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("piece")],[argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' whether height is greater than width ')),
%~                          return_value( compare_ops_left_comparators( gt_token(>),
%~                                          call_func_args("height",["piece"]),
%~                                          call_func_args("width",["piece"])))])))
%~
%~ replacing_with_var(call_func_args("height",[PIECE_01]))
%~
%~ replacing_with_var(call_func_args("width",[PIECE_01]))
%~
% Compiled KL-1 for portrait
portrait(PIECE_01,BOOLEAN_02) :-
  willBeType(BOOLEAN_02,'Boolean') ,
  comment(' whether height is greater than width ') ,
  height(PIECE_01,ARG_03) ,
  width(PIECE_01,ARG_04) ,
  compare_ops_left_comparators(gt_token(>),ARG_03,ARG_04,BOOLEAN_02) ,
  exit_proc(BOOLEAN_02).
%~ % Universal AST Pass #0
%~ def( "colorcount",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("element"),
%~           argument_name("value")],
%~         [argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' number of cells with color ')),
%~                          if_test_body(
%~                             call_func_args("isinstance",["element","tuple"]),
%~                             body_stmts( [ return_value( call_func_args( "sum", [
%~                                                           generator_exp_elt_generators(
%~                                                              call_func_args(
%~                                                                 qualified_identifier_identifiers(["row",boxed_attribute_value("count")]),
%~                                                                 ["value"]),
%~                                                              [comprehension_target_iter("row","element")])]))])),
%~                          return_value( call_func_args( "sum", [
%~                                          generator_exp_elt_generators(
%~                                             compare_ops_left_comparators(eq_token(==),"v","value"),
%~                                             [ comprehension_target_iter(tuple_elts(["v","_"]),"element")])]))])))
%~
%~ replacing_with_var( body_stmts( [ return_value( call_func_args( "sum", [
%~                                                   generator_exp_elt_generators(
%~                                                      call_func_args(
%~                                                         qualified_identifier_identifiers(["row",boxed_attribute_value("count")]),
%~                                                         [VALUE_02]),
%~                                                      [comprehension_target_iter("row",ELEMENT_01)])]))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[ELEMENT_01,"tuple"]))
%~
%~ replacing_with_var( return_value( call_func_args( "sum", [
%~                                     generator_exp_elt_generators(
%~                                        call_func_args(
%~                                           qualified_identifier_identifiers(["row",boxed_attribute_value("count")]),
%~                                           [VALUE_02]),
%~                                        [comprehension_target_iter("row",ELEMENT_01)])])))
%~
%~ replacing_with_var( call_func_args( "sum", [
%~                       generator_exp_elt_generators(
%~                          call_func_args(
%~                             qualified_identifier_identifiers(["row",boxed_attribute_value("count")]),
%~                             [VALUE_02]),
%~                          [comprehension_target_iter("row",ELEMENT_01)])]))
%~
%~ replacing_with_var(call_func_args(==,["v",VALUE_02]))
%~
%~ replacing_with_var(tuple_elts(["v","_"]))
%~
%~ replacing_with_var( call_func_args(
%~                        qualified_identifier_identifiers(["row",boxed_attribute_value("count")]),
%~                        [VALUE_02]))
%~
%~ replacing_with_var(boxed_attribute_value("count"))
%~
% Compiled KL-1 for colorcount
colorcount(ELEMENT_01,VALUE_02,INTEGER_03) :-
  willBeType(INTEGER_03,'Integer') ,
  comment(' number of cells with color ') ,
  (/*2*/
    [ call( [ op_call( [ boxed_attribute_value("count",ARG_013),
                         qualified_identifier_identifiers(["row",ARG_013],VALUE_02,ARG_012)])]),
      generator_exp_elt_generators( ARG_012,
        [assign_targets_value1("row",ELEMENT_01)],
        ARG_09),
      sum(ARG_09,ARG_08),
      return_value(ARG_08,ARG_07),
      body_stmts([ARG_07],ARG_04),
      [ isinstance(ELEMENT_01,TUPLE,ARG_05),
        testif(ARG_05)] ->
        ARG_04]) ,
  call([op_call(op_call(==,V,VALUE_02,ARG_010))]) ,
  generator_exp_elt_generators( ARG_010,
    [ into_tuple(V,"_",ARG_011),
      assign_targets_value1(ARG_011,ELEMENT_01)],
    ARG_06) ,
  sum(ARG_06,INTEGER_03) ,
  exit_proc(INTEGER_03).
%~ % Universal AST Pass #0
%~ def( "colorfilter",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("objs"),
%~           argument_name("value")],
%~         [argument_type("Objects")]),
%~      block_statements( [ expr_value(string_value(' filter objects by color ')),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators( "obj", [
%~                                            comprehension_target_iter_ifs( "obj",
%~                                              "objs",
%~                                              [ compare_ops_left_comparators( eq_token(==),
%~                                                  subscript_value_slice(call_func_args("next",[call_func_args("iter",["obj"])]),0),
%~                                                  "value")])])]))])))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( "obj",
%~                       OBJS_01,
%~                       [ compare_ops_left_comparators( eq_token(==),
%~                           subscript_value_slice(call_func_args("next",[call_func_args("iter",["obj"])]),0),
%~                           VALUE_02)]))
%~
%~ replacing_with_var( call_func_args( ==, [
%~                       subscript_value_slice(call_func_args("next",[call_func_args("iter",["obj"])]),0),
%~                       VALUE_02]))
%~
% Compiled KL-1 for colorfilter
colorfilter(OBJS_01,VALUE_02,OBJECTS_03) :-
  willBeType(OBJECTS_03,'Objects') ,
  comment(' filter objects by color ') ,
  iter(OBJ,ARG_09) ,
  next(ARG_09,ARG_08) ,
  subscript_value_slice(ARG_08,0,ARG_07) ,
  call([op_call(op_call(==,ARG_07,VALUE_02,ARG_06))]) ,
  comprehension_target_iter_ifs(OBJ,OBJS_01,[ARG_06],ARG_05) ,
  generator_exp_elt_generators(OBJ,[ARG_05],ARG_04) ,
  frozenset(ARG_04,OBJECTS_03) ,
  exit_proc(OBJECTS_03).
%~ % Universal AST Pass #0
%~ def( "sizefilter",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("container"),
%~           argument_name("n")],
%~         [argument_type("FrozenSet")]),
%~      block_statements( [ expr_value(string_value(' filter items by size ')),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators( "item", [
%~                                            comprehension_target_iter_ifs( "item",
%~                                              "container",
%~                                              [ compare_ops_left_comparators(eq_token(==),call_func_args("len",["item"]),"n")])])]))])))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( "item",
%~                       CONTAINER_01,
%~                       [ compare_ops_left_comparators(eq_token(==),call_func_args("len",["item"]),N_02)]))
%~
%~ replacing_with_var(call_func_args(==,[call_func_args("len",["item"]),N_02]))
%~
% Compiled KL-1 for sizefilter
sizefilter(CONTAINER_01,N_02,FROZENSET_03) :-
  willBeType(FROZENSET_03,'FrozenSet') ,
  comment(' filter items by size ') ,
  len(ITEM,ARG_07) ,
  call([op_call(op_call(==,ARG_07,N_02,ARG_06))]) ,
  comprehension_target_iter_ifs(ITEM,CONTAINER_01,[ARG_06],ARG_05) ,
  generator_exp_elt_generators(ITEM,[ARG_05],ARG_04) ,
  frozenset(ARG_04,FROZENSET_03) ,
  exit_proc(FROZENSET_03).
%~ % Universal AST Pass #0
%~ def( "asindices",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("grid")],[argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' indices of all grid cells ')),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                            comprehension_target_iter("i",call_func_args("range",[call_func_args("len",["grid"])])),
%~                                            comprehension_target_iter( "j",
%~                                              call_func_args("range",[call_func_args("len",[subscript_value_slice("grid",0)])]))])]))])))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var( comprehension_target_iter("i",call_func_args("range",[call_func_args("len",[GRID_01])])))
%~
%~ replacing_with_var( comprehension_target_iter( "j",
%~                       call_func_args("range",[call_func_args("len",[subscript_value_slice(GRID_01,0)])])))
%~
%~ replacing_with_var(call_func_args("range",[call_func_args("len",[GRID_01])]))
%~
%~ replacing_with_var( call_func_args("range",[call_func_args("len",[subscript_value_slice(GRID_01,0)])]))
%~
% Compiled KL-1 for asindices
asindices(GRID_01,INDICES_02) :-
  willBeType(INDICES_02,'Indices') ,
  comment(' indices of all grid cells ') ,
  into_tuple(I,J,ARG_04) ,
  len(GRID_01,ARG_08) ,
  range(ARG_08,ARG_07) ,
  comprehension_target_iter(I,ARG_07,ARG_05) ,
  subscript_value_slice(GRID_01,0,ARG_011) ,
  len(ARG_011,ARG_010) ,
  range(ARG_010,ARG_09) ,
  comprehension_target_iter(J,ARG_09,ARG_06) ,
  generator_exp_elt_generators(ARG_04,[ARG_05,ARG_06],ARG_03) ,
  frozenset(ARG_03,INDICES_02) ,
  exit_proc(INDICES_02).
%~ % Universal AST Pass #0
%~ def( "ofcolor",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"),
%~           argument_name("value")],
%~         [argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' indices of all grid cells with value ')),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                            comprehension_target_iter(tuple_elts(["i","r"]),call_func_args("enumerate",["grid"])),
%~                                            comprehension_target_iter_ifs( tuple_elts(["j","v"]),
%~                                              call_func_args("enumerate",["r"]),
%~                                              [ compare_ops_left_comparators(eq_token(==),"v","value")])])]))])))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var( comprehension_target_iter(tuple_elts(["i","r"]),call_func_args("enumerate",[GRID_01])))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( tuple_elts(["j","v"]),
%~                       call_func_args("enumerate",["r"]),
%~                       [ compare_ops_left_comparators(eq_token(==),"v",VALUE_02)]))
%~
%~ replacing_with_var(tuple_elts(["i","r"]))
%~
%~ replacing_with_var(call_func_args("enumerate",[GRID_01]))
%~
%~ replacing_with_var(tuple_elts(["j","v"]))
%~
%~ replacing_with_var(call_func_args("enumerate",["r"]))
%~
%~ replacing_with_var(call_func_args(==,["v",VALUE_02]))
%~
% Compiled KL-1 for ofcolor
ofcolor(GRID_01,VALUE_02,INDICES_03) :-
  willBeType(INDICES_03,'Indices') ,
  comment(' indices of all grid cells with value ') ,
  into_tuple(I,J,ARG_05) ,
  into_tuple(I,R,ARG_08) ,
  enumerate(GRID_01,ARG_09) ,
  comprehension_target_iter(ARG_08,ARG_09,ARG_06) ,
  into_tuple(J,V,ARG_010) ,
  enumerate(R,ARG_011) ,
  call([op_call(op_call(==,V,VALUE_02,ARG_012))]) ,
  comprehension_target_iter_ifs(ARG_010,ARG_011,[ARG_012],ARG_07) ,
  generator_exp_elt_generators(ARG_05,[ARG_06,ARG_07],ARG_04) ,
  frozenset(ARG_04,INDICES_03) ,
  exit_proc(INDICES_03).
%~ % Universal AST Pass #0
%~ def( "ulcorner",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
%~      block_statements( [ expr_value(string_value(' index of upper left corner ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          call_func_args( "map", [
%~                                            "min",
%~                                            call_func_args("zip",[starred_value(call_func_args("toindices",["patch"]))])])]))])))
%~
% Compiled KL-1 for ulcorner
ulcorner(PATCH_01,INTEGERTUPLE_02) :-
  willBeType(INTEGERTUPLE_02,'IntegerTuple') ,
  comment(' index of upper left corner ') ,
  toindices(PATCH_01,ARG_06) ,
  starred_value(ARG_06,ARG_05) ,
  zip(ARG_05,ARG_04) ,
  map(MIN,ARG_04,ARG_03) ,
  tuple(ARG_03,INTEGERTUPLE_02) ,
  exit_proc(INTEGERTUPLE_02).
%~ % Universal AST Pass #0
%~ def( "urcorner",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
%~      block_statements( [ expr_value(string_value(' index of upper right corner ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          call_func_args( "map", [
%~                                            lambda_args_body( arguments_args([argument_name("ix")]),
%~                                              body_stmts( call_func_args(
%~                                                             subscript_value_slice(
%~                                                                dict_keys_values([0,1],["min","max"]),
%~                                                                subscript_value_slice("ix",0)),
%~                                                             [subscript_value_slice("ix",1)]))),
%~                                            call_func_args( "enumerate", [
%~                                              call_func_args("zip",[starred_value(call_func_args("toindices",["patch"]))])])])]))])))
%~
%~ replacing_with_var(arguments_args([argument_name("ix")]))
%~
%~ replacing_with_var( body_stmts( call_func_args(
%~                                    subscript_value_slice(
%~                                       dict_keys_values([0,1],["min","max"]),
%~                                       subscript_value_slice("ix",0)),
%~                                    [subscript_value_slice("ix",1)])))
%~
%~ replacing_with_var(argument_name("ix"))
%~
%~ replacing_with_var( call_func_args(
%~                        subscript_value_slice(
%~                           dict_keys_values([0,1],["min","max"]),
%~                           subscript_value_slice("ix",0)),
%~                        [subscript_value_slice("ix",1)]))
%~
%~ replacing_with_var(dict_keys_values([0,1],["min","max"]))
%~
%~ replacing_with_var(subscript_value_slice("ix",0))
%~
% Compiled KL-1 for urcorner
urcorner(PATCH_01,INTEGERTUPLE_02) :-
  willBeType(INTEGERTUPLE_02,'IntegerTuple') ,
  comment(' index of upper right corner ') ,
  argument_name(IX,ARG_010) ,
  arguments_args([ARG_010],ARG_07) ,
  subscript_value_slice(IX,1,ARG_012) ,
  call( [ op_call( [ dict_keys_values([0,1],["min","max"],ARG_014),
                     subscript_value_slice(IX,0,ARG_015),
                     subscript_value_slice(ARG_014,ARG_015,ARG_012,ARG_011)])]) ,
  body_stmts(ARG_011,ARG_08) ,
  lambda_args_body(ARG_07,ARG_08,ARG_04) ,
  toindices(PATCH_01,ARG_013) ,
  starred_value(ARG_013,ARG_09) ,
  zip(ARG_09,ARG_06) ,
  enumerate(ARG_06,ARG_05) ,
  map(ARG_04,ARG_05,ARG_03) ,
  tuple(ARG_03,INTEGERTUPLE_02) ,
  exit_proc(INTEGERTUPLE_02).
%~ % Universal AST Pass #0
%~ def( "llcorner",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
%~      block_statements( [ expr_value(string_value(' index of lower left corner ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          call_func_args( "map", [
%~                                            lambda_args_body( arguments_args([argument_name("ix")]),
%~                                              body_stmts( call_func_args(
%~                                                             subscript_value_slice(
%~                                                                dict_keys_values([0,1],["max","min"]),
%~                                                                subscript_value_slice("ix",0)),
%~                                                             [subscript_value_slice("ix",1)]))),
%~                                            call_func_args( "enumerate", [
%~                                              call_func_args("zip",[starred_value(call_func_args("toindices",["patch"]))])])])]))])))
%~
%~ replacing_with_var(arguments_args([argument_name("ix")]))
%~
%~ replacing_with_var( body_stmts( call_func_args(
%~                                    subscript_value_slice(
%~                                       dict_keys_values([0,1],["max","min"]),
%~                                       subscript_value_slice("ix",0)),
%~                                    [subscript_value_slice("ix",1)])))
%~
%~ replacing_with_var(argument_name("ix"))
%~
%~ replacing_with_var( call_func_args(
%~                        subscript_value_slice(
%~                           dict_keys_values([0,1],["max","min"]),
%~                           subscript_value_slice("ix",0)),
%~                        [subscript_value_slice("ix",1)]))
%~
%~ replacing_with_var(dict_keys_values([0,1],["max","min"]))
%~
%~ replacing_with_var(subscript_value_slice("ix",0))
%~
% Compiled KL-1 for llcorner
llcorner(PATCH_01,INTEGERTUPLE_02) :-
  willBeType(INTEGERTUPLE_02,'IntegerTuple') ,
  comment(' index of lower left corner ') ,
  argument_name(IX,ARG_010) ,
  arguments_args([ARG_010],ARG_07) ,
  subscript_value_slice(IX,1,ARG_012) ,
  call( [ op_call( [ dict_keys_values([0,1],["max","min"],ARG_014),
                     subscript_value_slice(IX,0,ARG_015),
                     subscript_value_slice(ARG_014,ARG_015,ARG_012,ARG_011)])]) ,
  body_stmts(ARG_011,ARG_08) ,
  lambda_args_body(ARG_07,ARG_08,ARG_04) ,
  toindices(PATCH_01,ARG_013) ,
  starred_value(ARG_013,ARG_09) ,
  zip(ARG_09,ARG_06) ,
  enumerate(ARG_06,ARG_05) ,
  map(ARG_04,ARG_05,ARG_03) ,
  tuple(ARG_03,INTEGERTUPLE_02) ,
  exit_proc(INTEGERTUPLE_02).
%~ % Universal AST Pass #0
%~ def( "lrcorner",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
%~      block_statements( [ expr_value(string_value(' index of lower right corner ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          call_func_args( "map", [
%~                                            "max",
%~                                            call_func_args("zip",[starred_value(call_func_args("toindices",["patch"]))])])]))])))
%~
% Compiled KL-1 for lrcorner
lrcorner(PATCH_01,INTEGERTUPLE_02) :-
  willBeType(INTEGERTUPLE_02,'IntegerTuple') ,
  comment(' index of lower right corner ') ,
  toindices(PATCH_01,ARG_06) ,
  starred_value(ARG_06,ARG_05) ,
  zip(ARG_05,ARG_04) ,
  map(MAX,ARG_04,ARG_03) ,
  tuple(ARG_03,INTEGERTUPLE_02) ,
  exit_proc(INTEGERTUPLE_02).
%~ % Universal AST Pass #0
%~ def( "crop",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"), argument_name("start"),argument_name("dims")],
%~         [argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' subgrid specified by start and dimension ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators(
%~                                             subscript_value_slice( "r",
%~                                               slice_lower_upper( subscript_value_slice("start",1),
%~                                                 bin_op_left_right(add_token(+),subscript_value_slice("start",1),subscript_value_slice("dims",1)))),
%~                                             [ comprehension_target_iter( "r",
%~                                                 subscript_value_slice( "grid",
%~                                                   slice_lower_upper( subscript_value_slice("start",0),
%~                                                     bin_op_left_right(add_token(+),subscript_value_slice("start",0),subscript_value_slice("dims",0)))))])]))])))
%~
%~ replacing_with_var( subscript_value_slice( "r",
%~                       slice_lower_upper( subscript_value_slice(START_02,1),
%~                         bin_op_left_right(add_token(+),subscript_value_slice(START_02,1),subscript_value_slice(DIMS_03,1)))))
%~
%~ replacing_with_var( comprehension_target_iter( "r",
%~                       subscript_value_slice( GRID_01,
%~                         slice_lower_upper( subscript_value_slice(START_02,0),
%~                           bin_op_left_right(add_token(+),subscript_value_slice(START_02,0),subscript_value_slice(DIMS_03,0))))))
%~
%~ replacing_with_var( subscript_value_slice( GRID_01,
%~                       slice_lower_upper( subscript_value_slice(START_02,0),
%~                         bin_op_left_right(add_token(+),subscript_value_slice(START_02,0),subscript_value_slice(DIMS_03,0)))))
%~
%~ replacing_with_var(subscript_value_slice(START_02,1))
%~
%~ replacing_with_var( bin_op_left_right(add_token(+),subscript_value_slice(START_02,1),subscript_value_slice(DIMS_03,1)))
%~
%~ replacing_with_var(subscript_value_slice(START_02,0))
%~
%~ replacing_with_var( bin_op_left_right(add_token(+),subscript_value_slice(START_02,0),subscript_value_slice(DIMS_03,0)))
%~
% Compiled KL-1 for crop
crop(GRID_01,START_02,DIMS_03,GRID_04) :-
  willBeType(GRID_04,'Grid') ,
  comment(' subgrid specified by start and dimension ') ,
  subscript_value_slice(START_02,1,ARG_011) ,
  subscript_value_slice(START_02,1,ARG_013) ,
  subscript_value_slice(DIMS_03,1,ARG_014) ,
  call([op_call(add_token(+,ARG_013,ARG_014,ARG_012))]) ,
  slice_lower_upper(ARG_011,ARG_012,ARG_07) ,
  subscript_value_slice(R,ARG_07,ARG_06) ,
  subscript_value_slice(START_02,0,ARG_015) ,
  subscript_value_slice(START_02,0,ARG_017) ,
  subscript_value_slice(DIMS_03,0,ARG_018) ,
  call([op_call(add_token(+,ARG_017,ARG_018,ARG_016))]) ,
  slice_lower_upper(ARG_015,ARG_016,ARG_010) ,
  subscript_value_slice(GRID_01,ARG_010,ARG_09) ,
  comprehension_target_iter(R,ARG_09,ARG_08) ,
  generator_exp_elt_generators(ARG_06,[ARG_08],ARG_05) ,
  tuple(ARG_05,GRID_04) ,
  exit_proc(GRID_04).
%~ % Universal AST Pass #0
%~ def( "toindices",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' indices of object cells ')),
%~                          if_test_body(
%~                             compare_ops_left_comparators(eq_token(==),call_func_args("len",["patch"]),0),
%~                             body_stmts([return_value(call_func("frozenset"))])),
%~                          if_test_body(
%~                             call_func_args( "isinstance", [
%~                               subscript_value_slice(call_func_args("next",[call_func_args("iter",["patch"])]),1),
%~                               "tuple"]),
%~                             body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                           generator_exp_elt_generators( "index", [
%~                                                             comprehension_target_iter(tuple_elts(["value","index"]),"patch")])]))])),
%~                          return_value("patch")])))
%~
%~ replacing_with_var(body_stmts([return_value(call_func("frozenset"))]))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),call_func_args("len",[PATCH_01]),0))
%~
%~ replacing_with_var( body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                   generator_exp_elt_generators( "index", [
%~                                                     comprehension_target_iter(tuple_elts(["value","index"]),PATCH_01)])]))]))
%~
%~ replacing_with_var( call_func_args( "isinstance", [
%~                       subscript_value_slice(call_func_args("next",[call_func_args("iter",[PATCH_01])]),1),
%~                       "tuple"]))
%~
%~ replacing_with_var(return_value(call_func("frozenset")))
%~
%~ replacing_with_var(call_func_args("len",[PATCH_01]))
%~
%~ replacing_with_var( return_value( call_func_args( "frozenset", [
%~                                     generator_exp_elt_generators( "index", [
%~                                       comprehension_target_iter(tuple_elts(["value","index"]),PATCH_01)])])))
%~
%~ replacing_with_var(call_func("frozenset"))
%~
%~ replacing_with_var( call_func_args( "frozenset", [
%~                       generator_exp_elt_generators( "index", [
%~                         comprehension_target_iter(tuple_elts(["value","index"]),PATCH_01)])]))
%~
%~ replacing_with_var(tuple_elts(["value","index"]))
%~
% Compiled KL-1 for toindices
toindices(PATCH_01,INDICES_02) :-
  willBeType(INDICES_02,'Indices') ,
  comment(' indices of object cells ') ,
  (/*2*/
    [ make_new("frozenset",ARG_012),
      return_value(ARG_012,ARG_08),
      body_stmts([ARG_08],ARG_03),
      [ len(PATCH_01,ARG_09),
        compare_ops_left_comparators(eq_token(==),ARG_09,0,ARG_04),
        testif(ARG_04)] ->
        ARG_03]) ,
  (/*2*/
    [ generator_exp_elt_generators( INDEX,
        [ into_tuple(VALUE,INDEX,ARG_016),
          assign_targets_value1(ARG_016,PATCH_01)],
        ARG_014),
      frozenset(ARG_014,ARG_013),
      return_value(ARG_013,ARG_010),
      body_stmts([ARG_010],ARG_05),
      [ iter(PATCH_01,ARG_015),
        next(ARG_015,ARG_011),
        subscript_value_slice(ARG_011,1,ARG_07),
        isinstance(ARG_07,TUPLE,ARG_06),
        testif(ARG_06)] ->
        ARG_05]) ,
  call(INDICES_02=PATCH_01) ,
  exit_proc(INDICES_02).
%~ % Universal AST Pass #0
%~ def( "recolor",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("value"),
%~           argument_name("patch")],
%~         [argument_type("Object")]),
%~      block_statements( [ expr_value(string_value(' recolor patch ')),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators( tuple_elts(["value","index"]), [
%~                                            comprehension_target_iter("index",call_func_args("toindices",["patch"]))])]))])))
%~
%~ replacing_with_var(tuple_elts([VALUE_01,"index"]))
%~
%~ replacing_with_var(comprehension_target_iter("index",call_func_args("toindices",[PATCH_02])))
%~
%~ replacing_with_var(call_func_args("toindices",[PATCH_02]))
%~
% Compiled KL-1 for recolor
recolor(VALUE_01,PATCH_02,OBJECT_03) :-
  willBeType(OBJECT_03,'Object') ,
  comment(' recolor patch ') ,
  into_tuple(VALUE_01,INDEX,ARG_05) ,
  toindices(PATCH_02,ARG_07) ,
  comprehension_target_iter(INDEX,ARG_07,ARG_06) ,
  generator_exp_elt_generators(ARG_05,[ARG_06],ARG_04) ,
  frozenset(ARG_04,OBJECT_03) ,
  exit_proc(OBJECT_03).
%~ % Universal AST Pass #0
%~ def( "shift",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("patch"),
%~           argument_name("directions")],
%~         [argument_type("Patch")]),
%~      block_statements( [ expr_value(string_value(' shift patch ')),
%~                          assign_targets_value([tuple_elts(["di","dj"])],"directions"),
%~                          if_test_body(
%~                             call_func_args( "isinstance", [
%~                               subscript_value_slice(call_func_args("next",[call_func_args("iter",["patch"])]),1),
%~                               "tuple"]),
%~                             body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                           generator_exp_elt_generators(
%~                                                              tuple_elts( [ "value",
%~                                                                            tuple_elts([bin_op_left_right(add_token(+),"i","di"),bin_op_left_right(add_token(+),"j","dj")])]),
%~                                                              [ comprehension_target_iter(tuple_elts(["value",tuple_elts(["i","j"])]),"patch")])]))])),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators(
%~                                             tuple_elts([bin_op_left_right(add_token(+),"i","di"),bin_op_left_right(add_token(+),"j","dj")]),
%~                                             [ comprehension_target_iter(tuple_elts(["i","j"]),"patch")])]))])))
%~
%~ replacing_with_var( body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                   generator_exp_elt_generators(
%~                                                      tuple_elts( [ "value",
%~                                                                    tuple_elts([bin_op_left_right(add_token(+),"i","di"),bin_op_left_right(add_token(+),"j","dj")])]),
%~                                                      [ comprehension_target_iter(tuple_elts(["value",tuple_elts(["i","j"])]),PATCH_01)])]))]))
%~
%~ replacing_with_var( call_func_args( "isinstance", [
%~                       subscript_value_slice(call_func_args("next",[call_func_args("iter",[PATCH_01])]),1),
%~                       "tuple"]))
%~
%~ replacing_with_var( return_value( call_func_args( "frozenset", [
%~                                     generator_exp_elt_generators(
%~                                        tuple_elts( [ "value",
%~                                                      tuple_elts([bin_op_left_right(add_token(+),"i","di"),bin_op_left_right(add_token(+),"j","dj")])]),
%~                                        [ comprehension_target_iter(tuple_elts(["value",tuple_elts(["i","j"])]),PATCH_01)])])))
%~
%~ replacing_with_var( call_func_args( "frozenset", [
%~                       generator_exp_elt_generators(
%~                          tuple_elts( [ "value",
%~                                        tuple_elts([bin_op_left_right(add_token(+),"i","di"),bin_op_left_right(add_token(+),"j","dj")])]),
%~                          [ comprehension_target_iter(tuple_elts(["value",tuple_elts(["i","j"])]),PATCH_01)])]))
%~
%~ replacing_with_var( tuple_elts([bin_op_left_right(add_token(+),"i","di"),bin_op_left_right(add_token(+),"j","dj")]))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var( tuple_elts( [ "value",
%~                                   tuple_elts([bin_op_left_right(add_token(+),"i","di"),bin_op_left_right(add_token(+),"j","dj")])]))
%~
%~ replacing_with_var(tuple_elts(["value",tuple_elts(["i","j"])]))
%~
% Compiled KL-1 for shift
shift(PATCH_01,DIRECTIONS_02,PATCH_03) :-
  willBeType(PATCH_03,'Patch') ,
  comment(' shift patch ') ,
  from_tuple(DIRECTIONS_02,DI,DJ) ,
  (/*2*/
    [ call([op_call(add_token(+,I,DI,ARG_019))]),
      call([op_call(add_token(+,J,DJ,ARG_020))]),
      tuple_elts(ARG_019,ARG_020,ARG_018),
      tuple_elts(VALUE,ARG_018,ARG_017),
      generator_exp_elt_generators( ARG_017,
        [ into_tuple(I,J,ARG_022),
          tuple_elts(VALUE,ARG_022,ARG_021),
          assign_targets_value1(ARG_021,PATCH_01)],
        ARG_011),
      frozenset(ARG_011,ARG_010),
      return_value(ARG_010,ARG_08),
      body_stmts([ARG_08],ARG_04),
      [ iter(PATCH_01,ARG_012),
        next(ARG_012,ARG_09),
        subscript_value_slice(ARG_09,1,ARG_06),
        isinstance(ARG_06,TUPLE,ARG_05),
        testif(ARG_05)] ->
        ARG_04]) ,
  call([op_call(add_token(+,I,DI,ARG_014))]) ,
  call([op_call(add_token(+,J,DJ,ARG_015))]) ,
  tuple_elts(ARG_014,ARG_015,ARG_013) ,
  generator_exp_elt_generators( ARG_013,
    [ into_tuple(I,J,ARG_016),
      assign_targets_value1(ARG_016,PATCH_01)],
    ARG_07) ,
  frozenset(ARG_07,PATCH_03) ,
  exit_proc(PATCH_03).
%~ % Universal AST Pass #0
%~ def( "normalize",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("Patch")]),
%~      block_statements( [ expr_value(string_value(' moves upper left corner to origin ')),
%~                          return_value( call_func_args( "shift", [
%~                                          "patch",
%~                                          tuple_elts( [ unary_op_operand(us_ub_token(-),call_func_args("uppermost",["patch"])),
%~                                                        unary_op_operand(us_ub_token(-),call_func_args("leftmost",["patch"]))])]))])))
%~
%~ replacing_with_var(call_func_args("uppermost",[PATCH_01]))
%~
%~ replacing_with_var(call_func_args("leftmost",[PATCH_01]))
%~
% Compiled KL-1 for normalize
normalize(PATCH_01,PATCH_02) :-
  willBeType(PATCH_02,'Patch') ,
  comment(' moves upper left corner to origin ') ,
  uppermost(PATCH_01,ARG_06) ,
  unary_op_operand(us_ub_token(-),ARG_06,ARG_04) ,
  leftmost(PATCH_01,ARG_07) ,
  unary_op_operand(us_ub_token(-),ARG_07,ARG_05) ,
  tuple_elts(ARG_04,ARG_05,ARG_03) ,
  shift(PATCH_01,ARG_03,PATCH_02) ,
  exit_proc(PATCH_02).
%~ % Universal AST Pass #0
%~ def( "dneighbors",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("loc")],[argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' directly adjacent indices ')),
%~                          return_value( call_func_args( "frozenset", [
%~                                          set_elts( [ tuple_elts( [ bin_op_left_right(sub_token(-),subscript_value_slice("loc",0),1),
%~                                                                    subscript_value_slice("loc",1)]),
%~                                                      tuple_elts( [ bin_op_left_right(add_token(+),subscript_value_slice("loc",0),1),
%~                                                                    subscript_value_slice("loc",1)]),
%~                                                      tuple_elts( [ subscript_value_slice("loc",0),
%~                                                                    bin_op_left_right(sub_token(-),subscript_value_slice("loc",1),1)]),
%~                                                      tuple_elts( [ subscript_value_slice("loc",0),
%~                                                                    bin_op_left_right(add_token(+),subscript_value_slice("loc",1),1)])])]))])))
%~
%~ replacing_with_var( tuple_elts( [ bin_op_left_right(sub_token(-),subscript_value_slice(LOC_01,0),1),
%~                                   subscript_value_slice(LOC_01,1)]))
%~
%~ replacing_with_var( tuple_elts( [ bin_op_left_right(add_token(+),subscript_value_slice(LOC_01,0),1),
%~                                   subscript_value_slice(LOC_01,1)]))
%~
%~ replacing_with_var( tuple_elts( [ subscript_value_slice(LOC_01,0),
%~                                   bin_op_left_right(sub_token(-),subscript_value_slice(LOC_01,1),1)]))
%~
%~ replacing_with_var( tuple_elts( [ subscript_value_slice(LOC_01,0),
%~                                   bin_op_left_right(add_token(+),subscript_value_slice(LOC_01,1),1)]))
%~
% Compiled KL-1 for dneighbors
dneighbors(LOC_01,INDICES_02) :-
  willBeType(INDICES_02,'Indices') ,
  comment(' directly adjacent indices ') ,
  subscript_value_slice(LOC_01,0,ARG_016) ,
  call([op_call(sub_token(-,ARG_016,1,ARG_05))]) ,
  subscript_value_slice(LOC_01,1,ARG_06) ,
  tuple_elts(ARG_05,ARG_06,ARG_04) ,
  subscript_value_slice(LOC_01,0,ARG_017) ,
  call([op_call(add_token(+,ARG_017,1,ARG_08))]) ,
  subscript_value_slice(LOC_01,1,ARG_09) ,
  tuple_elts(ARG_08,ARG_09,ARG_07) ,
  subscript_value_slice(LOC_01,0,ARG_011) ,
  subscript_value_slice(LOC_01,1,ARG_018) ,
  call([op_call(sub_token(-,ARG_018,1,ARG_012))]) ,
  tuple_elts(ARG_011,ARG_012,ARG_010) ,
  subscript_value_slice(LOC_01,0,ARG_014) ,
  subscript_value_slice(LOC_01,1,ARG_019) ,
  call([op_call(add_token(+,ARG_019,1,ARG_015))]) ,
  tuple_elts(ARG_014,ARG_015,ARG_013) ,
  set_elts( [ARG_04,ARG_07,ARG_010,ARG_013],
    ARG_03) ,
  frozenset(ARG_03,INDICES_02) ,
  exit_proc(INDICES_02).
%~ % Universal AST Pass #0
%~ def( "ineighbors",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("loc")],[argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' diagonally adjacent indices ')),
%~                          return_value( call_func_args( "frozenset", [
%~                                          set_elts( [ tuple_elts( [ bin_op_left_right(sub_token(-),subscript_value_slice("loc",0),1),
%~                                                                    bin_op_left_right(sub_token(-),subscript_value_slice("loc",1),1)]),
%~                                                      tuple_elts( [ bin_op_left_right(sub_token(-),subscript_value_slice("loc",0),1),
%~                                                                    bin_op_left_right(add_token(+),subscript_value_slice("loc",1),1)]),
%~                                                      tuple_elts( [ bin_op_left_right(add_token(+),subscript_value_slice("loc",0),1),
%~                                                                    bin_op_left_right(sub_token(-),subscript_value_slice("loc",1),1)]),
%~                                                      tuple_elts( [ bin_op_left_right(add_token(+),subscript_value_slice("loc",0),1),
%~                                                                    bin_op_left_right(add_token(+),subscript_value_slice("loc",1),1)])])]))])))
%~
%~ replacing_with_var( tuple_elts( [ bin_op_left_right(sub_token(-),subscript_value_slice(LOC_01,0),1),
%~                                   bin_op_left_right(sub_token(-),subscript_value_slice(LOC_01,1),1)]))
%~
%~ replacing_with_var( tuple_elts( [ bin_op_left_right(sub_token(-),subscript_value_slice(LOC_01,0),1),
%~                                   bin_op_left_right(add_token(+),subscript_value_slice(LOC_01,1),1)]))
%~
%~ replacing_with_var( tuple_elts( [ bin_op_left_right(add_token(+),subscript_value_slice(LOC_01,0),1),
%~                                   bin_op_left_right(sub_token(-),subscript_value_slice(LOC_01,1),1)]))
%~
%~ replacing_with_var( tuple_elts( [ bin_op_left_right(add_token(+),subscript_value_slice(LOC_01,0),1),
%~                                   bin_op_left_right(add_token(+),subscript_value_slice(LOC_01,1),1)]))
%~
% Compiled KL-1 for ineighbors
ineighbors(LOC_01,INDICES_02) :-
  willBeType(INDICES_02,'Indices') ,
  comment(' diagonally adjacent indices ') ,
  subscript_value_slice(LOC_01,0,ARG_016) ,
  call([op_call(sub_token(-,ARG_016,1,ARG_05))]) ,
  subscript_value_slice(LOC_01,1,ARG_017) ,
  call([op_call(sub_token(-,ARG_017,1,ARG_06))]) ,
  tuple_elts(ARG_05,ARG_06,ARG_04) ,
  subscript_value_slice(LOC_01,0,ARG_018) ,
  call([op_call(sub_token(-,ARG_018,1,ARG_08))]) ,
  subscript_value_slice(LOC_01,1,ARG_019) ,
  call([op_call(add_token(+,ARG_019,1,ARG_09))]) ,
  tuple_elts(ARG_08,ARG_09,ARG_07) ,
  subscript_value_slice(LOC_01,0,ARG_020) ,
  call([op_call(add_token(+,ARG_020,1,ARG_011))]) ,
  subscript_value_slice(LOC_01,1,ARG_021) ,
  call([op_call(sub_token(-,ARG_021,1,ARG_012))]) ,
  tuple_elts(ARG_011,ARG_012,ARG_010) ,
  subscript_value_slice(LOC_01,0,ARG_022) ,
  call([op_call(add_token(+,ARG_022,1,ARG_014))]) ,
  subscript_value_slice(LOC_01,1,ARG_023) ,
  call([op_call(add_token(+,ARG_023,1,ARG_015))]) ,
  tuple_elts(ARG_014,ARG_015,ARG_013) ,
  set_elts( [ARG_04,ARG_07,ARG_010,ARG_013],
    ARG_03) ,
  frozenset(ARG_03,INDICES_02) ,
  exit_proc(INDICES_02).
%~ % Universal AST Pass #0
%~ def( "neighbors",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("loc")],[argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' adjacent indices ')),
%~                          return_value( bin_op_left_right( bit_or_token('|'),
%~                                          call_func_args("dneighbors",["loc"]),
%~                                          call_func_args("ineighbors",["loc"])))])))
%~
% Compiled KL-1 for neighbors
neighbors(LOC_01,INDICES_02) :-
  willBeType(INDICES_02,'Indices') ,
  comment(' adjacent indices ') ,
  dneighbors(LOC_01,ARG_03) ,
  ineighbors(LOC_01,ARG_04) ,
  call([op_call(bit_or_token('|',ARG_03,ARG_04,INDICES_02))]) ,
  exit_proc(INDICES_02).
%~ % Universal AST Pass #0
%~ def( "objects",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"), argument_name("univalued"),argument_name("diagonal"),
%~           argument_name("without_bg")],
%~         [argument_type("Objects")]),
%~      block_statements( [ expr_value(string_value(' objects occurring on the grid ')),
%~                          assign_targets_value( ["bg"],
%~                            if_exp_test_body_orelse( "without_bg",
%~                              call_func_args("mostcolor",["grid"]),
%~                              none_literal_value_token('None','None'))),
%~                          assign_targets_value(["objs"],call_func("set")),
%~                          assign_targets_value(["occupied"],call_func("set")),
%~                          assign_targets_value( [tuple_elts(["h","w"])],
%~                            tuple_elts( [ call_func_args("len",["grid"]),
%~                                          call_func_args("len",[subscript_value_slice("grid",0)])])),
%~                          assign_targets_value(["unvisited"],call_func_args("asindices",["grid"])),
%~                          assign_targets_value(["diagfun"],if_exp_test_body_orelse("diagonal","neighbors","dneighbors")),
%~                          for_target_iter_body( "loc",
%~                            "unvisited",
%~                            body_stmts( [ if_test_body(
%~                                             compare_ops_left_comparators(['python:In'],"loc","occupied"),
%~                                             body_stmts([['python:Continue']])),
%~                                          assign_targets_value( ["val"],
%~                                            subscript_value_slice(subscript_value_slice("grid",subscript_value_slice("loc",0)),subscript_value_slice("loc",1))),
%~                                          if_test_body(compare_ops_left_comparators(eq_token(==),"val","bg"),body_stmts([['python:Continue']])),
%~                                          assign_targets_value(["obj"],set_elts([tuple_elts(["val","loc"])])),
%~                                          assign_targets_value(["cands"],set_elts(["loc"])),
%~                                          while_test_body(
%~                                             compare_ops_left_comparators(gt_token(>),call_func_args("len",["cands"]),0),
%~                                             body_stmts( [ assign_targets_value(["neighborhood"],call_func("set")),
%~                                                           for_target_iter_body( "cand",
%~                                                             "cands",
%~                                                             body_stmts( [ assign_targets_value( ["v"],
%~                                                                             subscript_value_slice(subscript_value_slice("grid",subscript_value_slice("cand",0)),subscript_value_slice("cand",1))),
%~                                                                           if_test_body(
%~                                                                              if_exp_test_body_orelse( "univalued",
%~                                                                                compare_ops_left_comparators(eq_token(==),"val","v"),
%~                                                                                compare_ops_left_comparators(not_eq_token('!='),"v","bg")),
%~                                                                              body_stmts( [ expr_value( call_func_args(
%~                                                                                                           qualified_identifier_identifiers(["obj",boxed_attribute_value("add")]),
%~                                                                                                           [tuple_elts(["v","cand"])])),
%~                                                                                            expr_value( call_func_args(
%~                                                                                                           qualified_identifier_identifiers(["occupied",boxed_attribute_value("add")]),
%~                                                                                                           ["cand"])),
%~                                                                                            aug_assign_op_value_target( bit_or_token('|'),
%~                                                                                              set_comp_elt_generators( tuple_elts(["i","j"]), [
%~                                                                                                comprehension_target_iter_ifs( tuple_elts(["i","j"]),
%~                                                                                                  call_func_args("diagfun",["cand"]),
%~                                                                                                  [ bool_op_values( ['python:And'], [
%~                                                                                                      compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                                                                                      compare_ops_left_comparators(
%~                                                                                                         ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])])]),
%~                                                                                              "neighborhood")]))])),
%~                                                           assign_targets_value(["cands"],bin_op_left_right(sub_token(-),"neighborhood","occupied"))])),
%~                                          expr_value( call_func_args(
%~                                                         qualified_identifier_identifiers(["objs",boxed_attribute_value("add")]),
%~                                                         [ call_func_args("frozenset",["obj"])]))])),
%~                          return_value(call_func_args("frozenset",["objs"]))])))
%~
%~ replacing_with_var( body_stmts( [ if_test_body(
%~                                      compare_ops_left_comparators(['python:In'],LOC_07,"occupied"),
%~                                      body_stmts([['python:Continue']])),
%~                                   assign_targets_value( ["val"],
%~                                     subscript_value_slice(
%~                                        subscript_value_slice(GRID_01,subscript_value_slice(LOC_07,0)),
%~                                        subscript_value_slice(LOC_07,1))),
%~                                   if_test_body(compare_ops_left_comparators(eq_token(==),"val","bg"),body_stmts([['python:Continue']])),
%~                                   assign_targets_value(["obj"],set_elts([tuple_elts(["val",LOC_07])])),
%~                                   assign_targets_value(["cands"],set_elts([LOC_07])),
%~                                   while_test_body(
%~                                      compare_ops_left_comparators(gt_token(>),call_func_args("len",["cands"]),0),
%~                                      body_stmts( [ assign_targets_value(["neighborhood"],call_func("set")),
%~                                                    for_target_iter_body( "cand",
%~                                                      "cands",
%~                                                      body_stmts( [ assign_targets_value( ["v"],
%~                                                                      subscript_value_slice(
%~                                                                         subscript_value_slice(GRID_01,subscript_value_slice("cand",0)),
%~                                                                         subscript_value_slice("cand",1))),
%~                                                                    if_test_body(
%~                                                                       if_exp_test_body_orelse( UNIVALUED_02,
%~                                                                         compare_ops_left_comparators(eq_token(==),"val","v"),
%~                                                                         compare_ops_left_comparators(not_eq_token('!='),"v","bg")),
%~                                                                       body_stmts( [ expr_value( call_func_args(
%~                                                                                                    qualified_identifier_identifiers(["obj",boxed_attribute_value("add")]),
%~                                                                                                    [tuple_elts(["v","cand"])])),
%~                                                                                     expr_value( call_func_args(
%~                                                                                                    qualified_identifier_identifiers(["occupied",boxed_attribute_value("add")]),
%~                                                                                                    ["cand"])),
%~                                                                                     aug_assign_op_value_target( bit_or_token('|'),
%~                                                                                       set_comp_elt_generators( tuple_elts(["i","j"]), [
%~                                                                                         comprehension_target_iter_ifs( tuple_elts(["i","j"]),
%~                                                                                           call_func_args("diagfun",["cand"]),
%~                                                                                           [ bool_op_values( ['python:And'], [
%~                                                                                               compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                                                                               compare_ops_left_comparators(
%~                                                                                                  ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])])]),
%~                                                                                       "neighborhood")]))])),
%~                                                    assign_targets_value(["cands"],bin_op_left_right(sub_token(-),"neighborhood","occupied"))])),
%~                                   expr_value( call_func_args(
%~                                                  qualified_identifier_identifiers(["objs",boxed_attribute_value("add")]),
%~                                                  [ call_func_args("frozenset",["obj"])]))]))
%~
%~ replacing_with_var( if_test_body(
%~                        compare_ops_left_comparators(['python:In'],LOC_07,"occupied"),
%~                        body_stmts([['python:Continue']])))
%~
%~ replacing_with_var( if_test_body(compare_ops_left_comparators(eq_token(==),"val","bg"),body_stmts([['python:Continue']])))
%~
%~ replacing_with_var( while_test_body(
%~                        compare_ops_left_comparators(gt_token(>),call_func_args("len",["cands"]),0),
%~                        body_stmts( [ assign_targets_value(["neighborhood"],call_func("set")),
%~                                      for_target_iter_body( "cand",
%~                                        "cands",
%~                                        body_stmts( [ assign_targets_value( ["v"],
%~                                                        subscript_value_slice(
%~                                                           subscript_value_slice(GRID_01,subscript_value_slice("cand",0)),
%~                                                           subscript_value_slice("cand",1))),
%~                                                      if_test_body(
%~                                                         if_exp_test_body_orelse( UNIVALUED_02,
%~                                                           compare_ops_left_comparators(eq_token(==),"val","v"),
%~                                                           compare_ops_left_comparators(not_eq_token('!='),"v","bg")),
%~                                                         body_stmts( [ expr_value( call_func_args(
%~                                                                                      qualified_identifier_identifiers(["obj",boxed_attribute_value("add")]),
%~                                                                                      [tuple_elts(["v","cand"])])),
%~                                                                       expr_value( call_func_args(
%~                                                                                      qualified_identifier_identifiers(["occupied",boxed_attribute_value("add")]),
%~                                                                                      ["cand"])),
%~                                                                       aug_assign_op_value_target( bit_or_token('|'),
%~                                                                         set_comp_elt_generators( tuple_elts(["i","j"]), [
%~                                                                           comprehension_target_iter_ifs( tuple_elts(["i","j"]),
%~                                                                             call_func_args("diagfun",["cand"]),
%~                                                                             [ bool_op_values( ['python:And'], [
%~                                                                                 compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                                                                 compare_ops_left_comparators(
%~                                                                                    ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])])]),
%~                                                                         "neighborhood")]))])),
%~                                      assign_targets_value(["cands"],bin_op_left_right(sub_token(-),"neighborhood","occupied"))])))
%~
%~ replacing_with_var( expr_value( call_func_args(
%~                                    qualified_identifier_identifiers(["objs",boxed_attribute_value("add")]),
%~                                    [ call_func_args("frozenset",["obj"])])))
%~
%~ replacing_with_var(compare_ops_left_comparators(['python:In'],LOC_07,"occupied"))
%~
%~ replacing_with_var(body_stmts([['python:Continue']]))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),"val","bg"))
%~
%~ replacing_with_var(body_stmts([['python:Continue']]))
%~
%~ replacing_with_var(compare_ops_left_comparators(gt_token(>),call_func_args("len",["cands"]),0))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value(["neighborhood"],call_func("set")),
%~                                   for_target_iter_body( "cand",
%~                                     "cands",
%~                                     body_stmts( [ assign_targets_value( ["v"],
%~                                                     subscript_value_slice(
%~                                                        subscript_value_slice(GRID_01,subscript_value_slice("cand",0)),
%~                                                        subscript_value_slice("cand",1))),
%~                                                   if_test_body(
%~                                                      if_exp_test_body_orelse( UNIVALUED_02,
%~                                                        compare_ops_left_comparators(eq_token(==),"val","v"),
%~                                                        compare_ops_left_comparators(not_eq_token('!='),"v","bg")),
%~                                                      body_stmts( [ expr_value( call_func_args(
%~                                                                                   qualified_identifier_identifiers(["obj",boxed_attribute_value("add")]),
%~                                                                                   [tuple_elts(["v","cand"])])),
%~                                                                    expr_value( call_func_args(
%~                                                                                   qualified_identifier_identifiers(["occupied",boxed_attribute_value("add")]),
%~                                                                                   ["cand"])),
%~                                                                    aug_assign_op_value_target( bit_or_token('|'),
%~                                                                      set_comp_elt_generators( tuple_elts(["i","j"]), [
%~                                                                        comprehension_target_iter_ifs( tuple_elts(["i","j"]),
%~                                                                          call_func_args("diagfun",["cand"]),
%~                                                                          [ bool_op_values( ['python:And'], [
%~                                                                              compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                                                              compare_ops_left_comparators(
%~                                                                                 ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])])]),
%~                                                                      "neighborhood")]))])),
%~                                   assign_targets_value(["cands"],bin_op_left_right(sub_token(-),"neighborhood","occupied"))]))
%~
%~ replacing_with_var(boxed_attribute_value("add"))
%~
%~ replacing_with_var(tuple_elts(["val",LOC_07]))
%~
%~ replacing_with_var(call_func_args("len",["cands"]))
%~
%~ replacing_with_var( for_target_iter_body( "cand",
%~                       "cands",
%~                       body_stmts( [ assign_targets_value( ["v"],
%~                                       subscript_value_slice(
%~                                          subscript_value_slice(GRID_01,subscript_value_slice("cand",0)),
%~                                          subscript_value_slice("cand",1))),
%~                                     if_test_body(
%~                                        if_exp_test_body_orelse( UNIVALUED_02,
%~                                          compare_ops_left_comparators(eq_token(==),"val","v"),
%~                                          compare_ops_left_comparators(not_eq_token('!='),"v","bg")),
%~                                        body_stmts( [ expr_value( call_func_args(
%~                                                                     qualified_identifier_identifiers(["obj",boxed_attribute_value("add")]),
%~                                                                     [tuple_elts(["v","cand"])])),
%~                                                      expr_value( call_func_args(
%~                                                                     qualified_identifier_identifiers(["occupied",boxed_attribute_value("add")]),
%~                                                                     ["cand"])),
%~                                                      aug_assign_op_value_target( bit_or_token('|'),
%~                                                        set_comp_elt_generators( tuple_elts(["i","j"]), [
%~                                                          comprehension_target_iter_ifs( tuple_elts(["i","j"]),
%~                                                            call_func_args("diagfun",["cand"]),
%~                                                            [ bool_op_values( ['python:And'], [
%~                                                                compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                                                compare_ops_left_comparators(
%~                                                                   ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])])]),
%~                                                        "neighborhood")]))])))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value( ["v"],
%~                                     subscript_value_slice(
%~                                        subscript_value_slice(GRID_01,subscript_value_slice("cand",0)),
%~                                        subscript_value_slice("cand",1))),
%~                                   if_test_body(
%~                                      if_exp_test_body_orelse( UNIVALUED_02,
%~                                        compare_ops_left_comparators(eq_token(==),"val","v"),
%~                                        compare_ops_left_comparators(not_eq_token('!='),"v","bg")),
%~                                      body_stmts( [ expr_value( call_func_args(
%~                                                                   qualified_identifier_identifiers(["obj",boxed_attribute_value("add")]),
%~                                                                   [tuple_elts(["v","cand"])])),
%~                                                    expr_value( call_func_args(
%~                                                                   qualified_identifier_identifiers(["occupied",boxed_attribute_value("add")]),
%~                                                                   ["cand"])),
%~                                                    aug_assign_op_value_target( bit_or_token('|'),
%~                                                      set_comp_elt_generators( tuple_elts(["i","j"]), [
%~                                                        comprehension_target_iter_ifs( tuple_elts(["i","j"]),
%~                                                          call_func_args("diagfun",["cand"]),
%~                                                          [ bool_op_values( ['python:And'], [
%~                                                              compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                                              compare_ops_left_comparators(
%~                                                                 ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])])]),
%~                                                      "neighborhood")]))]))
%~
%~ replacing_with_var(make_new("set","neighborhood"))
%~
%~ replacing_with_var( if_test_body(
%~                        if_exp_test_body_orelse( UNIVALUED_02,
%~                          compare_ops_left_comparators(eq_token(==),"val","v"),
%~                          compare_ops_left_comparators(not_eq_token('!='),"v","bg")),
%~                        body_stmts( [ expr_value( call_func_args(
%~                                                     qualified_identifier_identifiers(["obj",boxed_attribute_value("add")]),
%~                                                     [tuple_elts(["v","cand"])])),
%~                                      expr_value( call_func_args(
%~                                                     qualified_identifier_identifiers(["occupied",boxed_attribute_value("add")]),
%~                                                     ["cand"])),
%~                                      aug_assign_op_value_target( bit_or_token('|'),
%~                                        set_comp_elt_generators( tuple_elts(["i","j"]), [
%~                                          comprehension_target_iter_ifs( tuple_elts(["i","j"]),
%~                                            call_func_args("diagfun",["cand"]),
%~                                            [ bool_op_values( ['python:And'], [
%~                                                compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                                compare_ops_left_comparators(
%~                                                   ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])])]),
%~                                        "neighborhood")])))
%~
%~ replacing_with_var( if_exp_test_body_orelse( UNIVALUED_02,
%~                       compare_ops_left_comparators(eq_token(==),"val","v"),
%~                       compare_ops_left_comparators(not_eq_token('!='),"v","bg")))
%~
%~ replacing_with_var( body_stmts( [ expr_value( call_func_args(
%~                                                  qualified_identifier_identifiers(["obj",boxed_attribute_value("add")]),
%~                                                  [tuple_elts(["v","cand"])])),
%~                                   expr_value( call_func_args(
%~                                                  qualified_identifier_identifiers(["occupied",boxed_attribute_value("add")]),
%~                                                  ["cand"])),
%~                                   aug_assign_op_value_target( bit_or_token('|'),
%~                                     set_comp_elt_generators( tuple_elts(["i","j"]), [
%~                                       comprehension_target_iter_ifs( tuple_elts(["i","j"]),
%~                                         call_func_args("diagfun",["cand"]),
%~                                         [ bool_op_values( ['python:And'], [
%~                                             compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                             compare_ops_left_comparators(
%~                                                ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])])]),
%~                                     "neighborhood")]))
%~
%~ replacing_with_var( expr_value( call_func_args(
%~                                    qualified_identifier_identifiers(["obj",boxed_attribute_value("add")]),
%~                                    [tuple_elts(["v","cand"])])))
%~
%~ replacing_with_var( expr_value( call_func_args(
%~                                    qualified_identifier_identifiers(["occupied",boxed_attribute_value("add")]),
%~                                    ["cand"])))
%~
%~ replacing_with_var( aug_assign_op_value_target( bit_or_token('|'),
%~                       set_comp_elt_generators( tuple_elts(["i","j"]), [
%~                         comprehension_target_iter_ifs( tuple_elts(["i","j"]),
%~                           call_func_args("diagfun",["cand"]),
%~                           [ bool_op_values( ['python:And'], [
%~                               compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                               compare_ops_left_comparators(
%~                                  ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])])]),
%~                       "neighborhood"))
%~
%~ replacing_with_var(boxed_attribute_value("add"))
%~
%~ replacing_with_var(boxed_attribute_value("add"))
%~
%~ replacing_with_var( set_comp_elt_generators( tuple_elts(["i","j"]), [
%~                       comprehension_target_iter_ifs( tuple_elts(["i","j"]),
%~                         call_func_args("diagfun",["cand"]),
%~                         [ bool_op_values( ['python:And'], [
%~                             compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                             compare_ops_left_comparators(
%~                                ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])])]))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( tuple_elts(["i","j"]),
%~                       call_func_args("diagfun",["cand"]),
%~                       [ bool_op_values( ['python:And'], [
%~                           compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                           compare_ops_left_comparators(
%~                              ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])]))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var(call_func_args("diagfun",["cand"]))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["j","w"])))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["i","h"]))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["j","w"]))
%~
% Compiled KL-1 for objects
objects(GRID_01,UNIVALUED_02,DIAGONAL_03,WITHOUT_BG_04,OBJECTS_05) :-
  willBeType(OBJECTS_05,'Objects') ,
  comment(' objects occurring on the grid ') ,
  (/*2*/
    testif(WITHOUT_BG_04)->[mostcolor(GRID_01,BG)] ;
    [none_literal_value_token('None','None',BG)]) ,
  make_new("set",OBJS) ,
  make_new("set",OCCUPIED) ,
  len(GRID_01,H) ,
  subscript_value_slice(GRID_01,0,ARG_06) ,
  len(ARG_06,W) ,
  asindices(GRID_01,UNVISITED) ,
  testif(DIAGONAL_03)->call("diagfun"="neighbors");call("diagfun"="dneighbors") ,
  compare_ops_left_comparators(['python:In'],LOC_07,OCCUPIED,ARG_016) ,
  body_stmts(['python:Continue'],ARG_017) ,
  if_test_body(ARG_016,ARG_017,ARG_09) ,
  compare_ops_left_comparators(eq_token(==),VAL,BG,ARG_018) ,
  body_stmts(['python:Continue'],ARG_019) ,
  if_test_body(ARG_018,ARG_019,ARG_010) ,
  len(CANDS,ARG_025) ,
  compare_ops_left_comparators(gt_token(>),ARG_025,0,ARG_020) ,
  (/*2*/
    testif(UNIVALUED_02) ->
      [ compare_ops_left_comparators(eq_token(==),VAL,V,ARG_032)] ;
    [ compare_ops_left_comparators(not_eq_token('!='),V,BG,ARG_032)]) ,
  into_tuple(V,CAND,ARG_036) ,
  call( [ op_call( [ boxed_attribute_value("add",ARG_039),
                     qualified_identifier_identifiers( [OBJ,ARG_039],
                       ARG_036,
                       ARG_035)])]) ,
  call( [ op_call( [ boxed_attribute_value("add",ARG_040),
                     qualified_identifier_identifiers( [OCCUPIED,ARG_040],
                       CAND,
                       ARG_037)])]) ,
  into_tuple(I,J,ARG_042) ,
  into_tuple(I,J,ARG_044) ,
  diagfun(CAND,ARG_045) ,
  comprehension_target_iter_ifs( ARG_044,
    ARG_045,
    [ ops([lt_e_token(<=),lt_token(<)],ARG_048),
      comparators([I,H],ARG_049),
      compare_ops_left_comparators(ARG_048,0,ARG_049,ARG_046),
      ops([lt_e_token(<=),lt_token(<)],ARG_050),
      comparators([J,W],ARG_051),
      compare_ops_left_comparators(ARG_050,0,ARG_051,ARG_047),
      bool_op_values(['python:And'],[ARG_046,ARG_047])],
    ARG_043) ,
  set_comp_elt_generators(ARG_042,[ARG_043],ARG_041) ,
  aug_assign_op_value_target(bit_or_token('|'),ARG_041,NEIGHBORHOOD,ARG_038) ,
  body_stmts([ARG_035,ARG_037,ARG_038],ARG_033) ,
  if_test_body(ARG_032,ARG_033,ARG_029) ,
  body_stmts(
     [ subscript_value_slice(CAND,0,ARG_034),
       subscript_value_slice(GRID_01,ARG_034,ARG_030),
       subscript_value_slice(CAND,1,ARG_031),
       subscript_value_slice(ARG_030,ARG_031,V),
       ARG_029],
     ARG_027) ,
  for_target_iter_body(CAND,CANDS,ARG_027,ARG_026) ,
  make_new("set",NEIGHBORHOOD,ARG_028) ,
  body_stmts(
     [ ARG_028,
       ARG_026,
       call([op_call(sub_token(-,NEIGHBORHOOD,OCCUPIED,CANDS))])],
     ARG_021) ,
  while_test_body(ARG_020,ARG_021,ARG_011) ,
  frozenset(OBJ,ARG_013) ,
  call( [ op_call( [ boxed_attribute_value("add",ARG_022),
                     qualified_identifier_identifiers( [OBJS,ARG_022],
                       ARG_013,
                       ARG_012)])]) ,
  body_stmts(
     [ ARG_09,
       subscript_value_slice(LOC_07,0,ARG_023),
       subscript_value_slice(GRID_01,ARG_023,ARG_014),
       subscript_value_slice(LOC_07,1,ARG_015),
       subscript_value_slice(ARG_014,ARG_015,VAL),
       ARG_010,
       into_tuple(VAL,LOC_07,ARG_024),
       set_elts([ARG_024],OBJ),
       set_elts([LOC_07],CANDS), ARG_011,ARG_012],
     ARG_08) ,
  for_each(call(LOC_07=UNVISITED),ARG_08) ,
  frozenset(OBJS,OBJECTS_05) ,
  exit_proc(OBJECTS_05).
%~ % Universal AST Pass #0
%~ def( "partition",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("grid")],[argument_type("Objects")]),
%~      block_statements( [ expr_value(string_value(' each cell with the same value part of the same object ')),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators(
%~                                             call_func_args( "frozenset", [
%~                                               generator_exp_elt_generators(
%~                                                  tuple_elts(["v",tuple_elts(["i","j"])]),
%~                                                  [ comprehension_target_iter(tuple_elts(["i","r"]),call_func_args("enumerate",["grid"])),
%~                                                    comprehension_target_iter_ifs( tuple_elts(["j","v"]),
%~                                                      call_func_args("enumerate",["r"]),
%~                                                      [ compare_ops_left_comparators(eq_token(==),"v","value")])])]),
%~                                             [ comprehension_target_iter("value",call_func_args("palette",["grid"]))])]))])))
%~
%~ replacing_with_var( call_func_args( "frozenset", [
%~                       generator_exp_elt_generators(
%~                          tuple_elts(["v",tuple_elts(["i","j"])]),
%~                          [ comprehension_target_iter(tuple_elts(["i","r"]),call_func_args("enumerate",[GRID_01])),
%~                            comprehension_target_iter_ifs( tuple_elts(["j","v"]),
%~                              call_func_args("enumerate",["r"]),
%~                              [ compare_ops_left_comparators(eq_token(==),"v","value")])])]))
%~
%~ replacing_with_var(comprehension_target_iter("value",call_func_args("palette",[GRID_01])))
%~
%~ replacing_with_var(call_func_args("palette",[GRID_01]))
%~
%~ replacing_with_var(tuple_elts(["v",tuple_elts(["i","j"])]))
%~
%~ replacing_with_var( comprehension_target_iter(tuple_elts(["i","r"]),call_func_args("enumerate",[GRID_01])))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( tuple_elts(["j","v"]),
%~                       call_func_args("enumerate",["r"]),
%~                       [ compare_ops_left_comparators(eq_token(==),"v","value")]))
%~
%~ replacing_with_var(tuple_elts(["i","r"]))
%~
%~ replacing_with_var(call_func_args("enumerate",[GRID_01]))
%~
%~ replacing_with_var(tuple_elts(["j","v"]))
%~
%~ replacing_with_var(call_func_args("enumerate",["r"]))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),"v","value"))
%~
% Compiled KL-1 for partition
partition(GRID_01,OBJECTS_02) :-
  willBeType(OBJECTS_02,'Objects') ,
  comment(' each cell with the same value part of the same object ') ,
  into_tuple(I,J,ARG_09) ,
  tuple_elts(V,ARG_09,ARG_08) ,
  into_tuple(I,R,ARG_012) ,
  enumerate(GRID_01,ARG_013) ,
  comprehension_target_iter(ARG_012,ARG_013,ARG_010) ,
  into_tuple(J,V,ARG_014) ,
  enumerate(R,ARG_015) ,
  compare_ops_left_comparators(eq_token(==),V,"value",ARG_016) ,
  comprehension_target_iter_ifs(ARG_014,ARG_015,[ARG_016],ARG_011) ,
  generator_exp_elt_generators(ARG_08,[ARG_010,ARG_011],ARG_05) ,
  frozenset(ARG_05,ARG_04) ,
  palette(GRID_01,ARG_07) ,
  comprehension_target_iter("value",ARG_07,ARG_06) ,
  generator_exp_elt_generators(ARG_04,[ARG_06],ARG_03) ,
  frozenset(ARG_03,OBJECTS_02) ,
  exit_proc(OBJECTS_02).
%~ % Universal AST Pass #0
%~ def( "fgpartition",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("grid")],[argument_type("Objects")]),
%~      block_statements( [ expr_value(string_value(' each cell with the same value part of the same object without background ')),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators(
%~                                             call_func_args( "frozenset", [
%~                                               generator_exp_elt_generators(
%~                                                  tuple_elts(["v",tuple_elts(["i","j"])]),
%~                                                  [ comprehension_target_iter(tuple_elts(["i","r"]),call_func_args("enumerate",["grid"])),
%~                                                    comprehension_target_iter_ifs( tuple_elts(["j","v"]),
%~                                                      call_func_args("enumerate",["r"]),
%~                                                      [ compare_ops_left_comparators(eq_token(==),"v","value")])])]),
%~                                             [ comprehension_target_iter( "value",
%~                                                 bin_op_left_right( sub_token(-),
%~                                                   call_func_args("palette",["grid"]),
%~                                                   set_elts([call_func_args("mostcolor",["grid"])])))])]))])))
%~
%~ replacing_with_var( call_func_args( "frozenset", [
%~                       generator_exp_elt_generators(
%~                          tuple_elts(["v",tuple_elts(["i","j"])]),
%~                          [ comprehension_target_iter(tuple_elts(["i","r"]),call_func_args("enumerate",[GRID_01])),
%~                            comprehension_target_iter_ifs( tuple_elts(["j","v"]),
%~                              call_func_args("enumerate",["r"]),
%~                              [ compare_ops_left_comparators(eq_token(==),"v","value")])])]))
%~
%~ replacing_with_var( comprehension_target_iter( "value",
%~                       bin_op_left_right( sub_token(-),
%~                         call_func_args("palette",[GRID_01]),
%~                         set_elts([call_func_args("mostcolor",[GRID_01])]))))
%~
%~ replacing_with_var( bin_op_left_right( sub_token(-),
%~                       call_func_args("palette",[GRID_01]),
%~                       set_elts([call_func_args("mostcolor",[GRID_01])])))
%~
%~ replacing_with_var(tuple_elts(["v",tuple_elts(["i","j"])]))
%~
%~ replacing_with_var( comprehension_target_iter(tuple_elts(["i","r"]),call_func_args("enumerate",[GRID_01])))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( tuple_elts(["j","v"]),
%~                       call_func_args("enumerate",["r"]),
%~                       [ compare_ops_left_comparators(eq_token(==),"v","value")]))
%~
%~ replacing_with_var(tuple_elts(["i","r"]))
%~
%~ replacing_with_var(call_func_args("enumerate",[GRID_01]))
%~
%~ replacing_with_var(tuple_elts(["j","v"]))
%~
%~ replacing_with_var(call_func_args("enumerate",["r"]))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),"v","value"))
%~
%~ replacing_with_var(call_func_args("mostcolor",[GRID_01]))
%~
% Compiled KL-1 for fgpartition
fgpartition(GRID_01,OBJECTS_02) :-
  willBeType(OBJECTS_02,'Objects') ,
  comment(' each cell with the same value part of the same object without background ') ,
  into_tuple(I,J,ARG_011) ,
  tuple_elts(V,ARG_011,ARG_010) ,
  into_tuple(I,R,ARG_014) ,
  enumerate(GRID_01,ARG_015) ,
  comprehension_target_iter(ARG_014,ARG_015,ARG_012) ,
  into_tuple(J,V,ARG_016) ,
  enumerate(R,ARG_017) ,
  compare_ops_left_comparators(eq_token(==),V,"value",ARG_018) ,
  comprehension_target_iter_ifs(ARG_016,ARG_017,[ARG_018],ARG_013) ,
  generator_exp_elt_generators(ARG_010,[ARG_012,ARG_013],ARG_05) ,
  frozenset(ARG_05,ARG_04) ,
  palette(GRID_01,ARG_08) ,
  mostcolor(GRID_01,ARG_019) ,
  set_elts([ARG_019],ARG_09) ,
  call([op_call(sub_token(-,ARG_08,ARG_09,ARG_07))]) ,
  comprehension_target_iter("value",ARG_07,ARG_06) ,
  generator_exp_elt_generators(ARG_04,[ARG_06],ARG_03) ,
  frozenset(ARG_03,OBJECTS_02) ,
  exit_proc(OBJECTS_02).
%~ % Universal AST Pass #0
%~ def( "uppermost",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' row index of uppermost occupied cell ')),
%~                          return_value( call_func_args( "min", [
%~                                          generator_exp_elt_generators( "i", [
%~                                            comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",["patch"]))])]))])))
%~
%~ replacing_with_var( comprehension_target_iter( tuple_elts(["i","j"]),
%~                       call_func_args("toindices",[PATCH_01])))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var(call_func_args("toindices",[PATCH_01]))
%~
% Compiled KL-1 for uppermost
uppermost(PATCH_01,INTEGER_02) :-
  willBeType(INTEGER_02,'Integer') ,
  comment(' row index of uppermost occupied cell ') ,
  into_tuple(I,J,ARG_05) ,
  toindices(PATCH_01,ARG_06) ,
  comprehension_target_iter(ARG_05,ARG_06,ARG_04) ,
  generator_exp_elt_generators(I,[ARG_04],ARG_03) ,
  min(ARG_03,INTEGER_02) ,
  exit_proc(INTEGER_02).
%~ % Universal AST Pass #0
%~ def( "lowermost",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' row index of lowermost occupied cell ')),
%~                          return_value( call_func_args( "max", [
%~                                          generator_exp_elt_generators( "i", [
%~                                            comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",["patch"]))])]))])))
%~
%~ replacing_with_var( comprehension_target_iter( tuple_elts(["i","j"]),
%~                       call_func_args("toindices",[PATCH_01])))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var(call_func_args("toindices",[PATCH_01]))
%~
% Compiled KL-1 for lowermost
lowermost(PATCH_01,INTEGER_02) :-
  willBeType(INTEGER_02,'Integer') ,
  comment(' row index of lowermost occupied cell ') ,
  into_tuple(I,J,ARG_05) ,
  toindices(PATCH_01,ARG_06) ,
  comprehension_target_iter(ARG_05,ARG_06,ARG_04) ,
  generator_exp_elt_generators(I,[ARG_04],ARG_03) ,
  max(ARG_03,INTEGER_02) ,
  exit_proc(INTEGER_02).
%~ % Universal AST Pass #0
%~ def( "leftmost",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' column index of leftmost occupied cell ')),
%~                          return_value( call_func_args( "min", [
%~                                          generator_exp_elt_generators( "j", [
%~                                            comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",["patch"]))])]))])))
%~
%~ replacing_with_var( comprehension_target_iter( tuple_elts(["i","j"]),
%~                       call_func_args("toindices",[PATCH_01])))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var(call_func_args("toindices",[PATCH_01]))
%~
% Compiled KL-1 for leftmost
leftmost(PATCH_01,INTEGER_02) :-
  willBeType(INTEGER_02,'Integer') ,
  comment(' column index of leftmost occupied cell ') ,
  into_tuple(I,J,ARG_05) ,
  toindices(PATCH_01,ARG_06) ,
  comprehension_target_iter(ARG_05,ARG_06,ARG_04) ,
  generator_exp_elt_generators(J,[ARG_04],ARG_03) ,
  min(ARG_03,INTEGER_02) ,
  exit_proc(INTEGER_02).
%~ % Universal AST Pass #0
%~ def( "rightmost",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' column index of rightmost occupied cell ')),
%~                          return_value( call_func_args( "max", [
%~                                          generator_exp_elt_generators( "j", [
%~                                            comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",["patch"]))])]))])))
%~
%~ replacing_with_var( comprehension_target_iter( tuple_elts(["i","j"]),
%~                       call_func_args("toindices",[PATCH_01])))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var(call_func_args("toindices",[PATCH_01]))
%~
% Compiled KL-1 for rightmost
rightmost(PATCH_01,INTEGER_02) :-
  willBeType(INTEGER_02,'Integer') ,
  comment(' column index of rightmost occupied cell ') ,
  into_tuple(I,J,ARG_05) ,
  toindices(PATCH_01,ARG_06) ,
  comprehension_target_iter(ARG_05,ARG_06,ARG_04) ,
  generator_exp_elt_generators(J,[ARG_04],ARG_03) ,
  max(ARG_03,INTEGER_02) ,
  exit_proc(INTEGER_02).
%~ % Universal AST Pass #0
%~ def( "square",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("piece")],[argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' whether the piece forms a square ')),
%~                          return_value( if_exp_test_body_orelse(
%~                                           call_func_args("isinstance",["piece","tuple"]),
%~                                           compare_ops_left_comparators( eq_token(==),
%~                                             call_func_args("len",["piece"]),
%~                                             call_func_args("len",[subscript_value_slice("piece",0)])),
%~                                           bool_op_values( ['python:And'], [
%~                                             compare_ops_left_comparators( eq_token(==),
%~                                               bin_op_left_right(mult_token(*),call_func_args("height",["piece"]),call_func_args("width",["piece"])),
%~                                               call_func_args("len",["piece"])),
%~                                             compare_ops_left_comparators( eq_token(==),
%~                                               call_func_args("height",["piece"]),
%~                                               call_func_args("width",["piece"]))])))])))
%~
%~ replacing_with_var(call_func_args("isinstance",[PIECE_01,"tuple"]))
%~
%~ replacing_with_var(call_func_args("len",[PIECE_01]))
%~
%~ replacing_with_var(call_func_args("len",[subscript_value_slice(PIECE_01,0)]))
%~
%~ replacing_with_var( compare_ops_left_comparators( eq_token(==),
%~                       bin_op_left_right( mult_token(*),
%~                         call_func_args("height",[PIECE_01]),
%~                         call_func_args("width",[PIECE_01])),
%~                       call_func_args("len",[PIECE_01])))
%~
%~ replacing_with_var( compare_ops_left_comparators( eq_token(==),
%~                       call_func_args("height",[PIECE_01]),
%~                       call_func_args("width",[PIECE_01])))
%~
%~ replacing_with_var( bin_op_left_right( mult_token(*),
%~                       call_func_args("height",[PIECE_01]),
%~                       call_func_args("width",[PIECE_01])))
%~
%~ replacing_with_var(call_func_args("len",[PIECE_01]))
%~
%~ replacing_with_var(call_func_args("height",[PIECE_01]))
%~
%~ replacing_with_var(call_func_args("width",[PIECE_01]))
%~
% Compiled KL-1 for square
square(PIECE_01,BOOLEAN_02) :-
  willBeType(BOOLEAN_02,'Boolean') ,
  comment(' whether the piece forms a square ') ,
  (/*2*/
    [ isinstance(PIECE_01,TUPLE,ARG_03),
      testif(ARG_03)] ->
      [ len(PIECE_01,ARG_04),
        subscript_value_slice(PIECE_01,0,ARG_06),
        len(ARG_06,ARG_05),
        compare_ops_left_comparators(eq_token(==),ARG_04,ARG_05,BOOLEAN_02)] ;
    [ height(PIECE_01,ARG_010),
      width(PIECE_01,ARG_011),
      call([op_call(mult_token(*,ARG_010,ARG_011,ARG_09))]),
      len(PIECE_01,ARG_012),
      compare_ops_left_comparators(eq_token(==),ARG_09,ARG_012,ARG_07),
      height(PIECE_01,ARG_013),
      width(PIECE_01,ARG_014),
      compare_ops_left_comparators(eq_token(==),ARG_013,ARG_014,ARG_08),
      bool_op_values(['python:And'],[ARG_07,ARG_08],BOOLEAN_02)]) ,
  exit_proc(BOOLEAN_02).
%~ % Universal AST Pass #0
%~ def( "vline",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' whether the piece forms a vertical line ')),
%~                          return_value( bool_op_values( ['python:And'], [
%~                                          compare_ops_left_comparators( eq_token(==),
%~                                            call_func_args("height",["patch"]),
%~                                            call_func_args("len",["patch"])),
%~                                          compare_ops_left_comparators(eq_token(==),call_func_args("width",["patch"]),1)]))])))
%~
%~ replacing_with_var( compare_ops_left_comparators( eq_token(==),
%~                       call_func_args("height",[PATCH_01]),
%~                       call_func_args("len",[PATCH_01])))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),call_func_args("width",[PATCH_01]),1))
%~
%~ replacing_with_var(call_func_args("height",[PATCH_01]))
%~
%~ replacing_with_var(call_func_args("len",[PATCH_01]))
%~
%~ replacing_with_var(call_func_args("width",[PATCH_01]))
%~
% Compiled KL-1 for vline
vline(PATCH_01,BOOLEAN_02) :-
  willBeType(BOOLEAN_02,'Boolean') ,
  comment(' whether the piece forms a vertical line ') ,
  height(PATCH_01,ARG_05) ,
  len(PATCH_01,ARG_06) ,
  compare_ops_left_comparators(eq_token(==),ARG_05,ARG_06,ARG_03) ,
  width(PATCH_01,ARG_07) ,
  compare_ops_left_comparators(eq_token(==),ARG_07,1,ARG_04) ,
  bool_op_values(['python:And'],[ARG_03,ARG_04],BOOLEAN_02) ,
  exit_proc(BOOLEAN_02).
%~ % Universal AST Pass #0
%~ def( "hline",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' whether the piece forms a horizontal line ')),
%~                          return_value( bool_op_values( ['python:And'], [
%~                                          compare_ops_left_comparators( eq_token(==),
%~                                            call_func_args("width",["patch"]),
%~                                            call_func_args("len",["patch"])),
%~                                          compare_ops_left_comparators(eq_token(==),call_func_args("height",["patch"]),1)]))])))
%~
%~ replacing_with_var( compare_ops_left_comparators( eq_token(==),
%~                       call_func_args("width",[PATCH_01]),
%~                       call_func_args("len",[PATCH_01])))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),call_func_args("height",[PATCH_01]),1))
%~
%~ replacing_with_var(call_func_args("width",[PATCH_01]))
%~
%~ replacing_with_var(call_func_args("len",[PATCH_01]))
%~
%~ replacing_with_var(call_func_args("height",[PATCH_01]))
%~
% Compiled KL-1 for hline
hline(PATCH_01,BOOLEAN_02) :-
  willBeType(BOOLEAN_02,'Boolean') ,
  comment(' whether the piece forms a horizontal line ') ,
  width(PATCH_01,ARG_05) ,
  len(PATCH_01,ARG_06) ,
  compare_ops_left_comparators(eq_token(==),ARG_05,ARG_06,ARG_03) ,
  height(PATCH_01,ARG_07) ,
  compare_ops_left_comparators(eq_token(==),ARG_07,1,ARG_04) ,
  bool_op_values(['python:And'],[ARG_03,ARG_04],BOOLEAN_02) ,
  exit_proc(BOOLEAN_02).
%~ % Universal AST Pass #0
%~ def( "hmatching",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' whether there exists a row for which both patches have cells ')),
%~                          return_value( compare_ops_left_comparators( gt_token(>),
%~                                          call_func_args( "len", [
%~                                            bin_op_left_right( bit_and_token(&),
%~                                              call_func_args( "set", [
%~                                                generator_exp_elt_generators( "i", [
%~                                                  comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",["a"]))])]),
%~                                              call_func_args( "set", [
%~                                                generator_exp_elt_generators( "i", [
%~                                                  comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",["b"]))])]))]),
%~                                          0))])))
%~
%~ replacing_with_var( call_func_args( "len", [
%~                       bin_op_left_right( bit_and_token(&),
%~                         call_func_args( "set", [
%~                           generator_exp_elt_generators( "i", [
%~                             comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",[A_01]))])]),
%~                         call_func_args( "set", [
%~                           generator_exp_elt_generators( "i", [
%~                             comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",[B_02]))])]))]))
%~
%~ replacing_with_var( comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",[A_01])))
%~
%~ replacing_with_var( comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",[B_02])))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var(call_func_args("toindices",[A_01]))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var(call_func_args("toindices",[B_02]))
%~
% Compiled KL-1 for hmatching
hmatching(A_01,B_02,BOOLEAN_03) :-
  willBeType(BOOLEAN_03,'Boolean') ,
  comment(' whether there exists a row for which both patches have cells ') ,
  into_tuple(I,J,ARG_012) ,
  toindices(A_01,ARG_013) ,
  comprehension_target_iter(ARG_012,ARG_013,ARG_010) ,
  generator_exp_elt_generators(I,[ARG_010],ARG_08) ,
  set(ARG_08,ARG_06) ,
  into_tuple(I,J,ARG_014) ,
  toindices(B_02,ARG_015) ,
  comprehension_target_iter(ARG_014,ARG_015,ARG_011) ,
  generator_exp_elt_generators(I,[ARG_011],ARG_09) ,
  set(ARG_09,ARG_07) ,
  call([op_call(bit_and_token(&,ARG_06,ARG_07,ARG_05))]) ,
  len(ARG_05,ARG_04) ,
  compare_ops_left_comparators(gt_token(>),ARG_04,0,BOOLEAN_03) ,
  exit_proc(BOOLEAN_03).
%~ % Universal AST Pass #0
%~ def( "vmatching",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' whether there exists a column for which both patches have cells ')),
%~                          return_value( compare_ops_left_comparators( gt_token(>),
%~                                          call_func_args( "len", [
%~                                            bin_op_left_right( bit_and_token(&),
%~                                              call_func_args( "set", [
%~                                                generator_exp_elt_generators( "j", [
%~                                                  comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",["a"]))])]),
%~                                              call_func_args( "set", [
%~                                                generator_exp_elt_generators( "j", [
%~                                                  comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",["b"]))])]))]),
%~                                          0))])))
%~
%~ replacing_with_var( call_func_args( "len", [
%~                       bin_op_left_right( bit_and_token(&),
%~                         call_func_args( "set", [
%~                           generator_exp_elt_generators( "j", [
%~                             comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",[A_01]))])]),
%~                         call_func_args( "set", [
%~                           generator_exp_elt_generators( "j", [
%~                             comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",[B_02]))])]))]))
%~
%~ replacing_with_var( comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",[A_01])))
%~
%~ replacing_with_var( comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("toindices",[B_02])))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var(call_func_args("toindices",[A_01]))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var(call_func_args("toindices",[B_02]))
%~
% Compiled KL-1 for vmatching
vmatching(A_01,B_02,BOOLEAN_03) :-
  willBeType(BOOLEAN_03,'Boolean') ,
  comment(' whether there exists a column for which both patches have cells ') ,
  into_tuple(I,J,ARG_012) ,
  toindices(A_01,ARG_013) ,
  comprehension_target_iter(ARG_012,ARG_013,ARG_010) ,
  generator_exp_elt_generators(J,[ARG_010],ARG_08) ,
  set(ARG_08,ARG_06) ,
  into_tuple(I,J,ARG_014) ,
  toindices(B_02,ARG_015) ,
  comprehension_target_iter(ARG_014,ARG_015,ARG_011) ,
  generator_exp_elt_generators(J,[ARG_011],ARG_09) ,
  set(ARG_09,ARG_07) ,
  call([op_call(bit_and_token(&,ARG_06,ARG_07,ARG_05))]) ,
  len(ARG_05,ARG_04) ,
  compare_ops_left_comparators(gt_token(>),ARG_04,0,BOOLEAN_03) ,
  exit_proc(BOOLEAN_03).
%~ % Universal AST Pass #0
%~ def( "manhattan",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' closest manhattan distance between two patches ')),
%~                          return_value( call_func_args( "min", [
%~                                          generator_exp_elt_generators(
%~                                             bin_op_left_right( add_token(+),
%~                                               call_func_args("abs",[bin_op_left_right(sub_token(-),"ai","bi")]),
%~                                               call_func_args("abs",[bin_op_left_right(sub_token(-),"aj","bj")])),
%~                                             [ comprehension_target_iter(tuple_elts(["ai","aj"]),call_func_args("toindices",["a"])),
%~                                               comprehension_target_iter(tuple_elts(["bi","bj"]),call_func_args("toindices",["b"]))])]))])))
%~
%~ replacing_with_var( bin_op_left_right( add_token(+),
%~                       call_func_args("abs",[bin_op_left_right(sub_token(-),"ai","bi")]),
%~                       call_func_args("abs",[bin_op_left_right(sub_token(-),"aj","bj")])))
%~
%~ replacing_with_var( comprehension_target_iter(tuple_elts(["ai","aj"]),call_func_args("toindices",[A_01])))
%~
%~ replacing_with_var( comprehension_target_iter(tuple_elts(["bi","bj"]),call_func_args("toindices",[B_02])))
%~
%~ replacing_with_var(tuple_elts(["ai","aj"]))
%~
%~ replacing_with_var(call_func_args("toindices",[A_01]))
%~
%~ replacing_with_var(tuple_elts(["bi","bj"]))
%~
%~ replacing_with_var(call_func_args("toindices",[B_02]))
%~
% Compiled KL-1 for manhattan
manhattan(A_01,B_02,INTEGER_03) :-
  willBeType(INTEGER_03,'Integer') ,
  comment(' closest manhattan distance between two patches ') ,
  call([op_call(sub_token(-,AI,BI,ARG_010))]) ,
  abs(ARG_010,ARG_06) ,
  call([op_call(sub_token(-,AJ,BJ,ARG_011))]) ,
  abs(ARG_011,ARG_07) ,
  call([op_call(add_token(+,ARG_06,ARG_07,ARG_05))]) ,
  into_tuple(AI,AJ,ARG_012) ,
  toindices(A_01,ARG_013) ,
  comprehension_target_iter(ARG_012,ARG_013,ARG_08) ,
  into_tuple(BI,BJ,ARG_014) ,
  toindices(B_02,ARG_015) ,
  comprehension_target_iter(ARG_014,ARG_015,ARG_09) ,
  generator_exp_elt_generators(ARG_05,[ARG_08,ARG_09],ARG_04) ,
  min(ARG_04,INTEGER_03) ,
  exit_proc(INTEGER_03).
%~ % Universal AST Pass #0
%~ def( "adjacent",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' whether two patches are adjacent ')),
%~                          return_value(compare_ops_left_comparators(eq_token(==),call_func_args("manhattan",["a","b"]),1))])))
%~
%~ replacing_with_var(call_func_args("manhattan",[A_01,B_02]))
%~
% Compiled KL-1 for adjacent
adjacent(A_01,B_02,BOOLEAN_03) :-
  willBeType(BOOLEAN_03,'Boolean') ,
  comment(' whether two patches are adjacent ') ,
  manhattan(A_01,B_02,ARG_04) ,
  compare_ops_left_comparators(eq_token(==),ARG_04,1,BOOLEAN_03) ,
  exit_proc(BOOLEAN_03).
%~ % Universal AST Pass #0
%~ def( "bordering",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("patch"),
%~           argument_name("grid")],
%~         [argument_type("Boolean")]),
%~      block_statements( [ expr_value(string_value(' whether a patch is adjacent to a grid border ')),
%~                          return_value( bool_op_values( ['python:Or'], [
%~                                          compare_ops_left_comparators(eq_token(==),call_func_args("uppermost",["patch"]),0),
%~                                          compare_ops_left_comparators(eq_token(==),call_func_args("leftmost",["patch"]),0),
%~                                          compare_ops_left_comparators( eq_token(==),
%~                                            call_func_args("lowermost",["patch"]),
%~                                            bin_op_left_right(sub_token(-),call_func_args("len",["grid"]),1)),
%~                                          compare_ops_left_comparators( eq_token(==),
%~                                            call_func_args("rightmost",["patch"]),
%~                                            bin_op_left_right(sub_token(-),call_func_args("len",[subscript_value_slice("grid",0)]),1))]))])))
%~
%~ replacing_with_var( compare_ops_left_comparators(eq_token(==),call_func_args("uppermost",[PATCH_01]),0))
%~
%~ replacing_with_var( compare_ops_left_comparators(eq_token(==),call_func_args("leftmost",[PATCH_01]),0))
%~
%~ replacing_with_var( compare_ops_left_comparators( eq_token(==),
%~                       call_func_args("lowermost",[PATCH_01]),
%~                       bin_op_left_right(sub_token(-),call_func_args("len",[GRID_02]),1)))
%~
%~ replacing_with_var( compare_ops_left_comparators( eq_token(==),
%~                       call_func_args("rightmost",[PATCH_01]),
%~                       bin_op_left_right(sub_token(-),call_func_args("len",[subscript_value_slice(GRID_02,0)]),1)))
%~
%~ replacing_with_var(call_func_args("uppermost",[PATCH_01]))
%~
%~ replacing_with_var(call_func_args("leftmost",[PATCH_01]))
%~
%~ replacing_with_var(call_func_args("lowermost",[PATCH_01]))
%~
%~ replacing_with_var(bin_op_left_right(sub_token(-),call_func_args("len",[GRID_02]),1))
%~
%~ replacing_with_var(call_func_args("rightmost",[PATCH_01]))
%~
%~ replacing_with_var( bin_op_left_right(sub_token(-),call_func_args("len",[subscript_value_slice(GRID_02,0)]),1))
%~
% Compiled KL-1 for bordering
bordering(PATCH_01,GRID_02,BOOLEAN_03) :-
  willBeType(BOOLEAN_03,'Boolean') ,
  comment(' whether a patch is adjacent to a grid border ') ,
  uppermost(PATCH_01,ARG_08) ,
  compare_ops_left_comparators(eq_token(==),ARG_08,0,ARG_04) ,
  leftmost(PATCH_01,ARG_09) ,
  compare_ops_left_comparators(eq_token(==),ARG_09,0,ARG_05) ,
  lowermost(PATCH_01,ARG_010) ,
  len(GRID_02,ARG_012) ,
  call([op_call(sub_token(-,ARG_012,1,ARG_011))]) ,
  compare_ops_left_comparators(eq_token(==),ARG_010,ARG_011,ARG_06) ,
  rightmost(PATCH_01,ARG_013) ,
  subscript_value_slice(GRID_02,0,ARG_016) ,
  len(ARG_016,ARG_015) ,
  call([op_call(sub_token(-,ARG_015,1,ARG_014))]) ,
  compare_ops_left_comparators(eq_token(==),ARG_013,ARG_014,ARG_07) ,
  bool_op_values( ['python:Or'],
    [ARG_04,ARG_05,ARG_06,ARG_07],
    BOOLEAN_03) ,
  exit_proc(BOOLEAN_03).
%~ % Universal AST Pass #0
%~ def( "centerofmass",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
%~      block_statements( [ expr_value(string_value(' center of mass ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          call_func_args( "map", [
%~                                            lambda_args_body( arguments_args([argument_name("x")]),
%~                                              body_stmts( bin_op_left_right(floor_div_token(//),call_func_args("sum",["x"]),call_func_args("len",["patch"])))),
%~                                            call_func_args("zip",[starred_value(call_func_args("toindices",["patch"]))])])]))])))
%~
%~ replacing_with_var(arguments_args([argument_name("x")]))
%~
%~ replacing_with_var( body_stmts( bin_op_left_right( floor_div_token(//),
%~                                   call_func_args("sum",["x"]),
%~                                   call_func_args("len",[PATCH_01]))))
%~
%~ replacing_with_var(argument_name("x"))
%~
%~ replacing_with_var( bin_op_left_right( floor_div_token(//),
%~                       call_func_args("sum",["x"]),
%~                       call_func_args("len",[PATCH_01])))
%~
% Compiled KL-1 for centerofmass
centerofmass(PATCH_01,INTEGERTUPLE_02) :-
  willBeType(INTEGERTUPLE_02,'IntegerTuple') ,
  comment(' center of mass ') ,
  argument_name(X,ARG_010) ,
  arguments_args([ARG_010],ARG_07) ,
  sum(X,ARG_012) ,
  len(PATCH_01,ARG_013) ,
  call([op_call(floor_div_token(//,ARG_012,ARG_013,ARG_011))]) ,
  body_stmts(ARG_011,ARG_08) ,
  lambda_args_body(ARG_07,ARG_08,ARG_04) ,
  toindices(PATCH_01,ARG_09) ,
  starred_value(ARG_09,ARG_06) ,
  zip(ARG_06,ARG_05) ,
  map(ARG_04,ARG_05,ARG_03) ,
  tuple(ARG_03,INTEGERTUPLE_02) ,
  exit_proc(INTEGERTUPLE_02).
%~ % Universal AST Pass #0
%~ def( "palette",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("element")],[argument_type("IntegerSet")]),
%~      block_statements( [ expr_value(string_value(' colors occurring in object or grid ')),
%~                          if_test_body(
%~                             call_func_args("isinstance",["element","tuple"]),
%~                             body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                           set_comp_elt_generators( "v", [
%~                                                             comprehension_target_iter("r","element"),
%~                                                             comprehension_target_iter("v","r")])]))])),
%~                          return_value( call_func_args( "frozenset", [
%~                                          set_comp_elt_generators("v",[comprehension_target_iter(tuple_elts(["v","_"]),"element")])]))])))
%~
%~ replacing_with_var( body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                   set_comp_elt_generators( "v", [
%~                                                     comprehension_target_iter("r",ELEMENT_01),
%~                                                     comprehension_target_iter("v","r")])]))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[ELEMENT_01,"tuple"]))
%~
%~ replacing_with_var( return_value( call_func_args( "frozenset", [
%~                                     set_comp_elt_generators( "v", [
%~                                       comprehension_target_iter("r",ELEMENT_01),
%~                                       comprehension_target_iter("v","r")])])))
%~
%~ replacing_with_var( call_func_args( "frozenset", [
%~                       set_comp_elt_generators( "v", [
%~                         comprehension_target_iter("r",ELEMENT_01),
%~                         comprehension_target_iter("v","r")])]))
%~
%~ replacing_with_var(tuple_elts(["v","_"]))
%~
%~ replacing_with_var(comprehension_target_iter("v","r"))
%~
% Compiled KL-1 for palette
palette(ELEMENT_01,INTEGERSET_02) :-
  willBeType(INTEGERSET_02,'IntegerSet') ,
  comment(' colors occurring in object or grid ') ,
  (/*2*/
    [ comprehension_target_iter(V,"r",ARG_010),
      set_comp_elt_generators( V,
        [ assign_targets_value1("r",ELEMENT_01),
          ARG_010],
        ARG_08),
      frozenset(ARG_08,ARG_07),
      return_value(ARG_07,ARG_06),
      body_stmts([ARG_06],ARG_03),
      [ isinstance(ELEMENT_01,TUPLE,ARG_04),
        testif(ARG_04)] ->
        ARG_03]) ,
  set_comp_elt_generators( V,
    [ into_tuple(V,"_",ARG_09),
      assign_targets_value1(ARG_09,ELEMENT_01)],
    ARG_05) ,
  frozenset(ARG_05,INTEGERSET_02) ,
  exit_proc(INTEGERSET_02).
%~ % Universal AST Pass #0
%~ def( "numcolors",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("element")],[argument_type("IntegerSet")]),
%~      block_statements( [ expr_value(string_value(' number of colors occurring in object or grid ')),
%~                          return_value(call_func_args("len",[call_func_args("palette",["element"])]))])))
%~
% Compiled KL-1 for numcolors
numcolors(ELEMENT_01,INTEGERSET_02) :-
  willBeType(INTEGERSET_02,'IntegerSet') ,
  comment(' number of colors occurring in object or grid ') ,
  palette(ELEMENT_01,ARG_03) ,
  len(ARG_03,INTEGERSET_02) ,
  exit_proc(INTEGERSET_02).
%~ % Universal AST Pass #0
%~ def( "color",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("obj")],[argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' color of object ')),
%~                          return_value(subscript_value_slice(call_func_args("next",[call_func_args("iter",["obj"])]),0))])))
%~
% Compiled KL-1 for color
color(OBJ_01,INTEGER_02) :-
  willBeType(INTEGER_02,'Integer') ,
  comment(' color of object ') ,
  iter(OBJ_01,ARG_04) ,
  next(ARG_04,ARG_03) ,
  subscript_value_slice(ARG_03,0,INTEGER_02) ,
  exit_proc(INTEGER_02).
%~ % Universal AST Pass #0
%~ def( "toobject",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("patch"),
%~           argument_name("grid")],
%~         [argument_type("Object")]),
%~      block_statements( [ expr_value(string_value(' object from patch and grid ')),
%~                          assign_targets_value( [tuple_elts(["h","w"])],
%~                            tuple_elts( [ call_func_args("len",["grid"]),
%~                                          call_func_args("len",[subscript_value_slice("grid",0)])])),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators(
%~                                             tuple_elts( [ subscript_value_slice(subscript_value_slice("grid","i"),"j"),
%~                                                           tuple_elts(["i","j"])]),
%~                                             [ comprehension_target_iter_ifs( tuple_elts(["i","j"]),
%~                                                 call_func_args("toindices",["patch"]),
%~                                                 [ bool_op_values( ['python:And'], [
%~                                                     compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                                     compare_ops_left_comparators(
%~                                                        ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])])])]))])))
%~
%~ replacing_with_var( tuple_elts( [ subscript_value_slice(subscript_value_slice(GRID_02,"i"),"j"),
%~                                   tuple_elts(["i","j"])]))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( tuple_elts(["i","j"]),
%~                       call_func_args("toindices",[PATCH_01]),
%~                       [ bool_op_values( ['python:And'], [
%~                           compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                           compare_ops_left_comparators(
%~                              ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])]))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var(call_func_args("toindices",[PATCH_01]))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["j","w"])))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["i","h"]))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["j","w"]))
%~
% Compiled KL-1 for toobject
toobject(PATCH_01,GRID_02,OBJECT_03) :-
  willBeType(OBJECT_03,'Object') ,
  comment(' object from patch and grid ') ,
  len(GRID_02,H) ,
  subscript_value_slice(GRID_02,0,ARG_04) ,
  len(ARG_04,W) ,
  subscript_value_slice(GRID_02,I,ARG_010) ,
  subscript_value_slice(ARG_010,J,ARG_07) ,
  into_tuple(I,J,ARG_08) ,
  tuple_elts(ARG_07,ARG_08,ARG_06) ,
  into_tuple(I,J,ARG_011) ,
  toindices(PATCH_01,ARG_012) ,
  comprehension_target_iter_ifs( ARG_011,
    ARG_012,
    [ ops([lt_e_token(<=),lt_token(<)],ARG_015),
      comparators([I,H],ARG_016),
      compare_ops_left_comparators(ARG_015,0,ARG_016,ARG_013),
      ops([lt_e_token(<=),lt_token(<)],ARG_017),
      comparators([J,W],ARG_018),
      compare_ops_left_comparators(ARG_017,0,ARG_018,ARG_014),
      bool_op_values(['python:And'],[ARG_013,ARG_014])],
    ARG_09) ,
  generator_exp_elt_generators(ARG_06,[ARG_09],ARG_05) ,
  frozenset(ARG_05,OBJECT_03) ,
  exit_proc(OBJECT_03).
%~ % Universal AST Pass #0
%~ def( "asobject",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("grid")],[argument_type("Object")]),
%~      block_statements( [ expr_value(string_value(' conversion of grid to object ')),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators(
%~                                             tuple_elts(["v",tuple_elts(["i","j"])]),
%~                                             [ comprehension_target_iter(tuple_elts(["i","r"]),call_func_args("enumerate",["grid"])),
%~                                               comprehension_target_iter(tuple_elts(["j","v"]),call_func_args("enumerate",["r"]))])]))])))
%~
%~ replacing_with_var(tuple_elts(["v",tuple_elts(["i","j"])]))
%~
%~ replacing_with_var( comprehension_target_iter(tuple_elts(["i","r"]),call_func_args("enumerate",[GRID_01])))
%~
%~ replacing_with_var( comprehension_target_iter(tuple_elts(["j","v"]),call_func_args("enumerate",["r"])))
%~
%~ replacing_with_var(tuple_elts(["i","r"]))
%~
%~ replacing_with_var(call_func_args("enumerate",[GRID_01]))
%~
%~ replacing_with_var(tuple_elts(["j","v"]))
%~
%~ replacing_with_var(call_func_args("enumerate",["r"]))
%~
% Compiled KL-1 for asobject
asobject(GRID_01,OBJECT_02) :-
  willBeType(OBJECT_02,'Object') ,
  comment(' conversion of grid to object ') ,
  into_tuple(I,J,ARG_05) ,
  tuple_elts(V,ARG_05,ARG_04) ,
  into_tuple(I,R,ARG_08) ,
  enumerate(GRID_01,ARG_09) ,
  comprehension_target_iter(ARG_08,ARG_09,ARG_06) ,
  into_tuple(J,V,ARG_010) ,
  enumerate(R,ARG_011) ,
  comprehension_target_iter(ARG_010,ARG_011,ARG_07) ,
  generator_exp_elt_generators(ARG_04,[ARG_06,ARG_07],ARG_03) ,
  frozenset(ARG_03,OBJECT_02) ,
  exit_proc(OBJECT_02).
%~ % Universal AST Pass #0
%~ def( "rot90",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' quarter clockwise rotation ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators( "row", [
%~                                            comprehension_target_iter( "row",
%~                                              call_func_args( "zip", [
%~                                                starred_value(subscript_value_slice("grid",slice_step(unary_op_operand(us_ub_token(-),1))))]))])]))])))
%~
%~ replacing_with_var( comprehension_target_iter( "row",
%~                       call_func_args( "zip", [
%~                         starred_value(subscript_value_slice(GRID_01,slice_step(unary_op_operand(us_ub_token(-),1))))])))
%~
%~ replacing_with_var( call_func_args( "zip", [
%~                       starred_value(subscript_value_slice(GRID_01,slice_step(unary_op_operand(us_ub_token(-),1))))]))
%~
%~ replacing_with_var(unary_op_operand(us_ub_token(-),1))
%~
% Compiled KL-1 for rot90
rot90(GRID_01,GRID_02) :-
  willBeType(GRID_02,'Grid') ,
  comment(' quarter clockwise rotation ') ,
  unary_op_operand(us_ub_token(-),1,ARG_09) ,
  slice_step(ARG_09,ARG_08) ,
  subscript_value_slice(GRID_01,ARG_08,ARG_07) ,
  starred_value(ARG_07,ARG_06) ,
  zip(ARG_06,ARG_05) ,
  comprehension_target_iter("row",ARG_05,ARG_04) ,
  generator_exp_elt_generators("row",[ARG_04],ARG_03) ,
  tuple(ARG_03,GRID_02) ,
  exit_proc(GRID_02).
%~ % Universal AST Pass #0
%~ def( "rot180",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' half rotation ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators(
%~                                             call_func_args("tuple",[subscript_value_slice("row",slice_step(unary_op_operand(us_ub_token(-),1)))]),
%~                                             [ comprehension_target_iter("row",subscript_value_slice("grid",slice_step(unary_op_operand(us_ub_token(-),1))))])]))])))
%~
%~ replacing_with_var( call_func_args("tuple",[subscript_value_slice("row",slice_step(unary_op_operand(us_ub_token(-),1)))]))
%~
%~ replacing_with_var( comprehension_target_iter( "row",
%~                       subscript_value_slice(GRID_01,slice_step(unary_op_operand(us_ub_token(-),1)))))
%~
%~ replacing_with_var(subscript_value_slice(GRID_01,slice_step(unary_op_operand(us_ub_token(-),1))))
%~
%~ replacing_with_var(unary_op_operand(us_ub_token(-),1))
%~
%~ replacing_with_var(unary_op_operand(us_ub_token(-),1))
%~
% Compiled KL-1 for rot180
rot180(GRID_01,GRID_02) :-
  willBeType(GRID_02,'Grid') ,
  comment(' half rotation ') ,
  unary_op_operand(us_ub_token(-),1,ARG_010) ,
  slice_step(ARG_010,ARG_07) ,
  subscript_value_slice(ROW,ARG_07,ARG_05) ,
  tuple(ARG_05,ARG_04) ,
  unary_op_operand(us_ub_token(-),1,ARG_011) ,
  slice_step(ARG_011,ARG_09) ,
  subscript_value_slice(GRID_01,ARG_09,ARG_08) ,
  comprehension_target_iter(ROW,ARG_08,ARG_06) ,
  generator_exp_elt_generators(ARG_04,[ARG_06],ARG_03) ,
  tuple(ARG_03,GRID_02) ,
  exit_proc(GRID_02).
%~ % Universal AST Pass #0
%~ def( "rot270",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' quarter anticlockwise rotation ')),
%~                          return_value( subscript_value_slice(
%~                                           call_func_args( "tuple", [
%~                                             generator_exp_elt_generators(
%~                                                call_func_args("tuple",[subscript_value_slice("row",slice_step(unary_op_operand(us_ub_token(-),1)))]),
%~                                                [ comprehension_target_iter( "row",
%~                                                    call_func_args( "zip", [
%~                                                      starred_value(subscript_value_slice("grid",slice_step(unary_op_operand(us_ub_token(-),1))))]))])]),
%~                                           slice_step(unary_op_operand(us_ub_token(-),1))))])))
%~
%~ replacing_with_var(unary_op_operand(us_ub_token(-),1))
%~
%~ replacing_with_var( call_func_args("tuple",[subscript_value_slice("row",slice_step(unary_op_operand(us_ub_token(-),1)))]))
%~
%~ replacing_with_var( comprehension_target_iter( "row",
%~                       call_func_args( "zip", [
%~                         starred_value(subscript_value_slice(GRID_01,slice_step(unary_op_operand(us_ub_token(-),1))))])))
%~
%~ replacing_with_var( call_func_args( "zip", [
%~                       starred_value(subscript_value_slice(GRID_01,slice_step(unary_op_operand(us_ub_token(-),1))))]))
%~
%~ replacing_with_var(unary_op_operand(us_ub_token(-),1))
%~
%~ replacing_with_var(unary_op_operand(us_ub_token(-),1))
%~
% Compiled KL-1 for rot270
rot270(GRID_01,GRID_02) :-
  willBeType(GRID_02,'Grid') ,
  comment(' quarter anticlockwise rotation ') ,
  unary_op_operand(us_ub_token(-),1,ARG_014) ,
  slice_step(ARG_014,ARG_010) ,
  subscript_value_slice(ROW,ARG_010,ARG_08) ,
  tuple(ARG_08,ARG_07) ,
  unary_op_operand(us_ub_token(-),1,ARG_016) ,
  slice_step(ARG_016,ARG_015) ,
  subscript_value_slice(GRID_01,ARG_015,ARG_013) ,
  starred_value(ARG_013,ARG_012) ,
  zip(ARG_012,ARG_011) ,
  comprehension_target_iter(ROW,ARG_011,ARG_09) ,
  generator_exp_elt_generators(ARG_07,[ARG_09],ARG_05) ,
  tuple(ARG_05,ARG_03) ,
  unary_op_operand(us_ub_token(-),1,ARG_06) ,
  slice_step(ARG_06,ARG_04) ,
  subscript_value_slice(ARG_03,ARG_04,GRID_02) ,
  exit_proc(GRID_02).
%~ % Universal AST Pass #0
%~ def( "hmirror",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("piece")],[argument_type("Piece")]),
%~      block_statements( [ expr_value(string_value(' mirroring along horizontal ')),
%~                          if_test_body(
%~                             call_func_args("isinstance",["piece","tuple"]),
%~                             body_stmts([return_value(subscript_value_slice("piece",slice_step(unary_op_operand(us_ub_token(-),1))))])),
%~                          assign_targets_value( ["d"],
%~                            bin_op_left_right( add_token(+),
%~                              subscript_value_slice(call_func_args("ulcorner",["piece"]),0),
%~                              subscript_value_slice(call_func_args("lrcorner",["piece"]),0))),
%~                          if_test_body(
%~                             call_func_args( "isinstance", [
%~                               subscript_value_slice(call_func_args("next",[call_func_args("iter",["piece"])]),1),
%~                               "tuple"]),
%~                             body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                           generator_exp_elt_generators(
%~                                                              tuple_elts(["v",tuple_elts([bin_op_left_right(sub_token(-),"d","i"),"j"])]),
%~                                                              [ comprehension_target_iter(tuple_elts(["v",tuple_elts(["i","j"])]),"piece")])]))])),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators(
%~                                             tuple_elts([bin_op_left_right(sub_token(-),"d","i"),"j"]),
%~                                             [ comprehension_target_iter(tuple_elts(["i","j"]),"piece")])]))])))
%~
%~ replacing_with_var( body_stmts( [ return_value(subscript_value_slice(PIECE_01,slice_step(unary_op_operand(us_ub_token(-),1))))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[PIECE_01,"tuple"]))
%~
%~ replacing_with_var( body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                   generator_exp_elt_generators(
%~                                                      tuple_elts(["v",tuple_elts([bin_op_left_right(sub_token(-),"d","i"),"j"])]),
%~                                                      [ comprehension_target_iter(tuple_elts(["v",tuple_elts(["i","j"])]),PIECE_01)])]))]))
%~
%~ replacing_with_var( call_func_args( "isinstance", [
%~                       subscript_value_slice(call_func_args("next",[call_func_args("iter",[PIECE_01])]),1),
%~                       "tuple"]))
%~
%~ replacing_with_var( return_value(subscript_value_slice(PIECE_01,slice_step(unary_op_operand(us_ub_token(-),1)))))
%~
%~ replacing_with_var( return_value( call_func_args( "frozenset", [
%~                                     generator_exp_elt_generators(
%~                                        tuple_elts(["v",tuple_elts([bin_op_left_right(sub_token(-),"d","i"),"j"])]),
%~                                        [ comprehension_target_iter(tuple_elts(["v",tuple_elts(["i","j"])]),PIECE_01)])])))
%~
%~ replacing_with_var(subscript_value_slice(PIECE_01,slice_step(unary_op_operand(us_ub_token(-),1))))
%~
%~ replacing_with_var( call_func_args( "frozenset", [
%~                       generator_exp_elt_generators(
%~                          tuple_elts(["v",tuple_elts([bin_op_left_right(sub_token(-),"d","i"),"j"])]),
%~                          [ comprehension_target_iter(tuple_elts(["v",tuple_elts(["i","j"])]),PIECE_01)])]))
%~
%~ replacing_with_var(tuple_elts([bin_op_left_right(sub_token(-),"d","i"),"j"]))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var(unary_op_operand(us_ub_token(-),1))
%~
%~ replacing_with_var( tuple_elts(["v",tuple_elts([bin_op_left_right(sub_token(-),"d","i"),"j"])]))
%~
%~ replacing_with_var(tuple_elts(["v",tuple_elts(["i","j"])]))
%~
% Compiled KL-1 for hmirror
hmirror(PIECE_01,PIECE_02) :-
  willBeType(PIECE_02,'Piece') ,
  comment(' mirroring along horizontal ') ,
  (/*2*/
    [ unary_op_operand(us_ub_token(-),1,ARG_024),
      slice_step(ARG_024,ARG_017),
      subscript_value_slice(PIECE_01,ARG_017,ARG_016),
      return_value(ARG_016,ARG_011),
      body_stmts([ARG_011],ARG_03),
      [ isinstance(PIECE_01,TUPLE,ARG_04),
        testif(ARG_04)] ->
        ARG_03]) ,
  ulcorner(PIECE_01,ARG_012) ,
  subscript_value_slice(ARG_012,0,ARG_05) ,
  lrcorner(PIECE_01,ARG_013) ,
  subscript_value_slice(ARG_013,0,ARG_06) ,
  call([op_call(add_token(+,ARG_05,ARG_06,D))]) ,
  (/*2*/
    [ call([op_call(sub_token(-,D,I,ARG_027))]),
      tuple_elts(ARG_027,J,ARG_026),
      tuple_elts(V,ARG_026,ARG_025),
      generator_exp_elt_generators( ARG_025,
        [ into_tuple(I,J,ARG_029),
          tuple_elts(V,ARG_029,ARG_028),
          assign_targets_value1(ARG_028,PIECE_01)],
        ARG_019),
      frozenset(ARG_019,ARG_018),
      return_value(ARG_018,ARG_014),
      body_stmts([ARG_014],ARG_07),
      [ iter(PIECE_01,ARG_020),
        next(ARG_020,ARG_015),
        subscript_value_slice(ARG_015,1,ARG_09),
        isinstance(ARG_09,TUPLE,ARG_08),
        testif(ARG_08)] ->
        ARG_07]) ,
  call([op_call(sub_token(-,D,I,ARG_022))]) ,
  tuple_elts(ARG_022,J,ARG_021) ,
  generator_exp_elt_generators( ARG_021,
    [ into_tuple(I,J,ARG_023),
      assign_targets_value1(ARG_023,PIECE_01)],
    ARG_010) ,
  frozenset(ARG_010,PIECE_02) ,
  exit_proc(PIECE_02).
%~ % Universal AST Pass #0
%~ def( "vmirror",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("piece")],[argument_type("Piece")]),
%~      block_statements( [ expr_value(string_value(' mirroring along vertical ')),
%~                          if_test_body(
%~                             call_func_args("isinstance",["piece","tuple"]),
%~                             body_stmts( [ return_value( call_func_args( "tuple", [
%~                                                           generator_exp_elt_generators(
%~                                                              subscript_value_slice("row",slice_step(unary_op_operand(us_ub_token(-),1))),
%~                                                              [comprehension_target_iter("row","piece")])]))])),
%~                          assign_targets_value( ["d"],
%~                            bin_op_left_right( add_token(+),
%~                              subscript_value_slice(call_func_args("ulcorner",["piece"]),1),
%~                              subscript_value_slice(call_func_args("lrcorner",["piece"]),1))),
%~                          if_test_body(
%~                             call_func_args( "isinstance", [
%~                               subscript_value_slice(call_func_args("next",[call_func_args("iter",["piece"])]),1),
%~                               "tuple"]),
%~                             body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                           generator_exp_elt_generators(
%~                                                              tuple_elts(["v",tuple_elts(["i",bin_op_left_right(sub_token(-),"d","j")])]),
%~                                                              [ comprehension_target_iter(tuple_elts(["v",tuple_elts(["i","j"])]),"piece")])]))])),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators(
%~                                             tuple_elts(["i",bin_op_left_right(sub_token(-),"d","j")]),
%~                                             [ comprehension_target_iter(tuple_elts(["i","j"]),"piece")])]))])))
%~
%~ replacing_with_var( body_stmts( [ return_value( call_func_args( "tuple", [
%~                                                   generator_exp_elt_generators(
%~                                                      subscript_value_slice("row",slice_step(unary_op_operand(us_ub_token(-),1))),
%~                                                      [comprehension_target_iter("row",PIECE_01)])]))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[PIECE_01,"tuple"]))
%~
%~ replacing_with_var( body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                   generator_exp_elt_generators(
%~                                                      tuple_elts(["v",tuple_elts(["i",bin_op_left_right(sub_token(-),"d","j")])]),
%~                                                      [ comprehension_target_iter(tuple_elts(["v",tuple_elts(["i","j"])]),PIECE_01)])]))]))
%~
%~ replacing_with_var( call_func_args( "isinstance", [
%~                       subscript_value_slice(call_func_args("next",[call_func_args("iter",[PIECE_01])]),1),
%~                       "tuple"]))
%~
%~ replacing_with_var( return_value( call_func_args( "tuple", [
%~                                     generator_exp_elt_generators(
%~                                        subscript_value_slice("row",slice_step(unary_op_operand(us_ub_token(-),1))),
%~                                        [comprehension_target_iter("row",PIECE_01)])])))
%~
%~ replacing_with_var( return_value( call_func_args( "frozenset", [
%~                                     generator_exp_elt_generators(
%~                                        tuple_elts(["v",tuple_elts(["i",bin_op_left_right(sub_token(-),"d","j")])]),
%~                                        [ comprehension_target_iter(tuple_elts(["v",tuple_elts(["i","j"])]),PIECE_01)])])))
%~
%~ replacing_with_var( call_func_args( "tuple", [
%~                       generator_exp_elt_generators(
%~                          subscript_value_slice("row",slice_step(unary_op_operand(us_ub_token(-),1))),
%~                          [comprehension_target_iter("row",PIECE_01)])]))
%~
%~ replacing_with_var( call_func_args( "frozenset", [
%~                       generator_exp_elt_generators(
%~                          tuple_elts(["v",tuple_elts(["i",bin_op_left_right(sub_token(-),"d","j")])]),
%~                          [ comprehension_target_iter(tuple_elts(["v",tuple_elts(["i","j"])]),PIECE_01)])]))
%~
%~ replacing_with_var(tuple_elts(["i",bin_op_left_right(sub_token(-),"d","j")]))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var(subscript_value_slice("row",slice_step(unary_op_operand(us_ub_token(-),1))))
%~
%~ replacing_with_var( tuple_elts(["v",tuple_elts(["i",bin_op_left_right(sub_token(-),"d","j")])]))
%~
%~ replacing_with_var(tuple_elts(["v",tuple_elts(["i","j"])]))
%~
%~ replacing_with_var(unary_op_operand(us_ub_token(-),1))
%~
% Compiled KL-1 for vmirror
vmirror(PIECE_01,PIECE_02) :-
  willBeType(PIECE_02,'Piece') ,
  comment(' mirroring along vertical ') ,
  (/*2*/
    [ unary_op_operand(us_ub_token(-),1,ARG_031),
      slice_step(ARG_031,ARG_025),
      subscript_value_slice(ROW,ARG_025,ARG_024),
      generator_exp_elt_generators( ARG_024,
        [assign_targets_value1(ROW,PIECE_01)],
        ARG_017),
      tuple(ARG_017,ARG_016),
      return_value(ARG_016,ARG_011),
      body_stmts([ARG_011],ARG_03),
      [ isinstance(PIECE_01,TUPLE,ARG_04),
        testif(ARG_04)] ->
        ARG_03]) ,
  ulcorner(PIECE_01,ARG_012) ,
  subscript_value_slice(ARG_012,1,ARG_05) ,
  lrcorner(PIECE_01,ARG_013) ,
  subscript_value_slice(ARG_013,1,ARG_06) ,
  call([op_call(add_token(+,ARG_05,ARG_06,D))]) ,
  (/*2*/
    [ call([op_call(sub_token(-,D,J,ARG_028))]),
      tuple_elts(I,ARG_028,ARG_027),
      tuple_elts(V,ARG_027,ARG_026),
      generator_exp_elt_generators( ARG_026,
        [ into_tuple(I,J,ARG_030),
          tuple_elts(V,ARG_030,ARG_029),
          assign_targets_value1(ARG_029,PIECE_01)],
        ARG_019),
      frozenset(ARG_019,ARG_018),
      return_value(ARG_018,ARG_014),
      body_stmts([ARG_014],ARG_07),
      [ iter(PIECE_01,ARG_020),
        next(ARG_020,ARG_015),
        subscript_value_slice(ARG_015,1,ARG_09),
        isinstance(ARG_09,TUPLE,ARG_08),
        testif(ARG_08)] ->
        ARG_07]) ,
  call([op_call(sub_token(-,D,J,ARG_022))]) ,
  tuple_elts(I,ARG_022,ARG_021) ,
  generator_exp_elt_generators( ARG_021,
    [ into_tuple(I,J,ARG_023),
      assign_targets_value1(ARG_023,PIECE_01)],
    ARG_010) ,
  frozenset(ARG_010,PIECE_02) ,
  exit_proc(PIECE_02).
%~ % Universal AST Pass #0
%~ def( "dmirror",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("piece")],[argument_type("Piece")]),
%~      block_statements( [ expr_value(string_value(' mirroring along diagonal ')),
%~                          if_test_body(
%~                             call_func_args("isinstance",["piece","tuple"]),
%~                             body_stmts( [ return_value(call_func_args("tuple",[call_func_args("zip",[starred_value("piece")])]))])),
%~                          assign_targets_value([tuple_elts(["a","b"])],call_func_args("ulcorner",["piece"])),
%~                          if_test_body(
%~                             call_func_args( "isinstance", [
%~                               subscript_value_slice(call_func_args("next",[call_func_args("iter",["piece"])]),1),
%~                               "tuple"]),
%~                             body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                           generator_exp_elt_generators(
%~                                                              tuple_elts( [ "v",
%~                                                                            tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"j","b"),"a"),
%~                                                                                          bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"i","a"),"b")])]),
%~                                                              [ comprehension_target_iter(tuple_elts(["v",tuple_elts(["i","j"])]),"piece")])]))])),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators(
%~                                             tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"j","b"),"a"),
%~                                                           bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"i","a"),"b")]),
%~                                             [ comprehension_target_iter(tuple_elts(["i","j"]),"piece")])]))])))
%~
%~ replacing_with_var( body_stmts( [ return_value(call_func_args("tuple",[call_func_args("zip",[starred_value(PIECE_01)])]))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[PIECE_01,"tuple"]))
%~
%~ replacing_with_var( body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                   generator_exp_elt_generators(
%~                                                      tuple_elts( [ "v",
%~                                                                    tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"j","b"),"a"),
%~                                                                                  bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"i","a"),"b")])]),
%~                                                      [ comprehension_target_iter(tuple_elts(["v",tuple_elts(["i","j"])]),PIECE_01)])]))]))
%~
%~ replacing_with_var( call_func_args( "isinstance", [
%~                       subscript_value_slice(call_func_args("next",[call_func_args("iter",[PIECE_01])]),1),
%~                       "tuple"]))
%~
%~ replacing_with_var( return_value(call_func_args("tuple",[call_func_args("zip",[starred_value(PIECE_01)])])))
%~
%~ replacing_with_var( return_value( call_func_args( "frozenset", [
%~                                     generator_exp_elt_generators(
%~                                        tuple_elts( [ "v",
%~                                                      tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"j","b"),"a"),
%~                                                                    bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"i","a"),"b")])]),
%~                                        [ comprehension_target_iter(tuple_elts(["v",tuple_elts(["i","j"])]),PIECE_01)])])))
%~
%~ replacing_with_var(call_func_args("tuple",[call_func_args("zip",[starred_value(PIECE_01)])]))
%~
%~ replacing_with_var( call_func_args( "frozenset", [
%~                       generator_exp_elt_generators(
%~                          tuple_elts( [ "v",
%~                                        tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"j","b"),"a"),
%~                                                      bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"i","a"),"b")])]),
%~                          [ comprehension_target_iter(tuple_elts(["v",tuple_elts(["i","j"])]),PIECE_01)])]))
%~
%~ replacing_with_var( tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"j","b"),"a"),
%~                                   bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"i","a"),"b")]))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var( tuple_elts( [ "v",
%~                                   tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"j","b"),"a"),
%~                                                 bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"i","a"),"b")])]))
%~
%~ replacing_with_var(tuple_elts(["v",tuple_elts(["i","j"])]))
%~
% Compiled KL-1 for dmirror
dmirror(PIECE_01,PIECE_02) :-
  willBeType(PIECE_02,'Piece') ,
  comment(' mirroring along diagonal ') ,
  (/*2*/
    [ starred_value(PIECE_01,ARG_021),
      zip(ARG_021,ARG_014),
      tuple(ARG_014,ARG_013),
      return_value(ARG_013,ARG_010),
      body_stmts([ARG_010],ARG_03),
      [ isinstance(PIECE_01,TUPLE,ARG_04),
        testif(ARG_04)] ->
        ARG_03]) ,
  into_tuple(A,B,ARG_05) ,
  ulcorner(PIECE_01,ARG_05) ,
  (/*2*/
    [ call([op_call(sub_token(-,J,B,ARG_031))]),
      call([op_call(add_token(+,ARG_031,A,ARG_027))]),
      call([op_call(sub_token(-,I,A,ARG_032))]),
      call([op_call(add_token(+,ARG_032,B,ARG_028))]),
      tuple_elts(ARG_027,ARG_028,ARG_026),
      tuple_elts(V,ARG_026,ARG_025),
      generator_exp_elt_generators( ARG_025,
        [ into_tuple(I,J,ARG_030),
          tuple_elts(V,ARG_030,ARG_029),
          assign_targets_value1(ARG_029,PIECE_01)],
        ARG_016),
      frozenset(ARG_016,ARG_015),
      return_value(ARG_015,ARG_011),
      body_stmts([ARG_011],ARG_06),
      [ iter(PIECE_01,ARG_017),
        next(ARG_017,ARG_012),
        subscript_value_slice(ARG_012,1,ARG_08),
        isinstance(ARG_08,TUPLE,ARG_07),
        testif(ARG_07)] ->
        ARG_06]) ,
  call([op_call(sub_token(-,J,B,ARG_022))]) ,
  call([op_call(add_token(+,ARG_022,A,ARG_019))]) ,
  call([op_call(sub_token(-,I,A,ARG_023))]) ,
  call([op_call(add_token(+,ARG_023,B,ARG_020))]) ,
  tuple_elts(ARG_019,ARG_020,ARG_018) ,
  generator_exp_elt_generators( ARG_018,
    [ into_tuple(I,J,ARG_024),
      assign_targets_value1(ARG_024,PIECE_01)],
    ARG_09) ,
  frozenset(ARG_09,PIECE_02) ,
  exit_proc(PIECE_02).
%~ % Universal AST Pass #0
%~ def( "cmirror",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("piece")],[argument_type("Piece")]),
%~      block_statements( [ expr_value(string_value(' mirroring along counterdiagonal ')),
%~                          if_test_body(
%~                             call_func_args("isinstance",["piece","tuple"]),
%~                             body_stmts( [ return_value( call_func_args( "tuple", [
%~                                                           call_func_args( "zip", [
%~                                                             starred_value( generator_exp_elt_generators(
%~                                                                               subscript_value_slice("r",slice_step(unary_op_operand(us_ub_token(-),1))),
%~                                                                               [ comprehension_target_iter("r",subscript_value_slice("piece",slice_step(unary_op_operand(us_ub_token(-),1))))]))])]))])),
%~                          return_value( call_func_args("vmirror",[call_func_args("dmirror",[call_func_args("vmirror",["piece"])])]))])))
%~
%~ replacing_with_var( body_stmts( [ return_value( call_func_args( "tuple", [
%~                                                   call_func_args( "zip", [
%~                                                     starred_value( generator_exp_elt_generators(
%~                                                                       subscript_value_slice("r",slice_step(unary_op_operand(us_ub_token(-),1))),
%~                                                                       [ comprehension_target_iter("r",subscript_value_slice(PIECE_01,slice_step(unary_op_operand(us_ub_token(-),1))))]))])]))]))
%~
%~ replacing_with_var(call_func_args("isinstance",[PIECE_01,"tuple"]))
%~
%~ replacing_with_var( return_value( call_func_args( "tuple", [
%~                                     call_func_args( "zip", [
%~                                       starred_value( generator_exp_elt_generators(
%~                                                         subscript_value_slice("r",slice_step(unary_op_operand(us_ub_token(-),1))),
%~                                                         [ comprehension_target_iter("r",subscript_value_slice(PIECE_01,slice_step(unary_op_operand(us_ub_token(-),1))))]))])])))
%~
%~ replacing_with_var( call_func_args( "tuple", [
%~                       call_func_args( "zip", [
%~                         starred_value( generator_exp_elt_generators(
%~                                           subscript_value_slice("r",slice_step(unary_op_operand(us_ub_token(-),1))),
%~                                           [ comprehension_target_iter("r",subscript_value_slice(PIECE_01,slice_step(unary_op_operand(us_ub_token(-),1))))]))])]))
%~
%~ replacing_with_var(subscript_value_slice("r",slice_step(unary_op_operand(us_ub_token(-),1))))
%~
%~ replacing_with_var( comprehension_target_iter("r",subscript_value_slice(PIECE_01,slice_step(unary_op_operand(us_ub_token(-),1)))))
%~
%~ replacing_with_var(subscript_value_slice(PIECE_01,slice_step(unary_op_operand(us_ub_token(-),1))))
%~
%~ replacing_with_var(unary_op_operand(us_ub_token(-),1))
%~
%~ replacing_with_var(unary_op_operand(us_ub_token(-),1))
%~
% Compiled KL-1 for cmirror
cmirror(PIECE_01,PIECE_02) :-
  willBeType(PIECE_02,'Piece') ,
  comment(' mirroring along counterdiagonal ') ,
  (/*2*/
    [ unary_op_operand(us_ub_token(-),1,ARG_017),
      slice_step(ARG_017,ARG_013),
      subscript_value_slice(R,ARG_013,ARG_012),
      unary_op_operand(us_ub_token(-),1,ARG_018),
      slice_step(ARG_018,ARG_016),
      subscript_value_slice(PIECE_01,ARG_016,ARG_015),
      comprehension_target_iter(R,ARG_015,ARG_014),
      generator_exp_elt_generators(ARG_012,[ARG_014],ARG_011),
      starred_value(ARG_011,ARG_010),
      zip(ARG_010,ARG_09),
      tuple(ARG_09,ARG_08),
      return_value(ARG_08,ARG_06),
      body_stmts([ARG_06],ARG_03),
      [ isinstance(PIECE_01,TUPLE,ARG_04),
        testif(ARG_04)] ->
        ARG_03]) ,
  vmirror(PIECE_01,ARG_07) ,
  dmirror(ARG_07,ARG_05) ,
  vmirror(ARG_05,PIECE_02) ,
  exit_proc(PIECE_02).
%~ % Universal AST Pass #0
%~ def( "fill",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"), argument_name("value"),argument_name("patch")],
%~         [argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' fill value at indices ')),
%~                          assign_targets_value( [tuple_elts(["h","w"])],
%~                            tuple_elts( [ call_func_args("len",["grid"]),
%~                                          call_func_args("len",[subscript_value_slice("grid",0)])])),
%~                          assign_targets_value( ["grid_filled"],
%~                            call_func_args( "list", [
%~                              generator_exp_elt_generators(call_func_args("list",["row"]),[comprehension_target_iter("row","grid")])])),
%~                          for_target_iter_body( tuple_elts(["i","j"]),
%~                            call_func_args("toindices",["patch"]),
%~                            body_stmts( [ if_test_body(
%~                                             bool_op_values( ['python:And'], [
%~                                               compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                               compare_ops_left_comparators(
%~                                                  ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
%~                                             body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("grid_filled","i"),"j")],"value")]))])),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators(
%~                                             call_func_args("tuple",["row"]),
%~                                             [comprehension_target_iter("row","grid_filled")])]))])))
%~
%~ replacing_with_var( body_stmts( [ if_test_body(
%~                                      bool_op_values( ['python:And'], [
%~                                        compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                        compare_ops_left_comparators(
%~                                           ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
%~                                      body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("grid_filled","i"),"j")],VALUE_02)]))]))
%~
%~ replacing_with_var( if_test_body(
%~                        bool_op_values( ['python:And'], [
%~                          compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                          compare_ops_left_comparators(
%~                             ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
%~                        body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("grid_filled","i"),"j")],VALUE_02)])))
%~
%~ replacing_with_var(call_func_args("list",["row"]))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("grid_filled","i"),"j")],VALUE_02)]))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["j","w"])))
%~
%~ replacing_with_var(call_func_args("tuple",["row"]))
%~
%~ replacing_with_var(comprehension_target_iter("row","grid_filled"))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["i","h"]))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["j","w"]))
%~
%~ replacing_with_var(subscript_value_slice(subscript_value_slice("grid_filled","i"),"j"))
%~
% Compiled KL-1 for fill
fill(GRID_01,VALUE_02,PATCH_03,GRID_04) :-
  willBeType(GRID_04,'Grid') ,
  comment(' fill value at indices ') ,
  len(GRID_01,H) ,
  subscript_value_slice(GRID_01,0,ARG_05) ,
  len(ARG_05,W) ,
  list(ROW,ARG_011) ,
  generator_exp_elt_generators( ARG_011,
    [assign_targets_value1(ROW,GRID_01)],
    ARG_06) ,
  list(ARG_06,GRID_FILLED) ,
  body_stmts(
     [ call( [ subscript_value_slice(GRID_FILLED,I,ARG_022),
               subscript_value_slice(ARG_022,J,ARG_021),
               ARG_021=VALUE_02])],
     ARG_012) ,
  if_test_body(
     [ ops([lt_e_token(<=),lt_token(<)],ARG_017),
       comparators([I,H],ARG_018),
       compare_ops_left_comparators(ARG_017,0,ARG_018,ARG_013),
       ops([lt_e_token(<=),lt_token(<)],ARG_019),
       comparators([J,W],ARG_020),
       compare_ops_left_comparators(ARG_019,0,ARG_020,ARG_014),
       bool_op_values(['python:And'],[ARG_013,ARG_014])], ARG_012,ARG_010) ,
  body_stmts([ARG_010],ARG_08) ,
  for_each([toindices(PATCH_03,'$VAR'('TUPLE_ELTS([I,J])_07'))],ARG_08) ,
  tuple(ROW,ARG_015) ,
  comprehension_target_iter(ROW,"grid_filled",ARG_016) ,
  generator_exp_elt_generators(ARG_015,[ARG_016],ARG_09) ,
  tuple(ARG_09,GRID_04) ,
  exit_proc(GRID_04).
%~ % Universal AST Pass #0
%~ def( "paint",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"),
%~           argument_name("obj")],
%~         [argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' paint object to grid ')),
%~                          assign_targets_value( [tuple_elts(["h","w"])],
%~                            tuple_elts( [ call_func_args("len",["grid"]),
%~                                          call_func_args("len",[subscript_value_slice("grid",0)])])),
%~                          assign_targets_value( ["grid_painted"],
%~                            call_func_args( "list", [
%~                              generator_exp_elt_generators(call_func_args("list",["row"]),[comprehension_target_iter("row","grid")])])),
%~                          for_target_iter_body(
%~                             tuple_elts(["value",tuple_elts(["i","j"])]),
%~                             "obj",
%~                             body_stmts( [ if_test_body(
%~                                              bool_op_values( ['python:And'], [
%~                                                compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                                compare_ops_left_comparators(
%~                                                   ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
%~                                              body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("grid_painted","i"),"j")],"value")]))])),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators(
%~                                             call_func_args("tuple",["row"]),
%~                                             [comprehension_target_iter("row","grid_painted")])]))])))
%~
%~ replacing_with_var( body_stmts( [ if_test_body(
%~                                      bool_op_values( ['python:And'], [
%~                                        compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                        compare_ops_left_comparators(
%~                                           ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
%~                                      body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("grid_painted","i"),"j")],"value")]))]))
%~
%~ replacing_with_var( if_test_body(
%~                        bool_op_values( ['python:And'], [
%~                          compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                          compare_ops_left_comparators(
%~                             ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
%~                        body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("grid_painted","i"),"j")],"value")])))
%~
%~ replacing_with_var(call_func_args("list",["row"]))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("grid_painted","i"),"j")],"value")]))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["j","w"])))
%~
%~ replacing_with_var(call_func_args("tuple",["row"]))
%~
%~ replacing_with_var(comprehension_target_iter("row","grid_painted"))
%~
%~ replacing_with_var(subscript_value_slice(subscript_value_slice("grid_painted","i"),"j"))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["i","h"]))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["j","w"]))
%~
% Compiled KL-1 for paint
paint(GRID_01,OBJ_02,GRID_03) :-
  willBeType(GRID_03,'Grid') ,
  comment(' paint object to grid ') ,
  len(GRID_01,H) ,
  subscript_value_slice(GRID_01,0,ARG_04) ,
  len(ARG_04,W) ,
  list(ROW,ARG_010) ,
  generator_exp_elt_generators( ARG_010,
    [assign_targets_value1(ROW,GRID_01)],
    ARG_05) ,
  list(ARG_05,GRID_PAINTED) ,
  body_stmts(
     [ subscript_value_slice(GRID_PAINTED,I,ARG_017),
       subscript_value_slice(ARG_017,J,ARG_016),
       call(ARG_016="value")],
     ARG_011) ,
  if_test_body(
     [ ops([lt_e_token(<=),lt_token(<)],ARG_018),
       comparators([I,H],ARG_019),
       compare_ops_left_comparators(ARG_018,0,ARG_019,ARG_012),
       ops([lt_e_token(<=),lt_token(<)],ARG_020),
       comparators([J,W],ARG_021),
       compare_ops_left_comparators(ARG_020,0,ARG_021,ARG_013),
       bool_op_values(['python:And'],[ARG_012,ARG_013])], ARG_011,ARG_09) ,
  body_stmts([ARG_09],ARG_07) ,
  for_each(call('$VAR'('TUPLE_ELTS([VALUE,TUPLE_ELTS([I,J])])_06')=OBJ_02),ARG_07) ,
  tuple(ROW,ARG_014) ,
  comprehension_target_iter(ROW,"grid_painted",ARG_015) ,
  generator_exp_elt_generators(ARG_014,[ARG_015],ARG_08) ,
  tuple(ARG_08,GRID_03) ,
  exit_proc(GRID_03).
%~ % Universal AST Pass #0
%~ def( "underfill",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"), argument_name("value"),argument_name("patch")],
%~         [argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' fill value at indices that are background ')),
%~                          assign_targets_value( [tuple_elts(["h","w"])],
%~                            tuple_elts( [ call_func_args("len",["grid"]),
%~                                          call_func_args("len",[subscript_value_slice("grid",0)])])),
%~                          assign_targets_value(["bg"],call_func_args("mostcolor",["grid"])),
%~                          assign_targets_value( ["g"],
%~                            call_func_args( "list", [
%~                              generator_exp_elt_generators(call_func_args("list",["r"]),[comprehension_target_iter("r","grid")])])),
%~                          for_target_iter_body( tuple_elts(["i","j"]),
%~                            call_func_args("toindices",["patch"]),
%~                            body_stmts( [ if_test_body(
%~                                             bool_op_values( ['python:And'], [
%~                                               compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                               compare_ops_left_comparators(
%~                                                  ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
%~                                             body_stmts( [ if_test_body(
%~                                                              compare_ops_left_comparators(eq_token(==),subscript_value_slice(subscript_value_slice("g","i"),"j"),"bg"),
%~                                                              body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],"value")]))]))])),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators(call_func_args("tuple",["r"]),[comprehension_target_iter("r","g")])]))])))
%~
%~ replacing_with_var( body_stmts( [ if_test_body(
%~                                      bool_op_values( ['python:And'], [
%~                                        compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                        compare_ops_left_comparators(
%~                                           ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
%~                                      body_stmts( [ if_test_body(
%~                                                       compare_ops_left_comparators(eq_token(==),subscript_value_slice(subscript_value_slice("g","i"),"j"),"bg"),
%~                                                       body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],VALUE_02)]))]))]))
%~
%~ replacing_with_var( if_test_body(
%~                        bool_op_values( ['python:And'], [
%~                          compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                          compare_ops_left_comparators(
%~                             ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
%~                        body_stmts( [ if_test_body(
%~                                         compare_ops_left_comparators(eq_token(==),subscript_value_slice(subscript_value_slice("g","i"),"j"),"bg"),
%~                                         body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],VALUE_02)]))])))
%~
%~ replacing_with_var(call_func_args("list",["r"]))
%~
%~ replacing_with_var( body_stmts( [ if_test_body(
%~                                      compare_ops_left_comparators(eq_token(==),subscript_value_slice(subscript_value_slice("g","i"),"j"),"bg"),
%~                                      body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],VALUE_02)]))]))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["j","w"])))
%~
%~ replacing_with_var(call_func_args("tuple",["r"]))
%~
%~ replacing_with_var(comprehension_target_iter("r","g"))
%~
%~ replacing_with_var( if_test_body(
%~                        compare_ops_left_comparators(eq_token(==),subscript_value_slice(subscript_value_slice("g","i"),"j"),"bg"),
%~                        body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],VALUE_02)])))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["i","h"]))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["j","w"]))
%~
%~ replacing_with_var( compare_ops_left_comparators(eq_token(==),subscript_value_slice(subscript_value_slice("g","i"),"j"),"bg"))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],VALUE_02)]))
%~
%~ replacing_with_var(subscript_value_slice(subscript_value_slice("g","i"),"j"))
%~
%~ replacing_with_var(subscript_value_slice(subscript_value_slice("g","i"),"j"))
%~
% Compiled KL-1 for underfill
underfill(GRID_01,VALUE_02,PATCH_03,GRID_04) :-
  willBeType(GRID_04,'Grid') ,
  comment(' fill value at indices that are background ') ,
  len(GRID_01,H) ,
  subscript_value_slice(GRID_01,0,ARG_05) ,
  len(ARG_05,W) ,
  mostcolor(GRID_01,BG) ,
  list(R,ARG_011) ,
  generator_exp_elt_generators( ARG_011,
    [assign_targets_value1(R,GRID_01)],
    ARG_06) ,
  list(ARG_06,G) ,
  subscript_value_slice(G,I,ARG_025) ,
  subscript_value_slice(ARG_025,J,ARG_024) ,
  compare_ops_left_comparators(eq_token(==),ARG_024,BG,ARG_022) ,
  body_stmts(
     [ call( [ subscript_value_slice(G,I,ARG_027),
               subscript_value_slice(ARG_027,J,ARG_026),
               ARG_026=VALUE_02])],
     ARG_023) ,
  if_test_body(ARG_022,ARG_023,ARG_017) ,
  body_stmts([ARG_017],ARG_012) ,
  if_test_body(
     [ ops([lt_e_token(<=),lt_token(<)],ARG_018),
       comparators([I,H],ARG_019),
       compare_ops_left_comparators(ARG_018,0,ARG_019,ARG_013),
       ops([lt_e_token(<=),lt_token(<)],ARG_020),
       comparators([J,W],ARG_021),
       compare_ops_left_comparators(ARG_020,0,ARG_021,ARG_014),
       bool_op_values(['python:And'],[ARG_013,ARG_014])], ARG_012,ARG_010) ,
  body_stmts([ARG_010],ARG_08) ,
  for_each([toindices(PATCH_03,'$VAR'('TUPLE_ELTS([I,J])_07'))],ARG_08) ,
  tuple(R,ARG_015) ,
  comprehension_target_iter(R,G,ARG_016) ,
  generator_exp_elt_generators(ARG_015,[ARG_016],ARG_09) ,
  tuple(ARG_09,GRID_04) ,
  exit_proc(GRID_04).
%~ % Universal AST Pass #0
%~ def( "underpaint",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"),
%~           argument_name("obj")],
%~         [argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' paint object to grid where there is background ')),
%~                          assign_targets_value( [tuple_elts(["h","w"])],
%~                            tuple_elts( [ call_func_args("len",["grid"]),
%~                                          call_func_args("len",[subscript_value_slice("grid",0)])])),
%~                          assign_targets_value(["bg"],call_func_args("mostcolor",["grid"])),
%~                          assign_targets_value( ["g"],
%~                            call_func_args( "list", [
%~                              generator_exp_elt_generators(call_func_args("list",["r"]),[comprehension_target_iter("r","grid")])])),
%~                          for_target_iter_body(
%~                             tuple_elts(["value",tuple_elts(["i","j"])]),
%~                             "obj",
%~                             body_stmts( [ if_test_body(
%~                                              bool_op_values( ['python:And'], [
%~                                                compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                                compare_ops_left_comparators(
%~                                                   ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
%~                                              body_stmts( [ if_test_body(
%~                                                               compare_ops_left_comparators(eq_token(==),subscript_value_slice(subscript_value_slice("g","i"),"j"),"bg"),
%~                                                               body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],"value")]))]))])),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators(call_func_args("tuple",["r"]),[comprehension_target_iter("r","g")])]))])))
%~
%~ replacing_with_var( body_stmts( [ if_test_body(
%~                                      bool_op_values( ['python:And'], [
%~                                        compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                        compare_ops_left_comparators(
%~                                           ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
%~                                      body_stmts( [ if_test_body(
%~                                                       compare_ops_left_comparators(eq_token(==),subscript_value_slice(subscript_value_slice("g","i"),"j"),"bg"),
%~                                                       body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],"value")]))]))]))
%~
%~ replacing_with_var( if_test_body(
%~                        bool_op_values( ['python:And'], [
%~                          compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                          compare_ops_left_comparators(
%~                             ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
%~                        body_stmts( [ if_test_body(
%~                                         compare_ops_left_comparators(eq_token(==),subscript_value_slice(subscript_value_slice("g","i"),"j"),"bg"),
%~                                         body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],"value")]))])))
%~
%~ replacing_with_var(call_func_args("list",["r"]))
%~
%~ replacing_with_var( body_stmts( [ if_test_body(
%~                                      compare_ops_left_comparators(eq_token(==),subscript_value_slice(subscript_value_slice("g","i"),"j"),"bg"),
%~                                      body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],"value")]))]))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["j","w"])))
%~
%~ replacing_with_var(call_func_args("tuple",["r"]))
%~
%~ replacing_with_var(comprehension_target_iter("r","g"))
%~
%~ replacing_with_var( if_test_body(
%~                        compare_ops_left_comparators(eq_token(==),subscript_value_slice(subscript_value_slice("g","i"),"j"),"bg"),
%~                        body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],"value")])))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["i","h"]))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["j","w"]))
%~
%~ replacing_with_var( compare_ops_left_comparators(eq_token(==),subscript_value_slice(subscript_value_slice("g","i"),"j"),"bg"))
%~
%~ replacing_with_var( body_stmts([assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],"value")]))
%~
%~ replacing_with_var(subscript_value_slice(subscript_value_slice("g","i"),"j"))
%~
%~ replacing_with_var(subscript_value_slice(subscript_value_slice("g","i"),"j"))
%~
% Compiled KL-1 for underpaint
underpaint(GRID_01,OBJ_02,GRID_03) :-
  willBeType(GRID_03,'Grid') ,
  comment(' paint object to grid where there is background ') ,
  len(GRID_01,H) ,
  subscript_value_slice(GRID_01,0,ARG_04) ,
  len(ARG_04,W) ,
  mostcolor(GRID_01,BG) ,
  list(R,ARG_010) ,
  generator_exp_elt_generators( ARG_010,
    [assign_targets_value1(R,GRID_01)],
    ARG_05) ,
  list(ARG_05,G) ,
  subscript_value_slice(G,I,ARG_024) ,
  subscript_value_slice(ARG_024,J,ARG_023) ,
  compare_ops_left_comparators(eq_token(==),ARG_023,BG,ARG_021) ,
  body_stmts(
     [ subscript_value_slice(G,I,ARG_026),
       subscript_value_slice(ARG_026,J,ARG_025),
       call(ARG_025="value")],
     ARG_022) ,
  if_test_body(ARG_021,ARG_022,ARG_016) ,
  body_stmts([ARG_016],ARG_011) ,
  if_test_body(
     [ ops([lt_e_token(<=),lt_token(<)],ARG_017),
       comparators([I,H],ARG_018),
       compare_ops_left_comparators(ARG_017,0,ARG_018,ARG_012),
       ops([lt_e_token(<=),lt_token(<)],ARG_019),
       comparators([J,W],ARG_020),
       compare_ops_left_comparators(ARG_019,0,ARG_020,ARG_013),
       bool_op_values(['python:And'],[ARG_012,ARG_013])], ARG_011,ARG_09) ,
  body_stmts([ARG_09],ARG_07) ,
  for_each(call('$VAR'('TUPLE_ELTS([VALUE,TUPLE_ELTS([I,J])])_06')=OBJ_02),ARG_07) ,
  tuple(R,ARG_014) ,
  comprehension_target_iter(R,G,ARG_015) ,
  generator_exp_elt_generators(ARG_014,[ARG_015],ARG_08) ,
  tuple(ARG_08,GRID_03) ,
  exit_proc(GRID_03).
%~ % Universal AST Pass #0
%~ def( "hupscale",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"),
%~           argument_name("factor")],
%~         [argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' upscale grid horizontally ')),
%~                          assign_targets_value(["g"],call_func("tuple")),
%~                          for_target_iter_body( "row",
%~                            "grid",
%~                            body_stmts( [ assign_targets_value(["r"],call_func("tuple")),
%~                                          for_target_iter_body( "value",
%~                                            "row",
%~                                            body_stmts( [ assign_targets_value( ["r"],
%~                                                            bin_op_left_right( add_token(+),
%~                                                              "r",
%~                                                              call_func_args( "tuple", [
%~                                                                generator_exp_elt_generators( "value", [
%~                                                                  comprehension_target_iter("num",call_func_args("range",["factor"]))])])))])),
%~                                          assign_targets_value(["g"],bin_op_left_right(add_token(+),"g",tuple_elts(["r"])))])),
%~                          return_value("g")])))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value(["r"],call_func("tuple")),
%~                                   for_target_iter_body( "value",
%~                                     ROW_04,
%~                                     body_stmts( [ assign_targets_value( ["r"],
%~                                                     bin_op_left_right( add_token(+),
%~                                                       "r",
%~                                                       call_func_args( "tuple", [
%~                                                         generator_exp_elt_generators( "value", [
%~                                                           comprehension_target_iter("num",call_func_args("range",[FACTOR_02]))])])))])),
%~                                   assign_targets_value(["g"],bin_op_left_right(add_token(+),"g",tuple_elts(["r"])))]))
%~
%~ replacing_with_var( for_target_iter_body( "value",
%~                       ROW_04,
%~                       body_stmts( [ assign_targets_value( ["r"],
%~                                       bin_op_left_right( add_token(+),
%~                                         "r",
%~                                         call_func_args( "tuple", [
%~                                           generator_exp_elt_generators( "value", [
%~                                             comprehension_target_iter("num",call_func_args("range",[FACTOR_02]))])])))])))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value( ["r"],
%~                                     bin_op_left_right( add_token(+),
%~                                       "r",
%~                                       call_func_args( "tuple", [
%~                                         generator_exp_elt_generators( "value", [
%~                                           comprehension_target_iter("num",call_func_args("range",[FACTOR_02]))])])))]))
%~
%~ replacing_with_var(make_new("tuple","r"))
%~
%~ replacing_with_var(comprehension_target_iter("num",call_func_args("range",[FACTOR_02])))
%~
%~ replacing_with_var(call_func_args("range",[FACTOR_02]))
%~
% Compiled KL-1 for hupscale
hupscale(GRID_01,FACTOR_02,GRID_03) :-
  willBeType(GRID_03,'Grid') ,
  comment(' upscale grid horizontally ') ,
  make_new("tuple",G) ,
  body_stmts(
     [ range(FACTOR_02,ARG_013),
       comprehension_target_iter("num",ARG_013,ARG_012),
       generator_exp_elt_generators("value",[ARG_012],ARG_011),
       tuple(ARG_011,ARG_010),
       call([op_call(add_token(+,R,ARG_010,R))])],
     ARG_08) ,
  for_target_iter_body("value",ROW_04,ARG_08,ARG_06) ,
  make_new("tuple",R,ARG_09) ,
  body_stmts(
     [ ARG_09, ARG_06,tuple_elts(R,ARG_07),
       call([op_call(add_token(+,G,ARG_07,G))])],
     ARG_05) ,
  for_each(call(ROW_04=GRID_01),ARG_05) ,
  call(GRID_03=G) ,
  exit_proc(GRID_03).
%~ % Universal AST Pass #0
%~ def( "vupscale",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"),
%~           argument_name("factor")],
%~         [argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' upscale grid vertically ')),
%~                          assign_targets_value(["g"],call_func("tuple")),
%~                          for_target_iter_body( "row",
%~                            "grid",
%~                            body_stmts( [ assign_targets_value( ["g"],
%~                                            bin_op_left_right( add_token(+),
%~                                              "g",
%~                                              call_func_args( "tuple", [
%~                                                generator_exp_elt_generators( "row", [
%~                                                  comprehension_target_iter("num",call_func_args("range",["factor"]))])])))])),
%~                          return_value("g")])))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value( ["g"],
%~                                     bin_op_left_right( add_token(+),
%~                                       "g",
%~                                       call_func_args( "tuple", [
%~                                         generator_exp_elt_generators( ROW_04, [
%~                                           comprehension_target_iter("num",call_func_args("range",[FACTOR_02]))])])))]))
%~
%~ replacing_with_var(comprehension_target_iter("num",call_func_args("range",[FACTOR_02])))
%~
%~ replacing_with_var(call_func_args("range",[FACTOR_02]))
%~
% Compiled KL-1 for vupscale
vupscale(GRID_01,FACTOR_02,GRID_03) :-
  willBeType(GRID_03,'Grid') ,
  comment(' upscale grid vertically ') ,
  make_new("tuple",G) ,
  body_stmts(
     [ range(FACTOR_02,ARG_09),
       comprehension_target_iter("num",ARG_09,ARG_08),
       generator_exp_elt_generators(ROW_04,[ARG_08],ARG_07),
       tuple(ARG_07,ARG_06),
       call([op_call(add_token(+,G,ARG_06,G))])],
     ARG_05) ,
  for_each(call(ROW_04=GRID_01),ARG_05) ,
  call(GRID_03=G) ,
  exit_proc(GRID_03).
%~ % Universal AST Pass #0
%~ def( "upscale",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("element"),
%~           argument_name("factor")],
%~         [argument_type("Element")]),
%~      block_statements( [ expr_value(string_value(' upscale object or grid ')),
%~                          if_test_body_orelse(
%~                             call_func_args("isinstance",["element","tuple"]),
%~                             body_stmts( [ assign_targets_value(["g"],call_func("tuple")),
%~                                           for_target_iter_body( "row",
%~                                             "element",
%~                                             body_stmts( [ assign_targets_value(["upscaled_row"],call_func("tuple")),
%~                                                           for_target_iter_body( "value",
%~                                                             "row",
%~                                                             body_stmts( [ assign_targets_value( ["upscaled_row"],
%~                                                                             bin_op_left_right( add_token(+),
%~                                                                               "upscaled_row",
%~                                                                               call_func_args( "tuple", [
%~                                                                                 generator_exp_elt_generators( "value", [
%~                                                                                   comprehension_target_iter("num",call_func_args("range",["factor"]))])])))])),
%~                                                           assign_targets_value( ["g"],
%~                                                             bin_op_left_right( add_token(+),
%~                                                               "g",
%~                                                               call_func_args( "tuple", [
%~                                                                 generator_exp_elt_generators( "upscaled_row", [
%~                                                                   comprehension_target_iter("num",call_func_args("range",["factor"]))])])))])),
%~                                           return_value("g")]),
%~                             orelse_else_stmts( [ if_test_body(
%~                                                     compare_ops_left_comparators(eq_token(==),call_func_args("len",["element"]),0),
%~                                                     body_stmts([return_value(call_func("frozenset"))])),
%~                                                  assign_targets_value( [tuple_elts(["di_inv","dj_inv"])],
%~                                                    call_func_args("ulcorner",["element"])),
%~                                                  assign_targets_value( [tuple_elts(["di","dj"])],
%~                                                    tuple_elts([unary_op_operand(us_ub_token(-),"di_inv"),unary_op_operand(us_ub_token(-),"dj_inv")])),
%~                                                  assign_targets_value( ["normed_obj"],
%~                                                    call_func_args("shift",["element",tuple_elts(["di","dj"])])),
%~                                                  assign_targets_value(["o"],call_func("set")),
%~                                                  for_target_iter_body(
%~                                                     tuple_elts(["value",tuple_elts(["i","j"])]),
%~                                                     "normed_obj",
%~                                                     body_stmts( [ for_target_iter_body( "io",
%~                                                                     call_func_args("range",["factor"]),
%~                                                                     body_stmts( [ for_target_iter_body( "jo",
%~                                                                                     call_func_args("range",["factor"]),
%~                                                                                     body_stmts( [ expr_value( call_func_args(
%~                                                                                                                  qualified_identifier_identifiers(["o",boxed_attribute_value("add")]),
%~                                                                                                                  [ tuple_elts( [ "value",
%~                                                                                                                                  tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"i","factor"),"io"),
%~                                                                                                                                                bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"j","factor"),"jo")])])]))]))]))])),
%~                                                  return_value( call_func_args( "shift", [
%~                                                                  call_func_args("frozenset",["o"]),
%~                                                                  tuple_elts(["di_inv","dj_inv"])]))]))])))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body(
%~                                             compare_ops_left_comparators(eq_token(==),call_func_args("len",[ELEMENT_01]),0),
%~                                             body_stmts([return_value(call_func("frozenset"))])),
%~                                          assign_targets_value( [tuple_elts(["di_inv","dj_inv"])],
%~                                            call_func_args("ulcorner",[ELEMENT_01])),
%~                                          assign_targets_value( [tuple_elts(["di","dj"])],
%~                                            tuple_elts([unary_op_operand(us_ub_token(-),"di_inv"),unary_op_operand(us_ub_token(-),"dj_inv")])),
%~                                          assign_targets_value( ["normed_obj"],
%~                                            call_func_args("shift",[ELEMENT_01,tuple_elts(["di","dj"])])),
%~                                          assign_targets_value(["o"],call_func("set")),
%~                                          for_target_iter_body(
%~                                             tuple_elts(["value",tuple_elts(["i","j"])]),
%~                                             "normed_obj",
%~                                             body_stmts( [ for_target_iter_body( "io",
%~                                                             call_func_args("range",[FACTOR_02]),
%~                                                             body_stmts( [ for_target_iter_body( "jo",
%~                                                                             call_func_args("range",[FACTOR_02]),
%~                                                                             body_stmts( [ expr_value( call_func_args(
%~                                                                                                          qualified_identifier_identifiers(["o",boxed_attribute_value("add")]),
%~                                                                                                          [ tuple_elts( [ "value",
%~                                                                                                                          tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"i",FACTOR_02),"io"),
%~                                                                                                                                        bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"j",FACTOR_02),"jo")])])]))]))]))])),
%~                                          return_value( call_func_args( "shift", [
%~                                                          call_func_args("frozenset",["o"]),
%~                                                          tuple_elts(["di_inv","dj_inv"])]))]))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value(["g"],call_func("tuple")),
%~                                   for_target_iter_body( "row",
%~                                     ELEMENT_01,
%~                                     body_stmts( [ assign_targets_value(["upscaled_row"],call_func("tuple")),
%~                                                   for_target_iter_body( "value",
%~                                                     "row",
%~                                                     body_stmts( [ assign_targets_value( ["upscaled_row"],
%~                                                                     bin_op_left_right( add_token(+),
%~                                                                       "upscaled_row",
%~                                                                       call_func_args( "tuple", [
%~                                                                         generator_exp_elt_generators( "value", [
%~                                                                           comprehension_target_iter("num",call_func_args("range",[FACTOR_02]))])])))])),
%~                                                   assign_targets_value( ["g"],
%~                                                     bin_op_left_right( add_token(+),
%~                                                       "g",
%~                                                       call_func_args( "tuple", [
%~                                                         generator_exp_elt_generators( "upscaled_row", [
%~                                                           comprehension_target_iter("num",call_func_args("range",[FACTOR_02]))])])))])),
%~                                   return_value("g")]))
%~
%~ replacing_with_var(call_func_args("isinstance",[ELEMENT_01,"tuple"]))
%~
%~ replacing_with_var( if_test_body(
%~                        compare_ops_left_comparators(eq_token(==),call_func_args("len",[ELEMENT_01]),0),
%~                        body_stmts([return_value(call_func("frozenset"))])))
%~
%~ replacing_with_var( for_target_iter_body(
%~                        tuple_elts(["value",tuple_elts(["i","j"])]),
%~                        "normed_obj",
%~                        body_stmts( [ for_target_iter_body( "io",
%~                                        call_func_args("range",[FACTOR_02]),
%~                                        body_stmts( [ for_target_iter_body( "jo",
%~                                                        call_func_args("range",[FACTOR_02]),
%~                                                        body_stmts( [ expr_value( call_func_args(
%~                                                                                     qualified_identifier_identifiers(["o",boxed_attribute_value("add")]),
%~                                                                                     [ tuple_elts( [ "value",
%~                                                                                                     tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"i",FACTOR_02),"io"),
%~                                                                                                                   bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"j",FACTOR_02),"jo")])])]))]))]))])))
%~
%~ replacing_with_var( return_value( call_func_args( "shift", [
%~                                     call_func_args("frozenset",["o"]),
%~                                     tuple_elts(["di_inv","dj_inv"])])))
%~
%~ replacing_with_var( for_target_iter_body( "row",
%~                       ELEMENT_01,
%~                       body_stmts( [ assign_targets_value(["upscaled_row"],call_func("tuple")),
%~                                     for_target_iter_body( "value",
%~                                       "row",
%~                                       body_stmts( [ assign_targets_value( ["upscaled_row"],
%~                                                       bin_op_left_right( add_token(+),
%~                                                         "upscaled_row",
%~                                                         call_func_args( "tuple", [
%~                                                           generator_exp_elt_generators( "value", [
%~                                                             comprehension_target_iter("num",call_func_args("range",[FACTOR_02]))])])))])),
%~                                     assign_targets_value( ["g"],
%~                                       bin_op_left_right( add_token(+),
%~                                         "g",
%~                                         call_func_args( "tuple", [
%~                                           generator_exp_elt_generators( "upscaled_row", [
%~                                             comprehension_target_iter("num",call_func_args("range",[FACTOR_02]))])])))])))
%~
%~ replacing_with_var(return_value("g"))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),call_func_args("len",[ELEMENT_01]),0))
%~
%~ replacing_with_var(body_stmts([return_value(call_func("frozenset"))]))
%~
%~ replacing_with_var(tuple_elts(["value",tuple_elts(["i","j"])]))
%~
%~ replacing_with_var( body_stmts( [ for_target_iter_body( "io",
%~                                     call_func_args("range",[FACTOR_02]),
%~                                     body_stmts( [ for_target_iter_body( "jo",
%~                                                     call_func_args("range",[FACTOR_02]),
%~                                                     body_stmts( [ expr_value( call_func_args(
%~                                                                                  qualified_identifier_identifiers(["o",boxed_attribute_value("add")]),
%~                                                                                  [ tuple_elts( [ "value",
%~                                                                                                  tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"i",FACTOR_02),"io"),
%~                                                                                                                bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"j",FACTOR_02),"jo")])])]))]))]))]))
%~
%~ replacing_with_var( call_func_args( "shift", [
%~                       call_func_args("frozenset",["o"]),
%~                       tuple_elts(["di_inv","dj_inv"])]))
%~
%~ replacing_with_var(make_new("set","o"))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value(["upscaled_row"],call_func("tuple")),
%~                                   for_target_iter_body( "value",
%~                                     "row",
%~                                     body_stmts( [ assign_targets_value( ["upscaled_row"],
%~                                                     bin_op_left_right( add_token(+),
%~                                                       "upscaled_row",
%~                                                       call_func_args( "tuple", [
%~                                                         generator_exp_elt_generators( "value", [
%~                                                           comprehension_target_iter("num",call_func_args("range",[FACTOR_02]))])])))])),
%~                                   assign_targets_value( ["g"],
%~                                     bin_op_left_right( add_token(+),
%~                                       "g",
%~                                       call_func_args( "tuple", [
%~                                         generator_exp_elt_generators( "upscaled_row", [
%~                                           comprehension_target_iter("num",call_func_args("range",[FACTOR_02]))])])))]))
%~
%~ replacing_with_var(make_new("tuple","g"))
%~
%~ replacing_with_var(call_func_args("len",[ELEMENT_01]))
%~
%~ replacing_with_var(return_value(call_func("frozenset")))
%~
%~ replacing_with_var( for_target_iter_body( "io",
%~                       call_func_args("range",[FACTOR_02]),
%~                       body_stmts( [ for_target_iter_body( "jo",
%~                                       call_func_args("range",[FACTOR_02]),
%~                                       body_stmts( [ expr_value( call_func_args(
%~                                                                    qualified_identifier_identifiers(["o",boxed_attribute_value("add")]),
%~                                                                    [ tuple_elts( [ "value",
%~                                                                                    tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"i",FACTOR_02),"io"),
%~                                                                                                  bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"j",FACTOR_02),"jo")])])]))]))])))
%~
%~ replacing_with_var( for_target_iter_body( "value",
%~                       "row",
%~                       body_stmts( [ assign_targets_value( ["upscaled_row"],
%~                                       bin_op_left_right( add_token(+),
%~                                         "upscaled_row",
%~                                         call_func_args( "tuple", [
%~                                           generator_exp_elt_generators( "value", [
%~                                             comprehension_target_iter("num",call_func_args("range",[FACTOR_02]))])])))])))
%~
%~ replacing_with_var(call_func("frozenset"))
%~
%~ replacing_with_var(call_func_args("range",[FACTOR_02]))
%~
%~ replacing_with_var( body_stmts( [ for_target_iter_body( "jo",
%~                                     call_func_args("range",[FACTOR_02]),
%~                                     body_stmts( [ expr_value( call_func_args(
%~                                                                  qualified_identifier_identifiers(["o",boxed_attribute_value("add")]),
%~                                                                  [ tuple_elts( [ "value",
%~                                                                                  tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"i",FACTOR_02),"io"),
%~                                                                                                bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"j",FACTOR_02),"jo")])])]))]))]))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value( ["upscaled_row"],
%~                                     bin_op_left_right( add_token(+),
%~                                       "upscaled_row",
%~                                       call_func_args( "tuple", [
%~                                         generator_exp_elt_generators( "value", [
%~                                           comprehension_target_iter("num",call_func_args("range",[FACTOR_02]))])])))]))
%~
%~ replacing_with_var(make_new("tuple","upscaled_row"))
%~
%~ replacing_with_var( for_target_iter_body( "jo",
%~                       call_func_args("range",[FACTOR_02]),
%~                       body_stmts( [ expr_value( call_func_args(
%~                                                    qualified_identifier_identifiers(["o",boxed_attribute_value("add")]),
%~                                                    [ tuple_elts( [ "value",
%~                                                                    tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"i",FACTOR_02),"io"),
%~                                                                                  bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"j",FACTOR_02),"jo")])])]))])))
%~
%~ replacing_with_var(call_func_args("range",[FACTOR_02]))
%~
%~ replacing_with_var( body_stmts( [ expr_value( call_func_args(
%~                                                  qualified_identifier_identifiers(["o",boxed_attribute_value("add")]),
%~                                                  [ tuple_elts( [ "value",
%~                                                                  tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"i",FACTOR_02),"io"),
%~                                                                                bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"j",FACTOR_02),"jo")])])]))]))
%~
%~ replacing_with_var(comprehension_target_iter("num",call_func_args("range",[FACTOR_02])))
%~
%~ replacing_with_var( expr_value( call_func_args(
%~                                    qualified_identifier_identifiers(["o",boxed_attribute_value("add")]),
%~                                    [ tuple_elts( [ "value",
%~                                                    tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"i",FACTOR_02),"io"),
%~                                                                  bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"j",FACTOR_02),"jo")])])])))
%~
%~ replacing_with_var(call_func_args("range",[FACTOR_02]))
%~
%~ replacing_with_var(boxed_attribute_value("add"))
%~
%~ replacing_with_var(comprehension_target_iter("num",call_func_args("range",[FACTOR_02])))
%~
%~ replacing_with_var(call_func_args("range",[FACTOR_02]))
%~
% Compiled KL-1 for upscale
upscale(ELEMENT_01,FACTOR_02,ELEMENT_03) :-
  willBeType(ELEMENT_03,'Element') ,
  comment(' upscale object or grid ') ,
  len(ELEMENT_01,ARG_025) ,
  compare_ops_left_comparators(eq_token(==),ARG_025,0,ARG_014) ,
  make_new("frozenset",ARG_030) ,
  return_value(ARG_030,ARG_026) ,
  body_stmts([ARG_026],ARG_015) ,
  if_test_body(ARG_014,ARG_015,ARG_07) ,
  into_tuple(I,J,ARG_017) ,
  tuple_elts(VALUE,ARG_017,ARG_016) ,
  range(FACTOR_02,ARG_031) ,
  range(FACTOR_02,ARG_038) ,
  call([op_call(mult_token(*,I,FACTOR_02,ARG_051))]) ,
  call([op_call(add_token(+,ARG_051,IO,ARG_048))]) ,
  call([op_call(mult_token(*,J,FACTOR_02,ARG_052))]) ,
  call([op_call(add_token(+,ARG_052,JO,ARG_049))]) ,
  tuple_elts(ARG_048,ARG_049,ARG_045) ,
  tuple_elts(VALUE,ARG_045,ARG_043) ,
  call( [ op_call( [ boxed_attribute_value("add",ARG_046),
                     qualified_identifier_identifiers([O,ARG_046],ARG_043,ARG_042)])]) ,
  body_stmts([ARG_042],ARG_039) ,
  for_target_iter_body(JO,ARG_038,ARG_039,ARG_036) ,
  body_stmts([ARG_036],ARG_032) ,
  for_target_iter_body(IO,ARG_031,ARG_032,ARG_027) ,
  body_stmts([ARG_027],ARG_018) ,
  for_target_iter_body(ARG_016,"normed_obj",ARG_018,ARG_08) ,
  frozenset(O,ARG_020) ,
  into_tuple("di_inv","dj_inv",ARG_021) ,
  shift(ARG_020,ARG_021,ARG_019) ,
  return_value(ARG_019,ARG_09) ,
  make_new("set",O,ARG_022) ,
  orelse_else_stmts(
     [ ARG_07,
       into_tuple("di_inv","dj_inv",ARG_010),
       ulcorner(ELEMENT_01,ARG_010),
       unary_op_operand(us_ub_token(-),"di_inv",DI),
       unary_op_operand(us_ub_token(-),"dj_inv",DJ),
       into_tuple(DI,DJ,ARG_011),
       shift(ELEMENT_01,ARG_011,NORMED_OBJ), ARG_022,ARG_08,ARG_09],
     ARG_04) ,
  (/*2*/
    [ body_stmts(
         [ range(FACTOR_02,ARG_050),
           comprehension_target_iter("num",ARG_050,ARG_047),
           generator_exp_elt_generators(VALUE,[ARG_047],ARG_040),
           tuple(ARG_040,ARG_037),
           call([op_call(add_token(+,UPSCALED_ROW,ARG_037,UPSCALED_ROW))])],
         ARG_033),
      for_target_iter_body(VALUE,"row",ARG_033,ARG_028),
      make_new(TUPLE,"upscaled_row",ARG_034),
      body_stmts(
         [ ARG_034,
           ARG_028,
           range(FACTOR_02,ARG_044),
           comprehension_target_iter("num",ARG_044,ARG_041),
           generator_exp_elt_generators("upscaled_row",[ARG_041],ARG_035),
           tuple(ARG_035,ARG_029),
           call([op_call(add_token(+,G,ARG_029,G))])],
         ARG_023),
      for_target_iter_body("row",ELEMENT_01,ARG_023,ARG_012),
      return_value(G,ARG_013),
      make_new(TUPLE,G,ARG_024),
      body_stmts([ARG_024,ARG_012,ARG_013],ARG_05),
      [ isinstance(ELEMENT_01,TUPLE,ARG_06),
        testif(ARG_06)] ->
        ARG_05] ;
    ARG_04).
%~ % Universal AST Pass #0
%~ def( "downscale",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"),
%~           argument_name("factor")],
%~         [argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' downscale grid ')),
%~                          assign_targets_value( [tuple_elts(["h","w"])],
%~                            tuple_elts( [ call_func_args("len",["grid"]),
%~                                          call_func_args("len",[subscript_value_slice("grid",0)])])),
%~                          assign_targets_value(["g"],call_func("tuple")),
%~                          for_target_iter_body( "i",
%~                            call_func_args("range",["h"]),
%~                            body_stmts( [ assign_targets_value(["r"],call_func("tuple")),
%~                                          for_target_iter_body( "j",
%~                                            call_func_args("range",["w"]),
%~                                            body_stmts( [ if_test_body(
%~                                                             compare_ops_left_comparators(eq_token(==),bin_op_left_right(mod_token('%'),"j","factor"),0),
%~                                                             body_stmts( [ assign_targets_value( ["r"],
%~                                                                             bin_op_left_right(add_token(+),"r",tuple_elts([subscript_value_slice(subscript_value_slice("grid","i"),"j")])))]))])),
%~                                          assign_targets_value(["g"],bin_op_left_right(add_token(+),"g",tuple_elts(["r"])))])),
%~                          assign_targets_value(["h"],call_func_args("len",["g"])),
%~                          assign_targets_value(["dsg"],call_func("tuple")),
%~                          for_target_iter_body( "i",
%~                            call_func_args("range",["h"]),
%~                            body_stmts( [ if_test_body(
%~                                             compare_ops_left_comparators(eq_token(==),bin_op_left_right(mod_token('%'),"i","factor"),0),
%~                                             body_stmts( [ assign_targets_value( ["dsg"],
%~                                                             bin_op_left_right(add_token(+),"dsg",tuple_elts([subscript_value_slice("g","i")])))]))])),
%~                          return_value("dsg")])))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value(["r"],call_func("tuple")),
%~                                   for_target_iter_body( "j",
%~                                     call_func_args("range",["w"]),
%~                                     body_stmts( [ if_test_body(
%~                                                      compare_ops_left_comparators(eq_token(==),bin_op_left_right(mod_token('%'),"j",FACTOR_02),0),
%~                                                      body_stmts( [ assign_targets_value( ["r"],
%~                                                                      bin_op_left_right( add_token(+),
%~                                                                        "r",
%~                                                                        tuple_elts([subscript_value_slice(subscript_value_slice(GRID_01,I_05),"j")])))]))])),
%~                                   assign_targets_value(["g"],bin_op_left_right(add_token(+),"g",tuple_elts(["r"])))]))
%~
%~ replacing_with_var( body_stmts( [ if_test_body(
%~                                      compare_ops_left_comparators(eq_token(==),bin_op_left_right(mod_token('%'),I_07,FACTOR_02),0),
%~                                      body_stmts( [ assign_targets_value( ["dsg"],
%~                                                      bin_op_left_right(add_token(+),"dsg",tuple_elts([subscript_value_slice("g",I_07)])))]))]))
%~
%~ replacing_with_var( for_target_iter_body( "j",
%~                       call_func_args("range",["w"]),
%~                       body_stmts( [ if_test_body(
%~                                        compare_ops_left_comparators(eq_token(==),bin_op_left_right(mod_token('%'),"j",FACTOR_02),0),
%~                                        body_stmts( [ assign_targets_value( ["r"],
%~                                                        bin_op_left_right( add_token(+),
%~                                                          "r",
%~                                                          tuple_elts([subscript_value_slice(subscript_value_slice(GRID_01,I_05),"j")])))]))])))
%~
%~ replacing_with_var( if_test_body(
%~                        compare_ops_left_comparators(eq_token(==),bin_op_left_right(mod_token('%'),I_07,FACTOR_02),0),
%~                        body_stmts( [ assign_targets_value( ["dsg"],
%~                                        bin_op_left_right(add_token(+),"dsg",tuple_elts([subscript_value_slice("g",I_07)])))])))
%~
%~ replacing_with_var(call_func_args("range",["w"]))
%~
%~ replacing_with_var( body_stmts( [ if_test_body(
%~                                      compare_ops_left_comparators(eq_token(==),bin_op_left_right(mod_token('%'),"j",FACTOR_02),0),
%~                                      body_stmts( [ assign_targets_value( ["r"],
%~                                                      bin_op_left_right( add_token(+),
%~                                                        "r",
%~                                                        tuple_elts([subscript_value_slice(subscript_value_slice(GRID_01,I_05),"j")])))]))]))
%~
%~ replacing_with_var(make_new("tuple","r"))
%~
%~ replacing_with_var( compare_ops_left_comparators(eq_token(==),bin_op_left_right(mod_token('%'),I_07,FACTOR_02),0))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value( ["dsg"],
%~                                     bin_op_left_right(add_token(+),"dsg",tuple_elts([subscript_value_slice("g",I_07)])))]))
%~
%~ replacing_with_var( if_test_body(
%~                        compare_ops_left_comparators(eq_token(==),bin_op_left_right(mod_token('%'),"j",FACTOR_02),0),
%~                        body_stmts( [ assign_targets_value( ["r"],
%~                                        bin_op_left_right( add_token(+),
%~                                          "r",
%~                                          tuple_elts([subscript_value_slice(subscript_value_slice(GRID_01,I_05),"j")])))])))
%~
%~ replacing_with_var( compare_ops_left_comparators(eq_token(==),bin_op_left_right(mod_token('%'),"j",FACTOR_02),0))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value( ["r"],
%~                                     bin_op_left_right( add_token(+),
%~                                       "r",
%~                                       tuple_elts([subscript_value_slice(subscript_value_slice(GRID_01,I_05),"j")])))]))
%~
%~ replacing_with_var(call_func_args(mod_token('%'),[I_07,FACTOR_02]))
%~
%~ replacing_with_var(call_func_args(mod_token('%'),["j",FACTOR_02]))
%~
% Compiled KL-1 for downscale
downscale(GRID_01,FACTOR_02,GRID_03) :-
  willBeType(GRID_03,'Grid') ,
  comment(' downscale grid ') ,
  len(GRID_01,H) ,
  subscript_value_slice(GRID_01,0,ARG_04) ,
  len(ARG_04,W) ,
  make_new("tuple",G) ,
  range(W,ARG_012) ,
  call([op_call(mod_token('%',J,FACTOR_02,ARG_024))]) ,
  compare_ops_left_comparators(eq_token(==),ARG_024,0,ARG_019) ,
  body_stmts(
     [ subscript_value_slice(GRID_01,I_05,ARG_026),
       subscript_value_slice(ARG_026,J,ARG_025),
       tuple_elts(ARG_025,ARG_023),
       call([op_call(add_token(+,R,ARG_023,R))])],
     ARG_020) ,
  if_test_body(ARG_019,ARG_020,ARG_017) ,
  body_stmts([ARG_017],ARG_013) ,
  for_target_iter_body(J,ARG_012,ARG_013,ARG_09) ,
  make_new("tuple",R,ARG_014) ,
  body_stmts(
     [ ARG_014, ARG_09,tuple_elts(R,ARG_010),
       call([op_call(add_token(+,G,ARG_010,G))])],
     ARG_06) ,
  for_each([range(H,I_05)],ARG_06) ,
  len(G,H) ,
  make_new("tuple",DSG) ,
  call([op_call(mod_token('%',I_07,FACTOR_02,ARG_021))]) ,
  compare_ops_left_comparators(eq_token(==),ARG_021,0,ARG_015) ,
  body_stmts(
     [ subscript_value_slice(G,I_07,ARG_022),
       tuple_elts(ARG_022,ARG_018),
       call([op_call(add_token(+,DSG,ARG_018,DSG))])],
     ARG_016) ,
  if_test_body(ARG_015,ARG_016,ARG_011) ,
  body_stmts([ARG_011],ARG_08) ,
  for_each([range(H,I_07)],ARG_08) ,
  call(GRID_03=DSG) ,
  exit_proc(GRID_03).
%~ % Universal AST Pass #0
%~ def( "hconcat",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("a"),argument_name("b")],[argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' concatenate two grids horizontally ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators(
%~                                             bin_op_left_right(add_token(+),"i","j"),
%~                                             [ comprehension_target_iter(tuple_elts(["i","j"]),call_func_args("zip",["a","b"]))])]))])))
%~
%~ replacing_with_var(bin_op_left_right(add_token(+),"i","j"))
%~
%~ replacing_with_var( comprehension_target_iter( tuple_elts(["i","j"]),
%~                       call_func_args("zip",[A_01,B_02])))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var(call_func_args("zip",[A_01,B_02]))
%~
% Compiled KL-1 for hconcat
hconcat(A_01,B_02,GRID_03) :-
  willBeType(GRID_03,'Grid') ,
  comment(' concatenate two grids horizontally ') ,
  call([op_call(add_token(+,I,J,ARG_05))]) ,
  into_tuple(I,J,ARG_07) ,
  zip(A_01,B_02,ARG_08) ,
  comprehension_target_iter(ARG_07,ARG_08,ARG_06) ,
  generator_exp_elt_generators(ARG_05,[ARG_06],ARG_04) ,
  tuple(ARG_04,GRID_03) ,
  exit_proc(GRID_03).
%~ % Universal AST Pass #0
%~ def( "vconcat",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("a"),argument_name("b")],[argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' concatenate two grids vertically ')),
%~                          return_value(bin_op_left_right(add_token(+),"a","b"))])))
%~
% Compiled KL-1 for vconcat
vconcat(A_01,B_02,GRID_03) :-
  willBeType(GRID_03,'Grid') ,
  comment(' concatenate two grids vertically ') ,
  call([op_call(add_token(+,A_01,B_02,GRID_03))]) ,
  exit_proc(GRID_03).
%~ % Universal AST Pass #0
%~ def( "subgrid",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("patch"),
%~           argument_name("grid")],
%~         [argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' smallest subgrid containing object ')),
%~                          return_value( call_func_args( "crop", [
%~                                          "grid",
%~                                          call_func_args("ulcorner",["patch"]),
%~                                          call_func_args("shape",["patch"])]))])))
%~
% Compiled KL-1 for subgrid
subgrid(PATCH_01,GRID_02,GRID_03) :-
  willBeType(GRID_03,'Grid') ,
  comment(' smallest subgrid containing object ') ,
  ulcorner(PATCH_01,ARG_04) ,
  shape(PATCH_01,ARG_05) ,
  crop(GRID_02,ARG_04,ARG_05,GRID_03) ,
  exit_proc(GRID_03).
%~ % Universal AST Pass #0
%~ def( "hsplit",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"),
%~           argument_name("n")],
%~         [argument_type("Tuple")]),
%~      block_statements( [ expr_value(string_value(' split grid horizontally ')),
%~                          assign_targets_value( [tuple_elts(["h","w"])],
%~                            tuple_elts( [ call_func_args("len",["grid"]),
%~                                          bin_op_left_right(floor_div_token(//),call_func_args("len",[subscript_value_slice("grid",0)]),"n")])),
%~                          assign_targets_value( ["offset"],
%~                            compare_ops_left_comparators( not_eq_token('!='),
%~                              bin_op_left_right(mod_token('%'),call_func_args("len",[subscript_value_slice("grid",0)]),"n"),
%~                              0)),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators(
%~                                             call_func_args( "crop", [
%~                                               "grid",
%~                                               tuple_elts( [ 0,
%~                                                             bin_op_left_right( add_token(+),
%~                                                               bin_op_left_right(mult_token(*),"w","i"),
%~                                                               bin_op_left_right(mult_token(*),"i","offset"))]),
%~                                               tuple_elts(["h","w"])]),
%~                                             [ comprehension_target_iter("i",call_func_args("range",["n"]))])]))])))
%~
%~ replacing_with_var( call_func_args( mod_token('%'), [
%~                       call_func_args("len",[subscript_value_slice(GRID_01,0)]),
%~                       N_02]))
%~
%~ replacing_with_var( call_func_args( "crop", [
%~                       GRID_01,
%~                       tuple_elts( [ 0,
%~                                     bin_op_left_right( add_token(+),
%~                                       bin_op_left_right(mult_token(*),"w","i"),
%~                                       bin_op_left_right(mult_token(*),"i","offset"))]),
%~                       tuple_elts(["h","w"])]))
%~
%~ replacing_with_var(comprehension_target_iter("i",call_func_args("range",[N_02])))
%~
%~ replacing_with_var(call_func_args("range",[N_02]))
%~
% Compiled KL-1 for hsplit
hsplit(GRID_01,N_02,TUPLE_03) :-
  willBeType(TUPLE_03,'Tuple') ,
  comment(' split grid horizontally ') ,
  len(GRID_01,H) ,
  subscript_value_slice(GRID_01,0,ARG_06) ,
  len(ARG_06,ARG_04) ,
  call([op_call(floor_div_token(//,ARG_04,N_02,W))]) ,
  subscript_value_slice(GRID_01,0,ARG_013) ,
  len(ARG_013,ARG_08) ,
  call([op_call(mod_token('%',ARG_08,N_02,ARG_07))]) ,
  compare_ops_left_comparators(not_eq_token('!='),ARG_07,0,OFFSET) ,
  call([op_call(mult_token(*,W,I,ARG_016))]) ,
  call([op_call(mult_token(*,I,OFFSET,ARG_017))]) ,
  call([op_call(add_token(+,ARG_016,ARG_017,ARG_014))]) ,
  tuple_elts(0,ARG_014,ARG_010) ,
  into_tuple(H,W,ARG_011) ,
  crop(GRID_01,ARG_010,ARG_011,ARG_09) ,
  range(N_02,ARG_015) ,
  comprehension_target_iter(I,ARG_015,ARG_012) ,
  generator_exp_elt_generators(ARG_09,[ARG_012],ARG_05) ,
  tuple(ARG_05,TUPLE_03) ,
  exit_proc(TUPLE_03).
%~ % Universal AST Pass #0
%~ def( "vsplit",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"),
%~           argument_name("n")],
%~         [argument_type("Tuple")]),
%~      block_statements( [ expr_value(string_value(' split grid vertically ')),
%~                          assign_targets_value( [tuple_elts(["h","w"])],
%~                            tuple_elts( [ bin_op_left_right(floor_div_token(//),call_func_args("len",["grid"]),"n"),
%~                                          call_func_args("len",[subscript_value_slice("grid",0)])])),
%~                          assign_targets_value( ["offset"],
%~                            compare_ops_left_comparators( not_eq_token('!='),
%~                              bin_op_left_right(mod_token('%'),call_func_args("len",["grid"]),"n"),
%~                              0)),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators(
%~                                             call_func_args( "crop", [
%~                                               "grid",
%~                                               tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"h","i"),bin_op_left_right(mult_token(*),"i","offset")),
%~                                                             0]),
%~                                               tuple_elts(["h","w"])]),
%~                                             [ comprehension_target_iter("i",call_func_args("range",["n"]))])]))])))
%~
%~ replacing_with_var( call_func_args(mod_token('%'),[call_func_args("len",[GRID_01]),N_02]))
%~
%~ replacing_with_var( call_func_args( "crop", [
%~                       GRID_01,
%~                       tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"h","i"),bin_op_left_right(mult_token(*),"i","offset")),
%~                                     0]),
%~                       tuple_elts(["h","w"])]))
%~
%~ replacing_with_var(comprehension_target_iter("i",call_func_args("range",[N_02])))
%~
%~ replacing_with_var(call_func_args("range",[N_02]))
%~
% Compiled KL-1 for vsplit
vsplit(GRID_01,N_02,TUPLE_03) :-
  willBeType(TUPLE_03,'Tuple') ,
  comment(' split grid vertically ') ,
  len(GRID_01,ARG_04) ,
  call([op_call(floor_div_token(//,ARG_04,N_02,H))]) ,
  subscript_value_slice(GRID_01,0,ARG_05) ,
  len(ARG_05,W) ,
  len(GRID_01,ARG_08) ,
  call([op_call(mod_token('%',ARG_08,N_02,ARG_07))]) ,
  compare_ops_left_comparators(not_eq_token('!='),ARG_07,0,OFFSET) ,
  call([op_call(mult_token(*,H,I,ARG_015))]) ,
  call([op_call(mult_token(*,I,OFFSET,ARG_016))]) ,
  call([op_call(add_token(+,ARG_015,ARG_016,ARG_013))]) ,
  tuple_elts(ARG_013,0,ARG_010) ,
  into_tuple(H,W,ARG_011) ,
  crop(GRID_01,ARG_010,ARG_011,ARG_09) ,
  range(N_02,ARG_014) ,
  comprehension_target_iter(I,ARG_014,ARG_012) ,
  generator_exp_elt_generators(ARG_09,[ARG_012],ARG_06) ,
  tuple(ARG_06,TUPLE_03) ,
  exit_proc(TUPLE_03).
%~ % Universal AST Pass #0
%~ def( "cellwise",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"), argument_name("b"),argument_name("fallback")],
%~         [argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' cellwise match of two grids ')),
%~                          assign_targets_value( [tuple_elts(["h","w"])],
%~                            tuple_elts( [ call_func_args("len",["a"]),
%~                                          call_func_args("len",[subscript_value_slice("a",0)])])),
%~                          assign_targets_value(["resulting_grid"],call_func("tuple")),
%~                          for_target_iter_body( "i",
%~                            call_func_args("range",["h"]),
%~                            body_stmts( [ assign_targets_value(["row"],call_func("tuple")),
%~                                          for_target_iter_body( "j",
%~                                            call_func_args("range",["w"]),
%~                                            body_stmts( [ assign_targets_value(["a_value"],subscript_value_slice(subscript_value_slice("a","i"),"j")),
%~                                                          assign_targets_value( ["value"],
%~                                                            if_exp_test_body_orelse(
%~                                                               compare_ops_left_comparators(eq_token(==),"a_value",subscript_value_slice(subscript_value_slice("b","i"),"j")), "a_value","fallback")),
%~                                                          assign_targets_value(["row"],bin_op_left_right(add_token(+),"row",tuple_elts(["value"])))])),
%~                                          assign_targets_value( ["resulting_grid"],
%~                                            bin_op_left_right(add_token(+),"resulting_grid",tuple_elts(["row"])))])),
%~                          return_value("resulting_grid")])))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value(["row"],call_func("tuple")),
%~                                   for_target_iter_body( "j",
%~                                     call_func_args("range",["w"]),
%~                                     body_stmts( [ assign_targets_value( ["a_value"],
%~                                                     subscript_value_slice(subscript_value_slice(A_01,I_06),"j")),
%~                                                   assign_targets_value( ["value"],
%~                                                     if_exp_test_body_orelse(
%~                                                        compare_ops_left_comparators( eq_token(==),
%~                                                          "a_value",
%~                                                          subscript_value_slice(subscript_value_slice(B_02,I_06),"j")), "a_value",FALLBACK_03)),
%~                                                   assign_targets_value(["row"],bin_op_left_right(add_token(+),"row",tuple_elts(["value"])))])),
%~                                   assign_targets_value( ["resulting_grid"],
%~                                     bin_op_left_right(add_token(+),"resulting_grid",tuple_elts(["row"])))]))
%~
%~ replacing_with_var( for_target_iter_body( "j",
%~                       call_func_args("range",["w"]),
%~                       body_stmts( [ assign_targets_value( ["a_value"],
%~                                       subscript_value_slice(subscript_value_slice(A_01,I_06),"j")),
%~                                     assign_targets_value( ["value"],
%~                                       if_exp_test_body_orelse(
%~                                          compare_ops_left_comparators( eq_token(==),
%~                                            "a_value",
%~                                            subscript_value_slice(subscript_value_slice(B_02,I_06),"j")), "a_value",FALLBACK_03)),
%~                                     assign_targets_value(["row"],bin_op_left_right(add_token(+),"row",tuple_elts(["value"])))])))
%~
%~ replacing_with_var(call_func_args("range",["w"]))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value( ["a_value"],
%~                                     subscript_value_slice(subscript_value_slice(A_01,I_06),"j")),
%~                                   assign_targets_value( ["value"],
%~                                     if_exp_test_body_orelse(
%~                                        compare_ops_left_comparators( eq_token(==),
%~                                          "a_value",
%~                                          subscript_value_slice(subscript_value_slice(B_02,I_06),"j")), "a_value",FALLBACK_03)),
%~                                   assign_targets_value(["row"],bin_op_left_right(add_token(+),"row",tuple_elts(["value"])))]))
%~
%~ replacing_with_var(make_new("tuple","row"))
%~
%~ replacing_with_var( compare_ops_left_comparators( eq_token(==),
%~                       "a_value",
%~                       subscript_value_slice(subscript_value_slice(B_02,I_06),"j")))
%~
%~ replacing_with_var(subscript_value_slice(subscript_value_slice(B_02,I_06),"j"))
%~
% Compiled KL-1 for cellwise
cellwise(A_01,B_02,FALLBACK_03,GRID_04) :-
  willBeType(GRID_04,'Grid') ,
  comment(' cellwise match of two grids ') ,
  len(A_01,H) ,
  subscript_value_slice(A_01,0,ARG_05) ,
  len(ARG_05,W) ,
  make_new("tuple","resulting_grid") ,
  range(W,ARG_010) ,
  body_stmts(
     [ subscript_value_slice(A_01,I_06,ARG_013),
       subscript_value_slice(ARG_013,J,A_VALUE),
       (/*2*/
         [ subscript_value_slice(B_02,I_06,ARG_017),
           subscript_value_slice(ARG_017,J,ARG_016),
           compare_ops_left_comparators(eq_token(==),"a_value",ARG_016,ARG_015),
           testif(ARG_015)] ->
           call(FALLBACK_03="a_value")),
       tuple_elts(VALUE,ARG_014),
       call([op_call(add_token(+,ROW,ARG_014,ROW))])],
     ARG_011) ,
  for_target_iter_body(J,ARG_010,ARG_011,ARG_08) ,
  make_new("tuple",ROW,ARG_012) ,
  body_stmts(
     [ ARG_012, ARG_08,tuple_elts(ROW,ARG_09),
       call([op_call(add_token(+,RESULTING_GRID,ARG_09,RESULTING_GRID))])],
     ARG_07) ,
  for_each([range(H,I_06)],ARG_07) ,
  call(GRID_04="resulting_grid") ,
  exit_proc(GRID_04).
%~ % Universal AST Pass #0
%~ def( "replace",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"), argument_name("replacee"),argument_name("replacer")],
%~         [argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' color substitution ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators(
%~                                             call_func_args( "tuple", [
%~                                               generator_exp_elt_generators(
%~                                                  if_exp_test_body_orelse(compare_ops_left_comparators(eq_token(==),"v","replacee"),"replacer","v"),
%~                                                  [comprehension_target_iter("v","r")])]),
%~                                             [comprehension_target_iter("r","grid")])]))])))
%~
%~ replacing_with_var( call_func_args( "tuple", [
%~                       generator_exp_elt_generators(
%~                          if_exp_test_body_orelse(compare_ops_left_comparators(eq_token(==),"v",REPLACEE_02),REPLACER_03,"v"),
%~                          [comprehension_target_iter("v","r")])]))
%~
%~ replacing_with_var( if_exp_test_body_orelse(compare_ops_left_comparators(eq_token(==),"v",REPLACEE_02),REPLACER_03,"v"))
%~
%~ replacing_with_var(comprehension_target_iter("v","r"))
%~
%~ replacing_with_var(call_func_args(==,["v",REPLACEE_02]))
%~
% Compiled KL-1 for replace
replace(GRID_01,REPLACEE_02,REPLACER_03,GRID_04) :-
  willBeType(GRID_04,'Grid') ,
  comment(' color substitution ') ,
  (/*2*/
    [ call([op_call(op_call(==,V,REPLACEE_02,ARG_010))]),
      testif(ARG_010)] ->
      true ;
    call(REPLACER_03=V)) ,
  comprehension_target_iter(V,"r",ARG_09) ,
  generator_exp_elt_generators(REPLACER_03,[ARG_09],ARG_07) ,
  tuple(ARG_07,ARG_06) ,
  generator_exp_elt_generators( ARG_06,
    [assign_targets_value1("r",GRID_01)],
    ARG_05) ,
  tuple(ARG_05,GRID_04) ,
  exit_proc(GRID_04).
%~ % Universal AST Pass #0
%~ def( "switch",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"), argument_name("a"),argument_name("b")],
%~         [argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' color switching ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators(
%~                                             call_func_args( "tuple", [
%~                                               generator_exp_elt_generators(
%~                                                  if_exp_test_body_orelse(
%~                                                     bool_op_values( ['python:And'], [
%~                                                       compare_ops_left_comparators(not_eq_token('!='),"v","a"),
%~                                                       compare_ops_left_comparators(not_eq_token('!='),"v","b")]),
%~                                                     "v",
%~                                                     subscript_value_slice(dict_keys_values(["a","b"],["b","a"]),"v")),
%~                                                  [comprehension_target_iter("v","r")])]),
%~                                             [comprehension_target_iter("r","grid")])]))])))
%~
%~ replacing_with_var( call_func_args( "tuple", [
%~                       generator_exp_elt_generators(
%~                          if_exp_test_body_orelse(
%~                             bool_op_values( ['python:And'], [
%~                               compare_ops_left_comparators(not_eq_token('!='),"v",A_02),
%~                               compare_ops_left_comparators(not_eq_token('!='),"v",B_03)]),
%~                             "v",
%~                             subscript_value_slice(
%~                                dict_keys_values([A_02,B_03],[B_03,A_02]),
%~                                "v")),
%~                          [comprehension_target_iter("v","r")])]))
%~
%~ replacing_with_var( if_exp_test_body_orelse(
%~                        bool_op_values( ['python:And'], [
%~                          compare_ops_left_comparators(not_eq_token('!='),"v",A_02),
%~                          compare_ops_left_comparators(not_eq_token('!='),"v",B_03)]),
%~                        "v",
%~                        subscript_value_slice(
%~                           dict_keys_values([A_02,B_03],[B_03,A_02]),
%~                           "v")))
%~
%~ replacing_with_var(comprehension_target_iter("v","r"))
%~
%~ replacing_with_var(call_func_args('!=',["v",A_02]))
%~
%~ replacing_with_var(call_func_args('!=',["v",B_03]))
%~
% Compiled KL-1 for switch
switch(GRID_01,A_02,B_03,GRID_04) :-
  willBeType(GRID_04,'Grid') ,
  comment(' color switching ') ,
  (/*2*/
    testif( [ call([op_call(op_call('!=',V,A_02,ARG_011))]),
              call([op_call(op_call('!=',V,B_03,ARG_012))]),
              bool_op_values(['python:And'],[ARG_011,ARG_012])]) ->
      true ;
    [ dict_keys_values( [A_02,B_03],
        [B_03,A_02],
        ARG_010),
      subscript_value_slice(ARG_010,V,V)]) ,
  comprehension_target_iter(V,"r",ARG_09) ,
  generator_exp_elt_generators(V,[ARG_09],ARG_07) ,
  tuple(ARG_07,ARG_06) ,
  generator_exp_elt_generators( ARG_06,
    [assign_targets_value1("r",GRID_01)],
    ARG_05) ,
  tuple(ARG_05,GRID_04) ,
  exit_proc(GRID_04).
%~ % Universal AST Pass #0
%~ def( "center",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
%~      block_statements( [ expr_value(string_value(' center of the patch ')),
%~                          return_value( tuple_elts( [ bin_op_left_right( add_token(+),
%~                                                        call_func_args("uppermost",["patch"]),
%~                                                        bin_op_left_right(floor_div_token(//),call_func_args("height",["patch"]),2)),
%~                                                      bin_op_left_right( add_token(+),
%~                                                        call_func_args("leftmost",["patch"]),
%~                                                        bin_op_left_right(floor_div_token(//),call_func_args("width",["patch"]),2))]))])))
%~
% Compiled KL-1 for center
center(PATCH_01,INTEGERTUPLE_02) :-
  willBeType(INTEGERTUPLE_02,'IntegerTuple') ,
  comment(' center of the patch ') ,
  uppermost(PATCH_01,ARG_05) ,
  height(PATCH_01,ARG_09) ,
  call([op_call(floor_div_token(//,ARG_09,2,ARG_06))]) ,
  call([op_call(add_token(+,ARG_05,ARG_06,ARG_03))]) ,
  leftmost(PATCH_01,ARG_07) ,
  width(PATCH_01,ARG_010) ,
  call([op_call(floor_div_token(//,ARG_010,2,ARG_08))]) ,
  call([op_call(add_token(+,ARG_07,ARG_08,ARG_04))]) ,
  tuple_elts(ARG_03,ARG_04,INTEGERTUPLE_02) ,
  exit_proc(INTEGERTUPLE_02).
%~ % Universal AST Pass #0
%~ def( "position",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("IntegerTuple")]),
%~      block_statements( [ expr_value(string_value(' relative position between two patches ')),
%~                          assign_targets_value( [tuple_elts(["ia","ja"])],
%~                            call_func_args("center",[call_func_args("toindices",["a"])])),
%~                          assign_targets_value( [tuple_elts(["ib","jb"])],
%~                            call_func_args("center",[call_func_args("toindices",["b"])])),
%~                          if_test_body_orelse(
%~                             compare_ops_left_comparators(eq_token(==),"ia","ib"),
%~                             body_stmts( [ return_value( tuple_elts( [ 0,
%~                                                                       if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))]),
%~                             orelse_else_stmts( [ if_test_body_orelse(
%~                                                     compare_ops_left_comparators(eq_token(==),"ja","jb"),
%~                                                     body_stmts( [ return_value( tuple_elts( [ if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ia","ib"),1,unary_op_operand(us_ub_token(-),1)),
%~                                                                                               0]))]),
%~                                                     orelse_else_stmts( [ if_test_body_orelse(
%~                                                                             compare_ops_left_comparators(lt_token(<),"ia","ib"),
%~                                                                             body_stmts( [ return_value( tuple_elts( [ 1,
%~                                                                                                                       if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))]),
%~                                                                             orelse_else_stmts( [ if_test_body(
%~                                                                                                     compare_ops_left_comparators(gt_token(>),"ia","ib"),
%~                                                                                                     body_stmts( [ return_value( tuple_elts( [ unary_op_operand(us_ub_token(-),1),
%~                                                                                                                                               if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))]))]))]))]))])))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body_orelse(
%~                                             compare_ops_left_comparators(eq_token(==),"ja","jb"),
%~                                             body_stmts( [ return_value( tuple_elts( [ if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ia","ib"),1,unary_op_operand(us_ub_token(-),1)),
%~                                                                                       0]))]),
%~                                             orelse_else_stmts( [ if_test_body_orelse(
%~                                                                     compare_ops_left_comparators(lt_token(<),"ia","ib"),
%~                                                                     body_stmts( [ return_value( tuple_elts( [ 1,
%~                                                                                                               if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))]),
%~                                                                     orelse_else_stmts( [ if_test_body(
%~                                                                                             compare_ops_left_comparators(gt_token(>),"ia","ib"),
%~                                                                                             body_stmts( [ return_value( tuple_elts( [ unary_op_operand(us_ub_token(-),1),
%~                                                                                                                                       if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))]))]))]))]))
%~
%~ replacing_with_var( body_stmts( [ return_value( tuple_elts( [ 0,
%~                                                               if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))]))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),"ia","ib"))
%~
%~ replacing_with_var( if_test_body_orelse(
%~                        compare_ops_left_comparators(eq_token(==),"ja","jb"),
%~                        body_stmts( [ return_value( tuple_elts( [ if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ia","ib"),1,unary_op_operand(us_ub_token(-),1)),
%~                                                                  0]))]),
%~                        orelse_else_stmts( [ if_test_body_orelse(
%~                                                compare_ops_left_comparators(lt_token(<),"ia","ib"),
%~                                                body_stmts( [ return_value( tuple_elts( [ 1,
%~                                                                                          if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))]),
%~                                                orelse_else_stmts( [ if_test_body(
%~                                                                        compare_ops_left_comparators(gt_token(>),"ia","ib"),
%~                                                                        body_stmts( [ return_value( tuple_elts( [ unary_op_operand(us_ub_token(-),1),
%~                                                                                                                  if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))]))]))])))
%~
%~ replacing_with_var( return_value( tuple_elts( [ 0,
%~                                                 if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))])))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),"ja","jb"))
%~
%~ replacing_with_var( body_stmts( [ return_value( tuple_elts( [ if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ia","ib"),1,unary_op_operand(us_ub_token(-),1)),
%~                                                               0]))]))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body_orelse(
%~                                             compare_ops_left_comparators(lt_token(<),"ia","ib"),
%~                                             body_stmts( [ return_value( tuple_elts( [ 1,
%~                                                                                       if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))]),
%~                                             orelse_else_stmts( [ if_test_body(
%~                                                                     compare_ops_left_comparators(gt_token(>),"ia","ib"),
%~                                                                     body_stmts( [ return_value( tuple_elts( [ unary_op_operand(us_ub_token(-),1),
%~                                                                                                               if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))]))]))]))
%~
%~ replacing_with_var( tuple_elts( [ 0,
%~                                   if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))
%~
%~ replacing_with_var( return_value( tuple_elts( [ if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ia","ib"),1,unary_op_operand(us_ub_token(-),1)),
%~                                                 0])))
%~
%~ replacing_with_var( if_test_body_orelse(
%~                        compare_ops_left_comparators(lt_token(<),"ia","ib"),
%~                        body_stmts( [ return_value( tuple_elts( [ 1,
%~                                                                  if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))]),
%~                        orelse_else_stmts( [ if_test_body(
%~                                                compare_ops_left_comparators(gt_token(>),"ia","ib"),
%~                                                body_stmts( [ return_value( tuple_elts( [ unary_op_operand(us_ub_token(-),1),
%~                                                                                          if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))]))])))
%~
%~ replacing_with_var( tuple_elts( [ if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ia","ib"),1,unary_op_operand(us_ub_token(-),1)),
%~                                   0]))
%~
%~ replacing_with_var(compare_ops_left_comparators(lt_token(<),"ia","ib"))
%~
%~ replacing_with_var( body_stmts( [ return_value( tuple_elts( [ 1,
%~                                                               if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))]))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body(
%~                                             compare_ops_left_comparators(gt_token(>),"ia","ib"),
%~                                             body_stmts( [ return_value( tuple_elts( [ unary_op_operand(us_ub_token(-),1),
%~                                                                                       if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))]))]))
%~
%~ replacing_with_var(compare_ops_left_comparators(lt_token(<),"ja","jb"))
%~
%~ replacing_with_var( return_value( tuple_elts( [ 1,
%~                                                 if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))])))
%~
%~ replacing_with_var( if_test_body(
%~                        compare_ops_left_comparators(gt_token(>),"ia","ib"),
%~                        body_stmts( [ return_value( tuple_elts( [ unary_op_operand(us_ub_token(-),1),
%~                                                                  if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))])))
%~
%~ replacing_with_var(compare_ops_left_comparators(lt_token(<),"ia","ib"))
%~
%~ replacing_with_var( tuple_elts( [ 1,
%~                                   if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))
%~
%~ replacing_with_var(compare_ops_left_comparators(gt_token(>),"ia","ib"))
%~
%~ replacing_with_var( body_stmts( [ return_value( tuple_elts( [ unary_op_operand(us_ub_token(-),1),
%~                                                               if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))]))
%~
%~ replacing_with_var( return_value( tuple_elts( [ unary_op_operand(us_ub_token(-),1),
%~                                                 if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))])))
%~
%~ replacing_with_var(compare_ops_left_comparators(lt_token(<),"ja","jb"))
%~
%~ replacing_with_var( tuple_elts( [ unary_op_operand(us_ub_token(-),1),
%~                                   if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"ja","jb"),1,unary_op_operand(us_ub_token(-),1))]))
%~
%~ replacing_with_var(compare_ops_left_comparators(lt_token(<),"ja","jb"))
%~
% Compiled KL-1 for position
position(A_01,B_02,INTEGERTUPLE_03) :-
  willBeType(INTEGERTUPLE_03,'IntegerTuple') ,
  comment(' relative position between two patches ') ,
  toindices(A_01,ARG_04) ,
  into_tuple(IA,JA,ARG_05) ,
  center(ARG_04,ARG_05) ,
  toindices(B_02,ARG_06) ,
  into_tuple(IB,JB,ARG_07) ,
  center(ARG_06,ARG_07) ,
  compare_ops_left_comparators(eq_token(==),JA,JB,ARG_013) ,
  (/*2*/
    [ compare_ops_left_comparators(lt_token(<),IA,IB,ARG_028),
      testif(ARG_028)] ->
      call(ARG_021=1) ;
    [ unary_op_operand(us_ub_token(-),1,ARG_021)]) ,
  tuple_elts(ARG_021,0,ARG_020) ,
  return_value(ARG_020,ARG_018) ,
  body_stmts([ARG_018],ARG_014) ,
  compare_ops_left_comparators(lt_token(<),IA,IB,ARG_022) ,
  (/*2*/
    [ compare_ops_left_comparators(lt_token(<),JA,JB,ARG_034),
      testif(ARG_034)] ->
      call(ARG_030=1) ;
    [ unary_op_operand(us_ub_token(-),1,ARG_030)]) ,
  tuple_elts(1,ARG_030,ARG_029) ,
  return_value(ARG_029,ARG_026) ,
  body_stmts([ARG_026],ARG_023) ,
  compare_ops_left_comparators(gt_token(>),IA,IB,ARG_031) ,
  unary_op_operand(us_ub_token(-),1,ARG_036) ,
  (/*2*/
    [ compare_ops_left_comparators(lt_token(<),JA,JB,ARG_038),
      testif(ARG_038)] ->
      call(ARG_037=1) ;
    [ unary_op_operand(us_ub_token(-),1,ARG_037)]) ,
  tuple_elts(ARG_036,ARG_037,ARG_035) ,
  return_value(ARG_035,ARG_033) ,
  body_stmts([ARG_033],ARG_032) ,
  if_test_body(ARG_031,ARG_032,ARG_027) ,
  orelse_else_stmts([ARG_027],ARG_024) ,
  if_test_body_orelse(ARG_022,ARG_023,ARG_024,ARG_019) ,
  orelse_else_stmts([ARG_019],ARG_015) ,
  if_test_body_orelse(ARG_013,ARG_014,ARG_015,ARG_011) ,
  orelse_else_stmts([ARG_011],ARG_08) ,
  (/*2*/
    [ (/*2*/
        [ compare_ops_left_comparators(lt_token(<),JA,JB,ARG_025),
          testif(ARG_025)] ->
          call(ARG_017=1) ;
        [ unary_op_operand(us_ub_token(-),1,ARG_017)]),
      tuple_elts(0,ARG_017,ARG_016),
      return_value(ARG_016,ARG_012),
      body_stmts([ARG_012],ARG_09),
      [ compare_ops_left_comparators(eq_token(==),IA,IB,ARG_010),
        testif(ARG_010)] ->
        ARG_09] ;
    ARG_08).
%~ % Universal AST Pass #0
%~ def( "index",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"),
%~           argument_name("loc")],
%~         [argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' color at location ')),
%~                          assign_targets_value([tuple_elts(["i","j"])],"loc"),
%~                          assign_targets_value( [tuple_elts(["h","w"])],
%~                            tuple_elts( [ call_func_args("len",["grid"]),
%~                                          call_func_args("len",[subscript_value_slice("grid",0)])])),
%~                          if_test_body(
%~                             unary_op_operand( ['python:Not'],
%~                               bool_op_values( ['python:And'], [
%~                                 compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                                 compare_ops_left_comparators(
%~                                    ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])),
%~                             body_stmts([return_value(none_literal_value_token('None','None'))])),
%~                          return_value( subscript_value_slice(subscript_value_slice("grid",subscript_value_slice("loc",0)),subscript_value_slice("loc",1)))])))
%~
%~ replacing_with_var(body_stmts([return_value(none_literal_value_token('None','None'))]))
%~
%~ replacing_with_var( unary_op_operand( ['python:Not'],
%~                       bool_op_values( ['python:And'], [
%~                         compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
%~                         compare_ops_left_comparators(
%~                            ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])))
%~
%~ replacing_with_var(return_value(none_literal_value_token('None','None')))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["j","w"])))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["i","h"]))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["j","w"]))
%~
% Compiled KL-1 for index
index(GRID_01,LOC_02,INTEGER_03) :-
  willBeType(INTEGER_03,'Integer') ,
  comment(' color at location ') ,
  from_tuple(LOC_02,I,J) ,
  len(GRID_01,H) ,
  subscript_value_slice(GRID_01,0,ARG_04) ,
  len(ARG_04,W) ,
  (/*2*/
    [ return_value(none_literal_value_token('None','None'),ARG_09),
      body_stmts([ARG_09],ARG_05),
      [ unary_op_operand( ['python:Not'],
          [ ops([lt_e_token(<=),lt_token(<)],ARG_013),
            comparators([I,H],ARG_014),
            compare_ops_left_comparators(ARG_013,0,ARG_014,ARG_010),
            ops([lt_e_token(<=),lt_token(<)],ARG_015),
            comparators([J,W],ARG_016),
            compare_ops_left_comparators(ARG_015,0,ARG_016,ARG_011),
            bool_op_values(['python:And'],[ARG_010,ARG_011])],
          ARG_06),
        testif(ARG_06)] ->
        ARG_05]) ,
  subscript_value_slice(LOC_02,0,ARG_012) ,
  subscript_value_slice(GRID_01,ARG_012,ARG_07) ,
  subscript_value_slice(LOC_02,1,ARG_08) ,
  subscript_value_slice(ARG_07,ARG_08,INTEGER_03) ,
  exit_proc(INTEGER_03).
%~ % Universal AST Pass #0
%~ def( "canvas",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("value"),
%~           argument_name("dimensions")],
%~         [argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' grid construction ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators(
%~                                             call_func_args( "tuple", [
%~                                               generator_exp_elt_generators( "value", [
%~                                                 comprehension_target_iter("j",call_func_args("range",[subscript_value_slice("dimensions",1)]))])]),
%~                                             [ comprehension_target_iter("i",call_func_args("range",[subscript_value_slice("dimensions",0)]))])]))])))
%~
%~ replacing_with_var( call_func_args( "tuple", [
%~                       generator_exp_elt_generators( VALUE_01, [
%~                         comprehension_target_iter("j",call_func_args("range",[subscript_value_slice(DIMENSIONS_02,1)]))])]))
%~
%~ replacing_with_var( comprehension_target_iter("i",call_func_args("range",[subscript_value_slice(DIMENSIONS_02,0)])))
%~
%~ replacing_with_var(call_func_args("range",[subscript_value_slice(DIMENSIONS_02,0)]))
%~
%~ replacing_with_var( comprehension_target_iter("j",call_func_args("range",[subscript_value_slice(DIMENSIONS_02,1)])))
%~
%~ replacing_with_var(call_func_args("range",[subscript_value_slice(DIMENSIONS_02,1)]))
%~
% Compiled KL-1 for canvas
canvas(VALUE_01,DIMENSIONS_02,GRID_03) :-
  willBeType(GRID_03,'Grid') ,
  comment(' grid construction ') ,
  subscript_value_slice(DIMENSIONS_02,1,ARG_012) ,
  range(ARG_012,ARG_011) ,
  comprehension_target_iter("j",ARG_011,ARG_010) ,
  generator_exp_elt_generators(VALUE_01,[ARG_010],ARG_06) ,
  tuple(ARG_06,ARG_05) ,
  subscript_value_slice(DIMENSIONS_02,0,ARG_09) ,
  range(ARG_09,ARG_08) ,
  comprehension_target_iter("i",ARG_08,ARG_07) ,
  generator_exp_elt_generators(ARG_05,[ARG_07],ARG_04) ,
  tuple(ARG_04,GRID_03) ,
  exit_proc(GRID_03).
%~ % Universal AST Pass #0
%~ def( "corners",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' indices of corners ')),
%~                          return_value( call_func_args( "frozenset", [
%~                                          set_elts( [ call_func_args("ulcorner",["patch"]),
%~                                                      call_func_args("urcorner",["patch"]),
%~                                                      call_func_args("llcorner",["patch"]),
%~                                                      call_func_args("lrcorner",["patch"])])]))])))
%~
%~ replacing_with_var(call_func_args("ulcorner",[PATCH_01]))
%~
%~ replacing_with_var(call_func_args("urcorner",[PATCH_01]))
%~
%~ replacing_with_var(call_func_args("llcorner",[PATCH_01]))
%~
%~ replacing_with_var(call_func_args("lrcorner",[PATCH_01]))
%~
% Compiled KL-1 for corners
corners(PATCH_01,INDICES_02) :-
  willBeType(INDICES_02,'Indices') ,
  comment(' indices of corners ') ,
  ulcorner(PATCH_01,ARG_04) ,
  urcorner(PATCH_01,ARG_05) ,
  llcorner(PATCH_01,ARG_06) ,
  lrcorner(PATCH_01,ARG_07) ,
  set_elts( [ARG_04,ARG_05,ARG_06,ARG_07],
    ARG_03) ,
  frozenset(ARG_03,INDICES_02) ,
  exit_proc(INDICES_02).
%~ % Universal AST Pass #0
%~ def( "connect",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("a"),
%~           argument_name("b")],
%~         [argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' line between two points ')),
%~                          assign_targets_value([tuple_elts(["ai","aj"])],"a"),
%~                          assign_targets_value([tuple_elts(["bi","bj"])],"b"),
%~                          assign_targets_value(["si"],call_func_args("min",["ai","bi"])),
%~                          assign_targets_value( ["ei"],
%~                            bin_op_left_right(add_token(+),call_func_args("max",["ai","bi"]),1)),
%~                          assign_targets_value(["sj"],call_func_args("min",["aj","bj"])),
%~                          assign_targets_value( ["ej"],
%~                            bin_op_left_right(add_token(+),call_func_args("max",["aj","bj"]),1)),
%~                          if_test_body_orelse(
%~                             compare_ops_left_comparators(eq_token(==),"ai","bi"),
%~                             body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                           generator_exp_elt_generators( tuple_elts(["ai","j"]), [
%~                                                             comprehension_target_iter("j",call_func_args("range",["sj","ej"]))])]))]),
%~                             orelse_else_stmts( [ if_test_body_orelse(
%~                                                     compare_ops_left_comparators(eq_token(==),"aj","bj"),
%~                                                     body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                                                   generator_exp_elt_generators( tuple_elts(["i","aj"]), [
%~                                                                                     comprehension_target_iter("i",call_func_args("range",["si","ei"]))])]))]),
%~                                                     orelse_else_stmts( [ if_test_body_orelse(
%~                                                                             compare_ops_left_comparators( eq_token(==),
%~                                                                               bin_op_left_right(sub_token(-),"bi","ai"),
%~                                                                               bin_op_left_right(sub_token(-),"bj","aj")),
%~                                                                             body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                                                                           generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                                                                                             comprehension_target_iter( tuple_elts(["i","j"]),
%~                                                                                                               call_func_args( "zip", [
%~                                                                                                                 call_func_args("range",["si","ei"]),
%~                                                                                                                 call_func_args("range",["sj","ej"])]))])]))]),
%~                                                                             orelse_else_stmts( [ if_test_body(
%~                                                                                                     compare_ops_left_comparators( eq_token(==),
%~                                                                                                       bin_op_left_right(sub_token(-),"bi","ai"),
%~                                                                                                       bin_op_left_right(sub_token(-),"aj","bj")),
%~                                                                                                     body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                                                                                                   generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                                                                                                                     comprehension_target_iter( tuple_elts(["i","j"]),
%~                                                                                                                                       call_func_args( "zip", [
%~                                                                                                                                         call_func_args("range",["si","ei"]),
%~                                                                                                                                         call_func_args( "range", [
%~                                                                                                                                           bin_op_left_right(sub_token(-),"ej",1),
%~                                                                                                                                           bin_op_left_right(sub_token(-),"sj",1),
%~                                                                                                                                           unary_op_operand(us_ub_token(-),1)])]))])]))]))]))]))])),
%~                          return_value(call_func("frozenset"))])))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body_orelse(
%~                                             compare_ops_left_comparators(eq_token(==),"aj","bj"),
%~                                             body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                                           generator_exp_elt_generators( tuple_elts(["i","aj"]), [
%~                                                                             comprehension_target_iter("i",call_func_args("range",["si","ei"]))])]))]),
%~                                             orelse_else_stmts( [ if_test_body_orelse(
%~                                                                     compare_ops_left_comparators( eq_token(==),
%~                                                                       bin_op_left_right(sub_token(-),"bi","ai"),
%~                                                                       bin_op_left_right(sub_token(-),"bj","aj")),
%~                                                                     body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                                                                   generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                                                                                     comprehension_target_iter( tuple_elts(["i","j"]),
%~                                                                                                       call_func_args( "zip", [
%~                                                                                                         call_func_args("range",["si","ei"]),
%~                                                                                                         call_func_args("range",["sj","ej"])]))])]))]),
%~                                                                     orelse_else_stmts( [ if_test_body(
%~                                                                                             compare_ops_left_comparators( eq_token(==),
%~                                                                                               bin_op_left_right(sub_token(-),"bi","ai"),
%~                                                                                               bin_op_left_right(sub_token(-),"aj","bj")),
%~                                                                                             body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                                                                                           generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                                                                                                             comprehension_target_iter( tuple_elts(["i","j"]),
%~                                                                                                                               call_func_args( "zip", [
%~                                                                                                                                 call_func_args("range",["si","ei"]),
%~                                                                                                                                 call_func_args( "range", [
%~                                                                                                                                   bin_op_left_right(sub_token(-),"ej",1),
%~                                                                                                                                   bin_op_left_right(sub_token(-),"sj",1),
%~                                                                                                                                   unary_op_operand(us_ub_token(-),1)])]))])]))]))]))]))]))
%~
%~ replacing_with_var( body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                   generator_exp_elt_generators( tuple_elts(["ai","j"]), [
%~                                                     comprehension_target_iter("j",call_func_args("range",["sj","ej"]))])]))]))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),"ai","bi"))
%~
%~ replacing_with_var( if_test_body_orelse(
%~                        compare_ops_left_comparators(eq_token(==),"aj","bj"),
%~                        body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                      generator_exp_elt_generators( tuple_elts(["i","aj"]), [
%~                                                        comprehension_target_iter("i",call_func_args("range",["si","ei"]))])]))]),
%~                        orelse_else_stmts( [ if_test_body_orelse(
%~                                                compare_ops_left_comparators( eq_token(==),
%~                                                  bin_op_left_right(sub_token(-),"bi","ai"),
%~                                                  bin_op_left_right(sub_token(-),"bj","aj")),
%~                                                body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                                              generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                                                                comprehension_target_iter( tuple_elts(["i","j"]),
%~                                                                                  call_func_args( "zip", [
%~                                                                                    call_func_args("range",["si","ei"]),
%~                                                                                    call_func_args("range",["sj","ej"])]))])]))]),
%~                                                orelse_else_stmts( [ if_test_body(
%~                                                                        compare_ops_left_comparators( eq_token(==),
%~                                                                          bin_op_left_right(sub_token(-),"bi","ai"),
%~                                                                          bin_op_left_right(sub_token(-),"aj","bj")),
%~                                                                        body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                                                                      generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                                                                                        comprehension_target_iter( tuple_elts(["i","j"]),
%~                                                                                                          call_func_args( "zip", [
%~                                                                                                            call_func_args("range",["si","ei"]),
%~                                                                                                            call_func_args( "range", [
%~                                                                                                              bin_op_left_right(sub_token(-),"ej",1),
%~                                                                                                              bin_op_left_right(sub_token(-),"sj",1),
%~                                                                                                              unary_op_operand(us_ub_token(-),1)])]))])]))]))]))])))
%~
%~ replacing_with_var( return_value( call_func_args( "frozenset", [
%~                                     generator_exp_elt_generators( tuple_elts(["ai","j"]), [
%~                                       comprehension_target_iter("j",call_func_args("range",["sj","ej"]))])])))
%~
%~ replacing_with_var(compare_ops_left_comparators(eq_token(==),"aj","bj"))
%~
%~ replacing_with_var( body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                   generator_exp_elt_generators( tuple_elts(["i","aj"]), [
%~                                                     comprehension_target_iter("i",call_func_args("range",["si","ei"]))])]))]))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body_orelse(
%~                                             compare_ops_left_comparators( eq_token(==),
%~                                               bin_op_left_right(sub_token(-),"bi","ai"),
%~                                               bin_op_left_right(sub_token(-),"bj","aj")),
%~                                             body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                                           generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                                                             comprehension_target_iter( tuple_elts(["i","j"]),
%~                                                                               call_func_args( "zip", [
%~                                                                                 call_func_args("range",["si","ei"]),
%~                                                                                 call_func_args("range",["sj","ej"])]))])]))]),
%~                                             orelse_else_stmts( [ if_test_body(
%~                                                                     compare_ops_left_comparators( eq_token(==),
%~                                                                       bin_op_left_right(sub_token(-),"bi","ai"),
%~                                                                       bin_op_left_right(sub_token(-),"aj","bj")),
%~                                                                     body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                                                                   generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                                                                                     comprehension_target_iter( tuple_elts(["i","j"]),
%~                                                                                                       call_func_args( "zip", [
%~                                                                                                         call_func_args("range",["si","ei"]),
%~                                                                                                         call_func_args( "range", [
%~                                                                                                           bin_op_left_right(sub_token(-),"ej",1),
%~                                                                                                           bin_op_left_right(sub_token(-),"sj",1),
%~                                                                                                           unary_op_operand(us_ub_token(-),1)])]))])]))]))]))]))
%~
%~ replacing_with_var( call_func_args( "frozenset", [
%~                       generator_exp_elt_generators( tuple_elts(["ai","j"]), [
%~                         comprehension_target_iter("j",call_func_args("range",["sj","ej"]))])]))
%~
%~ replacing_with_var( return_value( call_func_args( "frozenset", [
%~                                     generator_exp_elt_generators( tuple_elts(["i","aj"]), [
%~                                       comprehension_target_iter("i",call_func_args("range",["si","ei"]))])])))
%~
%~ replacing_with_var( if_test_body_orelse(
%~                        compare_ops_left_comparators( eq_token(==),
%~                          bin_op_left_right(sub_token(-),"bi","ai"),
%~                          bin_op_left_right(sub_token(-),"bj","aj")),
%~                        body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                      generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                                        comprehension_target_iter( tuple_elts(["i","j"]),
%~                                                          call_func_args( "zip", [
%~                                                            call_func_args("range",["si","ei"]),
%~                                                            call_func_args("range",["sj","ej"])]))])]))]),
%~                        orelse_else_stmts( [ if_test_body(
%~                                                compare_ops_left_comparators( eq_token(==),
%~                                                  bin_op_left_right(sub_token(-),"bi","ai"),
%~                                                  bin_op_left_right(sub_token(-),"aj","bj")),
%~                                                body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                                              generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                                                                comprehension_target_iter( tuple_elts(["i","j"]),
%~                                                                                  call_func_args( "zip", [
%~                                                                                    call_func_args("range",["si","ei"]),
%~                                                                                    call_func_args( "range", [
%~                                                                                      bin_op_left_right(sub_token(-),"ej",1),
%~                                                                                      bin_op_left_right(sub_token(-),"sj",1),
%~                                                                                      unary_op_operand(us_ub_token(-),1)])]))])]))]))])))
%~
%~ replacing_with_var( call_func_args( "frozenset", [
%~                       generator_exp_elt_generators( tuple_elts(["i","aj"]), [
%~                         comprehension_target_iter("i",call_func_args("range",["si","ei"]))])]))
%~
%~ replacing_with_var( compare_ops_left_comparators( eq_token(==),
%~                       bin_op_left_right(sub_token(-),"bi","ai"),
%~                       bin_op_left_right(sub_token(-),"bj","aj")))
%~
%~ replacing_with_var( body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                   generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                                     comprehension_target_iter( tuple_elts(["i","j"]),
%~                                                       call_func_args( "zip", [
%~                                                         call_func_args("range",["si","ei"]),
%~                                                         call_func_args("range",["sj","ej"])]))])]))]))
%~
%~ replacing_with_var( orelse_else_stmts( [ if_test_body(
%~                                             compare_ops_left_comparators( eq_token(==),
%~                                               bin_op_left_right(sub_token(-),"bi","ai"),
%~                                               bin_op_left_right(sub_token(-),"aj","bj")),
%~                                             body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                                           generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                                                             comprehension_target_iter( tuple_elts(["i","j"]),
%~                                                                               call_func_args( "zip", [
%~                                                                                 call_func_args("range",["si","ei"]),
%~                                                                                 call_func_args( "range", [
%~                                                                                   bin_op_left_right(sub_token(-),"ej",1),
%~                                                                                   bin_op_left_right(sub_token(-),"sj",1),
%~                                                                                   unary_op_operand(us_ub_token(-),1)])]))])]))]))]))
%~
%~ replacing_with_var(tuple_elts(["ai","j"]))
%~
%~ replacing_with_var(comprehension_target_iter("j",call_func_args("range",["sj","ej"])))
%~
%~ replacing_with_var(bin_op_left_right(sub_token(-),"bi","ai"))
%~
%~ replacing_with_var(bin_op_left_right(sub_token(-),"bj","aj"))
%~
%~ replacing_with_var( return_value( call_func_args( "frozenset", [
%~                                     generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                       comprehension_target_iter( tuple_elts(["i","j"]),
%~                                         call_func_args( "zip", [
%~                                           call_func_args("range",["si","ei"]),
%~                                           call_func_args("range",["sj","ej"])]))])])))
%~
%~ replacing_with_var( if_test_body(
%~                        compare_ops_left_comparators( eq_token(==),
%~                          bin_op_left_right(sub_token(-),"bi","ai"),
%~                          bin_op_left_right(sub_token(-),"aj","bj")),
%~                        body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                      generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                                        comprehension_target_iter( tuple_elts(["i","j"]),
%~                                                          call_func_args( "zip", [
%~                                                            call_func_args("range",["si","ei"]),
%~                                                            call_func_args( "range", [
%~                                                              bin_op_left_right(sub_token(-),"ej",1),
%~                                                              bin_op_left_right(sub_token(-),"sj",1),
%~                                                              unary_op_operand(us_ub_token(-),1)])]))])]))])))
%~
%~ replacing_with_var(call_func_args("range",["sj","ej"]))
%~
%~ replacing_with_var(tuple_elts(["i","aj"]))
%~
%~ replacing_with_var(comprehension_target_iter("i",call_func_args("range",["si","ei"])))
%~
%~ replacing_with_var( call_func_args( "frozenset", [
%~                       generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                         comprehension_target_iter( tuple_elts(["i","j"]),
%~                           call_func_args( "zip", [
%~                             call_func_args("range",["si","ei"]),
%~                             call_func_args("range",["sj","ej"])]))])]))
%~
%~ replacing_with_var( compare_ops_left_comparators( eq_token(==),
%~                       bin_op_left_right(sub_token(-),"bi","ai"),
%~                       bin_op_left_right(sub_token(-),"aj","bj")))
%~
%~ replacing_with_var( body_stmts( [ return_value( call_func_args( "frozenset", [
%~                                                   generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                                     comprehension_target_iter( tuple_elts(["i","j"]),
%~                                                       call_func_args( "zip", [
%~                                                         call_func_args("range",["si","ei"]),
%~                                                         call_func_args( "range", [
%~                                                           bin_op_left_right(sub_token(-),"ej",1),
%~                                                           bin_op_left_right(sub_token(-),"sj",1),
%~                                                           unary_op_operand(us_ub_token(-),1)])]))])]))]))
%~
%~ replacing_with_var(call_func_args("range",["si","ei"]))
%~
%~ replacing_with_var(bin_op_left_right(sub_token(-),"bi","ai"))
%~
%~ replacing_with_var(bin_op_left_right(sub_token(-),"aj","bj"))
%~
%~ replacing_with_var( return_value( call_func_args( "frozenset", [
%~                                     generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                       comprehension_target_iter( tuple_elts(["i","j"]),
%~                                         call_func_args( "zip", [
%~                                           call_func_args("range",["si","ei"]),
%~                                           call_func_args( "range", [
%~                                             bin_op_left_right(sub_token(-),"ej",1),
%~                                             bin_op_left_right(sub_token(-),"sj",1),
%~                                             unary_op_operand(us_ub_token(-),1)])]))])])))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var( comprehension_target_iter( tuple_elts(["i","j"]),
%~                       call_func_args( "zip", [
%~                         call_func_args("range",["si","ei"]),
%~                         call_func_args("range",["sj","ej"])])))
%~
%~ replacing_with_var( call_func_args( "frozenset", [
%~                       generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                         comprehension_target_iter( tuple_elts(["i","j"]),
%~                           call_func_args( "zip", [
%~                             call_func_args("range",["si","ei"]),
%~                             call_func_args( "range", [
%~                               bin_op_left_right(sub_token(-),"ej",1),
%~                               bin_op_left_right(sub_token(-),"sj",1),
%~                               unary_op_operand(us_ub_token(-),1)])]))])]))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var( call_func_args( "zip", [
%~                       call_func_args("range",["si","ei"]),
%~                       call_func_args("range",["sj","ej"])]))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var( comprehension_target_iter( tuple_elts(["i","j"]),
%~                       call_func_args( "zip", [
%~                         call_func_args("range",["si","ei"]),
%~                         call_func_args( "range", [
%~                           bin_op_left_right(sub_token(-),"ej",1),
%~                           bin_op_left_right(sub_token(-),"sj",1),
%~                           unary_op_operand(us_ub_token(-),1)])])))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var( call_func_args( "zip", [
%~                       call_func_args("range",["si","ei"]),
%~                       call_func_args( "range", [
%~                         bin_op_left_right(sub_token(-),"ej",1),
%~                         bin_op_left_right(sub_token(-),"sj",1),
%~                         unary_op_operand(us_ub_token(-),1)])]))
%~
% Compiled KL-1 for connect
connect(A_01,B_02,INDICES_03) :-
  willBeType(INDICES_03,'Indices') ,
  comment(' line between two points ') ,
  from_tuple(A_01,AI,AJ) ,
  from_tuple(B_02,BI,BJ) ,
  min(AI,BI,SI) ,
  max(AI,BI,ARG_04) ,
  call([op_call(add_token(+,ARG_04,1,EI))]) ,
  min(AJ,BJ,SJ) ,
  max(AJ,BJ,ARG_05) ,
  call([op_call(add_token(+,ARG_05,1,EJ))]) ,
  compare_ops_left_comparators(eq_token(==),AJ,BJ,ARG_011) ,
  into_tuple(I,AJ,ARG_030) ,
  range(SI,EI,ARG_036) ,
  comprehension_target_iter(I,ARG_036,ARG_031) ,
  generator_exp_elt_generators(ARG_030,[ARG_031],ARG_019) ,
  frozenset(ARG_019,ARG_018) ,
  return_value(ARG_018,ARG_016) ,
  body_stmts([ARG_016],ARG_012) ,
  call([op_call(sub_token(-,BI,AI,ARG_025))]) ,
  call([op_call(sub_token(-,BJ,AJ,ARG_026))]) ,
  compare_ops_left_comparators(eq_token(==),ARG_025,ARG_026,ARG_020) ,
  into_tuple(I,J,ARG_040) ,
  into_tuple(I,J,ARG_044) ,
  range(SI,EI,ARG_046) ,
  range(SJ,EJ,ARG_047) ,
  zip(ARG_046,ARG_047,ARG_045) ,
  comprehension_target_iter(ARG_044,ARG_045,ARG_041) ,
  generator_exp_elt_generators(ARG_040,[ARG_041],ARG_033) ,
  frozenset(ARG_033,ARG_032) ,
  return_value(ARG_032,ARG_027) ,
  body_stmts([ARG_027],ARG_021) ,
  call([op_call(sub_token(-,BI,AI,ARG_037))]) ,
  call([op_call(sub_token(-,AJ,BJ,ARG_038))]) ,
  compare_ops_left_comparators(eq_token(==),ARG_037,ARG_038,ARG_034) ,
  into_tuple(I,J,ARG_048) ,
  into_tuple(I,J,ARG_050) ,
  range(SI,EI,ARG_052) ,
  call([op_call(sub_token(-,EJ,1,ARG_054))]) ,
  call([op_call(sub_token(-,SJ,1,ARG_055))]) ,
  unary_op_operand(us_ub_token(-),1,ARG_056) ,
  range(ARG_054,ARG_055,ARG_056,ARG_053) ,
  zip(ARG_052,ARG_053,ARG_051) ,
  comprehension_target_iter(ARG_050,ARG_051,ARG_049) ,
  generator_exp_elt_generators(ARG_048,[ARG_049],ARG_043) ,
  frozenset(ARG_043,ARG_042) ,
  return_value(ARG_042,ARG_039) ,
  body_stmts([ARG_039],ARG_035) ,
  if_test_body(ARG_034,ARG_035,ARG_028) ,
  orelse_else_stmts([ARG_028],ARG_022) ,
  if_test_body_orelse(ARG_020,ARG_021,ARG_022,ARG_017) ,
  orelse_else_stmts([ARG_017],ARG_013) ,
  if_test_body_orelse(ARG_011,ARG_012,ARG_013,ARG_09) ,
  orelse_else_stmts([ARG_09],ARG_06) ,
  (/*2*/
    [ into_tuple(AI,J,ARG_023),
      range(SJ,EJ,ARG_029),
      comprehension_target_iter(J,ARG_029,ARG_024),
      generator_exp_elt_generators(ARG_023,[ARG_024],ARG_015),
      frozenset(ARG_015,ARG_014),
      return_value(ARG_014,ARG_010),
      body_stmts([ARG_010],ARG_07),
      [ compare_ops_left_comparators(eq_token(==),AI,BI,ARG_08),
        testif(ARG_08)] ->
        ARG_07] ;
    ARG_06) ,
  make_new("frozenset",INDICES_03) ,
  exit_proc(INDICES_03).
%~ % Universal AST Pass #0
%~ def( "cover",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"),
%~           argument_name("patch")],
%~         [argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' remove object from grid ')),
%~                          return_value( call_func_args( "fill", [
%~                                          "grid",
%~                                          call_func_args("mostcolor",["grid"]),
%~                                          call_func_args("toindices",["patch"])]))])))
%~
% Compiled KL-1 for cover
cover(GRID_01,PATCH_02,GRID_03) :-
  willBeType(GRID_03,'Grid') ,
  comment(' remove object from grid ') ,
  mostcolor(GRID_01,ARG_04) ,
  toindices(PATCH_02,ARG_05) ,
  fill(GRID_01,ARG_04,ARG_05,GRID_03) ,
  exit_proc(GRID_03).
%~ % Universal AST Pass #0
%~ def( "trim",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' trim border of grid ')),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators(
%~                                             subscript_value_slice("r",slice_lower_upper(1,unary_op_operand(us_ub_token(-),1))),
%~                                             [ comprehension_target_iter("r",subscript_value_slice("grid",slice_lower_upper(1,unary_op_operand(us_ub_token(-),1))))])]))])))
%~
%~ replacing_with_var(subscript_value_slice("r",slice_lower_upper(1,unary_op_operand(us_ub_token(-),1))))
%~
%~ replacing_with_var( comprehension_target_iter( "r",
%~                       subscript_value_slice(GRID_01,slice_lower_upper(1,unary_op_operand(us_ub_token(-),1)))))
%~
%~ replacing_with_var(subscript_value_slice(GRID_01,slice_lower_upper(1,unary_op_operand(us_ub_token(-),1))))
%~
%~ replacing_with_var(unary_op_operand(us_ub_token(-),1))
%~
%~ replacing_with_var(unary_op_operand(us_ub_token(-),1))
%~
% Compiled KL-1 for trim
trim(GRID_01,GRID_02) :-
  willBeType(GRID_02,'Grid') ,
  comment(' trim border of grid ') ,
  unary_op_operand(us_ub_token(-),1,ARG_09) ,
  slice_lower_upper(1,ARG_09,ARG_05) ,
  subscript_value_slice(R,ARG_05,ARG_04) ,
  unary_op_operand(us_ub_token(-),1,ARG_010) ,
  slice_lower_upper(1,ARG_010,ARG_08) ,
  subscript_value_slice(GRID_01,ARG_08,ARG_07) ,
  comprehension_target_iter(R,ARG_07,ARG_06) ,
  generator_exp_elt_generators(ARG_04,[ARG_06],ARG_03) ,
  tuple(ARG_03,GRID_02) ,
  exit_proc(GRID_02).
%~ % Universal AST Pass #0
%~ def( "move",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"), argument_name("obj"),argument_name("offset")],
%~         [argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' move object on grid ')),
%~                          return_value( call_func_args( "paint", [
%~                                          call_func_args("cover",["grid","obj"]),
%~                                          call_func_args("shift",["obj","offset"])]))])))
%~
% Compiled KL-1 for move
move(GRID_01,OBJ_02,OFFSET_03,GRID_04) :-
  willBeType(GRID_04,'Grid') ,
  comment(' move object on grid ') ,
  cover(GRID_01,OBJ_02,ARG_05) ,
  shift(OBJ_02,OFFSET_03,ARG_06) ,
  paint(ARG_05,ARG_06,GRID_04) ,
  exit_proc(GRID_04).
%~ % Universal AST Pass #0
%~ def( "tophalf",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' upper half of grid ')),
%~                          return_value( subscript_value_slice( "grid",
%~                                          slice_upper(bin_op_left_right(floor_div_token(//),call_func_args("len",["grid"]),2))))])))
%~
%~ replacing_with_var(bin_op_left_right(floor_div_token(//),call_func_args("len",[GRID_01]),2))
%~
% Compiled KL-1 for tophalf
tophalf(GRID_01,GRID_02) :-
  willBeType(GRID_02,'Grid') ,
  comment(' upper half of grid ') ,
  len(GRID_01,ARG_05) ,
  call([op_call(floor_div_token(//,ARG_05,2,ARG_04))]) ,
  slice_upper(ARG_04,ARG_03) ,
  subscript_value_slice(GRID_01,ARG_03,GRID_02) ,
  exit_proc(GRID_02).
%~ % Universal AST Pass #0
%~ def( "bottomhalf",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' lower half of grid ')),
%~                          return_value( subscript_value_slice( "grid",
%~                                          slice_lower( bin_op_left_right( add_token(+),
%~                                                         bin_op_left_right(floor_div_token(//),call_func_args("len",["grid"]),2),
%~                                                         bin_op_left_right(mod_token('%'),call_func_args("len",["grid"]),2)))))])))
%~
%~ replacing_with_var( bin_op_left_right( add_token(+),
%~                       bin_op_left_right(floor_div_token(//),call_func_args("len",[GRID_01]),2),
%~                       bin_op_left_right(mod_token('%'),call_func_args("len",[GRID_01]),2)))
%~
% Compiled KL-1 for bottomhalf
bottomhalf(GRID_01,GRID_02) :-
  willBeType(GRID_02,'Grid') ,
  comment(' lower half of grid ') ,
  len(GRID_01,ARG_07) ,
  call([op_call(floor_div_token(//,ARG_07,2,ARG_05))]) ,
  len(GRID_01,ARG_08) ,
  call([op_call(mod_token('%',ARG_08,2,ARG_06))]) ,
  call([op_call(add_token(+,ARG_05,ARG_06,ARG_04))]) ,
  slice_lower(ARG_04,ARG_03) ,
  subscript_value_slice(GRID_01,ARG_03,GRID_02) ,
  exit_proc(GRID_02).
%~ % Universal AST Pass #0
%~ def( "lefthalf",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' left half of grid ')),
%~                          return_value( call_func_args("rot270",[call_func_args("tophalf",[call_func_args("rot90",["grid"])])]))])))
%~
% Compiled KL-1 for lefthalf
lefthalf(GRID_01,GRID_02) :-
  willBeType(GRID_02,'Grid') ,
  comment(' left half of grid ') ,
  rot90(GRID_01,ARG_04) ,
  tophalf(ARG_04,ARG_03) ,
  rot270(ARG_03,GRID_02) ,
  exit_proc(GRID_02).
%~ % Universal AST Pass #0
%~ def( "righthalf",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' right half of grid ')),
%~                          return_value( call_func_args("rot270",[call_func_args("bottomhalf",[call_func_args("rot90",["grid"])])]))])))
%~
% Compiled KL-1 for righthalf
righthalf(GRID_01,GRID_02) :-
  willBeType(GRID_02,'Grid') ,
  comment(' right half of grid ') ,
  rot90(GRID_01,ARG_04) ,
  bottomhalf(ARG_04,ARG_03) ,
  rot270(ARG_03,GRID_02) ,
  exit_proc(GRID_02).
%~ % Universal AST Pass #0
%~ def( "vfrontier",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("location")],[argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' vertical frontier ')),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators(
%~                                             tuple_elts(["i",subscript_value_slice("location",1)]),
%~                                             [ comprehension_target_iter("i",call_func_args("range",[30]))])]))])))
%~
%~ replacing_with_var(tuple_elts(["i",subscript_value_slice(LOCATION_01,1)]))
%~
%~ replacing_with_var(comprehension_target_iter("i",call_func_args("range",[30])))
%~
%~ replacing_with_var(call_func_args("range",[30]))
%~
% Compiled KL-1 for vfrontier
vfrontier(LOCATION_01,INDICES_02) :-
  willBeType(INDICES_02,'Indices') ,
  comment(' vertical frontier ') ,
  subscript_value_slice(LOCATION_01,1,ARG_05) ,
  tuple_elts(I,ARG_05,ARG_04) ,
  range(30,ARG_07) ,
  comprehension_target_iter(I,ARG_07,ARG_06) ,
  generator_exp_elt_generators(ARG_04,[ARG_06],ARG_03) ,
  frozenset(ARG_03,INDICES_02) ,
  exit_proc(INDICES_02).
%~ % Universal AST Pass #0
%~ def( "hfrontier",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("location")],[argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' horizontal frontier ')),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators(
%~                                             tuple_elts([subscript_value_slice("location",0),"j"]),
%~                                             [ comprehension_target_iter("j",call_func_args("range",[30]))])]))])))
%~
%~ replacing_with_var(tuple_elts([subscript_value_slice(LOCATION_01,0),"j"]))
%~
%~ replacing_with_var(comprehension_target_iter("j",call_func_args("range",[30])))
%~
%~ replacing_with_var(call_func_args("range",[30]))
%~
% Compiled KL-1 for hfrontier
hfrontier(LOCATION_01,INDICES_02) :-
  willBeType(INDICES_02,'Indices') ,
  comment(' horizontal frontier ') ,
  subscript_value_slice(LOCATION_01,0,ARG_05) ,
  tuple_elts(ARG_05,J,ARG_04) ,
  range(30,ARG_07) ,
  comprehension_target_iter(J,ARG_07,ARG_06) ,
  generator_exp_elt_generators(ARG_04,[ARG_06],ARG_03) ,
  frozenset(ARG_03,INDICES_02) ,
  exit_proc(INDICES_02).
%~ % Universal AST Pass #0
%~ def( "backdrop",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' indices in bounding box of patch ')),
%~                          assign_targets_value(["indices"],call_func_args("toindices",["patch"])),
%~                          assign_targets_value([tuple_elts(["si","sj"])],call_func_args("ulcorner",["indices"])),
%~                          assign_targets_value([tuple_elts(["ei","ej"])],call_func_args("lrcorner",["patch"])),
%~                          return_value( call_func_args( "frozenset", [
%~                                          generator_exp_elt_generators( tuple_elts(["i","j"]), [
%~                                            comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)])),
%~                                            comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))])]))])))
%~
%~ replacing_with_var(tuple_elts(["i","j"]))
%~
%~ replacing_with_var( comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)])))
%~
%~ replacing_with_var( comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)])))
%~
%~ replacing_with_var(call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))
%~
%~ replacing_with_var(call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))
%~
% Compiled KL-1 for backdrop
backdrop(PATCH_01,INDICES_02) :-
  willBeType(INDICES_02,'Indices') ,
  comment(' indices in bounding box of patch ') ,
  toindices(PATCH_01,INDICES) ,
  into_tuple(SI,SJ,ARG_03) ,
  ulcorner(INDICES,ARG_03) ,
  into_tuple(EI,EJ,ARG_04) ,
  lrcorner(PATCH_01,ARG_04) ,
  into_tuple(I,J,ARG_06) ,
  call([op_call(add_token(+,EI,1,ARG_010))]) ,
  range(SI,ARG_010,ARG_09) ,
  comprehension_target_iter(I,ARG_09,ARG_07) ,
  call([op_call(add_token(+,EJ,1,ARG_012))]) ,
  range(SJ,ARG_012,ARG_011) ,
  comprehension_target_iter(J,ARG_011,ARG_08) ,
  generator_exp_elt_generators(ARG_06,[ARG_07,ARG_08],ARG_05) ,
  frozenset(ARG_05,INDICES_02) ,
  exit_proc(INDICES_02).
%~ % Universal AST Pass #0
%~ def( "delta",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' indices in bounding box but not part of patch ')),
%~                          return_value( bin_op_left_right( sub_token(-),
%~                                          call_func_args("backdrop",["patch"]),
%~                                          call_func_args("toindices",["patch"])))])))
%~
% Compiled KL-1 for delta
delta(PATCH_01,INDICES_02) :-
  willBeType(INDICES_02,'Indices') ,
  comment(' indices in bounding box but not part of patch ') ,
  backdrop(PATCH_01,ARG_03) ,
  toindices(PATCH_01,ARG_04) ,
  call([op_call(sub_token(-,ARG_03,ARG_04,INDICES_02))]) ,
  exit_proc(INDICES_02).
%~ % Universal AST Pass #0
%~ def( "gravitate",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("source"),
%~           argument_name("destination")],
%~         [argument_type("IntegerTuple")]),
%~      block_statements( [ expr_value(string_value(' direction to move source until adjacent to destination ')),
%~                          assign_targets_value([tuple_elts(["si","sj"])],call_func_args("center",["source"])),
%~                          assign_targets_value( [tuple_elts(["di","dj"])],
%~                            call_func_args("center",["destination"])),
%~                          assign_targets_value([tuple_elts(["i","j"])],tuple_elts([0,0])),
%~                          if_test_body_orelse(
%~                             call_func_args("vmatching",["source","destination"]),
%~                             body_stmts( [ assign_targets_value( ["i"],
%~                                             if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"si","di"),1,unary_op_operand(us_ub_token(-),1)))]),
%~                             orelse_else_stmts( [ assign_targets_value( ["j"],
%~                                                    if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"sj","dj"),1,unary_op_operand(us_ub_token(-),1)))])),
%~                          assign_targets_value([tuple_elts(["gi","gj"])],tuple_elts(["i","j"])),
%~                          assign_targets_value(["c"],0),
%~                          while_test_body(
%~                             bool_op_values( ['python:And'], [
%~                               unary_op_operand(['python:Not'],call_func_args("adjacent",["source","destination"])),
%~                               compare_ops_left_comparators(lt_token(<),"c",42)]),
%~                             body_stmts( [ aug_assign_op_value_target(add_token(+),1,"c"),
%~                                           aug_assign_op_value_target(add_token(+),"i","gi"),
%~                                           aug_assign_op_value_target(add_token(+),"j","gj"),
%~                                           assign_targets_value( ["source"],
%~                                             call_func_args("shift",["source",tuple_elts(["i","j"])]))])),
%~                          return_value( tuple_elts([bin_op_left_right(sub_token(-),"gi","i"),bin_op_left_right(sub_token(-),"gj","j")]))])))
%~
%~ replacing_with_var( orelse_else_stmts( [ assign_targets_value( ["j"],
%~                                            if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"sj","dj"),1,unary_op_operand(us_ub_token(-),1)))]))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value( ["i"],
%~                                     if_exp_test_body_orelse(compare_ops_left_comparators(lt_token(<),"si","di"),1,unary_op_operand(us_ub_token(-),1)))]))
%~
%~ replacing_with_var(call_func_args("vmatching",[SOURCE_01,DESTINATION_02]))
%~
%~ replacing_with_var( body_stmts( [ aug_assign_op_value_target(add_token(+),1,"c"),
%~                                   aug_assign_op_value_target(add_token(+),"i","gi"),
%~                                   aug_assign_op_value_target(add_token(+),"j","gj"),
%~                                   assign_targets_value( [SOURCE_01],
%~                                     call_func_args("shift",[SOURCE_01,tuple_elts(["i","j"])]))]))
%~
%~ replacing_with_var( unary_op_operand( ['python:Not'],
%~                       call_func_args("adjacent",[SOURCE_01,DESTINATION_02])))
%~
%~ replacing_with_var(compare_ops_left_comparators(lt_token(<),"c",42))
%~
%~ replacing_with_var(aug_assign_op_value_target(add_token(+),1,"c"))
%~
%~ replacing_with_var(aug_assign_op_value_target(add_token(+),"i","gi"))
%~
%~ replacing_with_var(aug_assign_op_value_target(add_token(+),"j","gj"))
%~
%~ replacing_with_var(call_func_args("adjacent",[SOURCE_01,DESTINATION_02]))
%~
%~ replacing_with_var(compare_ops_left_comparators(lt_token(<),"sj","dj"))
%~
%~ replacing_with_var(compare_ops_left_comparators(lt_token(<),"si","di"))
%~
% Compiled KL-1 for gravitate
gravitate(SOURCE_01,DESTINATION_02,INTEGERTUPLE_03) :-
  willBeType(INTEGERTUPLE_03,'IntegerTuple') ,
  comment(' direction to move source until adjacent to destination ') ,
  into_tuple(SI,SJ,ARG_04) ,
  center(SOURCE_01,ARG_04) ,
  into_tuple(DI,DJ,ARG_05) ,
  center(DESTINATION_02,ARG_05) ,
  call(I=0) ,
  call(J=0) ,
  orelse_else_stmts(
     [ (/*2*/
         [ compare_ops_left_comparators(lt_token(<),SJ,DJ,ARG_019),
           testif(ARG_019)] ->
           call(J=1) ;
         [ unary_op_operand(us_ub_token(-),1,J)])],
     ARG_06) ,
  (/*2*/
    [ body_stmts(
         [ (/*2*/
             [ compare_ops_left_comparators(lt_token(<),SI,DI,ARG_020),
               testif(ARG_020)] ->
               call(I=1) ;
             [ unary_op_operand(us_ub_token(-),1,I)])],
         ARG_07),
      [ vmatching(SOURCE_01,DESTINATION_02,ARG_08),
        testif(ARG_08)] ->
        ARG_07] ;
    ARG_06) ,
  call("c"=0) ,
  aug_assign_op_value_target(add_token(+),1,"c",ARG_014) ,
  aug_assign_op_value_target(add_token(+),I,I,ARG_015) ,
  aug_assign_op_value_target(add_token(+),J,J,ARG_016) ,
  body_stmts(
     [ ARG_014, ARG_015,ARG_016,
       into_tuple(I,J,ARG_017),
       shift(SOURCE_01,ARG_017,SOURCE_01)],
     ARG_09) ,
  while_test_body(
     [ adjacent(SOURCE_01,DESTINATION_02,ARG_018),
       unary_op_operand(['python:Not'],ARG_018,ARG_010),
       compare_ops_left_comparators(lt_token(<),"c",42,ARG_011),
       bool_op_values(['python:And'],[ARG_010,ARG_011])],
     ARG_09) ,
  call([op_call(sub_token(-,GI,I,ARG_012))]) ,
  call([op_call(sub_token(-,GJ,J,ARG_013))]) ,
  tuple_elts(ARG_012,ARG_013,INTEGERTUPLE_03) ,
  exit_proc(INTEGERTUPLE_03).
%~ % Universal AST Pass #0
%~ def( "inbox",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' inbox for patch ')),
%~                          assign_targets_value( [tuple_elts(["ai","aj"])],
%~                            tuple_elts( [ bin_op_left_right(add_token(+),call_func_args("uppermost",["patch"]),1),
%~                                          bin_op_left_right(add_token(+),call_func_args("leftmost",["patch"]),1)])),
%~                          assign_targets_value( [tuple_elts(["bi","bj"])],
%~                            tuple_elts( [ bin_op_left_right(sub_token(-),call_func_args("lowermost",["patch"]),1),
%~                                          bin_op_left_right(sub_token(-),call_func_args("rightmost",["patch"]),1)])),
%~                          assign_targets_value( [tuple_elts(["si","sj"])],
%~                            tuple_elts( [ call_func_args("min",["ai","bi"]),
%~                                          call_func_args("min",["aj","bj"])])),
%~                          assign_targets_value( [tuple_elts(["ei","ej"])],
%~                            tuple_elts( [ call_func_args("max",["ai","bi"]),
%~                                          call_func_args("max",["aj","bj"])])),
%~                          assign_targets_value( ["vlines"],
%~                            bin_op_left_right( bit_or_token('|'),
%~                              set_comp_elt_generators( tuple_elts(["i","sj"]), [
%~                                comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]),
%~                              set_comp_elt_generators( tuple_elts(["i","ej"]), [
%~                                comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]))),
%~                          assign_targets_value( ["hlines"],
%~                            bin_op_left_right( bit_or_token('|'),
%~                              set_comp_elt_generators( tuple_elts(["si","j"]), [
%~                                comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]),
%~                              set_comp_elt_generators( tuple_elts(["ei","j"]), [
%~                                comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]))),
%~                          return_value(call_func_args("frozenset",[bin_op_left_right(bit_or_token('|'),"vlines","hlines")]))])))
%~
%~ replacing_with_var(tuple_elts(["i","sj"]))
%~
%~ replacing_with_var( comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)])))
%~
%~ replacing_with_var(tuple_elts(["i","ej"]))
%~
%~ replacing_with_var( comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)])))
%~
%~ replacing_with_var(tuple_elts(["si","j"]))
%~
%~ replacing_with_var( comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)])))
%~
%~ replacing_with_var(tuple_elts(["ei","j"]))
%~
%~ replacing_with_var( comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)])))
%~
%~ replacing_with_var(call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))
%~
%~ replacing_with_var(call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))
%~
%~ replacing_with_var(call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))
%~
%~ replacing_with_var(call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))
%~
% Compiled KL-1 for inbox
inbox(PATCH_01,INDICES_02) :-
  willBeType(INDICES_02,'Indices') ,
  comment(' inbox for patch ') ,
  uppermost(PATCH_01,ARG_03) ,
  call([op_call(add_token(+,ARG_03,1,AI))]) ,
  leftmost(PATCH_01,ARG_04) ,
  call([op_call(add_token(+,ARG_04,1,AJ))]) ,
  lowermost(PATCH_01,ARG_05) ,
  call([op_call(sub_token(-,ARG_05,1,BI))]) ,
  rightmost(PATCH_01,ARG_06) ,
  call([op_call(sub_token(-,ARG_06,1,BJ))]) ,
  min(AI,BI,SI) ,
  min(AJ,BJ,SJ) ,
  max(AI,BI,EI) ,
  max(AJ,BJ,EJ) ,
  into_tuple(I,SJ,ARG_012) ,
  call([op_call(add_token(+,EI,1,ARG_021))]) ,
  range(SI,ARG_021,ARG_020) ,
  comprehension_target_iter(I,ARG_020,ARG_013) ,
  set_comp_elt_generators(ARG_012,[ARG_013],ARG_07) ,
  into_tuple(I,EJ,ARG_014) ,
  call([op_call(add_token(+,EI,1,ARG_023))]) ,
  range(SI,ARG_023,ARG_022) ,
  comprehension_target_iter(I,ARG_022,ARG_015) ,
  set_comp_elt_generators(ARG_014,[ARG_015],ARG_08) ,
  call([op_call(bit_or_token('|',ARG_07,ARG_08,VLINES))]) ,
  into_tuple(SI,J,ARG_016) ,
  call([op_call(add_token(+,EJ,1,ARG_025))]) ,
  range(SJ,ARG_025,ARG_024) ,
  comprehension_target_iter(J,ARG_024,ARG_017) ,
  set_comp_elt_generators(ARG_016,[ARG_017],ARG_09) ,
  into_tuple(EI,J,ARG_018) ,
  call([op_call(add_token(+,EJ,1,ARG_027))]) ,
  range(SJ,ARG_027,ARG_026) ,
  comprehension_target_iter(J,ARG_026,ARG_019) ,
  set_comp_elt_generators(ARG_018,[ARG_019],ARG_010) ,
  call([op_call(bit_or_token('|',ARG_09,ARG_010,HLINES))]) ,
  call([op_call(bit_or_token('|',VLINES,HLINES,ARG_011))]) ,
  frozenset(ARG_011,INDICES_02) ,
  exit_proc(INDICES_02).
%~ % Universal AST Pass #0
%~ def( "outbox",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' outbox for patch ')),
%~                          assign_targets_value( [tuple_elts(["ai","aj"])],
%~                            tuple_elts( [ bin_op_left_right(sub_token(-),call_func_args("uppermost",["patch"]),1),
%~                                          bin_op_left_right(sub_token(-),call_func_args("leftmost",["patch"]),1)])),
%~                          assign_targets_value( [tuple_elts(["bi","bj"])],
%~                            tuple_elts( [ bin_op_left_right(add_token(+),call_func_args("lowermost",["patch"]),1),
%~                                          bin_op_left_right(add_token(+),call_func_args("rightmost",["patch"]),1)])),
%~                          assign_targets_value( [tuple_elts(["si","sj"])],
%~                            tuple_elts( [ call_func_args("min",["ai","bi"]),
%~                                          call_func_args("min",["aj","bj"])])),
%~                          assign_targets_value( [tuple_elts(["ei","ej"])],
%~                            tuple_elts( [ call_func_args("max",["ai","bi"]),
%~                                          call_func_args("max",["aj","bj"])])),
%~                          assign_targets_value( ["vlines"],
%~                            bin_op_left_right( bit_or_token('|'),
%~                              set_comp_elt_generators( tuple_elts(["i","sj"]), [
%~                                comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]),
%~                              set_comp_elt_generators( tuple_elts(["i","ej"]), [
%~                                comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]))),
%~                          assign_targets_value( ["hlines"],
%~                            bin_op_left_right( bit_or_token('|'),
%~                              set_comp_elt_generators( tuple_elts(["si","j"]), [
%~                                comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]),
%~                              set_comp_elt_generators( tuple_elts(["ei","j"]), [
%~                                comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]))),
%~                          return_value(call_func_args("frozenset",[bin_op_left_right(bit_or_token('|'),"vlines","hlines")]))])))
%~
%~ replacing_with_var(tuple_elts(["i","sj"]))
%~
%~ replacing_with_var( comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)])))
%~
%~ replacing_with_var(tuple_elts(["i","ej"]))
%~
%~ replacing_with_var( comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)])))
%~
%~ replacing_with_var(tuple_elts(["si","j"]))
%~
%~ replacing_with_var( comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)])))
%~
%~ replacing_with_var(tuple_elts(["ei","j"]))
%~
%~ replacing_with_var( comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)])))
%~
%~ replacing_with_var(call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))
%~
%~ replacing_with_var(call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))
%~
%~ replacing_with_var(call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))
%~
%~ replacing_with_var(call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))
%~
% Compiled KL-1 for outbox
outbox(PATCH_01,INDICES_02) :-
  willBeType(INDICES_02,'Indices') ,
  comment(' outbox for patch ') ,
  uppermost(PATCH_01,ARG_03) ,
  call([op_call(sub_token(-,ARG_03,1,AI))]) ,
  leftmost(PATCH_01,ARG_04) ,
  call([op_call(sub_token(-,ARG_04,1,AJ))]) ,
  lowermost(PATCH_01,ARG_05) ,
  call([op_call(add_token(+,ARG_05,1,BI))]) ,
  rightmost(PATCH_01,ARG_06) ,
  call([op_call(add_token(+,ARG_06,1,BJ))]) ,
  min(AI,BI,SI) ,
  min(AJ,BJ,SJ) ,
  max(AI,BI,EI) ,
  max(AJ,BJ,EJ) ,
  into_tuple(I,SJ,ARG_012) ,
  call([op_call(add_token(+,EI,1,ARG_021))]) ,
  range(SI,ARG_021,ARG_020) ,
  comprehension_target_iter(I,ARG_020,ARG_013) ,
  set_comp_elt_generators(ARG_012,[ARG_013],ARG_07) ,
  into_tuple(I,EJ,ARG_014) ,
  call([op_call(add_token(+,EI,1,ARG_023))]) ,
  range(SI,ARG_023,ARG_022) ,
  comprehension_target_iter(I,ARG_022,ARG_015) ,
  set_comp_elt_generators(ARG_014,[ARG_015],ARG_08) ,
  call([op_call(bit_or_token('|',ARG_07,ARG_08,VLINES))]) ,
  into_tuple(SI,J,ARG_016) ,
  call([op_call(add_token(+,EJ,1,ARG_025))]) ,
  range(SJ,ARG_025,ARG_024) ,
  comprehension_target_iter(J,ARG_024,ARG_017) ,
  set_comp_elt_generators(ARG_016,[ARG_017],ARG_09) ,
  into_tuple(EI,J,ARG_018) ,
  call([op_call(add_token(+,EJ,1,ARG_027))]) ,
  range(SJ,ARG_027,ARG_026) ,
  comprehension_target_iter(J,ARG_026,ARG_019) ,
  set_comp_elt_generators(ARG_018,[ARG_019],ARG_010) ,
  call([op_call(bit_or_token('|',ARG_09,ARG_010,HLINES))]) ,
  call([op_call(bit_or_token('|',VLINES,HLINES,ARG_011))]) ,
  frozenset(ARG_011,INDICES_02) ,
  exit_proc(INDICES_02).
%~ % Universal AST Pass #0
%~ def( "box",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' outline of patch ')),
%~                          assign_targets_value([tuple_elts(["ai","aj"])],call_func_args("ulcorner",["patch"])),
%~                          assign_targets_value([tuple_elts(["bi","bj"])],call_func_args("lrcorner",["patch"])),
%~                          assign_targets_value( [tuple_elts(["si","sj"])],
%~                            tuple_elts( [ call_func_args("min",["ai","bi"]),
%~                                          call_func_args("min",["aj","bj"])])),
%~                          assign_targets_value( [tuple_elts(["ei","ej"])],
%~                            tuple_elts( [ call_func_args("max",["ai","bi"]),
%~                                          call_func_args("max",["aj","bj"])])),
%~                          assign_targets_value( ["vlines"],
%~                            bin_op_left_right( bit_or_token('|'),
%~                              set_comp_elt_generators( tuple_elts(["i","sj"]), [
%~                                comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]),
%~                              set_comp_elt_generators( tuple_elts(["i","ej"]), [
%~                                comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]))),
%~                          assign_targets_value( ["hlines"],
%~                            bin_op_left_right( bit_or_token('|'),
%~                              set_comp_elt_generators( tuple_elts(["si","j"]), [
%~                                comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]),
%~                              set_comp_elt_generators( tuple_elts(["ei","j"]), [
%~                                comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]))),
%~                          return_value(call_func_args("frozenset",[bin_op_left_right(bit_or_token('|'),"vlines","hlines")]))])))
%~
%~ replacing_with_var(tuple_elts(["i","sj"]))
%~
%~ replacing_with_var( comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)])))
%~
%~ replacing_with_var(tuple_elts(["i","ej"]))
%~
%~ replacing_with_var( comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)])))
%~
%~ replacing_with_var(tuple_elts(["si","j"]))
%~
%~ replacing_with_var( comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)])))
%~
%~ replacing_with_var(tuple_elts(["ei","j"]))
%~
%~ replacing_with_var( comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)])))
%~
%~ replacing_with_var(call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))
%~
%~ replacing_with_var(call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))
%~
%~ replacing_with_var(call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))
%~
%~ replacing_with_var(call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))
%~
% Compiled KL-1 for box
box(PATCH_01,INDICES_02) :-
  willBeType(INDICES_02,'Indices') ,
  comment(' outline of patch ') ,
  into_tuple(AI,AJ,ARG_03) ,
  ulcorner(PATCH_01,ARG_03) ,
  into_tuple(BI,BJ,ARG_04) ,
  lrcorner(PATCH_01,ARG_04) ,
  min(AI,BI,SI) ,
  min(AJ,BJ,SJ) ,
  max(AI,BI,EI) ,
  max(AJ,BJ,EJ) ,
  into_tuple(I,SJ,ARG_010) ,
  call([op_call(add_token(+,EI,1,ARG_019))]) ,
  range(SI,ARG_019,ARG_018) ,
  comprehension_target_iter(I,ARG_018,ARG_011) ,
  set_comp_elt_generators(ARG_010,[ARG_011],ARG_05) ,
  into_tuple(I,EJ,ARG_012) ,
  call([op_call(add_token(+,EI,1,ARG_021))]) ,
  range(SI,ARG_021,ARG_020) ,
  comprehension_target_iter(I,ARG_020,ARG_013) ,
  set_comp_elt_generators(ARG_012,[ARG_013],ARG_06) ,
  call([op_call(bit_or_token('|',ARG_05,ARG_06,VLINES))]) ,
  into_tuple(SI,J,ARG_014) ,
  call([op_call(add_token(+,EJ,1,ARG_023))]) ,
  range(SJ,ARG_023,ARG_022) ,
  comprehension_target_iter(J,ARG_022,ARG_015) ,
  set_comp_elt_generators(ARG_014,[ARG_015],ARG_07) ,
  into_tuple(EI,J,ARG_016) ,
  call([op_call(add_token(+,EJ,1,ARG_025))]) ,
  range(SJ,ARG_025,ARG_024) ,
  comprehension_target_iter(J,ARG_024,ARG_017) ,
  set_comp_elt_generators(ARG_016,[ARG_017],ARG_08) ,
  call([op_call(bit_or_token('|',ARG_07,ARG_08,HLINES))]) ,
  call([op_call(bit_or_token('|',VLINES,HLINES,ARG_09))]) ,
  frozenset(ARG_09,INDICES_02) ,
  exit_proc(INDICES_02).
%~ % Universal AST Pass #0
%~ def( "shoot",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("start"),
%~           argument_name("direction")],
%~         [argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' line from starting point and direction ')),
%~                          return_value( call_func_args( "connect", [
%~                                          "start",
%~                                          tuple_elts( [ bin_op_left_right( add_token(+),
%~                                                          subscript_value_slice("start",0),
%~                                                          bin_op_left_right(mult_token(*),42,subscript_value_slice("direction",0))),
%~                                                        bin_op_left_right( add_token(+),
%~                                                          subscript_value_slice("start",1),
%~                                                          bin_op_left_right(mult_token(*),42,subscript_value_slice("direction",1)))])]))])))
%~
% Compiled KL-1 for shoot
shoot(START_01,DIRECTION_02,INDICES_03) :-
  willBeType(INDICES_03,'Indices') ,
  comment(' line from starting point and direction ') ,
  subscript_value_slice(START_01,0,ARG_07) ,
  subscript_value_slice(DIRECTION_02,0,ARG_011) ,
  call([op_call(mult_token(*,42,ARG_011,ARG_08))]) ,
  call([op_call(add_token(+,ARG_07,ARG_08,ARG_05))]) ,
  subscript_value_slice(START_01,1,ARG_09) ,
  subscript_value_slice(DIRECTION_02,1,ARG_012) ,
  call([op_call(mult_token(*,42,ARG_012,ARG_010))]) ,
  call([op_call(add_token(+,ARG_09,ARG_010,ARG_06))]) ,
  tuple_elts(ARG_05,ARG_06,ARG_04) ,
  connect(START_01,ARG_04,INDICES_03) ,
  exit_proc(INDICES_03).
%~ % Universal AST Pass #0
%~ def( "occurrences",
%~   function_type_body(
%~      function_type_arguments_returns(
%~         [ argument_name("grid"),
%~           argument_name("obj")],
%~         [argument_type("Indices")]),
%~      block_statements( [ expr_value(string_value(' locations of occurrences of object in grid ')),
%~                          assign_targets_value(["occs"],call_func("set")),
%~                          assign_targets_value(["normed"],call_func_args("normalize",["obj"])),
%~                          assign_targets_value( [tuple_elts(["h","w"])],
%~                            tuple_elts( [ call_func_args("len",["grid"]),
%~                                          call_func_args("len",[subscript_value_slice("grid",0)])])),
%~                          assign_targets_value([tuple_elts(["oh","ow"])],call_func_args("shape",["obj"])),
%~                          assign_targets_value( [tuple_elts(["h2","w2"])],
%~                            tuple_elts( [ bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"h","oh"),1),
%~                                          bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"w","ow"),1)])),
%~                          for_target_iter_body( "i",
%~                            call_func_args("range",["h2"]),
%~                            body_stmts( [ for_target_iter_body( "j",
%~                                            call_func_args("range",["w2"]),
%~                                            body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(true),'True')),
%~                                                          for_target_iter_body(
%~                                                             tuple_elts(["v",tuple_elts(["a","b"])]),
%~                                                             call_func_args("shift",["normed",tuple_elts(["i","j"])]),
%~                                                             body_stmts( [ if_test_body(
%~                                                                              unary_op_operand( ['python:Not'],
%~                                                                                bool_op_values( ['python:And'], [
%~                                                                                  compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["a","h"])),
%~                                                                                  compare_ops_left_comparators(
%~                                                                                     ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["b","w"])),
%~                                                                                  compare_ops_left_comparators(eq_token(==),subscript_value_slice(subscript_value_slice("grid","a"),"b"),"v")])),
%~                                                                              body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(false),'False')),
%~                                                                                            ['python:Break']]))])),
%~                                                          if_test_body( "occurs",
%~                                                            body_stmts( [ expr_value( call_func_args(
%~                                                                                         qualified_identifier_identifiers(["occs",boxed_attribute_value("add")]),
%~                                                                                         [tuple_elts(["i","j"])]))]))]))])),
%~                          return_value(call_func_args("frozenset",["occs"]))])))
%~
%~ replacing_with_var( body_stmts( [ for_target_iter_body( "j",
%~                                     call_func_args("range",["w2"]),
%~                                     body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(true),'True')),
%~                                                   for_target_iter_body(
%~                                                      tuple_elts(["v",tuple_elts(["a","b"])]),
%~                                                      call_func_args("shift",["normed",tuple_elts([I_08,"j"])]),
%~                                                      body_stmts( [ if_test_body(
%~                                                                       unary_op_operand( ['python:Not'],
%~                                                                         bool_op_values( ['python:And'], [
%~                                                                           compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["a","h"])),
%~                                                                           compare_ops_left_comparators(
%~                                                                              ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["b","w"])),
%~                                                                           compare_ops_left_comparators( eq_token(==),
%~                                                                             subscript_value_slice(subscript_value_slice(GRID_01,"a"),"b"),
%~                                                                             "v")])),
%~                                                                       body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(false),'False')),
%~                                                                                     ['python:Break']]))])),
%~                                                   if_test_body( "occurs",
%~                                                     body_stmts( [ expr_value( call_func_args(
%~                                                                                  qualified_identifier_identifiers(["occs",boxed_attribute_value("add")]),
%~                                                                                  [tuple_elts([I_08,"j"])]))]))]))]))
%~
%~ replacing_with_var( for_target_iter_body( "j",
%~                       call_func_args("range",["w2"]),
%~                       body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(true),'True')),
%~                                     for_target_iter_body(
%~                                        tuple_elts(["v",tuple_elts(["a","b"])]),
%~                                        call_func_args("shift",["normed",tuple_elts([I_08,"j"])]),
%~                                        body_stmts( [ if_test_body(
%~                                                         unary_op_operand( ['python:Not'],
%~                                                           bool_op_values( ['python:And'], [
%~                                                             compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["a","h"])),
%~                                                             compare_ops_left_comparators(
%~                                                                ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["b","w"])),
%~                                                             compare_ops_left_comparators( eq_token(==),
%~                                                               subscript_value_slice(subscript_value_slice(GRID_01,"a"),"b"),
%~                                                               "v")])),
%~                                                         body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(false),'False')),
%~                                                                       ['python:Break']]))])),
%~                                     if_test_body( "occurs",
%~                                       body_stmts( [ expr_value( call_func_args(
%~                                                                    qualified_identifier_identifiers(["occs",boxed_attribute_value("add")]),
%~                                                                    [tuple_elts([I_08,"j"])]))]))])))
%~
%~ replacing_with_var(call_func_args("range",["w2"]))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(true),'True')),
%~                                   for_target_iter_body(
%~                                      tuple_elts(["v",tuple_elts(["a","b"])]),
%~                                      call_func_args("shift",["normed",tuple_elts([I_08,"j"])]),
%~                                      body_stmts( [ if_test_body(
%~                                                       unary_op_operand( ['python:Not'],
%~                                                         bool_op_values( ['python:And'], [
%~                                                           compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["a","h"])),
%~                                                           compare_ops_left_comparators(
%~                                                              ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["b","w"])),
%~                                                           compare_ops_left_comparators( eq_token(==),
%~                                                             subscript_value_slice(subscript_value_slice(GRID_01,"a"),"b"),
%~                                                             "v")])),
%~                                                       body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(false),'False')),
%~                                                                     ['python:Break']]))])),
%~                                   if_test_body( "occurs",
%~                                     body_stmts( [ expr_value( call_func_args(
%~                                                                  qualified_identifier_identifiers(["occs",boxed_attribute_value("add")]),
%~                                                                  [tuple_elts([I_08,"j"])]))]))]))
%~
%~ replacing_with_var( for_target_iter_body(
%~                        tuple_elts(["v",tuple_elts(["a","b"])]),
%~                        call_func_args("shift",["normed",tuple_elts([I_08,"j"])]),
%~                        body_stmts( [ if_test_body(
%~                                         unary_op_operand( ['python:Not'],
%~                                           bool_op_values( ['python:And'], [
%~                                             compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["a","h"])),
%~                                             compare_ops_left_comparators(
%~                                                ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["b","w"])),
%~                                             compare_ops_left_comparators( eq_token(==),
%~                                               subscript_value_slice(subscript_value_slice(GRID_01,"a"),"b"),
%~                                               "v")])),
%~                                         body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(false),'False')),
%~                                                       ['python:Break']]))])))
%~
%~ replacing_with_var( if_test_body( "occurs",
%~                       body_stmts( [ expr_value( call_func_args(
%~                                                    qualified_identifier_identifiers(["occs",boxed_attribute_value("add")]),
%~                                                    [tuple_elts([I_08,"j"])]))])))
%~
%~ replacing_with_var(tuple_elts(["v",tuple_elts(["a","b"])]))
%~
%~ replacing_with_var(call_func_args("shift",["normed",tuple_elts([I_08,"j"])]))
%~
%~ replacing_with_var( body_stmts( [ if_test_body(
%~                                      unary_op_operand( ['python:Not'],
%~                                        bool_op_values( ['python:And'], [
%~                                          compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["a","h"])),
%~                                          compare_ops_left_comparators(
%~                                             ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["b","w"])),
%~                                          compare_ops_left_comparators( eq_token(==),
%~                                            subscript_value_slice(subscript_value_slice(GRID_01,"a"),"b"),
%~                                            "v")])),
%~                                      body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(false),'False')),
%~                                                    ['python:Break']]))]))
%~
%~ replacing_with_var( body_stmts( [ expr_value( call_func_args(
%~                                                  qualified_identifier_identifiers(["occs",boxed_attribute_value("add")]),
%~                                                  [tuple_elts([I_08,"j"])]))]))
%~
%~ replacing_with_var( if_test_body(
%~                        unary_op_operand( ['python:Not'],
%~                          bool_op_values( ['python:And'], [
%~                            compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["a","h"])),
%~                            compare_ops_left_comparators(
%~                               ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["b","w"])),
%~                            compare_ops_left_comparators( eq_token(==),
%~                              subscript_value_slice(subscript_value_slice(GRID_01,"a"),"b"),
%~                              "v")])),
%~                        body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(false),'False')),
%~                                      ['python:Break']])))
%~
%~ replacing_with_var( expr_value( call_func_args(
%~                                    qualified_identifier_identifiers(["occs",boxed_attribute_value("add")]),
%~                                    [tuple_elts([I_08,"j"])])))
%~
%~ replacing_with_var( unary_op_operand( ['python:Not'],
%~                       bool_op_values( ['python:And'], [
%~                         compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["a","h"])),
%~                         compare_ops_left_comparators(
%~                            ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["b","w"])),
%~                         compare_ops_left_comparators( eq_token(==),
%~                           subscript_value_slice(subscript_value_slice(GRID_01,"a"),"b"),
%~                           "v")])))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(false),'False')),
%~                                   ['python:Break']]))
%~
%~ replacing_with_var(boxed_attribute_value("add"))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["a","h"])))
%~
%~ replacing_with_var( compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["b","w"])))
%~
%~ replacing_with_var( compare_ops_left_comparators(eq_token(==),subscript_value_slice(subscript_value_slice(GRID_01,"a"),"b"),"v"))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["a","h"]))
%~
%~ replacing_with_var(ops([lt_e_token(<=),lt_token(<)]))
%~
%~ replacing_with_var(comparators(["b","w"]))
%~
%~ replacing_with_var(subscript_value_slice(subscript_value_slice(GRID_01,"a"),"b"))
%~
% Compiled KL-1 for occurrences
occurrences(GRID_01,OBJ_02,INDICES_03) :-
  willBeType(INDICES_03,'Indices') ,
  comment(' locations of occurrences of object in grid ') ,
  make_new("set",OCCS) ,
  normalize(OBJ_02,NORMED) ,
  len(GRID_01,H) ,
  subscript_value_slice(GRID_01,0,ARG_04) ,
  len(ARG_04,W) ,
  into_tuple(OH,OW,ARG_05) ,
  shape(OBJ_02,ARG_05) ,
  call([op_call(sub_token(-,H,OH,ARG_06))]) ,
  call([op_call(add_token(+,ARG_06,1,H2))]) ,
  call([op_call(sub_token(-,W,OW,ARG_07))]) ,
  call([op_call(add_token(+,ARG_07,1,W2))]) ,
  range(W2,ARG_011) ,
  into_tuple(A,B,ARG_016) ,
  tuple_elts(V,ARG_016,ARG_015) ,
  into_tuple(I_08,J,ARG_018) ,
  shift(NORMED,ARG_018,ARG_017) ,
  unary_op_operand( ['python:Not'],
    [ ops([lt_e_token(<=),lt_token(<)],ARG_030),
      comparators([A,H],ARG_031),
      compare_ops_left_comparators(ARG_030,0,ARG_031,ARG_027),
      ops([lt_e_token(<=),lt_token(<)],ARG_032),
      comparators([B,W],ARG_033),
      compare_ops_left_comparators(ARG_032,0,ARG_033,ARG_028),
      subscript_value_slice(GRID_01,A,ARG_035),
      subscript_value_slice(ARG_035,B,ARG_034),
      compare_ops_left_comparators(eq_token(==),ARG_034,V,ARG_029),
      bool_op_values(['python:And'],[ARG_027,ARG_028,ARG_029])],
    ARG_024) ,
  body_stmts(["occurs"=false,'python:Break'],ARG_025) ,
  if_test_body(ARG_024,ARG_025,ARG_021) ,
  body_stmts([ARG_021],ARG_019) ,
  for_target_iter_body(ARG_015,ARG_017,ARG_019,ARG_013) ,
  into_tuple(I_08,J,ARG_023) ,
  call( [ op_call( [ boxed_attribute_value("add",ARG_026),
                     qualified_identifier_identifiers( [OCCS,ARG_026],
                       ARG_023,
                       ARG_022)])]) ,
  body_stmts([ARG_022],ARG_020) ,
  if_test_body("occurs",ARG_020,ARG_014) ,
  body_stmts(["occurs"=true,ARG_013,ARG_014],ARG_012) ,
  for_target_iter_body(J,ARG_011,ARG_012,ARG_010) ,
  body_stmts([ARG_010],ARG_09) ,
  for_each([range(H2,I_08)],ARG_09) ,
  frozenset(OCCS,INDICES_03) ,
  exit_proc(INDICES_03).
%~ % Universal AST Pass #0
%~ def( "frontiers",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("grid")],[argument_type("Objects")]),
%~      block_statements( [ expr_value(string_value(' set of frontiers ')),
%~                          assign_targets_value( [tuple_elts(["h","w"])],
%~                            tuple_elts( [ call_func_args("len",["grid"]),
%~                                          call_func_args("len",[subscript_value_slice("grid",0)])])),
%~                          assign_targets_value( ["row_indices"],
%~                            call_func_args( "tuple", [
%~                              generator_exp_elt_generators( "i", [
%~                                comprehension_target_iter_ifs( tuple_elts(["i","r"]),
%~                                  call_func_args("enumerate",["grid"]),
%~                                  [ compare_ops_left_comparators(eq_token(==),call_func_args("len",[call_func_args("set",["r"])]),1)])])])),
%~                          assign_targets_value( ["column_indices"],
%~                            call_func_args( "tuple", [
%~                              generator_exp_elt_generators( "j", [
%~                                comprehension_target_iter_ifs( tuple_elts(["j","c"]),
%~                                  call_func_args("enumerate",[call_func_args("dmirror",["grid"])]),
%~                                  [ compare_ops_left_comparators(eq_token(==),call_func_args("len",[call_func_args("set",["c"])]),1)])])])),
%~                          assign_targets_value( ["hfrontiers"],
%~                            call_func_args( "frozenset", [
%~                              set_comp_elt_generators(
%~                                 call_func_args( "frozenset", [
%~                                   set_comp_elt_generators(
%~                                      tuple_elts( [ subscript_value_slice(subscript_value_slice("grid","i"),"j"),
%~                                                    tuple_elts(["i","j"])]),
%~                                      [ comprehension_target_iter("j",call_func_args("range",["w"]))])]),
%~                                 [comprehension_target_iter("i","row_indices")])])),
%~                          assign_targets_value( ["vfrontiers"],
%~                            call_func_args( "frozenset", [
%~                              set_comp_elt_generators(
%~                                 call_func_args( "frozenset", [
%~                                   set_comp_elt_generators(
%~                                      tuple_elts( [ subscript_value_slice(subscript_value_slice("grid","i"),"j"),
%~                                                    tuple_elts(["i","j"])]),
%~                                      [ comprehension_target_iter("i",call_func_args("range",["h"]))])]),
%~                                 [comprehension_target_iter("j","column_indices")])])),
%~                          return_value(bin_op_left_right(bit_or_token('|'),"hfrontiers","vfrontiers"))])))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( tuple_elts(["i","r"]),
%~                       call_func_args("enumerate",[GRID_01]),
%~                       [ compare_ops_left_comparators(eq_token(==),call_func_args("len",[call_func_args("set",["r"])]),1)]))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( tuple_elts(["j","c"]),
%~                       call_func_args("enumerate",[call_func_args("dmirror",[GRID_01])]),
%~                       [ compare_ops_left_comparators(eq_token(==),call_func_args("len",[call_func_args("set",["c"])]),1)]))
%~
%~ replacing_with_var( call_func_args( "frozenset", [
%~                       set_comp_elt_generators(
%~                          tuple_elts( [ subscript_value_slice(subscript_value_slice(GRID_01,"i"),"j"),
%~                                        tuple_elts(["i","j"])]),
%~                          [ comprehension_target_iter("j",call_func_args("range",["w"]))])]))
%~
%~ replacing_with_var(comprehension_target_iter("i","row_indices"))
%~
%~ replacing_with_var( call_func_args( "frozenset", [
%~                       set_comp_elt_generators(
%~                          tuple_elts( [ subscript_value_slice(subscript_value_slice(GRID_01,"i"),"j"),
%~                                        tuple_elts(["i","j"])]),
%~                          [ comprehension_target_iter("i",call_func_args("range",["h"]))])]))
%~
%~ replacing_with_var(comprehension_target_iter("j","column_indices"))
%~
%~ replacing_with_var(tuple_elts(["i","r"]))
%~
%~ replacing_with_var(call_func_args("enumerate",[GRID_01]))
%~
%~ replacing_with_var( compare_ops_left_comparators(eq_token(==),call_func_args("len",[call_func_args("set",["r"])]),1))
%~
%~ replacing_with_var(tuple_elts(["j","c"]))
%~
%~ replacing_with_var(call_func_args("enumerate",[call_func_args("dmirror",[GRID_01])]))
%~
%~ replacing_with_var( compare_ops_left_comparators(eq_token(==),call_func_args("len",[call_func_args("set",["c"])]),1))
%~
%~ replacing_with_var(call_func_args("len",[call_func_args("set",["r"])]))
%~
%~ replacing_with_var(call_func_args("len",[call_func_args("set",["c"])]))
%~
%~ replacing_with_var( tuple_elts( [ subscript_value_slice(subscript_value_slice(GRID_01,"i"),"j"),
%~                                   tuple_elts(["i","j"])]))
%~
%~ replacing_with_var(comprehension_target_iter("j",call_func_args("range",["w"])))
%~
%~ replacing_with_var( tuple_elts( [ subscript_value_slice(subscript_value_slice(GRID_01,"i"),"j"),
%~                                   tuple_elts(["i","j"])]))
%~
%~ replacing_with_var(comprehension_target_iter("i",call_func_args("range",["h"])))
%~
%~ replacing_with_var(call_func_args("range",["w"]))
%~
%~ replacing_with_var(call_func_args("range",["h"]))
%~
% Compiled KL-1 for frontiers
frontiers(GRID_01,OBJECTS_02) :-
  willBeType(OBJECTS_02,'Objects') ,
  comment(' set of frontiers ') ,
  len(GRID_01,H) ,
  subscript_value_slice(GRID_01,0,ARG_03) ,
  len(ARG_03,W) ,
  into_tuple(I,R,ARG_016) ,
  enumerate(GRID_01,ARG_017) ,
  set(R,ARG_024) ,
  len(ARG_024,ARG_023) ,
  compare_ops_left_comparators(eq_token(==),ARG_023,1,ARG_018) ,
  comprehension_target_iter_ifs(ARG_016,ARG_017,[ARG_018],ARG_08) ,
  generator_exp_elt_generators(I,[ARG_08],ARG_04) ,
  tuple(ARG_04,ROW_INDICES) ,
  into_tuple(J,C,ARG_019) ,
  dmirror(GRID_01,ARG_021) ,
  enumerate(ARG_021,ARG_020) ,
  set(C,ARG_026) ,
  len(ARG_026,ARG_025) ,
  compare_ops_left_comparators(eq_token(==),ARG_025,1,ARG_022) ,
  comprehension_target_iter_ifs(ARG_019,ARG_020,[ARG_022],ARG_09) ,
  generator_exp_elt_generators(J,[ARG_09],ARG_05) ,
  tuple(ARG_05,COLUMN_INDICES) ,
  subscript_value_slice(GRID_01,I,ARG_035) ,
  subscript_value_slice(ARG_035,J,ARG_028) ,
  into_tuple(I,J,ARG_029) ,
  tuple_elts(ARG_028,ARG_029,ARG_027) ,
  range(W,ARG_036) ,
  comprehension_target_iter(J,ARG_036,ARG_030) ,
  set_comp_elt_generators(ARG_027,[ARG_030],ARG_011) ,
  frozenset(ARG_011,ARG_010) ,
  comprehension_target_iter(I,"row_indices",ARG_012) ,
  set_comp_elt_generators(ARG_010,[ARG_012],ARG_06) ,
  frozenset(ARG_06,HFRONTIERS) ,
  subscript_value_slice(GRID_01,I,ARG_037) ,
  subscript_value_slice(ARG_037,J,ARG_032) ,
  into_tuple(I,J,ARG_033) ,
  tuple_elts(ARG_032,ARG_033,ARG_031) ,
  range(H,ARG_038) ,
  comprehension_target_iter(I,ARG_038,ARG_034) ,
  set_comp_elt_generators(ARG_031,[ARG_034],ARG_014) ,
  frozenset(ARG_014,ARG_013) ,
  comprehension_target_iter(J,"column_indices",ARG_015) ,
  set_comp_elt_generators(ARG_013,[ARG_015],ARG_07) ,
  frozenset(ARG_07,VFRONTIERS) ,
  call([op_call(bit_or_token('|',HFRONTIERS,VFRONTIERS,OBJECTS_02))]) ,
  exit_proc(OBJECTS_02).
%~ % Universal AST Pass #0
%~ def( "compress",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
%~      block_statements( [ expr_value(string_value(' removes frontiers from grid ')),
%~                          assign_targets_value( ["ri"],
%~                            call_func_args( "tuple", [
%~                              generator_exp_elt_generators( "i", [
%~                                comprehension_target_iter_ifs( tuple_elts(["i","r"]),
%~                                  call_func_args("enumerate",["grid"]),
%~                                  [ compare_ops_left_comparators(eq_token(==),call_func_args("len",[call_func_args("set",["r"])]),1)])])])),
%~                          assign_targets_value( ["ci"],
%~                            call_func_args( "tuple", [
%~                              generator_exp_elt_generators( "j", [
%~                                comprehension_target_iter_ifs( tuple_elts(["j","c"]),
%~                                  call_func_args("enumerate",[call_func_args("dmirror",["grid"])]),
%~                                  [ compare_ops_left_comparators(eq_token(==),call_func_args("len",[call_func_args("set",["c"])]),1)])])])),
%~                          return_value( call_func_args( "tuple", [
%~                                          generator_exp_elt_generators(
%~                                             call_func_args( "tuple", [
%~                                               generator_exp_elt_generators( "v", [
%~                                                 comprehension_target_iter_ifs( tuple_elts(["j","v"]),
%~                                                   call_func_args("enumerate",["r"]),
%~                                                   [ compare_ops_left_comparators(['python:NotIn'],"j","ci")])])]),
%~                                             [ comprehension_target_iter_ifs( tuple_elts(["i","r"]),
%~                                                 call_func_args("enumerate",["grid"]),
%~                                                 [ compare_ops_left_comparators(['python:NotIn'],"i","ri")])])]))])))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( tuple_elts(["i","r"]),
%~                       call_func_args("enumerate",[GRID_01]),
%~                       [ compare_ops_left_comparators(eq_token(==),call_func_args("len",[call_func_args("set",["r"])]),1)]))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( tuple_elts(["j","c"]),
%~                       call_func_args("enumerate",[call_func_args("dmirror",[GRID_01])]),
%~                       [ compare_ops_left_comparators(eq_token(==),call_func_args("len",[call_func_args("set",["c"])]),1)]))
%~
%~ replacing_with_var( call_func_args( "tuple", [
%~                       generator_exp_elt_generators( "v", [
%~                         comprehension_target_iter_ifs( tuple_elts(["j","v"]),
%~                           call_func_args("enumerate",["r"]),
%~                           [ compare_ops_left_comparators(['python:NotIn'],"j","ci")])])]))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( tuple_elts(["i","r"]),
%~                       call_func_args("enumerate",[GRID_01]),
%~                       [ compare_ops_left_comparators(['python:NotIn'],"i","ri")]))
%~
%~ replacing_with_var(tuple_elts(["i","r"]))
%~
%~ replacing_with_var(call_func_args("enumerate",[GRID_01]))
%~
%~ replacing_with_var( compare_ops_left_comparators(eq_token(==),call_func_args("len",[call_func_args("set",["r"])]),1))
%~
%~ replacing_with_var(tuple_elts(["j","c"]))
%~
%~ replacing_with_var(call_func_args("enumerate",[call_func_args("dmirror",[GRID_01])]))
%~
%~ replacing_with_var( compare_ops_left_comparators(eq_token(==),call_func_args("len",[call_func_args("set",["c"])]),1))
%~
%~ replacing_with_var(tuple_elts(["i","r"]))
%~
%~ replacing_with_var(call_func_args("enumerate",[GRID_01]))
%~
%~ replacing_with_var(compare_ops_left_comparators(['python:NotIn'],"i","ri"))
%~
%~ replacing_with_var(call_func_args("len",[call_func_args("set",["r"])]))
%~
%~ replacing_with_var(call_func_args("len",[call_func_args("set",["c"])]))
%~
%~ replacing_with_var( comprehension_target_iter_ifs( tuple_elts(["j","v"]),
%~                       call_func_args("enumerate",["r"]),
%~                       [ compare_ops_left_comparators(['python:NotIn'],"j","ci")]))
%~
%~ replacing_with_var(tuple_elts(["j","v"]))
%~
%~ replacing_with_var(call_func_args("enumerate",["r"]))
%~
%~ replacing_with_var(compare_ops_left_comparators(['python:NotIn'],"j","ci"))
%~
% Compiled KL-1 for compress
compress(GRID_01,GRID_02) :-
  willBeType(GRID_02,'Grid') ,
  comment(' removes frontiers from grid ') ,
  into_tuple(I,R,ARG_011) ,
  enumerate(GRID_01,ARG_012) ,
  set(R,ARG_022) ,
  len(ARG_022,ARG_021) ,
  compare_ops_left_comparators(eq_token(==),ARG_021,1,ARG_013) ,
  comprehension_target_iter_ifs(ARG_011,ARG_012,[ARG_013],ARG_06) ,
  generator_exp_elt_generators(I,[ARG_06],ARG_03) ,
  tuple(ARG_03,RI) ,
  into_tuple(J,C,ARG_014) ,
  dmirror(GRID_01,ARG_016) ,
  enumerate(ARG_016,ARG_015) ,
  set(C,ARG_024) ,
  len(ARG_024,ARG_023) ,
  compare_ops_left_comparators(eq_token(==),ARG_023,1,ARG_017) ,
  comprehension_target_iter_ifs(ARG_014,ARG_015,[ARG_017],ARG_07) ,
  generator_exp_elt_generators(J,[ARG_07],ARG_04) ,
  tuple(ARG_04,CI) ,
  into_tuple(J,V,ARG_026) ,
  enumerate(R,ARG_027) ,
  compare_ops_left_comparators(['python:NotIn'],J,CI,ARG_028) ,
  comprehension_target_iter_ifs(ARG_026,ARG_027,[ARG_028],ARG_025) ,
  generator_exp_elt_generators(V,[ARG_025],ARG_09) ,
  tuple(ARG_09,ARG_08) ,
  into_tuple(I,R,ARG_018) ,
  enumerate(GRID_01,ARG_019) ,
  compare_ops_left_comparators(['python:NotIn'],I,RI,ARG_020) ,
  comprehension_target_iter_ifs(ARG_018,ARG_019,[ARG_020],ARG_010) ,
  generator_exp_elt_generators(ARG_08,[ARG_010],ARG_05) ,
  tuple(ARG_05,GRID_02) ,
  exit_proc(GRID_02).
%~ % Universal AST Pass #0
%~ def( "hperiod",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("obj")],[argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' horizontal periodicity ')),
%~                          assign_targets_value(["normalized"],call_func_args("normalize",["obj"])),
%~                          assign_targets_value(["w"],call_func_args("width",["normalized"])),
%~                          for_target_iter_body( "p",
%~                            call_func_args("range",[1,"w"]),
%~                            body_stmts( [ assign_targets_value( ["offsetted"],
%~                                            call_func_args( "shift", [
%~                                              "normalized",
%~                                              tuple_elts([0,unary_op_operand(us_ub_token(-),"p")])])),
%~                                          assign_targets_value( ["pruned"],
%~                                            call_func_args( "frozenset", [
%~                                              set_comp_elt_generators(
%~                                                 tuple_elts(["c",tuple_elts(["i","j"])]),
%~                                                 [ comprehension_target_iter_ifs(
%~                                                      tuple_elts(["c",tuple_elts(["i","j"])]),
%~                                                      "offsetted",
%~                                                      [ compare_ops_left_comparators(gt_e_token(>=),"j",0)])])])),
%~                                          if_test_body(
%~                                             call_func_args(
%~                                                qualified_identifier_identifiers(["pruned",boxed_attribute_value("issubset")]),
%~                                                ["normalized"]),
%~                                             body_stmts([return_value("p")]))])),
%~                          return_value("w")])))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value( ["offsetted"],
%~                                     call_func_args( "shift", [
%~                                       "normalized",
%~                                       tuple_elts([0,unary_op_operand(us_ub_token(-),P_03)])])),
%~                                   assign_targets_value( ["pruned"],
%~                                     call_func_args( "frozenset", [
%~                                       set_comp_elt_generators(
%~                                          tuple_elts(["c",tuple_elts(["i","j"])]),
%~                                          [ comprehension_target_iter_ifs(
%~                                               tuple_elts(["c",tuple_elts(["i","j"])]),
%~                                               "offsetted",
%~                                               [ compare_ops_left_comparators(gt_e_token(>=),"j",0)])])])),
%~                                   if_test_body(
%~                                      call_func_args(
%~                                         qualified_identifier_identifiers(["pruned",boxed_attribute_value("issubset")]),
%~                                         ["normalized"]),
%~                                      body_stmts([return_value(P_03)]))]))
%~
%~ replacing_with_var( if_test_body(
%~                        call_func_args(
%~                           qualified_identifier_identifiers(["pruned",boxed_attribute_value("issubset")]),
%~                           ["normalized"]),
%~                        body_stmts([return_value(P_03)])))
%~
%~ replacing_with_var( call_func_args(
%~                        qualified_identifier_identifiers(["pruned",boxed_attribute_value("issubset")]),
%~                        ["normalized"]))
%~
%~ replacing_with_var(body_stmts([return_value(P_03)]))
%~
%~ replacing_with_var(boxed_attribute_value("issubset"))
%~
%~ replacing_with_var(tuple_elts(["c",tuple_elts(["i","j"])]))
%~
%~ replacing_with_var( comprehension_target_iter_ifs(
%~                        tuple_elts(["c",tuple_elts(["i","j"])]),
%~                        "offsetted",
%~                        [ compare_ops_left_comparators(gt_e_token(>=),"j",0)]))
%~
%~ replacing_with_var(tuple_elts(["c",tuple_elts(["i","j"])]))
%~
%~ replacing_with_var(compare_ops_left_comparators(gt_e_token(>=),"j",0))
%~
% Compiled KL-1 for hperiod
hperiod(OBJ_01,INTEGER_02) :-
  willBeType(INTEGER_02,'Integer') ,
  comment(' horizontal periodicity ') ,
  normalize(OBJ_01,NORMALIZED) ,
  width(NORMALIZED,W) ,
  call( [ op_call( [ boxed_attribute_value("issubset",ARG_011),
                     qualified_identifier_identifiers( [PRUNED,ARG_011],
                       NORMALIZED,
                       ARG_08)])]) ,
  body_stmts(
     [ call(INTEGER_02=P_03),
       exit_proc(INTEGER_02)],
     ARG_09) ,
  if_test_body(ARG_08,ARG_09,ARG_05) ,
  body_stmts(
     [ unary_op_operand(us_ub_token(-),P_03,ARG_010),
       tuple_elts(0,ARG_010,ARG_06),
       shift(NORMALIZED,ARG_06,OFFSETTED),
       into_tuple(I,J,ARG_013),
       tuple_elts(C,ARG_013,ARG_012),
       into_tuple(I,J,ARG_016),
       tuple_elts(C,ARG_016,ARG_015),
       compare_ops_left_comparators(gt_e_token(>=),J,0,ARG_017),
       comprehension_target_iter_ifs(ARG_015,OFFSETTED,[ARG_017],ARG_014),
       set_comp_elt_generators(ARG_012,[ARG_014],ARG_07),
       frozenset(ARG_07,PRUNED),
       ARG_05],
     ARG_04) ,
  for_each([range(1,W,P_03)],ARG_04) ,
  call(INTEGER_02=W) ,
  exit_proc(INTEGER_02).
%~ % Universal AST Pass #0
%~ def( "vperiod",
%~   function_type_body(
%~      function_type_arguments_returns([argument_name("obj")],[argument_type("Integer")]),
%~      block_statements( [ expr_value(string_value(' vertical periodicity ')),
%~                          assign_targets_value(["normalized"],call_func_args("normalize",["obj"])),
%~                          assign_targets_value(["h"],call_func_args("height",["normalized"])),
%~                          for_target_iter_body( "p",
%~                            call_func_args("range",[1,"h"]),
%~                            body_stmts( [ assign_targets_value( ["offsetted"],
%~                                            call_func_args( "shift", [
%~                                              "normalized",
%~                                              tuple_elts([unary_op_operand(us_ub_token(-),"p"),0])])),
%~                                          assign_targets_value( ["pruned"],
%~                                            call_func_args( "frozenset", [
%~                                              set_comp_elt_generators(
%~                                                 tuple_elts(["c",tuple_elts(["i","j"])]),
%~                                                 [ comprehension_target_iter_ifs(
%~                                                      tuple_elts(["c",tuple_elts(["i","j"])]),
%~                                                      "offsetted",
%~                                                      [ compare_ops_left_comparators(gt_e_token(>=),"i",0)])])])),
%~                                          if_test_body(
%~                                             call_func_args(
%~                                                qualified_identifier_identifiers(["pruned",boxed_attribute_value("issubset")]),
%~                                                ["normalized"]),
%~                                             body_stmts([return_value("p")]))])),
%~                          return_value("h")])))
%~
%~ replacing_with_var( body_stmts( [ assign_targets_value( ["offsetted"],
%~                                     call_func_args( "shift", [
%~                                       "normalized",
%~                                       tuple_elts([unary_op_operand(us_ub_token(-),P_03),0])])),
%~                                   assign_targets_value( ["pruned"],
%~                                     call_func_args( "frozenset", [
%~                                       set_comp_elt_generators(
%~                                          tuple_elts(["c",tuple_elts(["i","j"])]),
%~                                          [ comprehension_target_iter_ifs(
%~                                               tuple_elts(["c",tuple_elts(["i","j"])]),
%~                                               "offsetted",
%~                                               [ compare_ops_left_comparators(gt_e_token(>=),"i",0)])])])),
%~                                   if_test_body(
%~                                      call_func_args(
%~                                         qualified_identifier_identifiers(["pruned",boxed_attribute_value("issubset")]),
%~                                         ["normalized"]),
%~                                      body_stmts([return_value(P_03)]))]))
%~
%~ replacing_with_var( if_test_body(
%~                        call_func_args(
%~                           qualified_identifier_identifiers(["pruned",boxed_attribute_value("issubset")]),
%~                           ["normalized"]),
%~                        body_stmts([return_value(P_03)])))
%~
%~ replacing_with_var( call_func_args(
%~                        qualified_identifier_identifiers(["pruned",boxed_attribute_value("issubset")]),
%~                        ["normalized"]))
%~
%~ replacing_with_var(body_stmts([return_value(P_03)]))
%~
%~ replacing_with_var(boxed_attribute_value("issubset"))
%~
%~ replacing_with_var(tuple_elts(["c",tuple_elts(["i","j"])]))
%~
%~ replacing_with_var( comprehension_target_iter_ifs(
%~                        tuple_elts(["c",tuple_elts(["i","j"])]),
%~                        "offsetted",
%~                        [ compare_ops_left_comparators(gt_e_token(>=),"i",0)]))
%~
%~ replacing_with_var(tuple_elts(["c",tuple_elts(["i","j"])]))
%~
%~ replacing_with_var(compare_ops_left_comparators(gt_e_token(>=),"i",0))
%~
% Compiled KL-1 for vperiod
vperiod(OBJ_01,INTEGER_02) :-
  willBeType(INTEGER_02,'Integer') ,
  comment(' vertical periodicity ') ,
  normalize(OBJ_01,NORMALIZED) ,
  height(NORMALIZED,H) ,
  call( [ op_call( [ boxed_attribute_value("issubset",ARG_011),
                     qualified_identifier_identifiers( [PRUNED,ARG_011],
                       NORMALIZED,
                       ARG_08)])]) ,
  body_stmts(
     [ call(INTEGER_02=P_03),
       exit_proc(INTEGER_02)],
     ARG_09) ,
  if_test_body(ARG_08,ARG_09,ARG_05) ,
  body_stmts(
     [ unary_op_operand(us_ub_token(-),P_03,ARG_010),
       tuple_elts(ARG_010,0,ARG_06),
       shift(NORMALIZED,ARG_06,OFFSETTED),
       into_tuple(I,J,ARG_013),
       tuple_elts(C,ARG_013,ARG_012),
       into_tuple(I,J,ARG_016),
       tuple_elts(C,ARG_016,ARG_015),
       compare_ops_left_comparators(gt_e_token(>=),I,0,ARG_017),
       comprehension_target_iter_ifs(ARG_015,OFFSETTED,[ARG_017],ARG_014),
       set_comp_elt_generators(ARG_012,[ARG_014],ARG_07),
       frozenset(ARG_07,PRUNED),
       ARG_05],
     ARG_04) ,
  for_each([range(1,H,P_03)],ARG_04) ,
  call(INTEGER_02=H) ,
  exit_proc(INTEGER_02).
true.

114 ?-












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
