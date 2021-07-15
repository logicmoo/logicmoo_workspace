;;; phps-mode-automation-grammar --- Grammar -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:


;;; Code:


(require 'phps-mode-lexer)

(defconst
  phps-mode-automation-grammar-context-sensitive-attributes
  '(%prec)
  "List of context-sensitive attributes.")

(defconst
  phps-mode-automation-grammar-global-attributes
  '(%precedence %left %right %nonassoc)
  "List of valid global attributes.")

(defconst
  phps-mode-automation-grammar-global-declaration
  '(
    (%precedence T_THROW)
    (%precedence PREC_ARROW_FUNCTION)
    (%precedence T_INCLUDE T_INCLUDE_ONCE T_REQUIRE T_REQUIRE_ONCE)
    (%left T_LOGICAL_OR)
    (%left T_LOGICAL_XOR)
    (%left T_LOGICAL_AND)
    (%precedence T_PRINT)
    (%precedence T_YIELD)
    (%precedence T_DOUBLE_ARROW)
    (%precedence T_YIELD_FROM)
    (%precedence "=" T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL T_POW_EQUAL T_COALESCE_EQUAL)
    (%left "?" ":")
    (%right T_COALESCE)
    (%left T_BOOLEAN_OR)
    (%left T_BOOLEAN_AND)
    (%left "|")
    (%left "^")
    (%left "&")
    (%nonassoc T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_SPACESHIP)
    (%nonassoc "<" T_IS_SMALLER_OR_EQUAL ">" T_IS_GREATER_OR_EQUAL)
    (%left ".")
    (%left T_SL T_SR)
    (%left "+" "-")
    (%left "*" "/" "%")
    (%precedence "!")
    (%precedence T_INSTANCEOF)
    (%precendece "~" T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST "@" )
    (%right T_POW)
    (%precedence T_CLONE)
    (%precedence T_NOELSE)
    (%precedence T_ELSEIF)
    (%precedence T_ELSE)
    )
  "Declaration for grammar.")

(defconst
  phps-mode-automation-grammar-non-terminals
  '(
    absolute_trait_method_reference
    alt_if_stmt
    alt_if_stmt_without_else
    anonymous_class
    argument
    argument_list
    array_object_dereferencable
    array_pair
    array_pair_list
    attribute
    attribute_decl
    attribute_group
    attributed_class_statement
    attributed_parameter
    attributed_statement
    attributes
    backticks_expr
    backup_doc_comment
    backup_fn_flags
    backup_lex_pos
    callable_expr
    callable_variable
    case_list
    case_separator
    catch_list
    catch_name_list
    class_const_decl
    class_const_list
    class_constant
    class_declaration_statement
    class_modifier
    class_modifiers
    class_name
    class_name_list
    class_name_reference
    class_statement
    class_statement_list
    const_decl
    const_list
    constant
    ctor_arguments
    declare_statement
    dereferencable_scalar
    echo_expr
    echo_expr_list
    encaps_list
    encaps_var
    encaps_var_offset
    exit_expr
    expr
    extends_from
    finally_statement
    fn
    for_exprs
    for_statement
    foreach_statement
    foreach_variable
    fully_dereferencable
    function
    function_call
    function_declaration_statement
    global_var
    global_var_list
    group_use_declaration
    identifier
    if_stmt
    if_stmt_without_else
    implements_list
    inline_function
    inline_use_declaration
    inline_use_declarations
    inner_statement
    inner_statement_list
    interface_declaration_statement
    interface_extends_list
    internal_functions_in_yacc
    is_reference
    is_variadic
    isset_variable
    isset_variables
    legacy_namespace_name
    lexical_var
    lexical_var_list
    lexical_vars
    match
    match_arm
    match_arm_cond_list
    match_arm_list
    member_modifier
    member_name
    method_body
    method_modifiers
    mixed_group_use_declaration
    name
    namespace_declaration_name
    namespace_name
    new_expr
    new_variable
    non_empty_argument_list
    non_empty_array_pair_list
    non_empty_for_exprs
    non_empty_match_arm_list
    non_empty_member_modifiers
    non_empty_parameter_list
    optional_expr
    optional_type_without_static
    optional_variable
    optional_visibility_modifier
    parameter
    parameter_list
    possible_array_pair
    possible_comma
    property
    property_list
    property_name
    reserved_non_modifiers
    return_type
    returns_ref
    scalar
    semi_reserved
    simple_variable
    start
    statement
    static_member
    static_var
    static_var_list
    switch_case_list
    top_statement
    top_statement_list
    trait_adaptation
    trait_adaptation_list
    trait_adaptations
    trait_alias
    trait_declaration_statement
    trait_method_reference
    trait_precedence
    type
    type_expr
    type_expr_without_static
    type_without_static
    union_type
    union_type_without_static
    unprefixed_use_declaration
    unprefixed_use_declarations
    unset_variable
    unset_variables
    use_declaration
    use_declarations
    use_type
    variable
    variable_class_name
    variable_modifiers
    while_statement
    )
  "The non-terminals in grammar.")

(defconst
  phps-mode-automation-grammar-terminals
  '(
    "!"
    "%"
    "&"
    "("
    ")"
    "*"
    "+"
    "."
    ","
    "-"
    "/"
    ":"
    ";"
    "<"
    "="
    ">"
    "?"
    "@"
    "["
    "]"
    "}"
    "{"
    "^"
    "|"
    "~"
    "`"
    "'"
    "\""
    "$"
    PREC_ARROW_FUNCTION
    T_ABSTRACT
    T_AND_EQUAL
    T_ARG
    T_ARRAY
    T_ARRAY_CAST
    T_ARROW
    T_AS
    T_ASSIGN
    T_ATTRIBUTE
    T_BAD
    T_BOOL_CAST
    T_BOOLEAN_AND
    T_BOOLEAN_OR
    T_BREAK
    T_CALL
    T_CALLABLE
    T_CASE
    T_CATCH
    T_CLASS
    T_CLASS_C
    T_CLONE
    T_CLOSE
    T_CLOSURE
    T_COALESCE
    T_COALESCE_EQUAL
    T_COMMENT
    T_COMPILER
    T_CONCAT_EQUAL
    T_CONDITIONAL
    T_CONST
    T_CONSTANT_ENCAPSED_STRING
    T_CONTINUE
    T_CURLY_OPEN
    T_DEC
    T_DECL
    T_DECLARE
    T_DEFAULT
    T_DIM
    T_DIR
    T_DIV_EQUAL
    T_DNUMBER
    T_DO
    T_DOC
    T_DOLLAR_OPEN_CURLY_BRACES
    T_DOUBLE
    T_DOUBLE_ARROW
    T_DOUBLE_CAST
    T_ECHO
    T_ELLIPSIS
    T_ELSE
    T_ELSEIF
    T_EMPTY
    T_ENCAPS
    T_ENCAPSED_AND_WHITESPACE
    T_END_HEREDOC
    T_ENDDECLARE
    T_ENDFOR
    T_ENDFOREACH
    T_ENDIF
    T_ENDSWITCH
    T_ENDWHILE
    T_EQUAL
    T_ERROR
    T_EVAL
    T_EXIT
    T_EXPR
    T_EXTENDS
    T_FILE
    T_FINAL
    T_FINALLY
    T_FN
    T_FOR
    T_FOREACH
    T_FQ
    T_FUNC
    T_FUNC_C
    T_FUNCTION
    T_GLOBAL
    T_GOTO
    T_GREATER
    T_GROUP
    T_HALT_COMPILER
    T_IDENTICAL
    T_IF
    T_IMPLEMENTS
    T_INC
    T_INCLUDE
    T_INCLUDE_ONCE
    T_INLINE_HTML
    T_INSTANCEOF
    T_INSTEADOF
    T_INT_CAST
    T_INTERFACE
    T_IS_IDENTICAL
    T_IS_NOT_IDENTICAL
    T_IS_EQUAL
    T_IS_NOT_EQUAL
    T_IS_SMALLER_OR_EQUAL
    T_IS_GREATER_OR_EQUAL
    T_ISSET
    T_LABEL
    T_LINE
    T_LIST
    T_LNUMBER
    T_LOGICAL_AND
    T_LOGICAL_OR
    T_LOGICAL_XOR
    T_MAGIC
    T_MATCH
    T_METHOD
    T_METHOD_C
    T_MINUS_EQUAL
    T_MOD_EQUAL
    T_MUL_EQUAL
    T_NAME_FULLY_QUALIFIED
    T_NAME_RELATIVE
    T_NAME_QUALIFIED
    T_NAMESPACE
    T_NEW
    T_NOELSE
    T_NS_C
    T_NULLSAFE_OBJECT_OPERATOR
    T_NUM_STRING
    T_NS_SEPARATOR
    T_OBJECT_CAST
    T_OBJECT_OPERATOR
    T_OPEN
    T_OPERATOR
    T_OR_EQUAL
    T_PAAMAYIM_NEKUDOTAYIM
    T_PARAM
    T_PLUS_EQUAL
    T_POST
    T_POW
    T_POW_EQUAL
    T_PRE
    T_PRINT
    T_PRIVATE
    T_PROP
    T_PROTECTED
    T_PUBLIC
    T_REF
    T_REQUIRE
    T_REQUIRE_ONCE
    T_RETURN
    T_SHELL
    T_SILENCE
    T_SL
    T_SL_EQUAL
    T_SPACESHIP
    T_SR
    T_SR_EQUAL
    T_START_HEREDOC
    T_STATIC
    T_STMT
    T_STRING
    T_STRING_CAST
    T_STRING_VARNAME
    T_SWITCH
    T_THROW
    T_TRAIT
    T_TRAIT_C
    T_TRY
    T_TYPE
    T_UNARY
    T_UNPACK
    T_UNSET
    T_UNSET_CAST
    T_USE
    T_VAR
    T_VARIABLE
    T_WHILE
    T_WHITESPACE
    T_XOR_EQUAL
    T_YIELD
    T_YIELD_FROM
    )
  "The terminals of grammar.")

(defconst
  phps-mode-automation-grammar-look-ahead-number
  1
  "The look-ahead number of grammar.")

(defconst
  phps-mode-automation-grammar-productions
  '(

    (start
     top_statement_list
     )

    (reserved_non_modifiers
     T_INCLUDE
     T_INCLUDE_ONCE
     T_EVAL
     T_REQUIRE
     T_REQUIRE_ONCE
     T_LOGICAL_OR
     T_LOGICAL_XOR
     T_LOGICAL_AND
     T_INSTANCEOF
     T_NEW
     T_CLONE
     T_EXIT
     T_IF
     T_ELSEIF
     T_ELSE
     T_ENDIF
     T_ECHO
     T_DO
     T_WHILE
     T_ENDWHILE
     T_FOR
     T_ENDFOR
     T_FOREACH
     T_ENDFOREACH
     T_DECLARE
     T_ENDDECLARE
     T_AS
     T_TRY
     T_CATCH
     T_FINALLY
     T_THROW
     T_USE
     T_INSTEADOF
     T_GLOBAL
     T_VAR
     T_UNSET
     T_ISSET
     T_EMPTY
     T_CONTINUE
     T_GOTO
     T_FUNCTION
     T_CONST
     T_RETURN
     T_PRINT
     T_YIELD
     T_LIST
     T_SWITCH
     T_ENDSWITCH
     T_CASE
     T_DEFAULT
     T_BREAK
     T_ARRAY
     T_CALLABLE
     T_EXTENDS
     T_IMPLEMENTS
     T_NAMESPACE
     T_TRAIT
     T_INTERFACE
     T_CLASS
     T_CLASS_C
     T_TRAIT_C
     T_FUNC_C
     T_METHOD_C
     T_LINE
     T_FILE
     T_DIR
     T_NS_C
     T_FN
     T_MATCH
     )

    (semi_reserved
     reserved_non_modifiers
     T_STATIC
     T_ABSTRACT
     T_FINAL
     T_PRIVATE
     T_PROTECTED
     T_PUBLIC
     )

    (identifier
     T_STRING
     semi_reserved
     )

    (top_statement_list
     (top_statement_list top_statement)
     %empty
     )

    (namespace_declaration_name
     identifier
     T_NAME_QUALIFIED
     )

    (namespace_name
     T_STRING
     T_NAME_QUALIFIED
     )

    (legacy_namespace_name
     namespace_name
     T_NAME_FULLY_QUALIFIED
     )

    (name
     T_STRING
     T_NAME_QUALIFIED
     T_NAME_FULLY_QUALIFIED
     T_NAME_RELATIVE
     )

    (attribute_decl
     class_name
     (class_name argument_list)
     )

    (attribute_group
     attribute_decl
     (attribute_group "," attribute_decl)
     )

    (attribute
     (T_ATTRIBUTE
      attribute_group
      possible_comma "]")
     )

    (attributes
     attribute
     (attributes attribute)
     )

    (attributed_statement
     function_declaration_statement
     class_declaration_statement
     trait_declaration_statement
     interface_declaration_statement
     )

    (top_statement
     statement
     attributed_statement
     attributes
     attributed_statement
     (T_HALT_COMPILER "(" ")" ";")
     (T_NAMESPACE namespace_declaration_name ";")
     (T_NAMESPACE namespace_declaration_name)
     ("{" top_statement_list "}")
     T_NAMESPACE
     (T_USE mixed_group_use_declaration ";")
     (T_USE use_type group_use_declaration ";")
     (T_USE use_declarations ";")
     (T_USE use_type use_declarations ";")
     (T_CONST const_list ";")
     )

    (use_type
     T_FUNCTION
     T_CONST
     )

    (group_use_declaration
     (legacy_namespace_name T_NS_SEPARATOR "{" unprefixed_use_declarations possible_comma "}")
     )

    (mixed_group_use_declaration
     (legacy_namespace_name T_NS_SEPARATOR "{" inline_use_declarations possible_comma "}")
     )

    (possible_comma
     %empty
     ","
     )

    (inline_use_declarations
     (inline_use_declarations "," inline_use_declaration)
     inline_use_declaration
     )

    (unprefixed_use_declarations
     (unprefixed_use_declarations "," unprefixed_use_declaration)
     unprefixed_use_declaration
     )

    (use_declarations
     (use_declarations "," use_declaration)
     use_declaration
     )

    (inline_use_declaration
     unprefixed_use_declaration
     (use_type unprefixed_use_declaration)
     )

    (unprefixed_use_declaration
     namespace_name
     (namespace_name T_AS T_STRING)
     )

    (use_declaration
     legacy_namespace_name
     (legacy_namespace_name T_AS T_STRING)
     )

    (const_list
     (const_list "," const_decl)
     const_decl
     )

    (inner_statement_list
     (inner_statement_list inner_statement)
     %empty
     )

    (inner_statement
     statement
     attributed_statement
     (attributes attributed_statement)
     (T_HALT_COMPILER "(" ")" ";")
     )

    (statement
     ("{" inner_statement_list "}")
     if_stmt
     alt_if_stmt
     (T_WHILE "(" expr ")" while_statement)
     (T_DO statement T_WHILE "(" expr ")" ";")
     (T_FOR "(" for_exprs ";" for_exprs ";" for_exprs ")" for_statement)
     (T_SWITCH "(" expr ")" switch_case_list)
     (T_BREAK optional_expr ";")
     (T_CONTINUE optional_expr ";")
     (T_RETURN optional_expr ";")
     (T_GLOBAL global_var_list ";")
     (T_STATIC static_var_list ";")
     (T_ECHO echo_expr_list ";")
     T_INLINE_HTML
     (expr ";")
     (T_UNSET "(" unset_variables possible_comma ")" ";" )
     (T_FOREACH "(" expr T_AS foreach_variable ")" foreach_statement)
     (T_FOREACH "(" expr T_AS foreach_variable T_DOUBLE_ARROW foreach_variable ")" foreach_statement)
     (T_DECLARE "(" const_list ")" declare_statement ";")
     (T_TRY "{" inner_statement_list "}" catch_list finally_statement)
     (T_GOTO T_STRING ";")
     T_STRING
     )

    (catch_list
     %empty
     (catch_list T_CATCH "(" catch_name_list optional_variable ")" "{" inner_statement_list "}")
     )

    (catch_name_list
     class_name
     (catch_name_list "|" class_name)
     )

    (optional_variable
     %empty
     T_VARIABLE)

    (finally_statement
     %empty
     (T_FINALLY "{" inner_statement_list "}")
     )

    (unset_variables
     unset_variable
     (unset_variables "," unset_variable)
     )

    (unset_variable
     variable)

    (function_declaration_statement
     (function returns_ref T_STRING backup_doc_comment "(" parameter_list ")" return_type backup_fn_flags "{" inner_statement_list "}" backup_fn_flags)
     )

    (is_reference
     %empty
     "&"
     )

    (is_variadic
     %empty
     T_ELLIPSIS
     )

    (class_declaration_statement
     (class_modifiers T_CLASS)
     (T_STRING extends_from implements_list backup_doc_comment "{" class_statement_list "}")
     T_CLASS
     (T_STRING extends_from implements_list backup_doc_comment "{" class_statement_list "}")
     )

    (class_modifiers
     class_modifier
     (class_modifiers class_modifier)
     )

    (class_modifier
     T_ABSTRACT
     T_FINAL
     )

    (trait_declaration_statement
     T_TRAIT
     (T_STRING backup_doc_comment "{" class_statement_list "}")
     )

    (interface_declaration_statement
     T_INTERFACE
     (T_STRING interface_extends_list backup_doc_comment "{" class_statement_list "}")
     )

    (extends_from
     %empty
     (T_EXTENDS class_name)
     )

    (interface_extends_list
     %empty
     (T_EXTENDS class_name_list)
     )

    (implements_list
     %empty
     (T_IMPLEMENTS class_name_list)
     )

    (foreach_variable
     variable
     ("&" variable)
     (T_LIST "(" array_pair_list ")")
     ("[" array_pair_list "]")
     )

    (for_statement
     statement
     (":" inner_statement_list T_ENDFOR ";")
     )

    (foreach_statement
     statement
     (":" inner_statement_list T_ENDFOREACH ";")
     )

    (declare_statement
     statement
     (":" inner_statement_list T_ENDDECLARE ";")
     )

    (switch_case_list
     ("{" case_list "}")
     ("{" ";" case_list "}")
     (":" case_list T_ENDSWITCH ";")
     (":" ";" case_list T_ENDSWITCH ";")
     )

    (case_list
     %empty
     (case_list T_CASE expr case_separator inner_statement_list)
     (case_list T_DEFAULT case_separator inner_statement_list)
     )

    (case_separator
     ":"
     ";"
     )

    (match
     (T_MATCH "(" expr ")" "{" match_arm_list "}")
     )

    (match_arm_list
     %empty
     (non_empty_match_arm_list possible_comma)
     )

    (non_empty_match_arm_list
     match_arm
     (non_empty_match_arm_list "," match_arm)
     )

    (match_arm
     (match_arm_cond_list possible_comma T_DOUBLE_ARROW expr)
     (T_DEFAULT possible_comma T_DOUBLE_ARROW expr)
     )

    (match_arm_cond_list
     expr
     (match_arm_cond_list "," expr)
     )

    (while_statement
     statement
     (":" inner_statement_list T_ENDWHILE ";")
     )

    (if_stmt_without_else
     (T_IF "(" expr ")" statement)
     (if_stmt_without_else T_ELSEIF "(" expr ")" statement)
     )

    (if_stmt
     (if_stmt_without_else (%prec T_NOELSE))
     (if_stmt_without_else T_ELSE statement)
     )

    (alt_if_stmt_without_else
     (T_IF "(" expr ")" ":" inner_statement_list)
     (alt_if_stmt_without_else T_ELSEIF "(" expr ")" ":" inner_statement_list)
     )

    (alt_if_stmt
     (alt_if_stmt_without_else T_ENDIF ";")
     (alt_if_stmt_without_else T_ELSE ":" inner_statement_list T_ENDIF ";")
     )

    (parameter_list
     (non_empty_parameter_list possible_comma)
     %empty
     )

    (non_empty_parameter_list
     attributed_parameter
     (non_empty_parameter_list "," attributed_parameter)
     )

    (attributed_parameter
     (attributes parameter)
     parameter
     )

    (optional_visibility_modifier
     %empty
     T_PUBLIC
     T_PROTECTED
     T_PRIVATE
     )

    (parameter
     (optional_visibility_modifier optional_type_without_static is_reference is_variadic T_VARIABLE backup_doc_comment)
     (is_reference is_variadic T_VARIABLE backup_doc_comment)
     (optional_visibility_modifier optional_type_without_static is_reference is_variadic T_VARIABLE backup_doc_comment "=" expr)
     )

    (optional_type_without_static
     %empty
     type_expr_without_static
     )

    (type_expr
     type
     ("?" type)
     union_type
     )

    (type
     type_without_static
     T_STATIC
     )

    (union_type
     (type "|" type)
     (union_type "|" type)
     )

    (type_expr_without_static
     type_without_static
     ("?" type_without_static)
     union_type_without_static)

    (type_without_static
     T_ARRAY
     T_CALLABLE
     name)

    (union_type_without_static
     (type_without_static "|" type_without_static)
     (union_type_without_static "|" type_without_static))

    (return_type
     %empty
     (":" type_expr))

    (argument_list
     ("(" ")")
     ("(" non_empty_argument_list possible_comma ")"))

    (non_empty_argument_list
     argument
     (non_empty_argument_list "," argument))

    (argument
     expr
     (identifier ":" expr)
     (T_ELLIPSIS expr))

    (global_var_list
     (global_var_list "," global_var)
     global_var)

    (global_var
     simple_variable)

    (static_var_list
     (static_var_list "," static_var)
     static_var)

    (static_var
     T_VARIABLE
     (T_VARIABLE "=" expr))

    (class_statement_list
     (class_statement_list class_statement)
     %empty)

    (attributed_class_statement
     (variable_modifiers optional_type_without_static property_list ",")
     (method_modifiers T_CONST class_const_list ";")
     (method_modifiers function returns_ref identifier backup_doc_comment "(" parameter_list ")" return_type backup_fn_flags method_body backup_fn_flags))

    (class_statement
     attributed_class_statement
     (attributes attributed_class_statement)
     (T_USE class_name_list trait_adaptations))

    (class_name_list
     class_name
     (class_name_list "," class_name))

    (trait_adaptations
     ";"
     ("{" "}")
     ("{" trait_adaptation_list "}"))

    (trait_adaptation_list
     trait_adaptation
     (trait_adaptation_list trait_adaptation))

    (trait_adaptation
     (trait_precedence ";")
     (trait_alias ";"))

    (trait_precedence
     (absolute_trait_method_reference T_INSTEADOF class_name_list))

    (trait_alias
     (trait_method_reference T_AS T_STRING)
     (trait_method_reference T_AS reserved_non_modifiers)
     (trait_method_reference T_AS member_modifier identifier)
     (trait_method_reference T_AS member_modifier))

    (trait_method_reference
     identifier
     absolute_trait_method_reference)

    (absolute_trait_method_reference
     (class_name T_PAAMAYIM_NEKUDOTAYIM identifier))

    (method_body
     ";"
     "{" inner_statement_list "}")

    (variable_modifiers
     non_empty_member_modifiers
     T_VAR)

    (method_modifiers
     %empty
     non_empty_member_modifiers)

    (non_empty_member_modifiers
     member_modifier
     (non_empty_member_modifiers member_modifier))

    (member_modifier
     T_PUBLIC
     T_PROTECTED
     T_PRIVATE
     T_STATIC
     T_ABSTRACT
     T_FINAL)

    (property_list
     (property_list "," property)
     property)

    (property
     (T_VARIABLE backup_doc_comment)
     (T_VARIABLE "=" expr backup_doc_comment))

    (class_const_list
     (class_const_list "," class_const_decl)
     class_const_decl)

    (class_const_decl
     (identifier "=" expr backup_doc_comment))

    (const_decl
     (T_STRING "=" expr backup_doc_comment))

    (echo_expr_list
     (echo_expr_list "," echo_expr)
     echo_expr)

    (echo_expr
     expr)

    (for_exprs
     %empty
     non_empty_for_exprs)

    (non_empty_for_exprs
     (non_empty_for_exprs "," expr)
     expr)

    (anonymous_class
     (T_CLASS ctor_arguments extends_from implements_list backup_doc_comment "{" class_statement_list "}"))

    (new_expr
     (T_NEW class_name_reference ctor_arguments)
     (T_NEW anonymous_class)
     (T_NEW attributes anonymous_class))

    (expr
     variable
     (T_LIST "(" array_pair_list ")" "=" expr)
     ("[" array_pair_list "]" "=" expr)
     (variable "=" expr)
     (variable "=" "&" variable)
     (T_CLONE expr)
     (variable T_PLUS_EQUAL expr)
     (variable T_MINUS_EQUAL expr)
     (variable T_MUL_EQUAL expr)
     (variable T_POW_EQUAL expr)
     (variable T_DIV_EQUAL expr)
     (variable T_CONCAT_EQUAL expr)
     (variable T_MOD_EQUAL expr)
     (variable T_AND_EQUAL expr)
     (variable T_OR_EQUAL expr)
     (variable T_XOR_EQUAL expr)
     (variable T_SL_EQUAL expr)
     (variable T_SR_EQUAL expr)
     (variable T_COALESCE_EQUAL expr)
     (variable T_INC)
     (T_INC variable)
     (variable T_DEC)
     (T_DEC variable)
     (expr T_BOOLEAN_OR expr)
     (expr T_BOOLEAN_AND expr)
     (expr T_LOGICAL_OR expr)
     (expr T_LOGICAL_AND expr)
     (expr T_LOGICAL_XOR expr)
     (expr "|" expr)
     (expr "&" expr)
     (expr "^" expr)
     (expr "." expr)
     (expr "+" expr)
     (expr "-" expr)
     (expr "*" expr)
     (expr T_POW expr)
     (expr "/" expr)
     (expr "%" expr)
     (expr T_SL expr)
     (expr T_SR expr)
     ("+" (expr (%prec "~")))
     ("-" (expr (%prec "~")))
     ("!" expr)
     ("~" expr)
     (expr T_IS_IDENTICAL expr)
     (expr T_IS_NOT_IDENTICAL expr)
     (expr T_IS_EQUAL expr)
     (expr T_IS_NOT_EQUAL expr)
     (expr "<" expr)
     (expr T_IS_SMALLER_OR_EQUAL expr)
     (expr ">" expr)
     (expr T_IS_GREATER_OR_EQUAL expr)
     (expr T_SPACESHIP expr)
     (expr T_INSTANCEOF class_name_reference)
     ("(" expr ")")
     new_expr
     (expr "?" expr ":" expr)
     (expr "?" ":" expr)
     (expr T_COALESCE expr)
     internal_functions_in_yacc
     (T_INT_CAST expr)
     (T_DOUBLE_CAST expr)
     (T_STRING_CAST expr)
     (T_ARRAY_CAST expr)
     (T_OBJECT_CAST expr)
     (T_BOOL_CAST expr)
     (T_UNSET_CAST expr)
     (T_EXIT exit_expr)
     ("@" expr)
     scalar
     ("`" backticks_expr "`")
     (T_PRINT expr)
     T_YIELD
     (T_YIELD expr)
     (T_YIELD expr T_DOUBLE_ARROW expr)
     (T_YIELD_FROM expr)
     (T_THROW expr)
     inline_function
     (attributes inline_function)
     (T_STATIC inline_function)
     (attributes T_STATIC inline_function)
     match
     )

    (inline_function
     (function returns_ref backup_doc_comment "(" parameter_list ")" lexical_vars return_type backup_fn_flags "{" inner_statement_list "}" backup_fn_flags)
     (fn returns_ref backup_doc_comment "(" parameter_list ")" return_type T_DOUBLE_ARROW backup_fn_flags backup_lex_pos expr backup_fn_flags)
     )

    (fn
     T_FN)

    (function
     T_FUNCTION)

    (backup_doc_comment
     %empty)

    (backup_fn_flags
     (%empty (%prec PREC_ARROW_FUNCTION)))

    (backup_lex_pos
     %empty)

    (returns_ref
     %empty
     "&")

    (lexical_vars
     %empty
     (T_USE "(" lexical_var_list possible_comma ")")
     )

    (lexical_var_list
     (lexical_var_list "," lexical_var)
     lexical_var
     )

    (lexical_var
     T_VARIABLE
     ("&" T_VARIABLE))

    (function_call
     (name argument_list)
     (class_name T_PAAMAYIM_NEKUDOTAYIM member_name argument_list)
     (variable_class_name T_PAAMAYIM_NEKUDOTAYIM member_name argument_list)
     (callable_expr argument_list))

    (class_name
     T_STATIC
     name)

    (class_name_reference
     class_name
     new_variable
     "(" expr ")")

    (exit_expr
     %empty
     ("(" optional_expr ")"))

    (backticks_expr
     %empty
     T_ENCAPSED_AND_WHITESPACE
     encaps_list)

    (ctor_arguments
     %empty
     argument_list)

    (dereferencable_scalar
     (T_ARRAY "(" array_pair_list ")")
     ("[" array_pair_list "]")
     T_CONSTANT_ENCAPSED_STRING
     ("\"" encaps_list "\"")
     )

    (scalar
     T_LNUMBER
     T_DNUMBER
     (T_START_HEREDOC T_ENCAPSED_AND_WHITESPACE T_END_HEREDOC)
     (T_START_HEREDOC T_END_HEREDOC)
     (T_START_HEREDOC encaps_list T_END_HEREDOC)
     dereferencable_scalar
     constant
     class_constant
     )

    (constant
     name
     T_LINE
     T_FILE
     T_DIR
     T_TRAIT_C
     T_METHOD_C
     T_FUNC_C
     T_NS_C
     T_CLASS_C
     )

    (class_constant
     (class_name T_PAAMAYIM_NEKUDOTAYIM identifier)
     (variable_class_name T_PAAMAYIM_NEKUDOTAYIM identifier)
     )

    (optional_expr
     %empty
     expr
     )

    (variable_class_name
     fully_dereferencable)

    (fully_dereferencable
     variable
     ("(" expr ")")
     dereferencable_scalar
     class_constant
     )

    (array_object_dereferencable
     fully_dereferencable
     constant
     )

    (callable_expr
     callable_variable
     ("(" expr ")")
     dereferencable_scalar
     )

    (callable_variable
     simple_variable
     (array_object_dereferencable "[" optional_expr "]")
     (array_object_dereferencable "{" expr "}")
     (array_object_dereferencable T_OBJECT_OPERATOR property_name argument_list)
     (array_object_dereferencable T_NULLSAFE_OBJECT_OPERATOR property_name argument_list)
     function_call
     )

    (variable
     callable_variable
     static_member
     (array_object_dereferencable T_OBJECT_OPERATOR property_name)
     (array_object_dereferencable T_NULLSAFE_OBJECT_OPERATOR property_name)
     )

    (simple_variable
     T_VARIABLE
     ("$" "{" expr "}")
     ("$" simple_variable)
     )

    (static_member
     (class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable)
     (variable_class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable)
     )

    (new_variable
     simple_variable
     (new_variable "[" optional_expr "]")
     (new_variable "{" expr "}")
     (new_variable T_OBJECT_OPERATOR property_name)
     (new_variable T_NULLSAFE_OBJECT_OPERATOR property_name)
     (class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable)
     (new_variable T_PAAMAYIM_NEKUDOTAYIM simple_variable)
     )

    (member_name
     identifier
     ("{" expr "}")
     simple_variable
     )

    (property_name
     T_STRING
     ("{" expr "}")
     simple_variable
     )

    (array_pair_list
     non_empty_array_pair_list
     )

    (possible_array_pair
     %empty
     array_pair
     )

    (non_empty_array_pair_list
     (non_empty_array_pair_list "," possible_array_pair)
     possible_array_pair
     )

    (array_pair
     (expr T_DOUBLE_ARROW expr)
     expr
     (expr T_DOUBLE_ARROW "&" variable)
     ("&" variable)
     (T_ELLIPSIS expr)
     (expr T_DOUBLE_ARROW T_LIST "(" array_pair_list ")")
     (T_LIST "(" array_pair_list ")")
     )

    (encaps_list
     (encaps_list encaps_var)
     (encaps_list T_ENCAPSED_AND_WHITESPACE)
     encaps_var
     (T_ENCAPSED_AND_WHITESPACE encaps_var)
     )

    (encaps_var
     T_VARIABLE
     (T_VARIABLE "[" encaps_var_offset "]")
     (T_VARIABLE T_OBJECT_OPERATOR T_STRING)
     (T_VARIABLE T_NULLSAFE_OBJECT_OPERATOR T_STRING)
     (T_DOLLAR_OPEN_CURLY_BRACES expr "}")
     (T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME "}")
     (T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME "[" expr "]" "}")
     (T_CURLY_OPEN variable "}")
     )

    (encaps_var_offset
     T_STRING
     T_NUM_STRING
     ("-" T_NUM_STRING)
     T_VARIABLE
     )

    (internal_functions_in_yacc
     (T_ISSET "(" isset_variables possible_comma ")")
     (T_EMPTY "(" expr ")")
     (T_INCLUDE expr)
     (T_INCLUDE_ONCE expr)
     (T_EVAL "(" expr ")")
     (T_REQUIRE expr)
     (T_REQUIRE_ONCE expr)
     )

    (isset_variables
     isset_variable
     (isset_variables "," isset_variable)
     )

    (isset_variable
     expr
     )

    )
  "The productions of grammar.")

(defconst
  phps-mode-automation-grammar-start
  'start
  "The entry-point of grammar.")

(defconst
  phps-mode-automation-grammar-e-identifier
  '%empty
  "The e-identifier of grammar.")

(defconst
  phps-mode-automation-grammar-eof-identifier
  '$
  "The EOF-identifier of grammar.")

(defconst
  phps-mode-automation-grammar-lex-analyzer-function
  (lambda(index)
    (with-current-buffer "*phps-mode-lex-analyzer*"
      (unless (= (point) index)
        (goto-char index))
      (phps-mode-lexer--re2c)
      (when
          (boundp
           'phps-mode-lexer--generated-new-tokens)
        (car
         (nreverse
          phps-mode-lexer--generated-new-tokens)))))
  "The custom lex-analyzer.")

(defconst
  phps-mode-automation-grammar-precendece-attribute
  '%prec
  "The precedence attribute of the grammar.")

(defconst
  phps-mode-automation-grammar-precedence-comparison-function
  #'>
  "The precedence comparison function of the grammar.")

(defconst
  phps-mode-automation-grammar-lex-analyzer-get-function
  (lambda (token)
    (with-current-buffer "*phps-mode-lex-analyzer*"
      (let ((start (car (cdr token)))
            (end (cdr (cdr token))))
        (when (<= end (point-max))
          (buffer-substring-no-properties
           start
           end)))))
  "Fetch token meta data.")


(provide 'phps-mode-automation-grammar)

;;; phps-mode-automation-grammar.el ends here

