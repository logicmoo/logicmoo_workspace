;;; zenscript-parser.el --- ZenScript parser -*- lexical-binding: t -*-

;; Copyright (c) 2020 Eutro

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This module of zenscript-mode provides a ZenScript parser,
;; reading in characters from a buffer, tokenizing them
;; and generating a syntax tree.

;;; Code:

(defun zenscript--skip-ws-and-comments ()
  "Skip across any whitespace characters or comments."
  (skip-syntax-forward " >")
  (when (>= (point-max) (+ (point) 2))
    (let ((ppss (save-excursion
                  (parse-partial-sexp (point)
                                      (+ (point) 2)
                                      () () () t))))
      (when (nth 4 ppss)
        (parse-partial-sexp (point)
                            (point-max)
                            () () ppss 'syntax-table)
        (zenscript--skip-ws-and-comments)))))

(defun zenscript--map* (kwargs &rest kvs)
  "Make a hashmap.

KWARGS are additional arguments to `make-hash-table'.

Each odd element of KVS is a key corresponding to the value
immediately after it."
  (let ((table (apply #'make-hash-table
                      :size (/ (length kvs) 2)
                      kwargs)))
    (while kvs
      (puthash (car kvs) (cadr kvs) table)
      (setq kvs (cddr kvs)))
    table))

(define-hash-table-test 'zenscript--string-test-single
  (lambda (_a _b) t)
  (lambda (k)
    (pcase k
      (?\\ 0)
      (?\' 1)
      (_ 2))))

(define-hash-table-test 'zenscript--string-test-double
  (lambda (_a _b) t)
  (lambda (k)
    (pcase k
      (?\\ 0)
      (?\" 1)
      (_ 2))))

(defconst zenscript--token-dfa-states
  (vector
   (zenscript--map*
    ()
    ?a 1 ?b 1 ?c 1 ?d 1 ?e 1 ?f 1 ?g 1 ?h 1 ?i 1 ?j 1 ?k 1 ?l 1 ?m 1
    ?n 1 ?o 1 ?p 1 ?q 1 ?r 1 ?s 1 ?t 1 ?u 1 ?v 1 ?w 1 ?x 1 ?y 1 ?z 1
    ?A 1 ?B 1 ?C 1 ?D 1 ?E 1 ?F 1 ?G 1 ?H 1 ?I 1 ?J 1 ?K 1 ?L 1 ?M 1
    ?N 1 ?O 1 ?P 1 ?Q 1 ?R 1 ?S 1 ?T 1 ?U 1 ?V 1 ?W 1 ?X 1 ?Y 1 ?Z 1
    ?_ 1

    ?\{ 2 ?\} 3 ?\[ 4 ?\] 5

    ?. 6 ?, 8

    ?+ 9 ?- 11 ?* 13 ?/ 15 ?% 17

    ?| 19 ?& 22

    ?^ 25

    ?? 27 ?: 28 ?\( 29 ?\) 30

    ?~ 31

    ?\; 33

    ?< 34 ?> 36 ?= 38 ?! 40

    ?$ 42

    ?\' 43 ?\" 49

    ?0 56
    ?1 58 ?2 58 ?3 58 ?4 58 ?5 58 ?6 58 ?7 58 ?8 58 ?9 58)
   (zenscript--map* ; identifiers
    ()
    ?a 1 ?b 1 ?c 1 ?d 1 ?e 1 ?f 1 ?g 1 ?h 1 ?i 1 ?j 1 ?k 1 ?l 1 ?m 1
    ?n 1 ?o 1 ?p 1 ?q 1 ?r 1 ?s 1 ?t 1 ?u 1 ?v 1 ?w 1 ?x 1 ?y 1 ?z 1
    ?A 1 ?B 1 ?C 1 ?D 1 ?E 1 ?F 1 ?G 1 ?H 1 ?I 1 ?J 1 ?K 1 ?L 1 ?M 1
    ?N 1 ?O 1 ?P 1 ?Q 1 ?R 1 ?S 1 ?T 1 ?U 1 ?V 1 ?W 1 ?X 1 ?Y 1 ?Z 1
    ?_ 1 ?0 1 ?1 1 ?2 1 ?3 1 ?4 1 ?5 1 ?6 1 ?7 1 ?8 1 ?9 1)
   (zenscript--map* ()) ; {
   (zenscript--map* ()) ; }
   (zenscript--map* ()) ; [
   (zenscript--map* ()) ; ]
   (zenscript--map* () ?. 7) ; .
   (zenscript--map* ()) ; ..
   (zenscript--map* ()) ; ,
   (zenscript--map* () ?= 10) ; +
   (zenscript--map* ()) ; +=
   (zenscript--map*
    ()
    ?= 12
    ?0 56
    ?1 58 ?2 58 ?3 58 ?4 58 ?5 58 ?6 58 ?7 58 ?8 58 ?9 58) ; -
   (zenscript--map* ()) ; -=
   (zenscript--map* () ?= 14) ; *
   (zenscript--map* ()) ; *=
   (zenscript--map* () ?= 16) ; /
   (zenscript--map* ()); /=
   (zenscript--map* () ?= 18) ; %
   (zenscript--map* ()) ; %=
   (zenscript--map* () ?= 20 ?| 21) ; |
   (zenscript--map* ()) ; |=
   (zenscript--map* ()) ; ||
   (zenscript--map* () ?= 23 ?& 24) ; &
   (zenscript--map* ()) ; &=
   (zenscript--map* ()) ; &&
   (zenscript--map* () ?= 26) ; ^
   (zenscript--map* ()) ; ^=
   (zenscript--map* ()) ; ?
   (zenscript--map* ()) ; :
   (zenscript--map* ()) ; (
   (zenscript--map* ()) ; )
   (zenscript--map* () ?= 32) ; ~
   (zenscript--map* ()) ; ~=
   (zenscript--map* ()) ; ;
   (zenscript--map* () ?= 35) ; <
   (zenscript--map* ()) ; <=
   (zenscript--map* () ?= 37) ; >
   (zenscript--map* ()) ; >=
   (zenscript--map* () ?= 39) ; =
   (zenscript--map* ()) ; ==
   (zenscript--map* () ?= 41) ; !
   (zenscript--map* ()) ; !=
   (zenscript--map* ()) ; $
   ;; ' strings
   (zenscript--map*
    '(:test zenscript--string-test-single)
    ?\\ 44
    ?' 55
    ?. 43)
   (zenscript--map*
    ()
    ?u 45
    ?\' 43 ?\" 43 ?\\ 43 ?/ 43 ?b 43 ?f 43 ?n 43 ?r 43 ?t 43)
   (zenscript--map*
    ()
    ?0 46 ?1 46 ?2 46 ?3 46 ?4 46 ?5 46 ?6 46 ?7 46 ?8 46 ?9 46
    ?a 46 ?b 46 ?c 46 ?d 46 ?e 46 ?f 46
    ?A 46 ?B 46 ?C 46 ?D 46 ?E 46 ?F 46)
   (zenscript--map*
    ()
    ?0 47 ?1 47 ?2 47 ?3 47 ?4 47 ?5 47 ?6 47 ?7 47 ?8 47 ?9 47
    ?a 47 ?b 47 ?c 47 ?d 47 ?e 47 ?f 47
    ?A 47 ?B 47 ?C 47 ?D 47 ?E 47 ?F 47)
   (zenscript--map*
    ()
    ?0 48 ?1 48 ?2 48 ?3 48 ?4 48 ?5 48 ?6 48 ?7 48 ?8 48 ?9 48
    ?a 48 ?b 48 ?c 48 ?d 48 ?e 48 ?f 48
    ?A 48 ?B 48 ?C 48 ?D 48 ?E 48 ?F 48)
   (zenscript--map*
    ()
    ?0 43 ?1 43 ?2 43 ?3 43 ?4 43 ?5 43 ?6 43 ?7 43 ?8 43 ?9 43
    ?a 43 ?b 43 ?c 43 ?d 43 ?e 43 ?f 43
    ?A 43 ?B 43 ?C 43 ?D 43 ?E 43 ?F 43)
   ;; " strings
   (zenscript--map*
    '(:test zenscript--string-test-double)
    ?\\ 50
    ?\" 55
    ?. 49)
   (zenscript--map*
    ()
    ?u 51
    ?\' 49 ?\" 49 ?\\ 49 ?/ 49 ?b 49 ?f 49 ?n 49 ?r 49 ?t 49)
   (zenscript--map*
    ()
    ?0 52 ?1 52 ?2 52 ?3 52 ?4 52 ?5 52 ?6 52 ?7 52 ?8 52 ?9 52
    ?a 52 ?b 52 ?c 52 ?d 52 ?e 52 ?f 52
    ?A 52 ?B 52 ?C 52 ?D 52 ?E 52 ?F 52)
   (zenscript--map*
    ()
    ?0 53 ?1 53 ?2 53 ?3 53 ?4 53 ?5 53 ?6 53 ?7 53 ?8 53 ?9 53
    ?a 53 ?b 53 ?c 53 ?d 53 ?e 53 ?f 53
    ?A 53 ?B 53 ?C 53 ?D 53 ?E 53 ?F 53)
   (zenscript--map*
    ()
    ?0 54 ?1 54 ?2 54 ?3 54 ?4 54 ?5 54 ?6 54 ?7 54 ?8 54 ?9 54
    ?a 54 ?b 54 ?c 54 ?d 54 ?e 54 ?f 54
    ?A 54 ?B 54 ?C 54 ?D 54 ?E 54 ?F 54)
   (zenscript--map*
    ()
    ?0 49 ?1 49 ?2 49 ?3 49 ?4 49 ?5 49 ?6 49 ?7 49 ?8 49 ?9 49
    ?a 49 ?b 49 ?c 49 ?d 49 ?e 49 ?f 49
    ?A 49 ?B 49 ?C 49 ?D 49 ?E 49 ?F 49)
   (zenscript--map* ()) ;; finished strings
   ;; 0 start number
   (zenscript--map*
    ()
    ?x 57
    ?. 59) ; 0
   ;; hex numbers
   ;; -0x... shouldn't actually go here,
   ;; but it doesn't make a difference:
   ;; -0x... is valid ZenScript anyway,
   ;; it just gets immediately unary operator-ed
   (zenscript--map*
    ()
    ?0 57 ?1 57 ?2 57 ?3 57 ?4 57 ?5 57 ?6 57 ?7 57 ?8 57 ?9 57
    ?a 57 ?b 57 ?c 57 ?d 57 ?e 57 ?f 57
    ?A 57 ?B 57 ?C 57 ?D 57 ?E 57 ?F 57)
   ;; integers
   (zenscript--map*
    ()
    ?0 58 ?1 58 ?2 58 ?3 58 ?4 58 ?5 58 ?6 58 ?7 58 ?8 58 ?9 58
    ?. 59)
   ;; floats
   (zenscript--map*
    ()
    ?0 59 ?1 59 ?2 59 ?3 59 ?4 59 ?5 59 ?6 59 ?7 59 ?8 59 ?9 59
    ?e 60 ?E 60
    ?f 63 ?F 63 ?d 63 ?D 63) ;; required first digit
   (zenscript--map*
    ()
    ?0 59 ?1 59 ?2 59 ?3 59 ?4 59 ?5 59 ?6 59 ?7 59 ?8 59 ?9 59
    ?e 60 ?E 60
    ?f 63 ?F 63 ?d 63 ?D 63) ;; repeated digits (terminal)
   (zenscript--map*
    ()
    ?0 62 ?1 62 ?2 62 ?3 62 ?4 62 ?5 62 ?6 62 ?7 62 ?8 62 ?9 62
    ?+ 61 ?- 61) ;; [eE][\+\-]?[0-9]+
   (zenscript--map*
    () ;; [0-9]+ required first digit
    ?0 62 ?1 62 ?2 62 ?3 62 ?4 62 ?5 62 ?6 62 ?7 62 ?8 62 ?9 62
    ?f 63 ?F 63 ?d 63 ?D 63)
   (zenscript--map*
    ()
    ?0 62 ?1 62 ?2 62 ?3 62 ?4 62 ?5 62 ?6 62 ?7 62 ?8 62 ?9 62
    ?f 63 ?F 63 ?d 63 ?D 63) ;; [0-9]+ remaining digits (terminal)
   (zenscript--map* ()) ;; post-[fFdD] (terminal)
   )
  "A vector of states for the DFA.

Each state is a hash-table of characters
to the next state index.")

(defconst zenscript--token-dfa-finals
  (zenscript--map*
   ()
   1 'T_ID
   2 'T_AOPEN
   3 'T_ACLOSE
   4 'T_SQBROPEN
   5 'T_SQBRCLOSE
   6 'T_DOT
   7 'T_DOT2
   8 'T_COMMA
   9 'T_PLUS
   10 'T_PLUSASSIGN
   11 'T_MINUS
   12 'T_MINUSASSIGN
   13 'T_MUL
   14 'T_MULASSIGN
   15 'T_DIV
   16 'T_DIVASSIGN
   17 'T_MOD
   18 'T_MODASSIGN
   19 'T_OR
   20 'T_ORASSIGN
   21 'T_OR2
   22 'T_AND
   23 'T_ANDASSIGN
   24 'T_AND2
   25 'T_XOR
   26 'T_XORASSIGN
   27 'T_QUEST
   28 'T_COLON
   29 'T_BROPEN
   30 'T_BRCLOSE
   31 'T_TILDE
   32 'T_TILDEASSIGN
   33 'T_SEMICOLON
   34 'T_LT
   35 'T_LTEQ
   36 'T_GT
   37 'T_GTEQ
   38 'T_ASSIGN
   39 'T_EQ
   40 'T_NOT
   41 'T_NOTEQ
   42 'T_DOLLAR
   55 'T_STRINGVALUE
   56 'T_INTVALUE
   57 'T_INTVALUE
   58 'T_INTVALUE
   59 'T_FLOATVALUE
   62 'T_FLOATVALUE
   63 'T_FLOATVALUE)
  "A hashmap of final states indeces.

Each final state is mapped to the token it represents.")

(defun zenscript--tokenize-buffer (&optional from to no-error)
  "Read the buffer into a list of tokens.

FROM is the start position, and defaults to `point-min'.

TO is the end position, and defaults to `point-max'.

If a token is unrecognised, and NO-ERROR is nil,
'zenscript-unrecognised-token is thrown with point.
If NO-ERROR is non-nil, then parsing stops instead, returning the partially
accumulated list of tokens, and leaving point where it is.

If parsing concludes, then point is left at TO.

Note: this uses the syntax table to handle comments."
  (goto-char (or from (point-min)))
  (let ((to (or to (point-max)))
        (continue t)
        tokens)
    (zenscript--skip-ws-and-comments)
    (while (and continue (char-after))
      (let ((start (point))
            (next-token (zenscript--next-token)))
        (when (or (>= (point) to)
                  (not next-token))
          (setq continue ()))
        (if next-token
            (if (> (point) to)
                (goto-char start)
              (setq tokens (cons next-token tokens))
              (when (< (point) to)
                (zenscript--skip-ws-and-comments)))
          (unless no-error
            (throw 'zenscript-unrecognized-token
                   (point))))))
    (reverse tokens)))

(defconst zenscript--keyword-map
  (zenscript--map*
   '(:test equal)
   "frigginConstructor" 'T_ZEN_CONSTRUCTOR
   "zenConstructor" 'T_ZEN_CONSTRUCTOR
   "frigginClass" 'T_ZEN_CLASS
   "zenClass" 'T_ZEN_CLASS
   "instanceof" 'T_INSTANCEOF
   "static" 'T_STATIC
   "global" 'T_GLOBAL
   "import" 'T_IMPORT
   "false" 'T_FALSE
   "true" 'T_TRUE
   "null" 'T_NULL
   "break" 'T_BREAK
   "while" 'T_WHILE
   "val" 'T_VAL
   "var" 'T_VAR
   "return" 'T_RETURN
   "for" 'T_FOR
   "else" 'T_ELSE
   "if" 'T_IF
   "version" 'T_VERSION
   "as" 'T_AS
   "void" 'T_VOID
   "has" 'T_IN
   "in" 'T_IN
   "function" 'T_FUNCTION
   "string" 'T_STRING
   "double" 'T_DOUBLE
   "float" 'T_FLOAT
   "long" 'T_LONG
   "int" 'T_INT
   "short" 'T_SHORT
   "byte" 'T_BYTE
   "bool" 'T_BOOL
   "any" 'T_ANY)
  "A hash-table of keywords to tokens.")

(defun zenscript--next-token (&optional skip-whitespace)
  "Parse the next ZenScript token after point.

If SKIP-WHITESPACE is non-nil, whitespace and comments
are skipped according to `syntax-table'.

Return a list of the form

 (type val pos)

or nil if no token was recognised.

type:

  The type of the token, as seen here
  https://docs.blamejared.com/1.12/en/Dev_Area/ZenTokens/

val:

  The string value of the token.

pos:

  The position at which the token occured.

file:

  The file name of the buffer from which the token was read.

point is put after token, if one was found."
  (let ((begin (point)))
    (when skip-whitespace (zenscript--skip-ws-and-comments))
    (let ((token-start (point))
          (state 0))
      (while (and (char-after)
                  (let ((newstate
                         (gethash (char-after)
                                  (aref zenscript--token-dfa-states
                                        state))))
                    (when newstate (setq state newstate))
                    newstate))
        (forward-char))
      (let ((type (gethash state zenscript--token-dfa-finals))
            (val (buffer-substring-no-properties token-start (point))))
        (when (eq type 'T_ID)
          (setq type
                (or (gethash val zenscript--keyword-map)
                    'T_ID)))
        (if type
            (list type val token-start)
          (goto-char begin)
          ())))))

(defmacro zenscript--cdr! (list)
  "Set LIST to the cdr of LIST."
  `(setq ,list (cdr ,list)))

(defmacro zenscript--cons! (car list)
  "Do (cons CAR LIST) and set LIST to it."
  `(setq ,list (cons ,car ,list)))

(defvar zenscript-parse-error-hook ()
  "A list of hooks run when an error occurs while parsing.

Each hook is called with the error message and the token where it occured.")

(defun zenscript--throw-parse-error (message token)
  "Run `zenscript-parse-error-hook' and throw 'zenscript-parse-error with (MESSAGE TOKEN)."
  (run-hook-with-args 'zenscript-parse-error-hook message token)
  (throw 'zenscript-parse-error
         (list message token)))

(defun zenscript--make-tokenstream (token-list)
  "Make a tokenstream from a list of tokens, TOKEN-LIST, as returned by `zenscript--tokenize-buffer'."
  (lambda (op &rest args)
    (pcase op
      ('PEEK (car token-list))
      ('NEXT (prog1 (car token-list)
               (zenscript--cdr! token-list)))
      ('OPTIONAL (when (eq (car args) (caar token-list))
                   (prog1 (car token-list)
                     (zenscript--cdr! token-list))))
      ('REQUIRE (if (eq (car args) (caar token-list))
                    (prog1 (car token-list)
                      (zenscript--cdr! token-list))
                  (zenscript--throw-parse-error
                   (cadr args) (car token-list)))))))

(defun zenscript--require-token (type tokens message)
  "Require a token of type TYPE from TOKENS.

Return the first token if it is of the correct type, otherwise
zenscript--throw-parse-error with MESSAGE.

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (funcall tokens 'REQUIRE type message))

(defun zenscript--peek-token (tokens)
  "Look at the next token in the stream TOKENS, without consuming it.

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (funcall tokens 'PEEK))

(defun zenscript--get-token (tokens)
  "Get the next token in the stream TOKENS, consuming it.

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (funcall tokens 'NEXT))

(defun zenscript--optional-token (type tokens)
  "Get the next token if it is of type TYPE.

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (funcall tokens 'OPTIONAL type))

(defun zenscript--has-next-token (tokens)
  "Return t if TOKENS has any more tokens remaining.

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (when (zenscript--peek-token tokens) t))

(defun zenscript--parse-tokens (tokenlist)
  "Parse a list of ZenScript tokens.

TOKENLIST is a list of tokens as returned by `zenscript--tokenize-buffer'.

Returns a list of the form

 (imports functions zenclasses statements globals)

Which are lists of elements of the formats:

imports:

  (fqname pos rename)

  fqname:

    A list of strings representing the fully qualified name.

  pos:

    The position at which the import appears.

  rename:

    The name by which fqname should be referenced, or nil
    if the last name in fqname should be used.

functions:

  See `zenscript--parse-function'.

zenclasses:

  See `zenscript--parse-zenclass'.

statements:

  See `zenscript--parse-statement'.

globals:

  See `zenscript--parse-global'."
  (let ((tokens (zenscript--make-tokenstream tokenlist))
        imports functions zenclasses statements globals)
    (catch 'zenscript-parse-error
      (while (and (zenscript--has-next-token tokens)
                  (eq (car (zenscript--peek-token tokens))
                      'T_IMPORT))
        (let (fqname
              (pos (nth 2 (zenscript--get-token tokens)))
              rename)

          (zenscript--cons! (cadr (zenscript--require-token 'T_ID tokens
                                                            "identifier expected"))
                            fqname)
          (while (zenscript--optional-token 'T_DOT tokens)
            (zenscript--cons! (cadr (zenscript--require-token 'T_ID tokens
                                                              "identifier expected"))
                              fqname))

          (when (zenscript--optional-token 'T_AS tokens)
            (setq rename (cadr (zenscript--require-token 'T_ID tokens
                                                         "identifier expected"))))

          (zenscript--require-token 'T_SEMICOLON tokens
                                    "; expected")

          (zenscript--cons! (list (reverse fqname)
                                  pos
                                  rename)
                            imports))))
    (while (zenscript--has-next-token tokens)
      (catch 'zenscript-parse-error
        (pcase (car (zenscript--peek-token tokens))
          ((or 'T_GLOBAL 'T_STATIC)
           (zenscript--cons! (zenscript--parse-global tokens)
                             globals))
          ('T_FUNCTION
           (zenscript--cons! (zenscript--parse-function tokens)
                             functions))
          ('T_ZEN_CLASS
           (zenscript--cons! (zenscript--parse-zenclass tokens)
                             zenclasses))
          (_ (zenscript--cons! (zenscript--parse-statement tokens)
                               statements)))))
    (list (reverse imports)
          (reverse functions)
          (reverse zenclasses)
          (reverse statements)
          (reverse globals))))

(defun zenscript--parse-function (tokens)
  "Parse the next static function definition from TOKENS.

Return a list of the form:

 (name arguments type statements start end)

name:

  The token that is the name of the function.

arguments:

  A list of arguments as returned by `zenscript--parse-function-arguments'.

type:

  The ZenType that the function returns.

statements:

  A list of statements, which are as returned by `zenscript--parse-statement'.

start:

  The position after the opening {

end:

  The position before the closing }

function name(argname [as type], argname [as type], ...) [as type] {
  ...statements...
}"
  (zenscript--require-token 'T_FUNCTION tokens
                            "function expected")
  (let ((name (zenscript--require-token 'T_ID tokens
                                        "identifier expected"))
        (arguments (zenscript--parse-function-arguments tokens))
        (type (if (zenscript--optional-token 'T_AS tokens)
                  (zenscript--parse-zentype tokens)
                '(C_RAW "any")))
        (start (1+ (nth 2 (zenscript--require-token 'T_AOPEN tokens
                                                    "{ expected"))))
        statements end)
    (while (and (zenscript--has-next-token tokens)
                (not (let ((close (zenscript--optional-token 'T_ACLOSE tokens)))
                       (and close (setq end (nth 2 close))))))
      (zenscript--cons! (zenscript--parse-statement tokens) statements))
    (list name
          arguments
          type
          (reverse statements)
          start
          end)))

(defun zenscript--parse-function-arguments (tokens)
  "Parse a list of function arguments from TOKENS.

A list of arguments of the form:

 (name type)

name:

  The token that is the identifier of this binding.

type:

  The ZenType of this binding."
  (let (arguments)
    (zenscript--require-token 'T_BROPEN tokens
                              "( expected")
    (unless (zenscript--optional-token 'T_BRCLOSE tokens)
      (let (break)
        (while (not break)
          (zenscript--cons! (list (zenscript--require-token 'T_ID tokens
                                                            "identifier expected")
                                  (if (zenscript--optional-token 'T_AS tokens)
                                      (zenscript--parse-zentype tokens)
                                    '(C_RAW "any")))
                            arguments)
          (unless (zenscript--optional-token 'T_COMMA tokens)
            (zenscript--require-token 'T_BRCLOSE tokens
                                      ") or , expected")
            (setq break t)))))
    (reverse arguments)))

(defun zenscript--parse-zenclass (tokens)
  "Parse the next class definition from TOKENS.

A list of the form:

 (name fields constructors methods start end)

name:

  The token that is the name of this class.

fields:

  A list of fields, which are as returned by
  `zenscript--parse-zenclass-field'.

constructors:

  A list of constructors, which are as returned by
  `zenscript--parse-zenclass-constructor'.

methods:

  A list of methods, which are as returned by
  `zenscript--parse-zenclass-method'.

start:

  The position after the opening {

end:

  The position before the closing }"
  (zenscript--require-token 'T_ZEN_CLASS tokens
                            "zenClass expected")
  (let ((id (zenscript--require-token 'T_ID tokens
                                      "identifier expected"))
        (start (1+ (nth 2 (zenscript--require-token 'T_AOPEN tokens
                                                    "{ expected"))))
        keyword
        fields constructors methods)
    (while (setq keyword
                 (or (zenscript--optional-token 'T_VAL tokens)
                     (zenscript--optional-token 'T_VAR tokens)
                     (zenscript--optional-token 'T_STATIC tokens)
                     (zenscript--optional-token 'T_ZEN_CONSTRUCTOR tokens)
                     (zenscript--optional-token 'T_FUNCTION tokens)))
      (pcase (car keyword)
        ((or 'T_VAL 'T_VAR
             'T_STATIC)
         (zenscript--cons! (zenscript--parse-zenclass-field tokens (eq (car keyword)
                                                                       'T_STATIC))
                           fields))
        ('T_ZEN_CONSTRUCTOR
         (zenscript--cons! (zenscript--parse-zenclass-constructor tokens)
                           constructors))
        ('T_FUNCTION
         (zenscript--cons! (zenscript--parse-zenclass-method tokens)
                           methods))))
    (list id
          (reverse fields)
          (reverse constructors)
          (reverse methods)
          start
          (nth 2 (zenscript--require-token 'T_ACLOSE tokens
                                           "} expected")))))

(defun zenscript--parse-zenclass-field (tokens static)
  "Parse a field definition of a ZenClass from TOKENS.

A list of the form:

 (name type init static)

name:

  The token that is the name of this field.

type:

  The ZenType of the field.

init:

  The expression by which this field is initialized.

static:

  t if the field is static, nil otherwise.

STATIC should be true if the class field is static."
  (let ((id (zenscript--require-token 'T_ID tokens
                                      "identifier expected"))
        (type (if (zenscript--optional-token 'T_AS tokens)
                  (zenscript--parse-zentype tokens)
                '(C_RAW "any")))
        (init (when (zenscript--optional-token 'T_ASSIGN tokens)
                (zenscript--parse-expression tokens))))
    (zenscript--require-token 'T_SEMICOLON tokens
                              "; expected")
    (list id type init static)))

(defun zenscript--parse-zenclass-constructor (tokens)
  "Parse a constructor definition of a ZenClass from TOKENS.

A list of the form:

 (arguments statements start end)

arguments:

  The list of arguments, as returned by `zenscript--parse-function-arguments'.

statements:

  A list of statements, which are as returned by `zenscript--parse-statement'.

start:

  The position after the opening {

end:

  The position before the closing }"
  (let ((arguments (zenscript--parse-function-arguments tokens))
        (start (1+ (nth 2 (zenscript--require-token 'T_AOPEN tokens
                                                    "{ expected"))))
        statements end)
    (while (and (zenscript--has-next-token tokens)
                (not (let ((close (zenscript--optional-token 'T_ACLOSE tokens)))
                       (and close (setq end (nth 2 close))))))
      (zenscript--cons! (zenscript--parse-statement tokens) statements))
    (list arguments
          (reverse statements)
          start
          end)))

(defun zenscript--parse-zenclass-method (tokens)
  "Parse a method definition of a ZenClass from TOKENS.

A list of the form:

 (name arguments type statements start end)

name:

  The token that is the name of this method.

arguments:

  The list of arguments, as returned by `zenscript--parse-function-arguments'.

type:

  The ZenType that the method returns.

statements:

  A list of statements, which are as returned by `zenscript--parse-statement'.

start:

  The position after the opening {

end:

  The position before the closing }"
  (let ((id (zenscript--require-token 'T_ID tokens
                                      "identifier expected"))
        (arguments (zenscript--parse-function-arguments tokens))
        (type (if (zenscript--optional-token 'T_AS tokens)
                  (zenscript--parse-zentype tokens)
                '(C_RAW "any")))
        (start (1+ (nth 2 (zenscript--require-token 'T_AOPEN tokens
                                                    "{ expected"))))
        statements end)
    (while (and (zenscript--has-next-token tokens)
                (not (let ((close (zenscript--optional-token 'T_ACLOSE tokens)))
                       (and close (setq end (nth 2 close))))))
      (zenscript--cons! (zenscript--parse-statement tokens) statements))
    (list id
          arguments
          type
          (reverse statements)
          start
          end)))

(defun zenscript--parse-statement (tokens)
  "Parse the next statement from TOKENS.

A list of the form:

 (type start after . value)

type:

  The type of the statement, see below for possible values.

start:

  The position at which this statement started.

after:

  The position of the next token after this statement, or `point-max'.

value:

  The value of the statement, which varies by type.  See below.

The following types are possible:

  S_BLOCK:

    value: (statements end)

    statements:

      A list of statements, which are as returned by
      `zenscript--parse-statement'.

    end:

      The position after the closing }

  S_RETURN:

    value: (expression)

    expression:

      An expression that is the value of this return
      statement.  See `zenscript--parse-expression'.

  S_VAR:

    value: (name type initializer final)

    name:

      The token that is the name of this variable.

    type:

      The explicit ZenType of this variable.

    initializer:

      The expression that initializes this variable.

    final:

      t if this variable is final, nil otherwise.

  S_IF:

    value: (predicate then else)

    predicate:

      The expression that is evaluated to determine
      whether to evaluate THEN or ELSE.

    then:

      The statement to evaluate if predicate is true.

    else:

      The statement to evaluate if predicate is false.

  S_FOR:

    value: (names expression statement)

    names:

      A list of tokens that are bound variables.

    expression:

      The expression of what is being iterated over.

    statement:

      The expression to run in this for loop.

  S_WHILE:

    value: (predicate statement)

    predicate:

      The expression to test if the loop should be run.

    statement:

      The statement that is run each iteration.

  S_BREAK:

    value: ()

  S_CONTINUE:

    value: ()

  S_EXPRESSION:

    value: (expression)

    expression:

      The expression to evaluate as a statement.
      See `zenscript--parse-statement'.

  S_INVALID:

    value: (message)

      An invalid expression, something went wrong while parsing."
  (let* ((next (zenscript--peek-token tokens))
         (caught
          (catch 'zenscript-parse-error
            (pcase (car next)
              ('T_AOPEN
               (zenscript--get-token tokens)
               (let (statements end)
                 (while (and (zenscript--has-next-token tokens)
                             (not (let ((close (zenscript--optional-token 'T_ACLOSE tokens)))
                                    (and close (setq end (nth 2 close))))))
                   (zenscript--cons! (zenscript--parse-statement tokens) statements))
                 (list 'S_BLOCK (reverse statements) end)))
              ('T_RETURN
               (zenscript--get-token tokens)
               (list 'S_RETURN
                     (prog1
                         (unless (eq 'T_SEMICOLON
                                     (car (zenscript--peek-token tokens)))
                           (zenscript--parse-expression tokens))
                       (zenscript--require-token 'T_SEMICOLON tokens
                                                 "; expected"))))
              ((or 'T_VAR
                   'T_VAL)
               (zenscript--get-token tokens)
               (let* ((id (zenscript--require-token 'T_ID tokens
                                                    "identifier expected"))
                      initializer
                      type)
                 (when (zenscript--optional-token 'T_AS tokens)
                   (setq type (zenscript--parse-zentype tokens)))
                 (when (zenscript--optional-token 'T_ASSIGN tokens)
                   (setq initializer (zenscript--parse-expression tokens)))
                 (zenscript--require-token 'T_SEMICOLON tokens
                                           "; expected")
                 (list 'S_VAR id type initializer
                       (eq (car next)
                           'T_VAL))))
              ('T_IF
               (zenscript--get-token tokens)
               (list 'S_IF
                     (zenscript--parse-expression tokens)
                     (zenscript--parse-statement tokens)
                     (when (zenscript--optional-token 'T_ELSE tokens)
                       (zenscript--parse-statement tokens))))
              ('T_FOR
               (zenscript--get-token tokens)
               (list 'S_FOR
                     (let (break
                           names)
                       (zenscript--cons! (zenscript--require-token 'T_ID tokens
                                                                   "identifier expected")
                                         names)
                       (while (not break)
                         (if (zenscript--optional-token 'T_COMMA tokens)
                             (zenscript--cons! (zenscript--require-token 'T_ID tokens
                                                                         "identifier expected")
                                               names)
                           (setq break t)))
                       (reverse names))
                     (progn
                       (zenscript--require-token 'T_IN tokens
                                                 "in expected")
                       (zenscript--parse-expression tokens))
                     (zenscript--parse-statement tokens)))
              ('T_WHILE
               (zenscript--get-token tokens)
               (list 'S_WHILE
                     (zenscript--parse-expression tokens)
                     (zenscript--parse-statement tokens)))
              ('T_BREAK
               (zenscript--get-token tokens)
               (zenscript--require-token 'T_SEMICOLON tokens
                                         "; expected")
               (list 'S_BREAK))
              ('T_CONTINUE
               (zenscript--get-token tokens)
               (zenscript--require-token 'T_SEMICOLON tokens
                                         "; expected")
               (list 'S_CONTINUE))
              (_
               (list 'S_EXPRESSION
                     (prog1 (zenscript--parse-expression tokens)
                       (zenscript--require-token 'T_SEMICOLON tokens
                                                 "; expected")))))))
         (statement (if (stringp caught)
                        (list 'S_INVALID caught)
                      caught)))
    (cons
     (car statement)
     (cons
      (nth 2 next)
      (cons
       (or (nth 2 (zenscript--peek-token tokens))
           (point-max))
       (cdr statement))))))

(defun zenscript--parse-zentype (tokens)
  "Parse the next ZenType from TOKENS.

A ZenType is represented as a list of the following format:

 (category . value)

value:

  The value of the ZenType.  What VALUE is depends on
  the type, described below.

category:

  A symbol, the category of the ZenType.
  This may be any of those below.  The format of
  VALUE is also written for each entry:

  C_RAW: (name)

    A raw type is the simplest possible type.
    It has one value, its name, a string denoting
    its fully qualified name, as it may be imported
    or referenced without import.

    name:

      The name of the ZenType.

  C_FUNCTION: (argument-types return-type)

    A function type.  This is the type of
    a function object that can be called.

    argument-types:

      A list of ZenTypes that are the types
      of the function arguments.

    return-type:

      The ZenType returned by calling this function.

  C_LIST: (elem-type)

    A list type.  This would be equivalent to
    java.util.List<elem-type> in Java.

    elem-type:

      The ZenType of elements in the list.

  C_ARRAY: (elem-type)

    A Java array type.  This would be equivalent to
    elem-type[] in Java.

    elem-type:

      The ZenType of elements in the array.

  C_ASSOCIATIVE: (key-type val-type)

    A map type.  This would be equivalent to
    java.util.Map<key-type, val-type> in Java.

    key-type:

      The ZenType of keys in this map.

    val-type:

      The ZenType of values in this map.

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (let ((next (zenscript--get-token tokens))
        base)
    (pcase (car next)
      ('T_ANY (setq base '(C_RAW "any")))
      ('T_VOID (setq base '(C_RAW "void")))
      ('T_BOOL (setq base '(C_RAW "bool")))
      ('T_BYTE (setq base '(C_RAW "byte")))
      ('T_SHORT (setq base '(C_RAW "short")))
      ('T_INT (setq base '(C_RAW "int")))
      ('T_LONG (setq base '(C_RAW "long")))
      ('T_FLOAT (setq base '(C_RAW "float")))
      ('T_DOUBLE (setq base '(C_RAW "double")))
      ('T_STRING (setq base '(C_RAW "string")))
      ('T_ID
       (let ((type-name (cadr next)))
         (while (zenscript--optional-token 'T_DOT tokens)
           (setq type-name
                 (concat type-name "."
                         (cadr (zenscript--require-token 'T_ID tokens
                                                         "identifier expected")))))
         (setq base (list 'C_RAW type-name))))
      ('T_FUNCTION
       (let (argument-types)
         (zenscript--require-token 'T_BROPEN tokens
                                   "( expected")
         (unless (zenscript--optional-token 'T_BRCLOSE tokens)
           (zenscript--cons! (zenscript--parse-zentype tokens) argument-types)
           (while (zenscript--optional-token 'T_COMMA tokens)
             (zenscript--cons! (zenscript--parse-zentype tokens) argument-types))
           (zenscript--require-token 'T_BRCLOSE tokens
                                     ") expected"))
         (setq base (list 'C_FUNCTION (reverse argument-types) (zenscript--parse-zentype tokens)))))
      ('T_SQBROPEN
       (setq base (list 'C_LIST (zenscript--parse-zentype tokens)))
       (zenscript--require-token 'T_SQBRCLOSE tokens
                                 "] expected"))
      (_ (zenscript--throw-parse-error
          (format "Unknown type: %s" (cadr next))
          next)))
    (while (zenscript--optional-token 'T_SQBROPEN tokens)
      (if (zenscript--optional-token 'T_SQBRCLOSE tokens)
          (setq base (list 'C_ARRAY base))
        (setq base (list 'C_ASSOCIATIVE base (zenscript--parse-zentype tokens)))
        (zenscript--require-token 'T_SQBRCLOSE tokens
                                  "] expected")))
    base))

(defun zenscript--parse-expression (tokens)
  "Parse the next expression from TOKENS.

An expression is a list of the form:

 (type . value)

type:

  The type of expression.  See below for possible expressions.

value:

  The value of the expression, varies by type.

Each layer of `zenscript--parse-<layer>` except the last
delegates to a layer below.  See each layer for
details on the possible expression types at each layer.


This layer delegates to `zenscript--parse-conditional'.

This layer reads the following expressions:

 (delegated) represents a function call to the next layer
 (recursive) represents a recursive call
 T_...       represents a token of the type T_...

  E_ASSIGN: (delegated) | T_ASSIGN | (recursive)
            v           | _        | v
            -----------------------------------
            left        | _        | right

    value: (left right)

  E_OPASSIGN: (delegated) | T_PLUSASSIGN  | (recursive)
              v           | O_PLUS        | v
              -----------------------------------------
              (delegated) | T_MINUSASSIGN | (recursive)
              v           | O_MINUS       | v
              -----------------------------------------
              (delegated) | T_TILDEASSIGN | (recursive)
              v           | O_TILDE       | v
              -----------------------------------------
              (delegated) | T_MULASSIGN   | (recursive)
              v           | O_MUL         | v
              -----------------------------------------
              (delegated) | T_DIVASSIGN   | (recursive)
              v           | O_DIV         | v
              -----------------------------------------
              (delegated) | T_MODASSIGN   | (recursive)
              v           | O_MOD         | v
              -----------------------------------------
              (delegated) | T_ORASSIGN    | (recursive)
              v           | O_OR          | v
              -----------------------------------------
              (delegated) | T_ANDASSIGN   | (recursive)
              v           | O_AND         | v
              -----------------------------------------
              (delegated) | T_XORASSIGN   | (recursive)
              v           | O_XOR         | v
              -----------------------------------------
              (delegated) | T_PLUSASSIGN  | (recursive)
              v           | O_PLUS        | v
              -----------------------------------------
              left        | op            | right

    value: (left right op)

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (let ((token (zenscript--peek-token tokens))
        left)
    (unless token
      (zenscript--throw-parse-error
       "Unexpected end of file"
       ()))
    (setq left (zenscript--parse-conditional tokens))
    (unless (zenscript--peek-token tokens)
      (zenscript--throw-parse-error
       "Unexpected end of file"
       ()))

    (or (pcase (car (zenscript--peek-token tokens))
          ('T_ASSIGN
           (zenscript--get-token tokens)
           (list 'E_ASSIGN left (zenscript--parse-expression tokens)))
          ('T_PLUSASSIGN
           (zenscript--get-token tokens)
           (list 'E_OPASSIGN 'ADD left (zenscript--parse-expression tokens)))
          ('T_MINUSASSIGN
           (zenscript--get-token tokens)
           (list 'E_OPASSIGN 'SUB left (zenscript--parse-expression tokens)))
          ('T_TILDEASSIGN
           (zenscript--get-token tokens)
           (list 'E_OPASSIGN 'CAT left (zenscript--parse-expression tokens)))
          ('T_MULASSIGN
           (zenscript--get-token tokens)
           (list 'E_OPASSIGN 'MUL left (zenscript--parse-expression tokens)))
          ('T_DIVASSIGN
           (zenscript--get-token tokens)
           (list 'E_OPASSIGN 'DIV left (zenscript--parse-expression tokens)))
          ('T_MODASSIGN
           (zenscript--get-token tokens)
           (list 'E_OPASSIGN 'MOD left (zenscript--parse-expression tokens)))
          ('T_ORASSIGN
           (zenscript--get-token tokens)
           (list 'E_OPASSIGN 'OR left (zenscript--parse-expression tokens)))
          ('T_ANDASSIGN
           (zenscript--get-token tokens)
           (list 'E_OPASSIGN 'AND left (zenscript--parse-expression tokens)))
          ('T_XORASSIGN
           (zenscript--get-token tokens)
           (list 'E_OPASSIGN 'XOR left (zenscript--parse-expression tokens)))
          (_ ()))
        left)))

(defun zenscript--parse-conditional (tokens)
  "Possibly read a conditional expression from TOKENS.

This layer delegates to `zenscript--parse-or-or'.

This layer reads the following expressions:

 (delegated) represents a function call to the next layer
 (recursive) represents a recursive call
 T_...       represents a token of the type T_...

  E_CONDITIONAL: (delegated) | T_QUEST | (delegated) | T_COLON | (recursive)
                 v           | _       | v           | v       | v
                 -----------------------------------------------------------
                 predicate   | _       | then        | _       | else

    value: (predicate then else)

  ?: (delegated)

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (let ((left (zenscript--parse-or-or tokens)))
    (let ((quest (zenscript--optional-token 'T_QUEST tokens)))
      (if quest
          (list 'E_CONDITIONAL
                left
                (zenscript--parse-or-or tokens)
                (progn (zenscript--require-token 'T_COLON tokens
                                                 ": expected")
                       (zenscript--parse-conditional tokens)))
        left))))

(defun zenscript--parse-binary (token-type expression-type tokens parse-next)
  "Convenience function for the binary expressions below.

TOKEN-TYPE is the token representing this operation.

EXPRESSION-TYPE is the type of the expression that may
be parsed.

TOKENS is the tokenstream to read from.

PARSE-NEXT is the function to delegate to."
  (let ((left (funcall parse-next tokens)))
    (while (zenscript--optional-token token-type tokens)
      (setq left
            (list expression-type left
                  (funcall parse-next tokens))))
    left))

(defun zenscript--parse-or-or (tokens)
  "Possibly read an expression using ||s from TOKENS.

Delegates to `zenscript--parse-and-and'

Reads:

  E_OR2: (recursive) | T_OR2 | (delegated)
         v           |       | v
         ---------------------------------
         left        | _     | right

    value: (left right)

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (zenscript--parse-binary 'T_OR2 'E_OR2 tokens
                           #'zenscript--parse-and-and))

(defun zenscript--parse-and-and (tokens)
  "Possibly read an expression using &&s from TOKENS.

Delegates to `zenscript--parse-or'

Reads:

  E_AND2: (recursive) | T_AND2 | (delegated)
          v           |        | v
          ---------------------------------
          left        | _      | right

    value: (left right)

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (zenscript--parse-binary 'T_AND2 'E_AND2 tokens
                           #'zenscript--parse-or))

(defun zenscript--parse-or (tokens)
  "Possibly read an expression using |s from TOKENS.

Delegates to `zenscript--parse-xor'

Reads:

  E_OR (recursive) | T_OR | (delegated)
       v           |      | v
       ---------------------------------
       left        | _    | right

    value: (left right)

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (zenscript--parse-binary 'T_OR 'E_OR tokens
                           #'zenscript--parse-xor))

(defun zenscript--parse-xor (tokens)
  "Possibly read an expression using ^s from TOKENS.

Delegates to `zenscript--parse-and'

Reads:

  E_XOR (recursive) | T_XOR | (delegated)
        v           |       | v
        ---------------------------------
        left        | _     | right

    value: (left right)

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (zenscript--parse-binary 'T_XOR 'E_XOR tokens
                           #'zenscript--parse-and))

(defun zenscript--parse-and (tokens)
  "Possibly read an expression using &s from TOKENS.

Delegates to `zenscript--parse-comparison'

Reads:

  E_AND (recursive) | T_AND | (delegated)
        v           |       | v
        ---------------------------------
        left        | _     | right

    value: (left right)

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (zenscript--parse-binary 'T_AND 'E_AND tokens
                           #'zenscript--parse-comparison))

(defun zenscript--parse-comparison (tokens)
  "Possibly read a comparison expression from TOKENS.

Delegates to `zenscript--parse-add'

Reads:

  E_COMPARE: (delegated) | T_NOTEQ | (delegated)
             v           | C_EQ    | v
             -----------------------------------
             (delegated) | T_LT    | (delegated)
             v           | C_NE    | v
             -----------------------------------
             (delegated) | T_LTEQ  | (delegated)
             v           | C_LT    | v
             -----------------------------------
             (delegated) | T_GT    | (delegated)
             v           | C_LE    | v
             -----------------------------------
             (delegated) | T_GTEQ  | (delegated)
             v           | C_GT    | v
             -----------------------------------
             (delegated) | T_EQ    | (delegated)
             v           | C_GE    | v
             -----------------------------------
             left        | op      | right

    value: (left right op)

  E_BINARY: (delegated) | T_IN       | (delegated)
            v           | O_CONTAINS | v
            --------------------------------------
            left        | op         | right

    value: (left right op)

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (let* ((left (zenscript--parse-add tokens))
         (type (pcase (car (zenscript--peek-token tokens))
                 ('T_EQ 'C_EQ)
                 ('T_NOTEQ 'C_NE)
                 ('T_LT 'C_LT)
                 ('T_LTEQ 'C_LE)
                 ('T_GT 'C_GT)
                 ('T_GTEQ 'C_GE)
                 ('T_IN
                  (setq left
                        (list 'E_BINARY left
                              (progn
                                (zenscript--get-token tokens)
                                (zenscript--parse-add tokens))
                              'O_CONTAINS))
                  ;; doesn't count as a comparison
                  ;; but it's still here for some reason.
                  ()))))
    (if type
        (list 'E_COMPARE left
              (progn
                (zenscript--get-token tokens)
                (zenscript--parse-add tokens))
              type)
      left)))

(defun zenscript--parse-add (tokens)
  "Possibly read an addition-priority expression from TOKENS.

Delegates to `zenscript--parse-mul'

Reads:

  E_BINARY: (delegated) | T_MINUS | (delegated)
            v           | O_ADD   | v
            -----------------------------------
            (delegated) | T_TILDE | (delegated)
            v           | O_SUB   | v
            -----------------------------------
            (delegated) | T_PLUS  | (delegated)
            v           | O_CAT   | v
            -----------------------------------
            left        | op      | right

    value: (left right op)

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (let ((left (zenscript--parse-mul tokens)))
    (while (progn
             (cond ((zenscript--optional-token 'T_PLUS tokens)
                    (setq left (list 'E_BINARY left
                                     (zenscript--parse-mul tokens)
                                     'O_ADD)))
                   ((zenscript--optional-token 'T_MINUS tokens)
                    (setq left (list 'E_BINARY left
                                     (zenscript--parse-mul tokens)
                                     'O_SUB)))
                   ((zenscript--optional-token 'T_TILDE tokens)
                    (setq left (list 'E_BINARY left
                                     (zenscript--parse-mul tokens)
                                     'O_CAT)))
                   (t ()))))
    left))

(defun zenscript--parse-mul (tokens)
  "Possibly read an multiplication-priority expression from TOKENS.

Delegates to `zenscript--parse-unary'

Reads:

  E_BINARY: (delegated) | T_MUL | (delegated)
            v           | O_MUL | v
            ---------------------------------
            (delegated) | T_DIV | (delegated)
            v           | O_DIV | v
            ---------------------------------
            (delegated) | T_MOD | (delegated)
            v           | O_MOD | v
            ---------------------------------
            left        | op    | right

    value: (left right op)

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (let ((left (zenscript--parse-unary tokens)))
    (while (progn
             (cond ((zenscript--optional-token 'T_MUL tokens)
                    (setq left (list 'E_BINARY left
                                     (zenscript--parse-unary tokens)
                                     'O_MUL)))
                   ((zenscript--optional-token 'T_DIV tokens)
                    (setq left (list 'E_BINARY left
                                     (zenscript--parse-unary tokens)
                                     'O_DIV)))
                   ((zenscript--optional-token 'T_MOD tokens)
                    (setq left (list 'E_BINARY left
                                     (zenscript--parse-unary tokens)
                                     'O_MOD)))
                   (t ()))))
    left))

(defun zenscript--parse-unary (tokens)
  "Possibly read a unary expression from TOKENS.

Delegates to `zenscript--parse-postfix'

Reads:

  E_UNARY: T_NOT   | (recursive)
           O_NOT   | v
           ---------------------
           T_MINUS | (recursive)
           O_MINUS | v
           ---------------------
           op      | expr

    value: (expr op)

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (pcase (car (zenscript--peek-token tokens))
    ('T_NOT (list 'E_UNARY
                  (progn
                    (zenscript--get-token tokens)
                    (zenscript--parse-unary tokens))
                  'O_NOT))
    ('T_MINUS (list 'E_UNARY
                    (progn
                      (zenscript--get-token tokens)
                      (zenscript--parse-unary tokens))
                    'O_MINUS))
    (_ (zenscript--parse-postfix tokens))))

(defmacro zenscript--++ (val)
  "Increment VAL."
  `(setq ,val (1+ ,val)))

(defmacro zenscript--+= (val by)
  "Increment VAL by BY."
  `(setq ,val (+ ,val ,by)))

(defun zenscript--unescape-string-token (token)
  "Unescape the string token TOKEN, i.e. get the value it represents.

unescape_perl_string()
<p>
Tom Christiansen <tchrist@perl.com> Sun Nov 28 12:55:24 MST 2010
<p>
It's completely ridiculous that there's no standard unescape_java_string
function.  Since I have to do the damn thing myself, I might as well make
it halfway useful by supporting things Java was too stupid to consider in
strings:
<p>
=> \"?\" items are additions to Java string escapes but normal in Java
regexes
<p>
=> \"!\" items are also additions to Java regex escapes
<p>
Standard singletons: ?\\a ?\\e \\f \\n \\r \\t
<p>
NB: \\b is unsupported as backspace so it can pass-through to the regex
translator untouched; I refuse to make anyone doublebackslash it as
doublebackslashing is a Java idiocy I desperately wish would die out.
There are plenty of other ways to write it:
<p>
\\cH, \\12, \\012, \\x08 \\x{8}, \\u0008, \\U00000008
<p>
Octal escapes: \\0 \\0N \\0NN \\N \\NN \\NNN Can range up to !\\777 not \\377
<p>
TODO: add !\\o{NNNNN} last Unicode is 4177777 maxint is 37777777777
<p>
Control chars: ?\\cX Means: ord(X) ^ ord('@')
<p>
Old hex escapes: \\xXX unbraced must be 2 xdigits
<p>
Perl hex escapes: !\\x{XXX} braced may be 1-8 xdigits NB: proper Unicode
never needs more than 6, as highest valid codepoint is 0x10FFFF, not
maxint 0xFFFFFFFF
<p>
Lame Java escape: \\[IDIOT JAVA PREPROCESSOR]uXXXX must be exactly 4
xdigits;
<p>
I can't write XXXX in this comment where it belongs because the damned
Java Preprocessor can't mind its own business. Idiots!
<p>
Lame Python escape: !\\UXXXXXXXX must be exactly 8 xdigits
<p>
TODO: Perl translation escapes: \\Q \\U \\L \\E \\[IDIOT JAVA PREPROCESSOR]u
\\l These are not so important to cover if you're passing the result to
Pattern.compile(), since it handles them for you further downstream. Hm,
what about \\[IDIOT JAVA PREPROCESSOR]u?"
  (condition-case _
      (let ((oldstr (substring (cadr token) 1 -1))
            string-builder
            saw-backslash
            (i 0))
        (while (< i (length oldstr))
          (let ((cp (aref oldstr i)))
            (if (not saw-backslash)
                (if (eq cp ?\\)
                    (setq saw-backslash t)
                  (zenscript--cons! (char-to-string cp) string-builder))
              (pcase cp
                (?\\ (zenscript--cons! "\\" string-builder))
                (?r (zenscript--cons! "\r" string-builder))
                (?n (zenscript--cons! "\n" string-builder))
                (?f (zenscript--cons! "\f" string-builder))
                (?b (zenscript--cons! "\\b" string-builder)) ; pass through
                (?t (zenscript--cons! "\t" string-builder))
                (?a (zenscript--cons! "\a" string-builder))
                (?e (zenscript--cons! "\e" string-builder))
                ((or ?\' ?\") (zenscript--cons! (char-to-string cp) string-builder))
                (?c (zenscript--++ i) (zenscript--cons! (char-to-string (logxor (aref oldstr i) 64)) string-builder))
                ((or ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
                 (unless (eq cp ?0) (zenscript--+= i -1))
                 (if (eq (1+ i) (length oldstr))
                     (zenscript--cons! "\0" string-builder)
                   (zenscript--++ i)
                   (let ((digits 0)
                         (j 0))
                     (while (< j 3)
                       (if (eq (+ i j) (length oldstr))
                           (setq j 3) ;; break
                         (let ((ch (aref oldstr (+ i j))))
                           (if (or (< ch ?0) (> ch ?7))
                               (setq j 3) ;; break
                             (zenscript--++ digits))))
                       (zenscript--++ j))
                     (let ((value (string-to-number (substring oldstr i (+ i digits)) 8)))
                       (zenscript--cons! (char-to-string value) string-builder)
                       (zenscript--+= i (1- digits))))))
                (?x (zenscript--++ i)
                    (let (saw-brace
                          (j 0))
                      (when (eq (aref oldstr i) ?\{)
                        (zenscript--++ i)
                        (setq saw-brace t))
                      (while (< j 8)
                        (if (and (not saw-brace) (eq j 2))
                            (setq j 8) ;; break
                          (let ((ch (aref oldstr (+ i j))))
                            (if (and saw-brace (eq ch ?\}))
                                (setq j 8) ;; break
                              )))
                        (zenscript--++ j))
                      (let ((value (string-to-number (substring oldstr i (+ i j)) 16)))
                        (zenscript--cons! (char-to-string value) string-builder))
                      (when saw-brace
                        (zenscript--++ j))
                      (zenscript--+= i (1- j))))
                (?u (zenscript--++ i)
                    (let ((value (string-to-number (substring oldstr i (+ i 4)) 16)))
                      (zenscript--cons! (char-to-string value) string-builder))
                    (zenscript--+= i 4))
                (?U (zenscript--++ i)
                    (let ((value (string-to-number (substring oldstr i (+ i 8)) 16)))
                      (zenscript--cons! (char-to-string value) string-builder))
                    (zenscript--+= i 8))
                (_ (zenscript--cons! "\\" string-builder)
                   (zenscript--cons! (char-to-string cp) string-builder)))
              (setq saw-backslash ())))
          (zenscript--++ i))

        (when saw-backslash
          (zenscript--cons! "\\" string-builder))

        (apply #'concat (reverse string-builder)))
    (args-out-of-range
     (zenscript--throw-parse-error
      "Error parsing string"
      token))))

(defun zenscript--parse-postfix (tokens)
  "Possibly read a postfix expression from TOKENS.

Delegates to `zenscript--parse-primary'

  (expression) represents a call to `zenscript--parse-expression'
  (zentype) represents a call to `zenscript--parse-zentype'

Reads:

  E_MEMBER: (recursive) | T_DOT | T_ID
            v           | _     | (cadr v)
            ----------------------------------------------------------
            (recursive) | T_DOT | T_VERSION
            v           | _     | (cadr v)
            ----------------------------------------------------------
            (recursive) | T_DOT | T_STRING
            v           | _     | (zenscript--unescape-string-token v)
            ----------------------------------------------------------
            base        | _     | member

    value: (base member)

  E_BINARY: (recursive) | T_DOT2 | (expression)
            v           | _      | v
            -----------------------------------
            (recursive) | T_ID*  | (expression)
            v           | _      | v
            *only if (string= (cadr v) \"to\")
            -----------------------------------
            from        | _      | to

    value: (from to 'O_RANGE)

  E_INDEX: (recursive) | T_SQBROPEN | (expression) | T_SQBRCLOSE
           v           | _          | v            | _
           -----------------------------------------------------
           base        | _          | index        | _

    value: (base index)

  E_INDEX_SET: E_INDEX | T_ASSIGN | (expression)
               _       | _        | v
               ---------------------------------
               _       | _        | val

    A T_ASSIGN following an E_INDEX becomes an E_INDEX_SET.

    value: (base index val)

  E_CALL: (recursive) | T_BROPEN | [(expression) | ... T_COMMA] | T_BRCLOSE
          v           | _        | v                            | _
          -----------------------------------------------------------------
          base        | _        | args                         | _

    value: (base args)

  E_CAST: (recursive) | T_AS | (zentype)
          v           | _    | v
          ------------------------------
          base        | _    | type

    value: (base type)

  E_INSTANCEOF: (recursive) | T_INSTANCEOF | (zentype)
                v           | _            | v
                --------------------------------------
                base        | _            | type

    value: (base type)

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (let ((base (zenscript--parse-primary tokens)))
    (while
        (and (zenscript--peek-token tokens)
             (cond
              ((zenscript--optional-token 'T_DOT tokens)
               (let ((member (or (zenscript--optional-token 'T_ID tokens)
                                 (zenscript--optional-token 'T_VERSION tokens))))
                 (setq base
                       (list 'E_MEMBER base
                             (if member
                                 (cadr member)
                               (zenscript--unescape-string-token
                                (zenscript--require-token 'T_STRINGVALUE tokens
                                                          "identifier or string expected")))))))
              ((or (zenscript--optional-token 'T_DOT2 tokens)
                   (let ((peeked (zenscript--peek-token tokens)))
                     (and (eq (car peeked) 'T_ID)
                          (string-equal "to" (cadr peeked))
                          (zenscript--get-token tokens))))
               (setq base
                     (list 'E_BINARY base
                           (zenscript--parse-expression tokens)
                           'O_RANGE))
               ())
              ((zenscript--optional-token 'T_SQBROPEN tokens)
               (let ((index (zenscript--parse-expression tokens)))
                 (zenscript--require-token 'T_SQBRCLOSE tokens
                                           "] expected")
                 (setq base
                       (if (zenscript--optional-token 'T_ASSIGN tokens)
                           (list 'E_INDEX_SET base index (zenscript--parse-expression tokens))
                         (list 'E_INDEX base index)))))
              ((zenscript--optional-token 'T_BROPEN tokens)
               (let (arguments)
                 (unless (zenscript--optional-token 'T_BRCLOSE tokens)
                   (zenscript--cons! (zenscript--parse-expression tokens)
                                     arguments)
                   (while (zenscript--optional-token 'T_COMMA tokens)
                     (zenscript--cons! (zenscript--parse-expression tokens)
                                       arguments))
                   (zenscript--require-token 'T_BRCLOSE tokens
                                             ") expected"))
                 (setq base (list 'E_CALL base (reverse arguments)))))
              ((zenscript--optional-token 'T_AS tokens)
               (setq base (list 'E_CAST base (zenscript--parse-zentype tokens))))
              ((zenscript--optional-token 'T_INSTANCEOF tokens)
               (setq base (list 'E_INSTANCEOF base (zenscript--parse-zentype tokens))))
              (t ()))))
    base))

(defun zenscript--decode-long (string)
  "Convert STRING to a number as java.lang.Long#decode would."
  (if (string= string "")
      0
    (let ((radix 10)
          (index 0)
          negative)
      (cond ((eq (aref string 0)
                 ?\-)
             (setq negative t)
             (zenscript--++ index))
            ((eq (aref string 0)
                 ?\+)
             (zenscript--++ index)))
      (cond ((eq t (compare-strings string
                                    index (+ index 2)
                                    "0x"
                                    0 2
                                    t))
             (setq radix 16)
             (setq index (+ index 2)))
            ((eq t (compare-strings string
                                    index (+ index 1)
                                    "#"
                                    0 1))
             (setq radix 16)
             (zenscript--++ index))
            ((eq t (compare-strings string
                                    index (+ index 1)
                                    "0"
                                    0 1))
             (setq radix 8)
             (zenscript--++ index)))
      (let ((result (string-to-number (substring string index) radix)))
        (if negative (- result) result)))))

(defun zenscript--parse-primary (tokens)
  "Read a primary expression from TOKENS.

This is the last layer and does not delegate.

Reads:

  E_VALUE: _        | T_INTVALUE
           E_INT    | (zenscript--decode-long (cadr v))
           ---------|-------------------------------------
           _        | T_FLOATVALUE
           E_FLOAT  | (string-to-number (cadr v))
           ---------|-------------------------------------
           _        | T_STRINGVALUE
           E_STRING | (zenscript--unescape-string-token v)
           ---------|-------------------------------------
           type     | val

    value: (type val double-length?)

    double-length:

      If type is E_STRING, this is not present.

      If type is E_INT or E_FLOAT, this it t if a double-length
      Java primitive is represented here (long, double), and nil
      otherwise.

  E_VARIABLE: T_ID
              (cadr v)
              --------
              name

    value: (name)

  E_FUNCTION:

   T_FUNCTION | T_BROPEN | [T_ID | [T_AS (zentype)]? | ... T_COMMA]
   | T_BRCLOSE | [T_AS (zentype)]? | T_AOPEN | ... | T_ACLOSE

    An anonymous function.

    value: (arguments return-type statements start end)

      arguments:

        A list of arguments, as returned by
        `zenscript--parse-function-arguments'.

        name:

          The name of the argument, a string.

        type:

          The ZenType of the argument.

        pos:

          The pointer position the identifier token appeared at.

        start:

          The position after the opening {

        end:

          The position before the closing }

      return-type:

        The ZenType that this function returns.

      statements:

        A list of statements that are the function body.

  E_BRACKET: T_LT | ... | T_GT

    value: (tokens)

      tokens:

        A list of tokens that make up the bracket.

  E_LIST: T_SQBROPEN | [(expression) | ... T_COMMA] | T_SQBRCLOSE

    value: (elements)

      elements:

        A list of expressions that make up this list literal.

  E_MAP:

   T_AOPEN | [(expression) | T_COLON | (expression) | ... T_COMMA] | T_ACLOSE

    value: (keys values)

      keys:

        A list of expressions that are the keys of the map.

      values:

        A list of expressions that are the values of the map.

      The length of these two lists are the same, with each index of KEYS
      corresponding to the entry at the same index in VALUES.

  E_BOOL: T_TRUE
          t
          -------
          T_FALSE
          ()
          -------
          val

    value: (val)

      val:

        t if the boolean is TRUE, nil otherwise.

  E_NULL: T_NULL
          _
          ------
          _

    value: ()

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (pcase (car (zenscript--peek-token tokens))
    ('T_INTVALUE
     (let ((l (zenscript--decode-long (cadr (zenscript--get-token tokens)))))
       (list 'E_VALUE (list 'E_INT l (or (> l (- (expt 2 31) 1))
                                         (< l (- (expt 2 31))))))))
    ('T_FLOATVALUE
     (let* ((value (cadr (zenscript--get-token tokens)))
            (d (string-to-number value))
            (lastchar (aref value (- (length value) 1))))
       (list 'E_VALUE (list 'E_FLOAT d (not (or (eq lastchar ?\f)
                                                (eq lastchar ?\F)))))))
    ('T_STRINGVALUE
     (list 'E_VALUE (list 'E_STRING (zenscript--unescape-string-token
                                     (zenscript--get-token tokens)))))
    ('T_ID
     (list 'E_VARIABLE (cadr (zenscript--get-token tokens))))
    ('T_FUNCTION
     (zenscript--get-token tokens)
     (let ((arguments (zenscript--parse-function-arguments tokens))
           (return-type (if (zenscript--optional-token 'T_AS tokens)
                            (zenscript--parse-zentype tokens)
                          '(C_RAW "any")))
           (start (1+ (nth 2 (zenscript--require-token 'T_AOPEN tokens
                                                       "{ expected"))))
           statements end)
       (while (and (zenscript--has-next-token tokens)
                   (not (let ((close (zenscript--optional-token 'T_ACLOSE tokens)))
                          (and close (setq end (nth 2 close))))))
         (zenscript--cons! (zenscript--parse-statement tokens) statements))
       (list 'E_FUNCTION
             arguments
             return-type
             (reverse statements)
             start
             end)))
    ('T_LT
     (zenscript--get-token tokens)
     (let (btokens nexttoken)
       (while (and (zenscript--has-next-token tokens)
                   (not (zenscript--optional-token 'T_GT tokens))
                   (setq nexttoken (zenscript--get-token tokens)))
         (zenscript--cons! nexttoken btokens))
       (list 'E_BRACKET (reverse btokens))))
    ('T_SQBROPEN
     (zenscript--get-token tokens)
     (let (contents)
       (unless (zenscript--optional-token 'T_SQBRCLOSE tokens)
         (let (break)
           (while (and (not break)
                       (not (zenscript--optional-token 'T_SQBRCLOSE tokens)))
             (zenscript--cons! (zenscript--parse-expression tokens) contents)
             (unless (zenscript--optional-token 'T_COMMA tokens)
               (zenscript--require-token 'T_SQBRCLOSE tokens
                                         "] or , expected")
               (setq break t)))))
       (list 'E_LIST (reverse contents))))
    ('T_AOPEN
     (zenscript--get-token tokens)
     (let (keys values)
       (unless (zenscript--optional-token 'T_ACLOSE tokens)
         (let (break)
           (while (and (not break)
                       (not (zenscript--optional-token 'T_ACLOSE tokens)))
             (zenscript--cons! (zenscript--parse-expression tokens) keys)
             (zenscript--require-token 'T_COLON tokens
                                       ": expected")
             (zenscript--cons! (zenscript--parse-expression tokens) keys)
             (unless (zenscript--optional-token 'T_COMMA tokens)
               (zenscript--require-token 'T_ACLOSE tokens
                                         "} or , expected")
               (setq break t)))))
       (list 'E_MAP (reverse keys) (reverse values))))
    ('T_TRUE (zenscript--get-token tokens) (list 'E_BOOL t))
    ('T_FALSE (zenscript--get-token tokens) (list 'E_BOOL ()))
    ('T_NULL (zenscript--get-token tokens) (list 'E_NULL))
    ('T_BROPEN
     (zenscript--get-token tokens)
     (prog1 (zenscript--parse-expression tokens)
       (zenscript--require-token 'T_BRCLOSE tokens
                                 ") expected")))
    (_ (zenscript--throw-parse-error
        "Invalid expression"
        (zenscript--get-token tokens)))))

(defun zenscript--parse-global (tokens)
  "Parse the next global definition from TOKENS.

Return a list of the form:

 (name type value)

name:

  The token that is the name of this binding.

type:

  The ZenType of this binding.

value:

  The expression that is the initializer of this binding.

TOKENS must be a tokenstream from `zenscript--make-tokenstream'."
  (zenscript--get-token tokens)
  (let ((name (zenscript--require-token 'T_ID tokens
                                        "Global value requires a name!"))
        (type (if (zenscript--optional-token 'T_AS tokens)
                  (zenscript--parse-zentype tokens)
                '(C_RAW "any")))
        (value (progn (zenscript--require-token 'T_ASSIGN tokens
                                                "Global values have to be initialized!")
                      (zenscript--parse-expression tokens))))
    (zenscript--require-token 'T_SEMICOLON tokens
                              "; expected")
    (list name type value)))

(provide 'zenscript-parser)
;;; zenscript-parser.el ends here
