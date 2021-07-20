;;; zephir-mode.el --- Major mode for editing Zephir code -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.6.0
;; URL: https://github.com/zephir-lang/zephir-mode
;; Keywords: languages
;; Package-Requires: ((cl-lib "0.5") (pkg-info "0.4") (emacs "25.1"))
;; Revision: $Format:%h (%cD %d)$

;; This file is NOT part of GNU Emacs.

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;   GNU Emacs major mode for editing Zephir code.  Provides syntax
;;   highlighting, indentation, movement, Imenu and navigation support.
;;
;;   Zephir -- is a high level language that eases the creation and
;; maintainability of extensions for PHP.  Zephir extensions are
;; exported to C code that can be compiled and optimized by major C
;; compilers such as gcc/clang/vc++.  Functionality is exposed to the
;; PHP language.  For more information see URL `https://zephir-lang.com'.

;;;; Subword Mode:

;;   GNU Emacs comes with `subword-mode', a minor mode that allows you to
;; navigate the parts of a “camelCase” as if they were separate words.  For
;; example, Zephir Mode treats the variable “fooBarBaz” as a whole name by
;; default.  But if you enable `subword-mode' then Emacs will treat the variable
;; name as three separate words, and therefore word-related commands
;; (e.g. “M-f”, “M-b”, “M-d”, etc.) will only affect the “camelCase” part of the
;; name under the cursor.
;;
;;   If you want to always use `subword-mode' for Zephir files then you can add
;; this to your Emacs configuration:
;;
;;    (add-hook 'zephir-mode-hook
;;      #(lambda () (subword-mode 1)))

;;;; Imenu:

;;   There is a support to jump to namespaces, classes, functions, properties
;; and constant declarations.

;;;; Movement:

;;   Move to the beginning or end of the current block with `beginning-of-defun'
;; (“C-M-a”) and `end-of-defun' (“C-M-e”) respectively.

;;;; Indentation:

;;   Automatic indentation with indentation cycling is provided, it allows you
;; to navigate different available levels of indentation by hitting “TAB”
;; several times.  There are two options to use auto-indentation when inserting
;; newlines:
;;
;; 1) Enable the minor-mode `electric-indent-mode' (enabled by default) and use
;;    “RET”.  If this mode is disabled use `newline-and-indent', bound to “C-j”.
;;
;; 2) Add the following hook in your init file:
;;
;;    (add-hook 'zephir-mode-hook
;;      #'(lambda ()
;;          (define-key zephir-mode-map "\C-m" 'newline-and-indent)))
;;
;;   The first option is prefered since you'll get the same behavior for all
;; modes out-of-the-box.
;;
;;   `zephir-indent-tabs-mode' can be changed to insert tabs for indentation in
;; Zephir Mode.  `zephir-indent-level' can be used to contol indentation level
;; of Zephir statements.

;;;; Syntax checking:

;;   Presently flymake/flycheck support is NOT provided.

;;;; Support:

;;   Bug tracking is currently handled using the GitHub issue tracker at
;; `https://github.com/sergeyklay/zephir-mode/issues'.  Feel free to ask
;; question or make suggestions in our issue tracker.

;;;; History:

;;   History is tracked in the Git repository rather than in this file.  See URL
;; `https://github.com/zephir-lang/zephir-mode/blob/master/NEWS'.

;;;; Customize && Help:

;;   See “M-x apropos-command RET ^zephir- RET” for a list of all commands and
;; “M-x customize-group RET zephir RET” for a list of customizable variables.

;;; Code:


;;;; Requirements

(require 'subr-x)
(require 'zephir-face)

;; Tell the byte compiler about autoloaded functions from packages
(declare-function pkg-info-version-info "pkg-info" (package))

;; Pacify the byte compiler
(eval-when-compile
  (require 'rx)

  ;; 25.x compat
  (unless (fboundp 'prog-first-column)
    (defun prog-first-column () 0)))

(require 'font-lock)
(require 'imenu)
(require 'pkg-info)

(eval-when-compile
  (require 'regexp-opt))


;;;; Customization

;;;###autoload
(defgroup zephir nil
  "Major mode for editing Zephir code."
  :tag "Zephir"
  :prefix "zephir-"
  :group 'languages
  :link '(url-link :tag "GitHub Page" "https://github.com/zephir-lang/zephir-mode")
  :link '(url-link :tag "Zephir Site" "https://zephir-lang.com")
  :link '(emacs-commentary-link :tag "Commentary" "zephir-mode"))

(defcustom zephir-mode-hook nil
  "List of functions to call when entering Zephir Mode."
  :tag "Hook"
  :type 'hook
  :group 'zephir)

(defcustom zephir-indent-tabs-mode nil
  "Indentation can insert tabs in Zephir Mode if this is non-nil."
  :type 'boolean
  :group 'zephir
  :safe 'booleanp)

(defcustom zephir-indent-level 4
  "Indentation of Zephir statements."
  :type 'integer
  :group 'zephir
  :safe 'integerp)


;;;; Version information

(defun zephir-mode-version (&optional show-version)
  "Display string describing the version of Zephir Mode.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (pkg-info-version-info 'zephir-mode)))
    (when show-version
      (message "Zephir Mode version: %s" version))
    version))


;;;; Utilities

(defun zephir-syntax-context (&optional pos)
  "Determine the syntax context at POS, defaulting to point.
Return nil, if there is no special context at POS, or one of
`comment'
     POS is inside a comment
`single-quoted'
     POS is inside a single-quoted string
`double-quoted'
     POS is inside a double-quoted string"
  (let ((state (save-excursion (syntax-ppss pos))))
    (if (nth 4 state)
        'comment
      (pcase (nth 3 state)
        (`?\' 'single-quoted)
        (`?\" 'double-quoted)))))

(defun zephir-in-string-or-comment-p (&optional pos)
  "Determine whether POS is inside a string or a comment."
  (not (null (zephir-syntax-context pos))))

(defun zephir-in-string-p (&optional pos)
  "Determine whether POS is inside a string.
This function determines single-quoted as well as double-quoted strings."
  (let ((ctx (zephir-syntax-context pos)))
    (or (eq ctx 'single-quoted)
        (eq ctx 'double-quoted))))

(defun zephir-in-comment-p (&optional pos)
  "Determine whether POS is inside a comment."
  (and (zephir-in-string-or-comment-p pos)
       (not (zephir-in-string-p pos))))

(defun zephir-comment-start-pos (ctx)
  "Return the position of comment containing current point.
If point is not inside a comment, return nil.  Uses CTX as a syntax context."
  (and ctx (nth 4 ctx) (nth 8 ctx)))

(defun zephir-in-ipg (re-open)
  "Return the position of RE-OPEN when `point' is inside an “IPG”.

This function is intended to use when `point' is inside a
parenthetical group (IPG) eg. in an array, argument list,
etc.  Return nil, if point is not in an IPG."
  (save-excursion
    (let ((opoint (nth 1 (syntax-ppss))))
      (when (and opoint
                 (progn (goto-char opoint) (looking-at-p re-open)))
        opoint))))

(defun zephir-in-param-list-p ()
  "Determine whether `point' is in a function parameter list."
  (ignore-errors
    (save-excursion
      (when-let ((open-paren-pt (zephir-in-ipg "(")))
        open-paren-pt
        (goto-char open-paren-pt)
        (forward-symbol -1)
        (or (looking-at-p "\\_<f\\(?:unctio\\)?n\\_>" )
            (progn (forward-symbol -1)
                   (looking-at-p "\\_<f\\(?:unctio\\)?n\\_>")))))))

(defconst zephir--language-keywords
  '("array"
    "as"
    "break"
    "try"
    "catch"
    "throw"
    "clone"
    "reverse"
    "empty"
    "fetch"
    "let"
    "if"
    "else"
    "elseif"
    "while"
    "break"
    "continue"
    "typeof"
    "instanceof"
    "loop"
    "for"
    "in"
    "do"
    "switch"
    "case"
    "default"
    "eval"
    "isset"
    "likely"
    "unlikely"
    "static"
    "unset"
    "new"
    "fn"
    "function"
    "use"
    "implements"
    "extends"
    "namespace"
    "return"
    "class"
    "interface"
    "echo"
    "final"
    "abstract")
  "Zephir keywords not accounted for by any other `zephir-rx' constituents.")


;;;; Specialized rx

(defconst zephir-possible-visiblities
  '("public" "protected" "private" "internal" "inline" "scoped")
  "Possible values for visibility declaration in Zephir code.")

(eval-when-compile
  (defconst zephir-rx-constituents
    `(
      ;; Identifier
      (identifier . ,(rx (? ?$)
                         word-start
                         (any "A-Z" "a-z" ?_)
                         (0+ (any "A-Z" "a-z" "0-9" ?_))
                         word-end))

      ;; Magic constants
      (magic-const . ,(rx symbol-start
                          (or "__LINE__" "__FILE__" "__FUNCTION__"
                              "__CLASS__" "__METHOD__" "__NAMESPACE__")
                          symbol-end))

      ;; Predefined language constants
      (builtin-const . ,(rx symbol-start
                            (or "null" "true" "false")
                            symbol-end))

      ;; User-defined constants
      (constant . ,(rx symbol-start
                       (any "A-Z" ?_)
                       (+ (any "A-Z" "0-9" ?_))
                       symbol-end))

      ;; Function declaraion
      (fn-decl . ,(rx symbol-start (or "fn" "function") symbol-end))

      ;; Namespace, class or interface name
      (classlike . ,(rx symbol-start
                        (optional ?$)
                        (any "A-Z" "a-z" ?_)
                        (0+ (any "A-Z" "a-z" "0-9" ?_))
                        (0+
                         (and "\\"
                              (any "A-Z" "a-z" ?_)
                              (+ (any "A-Z" "a-z" "0-9" ?_))))
                        symbol-end))

      ;; Data types
      (data-type . ,(rx (or (and (? "u") "int")
                            (and "bool" (? "ean"))
                            (and (? "u") "long")
                            (and (? "u") "char")
                            (and (? "i") "string")
                            (and (or "dou" "calla") "ble")
                            "float"
                            "resource"
                            "object"
                            "var"
                            "void"
                            "array"))))
    "Additional special sexps for `zephir-rx'.")

  (defmacro zephir-rx (&rest sexps)
    "Zephir-specific replacement for `rx'.

In addition to the standard forms of `rx', the following forms
are available:

`identifier'
     Any valid identifier with optional dollar sign, e.g. function name,
     variable name, etc.

`magic-const'
     Magical keyword that is expanded at compile time.

`builtin-const'
     Predefined language constants.

`constant'
     User-defined constants.
     By convention, constant identifiers are always uppercase.

`fn-decl'
     A function declaraion.

`classlike'
     A valid namespace, class or interface name without leading ‘\\’.

`data-type'
     Any valid data type.

See `rx' documentation for more information about REGEXPS param."
    (let ((rx-constituents (append zephir-rx-constituents rx-constituents)))
      (rx-to-string (cond ((null sexps) (error "No regexp is provided"))
                          ((cdr sexps)  `(and ,@sexps))
                          (t            (car sexps)))
                    t))))

(defun zephir-create-regexp-for-classlike (&optional type)
  "Make a regular expression for a ‘classlike’ with the given TYPE.

Optional TYPE must be a string that specifies the type of a
object, such as ‘interface’ or ‘namespace’.

The regular expression this function returns will check for other
keywords that can appear in ‘classlike’ signatures,
e.g. ‘abstract’ or ‘final’.  The regular expression will have two
capture groups which will be TYPE and the name of an object
respectively."
  (let ((type (or type "class"))
        (line-start "")
        (modifier "")
        (root-ns ""))
    (cond
     ((string= type "class")
      (setq line-start "^"
            ;; Class modifier, e.g. if “abstract” or “final”
            modifier "\\(?:\\(?:abstract\\|final\\)?\\s-+\\)?"))
     ((string= type "interface")
      (setq line-start "^"))
     ((string= type "namespace")
      (setq line-start "^"))
     ((string= type "extends")
      (setq root-ns "\\\\?")))

    ;; Concatenate regexp parts
    (concat
     line-start
     "\\s-*"
     modifier

     ;; Object type, group #1
     "\\(" type "\\)"

     "\\s-+"

     ;; Object name, group #2.
     "\\(" root-ns (zephir-rx classlike) "\\)")))

(defun zephir-create-regexp-for-constant ()
  "Make a regular expression for a constant definition.

The regular expression will have two capture groups which will be
the word ‘const’ and the name of a constant respectively."
  (zephir-rx word-start (group "const")
             (+ (syntax whitespace)) (? "&")
             (group identifier)))

(defun zephir-create-regexp-for-function (&optional visibility)
  "Make a regular expression for a function with the given VISIBILITY.

Optional VISIBILITY, when passed, must be a string that specifies
the visibility for a Zephir function, e.g. ‘scoped’ or ‘public’.
The parameter VISIBILITY can itself also be a regular expression.

The regular expression this function returns will check for other
keywords that can appear in method signatures, e.g. ‘final’ or
‘deprecated’.  The regular expression will have two capture
groups which will be the word ‘function’ (or ‘fn’) and the name
of a function respectively."
  (let ((visibility (or visibility zephir-possible-visiblities)))
    (when (stringp visibility)
      (setq visibility (list visibility)))
    (rx-to-string `(: line-start
                      (* (syntax whitespace))
                      (? "deprecated" (+ (syntax whitespace)))
                      (* (or "abstract" "final")
                         (+ (syntax whitespace)))
                      (or ,@visibility)
                      (+ (syntax whitespace))
                      (? "static" (+ (syntax whitespace)))
                      (group (or "fn" "function"))
                      (+ (syntax whitespace))
                      (group symbol-start
                             (? ?$)
                             (any "A-Z" "a-z" ?_)
                             (0+ (any "A-Z" "a-z" "0-9" ?_))
                             symbol-end)
                      (* (syntax whitespace))
		      "("))))


;;;; Navigation

(defun zephir-beginning-of-defun (&optional arg)
  "Move the beginning of the ARGth Zephir function from point.
Implements Zephir version of `beginning-of-defun-function'."
  (interactive "p")
  (let ((arg (or arg 1))
        (case-fold-search t))
    (while (> arg 0)
      (re-search-backward (zephir-create-regexp-for-function) nil 'noerror)
      (back-to-indentation)
      (setq arg (1- arg)))
    (while (< arg 0)
      (end-of-line 1)
      (let ((opoint (point)))
        (beginning-of-defun 1)
        (forward-list 2)
        (forward-line 1)
        (when (eq opoint (point))
          (re-search-forward (zephir-create-regexp-for-function) nil 'noerror)
          (back-to-indentation))
        (setq arg (1+ arg))))))

(defun zephir-end-of-defun (&optional arg)
  "Move the end of the ARG'th Zephir function from point.
Implements Zephir version of `end-of-defun-function'.  For more
see `zephir-beginning-of-defun'."
  (interactive "p")
  (zephir-beginning-of-defun (- (or arg 1))))


;;;; Indentation code

(defun zephir-indent-block (block-start)
  "Return the proper indentation for the block.
BLOCK-START must contain open bracket position of the block."
  (save-excursion
    (let ((offset 0))
      (unless (looking-at-p "}")
        (setq offset zephir-indent-level))
      (when (and block-start (progn (goto-char block-start) (looking-at-p "{")))
        (+ (current-indentation) offset)))))

(defun zephir-indent-listlike (pt-start re-close)
  "Return the proper indentation for the ‘listlike’.

PT-START must contain open bracket position of the ‘listlike’.
RE-CLOSE must contain the regular expression that uniquely
identifies the close bracket of the ‘listlike’."
  ;; The diagram below explains the purpose of the variables:
  ;;
  ;;    `pt-start'
  ;;            |
  ;;            |
  ;;  let map = [
  ;;      "none" : \Redis:SERIALIZER_NONE,
  ;;      "php"  : \Redis:SERIALIZER_PHP
  ;;          #];------------------------ `re-close'
  ;;          |
  ;;          |________ Suppose `point' is here (#)
  ;;
  (save-excursion
    (if (looking-at-p re-close)
        ;; Closing bracket on a line by itself
        (progn
          (goto-char pt-start)
          ;; The code below will check for the following list styles:
          ;;
          ;; // array
          ;; let attributes = [ "type" : "text/css",
          ;;                    "href" : "main.css",
          ;;                    "rel"  : "stylesheet" ];
          ;;
          ;; // arguments list
          ;; create_instance_params( definition,
          ;;                         options );
          (if (save-excursion (forward-char) (eolp))
              (current-indentation)
            (current-column)))
      ;; Otherwise, use normal indentation if the `point' is at the
      ;; end of the line:
      ;;
      ;; // array
      ;; let logger = [
      ;;     Logger::ALERT    : LOG_ALERT,
      ;;     Logger::CRITICAL : LOG_CRIT,
      ;;     [ 1 ],
      ;;     [
      ;;         foo,
      ;;         bar
      ;;     ]
      ;; ];
      ;;
      ;; // argument list
      ;; this->interpolate(
      ;;     item->getMessage(),
      ;;     item->getContext()
      ;; );
      (goto-char pt-start)
      (forward-char 1)
      (if (eolp)
          (+ (current-indentation) zephir-indent-level)
        ;; Othewise, align as described above
        (re-search-forward "\\S-")
        (forward-char -1)
        (current-column)))))

(defun zephir--proper-indentation (ctx)
  "Return the proper indentation for the current line.
This uses CTX as a current parse state."
  (save-excursion
    ;; Move `point' to the first non-whitespace character in the current line
    (back-to-indentation)

    (cond
     ;; Inside multiline string.
     ;; If we're inside a string and strings aren't to be indented,
     ;; return current indentation.
     ((zephir-in-string-p) (current-indentation))

     ;; Multiline commentary
     ((zephir-in-comment-p)
      ;; Make sure comment line is indentet proper way relative to
      ;; open-comment (“/*”) for all possible use-cases.
      ;;
      ;;   /**
      ;;    * This is the summary for a DocBlock.
      ;;    *
      ;;    * This is the description for a DocBlock.
      ;;    */
      ;;
      ;;   /*
      ;;    * C-style comments
      ;;    * can contain multiple lines.
      ;;    */
      ;;
      ;;   /*
      ;;     C-style comments
      ;;     can contain multiple lines.
      ;;    */
      (let ((asteriks-marker-p (looking-at-p "\\*+")))
        (save-excursion
          (goto-char (zephir-comment-start-pos ctx))
          (+ (current-indentation)
             (if asteriks-marker-p 0 1)
             1))))

     ;; TODO(serghei): Cover use-case for single-line comments (“//”)

     ;; When `point' is inside an innermost parenthetical grouping
     ((let ((array-start (zephir-in-ipg "\\["))
            (arglist-start (zephir-in-ipg "("))
            (block-start (zephir-in-ipg "{")))
        (cond
         (array-start (zephir-indent-listlike array-start "]"))
         (arglist-start (zephir-indent-listlike arglist-start ")"))
         (block-start (zephir-indent-block block-start)))))

     ;; Otherwise indent to the first column
     (t (prog-first-column)))))

(defun zephir-indent-line ()
  "Indent current line as Zephir code."
  (interactive)
  (if (bobp)
      ;; First line is always non-indented
      (indent-line-to 0)
    (let* (
           ;; Save the current parse state.
           ;; We will need it in `zephir--proper-indentation'.
           (ctx (save-excursion (syntax-ppss (point-at-bol))))

           ;; The first non-whitespace character (l)
           ;; |          +-------------------------- The offset (-)
           ;; |          |
           ;; |          |              +------------- Whitespace characters (.)
           ;; |_________________________|______________
           ;; let foo = bar;...........................#
           ;;                                          |
           ;;                                          |
           ;; The current point position (#) ----------+
	   ;;
	   (offset (- (point) (save-excursion (back-to-indentation) (point)))))
      (indent-line-to (zephir--proper-indentation ctx))
      ;; Move point to the previous offset
      (when (> offset 0) (forward-char offset)))))


;;;; Font Locking

(defun zephir-font-lock-syntactic-face (state)
  "Specify font lock faces based on syntax table entries.
Uses STATE as a syntax context."
  (if (nth 3 state)
      font-lock-string-face
    (if (save-excursion
          (goto-char (zephir-comment-start-pos state))
          (looking-at "/\\*\\*"))
        font-lock-doc-face
      font-lock-comment-face)))

(defvar zephir-font-lock-keywords
  `(;; Class declaration specification keywords.
    ;;
    ;; Highlight occurrences of ‘extends Foo\Bar’.
    (,(zephir-create-regexp-for-classlike "extends")
     (1 'zephir-class-declaration-spec-face)
     (2 font-lock-type-face))

    ;; Highlight occurrences of ‘implements Foo, Bar’.
    ;;
    ;; The first regexp is the anchor of the fontification, meaning the
    ;; "starting point" of the region:
    ;;
    ;;                  ------------ Starting point
    ;;                  |
    ;; class Terminator implements \Robot, \Machine
    ;; {
    ;; }
    ;;
    ("\\_<\\(implements\\)\\_>\\s-+"
     ;; Fontify the `implements' as a `zephir-class-declaration-spec-face'.
     (1 'zephir-class-declaration-spec-face)
     ;; Look for symbols after the space (‘\\s-+’), they are classes.
     ("\\(\\(?:\\sw\\|\\s_\\|\\\\\\)+\\)\\_>"
      ;; Set the limit of search to the current `implements' form only.
      (save-excursion
        (re-search-forward "{\\|;\\|extends" nil 'noerror)
        (forward-char -1)
        (point))
      ;; When we found all the classes in the region (`implements' form)
      ;; go back to the ‘\\s-+’ marker.
      (progn (re-search-backward "\\_<\\(implements\\)\\_>\\s-+")
             (forward-symbol 1))
      ;; Fontify each matched symbol as class.
      (1 font-lock-type-face)))

    ;; Highlight occurrences of namespace declarations (‘namespace Foo’)
    (,(zephir-create-regexp-for-classlike "namespace")
     (1 'zephir-namespace-declaration-face)
     (2 font-lock-type-face))

    ;; Highlight occurrences of import statements (‘use Foo’)
    (,(rx-to-string `(: (* (syntax whitespace))
                        symbol-start (group "use") symbol-end
                        (+ (syntax whitespace))
                        (group (+ (or (syntax word)
                                      (syntax symbol)
                                      ?\\)))))
     (1 'zephir-import-declaration-face)
     (2 font-lock-type-face))

    ;; Class declaration keywords ‘class Foo’, ‘interface Foo’
    (,(zephir-create-regexp-for-classlike)
     (1 'zephir-class-declaration-face)
     (2 font-lock-type-face))
    (,(zephir-create-regexp-for-classlike "interface")
     (1 'zephir-class-declaration-face)
     (2 font-lock-type-face))

    ;; Highlight occurrences of class modifiers (‘abstract’, ‘final’)
    (,(zephir-rx symbol-start (group (or "abstract" "final")) symbol-end
                 (+ (syntax whitespace))
                 symbol-start "class" symbol-end)
     1 'zephir-class-modifier-face)

    ;; Highlight occurrences of method modifiers (‘abstract’, ‘final’)
    (,(rx-to-string `(: symbol-start (group (or "abstract" "final")) symbol-end
                        (+ (syntax whitespace))
                        (+ (or ,@(append
                                  zephir-possible-visiblities
                                  '("static")))
                           (+ (syntax whitespace)))
                        symbol-start "function" symbol-end))
     1 'zephir-method-modifier-face)

    ;; Fontify methods call like ‘object->method()’
    (,(zephir-rx (group "->") (group identifier)
                 (* (syntax whitespace))"(")
     (1 'zephir-object-operator-face)
     (2 'zephir-method-call-face))

    ;; Highlight definition of user defined constants
    (,(zephir-create-regexp-for-constant)
     (1 'zephir-keyword-face)
     (2 'zephir-constant-assign-face))

    ;; Highlight occurrences of magic constants
    (,(zephir-rx (group magic-const))
     1 'zephir-magical-constant-face)

    ;; Highlight occurrences of built-in constants
    (,(zephir-rx (group (or constant builtin-const)))
     1 'zephir-constant-face)

    ;; Highlight occurrences of the word ‘this’
    (,(zephir-rx word-start (group "this") word-end)
     1 'zephir-this-face)

    ;; Highlight properties like ‘object->property’
    (,(zephir-rx (group "->") (group identifier)
                 (* (syntax whitespace)))
     (1 'zephir-object-operator-face)
     (2 'zephir-property-name-face))

    ;; Highlight function/method name i.e. ‘function foo ()’
    (,(zephir-rx word-start (group fn-decl)
                 (+ (syntax whitespace))
                 (group identifier)
                 (* (syntax whitespace)) "(")
     (1 'zephir-keyword-face)
     (2 'zephir-function-name-face))

    ;; Type hints i.e. ‘int a’
    (,(zephir-rx (? "const" (+ (syntax whitespace)))
                 word-boundary (group data-type) (? ?!)
                 (+ (syntax whitespace)) (? ?&)
                 (group identifier))
     (1 'zephir-type-face)
     (2 'zephir-variable-name-face))

    ;; Type hints i.e. ‘<AdapterFactory> factory’
    (,(zephir-rx (? "const" (+ (syntax whitespace)))
                 "<" (group (+ (or (syntax word) (syntax symbol) "\\"))) ">"
                 (+ (syntax whitespace)) (? ?&)
                 (group identifier))
     (1 'zephir-type-face)
     (2 'zephir-variable-name-face))

    ;; Continued formal parameter list i.e. ‘function foo (a, b, c, d, e)’
    (,(zephir-rx (* (syntax whitespace)) (? ?&) identifier
                 (* (syntax whitespace)) (in "," ")" "="))
     (,(zephir-rx identifier)
      (if (save-excursion (backward-char)
                          (zephir-in-param-list-p))
          (forward-symbol -1)
        (end-of-line))
      (end-of-line)
      (0 'zephir-variable-name-face)))

    ;; Return type hints
    (,(zephir-rx (or (:")" (* (syntax whitespace)) "->") "|")
                 (* (syntax whitespace)) (? ?<)
                 (group (+ (or (syntax word) (syntax symbol) "\\")))
                 (? ?>) (* (syntax whitespace)))
     1 'zephir-type-face)

    ;; ‘... as Foo’
    (,(zephir-create-regexp-for-classlike "as")
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))

    ;; Builtin declarations and reserverd keywords
    (,(regexp-opt (append zephir--language-keywords
                          zephir-possible-visiblities)
                  'symbols)
     1 'zephir-keyword-face)

    ;; Function names, i.e. ‘function foo’
    ;; TODO(serghei): deprecated <visibility> function <name>
    ;; TODO(serghei): <visibility> static function <name>
    ;; TODO(serghei): deprecated function <name>
    ;; TODO(serghei): let foo = function () {}
    (,(zephir-create-regexp-for-function)
     (1 'zephir-keyword-face)
     (2 'zephir-function-name-face))

    ;; Highlight occurrences of class' properties (‘public foo’).
    ;;
    ;; The first regexp is the anchor of the fontification, meaning the
    ;; "starting point" of the region:
    ;;
    ;; ------------------------- Starting point
    ;; |
    ;; public static myVar = [] { get, set, toString };
    ;;
    ("\\_<\\(p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)\\)\\_>\\s-+"
     ;; Fontify the property visibility as a `zephir-keyword-face'.
     (1 'zephir-keyword-face)
     ;; Look for symbols after the space (‘\\s-+’), they are properties.
     ("\\(\\$?\\<[A-Z_a-z][0-9A-Z_a-z]*\\>\\)"
      ;; Set the limit of search to the current property form only.
      (save-excursion
        (re-search-forward "\\(?:[;={]\\)" nil 'noerror)
        (forward-char -1)
        (point))
      ;; When we found a property in the region go back to the ‘\\s-+’ marker.
      (progn
        (re-search-backward
         "\\_<\\(p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)\\)\\_>\\s-+")
        (forward-symbol 1))
      ;; Fontify each matched symbol as property.
      (1 'zephir-property-name-face))
     ;; When done with the symbols look for the ‘static’ word.
     ("static"
      ;; We are starting from the visibility word again.
      (save-excursion
        (re-search-forward "\\s-" nil 'noerror)
        (forward-char -1)
        (point))
      ;; When we found a property in the region go back to the ‘\\s-+’ marker.
      (progn
        (re-search-backward
         "\\_<\\(p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)\\)\\_>\\s-+")
        (forward-symbol 1))
      ;; Fontify the found word as `zephir-keyword-face'.
      (0 'zephir-keyword-face t))
     ;; Finally search for ‘{ get, set, toString }’ form.
     ("\\<\\(?:get\\|set\\|toString\\)\\>"
      ;; We are starting from the visibility word again.
      (save-excursion
        (re-search-forward "}" nil 'noerror)
        (forward-char -1)
        (point))
      ;; Do not move back when we've found all matches to ensure
      ;; forward progress.  At this point we are done with the form.
      nil
      ;; Fontify the found word as `zephir-keyword-face'.
      (0 'zephir-keyword-face t))))
  "Font lock keywords for Zephir Mode.")


;;;; Alignment


;;;; Imenu

(defvar zephir-imenu-generic-expression
  `(("Namespaces"
     ,(zephir-create-regexp-for-classlike "namespace") 2)
    ("Classes"
     ,(zephir-create-regexp-for-classlike) 2)
    ("Interfaces"
     ,(zephir-create-regexp-for-classlike "interface") 2)
    ("All Methods"
     ,(zephir-create-regexp-for-function) 2)
    ("Public Methods"
     ,(zephir-create-regexp-for-function '("public")) 2)
    ("Protected Methods"
     ,(zephir-create-regexp-for-function '("protected")) 2)
    ("Private Methods"
     ,(zephir-create-regexp-for-function '("private")) 2)
    ("Properties"
     ,(zephir-rx line-start
                 (* (syntax whitespace))
                 (or "public" "protected" "private")
                 (+ (syntax whitespace))
                 (group identifier)
                 (* (syntax whitespace))
                 (any "=" ";" "{"))
     1)
    ("Constants"
     ,(zephir-create-regexp-for-constant) 2))
  "Imenu generic expression for `zephir-mode'.
For more see `imenu-generic-expression'.")


;;;; Initialization

(defvar zephir-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Symbol constituents
    (modify-syntax-entry ?_   "_"      table)
    (modify-syntax-entry ?$   "_"      table)

    ;; Punctuaction constituents
    (modify-syntax-entry ?+   "."      table)
    (modify-syntax-entry ?-   "."      table)
    (modify-syntax-entry ?=   "."      table)
    (modify-syntax-entry ?%   "."      table)
    (modify-syntax-entry ?<   "."      table)
    (modify-syntax-entry ?>   "."      table)
    (modify-syntax-entry ?&   "."      table)
    (modify-syntax-entry ?|   "."      table)

    ;; Characters used to delimit string constants
    (modify-syntax-entry ?\"  "\""     table)
    (modify-syntax-entry ?\'  "\""     table)

    ;; Comment enders
    (modify-syntax-entry ?\n  "> b"    table)

    ;; Set up block and line oriented comments
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23"   table)

    ;; The parenthesis, braces and brackets
    (modify-syntax-entry ?\(  "()"     table)
    (modify-syntax-entry ?\)  ")("     table)
    (modify-syntax-entry ?\{  "(}"     table)
    (modify-syntax-entry ?\}  "){"     table)
    (modify-syntax-entry ?\[  "(]"     table)
    (modify-syntax-entry ?\]  ")["     table)
    table)
  "Syntax table in use in `zephir-mode' buffers.

This includes setting ' and \" as string delimiters, and setting up
the comment syntax tokens handle both line style \"//\" and block style
\"/*\" \"*/\" comments.")

;;;###autoload
(define-derived-mode zephir-mode prog-mode "Zephir" ()
  "A major mode for editing Zephir code.

\\{zephir-mode-map}

Turning on Zephir Mode calls the value of `prog-mode-hook' and then of
`zephir-mode-hook', if they are non-nil."
  :group 'zephir

  ;; Comments setup
  (setq-local comment-use-syntax t)
  (setq-local comment-auto-fill-only-comments t)
  (setq-local comment-multi-line t)
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(/[*/]+\\)\\s-*")
  (setq-local comment-end "")

  ;; Font locking
  (setq-local font-lock-syntactic-face-function
              #'zephir-font-lock-syntactic-face)
  (setq-local font-lock-defaults
              '((zephir-font-lock-keywords) ; keywords
                nil                         ; keywords-only
                nil))                       ; case-fold

  ;; TODO(serghei): Paragraphs

  ;; Imenu
  (setq-local imenu-generic-expression zephir-imenu-generic-expression)

  ;; Navigation
  (setq-local beginning-of-defun-function #'zephir-beginning-of-defun)
  (setq-local end-of-defun-function #'zephir-end-of-defun)

  ;; Indentation
  (setq-local indent-line-function #'zephir-indent-line)
  (setq-local indent-tabs-mode zephir-indent-tabs-mode))


;; Invoke zephir-mode when appropriate

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zep\\'" . zephir-mode))

(provide 'zephir-mode)
;;; zephir-mode.el ends here
