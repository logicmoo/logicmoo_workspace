;;; zen-mode.el --- A major mode for the Zen programming language -*- lexical-binding: t -*-

;; Version: 0.8.20200609
;; Package-Version: 0.8.20200609
;; Package-Commit: c1b1806358f3cce6c04b30699987d82dc7d42559
;; Author: Andrea Orru <andreaorru1991@gmail.com>, Andrew Kelley <superjoe30@gmail.com>, kristopher tate <kt@connectfree.co.jp>, Yoshitaka Takemoto <yt.3b8@connectfree.co.jp>
;; Keywords: zen, languages
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/zenlang/zen-mode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides a major mode for the Zen programming language

;;; Code:

(defun zen-re-word (inner)
  "Construct a regular expression for the word INNER."
  (concat "\\<" inner "\\>"))

(defun zen-re-grab (inner)
  "Construct a group regular expression for INNER."
  (concat "\\(" inner "\\)"))

(defun zen-re-group (inner)
  "Construct a non-capture group regular expression for INNER."
  (concat "\\(?:" inner "\\)"))

(defconst zen-re-identifier "[[:word:]_][[:word:]_[:digit:]]*")
(defconst zen-re-id-path (concat zen-re-identifier
                                 (zen-re-group (concat "[[:space:]]*\\.[[:space:]]*"
                                                       zen-re-identifier))
                                 "*"))
(defconst zen-builtin-types
  "@\\(?:Frame\\|Vector\\|Type\\(Of\\)?\\)")

(defconst zen-re-align-mut-volatile-syntax
  (concat (zen-re-group
           (concat
            (zen-re-group
             (concat
              "volatile[[:space:]]\\|mut[[:space:]]\\|allowzero[[:space:]]"
              "align(\\(?:[[:space:]]*[[:digit:]]+[[:space:]]*\\|[[:space:]]*@alignOf("
              zen-re-identifier
              "[[:space:]]*)\\))"
              ))
            "[[:space:]]*"
            ))))

(defconst zen-re-array-type-syntax
  (concat "\\["
          (zen-re-group "\\*c?\\|[[:digit:]]+")
          "?"
          (zen-re-group (concat ":[[:digit:]]+\\|:" ;; :0
                                zen-re-id-path))    ;; :sentinel
          "?\\][[:space:]]*"
          (zen-re-group zen-re-align-mut-volatile-syntax)"*"))


(defconst zen-re-pointer-type-syntax
  (concat "[*]"
          (zen-re-group ":vtable[[:space:]]+")"?"
          zen-re-align-mut-volatile-syntax "*"
          (zen-re-group "vtable")"?"))

(defconst zen-re-const-error-syntax
  "[]*][[:space:]]*\\(\\<const\\>\\)")

(defconst zen-re-types-syntax
  (concat (zen-re-group (concat "\\??" ;; optional
                                (zen-re-group zen-re-pointer-type-syntax) "?"
                                (zen-re-group zen-re-array-type-syntax) "?"
                                "[[:space:]]*")) "*"
          (zen-re-group (concat
                         zen-builtin-types
                         "\\|"
                         zen-re-id-path))))

(defconst zen-re-type-annotation
  (concat (zen-re-grab zen-re-identifier)
          "[[:space:]]*:[[:space:]]*"
          (zen-re-grab zen-re-types-syntax)))


(defun zen-re-definition (dtype)
  "Construct a regular expression for definitions of type DTYPE."
  (concat (zen-re-word dtype) "[[:space:]]+" (zen-re-grab zen-re-identifier)))

(defconst zen-re-catch-value
  (concat (zen-re-group "catch\\|else\\|=>\\|)")
          "[[:space:]]*"
          (zen-re-grab "|")
          "\\*?"
          (zen-re-grab zen-re-identifier)
          (zen-re-grab (concat
                        (zen-re-group (concat
                                       "[[:space:]]*,[[:space:]]*"
                                       zen-re-identifier))
                        "?"))
          (zen-re-grab "|")))


(defconst zen-re-break-syntax
  (concat (zen-re-grab "break")
          (zen-re-grab (concat
                        (zen-re-group
                         (concat "[[:space:]]+:"
                                 zen-re-identifier))
                        "?"))))
(defconst zen-re-label-syntax
  (concat (zen-re-grab (concat zen-re-identifier ":"))
          "[[:space:]]*{"))

(defconst zen-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?= ?! ?< ?>))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 12" table)
    (modify-syntax-entry ?\n ">"    table)

    table))

(defconst zen-keywords
  '(
    ;; Storage
    "const" "var" "extern" "packed" "export" "pub" "noalias" "inline" "mut"
    "comptime" "callconv" "volatile" "align" "linksection"
    "threadlocal" "allowzero"

    ;; Structure
    "struct" "enum" "union" "interface" "error"

    ;; Statement
    "return" "continue" "asm" "defer" "errdefer" "unreachable" ;;"break"
    "try" "catch" "async" "await" "suspend" "resume" "cancel"

    ;; Conditional
    "if" "else" "switch" "and" "or" "orelse"

    ;; Repeat
    "while" "for"

    ;; Other keywords
    "fn" "test" "usingnamespace" "nosuspend"

    ;; function
    "noinline" "deprecated"
    ))
(defconst zen-slice-range "\\.\\.")
(defconst zen-int-range "\\.\\.\\.")
(defconst zen-types
  '(
    ;; Integer types
    "i1" "u1" "i2" "u2" "i3" "u3" "i4" "u4" "i5" "u5" "i6" "u6" "i7" "u7" "i8" "u8"
    "i16" "u16" "i29" "u29" "i32" "u32" "i64" "u64" "i128" "u128"
    "isize" "usize"

    ;; Floating types
    "f16" "f32" "f64" "f128"

    ;; C types
    "c_short" "c_ushort" "c_int" "c_uint" "c_long" "c_ulong"
    "c_longlong" "c_ulonglong" "c_longdouble" "c_void"

    ;; Comptime types
    "comptime_int" "comptime_float"

    ;; Other types
    "bool" "void" "noreturn" "type" "anyerror" "anytype"
    ;; Frame
    "anyframe"
    ))


(defconst zen-constants
  '(
    ;; Boolean
    "true" "false"

    ;; Other constants
    "null" "undefined" "this"))

(defconst zen-electric-indent-chars
  '( ?\; ?, ?\) ?\] ?} ))

(defgroup zen nil
  "Support for Zen."
  :link '(url-link "https://www.zen-lang.org/")
  :group 'languages)

(defcustom zen-indent-offset 4
  "Indent Zen code by this number of spaces."
  :type 'integer
  :group 'zen
  :safe #'integerp)

(defface zen-multiline-string-face
  '((t :inherit font-lock-string-face))
  "Face for multiline string literals."
  :group 'zen)

(defface zen-label-face
  '((t :inherit font-lock-builtin-face))
  "Face for label."
  :group 'zen)

(defface zen-catch-vertical-bar-face
  '((t :inherit font-lock-negation-char-face))
  "Face for catch |x|."
  :group 'zen)

(defface zen-slice-range-face
  '((t :inherit font-lock-keyword-face))
  "Face for .."
  :group 'zen)

(defface zen-int-range-face
  '((t :inherit font-lock-negation-char-face))
  "Face for ..."
  :group 'zen)

(defface zen-error-face
  '((t :inherit font-lock-warning-face
       :underline t
       ))
  "Face for *const, ]const, deprecated"
  :group 'zen)

(defvar zen-font-lock-keywords
  `(
    (,zen-re-const-error-syntax 1 'zen-error-face)
    (,"deprecated" . 'zen-error-face)
    (,(zen-re-definition "fn") 1 font-lock-function-name-face)
    (,(zen-re-definition "var") 1 font-lock-variable-name-face)
    (,(zen-re-definition "const") 1 font-lock-variable-name-face)

    (,zen-builtin-types . font-lock-type-face)
    ;; Builtins (prefixed with @)
    (,(concat "@" zen-re-identifier) . font-lock-builtin-face)

    (,(regexp-opt zen-keywords  'symbols) . font-lock-keyword-face)
    ;; break :label
    (,zen-re-break-syntax (1 font-lock-keyword-face)
                          (2 'zen-label-face))
    (,zen-re-label-syntax 1 'zen-label-face)
    ;; Type annotations (both variable and type)
    (,zen-re-type-annotation  (1 font-lock-variable-name-face)
                              (2 font-lock-type-face))
    ;; |value|
    (,zen-re-catch-value (1 'zen-catch-vertical-bar-face)
                         (2 font-lock-variable-name-face)
                         (3 font-lock-variable-name-face)
                         (4 'zen-catch-vertical-bar-face))


    ;; Keywords, constants and types
    (,(regexp-opt zen-constants 'symbols) . font-lock-constant-face)
    (,(regexp-opt zen-types     'symbols) . font-lock-type-face)
    (,zen-int-range . 'zen-int-range-face)
    (,zen-slice-range . 'zen-slice-range-face)
    ("[!.]+" . font-lock-negation-char-face)

    ))


(defun zen-paren-nesting-level nil "Return paren nesting level." () (nth 0 (syntax-ppss)))
(defun zen-currently-in-str nil "Are we currently inside a string?" () (nth 3 (syntax-ppss)))
(defun zen-start-of-current-str-or-comment nil "Are we at the start of current string or comment?" () (nth 8 (syntax-ppss)))

(defun zen-skip-backwards-past-whitespace-and-comments nil "Used for skipping backwards past whitespace and comments." ()
  (while (or
          ;; If inside a comment, jump to start of comment.
          (let ((start (zen-start-of-current-str-or-comment)))
            (and start
                 (not (zen-currently-in-str))
                 (goto-char start)))
          ;; Skip backwards past whitespace and comment end delimiters.
          (/= 0 (skip-syntax-backward " >")))))

(defun zen-mode-indent-line nil "Indent line function for `zen-mode'."
  (interactive)
  ;; First, calculate the column that this line should be indented to.
  (let ((indent-col
         (save-excursion
           (back-to-indentation)
           (let* (;; paren-level: How many sets of parens (or other delimiters)
                  ;;   we're within, except that if this line closes the
                  ;;   innermost set(s) (e.g. the line is just "}"), then we
                  ;;   don't count those set(s).
                  (paren-level
                   (save-excursion
                     (while (looking-at "[]})]") (forward-char))
                     (zen-paren-nesting-level)))
                  ;; prev-block-indent-col: If we're within delimiters, this is
                  ;; the column to which the start of that block is indented
                  ;; (if we're not, this is just zero).
                  (prev-block-indent-col
                   (if (<= paren-level 0) 0
                     (save-excursion
                       (while (>= (zen-paren-nesting-level) paren-level)
                         (backward-up-list)
                         (back-to-indentation))
                       (current-column))))
                  ;; base-indent-col: The column to which a complete expression
                  ;;   on this line should be indented.
                  (base-indent-col
                   (if (<= paren-level 0)
                       prev-block-indent-col
                     (or (save-excursion
                           (backward-up-list)
                           (forward-char)
                           (and (not (looking-at " *\\(//[^\n]*\\)?\n"))
                                (current-column)))
                         (+ prev-block-indent-col zen-indent-offset))))
                  ;; is-expr-continutation: True if this line continues an
                  ;; expression from the previous line, false otherwise.
                  (is-expr-continutation
                   (and
                    (not (looking-at "[]});]"))
                    (save-excursion
                      (zen-skip-backwards-past-whitespace-and-comments)
                      (when (> (point) 1)
                        (backward-char)
                        (not (looking-at "[,;([{}]")))))))
             ;; Now we can calculate indent-col:
             (if is-expr-continutation
                 (+ base-indent-col zen-indent-offset)
               base-indent-col)))))
    ;; If point is within the indentation whitespace, move it to the end of the
    ;; new indentation whitespace (which is what the indent-line-to function
    ;; always does).  Otherwise, we don't want point to move, so we use a
    ;; save-excursion.
    (if (<= (current-column) (current-indentation))
        (indent-line-to indent-col)
      (save-excursion (indent-line-to indent-col)))))

(defun zen-syntax-propertize-to-newline-if-in-multiline-str (end)
  "Check if we're in a multiline string literal; if we're not, do nothing.  \
Return at EOF or when END is found."

  (when (zen-currently-in-str)
    (let ((start (zen-start-of-current-str-or-comment)))
      (when (save-excursion
              (goto-char start)
              (looking-at "\\\\\\\\"))
        ;; At this point, we've determined that we're within a multiline string
        ;; literal.  Let `stop' be the position of the closing newline, or
        ;; `end', whichever comes first.
        (let ((stop (if (save-excursion
                          (goto-char start)
                          (re-search-forward "\n" end t))
                        (prog1 (match-end 0)
                          (let ((str-contents-begin (+ 1 start))
                                (str-contents-end   (match-beginning 0))
                                (str-end (match-beginning 0)))
                            ;; set characters
                            (when (< str-contents-begin str-contents-end)
                              (put-text-property str-contents-begin
                                                 str-contents-end
                                                 'syntax-table
                                                 (string-to-syntax ".")))
                            ;; close string
                            (put-text-property str-end
                                               (+ str-end 1)
                                               'syntax-table
                                               (string-to-syntax "|"))))
                      end)))
          (goto-char stop))))))

(defun zen-equal-nonnull (a b)
  (and a b (= a b)))

(defun zen-syntax-propertize (start end)
  "Function for applying `syntax-table' properties to a specified stretch of text between START and END."
  (goto-char start)
  (zen-syntax-propertize-to-newline-if-in-multiline-str end)
  (funcall
   (syntax-propertize-rules
    ;; Multiline strings
    ("\\(\\\\\\)\\\\"
     (1 (prog1 "|"
          (if (zen-equal-nonnull (match-beginning 0) (zen-start-of-current-str-or-comment))
              ;; multiline strings
              (progn
                (goto-char (match-end 0))
                (zen-syntax-propertize-to-newline-if-in-multiline-str end))
              ;; * escape character
              (put-text-property (match-beginning 0)
                                 (+ 1 (match-beginning 0))
                                 'syntax-table
                                 (string-to-syntax "\\")))))))
   (point) end))

(defun zen-mode-syntactic-face-function (state)
  "Determines which face to use for a given STATE syntactic element (a string or a comment)."
  (if (nth 3 state)
      (save-excursion
        (goto-char (nth 8 state))
        (if (looking-at "\\\\\\\\")
            'zen-multiline-string-face
          'font-lock-string-face))
    (save-excursion
      (goto-char (nth 8 state))
      (if (looking-at "///[^/]")
          'font-lock-doc-face
        'font-lock-comment-face))))

;;; Imenu support
(defun zen-re-structure-def-imenu (stype)
  "Construct a regular expression for strucutres definitions of type STYPE."
  (concat (zen-re-word "const") "[[:space:]]+"
      (zen-re-grab zen-re-identifier)
      ".*"
      (zen-re-word stype)))

(defvar zen-imenu-generic-expression
  (append (mapcar (lambda (x)
            (list (capitalize x) (zen-re-structure-def-imenu x) 1))
          '("enum" "struct" "union" "interface"))
      `(("Fn" ,(zen-re-definition "fn") 1))))

;;;###autoload
(define-derived-mode zen-mode prog-mode "Zen"
  "A major mode for the Zen programming language."
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local electric-indent-chars
              (append zen-electric-indent-chars
                      (and (boundp 'electric-indent-chars)
                           electric-indent-chars)))
  (setq-local indent-line-function 'zen-mode-indent-line)
  (setq-local indent-tabs-mode nil)  ; Zen forbids tab characters.
  (setq-local syntax-propertize-function 'zen-syntax-propertize)
  (setq-local imenu-generic-expression zen-imenu-generic-expression)
  (setq font-lock-defaults '(zen-font-lock-keywords
                             nil nil nil nil
                             (font-lock-syntactic-face-function
                              . zen-mode-syntactic-face-function))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zen\\'" . zen-mode))

(provide 'zen-mode)
;;; zen-mode.el ends here
