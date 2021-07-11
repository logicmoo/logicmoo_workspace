;;; phps-mode-lexer.el -- Lexer for PHPs -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; The idea is gathering everything directly related to the lexer in this file,
;; any higher order meta-lexer logic goes into `phps-mode-lex-analyzer.el'.
;;
;; Features:
;; * Defines the lexer for this grammar based on the Zend PHP Lexer at
;;  `https://github.com/php/php-src/blob/master/Zend/zend_language_scanner.l'
;;  which is using re2c.

;;; Code:


(require 'phps-mode-macros)
(require 'phps-mode-parser-grammar-macro)

(require 'semantic)
(require 'semantic/lex)
(require 'subr-x)


(define-error 'phps-lexer-error "PHPs Lexer Error")


;; INITIALIZE SETTINGS


(phps-mode-parser-grammar-macro-CG
 'parser-mode t)
(phps-mode-parser-grammar-macro-CG
 'short-tags t)


;; SETTINGS


;; @see https://secure.php.net/manual/en/language.types.integer.php
(defconst phps-mode-lexer--long-limit
  2147483648
  "Limit for 32-bit integer.")

(defconst phps-mode-lexer--bnum
  "0b[01]+"
  "Boolean number.")

(defconst phps-mode-lexer--hnum
  "0x[0-9a-fA-F]+"
  "Hexadecimal number.")

(defconst phps-mode-lexer--lnum
  "[0-9]+"
  "Long number.")

(defconst phps-mode-lexer--dnum
  "\\([0-9]*\\.[0-9]+\\)\\|\\([0-9]+\\.[0-9]*\\)"
  "Double number.")

(defconst phps-mode-lexer--exponent-dnum
  (format "\\(\\(%s\\|%s\\)[eE][\\+-]?%s\\)"
          phps-mode-lexer--lnum
          phps-mode-lexer--dnum
          phps-mode-lexer--lnum)
  "Exponent double number.")

(defconst phps-mode-lexer--label
  "[A-Za-z_[:nonascii:]][0-9A-Za-z_[:nonascii:]]*"
  "Labels are used for names.")
;; NOTE original is [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]*
;; NOTE Rebuilt for comparability with emacs-lisp

(defconst phps-mode-lexer--whitespace
  "[ \n\r\t]+"
  "White-space.")

(defconst phps-mode-lexer--tabs-and-spaces
  "[ \t]*"
  "Tabs and white-spaces.")

(defconst phps-mode-lexer--tokens
  "[][;:,.()|^&+/*=%!~$<>?@-]"
  "Tokens.")
;; NOTE Original is [;:,.\[\]()|^&+-/*=%!~$<>?@]
;; NOTE The hyphen moved last since it has special meaning and to avoid it being interpreted as a range.

(defconst phps-mode-lexer--any-char
  "[^z-a]"
  "Any character.  The Zend equivalent is [^] but is not possible in Emacs Lisp.")

(defconst phps-mode-lexer--newline
  "[\n\r]"
  "Newline characters.  The Zend equivalent is (\"\r\"|\"\n\"|\"\r\n\").")


;; VARIABLES


(defvar-local phps-mode-lexer--generated-tokens nil
  "List of current generated tokens.")

(defvar-local phps-mode-lexer--generated-new-tokens nil
  "List of current newly generated tokens.")

(defvar-local phps-mode-lexer--state nil
  "Current state of lexer.")

(defvar-local phps-mode-lexer--state-stack nil
  "Current state-stack of lexer.")

(defvar-local phps-mode-lexer--states nil
  "History of state and state-stack.")

(defvar-local phps-mode-lexer--heredoc-label nil
  "Current heredoc label.")

(defvar-local phps-mode-lexer--heredoc-label-stack nil
  "Stack of heredoc labels.")

(defvar-local phps-mode-lexer--match-length nil
  "Maximum match length.")

(defvar-local phps-mode-lexer--match-body nil
  "Lambda om maximum match.")

(defvar-local phps-mode-lexer--match-data nil
  "Match data.")

(defvar-local phps-mode-lexer--nest-location-stack nil
  "Nesting stack.")

(defvar-local phps-mode-lexer--restart-flag nil
  "Flag whether to restart or not.")

;; HELPER FUNCTIONS


(defun phps-mode-lexer--parser-mode ()
  "Return whether we have some expected value or not."
  nil)

(defun phps-mode-lexer--begin (state)
  "Begin STATE."
  (phps-mode-debug-message
   (message "Begin state: %s" state))
  (setq phps-mode-lexer--state state))

(defun phps-mode-lexer--yy-push-state (state)
  "Add STATE to stack and then begin state."
  (push
   phps-mode-lexer--state
   phps-mode-lexer--state-stack)
  (phps-mode-debug-message
   (message
    "Pushed state: %s"
    phps-mode-lexer--state))
  (phps-mode-lexer--begin state))

(defun phps-mode-lexer--yy-pop-state ()
  "Pop current state from stack."
  (let ((old-state (pop phps-mode-lexer--state-stack)))
    (phps-mode-debug-message
     (message
      "Popped state: %s"
      old-state))

    ;; (message "Going back to poppped state %s" old-state)
    (if old-state
        (phps-mode-lexer--begin old-state)
      (signal
       'phps-lexer-error
       (list
        (format "Trying to pop last state at %d" (point))
        (point))))))

(defun phps-mode-lexer--move-forward (position)
  "Move forward to POSITION."
  (setq semantic-lex-end-point position))

(defun phps-mode-lexer--yyless (points)
  "Move lexer back POINTS."
  (setq semantic-lex-end-point (- semantic-lex-end-point points))
  (forward-char (- points)))

(defun phps-mode-lexer--inline-char-handler ()
  "Mimic inline_char_handler."
  (let ((start (match-beginning 0)))
    (let ((string-start (search-forward "<?" nil t)))
      (if string-start
          (phps-mode-lexer--return-token 'T_INLINE_HTML start (- string-start 2))
        (phps-mode-lexer--return-token 'T_INLINE_HTML start (point-max))))))

(defun phps-mode-lexer--enter-nesting (&optional opening)
  "Enter nesting of OPENING."
  (unless opening
    (setq
     opening
     (buffer-substring-no-properties
      (match-beginning 0)
      (match-end 0))))
  (phps-mode-debug-message
   (message
    "Entered nesting '%s'"
    opening))
  (push
   `(,opening ,(point))
   phps-mode-lexer--nest-location-stack))

(defun phps-mode-lexer--handle-newline ()
  "Handle newline."
  ;; TODO Implement this?
  )

(defun phps-mode-lexer--exit-nesting (closing)
  "Exit nesting of CLOSING."
  (unless phps-mode-lexer--nest-location-stack
    (signal
     'phps-lexer-error
     (list
      (format "Unmatched '%s' at point %d" closing (point))
      (point))))
  (let ((opening
         (car
          phps-mode-lexer--nest-location-stack)))
    (when (and
           opening
           (or
            (and (string= (car opening) "{")
                 (not (string= closing "}")))
            (and (string= (car opening) "[")
                 (not (string= closing "]")))
            (and (string= (car opening) "(")
                 (not (string= closing ")")))))
      (signal
       'phps-lexer-error
       (list
        (format
         "Bad nesting '%s' started at '%s' vs '%s' at %d'"
         (car opening)
         (car (cdr opening))
         closing
         (point))
        (point))))
    (phps-mode-debug-message
     (message
      "Exited nesting '%s'"
      closing))
    (pop phps-mode-lexer--nest-location-stack)
    t))

(defun phps-mode-lexer--emit-token (token start end)
  "Emit TOKEN with START and END."
  (when (= start end)
    (signal
     'phps-lexer-error
     (list
      (format "Empty token detected: %s %s %s" token start end)
      start
      end
      token)))

  (semantic-lex-push-token (semantic-lex-token token start end))
  (push `(,token ,start . ,end) phps-mode-lexer--generated-tokens)
  (push `(,token ,start . ,end) phps-mode-lexer--generated-new-tokens)

  (phps-mode-debug-message
   (message
    "Emitted token '%s' -> %s"
    (buffer-substring-no-properties
     start
     end)
    `(,token ,start . ,end)))
  
  ;; Push token start, end, lexer state and state stack to variable
  (push
   (list
    start
    end
    phps-mode-lexer--state
    phps-mode-lexer--state-stack
    phps-mode-lexer--heredoc-label
    phps-mode-lexer--heredoc-label-stack
    phps-mode-lexer--nest-location-stack)
   phps-mode-lexer--states))

(defun phps-mode-lexer--get-next-unescaped (character)
  "Find where next un-escaped CHARACTER comes, if none is found return nil."
  ;; (message "phps-mode-lexer--get-next-unescaped(%s)" character)
  (let ((escaped nil)
        (pos nil))
    (while (and (not pos)
                (< (point) (point-max)))
      (progn
        ;; (message "Setting forward one %s vs %s" (point) (point-max))
        (forward-char)
        (if (and (not escaped)
                 (looking-at-p character))
            (setq pos (1+ (point)))
          (if (looking-at-p "\\\\")
              (setq escaped (not escaped))
            (setq escaped nil)))))
    pos))

(defun phps-mode-lexer--skip-token (_token &optional start end)
  "Skip TOKEN to list with START and END."
  (unless start
    (setq start (match-beginning 0)))
  (unless end
    (setq end (match-end 0)))
  (setq semantic-lex-end-point end))

(defmacro phps-mode-lexer--match-macro (conditions &rest body)
  "Check if CONDITIONS hold, if so execute BODY."
  `(phps-mode-lexer--re2c-rule
    ,conditions
    (lambda()
      ,@body)))

(defun phps-mode-lexer--return-token (&optional token start end)
  "Return TOKEN with START and END."
  (unless start
    (setq start (match-beginning 0)))
  (unless end
    (setq end (match-end 0)))
  (unless token
    (setq
     token
     (buffer-substring-no-properties
      start
      end)))
  (phps-mode-lexer--emit-token
   token
   start
   end))

(defun phps-mode-lexer--check-nesting-at-end ()
  "Check nesting at end."
  (when phps-mode-lexer--nest-location-stack
    (signal
     'phps-lexer-error
     (list
      (format "Bad nesting end at '%d'" (point))
      (point))))
  t)

(defun phps-mode-lexer--return-end-token ()
  "Return end token."
  (if (and
       (phps-mode-lexer--check-nesting-at-end)
       (phps-mode-lexer--parser-mode))
      (phps-mode-lexer--return-token 'T_ERROR)
    (phps-mode-lexer--return-token 'END)))

(defun phps-mode-lexer--reset-doc-comment ()
  "Reset doc comment."
  (when (phps-mode-parser-grammar-macro-CG 'doc_comment)
    (phps-mode-parser-grammar-macro-CG 'doc_comment nil)))

(defun phps-mode-lexer--return-token-with-indent (&optional token start end)
  "Return TOKEN with START and END."
  (phps-mode-lexer--return-token
   token
   start
   end))

;; TODO Do something with offset?
(defun phps-mode-lexer--return-token-with-str (token _offset &optional start end)
  "Return TOKEN at OFFSET with START and END."
  (unless start
    (setq start (match-beginning 0)))
  (unless end
    (setq end (match-end 0)))
  (phps-mode-lexer--return-token token start end))

(defun phps-mode-lexer--return-whitespace ()
  "Return whitespace."
  (phps-mode-lexer--move-forward (match-end 0)))

(defun phps-mode-lexer--return-exit-nesting-token (&optional token start end)
  "Return TOKEN if it does not exit a nesting with optional START and END."
  (unless start
    (setq start (match-beginning 0)))
  (unless end
    (setq end (match-end 0)))
  (unless token
    (setq
     token
     (buffer-substring-no-properties
      start
      end)))
  (if (and
       (phps-mode-lexer--exit-nesting token)
       (phps-mode-lexer--parser-mode))
      (phps-mode-lexer--return-token 'T_ERROR)
    (phps-mode-lexer--return-token
     token
     start
     end)))

(defun phps-mode-lexer--restart ()
  "Restart."
  (setq phps-mode-lexer--restart-flag t))

(defun phps-mode-lexer--return-token-with-val (&optional token start end)
  "Return TOKEN with START and END."
  (phps-mode-lexer--return-token token start end))

(defun phps-mode-lexer--return-or-skip-token (&optional token start end)
  "Return TOKEN with START and END but only in parse-mode."
  (if (phps-mode-lexer--parser-mode)
      (phps-mode-lexer--skip-token token start end)
    (phps-mode-lexer--return-token token start end)))


;; LEXER FUNCTIONS BELOW


(defun phps-mode-lexer--re2c-rule (condition body)
  "Process rule with CONDITION and BODY."
  (when condition
    (let ((match-end (match-end 0))
          (match-beginning (match-beginning 0)))
      (let ((matching-length (- match-end match-beginning)))
        (when (> matching-length 0)
          (when (or (not phps-mode-lexer--match-length)
                    (> matching-length phps-mode-lexer--match-length))
            (setq phps-mode-lexer--match-length matching-length)
            (setq phps-mode-lexer--match-body body)
            (setq phps-mode-lexer--match-data (match-data))))))))

(defun phps-mode-lexer--re2c-execute ()
  "Execute matching body (if any)."
  (if phps-mode-lexer--match-body
      (progn
        (set-match-data phps-mode-lexer--match-data)
        (funcall phps-mode-lexer--match-body))
    (signal
     'phps-lexer-error
     (list
      (format "Found no matching lexer rule to execute at %d" (point))
      (point)))))

(defun phps-mode-lexer--reset-match-data ()
  "Reset match data."
  (setq phps-mode-lexer--match-length 0)
  (setq phps-mode-lexer--match-data nil)
  (setq phps-mode-lexer--match-body nil))

;; If multiple rules match, re2c prefers the longest match.
;; If rules match the same string, the earlier rule has priority.
;; @see http://re2c.org/manual/syntax/syntax.html
(defun phps-mode-lexer--re2c ()
  "Elisp port of original Zend re2c lexer."

  (setq phps-mode-lexer--generated-new-tokens nil)
  (setq phps-mode-lexer--restart-flag nil)
  (let ((old-start (point)))
    (phps-mode-debug-message
     (let ((start (point))
           (end (+ (point) 5))
           (lookahead))
       (when (> end (point-max))
         (setq end (point-max)))
       (setq
        lookahead
        (buffer-substring-no-properties
         start
         end))
     (message
      "\nRunning lexer from point %s, state: %s, lookahead: '%s'.."
      old-start
      phps-mode-lexer--state
      lookahead)))
    (phps-mode-lexer--reset-match-data)
    
    (let ((SHEBANG (equal phps-mode-lexer--state 'SHEBANG))
          (ST_IN_SCRIPTING (equal phps-mode-lexer--state 'ST_IN_SCRIPTING))
          (ST_INITIAL (equal phps-mode-lexer--state 'ST_INITIAL))
          (ST_LOOKING_FOR_PROPERTY (equal phps-mode-lexer--state 'ST_LOOKING_FOR_PROPERTY))
          (ST_DOUBLE_QUOTES (equal phps-mode-lexer--state 'ST_DOUBLE_QUOTES))
          (ST_BACKQUOTE (equal phps-mode-lexer--state 'ST_BACKQUOTE))
          (ST_HEREDOC (equal phps-mode-lexer--state 'ST_HEREDOC))
          (ST_NOWDOC (equal phps-mode-lexer--state 'ST_NOWDOC))
          (ST_LOOKING_FOR_VARNAME (equal phps-mode-lexer--state 'ST_LOOKING_FOR_VARNAME))
          (ST_END_HEREDOC (equal phps-mode-lexer--state 'ST_END_HEREDOC))
          (ST_VAR_OFFSET (equal phps-mode-lexer--state 'ST_VAR_OFFSET)))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "exit"))
       (phps-mode-lexer--return-token-with-indent 'T_EXIT))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "die"))
       (phps-mode-lexer--return-token-with-indent 'T_DIE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "fn"))
       (phps-mode-lexer--return-token-with-indent 'T_FN))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "function"))
       (phps-mode-lexer--return-token-with-indent 'T_FUNCTION))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "const"))
       (phps-mode-lexer--return-token-with-indent 'T_CONST))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "return"))
       (phps-mode-lexer--return-token-with-indent 'T_RETURN))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "#\\["))
       (phps-mode-lexer--enter-nesting "[")
       (phps-mode-lexer--return-token 'T_ATTRIBUTE))

      (phps-mode-lexer--match-macro
       (and
        ST_IN_SCRIPTING
        (looking-at
         (concat
          "yield"
          phps-mode-lexer--whitespace
          "from"
          "[^a-zA-Z0-9_\x80-\xff]")))
       (phps-mode-lexer--return-token-with-indent 'T_YIELD_FROM))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "yield"))
       (phps-mode-lexer--return-token-with-indent 'T_YIELD))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "try"))
       (phps-mode-lexer--return-token-with-indent 'T_TRY))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "catch"))
       (phps-mode-lexer--return-token-with-indent 'T_CATCH))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "finally"))
       (phps-mode-lexer--return-token-with-indent 'T_FINALLY))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "throw"))
       (phps-mode-lexer--return-token-with-indent 'T_THROW))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "if"))
       (phps-mode-lexer--return-token-with-indent 'T_IF))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "elseif"))
       (phps-mode-lexer--return-token-with-indent 'T_ELSEIF))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "endif"))
       (phps-mode-lexer--return-token-with-indent 'T_ENDIF))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "else"))
       (phps-mode-lexer--return-token-with-indent 'T_ELSE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "while"))
       (phps-mode-lexer--return-token-with-indent 'T_WHILE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "endwhile"))
       (phps-mode-lexer--return-token-with-indent 'T_ENDWHILE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "do"))
       (phps-mode-lexer--return-token-with-indent 'T_DO))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "for"))
       (phps-mode-lexer--return-token-with-indent 'T_FOR))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "endfor"))
       (phps-mode-lexer--return-token-with-indent 'T_ENDFOR))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "foreach"))
       (phps-mode-lexer--return-token-with-indent 'T_FOREACH))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "endforeach"))
       (phps-mode-lexer--return-token-with-indent 'T_ENDFOREACH))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "declare"))
       (phps-mode-lexer--return-token-with-indent 'T_DECLARE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "enddeclare"))
       (phps-mode-lexer--return-token-with-indent 'T_ENDDECLARE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "instanceof"))
       (phps-mode-lexer--return-token-with-indent 'T_INSTANCEOF))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "as"))
       (phps-mode-lexer--return-token-with-indent 'T_AS))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "switch"))
       (phps-mode-lexer--return-token-with-indent 'T_SWITCH))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "endswitch"))
       (phps-mode-lexer--return-token-with-indent 'T_ENDSWITCH))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "case"))
       (phps-mode-lexer--return-token-with-indent 'T_CASE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "default"))
       (phps-mode-lexer--return-token-with-indent 'T_DEFAULT))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "break"))
       (phps-mode-lexer--return-token-with-indent 'T_BREAK))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "continue"))
       (phps-mode-lexer--return-token-with-indent 'T_CONTINUE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "goto"))
       (phps-mode-lexer--return-token-with-indent 'T_GOTO))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "echo"))
       (phps-mode-lexer--return-token-with-indent 'T_ECHO))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "print"))
       (phps-mode-lexer--return-token-with-indent 'T_PRINT))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "class"))
       (phps-mode-lexer--return-token-with-indent 'T_CLASS))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "interface"))
       (phps-mode-lexer--return-token-with-indent 'T_INTERFACE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "trait"))
       (phps-mode-lexer--return-token-with-indent 'T_TRAIT))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "extends"))
       (phps-mode-lexer--return-token-with-indent 'T_EXTENDS))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "implements"))
       (phps-mode-lexer--return-token-with-indent 'T_IMPLEMENTS))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "->"))
       (phps-mode-lexer--yy-push-state 'ST_LOOKING_FOR_PROPERTY)
       (phps-mode-lexer--return-token-with-indent 'T_OBJECT_OPERATOR))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "?->"))
       (phps-mode-lexer--yy-push-state 'ST_LOOKING_FOR_PROPERTY)
       (phps-mode-lexer--return-token-with-indent 'T_NULLSAFE_OBJECT_OPERATOR))

      (phps-mode-lexer--match-macro
       (and (or ST_IN_SCRIPTING ST_LOOKING_FOR_PROPERTY)
            (looking-at phps-mode-lexer--whitespace))
       (phps-mode-lexer--return-whitespace))

      (phps-mode-lexer--match-macro
       (and ST_LOOKING_FOR_PROPERTY (looking-at "->"))
       (phps-mode-lexer--return-token 'T_OBJECT_OPERATOR))

      (phps-mode-lexer--match-macro
       (and ST_LOOKING_FOR_PROPERTY (looking-at "?->"))
       (phps-mode-lexer--return-token 'T_NULLSAFE_OBJECT_OPERATOR))

      (phps-mode-lexer--match-macro
       (and ST_LOOKING_FOR_PROPERTY (looking-at phps-mode-lexer--label))
       (phps-mode-lexer--yy-pop-state)
       (phps-mode-lexer--return-token-with-str 'T_STRING 0))

      (phps-mode-lexer--match-macro
       (and ST_LOOKING_FOR_PROPERTY (looking-at phps-mode-lexer--any-char))
       (phps-mode-lexer--yyless 0)
       (phps-mode-lexer--yy-pop-state)
       (phps-mode-lexer--restart))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "::"))
       (phps-mode-lexer--return-token 'T_PAAMAYIM_NEKUDOTAYIM))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "\\.\\.\\."))
       (phps-mode-lexer--return-token 'T_ELLIPSIS))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "\\?\\?"))
       (phps-mode-lexer--return-token 'T_COALESCE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "new"))
       (phps-mode-lexer--return-token-with-indent 'T_NEW))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "clone"))
       (phps-mode-lexer--return-token-with-indent 'T_CLONE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "var"))
       (phps-mode-lexer--return-token-with-indent 'T_VAR))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "("
              phps-mode-lexer--tabs-and-spaces
              "\\(int\\|integer\\)"
              phps-mode-lexer--tabs-and-spaces
              ")")))
       (phps-mode-lexer--return-token 'T_INT_CAST))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "("
              phps-mode-lexer--tabs-and-spaces
              "\\(double\\|float\\)"
              phps-mode-lexer--tabs-and-spaces
              ")")))
       (phps-mode-lexer--return-token 'T_DOUBLE_CAST))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "("
              phps-mode-lexer--tabs-and-spaces
              "\\(real\\)"
              phps-mode-lexer--tabs-and-spaces
              ")")))
       (when (phps-mode-lexer--parser-mode)
         (signal
          'phps-lexer-error
          (list
           (format
            "The (real) cast has been removed, use (float) instead at %d"
            (match-beginning 0))
           (match-beginning 0)
           (match-end 0))))
       (phps-mode-lexer--return-token 'T_DOUBLE_CAST))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "("
              phps-mode-lexer--tabs-and-spaces
              "\\(string\\|binary\\)"
              phps-mode-lexer--tabs-and-spaces
              ")")))
       (phps-mode-lexer--return-token 'T_STRING_CAST))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "("
              phps-mode-lexer--tabs-and-spaces
              "array"
              phps-mode-lexer--tabs-and-spaces
              ")")))
       (phps-mode-lexer--return-token 'T_ARRAY_CAST))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "("
              phps-mode-lexer--tabs-and-spaces
              "object"
              phps-mode-lexer--tabs-and-spaces
              ")")))
       (phps-mode-lexer--return-token 'T_OBJECT_CAST))

      (phps-mode-lexer--match-macro
       (and
        ST_IN_SCRIPTING
        (looking-at
         (concat
          "("
          phps-mode-lexer--tabs-and-spaces
          "\\(bool\\|boolean\\)"
          phps-mode-lexer--tabs-and-spaces
          ")")))
       (phps-mode-lexer--return-token 'T_BOOL_CAST))

      (phps-mode-lexer--match-macro
       (and
        ST_IN_SCRIPTING
        (looking-at
         (concat
          "("
          phps-mode-lexer--tabs-and-spaces
          "unset"
          phps-mode-lexer--tabs-and-spaces ")")))
       (phps-mode-lexer--return-token 'T_UNSET_CAST))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "eval"))
       (phps-mode-lexer--return-token-with-indent 'T_EVAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "include"))
       (phps-mode-lexer--return-token-with-indent 'T_INCLUDE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "include_once"))
       (phps-mode-lexer--return-token-with-indent 'T_INCLUDE_ONCE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "require"))
       (phps-mode-lexer--return-token-with-indent 'T_REQUIRE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "require_once"))
       (phps-mode-lexer--return-token-with-indent 'T_REQUIRE_ONCE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "namespace"))
       (phps-mode-lexer--return-token-with-indent 'T_NAMESPACE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "use"))
       (phps-mode-lexer--return-token-with-indent 'T_USE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "insteadof"))
       (phps-mode-lexer--return-token-with-indent 'T_INSTEADOF))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "global"))
       (phps-mode-lexer--return-token-with-indent 'T_GLOBAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "isset"))
       (phps-mode-lexer--return-token-with-indent 'T_ISSET))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "empty"))
       (phps-mode-lexer--return-token-with-indent 'T_EMPTY))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "__halt_compiler"))
       (phps-mode-lexer--return-token-with-indent 'T_HALT_COMPILER))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "static"))
       (phps-mode-lexer--return-token-with-indent 'T_STATIC))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "abstract"))
       (phps-mode-lexer--return-token-with-indent 'T_ABSTRACT))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "final"))
       (phps-mode-lexer--return-token-with-indent 'T_FINAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "private"))
       (phps-mode-lexer--return-token-with-indent 'T_PRIVATE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "protected"))
       (phps-mode-lexer--return-token-with-indent 'T_PROTECTED))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "public"))
       (phps-mode-lexer--return-token-with-indent 'T_PUBLIC))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "unset"))
       (phps-mode-lexer--return-token-with-indent 'T_UNSET))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "=>"))
       (phps-mode-lexer--return-token 'T_DOUBLE_ARROW))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "list"))
       (phps-mode-lexer--return-token-with-indent 'T_LIST))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "array"))
       (phps-mode-lexer--return-token-with-indent 'T_ARRAY))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "callable"))
       (phps-mode-lexer--return-token-with-indent 'T_CALLABLE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "\\+\\+"))
       (phps-mode-lexer--return-token 'T_INC))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "--"))
       (phps-mode-lexer--return-token 'T_DEC))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "==="))
       (phps-mode-lexer--return-token 'T_IS_IDENTICAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "!=="))
       (phps-mode-lexer--return-token 'T_IS_NOT_IDENTICAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "=="))
       (phps-mode-lexer--return-token 'T_IS_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "\\(!=\\|<>\\)"))
       (phps-mode-lexer--return-token 'T_IS_NOT_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "<=>"))
       (phps-mode-lexer--return-token 'T_SPACESHIP))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "<="))
       (phps-mode-lexer--return-token 'T_IS_SMALLER_OR_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at ">="))
       (phps-mode-lexer--return-token 'T_IS_GREATER_OR_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "\\+="))
       (phps-mode-lexer--return-token 'T_PLUS_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "-="))
       (phps-mode-lexer--return-token 'T_MINUS_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "\\*="))
       (phps-mode-lexer--return-token 'T_MUL_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "\\*\\\\\\*="))
       (phps-mode-lexer--return-token 'T_POW_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "\\*\\\\\\*"))
       (phps-mode-lexer--return-token 'T_POW))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "/="))
       (phps-mode-lexer--return-token 'T_DIV_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "\\.="))
       (phps-mode-lexer--return-token 'T_CONCAT_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "%="))
       (phps-mode-lexer--return-token 'T_MOD_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "<<="))
       (phps-mode-lexer--return-token 'T_SL_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at ">>="))
       (phps-mode-lexer--return-token 'T_SR_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "&="))
       (phps-mode-lexer--return-token 'T_AND_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "|="))
       (phps-mode-lexer--return-token 'T_OR_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "\\^="))
       (phps-mode-lexer--return-token 'T_XOR_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "\\?\\?="))
       (phps-mode-lexer--return-token 'T_COALESCE_EQUAL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "||"))
       (phps-mode-lexer--return-token 'T_BOOLEAN_OR))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "&&"))
       (phps-mode-lexer--return-token 'T_BOOLEAN_AND))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "OR"))
       (phps-mode-lexer--return-token 'T_LOGICAL_OR))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "AND"))
       (phps-mode-lexer--return-token 'T_LOGICAL_AND))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "XOR"))
       (phps-mode-lexer--return-token 'T_LOGICAL_XOR))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "<<"))
       (phps-mode-lexer--return-token 'T_SL))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at ">>"))
       (phps-mode-lexer--return-token 'T_SR))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING
            (looking-at
             (concat "\\(" "]" "\\|" ")" "\\)")))
       (phps-mode-lexer--return-exit-nesting-token))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING
            (looking-at
             (concat "\\(" "\\[" "\\|" "(" "\\)")))
       (phps-mode-lexer--enter-nesting)
       (phps-mode-lexer--return-token))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at phps-mode-lexer--tokens))
       (phps-mode-lexer--return-token))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "{"))
       (phps-mode-lexer--yy-push-state 'ST_IN_SCRIPTING)
       (phps-mode-lexer--enter-nesting "{")
       (phps-mode-lexer--return-token))

      (phps-mode-lexer--match-macro
       (and (or ST_DOUBLE_QUOTES ST_BACKQUOTE ST_HEREDOC) (looking-at "\\${"))
       (phps-mode-lexer--yy-push-state 'ST_LOOKING_FOR_VARNAME)
       (phps-mode-lexer--enter-nesting "{")
       (phps-mode-lexer--return-token 'T_DOLLAR_OPEN_CURLY_BRACES))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "}"))
       (phps-mode-lexer--reset-doc-comment)
       (when phps-mode-lexer--state-stack
         (phps-mode-lexer--yy-pop-state))
       (phps-mode-lexer--return-exit-nesting-token))

      (phps-mode-lexer--match-macro
       (and
        ST_LOOKING_FOR_VARNAME
        (looking-at (concat phps-mode-lexer--label "[\\[}]")))
       (let* ((start (match-beginning 0))
              (end (1- (match-end 0)))
              (_data (buffer-substring-no-properties start end)))
         (phps-mode-lexer--yyless 1)
         (phps-mode-lexer--yy-pop-state)
         (phps-mode-lexer--yy-push-state 'ST_IN_SCRIPTING)
         (phps-mode-lexer--return-token 'T_STRING_VARNAME start end)))

      (phps-mode-lexer--match-macro
       (and
        ST_LOOKING_FOR_VARNAME
        (looking-at phps-mode-lexer--any-char))
       (phps-mode-lexer--yyless 0)
       (phps-mode-lexer--yy-pop-state)
       (phps-mode-lexer--yy-push-state 'ST_IN_SCRIPTING)
       (phps-mode-lexer--restart))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING
            (looking-at phps-mode-lexer--bnum))
       (let* ((start (match-beginning 0))
              (end (match-end 0))
              (data (buffer-substring-no-properties (+ start 2) end))
              (long-number (string-to-number data 2)))
         ;; (message "Binary number %s from %s" long-number data)
         (if (> long-number phps-mode-lexer--long-limit)
             (phps-mode-lexer--return-token 'T_DNUMBER)
           (phps-mode-lexer--return-token 'T_LNUMBER))))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING
            (looking-at phps-mode-lexer--lnum))
       (let* ((start (match-beginning 0))
              (end (match-end 0))
              (data (string-to-number (buffer-substring-no-properties start end))))
         ;; (message "Long number: %d" data)
         (if (> data phps-mode-lexer--long-limit)
             (phps-mode-lexer--return-token 'T_DNUMBER)
           (phps-mode-lexer--return-token 'T_LNUMBER))))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at phps-mode-lexer--hnum))
       (let* ((start (match-beginning 0))
              (end (match-end 0))
              (data (buffer-substring-no-properties (+ start 2) end))
              (long-number (string-to-number data 16)))
         ;; (message "Hexadecimal number %s from %s" long-number data)
         (if (> long-number phps-mode-lexer--long-limit)
             (phps-mode-lexer--return-token 'T_DNUMBER)
           (phps-mode-lexer--return-token 'T_LNUMBER))))

      (phps-mode-lexer--match-macro
       (and ST_VAR_OFFSET (looking-at "\\([0]\\|[1-9][0-9]*\\)"))
       (phps-mode-lexer--return-token 'T_NUM_STRING))

      (phps-mode-lexer--match-macro
       (and ST_VAR_OFFSET
            (looking-at
             (concat "\\("
                     phps-mode-lexer--lnum "\\|"
                     phps-mode-lexer--hnum "\\|"
                     phps-mode-lexer--bnum "\\)")))
       (phps-mode-lexer--return-token 'T_NUM_STRING))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING
            (or (looking-at phps-mode-lexer--dnum)
                (looking-at phps-mode-lexer--exponent-dnum)))
       (phps-mode-lexer--return-token 'T_DNUMBER))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "__CLASS__"))
       (phps-mode-lexer--return-token-with-indent 'T_CLASS_C))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "__TRAIT__"))
       (phps-mode-lexer--return-token-with-indent 'T_TRAIT_C))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "__FUNCTION__"))
       (phps-mode-lexer--return-token-with-indent 'T_FUNC_C))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "__METHOD__"))
       (phps-mode-lexer--return-token-with-indent 'T_METHOD_C))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "__LINE__"))
       (phps-mode-lexer--return-token-with-indent 'T_LINE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "__FILE__"))
       (phps-mode-lexer--return-token-with-indent 'T_FILE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "__DIR__"))
       (phps-mode-lexer--return-token-with-indent 'T_DIR))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "__NAMESPACE__"))
       (phps-mode-lexer--return-token-with-indent 'T_NS_C))

      (phps-mode-lexer--match-macro
       (and SHEBANG (looking-at (concat "#!.*" phps-mode-lexer--newline)))
       (let ((lineno
              (1+
               (phps-mode-parser-grammar-macro-CG 'zend_lineno))))
         (phps-mode-parser-grammar-macro-CG 'zend-lineno lineno))
       (phps-mode-lexer--begin 'ST_INITIAL)
       (phps-mode-lexer--restart))

      (phps-mode-lexer--match-macro
       (and SHEBANG (looking-at phps-mode-lexer--any-char))
       (phps-mode-lexer--yyless 0)
       (phps-mode-lexer--begin 'ST_INITIAL)
       (phps-mode-lexer--restart))

      (phps-mode-lexer--match-macro
       (and ST_INITIAL (looking-at "<\\?="))
       (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
       (when (phps-mode-lexer--parser-mode)
         (phps-mode-lexer--return-token-with-indent 'T_ECHO))
       (phps-mode-lexer--return-token 'T_OPEN_TAG_WITH_ECHO))

      (phps-mode-lexer--match-macro
       (and
        ST_INITIAL
        (looking-at
         (concat
          "<\\?php\\([ \t]\\|"
          phps-mode-lexer--newline
          "\\)")))
       (phps-mode-lexer--handle-newline)
       (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
       (phps-mode-lexer--return-or-skip-token 'T_OPEN_TAG))

      (phps-mode-lexer--match-macro
       (and ST_INITIAL (looking-at "<\\?php"))
       (let ((start (match-beginning 0))
             (end (match-end 0)))

         ;; Allow <?php followed by end of file.
         (cond

          ((equal end (point-max))
           (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
           (phps-mode-lexer--return-or-skip-token 'T_OPEN_TAG))

          ((phps-mode-parser-grammar-macro-CG 'short-tags)
           (phps-mode-lexer--yyless 2)
           (setq end (- end 2))
           (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
           (phps-mode-lexer--return-or-skip-token
            'T_OPEN_TAG
            start
            end))

          (t
           (phps-mode-lexer--inline-char-handler)))))

      (phps-mode-lexer--match-macro
       (and ST_INITIAL (looking-at "<\\?"))
       (if (phps-mode-parser-grammar-macro-CG 'short-tags)
           (progn
             (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
             (phps-mode-lexer--return-or-skip-token 'T_OPEN_TAG))
         (phps-mode-lexer--inline-char-handler)))

      (phps-mode-lexer--match-macro
       (and ST_INITIAL (looking-at phps-mode-lexer--any-char))
       (if (= (point) (point-max))
           (phps-mode-lexer--return-end-token)
         (phps-mode-lexer--inline-char-handler)))

      ;; Make sure a label character follows "->" or "?->", otherwise there is no property
      ;; and "->"/"?->" will be taken literally
      (phps-mode-lexer--match-macro
       (and (or ST_DOUBLE_QUOTES ST_HEREDOC ST_BACKQUOTE)
            (looking-at
             (concat
              "\\$"
              phps-mode-lexer--label
              "->"
              "[a-zA-Z_\x80-\xff]")))
       (phps-mode-lexer--yyless 3)
       (phps-mode-lexer--yy-push-state 'ST_LOOKING_FOR_PROPERTY)
       (phps-mode-lexer--return-token-with-str
        'T_VARIABLE
        1
        (match-beginning 0)
        (- (match-end 0) 3)))

      (phps-mode-lexer--match-macro
       (and (or ST_DOUBLE_QUOTES ST_HEREDOC ST_BACKQUOTE)
            (looking-at
             (concat
              "\\$"
              phps-mode-lexer--label
              "\\?->"
              "[a-zA-Z_\x80-\xff]")))
       (phps-mode-lexer--yyless 4)
       (phps-mode-lexer--yy-push-state 'ST_LOOKING_FOR_PROPERTY)
       (phps-mode-lexer--return-token-with-str
        'T_VARIABLE
        1
        (match-beginning 0)
        (- (match-end 0) 4)))

      ;; A [ always designates a variable offset, regardless of what follows
      (phps-mode-lexer--match-macro
       (and (or ST_DOUBLE_QUOTES ST_HEREDOC ST_BACKQUOTE)
            (looking-at
             (concat
              "\\$"
              phps-mode-lexer--label
              "\\[")))
       (phps-mode-lexer--yyless 1)
       (phps-mode-lexer--yy-push-state 'ST_VAR_OFFSET)
       (phps-mode-lexer--return-token-with-str
        'T_VARIABLE
        1
        (match-beginning 0)
        (- (match-end 0) 1)))

      (phps-mode-lexer--match-macro
       (and (or
             ST_IN_SCRIPTING
             ST_DOUBLE_QUOTES
             ST_HEREDOC
             ST_BACKQUOTE
             ST_VAR_OFFSET)
            (looking-at
             (concat
              "\\$"
              phps-mode-lexer--label)))
       (phps-mode-lexer--return-token-with-str 'T_VARIABLE 1))

      (phps-mode-lexer--match-macro
       (and ST_VAR_OFFSET (looking-at "\\]"))
       (phps-mode-lexer--yy-pop-state)
       (phps-mode-lexer--return-token-with-str "]" 1))

      (phps-mode-lexer--match-macro
       (and ST_VAR_OFFSET
            (looking-at
             (concat "\\(" phps-mode-lexer--tokens
                     "\\|[{}\"`]\\)")))
       (let* ((start (match-beginning 0))
              (end (match-end 0))
              (data (buffer-substring-no-properties start end)))
         ;; Only '[' or '-' can be valid, but returning other tokens will allow a more explicit parse error
         (phps-mode-lexer--return-token data)))

      (phps-mode-lexer--match-macro
       (and ST_VAR_OFFSET (looking-at (concat "[ \n\r\t'#]")))
       ;; Invalid rule to return a more explicit parse error with proper line number
       (phps-mode-lexer--yyless 0)
       (phps-mode-lexer--yy-pop-state)
       (phps-mode-lexer--return-token-with-val 'T_ENCAPSED_AND_WHITESPACE))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "namespace"
              "\\("
              "\\\\"
              phps-mode-lexer--label
              "\\)+")))
       (phps-mode-lexer--return-token-with-str
        'T_NAME_RELATIVE
        (1- (length "namespace\\"))))

      (phps-mode-lexer--match-macro
       (and
        ST_IN_SCRIPTING
        (looking-at (concat
                     phps-mode-lexer--label
                     "\\("
                     "\\\\"
                     phps-mode-lexer--label
                     "\\)+")))
       (phps-mode-lexer--return-token-with-str
        'T_NAME_QUALIFIED
        0))

      (phps-mode-lexer--match-macro
       (and
        ST_IN_SCRIPTING
        (looking-at (concat
                     "\\\\"
                     phps-mode-lexer--label
                     "\\("
                     "\\\\"
                     phps-mode-lexer--label
                     "\\)*")))
       (phps-mode-lexer--return-token-with-str
        'T_NAME_FULLY_QUALIFIED
        1))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "\\\\"))
       (phps-mode-lexer--return-token 'T_NS_SEPARATOR))

      (phps-mode-lexer--match-macro
       (and
        (or ST_IN_SCRIPTING ST_VAR_OFFSET)
        (looking-at phps-mode-lexer--label))
       (phps-mode-lexer--return-token-with-str 'T_STRING 0))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "\\(#\\|//\\)"))
       (let* ((start (match-beginning 0))
              (end (match-end 0))
              (_data (buffer-substring-no-properties start end))
              (line (buffer-substring-no-properties end (line-end-position))))
         (if (string-match "\\?>" line)
             (phps-mode-lexer--return-or-skip-token
              'T_COMMENT
              start
              (+ end (match-beginning 0)))
           (phps-mode-lexer--return-or-skip-token
            'T_COMMENT
            start
            (line-end-position)))))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "\\(/\\*\\|/\\*\\*"
              phps-mode-lexer--whitespace
              "\\)")))
       (let* ((start (match-beginning 0))
              (end (match-end 0))
              (_data (buffer-substring-no-properties start end))
              (doc-com (looking-at-p (concat "/\\*\\*" phps-mode-lexer--whitespace))))
         (let ((string-start (search-forward "*/" nil t)))
           (if string-start
               (if doc-com
                   (progn
                     (phps-mode-lexer--reset-doc-comment)
                     (phps-mode-lexer--return-token
                      'T_DOC_COMMENT
                      start)
                     (phps-mode-parser-grammar-macro-CG 'doc_comment t))
                 (phps-mode-lexer--return-token
                  'T_COMMENT start))
             (progn
               (signal
                'phps-lexer-error
                (list
                 (format
                  "Unterminated comment starting at %d"
                  start)
                 start)))))))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "\\?>"
              phps-mode-lexer--newline
              "?")))
       (let ((start (match-beginning 0))
             (end (match-end 0)))
         (when (= (- end start) 3)
           (setq end (1- end)))
         (phps-mode-lexer--begin 'ST_INITIAL)
         (when (phps-mode-lexer--parser-mode)
           (phps-mode-lexer--return-token
            ";"
            start
            end))
         (phps-mode-lexer--return-token
          'T_CLOSE_TAG
          start
          end)))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "'"))
       (let* ((start (match-beginning 0))
              (end (match-end 0))
              (_data (buffer-substring-no-properties start end))
              (un-escaped-end (phps-mode-lexer--get-next-unescaped "'")))
         (if un-escaped-end
             (phps-mode-lexer--return-token
              'T_CONSTANT_ENCAPSED_STRING
              start
              un-escaped-end)
           ;; Unclosed single quotes
           (phps-mode-lexer--return-token-with-val
            'T_ENCAPSED_AND_WHITESPACE
            start
            (point-max))
           (phps-mode-lexer--move-forward
            (point-max)))))

      ;; Double quoted string
      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "\""))
       (let* ((start (match-beginning 0))
              (end (match-end 0))
              (_data (buffer-substring-no-properties start end))
              (open-quote t))

         ;; Move forward from the double-quote one character
         (forward-char)

         (while open-quote
           (let ((string-start
                  (search-forward-regexp
                   (concat
                    "\\(\""
                    "\\|\\$" phps-mode-lexer--label
                    "\\|\\${" phps-mode-lexer--label
                    "\\|{\\$" phps-mode-lexer--label "\\)")
                   nil t)))

             ;; Do we find a ending double quote or starting variable?
             (if string-start
                 (let ((string-start (match-beginning 0))
                       (is-escaped nil)
                       (is-escaped-1 nil)
                       (is-escaped-2 nil))

                   ;; Check whether one character back is escape character
                   (goto-char (1- string-start))
                   (setq is-escaped-1 (looking-at-p "\\\\"))

                   ;; Check whether two characters back is escape character
                   (goto-char (- string-start 2))
                   (setq is-escaped-2 (looking-at-p "\\\\"))

                   (setq is-escaped
                         (and
                          is-escaped-1
                          (not is-escaped-2)))

                   ;; Do we find variable inside quote?
                   (goto-char string-start)

                   ;; Process character if it's not escaped
                   (if is-escaped
                       (forward-char 1)
                     (setq open-quote nil)
                     (if (looking-at "\"")
                         (let ((_double-quoted-string
                                (buffer-substring-no-properties start (+ string-start 1))))
                           ;; (message "Double quoted string: %s" _double-quoted-string)
                           (phps-mode-lexer--return-token-with-val
                            'T_CONSTANT_ENCAPSED_STRING
                            start
                            (+ string-start 1)))
                       ;; (message "Found variable after '%s' at %s-%s" (buffer-substring-no-properties start string-start) start string-start)
                       (phps-mode-lexer--begin 'ST_DOUBLE_QUOTES)
                       (phps-mode-lexer--return-token "\"" start (1+ start)))))
               (progn
                 (setq open-quote nil)
                 (signal
                  'phps-lexer-error
                  (list
                   (format "Found no ending of quote at %s" start)
                   start))))))))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "<<<"
              phps-mode-lexer--tabs-and-spaces
              "\\("
              phps-mode-lexer--label
              "\\|'"
              phps-mode-lexer--label
              "'\\|\""
              phps-mode-lexer--label
              "\"\\)"
              phps-mode-lexer--newline)))
       (let* ((start (match-beginning 0))
              (end (match-end 0))
              (data
               (buffer-substring-no-properties
                (match-beginning 1)
                (match-end 1))))

         ;; Determine if it's HEREDOC or NOWDOC and extract label here
         (if (string= (substring data 0 1) "'")
             (progn
               (setq
                phps-mode-lexer--heredoc-label
                (substring data 1 (- (length data) 1)))
               (phps-mode-lexer--begin 'ST_NOWDOC))
           (progn
             (if (string= (substring data 0 1) "\"")
                 (setq
                  phps-mode-lexer--heredoc-label
                  (substring data 1 (- (length data) 1)))
               (setq
                phps-mode-lexer--heredoc-label
                data))
             (phps-mode-lexer--begin 'ST_HEREDOC)))

         ;; Check for ending label on the next line
         (when (string=
                (buffer-substring-no-properties
                 end
                 (+ end
                    (length
                     phps-mode-lexer--heredoc-label)))
                phps-mode-lexer--heredoc-label)
           (phps-mode-lexer--begin 'ST_END_HEREDOC))

         (push
          `(,phps-mode-lexer--heredoc-label ,start ,end)
          phps-mode-lexer--heredoc-label-stack)
         ;; (message "Found heredoc or nowdoc at %s with label %s" data phps-mode-lexer--heredoc-label)

         (phps-mode-parser-grammar-macro-CG
          'doc_comment
          t)
         (phps-mode-lexer--return-token
          'T_START_HEREDOC
          start
          end)))

      (phps-mode-lexer--match-macro
       (and ST_IN_SCRIPTING (looking-at "[`]"))
       ;; (message "Begun backquote at %s-%s" (match-beginning 0) (match-end 0))
       (phps-mode-lexer--begin 'ST_BACKQUOTE)
       (phps-mode-lexer--return-token "`"))

      (phps-mode-lexer--match-macro
       (and ST_END_HEREDOC
            (looking-at
             (concat phps-mode-lexer--any-char)))
       (let* ((start (match-beginning 0))
              (end (+ start
                      (length
                       phps-mode-lexer--heredoc-label)
                      1))
              (_data (buffer-substring-no-properties start end)))
         ;; (message "Found ending heredoc at %s, %s of %s" _data (thing-at-point 'line) phps-mode-lexer--heredoc-label)
         (pop phps-mode-lexer--heredoc-label-stack)
         (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
         (phps-mode-lexer--return-token 'T_END_HEREDOC start end)))

      (phps-mode-lexer--match-macro
       (and (or ST_DOUBLE_QUOTES ST_BACKQUOTE ST_HEREDOC) (looking-at "{\\$"))
       (phps-mode-lexer--yy-push-state 'ST_IN_SCRIPTING)
       (phps-mode-lexer--yyless 1)
       (phps-mode-lexer--enter-nesting "{")
       (phps-mode-lexer--return-token
        'T_CURLY_OPEN
        (match-beginning 0)
        (- (match-end 0) 1)))

      (phps-mode-lexer--match-macro
       (and ST_DOUBLE_QUOTES (looking-at "[\"]"))
       (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
       (phps-mode-lexer--return-token "\""))

      (phps-mode-lexer--match-macro
       (and ST_BACKQUOTE (looking-at "[`]"))
       (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
       (phps-mode-lexer--return-token "`"))

      (phps-mode-lexer--match-macro
       (and ST_DOUBLE_QUOTES
            (looking-at phps-mode-lexer--any-char))
       (let ((start (point))
             (start-error (car (cdr (nth 2 phps-mode-lexer--generated-tokens)))))
         (let ((string-start (search-forward-regexp "[^\\\\]\"" nil t)))
           (if string-start
               (let* ((end (- (match-end 0) 1))
                      (double-quoted-string (buffer-substring-no-properties start end)))
                 ;; Do we find variable inside quote?
                 (if (or (string-match (concat "\\${" phps-mode-lexer--label) double-quoted-string)
                         (string-match (concat "{\\$" phps-mode-lexer--label) double-quoted-string)
                         (string-match (concat "\\$" phps-mode-lexer--label) double-quoted-string))
                     (progn
                       (let ((variable-start (+ start (match-beginning 0))))

                         ;; (message "Found starting expression inside double-quoted string at: %s %s" start variable-start)
                         (phps-mode-lexer--return-token-with-val
                          'T_CONSTANT_ENCAPSED_STRING
                          start
                          variable-start)))
                   (progn
                     (phps-mode-lexer--return-token-with-val
                      'T_CONSTANT_ENCAPSED_STRING
                      start
                      end)
                     ;; (message "Found end of quote at %s-%s, moving ahead after '%s'" start end (buffer-substring-no-properties start end))
                     )))
             (progn
               (signal
                'phps-lexer-error
                (list
                 (format "Found no ending of double quoted region starting at %d" start-error)
                 start-error)))))))

      (phps-mode-lexer--match-macro
       (and ST_BACKQUOTE (looking-at phps-mode-lexer--any-char))
       (let ((start (car (cdr (car phps-mode-lexer--generated-tokens)))))
         (let ((string-start (search-forward-regexp "\\([^\\\\]`\\|\\$\\|{\\)" nil t)))
           (if string-start
               (let ((start (- (match-end 0) 1)))
                 ;; (message "Skipping backquote forward over %s" (buffer-substring-no-properties old-start start))
                 (phps-mode-lexer--return-token-with-val
                  'T_ENCAPSED_AND_WHITESPACE
                  old-start
                  start))
             (progn
               (signal
                'phps-lexer-error
                (list
                 (format "Found no ending of back-quoted string starting at %d" start)
                 start)))))))

      (phps-mode-lexer--match-macro
       (and ST_HEREDOC (looking-at phps-mode-lexer--any-char))
       ;; Check for $, ${ and {$ forward
       (let ((old-start (car (cdr (car phps-mode-lexer--heredoc-label-stack))))
             (old-end (point)))
         (let ((string-start
                (search-forward-regexp
                 (concat
                  "\\(\n"
                  phps-mode-lexer--heredoc-label
                  ";?\n\\|\\$"
                  phps-mode-lexer--label
                  "\\|{\\$"
                  phps-mode-lexer--label
                  "\\|\\${"
                  phps-mode-lexer--label
                  "\\)"
                  )
                 nil
                 t)))
           (if string-start
               (let* ((start (match-beginning 0))
                      (end (match-end 0))
                      (data (buffer-substring-no-properties start end)))

                 (cond

                  ((string-match
                    (concat
                     "\n"
                     phps-mode-lexer--heredoc-label
                     ";?\n"
                     )
                    data)
                   ;; (message "Found heredoc end at %s-%s" start end)
                   (phps-mode-lexer--return-token-with-val
                    'T_ENCAPSED_AND_WHITESPACE
                    old-end
                    start)
                   (phps-mode-lexer--begin
                    'ST_END_HEREDOC))

                  (t
                   ;; (message "Found variable at '%s'.. Skipping forward to %s" data start)
                   (phps-mode-lexer--return-token-with-val
                    'T_ENCAPSED_AND_WHITESPACE
                    old-end
                    start))

                  ))
             (progn
               (signal
                'phps-lexer-error
                (list
                 (format "Found no ending of heredoc starting at %d" old-start)
                 old-start)))))))

      (phps-mode-lexer--match-macro
       (and ST_NOWDOC (looking-at phps-mode-lexer--any-char))
       (let ((start (car (cdr (car phps-mode-lexer--generated-tokens)))))
         (let ((string-start (search-forward-regexp
                              (concat
                               "\n"
                               phps-mode-lexer--heredoc-label
                               ";?\\\n"
                               ) nil t)))
           (if string-start
               (let* ((start (match-beginning 0))
                      (end (match-end 0))
                      (_data (buffer-substring-no-properties start end)))
                 ;; (message "Found something ending at %s" _data)
                 ;; (message "Found nowdoc end at %s-%s" start end)
                 (phps-mode-lexer--return-token-with-val
                  'T_ENCAPSED_AND_WHITESPACE
                  old-start
                  start)
                 (phps-mode-lexer--begin
                  'ST_END_HEREDOC))
             (progn
               (signal
                'phps-lexer-error
                (list
                 (format "Found no ending of nowdoc starting at %d" start)
                 start)))))))

      (phps-mode-lexer--match-macro
       (and (or ST_IN_SCRIPTING ST_VAR_OFFSET)
            (looking-at phps-mode-lexer--any-char))
       (signal
        'phps-lexer-error
        (list
         (format "Unexpected character at %d" (match-beginning 0))
         (match-beginning 0))))

      (if phps-mode-lexer--match-length
          (progn
            (phps-mode-debug-message
             (message
              "Found match %s"
              phps-mode-lexer--match-body))
            (phps-mode-lexer--re2c-execute)

            (when phps-mode-lexer--restart-flag
              (phps-mode-debug-message
               (message "Restarting lexer"))
              (phps-mode-lexer--re2c)))
        (phps-mode-debug-message
         (message "Found nothing at %d" (point)))))))


(provide 'phps-mode-lexer)

;;; phps-mode-lexer.el ends here
