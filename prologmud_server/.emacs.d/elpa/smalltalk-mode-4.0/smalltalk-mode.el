;;; smalltalk-mode.el --- Major mode for the GNU Smalltalk programming language  -*- lexical-binding:t -*-

;; Author: Steve Byrne
;; Maintainer: Derek Zhou <derek@3qin.us>
;; Version: 4.0
;; Copyright 1988-2021  Free Software Foundation, Inc.

;; This file is part of GNU Smalltalk.

;; GNU Smalltalk is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; GNU Smalltalk is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode to edit GNU Smalltalk code (http://smalltalk.gnu.org/)
;; and interact with an inferior GST interactive session.

;; Provides the usual functionality:
;; - Keyword highlighting.
;; - Automatic indentation.
;; - Syntax-aware navigation.
;; - Code templates.
;; - Interacting with an Smalltalk REPL from within the source code.
;; - Support for prettify-symbols-mode.
;; - defun navigation.

;;; Old History:

;; Incorporates Frank Caggiano's changes for Emacs 19.
;; Updates and changes for Emacs 20 and 21 by David Forster

;;; News

;; New in 4.0:
;; - Completely rewritten indentation code, which now relies on SMIE
;; - Support for `prettify-symbols-mode'
;; - Use `electric-indent-mode'

;;; Code:

(require 'smie nil t)                   ;Not indispensable (yet).

;; ===[ Variables and constants ]=====================================

(defgroup smalltalk-mode ()
  "Custom group for the Smalltalk major mode"
  :group 'languages)

(defvar smalltalk-name-regexp "[[:alpha:]][[:alnum:]_]*"
  "A regular expression that matches a Smalltalk identifier.")

(defvar smalltalk-keyword-regexp (concat smalltalk-name-regexp ":")
  "A regular expression that matches a Smalltalk keyword.")

(defvar smalltalk-name-chars "[:alnum:]_"
  "The collection of character that can compose a Smalltalk identifier.")

(defvar smalltalk-whitespace " \t\n\f")

(defcustom smalltalk-indent-amount 4
  "'Tab size'; used for simple indentation alignment."
  :type 'integer)

(defcustom smalltalk-indent-align-colon nil
  "If non-nil, try and align the `:' of keyword selectors."
  :type 'boolean)

(defcustom smalltalk-use-smie (featurep 'smie)
  "Whether to use SMIE for indentation and navigation.
Requires Emacs≥23.3."
  :type 'boolean)

;;;; ---[ Syntax Table ]------------------------------------------------

;; This may very well be a bug, but certin chars like ?+ are set to be
;; punctuation, when in fact one might think of them as words (that
;; is, they are valid selector names).  Some functions will fail
;; however, (like smalltalk-begin-of-defun) so there punctuation.
;; Works for now...

(defvar smalltalk-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?:  ".   " table) ; Symbol-char
    (modify-syntax-entry ?_  "_   " table) ; Symbol-char
    (modify-syntax-entry ?\" "!   " table) ; Comment (generic)
    (modify-syntax-entry ?'  "\"' " table) ; String
    (modify-syntax-entry ?#  "'   " table) ; Symbol or Array constant
    (modify-syntax-entry ?\( "()  " table) ; Grouping
    (modify-syntax-entry ?\) ")(  " table) ; Grouping
    (modify-syntax-entry ?\[ "(]  " table) ; Block-open
    (modify-syntax-entry ?\] ")[  " table) ; Block-close
    (modify-syntax-entry ?{  "(}  " table) ; Array-open
    (modify-syntax-entry ?}  "){  " table) ; Array-close
    (modify-syntax-entry ?$  "/   " table) ; Character literal
    (modify-syntax-entry ?!  ".   " table) ; End message / Delimit defs
    (modify-syntax-entry ?\; ".   " table) ; Cascade
    (modify-syntax-entry ?|  ".   " table) ; Temporaries
    (modify-syntax-entry ?^  ".   " table) ; Return
    ;; Just to make sure these are not set to "w   "
    (modify-syntax-entry ?<  ".   " table)
    (modify-syntax-entry ?>  ".   " table)
    (modify-syntax-entry ?+  ".   " table) ; math
    (modify-syntax-entry ?-  ".   " table) ; math
    (modify-syntax-entry ?*  ".   " table) ; math
    (modify-syntax-entry ?/  ".   " table) ; math
    (modify-syntax-entry ?=  ".   " table) ; bool/assign
    (modify-syntax-entry ?%  ".   " table) ; valid selector
    (modify-syntax-entry ?&  ".   " table) ; boolean
    (modify-syntax-entry ?\\ ".   " table) ; ???
    (modify-syntax-entry ?~  ".   " table) ; misc. selector
    (modify-syntax-entry ?@  ".   " table) ; Point
    (modify-syntax-entry ?,  ".   " table) ; concat
    table)
  "Syntax table used by Smalltalk mode.")

;;;; ---[ Abbrev table ]------------------------------------------------

(define-abbrev-table 'smalltalk-mode-abbrev-table ()
  "Abbrev table in use in `smalltalk-mode' buffers.")

;;;; ---[ Keymap ]------------------------------------------------------

(defvar smalltalk-template-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "p" 'smalltalk-private-template)
    (define-key keymap "c" 'smalltalk-class-template)
    (define-key keymap "i" 'smalltalk-instance-template)
    keymap)
  "Keymap of template creation keys.")

(defvar smalltalk-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; the following four are deprecated, use C-M- ones
    (unless smalltalk-use-smie
      (define-key keymap "\C-c\C-a"   'smalltalk-begin-of-defun)
      (define-key keymap "\C-c\C-e"   'smalltalk-end-of-defun)
      (define-key keymap "\C-c\C-f"   'smalltalk-forward-sexp)
      (define-key keymap "\C-c\C-b"   'smalltalk-backward-sexp))

    (define-key keymap "\C-c\C-p"   'smalltalk-goto-previous-keyword)
    (define-key keymap "\C-c\C-n"   'smalltalk-goto-next-keyword)
    (define-key keymap "\C-c\C-t"      smalltalk-template-map)
    ;; the following four are NOT deprecated
    (unless smalltalk-use-smie
      (define-key keymap "\C-\M-a"   'smalltalk-begin-of-defun)
      (define-key keymap "\C-\M-e"   'smalltalk-end-of-defun)
      (define-key keymap "\C-\M-f"   'smalltalk-forward-sexp)
      (define-key keymap "\C-\M-b"   'smalltalk-backward-sexp))
    ;; FIXME: Use post-self-insert-hook!
    (define-key keymap "!" 	   'smalltalk-bang)
    ;; `electric-indent-local-mode' was added when we changed
    ;; `electric-indent-mode' to be enabled by default, in which case we'll get
    ;; the same result as `smalltalk-colon' via electric-indent-chars.
    (unless (fboundp 'electric-indent-local-mode)
      (define-key keymap ":"	   'smalltalk-colon))

    ;; -----

    ;; FIXME: The Elisp doc says:
    ;; • The key sequences bound in a major mode keymap should usually start
    ;;   with ‘C-c’, followed by a control character, a digit, or ‘{’, ‘}’,
    ;;   ‘<’, ‘>’, ‘:’ or ‘;’.  The other punctuation characters are
    ;;   reserved for minor modes, and ordinary letters are reserved for
    ;;   users.
    (define-key keymap "\C-cd"     'smalltalk-doit)
    (define-key keymap "\C-cf"     'smalltalk-filein-buffer)
    (define-key keymap "\C-cm"     'gst)
    (define-key keymap "\C-cp"     'smalltalk-print)
    (define-key keymap "\C-cq"     'smalltalk-quit)
    (define-key keymap "\C-cs"     'smalltalk-snapshot)
    
    keymap)
  "Keymap for Smalltalk mode.")

(defconst smalltalk-binsel "[-+*/~,<>=|&?]\\{1,2\\}\\|\\(:=\\)"
  "Smalltalk binary selectors.
Also matches the assignment operator (in submatch 1).")

(defconst smalltalk-font-lock-keywords
  `((,(concat "#" smalltalk-name-regexp) (0 'font-lock-constant-face))
    (,(concat "\\<" smalltalk-name-regexp ":")
     (0 'font-lock-function-name-face))
    ;; FIXME: This should not apply to the < and > of pragmas!
    (,smalltalk-binsel (0 'font-lock-function-name-face))
    ("\\^" (0 'font-lock-builtin-face))
    ("\\$." (0 'font-lock-string-face)) ;; Chars
    ("\\<[[:upper:]][[:alnum:]_]*\\>" (0 'font-lock-type-face)))
  "Basic Smalltalk keywords font-locking.")

(defconst smalltalk-font-lock-keywords-1
  smalltalk-font-lock-keywords
  "Level 1 Smalltalk font-locking keywords.")

(defconst smalltalk-font-lock-keywords-2
  (append smalltalk-font-lock-keywords-1
	  `(("\\<\\(true\\|false\\|nil\\)\\>" . font-lock-constant-face)
	    ("\\<\\(self\\|super\\)\\>" . font-lock-keyword-face)
	    (":[[:lower:]][[:alnum:]_]*" . font-lock-variable-name-face)))
  
  "Level 2 Smalltalk font-locking keywords.")

(defvar smalltalk-last-category ""
  "Category of last method.")

;;;; ---[ Syntax propertize ]-------------------------------------------

(defmacro smalltalk--when-fboundp (sym exp)
  (declare (indent 1) (debug (symbolp form)))
  (if (fboundp sym)
      exp
    ;; `sym' is not defined during compilation, but keep the test at run-time,
    ;; in case we use the compiled file on a newer Emacs.
    `(eval '(if (fboundp ',sym) ,exp))))

(defun smalltalk--pragma-start-p (pos)
  "Return non-nil if the `<' at point starts a pragma."
  ;; I think in practice it's not disastrous if we fail to mark some pragmas,
  ;; whereas if we mistakenly mark a binary `<' as an open-paren, this
  ;; can throw things off pretty badly, so when in doubt presume it's just
  ;; a binary `<'.
  (save-excursion
    (goto-char pos)
    (forward-comment (- (point)))
    (pcase (char-before)
      ((or `nil `?\[) t)
      (`?> (and (< (point) pos)      ;; >< is a binary selector
                (equal (syntax-after (1- (point)))
                       (string-to-syntax ")<")))))))


(defun smalltalk--pragma-end-p (pos)
  "Return non-nil if the `>' at point ends a pragma."
  (save-excursion
    (let ((ppss (syntax-ppss pos)))
      (and (nth 1 ppss)
           (eq ?< (char-after (nth 1 ppss)))))))

(defconst smalltalk--syntax-propertize
  (smalltalk--when-fboundp syntax-propertize-rules
    (syntax-propertize-rules
     ;; $ is marked as escaping because it can escape a ' or a " when
     ;; used for a character literal, but not when used within strings.
     ("\\(\\$\\)[][(){}'\")]"
      (1 (if (nth 8 (syntax-ppss)) (string-to-syntax "."))))
     ;; A '' within a string is an escaped quote and not the end of a string
     ;; and the beginning of another.
     ("''" (0 (if (save-excursion (nth 3 (syntax-ppss (match-beginning 0))))
                  (string-to-syntax "."))))
     ("<" (0 (if (smalltalk--pragma-start-p (match-beginning 0))
                 (string-to-syntax "(>"))))
     (">" (0 (if (smalltalk--pragma-end-p (match-beginning 0))
                 (string-to-syntax ")<"))))
     ;; FIXME: Ugly Hack!  Mark the newline typically placed
     ;; after the method header as a separator in the "gst2-aka-bang" syntax.
     ("![ \t\n]*\n[[:lower:]][[:alnum:]_:. \t]*\\(\n\\)" (1 "."))
     ;; Similarly to the previous one, mark the \n after the `]' closing
     ;; a "scope".
     ;; FIXME: This presumes there is a \n after the `]' and there's no comment
     ;; or other funny business between the two.
     ("][ \t]*\\(\n\\)"
      (1 (save-excursion
           (let ((ppss (syntax-ppss (match-beginning 0))))
             (when (nth 1 ppss)
               (goto-char (nth 1 ppss))
               (unless (smalltalk--smie-exp-p)
                 (string-to-syntax ".")))))))
     )))

(defun smalltalk--goto-defun-start (arg)
  "Move to the beginning of a defun.

If search is successful, return t; point ends up at the beginning
of the line where the search succeeded.  Otherwise, return nil."
  (if (< arg 0)
      (progn
        (while (and
	        (re-search-forward "[[!]" nil 'move)
	        (or (nth 8 (syntax-ppss)) ;False positive within string/comment.
                    (and (eq (char-before) ?\[)
                         ;; Check if it's a defun or a mere block.
                         (save-excursion
                           (forward-char -1)
                           (smalltalk--smie-exp-p)))
                    (< (setq arg (1+ arg)) 0)     ;Skip N times.
                    )))
        (forward-char -1))
    (forward-comment (- (point)))
    (if (eq (char-before) ?!) (forward-char -1))
    (while (and
	    (re-search-backward "[[!]" nil 'move)
	    (or (nth 8 (syntax-ppss))     ;False positive within string/comment.
                (and (eq (char-after) ?\[)
                     (smalltalk--smie-exp-p)) ;Not a defun but a mere block.
                (> (setq arg (1- arg)) 0)     ;Skip N times.
                ))))
  (pcase (char-after)
    (`?\[ (smalltalk--smie-begin-def))
    (`?! (forward-char 1)
         (forward-comment (point-max)))))

(defun smalltalk--goto-defun-end ()
  "Move forward to end of defun at point."
  ;; We can presume that we're at position returned by
  ;; `smalltalk--goto-defun-start'.
  (let ((pos (point)))
    (forward-comment (- (point)))
    (if (eq (char-before) ?!)
        (smie-forward-sexp "!")
      (goto-char pos)
      (while (and (search-forward "[" nil t)
	          (nth 8 (syntax-ppss)))) ;; ignote string/comment
      (backward-char 1)
      (forward-sexp))))

;;;; ---[ SMIE support ]------------------------------------------------

;; FIXME: This is still rough around the edges, but is fairly usable
;; in non-bang-style files.  About as good as the old indentation code now.

(defvar smalltalk--smie-grammar
  ;; The "bang syntax" is described at
  ;; https://www.gnu.org/software/smalltalk/manual/html_node/The-syntax.html
  ;; as:
  ;;
  ;;     methods: ``!'' id [``class''] ``methodsFor:'' string
  ;;              ``!'' [method ``!'']+ ``!''
  ;;
  ;; The basic problem is that I can't find a clear description of the syntax
  ;; of class and method declarations (other than the bang-style above),
  ;; so I'm not sure how to handle those.
  ;; In the mean time, there are several other problems:
  ;; - `|' corresponds to both `let' and `in' in `let <vars> in <exp>' so
  ;;   we need to try and distinguish the two cases
  ;;   (done in smalltalk--smie-|-kind).
  ;; - `<' and `>' are selectors and but are also used for meta info of the
  ;;   form <foo: 'bar'>.
  (when (fboundp 'smie-bnf->prec2)
    (smie-prec2->grammar
     (smie-bnf->prec2
      '((id )                           ;("id")
        (blockbody (id "|" exp)         ;Block with args
                   (exp))               ;Block without args
        (exp ("|-open" id "|" exp)      ;Local var declaration
             ("[" blockbody "]")        ;Block
             ("^" exp)                  ;Return
             (exp "bin-sel" exp)
             (exp "kw-sel" exp)
             (id)
             ("<" id ">")              ;Meta info like `comment' and `category'
             (exp "!" exp)             ;GNU Smalltalk extension
             (id ":=" exp)             ;Assignment
             (id "_" exp)             ;Assignment
             (exp ";" exp)             ;Message cascading
             (exp "\n" exp)            ;Separator for bang method header
             (exp "." exp)))           ;Separate instructions
      '((assoc "!") (assoc "|") (assoc "." "\n") (noassoc ":=" "_" "^")
        (assoc ";") (assoc "kw-sel" "bin-sel"))))))

(defconst smalltalk--smie-id-re
  (concat "\\(:\\)?" smalltalk-name-regexp
          "\\(?:\\." smalltalk-name-regexp "\\)*"
          "\\(:\\)?"))

(defconst smalltalk--smie-symbol-re
  (concat "#\\(?:" smalltalk-binsel
          "\\|\\(?:"
          smalltalk-name-regexp
          "\\(?:\\(?::" smalltalk-name-regexp "\\)*:"
          "\\|\\(?:\\." smalltalk-name-regexp "\\)*"
          "\\)\\)\\)"))

(defconst smalltalk--smie-number-re
  "\\(?:[0-9]+r\\)?-?[0-9][0-9.]*\\(?:[deqs]-?[0-9]+\\)?")

(defun smalltalk--smie-|-kind ()
  ;; FIXME: `|' can also be a binary-selector!
  (if (save-excursion
        (forward-comment (- (point)))
        (memq (char-syntax (preceding-char)) '(?w ?_)))
      "|"
    "|-open"))

(defun smalltalk--smie-forward-token ()
  (forward-comment (point-max))
  (cond
   ((looking-at "\\s(\\|\\s)") "")
   ;; Symbol literals are easy to lex when going forward.
   ((looking-at smalltalk--smie-symbol-re)
    (goto-char (match-end 0)) "lit-symbol")
   ((looking-at smalltalk--smie-number-re)
    (goto-char (match-end 0)) "lit-number")
   ((looking-at smalltalk--smie-id-re)
    (goto-char (match-end 0))
    (cond
     ((match-beginning 2) "kw-sel")
     (t "id")))
   ((looking-at smalltalk-binsel)
    (goto-char (match-end 0))
    (or (match-string 1) "bin-sel"))
   ((looking-at "|")
    (let ((pos (match-end 0)))
      (prog1 (smalltalk--smie-|-kind)
        (goto-char pos))))
   ((looking-at "[;.!^]")
    (goto-char (match-end 0))
    (match-string 0))
   (t (smie-default-forward-token))))

(defun smalltalk--smie-backward-token ()
  (forward-comment (- (point)))
  (cond
   ((looking-back "\\s(\\|\\s)" (1- (point))) "")
   ((and (eq (char-before) ?:)
         (memq (char-syntax (or (char-before (1- (point))) ?\ )) '(?w ?_)))
    (forward-char -1)
    (skip-chars-backward "[:alnum:]_")
    ;; (skip-chars-forward "0-9_.")        ;Maybe we skipped too much!
    (if (and (eq (char-before) ?:)
             (looking-back smalltalk--smie-symbol-re (line-beginning-position)))
        (progn
         (goto-char (match-beginning 0))
         "lit-symbol")
      "kw-sel"))
   ((memq (char-syntax (preceding-char)) '(?w ?_))
    (skip-chars-backward "[:alnum:]_.")
    ;; (skip-chars-forward "0-9_.")        ;Maybe we skipped too much!
    (cond
     ((eq (char-before) ?#)
      (forward-char -1)
      "lit-symbol")
     ((eq (char-before) ?-)
      (forward-char -1)
      "lit-number")
     (t "id")))
   ((looking-back smalltalk-binsel (- (point) 2) t)
    (goto-char (match-beginning 0))
    (if (eq (char-before) ?#)
        (progn
          (forward-char -1)
          "lit-symbol")
      (or (match-string 1) "bin-sel")))
   ((eq ?| (char-before))
    (forward-char -1)
    (smalltalk--smie-|-kind))
   ((memql (char-before) '(?\; ?\. ?^ ?!))
    (forward-char -1)
    (buffer-substring (point) (1+ (point))))
   (t (smie-default-backward-token))))

(defun smalltalk--smie-exp-p ()
  "Return non-nil if the thing at point is allowed to be an /expr/."
  (save-excursion
    (pcase (smalltalk--smie-backward-token)
      ((or `"bin-sel" `"kw-sel" ":=" "_" `"." `"^" `"!" "|")
       t)
      ((or `"|-open" `";" `"lit-symbol" "lit-number") nil)
      ;;`""' means we bumped into a paren or a string.
      (`"" (looking-back "\\s(" (1- (point))))
      (_
       ;; Presumably a plain `id', which is either a reference to a variable,
       ;; or a unary selector.  In either case we can't be just an /expr/.
       nil))))

(defun smalltalk--smie-begin-def ()
  (let ((pos nil))
    (while (member (smalltalk--smie-backward-token)
                   '("bin-sel" "kw-sel" "id"))
      (setq pos (point)))
    (goto-char pos)))

(defun smalltalk--smie-rules (method arg)
  (pcase (cons method arg)
    (`(:elem . basic) smalltalk-indent-amount)
    (`(:after . "|") 0)
    (`(:after . ">") 0)                 ;Indentation after a pragma.
    (`(:after . ,(or ":=" "_")) smalltalk-indent-amount)
    (`(:after . "\n") (if (smie-rule-parent-p "!") ;GST2 method header separator
                          smalltalk-indent-amount))
    (`(:after . ";")
     (save-excursion
       (forward-char 1)
       (let ((parent (smie-backward-sexp 'halfsexp)))
         ;; We do:
         ;;
	 ;;     ^self new file: aFile;
         ;;           fooselector name: aString;
         ;;           yourself
         ;;
         ;; but I wonder if we shouldn't instead try to do:
         ;;
	 ;;     ^self new file: aFile;
         ;;               fooselector name: aString;
         ;;                           yourself
         (pcase (nth 2 parent)
           (`";" nil)
           (_ (forward-sexp 1) (forward-comment 1)
              `(column . ,(current-column)))))))
    (`(:before . "kw-sel")
     (let ((pos (point))
           (kw-len (and (looking-at smalltalk--smie-id-re)
                        (string-width (match-string 0)))))
       (when kw-len
         (save-excursion
           (goto-char (match-end 0))
           (let ((parent (smie-backward-sexp 'halfsexp)))
             (pcase (nth 2 parent)
              (`"kw-sel"
               (goto-char (nth 1 parent))
               (let ((parent-len (and (looking-at smalltalk--smie-id-re)
                                      (string-width (match-string 0)))))
                 (if smalltalk-indent-align-colon
                     (- parent-len kw-len)
                   0)))
              ("!" 0)
              (`"bin-sel" 0)            ;FIXME: Not sure what to do here.
              (_
               (when (< (point) pos)
                 (let ((c (current-column)))
                   `(column . ,(+ c smalltalk-indent-amount)))))))))))
    (`(:before . "[")
     (if (smalltalk--smie-exp-p)
         ;; Just a block.
         (if (smie-rule-hanging-p) (smie-rule-parent))
       ;; We're not a block, so presumably some class/method definition.
       ;; Find the beginning of that definition.
       (save-excursion
         (smalltalk--smie-begin-def)
         `(column . ,(current-column)))))
    ))

;;;; ---[ Prettify-symbols ]--------------------------------------------

(defvar smalltalk-prettify-symbols-alist
  '(("^" . ?↑)
    ("_" . ?←)
    (":=" . ?←)))

;;;; ---[ Interactive functions ]---------------------------------------

;;;###autoload
(define-derived-mode smalltalk-mode prog-mode "Smalltalk"
  "Major mode for editing Smalltalk code.

Commands:
\\{smalltalk-mode-map}"
  (setq local-abbrev-table smalltalk-mode-abbrev-table) ;FIXME: Needed?
  
  ;; Buffer locals

  (set (make-local-variable 'paragraph-start)
       (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate)
       paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'indent-line-function)
       #'smalltalk-indent-line)
  (when (boundp 'electric-indent-chars)
    ;; Instead of `smalltalk-colon'.
    (add-to-list (make-local-variable 'electric-indent-chars) ?\:))

  (when (and smalltalk-use-smie (fboundp 'smie-setup))
    (smie-setup smalltalk--smie-grammar #'smalltalk--smie-rules
                :forward-token  #'smalltalk--smie-forward-token
                :backward-token #'smalltalk--smie-backward-token))

  (set (make-local-variable 'prettify-symbols-alist)
       smalltalk-prettify-symbols-alist)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'comment-start) "\"")
  (set (make-local-variable 'comment-end) "\"")
  ;; (set (make-local-variable 'comment-column) 32)
  (set (make-local-variable 'comment-start-skip) "\"[ \t]*")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\"")
  ;; Doesn't seem useful...?
  (set (make-local-variable 'comment-indent-function)
       #'smalltalk-comment-indent)
  ;; For interactive f-b sexp
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'syntax-propertize-function)
       smalltalk--syntax-propertize)
  (set (make-local-variable 'beginning-of-defun-function)
       #'smalltalk--goto-defun-start)
  (set (make-local-variable 'end-of-defun-function)
       #'smalltalk--goto-defun-end)
  ;; font-locking
  (set (make-local-variable 'font-lock-defaults)
       '((smalltalk-font-lock-keywords
	  smalltalk-font-lock-keywords-1
	  smalltalk-font-lock-keywords-2)
	 nil nil nil nil))

  ;; tags
  (set (make-local-variable 'find-tag-default-function)
       #'smalltalk-find-message))

;; For hideshow.
;; Hideshow does not cope with comment-start and comment-end being the same
;; so I change it to "= ... =" which seems to be the convention in GNU smalltalk
(add-to-list 'hs-special-modes-alist '(smalltalk-mode "\\[" "\\]" "\"=" nil nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.st\\'" . smalltalk-mode))

;; GNU Smalltalk apparently uses files with extension `.star' which use
;; the zip format.
;; Duplicate zip files' setup for those .star files or fall back on
;; archive-mode, which scans file contents to determine type so is
;; safe to use.
;;;###autoload
(add-to-list
 'auto-mode-alist
 (cons "\\.star\\'"
       (catch 'archive-mode
	 (dolist (mode-assoc auto-mode-alist 'archive-mode)
	   (and (string-match (car mode-assoc) "Starfile.zip")
		(functionp (cdr mode-assoc))
		(throw 'archive-mode (cdr mode-assoc)))))))

;;;###autoload
(add-to-list (if (boundp 'inhibit-local-variables-regexps) ;Emacs≥24.1
                 'inhibit-local-variables-regexps
               'inhibit-first-line-modes-regexp)
             "\\.star\\'")


(defun smalltalk-tab ()
  (interactive)
  (let (col)
    ;; round up, with overflow
    (setq col (* (/ (+ (current-column) smalltalk-indent-amount)
		    smalltalk-indent-amount)
		 smalltalk-indent-amount))
    (indent-to-column col)))

(defun smalltalk-bang-begin-of-defun ()
  (let ((parse-sexp-ignore-comments t) here start)
    (setq here (point))
    (while (and (search-backward "!" nil 'to-end)
                (let ((ppss (syntax-ppss)))
                  (when (nth 8 ppss)      ;In string or comment.
                    (goto-char (nth 8 ppss))
                    'keep-searching))))
    (setq start (point))
    (if (looking-at "!")
	(forward-char 1))
    (smalltalk-forward-whitespace)
    ;; check to see if we were already at the start of a method
    ;; in which case, the semantics are to go to the one preceeding
    ;; this one
    (if (and (= here (point))
	     (/= start (point-min)))
	(progn
	  (goto-char start)
	  (smalltalk-backward-whitespace) ;may be at ! "foo" !
	  (if (= (preceding-char) ?!)
	      (backward-char 1))
	  (smalltalk-begin-of-defun)))))  ;and go to the next one

(defun smalltalk-scope-begin-of-defun ()
  (let (here prev (start (smalltalk-current-scope-point)))
    (if (and start (/= (point) start))
	(progn
    (backward-char 1)
    (skip-chars-backward " \t")
    (if (bolp)
	(backward-char 1)
      (end-of-line))
    (setq here (point))
	       (goto-char start)
	       (skip-chars-forward "^[")
	       (forward-char 1)
	       (condition-case nil
		   (while (< (point) here)
		     (if (looking-at "[ \t]*\\[") (setq prev (point)))
		     (forward-sexp 1))
		 (error t))
	       (if prev
		   (progn
		     (goto-char prev)
		     (condition-case nil
			 (progn
			   (forward-sexp 1)
			   (if (and (< (point) here)
				    (= (char-before) ?\]))
			       (progn
				 (skip-syntax-forward " \t")
				 (setq prev (point)))))
		       (error t))
		     (goto-char prev)
		     (beginning-of-line)
		     (skip-chars-forward " \t"))
		 (goto-char start))))))

(defun smalltalk-begin-of-defun ()
  "Skips to the beginning of the current method.
If already at the beginning of a method, skips to the beginning
of the previous one."
  (interactive)
  (if (smalltalk-in-bang-syntax)
      (smalltalk-bang-begin-of-defun)
    (smalltalk-scope-begin-of-defun)))

(defun smalltalk-begin-of-scope ()
  "Skips to the beginning of the current method.
If already at the beginning of a method, skips to the beginning
of the previous one."
  (interactive)
  (let ((start (smalltalk-current-scope-point)))
    (if start (goto-char start))))


(defun smalltalk-forward-sexp (n)
  "Move point left to the next smalltalk expression."
  (interactive "p")
  ;; FIXME: Why not just use `forward-sexp'?
  (cond ((< n 0)
	 (smalltalk-backward-sexp (- n)))
	((null parse-sexp-ignore-comments)
	 (forward-sexp n))
	(t
	 (while (> n 0)
	   (smalltalk-forward-whitespace)
	   (forward-sexp 1)
	   (setq n (1- n))))))

(defun smalltalk-backward-sexp (n)
  "Move point right to the next smalltalk expression."
  (interactive "p")
  (cond ((< n 0)
	 (smalltalk-forward-sexp (- n)))
	((null parse-sexp-ignore-comments)
	 (backward-sexp n))
	(t
	 (while (> n 0)
	   (smalltalk-backward-whitespace)
	   (backward-sexp 1)
	   (setq n (1- n))))))

(define-obsolete-function-alias 'smalltalk-reindent
  #'indent-according-to-mode nil)

(define-obsolete-function-alias 'smalltalk-newline-and-indent
  #'newline-and-indent nil)

(defun smalltalk-colon ()
  "Possibly reindents a line when a colon is typed.
If the colon appears on a keyword that's at the start of the line (ignoring
whitespace, of course), then the previous line is examined to see if there
is a colon on that line, in which case this colon should be aligned with the
left most character of that keyword.  This function is not fooled by nested
expressions."
  (interactive)
  (let (needs-indent state (parse-sexp-ignore-comments t))
    (setq state (syntax-ppss))

    (if (null (nth 8 state))		;we're not in string or comment
	(progn
	  (save-excursion
      	    (skip-chars-backward "A-Za-z0-9_")
	    (if (and (looking-at smalltalk-name-regexp)
		     (not (smalltalk-at-begin-of-defun)))
		(setq needs-indent (smalltalk-white-to-bolp))))
	  (and needs-indent
	       (smalltalk-indent-for-colon))))
    ;; out temporarily
    ;;    (expand-abbrev)			;I don't think this is the "correct"
    ;;					;way to do this...I suspect that
    ;;					;some flavor of "call interactively"
    ;;					;is better.
    (self-insert-command 1)))

(defun smalltalk-bang ()
  "Go to the end of the method definition."
  (interactive)
  (cond ((nth 8 (syntax-ppss))          ;Inside a string or comment.
         (insert "!"))
        ((smalltalk-in-bang-syntax)
         (progn (insert "!")
                (save-excursion
                  (beginning-of-line)
                  (if (looking-at "^[ \t]+!")
                      (delete-horizontal-space)))))
        (t (smalltalk-end-of-defun))))

(defun smalltalk-end-of-defun ()
  (interactive)
  (if (smalltalk-in-bang-syntax)
      (progn (search-forward "!")
	     (forward-char 1)
	     (if (looking-at "[ \t\n]+!")
		 (progn (search-forward 1)
			(forward-char 1))))
    (progn (end-of-line)
	   (smalltalk-begin-of-defun)
	   (skip-chars-forward "^[")
	   (forward-sexp 1)
	   (skip-chars-forward " \t\n"))))

(defun smalltalk-last-category-name ()
  smalltalk-last-category)

(defun smalltalk-insert-indented-line (string)
  (insert (format "%s\n" string))
  (save-excursion
    (backward-char 1)
    (smalltalk-indent-line)))
 
(defun smalltalk-maybe-insert-spacing-line (n)
  (if (not (save-excursion
	     (forward-line (- n))
	     (looking-at "^[ \t]*$")))
      (insert "\n")))

(defun smalltalk-insert-method-body (selector-name category-name)
  (let (insert-at-top)
    (beginning-of-line)
    (smalltalk-forward-whitespace)
    (beginning-of-line)
    (setq insert-at-top (smalltalk-at-begin-of-defun))
    (if (not insert-at-top)
	(progn (smalltalk-end-of-defun)
	       (beginning-of-line)))
    (smalltalk-maybe-insert-spacing-line 1)
    (smalltalk-insert-indented-line (format "%s [" selector-name))
    (save-excursion
      (insert "\n")
      (if (not (equal category-name ""))
	  (smalltalk-insert-indented-line (format "<category: '%s'>" category-name)))
      (smalltalk-insert-indented-line "]")
      (smalltalk-maybe-insert-spacing-line 0))
    (smalltalk-indent-line)
    (end-of-line)))

(defun smalltalk-instance-template-fn (class-name selector-name category-name)
  (setq smalltalk-last-category category-name)
  (smalltalk-exit-class-scope)
  (smalltalk-insert-method-body
   (if (equal class-name (smalltalk-current-class-name))
       selector-name
     (format "%s >> %s" class-name selector-name))
   category-name))

(defun smalltalk-class-template-fn (class-name selector-name category-name)
  (setq smalltalk-last-category category-name)
  (if (and (equal selector-name "")
	   (equal class-name (smalltalk-current-class-name)))
      (progn (smalltalk-insert-method-body (format "    %s class" class-name) "")
	     (setq smalltalk-last-category "instance creation"))
    (smalltalk-insert-method-body
     (if (and (smalltalk-in-class-scope)
	      (equal class-name (smalltalk-current-class-name)))
	 selector-name
       (format "%s class >> %s" class-name selector-name))
     category-name)))

(defun smalltalk-private-template-fn (class-name selector-name)
  (if (smalltalk-in-class-scope)
      (smalltalk-class-template-fn class-name selector-name "private")
    (smalltalk-instance-template-fn class-name selector-name "private")))

(defun smalltalk-maybe-read-class (with-class)
   (if (= with-class 1)
       (smalltalk-current-class-name)
     (read-string "Class: " (smalltalk-current-class-name))))

(defun smalltalk-instance-template (with-class)
  (interactive "p")
  (smalltalk-instance-template-fn
   (smalltalk-maybe-read-class with-class)
   (read-string "Selector: ")
   (read-string "Category: " (smalltalk-last-category-name))))

(defun smalltalk-class-template (with-class)
  (interactive "p")
  (let* ((class-name (smalltalk-maybe-read-class with-class))
	 (selector-name (read-string "Selector: "))
	 (category-name (if (equal selector-name "") ""
			  (read-string "Category: "
				       (smalltalk-last-category-name)))))
  (smalltalk-class-template-fn class-name selector-name category-name)))
   

(defun smalltalk-private-template (with-class)
  (interactive "p")
  (smalltalk-private-template-fn
   (smalltalk-maybe-read-class with-class)
   (read-string "Selector: ")))

;;;; ---[ Non-interactive functions ]-----------------------------------

;; This is used by indent-for-comment
;; to decide how much to indent a comment in Smalltalk code
;; based on its context.
(defun smalltalk-comment-indent ()
  (if (looking-at "^\"")
      0				;Existing comment at bol stays there.
    comment-column))	; except leave at least one space.

(defun smalltalk-indent-line ()
  (smalltalk-indent-to-column
   (save-excursion
     (beginning-of-line)
     (skip-chars-forward " \t")
     (if (and (not (smalltalk-in-comment))
	      (looking-at "[[:alpha:]][[:alnum:]_]*:")
	      (not (smalltalk-at-begin-of-defun)))
	 (smalltalk-indent-for-colon)
       (smalltalk-calculate-indent)))))
 
(defun smalltalk-toplevel-indent (for-scope)
  (condition-case nil
      (save-excursion
	(save-restriction
	  (widen)
	  (end-of-line)
	  (let ((orig (line-beginning-position)))
	    (if for-scope (smalltalk-begin-of-scope) (smalltalk-begin-of-defun))
	    (smalltalk-forward-whitespace)
	    (if (= orig (line-beginning-position))
	        (smalltalk-current-column)
	      (+ smalltalk-indent-amount (smalltalk-current-column))))))
    (error 0)))
     
(defun smalltalk-statement-indent ()
  (let (indent-amount state close
	(parse-sexp-ignore-comments nil))
    (save-excursion
      (save-restriction
	(widen)
	(beginning-of-line)
	(setq close (looking-at "[ \t]*]"))
	(narrow-to-region (point-min) (point)) ;only care about what's before
	(setq state (syntax-ppss))
	(cond ((nth 4 state) ;in a comment
	       (save-excursion
		 (smalltalk-backward-comment)
		 (setq indent-amount
		       (+ (current-column) (if (= (current-column) 0) 0 1)))))
	      ((equal (nth 3 state) ?')	;in a string
	       (setq indent-amount 0))
	      (close ;just before a closing bracket
	       (save-excursion
		 (condition-case nil
		     (progn (widen)
			    (smalltalk-forward-whitespace)
			    (forward-char)
			    (backward-sexp 1)
			    (beginning-of-line)
			    (smalltalk-forward-whitespace)
			    (setq indent-amount (current-column))))))
	      (t
	       (save-excursion
		 (smalltalk-backward-whitespace)
		 (if (or (bobp)
			 (= (preceding-char) ?!))
		     (setq indent-amount 0)))))
	(if (null indent-amount)
	    (progn
	      (smalltalk-narrow-to-method)
	      (beginning-of-line)
	      (setq state (smalltalk-parse-sexp-and-narrow-to-paren))
	      (smalltalk-backward-whitespace)
	      (cond ((bobp)		;must be first statment in block or exp
		     (if (nth 1 state)	;we're in a paren exp
			 (if (looking-at "$")
			     ;; block with no statements, indent by 4
			     (setq indent-amount (+ (smalltalk-current-indent)
						    smalltalk-indent-amount))

			     ;; block with statements, indent to first non-whitespace
			     (setq indent-amount (smalltalk-current-column)))

		       ;; we're top level
		       (setq indent-amount (smalltalk-toplevel-indent nil))))
		    ((smalltalk-at-end-of-statement) ;end of statement or after temps
		     (smalltalk-find-statement-begin)
		     (setq indent-amount (smalltalk-current-column)))
		    ((= (preceding-char) ?:)
		     (beginning-of-line)
		     (smalltalk-forward-whitespace)
		     (setq indent-amount (+ (smalltalk-current-column)
					    smalltalk-indent-amount)))
		    ((= (preceding-char) ?>) ;maybe <primitive: xxx>
		     (save-excursion
		       (beginning-of-line)
		       (if (looking-at "[ \t]*<[ \t]*[[:alpha:]]+:")
			   (setq indent-amount (smalltalk-toplevel-indent nil))))))))
	(or indent-amount
	    (save-excursion
	      (condition-case nil
		  (smalltalk-find-statement-begin)
		  (error (beginning-of-line)))
	      (+ (smalltalk-current-column)
		 smalltalk-indent-amount)))))))

(defun smalltalk-at-end-of-statement ()
  (save-excursion
    (or (= (preceding-char) ?.)
	(and (= (preceding-char) ?|)
	     (progn
	       (backward-char 1)
	       (while (and (not (bobp)) (looking-back "[ \t\na-zA-Z]" nil))
		 (skip-chars-backward " \t\n")
		 (skip-chars-backward "a-zA-Z"))
	       (if (= (preceding-char) ?|)
		   (progn
		     (backward-char 1)
		     (skip-chars-backward " \t\n")))
	       (bobp))))))

(defun smalltalk-calculate-indent ()
    (cond
     ((smalltalk-at-begin-of-scope) (smalltalk-toplevel-indent t))
     ((smalltalk-at-begin-of-defun) (smalltalk-toplevel-indent t))
     (t (smalltalk-statement-indent))))


(defun smalltalk-in-comment ()
  "Return non-nil if the current location is inside a comment."
  (nth 4 (syntax-ppss)))

(defun smalltalk-forward-whitespace ()
  "Skip white space and comments forward, stopping at end of buffer
or non-white space, non-comment character"
  (while (looking-at (concat "[" smalltalk-whitespace "]"))
    (skip-chars-forward smalltalk-whitespace)
    (if (= (following-char) ?\")
	(forward-comment 1))))

;; (defun smalltalk-forward-whitespace ()
;;   "Skip white space and comments forward, stopping at end of buffer
;; or non-white space, non-comment character"
;;   (forward-comment 1)
;;   (if (= (following-char) ?\n)
;;       (forward-char)))

(defun smalltalk-backward-whitespace ()
  "Like forward whitespace only going towards the start of the buffer."
  (while (progn (skip-chars-backward smalltalk-whitespace)
		(= (preceding-char) ?\"))
    (search-backward "\"" nil t 2)))
	
(defun smalltalk-current-column ()
  "Return the current column of the given line, regardless of narrowed buffer."
  (save-restriction
    (widen)
    (current-column)))			;this changed in 18.56

(defun smalltalk-current-indent ()
  "Return the indentation of the given line, regardless of narrowed buffer."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (skip-chars-forward " \t")
      (current-column))))

(defun smalltalk-find-statement-begin ()
  "Leaves the point at the first non-blank, non-comment character of a new
statement.  If begininning of buffer is reached, then the point is left there.
This routine only will return with the point pointing at the first non-blank
on a line; it won't be fooled by multiple statements on a line into stopping
prematurely.  Also, goes to start of method if we started in the method
selector."
  (let (start ch)
    (if (= (preceding-char) ?.)		;if we start at eos
	(backward-char 1))		;we find the begin of THAT stmt
    (while (and (null start) (not (bobp)))
      (smalltalk-backward-whitespace)
      (cond ((= (setq ch (preceding-char)) ?.)
	     (let (saved-point)
	       (setq saved-point (point))
	       (smalltalk-forward-whitespace)
	       (if (smalltalk-white-to-bolp)
		   (setq start (point))
		 (goto-char saved-point)
		 (smalltalk-backward-sexp 1))
	       ))
	    ((= ch ?^)			;HACK -- presuming that when we back
					;up into a return that we're at the
					;start of a statement
	     (backward-char 1)
	     (setq start (point)))
	    ((= ch ?!)
	     (smalltalk-forward-whitespace)
	     (setq start (point)))
	    (t
	     (smalltalk-backward-sexp 1))))
    (if (null start)
      (progn
	(goto-char (point-min))
	(smalltalk-forward-whitespace)
	(setq start (point))))
    start))

(defun smalltalk-match-paren (state)
  "Answer the closest previous open paren.
Actually, skips over any block parameters, and skips over the whitespace
following on the same line."
  (let ((paren-addr (nth 1 state))
	c done)
    (if (not paren-addr)
	()
      (save-excursion
	(goto-char paren-addr)
	(setq c (following-char))
	(cond ((or (eq c ?\() (eq c ?{))
	       (1+ (point)))
	      ((eq c ?\[)
	       (forward-char 1)

	       ;; Now skip over the block parameters, if any
	       (setq done nil)
	       (while (not done)
		 (skip-chars-forward " \t")
		 (setq c (following-char))
		 (cond ((eq c ?:)
			(smalltalk-forward-sexp 1))
		       ((eq c ?|)
			(forward-char 1) ;skip vbar
			(skip-chars-forward " \t")
			(setq done t))	;and leave
		       (t
			(setq done t))))

	       ;; Now skip over the block temporaries, if any
	       (cond ((eq (following-char) ?|)
		      (setq done nil)
		      (forward-char 1))
		     (t
		      (setq done t)))
	       
	       (while (not done)
		 (skip-chars-forward " \t")
		 (setq c (following-char))
		 (cond ((eq c ?|)
			(forward-char 1) ;skip vbar
			(skip-chars-forward " \t")
			(setq done t))	;and leave
		       (t
			(smalltalk-forward-sexp 1))))

	       (point)))))))

(defun smalltalk-parse-sexp-and-narrow-to-paren ()
  "Narrows the region to between point and the closest previous open paren.
Actually, skips over any block parameters, and skips over the whitespace
following on the same line."
  (let*	((parse-sexp-ignore-comments t)
	 (state (syntax-ppss))
	 (start (smalltalk-match-paren state)))
    (if (null start) () (narrow-to-region start (point)))
    state))

(defun smalltalk-at-begin-of-scope ()
  "Return T if at the beginning of a class or namespace definition, otherwise nil."
  (save-excursion
    (end-of-line)
    (if (smalltalk-in-bang-syntax)
	(let ((parse-sexp-ignore-comments t))
	  (and (bolp)
	       (progn (smalltalk-backward-whitespace)
		      (= (preceding-char) ?!))))
      (let ((curr-line-beg (line-beginning-position)))
	(if (smalltalk-begin-of-scope)
	    (= curr-line-beg (line-beginning-position)))))))

(defun smalltalk-at-begin-of-defun ()
  "Return T if at the beginning of a method definition, otherwise nil."
  (save-excursion
    (end-of-line)
    (if (smalltalk-in-bang-syntax)
	(let ((parse-sexp-ignore-comments t))
	  (and (bolp)
	       (progn (smalltalk-backward-whitespace)
		      (= (preceding-char) ?!))))
      (let ((curr-line-beg (line-beginning-position)))
	(if (smalltalk-begin-of-defun)
	    (= curr-line-beg (line-beginning-position)))))))

(defun smalltalk-indent-for-colon ()
  (let (indent-amount c done default-amount
		     (parse-sexp-ignore-comments t))
    ;; we're called only for lines which look like "<whitespace>foo:"
    (save-excursion
      (save-restriction
	(widen)
	(beginning-of-line)
	(smalltalk-end-of-paren)
	(smalltalk-narrow-to-method)
	(smalltalk-parse-sexp-and-narrow-to-paren)
	(narrow-to-region (point-min) (point))
	(smalltalk-backward-whitespace)
	(cond
	 ((bobp)
	  (setq indent-amount (smalltalk-toplevel-indent t)))
	 ((eq (setq c (preceding-char)) ?\;)	; cascade before, treat as stmt continuation
	  (smalltalk-find-statement-begin)
	  (setq indent-amount (+ (smalltalk-current-column)
				 smalltalk-indent-amount)))
	 ((eq c ?.)	; stmt end, indent like it (syntax error here?)
	  (smalltalk-find-statement-begin)
	  (setq indent-amount (smalltalk-current-column)))
	 (t				;could be a winner
	    (smalltalk-find-statement-begin)
	    ;; we know that since we weren't at bobp above after backing
	    ;; up over white space, and we didn't run into a ., we aren't
	    ;; at the beginning of a statement, so the default indentation
	    ;; is one level from statement begin
	    (setq default-amount
		  (+ (smalltalk-current-column) ;just in case
		     smalltalk-indent-amount))
	    ;; might be at the beginning of a method (the selector), decide
	    ;; this here
	    (if (not (looking-at smalltalk-keyword-regexp ))
		;; not a method selector
		(while (and (not done) (not (eobp)))
		  (smalltalk-forward-sexp 1) ;skip over receiver
		  (smalltalk-forward-whitespace)
		  (cond ((eq (following-char) ?\;)
			 (setq done t)
			 (setq indent-amount default-amount))
			((and (null indent-amount) ;pick up only first one
			      (looking-at smalltalk-keyword-regexp))
			 (setq indent-amount (smalltalk-current-column))))))
	    (and (null indent-amount)
		 (setq indent-amount default-amount))))))
    (or indent-amount (smalltalk-current-indent))))

(defun smalltalk-end-of-paren ()
  (let ((prev-point (point)))
	(smalltalk-safe-forward-sexp)
	(while (not (= (point) prev-point))
	  (setq prev-point (point))
	  (smalltalk-safe-forward-sexp))))

(defun smalltalk-indent-to-column (col)
  (if (/= col (smalltalk-current-indent))
      (save-excursion
	(beginning-of-line)
	(delete-horizontal-space)
	(indent-to col)))
  (if (bolp)
      ;;delete horiz space may have moved us to bol instead of staying where
      ;; we were.  this fixes it up.
      (move-to-column col)))

(defun smalltalk-narrow-to-method ()
  "Narrow the buffer to the contents of the method, exclusive of the
method selector and temporaries."
  (let ((end (point))
	(parse-sexp-ignore-comments t)
	handled)
    (save-excursion
      (smalltalk-begin-of-defun)
      (if (looking-at "[[:alpha:]]")	;either unary or keyword msg
	  ;; or maybe an immediate expression...
	  (progn
	    (forward-sexp)
	    (if (= (following-char) ?:) ;keyword selector
		(progn			;parse full keyword selector
		  (backward-sexp 1)	;setup for common code
		  (smalltalk-forward-keyword-selector))
	      ;; else maybe just a unary selector or maybe not
	      ;; see if there's stuff following this guy on the same line
	      (let (here eol-point)
		(setq here (point))
		(end-of-line)
		(setq eol-point (point))
		(goto-char here)
		(smalltalk-forward-whitespace)
		(if (< (point) eol-point) ;if there is, we're not a method
					; (a heuristic guess)
		    (beginning-of-line)
		  (goto-char here)))))	;else we're a unary method (guess)
	;; this must be a binary selector, or a temporary
	(if (= (following-char) ?|)
	    (progn			;could be temporary
	      (end-of-line)
	      (smalltalk-backward-whitespace)
	      (if (= (preceding-char) ?|)
		  (progn
		    (setq handled t)))
	      (beginning-of-line)))
	(if (not handled)
	    (progn
	      (skip-chars-forward (concat "^" smalltalk-whitespace))
	      (smalltalk-forward-whitespace)
	      (skip-chars-forward smalltalk-name-chars)))) ;skip over operand
      (when (not (smalltalk-in-bang-syntax))
        (skip-chars-forward "^[")
	(unless (eobp) (forward-char)))
      (smalltalk-forward-whitespace)

      ;;sbb  6-Sep-93 14:58:54 attempted fix(skip-chars-forward smalltalk-whitespace)
      (if (= (following-char) ?|)	;scan for temporaries
	  (progn
	    (forward-char)		;skip over |
	    (smalltalk-forward-whitespace)
	    (while (and (not (eobp))
			(looking-at "[a-zA-Z_]"))
	      (skip-chars-forward smalltalk-name-chars)
	      (smalltalk-forward-whitespace)
	      )
	    (if (and (= (following-char) ?|) ;only if a matching | as a temp
		     (< (point) end))	;and we're after the temps
		(narrow-to-region (1+ (point)) end))) ;do we limit the buffer
	;; added "and <..." Dec 29 1991 as a test
	(and (< (point) end)
	     (narrow-to-region (point) end))))))

(defun smalltalk-forward-keyword-selector ()
  "Starting on a keyword, this function skips forward over a keyword selector.
It is typically used to skip over the actual selector for a method."
  (let (done)
    (while (not done)
      (if (not (looking-at "[a-zA-Z_]"))
	  (setq done t)
	(skip-chars-forward smalltalk-name-chars)
	(if (= (following-char) ?:)
	    (progn
	      (forward-char)
	      (smalltalk-forward-sexp 1)
	      (smalltalk-forward-whitespace))
	  (setq done t)
	  (backward-sexp 1))))))

(defun smalltalk-white-to-bolp ()
  "Return T if from the current position to beginning of line is whitespace.
Whitespace is defined as spaces, tabs, and comments."
  (let (done is-white line-start-pos)
    (save-excursion
      (save-excursion
	(beginning-of-line)
	(setq line-start-pos (point)))
      (while (not done)
	(and (not (bolp))
	     (skip-chars-backward " \t"))
	(cond ((bolp)
	       (setq done t)
	       (setq is-white t))
	      ((= (char-after (1- (point))) ?\")
	       (backward-sexp)
	       (if (< (point) line-start-pos) ;comment is multi line
		   (setq done t)))
	      (t
	       (setq done t))))
      is-white)))


(defun smalltalk-backward-comment ()
  (search-backward "\"")		;find its start
  (while (= (preceding-char) ?\")	;skip over doubled ones
    (backward-char 1)
    (search-backward "\"")))

(defun smalltalk-current-class ()
  (let ((here (point))
	curr-hit-point curr-hit new-hit-point new-hit)
    (save-excursion
      (if (setq curr-hit-point
		(search-backward-regexp "^![ \t]*\\(\\(\\w+\\.\\)*\\w+\\)[ \t]+" nil t))
	  (setq curr-hit (buffer-substring
			  (match-beginning 1)
			  (match-end 1)))))

    (save-excursion
      (if (setq new-hit-point
		(search-backward-regexp
		 "^[ \t]*\\(\\w+\\)[ \t]+class[ \t]+\\[" nil t))
	  (setq new-hit (buffer-substring
			 (match-beginning 1)
			 (match-end 1)))))
    (if (and new-hit-point
	     (or (not curr-hit-point) (> new-hit-point curr-hit-point))
	     (smalltalk-in-class-scope-of here new-hit-point))
	  (progn (setq curr-hit-point new-hit-point)
		 (setq curr-hit new-hit)))

    (save-excursion
      (if (setq new-hit-point
		(search-backward-regexp
		 "^[ \t]*\\(\\(\\w+\\.\\)*\\w+\\)[ \t]+extend[ \t]+\\[" nil t))
	  (setq new-hit (buffer-substring
			 (match-beginning 1)
			 (match-end 1)))))
    (if (and new-hit-point
	     (or (not curr-hit-point) (> new-hit-point curr-hit-point)))
	  (progn (setq curr-hit-point new-hit-point)
		 (setq curr-hit new-hit)))

    (save-excursion
      (if (setq new-hit-point
		(search-backward-regexp
		 "^[ \t]*\\(\\w+\\.\\)*\\w+[ \t]+\\(variable\\|variableWord\\|variableByte\\)?subclass:[ \t]+#?\\(\\w+\\)" nil t))
	  (setq new-hit (buffer-substring
			 (match-beginning 3)
			 (match-end 3)))))
    (if (and new-hit-point
	     (or (not curr-hit-point) (> new-hit-point curr-hit-point)))
	(progn (setq curr-hit-point new-hit-point)
	       (setq curr-hit new-hit)))
    (cons curr-hit curr-hit-point)))

(defun smalltalk-update-hit-point (current search)
  (save-excursion
    (let ((new-hit-point (funcall search)))
      (if (and new-hit-point
               (or (not current) (> new-hit-point current)))
          new-hit-point
        current))))

(defun smalltalk-current-scope-point ()
  (let ((curr-hit-point (smalltalk-current-class-point)))
    (setq curr-hit-point
	  (smalltalk-update-hit-point curr-hit-point
				      (lambda () (search-backward-regexp "^[ \t]*Eval[ \t]+\\[" nil t))))
    (setq curr-hit-point
	  (smalltalk-update-hit-point curr-hit-point
				      (lambda () (search-backward-regexp "^[ \t]*Namespace[ \t]+current:[ \t]+[A-Za-z0-9_.]+[ \t]+\\[" nil t))))
    curr-hit-point))

(defun smalltalk-current-class-point ()
    (cdr (smalltalk-current-class)))

(defun smalltalk-current-class-name ()
    (car (smalltalk-current-class)))

(defun smalltalk-in-bang-syntax ()
  (let ((curr-hit-point (smalltalk-current-class-point)))
    (and curr-hit-point
	 (save-excursion
	   (goto-char curr-hit-point)
	   (beginning-of-line)
	   (looking-at "!")))))

(defun smalltalk-in-class-scope-of (orig curr-hit-point)
  (save-excursion
    (goto-char curr-hit-point)
    (skip-chars-forward " \t")
    (skip-chars-forward smalltalk-name-chars)
    (skip-chars-forward " \t")
    (and (= (following-char) ?c)
	 ;; check if the class scope ends after the point
	 (condition-case nil
	     (progn (skip-chars-forward "^[")
		    (forward-sexp 1)
		    (> (point) orig))
	   (error t)))))

(defun smalltalk-in-class-scope ()
  (let ((curr-hit-point (smalltalk-current-class-point)))
    (and curr-hit-point
	 (smalltalk-in-class-scope-of (point) curr-hit-point))))

(defun smalltalk-exit-class-scope ()
  (interactive)
  (if (smalltalk-in-class-scope)
      (progn (smalltalk-begin-of-scope)
	     (skip-chars-forward "^[")
	     (smalltalk-end-of-defun))))

(defun smalltalk-find-message ()
  (save-excursion
    (smalltalk-goto-beginning-of-statement)
    (cond
     ((smalltalk-looking-at-unary-send)
      (if (not (smalltalk-has-sender))
          (progn
            (smalltalk-safe-forward-sexp)
            (smalltalk-safe-forward-sexp)
            (smalltalk-find-message))
        (buffer-substring-no-properties (point) (progn (smalltalk-safe-forward-sexp)(point)))))
     ((smalltalk-looking-at-keyword-send)
      (concat (smalltalk-find-beginning-of-keyword-send) (smalltalk-find-end-of-keyword-send))))))
	 
(defun smalltalk-safe-backward-sexp ()
  (let (prev-point)
    (condition-case nil
	(progn
	  (setq prev-point (point))
	  (smalltalk-backward-sexp 1))
      (error (goto-char prev-point)))))

(defun smalltalk-safe-forward-sexp ()
  (let (prev-point)
    (condition-case nil
	(progn
	  (setq prev-point (point))
	  (smalltalk-forward-sexp 1))
      (error (goto-char prev-point)))))

(defun smalltalk-goto-beginning-of-statement ()
  (if (not (looking-back "[ \t\n]" nil nil))
      (smalltalk-safe-backward-sexp)))

(defun smalltalk-has-sender ()
  (save-excursion
    (smalltalk-backward-whitespace)
    (looking-back "[]})A-Za-z0-9']" nil)))

(defun smalltalk-looking-at-binary-send ()
  (looking-at "[^]A-Za-z0-9:_(){}[;.\'\"]+[ \t\n]"))

(defun smalltalk-looking-at-unary-send ()
  (looking-at "[A-Za-z][A-Za-z0-9]*[ \t\n]"))

(defun smalltalk-looking-at-keyword-send ()
  (looking-at "[A-Za-z][A-Za-z0-9_]*:"))

(defun smalltalk-looking-back-keyword-send ()
  (looking-back "[[:alpha:]][[:alnum:]_]*:" nil))

(defun smalltalk-find-end-of-keyword-send ()
  (save-excursion
    (smalltalk-forward-whitespace)
    (if (or (looking-at "[.;]") (= (smalltalk-next-keyword) (point)))
	""
      (progn
	(smalltalk-goto-next-keyword)
	(concat (buffer-substring-no-properties (save-excursion (progn (smalltalk-safe-backward-sexp) (point))) (point))
		(smalltalk-find-end-of-keyword-send))))))

(defun smalltalk-find-beginning-of-keyword-send ()
  (save-excursion
    (let ((begin-of-defun (smalltalk-at-begin-of-defun)))
      (smalltalk-backward-whitespace)
      (if (or (if begin-of-defun
		  (looking-back "[].;]" nil)
		(looking-back "[.;]" nil))
	      (= (smalltalk-previous-keyword) (point)))
	  ""
	(progn
	  (smalltalk-goto-previous-keyword)
	  (concat (smalltalk-find-beginning-of-keyword-send)
		  (buffer-substring-no-properties (point) (progn (smalltalk-safe-forward-sexp)(+ (point) 1)))))))))

(defun smalltalk-goto-previous-keyword ()
  "Go to the previous keyword of the current message send."
  (goto-char (smalltalk-previous-keyword)))

(defun smalltalk-goto-next-keyword ()
  "Go to the next keyword of the current message send."
  (goto-char (smalltalk-next-keyword)))

(defun smalltalk-previous-keyword-1 ()
  (smalltalk-backward-whitespace)
  (if (looking-back "[>[({.^]" nil) ;; not really ok when > is sent in a keyword arg
      nil
    (if (= (point) (save-excursion (smalltalk-safe-backward-sexp) (point)))
	nil
      (progn
	(smalltalk-safe-backward-sexp)
	(if (smalltalk-looking-at-keyword-send)
	    (point)
	  (smalltalk-previous-keyword-1))))))

(defun smalltalk-next-keyword-1 ()
  (smalltalk-forward-whitespace)
  (if (looking-at "[])};.]")
      nil
    (if (= (point) (save-excursion (smalltalk-safe-forward-sexp) (point)))
	nil
      (progn
	(smalltalk-safe-forward-sexp)
        (skip-chars-forward ":")
        (if (smalltalk-looking-back-keyword-send)
            (point)
          (smalltalk-next-keyword-1))))))

(defun smalltalk-previous-keyword ()
  (or (save-excursion (smalltalk-previous-keyword-1)) (point)))

(defun smalltalk-next-keyword ()
  (or (save-excursion (smalltalk-next-keyword-1)) (point)))

(provide 'smalltalk-mode)

;;; smalltalk-mode.el ends here
