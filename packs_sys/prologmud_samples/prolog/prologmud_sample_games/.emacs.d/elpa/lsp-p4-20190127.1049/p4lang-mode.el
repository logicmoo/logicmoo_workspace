;;; p4lang-mode.el --- example of a CC Mode derived mode for a new language

;; Author:     2002 Martin Stjernholm
;; Maintainer: Unmaintained
;; Created:    October 2002
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a simple example of a separate mode derived from CC Mode
;; for a hypothetical language called p4lang that
;; is similar to Java.  It's provided as a guide to show how to use CC
;; Mode as the base in a clean way without depending on the internal
;; implementation details.
;;
;; Currently it only shows the bare basics in mode setup and how to
;; use the language constant system to change some of the keywords
;; that are recognized in various situations.

;; Note: The interface used in this file requires CC Mode 5.30 or
;; later.

;;; Code:

(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'p4lang-mode 'c-mode))

;; P4LANG has no boolean but a string and a vector type.
(c-lang-defconst c-primitive-type-kwds
  p4lang (append
          '("action" "apply" "bit" "control" "header" "in" "inout"
            "out" "parser" "state" "string" "transition" "vector")
	     (delete "boolean"
		     ;; Use append to not be destructive on the
		     ;; return value below.
		     (append
		      ;; Due to the fallback to Java, we need not give
		      ;; a language to `c-lang-const'.
		      (c-lang-const c-primitive-type-kwds)
		      nil))))

;; Function declarations begin with "function" in this language.
;; There's currently no special keyword list for that in CC Mode, but
;; treating it as a modifier works fairly well.
(c-lang-defconst c-modifier-kwds
  p4lang (cons "function" (c-lang-const c-modifier-kwds)))

;; No cpp in this language, but there's still a "#pragma" directive to
;; fontify.  (The definitions for the extra keywords above are enough
;; to incorporate them into the fontification regexps for types and
;; keywords, so no additional font-lock patterns are required.)
(c-lang-defconst c-cpp-matchers
  p4lang (cons
      ;; Use the eval form for `font-lock-keywords' to be able to use
      ;; the `c-preprocessor-face-name' variable that maps to a
      ;; suitable face depending on the (X)Emacs version.
      '(eval . (list "^\\s *\\(#pragma\\)\\>\\(.*\\)"
		     (list 1 c-preprocessor-face-name)
		     '(2 font-lock-string-face)))
      ;; There are some other things in `c-cpp-matchers' besides the
      ;; preprocessor support, so include it.
      (c-lang-const c-cpp-matchers)))

(defcustom p4lang-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in P4LANG mode.
Each list item should be a regexp matching a single identifier.")

(defconst p4lang-font-lock-keywords-1 (c-lang-const c-matchers-1 p4lang)
  "Minimal highlighting for P4LANG mode.")

(defconst p4lang-font-lock-keywords-2 (c-lang-const c-matchers-2 p4lang)
  "Fast normal highlighting for P4LANG mode.")

(defconst p4lang-font-lock-keywords-3 (c-lang-const c-matchers-3 p4lang)
  "Accurate normal highlighting for P4LANG mode.")

(defvar p4lang-font-lock-keywords p4lang-font-lock-keywords-3
  "Default expressions to highlight in P4LANG mode.")

(defvar p4lang-mode-syntax-table nil
  "Syntax table used in p4lang-mode buffers.")
(or p4lang-mode-syntax-table
    (setq p4lang-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table p4lang))))

(defvar p4lang-mode-abbrev-table nil
  "Abbreviation table used in p4lang-mode buffers.")
(c-define-abbrev-table 'p4lang-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar p4lang-mode-map (let ((map (c-make-inherited-keymap)))
		      ;; Add bindings which are only useful for P4LANG
		      map)
  "Keymap used in p4lang-mode buffers.")

(easy-menu-define p4lang-menu p4lang-mode-map "P4LANG Mode Commands"
		  ;; Can use `p4lang' as the language for `c-mode-menu'
		  ;; since its definition covers any language.  In
		  ;; this case the language is used to adapt to the
		  ;; nonexistence of a cpp pass and thus removing some
		  ;; irrelevant menu alternatives.
		  (cons "P4LANG" (c-lang-const c-mode-menu p4lang)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.p4\\'" . p4lang-mode))

;;;###autoload
(defun p4lang-mode ()
  "Major mode for editing P4 code.
This is a simple example of a separate mode derived from CC Mode to
support a language with syntax similar to C/C++/ObjC/Java/IDL/Pike.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `p4lang-mode-hook'.

Key bindings:
\\{p4lang-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table p4lang-mode-syntax-table)
  (setq major-mode 'p4lang-mode
	mode-name "p4lang"
	local-abbrev-table p4lang-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars p4lang-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'p4lang-mode)
  (easy-menu-add p4lang-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'p4lang-mode-hook)
  (c-update-modeline))


(provide 'p4lang-mode)

;;; p4lang-mode.el ends here
