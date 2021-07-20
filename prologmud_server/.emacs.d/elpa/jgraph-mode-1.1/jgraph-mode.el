;;; jgraph-mode.el --- Major mode for Jgraph files  -*- lexical-binding:t -*-

;; Copyright (C) 2006, 2011-2012, 2014, 2015  Free Software Foundation, Inc

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 1.1
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: tex, wp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple editing mode for the Jgraph graph plotting tool.
;; You can find more info at http://web.eecs.utk.edu/~plank/plank/jgraph/jgraph.html

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup jgraph-mode ()
  "Major mode for Jgraph files."
  :group 'tools)

(defvar jgraph-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `jgraph-mode'.")

;; (easy-menu-define jgraph-mode-menu jgraph-mode-map
;;   "Menu for `jgraph-mode'."
;;   '("Jgraph"
;;     ["Do some stuff" jgraph-do-bar :active mark-active]
;;     "..."
;;     ["Do some other" jgraph-do-foo]
;;     ("Sub-jgraph"
;;      ["Haha" jgraph-crash])
;;     ...))

(defvar jgraph-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; tokens are made of any non-whitespace chars.
    (map-char-table (lambda (c v) (if (not (memq (car v) '(0 2 3)))
                                 (modify-syntax-entry c "_" st)))
                    st)
    (modify-syntax-entry ?\( "_ 1n" st)
    (modify-syntax-entry ?\) "_ 4n" st)
    (modify-syntax-entry ?\* "_ 23" st)
    st)
  "Syntax table for `jgraph-mode'.")

(defun jgraph-extract-commands ()
  (let ((sections nil)
        (tok-cmds nil))
    (goto-char (point-min))
    (while (and (re-search-forward "^\\.RE" nil t)
                (re-search-forward "^\\.B \\(.*\\)" nil t))
      (let ((section (downcase (match-string 1)))
            (commands nil)
            (end (save-excursion
                   (and (re-search-forward "^\\.RE" nil t)
                        (match-beginning 0)))))
        (setq section (replace-regexp-in-string "simple" "" section))
        (setq section (replace-regexp-in-string "advanced" "" section))
        (setq section (replace-regexp-in-string "editing" "" section))
        (setq section (replace-regexp-in-string "commands" "" section))
        (setq section (replace-regexp-in-string "\\\\" "" section))
        (setq section (replace-regexp-in-string "\\` *" "" section))
        (setq section (replace-regexp-in-string " *\\'" "" section))
        (setq section (replace-regexp-in-string " " "-" section))
        (setq section (intern section))
        (while (re-search-forward "^\\.TP\n\\(?:\\.B \\|\\\\fB\\)\\([^ :\t\n\\]+\\)\\([ {\\fI]*token\\)?" end 'move)
          (let ((cmd (match-string-no-properties 1)))
            (if (match-end 2) (cl-pushnew cmd tok-cmds :test #'equal))
            (cl-pushnew cmd commands :test #'equal)))
        (setq commands (nreverse commands))
        (let ((sec (assoc section sections)))
          (if sec (nconc sec commands)
            (push (cons section commands) sections)))))
    (nreverse (cons (cons 'token tok-cmds) sections))))

(defconst jgraph-commands
  ;; Obtained by running (jgraph-extract-commands) in the jgraph.1 nroff doc.
  '((top-level-description "newgraph" "graph" "copygraph" "newpage" "X" "Y"
                           "bbox" "preamble" "epilogue")
    (graph "xaxis" "yaxis" "newcurve" "curve" "newline" "copycurve" "title"
           "legend" "newstring" "string" "copystring" "border" "noborder"
           "clip" "noclip" "inherit_axes" "x_translate" "y_translate" "X" "Y")
    (axis "linear" "log" "min" "max" "size" "log_base" "hash" "shash" "mhash"
          "precision" "hash_format" "label" "draw_at" "nodraw" "draw"
          "grid_lines" "no_grid_lines" "mgrid_lines" "no_mgrid_lines" "gray"
          "color" "grid_gray" "grid_color" "mgrid_gray" "mgrid_color" "hash_at"
          "mhash_at" "hash_label" "hash_labels" "hash_scale"
          "draw_hash_marks_at" "draw_hash_labels_at" "auto_hash_marks"
          "no_auto_hash_marks" "auto_hash_labels" "no_auto_hash_labels"
          "draw_axis" "no_draw_axis" "draw_axis_label" "no_draw_axis_label"
          "draw_hash_marks" "no_draw_hash_marks" "draw_hash_labels"
          "no_draw_hash_labels")
    (curve "pts" "x_epts" "y_epts" "marktype" "marksize" "mrotate" "gray"
           "color" "fill" "cfill" "pattern" "poly" "nopoly" "pfill" "pcfill"
           "ppattern" "gmarks" "postscript" "eps" "larrows" "rarrows"
           "nolarrows" "norarrows" "larrow" "rarrow" "nolarrow" "norarrow"
           "asize" "afill" "apattern" "linetype" "glines" "linethickness"
           "bezier" "nobezier" "clip" "noclip" "label")
    (label "x" "y" "font" "fontsize" "linesep" "hjl" "hjc" "hjr" "vjt" "vjc"
           "vjb" "rotate" "lgray" "lcolor")
    (legend "on" "off" "linelength" "linebreak" "midspace" "defaults" "left"
            "top" "bottom" "x" "y" "custom")
    (hash-label "at")
    ;; Commands that can be followed by a token.
    (token "apattern" "eps" "postscript" "ppattern" "pattern" "hash_format"
           "epilogue" "preamble")))

(defconst jgraph-file-include-commands
  '("include" "preamble" "epilogue" "postscript" "eps"))

(defvar jgraph-font-lock-keywords
  `((,(concat "\\_<" (regexp-opt jgraph-file-include-commands)
              "[ \t]+\\(\\sw+\\)")
     (1 font-lock-constant-face))
    (,(concat "^" (regexp-opt
                   (cdr (assoc 'top-level-description jgraph-commands)))
              "\\_>")
     . font-lock-function-name-face)
    (,(concat "\\_<"
              (regexp-opt (cons "include"
                                (apply 'append (mapcar 'cdr jgraph-commands))))
              "\\_>")
     . font-lock-keyword-face)
    )
  "Keyword highlighting specification for `jgraph-mode'.")

;; (defvar jgraph-imenu-generic-expression
;;   '(("Vars" "^defvar[ \t]+\\(\\(\\sw\\|\\s_\\)+\\)" 1)
;;     (nil "^function[ \t]+\\(\\(\\sw\\|\\s_\\)+\\)" 1)
;;     ...)
;;   "Regex patterns for the index menu of `jgraph-mode'.")

;; (defvar jgraph-outline-regexp "(\\|;;;+"
;;   "Regexp for `outline-minor-mode' in `jgraph-mode'.")

;; Abbreviations and Skeletons

;; (define-skeleton jgraph-insert-if
;;   "Jgraph mode skeleton for if..then expressions."
;;   nil
;;   "if " _ \n "then " _ \n "else " _ \n "fi" \n)

;; (define-skeleton jgraph-insert-begend
;;   "Jgraph mode skeleton for begin<x>...end<x> expressions."
;;   "Block name: "
;;   "begin<" str ">" \n _ \n "end<" str ">" \n)

;; (define-abbrev-table 'jgraph-mode-abbrev-table
;;   '(("if" "" jgraph-insert-if nil t)
;;     ("cwcc" "call-with-current-continuation" nil nil t)
;;     ("begin" "" jgraph-insert-begend nil t)
;;     ))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jgr\\'" . jgraph-mode))

(defun jgraph--syntax-end-of-string (limit)
  (when (eq t (nth 3 (syntax-ppss)))
    (when (re-search-forward "\\(?:\\=\\|[^\\]\\)\\(\n\\)" limit t)
      (put-text-property (match-beginning 1) (match-end 1)
                         'syntax-table (string-to-syntax "|")))))

(defun jgraph--syntax-propertize (start end)
  (goto-char start)
  (jgraph--syntax-end-of-string end)
  (funcall
   (syntax-propertize-rules
    ("\\s-:\\(\\s-\\)" (1 (prog1 "|" (jgraph--syntax-end-of-string end)))))
   start end))

;;;###autoload
(define-derived-mode jgraph-mode prog-mode "Jgraph"
  "A major mode for editing Jgraph files."
  (set (make-local-variable 'comment-start) "(* ")
  (set (make-local-variable 'comment-start-skip) "\\_<(\\*\\_>[ \t]*")
  (set (make-local-variable 'comment-end) " *)")
  (set (make-local-variable 'comment-start-skip) "[ \t]*\\_<\\*)\\_>")
  (set (make-local-variable 'font-lock-defaults)
       '(jgraph-font-lock-keywords))
  (set (make-local-variable 'syntax-propertize-function)
       #'jgraph--syntax-propertize)
  (set (make-local-variable 'indent-line-function) 'jgraph-indent-line)
  ;; (set (make-local-variable 'imenu-generic-expression)
  ;;      jgraph-imenu-generic-expression)
  ;; (set (make-local-variable 'outline-regexp) jgraph-outline-regexp)
  )

;;; Indentation

(defcustom jgraph-indent-offset 4
  "Basic indentation step size in `jgraph-mode'."
  :type 'integer)

(defun jgraph-indent-line ()
  "Indent current line of Jgraph code."
  (interactive)
  (let* ((savep (point))
	 (indent (or (with-demoted-errors
                       (save-excursion
                         (forward-line 0)
                         (skip-chars-forward " \t")
                         (if (>= (point) savep) (setq savep nil))
                         (jgraph-indent-calculate)))
                     'noindent)))
    (if (not (numberp indent))
        indent
      (setq indent (max indent 0))
      (if savep
          (save-excursion (indent-line-to indent))
        (indent-line-to indent)))))

(defun jgraph-indent-calculate ()
  "Return the column to which the current line should be indented."
  (save-excursion
    (skip-chars-forward " \t")
    (or (when (looking-at (concat (regexp-opt
                                   (cdr (assoc 'top-level-description
                                               jgraph-commands)))
                                  "\\_>"))
          0)
        'noindent)))

;;;; ChangeLog:

;; 2015-02-19  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* jgraph-mode.el (jgraph-mode): Fix strings.
;; 
;; 	Fixes: debbugs:19898
;; 
;; 	(jgraph--syntax-end-of-string, jgraph--syntax-propertize): New
;; 	functions.
;; 
;; 2014-10-15  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/jgraph-mode/jgraph-mode.el: Use lexical-binding.
;; 	(jgraph-extract-commands): Avoid add-to-list on local var.
;; 
;; 2012-11-29  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/jgraph-mode/jgraph-mode.el: Add `Version' header.
;; 
;; 2011-09-23  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	New package jgraph-mode.
;; 


(provide 'jgraph-mode)
;;; jgraph-mode.el ends here
