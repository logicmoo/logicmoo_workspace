;;; zenscript-indentation.el --- Code indentation for ZenScript -*- lexical-binding: t -*-

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

;; This module of zenscript-mode provides indentation for ZenScript code.

;; The indentation rules are based on those of similar languages,
;; such as JavaScript and Java.

;;; Code:

(require 'zenscript-common)

(defcustom zenscript-indent-level 4
  "Number of spaces for each indentation step in `zensript-mode'."
  :type 'integer
  :safe 'integerp
  :group 'zenscript)

(defcustom zenscript-paren-indent-offset 0
  "Number of additional spaces for indenting expressions in parentheses.
The value must be no less than minus `zenscript-indent-level'."
  :type 'integer
  :safe 'integerp
  :group 'zenscript)

(defcustom zenscript-square-indent-offset 0
  "Number of additional spaces for indenting expressions in square braces.
The value must be no less than minus `zenscript-indent-level'."
  :type 'integer
  :safe 'integerp
  :group 'zenscript)

(defcustom zenscript-curly-indent-offset 0
  "Number of additional spaces for indenting expressions in curly braces.
The value must be no less than minus `zenscript-indent-level'."
  :type 'integer
  :safe 'integerp
  :group 'zenscript)

(defcustom zenscript-expr-indent-offset 0
  "Number of additional spaces for indenting continued expressions.
The value must be no less than minus `zenscript-indent-level'."
  :type 'integer
  :safe 'integerp
  :group 'zenscript)

(defun zenscript--find-newline-backward ()
  "Move backward to the nearest newline that is not in a block comment."
  (let ((continue t)
        (result t))
    (while continue
      (setq continue nil)
      (if (search-backward "\n" nil t)
          (let ((parse (syntax-ppss)))
            ;; We match the end of a // comment but not a newline in a
            ;; block comment.
            (when (nth 4 parse)
              (goto-char (nth 8 parse))
              ;; If we saw a block comment, keep trying.
              (unless (nth 7 parse)
                (setq continue t))))
        (setq result nil)))
    result))

(defconst zenscript--indentation-operator-re
  (concat "[+-*/%|&^?:~<>=!$\\.)]\\([^-+*/]=?\\)\\|"
          (zenscript--word-from zenscript-operator-keywords))
  "Regex matching operators that affect the indentation of continued expressions.")

(defun zenscript--looking-at-operator-p ()
  "Return non-nil if point is on an operator."
  (and (looking-at-p zenscript--indentation-operator-re)
       (not (looking-at-p "<\\w+.+?>"))))

(defun zenscript--continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (if (zenscript--looking-at-operator-p)
        (if (eq (char-after) ?/)
            (prog1
                (not (nth 3 (syntax-ppss (1+ (point)))))
              (forward-char -1))
          (or
           (not (memq (char-after) '(?- ?+)))
           (progn
             (forward-comment (- (point)))
             (not (memq (char-before) '(?, ?\[ ?\())))))
      (and (zenscript--find-newline-backward)
           (progn
             (skip-chars-backward " \t")
             (or (bobp) (backward-char))
             (and (not (bobp))
                  (save-excursion (backward-char) (not (looking-at "[/*]/")))
                  (zenscript--looking-at-operator-p)
                  (and (progn (backward-char)
                              (not (looking-at "+\\+\\|--\\|/[/*]"))))))))))

(defun zenscript--get-indentation (parse-status)
  "Return the indentation for the current line.

PARSE-STATUS is the status returned from `syntax-pps`."
  (save-excursion
    (back-to-indentation)
    (cond (;; inside comment
           (nth 4 parse-status)
           (+ (if (looking-at-p "*")
                  1
                2)
              (progn
                (goto-char (nth 8 parse-status))
                (current-column))))
          ((nth 3 parse-status) 0) ; inside string
          ((nth 1 parse-status)
           ;; A single closing paren/bracket should be indented at the
           ;; same level as the opening statement.
           (let ((same-indent-p (looking-at "[]})]"))
                 (continued-expr-p (zenscript--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                 (progn ; nothing following the opening paren/bracket
                   (skip-syntax-backward " ")
                   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (cond (same-indent-p (current-column))
                         (continued-expr-p (+ (current-column) (* 2 zenscript-indent-level)
                                              zenscript-expr-indent-offset))
                         (t (+ (current-column) zenscript-indent-level
                               (pcase (char-after (nth 1 parse-status))
                                 (?\( zenscript-paren-indent-offset)
                                 (?\[ zenscript-square-indent-offset)
                                 (?\{ zenscript-curly-indent-offset))))))
               ;; If there is something following the opening
               ;; paren/bracket, everything else should be indented at
               ;; the same level.
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               ;; continued expressions should still get an extra indent
               (+ (if continued-expr-p
                      (+ zenscript-indent-level
                         zenscript-expr-indent-offset)
                    0)
                  (current-column)))))
          ((zenscript--continued-expression-p)
           (+ zenscript-indent-level
              zenscript-expr-indent-offset))
          (t 0))))

(defun zenscript-indent-line ()
  "Indent the current line as ZenScript."
  (interactive)
  (let* ((parse-status (save-excursion (syntax-ppss (point-at-bol))))
         (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (if (nth 3 parse-status) ;; in a string
        'noindent
      (indent-line-to (zenscript--get-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

(defun zenscript--init-indents ()
  "Initialize hooks and locals required by `zenscript-indentation`."
  (setq-local electric-indent-chars '(?\( ?\) ?\{ ?\} ?\[ ?\] ?\n ?\;))
  (setq-local indent-line-function #'zenscript-indent-line))

(provide 'zenscript-indentation)
;;; zenscript-indentation.el ends here
