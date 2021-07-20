;;; org-if-reader.el --- S-Expression Reader for org-if -*- lexical-binding: t -*-

;; Copyright © 2015 Philip Woods

;; Author: Philip Woods <elzairthesorcerer@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file parses s-expressions of the org-if language.
;; I used a custom reader for better internationalization.

;; Users can now do decimal-based arithmetic using the glyphs with
;; which they are most comfortable (example: ٠-߉ instead of 0-9).

;; Furthermore, they can also represent strings using any glyphs in the
;; Pi & Pf categories (example: “this is a valid string”).

;;; Code:

(require 'cl-lib)

(defvar *org-if-read-funcs*
  '()
  "A-List of functions for `org-if-read-token' to use.")
(defvar *org-if-left-paren-token*
  (list '*left-paren-token*)
  "Token for '('.")
(defvar *org-if-right-paren-token*
  (list '*right-paren-token*)
  "Token for ')'.")
(defvar *org-if-null-token*
  (list '*null-token*)
  "Token for nil.")

(defmacro org-if-add-syntax (fn1 fn2)
  "Append cons cell of lambda functions FN1 & FN2 to `*org-if-read-funcs*'."
  `(setf *org-if-read-funcs* (cons (cons #',fn1 #',fn2) *org-if-read-funcs*)))

;; Parse Whitespace
(org-if-add-syntax
 (lambda (char)
   (or (eq (get-char-code-property char 'general-category) 'Zs)
       (member char '(?\C-i ?\C-j ?\C-k ?\C-m))))
 (lambda (str)
  (let ((pos 1))
    (while (and (< pos (length str))
                (or (eq (get-char-code-property (aref str pos) 'general-category) 'Zs)
                    (member (aref str pos) '(?\C-i ?\C-j ?\C-k ?\C-m))))
      (cl-incf pos 1))
    (cons *org-if-null-token* (substring str pos)))))

;; Parse Comment
(org-if-add-syntax
 (lambda (char)
   (eq char ?\; ))
 (lambda (str)
   (let ((pos 1))
    (while (and (< pos (length str))
                (not (eq (aref str pos) ?\C-j)))
      (cl-incf pos 1))
    (cons *org-if-null-token* (substring str pos)))))

;; Parse Beginning of Expression
(org-if-add-syntax
 (lambda (char)
   (eq char ?\( ))
 (lambda (str)
  (cons *org-if-left-paren-token*
        (substring str 1))))

;; Parse End of Expression
(org-if-add-syntax
 (lambda (char)
  (eq char ?\) ))
 (lambda (str)
  (cons *org-if-right-paren-token*
        (substring str 1))))

;; Parse String
(org-if-add-syntax
 (lambda (char)
    (or (eq char ?\")
        (eq (get-char-code-property char 'general-category) 'Pi)))
 (lambda (str)
  (let ((pos     1)
        (escaped nil)
        (escape-table '(?\\ ?\\ t ?\C-i n ?\C-j v ?\C-k r ?\C-m ?\" ?\"
                            ?« ?« ?‘ ?‘ ?‛ ?‛ ?“ ?“ ?‟ ?‟ ?‹ ?‹
                            ?⸂ ?⸂ ?⸄ ?⸄ ?⸉ ?⸉ ?⸌ ?⸌ ?⸜ ?⸜ ?⸠ ?⸠
                            ?» ?» ?’ ?’ ?” ?” ?› ?› ?⸃ ?⸃ ?⸅ ?⸅
                            ?⸍ ?⸍ ?⸝ ?⸝ ?⸡ ?⸡))
        (chars  '()))
    (while (not (and (not escaped)
                     (or (eq (aref str pos) ?\")
                         (eq (get-char-code-property (aref str pos)
                                                     'general-category)
                             'Pf))))
      (when (>= pos (length str))
        (error "Invalid string: %s" str))
      (let ((next-char (aref str pos)))
        (cond (escaped
               (setf chars
                     (cons (plist-get escape-table next-char)
                           chars))
               (setf escaped nil))
              ((and (not escaped)
                    (eq next-char ?\\))
               (setf escaped t))
              (t (setf chars (cons next-char chars)))))
      (cl-incf pos 1))
    (cl-incf pos 1)
    (cons (apply #'string (reverse chars))
          (substring str pos)))))

;; Parse Symbol
(org-if-add-syntax
 (lambda (char)
    (and (member (get-char-code-property char 'general-category)
                 '(Ll Lm Lo Lt Lu Mc Me Mn Nl No Pc Pd Po Sc Sk Sm So))
         (not (member char '(?¯ ?\" ?\;)))))
 (lambda (str)
  (cl-labels ((char-symbol-p (char)
                             (and (member (get-char-code-property char
                                                                  'general-category)
                                          '(Ll Lm Lo Lt Lu Mc Me Mn Nd Nl No
                                               Pc Pd Sc Sk Sm So))
                                  (not (member char '(?\" ?\;))))))
    (let ((pos 1))
      (while (and (< pos (length str))
                  (char-symbol-p (aref str pos)))
        (cl-incf pos 1))
      (cons (intern (substring str 0 pos))
            (substring str pos))))))

;; Parse Number
(org-if-add-syntax
 (lambda (char)
  (or (eq (get-char-code-property char 'general-category) 'Nd)
      (eq char ?¯)))
 (lambda (str)
  (let ((pos      0)
        (negp     nil)
        (integers '())
        (decimals '())
        (exp      1)
        (num      0))
    ; Handle negative numbers
    (when (eq (aref str 0) ?¯)
      (setf negp t)
      (cl-incf pos 1))
    ; Parse integer part
    (while (and (< pos (length str))
                (eq (get-char-code-property (aref str pos)
                                            'general-category)
                    'Nd))
      (setf integers (cons (get-char-code-property (aref str pos)
                                                   'decimal-digit-value)
                           integers))
      (cl-incf pos 1))
    ; Parse decimal part (if present)
    (when (and (< pos (length str))
               (eq (aref str pos) ?.))
      (cl-incf pos 1)
      (while (and (< pos (length str))
                  (eq (get-char-code-property (aref str pos)
                                              'general-category)
                      'Nd))
        (setf decimals (cons (get-char-code-property (aref str pos)
                                                     'decimal-digit-value)
                             decimals))
        (cl-incf pos 1)))
    ; Add integer part to num
    (dolist (digit integers num)
      (setf num (+ num (* digit exp)))
      (setf exp (* exp 10)))
    ; Add decimal part, if present, to num
    (setf decimals (reverse decimals))
    (setf exp .1)
    (dolist (digit decimals num)
      (setf num (+ num (* digit exp)))
      (setf exp (/ exp 10)))
    ; Make sure number has correct sign
    (when negp
      (setf num (- num)))
    (cons num (substring str pos)))))

(defun org-if-read-token (str)
  "Read a token from the beginning of input string STR.
Return cons cell containing parsed representation of token and
index of next character to read."
  (cl-labels ((rt-help (syntax-funcs)
                       (when (null syntax-funcs)
                         (error "Invalid token: %s" (aref str 0)))
                       (if (funcall (caar syntax-funcs) (aref str 0))
                           (funcall (cdar syntax-funcs) str)
                         (rt-help (cdr syntax-funcs)))))
    (rt-help *org-if-read-funcs*)))

(defun org-if-read-list (str list-so-far)
  "Read list from input string STR into LIST-SO-FAR."
  (let ((token (org-if-read-token str)))
    (cond ((eq (car token) *org-if-right-paren-token*)
           (cons (reverse list-so-far) (cdr token)))
          ((eq (car token) *org-if-left-paren-token*)
           (let ((sublist (org-if-read-list (cdr token) '())))
             (org-if-read-list (cdr sublist) (cons (car sublist) list-so-far))))
          (t (org-if-read-list (cdr token)
                               (if (eq (car token) *org-if-null-token*)
                                   list-so-far
                                 (cons (car token) list-so-far)))))))

(defun org-if-read (str)
  "Parse input string STR into abstract syntax tree."
  (let ((next-token (org-if-read-token str)))
    (if (eq (car next-token) *org-if-left-paren-token*)
        (org-if-read-list (cdr next-token) '())
      next-token)))

(provide 'org-if-reader)
;;; org-if-reader.el ends here
