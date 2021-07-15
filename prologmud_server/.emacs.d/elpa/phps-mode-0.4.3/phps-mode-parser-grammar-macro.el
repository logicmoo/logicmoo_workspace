;;; phps-mode-parser-grammar-macro.el --- Potential grammar macros for parser  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


(require 'semantic/wisent/grammar)

(defvar phps-mode-parser-grammar-macro-CG-data
  (make-hash-table :test 'equal)
  "A hash-table with all settings.")

(defun phps-mode-parser-grammar-macro-CG (subject &optional value)
  "Return and optionally set VALUE of SUBJECT."
  (if value
      (puthash subject value phps-mode-parser-grammar-macro-CG-data)
    (gethash subject phps-mode-parser-grammar-macro-CG-data)))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE (symbol value &rest attributes)
  "Create AST SYMBOL with VALUE."
  `(wisent-raw-tag
    (semantic-tag ,symbol ,value ,@attributes)))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_ASSIGN_OP (symbol subject object)
  "Create AST assignment."
  `(wisent-raw-tag
    (semantic-tag ,symbol 'ZEND_AST_ASSIGN_OP :object ,object :subject ,subject)))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_BINARY_OP (symbol subject object)
  "Create AST assignment."
  `(wisent-raw-tag
    (semantic-tag ,symbol 'ZEND_AST_BINARY_OP :object ,object :subject ,subject)))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_CAST (symbol subject)
  "Create cast."
  `(wisent-raw-tag
    (semantic-tag ,symbol 'ZEND_AST_CAST :subject ,subject)))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_EX (symbol operator &optional subject subject2)
  "Create stuff."
  (let ((attributes `(:operator ,operator)))
    (when subject
      (plist-put attributes :subject subject))
    (when subject2
      (plist-put attributes :subject2 subject2))
    `(wisent-raw-tag
      (semantic-tag ,symbol 'ZEND_AST_EX ,@attributes))))

(defun phps-mode-parser-grammar-macro-ZEND_AST_LIST_RTRIM (subject)
  `(wisent-raw-tag
    (semantic-tag ,subject 'ZEND_AST_LIST_RTRIM)))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_LIST (size type &rest _elements)
  `(wisent-raw-tag
    (semantic-tag "" ,type :elements @elements :size ,size)))

(defun phps-mode-parser-grammar-macro-ZEND_AST_LIST_ADD (array new-element)
  `(wisent-raw-tag
    (semantic-tag ,array 'ZEND_AST_LIST_ADD :new-element ,new-element)))

(defun phps-mode-parser-grammar-macro-ZEND_NEGATE_NUM_STRING (num-string)
  `(* (string-to-number ,num-string) -1))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_CLASS_CONST_OR_NAME (object member)
  `(wisent-raw-tag
    (semantic-tag ,object 'ZEND_AST_CLASS_CONST_OR_NAME :member ,member)))

(defun phps-mode-parser-grammar-macro-ZVAL_INTERNED_STR (_callback string)
  "Convert string to symbol."
  `(let ((symbol ',string))
     (funcall callback symbol)))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_ZVAL_EX (subject symbol)
  `(wisent-raw-tag
    (semantic-tag ,subject ,symbol)))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_ZVAL (subject)
  `(wisent-raw-tag
    (semantic-tag ,subject 'ZEND_AST_ZVAL :attr 0)))

(defun phps-mode-parser-grammar-macro-ZEND_LEX_TSTRING (callback)
  (let ((token (semantic-lex (point) (point-max) nil 1)))
    (funcall callback token)))

(provide 'phps-mode-parser-grammar-macro)
;;; phps-mode-parser-grammar-macro.el ends here
