;;; phps-mode-automation --- Generate a Wisent Parser file -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;;; Uses a parser to convert LALR Yacc grammar to Wisent grammar

;;; AST should be like this: (root (block (rule (logic))))


;;; Code:

(require 'phps-mode-automation-grammar)

(when (fboundp 'parser-generator-lr-export-to-elisp)

  (let ((php-yacc-url "https://raw.githubusercontent.com/php/php-src/php-8.0.0/Zend/zend_language_parser.y")
        (php-yacc-file (expand-file-name "zend_language_parser.y")))

    ;; Download Yacc if not available
    (unless (file-exists-p php-yacc-file)
      (message "Downloading PHP 8.0 YACC grammar..")
      (url-copy-file php-yacc-url php-yacc-file t t)
      (message "Download of PHP 8.0 YACC grammar completed"))

    ;; Prepare export
    (when (fboundp 'parser-generator-set-grammar)
      (parser-generator-set-grammar
       `(
         ,phps-mode-automation-grammar-non-terminals
         ,phps-mode-automation-grammar-terminals
         ,phps-mode-automation-grammar-productions
         ,phps-mode-automation-grammar-start
         )
       ))
    (when (fboundp 'parser-generator-set-look-ahead-number)
      (parser-generator-set-look-ahead-number
       phps-mode-automation-grammar-look-ahead-number))
    (when (boundp 'parser-generator--e-identifier)
      (setq
       parser-generator--e-identifier
       phps-mode-automation-grammar-e-identifier))
    (when (boundp 'parser-generator--eof-identifier)
      (setq
       parser-generator--eof-identifier
       phps-mode-automation-grammar-eof-identifier))
    (when (boundp 'parser-generator-lex-analyzer--function)
      (setq
       parser-generator-lex-analyzer--function
       phps-mode-automation-grammar-lex-analyzer-function))
    (when (boundp 'parser-generator-lex-analyzer--get-function)
      (setq
       parser-generator-lex-analyzer--get-function
       phps-mode-automation-grammar-lex-analyzer-get-function))
    (when (boundp 'parser-generator-lr--precedence-attribute)
      (setq
       parser-generator-lr--precedence-attribute
       phps-mode-automation-grammar-precendece-attribute))
    (when (boundp 'parser-generator-lr--precedence-comparison-function)
      (setq
       parser-generator-lr--precedence-comparison-function
       phps-mode-automation-grammar-precedence-comparison-function))
    (when (boundp 'parser-generator--global-declaration)
      (setq
       parser-generator--global-declaration
       phps-mode-automation-grammar-global-declaration))
    (when (boundp 'parser-generator--context-sensitive-attributes)
      (setq
       parser-generator--context-sensitive-attributes
       phps-mode-automation-grammar-context-sensitive-attributes))
    (when (boundp 'parser-generator--global-attributes)
      (setq
       parser-generator--global-attributes
       phps-mode-automation-grammar-global-declaration))
    (when (fboundp 'parser-generator-process-grammar)
      (parser-generator-process-grammar))
    (when (fboundp 'parser-generator-lr-generate-parser-tables)
      (parser-generator-lr-generate-parser-tables))

    ;; Export
    (let ((export (parser-generator-lr-export-to-elisp "phps-mode-parser")))
      (message "export: %s" export))

    (message "Automation completed")))

(provide 'phps-mode-automation)
;;; phps-mode-automation.el ends here
