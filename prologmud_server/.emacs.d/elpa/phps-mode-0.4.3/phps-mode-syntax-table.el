;;; phps-mode-syntax-table.el --- Syntax table for PHPs -*- lexical-binding: t -*-

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

;; Please see README.md from the same repository for extended documentation.


;;; Code:

(defvar phps-mode-syntax-table
  (let ((phps-mode-syntax-table (make-syntax-table)))

    ;; This is added so entity names with underscores can be more easily parsed as one word


    ;; WHITE-SPACE
    
    ;; Treat spaces as white-space
    (modify-syntax-entry ?\s  " " phps-mode-syntax-table)

    ;; Treat line-feed as white-space
    (modify-syntax-entry ?\f " " phps-mode-syntax-table)

    ;; Treat spce as white-space
    (modify-syntax-entry ?\s " " phps-mode-syntax-table)

    ;; Treat tabs as white-space
    (modify-syntax-entry ?\t " " phps-mode-syntax-table)

    ;; Treat newline as white-space
    (modify-syntax-entry ?\n " " phps-mode-syntax-table)

    ;; Treat carriage-return as white-space
    (modify-syntax-entry ?\r " " phps-mode-syntax-table)


    ;; PUNCTUATIONS
    
    ;; Treat underscore, dollar-sign, question-mark as punctuations
    (modify-syntax-entry ?_ "." phps-mode-syntax-table)

    ;; Treat dollar-sign as a punctuation
    (modify-syntax-entry ?\$ "." phps-mode-syntax-table)

    ;; Treat question-mark as a punctuation
    (modify-syntax-entry ?\? "." phps-mode-syntax-table)

    ;; Treat backslash as a punctuation
    (modify-syntax-entry ?\\ "." phps-mode-syntax-table)


    ;; PARENTHESIS

    ;; Treat opening round bracket as open-parenthesis closed by )
    (modify-syntax-entry ?\( "()" phps-mode-syntax-table)

    ;; Treat closing round bracket as close-parenthesis opened by (
    (modify-syntax-entry ?\) ")(" phps-mode-syntax-table)

    ;; Treat opening square bracket as open-parenthesis closed by ]
    (modify-syntax-entry ?\[ "(]" phps-mode-syntax-table)

    ;; Treat closing square bracket as close-parenthesis opened by [
    (modify-syntax-entry ?\] ")[" phps-mode-syntax-table)

    ;; Treat opening curly bracket as open-parenthesis closed by }
    (modify-syntax-entry ?\{ "(}" phps-mode-syntax-table)

    ;; Treat closing curly bracket as close-parenthesis opened by {
    (modify-syntax-entry ?\} "){" phps-mode-syntax-table)


    ;; STRING QUOTE

    ;; Treat double quoted string as string quote
    (modify-syntax-entry ?\" "\"" phps-mode-syntax-table)

    ;; Treat single quoted string as string quote
    (modify-syntax-entry ?' "\"" phps-mode-syntax-table)

    ;; Treat back-quoted string as string quote
    (modify-syntax-entry ?` "\"" phps-mode-syntax-table)


    ;; GENERIC COMMENT FENCE

    ;; Double slash starts comment type b
    (modify-syntax-entry ?/ ". 124b" phps-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" phps-mode-syntax-table)

    ;; Newline end comment type b
    (modify-syntax-entry ?\n "> b" phps-mode-syntax-table)
    (modify-syntax-entry ?\r "> b" phps-mode-syntax-table)


    phps-mode-syntax-table)
  "Syntax table for phps-mode.")

(provide 'phps-mode-syntax-table)

;;; phps-mode-syntax-table.el ends here
