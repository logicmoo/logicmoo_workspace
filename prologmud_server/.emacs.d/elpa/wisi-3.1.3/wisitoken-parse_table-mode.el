;; wisitoken-parse_table-mode.el --- For navigating in a parse table as output by wisitoken-bnf-generate. -*- lexical-binding:t -*-
;;
;; Copyright (C) 2017 - 2020  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: parser
;; Version: 1.0
;; package-requires: ((emacs "25.1"))
;; URL: http://www.nongnu.org/ada-mode/wisi/wisi.html
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'xref)

(defun wisitoken-parse_table--xref-backend () 'wisitoken-parse_table)

(cl-defgeneric xref-backend-identifier-completion-table ((_backend (eql wisitoken-parse_table)))
  ;; could complete on nonterms, find productions
  nil)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql wisitoken-parse_table)))
  ;; if we are on one of:
  ;; - ’goto state nnn’ in a state action
  ;; => return nnn state
  ;;
  ;; or
  ;; - foo <= bar baz
  ;; => return nonterminal name at point
  ;;
  ;; - 'reduce n tokens to <nonterminal> <prod_id>'
  ;; => return 'prod_id: name'
  (cond
   ((save-excursion
      (beginning-of-line)
      ;; "go to" for bison output
      (search-forward-regexp "go ?to state \\([0-9]+\\)" (line-end-position) t))
    (match-string 1))

   ((save-excursion
      (beginning-of-line)
      (search-forward-regexp "reduce [0-9]+ tokens to \\([[:alnum:]_]+\\) \\([0-9.]+\\)" (line-end-position) t))
    (concat (match-string 2) ": " (match-string 1)))

   (t
    (thing-at-point 'symbol))))

(cl-defgeneric xref-backend-definitions ((_backend (eql wisitoken-parse_table)) identifier)
  ;; IDENTIFIER is from xref-back-identifier-at-point; a state number or a nonterminal
  (let ((state-p (string-match "\\`[0-9]+\\'" identifier))
	(prod_id-p (string-match "\\`[0-9.]+: " identifier)))
    (save-excursion
      (goto-char (point-min))
      (cond
       (state-p
	(search-forward-regexp (concat "^State " identifier ":$")))

       (prod_id-p
	(search-forward-regexp (concat identifier " <=")))

       (t
	(search-forward-regexp (concat "^[0-9.]+: " identifier " <=")))
       )
      (list (xref-make identifier (xref-make-buffer-location (current-buffer) (match-beginning 0))))
      )))

;;;###autoload
(define-minor-mode wisitoken-parse_table-mode
  "Provides navigation in wisi-generate parse table output."
  nil ":parse_table" nil
  (add-hook 'xref-backend-functions #'wisitoken-parse_table--xref-backend nil t)

  (if wisitoken-parse_table-mode
      (read-only-mode 0)
    (read-only-mode 1)
  ))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.parse_table.*\\'" . wisitoken-parse_table-mode))

(provide 'wisitoken-parse_table-mode)
;; end of file
