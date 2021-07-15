;;; testsuit.el --- Test Suite for PSGML

;; Copyright (C)  2017 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'psgml)
(require 'psgml-parse)

(defconst psgml-test-cases
  '(
    ("tc01.sgml" (warning "Undefined entity.*"))
    ("tc02.xml")
    ("tc03.xml")
    ("tc04.sgml")
    ("tc05.sgml")
    ("tc07.sgml")
    ("tc08.xml")
    ("tc13.el" (warning "Invalid character"))
    ("tc15.el")
    ("tc16.el")
    ("tc17.html")
    ("tc18.el")
    ("tc19.sgml" (warning "B end-tag implied by B start-tag"))
    ("tc20.sgml" (warning "Start-tag of undefined element FOO"))
    ("tc21.sgml")
    ("tc22.el")
    ("tc23.sgml")
    ))


(defun testsuit-pi-handler (string)
  (when (string-match "ASSERT\\>" string )
    (let ((form (car (read-from-string (substring string (match-end 0))))))
      (cl-assert (eval form) nil
                 "Assertion fail: %S" form))))

  
(defun testsuit-run-test-case (case-description)
  (let* ((file (first case-description))
         (expected (rest case-description))
         (sgml-show-warnings t)
         (warning-expected nil)
         (sgml-pi-function 'testsuit-pi-handler))
    (setq sgml-catalog-assoc nil)       ; To allow testing catalog parsing
    (setq sgml-ecat-assoc nil)
    (message "--Testing %s" file)
    (find-file file)
    (setq sgml-warning-message-flag nil)
    (condition-case errcode
        (progn
          (if (string-match "\\.el\\'" buffer-file-name)
              (progn (eval-buffer))
            (message "current buffer: %s" (current-buffer))
            (sgml-load-doctype)
            ;;(sgml-next-trouble-spot)
            (sgml-parse-until-end-of nil)))
      (error
       (if expected
           (pcase (caar expected)
             (`error (debug)))
         (error "Unexpected %s" errcode))))

    (dolist (test expected)
      (pcase (car test)
        (`warning
         (setq warning-expected t)
         (let ((warning-pattern (cadr test)))
           (with-current-buffer  "*Messages*"
             (goto-char (point-min))
             (or (re-search-forward warning-pattern nil t)
                 (error "No %s warning" warning-pattern)))))
        (`assert
         (or (eval (cadr test))
             (error "Fail: %s" (cadr test))))))
    (when (and sgml-warning-message-flag (not warning-expected))
      (error "Unexpected warnings")) ))


(defun testsuit-run ()
  (interactive)
  (cl-loop for tc in psgml-test-cases
           do (testsuit-run-test-case tc))
  (message "Done"))

;;; testsuit.el ends here
