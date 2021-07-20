;;; zenscript-completion.el --- Code completion for ZenScript -*- lexical-binding: t -*-

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

;; This module of zenscript-mode provides code completion
;; based upon knowledge of the language's keywords, and from
;; information provided by zenscript-language.

;;; Code:

(require 'zenscript-common)
(require 'zenscript-language)

(defun zenscript-complete-other (prefix)
  "Complete PREFIX with context at point."
  (all-completions prefix
                   (append zenscript-all-keywords
                           zenscript-constants
                           (zenscript--buffer-vals)
                           (mapcar #'car (zenscript--get-bindings)))))

(defun zenscript-complete-import (prefix)
  "Complete the import PREFIX."
  (all-completions prefix (zenscript--get-importables)))

(defun zenscript--looking-at-import-p ()
  "Return non-nil if the current line is an import statement."
  (save-excursion (back-to-indentation) (looking-at-p "import")))

(defun zenscript-complete-member-access (prefix)
  "Complete the access to the member starting with PREFIX."
  (zenscript--looking-at-member-access-p)
  (all-completions prefix (zenscript--get-members ())))

(defun zenscript--looking-at-backwards-p (regex)
  "Return non-nil if searching REGEX backwards ends at point."
  (= (point)
     (save-excursion
       (or (and (re-search-backward regex (point-min) t)
                (match-end 0))
           0))))

(defun zenscript--looking-at-member-access-p ()
  "Return non-nil if looking at member access."
  (zenscript--looking-at-backwards-p "\\.\\(\\([a-zA-Z_][a-zA-Z_0-9]*\\)?\\)"))

(defun zenscript--complete-preprocessor (prefix)
  "Complete the preprocessor beginning with PREFIX."
  (all-completions prefix (mapcar (lambda (pp) (concat "#" pp))
                                  zenscript-preprocessors)))

(defun zenscript--looking-at-preprocessor-p ()
  "Return non-nil if looking at a preprocessor."
  (zenscript--looking-at-backwards-p "#\\w*"))

(defun zenscript-complete-at-point ()
  "Complete the symbol at point."
  (cond ((zenscript--looking-at-import-p)
         (let ((bounds (bounds-of-thing-at-point 'symbol)))
           (list (if bounds (car bounds) (point))
                 (point)
                 (completion-table-dynamic #'zenscript-complete-import))))
        ((zenscript--looking-at-member-access-p)
         (list (match-beginning 1)
               (match-end 1)
               (completion-table-dynamic #'zenscript-complete-member-access)))
        ((zenscript--looking-at-preprocessor-p)
         (list (match-beginning 0)
               (match-end 0)
               (completion-table-dynamic #'zenscript--complete-preprocessor)))
        (t (let ((bounds (bounds-of-thing-at-point 'zenscript-identifier)))
             (list (if bounds (car bounds) (point))
                   (point)
                   (completion-table-dynamic #'zenscript-complete-other))))))

(defun zenscript--init-completion ()
  "Initialize hooks and locals required by `zenscript-completion`."
  (add-hook 'completion-at-point-functions #'zenscript-complete-at-point t t))

(provide 'zenscript-completion)
;;; zenscript-completion.el ends here
