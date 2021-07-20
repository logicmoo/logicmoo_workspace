;;; zenscript-highlighting.el --- Code highlighting for ZenScript -*- lexical-binding: t -*-

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

;; This module of zenscript-mode provides code highlighting
;; with font-lock.

;;; Code:

(require 'zenscript-common)

(defface zenscript-preprocessor-face
  '((t (:inherit font-lock-constant-face)))
  "The face used for preprocessors, like `#debug`, `#loader` etc."
  :group 'zenscript)

(defface zenscript-bracket-prefix-face
  '((t (:inherit font-lock-constant-face)))
  "The face used for the first token in bracket handlers."
  :group 'zenscript)

(defconst zenscript-mode-font-lock-keywords
  `(;; preprocessors
    (,(concat "\\(#\\)\\(" (regexp-opt zenscript-preprocessors) "\\)")
     (1 font-lock-keyword-face t)
     (2 'zenscript-preprocessor-face t)
     ("\\w+" () () (0 font-lock-variable-name-face)))
    ;; declarations
    (,(concat (zenscript--word-from zenscript-var-keywords) "\\s-+" "\\(\\w+\\)")
     (1 font-lock-variable-name-face))
    (,(concat (zenscript--word-from zenscript-function-keywords) "\\s-+" "\\(\\w+\\)")
     (1 font-lock-function-name-face))
    (,(concat (zenscript--word-from zenscript-class-keywords) "\\s-+" "\\(\\w+\\)")
     (1 font-lock-type-face))
    ;; primitive types and string
    (,(zenscript--word-from zenscript-storage-keywords)
     (0 font-lock-type-face))
    ;; constants
    (,(zenscript--word-from zenscript-constants)
     (0 font-lock-constant-face))
    ;; other keywords
    ,(zenscript--word-from zenscript-all-keywords)
    ;; imports
    ("\\bimport\\b"
     ("\\w+" () ()
      (0 font-lock-type-face)))
    ;; as/instanceof
    ("\\b\\(?:as\\|instanceof\\)\\s-+\\(\\w+\\)"
     (1 font-lock-type-face))
    ;; function invocation
    ("\\(\\w+\\)(" (1 font-lock-function-name-face))
    ;; numbers
    (,(concat
       ;; integers
       "\\b\\(\\(0\\(x\\|X\\)[0-9a-fA-F]*\\)"
       "\\|"
       ;; floats
       "\\(\\([0-9]+\\.?[0-9]*\\)\\|\\(\\.[0-9]+\\)\\)"
       "\\(\\(e\\|E\\)\\(\\+\\|-\\)?[0-9]+\\)?\\)"
       "\\([LlFfUuDd]\\|UL\\|ul\\)?\\b")
     (0 font-lock-constant-face))
    ;; bracket handlers
    ("<\\(\\w+\\).+?>"
     (1 'zenscript-bracket-prefix-face))
    ;; operators
    ,(regexp-opt zenscript-punctuation))
  "The list of keywords to use with `font-lock-keywords`.")

(defun zenscript--init-highlighting ()
  "Initialize hooks and locals required by `zenscript-highlighting`."
  (setq font-lock-defaults '(zenscript-mode-font-lock-keywords)))

(provide 'zenscript-highlighting)
;;; zenscript-highlighting.el ends here
