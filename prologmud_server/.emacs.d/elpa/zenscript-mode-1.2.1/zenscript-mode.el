;;; zenscript-mode.el --- Major mode for ZenScript -*- lexical-binding: t -*-

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

;; Version: 1.2.1
;; URL: https://github.com/eutropius225/zenscript-mode
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Major mode for ZenScript.

;;; Code:

(require 'json)
(require 'zenscript-common)
(require 'zenscript-highlighting)
(require 'zenscript-completion)
(require 'zenscript-indentation)
(require 'zenscript-language)

(defgroup zenscript nil
  "Major mode for editing ZenScript code."
  :prefix "zenscript-"
  :group 'languages)

(defconst zenscript-mode-version
  "1.2.1"
  "The current version of `zenscript-mode`.")

(defconst zenscript-docs-base-url "https://docs.blamejared.com/"
  "The base URL for the official CraftTweaker docs.")

(defconst zenscript-game-versions '("1.12" "1.14" "1.15" "1.16"))

(defconst zenscript-docs-languages '("de" "en" "es" "fr" "it" "ja" "ko" "pl" "ru" "zh"))

;;;###autoload
(defun zenscript-view-docs ()
  "Open the CraftTweaker docs in your default browser."
  (interactive)
  (let ((ver (completing-read "Game version (default: 1.12): "
                              zenscript-game-versions
                              () () () () "1.12"))
        (loc (completing-read "Select your language (default: en): "
                              zenscript-docs-languages
                              () () () () "en")))
    (browse-url (concat zenscript-docs-base-url ver "/" loc))))

;;;###autoload
(defun zenscript-search-docs ()
  "Search for a string in the CraftTweaker docs in your default browser."
  (interactive)
  (let ((ver (completing-read "Game version (default: 1.12): "
                              zenscript-game-versions
                              () () () () "1.12"))
        (loc (completing-read "Select your language (default: en): "
                              zenscript-docs-languages
                              () () () () "en"))
        (query (read-string "Search: ")))
    (browse-url (concat zenscript-docs-base-url ver "/" loc "/search/?search=" query))))

(defconst zenscript-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-v C-d") #'zenscript-view-docs)
    (define-key keymap (kbd "C-c C-s C-d") #'zenscript-search-docs)
    (define-key keymap (kbd "M-.") #'zenscript-goto-definition-at-point)
    (define-key keymap (kbd "M-f") #'forward-zenscript-identifier)
    (define-key keymap (kbd "M-b") #'backward-zenscript-identifier)
    (define-key keymap (kbd "<M-right>") #'forward-zenscript-identifier)
    (define-key keymap (kbd "<M-left>") #'backward-zenscript-identifier)
    keymap))

;;;###autoload
(define-derived-mode zenscript-mode prog-mode "ZenScript"
  "Major mode for ZenScript."
  (use-local-map zenscript-mode-map)
  (zenscript--init-highlighting)
  (zenscript--init-common)
  (zenscript--init-indents)
  (zenscript--init-completion)
  (zenscript--init-language))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zs\\'" . zenscript-mode))

(provide 'zenscript-mode)
;;; zenscript-mode.el ends here
