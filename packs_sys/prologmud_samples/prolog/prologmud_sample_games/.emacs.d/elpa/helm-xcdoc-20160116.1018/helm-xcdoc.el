;;; helm-xcdoc.el --- Search Xcode Document by docsetutil and eww with helm interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Ryo Fujimoto

;; Author: Ryo Fujimoto <fujimisakri@gmail.com>
;; URL: https://github.com/fujimisakari/emacs-helm-xcdoc
;; Package-Version: 20160116.1018
;; Package-Commit: a85612149a6d8e18ab309b3db2d222ce39c42049
;; Version: 0.0.1
;; Package-Requires: ((helm "1.5") (emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `helm-xcdoc.el' will be able to view on a eww by searching in the Xcode Document at helm interface
;;

;; To use this package, add these lines to your init.el or .emacs file:
;;
;;  (require 'helm-xcdoc)
;;  (setq helm-xcdoc-command-path "/Applications/Xcode.app/Contents/Developer/usr/bin/docsetutil")
;;  (setq helm-xcdoc-document-path "~/Library/Developer/Shared/Documentation/DocSets/com.apple.adc.documentation.AppleiOS8.1.iOSLibrary.docset")
;;
;; ----------------------------------------------------------------
;;
;; to search document
;; M-x helm-xcdoc-search
;;
;; to search document with other-window
;; M-x helm-xcdoc-search-other-window
;;

;;; Code:

(require 'helm)
(require 'helm-utils)

(defgroup helm-xcdoc nil
  "Search Xcode Document with helm interface"
  :group 'helm)

(defcustom helm-xcdoc-command-path nil
  "command path of `docsletutil'"
  :group 'helm-xcdoc)

(defcustom helm-xcdoc-command-option nil
  "Command line option of `docsetutil'. This is appended after `helm-xcdoc-command-path'"
  :type 'string
  :group 'helm-xcdoc)

(defcustom helm-xcdoc-document-path nil
  "please set docset path like:
\"~/Library/Developer/Shared/Documentation/DocSets/com.apple.adc.documentation.AppleiOS8.1.iOSLibrary.docset\""
  :group 'helm-xcdoc)

(defcustom helm-xcdoc-maximum-candidates 100
  "Maximum number of helm candidates"
  :type 'integer
  :group 'helm-xcdoc)

(defcustom helm-xcdoc-log-level -1
  "Logging level, only messages with level lower or equal will be logged.
-1 = NONE, 0 = ERROR, 1 = WARNING, 2 = INFO, 3 = DEBUG"
  :type 'integer
  :group 'helm-xcdoc)

(defvar helm-xcdoc--query ""
  "query word")
(defvar helm-xcdoc--use-otherwin nil
  "split window flag")

(defconst helm-xcdoc--buffer "*helm xcdoc*")

(defun helm-xcdoc-log (level text &rest args)
  "Log a message at level LEVEL.
If LEVEL is higher than `helm-xcdoc-log', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format')."
  (if (<= level helm-xcdoc-log-level)
      (let* ((msg (apply 'format text args)))
        (message "%s" msg))))

(defun helm-xcdoc--set-start-at (&rest _)
  (goto-char (point-min))
  (re-search-forward helm-xcdoc--query nil t))
(advice-add 'eww-render :after 'helm-xcdoc--set-start-at)

(defun helm-xcdoc--construct-command (query _docset)
  (unless (executable-find helm-xcdoc-command-path)
    (error "'docsetutil' is not installed."))
  (unless (file-directory-p helm-xcdoc-document-path)
    (error "Document Directory not found"))
  (let ((cmds (list helm-xcdoc-command-path)))
    (setq cmds (append cmds (list "search -query" query)))
    (when helm-xcdoc-command-option
      (setq cmds (append cmds (list helm-xcdoc-command-option))))
    (setq cmds (append cmds (list helm-xcdoc-document-path)))
    (mapconcat 'identity cmds " ")))

(defun helm-xcdoc--excecute-search (query docset)
  (let ((cmd (helm-xcdoc--construct-command query docset))
        (call-shell-command-fn 'shell-command-to-string))
    (helm-xcdoc-log 3 "shell command: %s" cmd)
    (funcall call-shell-command-fn cmd)))

(defun helm-xcdoc--remove-hash (s)
  (replace-regexp-in-string (rx "#//" (* not-newline)) "" s))

(defun helm-xcdoc--construct-candidates-from-command-res (res)
  (let ((path-list (split-string res "\n")))
    (setq path-list (cl-remove-if-not (lambda (s) (string-match ".*\\.html.*" s)) path-list))
    (setq path-list (mapcar (lambda (s) (car (last (split-string s " "))))
                      (mapcar 'helm-xcdoc--remove-hash path-list)))
    (sort (delete-dups path-list) 'string<)))

(defun helm-xcdoc--catdir (s1 s2)
  (let ((s1 (replace-regexp-in-string (rx "/" eol) "" s1))
        (s2 (replace-regexp-in-string (rx bol "/") "" s2)))
    (concat s1 "/" s2)))

(defun helm-xcdoc--extract-html (file-path)
  (let ((get-html-path (lambda (docpath html-return-search)
                         (helm-xcdoc--catdir (helm-xcdoc--catdir docpath "Contents/Resources/Documents/") html-return-search))))
    (funcall get-html-path (expand-file-name helm-xcdoc-document-path) file-path)))

(defun helm-xcdoc--open-eww (file-path)
  (if helm-xcdoc--use-otherwin
      (let ((buf (current-buffer)))
        (eww-open-file (helm-xcdoc--extract-html file-path))
        (switch-to-buffer buf)
        (pop-to-buffer "*eww*"))
    (eww-open-file (helm-xcdoc--extract-html file-path))))

(defun helm-xcdoc--open-eww-other-window (file-path)
  (let ((helm-xcdoc--use-otherwin t))
    (helm-xcdoc--open-eww file-path)))

(defun helm-xcdoc--search-init ()
  (let ((buf-coding buffer-file-coding-system))
    (with-current-buffer (helm-candidate-buffer 'global)
      (let ((coding-system-for-read buf-coding)
            (coding-system-for-write buf-coding))
        (mapc (lambda (row)
                (insert (concat row "\n")))
              (helm-xcdoc--construct-candidates-from-command-res
               (helm-xcdoc--excecute-search helm-xcdoc--query helm-xcdoc-document-path)))
        (if (zerop (length (buffer-string)))
            (error "No output: '%s'" helm-xcdoc--query))))))

(defvar helm-xcdoc--open-eww-action
  '(("Open eww" . helm-xcdoc--open-eww)
    ("Open eww other window" . helm-xcdoc--open-eww-other-window)))

(defvar helm-source-xcdoc-search
  (helm-build-in-buffer-source "Xcode Document List"
    :init 'helm-xcdoc--search-init
    :candidate-number-limit helm-xcdoc-maximum-candidates
    :action helm-xcdoc--open-eww-action))

(defun helm-xcdoc--search-prepare (srcs query &optional otherwin)
  (let ((symbol-name-at-cursor (thing-at-point 'symbol)))
    (if (and (string= query "") (not symbol-name-at-cursor))
        (error "Input is empty!!"))
    (if (string= query "")
        (setq helm-xcdoc--query symbol-name-at-cursor)
      (setq helm-xcdoc--query query))
    (setq helm-xcdoc--use-otherwin otherwin)
    (helm-xcdoc-log 3 "helm-xcdoc--query %s" helm-xcdoc--query)
    (helm :sources srcs :buffer helm-xcdoc--buffer)))

(defun helm-xcdoc--prompt ()
  (let ((symbol-name-at-cursor (thing-at-point 'symbol)))
    (if symbol-name-at-cursor
        (read-string (format "Search word(default \"%s\"): " symbol-name-at-cursor))
      (read-string "Search word: "))))

;;;###autoload
(defun helm-xcdoc-search (query)
  "search xcode document"
  (interactive (list (helm-xcdoc--prompt)))
  (helm-xcdoc--search-prepare '(helm-source-xcdoc-search) query))

;;;###autoload
(defun helm-xcdoc-search-other-window (query)
  "search xcode document with other-window"
  (interactive (list (helm-xcdoc--prompt)))
  (helm-xcdoc--search-prepare '(helm-source-xcdoc-search) query t))

(provide 'helm-xcdoc)

;;; helm-xcdoc.el ends here
