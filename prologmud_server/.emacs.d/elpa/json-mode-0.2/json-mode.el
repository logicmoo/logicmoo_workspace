;;; json-mode.el --- Major mode for editing JSON files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
;; Maintainer: Simen Heggestøyl <simenheg@gmail.com>
;; Version: 0.2
;; Package-Requires: ((emacs "25.1"))
;; Keywords: data

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

;; Major mode for editing JavaScript Object Notation (JSON) data files.
;; Read more about JSON at http://json.org/.

;; It provides support for indentation and syntax highlighting.

;; Thanks to Josh Johnston for writing the original JSON mode!

;;; Code:

(require 'json)
(require 'smie)

(defgroup json-mode nil
  "JavaScript Object Notation (JSON) editing mode."
  :tag "JSON Mode"
  :group 'data)

(defcustom json-mode-indent-level 2
  "Basic size of one indentation step."
  :type 'integer)

(defface json-mode-object-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face to use for JSON object names.")

(defvar json-mode-map
  (let ((map (make-sparse-keymap "JSON")))
    (define-key map "\C-c\C-f" #'json-mode-pretty-print-dwim)
    (define-key map "\C-c\C-p" #'json-mode-show-path)
    (easy-menu-define json-menu map "JSON mode menu"
      `("JSON"
        :help "JSON-specific features"
        ["Pretty-print region" json-mode-pretty-print-dwim
         :visible (region-active-p)]
        ["Pretty-print buffer" json-mode-pretty-print-dwim
         :visible (not (region-active-p))]
        ["Show path" json-mode-show-path]))
    map)
  "Keymap used in JSON mode.")

(defvar json-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Objects
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    ;; Arrays
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    ;; Comments
    (modify-syntax-entry ?/ ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(defconst json-mode--keywords '("true" "false" "null")
  "List of JSON keywords.")

(defvar json-mode-font-lock-keywords
  `(;; Constants
    (,(concat "\\<" (regexp-opt json-mode--keywords) "\\>")
     (0 font-lock-constant-face))))

(defun json-mode--string-is-object-name-p (startpos)
  "Return t if STARTPOS is at the beginning of an object name."
  (save-excursion
    (goto-char startpos)
    (and (eq (char-after) ?\")
         (condition-case nil
             (progn (forward-sexp 1) t)
           (scan-error nil))
         (looking-at "[[:blank:]]*:"))))

(defun json-font-lock-syntactic-face-function (state)
  "Determine which face to use for strings and comments.
Object names receive the face `json-mode-object-name-face' to
distinguish them from other strings."
  (cond
   ((nth 4 state) font-lock-comment-face)
   ((and (nth 3 state)
         (json-mode--string-is-object-name-p (nth 8 state)))
    'json-mode-object-name-face)
   (t font-lock-string-face)))

(defconst json-mode--smie-grammar
  (smie-prec2->grammar
   (smie-precs->prec2 '((assoc ",") (left ":")))))

(defun json-mode--smie-rules (method arg)
  "Provide indentation rules for METHOD given ARG.
See the documentation of `smie-rules-function' for further
information."
  (pcase (cons method arg)
    (`(:elem . basic) json-mode-indent-level)))

(defun json-mode-pretty-print-dwim (&optional alphabetical)
  "Pretty print region if active, else pretty print the buffer.
`json-mode-indent-level' will be used as indentation offset.  If
ALPHABETICAL is non-nil (interactively, with a prefix argument),
JSON object members will be sorted alphabetically by their keys."
  (interactive "P")
  (let ((json-encoding-default-indentation
         (make-string json-mode-indent-level ?\s)))
    (if (use-region-p)
        (funcall
         (if alphabetical
             #'json-pretty-print-ordered
           #'json-pretty-print)
         (region-beginning) (region-end))
      (funcall
       (if alphabetical
           #'json-pretty-print-buffer-ordered
         #'json-pretty-print-buffer)))))

(defun json-mode-show-path ()
  "Show the path to the JSON value under point.
The value is also copied to the kill ring."
  (interactive)
  (let ((path (json-path-to-position (point))))
    (if path
        (let ((formatted-path
               (json-mode--format-path (plist-get path :path))))
          (when (fboundp 'pulse-momentary-highlight-region)
            (pulse-momentary-highlight-region
             (plist-get path :match-start)
             (plist-get path :match-end)))
          (message formatted-path)
          (kill-new formatted-path))
      (message "Not a JSON value"))))

(defun json--which-func ()
  (let ((path (plist-get (json-path-to-position (point)) :path)))
    (when path
      ;; There's not much space in the modeline, so this needs
      ;; to be more compact than what `json-mode--format-path' produces.
      ;; FIXME: Even in this more compact form it can easily get too long
      ;; for comfort.  Add some way to shorten it.
      (mapconcat (lambda (key) (format "%s" key)) path ";"))))

(defun json-mode--format-path (path)
  "Return PATH formatted as a JSON data selector.
PATH should be a list of keys, which can be either strings or
integers."
  (mapconcat (lambda (key) (format "[%S]" key)) path ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;;;###autoload
(define-derived-mode json-mode prog-mode "JSON"
  "Major mode for editing JavaScript Object Notation (JSON) data files."
  (setq-local
   font-lock-defaults
   '(json-mode-font-lock-keywords
     nil nil nil nil
     (font-lock-syntactic-face-function
      . json-font-lock-syntactic-face-function)))
  ;; JSON has no comment syntax, but we set this to keep SMIE happy.
  ;; Also, some JSON extensions allow comments.
  (add-hook 'which-func-functions #'json--which-func nil t)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (smie-setup json-mode--smie-grammar #'json-mode--smie-rules))

(defun json--jit-wrap (beg end)
  (save-excursion
    (remove-overlays beg end 'json-jit-wrap t)
    (goto-char beg)
    (let ((ppss (syntax-ppss beg))
          (last beg))
      (while (re-search-forward "[][{},]+" end t)
        (let* ((pos (match-beginning 0))
               (closer (memq (char-after pos) '(?\] ?\}))))
          (setq ppss (parse-partial-sexp last (point) nil nil ppss)
                last (point))
          (unless (nth 8 ppss)
            (let ((s (concat "\n" (make-string
                                   (* json-mode-indent-level (nth 0 ppss))
                                   ?\s)))
                  (ol (make-overlay (1- (point)) (point))))
              (overlay-put ol 'json-jit-wrap t)
              (overlay-put ol 'after-string s)
              (when closer
                (let ((ol (make-overlay pos (1+ pos))))
                  (overlay-put ol 'json-jit-wrap t)
                  (overlay-put ol 'before-string s))))))))))

;; FIXME: On a small json file, this seems to work OK, but
;; line-based movement is already occasionally very slow.
;; I haven't dared to try it on a largish json file.
(define-minor-mode json-jit-wrap-mode
  "Add virtual newlines."
  :global nil
  (if json-jit-wrap-mode
      (progn
        ;; Note: this jitter works (almost) character-by-character
        ;; so doesn't need to round up to while lines (which is great!),
        ;; but of course, if font-lock is enabled, jit-lock will
        ;; still end up calling us one whole line at a time :-(
        (jit-lock-register #'json--jit-wrap)
        (add-hook 'change-major-mode-hook
                  (lambda () (json-jit-wrap-mode -1)) nil t))
    (jit-lock-unregister #'json--jit-wrap)
    (remove-overlays (point-min) (point-max) 'json-jit-wrap t)))

;;;; ChangeLog:

;; 2020-11-09  Simen Heggestøyl  <simenheg@runbox.com>
;; 
;; 	Bump json-mode version to 0.2
;; 
;; 2020-07-22  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* json-mode/json-mode.el (json-jit-wrap-mode): New minor mode
;; 
;; 	(json--jit-wrap): New function.
;; 
;; 2020-07-22  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* json-mode/json-mode.el: Add which-func support
;; 
;; 	(json--which-func): New function.
;; 	(json-mode): Use it.
;; 	(json-mode--keywords): New var.
;; 	(json-mode-font-lock-keywords): Use it instead of the obsolete
;; 	`json-keywords`.
;; 
;; 2016-12-18  Simen Heggestøyl  <simenheg@gmail.com>
;; 
;; 	Use syntactic fontification for JSON object names
;; 
;; 	* packages/json-mode/json-mode.el (json-mode-font-lock-keywords): Don't 
;; 	match object names and strings.
;; 	(json-mode--string-is-object-name-p): New function determining whether a 
;; 	position is at the beginning of an object name.
;; 	(json-font-lock-syntactic-face-function): Fontify object names.
;; 
;; 2016-12-11  Simen Heggestøyl  <simenheg@gmail.com>
;; 
;; 	New package: json-mode
;; 


(provide 'json-mode)

;;; json-mode.el ends here
