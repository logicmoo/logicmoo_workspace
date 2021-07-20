;;; multishell-list.el --- tabulated-list-mode for multishell shell buffers  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2020 Free Software Foundation, Inc. and Ken Manheimer

;; Author: Ken Manheimer <ken.manheimer@gmail.com>
;; Version: 1.1.8
;; Created: 2016 -- first public availability
;; Keywords: processes
;; URL: https://github.com/kenmanheimer/EmacsMultishell

;; See multishell.el for commentary, change log, etc.

(require 'tabulated-list)
(require 'multishell)
(eval-when-compile (require 'cl-lib))

(defgroup multishell-list nil
  "Show a menu of all shell buffers in a buffer."
  :group 'multishell)

(defface multishell-list-name
  '((t (:weight bold)))
  "Face for shell names in the Multishell List."
  :group 'multishell-list)

(defun multishell-list-open-pop (&optional arg)
  "Pop to current entry's shell in separate window.

The shell is started if it's not already going, unless this is
invoked with optional `universal-argument'. In that case we
pop to the buffer but don't change its run state."
  (interactive "P")
  (let ((list-buffer (current-buffer))
        (entry (tabulated-list-get-id)))
    (if arg
        (pop-to-buffer
         (multishell-bracket (multishell-name-from-entry entry)))
      (multishell-list-dispatch-selected entry t))
    (with-current-buffer list-buffer
      (revert-buffer)
      (multishell-list-goto-item-by-entry entry))))

(defun multishell-list-open-as-default ()
  "Pop to current entry's shell, and set as the default shell."
  (interactive)
  (let ((list-buffer (current-buffer))
        (entry (tabulated-list-get-id)))
    (message "%s <==" (multishell-name-from-entry entry))
    (multishell-list-dispatch-selected entry t t)
    (with-current-buffer list-buffer
      (revert-buffer)
      (multishell-list-goto-item-by-entry entry))))

(defun multishell-list-open-here (&optional arg)
  "Switch to current entry's shell buffer.

The shell is started if it's not already going, unless this is
invoked with optional `universal-argument'. In that case we
switch to the buffer but don't activate (or deactivate) it it."
  (interactive "P")
  (let* ((list-buffer (current-buffer))
         (entry  (tabulated-list-get-id)))
    (if arg
        (switch-to-buffer
         (multishell-bracket (multishell-name-from-entry entry)))
      (multishell-list-dispatch-selected entry nil))
    (with-current-buffer list-buffer
      (revert-buffer))))

(defun multishell-list-delete (&optional _arg)
  "Remove current shell entry, and prompt for buffer-removal if present."
  (interactive)
  (let* ((entry (tabulated-list-get-id))
         (name (multishell-name-from-entry entry))
         (name-bracketed (multishell-bracket name))
         (buffer (get-buffer name-bracketed)))
    (when (multishell-delete-history-name name)
      (and buffer
           ;; If the process is live, let shell-mode get confirmation:
           (or (comint-check-proc (current-buffer))
               (y-or-n-p (format "Kill buffer %s? " name-bracketed)))
           (kill-buffer name-bracketed)))
    (tabulated-list-delete-entry)))

(defun multishell-list-edit-entry (&optional arg)
  "Edit the value of current shell entry.

Submitting the change will not launch the entry, unless this is
invoked with optional `universal-argument'. In the latter case,
submitting the entry will pop to the shell in a new window,
starting it if it's not already going."

  (interactive "P")
  (let* ((list-buffer (current-buffer))
         (entry (tabulated-list-get-id))
         (name (multishell-name-from-entry entry))
         (revised (multishell-read-unbracketed-entry
                   (format "Edit shell spec for %s: " name)
                   entry
                   'no-record))
         (revised-name (multishell-name-from-entry revised))
         buffer)
    (when (not (string= revised entry))
      (multishell-replace-entry entry revised)
      (when (and (not (string= name revised-name))
                 (setq buffer (get-buffer (multishell-bracket name))))
        (with-current-buffer buffer
          (rename-buffer (multishell-bracket revised-name)))))
    (when arg
      (multishell-list-dispatch-selected revised-name t))
    (with-current-buffer list-buffer
      (revert-buffer)
      (multishell-list-goto-item-by-entry revised))))

(defun multishell-list-clone-entry (&optional arg)
  "Create a new list entry, edited from the current one, ready to launch.

If you provide an optional `universal-argument', the new entry
will be launched when it's created.

The already existing original entry is left untouched."
  (interactive "P")
  (let* ((prototype (tabulated-list-get-id))
         (name (multishell-name-from-entry prototype))
         (new (multishell-read-unbracketed-entry
               (format "Clone new shell spec from %s: " name)
               prototype
               'no-record))
         (new-name (multishell-name-from-entry new))
         (new-path (cadr (multishell-split-entry new))))
    (when (not (string= new prototype))
      (multishell-register-name-to-path new-name new-path)
      (revert-buffer)
      (multishell-list-goto-item-by-entry new)
      (when arg
        (multishell-list-dispatch-selected new-name t)))))

(defun multishell-list-mouse-select (event)
  "Select the shell whose line is clicked."
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (let ((entry (tabulated-list-get-id (posn-point (event-end event)))))
    (multishell-list-dispatch-selected entry nil)))

(defun multishell-list-dispatch-selected (entry pop &optional set-primary)
  "Go to multishell ENTRY, popping to window if POP is non-nil.

Optional arg SET-PRIMARY non-nil sets `multishell-primary-name' to entry.

Provide for concluding minibuffer interaction if we're in completing mode."
  (let ((set-primary-as-arg (and set-primary '(16))))
    (if multishell-completing-read
        ;; In multishell completing-read, arrange to conclude minibuffer input:
        (throw 'multishell-minibuffer-exit (list entry pop set-primary-as-arg))
      (multishell-pop-to-shell set-primary-as-arg entry (not pop)))))

(defun multishell-list-placeholder (value default)
  "Return VALUE if non-empty string, else DEFAULT."
  (if (or (not value) (string= value ""))
      default
    value))
(defconst multishell-list-active-flag "+")
(defconst multishell-list-inactive-flag ".")
(defconst multishell-list-absent-flag "x")

(defun multishell-list-entries ()
  "Generate multishell name/path-spec entries list for tabulated-list."
  (let ((recency 0))
    (mapcar #'(lambda (entry)
                (setq recency (1+ recency))
                (let* ((splat (multishell-split-entry entry))
                       (name (car splat))
                       (buffer (and name
                                    (get-buffer
                                     (multishell-bracket name))))
                       (status (cond ((not buffer)
                                      multishell-list-absent-flag)
                                     ((comint-check-proc buffer)
                                      multishell-list-active-flag)
                                     (t multishell-list-inactive-flag)))
                       (rest (cadr splat))
                       (dir (and rest (or (file-remote-p rest 'localname)
                                          rest)))
                       (hops (and dir
                                  (file-remote-p rest 'localname)
                                  (substring
                                   rest 0 (- (length rest) (length dir))))))
                  (when (not name)
                    (setq name (multishell-name-from-entry entry)))
                  (list entry
                        (vector (format "%d" recency)
                                status
                                (multishell-list--decorate-name name)
                                (multishell-list-placeholder hops "-")
                                (multishell-list-placeholder dir "~")))))
            (multishell-all-entries))))

(defun multishell-list-goto-item-by-entry (entry)
  "Position at beginning of line of tabulated list item for ENTRY."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (string= (tabulated-list-get-id) entry)))
    (forward-line 1)))

(define-obsolete-function-alias 'multishell-collate-row-strings-as-numbers
  #'multishell-list--collate-row-strings-as-numbers "multishell 1.1.6")
(defun multishell-list--collate-row-strings-as-numbers (a b)
  (let ((a (aref (cadr a) 0))
        (b (aref (cadr b) 0)))
    (> (string-to-number a) (string-to-number b))))

(defun multishell-list--decorate-name (name)
  (propertize name
              'font-lock-face 'multishell-list-name
              'mouse-face 'highlight))

(defvar multishell-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "c") 'multishell-list-clone-entry)
    (define-key map (kbd "d") 'multishell-list-delete)
    (define-key map (kbd "\C-k") 'multishell-list-delete)
    (define-key map (kbd "k") 'multishell-list-delete)
    (define-key map (kbd "e") 'multishell-list-edit-entry)
    (define-key map (kbd "o") 'multishell-list-open-pop)
    (define-key map (kbd " ") 'multishell-list-open-pop)
    (define-key map (kbd "O") 'multishell-list-open-as-default)
    (define-key map (kbd "RET") 'multishell-list-open-here)
    (define-key map [mouse-2] 'multishell-list-mouse-select)
    (define-key map [follow-link] 'mouse-face)
    map))
(define-derived-mode multishell-list-mode
    tabulated-list-mode "Shells"
  "Major mode for listing current and historically registered shells.

Initial sort is from most to least recently used:

- First active shells, flagged with `+' a plus sign
- Then, inactive shells, flagged with `.' a period
- Then historical shells that currently have no buffer, flagged with `x' an ex

\\{multishell-list-mode-map\}"
  (setq tabulated-list-format
        [;; (name width sort '(:right-align nil :pad-right nil))
         ("#" 0 multishell-list--collate-row-strings-as-numbers :pad-right 1)
         ("! " 1 t :pad-right 1)
         ("Name" 15 t)
         ("Hops" 30 t)
         ("Directory" 30 t)]
        tabulated-list-sort-key '("#" . t)
        tabulated-list-entries #'multishell-list-entries)
  (tabulated-list-init-header))

(defun multishell-list-cull-dups (entries)
  "Return list of multishell ENTRIES sans ones with duplicate names.

For duplicates, we prefer the ones that have paths."
  (let ((tally (make-hash-table :test #'equal))
        got name name-order-reversed already)
    (dolist (entry entries)
      (setq name (multishell-name-from-entry entry)
            already (gethash name tally nil))
      (when (not already)
        (push name name-order-reversed))
      (when (or (not already) (< (length already) (length entry)))
        ;; Add new or replace shorter prior entry for name:
        (puthash name entry tally)))
    (dolist (name name-order-reversed)
      (push (gethash name tally) got))
    got))

;;;###autoload
(defun multishell-list (&optional completing)
  "Edit your current and historic list of shell buffers.

If optional COMPLETING is nil, we present the full
`multishell-history' list in a popped buffer named `*Shells*'.

In the buffer, hit ? or h for a list of commands.

When optional COMPLETING is non-nil, it must be a list of
multishell-history completion candidate entries, as provided by
`completing-read'. Then we present the list as a part of
minibuffer completion.

You can get to the shells listing by recursively invoking
\\[multishell-pop-to-shell] at the `multishell-pop-to-shell'
`universal-argument' prompts."
  (interactive)
  (let ((from-entry (car (multishell-history-entries
                          (multishell-unbracket (buffer-name
                                                 (current-buffer))))))
        (buffer (get-buffer-create (if completing
                                       "*Completions*"
                                     "*Shells*"))))
    (if completing
        (set-buffer buffer)
      (pop-to-buffer buffer))
    (multishell-list-mode)
    (cl-progv
        ;; Temporarily assign multishell-history only when completing:
        (when completing '(multishell-history))
        (when completing
          (list (multishell-list-cull-dups (mapcar #'substring-no-properties
                                                   completing))))
      (tabulated-list-print))
    (when completing
      )
    (when from-entry
      (multishell-list-goto-item-by-entry from-entry))))

(provide 'multishell-list)

;;; multishell-list.el ends here
