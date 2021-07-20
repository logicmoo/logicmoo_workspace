;;; treefactor.el --- Restructure your messy Org documents     -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Leo Littlebook

;; Author: Leo Littlebook <Leo.Littlebook@gmail.com>
;; Keywords: outlines, files, convenience
;; Package-Version: 3.2.2
;; Package-Commit: 75357757022a4399ab772ff0d92065bd114dabe9
;; Package-Requires: ((emacs "26.1") (dash "2.16.0") (f "0.20.0") (org "9.2.6") (avy "0.5.0"))
;; URL: https://github.com/cyberthal/treefactor
;; Version: 3.2.2

;;; Commentary:

;; Treefactor provides commands to incrementally refile files in Dired and to
;; refactor outlines. It allows Org mode to manage a dynamic meta-outline
;; combining both the directory hierarchy and outlines within files.

;; Read the manual at

;;   https://treefactor-docs.nfshost.com

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License

;;; Code:

;; * treefactor.el
;; * offset
;; ** Config
;; *** require

(require 'org)
(require 'dired)
(require 'dash)
(require 'f)
(require 'avy)

;; *** define vars and funcs

(defvar treefactor-object-text nil
  "Stores the last text Treefactor killed or copied.")

(defvar user-home-directory) ; Spacemacs variable
(defvar isearch-string)
(defvar dired-isearch-filenames)
(defvar org-id-extra-files)

;; *** customization

(defgroup treefactor nil "Refactor prose and incrementally refile."
  :group 'convenience
  :group 'files)

;; **** Alias prefixes

(defcustom treefactor-use-alias-prefixes nil
  "Non-nil if prefix aliases should be created for user commands."
  :type 'boolean)

(defcustom treefactor-alias-prefix-1 "tro"
  "First prefix for aliased user commands. No dash needed.

Do not set to treefactor or it will cause an infinite loop."
  :type 'string)

(defcustom treefactor-alias-prefix-2 ""
  "Second prefix for aliased user commands. No dash needed.

Do not set to treefactor or it will cause an infinite loop."
  :type 'string)

;; **** Org search scope

(defcustom treefactor-org-agenda-dir nil
  "Directory of variable `org-agenda-files'. Requires quotes and trailing slash."
  :type 'directory)

(defcustom treefactor-org-id-extra-dir nil
  "Directory of variable `org-id-extra-files'. Requires quotes and trailing slash."
  :type 'directory)

;; *** aliases

(defvar treefactor-user-commands
  (list "throw" "up" "delete-this-buffer-and-file" "org-store-link-fold-drawer" "org-dired-zinks" "org-duplicate-heading-to-other-window" "org-refactor-heading" "clear-org-search-scope" "refresh-org-search-scope" "pipify-lines" "org-insert-heading-divider" "rename-next-heading" "org-timestamp-now-inactive" "org-toggle-checkbox-forward-line"))

(defun treefactor-defalias-1 (suffix)
  "Alias `treefactor' function SUFFIX to `treefactor-alias-prefix-1'."
  (defalias
    (intern (concat treefactor-alias-prefix-1 "-" suffix))
    (intern (concat "treefactor-" suffix))))

(defun treefactor-defalias-2 (suffix)
  "Alias `treefactor' function SUFFIX to `treefactor-alias-prefix-2'."
  (defalias
    (intern (concat treefactor-alias-prefix-2 "-" suffix))
    (intern (concat "treefactor-" suffix))))

(when treefactor-use-alias-prefixes
  (when treefactor-alias-prefix-1
    (mapc #'treefactor-defalias-1 treefactor-user-commands))
  (when treefactor-alias-prefix-2
    (mapc #'treefactor-defalias-2 treefactor-user-commands)))

;; ** Refile

;; *** define variables and declare functions

(defvar treefactor-inbox-file-header)

(declare-function outshine-narrow-to-subtree "outshine" ())

;; *** main defun

;;;###autoload
(defun treefactor-throw (&optional count)
  "Refile text/file to target in next window COUNT times.
Select a line from target list using `isearch' then `avy'."
  (interactive "p")

  (dotimes (_var count)
    (unwind-protect
        (treefactor-throw-object-mode-check))))

(defun treefactor-throw-object-mode-check ()
  "Determine correct action based on current window mode.
If in dired, refile files. If not, refile text."

  (if (eq major-mode 'dired-mode)
      (treefactor-throw-file)
    (treefactor-throw-text)))

;; *** flow control dispatcher
;; **** refile text
;; ***** main defun

(defun treefactor-throw-text ()
  "Refile text to either Dired or an outline."

  (select-window (next-window))
  (let ((treefactor-in-dired-p (eq major-mode 'dired-mode)))
    (select-window (previous-window))

    (if treefactor-in-dired-p
        (treefactor-throw-text-to-dired)
      (call-interactively #'treefactor-throw-text-to-outline)))
  (other-window -1)                     ; because save-selected-window fails
  (save-buffer))

;; ***** destination = dired

;; ****** main defun

(defun treefactor-throw-text-to-dired ()
  "Refile text to a searched target in an adjacent Dired buffer."

  (select-window (next-window))
  (let ((treefactor-dired-starting-buffer (current-buffer)))
    (treefactor-search-dired-open)
    (select-window (previous-window))
    (treefactor-snort-text)
    (select-window (next-window))
    (if buffer-file-name
        (treefactor-insert-text-to-file-blind)
      (treefactor-insert-text-to-directory))
    (switch-to-buffer treefactor-dired-starting-buffer) ; cuz save-current-buffer bugged
    (forward-char 2)))

;; ****** destination = dir

(defun treefactor-insert-text-to-directory ()
  "Insert `treefactor-object-text' to Inbox.org."

  (treefactor-create-open-inbox-file)
  (treefactor-insert-to-end-of-buffer)
  (treefactor-text-inserted-to-buffer-path-message))

;; ****** destination = file

(defun treefactor-insert-text-to-file-blind ()
  "Put point either before first level-1 heading or at end of buffer.
Normally one wants to yank to the end of the buffer.
But if it's a polished document rather than an inbox,
then one wants the new text at the top, where it's more noticeable.
Assume a polished document will have a level-1 near the top."

  (goto-char (point-min))
  (condition-case nil
      (progn (re-search-forward "^* ")       ; Search for a level-1 headline.
       (goto-char (point-at-bol))
       (insert treefactor-object-text))
    (error (treefactor-insert-to-end-of-buffer)))
  (save-buffer)
  (treefactor-text-inserted-to-buffer-path-message))

;; ***** destination = text
;; ****** main defun

(defun treefactor-throw-text-to-outline ()
  "Refile text to an outline heading in the next window.

Assume that the first line of the target window is the parent
heading. Show the parent's direct child headings. Use `isearch'.
If multiple matches result, pick one with `avy'.

Refile text at point in home window to the bottom of the target heading.
Refiled text may be a line or an outline heading."

  (interactive)

  (select-window (next-window))

  ;; reset target list visibility
  (goto-char (point-min))

  (if (org-at-heading-p)
      (outline-hide-subtree)
    (outline-next-visible-heading 1))

  (if (org-at-heading-p)
      (outline-hide-subtree)
    (goto-char (point-min))
    (user-error "%s" "Outline not found in visible region"))

  (outline-show-children 1)
  (outline-hide-body)

  (let ((isearch-invisible nil)
        (search-exit-option nil))
    (isearch-forward))

  (if (eq (point) (point-min))
      (user-error "%s" "User quit isearch"))

  (if (eq 1 (length (avy--regex-candidates (regexp-quote isearch-string)))) ; 1 match?
      (treefactor-place-in-outline)
    (treefactor-avy-isearch)
    (treefactor-place-in-outline)))

;; ****** place in outline

(defun treefactor-place-in-outline ()
  "Refile text from previous window's heading to heading at point."

  (save-restriction
    (org-narrow-to-subtree)
    (treefactor-region-ends-n-newlines 2)
    (save-selected-window (select-window (previous-window))
                          (treefactor-snort-text))
    (insert treefactor-object-text)
    (save-buffer)
    (goto-char (point-min)))
  (outline-hide-subtree))

;; **** refile file

(defun treefactor-throw-file ()
  "Refile file(s) from Dired to searched target in next window."

  (select-window (next-window))
  (treefactor-search-dired-open)
  (mkdir (concat default-directory "0-Inbox/") 1)
  (find-file (concat default-directory "0-Inbox/"))
  (select-window (previous-window))
  (dired-do-rename)

  (select-window (next-window))
  (dired-up-directory)     ; restores original dired buffer.
  (dired-up-directory)     ; necessary because save-current-buffer won't restore
                                        ; after dired-do-rename.
  (forward-char 2))

;; *** refile up
;; **** main defun

;;;###autoload
(defun treefactor-up (&optional count)
  "Refile file or text one step upwards, COUNT times.

Text will go to an Inbox.org of the same directory level, or one
higher if already in an Inbox.org

File will go one directory level higher beneath a 0-Inbox/,
unless already under 0-Inbox/, in which case two higher beneath a
0-Inbox/"

  (interactive "p")

  (dotimes (var count)

    (if (eq major-mode 'dired-mode)
        (treefactor-up-file)
      (treefactor-up-text))
    (message "Refiled up %s times" (1+ var))))

;; **** jump height

(defun treefactor-jump-destination ()
  "Return how many directory levels up `treefactor-up' should go."

  (if (eq major-mode 'dired-mode)
      (concat default-directory
              ;; Returns ../ unless parent dir is 0-Inbox, then ../../
              (if (treefactor-parent-dir-inbox-p)
                  "../../"
                "../"))
    (concat default-directory
            ;; "Returns ../ unless buffer's file is Inbox.org, then nil
            (when (treefactor-file-inbox-p) "../"))))

;; **** object = text

(defun treefactor-up-text ()
  "Refile text up to the next Inbox.org.

Text will go to an Inbox.org of the same directory level, or one
higher if already in an Inbox.org"

  (let ((treefactor-buffer-home (current-buffer))
        (default-directory (treefactor-jump-destination)))
    (treefactor-snort-text)
    (treefactor-create-open-inbox-file)
    (treefactor-insert-text-to-file-blind)
    (switch-to-buffer treefactor-buffer-home) ; because save-current-buffer failed here
    (save-buffer)))

;; **** target = file

(defun treefactor-up-file ()
  "Refile file up to the next 0-Inbox/.

File will go one directory level higher and beneath 0-Inbox/,
unless already under 0-Inbox/, in which case two higher and beneath 0-Inbox/."

  (let* ((treefactor-jump-destination (treefactor-jump-destination))
         (treefactor-inbox-dir (concat treefactor-jump-destination "0-Inbox/")))
    (unless (file-exists-p treefactor-inbox-dir)
      (mkdir treefactor-inbox-dir))
    (rename-file (dired-get-filename "no-dir") treefactor-inbox-dir)
    (message "File refiled to %s" treefactor-jump-destination))
  (revert-buffer)        ; refreshes screen significantly faster than otherwise.
  )

;; *** library
;; **** snort type
;; ***** text mode?

(defun treefactor-snort-text ()
  "If heading or line of text to `treefactor-snort-line' variable."
  (cond ((eq major-mode 'org-mode) (treefactor-snort-text-org))
        ((-contains-p minor-mode-list 'outshine-mode) (treefactor-snort-text-outshine))
        ((-contains-p minor-mode-list 'outline-minor-mode) (treefactor-snort-text-outline))
        (t (treefactor-snort-line))))

;; ***** at heading?

(defun treefactor-snort-text-org ()
  "Store text. Range stored depends on local context.

If in a an `org' heading, store the heading. Otherwise, store the
line."

  (if (org-at-heading-p) (treefactor-snort-org-heading)
    (treefactor-snort-line)))

(defun treefactor-snort-text-outshine ()
  "Store text. Range stored depends on context.

If inside an `outshine' outline heading, store text. Otherwise,
store line."

  (if (outline-on-heading-p) (treefactor-snort-outshine-heading)
    (treefactor-snort-line)))

(defun treefactor-snort-text-outline ()
  "Store text. Range stored depends on context.

If in an outline heading, store the heading. Otherwise store
line."

  (if (outline-on-heading-p) (treefactor-snort-outline-heading)
    (treefactor-snort-line)))

;; ***** heading type?

(defun treefactor-snort-org-heading ()
  "Store an `org' heading."

  (save-restriction
    (org-narrow-to-subtree)
    (treefactor-snort-visible)))

(defun treefactor-snort-outshine-heading ()
  "Store an `outshine' heading."

  (save-restriction
    (outshine-narrow-to-subtree)
    (treefactor-snort-visible)))

(defun treefactor-snort-outline-heading ()
  "Store an outline heading."

  (save-restriction
    (org-narrow-to-subtree)
    (treefactor-snort-visible)))

;; ***** line

(defun treefactor-snort-line ()
  "Move a line of text to variable `treefactor-object-text'."

  (if (eq (point-min) (point-max))
      (user-error "%s" "Selected line is empty")
    (setq treefactor-object-text
          (concat (delete-and-extract-region (line-beginning-position) (line-end-position))
                  "\n"))
    (treefactor-delete-leftover-empty-line)))

;; **** files
;; ***** Find the searched dired entry

(defun treefactor-search-dired-open ()
  "Open the `dired' file that the user picked using `isearch'."

  (unless (eq major-mode 'dired-mode)
    (user-error "%s" "Mode must be Dired"))

  (goto-char (point-min))

  (let ((inhibit-message t))
    (dired-hide-details-mode))

  (let ((isearch-invisible nil)
        (dired-isearch-filenames t)     ; use var cuz function causes error
        (search-exit-option nil))
    (isearch-forward))

  ;; (treefactor-avy-isearch) cuz avy searches hidden dired text
  ;; https://github.com/abo-abo/avy/issues/282
  ;; isearch-lazy-count might also work when it's released.

  (dired-find-file))

;; ***** check whether immediate parent dir is "0-Inbox"

(defun treefactor-parent-dir-inbox-p ()
  "Return t if parent dir is 0-Inbox."

  (equal
   (file-name-nondirectory (directory-file-name default-directory)) ; Return parent directory.
   "0-Inbox"))

;; ***** check whether file is "Inbox.org"

(defun treefactor-file-inbox-p ()
  "Return t if buffer's filename is Inbox.org."

  (if (eq (buffer-file-name) nil)
      (user-error "%s" "Buffer has no file"))

  (equal
   (file-name-nondirectory (buffer-file-name))
   "Inbox.org"))

;; ***** Inbox.org creation
;; ****** Create open Inbox.org

(defun treefactor-create-open-inbox-file ()
  "If no Inbox.org, make it and insert *** offset."

  (let* ((treefactor-inbox-file-path (concat default-directory "Inbox.org"))
         (treefactor-inbox-file-buffer (find-buffer-visiting treefactor-inbox-file-path)))

    (cond (treefactor-inbox-file-buffer (set-buffer treefactor-inbox-file-buffer)) ; select buffer if exists
          ((file-exists-p treefactor-inbox-file-path) (find-file treefactor-inbox-file-path)) ; open file if exists
          ;; else create and open file
          (t (f-touch "Inbox.org")
             (find-file treefactor-inbox-file-path)
             (insert treefactor-inbox-file-header)
             (goto-char (point-min))
             (org-cycle-hide-drawers 1)
             (goto-char (point-max))))))

;; ****** customization

(defcustom treefactor-inbox-file-header "*** Inbox.org\n:PROPERTIES:\n:VISIBILITY: children\n:END:\n\n"
  "Header inserted into new Inbox.org files created by `treefactor-throw-text' and `treefactor-up-text'."
  :type '(string)
  :group 'treefactor)

;; ** Utilities
;; *** Delete buffer and file

;;;###autoload
(defun treefactor-delete-this-buffer-and-file ()
  "Delete file visited by current buffer and kill buffer."
  (interactive)

  (let ((filename (buffer-file-name)))
    (if (buffer-narrowed-p)
        (user-error "%s" "Buffer is narrowed")
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (kill-buffer (current-buffer))
        (delete-file filename)
        (message "File `%s' successfully removed" filename)))))

;; *** Pipify lines

(defun treefactor-pipify-lines ()
  "Convert consecutive lines into a single line separated by pipes."
  (interactive)
    (end-of-line)
    (insert " | ")
    (delete-char 1)
    (end-of-line))

;; *** Org

;; **** Refactor heading

(defun treefactor-org-refactor-heading ()
  "From a single-window frame in `org-mode', setup frame to refactor an heading.

A duplicate of the heading is created with the suffix | REFACTORED.
The first window narrows to the original heading. The new window displays an
INBOX heading. The user transfers text from the first window to the second."

  (interactive)

  (cond ((unless (eq major-mode 'org-mode) t) (user-error "%s" "Error, must be in org-mode"))
        ((unless (eq 1 (length (window-list))) t) (user-error "%s" "Error, must have only one window open in frame"))
        ((unless (progn
                   (org-narrow-to-subtree)
                   (org-previous-visible-heading 1)
                   (org-at-heading-p)) t) (user-error "%s" "Error, point must be inside a heading"))
        (t
             ;; ensure region ends with two newlines
             (goto-char (point-max))
             (if (bolp)
                 (org-N-empty-lines-before-current 1)
               (insert "\n\n"))

             ;; duplicate the heading
             (goto-char (point-max))
             (insert (buffer-substring (point-min) (point-max)))

             ;; title REFACTORED heading
             (goto-char (point-min))
             (org-show-all '(headings))
             (goto-char (line-end-position))
             (insert " | REFACTORED")
             (org-cycle)

             ;; create INBOX heading
             (org-global-cycle)
             (org-next-visible-heading 1)
             (org-insert-heading '(4))
             (insert "INBOX")

             ;; prep INBOX heading
             (org-tree-to-indirect-buffer)
             (select-window (next-window))
             (goto-char (point-max))
             (org-insert-heading)
             (org-demote)
             (insert "/")

             ;; prep heading for refactoring
             (other-window -1)  ; ensures that golden-ratio enlarges home window
             (org-previous-visible-heading 1)
             (org-cycle)
             (org-narrow-to-subtree)
             (org-show-all '(headings))
             (org-cycle-hide-drawers 1))))

;; **** Duplicate heading to other window

;;;###autoload
(defun treefactor-org-duplicate-heading-to-other-window ()
  "Append heading at point to end of next window's buffer."
  (interactive)

  (save-restriction
    (org-narrow-to-subtree)
    (treefactor-region-ends-n-newlines 1)
    (let ((home-buffer (current-buffer)))
      (save-selected-window
        (select-window (next-window))
        (treefactor-region-ends-n-newlines 2)
        (insert-buffer-substring home-buffer)))))

;; **** Insert heading divider

(defun treefactor-org-insert-heading-divider ()
  "Create a heading. Advance two paragraphs. Recenter view."
  (interactive)

  (org-open-line 2)
  (org-insert-heading)
  (insert "?")
  (org-forward-paragraph)
  (org-forward-paragraph)
  (recenter-top-bottom 10))

;; **** Rename next heading

(defun treefactor-rename-next-heading ()
  "Go to the end of the next headline, narrowed."
  (interactive)

  (org-narrow-to-subtree)
  (org-previous-visible-heading 1)
  (widen)
  (org-cycle -1)
  (org-next-visible-heading 1)
  (org-narrow-to-subtree)
  (goto-char (line-end-position)))

;; **** Insert inactive timestamp of current time

(defun treefactor-org-timestamp-now-inactive ()
  "Insert inactive timestamp of current time."

  ;; Calls org-time-stamp-inactive with universal prefix
  (interactive)
  (org-insert-time-stamp (current-time) t t))

;; **** Advance checkboxes

(defun treefactor-org-toggle-checkbox-forward-line ()
  "Toggle checkbox and advance one line."
  (interactive)

  (org-toggle-checkbox)
  (forward-line 1))

;; **** Links
;; ***** Store link and fold the PROPERTIES drawer

;;;###autoload
(defun treefactor-org-store-link-fold-drawer ()
  "Store an org link to a heading, and fold the drawer."
  (interactive)

  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (org-store-link nil t) ; Without interactive=t arg, no org link gets created.
      (org-previous-visible-heading 1)
      (org-cycle-hide-drawers 1))))

;; ***** create Zinks.org

;;;###autoload
(defun treefactor-org-dired-zinks ()
  "Make Zinks.org.  Insert org-id link.

Link title's path is relative to `vc-root-dir' if present,
else `user-home-directory'."
  (interactive)

  (let ((zinks-filename (concat default-directory "Zinks.org")))
    (if (file-exists-p zinks-filename)
        (user-error "%s" "Zinks.org already exists")
      (find-file zinks-filename)
      (insert (concat "*** "
                      (file-relative-name (file-name-directory buffer-file-name)
                                          (cond ((vc-root-dir) (vc-root-dir))
                                                (user-home-directory user-home-directory) ; Spacemacs variable. If missing, no problem.
                                                ))
                      "\n\n"))
      (treefactor-org-store-link-fold-drawer)
      (save-buffer)
      (goto-char (point-max)))))

;; **** Search scope
;; ***** Clear

(defun treefactor-clear-org-search-scope ()
  "Clear `org' search scope file list."
  (interactive)

  (setq org-agenda-files nil)
  (setq org-id-extra-files nil))

;; ***** Refresh

(defun treefactor-refresh-org-search-scope ()
  "Recursively refresh `org' search scope."
  (interactive)

  (treefactor-clear-org-search-scope)

  (unless (file-directory-p treefactor-org-agenda-dir)
    (error "Not a directory `%s'" treefactor-org-agenda-dir))

  (unless (file-directory-p treefactor-org-id-extra-dir)
    (error "Not a directory `%s'" treefactor-org-id-extra-dir))

  (setq org-agenda-files
        (directory-files-recursively treefactor-org-agenda-dir "org$"))

  (setq org-id-extra-files
        (directory-files-recursively treefactor-org-id-extra-dir ".org$")))

;; ** Library

;; *** avy-isearch if multimatch

(defun treefactor-avy-isearch ()
  "Run `avy-isearch' in the visible portion of the current buffer."

  (goto-char (point-min))

  (let ((avy-all-windows nil)
        (avy-case-fold-search nil)
        (search-invisible nil))
    (unless (avy-isearch)               ; return nil if user quits
      (user-error "%s" "User quit Avy"))))

;; *** heading ends n newlines

(defun treefactor-region-ends-n-newlines (n)
  "Make region end in N newlines. Set point to end of region."

  (when (< n 0)
    (user-error "N is too small: %s" n))

  (let ((m (- n 1)))
    (goto-char (point-max))
    (if (bolp)
        (if (/= n 0)
            (org-N-empty-lines-before-current m)
          (org-N-empty-lines-before-current n)
          (delete-char -1))
      (insert (make-string n ?\n))))
  (goto-char (point-max)))

;; *** snort visible region

(defun treefactor-snort-visible ()
  "Move region to `treefactor-object-text'.  Widen.  Delete empty line."

  (treefactor-region-ends-n-newlines 1)
  (setq treefactor-object-text (delete-and-extract-region (point-min) (point-max)))
  (widen)
  (treefactor-delete-leftover-empty-line))

;; *** safely delete empty line

(defun treefactor-delete-leftover-empty-line ()
  "Deletes empty line at point, if there is one."

  (unless (and (bobp) (eobp))
    (if (bobp)
        (delete-char 1)
      (when
          (org--line-empty-p 1) ; (not my) bug: This wrongly returns nil when point is on an empty line at top of buffer.  Hence the workaround.
        (delete-char -1)
        (unless (eobp) (forward-char 1))))))

;; *** insert at bottom of buffer

(defun treefactor-insert-to-end-of-buffer ()
  "Add `treefactor-object-text' text to bottom of target buffer."

  (widen)
  (treefactor-region-ends-n-newlines 2)
  (insert treefactor-object-text)
  (save-buffer))

;; *** text inserted confirmation message

(defun treefactor-text-inserted-to-buffer-path-message ()
  "Report filename that text was inserted to.

Reported path is relative to vd-root-dir or ~/."

  (message "Inserted text into `%s'" (if (vc-root-dir)
                                         (expand-file-name buffer-file-name (vc-root-dir))
                                       (expand-file-name buffer-file-name user-home-directory))))

;; ** provide

(provide 'treefactor)

;;; treefactor.el ends here
