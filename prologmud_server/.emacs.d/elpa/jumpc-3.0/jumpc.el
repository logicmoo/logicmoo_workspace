;;; jumpc.el --- jump to previous insertion points  -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Ivan Kanis <ivan@kanis.fr>
;; Version: 3.0

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This implements the jump cursor feature found in vim.

;; A jump is added every time you insert a character on a different
;; line.

;; Jumps are remembered in a jump list.  With the C-o and C-i
;; command you can go to cursor positions before older jumps, and back
;; again.  Thus you can move up and down the list.

;; Jumps are read and saved in the same configuration file as vim so
;; you can switch back and forth between the two editors

;;;; THANKS:

;; Bram Moolenaar for writing a fine editor and helping needy children
;; in Uganda

;; Stefan Monnier for telling me how to add C-i binding

;; Ted Zlatanov for suggesting to use less agressive key bindings

;;;; BUGS:

;;;; INSTALLATION:

;; put this file somewhere in your load path then put the put the
;; following in your .emacs:
;;
;; (require 'jumpc)
;; (jumpc)
;;
;; Then either:
;;
;; (jumpc-bind-vim-key)
;;
;; Or:
;;
;; (global-set-key (kbd "<f8>") 'jumpc-jump-backward)
;; (global-set-key (kbd "<f9>") 'jumpc-jump-forward)
;;
;; The first will bind C-i and C-o just like vim.  The second bind
;; function keys 8 and 9. Of course you can pick any keys you like.  If
;; you use Emacs on a console you will have to pick the second form as
;; C-i and TAB are the same thing.
;;
;; If you use autoload you don't need to require the file

;;;; TODO

;; search for TODO within the file

;; Add rotate jump list feature in vim
;;   defined as JUMPLIST_ROTATE in mark.c
;;     "If last used entry is not at the top, put it at the top by
;;     rotating the stack until it is (the newer entries will be at
;;     the bottom). Keep one entry (the last used one) at the top."

;; Add a bit of looseness.  For example do not add jump points within
;; three lines of the last one.

;; Add jump list displaying each jump with the file and line in the
;; file.  Clicking on a line takes you to the jump location.

;; Add more commands that will insert jump.  Vim does: "'", "`", "G",
;; "/", "?", "n", "N", "%", "(", ")", "[[", "]]", "{", "}", ":s",
;; ":tag", "L", "M", "H"

;; Remove files that do not exist when reading and writing from
;; configuration file

;;;; VERSION

;; version 1

;; version 2
;;  - don't force vim key bindings
;;  - remove debugging message
;;  - insert a jump moves the index back to top of list
;;  - insert jumps goes back to top of list

;; version 3
;;  - remove deleted files

;;; Code:

(defvar jumpc-file "~/.viminfo"
  "File where jump information is written.")

(defvar jumpc-list nil
  "List of filenames and position to jump to.")

(defvar jumpc-index 0
  "Index of jump, 0 is the newest entry.")

(defun jumpc-read-list ()
  "Read jump list from file."
  (when (file-exists-p jumpc-file)
    (with-temp-buffer
      (insert-file-contents jumpc-file)
      (goto-char (point-min))
      (when (re-search-forward "# Jumplist (newest first):" nil t)
        (while (re-search-forward
                "-'  \\([0-9]*\\)  \\([0-9]*\\)  \\(.*\\)" nil t)
          (add-to-list 'jumpc-list
                       (list (string-to-number (match-string 1))
                             (string-to-number (match-string 2))
                             (expand-file-name (match-string 3)))
                       jumpc-list)))))
  jumpc-list)

(defun jumpc-write-list ()
  "Write jump list to file."
  (let (bgn end)
    (jumpc-remove-deleted-file)
    (find-file jumpc-file)
    (goto-char (point-min))
    (setq bgn (re-search-forward "# Jumplist (newest first):" nil t))
    (if bgn
        (progn
          (setq end (re-search-forward "^$"))
          (delete-region bgn end))
      ;; looks like the entry doesn't exist, tack it at the end
      (goto-char (point-max))
      (insert "# Jumplist (newest first):"))
    (insert "\n")
    (dolist (line jumpc-list)
      (insert (format "-'  %d  %d  %s\n"
                      (nth 0 line) (nth 1 line)
                      (abbreviate-file-name (nth 2 line)))))
    (save-buffer)))

(defun jumpc-jump-backward ()
  "Jump backward in list of jumps."
  (interactive)
  (jumpc-jump 1))

(defun jumpc-jump-forward ()
  "Jump forward in list of jumps."
  (interactive)
  (jumpc-jump -1))

;; TODO make it interactive with COUNT as argument
(defun jumpc-jump (count)
  "Jump COUNT from current index."
  (jumpc-remove-deleted-file)
  (let ((length (length jumpc-list)) file-name)
    ;; first backward motion adds current point in the list
    (when (and (> count 0) (= jumpc-index 0))
      (jumpc-insert))
    (setq jumpc-index (+ jumpc-index count))
    ;; fix index if it's out of boundary
    (cond
     ((< jumpc-index 0)
      (setq jumpc-index 0))
     ((> jumpc-index length)
      (setq jumpc-index length))
     (t
      (setq file-name (nth 2 (nth jumpc-index jumpc-list)))
      (find-file file-name)
      (goto-char (point-min))
      (forward-line (1- (nth 0 (nth jumpc-index jumpc-list))))
      (move-to-column (nth 0 (nth jumpc-index jumpc-list)))))))

(defun jumpc-insert ()
  "Insert jump location."
  ;; It means we are going back to the top of the list
  (setq jumpc-index 0)
  (when buffer-file-name
    (when (not (= (line-number-at-pos) (nth 0 (car jumpc-list))))
      (setq jumpc-list
            (cons (list (line-number-at-pos) (current-column) buffer-file-name)
                  jumpc-list)))))

(defun jumpc-remove-deleted-file ()
  "Remove deleted file in the list.
Returns list minus deleted files."
  (let ((length (length jumpc-list))
        (index 0)
        reduced-list element)
    (while (< index length)
      (setq element (nth index jumpc-list))
      (when (file-exists-p (nth 2 element))
        (setq reduced-list (cons element reduced-list)))
      (setq index (1+ index)))
    (setq jumpc-list reduced-list)))

(defun jumpc-bind-vim-key ()
  "Bind keys just like vim."
  (global-set-key (kbd "C-o") 'jumpc-jump-backward)
  (define-key input-decode-map [?\C-i] [control-i])
  (global-set-key [control-i] 'jumpc-jump-forward))

;;;###autoload
(defun jumpc ()
  "Initialize jump cursor."
  (interactive)
  (setq jumpc-list (jumpc-read-list))
  (defadvice self-insert-command
    (after jumpc-insert activate)
    "Insert jump position after insertion."
    (jumpc-insert))
  (add-hook 'kill-emacs-hook 'jumpc-write-list))


;; vi:et:sw=4:ts=4:
;; Local Variables:
;; compile-command: "make"
;; End:

;;;; ChangeLog:

;; 2013-06-22  Ivan Kanis  <ivan@kanis.fr>
;; 
;; 	jumpc version 3, handle deleted files
;; 
;; 2013-04-07  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Add jumpc.
;; 


(provide 'jumpc)
;;; jumpc.el ends here
