;;; notes-url.el --- Simplified url management routines for notes-mode

;;; Copyright (C) 1994-1998,2012  Free Software Foundation, Inc.

;; Author: <johnh@isi.edu>

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

;; This code was originallly cribbed from w3.el
;; by William M. Perry <wmperry@indiana.edu>,
;; but has since been completely rewritten.
;;
;; Why don't I just call his code?  Because to use
;; w3-follow-link I need to pull in at least 150k of w3.el
;; and 150k of url.el, all just to open a file on the local
;; computer.  Instead I've hacked his code down to the 3k
;; needed for opening local files.

;;; Code:

(require 'notes-variables)
(require 'notes-aux)

(defvar notes-last-url nil
  "Last URL interpreted.
This record is useful for debugging.")

;;;###autoload
(defun notes-w3-url (url &optional where best-effort)
  "Open a notes-url.  Handle simple URLs here, or call notes-w3-alternate-url.
Takes the URL as an argument.  Optionally you specify
WHERE the information should appear (either 'otherwindow or not,
defaults to not).
BEST-EFFORT causes notes-w3-url allows the tag portion of the URL to not
match.  If there's no tag match, it looks for the nearest matching prefix.

URLs optionally can begin with an URL: tag, which will be ignored.

notes-w3-url handles only <file://localhost/...> (or <file:///...>) URLs.
Other URLs it hands off to the routine in notes-w3-alternate-url
for processing.  If you use w3-mode, then
    (setq notes-w3-alternate-url 'w3-follow-link)
will have w3 handle tough URLs."
  (if (string-match "\\`[Uu][Rr][Ll]:" url)
      (setq url (substring url 4)))
  (if (not (string-match "\\`file://\\(localhost\\)?/\\(.*\\)\\'" url))
      (if (string-match "none" url)
	  (error "Notes-mode can't follow URL <none>.")
	(funcall notes-w3-alternate-url url where)) ;; now, with where! (emacs-20.4)
    (let ((filetag (match-string 2 url))
	  fname tag count count-string)
      ;; pick out the tag, if any
      (if (string-match "\\`\\([^#]*\\)#\\([0-9]+\\)?\\(.*\\)\\'" filetag)
	  (setq fname (match-string 1 filetag)
		count-string (match-string 2 filetag)
		count (if count-string (string-to-number count-string) 1)
		tag (match-string 3 filetag))
	(setq fname filetag
	      count 1
	      tag nil))
      ;; Hack---url's refering to notes-index files have different tags.
      ;; Otherwise notes-goto-index-entry fails on subjects like "* 252A".
      (if (and count-string tag (string-match "/index\\'" fname))
	  (setq tag (concat count-string tag)
		count-string "1"
		count 1))
      (if (not (string-match "\\`~" fname))   ; non-~ fnames start at fs root
	  (setq fname (concat "/" fname)))
      ;; open the file
      (cond
       ((equal where 'otherwindow) (find-file-other-window fname))
       (t (find-file (expand-file-name fname))))
      ;; handle the tag
      (if tag
	  (notes-w3-url-tag tag best-effort)
	t))))

(defun notes-w3-url-tag-backup (tag)
  "Strip the last ``part'' off of TAG."
  (let ((result)
	(separators " /\t.:")
	(buf (get-buffer-create " *notes-w3-url-tag-backup")))
    (with-current-buffer buf
      (erase-buffer)
      (insert tag)
      (goto-char (point-max))
      (skip-chars-backward (concat "^" separators))
      (skip-chars-backward separators)
      (delete-region (point) (point-max))
      (setq result (buffer-string)))
    (kill-buffer buf)
    result))

(defun notes-w3-url-tag (tag best-effort)
  "Find the TAG in the current buffer according to MODE.
BEST-EFFORT is either t (do prefix matching),
nil find the tag exactly,
or 'searching (used internally)."
  (cond
   ((not tag) nil)
   ((and (string= tag "") (eq best-effort 'searching)) nil)
   (t
    (goto-char (point-min))
    (if (re-search-forward
	 (concat "^" (regexp-quote tag)
		 (if (not (eq best-effort 'searching))
		     (if (eq major-mode 'notes-index-mode)
			 ": "
		       "$")))
	 (point-max) t count)
	t  ;; hit
      (if (not best-effort)
	  (error "Cannot find tag ``%s'' in %s." tag fname))
      (notes-w3-url-tag (notes-w3-url-tag-backup tag) 'searching)))))
  

(defun notes-w3-pass-through-alternate-url (url &optional where)
  "Pass a click event through to the old binding for notes-w3-url.
Try this combination:
  (add-hook 'notes-mode-load-hooks
            (function (lambda ()
                        (define-key notes-mode-map [mouse-2]
                          'notes-w3-follow-link-mouse)
                        (setq notes-w3-alternate-url
                          'notes-w3-my-alternate-url))))"
  (let ((event last-input-event))
    (funcall (lookup-key
	      (current-global-map)
	      (vector (car event)))
	     event nil)))

;;;###autoload
(defun notes-w3-follow-link (pt &optional where)
  "* Follow the URL at the point.
Takes a PT to look at and a WHERE to open the URL ('otherwindow or nil).
This code works hard to recognize URLs based on context information.
URLs can be quoted by whitespace, beginning and end of lines,
or the official < and >.

As a special case we also recognize (and skip) the text \"prev:\"
and \"next:\" before the URL.  Notes-mode uses these fields to link
entries."
  (interactive "d")
  (let*
      ((whitespace-regexp  "[ \t\n]")
       (quote-regexp whitespace-regexp)
       start end direction)
    (save-excursion
      ;; If we're on the URL header, skip over it so the next search works.
      (if (looking-at "[<A-Za-z]*:")
	  (skip-chars-forward "<A-Za-z:"))
      ;; First look backwards to whitespace or beginning of line
      ;; followed by a url header "asdf:".
      (if (re-search-backward "[ \t\n][^ \t\n]+:" (line-beginning-position) 1)
	  (forward-char 1)          ; whitespace bound
	(setq quote-regexp "\n"))   ; eoln bound
      ;; Handle the common case of next/prev pointers.
      ;; If we're on one, skip to the <> quoted URL which presumably
      ;; follows.  (This hack is to support a guy who doesn't use
      ;; the mouse and so looks up urls at the beginning of the line.)
      (if (looking-at "\\(prev\\|next\\):")
	  (skip-chars-forward "^<" (line-end-position)))
      ;; Check for a quoting character.
      (cond
       ((equal (char-after (point)) ?<)
	(progn
	  (setq quote-regexp ">")
	  (forward-char 1)))
       ((equal (char-after (point)) ?\")
	(progn
	  (setq quote-regexp "\"")
	  (forward-char 1))))
      ;; Remember start of url.
      (setq start (point))
      ;; Search for end of url.
      (if (re-search-forward quote-regexp (line-end-position) 1)
	  (forward-char -1))
      (setq end (point))
      ;; Interpret it (outside the save-excursion so we can go
      ;; to places in the same buffer).
      (setq notes-last-url (buffer-substring start end)))
    (notes-w3-url notes-last-url where)))

;;;###autoload
(defun notes-w3-follow-link-mouse (e)
  "* Follow the URL where the mouse is."
  (interactive "e")
  (mouse-set-point e)
  (notes-w3-follow-link (point)
			(if notes-w3-follow-link-mouse-other-window
			    'otherwindow
			  nil)))

(provide 'notes-url)
;;; notes-url.el ends here
